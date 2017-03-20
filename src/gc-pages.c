// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "gc.h"
#ifndef _OS_WINDOWS_
#  include <sys/resource.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _P64
#define DEFAULT_REGION_PG_COUNT (4096) // 64 MB
#else
#define DEFAULT_REGION_PG_COUNT (1024) // 16 MB
#endif
#define MIN_REGION_PG_COUNT (32) // same as the size of allocmap/freemap granularity

static int region_pg_cnt = DEFAULT_REGION_PG_COUNT;
static jl_mutex_t pagealloc_lock;
static size_t current_pg_count = 0;

void jl_gc_init_page(void)
{
    if (GC_PAGE_SZ * region_pg_cnt < jl_page_size)
        region_pg_cnt = jl_page_size / GC_PAGE_SZ; // exact division
}

#ifndef MAP_NORESERVE // not defined in POSIX, FreeBSD, etc.
#define MAP_NORESERVE (0)
#endif

// Try to allocate a memory block for a region with `pg_cnt` pages.
// Return `NULL` if allocation failed. Result is aligned to `GC_PAGE_SZ`.
static char *jl_gc_try_alloc_pages(int pg_cnt)
{
    size_t pages_sz = GC_PAGE_SZ * pg_cnt;
#ifdef _OS_WINDOWS_
    char *mem = (char*)VirtualAlloc(NULL, pages_sz + GC_PAGE_SZ,
                                    MEM_RESERVE, PAGE_READWRITE);
    if (mem == NULL)
        return NULL;
#else
    if (GC_PAGE_SZ > jl_page_size)
        pages_sz += GC_PAGE_SZ;
    char *mem = (char*)mmap(0, pages_sz, PROT_READ | PROT_WRITE,
                            MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED)
        return NULL;
#endif
    if (GC_PAGE_SZ > jl_page_size)
        // round data pointer up to the nearest gc_page_data-aligned
        // boundary if mmap didn't already do so.
        mem = (char*)gc_page_data(mem + GC_PAGE_SZ - 1);
    return mem;
}

// Allocate the memory for a new page. Starts with `region_pg_cnt` number
// of pages. Decrease 4x every time so that there are enough space for a few.
// more regions (or other allocations). The final page count is recorded
// and will be used as the starting count next time. If the page count is
// smaller `MIN_REGION_PG_COUNT` a `jl_memory_exception` is thrown.
// Assumes `pagealloc_lock` is acquired, the lock is released before the
// exception is thrown.
static jl_gc_pagemeta_t *jl_gc_alloc_new_page(void)
{
    unsigned pg, pg_cnt = region_pg_cnt;
    char *mem = NULL;
    while (1) {
        if (__likely((mem = jl_gc_try_alloc_pages(pg_cnt))))
            break;
        size_t min_region_pg_count = MIN_REGION_PG_COUNT;
        if (GC_PAGE_SZ * min_region_pg_count < jl_page_size)
            min_region_pg_count = jl_page_size / GC_PAGE_SZ; // exact division
        if (pg_cnt >= 4 * MIN_REGION_PG_COUNT) {
            pg_cnt /= 4;
            region_pg_cnt = pg_cnt;
        }
        else if (pg_cnt > MIN_REGION_PG_COUNT) {
            region_pg_cnt = pg_cnt = MIN_REGION_PG_COUNT;
        }
        else {
            JL_UNLOCK_NOGC(&pagealloc_lock);
            jl_throw(jl_memory_exception);
        }
    }

    jl_gc_pagemeta_t *page_meta = (jl_gc_pagemeta_t*)calloc(pg_cnt, sizeof(jl_gc_pagemeta_t));
    for (pg = 0; pg < pg_cnt; pg++) {
        struct jl_gc_metadata_ext info;
        uint32_t msk;
        unsigned i;
        region1_t **pregion1;
        region0_t **pregion0;
        jl_gc_pagemeta_t **pmeta;

        char *ptr = mem + (GC_PAGE_SZ * pg);
        page_meta[pg].data = ptr;

        i = REGION_INDEX(ptr);
        info.region_i = i % 32;
        info.region_i32 = i / 32;
        msk = (1 << info.region_i);
        if ((memory_map.freemap1[info.region_i32] & msk) == 0)
            memory_map.freemap1[info.region_i32] |= msk; // has free
        info.region1 = *(pregion1 = &memory_map.meta1[i]);
        if (!info.region1)
            info.region1 = (*pregion1 = (region1_t*)calloc(1, sizeof(region1_t)));

        i = REGION1_INDEX(ptr);
        info.region1_i = i % 32;
        info.region1_i32 = i / 32;
        msk = (1 << info.region1_i);
        if ((info.region1->freemap0[info.region1_i32] & msk) == 0)
            info.region1->freemap0[info.region1_i32] |= msk; // has free
        info.region0 = *(pregion0 = &info.region1->meta0[i]);
        if (!info.region0)
            info.region0 = (*pregion0 = (region0_t*)calloc(1, sizeof(region0_t)));

        i = REGION0_INDEX(ptr);
        info.region0_i = i % 32;
        info.region0_i32 = i / 32;
        msk = (1 << info.region0_i);
        info.region0->freemap[info.region0_i32] |= msk; // is free
        pmeta = &info.region0->meta[i];
        info.meta = (*pmeta = &page_meta[pg]);
    }
    return page_meta;
}

NOINLINE jl_gc_pagemeta_t *jl_gc_alloc_page(void)
{
    struct jl_gc_metadata_ext info;
    JL_LOCK_NOGC(&pagealloc_lock);

    // scan over memory_map for existing allocated but unused pages
    for (info.region_i32 = memory_map.lb; info.region_i32 < (REGION2_PG_COUNT + 31) / 32; info.region_i32++) {
        uint32_t freemap1 = memory_map.freemap1[info.region_i32];
        info.region_i = 0;
        while (freemap1) {
            unsigned next = ffs_u32(freemap1);
            info.region_i += next;
            freemap1 >>= (next + 1);
            info.region1 = memory_map.meta1[info.region_i + info.region_i32 * 32];
            // repeat over region1
            for (info.region1_i32 = info.region1->lb; info.region1_i32 < REGION1_PG_COUNT / 32; info.region1_i32++) {
                uint32_t freemap0 = info.region1->freemap0[info.region1_i32];
                info.region1_i = 0;
                while (freemap0) {
                    unsigned next = ffs_u32(freemap0);
                    info.region1_i += next;
                    freemap0 >>= (next + 1);
                    info.region0 = info.region1->meta0[info.region1_i + info.region1_i32 * 32];
                    // repeat over region0
                    for (info.region0_i32 = info.region0->lb; info.region0_i32 < REGION0_PG_COUNT / 32; info.region0_i32++) {
                        uint32_t freemap = info.region0->freemap[info.region0_i32];
                        if (freemap) {
                            info.region0_i = ffs_u32(freemap);
                            info.meta = info.region0->meta[info.region0_i + info.region0_i32 * 32];
                            assert(info.meta->data);
                            // new pages available starting at min of lb and region_i32
                            if (memory_map.lb < info.region_i32)
                                memory_map.lb = info.region_i32;
                            if (info.region1->lb < info.region1_i32)
                                info.region1->lb = info.region1_i32;
                            if (info.region0->lb < info.region0_i32)
                                info.region0->lb = info.region0_i32;
                            goto have_free_page;
                        }
                    }
                    info.region1->freemap0[info.region1_i32] &= ~(uint32_t)(1 << info.region1_i); // record that this was full
                }
            }
            memory_map.freemap1[info.region_i32] &= ~(uint32_t)(1 << info.region_i); // record that this was full
        }
    }

    // no existing pages found, allocate a new one
    {
        jl_gc_pagemeta_t *meta = jl_gc_alloc_new_page();
        info = page_metadata_ext(meta->data);
        assert(meta == info.meta);
    }
    // new pages available starting at max of lb and region_i32
    if (memory_map.lb > info.region_i32)
        memory_map.lb = info.region_i32;
    if (info.region1->lb > info.region1_i32)
        info.region1->lb = info.region1_i32;
    if (info.region0->lb > info.region0_i32)
        info.region0->lb = info.region0_i32;

have_free_page:
    if (memory_map.ub < info.region_i32)
        memory_map.ub = info.region_i32;
    if (info.region1->ub < info.region1_i32)
        info.region1->ub = info.region1_i32;
    if (info.region0->ub < info.region0_i32)
        info.region0->ub = info.region0_i32;

    // mark this entry as in-use and not free
    info.region0->freemap[info.region0_i32] &= ~(uint32_t)(1 << info.region0_i);
    info.region0->allocmap[info.region0_i32] |= (uint32_t)(1 << info.region0_i);
    info.region1->allocmap0[info.region1_i32] |= (uint32_t)(1 << info.region1_i);
    memory_map.allocmap1[info.region_i32] |= (uint32_t)(1 << info.region_i);

#ifdef _OS_WINDOWS_
    VirtualAlloc(meta->data, GC_PAGE_SZ, MEM_COMMIT, PAGE_READWRITE);
#endif
    current_pg_count++;
    gc_final_count_page(current_pg_count);
    JL_UNLOCK_NOGC(&pagealloc_lock);
    return info.meta;
}

void jl_gc_free_page(void *p)
{
    // update the allocmap and freemap to indicate this contains a free entry
    struct jl_gc_metadata_ext info = page_metadata_ext(p);
    uint32_t msk;
    msk = (uint32_t)(1 << info.region0_i);
    assert(!(info.region0->freemap[info.region0_i32] & msk));
    assert(info.region0->allocmap[info.region0_i32] & msk);
    info.region0->allocmap[info.region0_i32] &= ~msk;
    info.region0->freemap[info.region0_i32] |= msk;

    msk = (uint32_t)(1 << info.region1_i);
    assert(info.region1->allocmap0[info.region1_i32] & msk);
    if ((info.region1->freemap0[info.region1_i32] & msk) == 0)
        info.region1->freemap0[info.region1_i32] |= msk;

    msk = (uint32_t)(1 << info.region_i);
    assert(memory_map.allocmap1[info.region_i32] & msk);
    if ((memory_map.freemap1[info.region_i32] & msk) == 0)
        memory_map.freemap1[info.region_i32] |= msk;

    free(info.meta->ages);
    info.meta->ages = NULL;

    // tell the OS we don't need these pages right now
    size_t decommit_size = GC_PAGE_SZ;
    if (GC_PAGE_SZ < jl_page_size) {
        // ensure so we don't release more memory than intended
        size_t n_pages = jl_page_size / GC_PAGE_SZ; // exact division
        decommit_size = jl_page_size;
        p = (void*)((uintptr_t)p & ~(jl_page_size - 1)); // round down to the nearest physical page
        while (n_pages--) {
            struct jl_gc_metadata_ext info = page_metadata_ext(p);
            msk = (uint32_t)(1 << info.region0_i);
            if (info.region0->allocmap[info.region0_i32] & msk)
                goto no_decommit;
            p = (void*)((char*)p + GC_PAGE_SZ);
        }
    }
#ifdef _OS_WINDOWS_
    VirtualFree(p, decommit_size, MEM_DECOMMIT);
#else
    madvise(p, decommit_size, MADV_DONTNEED);
#endif

no_decommit:
    if (memory_map.lb > info.region_i32)
        memory_map.lb = info.region_i32;
    if (info.region1->lb > info.region1_i32)
        info.region1->lb = info.region1_i32;
    if (info.region0->lb > info.region0_i32)
        info.region0->lb = info.region0_i32;
    current_pg_count--;
}

#ifdef __cplusplus
}
#endif
