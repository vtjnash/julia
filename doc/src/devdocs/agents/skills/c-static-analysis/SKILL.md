---
name: c-static-analysis
description: Run Clang static analysis on Julia's C/C++ runtime and codegen, and satisfy the GC-rooting checker. Use after modifying runtime/codegen .c/.cpp files under src/ (excluding headers), before opening a PR.
---

# C/C++ static analysis (src/)

Use this after you modify a runtime/codegen C/C++ source file under `src/` (not
headers). Note that runtime changes also require a rebuild (`make -j`).

Run static analysis checks:

- First run `make -C src install-analysis-deps` to initialize dependencies (only
  needed once the first time, or after the LLVM/Clang toolchain dependencies
  change). This may download and install LLVM/Clang artifacts.
- For a source file such as `src/jloptions.c` or `src/codegen.cpp`, use the file
  stem without the `.c`/`.cpp` extension:
  ```sh
  make -C src analyze-<file-stem> -j8 [--output-sync]
  ```
  For example, to analyze `src/jloptions.c`, run:
  ```sh
  make -C src analyze-jloptions -j8
  ```
  Add `--output-sync` when your `make` supports it to keep parallel output
  grouped; otherwise omit it.
- `analyze-<file-stem>` runs four checks: `clang-sa` (static analyzer),
  `clang-tidy`, `clang-safety` (the thread-safety / safepoint checker), and
  `clang-sagc` (the GC-rooting checker). Rerun any one individually with
  `clang-sa-<file-stem>`, `clang-tidy-<file-stem>`, `clang-safety-<file-stem>`,
  or `clang-sagc-<file-stem>`.

## Fixing the GC-rooting checker (clang-sagc)

If `clang-sagc-<file-stem>` fails, first look for fixes that establish real
rooting, such as adding appropriate `JL_GC_PUSH`/`JL_GC_POP` scopes, or for
lock/control-flow fixes.

Do not add `JL_GC_PROMISE_ROOTED` without explicit user or maintainer
confirmation. `JL_GC_PROMISE_ROOTED` asserts that a value is already rooted; it
does not root the value. If it appears necessary, stop and ask for confirmation,
showing the exact expression, the existing root that makes it safe, and the
safepoints considered.

- Remember arguments are assumed rooted, so check the callers to make sure that
  is handled.
- As a diagnostic hint when asking for confirmation: if the value is temporarily
  moved through a struct or arraylist and then reloaded, the promised expression
  may need to refer to the reloaded field, such as
  `JL_GC_PROMISE_ROOTED(struct->field)`, immediately after the reload and before
  any use of that field.
- If confirmed, put the promise as early in the code as is legal, near the
  definition or reload rather than the use.

## Fixing the thread-safety / safepoint checker (clang-safety)

`clang-safety-<stem>` runs clang's `-Wthread-safety` over the safepoint
annotations (see `src/support/analyzer_annotations.h`). It models two
capabilities:

- `jl_notsafepoint` — held while inside a no-GC-lock region (entered by
  `JL_NOTSAFEPOINT_ENTER`, e.g. taking a no-gc lock; released by
  `JL_NOTSAFEPOINT_LEAVE`).
- `jl_gcunsaferegion` — held while in a gc-unsafe region, i.e. a state that is
  able to safepoint (entered/left by `JL_CANSAFEPOINT_ENTER`/`_LEAVE`).

Under this checker the annotations expand to capability requirements:

| Annotation | Requirement it imposes on callers |
| --- | --- |
| `JL_CANSAFEPOINT` | `jl_gcunsaferegion` held and `!jl_notsafepoint` |
| `JL_CANCALLBACK` | `!jl_notsafepoint` only (doesn't care about `jl_gcunsaferegion`) |
| `JL_CANSAFEPOINT_ENTER_LEAVE` | `!jl_gcunsaferegion` and `!jl_notsafepoint` |

Key mental model: `jl_notsafepoint` is treated as possibly-held at function
entry. `!jl_notsafepoint` only becomes provable after a `JL_NOTSAFEPOINT_LEAVE`
(a lock release) or because the function itself declares a requirement for it.
So an unannotated function cannot call a `!jl_notsafepoint` function until that
negative capability is established with `JL_CANSAFEPOINT`.

### Interpreting the errors

- `calling function 'X' requires negative capability '!jl_notsafepoint'` — you
  are calling something that may safepoint/callback while a no-gc lock is (or may
  be) held. Either you are genuinely holding a lock across a safepoint (a real
  bug — release it first, or move the call out), or the enclosing function simply
  hasn't declared that it may safepoint. In the latter case, annotate the
  enclosing function (below) so the requirement propagates to its callers.
- `calling function 'X' requires negative capability '!jl_gcunsaferegion'` — you
  are calling a `..._ENTER_LEAVE` (or `CANSAFEPOINT_ENTER`) function from a
  context that is already in a gc-unsafe region. Those functions transition the
  region themselves, so they must be called from a not-yet-gc-unsafe state.
- Errors reported inside an LLVM header (e.g. `ThreadSafeModule.h`) mean the
  checker followed an inlined template callback (like `withModuleDo`) — see the
  boundary note below.

### Implementing the fix

Pick the annotation for the enclosing function by how it is called:

- `JL_CANSAFEPOINT` — a normal function that may reach a safepoint and is always
  called from a gc-unsafe (safepoint-capable) context. This is the usual fix.
- `JL_CANCALLBACK` — the function may invoke arbitrary callbacks / run LLVM
  passes and does not care whether the caller is in a gc-unsafe region (e.g.
  code that also runs on worker). Requires only `!jl_notsafepoint`.
- `JL_CANSAFEPOINT_ENTER_LEAVE` — a callback invoked from an unknown / not-yet
  gc-unsafe context that enters and then leaves the gc-unsafe region internally
  (e.g. an ORC materialization callback, or anything called alongside an existing
  `_ENTER_LEAVE` sibling).

Adding one of these pushes the requirement up to the callers, so repeat up the
call chain until you reach a natural boundary that establishes `!jl_notsafepoint`:

- a `JL_NOTSAFEPOINT_LEAVE` (a real lock release) already present in a caller;
- a fresh thread entry point that holds no locks — annotate it
  `JL_CANSAFEPOINT_ENTER_LEAVE`;
- a Julia-facing entry point (e.g. a `JL_DLLEXPORT` called from Julia) whose C
  callers are not analyzed — annotating it is the terminus;
- an external/LLVM callback boundary — passing a lambda to an external function
  (e.g. ORC `withModuleDo`) swallows the lambda's requirement, so annotating the
  lambda ends the chain there.

Put the annotation on the function's first declaration (the header
prototype, not the definition, when they differ), or `clang-tidy` will flag it
with the julia plugins (and also can `--fix` it).

### When a safepoint is genuinely untrackable

If the safepoint happens behind an LLVM header template the checkers cannot see
through (e.g. `withModuleDo` inlines and inherits the caller's state, so the
GC checker cannot re-enable safepoints inside it), do not misannotate the whole
chain. Hide just the untrackable call from the analyzers, mirroring the existing
guard at `src/pipeline.cpp` around `MPM.run`:

```c++
#if !defined(__clang_safetyanalysis__)
    transform(M, R); // real safepoint work, checked via the annotated callee
#endif
```

Reserve this for real boundaries and explain it in a comment — it is not a way to
silence a legitimate rooting or lock-across-safepoint bug.
