//===- ArchiveWriter.h - ar archive file format writer ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Declares the writeArchive function for writing an archive file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM39_OBJECT_ARCHIVEWRITER_H
#define LLVM39_OBJECT_ARCHIVEWRITER_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Object/Archive.h"
#include "llvm/Support/FileSystem.h"

#include "llvm/Config/llvm-config.h"
#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 8
#define Expected ErrorOr
#define errorCodeToError std::error_code
#define consumeError(x)
#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 3
#define PosixZeroTime() PosixZeroTime
#include "llvm/Object/Archive.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/TimeValue.h"
#include "./EndianStream39.h"
namespace std {
    typedef llvm::error_code error_code;
}
#endif

namespace llvm39 {

using namespace llvm;

struct NewArchiveMember {
  std::unique_ptr<MemoryBuffer> Buf;
  sys::TimeValue ModTime = sys::TimeValue::PosixZeroTime();
  unsigned UID = 0, GID = 0, Perms = 0644;

  NewArchiveMember() = default;
  NewArchiveMember(NewArchiveMember &&Other)
      : Buf(std::move(Other.Buf)), ModTime(Other.ModTime), UID(Other.UID),
        GID(Other.GID), Perms(Other.Perms) {}
  NewArchiveMember &operator=(NewArchiveMember &&Other) {
    Buf = std::move(Other.Buf);
    ModTime = Other.ModTime;
    UID = Other.UID;
    GID = Other.GID;
    Perms = Other.Perms;
    return *this;
  }
  NewArchiveMember(std::unique_ptr<MemoryBuffer> BufRef);
};

std::pair<StringRef, std::error_code>
writeArchive(StringRef ArcName, std::vector<NewArchiveMember> &NewMembers,
             bool WriteSymtab, object::Archive::Kind Kind, bool Deterministic,
             bool Thin, std::unique_ptr<MemoryBuffer> OldArchiveBuf = nullptr);
}

#endif
