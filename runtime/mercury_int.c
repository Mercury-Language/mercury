// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_int.h"

MR_Integer MR_hash_int64(int64_t i)
{
    if (sizeof(int64_t) <= sizeof(MR_Integer)) {
        return (MR_Integer) i;
    } else {
        // XXX We could experiment with a different mixing function here
        // and below.
        return (uint32_t)i ^ ((uint64_t)i >> 32);
    }
}

MR_Integer MR_hash_uint64(uint64_t i)
{
    if (sizeof(uint64_t) <= sizeof(MR_Integer)) {
        return (MR_Integer) i;
    } else {
        return (uint32_t)i ^ (i >> 32);
    }
}
