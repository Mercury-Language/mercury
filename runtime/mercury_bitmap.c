// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_bitmap.c - bitmap handling

#include "mercury_imp.h"
#include "mercury_bitmap.h"

#include <stdio.h>

static MR_String MR_do_bitmap_to_string(MR_ConstBitmapPtr, MR_bool, MR_bool,
    MR_AllocSiteInfoPtr);

// Note that MR_bitmap_cmp and MR_hash_bitmap are actually defined
// as macros in mercury_bitmap.h, if we are using GNU C.
// We define them here whether or not we are using gcc, so that users
// can easily switch between gcc and cc without rebuilding the libraries.

#undef MR_bitmap_cmp

MR_Integer
MR_bitmap_cmp(MR_ConstBitmapPtr b1, MR_ConstBitmapPtr b2)
{
    MR_BITMAP_CMP_FUNC_BODY
}

#undef MR_hash_bitmap

MR_Integer
MR_hash_bitmap(MR_ConstBitmapPtr b)
{
    MR_HASH_BITMAP_FUNC_BODY
}

static const char hex_digits[] =
    {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F'};

MR_String
MR_bitmap_to_quoted_string_saved_hp(MR_ConstBitmapPtr b,
    MR_AllocSiteInfoPtr alloc_id)
{
    return MR_do_bitmap_to_string(b, MR_TRUE, MR_TRUE, alloc_id);
}

static MR_String
MR_do_bitmap_to_string(MR_ConstBitmapPtr b,
    MR_bool quote, MR_bool use_saved_hp, MR_AllocSiteInfoPtr alloc_id)
{
    MR_String   result;
    size_t      i;
    size_t      len;
    size_t      num_bytes;
    size_t      num_bits_len;
    int         start;
    char        num_bits_str[100];

    sprintf(num_bits_str, "%" MR_INTEGER_LENGTH_MODIFIER "d", b->num_bits);
    num_bits_len = strlen(num_bits_str);
    num_bytes = MR_bitmap_length_in_bytes(b->num_bits);
    len = 1 + num_bits_len + 1 + num_bytes * 2 + 1;
    if (quote) {
        len += 2;
    }

    if (use_saved_hp) {
        MR_allocate_aligned_string_saved_hp(result, len, alloc_id);
    } else {
        MR_allocate_aligned_string_msg(result, len, alloc_id);
    }

    if (quote) {
        result[0] = '"';
        result[1] = '<';
        result[len - 2] = '>';
        result[len - 1] = '"';
        start = 2;
    } else {
        result[0] = '<';
        result[len - 1] = '>';
        start = 1;
    }
    strcpy(result + start, num_bits_str);
    start += num_bits_len;
    result[start++] = ':';
    for (i = 0; i < num_bytes; i++) {
        result[start++] = hex_digits[(b->elements[i] >> 4) & 0xf];
        result[start++] = hex_digits[b->elements[i] & 0xf];
    }
    result[len] = '\0';
    return result;
}
