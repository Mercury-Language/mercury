// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_bitmap.h - bitmap handling

#ifndef MERCURY_BITMAP_H
#define MERCURY_BITMAP_H

#include "mercury_tags.h"
#include <string.h>     // for memcmp()

// The actual typedefs are in mercury_types.h to avoid problems with
// circular #includes.

// Like memcpy, but for bitmaps.
// The destination must already have been allocated.

#define MR_copy_bitmap(dest, src)                                       \
    do {                                                                \
        MR_BitmapPtr copy_dest = dest;                                  \
        MR_ConstBitmapPtr copy_src = src;                               \
        memcpy(copy_dest->elements, copy_src->elements,                 \
            MR_bitmap_length_in_bytes(copy_src->num_bits));             \
    } while (0)

// Like memcmp, but for bitmaps.

#define MR_do_bitmap_cmp(res,p1,p2)                                     \
    do {                                                                \
        MR_ConstBitmapPtr cmp_b1 = (p1);                                \
        MR_ConstBitmapPtr cmp_b2 = (p2);                                \
        MR_Integer cmp_size1 = cmp_b1->num_bits;                        \
        MR_Integer cmp_size2 = cmp_b2->num_bits;                        \
        if (cmp_size1 < cmp_size2) {                                    \
            (res) = -1;                                                 \
        } else if (cmp_size1 > cmp_size2) {                             \
            (res) = 1;                                                  \
        } else {                                                        \
            (res) = memcmp(cmp_b1->elements, cmp_b2->elements,          \
                        MR_bitmap_length_in_bytes(cmp_size1));          \
        }                                                               \
    } while (0)

MR_Integer MR_bitmap_cmp(MR_ConstBitmapPtr, MR_ConstBitmapPtr);

#if defined(MR_GNUC)
#define MR_bitmap_cmp(b1, b2)                                           \
    ({                                                                  \
        MR_Integer bitmap_cmp_result;                                   \
        MR_do_bitmap_cmp(bitmap_cmp_result, b1, b2);                    \
        bitmap_cmp_result;                                              \
    })
#endif

// If we are not using gcc, the actual definition of MR_bitmap_cmp is in
// runtime/mercury_bitmap.c; it uses the macro MR_BITMAP_CMP_FUNC_BODY below.

#define MR_BITMAP_CMP_FUNC_BODY                                         \
       MR_Integer bitmap_cmp_result;                                    \
       MR_do_bitmap_cmp(bitmap_cmp_result, b1, b2);                     \
       return bitmap_cmp_result;

#define MR_bitmap_eq(b1, b2) (MR_bitmap_cmp((b1), (b2)) == 0)

// MR_do_hash_bitmap(int & hash, MR_Word bitmap):
//
// Given a Mercury bitmap `bitmap', set `hash' to the hash value
// for that bitmap. (`hash' must be an lvalue.)
//
// This is an implementation detail used to implement MR_hash_bitmap().
// It should not be used directly. Use MR_hash_bitmap() instead.
//
// Note that hash_bitmap is also defined in library/bitmap.m.
// The definition here and the definition in bitmap.m must be kept equivalent.

#define MR_do_hash_bitmap(hash, b)                                      \
    {                                                                   \
        int len = 0;                                                    \
        MR_ConstBitmapPtr hash_bm = (b);                                \
        MR_CHECK_EXPR_TYPE((hash), int);                                \
        (hash) = 0;                                                     \
        while (len < MR_bitmap_length_in_bytes(hash_bm->num_bits)) {    \
            (hash) ^= ((hash) << 5);                                    \
            (hash) ^= hash_bm->elements[len];                           \
            len++;                                                      \
        }                                                               \
        (hash) ^= hash_bm->num_bits;                                    \
    }

// MR_hash_bitmap(b):
//
// Given a Mercury bitmap `b', return a hash value for that array.

MR_Integer    MR_hash_bitmap(MR_ConstBitmapPtr);

#if defined(MR_GNUC)
#define MR_hash_bitmap(b)                                               \
    ({                                                                  \
        MR_Integer hash_bitmap_result;                                  \
        MR_CHECK_EXPR_TYPE(b, MR_ConstBitmapPtr);                       \
        MR_do_hash_bitmap(hash_bitmap_result, (b));                     \
        hash_bitmap_result;                                             \
    })
#endif

// If we are not using gcc, the actual definition of MR_hash_bitmap is in
// runtime/mercury_bitmap.c;
// it uses the macro MR_HASH_BITMAP_FUNC_BODY below.

#define MR_HASH_BITMAP_FUNC_BODY                                        \
       MR_Integer hash_bitmap_result;                                   \
       MR_do_hash_bitmap(hash_bitmap_result, b);                        \
       return hash_bitmap_result;

// Convert a bitmap to a string consisting of a length followed by a colon
// and a string of hexadecimal digits surrounded by angle brackets and
// double quotes (e.g. "\"<24:12A>\""). Used by `deconstruct.functor/3'.

MR_String MR_bitmap_to_quoted_string_saved_hp(MR_ConstBitmapPtr,
        MR_AllocSiteInfoPtr alloc_id);

// Return the length of the element array in words.

#define MR_bitmap_length_in_words(bits)                                 \
        (((bits) / MR_WORDBITS) + (((bits) % MR_WORDBITS) != 0))

// We assume MR_uint_least8_t is 8 bits, which will be true on
// all sane machines.

#define MR_BITS_PER_BYTE 8

// Return the length of the element array in bytes.

#define MR_bitmap_length_in_bytes(bits)                                 \
        (((bits) / MR_BITS_PER_BYTE) + (((bits) % MR_BITS_PER_BYTE) != 0))

#define MR_bitmap_byte_index_for_bit(bit)                               \
    ((bit) / MR_BITS_PER_BYTE)
#define MR_bitmap_bit_index_within_byte(bit)                            \
    ((bit) % MR_BITS_PER_BYTE)

#define MR_bitmap_zero(bitmap)                                          \
    do {                                                                \
        size_t bytes = MR_bitmap_length_in_bytes((bitmap)->num_bits);   \
        memset((bitmap)->elements, 0, bytes);                           \
    } while (0)

#define MR_bitmap_get_bit(bitmap, bit)                                  \
    (!!(((bitmap)->elements[MR_bitmap_byte_index_for_bit(bit)]) &       \
        (1 << MR_bitmap_bit_index_within_byte(bit))))

#define MR_bitmap_set_bit(bitmap, bit)                                  \
    do {                                                                \
        MR_uint_least8_t    byte;                                       \
                                                                        \
        byte = (bitmap)->elements[MR_bitmap_byte_index_for_bit(bit)];   \
        byte |= 1 << MR_bitmap_bit_index_within_byte(bit);              \
        (bitmap)->elements[MR_bitmap_byte_index_for_bit(bit)] = byte;   \
    } while (0)

#define MR_bitmap_clear_bit(bitmap, bit)                                \
    do {                                                                \
        MR_uint_least8_t    byte;                                       \
                                                                        \
        byte = (bitmap)->elements[MR_bitmap_byte_index_for_bit(bit)];   \
        byte &= ~(1 << MR_bitmap_bit_index_within_byte(bit));           \
        (bitmap)->elements[MR_bitmap_byte_index_for_bit(bit)] =  byte;  \
    } while (0)

// void MR_allocate_bitmap_msg(MR_BitmapPtr ptr, size_t bits,
//          MR_Code *proclabel):
//
// Allocate enough word aligned memory to hold `bits' bits. Also record,
// for memory profiling purposes, the location, proclabel, of the allocation
// if profiling is enabled.
//
// BEWARE: this may modify `MR_hp', so it must only be called from places
// where `MR_hp' is valid. If calling it from inside a C function, rather than
// inside Mercury code, you may need to call MR_{save/restore}_transient_hp().

#define MR_allocate_bitmap_msg(ptr, bits, alloc_id)                     \
    do {                                                                \
        MR_Word    make_bitmap_tmp;                                     \
        MR_BitmapPtr make_bitmap_ptr;                                   \
        MR_offset_incr_hp_atomic_msg(make_bitmap_tmp, 0,                \
            MR_bitmap_length_in_words(bits) + 1, (alloc_id),            \
            "bitmap.bitmap/0");                                         \
        make_bitmap_ptr = (MR_BitmapPtr) make_bitmap_tmp;               \
        make_bitmap_ptr->num_bits = bits;                               \
        (ptr) = make_bitmap_ptr;                                        \
    } while (0)

#define MR_allocate_bitmap_saved_hp(ptr, bits, alloc_id)                \
    do {                                                                \
        MR_Word    make_bitmap_tmp;                                     \
        MR_BitmapPtr make_bitmap_ptr;                                   \
        MR_offset_incr_saved_hp_atomic(make_bitmap_tmp, 0,              \
            MR_bitmap_length_in_words(bits) + 1, (alloc_id),            \
            "bitmap.bitmap/0");                                         \
        make_bitmap_ptr = (MR_BitmapPtr) make_bitmap_tmp;               \
        make_bitmap_ptr->num_bits = bits;                               \
        (ptr) = make_bitmap_ptr;                                        \
    } while (0)

#endif // not MERCURY_BITMAP_H
