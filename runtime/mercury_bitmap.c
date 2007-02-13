/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_bitmap.c - bitmap handling */

#include "mercury_imp.h"
#include "mercury_bitmap.h"

#include <stdio.h>

static int MR_hex_char_to_int(char digit);
static MR_String MR_do_bitmap_to_string(MR_ConstBitmapPtr, MR_bool, MR_bool);

/*
** Note that MR_bitmap_cmp and MR_hash_bitmap are actually defined
** as macros in mercury_bitmap.h, if we're using GNU C.
** We define them here whether or not we're using gcc, so that users
** can easily switch between gcc and cc without rebuilding the libraries.
*/

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

static int 
MR_hex_char_to_int(char digit)
{
    switch (digit) {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'A': return 10;
        case 'B': return 11;
        case 'C': return 12;
        case 'D': return 13;
        case 'E': return 14;
        case 'F': return 15;
        default : return -1;
    }
}

MR_String
MR_bitmap_to_string(MR_ConstBitmapPtr b)
{
    return MR_do_bitmap_to_string(b, MR_FALSE, MR_TRUE);
}

MR_String
MR_bitmap_to_quoted_string_saved_hp(MR_ConstBitmapPtr b)
{
    return MR_do_bitmap_to_string(b, MR_TRUE, MR_TRUE);
}

static MR_String
MR_do_bitmap_to_string(MR_ConstBitmapPtr b,
    MR_bool quote, MR_bool use_saved_hp)
{
    MR_String result;
    int i;
    int len;
    int num_bytes;
    int num_bits_len;
    int start;
    char num_bits_str[100];

    sprintf(num_bits_str, "%d", b->num_bits);
    num_bits_len = strlen(num_bits_str);
    num_bytes = MR_bitmap_length_in_bytes(b->num_bits);
    len = 1 + num_bits_len + 1 + num_bytes * 2 + 1;
    if (quote) {
        len += 2;
    }

    if (use_saved_hp) {
        MR_allocate_aligned_string_saved_hp(result, len);
    } else {
        MR_allocate_aligned_string_msg(result, len, NULL);
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

MR_BitmapPtr
MR_string_to_bitmap(MR_ConstString s)
{
    MR_BitmapPtr result;
    int i;
    int len;
    int start;
    int res;
    unsigned int result_bits;

    len = strlen(s);
    if (len < 4 || s[0] != '<' || s[len - 1] != '>') {
        return NULL;
    }
    res = sscanf(s, "<%u:%n", &result_bits, &start);
    if (res != 1) {
        return NULL;
    }
    MR_allocate_bitmap_msg(result, (MR_Integer) result_bits, NULL);
    result->num_bits = result_bits;
    for (i = 0; i < MR_bitmap_length_in_bytes(result_bits); i++) {
        int h1, h2;
        if (start + 1 >= len - 1) {
            return NULL;
        }
        h1 = MR_hex_char_to_int(s[start++]);
        h2 = MR_hex_char_to_int(s[start++]);
        if (h1 < 0 || h2 < 0) {
            return NULL;
        }
        result->elements[i] = (MR_uint_least8_t) (h1 << 4) | h2;
    }
    return result;
}

