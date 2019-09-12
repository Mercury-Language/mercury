// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000-2002, 2006, 2011-2012 The University of Melbourne.
// Copyright (C) 2015-2016, 2018-2019 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_string.c - string handling

#include "mercury_imp.h"
#include "mercury_string.h"

#ifdef _MSC_VER
    // Disable warnings about using _vsnprintf being deprecated.
    #pragma warning(disable:4996)

    // va_copy is available from VC 2013 onwards.
    #if _MSC_VER < 1800
        #define va_copy(a, b)   ((a) = (b))
    #endif
#endif

#if defined(MR_HAVE__VSNPRINTF)
int
MR_vsnprintf(char *str, size_t size, const char *format, va_list ap)
{
    va_list     ap_copy;
    int         n;

    if (size == 0) {
        return _vsnprintf(NULL, 0, format, ap);
    }

    // _vsnprintf does not append a null terminator if the output is truncated.
    // Follow the MS advice of initialising the buffer to null before calling
    // _vsnprintf with a count strictly less than the buffer length.
    memset(str, 0, size);
    va_copy(ap_copy, ap);
    n = _vsnprintf(str, size - 1, format, ap_copy);
    va_end(ap_copy);

    if (n == -1) {
        // Return the number of characters that would have been written
        // without truncation, to match the behaviour of C99 vsnprintf.
        n = _vsnprintf(NULL, 0, format, ap);
    }

    return n;
}
#endif

#if defined(MR_HAVE__SNPRINTF)
int
MR_snprintf(char *str, size_t size, const char *format, ...)
{
    va_list     ap;
    int         n;

    va_start(ap, format);
    n = MR_vsnprintf(str, size, format, ap);
    va_end(ap);

    return n;
}
#endif

#define BUFFER_SIZE 4096

MR_String
MR_make_string(MR_AllocSiteInfoPtr alloc_id, const char *fmt, ...)
{
    va_list     ap;
    MR_String   result;
    int         n;
    char        *p;

    int         size = BUFFER_SIZE;
    char        fixed[BUFFER_SIZE];
    MR_bool     dynamically_allocated = MR_FALSE;

    // On the first iteration we try with a fixed-size buffer.
    // If that didn't work, use a dynamically allocated array twice
    // the size of the fixed array and keep growing the array until
    // the string fits.

    p = fixed;

    while (1) {
        // Try to print in the allocated space.
        va_start(ap, fmt);
        n = MR_vsnprintf(p, size, fmt, ap);
        va_end(ap);

        // If that worked, return the string.
        if (n > -1 && n < size) {
            break;
        }

        // Else try again with more space.
        if (n > -1) {   // glibc 2.1
            size = n + 1; // precisely what is needed
        } else {        // glibc 2.0
            size *= 2;  // twice the old size
        }

        if (!dynamically_allocated) {
            p = MR_NEW_ARRAY(char, size);
            dynamically_allocated = MR_TRUE;
        } else {
            p = MR_RESIZE_ARRAY(p, char, size);
        }
    }

    MR_restore_transient_hp();
    MR_allocate_aligned_string_msg(result, strlen(p), alloc_id);
    MR_save_transient_hp();
    strcpy(result, p);

    if (dynamically_allocated) {
        MR_free(p);
    }

    return result;
}

// The code for this function should be kept in sync with that of the
// quote_string predicates in library/term_io.m.
MR_bool
MR_escape_string_quote(MR_String *ptr, const char * string)
{
    MR_Integer pos = 0;
    size_t  num_code_units = 0;
    MR_Char ch;
    MR_bool must_escape = MR_FALSE;

    // Check if we need to add character escapes to the string.
    // XXX ILSEQ Check for surrogate code points.
    while ((ch = MR_utf8_get_next((MR_String) string, &pos)) > 0) {
        switch (ch) {
            case '\a':
            case '\b':
            case '\f':
            case '\n':
            case '\t':
            case '\r':
            case '\v':
            case '\"':
            case '\\':
                num_code_units += 2;
                must_escape = MR_TRUE;
                break;
            default:
                if (MR_is_control(ch)) {
                    // All control characters that do not have a specific
                    // backslash escape are octal escaped.
                    // This takes five code units.
                    num_code_units += 5;
                    must_escape = MR_TRUE;
                } else {
                    num_code_units += MR_utf8_width(ch);
                }
        }
    }

    // Check that the string's encoding was valid.
    if (ch < 0) {
        *ptr = NULL;
        return MR_FALSE;
    }

    if (must_escape) {
        char *dst;
   
        MR_allocate_aligned_string_saved_hp(*ptr,
            num_code_units + 2 /* quotes */ + 1 /* \0 */,
            NULL);

        dst = *ptr;
        dst[0] = '\"';
        dst++;
        pos = 0;
        while ((ch = MR_utf8_get_next((MR_String) string, &pos)) > 0) {
            switch (ch) {
                case '\a':
                    dst[0] = '\\';
                    dst[1] = 'a';
                    dst += 2;
                    break; 
                case '\b':
                    dst[0] = '\\';
                    dst[1] = 'b';
                    dst += 2;
                    break; 
                case '\f':
                    dst[0] = '\\';
                    dst[1] = 'f';
                    dst += 2;
                    break; 
                case '\n':
                    dst[0] = '\\';
                    dst[1] = 'n';
                    dst += 2;
                    break; 
                case '\t':
                    dst[0] = '\\';
                    dst[1] = 't';
                    dst += 2;
                    break; 
                case '\r':
                    dst[0] = '\\';
                    dst[1] = 'r';
                    dst += 2;
                    break; 
                case '\v':
                    dst[0] = '\\';
                    dst[1] = 'v';
                    dst += 2;
                    break; 
                case '\"':
                    dst[0] = '\\';
                    dst[1] = '\"';
                    dst += 2;
                    break; 
                case '\\':
                    dst[0] = '\\';
                    dst[1] = '\\';
                    dst += 2;
                    break;
                default:
                    if (MR_is_control(ch)) {
                        sprintf(dst, "\\%03" MR_INTEGER_LENGTH_MODIFIER "o\\",
                            (MR_Integer) ch);
                        dst += 5;
                    } else {
                        dst += MR_utf8_encode(dst, ch);
                    }
             }
        }
        dst[0] = '\"';
        dst[1] = '\0';
    } else {
        MR_make_aligned_string_copy_saved_hp_quote(*ptr, string, NULL);
    }
    return MR_TRUE;
}

// Note that MR_hash_string{,2,3,4,5,6} are actually defined as macros in
// mercury_string.h, if we are using GNU C.
// We define them here whether or not we are using gcc, so that users
// can easily switch between gcc and cc without rebuilding the libraries.

#undef MR_hash_string
#undef MR_hash_string2
#undef MR_hash_string3
#undef MR_hash_string4
#undef MR_hash_string5
#undef MR_hash_string6

MR_Integer
MR_hash_string(MR_ConstString s)
{
    MR_HASH_STRING_FUNC_BODY
}

MR_Integer
MR_hash_string2(MR_ConstString s)
{
    MR_HASH_STRING2_FUNC_BODY
}

MR_Integer
MR_hash_string3(MR_ConstString s)
{
    MR_HASH_STRING3_FUNC_BODY
}

MR_Integer
MR_hash_string4(MR_ConstString s)
{
    MR_HASH_STRING4_FUNC_BODY
}

MR_Integer
MR_hash_string5(MR_ConstString s)
{
    MR_HASH_STRING5_FUNC_BODY
}

MR_Integer
MR_hash_string6(MR_ConstString s)
{
    MR_HASH_STRING6_FUNC_BODY
}

MR_bool
MR_utf8_next(const MR_String s_, MR_Integer *pos)
{
    const unsigned char *s = (const unsigned char *)s_;
    int c;

    if (s[*pos] == '\0') {
        // End of string.
        return MR_FALSE;
    }

    for (;;) {
        ++(*pos);
        c = s[*pos];
        if (MR_utf8_is_single_byte(c) || MR_utf8_is_lead_byte(c)) {
            break;
        }
    }

    return MR_TRUE;
}

MR_bool
MR_utf8_prev(const MR_String s_, MR_Integer *pos)
{
    const unsigned char *s = (const unsigned char *)s_;
    int c;

    while (*pos > 0) {
        (*pos)--;
        c = s[*pos];
        if (MR_utf8_is_single_byte(c) || MR_utf8_is_lead_byte(c)) {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

MR_int_least32_t
MR_utf8_get(const MR_String s_, MR_Integer pos)
{
    const unsigned char *s = (const unsigned char *)s_;
    int c;
    int width;

    c = s[pos];
    if (MR_is_ascii(c)) {
        return c;
    } else {
        return MR_utf8_get_mb(s_, pos, &width);
    }
}

MR_int_least32_t
MR_utf8_get_mb(const MR_String s_, MR_Integer pos, int *width)
{
    const unsigned char *s = (const unsigned char *)s_;
    int c;
    int d;
    int minc;

    c = s[pos];

    // c <= 0x7f (ASCII) must be handled before calling this function.

    if (c <= 0xC1) {
        // Trailing byte of multi-byte sequence or an overlong encoding for
        // code point <= 127.

        return -2;
    }

    if (c <= 0xDF) {
        // 2-byte sequence.
        c &= 0x1F;
        *width = 2;
        minc = 0x80;
    }
    else if (c <= 0xEF) {
        // 3-byte sequence.
        c &= 0x0F;
        *width = 3;
        minc = 0x800;
    }
    else if (c <= 0xF4) {
        // 4-byte sequence.
        c &= 0x07;
        *width = 4;
        minc = 0x10000;
    }
    else {
        // Otherwise invalid.
        return -2;
    }

    switch (*width) {
        case 4:
            d = s[++pos];
            if (!MR_utf8_is_trail_byte(d)) {
                return -2;
            }
            c = (c << 6) | (d & 0x3F);
            // fall through
        case 3:
            d = s[++pos];
            if (!MR_utf8_is_trail_byte(d)) {
                return -2;
            }
            c = (c << 6) | (d & 0x3F);
            // fall through
        case 2:
            d = s[++pos];
            if (!MR_utf8_is_trail_byte(d)) {
                return -2;
            }
            c = (c << 6) | (d & 0x3F);
            break;
    }

    // Check for overlong forms or code point out of range.
    if (c < minc || c > 0x10FFFF) {
        return -2;
    }

    // Check for surrogate code points.
    if (MR_is_surrogate(c)) {
        return -2;
    }

    return c;
}

MR_int_least32_t
MR_utf8_get_next(const MR_String s, MR_Integer *pos)
{
    int c;

    c = s[*pos];
    if (MR_is_ascii(c)) {
        (*pos)++;
        return c;
    }

    return MR_utf8_get_next_mb(s, pos);
}

MR_int_least32_t
MR_utf8_get_next_mb(const MR_String s, MR_Integer *pos)
{
    int c, width;

    c = MR_utf8_get_mb(s, *pos, &width);
    if (c >= 0) {
        // Multibyte code point.
        (*pos) += width;
        return c;
    }

    // Some invalid byte sequence.
    MR_utf8_next(s, pos);
    return c;
}

MR_int_least32_t
MR_utf8_prev_get(const MR_String s, MR_Integer *pos)
{
    int c, width;

    if (MR_utf8_prev(s, pos)) {
        c = s[*pos];
        if (MR_is_ascii(c)) {
            return c;
        } else {
            return MR_utf8_get_mb(s, *pos, &width);
        }
    }

    // Past beginning.
    return -1;
}

size_t
MR_utf8_width(MR_Char c)
{
    // So we don't need to check for negative values nor use unsigned ints
    // in the interface, which are a pain.

    MR_UnsignedChar uc = c;

    if (uc <= 0x7f) {
        return 1;
    }
    if (uc <= 0x7ff) {
        return 2;
    }
    if (uc <= 0xffff) {
        return (MR_is_surrogate(uc)) ? 0 : 3;
    }
    if (uc <= 0x10ffff) {
        return 4;
    }
    // The rest are illegal.
    return 0;
}

size_t
MR_utf8_encode(char s_[], MR_Char c)
{
    unsigned char *s = (unsigned char *)s_;
    MR_UnsignedChar uc = c;

    if (uc <= 0x7f) {
        s[0] = uc;
        return 1;
    }

    if (uc <= 0x7ff) {
        s[0] = 0xC0 | ((uc >> 6) & 0x1F);
        s[1] = 0x80 |  (uc       & 0x3F);
        return 2;
    }

    if (uc <= 0xffff) {
        if (MR_is_surrogate(uc)) {
            return 0;
        }
        s[0] = 0xE0 | ((uc >> 12) & 0x0F);
        s[1] = 0x80 | ((uc >>  6) & 0x3F);
        s[2] = 0x80 |  (uc        & 0x3F);
        return 3;
    }

    if (uc <= 0x10ffff) {
        s[0] = 0xF0 | ((uc >> 18) & 0x07);
        s[1] = 0x80 | ((uc >> 12) & 0x3F);
        s[2] = 0x80 | ((uc >>  6) & 0x3F);
        s[3] = 0x80 |  (uc        & 0x3F);
        return 4;
    }

    // Otherwise is illegal.
    return 0;
}

MR_bool
MR_utf8_verify(const MR_String s)
{
    MR_Integer pos = 0;

    for (;;) {
        MR_int_least32_t c;

        c = MR_utf8_get_next(s, &pos);
        if (c == 0) {
            return MR_TRUE;
        }
        if (c < 0) {
            return MR_FALSE;
        }
    }
}
