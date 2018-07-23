// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2005, 2007, 2011-2012 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_string.h - string handling

#ifndef MERCURY_STRING_H
#define MERCURY_STRING_H

#include "mercury_heap.h"       // for MR_offset_incr_hp_atomic

#include <string.h>             // for strcmp() etc.
#include <stdarg.h>
#include <stdio.h>

// On Windows, snprintf/vsnprintf may be synonyms for _snprintf/_vsnprintf and
// thus not conform to the C99 specification. Since it is next to impossible to
// tell at compile time which implementation we are getting, just define a
// wrapper over _vsnprintf if it exists.
// Beginning with the UCRT in Visual Studio 2015 and Windows 10, snprintf and
// vsnprintf are C99 standard compliant so we may be able to drop this code
// eventually.

#if defined(MR_HAVE__VSNPRINTF)
    extern int
    MR_vsnprintf(char *str, size_t size, const char *format, va_list ap);
#elif defined(MR_HAVE_VSNPRINTF)
    #define MR_vsnprintf vsnprintf
#else
    #error "Missing both vsnprintf and _vsnprintf"
#endif

#if defined(MR_HAVE__SNPRINTF)
    extern int MR_snprintf(char *str, size_t size, const char *format, ...);
#elif defined(MR_HAVE_SNPRINTF)
    #define MR_snprintf snprintf
#else
    #error "Missing both snprintf and _snprintf"
#endif

// Mercury characters (Unicode code points) are given type `MR_Char',
// which is a typedef for `MR_int_least32_t'.
// Mercury strings are stored as pointers to '\0'-terminated arrays of `char'.
// Strings are UTF-8 encoded.
// Mercury strings must not contain null characters. Unexpected null characters
// are a source of security vulnerabilities.
//
// The actual typedefs are in mercury_types.h to avoid problems with
// circular #includes.
//
// typedef MR_int_least32_t     MR_Char;
// typedef MR_uint_least32_t    MR_UnsignedChar;
//
// typedef char                 *MR_String;
// typedef const char           *MR_ConstString;

// MR_string_const("...", len):
//
// Given a C string literal and its length, returns a Mercury string.

#define MR_string_const(string, len) ((MR_String) string)

#define MR_make_string_const(string)                                    \
                MR_string_const((string), sizeof(string) - 1)

// MR_bool MR_string_equal(MR_ConstString s1, MR_ConstString s2):
// Return true iff the two Mercury strings s1 and s2 are equal.

#define MR_string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)

// void MR_make_aligned_string(MR_String ptr, const char *string):
//
// Given a C string `string', set `ptr' to be a Mercury string with the
// same contents. (`ptr' must be an lvalue.) If the resulting Mercury string
// is to be used by Mercury code, then the string pointed to by `string'
// should have been either statically allocated or allocated on the
// Mercury heap.
//
// BEWARE: this may modify `MR_hp', so it must only be called from places
// where `MR_hp' is valid. If calling it from inside a C function, rather than
// inside Mercury code, you may need to call MR_{save/restore}_transient_hp().
//
// Algorithm: if the string is aligned, just set ptr equal to it.
// Otherwise, allocate space on the heap and copy the C string to
// the Mercury string.

#define MR_make_aligned_string(ptr, string)                             \
    do {                                                                \
        if (MR_tag((MR_Word) (string)) != 0) {                          \
            MR_make_aligned_string_copy((ptr), (string));               \
        } else {                                                        \
            /* The cast is there to cast away const, if needed. */      \
            (ptr) = (MR_String) (string);                               \
        }                                                               \
    } while (0)

// void MR_make_aligned_string_copy(MR_String ptr, const char * string):
//
// Same as MR_make_aligned_string(ptr, string), except that the string
// is guaranteed to be copied. This is useful for copying C strings
// onto the Mercury heap.
//
// BEWARE: this may modify `MR_hp', so it must only be called from places
// where `MR_hp' is valid. If calling it from inside a C function, rather than
// inside Mercury code, you may need to call MR_{save/restore}_transient_hp().

#define MR_make_aligned_string_copy(ptr, string)                        \
    MR_make_aligned_string_copy_msg((ptr), (string), NULL)

#define MR_make_aligned_string_copy_msg(ptr, string, alloc_id)          \
    do {                                                                \
        MR_Word make_aligned_string_tmp;                                \
        char    *make_aligned_string_ptr;                               \
                                                                        \
        MR_offset_incr_hp_atomic_msg(make_aligned_string_tmp, 0,        \
            (strlen(string) + sizeof(MR_Word)) / sizeof(MR_Word),       \
            (alloc_id), "string.string/0");                             \
        make_aligned_string_ptr = (char *) make_aligned_string_tmp;     \
        strcpy(make_aligned_string_ptr, (string));                      \
        (ptr) = make_aligned_string_ptr;                                \
    } while (0)

// void MR_make_aligned_string_copy_saved_hp(MR_String ptr,
//          const char * string):
//
// Same as MR_make_aligned_string_copy(ptr, string), except that it uses
// MR_offset_incr_saved_hp_atomic instead of MR_offset_incr_hp_atomic.

#define MR_make_aligned_string_copy_saved_hp(ptr, string, alloc_id)     \
    do {                                                                \
        MR_Word make_aligned_string_tmp;                                \
        char    *make_aligned_string_ptr;                               \
                                                                        \
        MR_offset_incr_saved_hp_atomic(make_aligned_string_tmp, 0,      \
            (strlen(string) + sizeof(MR_Word)) / sizeof(MR_Word),       \
            (alloc_id), "string.string/0");                             \
        make_aligned_string_ptr = (char *) make_aligned_string_tmp;     \
        strcpy(make_aligned_string_ptr, (string));                      \
        (ptr) = make_aligned_string_ptr;                                \
    } while (0)

// void MR_make_aligned_string_copy_saved_hp_quote(MR_String ptr,
//          const char * string):
//
// Same as MR_make_aligned_string_copy_saved_hp(ptr, string), except that
// it puts double quote marks at the start and end of the string.

#define MR_make_aligned_string_copy_saved_hp_quote(ptr, string, alloc_id) \
    do {                                                                  \
        MR_Word make_aligned_string_tmp;                                  \
        char    *make_aligned_string_ptr;                                 \
                                                                          \
        MR_offset_incr_saved_hp_atomic(make_aligned_string_tmp, 0,        \
            (strlen(string) + 2 + sizeof(MR_Word)) / sizeof(MR_Word),     \
            (alloc_id), "string.string/0");                               \
        make_aligned_string_ptr = (char *) make_aligned_string_tmp;       \
        sprintf(make_aligned_string_ptr, "%c%s%c", '"', string, '"');     \
        (ptr) = make_aligned_string_ptr;                                  \
    } while (0)

// void MR_allocate_aligned_string_msg(MR_String ptr, size_t len,
//          MR_Code *proclabel):
//
// Allocate enough word aligned memory to hold len characters. Also record
// the location, proclabel, of the allocation if profiling is enabled.
//
// BEWARE: this may modify `MR_hp', so it must only be called from places
// where `MR_hp' is valid. If calling it from inside a C function, rather than
// inside Mercury code, you may need to call MR_{save/restore}_transient_hp().

#define MR_allocate_aligned_string_msg(ptr, len, alloc_id)              \
    do {                                                                \
        MR_Word make_aligned_string_tmp;                                \
        char    *make_aligned_string_ptr;                               \
                                                                        \
        MR_offset_incr_hp_atomic_msg(make_aligned_string_tmp, 0,        \
            ((len) + sizeof(MR_Word)) / sizeof(MR_Word),                \
            (alloc_id), "string.string/0");                             \
        make_aligned_string_ptr = (char *) make_aligned_string_tmp;     \
        (ptr) = make_aligned_string_ptr;                                \
    } while (0)

#define MR_allocate_aligned_string_saved_hp(ptr, len, alloc_id)         \
    do {                                                                \
        MR_Word make_aligned_string_tmp;                                \
        char    *make_aligned_string_ptr;                               \
                                                                        \
        MR_offset_incr_saved_hp_atomic(make_aligned_string_tmp, 0,      \
            ((len) + sizeof(MR_Word)) / sizeof(MR_Word),                \
            (alloc_id), "string.string/0");                             \
        make_aligned_string_ptr = (char *) make_aligned_string_tmp;     \
        (ptr) = make_aligned_string_ptr;                                \
    } while (0)

// MR_do_hash_string{,2,3}(int & hash, MR_Word string):
//
// Given a Mercury string `string', set `hash' to the hash value
// for that string. (`hash' must be an lvalue.)
//
// This is an implementation detail used to implement MR_hash_string{,2,3}().
// It should not be used directly. Use MR_hash_string{,2,3}() instead.
//
// Note that these functions are also defined in library/string.m.
// The definition here and in string.m must be kept equivalent.

#define MR_do_hash_string(hash, s)                                      \
    {                                                                   \
        int len;                                                        \
        MR_CHECK_EXPR_TYPE(hash, int);                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        len = 0;                                                        \
        hash = 0;                                                       \
        while (((const unsigned char *)(s))[len]) {                     \
            hash ^= (hash << 5);                                        \
            hash ^= ((const unsigned char *)(s))[len];                  \
            len++;                                                      \
        }                                                               \
        hash ^= len;                                                    \
    }

#define MR_do_hash_string2(hash, s)                                     \
    {                                                                   \
        int len;                                                        \
        MR_CHECK_EXPR_TYPE(hash, int);                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        len = 0;                                                        \
        hash = 0;                                                       \
        while (((const unsigned char *)(s))[len]) {                     \
            hash = hash * 37;                                           \
            hash += ((const unsigned char *)(s))[len];                  \
            len++;                                                      \
        }                                                               \
        hash ^= len;                                                    \
    }

#define MR_do_hash_string3(hash, s)                                     \
    {                                                                   \
        int len;                                                        \
        MR_CHECK_EXPR_TYPE(hash, int);                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        len = 0;                                                        \
        hash = 0;                                                       \
        while (((const unsigned char *)(s))[len]) {                     \
            hash = hash * 49;                                           \
            hash += ((const unsigned char *)(s))[len];                  \
            len++;                                                      \
        }                                                               \
        hash ^= len;                                                    \
    }

#define MR_keep_30_bits(x)                                              \
    ((x) & ((1 << 30) - 1))

#define MR_do_hash_string4(hash, s)                                     \
    {                                                                   \
        int i;                                                          \
        MR_CHECK_EXPR_TYPE(hash, int);                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        hash = 0;                                                       \
        for (i = 0; ((const unsigned char *)(s))[i] != 0; i++) {        \
            hash = MR_keep_30_bits(hash ^ (hash << 5));                 \
            hash = hash ^ ((const unsigned char *)(s))[i];              \
        }                                                               \
        hash ^= i;                                                      \
    }

#define MR_do_hash_string5(hash, s)                                         \
    {                                                                       \
        int i;                                                              \
        MR_CHECK_EXPR_TYPE(hash, int);                                      \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                              \
        hash = 0;                                                           \
        for (i = 0; ((const unsigned char *)(s))[i] != 0; i++) {            \
            hash = MR_keep_30_bits(hash * 37);                              \
            hash = MR_keep_30_bits(hash + ((const unsigned char *)(s))[i]); \
        }                                                                   \
        hash ^= i;                                                          \
    }

#define MR_do_hash_string6(hash, s)                                         \
    {                                                                       \
        int i;                                                              \
        MR_CHECK_EXPR_TYPE(hash, int);                                      \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                              \
        hash = 0;                                                           \
        for (i = 0; ((const unsigned char *)(s))[i] != 0; i++) {            \
            hash = MR_keep_30_bits(hash * 49);                              \
            hash = MR_keep_30_bits(hash + ((const unsigned char *)(s))[i]); \
        }                                                                   \
        hash ^= i;                                                          \
    }

// MR_hash_string{,2,3}(s):
//
// Given a Mercury string `s', return a hash value for that string.

MR_Integer      MR_hash_string(MR_ConstString);
MR_Integer      MR_hash_string2(MR_ConstString);
MR_Integer      MR_hash_string3(MR_ConstString);
MR_Integer      MR_hash_string4(MR_ConstString);
MR_Integer      MR_hash_string5(MR_ConstString);
MR_Integer      MR_hash_string6(MR_ConstString);

#if defined(MR_GNUC)
#define MR_hash_string(s)                                               \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string(hash_string_result, s);                       \
        hash_string_result;                                             \
    })

#define MR_hash_string2(s)                                              \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string2(hash_string_result, s);                      \
        hash_string_result;                                             \
    })

#define MR_hash_string3(s)                                              \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string3(hash_string_result, s);                      \
        hash_string_result;                                             \
    })

#define MR_hash_string4(s)                                              \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string4(hash_string_result, s);                      \
        hash_string_result;                                             \
    })

#define MR_hash_string5(s)                                              \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string5(hash_string_result, s);                      \
        hash_string_result;                                             \
    })

#define MR_hash_string6(s)                                              \
    ({                                                                  \
        MR_Integer hash_string_result;                                  \
        MR_CHECK_EXPR_TYPE(s, MR_ConstString);                          \
        MR_do_hash_string6(hash_string_result, s);                      \
        hash_string_result;                                             \
    })
#endif

// If we are not using gcc, the actual definitions of these functions
// are runtime/mercury_string.c; they use the macros below.

#define MR_HASH_STRING_FUNC_BODY                                        \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string(hash_string_result, s);                        \
       return hash_string_result;
#define MR_HASH_STRING2_FUNC_BODY                                       \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string2(hash_string_result, s);                       \
       return hash_string_result;
#define MR_HASH_STRING3_FUNC_BODY                                       \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string3(hash_string_result, s);                       \
       return hash_string_result;
#define MR_HASH_STRING4_FUNC_BODY                                       \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string4(hash_string_result, s);                       \
       return hash_string_result;
#define MR_HASH_STRING5_FUNC_BODY                                       \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string5(hash_string_result, s);                       \
       return hash_string_result;
#define MR_HASH_STRING6_FUNC_BODY                                       \
       MR_Integer hash_string_result;                                   \
       MR_do_hash_string6(hash_string_result, s);                       \
       return hash_string_result;

// A version of strcmp to which we can pass Mercury words
// without having to cast the arguments first.

#define MR_strcmp(s, t)         strcmp((const char *)(s), (const char *)(t))

// Assuming that the first n code units of s and t are equal,
// are the rest of s and t equal?

#define MR_offset_streq(n, s, t)                                        \
    (strcmp((const char *)((s)+(n)), (const char *)((t)+(n))) == 0)

// Return the kth code unit in a string.

#define MR_nth_code_unit(s, k)                                          \
    ((unsigned) (((const unsigned char *) (s))[(k)]))

// Return an MR_String which has been created using the format string, fmt,
// passed to sprintf. If memory profiling is turned on, record the allocation
// as coming from proclabel. The MR_String returned has been allocated
// on the mercury heap using MR_allocate_aligned_string_msg.
//
// BEWARE: this may modify the saved copy of `MR_hp', so it must only be called
// from places where the saved copy of `MR_hp' is valid. You will generally
// need to call MR_{save/restore}_transient_hp() before/after calling
// this function.

MR_String MR_make_string(MR_AllocSiteInfoPtr alloc_id, const char *fmt, ...);

// Given a Mercury string `string', make a copy that inserts any required
// character escapes and places double quote marks at the start and end of
// the copy. On success, returns MR_TRUE and sets `ptr' to point to the copy
// of the string. Returns MR_FALSE if `string' is not a valid UTF-8 encoded
// string.
//
extern MR_bool MR_escape_string_quote(MR_String *ptr,
    const char * string);

// True if c is an ASCII code point, i.e. U+0000..U+007f.

#define MR_is_ascii(c)              ((unsigned) (c) <= 0x7f)

// True if c is a Unicode surrogate code point, i.e. U+D800..U+DFFF.

#define MR_is_surrogate(c)          (((unsigned) (c) & 0xF800) == 0xD800)

// True if c is a Unicode control code point, i.e. U+0000..U+001f,
// U+007f..U+009f.

#define MR_is_control(c) \
    ((0x00 <= (unsigned) (c) && (unsigned) (c) <= 0x1f) || \
     (0x7f <= (unsigned) (c) && (unsigned) (c) <= 0x9f))

// UTF-8 manipulation

#define MR_utf8_is_single_byte(c)   (((unsigned) (c) & 0x80) == 0)
#define MR_utf8_is_lead_byte(c)     (((unsigned) (c) - 0xC0) < 0x3E)
#define MR_utf8_is_trail_byte(c)    (((unsigned) (c) & 0xC0) == 0x80)

// Advance `*pos' to the beginning of the next code point in `s'.
// If `*pos' is already at the end of the string, return MR_FALSE
// without modifying `*pos'.

extern MR_bool          MR_utf8_next(const MR_String s_, MR_Integer *pos);

// Rewind `*pos' to the beginning of the previous code point in `s'.
// If `*pos' is already at the beginning of the string, return MR_FALSE
// without modifying `*pos'.

extern MR_bool          MR_utf8_prev(const MR_String s_, MR_Integer *pos);

// Decode and return the code point beginning at `pos' in `s'.
// Return 0 if at the end of the string (i.e. the NUL terminator).
// If an illegal code sequence exists at that offset, return -2.
//
// The _mb version requires s[pos] to be the lead byte of a multibyte code
// point.

extern MR_int_least32_t MR_utf8_get(const MR_String s, MR_Integer pos);
extern MR_int_least32_t MR_utf8_get_mb(const MR_String s, MR_Integer pos,
                            int *width);

// Decode the code point beginning at `pos' in `s', and advance `*pos'.
// The _mb version requires s[pos] to be the lead byte of a multibyte code
// point.

extern MR_int_least32_t MR_utf8_get_next(const MR_String s, MR_Integer *pos);
extern MR_int_least32_t MR_utf8_get_next_mb(const MR_String s,
                            MR_Integer *pos);

// Rewind `*pos' to the beginning of the previous code point in `s'
// and return that code code.
// Return -1 if `*pos' is already at the beginning of the string.

extern MR_int_least32_t MR_utf8_prev_get(const MR_String s, MR_Integer *pos);

// Return the number of bytes required to encode the code point `c' in UTF-8.

extern size_t           MR_utf8_width(MR_Char c);

// Encode the code point `c' into the buffer `s'.
// Return the number of bytes used.

extern size_t           MR_utf8_encode(char s[], MR_Char c);

// Return MR_TRUE iff `s' contains a valid UTF-8 encoded string.

extern MR_bool          MR_utf8_verify(const MR_String s);

#endif // not MERCURY_STRING_H
