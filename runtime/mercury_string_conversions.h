// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2025-2026 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_string_conversion.h - macros for C code in library/string.m.

#ifndef MERCURY_STRING_CONVERSIONS_H
#define MERCURY_STRING_CONVERSIONS_H

/////////////////////////////////////////////////////////////////////////
//
// Doing a binary search for the correct value of num_digits
// would yield better expected-case performance if the input values
// were randomly distributed along their entire range, but linear search
// is better for use cases where smaller input values are more likely
// than larger values.
//

#define get_num_decimal_digits_in_uint8(U8, num_digits) \
    do {                                                                \
        if ((U8) < 10U) {                               /* 10^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U8) < 100U) {                       /* 10^2 */      \
            (num_digits) = 2;                                           \
        } else {                                                        \
            /* UINT8_MAX = (2^8)-1 = 255, which needs 3 digits. */      \
            (num_digits) = 3;                                           \
        }                                                               \
    } while (0)

#define get_num_decimal_digits_in_uint16(U16, num_digits) \
    do {                                                                \
        if ((U16) < 10U) {                              /* 10^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U16) < 100U) {                      /* 10^2 */      \
            (num_digits) = 2;                                           \
        } else if ((U16) < 1000U) {                     /* 10^3 */      \
            (num_digits) = 3;                                           \
        } else if ((U16) < 10000U) {                    /* 10^4 */      \
            (num_digits) = 4;                                           \
        } else {                                                        \
            /* UINT16_MAX = (2^16)-1 = 65_535 which needs 5 digits. */  \
            (num_digits) = 5;                                           \
        }                                                               \
    } while (0)

#define get_num_decimal_digits_in_uint32(U32, num_digits) \
    do {                                                                \
        if ((U32) < 10U) {                              /* 10^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U32) < 100U) {                      /* 10^2 */      \
            (num_digits) = 2;                                           \
        } else if ((U32) < 1000U) {                     /* 10^3 */      \
            (num_digits) = 3;                                           \
        } else if ((U32) < 10000U) {                    /* 10^4 */      \
            (num_digits) = 4;                                           \
        } else if ((U32) < 100000U) {                   /* 10^5 */      \
            (num_digits) = 5;                                           \
        } else if ((U32) < 1000000U) {                  /* 10^6 */      \
            (num_digits) = 6;                                           \
        } else if ((U32) < 10000000U) {                 /* 10^7 */      \
            (num_digits) = 7;                                           \
        } else if ((U32) < 100000000U) {                /* 10^8 */      \
            (num_digits) = 8;                                           \
        } else if ((U32) < 1000000000U) {               /* 10^9 */      \
            (num_digits) = 9;                                           \
        } else {                                                        \
            /* UINT32_MAX = (2^32)-1 = 4_294_967_295, */                \
            /* which needs 10 digits. */                                \
            (num_digits) = 10;                                          \
        }                                                               \
    } while (0)

#define get_num_decimal_digits_in_uint64(U64, num_digits) \
    do {                                                                \
        if ((U64) < 10U) {                              /* 10^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U64) < 100U) {                      /* 10^2 */      \
            (num_digits) = 2;                                           \
        } else if ((U64) < 1000U) {                     /* 10^3 */      \
            (num_digits) = 3;                                           \
        } else if ((U64) < 10000U) {                    /* 10^4 */      \
            (num_digits) = 4;                                           \
        } else if ((U64) < 100000U) {                   /* 10^5 */      \
            (num_digits) = 5;                                           \
        } else if ((U64) < 1000000U) {                  /* 10^6 */      \
            (num_digits) = 6;                                           \
        } else if ((U64) < 10000000U) {                 /* 10^7 */      \
            (num_digits) = 7;                                           \
        } else if ((U64) < 100000000U) {                /* 10^8 */      \
            (num_digits) = 8;                                           \
        } else if ((U64) < 1000000000U) {               /* 10^9 */      \
            (num_digits) = 9;                                           \
        } else if ((U64) < 10000000000U) {              /* 10^10 */     \
            (num_digits) = 10;                                          \
        } else if ((U64) < 100000000000U) {             /* 10^11 */     \
            (num_digits) = 11;                                          \
        } else if ((U64) < 1000000000000U) {            /* 10^12 */     \
            (num_digits) = 12;                                          \
        } else if ((U64) < 10000000000000U) {           /* 10^13 */     \
            (num_digits) = 13;                                          \
        } else if ((U64) < 100000000000000U) {          /* 10^14 */     \
            (num_digits) = 14;                                          \
        } else if ((U64) < 1000000000000000U) {         /* 10^15 */     \
            (num_digits) = 15;                                          \
        } else if ((U64) < 10000000000000000U) {        /* 10^16 */     \
            (num_digits) = 16;                                          \
        } else if ((U64) < 100000000000000000U) {       /* 10^17 */     \
            (num_digits) = 17;                                          \
        } else if ((U64) < 1000000000000000000U) {      /* 10^18 */     \
            (num_digits) = 18;                                          \
        } else if ((U64) < 10000000000000000000U) {     /* 10^19 */     \
            (num_digits) = 19;                                          \
        } else {                                                        \
            /* UINT64_MAX = (2^64)-1 = 18_446_744_073_709_551_615, */   \
            /* which needs 20 digits. */                                \
            (num_digits) = 20;                                          \
        }                                                               \
    } while (0)

////////////////////

// XXX We do not define get_num_octal_digits_in_uintN for N=8 or 16.

#define get_num_octal_digits_in_uint32(U32, num_digits) \
    do {                                                                \
        if ((U32) < 010) {                              /* 8^1 */       \
            (num_digits) = 1;                                           \
        } else if ((U32) < 0100) {                      /* 8^2 */       \
            (num_digits) = 2;                                           \
        } else if ((U32) < 01000) {                     /* 8^3 */       \
            (num_digits) = 3;                                           \
        } else if ((U32) < 010000) {                    /* 8^4 */       \
            (num_digits) = 4;                                           \
        } else if ((U32) < 0100000) {                   /* 8^5 */       \
            (num_digits) = 5;                                           \
        } else if ((U32) < 01000000) {                  /* 8^6 */       \
            (num_digits) = 6;                                           \
        } else if ((U32) < 010000000) {                 /* 8^7 */       \
            (num_digits) = 7;                                           \
        } else if ((U32) < 0100000000) {                /* 8^8 */       \
            (num_digits) = 8;                                           \
        } else if ((U32) < 01000000000) {               /* 8^9 */       \
            (num_digits) = 9;                                           \
        } else if ((U32) < 010000000000) {              /* 8^10 */      \
            (num_digits) = 10;                                          \
        } else {                                                        \
            /* 32 bits is 10 groups of three bits, plus two bits. */    \
            (num_digits) = 11;                                          \
        }                                                               \
    } while (0)

#define get_num_octal_digits_in_uint64(U64, num_digits) \
    do {                                                                \
        if ((U64) < 010) {                              /* 8^1 */       \
            (num_digits) = 1;                                           \
        } else if ((U64) < 0100) {                      /* 8^2 */       \
            (num_digits) = 2;                                           \
        } else if ((U64) < 01000) {                     /* 8^3 */       \
            (num_digits) = 3;                                           \
        } else if ((U64) < 010000) {                    /* 8^4 */       \
            (num_digits) = 4;                                           \
        } else if ((U64) < 0100000) {                   /* 8^5 */       \
            (num_digits) = 5;                                           \
        } else if ((U64) < 01000000) {                  /* 8^6 */       \
            (num_digits) = 6;                                           \
        } else if ((U64) < 010000000) {                 /* 8^7 */       \
            (num_digits) = 7;                                           \
        } else if ((U64) < 0100000000) {                /* 8^8 */       \
            (num_digits) = 8;                                           \
        } else if ((U64) < 01000000000) {               /* 8^9 */       \
            (num_digits) = 9;                                           \
        } else if ((U64) < 010000000000) {              /* 8^10 */      \
            (num_digits) = 10;                                          \
        } else if ((U64) < 0100000000000) {             /* 8^11 */      \
            (num_digits) = 11;                                          \
        } else if ((U64) < 01000000000000) {            /* 8^12 */      \
            (num_digits) = 12;                                          \
        } else if ((U64) < 010000000000000) {           /* 8^13 */      \
            (num_digits) = 13;                                          \
        } else if ((U64) < 0100000000000000) {          /* 8^14 */      \
            (num_digits) = 14;                                          \
        } else if ((U64) < 01000000000000000) {         /* 8^15 */      \
            (num_digits) = 15;                                          \
        } else if ((U64) < 010000000000000000) {        /* 8^16 */      \
            (num_digits) = 16;                                          \
        } else if ((U64) < 0100000000000000000) {       /* 8^17 */      \
            (num_digits) = 17;                                          \
        } else if ((U64) < 01000000000000000000) {      /* 8^18 */      \
            (num_digits) = 18;                                          \
        } else if ((U64) < 010000000000000000000) {     /* 8^19 */      \
            (num_digits) = 19;                                          \
        } else if ((U64) < 0100000000000000000000) {    /* 8^20 */      \
            (num_digits) = 20;                                          \
        } else if ((U64) < 01000000000000000000000) {   /* 8^21 */      \
            (num_digits) = 21;                                          \
        } else {                                                        \
            /* 64 bits is 21 groups of three bits, plus one bit. */     \
            (num_digits) = 22;                                          \
        }                                                               \
    } while (0)

////////////////////

// XXX We do not define get_num_hex_digits_in_uintN for N=8 or 16.

#define get_num_hex_digits_in_uint32(U32, num_digits) \
    do {                                                                \
        if ((U32) < 0x10) {                             /* 16^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U32) < 0x100) {                     /* 16^2 */      \
            (num_digits) = 2;                                           \
        } else if ((U32) < 0x1000) {                    /* 16^3 */      \
            (num_digits) = 3;                                           \
        } else if ((U32) < 0x10000) {                   /* 16^4 */      \
            (num_digits) = 4;                                           \
        } else if ((U32) < 0x100000) {                  /* 16^5 */      \
            (num_digits) = 5;                                           \
        } else if ((U32) < 0x1000000) {                 /* 16^6 */      \
            (num_digits) = 6;                                           \
        } else if ((U32) < 0x10000000) {                /* 16^7 */      \
            (num_digits) = 7;                                           \
        } else if ((U32) < 0x100000000) {               /* 16^8 */      \
            (num_digits) = 8;                                           \
        } else {                                                        \
            /* 32 bits is 8 groups of four bits. */                     \
            (num_digits) = 16;                                          \
        }                                                               \
    } while (0)

#define get_num_hex_digits_in_uint64(U64, num_digits) \
    do {                                                                \
        if ((U64) < 0x10) {                             /* 16^1 */      \
            (num_digits) = 1;                                           \
        } else if ((U64) < 0x100) {                     /* 16^2 */      \
            (num_digits) = 2;                                           \
        } else if ((U64) < 0x1000) {                    /* 16^3 */      \
            (num_digits) = 3;                                           \
        } else if ((U64) < 0x10000) {                   /* 16^4 */      \
            (num_digits) = 4;                                           \
        } else if ((U64) < 0x100000) {                  /* 16^5 */      \
            (num_digits) = 5;                                           \
        } else if ((U64) < 0x1000000) {                 /* 16^6 */      \
            (num_digits) = 6;                                           \
        } else if ((U64) < 0x10000000) {                /* 16^7 */      \
            (num_digits) = 7;                                           \
        } else if ((U64) < 0x100000000) {               /* 16^8 */      \
            (num_digits) = 8;                                           \
        } else if ((U64) < 0x1000000000) {              /* 16^9 */      \
            (num_digits) = 9;                                           \
        } else if ((U64) < 0x10000000000) {             /* 16^10 */     \
            (num_digits) = 10;                                          \
        } else if ((U64) < 0x100000000000) {            /* 16^11 */     \
            (num_digits) = 11;                                          \
        } else if ((U64) < 0x1000000000000) {           /* 16^12 */     \
            (num_digits) = 12;                                          \
        } else if ((U64) < 0x10000000000000) {          /* 16^13 */     \
            (num_digits) = 13;                                          \
        } else if ((U64) < 0x100000000000000) {         /* 16^14 */     \
            (num_digits) = 14;                                          \
        } else if ((U64) < 0x1000000000000000) {        /* 16^15 */     \
            (num_digits) = 15;                                          \
        } else {                                                        \
            /* 64 bits is 16 groups of four bits. */                    \
            (num_digits) = 16;                                          \
        }                                                               \
    } while (0)

/////////////////////////////////////////////////////////////////////////
//
// NOTE The expression <('0' + (U % 10))> is likely to be faster than
// <\"0123456789\"[U % 10]>. However, we have to use the latter approach
// for hex digits, both because ASCII 'a' does not immediately follow '9',
// and because we want both lower case and upper case versions.
//
// NOTE All the following macros fill in S back-to-front.
//

#define fill_string_with_unsigned_decimal(S, U, num_digits, alloc) \
    do {                                                                \
        MR_allocate_aligned_string_msg((S), (num_digits), (alloc));     \
        (S)[(num_digits)] = '\0';                                       \
        int i = (num_digits) - 1;                                       \
        do {                                                            \
            (S)[i] = (char) ('0' + ((U) % 10));                         \
            (U) = (U) / 10;                                             \
            i--;                                                        \
        } while ((U) > 0);                                              \
    } while (0)

#define fill_string_with_negative_unsigned_decimal(S, U, num_digits, alloc) \
    do {                                                                \
        MR_allocate_aligned_string_msg((S), (1 + (num_digits)), (alloc));  \
        (S)[1 + (num_digits)] = '\0';                                   \
        int i = (num_digits);                                           \
        do {                                                            \
            (S)[i] = (char) ('0' + ((U) % 10));                         \
            (U) = (U) / 10;                                             \
            i--;                                                        \
        } while ((U) > 0);                                              \
        (S)[0] = '-';                                                   \
    } while (0)

#define fill_string_with_unsigned_octal(S, U, num_digits, alloc) \
    do {                                                                \
        MR_allocate_aligned_string_msg((S), (num_digits), (alloc));     \
        (S)[(num_digits)] = '\0';                                       \
        int i = (num_digits) - 1;                                       \
        do {                                                            \
            (S)[i] = (char) ('0' + ((U) & 07));                         \
            (U) = (U) >> 3;                                             \
            i--;                                                        \
        } while ((U) > 0);                                              \
    } while (0)

#define fill_string_with_unsigned_hex_lc(S, U, num_digits, alloc) \
    do {                                                                \
        MR_allocate_aligned_string_msg((S), (num_digits), (alloc));     \
        (S)[(num_digits)] = '\0';                                       \
        int i = (num_digits) - 1;                                       \
        do {                                                            \
            (S)[i] = "0123456789abcdef"[(U) & 0xf];                     \
            (U) = (U) >> 4;                                             \
            i--;                                                        \
        } while ((U) > 0);                                              \
    } while (0)

#define fill_string_with_unsigned_hex_uc(S, U, num_digits, alloc) \
    do {                                                                \
        MR_allocate_aligned_string_msg((S), (num_digits), (alloc));     \
        (S)[(num_digits)] = '\0';                                       \
        int i = (num_digits) - 1;                                       \
        do {                                                            \
            (S)[i] = "0123456789ABCDEF"[(U) & 0xf];                     \
            (U) = (U) >> 4;                                             \
            i--;                                                        \
        } while ((U) > 0);                                              \
    } while (0)

#endif // not MERCURY_STRING_CONVERSIONS_H
