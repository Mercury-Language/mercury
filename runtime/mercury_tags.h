// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-2001, 2003-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_tags.h - defines macros for tagging and untagging words.
// Also defines macros for accessing the Mercury list type from C.

#ifndef MERCURY_TAGS_H
#define MERCURY_TAGS_H

#include <limits.h>         // for `CHAR_BIT'
#include "mercury_conf.h"   // for `MR_LOW_TAG_BITS'
#include "mercury_types.h"  // for `MR_Word'
#include "mercury_std.h"    // for `MR_PASTE2'

// DEFINITIONS FOR WORD LAYOUT

#define MR_WORDBITS (CHAR_BIT * sizeof(MR_Word))

// MR_TAGBITS specifies the number of bits in each word
// that we can use for tags.

#ifndef MR_TAGBITS
  #define MR_TAGBITS  MR_LOW_TAG_BITS
#endif

#define MR_mktag(t)     (t)
#define MR_unmktag(w)   (w)
#define MR_tag(w)       (((MR_Word) (w)) & ((1 << MR_TAGBITS) - 1))
#define MR_mkbody(i)    ((i) << MR_TAGBITS)
#define MR_unmkbody(w)  ((MR_Word) (w) >> MR_TAGBITS)
#define MR_body(w, t)   ((MR_Word) (w) - (t))
#define MR_strip_tag(w) ((w) & ((~ (MR_Word) 0) << MR_TAGBITS))

// The result of MR_mkword() is cast to (MR_Word *), not to (MR_Word)
// because MR_mkword() may be used in initializers for static constants
// and casts from pointers to integral types are not valid
// constant-expressions in ANSI C. It cannot be (const MR_Word *) because
// some ANSI C compilers won't allow assignments where the RHS is of type
// const and the LHS is not declared const.

#define MR_mkword(t, p)             ((MR_Word *) ((char *) (p) + (t)))
#define MR_tmkword(t, p)            (MR_mkword(MR_mktag(t), p))
#define MR_tbmkword(t, p)           (MR_mkword(MR_mktag(t), MR_mkbody(p)))

#define MR_field(t, p, i)           ((MR_Word *) MR_body((p), (t)))[i]
#define MR_const_field(t, p, i)     ((const MR_Word *) MR_body((p), (t)))[i]
#define MR_cfield(t, p, i)          (MR_const_field(t, p, i))

#define MR_tfield(t, p, i)          (MR_field(MR_mktag(t), p, i))
#define MR_const_tfield(t, p, i)    (MR_const_field(MR_mktag(t), p, i))
#define MR_ctfield(t, p, i)         (MR_const_tfield(t, p, i))

#define MR_mask_field(p, i)         ((MR_Word *) MR_strip_tag(p))[i]
#define MR_const_mask_field(p, i)   ((const MR_Word *) MR_strip_tag(p))[i]

// The hl_ variants are the same, except their return type is MR_Box
// rather than MR_Word. These are used by the MLDS->C back-end.

#define MR_hl_field(t, p, i)        ((MR_Box *) MR_body((p), (t)))[i]
#define MR_hl_const_field(t, p, i)  ((const MR_Box *) MR_body((p), (t)))[i]

#define MR_hl_mask_field(p, i)      ((MR_Box *) MR_strip_tag(p))[i]
#define MR_hl_const_mask_field(p, i) ((const MR_Box *) MR_strip_tag(p))[i]

#define MR_hl_tfield(t, p, i)       (MR_hl_field(MR_mktag(t), p, i))
#define MR_hl_const_tfield(t, p, i) (MR_hl_const_field(MR_mktag(t), p, i))

// The following macros are used by handwritten C code that needs to access
// Mercury data structures. The definitions of these macros depend on the data
// representation scheme used by compiler/du_type_layout.m.

#define MR_FIRST_UNRESERVED_RAW_TAG  0

#define MR_RAW_TAG_NIL          MR_FIRST_UNRESERVED_RAW_TAG
#define MR_RAW_TAG_CONS         (MR_FIRST_UNRESERVED_RAW_TAG + 1)

#define MR_RAW_UNIV_TAG         MR_FIRST_UNRESERVED_RAW_TAG

#define MR_TAG_NIL      MR_mktag(MR_RAW_TAG_NIL)
#define MR_TAG_CONS     MR_mktag(MR_RAW_TAG_CONS)

#define MR_UNIV_TAG     MR_mktag(MR_RAW_UNIV_TAG)

#if MR_TAGBITS > 0
  // Cons cells are represented using two words.
  // We use the primary tag to distinguish between empty and non-empty lists.
  #define MR_list_empty()           ((MR_Word) MR_mkword(MR_TAG_NIL,    \
                                        MR_mkbody(0)))
  #define MR_list_is_empty(list)    (MR_tag(list) == MR_TAG_NIL)
  #define MR_list_head(list)        MR_field(MR_TAG_CONS, (list), 0)
  #define MR_list_tail(list)        MR_field(MR_TAG_CONS, (list), 1)
  #define MR_typed_list_cons(ti_head, head, ti_tail, tail)              \
    ((MR_Word) MR_mkword(MR_TAG_CONS,                                   \
        MR_create2((ti_head), (head), (ti_tail), (tail))))
  #define MR_list_empty_msg(alloc_id)   MR_list_empty()
  #define MR_typed_list_cons_msg(ti_head, head, ti_tail, tail, alloc_id)\
    ((MR_Word) MR_mkword(MR_TAG_CONS,                                   \
        MR_create2_msg((ti_head), (head), (ti_tail), (tail),            \
             alloc_id, "list.list/1")))
#else
  #error "MR_TAGBITS must be greater than zero"
#endif

// Since these macros are not defined in term size profiling grades,
// their use in those grades will cause errors from the C compiler.
// This is what we want: no visible change for existing users, and
// no incorrect sizes in term profiling grades caused by the lack of
// type information in these macros.

#ifndef MR_RECORD_TERM_SIZES
  #define MR_list_cons(head, tail)                                      \
    MR_typed_list_cons(                                                 \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, void, 0),        \
        (head),                                                         \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, void, 0),        \
        (tail))
  #define MR_list_cons_msg(head, tail, alloc_id)                        \
    MR_typed_list_cons_msg(                                             \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, void, 0),        \
        (head),                                                         \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, void, 0),        \
        (tail), alloc_id)
#endif

#define MR_univ_list_cons(head, tail)                                    \
    MR_typed_list_cons((MR_TypeInfo) MR_type_ctor_info_for_univ, (head), \
        MR_type_info_for_list_of_univ, (tail))

#define MR_univ_list_cons_msg(head, tail, alloc_id)                     \
    MR_typed_list_cons_msg(                                             \
        (MR_TypeInfo) MR_type_ctor_info_for_univ, (head),               \
        MR_type_info_for_list_of_univ, (tail), alloc_id)

#define MR_int_list_cons(head, tail)                                    \
    MR_typed_list_cons(                                                 \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0),         \
        (head), MR_type_info_for_list_of_int, (tail))

#define MR_int_list_cons_msg(head, tail, alloc_id)                      \
    MR_typed_list_cons_msg(                                             \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0),         \
        (head), MR_type_info_for_list_of_int, (tail), alloc_id)

#define MR_char_list_cons(head, tail)                                   \
    MR_typed_list_cons(                                                 \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0),   \
        (head), MR_type_info_for_list_of_char, (tail))

#define MR_char_list_cons_msg(head, tail, alloc_id)                     \
    MR_typed_list_cons_msg(                                             \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0),   \
        (head), MR_type_info_for_list_of_char, (tail), alloc_id)

#define MR_string_list_cons(head, tail)                                 \
    MR_typed_list_cons(                                                 \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0),      \
        (head), MR_type_info_for_list_of_string, (tail))

#define MR_string_list_cons_msg(head, tail, alloc_id)                   \
    MR_typed_list_cons_msg(                                             \
        (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0),      \
        (head), MR_type_info_for_list_of_string, (tail), alloc_id)

#define MR_type_info_list_cons(head, tail)                              \
    MR_typed_list_cons(MR_type_info_for_type_info, (head),              \
        MR_type_info_for_list_of_type_info, (tail))

#define MR_type_info_list_cons_msg(head, tail, alloc_id)                \
    MR_typed_list_cons_msg(MR_type_info_for_type_info, (head),          \
        MR_type_info_for_list_of_type_info, (tail), alloc_id)

#define MR_pseudo_type_info_list_cons(head, tail)                       \
    MR_typed_list_cons(MR_type_info_for_pseudo_type_info, (head),       \
        MR_type_info_for_list_of_pseudo_type_info, (tail))

#define MR_pseudo_type_info_list_cons_msg(head, tail, alloc_id)         \
    MR_typed_list_cons_msg(MR_type_info_for_pseudo_type_info, (head),   \
        MR_type_info_for_list_of_pseudo_type_info, (tail), alloc_id)

// When we still had .rt (reserve_tag) grades, we used this macro to ensure
// that the values of enum constants do not collide with the reserved tag
// value. However, since do not support .rt grades anymore, such chicanery
// is no longer necessary.
//
// Note that enums have the same size as ints, but not necessarily the same
// size as MR_Words. Types that are defined this way should not be used by
// Mercury code directly; instead a separate type with the correct size should
// be defined.

#define MR_CONVERT_C_ENUM_CONSTANT(x)   (x)

#define MR_DEFINE_MERCURY_ENUM_CONST(x) x

#define MR_GET_ENUM_VALUE(x)            (x)

// We used to define each enumeration constant in the runtime (not in Mercury)
// twice, with the same value: once under its plain name, and once prefixed
// with the string `mercury__private_builtin__'. The qualified version was
// used by the MLDS->C back-end, back when it forcibly either module- or
// type-qualified everything. We do not do that anymore.

#define MR_DEFINE_BUILTIN_ENUM_CONST(x) x

#define MR_INT_EQ(rval, val)    (((MR_Integer) (rval)) == ((MR_Integer) (val)))
#define MR_INT_NE(rval, val)    (((MR_Integer) (rval)) != ((MR_Integer) (val)))
#define MR_INT_LT(rval, val)    (((MR_Integer) (rval)) <  ((MR_Integer) (val)))
#define MR_INT_LE(rval, val)    (((MR_Integer) (rval)) <= ((MR_Integer) (val)))
#define MR_INT_GT(rval, val)    (((MR_Integer) (rval)) >  ((MR_Integer) (val)))
#define MR_INT_GE(rval, val)    (((MR_Integer) (rval)) >= ((MR_Integer) (val)))

#define MR_PTAG_TEST(rval, ptag)                                        \
    (MR_tag(rval) == MR_mktag(ptag))
#define MR_PTAG_TESTR(rval, ptag)                                       \
    (!MR_PTAG_TEST((rval), (ptag)))

#define MR_RTAGS_TEST(rval, ptag, stag)                                 \
    ((MR_tag(rval) == MR_mktag(ptag)) &&                                \
    (MR_const_tfield((ptag), (rval), 0) == (stag)))
#define MR_RTAGS_TESTR(rval, ptag, stag)                                \
    (!MR_RTAGS_TEST((rval), (ptag), (stag)))

#define MR_LTAGS_TEST(rval, ptag, stag)                                 \
    (((MR_Integer) (rval)) == ((MR_Integer) (MR_tbmkword(ptag, stag))))
#define MR_LTAGS_TESTR(rval, ptag, stag)                                \
    (!MR_LTAGS_TEST((rval), (ptag), (stag)))

#endif  // not MERCURY_TAGS_H
