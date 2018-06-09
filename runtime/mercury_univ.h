// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2003-2004 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_univ.h - definitions for manipulating univs.

#ifndef MERCURY_UNIV_H
#define MERCURY_UNIV_H

#include "mercury_regs.h"           // must come first (see mercury_imp.h)
#include "mercury_conf_param.h"     // for MR_RECORD_TERM_SIZES
#include "mercury_tags.h"           // for MR_field
#include "mercury_heap.h"           // for MR_tag_offset_incr_hp_msg
#include "mercury_debug.h"

// `univ' is usually represented as a two word structure.
// The first word contains the address of a type_info for the type.
// The second word contains the data. With MR_RECORD_TERM_SIZES,
// we add an extra field at offset -1 to record the size of the term.

#define MR_UNIV_OFFSET_FOR_TYPEINFO        0
#define MR_UNIV_OFFSET_FOR_DATA            1

#ifdef  MR_RECORD_TERM_SIZES
  #define MR_define_univ_size_slot(univ, typeinfo, value)               \
    do {                                                                \
        MR_define_size_slot(MR_UNIV_TAG, (univ),                        \
            MR_term_size((typeinfo), (value)));                         \
    } while (0)
#else
  #define MR_define_univ_size_slot(univ, typeinfo, value)    ((void) 0)
#endif

#define MR_define_univ_fields(univ, typeinfo, value)                    \
  do {                                                                  \
      MR_define_univ_size_slot((univ), (typeinfo), (value));            \
      MR_field(MR_UNIV_TAG, (univ), MR_UNIV_OFFSET_FOR_TYPEINFO)        \
          = (MR_Word) (typeinfo);                                       \
      MR_field(MR_UNIV_TAG, (univ), MR_UNIV_OFFSET_FOR_DATA)            \
          = (MR_Word) (value);                                          \
  } while (0)

#define MR_unravel_univ(univ, typeinfo, value)                          \
  do {                                                                  \
      (typeinfo) = (MR_TypeInfo) MR_field(MR_UNIV_TAG, (univ),          \
          MR_UNIV_OFFSET_FOR_TYPEINFO);                                 \
      (value) = MR_field(MR_UNIV_TAG, (univ),                           \
          MR_UNIV_OFFSET_FOR_DATA);                                     \
      MR_debug_unravel_univ((univ), (typeinfo), (value));               \
  } while (0)

  // Allocate a univ on the heap
  // XXX We should use MR_tag_offset_incr_hp_msg() here.
#define MR_new_univ_on_hp(univ, typeinfo, value)                        \
  do {                                                                  \
      MR_tag_offset_incr_hp((univ), MR_UNIV_TAG, MR_SIZE_SLOT_SIZE,     \
          MR_SIZE_SLOT_SIZE + 2);                                       \
      MR_define_univ_fields((univ), (typeinfo), (value));               \
      MR_debug_new_univ_on_hp((univ), (typeinfo), (value));             \
  } while (0)

#endif // MERCURY_UNIV_H
