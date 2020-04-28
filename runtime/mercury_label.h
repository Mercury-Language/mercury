// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1994-1998, 2000-2002, 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_label.h defines the interface to the label table, which is a pair of
// hash tables, one mapping from procedure names and the other from
// addresses to label information.
// The label information includes the name, address of the code, and
// layout information for that label.

#ifndef MERCURY_LABEL_H
#define MERCURY_LABEL_H

// We don't need mercury_conf_param.h itself, but
// - mercury_types.h includes it, and
// - if included *after* mercury_conf_param.h, MR_LOW_TAG_BITS get redefined
//   if MR_PREGENERATED_DIST is defined, which results in the failure of
//   bootcheck's namespace cleanliness test.

#include "mercury_conf.h"
#include "mercury_conf_param.h"     // for MR_NEED_ENTRY_LABEL_ARRAY etc
#include "mercury_types.h"          // for MR_Code, MR_ProcLayout etc
#include "mercury_dlist.h"          // for MR_Dlist

// This struct records information about entry labels. Elements in the
// entry label array are of this type. The table is sorted on address,
// to allow the garbage collector to locate the entry label of the procedure
// to which a entry label belongs by a variant of binary search.
//
// The name field is needed only for low-level debugging.

typedef struct s_entry {
    const MR_Code           *MR_entry_addr;
    const MR_ProcLayout     *MR_entry_layout;
    const char              *MR_entry_name;
} MR_Entry;

// This struct records information about internal (non-entry) labels.
// The internal label table is organized as a hash table, with the address
// being the key.
//
// The name field is needed only for low-level debugging.

typedef struct s_internal {
    const MR_Code           *MR_internal_addr;
    const MR_LabelLayout    *MR_internal_layout;
    const char              *MR_internal_name;
} MR_Internal;

extern void         MR_do_init_label_tables(void);

extern void         MR_do_insert_entry_label(const char *name,
                        MR_Code *addr, const MR_ProcLayout *entry_layout);

#ifdef  MR_NEED_ENTRY_LABEL_INFO
  #define MR_insert_entry_label(n, a, l)                                \
      MR_do_insert_entry_label((n), (a), (l))
#else
  #define MR_insert_entry_label(n, a, l)    // nothing
#endif  // not MR_NEED_ENTRY_LABEL_INFO

extern MR_Entry     *MR_prev_entry_by_addr(const MR_Code *addr);

extern  void        MR_insert_internal_label(const char *name,
                        MR_Code *addr, const MR_LabelLayout *label_layout);
extern  MR_Internal *MR_lookup_internal_by_addr(const MR_Code *addr);
extern  void        MR_process_all_internal_labels(void f(const void *));

extern  const char  *MR_lookup_entry_or_internal(const MR_Code *addr);

#endif // not MERCURY_LABEL_H
