// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2005-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_imp.h"
#include "mercury_array_macros.h"       // MR_bsearch etc
#include "mercury_trace_base.h"         // MR_TRACE_CALL_MERCURY
#include "type_desc.mh"                 // ML_get_type_info_for_type_info
#include "mercury_trace_hold_vars.h"

typedef struct {
    const char  *MR_held_name;
    MR_TypeInfo MR_held_type;
    MR_Word     MR_held_value;
} MR_HeldVar;

// The initial size of the held vars table.
#define MR_INIT_HELD_VARS   10

static MR_HeldVar   *MR_held_vars;
static int          MR_held_var_max = 0;
static int          MR_held_var_next = 0;

MR_bool
MR_add_hold_var(const char *name, const MR_TypeInfo typeinfo, MR_Word value)
{
    MR_TypeInfo old_type;
    MR_Word     old_value;
    int         slot;
    MR_Word     typeinfo_type_word;

    if (MR_lookup_hold_var(name, &old_type, &old_value)) {
        return MR_FALSE;
    }

    MR_TRACE_CALL_MERCURY(
        typeinfo_type_word = ML_get_type_info_for_type_info();
    );

    MR_ensure_room_for_next(MR_held_var, MR_HeldVar, MR_INIT_HELD_VARS);
    MR_prepare_insert_into_sorted(MR_held_vars, MR_held_var_next, slot,
        strcmp(MR_held_vars[slot].MR_held_name, name));
    MR_held_vars[slot].MR_held_name = strdup(name);
    MR_held_vars[slot].MR_held_type = (MR_TypeInfo) MR_make_permanent(
        (MR_Word) typeinfo, (MR_TypeInfo) typeinfo_type_word);
    MR_held_vars[slot].MR_held_value = MR_make_permanent(value, typeinfo);

    return MR_TRUE;
}

MR_bool
MR_lookup_hold_var(const char *name, MR_TypeInfo *typeinfo,
    MR_Word *value)
{
    MR_bool found;
    int     slot;

    MR_bsearch(MR_held_var_next, slot, found,
        strcmp(MR_held_vars[slot].MR_held_name, name));
    if (found) {
        *typeinfo = MR_held_vars[slot].MR_held_type;
        *value = MR_held_vars[slot].MR_held_value;
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
}

void
MR_trace_list_held_vars(FILE *fp)
{
    int i;

    for (i = 0; i < MR_held_var_next; i++) {
        fprintf(fp, "$%s\n", MR_held_vars[i].MR_held_name);
    }
}
