// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the declarations of the types and functions that
// we use to represent and manipulate user-defined event types.
//
// Main author: Zoltan Somogyi.

#ifndef MERCURY_TRACE_EVENT_H
#define MERCURY_TRACE_EVENT_H

#include "mercury_std.h"            // for MR_bool
#include "mercury_trace_term.h"     // for MR_CTerm and MR_FlatTerm

typedef struct MR_EventSet_Struct       *MR_EventSet;
typedef struct MR_EventSpecs_Struct     *MR_EventSpecs;
typedef struct MR_EventSpec_Struct      *MR_EventSpec;
typedef struct MR_EventAttrs_Struct     *MR_EventAttrs;
typedef struct MR_EventAttr_Struct      *MR_EventAttr;
typedef struct MR_EventAttrType_Struct  *MR_EventAttrType;

// The event_set_name field gives the name of an event set.
// The event_set_spec_list field gives a list of the specifications of
// all the events in that event set. The event_set_specs field has the same
// information, only in the form of an array indexable by event number.
// The event_set_num_events field gives the number of different event kinds
// in the event set, which is also the size of the event_set_specs array.

struct MR_EventSet_Struct {
    const char          *MR_event_set_name;
    MR_EventSpecs       MR_event_set_spec_list;
    MR_EventSpec        *MR_event_set_specs;
    MR_Unsigned         MR_event_set_num_events;
};

struct MR_EventSpecs_Struct {
    MR_EventSpec        MR_events_head;
    MR_EventSpecs       MR_events_tail;
};

struct MR_EventAttrs_Struct {
    MR_EventAttr        MR_attrs_head;
    MR_EventAttrs       MR_attrs_tail;
};

struct MR_EventSpec_Struct {
    unsigned            MR_event_num;
    int                 MR_event_linenumber;
    const char          *MR_event_name;
    MR_EventAttrs       MR_event_attributes;
};

struct MR_EventAttr_Struct {
    const char          *MR_attr_name;
    int                 MR_attr_linenumber;
    MR_EventAttrType    MR_attr_type;
};

typedef enum {
    MR_EVENT_ATTR_ORDINARY,
    MR_EVENT_ATTR_PURE_FUNCTION,
    MR_EVENT_ATTR_IMPURE_FUNCTION,
    MR_EVENT_ATTR_SYNTHESIZED
} MR_EventAttrKind;

struct MR_EventAttrType_Struct {
    MR_EventAttrKind    MR_type_kind;
    MR_CTerm            MR_type_term;       // if ORDINARY or SYNTHESIZED
    MR_FlatTerm         MR_type_synth_call; // if SYNTHESIZED
};

// The flex-generated scanner uses this function to read its input directly
// from the consensus event set specification. It reads up to buf_size bytes
// into buf, and returns the number of bytes read.

extern  int             MR_event_get_input(char *buf, int buf_size);

// Read the specification of a set of event types from the string given by
// event_set, which should be the contents of the event set specification file
// named filename. If the operation succeeded, return the result; otherwise,
// return NULL.

extern  MR_EventSet     MR_read_event_set(const char *filename,
                            const char *event_set);

// Print out the set of event specifications given by event_set to the given
// stream as a single Mercury term.

extern  void            MR_print_event_set(FILE *fp, MR_EventSet event_set);

#endif  // not MERCURY_TRACE_EVENT_H
