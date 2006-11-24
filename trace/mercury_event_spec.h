/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the declarations of the types and functions that
** we use to represent and manipulate user-defined event types.
**
** Main author: Zoltan Somogyi.
*/

#ifndef MERCURY_TRACE_EVENT_H
#define MERCURY_TRACE_EVENT_H

#include "mercury_std.h"            /* for MR_bool */
#include "mercury_trace_term.h"     /* for MR_CTerm and MR_FlatTerm */

typedef struct MR_EventSpecs_Struct     *MR_EventSpecs;
typedef struct MR_EventSpec_Struct      *MR_EventSpec;
typedef struct MR_EventAttrs_Struct     *MR_EventAttrs;
typedef struct MR_EventAttr_Struct      *MR_EventAttr;
typedef struct MR_EventAttrType_Struct  *MR_EventAttrType;

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
    int                 MR_event_lineno;
    const char          *MR_event_name;
    MR_EventAttrs       MR_event_attributes;
};

struct MR_EventAttr_Struct {
    const char          *MR_attr_name;
    MR_EventAttrType    MR_attr_type;
};

typedef enum {
    MR_EVENT_ATTR_ORDINARY,
    MR_EVENT_ATTR_FUNCTION,
    MR_EVENT_ATTR_SYNTHESIZED
} MR_EventAttrKind;

struct MR_EventAttrType_Struct {
    MR_EventAttrKind    MR_type_kind;
    MR_CTerm            MR_type_term;       /* if ORDINARY or SYNTHESIZED */
    MR_FlatTerm         MR_type_synth_call; /* if SYNTHESIZED */
};

/*
** Read the specification of a set of event types from the given string, which
** should be the contents of the event set specification file. Record the
** result in the module's private data structures. Return true if the operation
** succeeded; otherwise, return false.
*/

extern  MR_bool         MR_read_event_specs(const char *event_spec);

/*
** The flex-generated scanner uses this function to read its input directly
** from the consensus event set specification. It reads up to buf_size bytes
** into buf, and returns the number of bytes read.
*/

extern  int             MR_event_get_input(char *buf, int buf_size);

/*
** Print out the set of event specifications recorded in the module's private
** data structures to the given stream as a single Mercury term.
*/

extern  void            MR_print_event_specs(FILE *fp);


/*
** The table of event specifications, with counters saying which is the next
** free slot and how many slots are allocated.
*/

extern  MR_EventSpec    *MR_event_specs;
extern  int             MR_event_spec_next;
extern  int             MR_event_spec_max;

#endif  /* not MERCURY_TRACE_EVENT_H */
