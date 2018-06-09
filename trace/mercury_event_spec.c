// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2005-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the functions that we use to manipulate user-defined
// event types.
//
// Main author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_layout_util.h"
#include "mercury_runtime_util.h"
#include "mercury_trace_base.h"

#include "mercury_event_spec.h"
#include "mercury_event_spec_missing.h"
#include "mercury_event_scanner.h"
#include "mercury_event_parser.h"

#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////

// The table of event specifications, with counters saying which is the next
// free slot and how many slots are allocated.

static const char   *MR_event_spec_chars;
static unsigned     MR_event_spec_char_next;
static unsigned     MR_event_spec_char_max;

static  int         MR_compare_event_specs(const void *, const void *);

static  void        MR_print_attr_type_term(FILE *fp, MR_CTerm term);
static  void        MR_print_attr_synth_call(FILE *fp, MR_FlatTerm call);

////////////////////////////////////////////////////////////////////////////

int
MR_event_get_input(char *buf, int buf_size)
{
    unsigned    num_copied;

    if (MR_event_spec_char_next + buf_size < MR_event_spec_char_max) {
        num_copied = buf_size;
    } else {
        num_copied = MR_event_spec_char_max - MR_event_spec_char_next;
    }

    MR_memcpy(buf, MR_event_spec_chars + MR_event_spec_char_next, num_copied);
    MR_event_spec_char_next += num_copied;

#if 0
    {
        int         i;

        fprintf(stderr, "<<<");
        for (i = 0; i < num_copied; i++) {
            fprintf(stderr, "%c", buf[i]);
        }
        fprintf(stderr, ">>>");
    }
#endif

    return num_copied;
}

MR_EventSet
MR_read_event_set(const char *filename, const char *input_data)
{
    MR_EventSet     event_set;
    MR_EventSpecs   cur;
    MR_Unsigned     num_events;
    MR_Unsigned     i;

    // Set these globals up for calls to MR_event_get_input by the scanner.
    // The -1 is because MR_event_spec_char_max should contain the index
    // of the last byte in the string, not the index of the NULL byte.

    MR_event_spec_chars = input_data;
    MR_event_spec_char_max = strlen(input_data) - 1;
    MR_event_spec_char_next = 0;

    mercury_event_filename = filename;

    if (mercury_event_parse() != 0) {
        return NULL;
    }

    event_set = MR_NEW(struct MR_EventSet_Struct);
    event_set->MR_event_set_name = mercury_event_parsetree->MR_event_set_name;
    event_set->MR_event_set_spec_list =
        mercury_event_parsetree->MR_event_set_spec_list;

    num_events = 0;
    for (cur = event_set->MR_event_set_spec_list;
        cur != NULL; cur = cur->MR_events_tail)
    {
        num_events++;
    }

    event_set->MR_event_set_num_events = num_events;
    event_set->MR_event_set_specs = MR_NEW_ARRAY(MR_EventSpec, num_events);

    i = 0;
    for (cur = event_set->MR_event_set_spec_list;
        cur != NULL; cur = cur->MR_events_tail)
    {
        event_set->MR_event_set_specs[i] = cur->MR_events_head;
        i++;
    }

    // We keep the events in the original order, which will be the order
    // in which we assign them event numbers; the code here is in case
    // in future we want to order them by name.

#if 0
    qsort(MR_event_specs, MR_event_spec_max, sizeof(MR_EventSpec),
        MR_compare_event_specs);
#endif

    return event_set;
}

static int
MR_compare_event_specs(const void *a, const void *b)
{
    MR_EventSpec    *a_event;
    MR_EventSpec    *b_event;

    a_event = (MR_EventSpec *) a;
    b_event = (MR_EventSpec *) b;

    return strcmp((*a_event)->MR_event_name, (*b_event)->MR_event_name);
}

void
MR_print_event_set(FILE *fp, MR_EventSet event_set)
{
    int             event_num;
    MR_EventSpec    event;
    MR_EventSpecs   events;
    MR_EventAttr    attr;
    MR_EventAttrs   attrs;

    fprintf(fp, "event_set_spec(\"%s\",\n", event_set->MR_event_set_name);
    fprintf(fp, "[\n");

    for (event_num = 0, events = event_set->MR_event_set_spec_list;
        events != NULL;
        event_num++, events = events->MR_events_tail)
    {
        event = events->MR_events_head;
        fprintf(fp, "event_spec_term(\"%s\", %d, %d, [\n",
            event->MR_event_name, event->MR_event_num,
            event->MR_event_linenumber);

        for (attrs = event->MR_event_attributes; attrs != NULL;
            attrs = attrs->MR_attrs_tail)
        {
            attr = attrs->MR_attrs_head;

            fprintf(fp, "    event_attr_term(\"%s\", %d, ",
                attr->MR_attr_name, attr->MR_attr_linenumber);
            switch (attr->MR_attr_type->MR_type_kind) {
                case MR_EVENT_ATTR_ORDINARY:
                    fprintf(fp, "event_attr_type_ordinary(");
                    MR_print_attr_type_term(fp,
                        attr->MR_attr_type->MR_type_term);
                    fprintf(fp, ")");
                    break;

                case MR_EVENT_ATTR_PURE_FUNCTION:
                    fprintf(fp,
                        "event_attr_type_function(event_attr_pure_function)");
                    break;

                case MR_EVENT_ATTR_IMPURE_FUNCTION:
                    fprintf(fp,
                        "event_attr_type_function(event_attr_impure_function)");
                    break;

                case MR_EVENT_ATTR_SYNTHESIZED:
                    fprintf(fp, "event_attr_type_synthesized(");
                    MR_print_attr_type_term(fp,
                        attr->MR_attr_type->MR_type_term);
                    fprintf(fp, ",\n        ");
                    MR_print_attr_synth_call(fp,
                        attr->MR_attr_type->MR_type_synth_call);
                    fprintf(fp, ")");
                    break;
            }

            if (attrs->MR_attrs_tail == NULL) {
                fprintf(fp, ")\n");
            } else {
                fprintf(fp, "),\n");
            }
        }

        if (events->MR_events_tail != NULL) {
            fprintf(fp, "]),\n");
        } else {
            fprintf(fp, "])\n");
        }
    }

    fprintf(fp, "]).\n");
}

static void
MR_print_attr_type_term(FILE *fp, MR_CTerm term)
{
    MR_CArgs args;

    fprintf(fp, "event_attr_type_term(\"%s\", [", term->MR_term_functor);

    for (args = term->MR_term_args; args != NULL; args = args->MR_args_tail) {
        MR_print_attr_type_term(fp, args->MR_args_head);
        if (args->MR_args_tail != NULL) {
            fprintf(fp, ",");
        }
    }

    fprintf(fp, "])");
}

static void
MR_print_attr_synth_call(FILE *fp, MR_FlatTerm call)
{
    MR_FlatArgs args;

    fprintf(fp, "event_attr_synth_call_term(\"%s\", [",
        call->MR_flat_term_functor);

    for (args = call->MR_flat_term_args; args != NULL;
        args = args->MR_flat_args_tail)
    {
        if (args->MR_flat_args_tail == NULL) {
            fprintf(fp, "\"%s\"", args->MR_flat_args_head);
        } else {
            fprintf(fp, "\"%s\",", args->MR_flat_args_head);
        }
    }

    fprintf(fp, "])");
}
