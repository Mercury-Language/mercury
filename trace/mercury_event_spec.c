/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2002, 2005-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the functions that we use to manipulate user-defined
** event types.
**
** Main author: Zoltan Somogyi.
*/

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

/**************************************************************************/

/*
** The table of event specifications, with counters saying which is the next
** free slot and how many slots are allocated.
*/

MR_EventSpec        *MR_event_specs = NULL;
int                 MR_event_spec_next = 0;
int                 MR_event_spec_max = 0;

static const char   *MR_event_spec_chars;
static unsigned     MR_event_spec_char_next;
static unsigned     MR_event_spec_char_max;

static  int         MR_compare_event_specs(const void *, const void *);

static  void        MR_print_attr_type_term(FILE *fp, MR_CTerm term);
static  void        MR_print_attr_synth_call(FILE *fp, MR_FlatTerm call);

/**************************************************************************/

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

MR_bool
MR_read_event_specs(const char *input_data)
{
    MR_EventSpecs cur;

    /*
    ** Set these globals up for calls to MR_event_get_input by the scanner.
    ** The -1 is because MR_event_spec_char_max should contain the index
    ** of the last byte in the string, not the index of the NULL byte.
    */
    MR_event_spec_chars = input_data;
    MR_event_spec_char_max = strlen(input_data) - 1;
    MR_event_spec_char_next = 0;

    if (mercury_event_parse() != 0) {
        return MR_FALSE;
    }

    MR_event_spec_max = 0;
    for (cur = mercury_event_parsetree; cur != NULL; cur = cur->MR_events_tail)
    {
        MR_event_spec_max++;
    }

    MR_event_specs = MR_NEW_ARRAY(MR_EventSpec, MR_event_spec_max);

    MR_event_spec_next = 0;
    for (cur = mercury_event_parsetree; cur != NULL; cur = cur->MR_events_tail)
    {
        MR_event_specs[MR_event_spec_next] = cur->MR_events_head;
        MR_event_spec_next++;
    }

    /*
    ** We keep the events in the original order, which will be the order
    ** in which we assign them event numbers; the code here is in case
    ** in future we want to order them by name.
    */

#if 0
    qsort(MR_event_specs, MR_event_spec_max, sizeof(MR_EventSpec),
        MR_compare_event_specs);
#endif

    return MR_TRUE;
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
MR_print_event_specs(FILE *fp)
{
    int             event_num;
    MR_EventSpec    event;
    MR_EventAttr    attr;
    MR_EventAttrs   attrs;

    fprintf(fp, "[\n");

    for (event_num = 0; event_num < MR_event_spec_max; event_num++) {
        event = MR_event_specs[event_num];
        fprintf(fp, "event_spec_term(\"%s\", %d, %d, [\n",
            event->MR_event_name, event->MR_event_num, event->MR_event_lineno);

        for (attrs = event->MR_event_attributes; attrs != NULL;
            attrs = attrs->MR_attrs_tail)
        {
            attr = attrs->MR_attrs_head;

            fprintf(fp, "    event_attr_term(\"%s\", ", attr->MR_attr_name);
            switch (attr->MR_attr_type->MR_type_kind) {
                case MR_EVENT_ATTR_ORDINARY:
                    fprintf(fp, "event_attr_type_ordinary(");
                    MR_print_attr_type_term(fp,
                        attr->MR_attr_type->MR_type_term);
                    fprintf(fp, ")");
                    break;

                case MR_EVENT_ATTR_FUNCTION:
                    fprintf(fp, "event_attr_type_function");
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

        if (event_num == MR_event_spec_max - 1) {
            fprintf(fp, "])\n");
        } else {
            fprintf(fp, "]),\n");
        }
    }

    fprintf(fp, "].\n");
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

    fprintf(fp, "attr_synth_call(\"%s\", [", call->MR_flat_term_functor);

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
