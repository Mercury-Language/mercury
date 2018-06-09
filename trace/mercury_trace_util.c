// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000-2002,2004-2007,2012 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains utility functions that can be used by any or all
// of the various kinds of Mercury debuggers.
//
// Author: zs.

#include "mercury_imp.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_util.h"
#include "mercury_file.h"

#include "string.mh"

#include <ctype.h>

void
MR_c_file_to_mercury_file(FILE *c_file, MercuryFile *mercury_file)
{
    MR_mercuryfile_init(c_file, 1, mercury_file);
}

MR_bool
MR_trace_is_natural_number(const char *word, MR_Unsigned *value)
{
    if (word != NULL && MR_isdigit(*word)) {
        *value = *word - '0';
        word++;
        while (MR_isdigit(*word)) {
            *value = (*value * 10) + *word - '0';
            word++;
        }

        if (*word == '\0') {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

MR_bool
MR_trace_is_natural_number_pair(const char *word,
    MR_Unsigned *value1, MR_Unsigned *value2)
{
    if (word != NULL && MR_isdigit(*word)) {
        *value1 = *word - '0';
        word++;
        while (MR_isdigit(*word)) {
            *value1 = (*value1 * 10) + *word - '0';
            word++;
        }

        if (*word == '-') {
            word++;

            if (MR_isdigit(*word)) {
                *value2 = *word - '0';
                word++;
                while (MR_isdigit(*word)) {
                    *value2 = (*value2 * 10) + *word - '0';
                    word++;
                }

                if (*word == '\0') {
                    return MR_TRUE;
                }
            }
        }
    }

    return MR_FALSE;
}

MR_bool
MR_trace_is_nonneg_int(const char *word, int *value)
{
    if (word != NULL && MR_isdigit(*word)) {
        *value = *word - '0';
        word++;
        while (MR_isdigit(*word)) {
            *value = (*value * 10) + *word - '0';
            word++;
        }

        if (*word == '\0') {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

MR_bool
MR_trace_is_integer(const char *word, MR_Integer *value)
{
    int sign;

    if (*word == '-') {
        sign = -1;
        word++;
    } else {
        sign = 1;
    }

    if (MR_isdigit(*word)) {
        *value = *word - '0';
        word++;
        while (MR_isdigit(*word)) {
            *value = (*value * 10) + *word - '0';
            word++;
        }

        if (*word == '\0') {
            *value = *value * sign;
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

MR_bool
MR_trace_is_float(const char *word, MR_Float *value)
{
    return ML_string_to_float((MR_String) word, value);
}

void
MR_print_stack_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
    fprintf(fp, "sp = ");
    MR_print_detstackptr(fp, MR_saved_sp(saved_regs));
    fprintf(fp, "\ncurfr = ");
    MR_print_nondetstackptr(fp, MR_saved_curfr(saved_regs));
    fprintf(fp, "\nmaxfr = ");
    MR_print_nondetstackptr(fp, MR_saved_maxfr(saved_regs));
    fprintf(fp, "\n");
#endif
}

void
MR_print_heap_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_CONSERVATIVE_GC
    fprintf(fp, "hp = ");
    MR_print_heapptr(fp, MR_saved_hp(saved_regs));
    fprintf(fp, "\nsol_hp = ");
    MR_print_heapptr(fp, MR_saved_sol_hp(saved_regs));
    fprintf(fp, "\nmin_hp_rec = ");
    MR_print_heapptr(fp, MR_saved_min_hp_rec(saved_regs));
    fprintf(fp, "\nglobal_hp = ");
    MR_print_heapptr(fp, MR_saved_global_hp(saved_regs));
    fprintf(fp, "\n");
#endif
}

void
MR_print_tabling_regs(FILE *fp, MR_Word *saved_regs)
{
#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    fprintf(fp, "gen_next = %ld\n", (long) MR_saved_gen_next(saved_regs));
    fprintf(fp, "cut_next = %ld\n", (long) MR_saved_cut_next(saved_regs));
#endif
}

void
MR_print_succip_reg(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
    fprintf(fp, "succip = ");
    MR_print_label(fp, MR_saved_succip(saved_regs));
    fprintf(fp, "\n");
#endif
}

void
MR_print_r_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
    fprintf(fp, "r1 = %ld (%lx)\n",
        (long) MR_saved_reg_value(saved_regs, 1),
        (long) MR_saved_reg_value(saved_regs, 1));
    fprintf(fp, "r2 = %ld (%lx)\n",
        (long) MR_saved_reg_value(saved_regs, 2),
        (long) MR_saved_reg_value(saved_regs, 2));
    fprintf(fp, "r3 = %ld (%lx)\n",
        (long) MR_saved_reg_value(saved_regs, 3),
        (long) MR_saved_reg_value(saved_regs, 3));
    fprintf(fp, "r4 = %ld (%lx)\n",
        (long) MR_saved_reg_value(saved_regs, 4),
        (long) MR_saved_reg_value(saved_regs, 4));
    fprintf(fp, "r5 = %ld (%lx)\n",
        (long) MR_saved_reg_value(saved_regs, 5),
        (long) MR_saved_reg_value(saved_regs, 5));
#endif
}

void
MR_print_debug_vars(FILE *fp, MR_EventInfo *event_info)
{
#ifndef MR_HIGHLEVEL_CODE
    fprintf(fp, "from event info:\n");
    fprintf(fp, "call event %ld, call seq %ld, depth %ld\n",
        (long) event_info->MR_event_number,
        (long) event_info->MR_call_seqno,
        (long) event_info->MR_call_depth);
    fprintf(fp, "from global vars:\n");
    fprintf(fp, "call event %ld, call seq %ld, depth %ld\n",
        (long) MR_trace_event_number,
        (long) MR_trace_call_seqno,
        (long) MR_trace_call_depth);
#endif
}

MR_bool
MR_trace_proc_layout_is_builtin_catch(const MR_ProcLayout *layout)
{
    const MR_UserProcId *user;

    if (MR_PROC_LAYOUT_HAS_PROC_ID(layout)) {
        if (! MR_PROC_LAYOUT_IS_UCI(layout)) {
            user = &layout->MR_sle_user;
            if (MR_streq(user->MR_user_decl_module, "exception") &&
                MR_streq(user->MR_user_name, "builtin_catch") &&
                (user->MR_user_arity == 3))
            {
                return MR_TRUE;
            }
        }
    }
    return MR_FALSE;
}

MR_bool
MR_find_clique_entry_mdb(MR_EventInfo *event_info,
    MR_SelectedStackFrame which_frame, MR_Level *selected_level_ptr)
{
    const MR_LabelLayout    *layout;
    MR_Word                 *saved_regs;
    int                     clique_entry_level;
    int                     clique_parent_level;
    const char              *problem;

    layout = event_info->MR_event_sll;
    saved_regs = event_info->MR_saved_regs;

    problem = MR_find_clique_entry(layout,
        MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
        &clique_entry_level, &clique_parent_level);

    if (problem != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        return MR_TRUE;
    }

    if (which_frame == MR_CLIQUE_ENTRY_PARENT_FRAME) {
        if (clique_parent_level < 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: All the frames on the stack"
                "are recursive with the current procedure.\n");
            return MR_TRUE;
        }

        *selected_level_ptr = clique_parent_level;
    } else {
        *selected_level_ptr = clique_entry_level;
    }

    return MR_FALSE;
}

void
MR_trace_call_system_display_error_on_failure(FILE *err_stream, char *command)
{
    int     system_rv;

    if (system(NULL)) {
        system_rv = system(command);
        if (system_rv != 0) {
            fprintf(err_stream,
                "mdb: the shell command returned a non-zero exit code or was"
                " terminated abnormally.\n");
        }
    } else {
        fprintf(err_stream, "mdb: no shell found.\n");
    }
}
