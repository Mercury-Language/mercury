/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2000-2002, 2004-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains utility functions that can be used by any or all
** of the various kinds of Mercury debuggers.
**
** Author: zs.
*/

#include "mercury_imp.h"
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
MR_trace_is_natural_number(const char *word, int *value)
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
MR_trace_is_unsigned(const char *word, MR_Unsigned *value)
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
    MR_print_nondstackptr(fp, MR_saved_curfr(saved_regs));
    fprintf(fp, "\nmaxfr = ");
    MR_print_nondstackptr(fp, MR_saved_maxfr(saved_regs));
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
MR_print_debug_vars(FILE *fp, MR_Event_Info *event_info)
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
MR_trace_proc_layout_is_builtin_catch(const MR_Proc_Layout *layout)
{
    const MR_User_Proc_Id   *user;

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
