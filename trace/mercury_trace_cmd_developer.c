// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2008, 2011 The University of Melbourne.
// Copyright (C) 2013-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "developer" category.
//
// The structure of these files is:
//
// - all the #includes
// - local macros and declarations of local static functions
// - one function for each command in the category
// - any auxiliary functions
// - any command argument strings
// - option processing functions.

#include "mercury_std.h"
#include "mercury_getopt.h"
#include "mercury_types.h"
#include "mercury_tabling.h"
#include "mercury_trace_base.h"
#include "mercury_regs.h"
#include "mercury_runtime_util.h"

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_developer.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"

#include <stdio.h>

////////////////////////////////////////////////////////////////////////////

static  void        MR_trace_cmd_nondet_stack_2(MR_EventInfo *event_info,
                        MR_bool detailed, MR_FrameLimit frame_limit,
                        MR_SpecLineLimit line_limit);

static  const MR_ProcLayout
                    *MR_find_single_matching_proc(MR_ProcSpec *spec,
                        MR_bool verbose);

// The following data structures describe the information we have about the
// input arguments of tabled procedures. We use them to decode the call tables
// of such procedures.
//
// We use one MR_CallTableArg structure for each input argument.
//
// The step field specifies what data structure the tabling system uses to
// implement the trie nodes at the level of the call table corresponding to
// the relevant argument. At the moment, we support only four values of this
// field, MR_TABLE_STEP_INT, MR_TABLE_STEP_FLOAT, MR_TABLE_STEP_STRING and
// MR_TABLE_STEP_PROMISE_IMPLIED. The first three of these implicitly select
// the corresponding alternative in the arg_values union; the last one
// indicates the absence of a step.
//
// The start_node field specifies the start node of the relevant trie. For the
// first input argument, this will be the tabling pointer variable for the
// given procedure. For later input arguments, it will be the trie node you
// reach after following the current values of the previous arguments through
// the call table.
//
// The MR_{Int,Float,String}TableArgValues structs have the same fields and
// the same meanings, differing only in the types of the values they store.
// Each struct is used for one of two things.
//
// 1. To describe a value supplied by the user on the mdb command line.
//    In this case, the only field that matters is the cur_value field.
//
// 2. To describe the set of values you can find in a trie node, the one given
//    by the start_node field, and to specify which is the current one.
//    In this case, all the fields matter.
//
// The code that manipulates these structures distinguishes between the two
// uses based on argument number.
//
// The values array is managed with the macros in mercury_array_macros.h,
// so its size is given by the value_next field. The cur_index field gives the
// index of the current value, while the cur_value field gives the current
// value itself. (The contents of the cur_value field can be deduced from the
// contents of the other fields with use 2, but not with use 1.)
//
// The valid field in the MR_CallTableArg structure gives the validity
// of the values subfield of its arg_values field; if it is false, then the
// array is logically considered empty.

typedef struct {
    MR_Integer                  *MR_ctai_values;
    int                         MR_ctai_value_next;
    int                         MR_ctai_cur_index;
    MR_Integer                  MR_ctai_cur_value;
} MR_IntTableArgValues;

typedef struct {
    MR_Float                    *MR_ctaf_values;
    int                         MR_ctaf_value_next;
    int                         MR_ctaf_cur_index;
    MR_Float                    MR_ctaf_cur_value;
} MR_FloatTableArgValues;

typedef struct {
    MR_ConstString              *MR_ctas_values;
    int                         MR_ctas_value_next;
    int                         MR_ctas_cur_index;
    MR_ConstString              MR_ctas_cur_value;
} MR_StringTableArgValues;

typedef union {
    MR_IntTableArgValues        MR_cta_values_int;
    MR_FloatTableArgValues      MR_cta_values_float;
    MR_StringTableArgValues     MR_cta_values_string;
} MR_TableArgValues;

typedef struct {
    MR_TableTrieStep            MR_cta_step;
    int                         MR_cta_unfiltered_arg_num;
    MR_TrieNode                 MR_cta_start_node;
    MR_bool                     MR_cta_valid;
    MR_TableArgValues           MR_cta_arg_values;
} MR_CallTableArg;

#define MR_cta_int_values       MR_cta_arg_values.MR_cta_values_int.\
                                    MR_ctai_values
#define MR_cta_int_value_next   MR_cta_arg_values.MR_cta_values_int.\
                                    MR_ctai_value_next
#define MR_cta_int_cur_index    MR_cta_arg_values.MR_cta_values_int.\
                                    MR_ctai_cur_index
#define MR_cta_int_cur_value    MR_cta_arg_values.MR_cta_values_int.\
                                    MR_ctai_cur_value

#define MR_cta_float_values     MR_cta_arg_values.MR_cta_values_float.\
                                    MR_ctaf_values
#define MR_cta_float_value_next MR_cta_arg_values.MR_cta_values_float.\
                                    MR_ctaf_value_next
#define MR_cta_float_cur_index  MR_cta_arg_values.MR_cta_values_float.\
                                    MR_ctaf_cur_index
#define MR_cta_float_cur_value  MR_cta_arg_values.MR_cta_values_float.\
                                    MR_ctaf_cur_value

#define MR_cta_string_values    MR_cta_arg_values.MR_cta_values_string.\
                                    MR_ctas_values
#define MR_cta_string_value_next MR_cta_arg_values.MR_cta_values_string.\
                                    MR_ctas_value_next
#define MR_cta_string_cur_index MR_cta_arg_values.MR_cta_values_string.\
                                    MR_ctas_cur_index
#define MR_cta_string_cur_value MR_cta_arg_values.MR_cta_values_string.\
                                    MR_ctas_cur_value

// These functions fill in the data structure describing one input argument
// of a tabled procedure with a constant value given on the mdb command line.
// They return true if they succeed, and false if they fail (e.g. because the
// string given on the mdb command line does not describe a value of the
// required type).

static  MR_bool     MR_trace_fill_in_int_table_arg_slot(
                        MR_TrieNode *table_cur_ptr,
                        int arg_num, MR_ConstString given_arg,
                        MR_CallTableArg *call_table_arg_ptr);
static  MR_bool     MR_trace_fill_in_float_table_arg_slot(
                        MR_TrieNode *table_cur_ptr,
                        int arg_num, MR_ConstString given_arg,
                        MR_CallTableArg *call_table_arg_ptr);
static  MR_bool     MR_trace_fill_in_string_table_arg_slot(
                        MR_TrieNode *table_cur_ptr,
                        int arg_num, MR_ConstString given_arg,
                        MR_CallTableArg *call_table_arg_ptr);

// These functions fill in the data structure describing one input argument
// of a tabled procedure with the next value taken from the given trie node.
// They return true if there are no more values in the trie node, and false
// otherwise.

static  MR_bool     MR_update_int_table_arg_slot(MR_TrieNode *table_cur_ptr,
                        MR_CallTableArg *call_table_arg_ptr);
static  MR_bool     MR_update_float_table_arg_slot(MR_TrieNode *table_cur_ptr,
                        MR_CallTableArg *call_table_arg_ptr);
static  MR_bool     MR_update_string_table_arg_slot(MR_TrieNode *table_cur_ptr,
                        MR_CallTableArg *call_table_arg_ptr);

// Prints the given subgoal of the given procedure to MR_mdb_out.
static  void        MR_trace_cmd_table_print_tip(const MR_ProcLayout *proc,
                        int filtered_num_inputs,
                        MR_CallTableArg *call_table_args, MR_TrieNode table);

// Prints the given subgoal of the given procedure to MR_mdb_out.
static  void        MR_trace_print_subgoal(const MR_ProcLayout *proc,
                        MR_Subgoal *subgoal);
static  void        MR_trace_print_subgoal_debug(const MR_ProcLayout *proc,
                        MR_SubgoalDebug *subgoal_debug);

// Prints the given generator of the given procedure to MR_mdb_out.
static  void        MR_trace_print_generator(const MR_ProcLayout *proc,
                        MR_Generator *generator);
static  void        MR_trace_print_generator_debug(const MR_ProcLayout *proc,
                        MR_GenDebug *generator_debug);

// Prints the given consumer of the given procedure to MR_mdb_out.
static  void        MR_trace_print_consumer(const MR_ProcLayout *proc,
                        MR_Consumer *consumer);
static  void        MR_trace_print_consumer_debug(const MR_ProcLayout *proc,
                        MR_ConsumerDebug *consumer_debug);

// Prints the requested information inside the given MR_TypeCtorInfo.
static  void        MR_print_type_ctor_info(FILE *fp,
                        MR_TypeCtorInfo type_ctor_info,
                        MR_bool print_rep, MR_bool print_functors);

// Prints the requested information inside the given MR_TypeClassDeclInfo.
static  void        MR_print_class_decl_info(FILE *fp,
                        MR_TypeClassDeclInfo *type_class_decl_info,
                        MR_bool print_methods, MR_bool print_instances);

// Print the given pseudo-typeinfo.
static  void        MR_print_pseudo_type_info(FILE *fp,
                        MR_PseudoTypeInfo pseudo);

////////////////////////////////////////////////////////////////////////////

static  MR_bool     MR_trace_options_nondet_stack(MR_bool *detailed,
                        MR_FrameLimit *frame_limit, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_stats(char **filename, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_type_ctor(MR_bool *print_rep,
                        MR_bool *print_functors, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_class_decl(MR_bool *print_methods,
                        MR_bool *print_instances, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_all_procedures(MR_bool *separate,
                        MR_bool *uci, char **module, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_ambiguity(const char **outfile,
                        MR_bool *print_procs, MR_bool *print_types,
                        MR_bool *print_functors, char ***words,
                        int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_var_details(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        const char  *problem;

        problem = MR_trace_list_var_details(MR_mdb_out);
        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_term_size(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        const char  *problem;

        if (MR_streq(words[1], "*")) {
            problem = MR_trace_print_size_all(MR_mdb_out);
        } else {
            problem = MR_trace_print_size_one(MR_mdb_out, words[1]);
        }

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_flag(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char  *name;
    MR_bool     *flagptr;
    int         i;
    MR_bool     found;
    const char  *set_word;

    // Set this to NULL to avoid uninitialization warnings.
    flagptr = NULL;

    if (word_count == 1) {
        for (i = 0; i < MR_MAXFLAG; i++) {
            // The true values of the debugging flags are stored in
            // MR_saved_debug_state inside the call tree of MR_trace_event.

            flagptr = &MR_saved_debug_state.MR_sds_debugflags[
                MR_debug_flag_info[i].MR_debug_flag_index];
            name = MR_debug_flag_info[i].MR_debug_flag_name;
            if (*flagptr) {
                fprintf(MR_mdb_out, "Flag %s is set.\n", name);
            } else {
                fprintf(MR_mdb_out, "Flag %s is clear.\n", name);
            }
        }

        return KEEP_INTERACTING;
    } else if (word_count == 2) {
        name = words[1];
        set_word = NULL;
    } else if (word_count == 3) {
        name = words[1];
        set_word = words[2];
    } else {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    found = MR_FALSE;
    for (i = 0; i < MR_MAXFLAG; i++) {
        if (MR_streq(MR_debug_flag_info[i].MR_debug_flag_name, name)) {
            // The true values of the debugging flags are stored in
            // MR_saved_debug_state inside the call tree of MR_trace_event.

            flagptr = &MR_saved_debug_state.MR_sds_debugflags[
                MR_debug_flag_info[i].MR_debug_flag_index];
            found = MR_TRUE;
            break;
        }
    }

    if (!found) {
        fprintf(MR_mdb_out, "There is no flag named %s.\n", name);
        return KEEP_INTERACTING;
    }

    if (set_word != NULL) {
        if (MR_streq(set_word, "on")) {
            *flagptr = MR_TRUE;
            fprintf(MR_mdb_out, "Flag %s is now set.\n", name);
        } else if (MR_streq(set_word, "off")) {
            *flagptr = MR_FALSE;
            fprintf(MR_mdb_out, "Flag %s is now clear.\n", name);
        } else {
            MR_trace_usage_cur_cmd();
        }
    } else {
        if (*flagptr) {
            fprintf(MR_mdb_out, "Flag %s is set.\n", name);
        } else {
            fprintf(MR_mdb_out, "Flag %s is clear.\n", name);
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_subgoal(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    MR_SubgoalDebug *subgoal_debug;
    MR_Subgoal  *subgoal;
    int     n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_trace_init_modules();

        subgoal_debug = MR_lookup_subgoal_debug_num(n);
        if (subgoal_debug == NULL) {
            fprintf(MR_mdb_out, "no such subgoal\n");
        } else {
            MR_trace_print_subgoal_debug(NULL, subgoal_debug);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `subgoal' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_consumer(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    MR_ConsumerDebug    *consumer_debug;
    MR_Consumer         *consumer;
    int                 n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_trace_init_modules();

        consumer_debug = MR_lookup_consumer_debug_num(n);
        if (consumer_debug == NULL) {
            fprintf(MR_mdb_out, "no such consumer\n");
        } else {
            MR_trace_print_consumer_debug(NULL, consumer_debug);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `consumer' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_gen_stack(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    if (word_count == 1) {
        MR_bool saved_tabledebug;

        MR_trace_init_modules();
        saved_tabledebug = MR_tabledebug;
        MR_tabledebug = MR_TRUE;
        MR_print_gen_stack(MR_mdb_out);
        MR_tabledebug = saved_tabledebug;
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `gen_stack' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_cut_stack(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    if (word_count == 1) {
        MR_bool saved_tabledebug;

        MR_trace_init_modules();
        saved_tabledebug = MR_tabledebug;
        MR_tabledebug = MR_TRUE;
        MR_print_cut_stack(MR_mdb_out);
        MR_tabledebug = saved_tabledebug;
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `cut_stack' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_pneg_stack(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    if (word_count == 1) {
        MR_bool saved_tabledebug;

        MR_trace_init_modules();
        saved_tabledebug = MR_tabledebug;
        MR_tabledebug = MR_TRUE;
        MR_print_pneg_stack(MR_mdb_out);
        MR_tabledebug = saved_tabledebug;
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `pneg_stack' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_mm_stacks(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    if (word_count == 1) {
        MR_bool saved_tabledebug;

        MR_trace_init_modules();
        saved_tabledebug = MR_tabledebug;
        MR_tabledebug = MR_TRUE;
        MR_print_gen_stack(MR_mdb_out);
        fprintf(MR_mdb_out, "\n");
        MR_print_cut_stack(MR_mdb_out);
        fprintf(MR_mdb_out, "\n");
        MR_print_pneg_stack(MR_mdb_out);
        MR_tabledebug = saved_tabledebug;
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

    fprintf(MR_mdb_out, "mdb: the `pneg_stack' command is available "
        "only in stack copy minimal model tabling grades.\n");

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_nondet_stack(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool             detailed;
    MR_FrameLimit       frame_limit = 0;
    int                 line_limit = MR_stack_default_line_limit;
    MR_SpecLineLimit    spec_line_limit;

    detailed = MR_FALSE;
    if (! MR_trace_options_nondet_stack(&detailed, &frame_limit,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        MR_trace_cmd_nondet_stack_2(event_info, detailed, frame_limit,
            line_limit);
    } else if (word_count == 2 &&
        MR_trace_is_natural_number(words[1], &spec_line_limit))
    {
        MR_trace_cmd_nondet_stack_2(event_info, detailed, frame_limit,
            spec_line_limit);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_stack_regs(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Word     *saved_regs;

    saved_regs = event_info->MR_saved_regs;

    if (word_count == 1) {
        MR_print_stack_regs(MR_mdb_out, saved_regs);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_all_regs(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Word     *saved_regs;

    saved_regs = event_info->MR_saved_regs;

    if (word_count == 1) {
        MR_print_stack_regs(MR_mdb_out, saved_regs);
        MR_print_heap_regs(MR_mdb_out, saved_regs);
        MR_print_tabling_regs(MR_mdb_out, saved_regs);
        MR_print_succip_reg(MR_mdb_out, saved_regs);
        MR_print_r_regs(MR_mdb_out, saved_regs);
#ifdef  MR_DEEP_PROFILING
        MR_print_deep_prof_vars(MR_mdb_out, "mdb all_regs");
#endif
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_debug_vars(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_print_debug_vars(MR_mdb_out, event_info);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_stats(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    char    *filename;
    FILE    *fp;
    MR_bool should_close;
    char    errbuf[MR_STRERROR_BUF_SIZE];

    filename = NULL;
    if (! MR_trace_options_stats(&filename, &words, &word_count)) {
        // The usage message has already been printed.
        return KEEP_INTERACTING;
    }

    if (word_count != 2) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (filename != NULL) {
        fp = fopen(filename, "w");
        if (fp == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: error opening `%s': %s.\n",
                filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
            return KEEP_INTERACTING;
        }

        should_close = MR_TRUE;
    } else {
        fp = MR_mdb_out;
        should_close = MR_FALSE;
    }

    if (MR_streq(words[1], "procs")) {
        MR_proc_layout_stats(fp);
    } else if (MR_streq(words[1], "labels")) {
        MR_label_layout_stats(fp);
    } else if (MR_streq(words[1], "var_names")) {
        MR_var_name_stats(fp);
    } else if (MR_streq(words[1], "io_tabling")) {
        MR_io_tabling_stats(fp);
    } else {
        MR_trace_usage_cur_cmd();
    }

    if (should_close) {
        (void) fclose(fp);
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_print_optionals(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2 && MR_streq(words[1], "off")) {
        MR_print_optionals = MR_FALSE;
        MR_trace_set_level(MR_trace_current_level(), MR_print_optionals);
    } else if (word_count == 2 && MR_streq(words[1], "on")) {
        MR_print_optionals = MR_TRUE;
        MR_trace_set_level(MR_trace_current_level(), MR_print_optionals);
    } else if (word_count == 1) {
        fprintf(MR_mdb_out, "optional values are %sbeing printed\n",
            MR_print_optionals? "" : "not ");
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_unhide_events(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2 && MR_streq(words[1], "off")) {
        MR_trace_unhide_events = MR_FALSE;
        fprintf(MR_mdb_out, "Hidden events are hidden.\n");
    } else if (word_count == 2 && MR_streq(words[1], "on")) {
        MR_trace_unhide_events = MR_TRUE;
        MR_trace_have_unhid_events = MR_TRUE;
        fprintf(MR_mdb_out, "Hidden events are exposed.\n");
    } else if (word_count == 1) {
        fprintf(MR_mdb_out, "Hidden events are %s.\n",
            MR_trace_unhide_events? "exposed" : "hidden");
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_table(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_CallTableArg         *call_table_args;
    const MR_ProcLayout     *proc;
    MR_ProcSpec             spec;
    MR_ProcTableInfo        *pt;
    MR_TrieNode             table_cur;
    const MR_TableStepDesc  *input_step_descs;
    int                     num_inputs;
    int                     filtered_num_inputs;
    int                     cur_arg;
    int                     filtered_cur_arg;
    int                     num_tips;

    if (word_count < 2) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (! MR_parse_proc_spec(words[1], &spec)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: invalid procedure specification.\n");
        return KEEP_INTERACTING;
    }

    proc = MR_find_single_matching_proc(&spec, MR_TRUE);
    if (proc == NULL) {
        return KEEP_INTERACTING;
    }

    switch (MR_sle_eval_method(proc)) {
        case MR_EVAL_METHOD_NORMAL:
            MR_print_proc_id(MR_mdb_out, proc);
            fprintf(MR_mdb_out, " isn't tabled.\n");
            return KEEP_INTERACTING;

        case MR_EVAL_METHOD_LOOP_CHECK:
        case MR_EVAL_METHOD_MEMO:
        case MR_EVAL_METHOD_MINIMAL_STACK_COPY:
        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR:
            break;

        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER:
            MR_print_proc_id(MR_mdb_out, proc);
            fprintf(MR_mdb_out,
                " is the consumer; the generator has the table.\n");
            return KEEP_INTERACTING;

        case MR_EVAL_METHOD_TABLE_IO:
        case MR_EVAL_METHOD_TABLE_IO_DECL:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL:
            fprintf(MR_mdb_out,
                "IO tabled predicates do not have their own tables.\n");
            return KEEP_INTERACTING;
    }

    // words[0] is the command, words[1] is the procedure spec;
    // words[2] is the first argument. We step over the command and the
    // procedure spec, to leave words[] containing only the argument values.

    words += 2;
    word_count -= 2;

    pt = proc->MR_sle_table_info.MR_table_proc;
    num_inputs = pt->MR_pt_num_inputs;

    if (word_count > num_inputs) {
        fprintf(MR_mdb_out, "There are only %d input arguments.\n",
            num_inputs);
        return KEEP_INTERACTING;
    }

    call_table_args = MR_GC_NEW_ARRAY(MR_CallTableArg, num_inputs);
    if (call_table_args == NULL) {
        MR_fatal_error("MR_trace_cmd_table: "
            "couldn't allocate call_table_args");
    }

    table_cur = &pt->MR_pt_tablenode;
    input_step_descs = pt->MR_pt_steps_desc[MR_TABLE_CALL_TABLE];
    for (cur_arg = 0, filtered_cur_arg = 0; cur_arg < num_inputs; cur_arg++) {
        switch (input_step_descs[cur_arg].MR_tsd_trie_step) {
            case MR_TABLE_STEP_INT:
            case MR_TABLE_STEP_FLOAT:
            case MR_TABLE_STEP_STRING:
                // These are OK.
                call_table_args[filtered_cur_arg].MR_cta_step =
                    input_step_descs[cur_arg].MR_tsd_trie_step;
                call_table_args[filtered_cur_arg].MR_cta_valid = MR_FALSE;
                call_table_args[filtered_cur_arg].MR_cta_unfiltered_arg_num =
                    cur_arg;
                filtered_cur_arg++;

            case MR_TABLE_STEP_PROMISE_IMPLIED:
                // This argument doesn't exist in the table.
                break;

            default:
                fprintf(MR_mdb_out, "Sorry, can handle only "
                    "integer, float and string arguments for now.\n");
                MR_GC_free(call_table_args);
                return KEEP_INTERACTING;
        }
    }

    filtered_num_inputs = filtered_cur_arg;
    if (word_count > filtered_num_inputs) {
        fprintf(MR_mdb_out,
            "Sorry, this procedure has only %d tabled arguments\n",
            filtered_num_inputs);
        MR_GC_free(call_table_args);
        return KEEP_INTERACTING;
    }

    // Set up the values of the input arguments supplied on the command line,
    // to enable us to print them out in each call table entry.

    for (filtered_cur_arg = 0;
        filtered_cur_arg < word_count;
        filtered_cur_arg++)
    {
        MR_bool success;

        switch (call_table_args[filtered_cur_arg].MR_cta_step) {
            case MR_TABLE_STEP_INT:
                success = MR_trace_fill_in_int_table_arg_slot(&table_cur,
                    filtered_cur_arg + 1, words[filtered_cur_arg],
                    &call_table_args[filtered_cur_arg]);
                break;

            case MR_TABLE_STEP_FLOAT:
                success = MR_trace_fill_in_float_table_arg_slot(&table_cur,
                    filtered_cur_arg + 1, words[filtered_cur_arg],
                    &call_table_args[filtered_cur_arg]);
                break;

            case MR_TABLE_STEP_STRING:
                success = MR_trace_fill_in_string_table_arg_slot(&table_cur,
                    filtered_cur_arg + 1, words[filtered_cur_arg],
                    &call_table_args[filtered_cur_arg]);
                break;

            default:
                MR_fatal_error("arg not int, float or string after check");
        }

        if (! success) {
            // the error message has already been printed
            MR_GC_free(call_table_args);
            return KEEP_INTERACTING;
        }
    }

    if (word_count == filtered_num_inputs) {
        // The user specified values for all the input arguments,
        // so what we print is a single entry, not a table of entries,
        // and we don't need to loop over all the entries.

        MR_trace_cmd_table_print_tip(proc, filtered_num_inputs,
            call_table_args, table_cur);
        MR_GC_free(call_table_args);
        return KEEP_INTERACTING;
    }

    // The user left the values of some input arguments unspecified,
    // so we print a table of entries. Here we print the header.

    switch (MR_sle_eval_method(proc)) {
        case MR_EVAL_METHOD_LOOP_CHECK:
            fprintf(MR_mdb_out, "loopcheck table for ");
            MR_print_proc_id(MR_mdb_out, proc);
            fprintf(MR_mdb_out, ":\n");
            break;

        case MR_EVAL_METHOD_MEMO:
            fprintf(MR_mdb_out, "memo table for ");
            MR_print_proc_id(MR_mdb_out, proc);
            fprintf(MR_mdb_out, ":\n");
            break;

        case MR_EVAL_METHOD_MINIMAL_STACK_COPY:
        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR:
            fprintf(MR_mdb_out, "minimal model table for ");
            MR_print_proc_id(MR_mdb_out, proc);
            fprintf(MR_mdb_out, ":\n");
            break;

        case MR_EVAL_METHOD_NORMAL:
        case MR_EVAL_METHOD_TABLE_IO:
        case MR_EVAL_METHOD_TABLE_IO_DECL:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL:
        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER:
            MR_fatal_error("MR_trace_cmd_table: bad eval method");
    }

    // This loop prints the entries in the table.
    //
    // If we knew in advance that the user left (say) two input argument
    // positions unspecified, we could use a loop structure such as:
    //
    //  for value1 in <values in the trie at node start_node[0]>
    //      cur_value[1] = value1
    //      start_node[1] = follow value1 in start_node[0]
    //      for value2 in <values in the trie at node start_node[1]>
    //          cur_value[2] = value2
    //          start_node[2] = follow value2 in start_node[1]
    //          print <fixed args>, cur_value[1], cur_value[2]
    //      end for
    //  end for
    //
    // However, we don't know in advance how many input arguments the user
    // left unspecified. We therefore simulate the above with a single
    // loop, which can function as any one of the above nested loops.
    //
    // The value of cur_arg controls which one it is simulating at any
    // given time. Initially, cur_arg grows as we enter each of the above
    // loops one after another, at each stage recording the set of values
    // in the current trie node in the values array of the relevant
    // argument.
    //
    // We number the input arguments from 0 to filtered_num_inputs-1.
    // When cur_arg becomes equal to filtered_num_inputs, this means that
    // we have values for all the tabled input arguments, so we print the
    // corresponding call table entry. We then initiate backtracking:
    // we decrement cur_arg to get the next value of the last argument.
    // We also do this whenever we run out of values in any trie.
    //
    // We stop when we are about to backtrack out of the outermost loop.

    cur_arg = word_count;
    num_tips = 0;
    for (;;) {
        MR_bool no_more;
        MR_bool start_backtrack;

        switch (call_table_args[cur_arg].MR_cta_step) {
            case MR_TABLE_STEP_INT:
                no_more = MR_update_int_table_arg_slot(&table_cur,
                    &call_table_args[cur_arg]);
                break;

            case MR_TABLE_STEP_FLOAT:
                no_more = MR_update_float_table_arg_slot(&table_cur,
                    &call_table_args[cur_arg]);
                break;

            case MR_TABLE_STEP_STRING:
                no_more = MR_update_string_table_arg_slot(&table_cur,
                    &call_table_args[cur_arg]);
                break;

            default:
                MR_fatal_error("arg not int, float or string after check");
        }

        if (no_more) {
            // There aren't any more values in the current trie
            // of input argument cur_arg.

            start_backtrack = MR_TRUE;
        } else {
            // There is at least one more value in the current trie
            // of input argument cur_arg, so go on to the next trie
            // (if there is one).

            cur_arg++;

            if (cur_arg >= filtered_num_inputs) {
                MR_trace_cmd_table_print_tip(proc, filtered_num_inputs,
                    call_table_args, table_cur);
                num_tips++;
                start_backtrack = MR_TRUE;
            } else {
                start_backtrack = MR_FALSE;
            }
        }

        if (start_backtrack) {
            cur_arg--;
            table_cur = call_table_args[cur_arg].MR_cta_start_node;

            if (cur_arg < word_count) {
                break;
            }
        }
    }

    fprintf(MR_mdb_out, "end of table (%d %s)\n",
        num_tips, (num_tips == 1 ? "entry" : "entries"));
    MR_GC_free(call_table_args);
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_type_ctor(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char      *module_name;
    const char      *name;
    MR_Unsigned     arity;
    MR_bool         print_rep;
    MR_bool         print_functors;
    MR_TypeCtorInfo type_ctor_info;

    MR_do_init_modules_type_tables();

    print_rep = MR_FALSE;
    print_functors = MR_FALSE;
    if (! MR_trace_options_type_ctor(&print_rep, &print_functors,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 4 &&
        MR_trace_is_natural_number(words[3], &arity))
    {
        module_name = words[1];
        name = words[2];
        type_ctor_info = MR_lookup_type_ctor_info(module_name, name, arity);
        if (type_ctor_info != NULL) {
            MR_print_type_ctor_info(MR_mdb_out, type_ctor_info, print_rep,
                print_functors);
        } else {
            fprintf(MR_mdb_out, "there is no such type constructor\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_class_decl(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char              *module_name;
    const char              *name;
    MR_Unsigned             arity;
    MR_bool                 print_methods;
    MR_bool                 print_instances;
    MR_TypeClassDeclInfo    *type_class_decl_info;

    MR_do_init_modules_type_tables();

    print_methods = MR_FALSE;
    print_instances = MR_FALSE;
    if (! MR_trace_options_class_decl(&print_methods, &print_instances,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 4 &&
        MR_trace_is_natural_number(words[3], &arity))
    {
        module_name = words[1];
        name = words[2];
        type_class_decl_info = MR_lookup_type_class_decl_info(module_name,
            name, arity);
        if (type_class_decl_info != NULL) {
            MR_print_class_decl_info(MR_mdb_out, type_class_decl_info,
                print_methods, print_instances);
        } else {
            fprintf(MR_mdb_out, "there is no such type class\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_all_type_ctors(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool         print_rep;
    MR_bool         print_functors;
    MR_Dlist        *list;
    MR_Dlist        *element_ptr;
    MR_TypeCtorInfo type_ctor_info;
    const char      *module_name;
    int             count;

    MR_do_init_modules_type_tables();

    print_rep = MR_FALSE;
    print_functors = MR_FALSE;
    if (! MR_trace_options_type_ctor(&print_rep, &print_functors,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1 || word_count == 2) {
        if (word_count == 2) {
            module_name = words[1];
        } else {
            module_name = NULL;
        }

        list = MR_all_type_ctor_infos(NULL);
        count = 0;
        MR_for_dlist(element_ptr, list) {
            type_ctor_info = (MR_TypeCtorInfo) MR_dlist_data(element_ptr);
            if (module_name != NULL && strcmp(module_name,
                type_ctor_info->MR_type_ctor_module_name) != 0)
            {
                continue;
            }

            if (count > 0) {
                fprintf(MR_mdb_out, "\n");
            }
            MR_print_type_ctor_info(MR_mdb_out, type_ctor_info, print_rep,
                print_functors);
            count++;
        }

        fprintf(MR_mdb_out, "\nnumber of type constructors ");
        if (module_name == NULL) {
            fprintf(MR_mdb_out, "in the program: %d\n", count);
        } else {
            fprintf(MR_mdb_out, "in module %s: %d\n", module_name, count);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_all_class_decls(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool                 print_methods;
    MR_bool                 print_instances;
    MR_Dlist                *list;
    MR_Dlist                *element_ptr;
    MR_TypeClassDeclInfo    *type_class_decl_info;
    const char              *module_name;
    int                     count;

    MR_do_init_modules_type_tables();

    print_methods = MR_FALSE;
    print_instances = MR_FALSE;
    if (! MR_trace_options_class_decl(&print_methods, &print_instances,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1 || word_count == 2) {
        if (word_count == 2) {
            module_name = words[1];
        } else {
            module_name = NULL;
        }
        list = MR_all_type_class_decl_infos(NULL);
        count = 0;
        MR_for_dlist(element_ptr, list) {
            type_class_decl_info = (MR_TypeClassDeclInfo *)
                MR_dlist_data(element_ptr);
            if (module_name != NULL && strcmp(module_name,
                type_class_decl_info->MR_tcd_info_decl->
                MR_tc_decl_id->MR_tc_id_module_name) != 0)
            {
                continue;
            }

            if (count > 0) {
                fprintf(MR_mdb_out, "\n");
            }
            MR_print_class_decl_info(MR_mdb_out, type_class_decl_info,
                print_methods, print_instances);
            count++;
        }

        fprintf(MR_mdb_out, "\nnumber of type classes ");
        if (module_name == NULL) {
            fprintf(MR_mdb_out, "in the program: %d\n", count);
        } else {
            fprintf(MR_mdb_out, "in module %s: %d\n", module_name, count);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_all_procedures(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char      *filename;
    MR_bool         separate;
    MR_bool         uci;
    FILE            *fp;
    char            *module;
    char            errbuf[MR_STRERROR_BUF_SIZE];

    MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);

    separate = MR_FALSE;
    uci = MR_FALSE;
    module = NULL;
    if (! MR_trace_options_all_procedures(&separate, &uci, &module,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2) {
        filename = words[1];
        fp = fopen(filename, "w");
        if (fp == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: error opening `%s': %s.\n",
                filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
            return KEEP_INTERACTING;
        }

        MR_dump_module_tables(fp, separate, uci, module);
        if (fclose(fp) != 0) {
            fprintf(MR_mdb_err, "mdb: error writing to `%s': %s.\n",
                filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
            return KEEP_INTERACTING;
        } else {
            fprintf(MR_mdb_out, "mdb: wrote table to `%s'.\n", filename);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_ambiguity(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char      *filename;
    MR_bool         print_procs;
    MR_bool         print_types;
    MR_bool         print_functors;
    FILE            *fp;
    char            errbuf[MR_STRERROR_BUF_SIZE];

    filename = NULL;
    print_procs = MR_FALSE;
    print_types = MR_FALSE;
    print_functors = MR_FALSE;
    if (! MR_trace_options_ambiguity(&filename, &print_procs, &print_types,
        &print_functors, &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else {
        if (!print_procs && !print_types && !print_functors) {
            print_procs = MR_TRUE;
            print_types = MR_TRUE;
            print_functors = MR_TRUE;
        }

        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);

        if (filename == NULL) {
            fp = MR_mdb_out;
        } else {
            fp = fopen(filename, "w");
            if (fp == NULL) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: error opening `%s': %s.\n",
                    filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
                return KEEP_INTERACTING;
            }
        }

        // The words on the command line after the command name and the already
        // processed options are a list of modules names. If this list is not
        // empty, then we consider only the modules named here when looking for
        // ambiguities.

        MR_print_ambiguities(fp, print_procs, print_types, print_functors,
            &words[1], word_count - 1);

        if (filename != NULL) {
            fprintf(MR_mdb_out, "mdb: wrote report to `%s'.\n", filename);
            fclose(fp);
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_trail_details(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{

#if defined(MR_USE_TRAIL)

    MR_Word *saved_regs;

    saved_regs = event_info->MR_saved_regs;

    fprintf(MR_mdb_out,
        "trail pointer    : %" MR_INTEGER_LENGTH_MODIFIER
        "d (%" MR_INTEGER_LENGTH_MODIFIER "x)\n",
        (MR_Integer) MR_saved_trail_ptr(saved_regs),
        (MR_Integer) MR_saved_trail_ptr(saved_regs));
    fprintf(MR_mdb_out, "ticket counter   : %lu\n",
        (unsigned long) MR_saved_ticket_counter(saved_regs));
    fprintf(MR_mdb_out, "ticket high water: %lu\n",
        (unsigned long) MR_saved_ticket_high_water(saved_regs));
    fprintf(MR_mdb_out, "number of trail entries: %lu\n",
        (unsigned long) MR_num_trail_entries());

    #if defined(MR_TRAIL_SEGMENTS)
        fprintf(MR_mdb_out, "number of trail segments: %lu\n",
            (unsigned long) MR_num_trail_segments());
    #endif

#else // ! MR_USE_TRAIL

    fprintf(MR_mdb_out, "mdb: the `trail_details' command is available "
        "only in trailing grades.\n");

#endif // ! MR_USE_TRAIL

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

static void
MR_trace_cmd_nondet_stack_2(MR_EventInfo *event_info, MR_bool detailed,
    MR_FrameLimit frame_limit, MR_SpecLineLimit line_limit)
{
    const MR_LabelLayout    *layout;
    MR_Word                 *saved_regs;

    layout = event_info->MR_event_sll;
    saved_regs = event_info->MR_saved_regs;

    MR_trace_init_modules();
    if (detailed) {
        int saved_level;

        saved_level = MR_trace_current_level();
        MR_dump_nondet_stack_from_layout(MR_mdb_out, frame_limit, line_limit,
            MR_saved_maxfr(saved_regs), layout,
            MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs));
        MR_trace_set_level(saved_level, MR_print_optionals);
    } else {
        MR_dump_nondet_stack(MR_mdb_out, frame_limit, line_limit,
            MR_saved_maxfr(saved_regs));
    }
}

static const MR_ProcLayout *
MR_find_single_matching_proc(MR_ProcSpec *spec, MR_bool verbose)
{
    MR_MatchesInfo      matches;
    MR_Unsigned         n;
    int                 i;

    MR_register_all_modules_and_procs(MR_mdb_out, verbose);
    matches = MR_search_for_matching_procedures(spec);
    if (matches.match_proc_next == 0) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: there is no such procedure.\n");
        return NULL;
    } else if (matches.match_proc_next == 1) {
        return matches.match_procs[0];
    } else {
        char    buf[100];
        char    *line2;

        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "Ambiguous procedure specification. "
            "The matches are:\n");
        for (i = 0; i < matches.match_proc_next; i++) {
            fprintf(MR_mdb_out, "%d: ", i);
            MR_print_proc_id_and_nl(MR_mdb_out, matches.match_procs[i]);
        }

        sprintf(buf,
            "\nWhich procedure's table do you want to print (0-%"
            MR_INTEGER_LENGTH_MODIFIER "d)? ",
            matches.match_proc_next - 1);
        line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
        if (line2 == NULL || !MR_trace_is_natural_number(line2, &n)) {
            fprintf(MR_mdb_out, "none of them\n");
            return NULL;
        } else if (n >= matches.match_proc_next) {
            fprintf(MR_mdb_out, "invalid choice\n");
            return NULL;
        } else {

            if (line2 != NULL) {
                MR_free(line2);
            }
            return matches.match_procs[n];
        }
    }
}

static MR_bool
MR_trace_fill_in_int_table_arg_slot(MR_TrieNode *table_cur_ptr,
    int arg_num, MR_ConstString given_arg,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_Integer  n;
    MR_TrieNode table_next;

    if (! MR_trace_is_integer(given_arg, &n)) {
        fprintf(MR_mdb_out, "argument %d is not an integer.\n", arg_num);
        return MR_FALSE;
    }

    table_next = MR_int_hash_lookup(*table_cur_ptr, n);
    if (table_next == NULL) {
        fprintf(MR_mdb_out,
            "call table does not contain %" MR_INTEGER_LENGTH_MODIFIER "d"
            " in argument position %d.\n", n, arg_num);
        return MR_FALSE;
    }

    call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
    call_table_arg_ptr->MR_cta_valid = MR_TRUE;
    call_table_arg_ptr->MR_cta_int_values = NULL;
    call_table_arg_ptr->MR_cta_int_value_next = -1;
    call_table_arg_ptr->MR_cta_int_cur_index = -1;
    call_table_arg_ptr->MR_cta_int_cur_value = n;
    *table_cur_ptr = table_next;

    return MR_TRUE;
}

static MR_bool
MR_trace_fill_in_float_table_arg_slot(MR_TrieNode *table_cur_ptr,
    int arg_num, MR_ConstString given_arg,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_Float    f;
    MR_TrieNode table_next;

    if (! MR_trace_is_float(given_arg, &f)) {
        fprintf(MR_mdb_out, "argument %d is not a float.\n", arg_num);
        return MR_FALSE;
    }

    table_next = MR_float_hash_lookup(*table_cur_ptr, f);
    if (table_next == NULL) {
        fprintf(MR_mdb_out,
            "call table does not contain %f in argument position %d.\n",
            f, arg_num);
        return MR_FALSE;
    }

    call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
    call_table_arg_ptr->MR_cta_valid = MR_TRUE;
    call_table_arg_ptr->MR_cta_float_values = NULL;
    call_table_arg_ptr->MR_cta_float_value_next = -1;
    call_table_arg_ptr->MR_cta_float_cur_index = -1;
    call_table_arg_ptr->MR_cta_float_cur_value = f;
    *table_cur_ptr = table_next;

    return MR_TRUE;
}

static MR_bool
MR_trace_fill_in_string_table_arg_slot(MR_TrieNode *table_cur_ptr,
    int arg_num, MR_ConstString given_arg,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_ConstString  s;
    MR_TrieNode table_next;

    s = given_arg;

    table_next = MR_string_hash_lookup(*table_cur_ptr, s);
    if (table_next == NULL) {
        fprintf(MR_mdb_out,
            "call table does not contain %s in argument position %d.\n",
            s, arg_num);
        return MR_FALSE;
    }

    call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
    call_table_arg_ptr->MR_cta_valid = MR_TRUE;
    call_table_arg_ptr->MR_cta_string_values = NULL;
    call_table_arg_ptr->MR_cta_string_value_next = -1;
    call_table_arg_ptr->MR_cta_string_cur_index = -1;
    call_table_arg_ptr->MR_cta_string_cur_value = s;
    *table_cur_ptr = table_next;

    return MR_TRUE;
}

static MR_bool
MR_update_int_table_arg_slot(MR_TrieNode *table_cur_ptr,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_TrieNode table_next;
    MR_Integer  *values;
    int         value_next;

    if (call_table_arg_ptr->MR_cta_valid
        && call_table_arg_ptr->MR_cta_int_values != NULL)
    {
        call_table_arg_ptr->MR_cta_int_cur_index++;
    } else {
        if (! MR_get_int_hash_table_contents(*table_cur_ptr,
            &values, &value_next))
        {
            // There are no values in this trie node.
            call_table_arg_ptr->MR_cta_valid = MR_FALSE;
            return MR_TRUE;
        }

        call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
        call_table_arg_ptr->MR_cta_valid = MR_TRUE;
        call_table_arg_ptr->MR_cta_int_values = values;
        call_table_arg_ptr->MR_cta_int_value_next = value_next;
        call_table_arg_ptr->MR_cta_int_cur_index = 0;
    }

    if (call_table_arg_ptr->MR_cta_int_cur_index
        >= call_table_arg_ptr->MR_cta_int_value_next)
    {
        // We have already returned all the values in this trie node.
        call_table_arg_ptr->MR_cta_valid = MR_FALSE;
        return MR_TRUE;
    }

    call_table_arg_ptr->MR_cta_int_cur_value =
        call_table_arg_ptr->MR_cta_int_values[
            call_table_arg_ptr->MR_cta_int_cur_index];

    table_next = MR_int_hash_lookup(call_table_arg_ptr->MR_cta_start_node,
        call_table_arg_ptr->MR_cta_int_cur_value);

    if (table_next == NULL) {
        MR_fatal_error("MR_update_int_table_arg_slot: bad lookup");
    }

    *table_cur_ptr = table_next;
    return MR_FALSE;
}

static MR_bool
MR_update_float_table_arg_slot(MR_TrieNode *table_cur_ptr,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_TrieNode table_next;
    MR_Float    *values;
    int         value_next;

    if (call_table_arg_ptr->MR_cta_valid
        && call_table_arg_ptr->MR_cta_float_values != NULL)
    {
        call_table_arg_ptr->MR_cta_float_cur_index++;
    } else {
        if (! MR_get_float_hash_table_contents(*table_cur_ptr,
            &values, &value_next))
        {
            // There are no values in this trie node.
            call_table_arg_ptr->MR_cta_valid = MR_FALSE;
            return MR_TRUE;
        }

        call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
        call_table_arg_ptr->MR_cta_valid = MR_TRUE;
        call_table_arg_ptr->MR_cta_float_values = values;
        call_table_arg_ptr->MR_cta_float_value_next = value_next;
        call_table_arg_ptr->MR_cta_float_cur_index = 0;
    }

    if (call_table_arg_ptr->MR_cta_float_cur_index
        >= call_table_arg_ptr->MR_cta_float_value_next)
    {
        // We have already returned all the values in this trie node.
        call_table_arg_ptr->MR_cta_valid = MR_FALSE;
        return MR_TRUE;
    }

    call_table_arg_ptr->MR_cta_float_cur_value =
        call_table_arg_ptr->MR_cta_float_values[
            call_table_arg_ptr->MR_cta_float_cur_index];

    table_next = MR_float_hash_lookup(call_table_arg_ptr->MR_cta_start_node,
        call_table_arg_ptr->MR_cta_float_cur_value);

    if (table_next == NULL) {
        MR_fatal_error("MR_update_float_table_arg_slot: bad lookup");
    }

    *table_cur_ptr = table_next;
    return MR_FALSE;
}

static MR_bool
MR_update_string_table_arg_slot(MR_TrieNode *table_cur_ptr,
    MR_CallTableArg *call_table_arg_ptr)
{
    MR_TrieNode     table_next;
    MR_ConstString  *values;
    int             value_next;

    if (call_table_arg_ptr->MR_cta_valid
        && call_table_arg_ptr->MR_cta_string_values != NULL)
    {
        call_table_arg_ptr->MR_cta_string_cur_index++;
    } else {
        if (! MR_get_string_hash_table_contents(*table_cur_ptr,
            &values, &value_next))
        {
            // There are no values in this trie node.
            call_table_arg_ptr->MR_cta_valid = MR_FALSE;
            return MR_TRUE;
        }

        call_table_arg_ptr->MR_cta_start_node = *table_cur_ptr;
        call_table_arg_ptr->MR_cta_valid = MR_TRUE;
        call_table_arg_ptr->MR_cta_string_values = values;
        call_table_arg_ptr->MR_cta_string_value_next = value_next;
        call_table_arg_ptr->MR_cta_string_cur_index = 0;
    }

    if (call_table_arg_ptr->MR_cta_string_cur_index
        >= call_table_arg_ptr->MR_cta_string_value_next)
    {
        // We have already returned all the values in this trie node.
        call_table_arg_ptr->MR_cta_valid = MR_FALSE;
        return MR_TRUE;
    }

    call_table_arg_ptr->MR_cta_string_cur_value =
        call_table_arg_ptr->MR_cta_string_values[
            call_table_arg_ptr->MR_cta_string_cur_index];

    table_next = MR_string_hash_lookup(
        call_table_arg_ptr->MR_cta_start_node,
        call_table_arg_ptr->MR_cta_string_cur_value);

    if (table_next == NULL) {
        MR_fatal_error("MR_update_string_table_arg_slot: bad lookup");
    }

    *table_cur_ptr = table_next;
    return MR_FALSE;
}

static void
MR_trace_cmd_table_print_tip(const MR_ProcLayout *proc,
    int num_filtered_inputs, MR_CallTableArg *call_table_args,
    MR_TrieNode table)
{
    int             i;
    MR_EvalMethod   eval_method;

    fprintf(MR_mdb_out, "<");
    for (i = 0; i < num_filtered_inputs; i++) {
        if (i > 0) {
            fprintf(MR_mdb_out, ", ");
        }

        switch (call_table_args[i].MR_cta_step) {
            case MR_TABLE_STEP_INT:
                fprintf(MR_mdb_out, "%" MR_INTEGER_LENGTH_MODIFIER "d",
                    call_table_args[i].MR_cta_int_cur_value);
                break;

            case MR_TABLE_STEP_FLOAT:
                fprintf(MR_mdb_out, "%f",
                    call_table_args[i].MR_cta_float_cur_value);
                break;

            case MR_TABLE_STEP_STRING:
                fprintf(MR_mdb_out, "\"%s\"",
                    call_table_args[i].MR_cta_string_cur_value);
                break;

            default:
                MR_fatal_error("arg not int, float or string after check");
        }
    }

    fprintf(MR_mdb_out, ">: ");

    eval_method = MR_sle_eval_method(proc);
    switch (eval_method) {
        case MR_EVAL_METHOD_MINIMAL_STACK_COPY:
            {
                MR_Subgoal  *subgoal;

                fprintf(MR_mdb_out, "trie node %p\n", table);
                subgoal = table->MR_subgoal;
                if (subgoal == NULL) {
                    fprintf(MR_mdb_out, "uninitialized\n");
                } else {
                    MR_trace_print_subgoal(proc, subgoal);
                }
            }
            break;

        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR:
            {
                MR_GeneratorPtr generator;

                fprintf(MR_mdb_out, "trie node %p\n", table);
                generator = table->MR_generator;
                if (generator == NULL) {
                    fprintf(MR_mdb_out, "uninitialized\n");
                } else {
                    MR_trace_print_generator(proc, generator);
                }
            }
            break;

        case MR_EVAL_METHOD_MEMO:
            {
                MR_Determinism  detism;

                detism = proc->MR_sle_detism;
                if (MR_DETISM_DET_STACK(detism)) {
                    MR_print_memo_tip(MR_mdb_out, proc, table);
                } else {
                    MR_MemoNonRecordPtr record;

                    record = table->MR_memo_non_record;
                    MR_print_memo_non_record(MR_mdb_out, proc, record);
                }
            }
            break;

        case MR_EVAL_METHOD_LOOP_CHECK:
            MR_print_loopcheck_tip(MR_mdb_out, proc, table);
            break;

        case MR_EVAL_METHOD_NORMAL:
        case MR_EVAL_METHOD_TABLE_IO:
        case MR_EVAL_METHOD_TABLE_IO_DECL:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE:
        case MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL:
        case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER:
            MR_fatal_error("MR_trace_cmd_table_print_tip: bad eval method");
            break;
    }
}

static void
MR_trace_print_subgoal(const MR_ProcLayout *proc, MR_Subgoal *subgoal)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_print_subgoal(MR_mdb_out, proc, subgoal);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_trace_print_subgoal_debug(const MR_ProcLayout *proc,
    MR_SubgoalDebug *subgoal_debug)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_print_subgoal_debug(MR_mdb_out, proc, subgoal_debug);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_trace_print_generator(const MR_ProcLayout *proc, MR_Generator *generator)
{
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
    MR_print_generator(MR_mdb_out, proc, generator);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_trace_print_generator_debug(const MR_ProcLayout *proc,
    MR_GenDebug *generator_debug)
{
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
    MR_print_gen_debug(MR_mdb_out, proc, generator_debug);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_trace_print_consumer(const MR_ProcLayout *proc, MR_Consumer *consumer)
{
#if defined(MR_USE_MINIMAL_MODEL_STACK_COPY) \
        || defined(MR_USE_MINIMAL_MODEL_OWN_STACKS)
    MR_print_consumer(MR_mdb_out, proc, consumer);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_trace_print_consumer_debug(const MR_ProcLayout *proc,
    MR_ConsumerDebug *consumer_debug)
{
#if defined(MR_USE_MINIMAL_MODEL_STACK_COPY)
    MR_print_consumer_debug(MR_mdb_out, proc, consumer_debug);
#elif defined(MR_USE_MINIMAL_MODEL_STACK_COPY)
    MR_print_cons_debug(MR_mdb_out, proc, consumer_debug);
#else
    fprintf(MR_mdb_out, "minimal model tabling is not enabled\n");
#endif
}

static void
MR_print_type_ctor_info(FILE *fp, MR_TypeCtorInfo type_ctor_info,
    MR_bool print_rep, MR_bool print_functors)
{
    MR_TypeCtorRep              rep;
    MR_EnumFunctorDesc          *enum_functor;
    MR_DuFunctorDesc            *du_functor;
    MR_NotagFunctorDesc         *notag_functor;
    int                         num_functors;
    int                         i;

    fprintf(fp, "type constructor %s.%s/%d",
        type_ctor_info->MR_type_ctor_module_name,
        type_ctor_info->MR_type_ctor_name,
        (int) type_ctor_info->MR_type_ctor_arity);

    rep = MR_type_ctor_rep(type_ctor_info);
    if (print_rep) {
        fprintf(fp, ": %s\n", MR_ctor_rep_name[MR_GET_ENUM_VALUE(rep)]);
    } else {
        fprintf(fp, "\n");
    }

    if (print_functors) {
        num_functors = type_ctor_info->MR_type_ctor_num_functors;
        switch (rep) {
            case MR_TYPECTOR_REP_ENUM:
            case MR_TYPECTOR_REP_ENUM_USEREQ:
                for (i = 0; i < num_functors; i++) {
                    enum_functor = type_ctor_info->MR_type_ctor_functors.
                        MR_functors_enum[i];
                    if (i > 0) {
                        fprintf(fp, ", ");
                    }
                    fprintf(fp, "%s/0", enum_functor->MR_enum_functor_name);
                }
                fprintf(fp, "\n");
                break;

            case MR_TYPECTOR_REP_DU:
            case MR_TYPECTOR_REP_DU_USEREQ:
                for (i = 0; i < num_functors; i++) {
                    du_functor = type_ctor_info->MR_type_ctor_functors.
                        MR_functors_du[i];
                    if (i > 0) {
                        fprintf(fp, ", ");
                    }
                    fprintf(fp, "%s/%d", du_functor->MR_du_functor_name,
                        du_functor-> MR_du_functor_orig_arity);
                }
                fprintf(fp, "\n");
                break;

            case MR_TYPECTOR_REP_NOTAG:
            case MR_TYPECTOR_REP_NOTAG_USEREQ:
            case MR_TYPECTOR_REP_NOTAG_GROUND:
            case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
                notag_functor = type_ctor_info->MR_type_ctor_functors.
                    MR_functors_notag;
                fprintf(fp, "%s/1\n", notag_functor->MR_notag_functor_name);
                break;

            default:
                break;
        }
    }
}

static void
MR_print_class_decl_info(FILE *fp, MR_TypeClassDeclInfo *type_class_decl_info,
    MR_bool print_methods, MR_bool print_instances)
{
    MR_TypeClassDecl            type_class_decl;
    const MR_TypeClassId        *type_class_id;
    const MR_TypeClassMethod    *method;
    MR_Instance                 instance;
    MR_Dlist                    *list;
    MR_Dlist                    *element_ptr;
    int                         num_methods;
    int                         i;

    type_class_decl = type_class_decl_info->MR_tcd_info_decl;
    type_class_id = type_class_decl->MR_tc_decl_id;
    fprintf(fp, "type class %s.%s/%d\n",
        type_class_id->MR_tc_id_module_name,
        type_class_id->MR_tc_id_name,
        type_class_id->MR_tc_id_arity);

    if (print_methods) {
        num_methods = type_class_id->MR_tc_id_num_methods;
        fprintf(fp, "methods: ");

        for (i = 0; i < num_methods; i++) {
            if (i > 0) {
                fprintf(fp, ", ");
            }

            method = &type_class_id->MR_tc_id_methods[i];
            if (method->MR_tc_method_pred_func == MR_FUNCTION) {
                fprintf(fp, "func ");
            } else {
                fprintf(fp, "pred ");
            }

            fprintf(fp, "%s/%d", method->MR_tc_method_name,
                method->MR_tc_method_arity);
        }

        fprintf(fp, "\n");
    }

    if (print_instances) {
        list = type_class_decl_info->MR_tcd_info_instances;
        MR_for_dlist (element_ptr, list) {
            instance = (MR_Instance) MR_dlist_data(element_ptr);

            if (instance->MR_tc_inst_type_class != type_class_decl) {
                MR_fatal_error("instance/type class mismatch");
            }

            fprintf(fp, "instance ");

            for (i = 0; i < type_class_id->MR_tc_id_arity; i++) {
                if (i > 0) {
                    fprintf(fp, ", ");
                }

                MR_print_pseudo_type_info(fp,
                    instance->MR_tc_inst_type_args[i]);
            }

            fprintf(fp, "\n");
        }
    }
}

static void
MR_print_pseudo_type_info(FILE *fp, MR_PseudoTypeInfo pseudo)
{
    MR_TypeCtorInfo     type_ctor_info;
    MR_PseudoTypeInfo   *pseudo_args;
    MR_Integer          tvar_num;
    int                 arity;
    int                 i;

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo)) {
        tvar_num = (MR_Integer) pseudo;
        fprintf(fp, "T%d", (int) tvar_num);
    } else {
        type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo);
        fprintf(fp, "%s.%s",
            type_ctor_info->MR_type_ctor_module_name,
            type_ctor_info->MR_type_ctor_name);
        if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
            arity = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo);
            pseudo_args = (MR_PseudoTypeInfo *)
                &pseudo->MR_pti_var_arity_arity;
        } else {
            arity = type_ctor_info->MR_type_ctor_arity;
            pseudo_args = (MR_PseudoTypeInfo *) &pseudo->MR_pti_type_ctor_info;
        }

        if (type_ctor_info->MR_type_ctor_arity > 0) {
            fprintf(fp, "(");
            for (i = 1; i <= arity; i++) {
                if (i > 1) {
                    fprintf(fp, ", ");
                }

                MR_print_pseudo_type_info(fp, pseudo_args[i]);
            }
            fprintf(fp, ")");
        }
    }
}

////////////////////////////////////////////////////////////////////////////

// It is better to have a single completion where possible,
// so don't include `-d' here.

const char *const    MR_trace_nondet_stack_cmd_args[] =
    { "--detailed", NULL };

const char *const    MR_trace_stats_cmd_args[] =
    { "procs", "labels", "var_names", "io_tabling", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_nondet_stack_opts[] =
{
    { "detailed",       MR_no_argument,         NULL,   'd' },
    { "frame-limit",    MR_required_argument,   NULL,   'f' },
    { NULL,             MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_nondet_stack(MR_bool *detailed, MR_FrameLimit *frame_limit,
    char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "df:",
        MR_trace_nondet_stack_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'd':
                *detailed = MR_TRUE;
                break;

            case 'f':
                if (! MR_trace_is_natural_number(MR_optarg, frame_limit)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_stats_opts[] =
{
    { "file",      MR_required_argument,    NULL,   'f' },
    { NULL,        MR_no_argument,          NULL,   0   }
};

static MR_bool
MR_trace_options_stats(char **filename, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "f:",
        MR_trace_stats_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'f':
                *filename = MR_optarg;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_type_ctor_opts[] =
{
    { "print-rep",      MR_no_argument,     NULL,   'r' },
    { "print-functors", MR_no_argument,     NULL,   'f' },
    { NULL,             MR_no_argument,     NULL,   0   }
};

static MR_bool
MR_trace_options_type_ctor(MR_bool *print_rep, MR_bool *print_functors,
    char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "rf",
        MR_trace_type_ctor_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'f':
                *print_functors = MR_TRUE;
                break;

            case 'r':
                *print_rep = MR_TRUE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_class_decl_opts[] =
{
    { "print-methods",      MR_no_argument,     NULL,   'm' },
    { "print-instances",    MR_no_argument,     NULL,   'i' },
    { NULL,                 MR_no_argument,     NULL,   0   }
};

static MR_bool
MR_trace_options_class_decl(MR_bool *print_methods, MR_bool *print_instances,
    char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "mi",
        MR_trace_class_decl_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'm':
                *print_methods = MR_TRUE;
                break;

            case 'i':
                *print_instances = MR_TRUE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_all_procedures_opts[] =
{
    { "separate",   MR_no_argument,         NULL,   's' },
    { "uci",        MR_no_argument,         NULL,   'u' },
    { "module",     MR_required_argument,   NULL,   'm' },
    { NULL,         MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_all_procedures(MR_bool *separate, MR_bool *uci, char **module,
    char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "sum:",
        MR_trace_all_procedures_opts, NULL)) != EOF)
    {
        switch (c) {

            case 's':
                *separate = MR_TRUE;
                break;

            case 'u':
                *uci = MR_TRUE;
                break;

            case 'm':
                *module = MR_optarg;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_ambiguity_opts[] =
{
    { "outputfile", MR_required_argument,   NULL,   'o' },
    { "procedures", MR_no_argument,         NULL,   'p' },
    { "types",      MR_no_argument,         NULL,   't' },
    { "functors",   MR_no_argument,         NULL,   'f' },
    { NULL,         MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_ambiguity(const char **outfile, MR_bool *print_procs,
    MR_bool *print_types, MR_bool *print_functors, char ***words,
    int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "o:ptf",
        MR_trace_ambiguity_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'o':
                *outfile = MR_optarg;
                break;

            case 'p':
                *print_procs = MR_TRUE;
                break;

            case 't':
                *print_types = MR_TRUE;
                break;

            case 'f':
                *print_functors = MR_TRUE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}
