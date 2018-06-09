// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file manages a table listing the debuggable modules of the program,
// and subsidiary tables listing the procedures of each of those modules.
//
// Main author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_label.h"
#include "mercury_array_macros.h"
#include "mercury_stack_layout.h"
#include "mercury_stack_trace.h"
#include "mercury_dlist.h"

#include "mercury_trace_tables.h"
#include "mercury_trace_internal.h"
#include "mercury_trace.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

MR_TraceEventSet    *MR_trace_event_sets = NULL;
int                 MR_trace_event_set_next = 0;
int                 MR_trace_event_set_max = 0;

MR_bool             MR_trace_event_sets_are_all_consistent = MR_TRUE;
int                 MR_trace_event_sets_max_num_attr = -1;

// We record module layout structures in two tables. The MR_module_infos
// array contains one pointer to every module layout structure, and is ordered
// by the fully qualified module name. The MR_module_nickname array contains
// one reference to every module layout structure by every one of the module's
// (zero or more) less-than-fully-qualified names (which we call `nickname'
// here), ordered by the nickname.

typedef struct {
    const char          *MR_nick_name;
    MR_Dlist            *MR_nick_layouts;
                        // The list entries are MR_ModuleLayouts.
} MR_Module_Nick;

static  MR_Module_Nick  *MR_module_nicks;
static  int             MR_module_nick_next = 0;
static  int             MR_module_nick_max  = 0;

static  int             MR_module_info_proc_count = 0;

#define INIT_MODULE_TABLE_SIZE  10

static  const MR_ModuleLayout
                        *MR_search_module_info_by_name(const char *name);
static  MR_Dlist        *MR_search_module_info_by_nickname(const char *name);
static  const MR_ModuleLayout
                        *MR_search_module_info_by_unique_name(FILE *fp,
                            const char *name);

static  void            MR_insert_module_info(const MR_ModuleLayout *module);
static  void            MR_process_matching_procedures_in_module(
                            const MR_ModuleLayout *module, MR_ProcSpec *spec,
                            void f(void *, const MR_ProcLayout *),
                            void *data);
static  void            MR_process_line_layouts(const MR_ModuleFileLayout
                            *file_layout, int line,
                            MR_file_line_callback callback_func,
                            int callback_arg, int *num_line_matches_ptr);

static  MR_bool         MR_module_in_arena(const char *name, char **names,
                            int num_names);

static  int             MR_compare_proc_layout_by_name(const void *ptr1,
                            const void *ptr2);
static  int             MR_compare_type_ctor_by_name(const void *ptr1,
                            const void *ptr2);
static  int             MR_compare_functor_by_name(const void *ptr1,
                            const void *ptr2);

static  MR_bool         MR_parse_trailing_number(char *start, char **end,
                            int *number);
static  void            MR_translate_double_underscores(char *str);
static  char            *MR_get_module_info_name(int slot);

typedef struct {
    MR_PredFunc MR_complete_pf;

                // The word to complete, with `__' translated into '.'.

    char        *MR_complete_name;
    size_t      MR_complete_name_len;
    MR_bool     MR_complete_name_is_qualified;

                // Slot number of a module for which we should return all
                // procedures as completions, -1 if there is none.

    int         MR_unambiguous_matching_module;

                // Length of the part of the word to skip when testing for
                // module qualified completions in the current module,
                // zero if we shouldn't test for module qualified completions
                // in the current module.

    int         MR_complete_word_matches_module;
    int         MR_complete_current_module;
    int         MR_complete_current_proc;
} MR_ProcCompleterData;

static  char    *MR_trace_proc_spec_completer_next(const char *word,
                    size_t word_len, MR_CompleterData *completer_data);
static  void    MR_trace_proc_spec_completer_init_module(
                    MR_ProcCompleterData *data);
static  char    *MR_trace_complete_proc(MR_ProcCompleterData *data);
static  char    *MR_format_proc_spec_completion(MR_PredFunc pred_or_func,
                    const char *module, const char *name);
static  void    MR_free_proc_completer_data(MR_CompleterData completer_data);

void
MR_register_all_modules_and_procs(FILE *fp, MR_bool verbose)
{
    static  MR_bool done = MR_FALSE;

    if (! done) {
        if (verbose) {
            fprintf(fp, "Registering debuggable procedures... ");
            fflush(fp);
        }

        MR_trace_init_modules();
        done = MR_TRUE;
        if (verbose) {
            fprintf(fp, "done.\n");
            if (MR_module_info_next == 0) {
                fprintf(fp, "There are no debuggable modules.\n");
            } else if (MR_module_info_next == 1) {
                fprintf(fp,
                    "There is one debuggable module, with %d procedures.\n",
                    MR_module_info_proc_count);
            } else {
                fprintf(fp, "There are %d debuggable modules, "
                    "with a total of %d procedures.\n",
                    MR_module_info_next, MR_module_info_proc_count);
            }
        }
    }
}

#define MR_INIT_EVENT_SET_TABLE_SIZE    10

void
MR_register_module_layout_real(const MR_ModuleLayout *module)
{
    // MR_register_module_layout_real should only be called from
    // the initialization function of a module, which should be
    // called only once. The check here whether the module layout
    // already exists in the table is really only for paranoia.

    if (MR_search_module_info_by_name(module->MR_ml_name) == NULL) {
        MR_insert_module_info(module);

        if (module->MR_ml_user_event_set_desc != NULL) {
            int                 i;
            MR_bool             found;
            const char          *event_set_name;
            MR_TraceEventSet    *trace_event_set;

            event_set_name = module->MR_ml_user_event_set_name;

            found = MR_FALSE;
            for (i = 0; i < MR_trace_event_set_next; i++) {
                if (MR_streq(MR_trace_event_sets[i].MR_tes_name,
                    event_set_name))
                {
                    trace_event_set = &MR_trace_event_sets[i];
                    if (MR_strdiff(trace_event_set->MR_tes_desc,
                        module->MR_ml_user_event_set_desc))
                    {
                        trace_event_set->MR_tes_is_consistent = MR_FALSE;
                        fprintf(MR_mdb_out,
                            "The executable's modules were compiled with "
                            "inconsistent definitions of "
                            "user event set `%s'.\n",
                            event_set_name);
                    }

                    found = MR_TRUE;
                    break;
                }
            }

            if (!found) {
                MR_ensure_room_for_next(MR_trace_event_set,
                    MR_TraceEventSet, MR_INIT_EVENT_SET_TABLE_SIZE);
                trace_event_set =
                    &MR_trace_event_sets[MR_trace_event_set_next];
                MR_trace_event_set_next++;

                trace_event_set->MR_tes_name = event_set_name;
                trace_event_set->MR_tes_desc =
                    module->MR_ml_user_event_set_desc;
                trace_event_set->MR_tes_is_consistent = MR_TRUE;
                trace_event_set->MR_tes_num_specs =
                    module->MR_ml_num_user_event_specs;
                trace_event_set->MR_tes_specs =
                    module->MR_ml_user_event_specs;
                trace_event_set->MR_tes_event_set =
                    MR_read_event_set("no input file",
                        trace_event_set->MR_tes_desc);

                if (trace_event_set->MR_tes_event_set == NULL) {
                    fprintf(MR_mdb_out,
                        "Internal error: could not parse "
                        "the specification of event set `%s'.\n",
                        event_set_name);
                }

                if (MR_trace_event_sets_max_num_attr <
                    module->MR_ml_user_event_max_num_attr)
                {
                    MR_trace_event_sets_max_num_attr =
                        module->MR_ml_user_event_max_num_attr;
                }

            }
        }
    }
}

static const MR_ModuleLayout *
MR_search_module_info_by_name(const char *name)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_module_info_next, slot, found,
        strcmp(MR_module_infos[slot]->MR_ml_name, name));
    if (found) {
        return MR_module_infos[slot];
    } else {
        return NULL;
    }
}

static MR_Dlist *
MR_search_module_info_by_nickname(const char *name)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_module_nick_next, slot, found,
        strcmp(MR_module_nicks[slot].MR_nick_name, name));
    if (found) {
        return MR_module_nicks[slot].MR_nick_layouts;
    } else {
        return NULL;
    }
}

static const MR_ModuleLayout *
MR_search_module_info_by_unique_name(FILE *fp, const char *name)
{
    const MR_ModuleLayout   *module;
    const MR_Dlist          *modules;
    const MR_Dlist          *element_ptr;

    module = MR_search_module_info_by_name(name);
    if (module == NULL) {
        modules = MR_search_module_info_by_nickname(name);
        if (modules == NULL) {
            fprintf(fp,
                "There is no debugging info about module `%s'\n", name);
            return NULL;
        } else if (MR_dlist_length(modules) != 1) {
            fprintf(fp, "Module name `%s' is ambiguous.\n", name);
            fprintf(fp, "The matches are:\n");
            MR_for_dlist (element_ptr, modules) {
                module = (const MR_ModuleLayout *) MR_dlist_data(element_ptr);
                fprintf(fp, "%s\n", module->MR_ml_name);
            }

            return NULL;
        } else {
            module = (const MR_ModuleLayout *) MR_dlist_first(modules);
        }
    }

    return module;
}

static void
MR_insert_module_info(const MR_ModuleLayout *module)
{
    int         slot;
    MR_bool     found;
    const char  *nickname;

    MR_insert_module_info_into_module_table(module);

    MR_module_info_proc_count += module->MR_ml_proc_count;

    nickname = strchr(module->MR_ml_name, '.');
    while (nickname != NULL) {
        nickname++; // Step over the '.'.
        MR_bsearch(MR_module_nick_next, slot, found,
            strcmp(MR_module_nicks[slot].MR_nick_name, nickname));
        if (found) {
            MR_module_nicks[slot].MR_nick_layouts =
                MR_dlist_addtail(MR_module_nicks[slot].MR_nick_layouts,
                    module);
        } else {
            MR_GC_ensure_room_for_next(MR_module_nick, MR_Module_Nick,
                INIT_MODULE_TABLE_SIZE, NULL);
            MR_prepare_insert_into_sorted(MR_module_nicks, MR_module_nick_next,
                slot, strcmp(MR_module_nicks[slot].MR_nick_name, nickname));
            MR_module_nicks[slot].MR_nick_name = nickname;
            MR_module_nicks[slot].MR_nick_layouts = MR_dlist_makelist(module);
        }

        nickname = strchr(nickname, '.');
    }
}

void
MR_process_file_line_layouts(const char *file, int line,
    MR_file_line_callback callback_func, int callback_arg,
    int *num_file_matches_ptr, int *num_line_matches_ptr)
{
    const MR_ModuleFileLayout   *file_layout;
    size_t                      i;
    int                         j;

    for (i = 0; i < MR_module_info_next; i++) {
        for (j = 0; j < MR_module_infos[i]->MR_ml_filename_count; j++) {
            file_layout = MR_module_infos[i]->MR_ml_module_file_layout[j];
            if (MR_streq(file_layout->MR_mfl_filename, file)) {
                *num_file_matches_ptr = *num_file_matches_ptr + 1;
                MR_process_line_layouts(file_layout, line, callback_func,
                    callback_arg, num_line_matches_ptr);
            }
        }
    }
}

static void
MR_process_line_layouts(const MR_ModuleFileLayout *file_layout, int line,
    MR_file_line_callback callback_func, int callback_arg,
    int *num_line_matches_ptr)
{
    int         k;
    MR_bool     found;

    MR_bsearch(file_layout->MR_mfl_label_count, k, found,
        file_layout->MR_mfl_label_lineno[k] - line);

    if (found) {
        // The binary search found *one* label with the given linenumber.
        // We now find the *first* such label, ...

        while (k > 0 && file_layout->MR_mfl_label_lineno[k - 1] == line) {
            k--;
        }

        // ... and then do a callback for *all* such labels.

        while (k < file_layout->MR_mfl_label_count
            && file_layout->MR_mfl_label_lineno[k] == line)
        {
            (*callback_func)(file_layout->MR_mfl_label_layout[k],
                callback_arg);
            k++;
            *num_line_matches_ptr = *num_line_matches_ptr + 1;
        }
    }
}

void
MR_dump_module_tables(FILE *fp, MR_bool separate, MR_bool uci,
    char *module_name)
{
    const MR_ModuleLayout   *module;
    const MR_ProcLayout     *proc;
    size_t                  i;
    int                     j;

    if (module_name != NULL) {
        module = MR_search_module_info_by_unique_name(fp, module_name);
        if (module == NULL) {
            // The error message has already been printed.
            return;
        }
    } else {
        module = NULL;
    }

    for (i = 0; i < MR_module_info_next; i++) {
        if (module == NULL || module == MR_module_infos[i]) {
            for (j = 0; j < MR_module_infos[i]->MR_ml_proc_count; j++) {
                proc = MR_module_infos[i]->MR_ml_procs[j];
                if (uci || !MR_PROC_LAYOUT_IS_UCI(proc)) {
                    if (separate) {
                        MR_print_proc_separate(fp, proc);
                    } else {
                        MR_print_proc_id(fp, proc);
                    }

                    fprintf(fp, "\n");
                }
            }
        }
    }
}

void
MR_dump_module_list(FILE *fp)
{
    size_t     i;

    fprintf(fp, "List of debuggable modules\n\n");
    for (i = 0; i < MR_module_info_next; i++) {
        fprintf(fp, "%s\n", MR_module_infos[i]->MR_ml_name);
    }
}

void
MR_dump_module_procs(FILE *fp, const char *name)
{
    const MR_ModuleLayout   *module;
    MR_ConstString          decl_module;
    int                     i;

    module = MR_search_module_info_by_unique_name(fp, name);
    if (module == NULL) {
        // The error message has already been printed.
        return;
    }

    fprintf(fp, "List of procedures in module `%s'\n\n", name);
    for (i = 0; i < module->MR_ml_proc_count; i++) {
        decl_module = MR_get_proc_decl_module(module->MR_ml_procs[i]);
        // Only show procs which are declared in the module.
        if (MR_streq(decl_module, module->MR_ml_name)) {
            MR_print_proc_id_and_nl(fp, module->MR_ml_procs[i]);
        }
    }
}

static MR_bool
MR_module_in_arena(const char *name, char **names, int num_names)
{
    int i;

    if (num_names == 0) {
        return MR_TRUE;
    }

    for (i = 0; i < num_names; i++) {
        if (MR_streq(name, names[i])) {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

#define MR_proc_compare_name(proc1, proc2)                              \
    strcmp(proc1->MR_sle_user.MR_user_name,                             \
        proc2->MR_sle_user.MR_user_name)

#define MR_proc_compare_module_name(proc1, proc2)                       \
    strcmp(proc1->MR_sle_user.MR_user_decl_module,                      \
        proc2->MR_sle_user.MR_user_decl_module)

#define MR_proc_compare_pf(proc1, proc2)                                \
    ((int) proc1->MR_sle_user.MR_user_pred_or_func -                    \
        (int) proc2->MR_sle_user.MR_user_pred_or_func)

#define MR_proc_compare_arity(proc1, proc2)                             \
    (MR_sle_user_adjusted_arity(proc1) - MR_sle_user_adjusted_arity(proc2))

#define MR_proc_compare_mode(proc1, proc2)                              \
    (proc1->MR_sle_user.MR_user_mode - proc2->MR_sle_user.MR_user_mode)

#define MR_proc_same_name(proc1, proc2)                                 \
    (MR_proc_compare_name(proc1, proc2) == 0)

#define MR_proc_same_module_name(proc1, proc2)                          \
    (MR_proc_compare_module_name(proc1, proc2) == 0)

#define MR_proc_same_pf(proc1, proc2)                                   \
    (MR_proc_compare_pf(proc1, proc2) == 0)

#define MR_proc_same_arity(proc1, proc2)                                \
    (MR_proc_compare_arity(proc1, proc2) == 0)

#define MR_proc_same_name_module_pf_arity(proc1, proc2)                 \
    (MR_proc_same_name(proc1, proc2) &&                                 \
    MR_proc_same_module_name(proc1, proc2) &&                           \
    MR_proc_same_pf(proc1, proc2) &&                                    \
    MR_proc_same_arity(proc1, proc2))

static int
MR_compare_proc_layout_by_name(const void *ptr1, const void *ptr2)
{
    const MR_ProcLayout     **proc_addr1;
    const MR_ProcLayout     **proc_addr2;
    const MR_ProcLayout     *proc1;
    const MR_ProcLayout     *proc2;
    int                     result;

    proc_addr1 = (const MR_ProcLayout **) ptr1;
    proc_addr2 = (const MR_ProcLayout **) ptr2;
    proc1 = *proc_addr1;
    proc2 = *proc_addr2;

    result = MR_proc_compare_name(proc1, proc2);
    if (result != 0) {
        return result;
    }

    // Return equal only if the module name, pred_or_func and the arity
    // are the same as well, in order to group all procedures of a predicate
    // or function together.

    result = MR_proc_compare_module_name(proc1, proc2);
    if (result != 0) {
        return result;
    }

    result = MR_proc_compare_pf(proc1, proc2);
    if (result != 0) {
        return result;
    }

    result = MR_proc_compare_arity(proc1, proc2);
    if (result != 0) {
        return result;
    }

    return MR_proc_compare_mode(proc1, proc2);
}

#define MR_type_compare_name(type1, type2)                              \
    strcmp(type1->MR_type_ctor_name, type2->MR_type_ctor_name)

#define MR_type_compare_module_name(type1, type2)                       \
    strcmp(type1->MR_type_ctor_module_name, type2->MR_type_ctor_module_name)

#define MR_type_compare_arity(type1, type2)                              \
    (type1->MR_type_ctor_arity - type2->MR_type_ctor_arity)

#define MR_type_same_name(type1, type2)                                  \
    (MR_type_compare_name(type1, type2) == 0)

#define MR_type_same_module_name(type1, type2)                           \
    (MR_type_compare_module_name(type1, type2) == 0)

static int
MR_compare_type_ctor_by_name(const void *ptr1, const void *ptr2)
{
    const MR_TypeCtorInfo   *type_ctor_addr1;
    const MR_TypeCtorInfo   *type_ctor_addr2;
    MR_TypeCtorInfo         type_ctor1;
    MR_TypeCtorInfo         type_ctor2;
    int                     result;

    type_ctor_addr1 = (const MR_TypeCtorInfo *) ptr1;
    type_ctor_addr2 = (const MR_TypeCtorInfo *) ptr2;
    type_ctor1 = *type_ctor_addr1;
    type_ctor2 = *type_ctor_addr2;
    result = MR_type_compare_name(type_ctor1, type_ctor2);
    if (result != 0) {
        return result;
    }

    result = MR_type_compare_module_name(type_ctor1, type_ctor2);
    if (result != 0) {
        return result;
    }

    return MR_type_compare_arity(type_ctor1, type_ctor2);
}

typedef struct {
    const char  *MR_functor_name;
    int         MR_functor_arity;
    const char  *MR_functor_type_module;
    const char  *MR_functor_type_name;
    int         MR_functor_type_arity;
} MR_FunctorTypeCtor;

#define MR_functor_compare_name(functor1, functor2)                     \
    strcmp((functor1).MR_functor_name, (functor2).MR_functor_name)

#define MR_functor_same_name(functor1, functor2)                        \
    (MR_functor_compare_name(functor1, functor2) == 0)

#define MR_functor_compare_arity(functor1, functor2)                    \
    ((functor1).MR_functor_arity - (functor2).MR_functor_arity)

#define MR_functor_compare_type_module(functor1, functor2)              \
    strcmp((functor1).MR_functor_type_module,                           \
        (functor2).MR_functor_type_module)

#define MR_functor_compare_type_name(functor1, functor2)                \
    strcmp((functor1).MR_functor_type_name, (functor2).MR_functor_type_name)

#define MR_functor_compare_type_arity(functor1, functor2)               \
    ((functor1).MR_functor_type_arity - (functor2).MR_functor_type_arity)

static int
MR_compare_functor_by_name(const void *ptr1, const void *ptr2)
{
    const MR_FunctorTypeCtor    *addr1;
    const MR_FunctorTypeCtor    *addr2;
    int                         result;

    addr1 = (const MR_FunctorTypeCtor *) ptr1;
    addr2 = (const MR_FunctorTypeCtor *) ptr2;

    result = MR_functor_compare_name(*addr1, *addr2);
    if (result != 0) {
        return result;
    }

    result = MR_functor_compare_arity(*addr1, *addr2);
    if (result != 0) {
        return result;
    }

    result = MR_functor_compare_type_module(*addr1, *addr2);
    if (result != 0) {
        return result;
    }

    result = MR_functor_compare_type_name(*addr1, *addr2);
    if (result != 0) {
        return result;
    }

    return MR_functor_compare_type_arity(*addr1, *addr2);
}

void
MR_print_ambiguities(FILE *fp, MR_bool print_procs, MR_bool print_types,
    MR_bool print_functors, char **arena_module_names, int arena_num_modules)
{
    unsigned                    module_num;
    int                         proc_num;
    int                         type_num;
    int                         functor_num;
    int                         end_proc_num;
    int                         end_type_num;
    int                         end_functor_num;
    size_t                      num_procs;
    int                         num_all_types;
    int                         num_types;
    size_t                      num_functors;
    size_t                      next_proc_num;
    int                         procs_in_module;
    const MR_ModuleLayout       *module;
    const MR_ProcLayout         **procs;
    const MR_ProcLayout         *cur_proc;
    MR_TypeCtorInfo             *type_ctors;
    MR_TypeCtorInfo             type_ctor_info;
    MR_FunctorTypeCtor          *functors;
    MR_Dlist                    *type_ctor_list;
    MR_Dlist                    *element_ptr;
    MR_bool                     *report;
    MR_EnumFunctorDesc          **enum_functors;
    MR_DuFunctorDesc            **du_functors;
    MR_NotagFunctorDesc         *notag_functor;
    int                         num_distinct;
    int                         num_ambiguous;
    int                         num_ambiguous_total;
    int                         num_ambiguous_max;
    int                         i;

    // We compute each data structure regardless of the settings of
    // print_procs, print_types and print_functors because the implementation
    // of one may depend on the other; e.g. we calculate how many function
    // symbols to reserve space for while we build the types array.

    num_procs = 0;
    for (module_num = 0; module_num < MR_module_info_next; module_num++) {
        num_procs += MR_module_infos[module_num]->MR_ml_proc_count;
    }

    // num_procs is a conservative estimate of the number of user defined
    // procedures.

    procs = malloc(sizeof(const MR_ProcLayout *) * num_procs);
    if (procs == NULL) {
        fprintf(MR_mdb_err, "Error: could not allocate sufficient memory\n");
        return;
    }

    report = malloc(sizeof(MR_bool) * num_procs);
    if (report == NULL) {
        fprintf(MR_mdb_err, "Error: could not allocate sufficient memory\n");
        return;
    }

    next_proc_num = 0;
    for (module_num = 0; module_num < MR_module_info_next; module_num++) {
        module = MR_module_infos[module_num];
        if (MR_module_in_arena(module->MR_ml_name,
            arena_module_names, arena_num_modules))
        {
            procs_in_module = MR_module_infos[module_num]->MR_ml_proc_count;
            for (proc_num = 0; proc_num < procs_in_module; proc_num++) {
                cur_proc = module->MR_ml_procs[proc_num];
                if (! MR_PROC_LAYOUT_IS_UCI(cur_proc)) {
                    procs[next_proc_num] = cur_proc;
                    next_proc_num++;
                }
            }
        }
    }

    num_procs = next_proc_num;
    qsort(procs, num_procs, sizeof(const MR_ProcLayout *),
        MR_compare_proc_layout_by_name);

    if (print_procs) {
        fprintf(fp, "Ambiguous procedure names:\n");
        num_ambiguous = 0;
        num_ambiguous_total = 0;
        num_ambiguous_max = 0;

        proc_num = 0;
        while (proc_num < num_procs) {
            end_proc_num = proc_num + 1;
            while (end_proc_num < num_procs &&
                MR_proc_same_name(procs[proc_num], procs[end_proc_num]))
            {
                end_proc_num++;
            }

            if (end_proc_num > proc_num + 1) {
                report[proc_num] = MR_TRUE;
                num_distinct = 1;

                for (i = proc_num + 1; i < end_proc_num; i++) {
                    if (MR_proc_same_name_module_pf_arity(procs[i-1],
                        procs[i]))
                    {
                        report[i] = MR_FALSE;
                    } else {
                        report[i] = MR_TRUE;
                        num_distinct++;
                    }
                }

                if (num_distinct > 1) {
                    num_ambiguous++;
                    num_ambiguous_total += (end_proc_num - proc_num);
                    if ((end_proc_num - proc_num) > num_ambiguous_max) {
                        num_ambiguous_max = end_proc_num - proc_num;
                    }

                    fprintf(fp, "\n");
                    for (i = proc_num; i < end_proc_num; i++) {
                        if (report[i]) {
                            fprintf(fp, "%s %s.%s/%d\n",
                                (procs[i]->MR_sle_user.MR_user_pred_or_func
                                    == MR_PREDICATE ? "pred" : "func"),
                                procs[i]->MR_sle_user.MR_user_decl_module,
                                procs[i]->MR_sle_user.MR_user_name,
                                MR_sle_user_adjusted_arity(procs[i]));
                        }
                    }
                }
            }

            proc_num = end_proc_num;
        }

        if (num_ambiguous == 0) {
            fprintf(fp, "\nNone\n\n");
        } else {
            fprintf(fp, "\nTotal: %d names used %d times, ",
                num_ambiguous, num_ambiguous_total);
            fprintf(fp, "maximum %d, average: %.2f\n\n",
                num_ambiguous_max,
                (float) num_ambiguous_total / (float) num_ambiguous);
        }
    }

    free(procs);
    free(report);

    type_ctor_list = MR_all_type_ctor_infos(&num_all_types);
    type_ctors = malloc(sizeof(MR_TypeCtorInfo) * num_all_types);
    if (type_ctors == NULL) {
        fprintf(MR_mdb_err, "Error: could not allocate sufficient memory\n");
        return;
    }

    num_functors = 0;
    type_num = 0;
    MR_for_dlist (element_ptr, type_ctor_list) {
        type_ctor_info = (MR_TypeCtorInfo) MR_dlist_data(element_ptr);
        if (MR_module_in_arena(type_ctor_info->MR_type_ctor_module_name,
            arena_module_names, arena_num_modules))
        {
            type_ctors[type_num] = type_ctor_info;

            switch (MR_type_ctor_rep(type_ctor_info)) {
                case MR_TYPECTOR_REP_ENUM:
                case MR_TYPECTOR_REP_ENUM_USEREQ:
                case MR_TYPECTOR_REP_DUMMY:
                case MR_TYPECTOR_REP_DU:
                case MR_TYPECTOR_REP_DU_USEREQ:
                case MR_TYPECTOR_REP_NOTAG:
                case MR_TYPECTOR_REP_NOTAG_USEREQ:
                case MR_TYPECTOR_REP_NOTAG_GROUND:
                case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
                    num_functors += type_ctor_info->MR_type_ctor_num_functors;
#if 0
                    // for debugging only
                    fprintf(fp,
                        "TYPE %s.%s/%" MR_INTEGER_LENGTH_MODIFIER "d\t%s\t%d\n",
                        type_ctors[type_num]->MR_type_ctor_module_name,
                        type_ctors[type_num]->MR_type_ctor_name,
                        type_ctors[type_num]->MR_type_ctor_arity,
                        MR_ctor_rep_name[
                            MR_type_ctor_rep(type_ctors[type_num])],
                        num_functors);
#endif
                    break;

                default:
                    break;
            }

            type_num++;
        }
    }

    num_types = type_num;
    qsort(type_ctors, num_types, sizeof(MR_TypeCtorInfo),
        MR_compare_type_ctor_by_name);

    if (print_types) {
        fprintf(fp, "Ambiguous type names:\n");
        num_ambiguous = 0;
        num_ambiguous_total = 0;
        num_ambiguous_max = 0;

        type_num = 0;
        while (type_num < num_types) {
            end_type_num = type_num + 1;
            while (end_type_num < num_types &&
                MR_type_same_name(type_ctors[type_num],
                    type_ctors[end_type_num]))
            {
                end_type_num++;
            }

            if (end_type_num > type_num + 1) {
                num_ambiguous++;
                num_ambiguous_total += (end_type_num - type_num);
                if ((end_type_num - type_num) > num_ambiguous_max) {
                    num_ambiguous_max = end_type_num - type_num;
                }

                fprintf(fp, "\n");

                for (i = type_num; i < end_type_num; i++) {
                    fprintf(fp, "%s.%s/%" MR_INTEGER_LENGTH_MODIFIER "d\n",
                        type_ctors[i]->MR_type_ctor_module_name,
                        type_ctors[i]->MR_type_ctor_name,
                        type_ctors[i]->MR_type_ctor_arity);
                }
            }

            type_num = end_type_num;
        }

        if (num_ambiguous == 0) {
            fprintf(fp, "\nNone\n\n");
        } else {
            fprintf(fp, "\nTotal: %d names used %d times, ",
                num_ambiguous, num_ambiguous_total);
            fprintf(fp, "maximum %d, average: %.2f\n\n",
                num_ambiguous_max,
                (float) num_ambiguous_total / (float) num_ambiguous);
        }
    }

    functors = malloc(sizeof(MR_FunctorTypeCtor) * num_functors);
    if (functors == NULL) {
        fprintf(MR_mdb_err, "Error: could not allocate sufficient memory\n");
        return;
    }

    functor_num = 0;
    for (type_num = 0; type_num < num_types; type_num++) {
        type_ctor_info = type_ctors[type_num];

#if 0
        // for debugging only
        fprintf(fp, "FUNCTYPE %s.%s/%" MR_INTEGER_LENGTH_MODIFIER "d\n",
            type_ctors[type_num]->MR_type_ctor_module_name,
            type_ctors[type_num]->MR_type_ctor_name,
            type_ctors[type_num]->MR_type_ctor_arity);
#endif

        switch (MR_type_ctor_rep(type_ctor_info)) {
            case MR_TYPECTOR_REP_ENUM:
            case MR_TYPECTOR_REP_ENUM_USEREQ:
            case MR_TYPECTOR_REP_DUMMY:
                enum_functors =
                    MR_type_ctor_functors(type_ctor_info).MR_functors_enum;
                i = 0;
                while (i < type_ctor_info->MR_type_ctor_num_functors) {
                    functors[functor_num].MR_functor_name =
                        enum_functors[i]->MR_enum_functor_name;
                    functors[functor_num].MR_functor_arity = 0;
                    functors[functor_num].MR_functor_type_module =
                        type_ctor_info->MR_type_ctor_module_name;
                    functors[functor_num].MR_functor_type_name =
                        type_ctor_info->MR_type_ctor_name;
                    functors[functor_num].MR_functor_type_arity =
                        type_ctor_info->MR_type_ctor_arity;
                    functor_num++;
                    i++;
                }
                break;

            case MR_TYPECTOR_REP_DU:
            case MR_TYPECTOR_REP_DU_USEREQ:
                du_functors =
                    MR_type_ctor_functors(type_ctor_info).MR_functors_du;
                i = 0;
                while (i < type_ctor_info->MR_type_ctor_num_functors) {
                    functors[functor_num].MR_functor_name =
                        du_functors[i]->MR_du_functor_name;
                    functors[functor_num].MR_functor_arity =
                        du_functors[i]->MR_du_functor_orig_arity;
                    functors[functor_num].MR_functor_type_module =
                        type_ctor_info->MR_type_ctor_module_name;
                    functors[functor_num].MR_functor_type_name =
                        type_ctor_info->MR_type_ctor_name;
                    functors[functor_num].MR_functor_type_arity =
                        type_ctor_info->MR_type_ctor_arity;
                    functor_num++;
                    i++;
                }
                break;

            case MR_TYPECTOR_REP_NOTAG:
            case MR_TYPECTOR_REP_NOTAG_USEREQ:
            case MR_TYPECTOR_REP_NOTAG_GROUND:
            case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
                notag_functor =
                    MR_type_ctor_functors(type_ctor_info).MR_functors_notag;
                functors[functor_num].MR_functor_name =
                    notag_functor->MR_notag_functor_name;
                functors[functor_num].MR_functor_arity = 1;
                functors[functor_num].MR_functor_type_module =
                    type_ctor_info->MR_type_ctor_module_name;
                functors[functor_num].MR_functor_type_name =
                    type_ctor_info->MR_type_ctor_name;
                functors[functor_num].MR_functor_type_arity =
                    type_ctor_info->MR_type_ctor_arity;
                functor_num++;
                break;

            default:
                // Other kinds of types do not have user-defined function
                // symbols.
                break;
        }
    }

    qsort(functors, num_functors, sizeof(MR_FunctorTypeCtor),
        MR_compare_functor_by_name);

    if (print_functors) {
        fprintf(fp, "Ambiguous function symbols:\n");
        num_ambiguous = 0;
        num_ambiguous_total = 0;
        num_ambiguous_max = 0;

        functor_num = 0;
        while (functor_num < num_functors) {
            end_functor_num = functor_num + 1;
            while (end_functor_num < num_functors &&
                MR_functor_same_name(functors[functor_num],
                    functors[end_functor_num]))
            {
                end_functor_num++;
            }

            if (end_functor_num > functor_num + 1) {
                num_ambiguous++;
                num_ambiguous_total += (end_functor_num - functor_num);
                if ((end_functor_num - functor_num) > num_ambiguous_max) {
                    num_ambiguous_max = end_functor_num - functor_num;
                }

                fprintf(fp, "\n");
                for (i = functor_num; i < end_functor_num; i++) {
                    fprintf(fp, "%s/%d ",
                        functors[i].MR_functor_name,
                        functors[i].MR_functor_arity);
                    fprintf(fp, "%s.%s/%d\n",
                        functors[i].MR_functor_type_module,
                        functors[i].MR_functor_type_name,
                        functors[i].MR_functor_type_arity);
                }
            }

            functor_num = end_functor_num;
        }

        if (num_ambiguous == 0) {
            fprintf(fp, "\nNone\n\n");
        } else {
            fprintf(fp, "\nTotal: %d names used %d times, ",
                num_ambiguous, num_ambiguous_total);
            fprintf(fp, "maximum %d, average: %.2f\n\n",
                num_ambiguous_max,
                (float) num_ambiguous_total / (float) num_ambiguous);
        }
    }

    free(type_ctors);
    free(functors);
}

MR_bool
MR_parse_proc_spec(char *str, MR_ProcSpec *spec)
{
    char    *dash;
    char    *end;
    int     n;
    size_t  len;

    spec->MR_proc_module = NULL;
    spec->MR_proc_name   = NULL;
    spec->MR_proc_arity  = -1;
    spec->MR_proc_mode   = -1;
    spec->MR_proc_prefix = (MR_ProcPrefix) -1;

    len = strlen(str);

    // Check for the optional trailing arity and mode number.
    // This also checks for filename:linenumber breakpoint specifiers.

    end = str + len - 1;
    if (MR_parse_trailing_number(str, &end, &n)) {
        if (end == str) {
            // The string contains only a number.
            return MR_FALSE;
        }
        end--;
        if (*end == ':') {
            // filename:linenumber
            return MR_FALSE;
        } else if (*end == '-') {
            spec->MR_proc_mode = n;

            // Avoid modifying the string until we're sure
            // the parse can't fail.

            dash = end;

            end--;
            if (MR_parse_trailing_number(str, &end, &n)) {
                if (end == str) {
                    // The string contains only a number.
                    return MR_FALSE;
                }
                end--;
                if (*end == '/') {
                    *end = '\0';
                    spec->MR_proc_arity = n;
                    end--;
                }
            }
            *dash = '\0';
        } else if (*end == '/') {
            *end = '\0';
            end--;
            spec->MR_proc_arity = n;
        }
    }

    if (MR_strneq(str, "pred*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_PRED;
        str += 5;
    } else if (MR_strneq(str, "func*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_FUNC;
        str += 5;
    } else if (MR_strneq(str, "unif*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_UNIF;
        str += 5;
    } else if (MR_strneq(str, "comp*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_COMP;
        str += 5;
    } else if (MR_strneq(str, "indx*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_INDX;
        str += 5;
    } else if (MR_strneq(str, "init*", 5)) {
        spec->MR_proc_prefix = MR_PREFIX_INIT;
        str += 5;
    }

    // Search backwards for the end of the final module qualifier.
    // There must be at least one character before the qualifier.

    while (end > str) {
        if (*end == '.' || (*end == '_' && *(end + 1) == '_')) {
            if (*end == '.') {
                spec->MR_proc_name = end + 1;
            } else {
                spec->MR_proc_name = end + 2;
            }

            if (strlen(spec->MR_proc_name) == 0) {
                spec->MR_proc_name = NULL;
            }

            // Convert all occurences of '__' to '.'.

            *end = '\0';
            MR_translate_double_underscores(str);

            spec->MR_proc_module = str;

            return MR_TRUE;
        } else {
            end--;
        }
    }

    // There was no module qualifier.
    spec->MR_proc_name = str;
    if (strlen(spec->MR_proc_name) == 0) {
        spec->MR_proc_name = NULL;
    }
    return MR_TRUE;
}

// Convert all occurrences of `__' to `.'.

static void
MR_translate_double_underscores(char *start)
{
    int double_underscores = 0;
    char    *str;

    str = start;
    while (*str) {
        if (*str == '_' && *(str + 1) == '_') {
            *(str - double_underscores) = '.';
            double_underscores++;
            str++;
        } else {
            *(str - double_underscores) = *str;
        }
        str++;
    }
    *(str - double_underscores) = '\0';
}

// Go backwards over a string starting at `end', stopping at `start',
// parsing the trailing integer and storing it in `*n'.
// On return, `*end' points to the start of the trailing number.
// If no number was found, `*end' is unchanged.

static MR_bool
MR_parse_trailing_number(char *start, char **end, int *number)
{
    MR_bool found_digit;
    int     power_of_10;
    char    *tmp_end;

    found_digit = MR_FALSE;
    power_of_10 = 1;
    *number = 0;

    tmp_end = *end + 1;
    while (tmp_end > start && MR_isdigit(*(tmp_end - 1))) {
        found_digit = MR_TRUE;
        *number += power_of_10 * (*(tmp_end - 1) - '0');
        power_of_10 *= 10;
        tmp_end--;
    }
    if (found_digit) {
        *end = tmp_end;
    }
    return found_digit;
}

#define MR_INIT_MATCH_PROC_SIZE     8

static void
MR_register_matches(void *data, const MR_ProcLayout *entry)
{
    MR_MatchesInfo *m;

    m = (MR_MatchesInfo *) data;
    MR_ensure_room_for_next(m->match_proc, const MR_ProcLayout *,
        MR_INIT_MATCH_PROC_SIZE);
    m->match_procs[m->match_proc_next] = entry;
    m->match_proc_next++;
}

MR_MatchesInfo
MR_search_for_matching_procedures(MR_ProcSpec *spec)
{
    MR_MatchesInfo m;

    m.match_procs = NULL;
    m.match_proc_max = 0;
    m.match_proc_next = 0;
    MR_process_matching_procedures(spec, MR_register_matches, &m);
    return m;
}

// This struct is for communication between
// MR_register_match and MR_search_for_matching_procedure.

typedef struct {
    const MR_ProcLayout     *matching_entry;
    MR_bool                 match_unique;
} MR_MatchInfo;

static void
MR_register_match(void *data, const MR_ProcLayout *entry)
{
    MR_MatchInfo   *m;

    m = (MR_MatchInfo *) data;
    if (m->matching_entry == NULL) {
        m->matching_entry = entry;
    } else {
        m->match_unique = MR_FALSE;
    }
}

const MR_ProcLayout *
MR_search_for_matching_procedure(MR_ProcSpec *spec, MR_bool *unique)
{
    MR_MatchInfo   m;

    m.matching_entry = NULL;
    m.match_unique = MR_TRUE;
    MR_process_matching_procedures(spec, MR_register_match, &m);
    *unique = m.match_unique;
    return m.matching_entry;
}

void
MR_process_matching_procedures(MR_ProcSpec *spec,
    void f(void *, const MR_ProcLayout *), void *data)
{
    if (spec->MR_proc_module != NULL) {
        const MR_ModuleLayout   *module;

        module = MR_search_module_info_by_name(spec->MR_proc_module);
        if (module != NULL) {
            MR_process_matching_procedures_in_module(module, spec, f, data);
        } else {
            const MR_Dlist  *modules;
            const MR_Dlist  *element_ptr;

            modules = MR_search_module_info_by_nickname(spec->MR_proc_module);
            MR_for_dlist (element_ptr, modules) {
                module = (const MR_ModuleLayout *) MR_dlist_data(element_ptr);
                MR_process_matching_procedures_in_module(module, spec, f,
                    data);
            }
        }
    } else {
        unsigned    i;

        for (i = 0; i < MR_module_info_next; i++) {
            MR_process_matching_procedures_in_module(MR_module_infos[i], spec,
                f, data);
        }
    }
}

#define match_user_proc_name(spec, cur)                                 \
    (((spec)->MR_proc_name == NULL) ||                                  \
    MR_streq((spec)->MR_proc_name, cur->MR_sle_user.MR_user_name))

#define match_user_proc_arity(spec, cur)                                \
    (((spec)->MR_proc_arity < 0) ||                                     \
    (spec)->MR_proc_arity == MR_sle_user_adjusted_arity(cur))

#define match_user_proc_mode(spec, cur)                                 \
    (((spec)->MR_proc_mode < 0) ||                                      \
    (spec)->MR_proc_mode == cur->MR_sle_user.MR_user_mode)

#define match_user_proc_pf(spec, cur)                                   \
    (((int) (spec)->MR_proc_prefix < 0) ||                              \
    ( ( ((spec)->MR_proc_prefix == MR_PREFIX_PRED) &&                   \
        cur->MR_sle_user.MR_user_pred_or_func == MR_PREDICATE) ||       \
      ( ((spec)->MR_proc_prefix == MR_PREFIX_FUNC) &&                   \
        cur->MR_sle_user.MR_user_pred_or_func == MR_FUNCTION)           \
    ))

#define match_uci_type_name(spec, cur)                                  \
    (((spec)->MR_proc_name == NULL) ||                                  \
    MR_streq((spec)->MR_proc_name, cur->MR_sle_uci.MR_uci_type_name))

#define match_uci_type_arity(spec, cur)                                 \
    (((spec)->MR_proc_arity < 0) ||                                     \
    (spec)->MR_proc_arity == cur->MR_sle_uci.MR_uci_type_arity)

#define match_uci_proc_mode(spec, cur)                                  \
    (((spec)->MR_proc_mode < 0) ||                                      \
    (spec)->MR_proc_mode == cur->MR_sle_uci.MR_uci_mode)

#define match_uci_pred_name(spec, cur)                                  \
    (((int) (spec)->MR_proc_prefix < 0) ||                              \
    ( ( ((spec)->MR_proc_prefix == MR_PREFIX_UNIF) &&                   \
        MR_streq(cur->MR_sle_uci.MR_uci_pred_name, "__Unify__")) ||     \
      ( ((spec)->MR_proc_prefix == MR_PREFIX_COMP) &&                   \
        MR_streq(cur->MR_sle_uci.MR_uci_pred_name, "__Compare__")) ||   \
      ( ((spec)->MR_proc_prefix == MR_PREFIX_INDX) &&                   \
        MR_streq(cur->MR_sle_uci.MR_uci_pred_name, "__Index__"))        \
    ))

static void
MR_process_matching_procedures_in_module(const MR_ModuleLayout *module,
    MR_ProcSpec *spec, void f(void *, const MR_ProcLayout *), void *data)
{
    const MR_ProcLayout     *proc;
    int                     j;

    for (j = 0; j < module->MR_ml_proc_count; j++) {
        proc = module->MR_ml_procs[j];
        if (MR_PROC_LAYOUT_IS_UCI(proc)) {
            if (match_uci_type_name(spec, proc) &&
                match_uci_type_arity(spec, proc) &&
                match_uci_proc_mode(spec, proc) &&
                match_uci_pred_name(spec, proc))
            {
                f(data, proc);
            }
        } else {
            if (match_user_proc_name(spec, proc) &&
                match_user_proc_arity(spec, proc) &&
                match_user_proc_mode(spec, proc) &&
                match_user_proc_pf(spec, proc))
            {
                f(data, proc);
            }
        }
    }
}

void
MR_filter_user_preds(MR_MatchesInfo *matches)
{
    const MR_ProcLayout     *entry;
    int                     filter_pos;
    size_t                  i;

    filter_pos = 0;
    for(i = 0; i < matches->match_proc_next; i++) {
        entry = matches->match_procs[i];
        if (!MR_PROC_LAYOUT_IS_UCI(entry) &&
            (entry->MR_sle_user).MR_user_mode == 0)
        {
            matches->match_procs[filter_pos] = entry;
            filter_pos++;
        }
    }
    matches->match_proc_next = filter_pos;
}

MR_CompleterList *
MR_trace_module_completer(const char *word, size_t word_len)
{
    return MR_trace_sorted_array_completer(word, word_len, MR_module_info_next,
        MR_get_module_info_name);
}

static char *
MR_get_module_info_name(int slot)
{
    return (char *) MR_module_infos[slot]->MR_ml_name;
}

MR_CompleterList *
MR_trace_proc_spec_completer(const char *word, size_t word_len)
{
    MR_ProcCompleterData    *data;
    int                     slot;
    MR_bool                 found;

    MR_register_all_modules_and_procs(MR_mdb_out, MR_FALSE);

    data = MR_NEW(MR_ProcCompleterData);

    if (MR_strneq(word, "pred*", 5)) {
        data->MR_complete_pf = MR_PREDICATE;
        word += 5;
    } else if (MR_strneq(word, "func*", 5)) {
        data->MR_complete_pf = MR_FUNCTION;
        word += 5;
    } else {
        data->MR_complete_pf = -1;
    }

    data->MR_complete_name = MR_copy_string(word);
    MR_translate_double_underscores(data->MR_complete_name);
    data->MR_complete_name_len = strlen(data->MR_complete_name);
    data->MR_complete_name_is_qualified =
        strchr(data->MR_complete_name, '.') != NULL;
    data->MR_complete_word_matches_module = 0;
    data->MR_complete_current_module = -1;
    data->MR_complete_current_proc= -1;

    // Handle the case where the word matches the first part of a module name.
    // If the word unambiguously determines the module name, we want to return
    // module qualified completions for all the procedures in that module.
    // Otherwise, we just complete on the names of all of the matching modules
    // and unqualified procedure names.
    //
    // For example, given word to complete `f' and modules `foo' and `bar',
    // we want to return all the procedures in module `foo' as completions,
    // as well as all procedures whose unqualified names begin with `f'.
    // Given word to complete `foo.' and modules `foo' and `foo.bar' we want to
    // return `foo.bar.' and all the procedures in module `foo' as completions.
    //
    // We don't complete on the names of special (unify, index or compare)
    // predicates.

    MR_bsearch(MR_module_info_next, slot, found,
        strncmp(MR_module_infos[slot]->MR_ml_name,
            data->MR_complete_name, data->MR_complete_name_len));
    if (found) {
        data->MR_unambiguous_matching_module = slot;
        if (slot > 0 &&
            MR_strneq(MR_module_infos[slot - 1]->MR_ml_name,
                data->MR_complete_name, data->MR_complete_name_len))
        {
            data->MR_unambiguous_matching_module = -1;
        }
        if (slot < MR_module_info_next - 1 &&
            MR_strneq(MR_module_infos[slot + 1]->MR_ml_name,
                data->MR_complete_name, data->MR_complete_name_len))
        {
            data->MR_unambiguous_matching_module = -1;
        }
    } else {
        data->MR_unambiguous_matching_module = -1;
    }

    return MR_new_completer_elem(MR_trace_proc_spec_completer_next,
        (MR_CompleterData) data, MR_free_proc_completer_data);
}

static char *
MR_trace_proc_spec_completer_next(const char *dont_use_this_word,
    size_t dont_use_this_len, MR_CompleterData *completer_data)
{
    MR_ProcCompleterData    *data;
    char                    *name;
    size_t                  name_len;
    const char              *module_name;
    char                    *completion;

    data = (MR_ProcCompleterData *) *completer_data;

    name = data->MR_complete_name;
    name_len = data->MR_complete_name_len;

try_completion:
    if (data->MR_complete_current_module == -1 ||
        data->MR_complete_current_proc == -1 ||
        data->MR_complete_current_proc >=
            MR_module_infos[data->MR_complete_current_module]->
                MR_ml_proc_count)
    {
        // Move on to the next module.

        data->MR_complete_current_module++;
        if (data->MR_complete_current_module >= MR_module_info_next) {
            return NULL;
        }
        MR_trace_proc_spec_completer_init_module(data);

        // Complete on the module name if we aren't finding
        // qualified completions in this module.

        module_name = MR_module_infos[data->MR_complete_current_module]
            ->MR_ml_name;
        if (data->MR_complete_word_matches_module == 0 &&
            MR_strneq(name, module_name, name_len))
        {
            return MR_format_proc_spec_completion(data->MR_complete_pf,
                module_name, "");
        } else {
            goto try_completion;
        }
    } else {
        // Complete on the next procedure in the current module.

        completion = MR_trace_complete_proc(data);

        if (completion != NULL) {
            return completion;
        } else {
            goto try_completion;
        }
    }
}

// Set up the completer data for processing a module.

static void
MR_trace_proc_spec_completer_init_module(MR_ProcCompleterData *data)
{
    char    *name;
    size_t  name_len;
    char    *module_name;
    size_t  module_name_len;

    name = data->MR_complete_name;
    name_len = data->MR_complete_name_len;

    module_name = (char *)
        MR_module_infos[data->MR_complete_current_module]->MR_ml_name;
    module_name_len = strlen(module_name);

    // Work out whether we should find qualified completions
    // for procedures in this module.

    if (MR_strneq(module_name, name, module_name_len)
        && name_len > module_name_len
        && name[module_name_len] == '.'
        && strchr(name + module_name_len + 1, '.') == NULL)
    {
        // The name to complete matches the module name completely.
        // When searching for qualified completions skip past
        // the module name and the trailing '.'.

        data->MR_complete_word_matches_module = module_name_len + 1;
    } else if (data->MR_complete_current_module ==
        data->MR_unambiguous_matching_module)
    {
        // The name to complete matches the module name partially,
        // and does not match any other module name. We will be
        // matching all procedures, use the empty string as the
        // name to match against.

        data->MR_complete_word_matches_module = name_len;
    } else {
        data->MR_complete_word_matches_module = 0;
    }

    // If the name to complete is qualified, we should only
    // complete on procedures if the module name matches.

    if (data->MR_complete_name_is_qualified &&
        data->MR_complete_word_matches_module == 0)
    {
        data->MR_complete_current_proc = -1;
    } else {
        data->MR_complete_current_proc = 0;
    }
}

// Check whether the current procedure matches the word to be completed.
// To do: complete on arity and mode number.

static char *
MR_trace_complete_proc(MR_ProcCompleterData *data)
{
    char                    *completion;
    char                    *name;
    size_t                  name_len;
    char                    *unqualified_name;
    size_t                  unqualified_name_len;
    char                    *complete_module;
    const MR_ModuleLayout   *module_layout;
    const MR_ProcLayout     *proc_layout;

    name = data->MR_complete_name;
    name_len = data->MR_complete_name_len;

    unqualified_name = name + data->MR_complete_word_matches_module;
    unqualified_name_len = name_len - data->MR_complete_word_matches_module;

    module_layout = MR_module_infos[data->MR_complete_current_module];
    proc_layout = module_layout->MR_ml_procs[data->MR_complete_current_proc];

    if (
        ! MR_PROC_LAYOUT_IS_UCI(proc_layout) &&
        ( data->MR_complete_pf == -1 ||
          proc_layout->MR_sle_user.MR_user_pred_or_func == data->MR_complete_pf
        ) &&
        MR_strneq(proc_layout->MR_sle_user.MR_user_name, unqualified_name,
            unqualified_name_len))
    {
        if (data->MR_complete_word_matches_module != 0) {
            complete_module = (char *) module_layout->MR_ml_name;
        } else {
            complete_module = NULL;
        }
        completion = MR_format_proc_spec_completion(data->MR_complete_pf,
            complete_module, proc_layout->MR_sle_user.MR_user_name);
    } else {
        completion = NULL;
    }

    // Move on to the next procedure in the current module.

    data->MR_complete_current_proc++;

    if (data->MR_complete_word_matches_module != 0
        && data->MR_complete_current_proc >= module_layout->MR_ml_proc_count
        && ! data->MR_complete_name_is_qualified)
    {
        // We have finished checking for module qualified completions
        // in this module, now check for unqualified completions
        // if the word to complete doesn't contain a qualifier.

        data->MR_complete_word_matches_module = 0;
        data->MR_complete_current_proc = 0;
    }

    return completion;
}

static char *
MR_format_proc_spec_completion(MR_PredFunc pred_or_func,
    const char *module, const char *name)
{
    size_t  size;
    size_t  module_len;
    int     offset;
    char    *completion;

    size = strlen(name);
    if (pred_or_func != -1) {
        size += 5;
    }
    if (module != NULL) {
        // +1 for the '.'
        module_len = strlen(module);
        size += module_len + 1;
    } else {
        module_len = 0;     // Avoid a compiler warning.
    }
    completion = MR_malloc(size + 1);

    offset = 0;
    if (pred_or_func == MR_PREDICATE) {
        strcpy(completion, "pred*");
        offset += 5;
    } else if (pred_or_func == MR_FUNCTION) {
        strcpy(completion, "func*");
        offset += 5;
    }

    if (module != NULL) {
        strcpy(completion + offset, module);
        offset += module_len;
        completion[offset] = '.';
        offset++;
    }

    strcpy(completion + offset, name);

    return completion;
}

static void
MR_free_proc_completer_data(MR_CompleterData completer_data)
{
    MR_ProcCompleterData    *data;

    data = (MR_ProcCompleterData *) completer_data;

    MR_free(data->MR_complete_name);
    MR_free(data);
}

void
MR_print_proc_id_and_nl(FILE *fp, const MR_ProcLayout *entry_layout)
{
    MR_print_proc_id(fp, entry_layout);
    fprintf(fp, "\n");
}

MR_ConstString
MR_get_proc_decl_module(const MR_ProcLayout *proc)
{
    if (MR_PROC_LAYOUT_IS_UCI(proc)) {
        return (&proc->MR_sle_uci)->MR_uci_type_module;
    } else {
        return (&proc->MR_sle_user)->MR_user_decl_module;
    }
}

void
MR_print_pred_id_and_nl(FILE *fp, const MR_ProcLayout *entry_layout)
{
    MR_print_pred_id(fp, entry_layout);
    fprintf(fp, "\n");
}

void
MR_proc_layout_stats(FILE *fp)
{
    const MR_ModuleLayout       *module_layout;
    const MR_ProcLayout         *proc_layout;
    unsigned                    module_num;
    int                         proc_num;
    MR_Determinism              detism;
    int                         total;
    int                         histogram[MR_DETISM_MAX + 1];

    total = 0;
    for (detism = 0; detism <= MR_DETISM_MAX; detism++) {
        histogram[detism] = 0;
    }

    for (module_num = 0; module_num < MR_module_info_next; module_num++) {
        module_layout = MR_module_infos[module_num];

        for (proc_num = 0;
            proc_num < module_layout->MR_ml_proc_count;
            proc_num++)
        {
            proc_layout = module_layout->MR_ml_procs[proc_num];

            total++;
            if (0 <= proc_layout->MR_sle_detism &&
                proc_layout->MR_sle_detism <= MR_DETISM_MAX)
            {
                histogram[proc_layout->MR_sle_detism]++;
            }
        }
    }

    for (detism = 0; detism <= MR_DETISM_MAX; detism++) {
        if (histogram[detism] > 0) {
            fprintf(fp, "%-10s %10d (%5.2f%%)\n", MR_detism_names[detism],
                histogram[detism], ((float) 100 * histogram[detism]) / total);
        }
    }
    fprintf(fp, "%-10s %10d\n", "all ", total);
}

void
MR_label_layout_stats(FILE *fp)
{
    const MR_ModuleLayout       *module_layout;
    const MR_ModuleFileLayout   *file_layout;
    const MR_LabelLayout        *label_layout;
    unsigned                    module_num;
    int                         file_num;
    int                         label_num;
    MR_TracePort                port;
    int                         total;
    int                         histogram[MR_PORT_NUM_PORTS];
    int                         var_count_neg;
    int                         var_count_zero;
    int                         var_count_pos;
    int                         var_count_total;
    int                         no_long;
    int                         some_long;
    int                         long_total;

    var_count_neg  = 0;
    var_count_zero = 0;
    var_count_pos  = 0;
    no_long = 0;
    some_long = 0;

    total = 0;
    for (port = 0; port < MR_PORT_NUM_PORTS; port++) {
        histogram[port] = 0;
    }

    for (module_num = 0; module_num < MR_module_info_next; module_num++) {
        module_layout = MR_module_infos[module_num];

        for (file_num = 0;
            file_num < module_layout->MR_ml_filename_count;
            file_num++)
        {
            file_layout = module_layout->MR_ml_module_file_layout[file_num];

            for (label_num = 0;
                label_num < file_layout->MR_mfl_label_count;
                label_num++)
            {
                label_layout = file_layout->MR_mfl_label_layout[label_num];

                total++;
                if (0 <= label_layout->MR_sll_port &&
                    label_layout->MR_sll_port < MR_PORT_NUM_PORTS)
                {
                    histogram[label_layout->MR_sll_port]++;
                }

                if (label_layout->MR_sll_var_count < 0) {
                    var_count_neg++;
                } else if (label_layout->MR_sll_var_count == 0) {
                    var_count_zero++;
                } else {
                    var_count_pos++;
                }

                if (MR_long_desc_var_count(label_layout) > 0) {
                    some_long++;
                } else {
                    no_long++;
                }
            }
        }
    }

    for (port = 0; port < MR_PORT_NUM_PORTS; port++) {
        fprintf(fp, "%4s %10d (%5.2f%%)\n", MR_actual_port_names[port],
            histogram[port], ((float) 100 * histogram[port]) / total);
    }
    fprintf(fp, "%s %10d\n\n", "all ", total);

    var_count_total = var_count_neg + var_count_zero + var_count_pos;
    fprintf(fp, "var_count <0: %6d (%5.2f)\n",
        var_count_neg, (float) var_count_neg / (float) var_count_total);
    fprintf(fp, "var_count =0: %6d (%5.2f)\n",
        var_count_zero, (float) var_count_zero / (float) var_count_total);
    fprintf(fp, "var_count >0: %6d (%5.2f)\n\n",
        var_count_pos, (float) var_count_pos / (float) var_count_total);

    long_total = no_long + some_long;
    fprintf(fp, "no long:      %6d (%5.2f)\n",
        no_long, (float) no_long / (float) long_total);
    fprintf(fp, "some long:    %6d (%5.2f)\n\n",
        some_long, (float) some_long / (float) long_total);
}

void
MR_var_name_stats(FILE *fp)
{
    const MR_ModuleLayout       *module_layout;
    const MR_ProcLayout         *proc_layout;
    const MR_uint_least32_t     *var_names;
    unsigned                    module_num;
    int                         proc_num;
    int                         var_num;
    int                         num_var_nums;
    int                         total_string_table_bytes;
    int                         total_var_num_table_entries;
    int                         total_used_var_num_table_entries;
    int                         total_unused_var_num_table_entries;
    int                         total_num_procs;

    total_string_table_bytes = 0;
    total_var_num_table_entries = 0;
    total_used_var_num_table_entries = 0;
    total_num_procs = 0;

    for (module_num = 0; module_num < MR_module_info_next; module_num++) {
        module_layout = MR_module_infos[module_num];

        total_string_table_bytes += module_layout->MR_ml_string_table_size;

        for (proc_num = 0;
            proc_num < module_layout->MR_ml_proc_count;
            proc_num++)
        {
            proc_layout = module_layout->MR_ml_procs[proc_num];
            total_num_procs += 1;

            if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
                continue;
            }

            var_names = proc_layout->MR_sle_used_var_names;
            if (var_names != NULL) {
                num_var_nums = proc_layout->MR_sle_max_named_var_num + 1;

                total_var_num_table_entries += num_var_nums;
                for (var_num = 0; var_num < num_var_nums; var_num++) {
                    if (var_names[var_num] != 0) {
                        total_used_var_num_table_entries++;
                    }
                }
            }
        }
    }

    fprintf(fp, "%d modules, %d bytes in string tables, average %.2f\n",
        MR_module_info_next, total_string_table_bytes,
        (float) total_string_table_bytes / MR_module_info_next);
    fprintf(fp, "%d procedures, %d var numbers, average %.2f\n",
        total_num_procs, total_var_num_table_entries,
        (float) total_var_num_table_entries / total_num_procs);
    fprintf(fp, "%d procedures, %d used var numbers, average %.2f\n",
        total_num_procs, total_used_var_num_table_entries,
        (float) total_used_var_num_table_entries / total_num_procs);
    fprintf(fp, "%d var numbers, %d used, average %.2f%%\n",
        total_var_num_table_entries,
        total_used_var_num_table_entries,
        (float) 100 * total_used_var_num_table_entries /
            total_var_num_table_entries);
    total_unused_var_num_table_entries =
        total_var_num_table_entries - total_used_var_num_table_entries;
    fprintf(fp, "%d unused var numbers, %d bytes\n",
        total_unused_var_num_table_entries,
        4 * total_unused_var_num_table_entries);
}
