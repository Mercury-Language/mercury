// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2004-2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the declarations of the tables that contain
// the identities of the debuggable modules and their procedures.
//
// Main author: Zoltan Somogyi.

#ifndef MERCURY_TRACE_TABLES_H
#define MERCURY_TRACE_TABLES_H

#include    "mercury_stack_layout.h"
#include    "mercury_trace_completion.h"
#include    "mercury_event_spec.h"          // for MR_EventSet
#include    <stdio.h>

// MR_register_all_modules_and_procs gathers all available debugging info
// about the modules and procedures of the program into the module info table.
// If verbose is MR_TRUE, print progress and summary messages.

extern  void        MR_register_all_modules_and_procs(FILE *fp,
                        MR_bool verbose);

// MR_register_module_layout_real registers a module layout structure.
// It is called indirectly, through the function pointer
// MR_register_module_layout, by the module initialization code
// of modules compiled with debugging.

extern  void        MR_register_module_layout_real(
                        const MR_ModuleLayout *module);

// Every module that generates user defined events has a specification of the
// set of user events it was compiled with in its module layout structure.
// The debugger needs to know what these specifications are, and if they
// are consistent (which means that any two event sets with the same name
// must be identical).
//
// MR_trace_event_sets points to dynamically resizable array of
// MR_TraceEventSets, with MR_trace_event_set_max giving the current size
// of the array and MR_trace_event_set_next giving the number of elements
// currently filled in. The array is not sorted.
//
// The name field of an MR_TraceEventSet gives the name of the event set, while
// the string field will contain the string representation of the event set.
// The is_consistent field will be true iff all modules that use the event set
// with the same name have the same representation. The event_set field
// contains the parsed form of the string field; it is meaningful only if
// is_consistent is true.

typedef struct {
    const char              *MR_tes_name;
    const char              *MR_tes_desc;
    MR_bool                 MR_tes_is_consistent;
    MR_EventSet             MR_tes_event_set;
    int                     MR_tes_num_specs;
    const MR_UserEventSpec  *MR_tes_specs;
} MR_TraceEventSet;

extern  MR_TraceEventSet    *MR_trace_event_sets;
extern  int                 MR_trace_event_set_next;
extern  int                 MR_trace_event_set_max;

extern  MR_bool             MR_trace_event_sets_are_all_consistent;
extern  int                 MR_trace_event_sets_max_num_attr;

// MR_process_file_line_layouts searches all the module layout structures
// of the program for label layout structures corresponding to the given
// filename/linenumber combination. For all such labels, it calls the supplied
// callback function with a pointer to the label's layout structure and
// with the supplied integer callback argument.
//
// We return the number of times we find a file with the specified name
// in *num_file_matches_ptr, and the number of times we find a line
// with the specified number in such a file in *num_line_matches_ptr.
// If we find files with the specified name that do *not* contain
// the specified line number, we set *next_lower and *next_higher
// to the nearest lower and higher line numbers that the file *does*
// contain. If there are multiple such files, then we return only
// the highest next_lower and the lowest next_higher line number.

typedef void        (*MR_file_line_callback)(const MR_LabelLayout *, int);

extern  void        MR_process_file_line_layouts(const char *file, int line,
                        MR_file_line_callback callback_func, int callback_arg,
                        int *num_file_matches_ptr, int *num_line_matches_ptr,
                        int *next_lower, int *next_higher);

// These functions print (parts of) the module info table.
//
// MR_dump_module_tables lists all procedures in all modules, unless
// module is not NULL, in which case lists only procedures in the named module.
// Its output can be very big; it should be used only by developers,
// for debugging the debugger. The components of procedure names will be
// printed in separate fields if the separate argument is true, while the
// names of compiler-created (unify/compare/index etc) predicates will be
// printed if the uci argument is true.
//
// MR_dump_module_list lists the names of all the modules,
// while MR_dump_module_procs lists the names of all the procs in the named
// module. These are intended for ordinary, non-developer users.

extern  void        MR_dump_module_tables(FILE *fp, MR_bool separate,
                        MR_bool uci, char *module);

extern  void        MR_dump_module_list(FILE *fp);
extern  void        MR_dump_module_procs(FILE *fp, const char *name);

// Print the names of ambiguous procedures (predicates and functions), types,
// and/or function symbols. The ambiguity may exist because a predicate,
// function, type or (constructor) function symbol with that name is defined
// with more than one arity or in more than one module.
//
// If num_modules is positive, then the search for ambiguities should consider
// only predicates, functions, types and function symbols in the modules whose
// names appear in module_names[0] .. module_names[num_modules-1].

extern  void        MR_print_ambiguities(FILE *fp, MR_bool print_procs,
                        MR_bool print_types, MR_bool print_functors,
                        char **module_names, int num_modules);

// A procedure specification has several components, the meaning of which
// depends on whether the procedure is from a user defined procedure (user)
// or from a unify, compare, index or init procedure (uci).
//
// The meanings of the components are
//
//  the name of the module defining the procedure
//  the name of the predicate or function (user)
//      or the name of the type (uci)
//  the arity of the predicate or function (user)
//      or the arity of the type constructor (uci)
//  the mode of the predicate or function
//  whether the procedure is from a predicate or function (user)
//      or is a unify, compare or index procedure (uci)
//
// A NULL pointer for the string fields, and a negative number for the other
// fields signifies the absence of information about that field, which should
// therefore be treated as a wildcard.

typedef enum {
    MR_PREFIX_PRED,
    MR_PREFIX_FUNC,
    MR_PREFIX_UNIF,
    MR_PREFIX_COMP,
    MR_PREFIX_INDX,
    MR_PREFIX_INIT
} MR_ProcPrefix;

typedef struct {
    const char          *MR_proc_module;
    const char          *MR_proc_name;
    int                 MR_proc_arity;
    int                 MR_proc_mode;
    MR_ProcPrefix      MR_proc_prefix;
} MR_ProcSpec;

// Given a string containing the specification of a procedure in the form
//
//  [`pred*'|`func*']module:name/arity-mode
//
// in which some of the five components (but not the name) may be missing,
// parse it into the more usable form of a MR_ProcSpec. The original string
// may be overwritten in the process.
//
// Returns MR_TRUE if the string was correctly formed, and MR_FALSE otherwise.

extern  MR_bool MR_parse_proc_spec(char *str, MR_ProcSpec *spec);

// Search the tables for a procedure that matches the given specification.
// If no procedure matches, return NULL.
// If one procedure matches, return its layout structure,
// and set *unique to MR_TRUE.
// If more than one procedure matches, return the layout structure of one
// and set *unique to MR_FALSE.

extern  const MR_ProcLayout     *MR_search_for_matching_procedure(
                                    MR_ProcSpec *spec, MR_bool *unique);

// Search the tables for procedures that matches the given specification.
// Return their layout structures in the array in the match_procs field
// of the structure. The match_proc_next field says how many matches there are,
// and the match_proc_max field says how many entries the array has allocated
// for it.

typedef struct {
    const MR_ProcLayout     **match_procs;
    MR_Unsigned             match_proc_max;
    MR_Unsigned             match_proc_next;
} MR_MatchesInfo;

extern  MR_MatchesInfo MR_search_for_matching_procedures(MR_ProcSpec *spec);

// Filter out UCI procs and keep only mode number 0.

extern  void            MR_filter_user_preds(MR_MatchesInfo *matches);

// MR_process_matching_procedures(spec, f, data):
//
// For each procedure that matches the specification given by `spec',
// call `f(data, entry)', where `entry' is the entry layout for that procedure.
// The argument `data' is a `void *' which can be used to pass any other
// information needed by the function `f'.

extern  void            MR_process_matching_procedures(MR_ProcSpec *spec,
                            void f(void *, const MR_ProcLayout *), void *data);

// MR_print_proc_id_and_nl(fp, proc):
//
// Print the id of the procedure identified by proc, followed by a newline.

extern  void            MR_print_proc_id_and_nl(FILE *fp,
                            const MR_ProcLayout *proc);

// MR_print_pred_id_and_nl(fp, proc):
//
// Print the id of the predicate/function identified by proc,
// followed by a newline.

extern  void            MR_print_pred_id_and_nl(FILE *fp,
                            const MR_ProcLayout *proc);

// MR_get_proc_decl_module(proc):
//
// Return the module name the procedure is declared in.

extern  MR_ConstString  MR_get_proc_decl_module(const MR_ProcLayout *proc);

// MR_proc_layout_stats(fp):
//
// Prints statistics about the proc layout structures of the program.

extern  void            MR_proc_layout_stats(FILE *fp);

// MR_label_layout_stats(fp):
//
// Prints statistics about the label layout structures of the program.

extern  void            MR_label_layout_stats(FILE *fp);

// MR_var_name_stats(fp):
//
// Prints statistics about the space occupied by the variable names
// in the layout structures of the program.

extern  void            MR_var_name_stats(FILE *fp);

// A Readline completer for module names.
extern  MR_CompleterList    *MR_trace_module_completer(const char *, size_t);

// A Readline completer for procedure specifications.
extern  MR_CompleterList    *MR_trace_proc_spec_completer(const char *, size_t);

#endif  // not MERCURY_TRACE_TABLES_H
