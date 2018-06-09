// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1999-2008, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module looks after the debugger's information about the variables
// that are live at a given program point.
//
// When execution arrives at an event, the debugger should call the function
// MR_trace_init_point_vars to initialize this module's data structures
// to reflect the variables that are live at that event. During the processing
// of the various debugger commands while at that event, the debugger may
// call MR_trace_set_level or its MR_trace_set_level_from_layout variant
// zero or more times to change this module's notion of the "current" set
// of variables to refer instead to the variables that are live at the
// return address in a given ancestor. This module maintains its own record
// of what the current ancestor level is; the enquiry function
// MR_trace_current_level returns this information, while enquiry function
// MR_trace_current_level_details returns information about this level.
// Ancestor level 0 means the environment of the procedure call that generated
// the event.
//
// The debugger partitions the variables at a program point into three sets
// based on their type: those which are always printed, those which are always
// ignored (saved succips, void variables etc), and those which are optionally
// printed (typeinfos and typeclassinfos). The print_optionals argument of
// the following functions should be MR_TRUE iff you wish to print variables
// in the third set.
//
// The functions MR_trace_var_count, MR_trace_list_vars,
// MR_trace_return_var_info, MR_trace_headvar_num, MR_trace_parse_var_path,
// MR_trace_parse_browse_one, MR_trace_browse_one and MR_trace_browse_all
// all work in the context established by MR_trace_init_point_vars and
// possibly MR_trace_set_level.
//
// This context may say that there is no information available about
// the variables live at the current location (this is possible if the
// relevant module was not compiled with the right debugging flags).
// If this is the case, or if some other reason prevents these functions
// from carrying out their assigned tasks, most of these functions return
// a non-NULL string describing the problem; they return NULL if everything
// went OK. (MR_trace_set_level also returns a pointer to an error message
// -and refuses to change levels- if something goes wrong.)

#ifndef MERCURY_TRACE_VARS_H
#define MERCURY_TRACE_VARS_H

#include "mercury_types.h"              // for MR_Word etc
#include "mercury_stack_layout.h"       // for MR_LabelLayout etc
#include "mercury_type_info.h"          // for MR_TypeInfo
#include "mercury_trace_base.h"         // for MR_TracePort
#include "mercury_trace_browse.h"       // for MR_Browser
#include "mercury_trace_completion.h"   // for MR_CompleterList
#include <stdio.h>

typedef void    (*MR_Browser)(MR_Word type_info, MR_Word value,
                    MR_BrowseCallerType caller, MR_BrowseFormat format);

typedef void    (*MR_GoalBrowser)(MR_ConstString name, MR_Word arg_list,
                    MR_Word is_func, MR_BrowseCallerType caller,
                    MR_BrowseFormat format);

typedef enum {
    MR_VAR_SPEC_NUMBER,
    MR_VAR_SPEC_NAME,
    MR_VAR_SPEC_HELD_NAME,
    MR_VAR_SPEC_ATTRIBUTE
} MR_VarSpecKind;

typedef struct {
    MR_VarSpecKind      MR_var_spec_kind;
    MR_Unsigned         MR_var_spec_number; // valid if NUMBER
    const char          *MR_var_spec_name;  // valid if NAME, HELD_NAME
                                            // or ATTRIBUTE
} MR_VarSpec;

// This function converts a variable name or variable number to MR_VarSpec
// format.

extern  void        MR_convert_arg_to_var_spec(const char *word_spec,
                        MR_VarSpec *var_spec);

// Print the given MR_VarSpec.

extern  void        MR_print_var_spec(FILE *fp, MR_VarSpec *var_spec);

// These functions are documented near the top of this file.

extern  void        MR_trace_init_point_vars(const MR_LabelLayout *top_layout,
                        MR_Word *saved_regs, MR_Float *saved_f_regs,
                        MR_TracePort port, MR_bool print_optionals);
extern  const char  *MR_trace_set_level(int ancestor_level,
                        MR_bool print_optionals);
extern  const char  *MR_trace_set_level_from_layout(
                        const MR_LabelLayout *level_layout,
                        MR_Word *base_sp, MR_Word *base_curfr,
                        int ancestor_level, MR_bool print_optionals);
extern  int         MR_trace_current_level(void);
extern  void        MR_trace_current_level_details(
                        const MR_ProcLayout **entry_ptr,
                        const char **filename_ptr, int *linenumber_ptr,
                        MR_Word **base_sp_ptr, MR_Word **base_curfr_ptr);

// Return the number of live variables at the current point. If the required
// information is missing, return a negative number.

extern  int         MR_trace_var_count(void);

// Print the list of the names of variables live at the current point
// on the given file.

extern  const char  *MR_trace_list_vars(FILE *out);

// Print all the information this module has on the variables live
// at the current point on the given file. Intended only for debugging
// the debugger itself.

extern  const char  *MR_trace_list_var_details(FILE *out);

// Return as a side effect the name, type and value of the variable with the
// specified HLDS number, in the specified locations, all of which must be
// non-NULL. If the variable isn't live or isn't known, return a non-null
// string giving the problem.

extern const char   *MR_trace_return_hlds_var_info(int hlds_num,
                        MR_TypeInfo *type_info_ptr, MR_Word *value_ptr);

// Return as a side effect the name, type and value of the specified
// variable in the specified locations, except those which are NULL.
// Variable number n must be in the range 1..MR_trace_var_count().

extern  const char  *MR_trace_return_var_info(int n, const char **name_ptr,
                        MR_TypeInfo *type_info_ptr, MR_Word *value_ptr);

// If the variable specified by n is a head variable, then store
// its argument position in *num and return NULL, otherwise return an error.

extern  const char  *MR_trace_headvar_num(int n, int *num);

// Print the call of the current level as a goal.
//
// The goal is printed to the given file if the file pointer is non-NULL.
// The goal is printed by giving it to the specified browser.
//
// XXX Actually, the "out" parameter is currently ignored.

extern  const char  *MR_trace_browse_one_goal(FILE *out,
                        MR_GoalBrowser browser, MR_BrowseCallerType caller,
                        MR_BrowseFormat format);

// Print I/O action <action_number> as a goal.
//
// The goal is printed to the given file if the file pointer is non-NULL.
// The goal is printed by giving it to the specified browser.
//
// XXX Actually, the "out" parameter is currently ignored.

extern  const char  *MR_trace_browse_action(FILE *out,
                        MR_IoActionNum action_number,
                        MR_GoalBrowser browser, MR_BrowseCallerType caller,
                        MR_BrowseFormat format);

// Parse the given word into a variable specification and the specification
// of a path within that variable. The variable is specified by either its name
// or its sequence number in the set of live variables at the current point;
// the desired part is specified by zero or more suffixes of the form
// ^argnum or /argnum.

extern  const char  *MR_trace_parse_var_path(char *word_spec,
                        MR_VarSpec *var_spec, char **path);

// Parse the given word into a variable specification and the specification
// of a path within that variable, as with MR_trace_parse_var_path, then
// look up and record the term at that path in *type_info and *value,
// and return NULL. If there is a problem with changing to a specified subterm,
// then return the term path of the problematic subterm and set *bad_subterm
// to true. If there is some other problem, return a description of the problem
// and set *bad_subterm to false.

extern  const char  *MR_trace_parse_lookup_var_path(char *word_spec,
                        MR_TypeInfo *type_info, MR_Word *value,
                        MR_bool *bad_subterm);

// Print the (names and) values of (the specified parts of) the specified
// variable. (The variable is specified by either its name or its sequence
// number in the set of live variables at the current point; the desired part
// is specified by zero or more suffixes of the form ^argnum or /argnum.
//
// The names are printed to the file specified by the out parameter if
// print_var_name is set, which requires out to be non-NULL.
// The values are printed by giving them to the specified browser.
// The last argument governs whether this function returns an error
// if the given variable specification is ambiguous.
//
// XXX Actually, the "out" parameter is currently ignored by the browser.

extern  const char  *MR_trace_parse_browse_one(FILE *out,
                        MR_bool print_var_name, char *word_spec,
                        MR_Browser browser, MR_BrowseCallerType caller,
                        MR_BrowseFormat format, MR_bool must_be_unique);

// Print the (name and) value of the specified variable.
// The name is printed to the given file if print_var_name is set.
// The values are printed by giving them to the specified browser.
// The last argument governs whether this function returns an error
// if the given variable specification is ambiguous.
//
// XXX Actually, the "out" parameter is currently ignored by the browser.

extern  const char  *MR_trace_browse_one(FILE *out, MR_bool print_var_name,
                        MR_VarSpec var_spec, MR_Browser browser,
                        MR_BrowseCallerType caller, MR_BrowseFormat format,
                        MR_bool must_be_unique);

extern  const char  *MR_trace_browse_one_path(FILE *out,
                        MR_bool print_var_name, MR_VarSpec var_spec,
                        char *path, MR_Browser browser,
                        MR_BrowseCallerType caller, MR_BrowseFormat format,
                        MR_bool must_be_unique);

// Print the list of the names and values of all variables live at the current
// point. The variables names are printed directly to the given file, but
// only if the given file pointer is not NULL; the variable values are
// printed by calling the given browser function on them.
//
// XXX Actually, the "out" parameter is currently ignored by the browser.

extern  const char  *MR_trace_browse_all(FILE *out, MR_Browser browser,
                        MR_BrowseFormat format);

// Sets the current set of variables to be ones live at the program point
// referred to by level_layout, base_sp and base_curfr arguments, and then
// prints them all.
//
// XXX Actually, the "out" parameter is currently ignored by the browser.

extern  const char  *MR_trace_browse_all_on_level(FILE *out,
                        const MR_LabelLayout *level_layout,
                        MR_Word *base_sp, MR_Word *base_curfr,
                        int ancestor_level, MR_bool print_optionals);

// If the given variable specification is unambiguous, then set *type_info
// to the type of the specified variable, set *value to its value, and set
// *name to its name (the storage name points to will remain valid only until
// the next call to MR_lookup_unambiguous_var_spec). Return a non-NULL error
// message if this is not possible.

extern  const char  *MR_lookup_unambiguous_var_spec(MR_VarSpec var_spec,
                        MR_TypeInfo *type_info, MR_Word *value,
                        const char **name);

// *value and type_info describe a term, and path specifies a subterm of that
// term. If the path is valid, return NULL and put the specified subterm in
// *sub_value and **sub_type_info. If it is not valid, return the invalid part
// of the path specification. This can be turned into an error message with
// MR_trace_bad_path.

extern  char        *MR_select_specified_subterm(char *path,
                        MR_TypeInfo type_info, MR_Word *value,
                        MR_TypeInfo *sub_type_info, MR_Word **sub_value);

// Given an invalid path specification, return an error message for that error.
//
// The returned string is valid only until the next call to MR_trace_bad_path
// or MR_trace_bad_path_in_var.

extern  const char  *MR_trace_bad_path(char *fullpath, char *badpath);

// Given a path specification that does not exist within the specified
// variable, return an error message for that error.
//
// The returned string is valid only until the next call to MR_trace_bad_path
// or MR_trace_bad_path_in_var.

extern  const char  *MR_trace_bad_path_in_var(MR_VarSpec *var_spec,
                        char *fullpath, char *badpath);

// Print the size of the specified variable(s) to the specified file.
// Return a non-NULL error message if this is not possible.

extern  const char  *MR_trace_print_size_one(FILE *out, char *word_spec);

// Print the size of all the variables at the current program point to the
// specified file. Return a non-NULL error message if this is not possible.

extern  const char  *MR_trace_print_size_all(FILE *out);

// Return the current goal as the components of a synthetic term.

extern  void        MR_convert_goal_to_synthetic_term(const char **functor_ptr,
                        MR_Word *arg_list_ptr, MR_bool *is_func_ptr);

// Return lists of names and univ values in the current environment.

extern  void        MR_trace_return_bindings(MR_Word *names_ptr,
                        MR_Word *values_ptr);

// A Readline completer for variable names.

extern  MR_CompleterList *MR_trace_var_completer(const char *word,
                        size_t word_len);

#ifdef  MR_TRACE_CHECK_INTEGRITY

// Checks whether any stack frame at the current program point contains
// references to terms with corrupted representations.

extern  void        MR_trace_check_integrity(const MR_LabelLayout *layout,
                        MR_TracePort port);

#endif  // MR_TRACE_CHECK_INTEGRITY

#endif  // MERCURY_TRACE_VARS_H
