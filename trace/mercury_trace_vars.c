/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1999-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the code for managing information about the
** variables of the program being debugged for both the internal
** and external debuggers.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_memory.h"
#include "mercury_layout_util.h"
#include "mercury_deconstruct.h"
#include "mercury_term_size.h"
#include "mercury_stack_layout.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"

#include "mdb.browse.mh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
** This structure contains all the debugger's information about a variable.
**
** The fullname field obviously contains the variable's full name.
** If this name ends with a sequence of digits, then the basename field will
** contain the name of the variable minus those digits, the num_suffix field
** will contain the numeric value of this sequence of digits, and the
** has_suffix field will be set to true. If the full name does not end with
** a sequence of digits, then the basename field will contain the same string
** as the fullname field, and the has_suffix field will be set to false
** (the num_suffix field will not contain anything meaningful).
**
** If the variable is an argument (but not a type-info argument), the
** is_headvar field is set to the argument number (starting at 1).
** If the variable is not an argument, the is_headvar field will be 0.
** This field is used to list the head variables in order before the
** body variables.
**
** The is_ambiguous field will be set iff the full name of the variable
** does not uniquely identify it among all the variables live at the
** current point. What *is* guaranteed to uniquely identify a variable
** is its HLDS number, which will be in the hlds_number field.
**
** The last two fields contain the value of the variable and the typeinfo
** describing the type of this value.
*/

typedef struct {
    char            *MR_var_fullname;
    char            *MR_var_basename;
    int             MR_var_num_suffix;
    MR_bool         MR_var_has_suffix;
    int             MR_var_is_headvar;
    MR_bool         MR_var_is_ambiguous;
    int             MR_var_hlds_number;
    int             MR_var_seq_num_in_label;
    MR_TypeInfo     MR_var_type;
    MR_Word         MR_var_value;
} MR_Var_Details;

/*
** This structure contains all of the debugger's information about
** all the variables that are live at the current program point,
** where a program point is defined as the combination of a debugger
** event and an ancestor level.
**
** The top_layout, top_saved_regs and top_port fields together describe the
** abstract machine state at the current debugger event. The problem field
** points to a string containing an error message describing why the debugger
** can't print any variables at the current point. It will of course be
** NULL if the debugger can do so, which requires not only that the
** debugger have all the information it needs about the current point.
** Since the debugger doesn't allow the setting of the ancestor level
** to a given value if the selected point is missing any of the required
** information, the problem field can only be non-NULL if the ancestor
** level is zero (i.e. the point at the event itself is already missing
** some required info).
**
** The level_entry field contains the proc layout structure of the
** procedure at the selected ancestor level, and the level_base_sp and
** level_base_curfr fields contain the values appropriate for addressing
** the stack frame of the selected invocation of this procedure. This
** information is useful in looking up e.g. the call number of this invocation.
**
** The var_count field says how many variables are live at the current
** point. This many of the elements of the vars array are valid.
** The number of elements of the vars array for which space has been
** reserved is held in var_max.
*/

typedef struct {
    const MR_Label_Layout   *MR_point_top_layout;
    MR_Word                 *MR_point_top_saved_regs;
    MR_Trace_Port           MR_point_top_port;
    const char              *MR_point_problem;
    int                     MR_point_level;
    const MR_Proc_Layout    *MR_point_level_entry;
    const char              *MR_point_level_filename;
    int                     MR_point_level_linenumber;
    MR_Word                 *MR_point_level_base_sp;
    MR_Word                 *MR_point_level_base_curfr;
    int                     MR_point_var_count;
    int                     MR_point_var_max;
    MR_Var_Details          *MR_point_vars;
} MR_Point;

static  MR_bool         MR_trace_type_is_ignored(
                            MR_PseudoTypeInfo pseudo_type_info,
                            MR_bool print_optionals);
static  int             MR_trace_compare_var_details(const void *arg1,
                            const void *arg2);
static  int             MR_compare_slots_on_headvar_num(const void *p1,
                            const void *p2);
static  const char *    MR_trace_browse_one_path(FILE *out,
                            MR_bool print_var_name, MR_Var_Spec var_spec,
                            char *path, MR_Browser browser,
                            MR_Browse_Caller_Type caller,
                            MR_Browse_Format format, MR_bool must_be_unique);
static  char *          MR_trace_browse_var(FILE *out, MR_bool print_var_name,
                            MR_Var_Details *var, char *path,
                            MR_Browser browser, MR_Browse_Caller_Type caller,
                            MR_Browse_Format format);
static  const char *    MR_lookup_var_spec(MR_Var_Spec var_spec,
                            int *var_index_ptr, MR_bool *is_ambiguous_ptr);
static  char *          MR_trace_var_completer_next(const char *word,
                            size_t word_len, MR_Completer_Data *data);
static  const char *    MR_trace_bad_path(const char *path);
static  int             MR_trace_print_var_name(FILE *out,
                            MR_Var_Details *var);
static  const char *    MR_trace_valid_var_number(int var_number);

#define MR_INIT_VAR_DETAIL_COUNT        20
#define MR_TRACE_PADDED_VAR_NAME_LENGTH 23

static  MR_Point        MR_point;

/*
** These extern declarations are necessary because the modules defining
** these structures (some which are in Mercury and some of which are in C)
** do not export them. The types are a lie, but a safe lie.
*/

extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 1);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(private_builtin, type_ctor_info, 1);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(private_builtin, typeclass_info, 1);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(private_builtin, base_typeclass_info, 1);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(std_util, type_desc, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(std_util, type_ctor_desc, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(type_desc, type_desc, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(type_desc, type_ctor_desc, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(builtin, func, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(builtin, pred, 0);
extern const struct MR_TypeCtorInfo_Struct
  MR_TYPE_CTOR_INFO_NAME(builtin, void, 0);

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_NATIVE_GC)
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, succip, 0);
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, hp, 0);
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, curfr, 0);
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, maxfr, 0);
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, redoip, 0);
  extern const struct MR_TypeCtorInfo_Struct
    MR_TYPE_CTOR_INFO_NAME(builtin, redofr, 0);
#endif

static  MR_TypeCtorInfo
MR_trace_always_ignored_type_ctors[] =
{
    /* we ignore these until the browser can handle their varying arity, */
    /* or their definitions are updated. XXX */
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, typeclass_info, 1),
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, base_typeclass_info, 1),

    /* we ignore these because they should never be needed */
    &MR_TYPE_CTOR_INFO_NAME(builtin, void, 0),

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_NATIVE_GC)
    /* we ignore these because they are not interesting */
    &MR_TYPE_CTOR_INFO_NAME(builtin, succip, 0),
    &MR_TYPE_CTOR_INFO_NAME(builtin, hp, 0),
    &MR_TYPE_CTOR_INFO_NAME(builtin, curfr, 0),
    &MR_TYPE_CTOR_INFO_NAME(builtin, maxfr, 0),
    &MR_TYPE_CTOR_INFO_NAME(builtin, redoip, 0),
    &MR_TYPE_CTOR_INFO_NAME(builtin, redofr, 0),
#endif
    /* dummy member */
    NULL
};

static  MR_TypeCtorInfo
MR_trace_maybe_ignored_type_ctors[] =
{
    /*
    ** We can print values of these types (after a fashion),
    ** but users are usually not interested in their values.
    */
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 1),
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_ctor_info, 1),
    /* dummy member */
    NULL
};

static MR_bool
MR_trace_type_is_ignored(MR_PseudoTypeInfo pseudo_type_info,
    MR_bool print_optionals)
{
    MR_TypeCtorInfo type_ctor_info;
    int             always_ignore_type_ctor_count;
    int             maybe_ignore_type_ctor_count;
    int             i;

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
        return MR_FALSE;
    }

    type_ctor_info =
        MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo_type_info);
    always_ignore_type_ctor_count =
        sizeof(MR_trace_always_ignored_type_ctors) / sizeof(MR_Word *);

    for (i = 0; i < always_ignore_type_ctor_count; i++) {
        if (type_ctor_info == MR_trace_always_ignored_type_ctors[i]) {
            return MR_TRUE;
        }
    }

    if (print_optionals) {
        return MR_FALSE;
    }

    maybe_ignore_type_ctor_count =
        sizeof(MR_trace_maybe_ignored_type_ctors) / sizeof(MR_Word *);

    for (i = 0; i < maybe_ignore_type_ctor_count; i++) {
        if (type_ctor_info == MR_trace_maybe_ignored_type_ctors[i]) {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

void
MR_trace_init_point_vars(const MR_Label_Layout *top_layout,
    MR_Word *saved_regs, MR_Trace_Port port, MR_bool print_optionals)
{
    MR_point.MR_point_top_layout = top_layout;
    MR_point.MR_point_top_saved_regs = saved_regs;
    MR_point.MR_point_top_port = port;
    MR_point.MR_point_level = 0;
    MR_point.MR_point_problem = MR_trace_set_level(0, print_optionals);
}

const char *
MR_trace_set_level(int ancestor_level, MR_bool print_optionals)
{
    const char              *problem;
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    const MR_Label_Layout   *top_layout;
    const MR_Label_Layout   *level_layout;

    problem = NULL;
    top_layout = MR_point.MR_point_top_layout;
    base_sp = MR_saved_sp(MR_point.MR_point_top_saved_regs);
    base_curfr = MR_saved_curfr(MR_point.MR_point_top_saved_regs);
    level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
            &base_sp, &base_curfr, &problem);

    if (level_layout != NULL) {
        return MR_trace_set_level_from_layout(level_layout,
            base_sp, base_curfr, ancestor_level, print_optionals);
    } else {
        if (problem == NULL) {
            MR_fatal_error("MR_find_nth_ancestor failed "
                    "without reporting a problem");
        }

        return problem;
    }
}

const char *
MR_trace_set_level_from_layout(const MR_Label_Layout *level_layout,
    MR_Word *base_sp, MR_Word *base_curfr, int ancestor_level,
    MR_bool print_optionals)
{
    const MR_Proc_Layout    *entry;
    MR_Word                 *valid_saved_regs;
    int                     var_count;
    int                     num_added_args;
    int                     arity;
    MR_PredFunc             pred_or_func;
    MR_TypeInfo             *type_params;
    MR_Word                 value;
    MR_TypeInfo             type_info;
    MR_PseudoTypeInfo       pseudo_type_info;
    int                     i;
    int                     slot;
    int                     slot_max;
    int                     copylen;
    char                    *copy;
    char                    *s;
    const char              *name;
    const char              *string_table;
    MR_Integer              string_table_size;
    const char              *filename;
    int                     linenumber;

    entry = level_layout->MR_sll_entry;
    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(entry)) {
        return "this procedure does not have debugging information";
    }

    if (! MR_has_valid_var_count(level_layout)) {
        return "there is no information about live variables";
    }

    if (! MR_find_context(level_layout, &filename, &linenumber)) {
        filename = "";
        linenumber = 0;
    }

    /*
    ** After this point, we cannot find any more problems
    ** that would prevent us from assembling an accurate picture
    ** of the set of live variables at the given level,
    ** so we are free to modify the MR_point structure.
    */

    MR_point.MR_point_problem = NULL;
    MR_point.MR_point_level = ancestor_level;
    MR_point.MR_point_level_entry = entry;
    MR_point.MR_point_level_filename = filename;
    MR_point.MR_point_level_linenumber = linenumber;
    MR_point.MR_point_level_base_sp = base_sp;
    MR_point.MR_point_level_base_curfr = base_curfr;

    if (MR_has_valid_var_info(level_layout)) {
        var_count = MR_all_desc_var_count(level_layout);
    } else {
        /*
        ** If the count of variables is zero, then the rest of the
        ** information about the set of live variables (e.g. the
        ** type parameter array pointer) is not present. Continuing
        ** would therefore lead to a core dump.
        **
        ** Instead, we set up the remaining meaningful fields
        ** of MR_point.
        */

        MR_point.MR_point_var_count = 0;
        return NULL;
    }

    if (level_layout->MR_sll_var_nums == NULL) {
        return "there are no names for the live variables";
    }

    if (MR_saved_curfr(MR_point.MR_point_top_saved_regs) == base_curfr
        && MR_saved_sp(MR_point.MR_point_top_saved_regs) == base_sp
        && MR_point.MR_point_top_port != MR_PORT_EXCEPTION)
    {
        valid_saved_regs = MR_point.MR_point_top_saved_regs;
    } else {
        valid_saved_regs = NULL;
    }

    type_params = MR_materialize_type_params_base(level_layout,
                valid_saved_regs, base_sp, base_curfr);

    MR_ensure_big_enough(var_count, MR_point.MR_point_var, 
        MR_Var_Details, MR_INIT_VAR_DETAIL_COUNT);

    for (slot = 0; slot < MR_point.MR_point_var_count; slot++) {
        /* free the memory allocated by previous MR_copy_string */
        MR_free(MR_point.MR_point_vars[slot].MR_var_fullname);
        MR_free(MR_point.MR_point_vars[slot].MR_var_basename);
    }

    string_table = entry->MR_sle_module_layout->MR_ml_string_table;
    string_table_size = entry->MR_sle_module_layout->MR_ml_string_table_size;

    MR_proc_id_arity_addedargs_predfunc(entry, &arity, &num_added_args,
        &pred_or_func);

    slot = 0;
    for (i = 0; i < var_count; i++) {
        int var_num;
        int head_var_num;
        int offset;

        var_num = level_layout->MR_sll_var_nums[i];

        if (var_num == 0) {
            /* this value is not a variable */
            continue;
        }

        if (var_num > entry->MR_sle_max_named_var_num) {
            /* this value is a compiler-generated variable */
            continue;
        }

        /* the offset of variable number 1 is stored at index 0 */
        offset = entry->MR_sle_used_var_names[var_num - 1];
        if (offset > string_table_size) {
            MR_fatal_error("array bounds error on string table");
        }

        name = string_table + offset;
        if (name == NULL || MR_streq(name, "")) {
            /* this value is a compiler-generated variable */
            continue;
        }

        pseudo_type_info = MR_var_pti(level_layout, i);
        if (MR_trace_type_is_ignored(pseudo_type_info, print_optionals)) {
            continue;
        }

        if (! MR_get_type_and_value_base(level_layout, i, valid_saved_regs,
            base_sp, base_curfr, type_params, &type_info, &value))
        {
            /* this value is not a variable */
            continue;
        }

        MR_point.MR_point_vars[slot].MR_var_hlds_number = var_num;
        MR_point.MR_point_vars[slot].MR_var_seq_num_in_label = i;

        copy = MR_copy_string(name);
        MR_point.MR_point_vars[slot].MR_var_fullname = copy;
        MR_point.MR_point_vars[slot].MR_var_type = type_info;
        MR_point.MR_point_vars[slot].MR_var_value = value;

        /* we need another copy we can cut apart */
        copy = MR_copy_string(name);
        copylen = strlen(copy);
        s = copy + copylen - 1;
        while (s > copy && MR_isdigit(*s)) {
            s--;
        }

        if (s == copy + copylen - 1) {
            MR_point.MR_point_vars[slot].MR_var_has_suffix = MR_FALSE;
            /* num_suffix should not be used */
            MR_point.MR_point_vars[slot].MR_var_num_suffix = -1;
            MR_point.MR_point_vars[slot].MR_var_basename = copy;
        } else {
            if (MR_isdigit(*s)) {
                MR_fatal_error("variable name starts with digit");
            }

            MR_point.MR_point_vars[slot].MR_var_has_suffix = MR_TRUE;
            MR_point.MR_point_vars[slot].MR_var_num_suffix = atoi(s + 1);
            *(s + 1) = '\0';
            MR_point.MR_point_vars[slot].MR_var_basename = copy;
        }

        MR_point.MR_point_vars[slot].MR_var_is_headvar = 0;
        for (head_var_num = num_added_args;
                head_var_num < entry->MR_sle_num_head_vars;
                head_var_num++)
        {
            if (entry->MR_sle_head_var_nums[head_var_num] == var_num) {
                MR_point.MR_point_vars[slot].MR_var_is_headvar =
                    head_var_num - num_added_args + 1;
                break;
            }
        }

        MR_point.MR_point_vars[slot].MR_var_is_ambiguous = MR_FALSE;
        slot++;
    }

    slot_max = slot;
    MR_free(type_params);

    if (slot_max > 0) {
        qsort(MR_point.MR_point_vars, slot_max, sizeof(MR_Var_Details),
            MR_trace_compare_var_details);

        slot = 1;
        for (i = 1; i < slot_max; i++) {
            if (MR_point.MR_point_vars[i].MR_var_hlds_number ==
                MR_point.MR_point_vars[i-1].MR_var_hlds_number)
            {
                continue;
            }

            MR_memcpy(&MR_point.MR_point_vars[slot],
                &MR_point.MR_point_vars[i],
                sizeof(MR_Var_Details));

            if (MR_streq(
                MR_point.MR_point_vars[slot].MR_var_fullname,
                MR_point.MR_point_vars[slot-1].MR_var_fullname))
            {
                MR_point.MR_point_vars[slot - 1].MR_var_is_ambiguous = MR_TRUE;
                MR_point.MR_point_vars[slot].MR_var_is_ambiguous = MR_TRUE;
            }

            slot++;
        }

        slot_max = slot;
    }

    MR_point.MR_point_var_count = slot_max;
    return NULL;
}

/*
** This comparison function is used to sort variables
**
**  - first on basename,
**  - then on suffix,
**  - and then, if necessary, on HLDS number.
**
** The sorting on basenames is alphabetical except for head variables,
** which always come out first.
**
** The sorting on suffixes orders variables with the same basename
** so that they come out in order of numerically increasing suffix,
** with any variable sharing the same name but without a numeric suffix
** coming out last.
*/

static int
MR_trace_compare_var_details(const void *arg1, const void *arg2)
{
    MR_Var_Details  *var1;
    MR_Var_Details  *var2;
    int             var1_is_headvar;
    int             var2_is_headvar;
    int             diff;

    var1 = (MR_Var_Details *) arg1;
    var2 = (MR_Var_Details *) arg2;

    var1_is_headvar = var1->MR_var_is_headvar;
    var2_is_headvar = var2->MR_var_is_headvar;
    if (var1_is_headvar && var2_is_headvar) {
        return var1_is_headvar - var2_is_headvar;
    } else if (var1_is_headvar && ! var2_is_headvar) {
        return -1;
    } else if (! var1_is_headvar && var2_is_headvar) {
        return 1;
    }

    diff = strcmp(var1->MR_var_basename, var2->MR_var_basename);
    if (diff != 0) {
        return diff;
    }

    if (var1->MR_var_has_suffix && ! var2->MR_var_has_suffix) {
        return -1;
    } else if (! var1->MR_var_has_suffix && var2->MR_var_has_suffix) {
        return 1;
    }

    diff = var1->MR_var_num_suffix - var2->MR_var_num_suffix;
    if (diff != 0) {
        return diff;
    }

    return var1->MR_var_hlds_number - var2->MR_var_hlds_number;
}

int
MR_trace_current_level(void)
{
    return MR_point.MR_point_level;
}

void
MR_trace_current_level_details(const MR_Proc_Layout **entry_ptr,
    const char **filename_ptr, int *linenumber_ptr,
    MR_Word **base_sp_ptr, MR_Word **base_curfr_ptr)
{
    if (MR_point.MR_point_problem != NULL) {
        MR_fatal_error("cannot get details about current level");
    }

    if (entry_ptr != NULL) {
        *entry_ptr = MR_point.MR_point_level_entry;
    }

    if (filename_ptr != NULL) {
        *filename_ptr = MR_point.MR_point_level_filename;
    }

    if (linenumber_ptr != NULL) {
        *linenumber_ptr = MR_point.MR_point_level_linenumber;
    }

    if (base_sp_ptr != NULL) {
        *base_sp_ptr = MR_point.MR_point_level_base_sp;
    }

    if (base_curfr_ptr != NULL) {
        *base_curfr_ptr = MR_point.MR_point_level_base_curfr;
    }
}

int
MR_trace_var_count(void)
{
    if (MR_point.MR_point_problem != NULL) {
        return -1;
    }

    return MR_point.MR_point_var_count;
}

const char *
MR_trace_list_vars(FILE *out)
{
    int i;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        fprintf(out, "%9d ", i + 1);
        MR_trace_print_var_name(out, &MR_point.MR_point_vars[i]);
        fprintf(out, "\n");
    }

    return NULL;
}

const char *
MR_trace_list_var_details(FILE *out)
{
    MR_Var_Details  *details;
    int             i;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        details = &MR_point.MR_point_vars[i];
        fprintf(out, "\n");
        fprintf(out, "slot %d, seq %d, hlds %d: headvar: %d, ambiguous: %s\n",
            i, details->MR_var_seq_num_in_label,
            details->MR_var_hlds_number, details->MR_var_is_headvar,
            details->MR_var_is_ambiguous ? "yes" : "no");
        fprintf(out, "full <%s>, base <%s>, num_suffix %d, has_suffix %s\n",
            details->MR_var_fullname, details->MR_var_basename,
            details->MR_var_num_suffix,
            details->MR_var_has_suffix ? "yes" : "no");
        fprintf(out, "typeinfo %p, value %x\n",
            details->MR_var_type, details->MR_var_value);
    }

    return NULL;
}

const char *
MR_trace_return_hlds_var_info(int hlds_num, MR_TypeInfo *type_info_ptr,
    MR_Word *value_ptr)
{
    int i;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        if (MR_point.MR_point_vars[i].MR_var_hlds_number == hlds_num) {
            *type_info_ptr = MR_point.MR_point_vars[i].MR_var_type;
            *value_ptr = MR_point.MR_point_vars[i].MR_var_value;
            return NULL;
        }
    }

    return "no variable with specified hlds number";
}

const char *
MR_trace_return_var_info(int var_number, const char **name_ptr,
    MR_TypeInfo *type_info_ptr, MR_Word *value_ptr)
{
    const MR_Var_Details    *details;
    const char              *problem;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    problem = MR_trace_valid_var_number(var_number);
    if (problem != NULL) {
        return problem;
    }

    details = &MR_point.MR_point_vars[var_number - 1];
    
    if (name_ptr != NULL) {
        *name_ptr = details->MR_var_fullname;
    }
    if (type_info_ptr != NULL) {
        *type_info_ptr = details->MR_var_type;
    }
    if (value_ptr != NULL) {
        *value_ptr = details->MR_var_value;
    }

    return NULL;
}

const char *
MR_trace_headvar_num(int var_number, int *arg_pos)
{
    const MR_Var_Details    *details;
    const char              *problem;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    problem = MR_trace_valid_var_number(var_number);
    if (problem != NULL) {
        return problem;
    }

    details = &MR_point.MR_point_vars[var_number - 1];

    if (!details->MR_var_is_headvar) {
        return "not a head variable";
    }

    *arg_pos = details->MR_var_num_suffix;
    return NULL;
}

/*
** The following declaration allocates a cell to a typeinfo even if though
** its arity is zero. This wastes a word of space but avoids depending on the
** current typeinfo optimization scheme.
*/

#define unbound_ctor_name MR_NONSTD_TYPE_CTOR_INFO_NAME(mdb__util, unbound, 0)

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(unbound_ctor_name);

static
MR_static_type_info_arity_0(MR_unbound_typeinfo_struct, &unbound_ctor_name);

void
MR_convert_arg_to_var_spec(const char *word_spec, MR_Var_Spec *var_spec)
{
    int n;

    if (MR_trace_is_natural_number(word_spec, &n)) {
        var_spec->MR_var_spec_kind = MR_VAR_SPEC_NUMBER;
        var_spec->MR_var_spec_number = n;
        var_spec->MR_var_spec_name = NULL; /* unused */
    } else {
        var_spec->MR_var_spec_kind = MR_VAR_SPEC_NAME;
        var_spec->MR_var_spec_name = word_spec;
        var_spec->MR_var_spec_number = -1; /* unused */
    }
}

static int
MR_compare_slots_on_headvar_num(const void *p1, const void *p2)
{
    MR_Var_Details  *vars;
    int             s1;
    int             s2;

    vars = MR_point.MR_point_vars;
    s1 = * (int *) p1;
    s2 = * (int *) p2;

    if (! vars[s1].MR_var_is_headvar) {
        MR_fatal_error("MR_compare_slots_on_headvar_num: s1");
    }
    if (! vars[s2].MR_var_is_headvar) {
        MR_fatal_error("MR_compare_slots_on_headvar_num: s2");
    }

    if (vars[s1].MR_var_is_headvar < vars[s2].MR_var_is_headvar) {
        return -1;
    } else if (vars[s1].MR_var_is_headvar > vars[s2].MR_var_is_headvar) {
        return 1;
    } else {
        return 0;
    }
}

void
MR_convert_goal_to_synthetic_term(const char **functor_ptr,
    MR_Word *arg_list_ptr,
    MR_bool *is_func_ptr)
{
    const MR_Proc_Layout    *proc_layout;
    MR_ConstString          proc_name;
    MR_Word                 is_func;
    MR_Word                 arg_list;
    MR_Word                 arg;
    MR_TypeInfo             arg_list_typeinfo;
    MR_Var_Details          *vars;
    int                     headvar_num;
    int                     arity;
    int                     slot;
    int                     var_count;
    int                     next;
    int                     i;
    int                     *var_slot_array;

    proc_layout = MR_point.MR_point_level_entry;
    MR_generate_proc_name_from_layout(proc_layout, &proc_name, &arity,
        &is_func);

    vars = MR_point.MR_point_vars;
    var_count = MR_point.MR_point_var_count;
    var_slot_array = MR_malloc(sizeof(int) * var_count);

    next = 0;
    for (slot = MR_point.MR_point_var_count - 1; slot >= 0; slot--) {
        headvar_num = vars[slot].MR_var_is_headvar;
        if (headvar_num) {
            var_slot_array[next] = slot;
            next++;
        }
    }

    qsort(var_slot_array, next, sizeof(int), MR_compare_slots_on_headvar_num);

    MR_TRACE_USE_HP(

        /*
        ** Replace the slot numbers in the argument list
        ** with the argument values, adding entries for
        ** any unbound arguments (they will be printed
        ** as `_').
        */
        arg_list = MR_list_empty();
        i = next - 1;
        for (headvar_num = arity; headvar_num > 0; headvar_num--) {
            if (i >= 0 && vars[var_slot_array[i]].MR_var_is_headvar
                == headvar_num)
            {
                slot = var_slot_array[i];
                i--;
                MR_new_univ_on_hp(arg, vars[slot].MR_var_type,
                    vars[slot].MR_var_value);
            } else {
                MR_new_univ_on_hp(arg,
                    (MR_TypeInfo) &MR_unbound_typeinfo_struct, MR_UNBOUND);
            }

            arg_list = MR_univ_list_cons(arg, arg_list);
        }
    );

    *functor_ptr = proc_name;
    *arg_list_ptr = arg_list;
    *is_func_ptr = is_func;
}

const char *
MR_trace_browse_one_goal(FILE *out, MR_GoalBrowser browser,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    const char  *functor;
    MR_Word     arg_list;
    MR_bool     is_func;
    MR_bool     saved_io_tabling_enabled;

    MR_convert_goal_to_synthetic_term(&functor, &arg_list, &is_func);

    saved_io_tabling_enabled = MR_io_tabling_enabled;
    MR_io_tabling_enabled = MR_FALSE;
    (*browser)(functor, arg_list, is_func, caller, format);
    MR_io_tabling_enabled = saved_io_tabling_enabled;
    return NULL;
}

const char *
MR_trace_browse_action(FILE *out, int action_number, MR_GoalBrowser browser,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    MR_ConstString  proc_name;
    MR_Word         is_func;
    MR_Word         arg_list;
    const char      *problem;
    MR_bool         saved_io_tabling_enabled;

    problem = MR_trace_get_action(action_number, &proc_name,
        &is_func, &arg_list);
    if (problem != NULL) {
        return problem;
    }

    saved_io_tabling_enabled = MR_io_tabling_enabled;
    MR_io_tabling_enabled = MR_FALSE;
    (*browser)(proc_name, arg_list, is_func, caller, format);
    MR_io_tabling_enabled = saved_io_tabling_enabled;
    return NULL;
}

const char *
MR_trace_parse_browse_one(FILE *out, MR_bool print_var_name, char *word_spec,
    MR_Browser browser, MR_Browse_Caller_Type caller, MR_Browse_Format format,
    MR_bool must_be_unique)
{
    MR_Var_Spec var_spec;
    char        *path;
    char        *s;
    int         n;

    s = strpbrk(word_spec, "^/");

    if (s == NULL) {
        path = NULL;
    } else {
        path = s;

        do {
            if (*s == '^' || *s == '/') {
                s++;
            } else {
                return "bad component selector";
            }

            if (MR_isdigit(*s)) {
                s++;
                while (MR_isdigit(*s)) {
                    s++;
                }
            } else if (MR_isalnumunder(*s)) {
                s++;
                while (MR_isalnumunder(*s)) {
                    s++;
                }
            } else {
                return "bad component selector";
            }
        } while (*s != '\0');

        *path = '\0';
        path++;
    }

    MR_convert_arg_to_var_spec(word_spec, &var_spec);

    return MR_trace_browse_one_path(out, print_var_name, var_spec, path,
        browser, caller, format, must_be_unique);
}
 
const char *
MR_trace_browse_one(FILE *out, MR_bool print_var_name, MR_Var_Spec var_spec,
    MR_Browser browser, MR_Browse_Caller_Type caller, MR_Browse_Format format,
    MR_bool must_be_unique)
{
    return MR_trace_browse_one_path(out, print_var_name, var_spec, NULL,
        browser, caller, format, must_be_unique);
}

static const char *
MR_trace_browse_one_path(FILE *out, MR_bool print_var_name,
    MR_Var_Spec var_spec, char *path, MR_Browser browser,
    MR_Browse_Caller_Type caller, MR_Browse_Format format,
    MR_bool must_be_unique)
{
    int         i;
    MR_bool     is_ambiguous;
    const char  *problem;
    char        *bad_path;

    problem = MR_lookup_var_spec(var_spec, &i, &is_ambiguous);
    if (problem != NULL) {
        return problem;
    }

    if (! is_ambiguous) {
        bad_path = MR_trace_browse_var(out, print_var_name,
            &MR_point.MR_point_vars[i], path, browser, caller, format);
        if (bad_path != NULL) {
            return MR_trace_bad_path(bad_path);
        }
    } else {
        int success_count;

        if (must_be_unique) {
            return "variable name is not unique";
        }

        success_count = 0;
        do {
            bad_path = MR_trace_browse_var(out, print_var_name,
                &MR_point.MR_point_vars[i], path, browser, caller, format);

            if (bad_path == NULL) {
                success_count++;
            }

            i++;
        } while (i < MR_point.MR_point_var_count &&
            MR_streq(var_spec.MR_var_spec_name,
            MR_point.MR_point_vars[i].MR_var_fullname));

        if (success_count == 0) {
            return "the selected path does not exist in any of the variables with that name";
        }
    }

    return NULL;
}

const char *
MR_trace_print_size_one(FILE *out, char *word_spec)
{
#ifndef MR_RECORD_TERM_SIZES

    return "term sizes not available in this grade";

#else

    int         i;
    MR_bool     is_ambiguous;
    const char  *problem;
    MR_Var_Spec var_spec;

    MR_convert_arg_to_var_spec(word_spec, &var_spec);
    problem = MR_lookup_var_spec(var_spec, &i, &is_ambiguous);
    if (problem != NULL) {
        return problem;
    }

    if (is_ambiguous) {
        do {
            fprintf(out, "%20s: %6u\n",
                MR_point.MR_point_vars[i].MR_var_fullname,
                MR_term_size(MR_point.MR_point_vars[i].MR_var_type,
                    MR_point.MR_point_vars[i].MR_var_value));
            i++;
        } while (i < MR_point.MR_point_var_count &&
            MR_streq(var_spec.MR_var_spec_name,
            MR_point.MR_point_vars[i].MR_var_fullname));
    } else {
        fprintf(out, "%20s: %6u\n",
            MR_point.MR_point_vars[i].MR_var_fullname,
            MR_term_size(MR_point.MR_point_vars[i].MR_var_type,
                MR_point.MR_point_vars[i].MR_var_value));
    }

    return NULL;

#endif
}

const char *
MR_trace_print_size_all(FILE *out)
{
#ifndef MR_RECORD_TERM_SIZES
    return "term sizes not available in this grade";
#else
    int         i;
    const char  *problem;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        fprintf(out, "%-20s %6u\n",
            MR_point.MR_point_vars[i].MR_var_fullname,
            MR_term_size(MR_point.MR_point_vars[i].MR_var_type,
                MR_point.MR_point_vars[i].MR_var_value));
    }

    return NULL;
#endif
}

#define BAD_PATH_BUFFER_SIZE    128
#define BAD_PATH_MSG_PREFIX     "the path "
#define BAD_PATH_MSG_SUFFIX     " does not exist"

static const char *
MR_trace_bad_path(const char *path)
{
    static  char    buffer[BAD_PATH_BUFFER_SIZE];

    if (strlen(BAD_PATH_MSG_PREFIX) + strlen(path) +
        strlen(BAD_PATH_MSG_SUFFIX) < BAD_PATH_BUFFER_SIZE)
    {
        sprintf(buffer, "%s%s%s", BAD_PATH_MSG_PREFIX, path,
            BAD_PATH_MSG_SUFFIX);
        return buffer;
    } else {
        return "the given path does not exist";
    }
}

const char *
MR_trace_browse_all(FILE *out, MR_Browser browser, MR_Browse_Format format)
{
    int i;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    if (MR_point.MR_point_var_count == 0 && out != NULL) {
        fprintf(out, "mdb: there are no live variables.\n");
    }

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        (void) MR_trace_browse_var(out, MR_TRUE, &MR_point.MR_point_vars[i],
            NULL, browser, MR_BROWSE_CALLER_PRINT_ALL, format);
    }

    return NULL;
}

const char *
MR_trace_browse_all_on_level(FILE *out, const MR_Label_Layout *level_layout,
    MR_Word *base_sp, MR_Word *base_curfr, int ancestor_level,
    MR_bool print_optionals)
{
    const char  *problem;

    problem = MR_trace_set_level_from_layout(level_layout,
        base_sp, base_curfr, ancestor_level, print_optionals);
    if (problem != NULL) {
        return problem;
    }

    return MR_trace_browse_all(out, MR_trace_print, MR_BROWSE_DEFAULT_FORMAT);
}

static char *
MR_trace_browse_var(FILE *out, MR_bool print_var_name, MR_Var_Details *var,
    char *path, MR_Browser browser, MR_Browse_Caller_Type caller,
    MR_Browse_Format format)
{
    MR_TypeInfo typeinfo;
    MR_TypeInfo new_typeinfo;
    MR_Word     *value;
    MR_Word     *new_value;
    char        *old_path;
    int         arg_num;
    int         len;
    MR_bool     saved_io_tabling_enabled;

    typeinfo = var->MR_var_type;
    value = &var->MR_var_value;

    if (path != NULL) {
        while (*path != '\0') {
            old_path = path;

            if (MR_isdigit(*path)) {
                /* we have a field number */

                arg_num = 0;
                while (MR_isdigit(*path)) {
                    arg_num = arg_num * 10 + *path - '0';
                    path++;
                }

                /* MR_arg numbers fields from 0, not 1 */
                --arg_num;
            } else {
                /* we have a field name */
                char    saved_char;

                while (MR_isalnumunder(*path)) {
                    path++;
                }

                saved_char = *path;
                *path = '\0';

                if (! MR_named_arg_num(typeinfo, value, old_path, &arg_num))
                {
                    *path = saved_char;
                    return old_path;
                }

                *path = saved_char;
            }

            if (*path != '\0') {
                MR_assert(*path == '^' || *path == '/');
                path++; /* step over / or ^ */
            }

            if (MR_arg(typeinfo, value, arg_num, &new_typeinfo,
                &new_value, MR_NONCANON_CC))
            {
                typeinfo = new_typeinfo;
                value = new_value;
            } else {
                return old_path;
            }
        }
    }

    if (print_var_name) {
        if (out == NULL) {
            MR_fatal_error("MR_trace_browse_var: out == NULL");
        }

        /*
        ** The initial blanks are to visually separate
        ** the variable names from the prompt.
        */

        fprintf(out, "%7s", "");
        len = MR_trace_print_var_name(out, var);
        while (len < MR_TRACE_PADDED_VAR_NAME_LENGTH) {
            fputc(' ', out);
            len++;
        }

        /*
        ** We flush the output in case the browser is interactive.
        ** XXX we should pass out (and in, and err) to the browser.
        */

        fflush(out);
    }

    saved_io_tabling_enabled = MR_io_tabling_enabled;
    MR_io_tabling_enabled = MR_FALSE;
    (*browser)((MR_Word) typeinfo, *value, caller, format);
    MR_io_tabling_enabled = saved_io_tabling_enabled;
    return NULL;
}

/*
** Look up the specified variable. If the specified variable exists among the
** variables of the current program point, return NULL, and set *var_index_ptr
** to point to the index of the variable in the MR_point_vars array. If the
** specification matches exactly than one variable in the array, then
** *is_ambiguous_ptr will be set to false. If it matches more than one, then
** *is_ambiguous_ptr will be set to true, and *var_index_ptr will be set
** to the index of the lowest matching variable. You can then increment index
** until the name no longer matches to find all the matching variables.
** (Ambiguity is not possible if the variable is specified by number.)
**
** If the specified variable does not exist, the return value will point to an
** error message.
*/

static const char *
MR_lookup_var_spec(MR_Var_Spec var_spec, int *var_index_ptr,
    MR_bool *is_ambiguous_ptr)
{
    int         i;
    MR_bool     found;
    const char  *problem;

    if (MR_point.MR_point_problem != NULL) {
        return MR_point.MR_point_problem;
    }

    if (var_spec.MR_var_spec_kind == MR_VAR_SPEC_NUMBER) {
        problem = MR_trace_valid_var_number(var_spec.MR_var_spec_number);
        if (problem != NULL) {
            return problem;
        }

        *var_index_ptr = var_spec.MR_var_spec_number - 1;
        *is_ambiguous_ptr = MR_FALSE;
        return NULL;
    } else if (var_spec.MR_var_spec_kind == MR_VAR_SPEC_NAME) {
        found = MR_FALSE;
        for (i = 0; i < MR_point.MR_point_var_count; i++) {
            if (MR_streq(var_spec.MR_var_spec_name,
                MR_point.MR_point_vars[i].MR_var_fullname))
            {
                found = MR_TRUE;
                break;
            }
        }

        if (! found) {
            return "there is no such variable";
        }

        *var_index_ptr = i;
        if (MR_point.MR_point_vars[i].MR_var_is_ambiguous) {
            *is_ambiguous_ptr = MR_TRUE;
        } else {
            *is_ambiguous_ptr = MR_FALSE;
        }

        return NULL;
    } else {
        MR_fatal_error("internal error: bad var_spec kind");
        return NULL;
    }
}

const char *
MR_convert_var_spec_to_type_value(MR_Var_Spec var_spec,
    MR_TypeInfo *type_info_ptr, MR_Word *value_ptr)
{
    int         i;
    MR_bool     is_ambiguous;
    const char  *problem;

    problem = MR_lookup_var_spec(var_spec, &i, &is_ambiguous);
    if (problem != NULL) {
        return problem;
    }

    if (! is_ambiguous) {
        *type_info_ptr = MR_point.MR_point_vars[i].MR_var_type;
        *value_ptr = MR_point.MR_point_vars[i].MR_var_value;
        return NULL;
    } else {
        return "variable name is not unique";
    }
}

MR_ConstString
MR_hlds_var_name(const MR_Proc_Layout *entry, int hlds_var_num)
{
    const char  *string_table;
    MR_Integer  string_table_size;
    int         offset;

    string_table = entry->MR_sle_module_layout->MR_ml_string_table;
    string_table_size = entry->MR_sle_module_layout->MR_ml_string_table_size;

    if (hlds_var_num > entry->MR_sle_max_named_var_num) {
        /* this value is a compiler-generated variable */
        return NULL;
    }

    /* variable number 1 is stored at offset 0 */
    offset = entry->MR_sle_used_var_names[hlds_var_num - 1];
    if (offset > string_table_size) {
        MR_fatal_error("array bounds error on string table");
    }

    return string_table + offset;
}

MR_Completer_List *
MR_trace_var_completer(const char *word, size_t word_len)
{
    return MR_new_completer_elem(&MR_trace_var_completer_next,
        (MR_Completer_Data) 0, MR_trace_no_free);
}

static char *
MR_trace_var_completer_next(const char *word, size_t word_len,
    MR_Completer_Data *data)
{
    int slot;
    const char *var_name;

    slot = (int) *data;
    while (slot < MR_point.MR_point_var_count) {
        var_name = MR_point.MR_point_vars[slot].MR_var_fullname;
        slot++;
        if (MR_strneq(var_name, word, word_len)) {
            *data = (MR_Completer_Data) slot; 
            return MR_copy_string(var_name);
        }
    }
    return NULL;
}

static int
MR_trace_print_var_name(FILE *out, MR_Var_Details *var)
{
    int len;

    len = strlen(var->MR_var_fullname);
    fputs(var->MR_var_fullname, out);
    if (var->MR_var_is_ambiguous) {
        char    buf[256]; /* this should be plenty big enough */

        sprintf(buf, "(%d)", var->MR_var_hlds_number);
        len += strlen(buf);
        fputs(buf, out);
    }

    /*
    ** If the variable starts with "HeadVar__" then the
    ** argument number is part of the name.
    */
    if (var->MR_var_is_headvar &&
            ! MR_streq(var->MR_var_basename, "HeadVar__"))
    {
        char    buf[256]; /* this should be plenty big enough */

        sprintf(buf, " (arg %d)", var->MR_var_is_headvar);
        len += strlen(buf);
        fputs(buf, out);
    }

    return len;
}

static  const char *
MR_trace_valid_var_number(int var_number)
{
    if (var_number < 1) {
        return "invalid variable number";
    }

    if (var_number > MR_point.MR_point_var_count) {
        return "there aren't that many variables";
    }

    return NULL;
}

#ifdef  MR_TRACE_CHECK_INTEGRITY

static void
MR_trace_check_integrity_on_cur_level(void)
{
    int         i;

    for (i = 0; i < MR_point.MR_point_var_count; i++) {
        /*
        ** Printing the variable will fail if any part of the variable's value
        ** that is printed has been constructed incorrectly. The default print
        ** command will print only the top few levels of the variable, but
        ** since the construction of a memory cell is usually followed very
        ** closely by a call or an exit, this should be sufficient to catch
        ** most misconstructed terms.
        */
        (void) MR_trace_browse_var(stdout, MR_TRUE, &MR_point.MR_point_vars[i],
                (MR_String) (MR_Integer) "", MR_trace_print,
                MR_BROWSE_CALLER_PRINT, MR_BROWSE_DEFAULT_FORMAT);

        /*
        ** Looking up the term size can lead to a crash if the term has a
        ** memory cell that should have but doesn't have a size slot.
        */
        (void) MR_term_size(MR_point.MR_point_vars[i].MR_var_type,
            MR_point.MR_point_vars[i].MR_var_value);
    }
}

#define MR_INTEGRITY_ERROR_BUF_SIZE    512

void
MR_trace_check_integrity(const MR_Label_Layout *layout, MR_Trace_Port port)
{
    int             level;
    const char      *problem;
    char            buf[MR_INTEGRITY_ERROR_BUF_SIZE];
    MR_bool         saved_trace_enabled;
    int             MR_check_max_mr_num;
    MR_Word         MR_check_saved_regs[MR_MAX_FAKE_REG];
    static  int     MR_check_integrity_seq_num = 0;

    saved_trace_enabled = MR_trace_enabled;
    MR_trace_enabled = MR_FALSE;

    MR_compute_max_mr_num(MR_check_max_mr_num, layout);
    MR_restore_transient_registers();
    /* This also saves the regs in MR_fake_regs. */
    MR_copy_regs_to_saved_regs(MR_check_max_mr_num, MR_check_saved_regs);
	MR_trace_init_point_vars(layout, MR_check_saved_regs, port, MR_TRUE);

    if (MR_point.MR_point_problem != NULL) {
        MR_fatal_error(problem);
    }

    level = 0;
    do {
        MR_check_integrity_seq_num++;
        sprintf(buf, "integrity check at event %d, level %d, seq %d\n",
            MR_trace_event_number, level, MR_check_integrity_seq_num);
        MR_trace_report_msg = buf;
#if 0
        /* enable this code if necessary for debugging */
        fprintf(stdout, "%s", buf);
        fflush(stdout);
#endif
        MR_trace_check_integrity_on_cur_level();
        level++;
        problem = MR_trace_set_level(level, MR_TRUE);
    } while (problem == NULL);

    MR_restore_transient_registers();
    MR_saved_global_hp(MR_check_saved_regs) = MR_global_hp;
    MR_copy_saved_regs_to_regs(MR_check_max_mr_num, MR_check_saved_regs);
    MR_trace_report_msg = NULL;
    MR_trace_enabled = saved_trace_enabled;
}

#endif  /* MR_TRACE_CHECK_INTEGRITY */
