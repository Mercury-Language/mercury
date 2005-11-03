/*
** vim:sw=4 ts=4 expandtab
*/
/*
** Copyright (C) 2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file implements utility functions operating on the data structures
** defined in the corresponding header file.
**
** Author: Zoltan Somogyi
*/

#include "mercury_imp.h"
#include "mercury_stack_layout.h"

MR_ConstString
MR_hlds_var_name(const MR_Proc_Layout *entry, int hlds_var_num)
{
    const char  *string_table;
    MR_Integer  string_table_size;
    int         offset;

    string_table = entry->MR_sle_module_layout->MR_ml_string_table;
    string_table_size = entry->MR_sle_module_layout->MR_ml_string_table_size;

    if (hlds_var_num == 0) {
        /* this value is not a variable */
        return NULL;
    }

    if (hlds_var_num > entry->MR_sle_max_named_var_num) {
        /* this value is a compiler-generated variable */
        return NULL;
    }

    /* variable number 1 is stored at offset 0 */
    offset = entry->MR_sle_used_var_names[hlds_var_num - 1];
    if (offset > string_table_size) {
        MR_fatal_error("MR_hlds_var_name: bounds error on string table");
    }

    return string_table + offset;
}

int
MR_find_start_of_num_suffix(const char *str)
{
    int         len;
    const char  *s;

    len = strlen(str);
    s = str + len - 1;
    while (s > str && MR_isdigit(*s)) {
        s--;
    }

    if (s == str + len - 1) {
        return -1;
    } else {
        /* *(s+1) is the first character of the numerical suffix */
        return (s + 1) - str;
    }
}
