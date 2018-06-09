// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2005-2007, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file implements utility functions operating on the data structures
// defined in the corresponding header file.
//
// Author: Zoltan Somogyi

#include "mercury_imp.h"
#include "mercury_stack_layout.h"

MR_ConstString
MR_hlds_var_name(const MR_ProcLayout *entry, int hlds_var_num,
    int *should_copy)
{
    const MR_ModuleLayout   *module_layout;
    const char              *string_table;
    MR_Integer              string_table_size;
    int                     name_code;

    module_layout = entry->MR_sle_module_layout;
    string_table = module_layout->MR_ml_string_table;
    string_table_size = module_layout->MR_ml_string_table_size;

    if (hlds_var_num == 0) {
        // This value is not a variable.
        return NULL;
    }

    if (hlds_var_num > entry->MR_sle_max_named_var_num) {
        // This value is a compiler-generated variable.
        return NULL;
    }

    // Variable number 1 is stored at offset 0.
    name_code = entry->MR_sle_used_var_names[hlds_var_num - 1];
    return MR_name_in_string_table(string_table, string_table_size,
        name_code, should_copy);
}

MR_ConstString
MR_name_in_string_table(const char *string_table, MR_Integer string_table_size,
    MR_uint_least32_t name_code, int *should_copy)
{
    // The encoding decoded here is create by lookup_string_in_table
    // in compiler/stack_layout.m. The code here and there must be kept
    // in sync.

    if ((name_code & 0x1) != 0) {
        static  char    buf[MR_MAX_VARNAME_SIZE];
        int             kind;
        int             n;
        int             offset;

        name_code >>= 1;
        kind = name_code & 0x1f;
        name_code >>= 5;
        n = name_code & 0x3ff;
        offset = name_code >> 10;

        switch (kind) {
            case 0:
                if (n == 0) {
#ifdef  MR_HAVE_SNPRINTF
                    snprintf(buf, MR_MAX_VARNAME_SIZE, "STATE_VARIABLE_%s",
                        string_table + offset);
#else
                    sprintf(buf, "STATE_VARIABLE_%s",
                        string_table + offset);
#endif
                } else {
#ifdef  MR_HAVE_SNPRINTF
                    snprintf(buf, MR_MAX_VARNAME_SIZE, "STATE_VARIABLE_%s_%d",
                        string_table + offset, n - 1);
#else
                    sprintf(buf, "STATE_VARIABLE_%s_%d",
                        string_table + offset, n - 1);
#endif
                }
                break;

            case 1:
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_MAX_VARNAME_SIZE, "TypeCtorInfo_%d", n);
#else
                sprintf(buf, "TypeCtorInfo_%d", n);
#endif
                break;

            case 2:
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_MAX_VARNAME_SIZE, "TypeInfo_%d", n);
#else
                sprintf(buf, "TypeInfo_%d", n);
#endif
                break;

            case 3:
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_MAX_VARNAME_SIZE, "BaseTypeClassInfo_for_%s",
                    string_table + offset);
#else
                sprintf(buf, "BaseTypeClassInfo_for_%s",
                    string_table + offset);
#endif
                break;

            case 4:
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_MAX_VARNAME_SIZE, "TypeClassInfo_for_%s",
                    string_table + offset);
#else
                sprintf(buf, "TypeClassInfo_for_%s",
                    string_table + offset);
#endif
                break;

            case 5:
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_MAX_VARNAME_SIZE, "PolyConst%d", n);
#else
                sprintf(buf, "PolyConst%d", n);
#endif
                break;

            default:
                MR_fatal_error("MR_hlds_var_name: unknown kind");
                break;
        }

        if (should_copy != NULL) {
            *should_copy = MR_TRUE;
        }

        return buf;
    } else {
        int offset;

        offset = name_code >> 1;
        if (offset > string_table_size) {
            MR_fatal_error("MR_hlds_var_name: bounds error on string table");
        }

        if (should_copy != NULL) {
            *should_copy = MR_FALSE;
        }

        return string_table + offset;
    }
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
        // *(s+1) is the first character of the numerical suffix.
        return (s + 1) - str;
    }
}
