// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2003-2005, 2007, 2009, 2011 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_term_size.c
//
// This module defines a function for measuring the sizes of terms.

#include "mercury_imp.h"
#include "mercury_runtime_util.h" // For MR_STRERROR_BUF_SIZE.
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif
#include <string.h>

#ifdef MR_RECORD_TERM_SIZES

MR_ComplexityCounter    MR_complexity_word_counter = 0;
MR_ComplexityCounter    MR_complexity_cell_counter = 0;
MR_ComplexityCounter    MR_complexity_tick_counter = 0;

static  void    MR_write_complexity_proc(MR_ComplexityProc *proc);
static  void    MR_complexity_output_args_desc(FILE *fp,
                    MR_ComplexityProc *proc);

MR_Unsigned
MR_term_size(MR_TypeInfo type_info, MR_Word term)
{
    MR_TypeCtorInfo         type_ctor_info;
    MR_DuTypeLayout         du_type_layout;
    const MR_DuPtagLayout   *ptag_layout;
    int                     ptag;
    int                     sectag;
    int                     arity;
    int                     size;

try_again:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_term_size: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
            // XXX The code to handle these cases hasn't been written yet.
            MR_fatal_error("MR_term_size: RESERVED_ADDR");

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            du_type_layout = MR_type_ctor_layout(type_ctor_info).MR_layout_du;
            ptag = MR_tag(term);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
                case MR_SECTAG_NONE:
#ifdef MR_DEBUG_TERM_SIZES
                    if (ptag_layout->MR_sectag_alternatives[0]->
                        MR_du_functor_orig_arity <= 0)
                    {
                        MR_fatal_error("MR_term_size: zero arity ptag none");
                    }

                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag none %p -> %d\n",
                            (void *) term,
                            (int) MR_field(MR_mktag(ptag), term, -1));
                        printf("type %s.%s/%d, functor %s\n",
                            type_ctor_info->MR_type_ctor_module_name,
                            type_ctor_info->MR_type_ctor_name,
                            type_ctor_info->MR_type_ctor_arity,
                            ptag_layout->MR_sectag_alternatives[0]->
                                MR_du_functor_name);
                    }
#endif
                    return MR_field(MR_mktag(ptag), term, -1);

                case MR_SECTAG_NONE_DIRECT_ARG:
                     // The compiler should not generate direct arg tags
                     // in term size recording grades.

                     MR_fatal_error("MR_term_size: DIRECT_ARG");

                case MR_SECTAG_LOCAL:
#ifdef MR_DEBUG_TERM_SIZES
                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag local %p\n",
                            (void *) term);
                    }
#endif
                    return 0;

                case MR_SECTAG_REMOTE:
#ifdef MR_DEBUG_TERM_SIZES
                    sectag = MR_field(MR_mktag(ptag), term, 0);

                    if (ptag_layout->MR_sectag_alternatives[sectag]->
                        MR_du_functor_orig_arity <= 0)
                    {
                        MR_fatal_error("MR_term_size: zero arity ptag remote");
                    }

                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag remote %p -> %d\n",
                            (void *) term,
                            (int) MR_field(MR_mktag(ptag), term, -1));
                        printf("type %s.%s/%d, functor %s\n",
                            type_ctor_info->MR_type_ctor_module_name,
                            type_ctor_info->MR_type_ctor_name,
                            type_ctor_info->MR_type_ctor_arity,
                            ptag_layout->MR_sectag_alternatives[sectag]->
                                MR_du_functor_name);
                    }
#endif
                    return MR_field(MR_mktag(ptag), term, -1);

                case MR_SECTAG_VARIABLE:
                    MR_fatal_error("MR_term_size: VARIABLE");

                default:
                    fprintf(stderr, "sectag_locn: %d\n",
                        (int) ptag_layout->MR_sectag_locn);
                    MR_fatal_error("MR_term_size: sectag_locn");
            }

        case MR_TYPECTOR_REP_EQUIV:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: equiv %p\n", (void *) term);
            }
#endif
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            goto try_again;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: equiv ground %p\n", (void *) term);
            }
#endif
            type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            goto try_again;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: notag (usereq) %p\n", (void *) term);
            }
#endif
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                    MR_notag_functor_arg_type);
            MR_restore_transient_hp();
            goto try_again;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: notag ground (usereq) %p\n",
                    (void *) term);
            }
#endif
            type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag
                ->MR_notag_functor_arg_type);
            goto try_again;

        case MR_TYPECTOR_REP_TUPLE:
            arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
            if (arity == 0) {
                // Term may be a NULL pointer, so don't follow it.
                size = 0;
            } else {
                size = MR_field(MR_mktag(0), term, -1);
            }

#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: tuple %p -> %d\n",
                    (void *) term, size);
            }
#endif
            return size;

        case MR_TYPECTOR_REP_PRED:
        case MR_TYPECTOR_REP_FUNC:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: pred/func %p\n", (void *) term);
            }
#endif
            // Currently we don't collect stats on closure sizes.
            return 0;

        case MR_TYPECTOR_REP_ARRAY:
            // Currently we don't collect stats on array sizes.
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: array %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_BITMAP:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: bitmap %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: enum (usereq) %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_FOREIGN_ENUM:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
#ifdef MR_DEBUG_TERMSIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: foreign enum (usereq) %p\n",
                    (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_INT:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: int %p %" MR_INTEGER_LENGTH_MODIFIER "d\n",
                    (void *) term, (MR_Integer) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_UINT:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: uint %p %" MR_INTEGER_LENGTH_MODIFIER "u\n",
                    (void *) term, (MR_Unsigned) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_INT8:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: int8 %p %" MR_INTEGER_LENGTH_MODIFIER "d\n",
                    (void *) term, (MR_Integer) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_UINT8:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: uint8 %p %" MR_INTEGER_LENGTH_MODIFIER "u\n",
                    (void *) term, (MR_Unsigned) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_INT16:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: int16 %p %" MR_INTEGER_LENGTH_MODIFIER "d\n",
                    (void *) term, (MR_Integer) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_UINT16:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: uint16 %p %" MR_INTEGER_LENGTH_MODIFIER "u\n",
                    (void *) term, (MR_Unsigned) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_INT32:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: int32 %p %" MR_INTEGER_LENGTH_MODIFIER "d\n",
                    (void *) term, (MR_Integer) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_UINT32:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: uint32 %p %" MR_INTEGER_LENGTH_MODIFIER "u\n",
                    (void *) term, (MR_Unsigned) term);
            }
#endif
            return 0;

        // XXX Maybe we should also print the value of the term in the int64,
        // uint64 and float cases?
        case MR_TYPECTOR_REP_INT64:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: int64 %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_UINT64:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf(
                    "MR_term_size: uint64 %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_CHAR:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: char %p %c\n",
                    (void *) term, (char) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_FLOAT:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: float %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_STRING:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: string %p '%s'\n",
                    (void *) term, (char *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_SUCCIP:
        case MR_TYPECTOR_REP_HP:
        case MR_TYPECTOR_REP_CURFR:
        case MR_TYPECTOR_REP_MAXFR:
        case MR_TYPECTOR_REP_REDOFR:
        case MR_TYPECTOR_REP_REDOIP:
        case MR_TYPECTOR_REP_TRAIL_PTR:
        case MR_TYPECTOR_REP_TICKET:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: impl artifact type %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPECLASSINFO:
        case MR_TYPECTOR_REP_TYPECTORINFO:
        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        case MR_TYPECTOR_REP_TYPEDESC:
        case MR_TYPECTOR_REP_TYPECTORDESC:
        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: type_info etc %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_SUBGOAL:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: subgoal %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_STABLE_C_POINTER:
        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: c_pointer/foreign %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_REFERENCE:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: reference %p\n", (void *) term);
            }
#endif
            return 1;

        case MR_TYPECTOR_REP_DUMMY:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: dummy %p\n",
                    (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error("MR_term_size: VOID");

        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error("MR_term_size: UNKNOWN");

        default:
            fprintf(stderr, "default rep: %d\n",
                    (int) MR_type_ctor_rep(type_ctor_info));
            MR_fatal_error("MR_term_size: default");
    }

    MR_fatal_error("MR_term_size: unexpected fallthrough");
}

void
MR_write_complexity_procs(void)
{
    int                 proc_num;
    MR_ComplexityProc   *proc;

    for (proc_num = 0; proc_num < MR_num_complexity_procs; proc_num++) {
        proc = &MR_complexity_procs[proc_num];
        if (proc->MR_clp_num_profiled_args >= 0) {
            MR_write_complexity_proc(proc);
        }
    }
}

#define MR_COMPLEXITY_ARGS_DIR   "ComplexityArgs"
#define MR_COMPLEXITY_DATA_DIR   "ComplexityData"

static  MR_bool MR_have_created_complexity_dirs = MR_FALSE;
static  MR_bool MR_have_printed_complexity_dirs_error = MR_FALSE;

static void
MR_write_complexity_proc(MR_ComplexityProc *proc)
{
    char                    *full_proc_name;
    int                     full_proc_name_len;
    FILE                    *fp;
    char                    *args_filename;
    char                    *data_filename;
    const char              *data_filemode;
    struct stat             statbuf;
    char                    *slash;
    MR_ComplexityMetrics    *metrics;
    int                     num_profiled_args;
    int                     *sizes;
    int                     num_slots;
    MR_ComplexityPastSlots  *past_slots;
    char                    *cmd_buf;
    char                    errbuf[MR_STRERROR_BUF_SIZE];

    full_proc_name_len = strlen(proc->MR_clp_full_proc_name);
    full_proc_name = MR_malloc(100 + full_proc_name_len);
    strcpy(full_proc_name, proc->MR_clp_full_proc_name);

    // We can't have slash characters in the filenames we construct from
    // full_proc_name.

    while ((slash = strchr(full_proc_name, '/')) != NULL) {
        *slash = ':';
    }

    cmd_buf = MR_malloc(100 + 2 * full_proc_name_len);
        // Will be big enough.

    if (! MR_have_created_complexity_dirs) {
        sprintf(cmd_buf, "mkdir -p %s %s",
            MR_COMPLEXITY_ARGS_DIR, MR_COMPLEXITY_DATA_DIR);
        if (system(cmd_buf) != 0) {
            if (! MR_have_printed_complexity_dirs_error) {
                fprintf(stderr, "%s: cannot create %s and %s: %s\n",
                    MR_progname, MR_COMPLEXITY_ARGS_DIR,
                    MR_COMPLEXITY_DATA_DIR,
                    MR_strerror(errno, errbuf, sizeof(errbuf)));
                // there is no point in aborting
                MR_have_printed_complexity_dirs_error = MR_TRUE;
                return;
            }
        }
        MR_have_created_complexity_dirs = MR_TRUE;
    }

    args_filename = MR_malloc(100 + full_proc_name_len);
        // Will be big enough.
    sprintf(args_filename, "%s/%s", MR_COMPLEXITY_ARGS_DIR, full_proc_name);

    if (stat(args_filename, &statbuf) != 0) {
        // args_filename does not exist.
        fp = fopen(args_filename, "w");
        if (fp == NULL) {
            fprintf(stderr, "%s: cannot open %s: %s\n",
                MR_progname, args_filename,
                MR_strerror(errno, errbuf, sizeof(errbuf)));
            // There is no point in aborting.
            return;
        }

        MR_complexity_output_args_desc(fp, proc);
        fclose(fp);
        data_filemode = "w";
    } else {
        // args_filename does exist.
        char    *tmp_filename;

        tmp_filename = MR_malloc(100 + full_proc_name_len);
        sprintf(tmp_filename, "%s/%s.tmp",
            MR_COMPLEXITY_ARGS_DIR, full_proc_name);
        fp = fopen(tmp_filename, "w");
        if (fp == NULL) {
            fprintf(stderr, "%s: cannot open %s: %s\n",
                MR_progname, tmp_filename,
                MR_strerror(errno, errbuf, sizeof(errbuf)));
            // There is no point in aborting.
            return;
        }

        MR_complexity_output_args_desc(fp, proc);
        fclose(fp);

        sprintf(cmd_buf, "cmp -s %s %s", args_filename, tmp_filename);
        if (system(cmd_buf) == 0) {
            // The files are identical.
            (void) unlink(tmp_filename);
            data_filemode = "a";
        } else {
            // The files are different.
            rename(tmp_filename, args_filename);
            data_filemode = "w";
        }
    }

    data_filename = MR_malloc(100 + full_proc_name_len);
    sprintf(data_filename, "%s/%s", MR_COMPLEXITY_DATA_DIR, full_proc_name);

    fp = fopen(data_filename, data_filemode);
    if (fp == NULL) {
        fprintf(stderr, "%s: cannot open %s: %s\n",
            MR_progname, data_filename,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
        // There is no point in aborting.
        return;
    }

    num_profiled_args = proc->MR_clp_num_profiled_args;

    metrics = proc->MR_clp_metrics;
    sizes = proc->MR_clp_sizes;
    num_slots = proc->MR_clp_next_slot_num;
    past_slots = proc->MR_clp_past_slots;

    do {
        int slot, arg;

        for (slot = num_slots - 1; slot >= 0; slot--) {
            fprintf(fp, "%d %d %d",
                metrics[slot].MR_clpm_num_words,
                metrics[slot].MR_clpm_num_cells,
                metrics[slot].MR_clpm_num_ticks);

            for (arg = 0; arg < num_profiled_args; arg++) {
                fprintf(fp, " %d", sizes[slot * num_profiled_args + arg]);
            }

            fprintf(fp, "\n");
        }

        if (past_slots == NULL) {
            break;
        }

        metrics = past_slots->MR_clpps_metrics;
        sizes = past_slots->MR_clpps_sizes;
        num_slots = MR_COMPLEXITY_SLOTS_PER_CHUNK;
        past_slots = past_slots->MR_clpps_previous;
    } while (MR_TRUE);

    (void) fclose(fp);
}

static void
MR_complexity_output_args_desc(FILE *fp, MR_ComplexityProc *proc)
{
    int                     arg;
    int                     num_args;
    MR_ComplexityArgInfo    *arg_infos;

    arg_infos = proc->MR_clp_arg_infos;
    num_args = proc->MR_clp_num_args;

    for (arg = 0; arg < num_args; arg++) {
        if (arg_infos[arg].MR_clpai_maybe_name != NULL) {
            fprintf(fp, "%s ", arg_infos[arg].MR_clpai_maybe_name);
        } else {
            fprintf(fp, "_ ");
        }

        switch (arg_infos[arg].MR_clpai_kind) {
            case MR_COMPLEXITY_INPUT_VAR_SIZE:
                fprintf(fp, "profiled_input\n");
                break;
            case MR_COMPLEXITY_INPUT_FIX_SIZE:
                fprintf(fp, "unprofiled_input\n");
                break;
            case MR_COMPLEXITY_OUTPUT:
                fprintf(fp, "output\n");
                break;
            default:
                fprintf(fp, "unknown\n");
                break;
        }
    }
}

void
MR_init_complexity_proc(int proc_num, const char *fullname,
    int num_profiled_args, int num_args, MR_ComplexityArgInfo *arg_infos)
{
    MR_ComplexityProc   *proc;

    if (MR_complexity_procs == NULL) {
        fprintf(stderr, "%s: executable wasn't fully prepared "
            "for complexity experiment\n", MR_progname);
        exit(1);
    }

    proc = &MR_complexity_procs[proc_num];
    if (! MR_streq(fullname, proc->MR_clp_full_proc_name)) {
        fprintf(stderr, "%s: proc_num %d is %s: expected %s\n",
            MR_progname, proc_num, proc->MR_clp_full_proc_name, fullname);
        exit(1);
    }

    if (proc->MR_clp_num_profiled_args >= 0) {
        fprintf(stderr, "%s: proc_num %d: duplicate initialization\n",
            MR_progname, proc_num);
        exit(1);
    }

    if (num_profiled_args < 0) {
        fprintf(stderr, "%s: proc_num %d: bad num_profiled_args\n",
            MR_progname, proc_num);
        exit(1);
    }

    proc->MR_clp_num_profiled_args = num_profiled_args;
    proc->MR_clp_num_args = num_args;
    proc->MR_clp_arg_infos = arg_infos;

    proc->MR_clp_metrics = MR_NEW_ARRAY(MR_ComplexityMetrics,
        MR_COMPLEXITY_SLOTS_PER_CHUNK);
    proc->MR_clp_sizes = MR_NEW_ARRAY(int,
        MR_COMPLEXITY_SLOTS_PER_CHUNK * num_profiled_args);
}

void
MR_check_complexity_init(void)
{
    int                 proc_num;
    MR_bool             printed_heading;
    MR_ComplexityProc   *proc;

    printed_heading = MR_FALSE;
    for (proc_num = 0; proc_num < MR_num_complexity_procs; proc_num++) {
        proc = &MR_complexity_procs[proc_num];

        if (proc->MR_clp_num_profiled_args < 0) {
            if (! printed_heading) {
                fprintf(stderr, "%s: the following procedures are "
                    "not available for complexity experiment:\n",
                    MR_progname);
                printed_heading = MR_TRUE;
            }

            fprintf(stderr, "%s\n", proc->MR_clp_full_proc_name);
        }
    }

    if (printed_heading) {
        exit(1);
    }
}

MR_ComplexityIsActive
MR_complexity_is_active_func(int num_procs, int proc_num, const char *name,
    int num_profiled_inputs)
{
    MR_ComplexityProc   *proc;

    if (num_procs != MR_num_complexity_procs || MR_complexity_procs == NULL) {
        fprintf(stderr, "%s: executable wasn't fully prepared "
            "for complexity experiment\n", MR_progname);
        exit(1);
    }

    if (proc_num >= num_procs) {
        fprintf(stderr, "%s: proc_num %d >= num_procs %d\n",
            MR_progname, proc_num, num_procs);
        exit(1);
    }

    proc = &MR_complexity_procs[proc_num];
    if (! MR_streq(name, proc->MR_clp_full_proc_name)) {
        fprintf(stderr, "%s: proc_num %d is %s: expected %s\n",
            MR_progname, proc_num, proc->MR_clp_full_proc_name, name);
        exit(1);
    }

    if (proc->MR_clp_num_profiled_args != num_profiled_inputs) {
        fprintf(stderr, "%s: proc_num %d: bad num_profiled_inputs\n",
            MR_progname, proc_num);
        exit(1);
    }

    return proc->MR_clp_is_active;
}

int
MR_complexity_call_func(int procnum)
{
    MR_ComplexityProc       *proc;
    MR_ComplexityMetrics    *metrics;
    int                     slot;

    proc = &MR_complexity_procs[procnum];
    slot = proc->MR_clp_next_slot_num;
    if (slot < MR_COMPLEXITY_SLOTS_PER_CHUNK) {
        proc->MR_clp_next_slot_num++;
    } else {
        MR_ComplexityPastSlots  *past_slots;

        past_slots = MR_NEW(MR_ComplexityPastSlots);
        past_slots->MR_clpps_metrics = proc->MR_clp_metrics;
        past_slots->MR_clpps_sizes = proc->MR_clp_sizes;
        past_slots->MR_clpps_previous = proc->MR_clp_past_slots;
        proc->MR_clp_past_slots = past_slots;

        proc->MR_clp_metrics = MR_NEW_ARRAY(MR_ComplexityMetrics,
            MR_COMPLEXITY_SLOTS_PER_CHUNK);
        proc->MR_clp_sizes = MR_NEW_ARRAY(int,
            MR_COMPLEXITY_SLOTS_PER_CHUNK * proc->MR_clp_num_profiled_args);
        proc->MR_clp_next_slot_num = 1;
        slot = 0;
    }
    metrics = &proc->MR_clp_metrics[slot];
    metrics->MR_clpm_num_words -= MR_complexity_word_counter;
    metrics->MR_clpm_num_cells -= MR_complexity_cell_counter;
    metrics->MR_clpm_num_ticks -= MR_complexity_tick_counter;
    proc->MR_clp_is_active = MR_COMPLEXITY_IS_ACTIVE;

    return slot;
}

void
MR_complexity_fill_size_slot(MR_ComplexityProc *proc, int slot,
    int num_profiled_args, int argnum, int size)
{
    MR_ComplexityCounter    *sizes;

    sizes = proc->MR_clp_sizes;
    sizes[(slot * proc->MR_clp_num_profiled_args) + argnum] = size;
}

void
MR_complexity_leave_func(int procnum, int slot)
{
    MR_ComplexityProc       *proc;
    MR_ComplexityMetrics    *metrics;

    proc = &MR_complexity_procs[procnum];
    metrics = &proc->MR_clp_metrics[slot];
    metrics->MR_clpm_num_words += MR_complexity_word_counter;
    metrics->MR_clpm_num_cells += MR_complexity_cell_counter;
    metrics->MR_clpm_num_ticks += MR_complexity_tick_counter;
    proc->MR_clp_is_active = MR_COMPLEXITY_IS_INACTIVE;
}

void
MR_complexity_redo_func(int procnum, int slot)
{
    MR_ComplexityProc       *proc;
    MR_ComplexityMetrics    *metrics;

    proc = &MR_complexity_procs[procnum];
    metrics = &proc->MR_clp_metrics[slot];
    metrics->MR_clpm_num_words -= MR_complexity_word_counter;
    metrics->MR_clpm_num_cells -= MR_complexity_cell_counter;
    metrics->MR_clpm_num_ticks -= MR_complexity_tick_counter;
    proc->MR_clp_is_active = MR_COMPLEXITY_IS_ACTIVE;
}

#endif  // MR_RECORD_TERM_SIZES
