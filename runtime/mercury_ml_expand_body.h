/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_ml_expand_body.h
**
** This file is included several times in runtime/mercury_deconstruct.c. Each
** inclusion defines the body of one of several variants of the old ML_expand
** function, which, given a data word and its type_info, returned its functor,
** arity, argument vector and a type_info vector describing its arguments.
** One variant still does all that. The others perform different subsets of
** this task. The reason for having those specialized variants is that
** executing the full task can be extremely time consuming, especially when
** large arrays are involved. (Simply allocating and filling in an array of
** a million typeinfos can cause a system to start paging.) Therefore we try to
** make sure that in every circumstance we perform the minimum work possible.
**
** The code including this file must define these macros:
**
** EXPAND_FUNCTION_NAME     Gives the name of the function being defined.
**
** EXPAND_TYPE_NAME         Gives the name of the type of the expand_info
**                          argument.
**
** The code including this file may define these macros:
**
** EXPAND_FUNCTOR_FIELD     If defined, gives the name of the field in the
**                          expand_info structure that contains the name of the
**                          functor. This field should be of type
**                          MR_ConstString. The function will fill in this
**                          field.
**
** EXPAND_ARGS_FIELD        If defined, gives the name of the field in the
**                          expand_info structure that contains information
**                          about all the functor's arguments. This field
**                          should be of type MR_Expand_Args_Fields. The
**                          function will fill in this field.
**
** EXPAND_CHOSEN_ARG        If defined, the function will have an extra
**                          argument, chosen, which specifies the position of
**                          the one desired argument (with the first argument
**                          having position 0), and the function will fill in
**                          the fields of the MR_Expand_Chosen_Arg_Only
**                          structure.
**
** EXPAND_NAMED_ARG         If defined, the function will have an extra
**                          argument, chosen_name, which specifies the name
**                          of the one desired argument, and the function will
**                          fill in the fields of the MR_Expand_Chosen_Arg_Only
**                          structure.
**
** EXPAND_APPLY_LIMIT       If defined, the function will have an extra
**                          argument, max_arity. If the number of arguments
**                          exceeds this limit, the function will store FALSE
**                          in the limit_reached field of expand_info and will
**                          not fill in the other fields about the arguments.
**                          
**
** Most combinations are allowed, but
**
** - only one of EXPAND_ARGS_FIELD, EXPAND_CHOSEN_ARG and EXPAND_NAMED_ARG
**   may be defined at once, and
** - EXPAND_APPLY_LIMIT should be defined only if EXPAND_ARGS_FIELD is also
**   defined.
**
** Each variant of the function will fill in all the fields of the expand_info
** structure passed to it, although the set of fields in that structure will
** be different for different variants. The type in EXPAND_TYPE_NAME must be
** consistent with the set of defined optional macros.
**
** All variants contain the the integer field arity, which will be set to
** the number of arguments the functor has.
**
** The variants that return all the arguments do so in a field of type
** MR_Expand_Args_Fields. Its arg_type_infos subfield will contain a pointer
** to an array of arity MR_TypeInfos, one for each user-visible field of the
** cell. The arg_values field will contain a pointer to a block of
** arity + num_extra_args MR_Words, one for each field of the cell,
** whether user-visible or not. The first num_extra_args words will be
** the type infos and/or typeclass infos added by the implementation to
** describe the types of the existentially typed fields, while the last
** arity words will be the user-visible fields themselves.
**
** If the can_free_arg_type_infos field is true, then the array returned
** in the arg_type_infos field was allocated by this function, and should be
** freed by the caller when it has finished using the information it contains.
** Since the array will have been allocated using MR_GC_malloc(), it should be
** freed with MR_GC_free. (We need to use MR_GC_malloc() rather than
** MR_malloc() or malloc(), since this vector may contain pointers into the
** Mercury heap, and memory allocated with MR_malloc() or malloc() will not be
** traced by the Boehm collector.) The elements of the array should not be
** freed, since they point to previously allocated data, which is either
** on the heap or is in constant storage (e.g. type_ctor_infos).
** If the can_free_arg_type_infos field is false, then the array returned in
** the arg_type_infos field was not allocated by the function (it came from the
** type_info argument passed to it) and must not be freed.
**
** Please note:
**  These functions increment the heap pointer; however, on some platforms
**  the register windows mean that transient Mercury registers may be lost.
**  Before calling these functions, call MR_save_transient_registers(), and
**  afterwards, call MR_restore_transient_registers().
**
**  If you change this code, you may also have to reflect your changes
**  in runtime/mercury_deep_copy_body.h and runtime/mercury_tabling.c.
**
**  In several places, we call MR_fatal_error to signal inappropriate
**  deconstruction of noncanonical terms. These should all throw exceptions
**  instead, but it is not yet safe to throw exceptions across the C interface.
*/

#include    <stdio.h>
#include    "mercury_library_types.h"       /* for MR_ArrayType */

#ifdef MR_DEEP_PROFILING
  #include  "mercury_deep_profiling.h"
#endif

/* set up for recursive calls */
#ifdef  EXPAND_APPLY_LIMIT
  #define   EXTRA_ARG1  max_arity,
#else
  #define   EXTRA_ARG1
#endif
#ifdef  EXPAND_CHOSEN_ARG
  #define   EXTRA_ARG2  chosen,
#else
  #define   EXTRA_ARG2
#endif
#ifdef  EXPAND_NAMED_ARG
  #define   EXTRA_ARG3  chosen_name,
#else
  #define   EXTRA_ARG3
#endif
#define EXTRA_ARGS  EXTRA_ARG1 EXTRA_ARG2 EXTRA_ARG3

#if defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG)
  #define   EXPAND_ONE_ARG
#else   /* defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG) */
  #undef    EXPAND_ONE_ARG
#endif  /* defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG) */

/* set up macro for setting field names without #ifdefs */
#ifdef  EXPAND_FUNCTOR_FIELD
  #define handle_functor_name(name)                                     \
            do {                                                        \
                MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                              \
            } while (0)
  #define handle_noncanonical_name(tci)                                 \
            do {                                                        \
                MR_ConstString  name;                                   \
                                                                        \
                MR_restore_transient_hp();                              \
                name = MR_expand_type_name(tci);                        \
                MR_save_transient_hp();                                 \
                MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                              \
            } while (0)
#else   /* EXPAND_FUNCTOR_FIELD */
  #define handle_functor_name(name)                                     \
            ((void) 0)
  #define handle_noncanonical_name(tci)                                 \
            ((void) 0)
#endif  /* EXPAND_FUNCTOR_FIELD */

/* set up macros for the common code handling zero arity terms */

#ifdef  EXPAND_ARGS_FIELD
  #define handle_zero_arity_all_args()                                  \
            do {                                                        \
                expand_info->EXPAND_ARGS_FIELD.arg_values = NULL;       \
                expand_info->EXPAND_ARGS_FIELD.arg_type_infos = NULL;   \
                expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;      \
            } while (0)
#else   /* EXPAND_ARGS_FIELD */
  #define handle_zero_arity_all_args()                                  \
            ((void) 0)
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
  #define handle_zero_arity_one_arg()                                   \
            do {                                                        \
                expand_info->chosen_index_exists = FALSE;               \
            } while (0)
#else   /* EXPAND_ONE_ARG */
  #define handle_zero_arity_one_arg()                                   \
            ((void) 0)
#endif  /* EXPAND_ONE_ARG */

#define handle_zero_arity_args()                                        \
            do {                                                        \
                expand_info->arity = 0;                                 \
                handle_zero_arity_all_args();                           \
                handle_zero_arity_one_arg();                            \
            } while (0)

/***********************************************************************/

void
EXPAND_FUNCTION_NAME(MR_TypeInfo type_info, MR_Word *data_word_ptr,
    MR_noncanon_handling noncanon,
#ifdef  EXPAND_APPLY_LIMIT
    int max_arity,
#endif  /* EXPAND_APPLY_LIMIT */
#ifdef  EXPAND_CHOSEN_ARG
    int chosen,
#endif  /* EXPAND_CHOSEN_ARG */
#ifdef  EXPAND_NAMED_ARG
    MR_ConstString chosen_name,
#endif  /* EXPAND_NAMED_ARG */
    EXPAND_TYPE_NAME *expand_info)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_DuTypeLayout du_type_layout;
#ifdef EXPAND_NAMED_ARG
    /*
    ** No arm of the switch on type_ctor_rep handles named arguments by
    ** default. Only those type_ctor_reps that support named arguments
    ** need have code for searching for argument names. For the rest,
    ** initializing chosen to -1 ensures that no argument will be returned.
    */
    int chosen = -1;
#endif /* EXPAND_NAMED_ARG */

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
#ifdef  EXPAND_ARGS_FIELD
    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = FALSE;
#endif  /* EXPAND_ARGS_FIELD */
#ifdef  EXPAND_APPLY_LIMIT
    expand_info->limit_reached = FALSE;
#endif  /* EXPAND_APPLY_LIMIT */

    switch(MR_type_ctor_rep(type_ctor_info)) {

        case MR_TYPECTOR_REP_ENUM_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                break;
            }
            /* else fall through */

        case MR_TYPECTOR_REP_ENUM:
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                    layout_enum[*data_word_ptr]->MR_enum_functor_name);
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                break;
            }
            /* else fall through */

        case MR_TYPECTOR_REP_RESERVED_ADDR:
            {
				int i;
				MR_Word data;
				MR_ReservedAddrTypeLayout ra_layout;

				ra_layout = MR_type_ctor_layout(type_ctor_info).layout_reserved_addr;
				data = *data_word_ptr;

				/*
				** First check if this value is one of
				** the numeric reserved addresses.
				*/
				if ((MR_Unsigned) data <
					(MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
				{
					handle_functor_name(ra_layout->MR_ra_constants[data]->
							MR_ra_functor_name);
					handle_zero_arity_args();
					break;
				}

				/*
				** Next check if this value is one of the
				** the symbolic reserved addresses.
				*/
				for (i = 0; i < ra_layout->MR_ra_num_res_symbolic_addrs; i++) {
                    if (data == (MR_Word) ra_layout->
                            MR_ra_res_symbolic_addrs[i])
                    {
                        int offset;
                        offset = i + ra_layout->MR_ra_num_res_numeric_addrs;
                        handle_functor_name(ra_layout->
                            MR_ra_constants[offset]->MR_ra_functor_name);
						handle_zero_arity_args();
						/* "break" here would just exit the "for" loop */
						return;
					}
				}

				/*
				** Otherwise, it is not one of the reserved addresses,
				** so handle it like a normal DU type.
				*/
				du_type_layout = ra_layout->MR_ra_other_functors;
				goto du_type;
			}

        case MR_TYPECTOR_REP_DU_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                break;
            }
            /* else fall through */

        case MR_TYPECTOR_REP_DU:
			du_type_layout = MR_type_ctor_layout(type_ctor_info).layout_du;
			/* fall through */

			/*
			** This label handles both the DU case and the second half of the
			** RESERVED_ADDR case.  `du_type_layout' must be set before
			** this code is entered.
			*/
		du_type:
			{
                const MR_DuPtagLayout   *ptag_layout;
                const MR_DuFunctorDesc  *functor_desc;
                const MR_DuExistInfo    *exist_info;
                int                     extra_args;
                MR_Word                 data;
                int                     ptag;
                MR_Word                 sectag;
                MR_Word                 *arg_vector;

                data = *data_word_ptr;
                ptag = MR_tag(data);
                ptag_layout = &du_type_layout[ptag];

                switch (ptag_layout->MR_sectag_locn) {
                    case MR_SECTAG_NONE:
                        functor_desc = ptag_layout->MR_sectag_alternatives[0];
                        arg_vector = (MR_Word *) MR_body(data, ptag);
                        break;
                    case MR_SECTAG_LOCAL:
                        sectag = MR_unmkbody(data);
                        functor_desc =
                            ptag_layout->MR_sectag_alternatives[sectag];
                        arg_vector = NULL;
                        break;
                    case MR_SECTAG_REMOTE:
                        sectag = MR_field(ptag, data, 0);
                        functor_desc =
                            ptag_layout->MR_sectag_alternatives[sectag];
                        arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
                        break;
                    case MR_SECTAG_VARIABLE:
                        if (noncanon != MR_NONCANON_CC) {
                            /* XXX should throw an exception */
                            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                                ": attempt to deconstruct variable");
                            break;
                        } 

                        handle_functor_name("<<variable>>");
                        handle_zero_arity_args();
						return;
				}

                handle_functor_name(functor_desc->MR_du_functor_name);
                expand_info->arity = functor_desc->MR_du_functor_orig_arity;

#if     defined(EXPAND_ARGS_FIELD) || defined(EXPAND_ONE_ARG)
                exist_info = functor_desc->MR_du_functor_exist_info;
                if (exist_info != NULL) {
                    extra_args = exist_info->MR_exist_typeinfos_plain
                        + exist_info->MR_exist_tcis;
                } else {
                    extra_args = 0;
                }
#endif  /* defined(EXPAND_ARGS_FIELD) || defined(EXPAND_ONE_ARG) */

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (expand_info->arity > max_arity) {
                    expand_info->limit_reached = TRUE;
                } else
  #endif    /* EXPAND_APPLY_LIMIT */
                {
                    int i;

                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = extra_args;
                    expand_info->EXPAND_ARGS_FIELD.arg_values = arg_vector;
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        TRUE;
                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, expand_info->arity);

                    for (i = 0; i < expand_info->arity; i++) {
                        if (MR_arg_type_may_contain_var(functor_desc, i)) {
                            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                                MR_create_type_info_maybe_existq(
                                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
                                        type_info),
                                    functor_desc->MR_du_functor_arg_types[i],
                                    arg_vector, functor_desc);
                        } else {
                            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                                MR_pseudo_type_info_is_ground(
                                    functor_desc->MR_du_functor_arg_types[i]);
                        }
                    }
                }
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
  #ifdef  EXPAND_NAMED_ARG
                {
                    int i;

                    for (i = 0; i < expand_info->arity; i++) {
                        if (functor_desc->MR_du_functor_arg_names[i] != NULL
                            && streq(functor_desc->MR_du_functor_arg_names[i],
                                chosen_name))
                        {
                            chosen = i;
                            break;
                        }
                    }
                }
  #endif  /* EXPAND_NAMED_ARG */

                if (0 <= chosen && chosen < expand_info->arity) {
                    expand_info->chosen_index_exists = TRUE;
                    expand_info->chosen_value_ptr =
                        &arg_vector[extra_args + chosen];
                    if (MR_arg_type_may_contain_var(functor_desc, chosen)) {
                        expand_info->chosen_type_info =
                            MR_create_type_info_maybe_existq(
                                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
                                    type_info),
                                functor_desc->MR_du_functor_arg_types[chosen],
                                arg_vector, functor_desc);
                    } else {
                        expand_info->chosen_type_info =
                            MR_pseudo_type_info_is_ground(
                                functor_desc->MR_du_functor_arg_types[chosen]);
                    }
                } else {
                    expand_info->chosen_index_exists = FALSE;
                }
#endif  /* EXPAND_ONE_ARG */
            }
            break;

        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                break;
            }
            /* else fall through */

        case MR_TYPECTOR_REP_NOTAG:
            expand_info->arity = 1;
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                layout_notag->MR_notag_functor_name);

#ifdef  EXPAND_ARGS_FIELD
            expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
            expand_info->EXPAND_ARGS_FIELD.arg_values = data_word_ptr;
            expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = TRUE;
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                MR_GC_NEW_ARRAY(MR_TypeInfo, 1);
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[0] =
                MR_create_type_info(
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).layout_notag->
                        MR_notag_functor_arg_type);
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
  #ifdef    EXPAND_NAMED_ARG
            if (MR_type_ctor_layout(type_ctor_info).layout_notag
                    ->MR_notag_functor_arg_name != NULL
               && streq(chosen_name, MR_type_ctor_layout(type_ctor_info).
                    layout_notag->MR_notag_functor_arg_name))
            {
                chosen = 0;
            }
  #endif    /* EXPAND_NAMED_ARG */

            if (chosen == 0) {
                expand_info->chosen_index_exists = TRUE;
                expand_info->chosen_value_ptr = data_word_ptr;
                expand_info->chosen_type_info =
                    MR_create_type_info(
                        MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                        MR_type_ctor_layout(type_ctor_info).layout_notag->
                            MR_notag_functor_arg_type);
            } else {
                expand_info->chosen_index_exists = FALSE;
            }
#endif  /* EXPAND_ONE_ARG */
            break;

        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                break;
            }
            /* else fall through */

        case MR_TYPECTOR_REP_NOTAG_GROUND:
            expand_info->arity = 1;
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                layout_notag->MR_notag_functor_name);

#ifdef  EXPAND_ARGS_FIELD
            expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
            expand_info->EXPAND_ARGS_FIELD.arg_values = data_word_ptr;
            expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = TRUE;
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                MR_GC_NEW_ARRAY(MR_TypeInfo, 1);
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[0] =
                MR_pseudo_type_info_is_ground(
                    MR_type_ctor_layout(type_ctor_info).layout_notag->
                        MR_notag_functor_arg_type);
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
  #ifdef    EXPAND_NAMED_ARG
            if (MR_type_ctor_layout(type_ctor_info).layout_notag
                    ->MR_notag_functor_arg_name != NULL
               && streq(chosen_name, MR_type_ctor_layout(type_ctor_info).
                    layout_notag->MR_notag_functor_arg_name))
            {
                chosen = 0;
            }
  #endif    /* EXPAND_NAMED_ARG */

            if (chosen == 0) {
                expand_info->chosen_index_exists = TRUE;
                expand_info->chosen_value_ptr = data_word_ptr;
                expand_info->chosen_type_info =
                MR_pseudo_type_info_is_ground(
                    MR_type_ctor_layout(type_ctor_info).layout_notag
                        ->MR_notag_functor_arg_type);
            } else {
                expand_info->chosen_index_exists = FALSE;
            }
#endif  /* EXPAND_ONE_ARG */
            break;

        case MR_TYPECTOR_REP_EQUIV:
            {
                MR_TypeInfo eqv_type_info;

                eqv_type_info = MR_create_type_info(
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).layout_equiv);
                EXPAND_FUNCTION_NAME(eqv_type_info, data_word_ptr, noncanon,
                    EXTRA_ARGS expand_info);
            }
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            EXPAND_FUNCTION_NAME(MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).layout_equiv),
                data_word_ptr, noncanon, EXTRA_ARGS expand_info);
            break;

        case MR_TYPECTOR_REP_INT:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%ld", (long) data_word);
                MR_incr_saved_hp_atomic(MR_LVALUE_CAST(MR_Word, str),
                    (strlen(buf) + sizeof(MR_Word)) / sizeof(MR_Word));
                strcpy(str, buf);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  /* EXPAND_FUNCTOR_FIELD */

            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_CHAR:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                /* XXX should escape characters correctly */
                MR_Word data_word;
                char    *str;

                data_word = *data_word_ptr;
                MR_incr_saved_hp_atomic(MR_LVALUE_CAST(MR_Word, str),
                    (3 + sizeof(MR_Word)) / sizeof(MR_Word));
                    sprintf(str, "\'%c\'", (char) data_word);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  /* EXPAND_FUNCTOR_FIELD */

            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_FLOAT:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word     data_word;
                char        buf[500];
                MR_Float    f;
                char        *str;

                data_word = *data_word_ptr;
                f = MR_word_to_float(data_word);
                sprintf(buf, "%#.15g", f);
                MR_incr_saved_hp_atomic(MR_LVALUE_CAST(MR_Word, str),
                    (strlen(buf) + sizeof(MR_Word)) / sizeof(MR_Word));
                strcpy(str, buf);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  /* EXPAND_FUNCTOR_FIELD */

            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_STRING:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                /* XXX should escape characters correctly */
                MR_Word data_word;
                char    *str;

                data_word = *data_word_ptr;
                MR_incr_saved_hp_atomic(MR_LVALUE_CAST(MR_Word, str),
                    (strlen((MR_String) data_word) + 2 + sizeof(MR_Word))
                    / sizeof(MR_Word));
                sprintf(str, "%c%s%c", '"', (MR_String) data_word, '"');
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  /* EXPAND_FUNCTOR_FIELD */

            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_FUNC:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<function>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_PRED:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<predicate>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TUPLE:
            expand_info->arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);
            handle_functor_name("{}");

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
            if (expand_info->arity > max_arity) {
                expand_info->limit_reached = TRUE;
            } else
  #endif    /* EXPAND_APPLY_LIMIT */
            {
                expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                expand_info->EXPAND_ARGS_FIELD.arg_values =
                    (MR_Word *) *data_word_ptr;

                /*
                ** Type-infos are normally counted from one, but
                ** the users of this vector count from zero.
                */
                expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info) + 1;
            }
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
            if (0 <= chosen && chosen < expand_info->arity) {
                MR_Word *arg_vector;

                arg_vector = (MR_Word *) *data_word_ptr;
                expand_info->chosen_index_exists = TRUE;
                expand_info->chosen_value_ptr = &arg_vector[chosen];
                expand_info->chosen_type_info =
                    MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info)[chosen + 1];
            } else {
                expand_info->chosen_index_exists = FALSE;
            }
#endif  /* EXPAND_ONE_ARG */
            break;

        case MR_TYPECTOR_REP_UNIV: {
            MR_Word data_word;

            MR_TypeInfo univ_type_info;
            MR_Word univ_data;
                /*
                 * Univ is a two word structure, containing
                 * type_info and data.
                 */
            data_word = *data_word_ptr;
            MR_unravel_univ(data_word, univ_type_info, univ_data);
            EXPAND_FUNCTION_NAME(univ_type_info, &univ_data, noncanon,
                EXTRA_ARGS expand_info);
            break;
        }

        case MR_TYPECTOR_REP_VOID:
            /*
            ** There's no way to create values of type `void',
            ** so this should never happen.
            */
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": cannot expand void types");

        case MR_TYPECTOR_REP_C_POINTER:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<c_pointer>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TYPEINFO:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<typeinfo>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TYPECTORINFO:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<typectorinfo>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<typeclassinfo>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            if (noncanon == MR_NONCANON_ABORT) {
                /* XXX should throw an exception */
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                break;
            }

            handle_functor_name("<<basetypeclassinfo>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_ARRAY:
            {
                MR_ArrayType    *array;

                array = (MR_ArrayType *) *data_word_ptr;
                expand_info->arity = array->size;

                handle_functor_name("<<array>>");

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (expand_info->arity > max_arity) {
                    expand_info->limit_reached = TRUE;
                } else
  #endif    /* EXPAND_APPLY_LIMIT */
                {
                    MR_TypeInfoParams   params;
                    int                 i;

                    params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                    expand_info->EXPAND_ARGS_FIELD.arg_values =
                        &array->elements[0];
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        TRUE;
                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, array->size);
                    for (i = 0; i < array->size; i++) {
                        expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                            params[1];
                    }
                }
#endif  /* EXPAND_ARGS_FIELD */

#ifdef  EXPAND_ONE_ARG
                if (0 <= chosen && chosen < array->size) {
                    MR_TypeInfoParams   params;

                    params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                    expand_info->chosen_value_ptr = &array->elements[chosen];
                    expand_info->chosen_type_info = params[1];
                    expand_info->chosen_index_exists = TRUE;
                } else {
                    expand_info->chosen_index_exists = FALSE;
                }
#endif  /* EXPAND_ONE_ARG */
            }
            break;

        case MR_TYPECTOR_REP_SUCCIP:
            handle_functor_name("<<succip>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_HP:
            handle_functor_name("<<hp>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_CURFR:
            handle_functor_name("<<curfr>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_MAXFR:
            handle_functor_name("<<maxfr>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_REDOFR:
            handle_functor_name("<<redofr>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_REDOIP:
            handle_functor_name("<<redoip>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TRAIL_PTR:
            handle_functor_name("<<trail_ptr>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_TICKET:
            handle_functor_name("<<ticket>>");
            handle_zero_arity_args();
            break;

        case MR_TYPECTOR_REP_UNKNOWN:    /* fallthru */
        default:
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": cannot expand -- unknown data type");
            break;
    }
}

#undef  EXTRA_ARG1
#undef  EXTRA_ARG2
#undef  EXTRA_ARG3
#undef  EXTRA_ARGS
#undef  EXPAND_ONE_ARG
#undef  handle_functor_name
#undef  handle_noncanonical_name
#undef  handle_zero_arity_args
#undef  handle_zero_arity_all_args
#undef  handle_zero_arity_one_arg
