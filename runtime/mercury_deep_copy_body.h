/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** The internals of deep copy.
**
** Functions such as "copy", "copy_arg", "copy_type_info", "in_range",
** etc can be #defined to whatever functions are needed for a particular
** copying application.
*/

/*
** Prototypes.
*/

static  MR_Word         copy_arg(maybeconst MR_Word *parent_data_ptr,
                            maybeconst MR_Word *data_ptr,
                            const MR_DuFunctorDesc *functor_descriptor,
                            const MR_TypeInfoParams type_params,
                            const MR_PseudoTypeInfo arg_pseudotype_info,
                            const MR_Word *lower_limit,
                            const MR_Word *upper_limit);
static  MR_TypeInfo     copy_type_info(maybeconst MR_TypeInfo *type_info_ptr,
                            const MR_Word *lower_limit,
                            const MR_Word *upper_limit);
static  MR_Word         copy_typeclass_info(maybeconst MR_Word
                            *typeclass_info_ptr, const MR_Word *lower_limit,
                            const MR_Word *upper_limit);

MR_Word
copy(maybeconst MR_Word *data_ptr, MR_TypeInfo type_info,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_Word             data;
    MR_Word             new_data;
    MR_TypeCtorInfo     type_ctor_info;
    MR_DuTypeLayout     du_type_layout;

    data = *data_ptr;

try_again:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    switch (MR_type_ctor_rep(type_ctor_info)) {

    case MR_TYPECTOR_REP_ENUM:
    case MR_TYPECTOR_REP_ENUM_USEREQ:
        new_data = data;    /* just a copy of the actual item */
        break;

    case MR_TYPECTOR_REP_RESERVED_ADDR:
    case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
        {
            int j;
            MR_ReservedAddrTypeLayout ra_layout;

            ra_layout =
                MR_type_ctor_layout(type_ctor_info).layout_reserved_addr;

            /*
            ** First check if this value is one of
            ** the numeric reserved addresses.
            */
            if ((MR_Unsigned) data <
                (MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
            {
                new_data = data;
                break;
            }

            /*
            ** Next check if this value is one of the
            ** the symbolic reserved addresses.
            */
            for (j = 0; j < ra_layout->MR_ra_num_res_symbolic_addrs; j++) {
                if (data == (MR_Word) ra_layout->MR_ra_res_symbolic_addrs[j]) {
                   new_data = data;
                   /* "break" here would just exit the "for" loop */
                   return new_data;
                }
            }
                
            /*
            ** Otherwise, it is not one of the reserved addresses,
            ** so handle it like a normal DU type.
            */
            du_type_layout = ra_layout->MR_ra_other_functors;
            goto du_type;
        }

    case MR_TYPECTOR_REP_DU:
    case MR_TYPECTOR_REP_DU_USEREQ:
        du_type_layout = MR_type_ctor_layout(type_ctor_info).layout_du;
        /* fallthru */

    /*
    ** This label handles both the DU case and the second half of the
    ** RESERVED_ADDR case.  `du_type_layout' must be set before
    ** this code is entered.
    */
    du_type:
        {
            MR_Word               *data_value;
            const MR_DuPtagLayout *ptag_layout;
            int                   ptag;

            ptag = MR_tag(data);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
            case MR_SECTAG_LOCAL:
                new_data = data;    /* just a copy of the actual item */
                break;

                /*
                ** The code we want to execute for the MR_SECTAG_REMOTE
                ** and MR_SECTAG_NONE cases is the following. However,
                ** this code checks the secondary tag location several times
                ** and also checks whether exist_info is NULL several times.
                ** Since speed is important, we duplicate the code downstream
                ** of the first test of each kind. To avoid double maintenance
                ** problems, the stuff that is duplicated is macro invocations;
                ** each macro is of course defined only once.
                */

/*
**          case MR_SECTAG_REMOTE:
**          case MR_SECTAG_NONE:
**
**              data_value = (MR_Word *) MR_body(data, ptag);
**              if (in_range(data_value)) {
**                  const MR_DuFunctorDesc  *functor_desc;
**                  const MR_DuExistInfo    *exist_info;
**                  MR_bool                 have_sectag;
**                  int                     sectag;
**                  int                     cell_size;
**                  int                     cur_slot;
**                  int                     arity;
**                  int                     num_ti_plain;
**                  int                     num_tci;
**                  int                     i;
**
**                  have_sectag =
**                      (ptag_layout->MR_sectag_locn != MR_SECTAG_NONE);
**                  if (!have_sectag) {
**                      sectag = 0;
**                  } else {
**                      sectag = data_value[0];
**                  }
**
**                  functor_desc = ptag_layout->MR_sectag_alternatives[sectag];
**                  arity = functor_desc->MR_du_functor_orig_arity;
**                  exist_info = functor_desc->MR_du_functor_exist_info;
**
**                  if (!have_sectag) {
**                      cell_size = arity;
**                  } else {
**                      cell_size = 1 + arity;
**                  }
**
**                  if (exist_info == NULL) {
**                      num_ti_plain = 0;
**                      num_tci = 0;
**                  } else {
**                      num_ti_plain = exist_info->MR_exist_typeinfos_plain;
**                      num_tci = exist_info->MR_exist_tcis;
**                      cell_size += num_ti_plain + num_tci;
**                  }
**
**                  MR_incr_saved_hp(new_data, cell_size);
**
**                  if (!have_sectag) {
**                      cur_slot = 0;
**                  } else {
**                      MR_field(0, new_data, 0) = sectag;
**                      cur_slot = 1;
**                  }
**
**                  for (i = 0; i < num_ti_plain; i++) {
**                      MR_field(0, new_data, cur_slot) = (MR_Word)
**                          copy_type_info((MR_TypeInfo *)
**                              &data_value[cur_slot],
**                              lower_limit, upper_limit);
**                      cur_slot++;
**                  }
**
**                  for (i = 0; i < num_tci; i++) {
**                      MR_field(0, new_data, cur_slot) = (MR_Word)
**                          copy_typeclass_info(&data_value[cur_slot],
**                              lower_limit, upper_limit);
**                      cur_slot++;
**                  }
**
**                  for (i = 0; i < arity; i++) {
**                      if (MR_arg_type_may_contain_var(functor_desc, i)) {
**                          MR_Word *parent_data = (MR_Word *) new_data;
**                          if (have_sectag) {
**                              // skip past the secondary tag
**                              parent_data++;
**                          }
**                          MR_field(0, new_data, cur_slot) =
**                              copy_arg(parent_data, &data_value[cur_slot],
**                                  functor_desc,
**                                  MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
**                                      type_info),
**                                  functor_desc->MR_du_functor_arg_types[i],
**                                  lower_limit, upper_limit);
**                      } else {
**                          MR_field(0, new_data, cur_slot) =
**                              copy(&data_value[cur_slot],
**                                  MR_pseudo_type_info_is_ground(
**                                  functor_desc->MR_du_functor_arg_types[i]),
**                                  lower_limit, upper_limit);
**                      }
**                      cur_slot++;
**                  }
**
**                  new_data = (MR_Word) MR_mkword(ptag, new_data);
**                  leave_forwarding_pointer(data_ptr, new_data);
**              } else {
**                  new_data = data;
**                  found_forwarding_pointer(data);
**              }
**              break;
*/

/*
** IMPORTANT: the macros below should be kept in sync with the comment above.
*/

#define MR_DC_decl                                                      \
                    const MR_DuFunctorDesc  *functor_desc;              \
                    const MR_DuExistInfo    *exist_info;                \
                    MR_bool                 have_sectag;                \
                    int                     sectag;                     \
                    int                     cell_size;                  \
                    int                     cur_slot;                   \
                    int                     arity;                      \
                    int                     num_ti_plain;               \
                    int                     num_tci;                    \
                    int                     i;

#define MR_DC_functor_desc                                              \
                    functor_desc = ptag_layout->MR_sectag_alternatives  \
                        [sectag];                                       \
                    arity = functor_desc->MR_du_functor_orig_arity;     \
                    exist_info = functor_desc->MR_du_functor_exist_info;

#define MR_DC_setup_exist_info                                          \
                        num_ti_plain = exist_info->MR_exist_typeinfos_plain;\
                        num_tci = exist_info->MR_exist_tcis;            \
                        cell_size += num_ti_plain + num_tci;

#define MR_DC_copy_exist_info                                           \
                    for (i = 0; i < num_ti_plain; i++) {                \
                        MR_field(0, new_data, cur_slot) = (MR_Word)     \
                            copy_type_info((MR_TypeInfo *)              \
                                &data_value[cur_slot],                  \
                                lower_limit, upper_limit);              \
                        cur_slot++;                                     \
                    }                                                   \
                                                                        \
                    for (i = 0; i < num_tci; i++) {                     \
                        MR_field(0, new_data, cur_slot) = (MR_Word)     \
                            copy_typeclass_info(&data_value[cur_slot],  \
                                lower_limit, upper_limit);              \
                        cur_slot++;                                     \
                    }

#define MR_DC_copy_plain_args                                               \
                    for (i = 0; i < arity; i++) {                           \
                        if (MR_arg_type_may_contain_var(functor_desc, i)) { \
                            MR_Word *parent_data = (MR_Word *) new_data;    \
                            if (have_sectag) {                              \
                                /* skip past the secondary tag */           \
                                parent_data++;                              \
                            }                                               \
                            MR_field(0, new_data, cur_slot) =               \
                                copy_arg(parent_data, &data_value[cur_slot],\
                                    functor_desc,                           \
                                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR( \
                                        type_info),                         \
                                    functor_desc->MR_du_functor_arg_types[i],\
                                    lower_limit, upper_limit);              \
                        } else {                                            \
                            MR_field(0, new_data, cur_slot) =               \
                                copy(&data_value[cur_slot],                 \
                                    MR_pseudo_type_info_is_ground(          \
                                    functor_desc->MR_du_functor_arg_types[i]),\
                                    lower_limit, upper_limit);              \
                        }                                                   \
                        cur_slot++;                                         \
                    }

            /*
            ** IMPORTANT: the code below should be kept in sync
            ** with the comment above.
            */

            case MR_SECTAG_REMOTE:
                data_value = (MR_Word *) MR_body(data, ptag);
                if (in_range(data_value)) {
                    MR_DC_decl
                    have_sectag = MR_TRUE;
                    sectag = data_value[0];
                    MR_DC_functor_desc
                    cell_size = 1 + arity;
                    if (exist_info != NULL) {
                        MR_DC_setup_exist_info
                        MR_incr_saved_hp(new_data, cell_size);
                        MR_field(0, new_data, 0) = sectag;
                        cur_slot = 1;
                        MR_DC_copy_exist_info
                    } else {
                        MR_incr_saved_hp(new_data, cell_size);
                        MR_field(0, new_data, 0) = sectag;
                        cur_slot = 1;
                    }
                    MR_DC_copy_plain_args

                    new_data = (MR_Word) MR_mkword(ptag, new_data);
                    leave_forwarding_pointer(data_ptr, new_data);
                } else {
                    new_data = data;
                    found_forwarding_pointer(data);
                }
                break;

            /*
            ** IMPORTANT: the code below should be kept in sync
            ** with the comment above.
            */

            case MR_SECTAG_NONE:
                data_value = (MR_Word *) MR_body(data, ptag);
                if (in_range(data_value)) {
                    MR_DC_decl
                    have_sectag = MR_FALSE;
                    sectag = 0;
                    MR_DC_functor_desc
                    cell_size = arity;
                    if (exist_info != NULL) {
                        MR_DC_setup_exist_info
                        MR_incr_saved_hp(new_data, cell_size);
                        cur_slot = 0;
                        MR_DC_copy_exist_info
                    } else {
                        MR_incr_saved_hp(new_data, cell_size);
                        cur_slot = 0;
                    }
                    MR_DC_copy_plain_args

                    new_data = (MR_Word) MR_mkword(ptag, new_data);
                    leave_forwarding_pointer(data_ptr, new_data);
                } else {
                    new_data = data;
                    found_forwarding_pointer(data);
                }
                break;

            case MR_SECTAG_VARIABLE:
                MR_fatal_error("copy(): attempt to copy variable");

            default:
                MR_fatal_error("copy(): unknown sectag_locn");

            } /* end switch on sectag_locn */
        }
        break;

    case MR_TYPECTOR_REP_NOTAG:
    case MR_TYPECTOR_REP_NOTAG_USEREQ:
        new_data = copy_arg(NULL, data_ptr, NULL,
            MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).layout_notag->
            MR_notag_functor_arg_type, lower_limit, upper_limit);
        break;

    case MR_TYPECTOR_REP_NOTAG_GROUND:
    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        type_info = MR_pseudo_type_info_is_ground(
            MR_type_ctor_layout(type_ctor_info).layout_notag
            ->MR_notag_functor_arg_type);
        goto try_again;
        break;

    case MR_TYPECTOR_REP_EQUIV:
        new_data = copy_arg(NULL, data_ptr, NULL,
            MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).layout_equiv,
            lower_limit, upper_limit);
        break;

    case MR_TYPECTOR_REP_EQUIV_GROUND:
        type_info = MR_pseudo_type_info_is_ground(
            MR_type_ctor_layout(type_ctor_info).layout_equiv);
        goto try_again;
        break;

    case MR_TYPECTOR_REP_INT:  /* fallthru */
    case MR_TYPECTOR_REP_CHAR:
        new_data = data;
        break;

    case MR_TYPECTOR_REP_FLOAT:
        #ifdef MR_BOXED_FLOAT
            {
                MR_Word    *data_value;

                assert(MR_tag(data) == 0);
                data_value = (MR_Word *) MR_body(data, MR_mktag(0));

                if (in_range(data_value)) {
                    MR_restore_transient_hp();
#ifdef MR_HIGHLEVEL_CODE
                    /*
                    ** We can't use MR_float_to_word, since it uses
                    ** MR_hp, which in grade hlc.par.gc will be a
                    ** reference to thread-local storage that we haven't
                    ** allocated.
                    */
                    new_data = (MR_Word) MR_box_float(MR_unbox_float(data));
#else
                    new_data = MR_float_to_word(MR_word_to_float(data));
#endif
                    MR_save_transient_hp();
                    leave_forwarding_pointer(data_ptr, new_data);
                } else {
                    new_data = data;
                    found_forwarding_pointer(data);
                }
            }
        #else
            new_data = data;
        #endif
        break;

    case MR_TYPECTOR_REP_STRING:
        {
            /*
            ** Not all Mercury strings are aligned; in particular,
            ** string constants containing the empty string may be
            ** allocated unaligned storage by the C compiler.
            */

            if (in_range((MR_Word *) data)) {
                MR_incr_saved_hp_atomic(new_data,
                    (strlen((MR_String) data) + sizeof(MR_Word)) / 
                        sizeof(MR_Word));
                strcpy((MR_String) new_data, (MR_String) data);
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
        }
        break;

    case MR_TYPECTOR_REP_FUNC:
    case MR_TYPECTOR_REP_PRED:
        {
            MR_Word    *data_value;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            /*
            ** Closures have the structure given by the MR_Closure type.
            **
            ** Their type_infos have a pointer to type_ctor_info for
            ** pred/0 or func/0, the number of argument typeinfos,
            ** and then the argument typeinfos themselves.
            **
            ** XXX pred needs to handle traversals.
            */
            if (in_range(data_value)) {
                MR_Unsigned         args, i;
                MR_Closure          *old_closure;
                MR_Closure          *new_closure;
                MR_Closure_Layout   *closure_layout;
                MR_TypeInfo         *type_info_arg_vector;

                old_closure = (MR_Closure *) data_value;
                closure_layout = old_closure->MR_closure_layout;
                args = old_closure->MR_closure_num_hidden_args;

                /* create new closure */
                MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, new_closure),
                    args + 3);

                /* copy the fixed fields */
                new_closure->MR_closure_layout = closure_layout;
                new_closure->MR_closure_num_hidden_args = args;
                new_closure->MR_closure_code = old_closure->MR_closure_code;
                        
                /*
                ** Fill in the pseudo_typeinfos in the closure layout
                ** with the values from the closure.
                */
                type_info_arg_vector = MR_materialize_closure_typeinfos(
                    old_closure);

                /* copy the arguments */
                for (i = 0; i < args; i++) {
                    MR_PseudoTypeInfo arg_pseudo_type_info;

                    arg_pseudo_type_info =
                        closure_layout->MR_closure_arg_pseudo_type_info[i];
                    new_closure->MR_closure_hidden_args_0[i] =
                        copy_arg(NULL,
                            &old_closure->MR_closure_hidden_args_0[i], NULL,
                            type_info_arg_vector, arg_pseudo_type_info,
                            lower_limit, upper_limit);
                }

                if (type_info_arg_vector != NULL) {
                    MR_free(type_info_arg_vector);
                }

                new_data = (MR_Word) new_closure;
                leave_forwarding_pointer(data_ptr, new_data);
            } else if (in_traverse_range(data_value)) {
                MR_fatal_error("sorry, unimplemented: traversal of closures");
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
        }
        break;

    case MR_TYPECTOR_REP_TUPLE:
        {
            MR_Word    *data_value;
            int     arity, i;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            if (in_range(data_value)) {
                MR_Word *new_data_ptr;
                MR_TypeInfo *arg_typeinfo_vector;

                arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);

                if (arity == 0) {
                    new_data = (MR_Word) NULL;
                } else {
                    /* allocate space for the new tuple */
                    MR_incr_saved_hp(new_data, arity);
                    new_data_ptr = (MR_Word *) new_data;

                    arg_typeinfo_vector =
                        MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info);
                    for (i = 0; i < arity; i++) {
                       /* type_infos are counted from one */
                       new_data_ptr[i] = copy(&data_value[i],
                            (const MR_TypeInfo) arg_typeinfo_vector[i + 1],
                            lower_limit, upper_limit);
                    }
                    leave_forwarding_pointer(data_ptr, new_data);
                }
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
        }
        break;

    case MR_TYPECTOR_REP_VOID:
        MR_fatal_error("Cannot copy a void type");
        break;

    case MR_TYPECTOR_REP_ARRAY:
        {
            MR_Word *data_value;
            int     i;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            if (in_range(data_value)) {
                MR_ArrayType *new_array;
                MR_ArrayType *old_array;
                MR_Integer array_size;

                old_array = (MR_ArrayType *) data_value;
                array_size = old_array->size;
                new_array = MR_make_array(array_size);
                new_array->size = array_size;
                for (i = 0; i < array_size; i++) {
                    new_array->elements[i] = copy_arg(NULL,
                        &old_array->elements[i], NULL,
                        MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                        (const MR_PseudoTypeInfo) 1, lower_limit, upper_limit);
                }
                new_data = (MR_Word) new_array;
                leave_forwarding_pointer(data_ptr, new_data);
            } else if (in_traverse_range(data_value)) {
                MR_ArrayType *old_array;
                MR_Integer array_size;

                old_array = (MR_ArrayType *) data_value;
                array_size = old_array->size;
                for (i = 0; i < array_size; i++) {
                    copy_arg(NULL, 
                        &old_array->elements[i], NULL, 
                        MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                        (const MR_PseudoTypeInfo) 1, lower_limit, upper_limit);
                }
                new_data = data;
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
        }
        break;

    case MR_TYPECTOR_REP_TYPEINFO:
        new_data = (MR_Word) copy_type_info((MR_TypeInfo *) data_ptr,
            lower_limit, upper_limit);
        break;

    case MR_TYPECTOR_REP_TYPECTORINFO:
        /* type_ctor_infos are always static */
        new_data = data;
        break;

    case MR_TYPECTOR_REP_TYPECLASSINFO:
        new_data = (MR_Word) copy_typeclass_info(data_ptr,
            lower_limit, upper_limit);
        break;

    case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        /* base_typeclass_infos are always static */
        new_data = data;
        break;

    case MR_TYPECTOR_REP_C_POINTER:
        {
            MR_Word *data_value;
            int     data_tag;

            /* XXX simplify: tag should be zero */
            data_tag = MR_tag(data);
            data_value = (MR_Word *) MR_body(data, data_tag);

            if (in_range(data_value) || in_traverse_range(data_value)) {
                /*
                ** This error occurs if we try to copy() a
                ** `c_pointer' type that points to memory allocated
                ** on the Mercury heap.
                */
                MR_fatal_error("Cannot copy a c_pointer type");
            } else {
                new_data = data;
            }
        }
        break;

    case MR_TYPECTOR_REP_SUCCIP: /* fallthru */
    case MR_TYPECTOR_REP_REDOIP:
        /* code addresses are never relocated */
        new_data = data;
        break;

    case MR_TYPECTOR_REP_HP:
        /*
        ** Tyson hasn't yet moved the code for copying saved heap pointers
        ** here.
        */
        MR_fatal_error("Sorry, not implemented: copying saved heap pointers");
        break;

    case MR_TYPECTOR_REP_CURFR: /* fallthru */
    case MR_TYPECTOR_REP_MAXFR:
        /* we do not modify the layout of the nondet stack */
        new_data = data;
        break;

    case MR_TYPECTOR_REP_TRAIL_PTR:
    case MR_TYPECTOR_REP_TICKET:
        /* XXX we do not yet compress the trail when doing gc */
        new_data = data;
        break;

    case MR_TYPECTOR_REP_UNKNOWN: /* fallthru */
    default:
        MR_fatal_error("Unknown layout type in deep copy");
        break;
    }

    return new_data;
}

/*
** copy_arg is like copy() except that it takes a pseudo_type_info
** (namely arg_pseudo_type_info) rather than a type_info.
** The pseudo_type_info may contain type variables,
** which refer to arguments of the term_type_info.
**
** It also takes a pointer to the data of the parent of this piece of data
** and a functor descriptor for the parent in case the data being copied is
** existentially quantified.
*/

static MR_Word
copy_arg(maybeconst MR_Word *parent_data_ptr, maybeconst MR_Word *data_ptr,
    const MR_DuFunctorDesc *functor_descriptor,
    const MR_TypeInfoParams type_params,
    const MR_PseudoTypeInfo arg_pseudo_type_info,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_MemoryList   allocated_memory_cells;
    MR_TypeInfo     new_type_info;
    MR_Word         new_data;

    allocated_memory_cells = NULL;
    new_type_info = MR_make_type_info_maybe_existq(type_params,
        arg_pseudo_type_info, parent_data_ptr,
        functor_descriptor, &allocated_memory_cells);

    new_data = copy(data_ptr, new_type_info, lower_limit, upper_limit);
    MR_deallocate(allocated_memory_cells);

    return new_data;
}

static MR_TypeInfo
copy_type_info(maybeconst MR_TypeInfo *type_info_ptr,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_TypeInfo type_info = *type_info_ptr;

    if (in_range((MR_Word *) type_info)) {
        MR_TypeCtorInfo type_ctor_info;
        MR_Word         *new_type_info_arena;
        MR_TypeInfo     *type_info_args;
        MR_TypeInfo     *new_type_info_args;
        int             arity;
        int             i;

        /*
        ** Note that we assume type_ctor_infos will always be
        ** allocated statically, so we never copy them.
        */

        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        /*
        ** Optimize a special case: if there are no arguments,
        ** we don't need to construct a type_info; instead,
        ** we can just return the type_ctor_info.
        */

        if ((MR_Word) type_info == (MR_Word) type_ctor_info) {
            return (MR_TypeInfo) type_ctor_info;
        }

        if (MR_type_ctor_rep_is_variable_arity(
            MR_type_ctor_rep(type_ctor_info)))
        {
            arity = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info);
            type_info_args =
                MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(type_info);
            MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, new_type_info_arena),
                MR_higher_order_type_info_size(arity));
            MR_fill_in_higher_order_type_info(new_type_info_arena,
                type_ctor_info, arity, new_type_info_args);
        } else {
            arity = type_ctor_info->MR_type_ctor_arity;
            type_info_args = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
            MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, new_type_info_arena),
                MR_first_order_type_info_size(arity));
            MR_fill_in_first_order_type_info(new_type_info_arena,
                type_ctor_info, new_type_info_args);
        }
        for (i = 1; i <= arity; i++) {
            new_type_info_args[i] = copy_type_info(&type_info_args[i],
                lower_limit, upper_limit);
        }
        leave_forwarding_pointer((MR_Word *) type_info_ptr,
            (MR_Word) new_type_info_arena);
        return (MR_TypeInfo) new_type_info_arena;
    } else {
        found_forwarding_pointer(type_info);
        return type_info;
    }
}

static MR_Word
copy_typeclass_info(maybeconst MR_Word *typeclass_info_ptr,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_Word *typeclass_info = (MR_Word *) *typeclass_info_ptr;

    if (in_range(typeclass_info)) {
        MR_Word *base_typeclass_info;
        MR_Word *new_typeclass_info;
        int     num_arg_typeinfos;
        int     num_super;
        int     num_instance_constraints;
        int     num_unconstrained;
        int     i;

        /*
        ** Note that we assume base_typeclass_infos will always be
        ** allocated statically, so we never copy them.
        */

        base_typeclass_info = (MR_Word *) *typeclass_info;

        num_instance_constraints = 
            MR_typeclass_info_num_instance_constraints(typeclass_info);
        num_unconstrained = 
            MR_typeclass_info_num_extra_instance_args(typeclass_info) 
                - num_instance_constraints;
        num_super = MR_typeclass_info_num_superclasses(typeclass_info);
        num_arg_typeinfos = MR_typeclass_info_num_type_infos(typeclass_info);
        MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, new_typeclass_info),
            num_instance_constraints + num_super + num_arg_typeinfos + 1);

        new_typeclass_info[0] = (MR_Word) base_typeclass_info;

            /* 
            ** First, copy typeinfos for unconstrained tvars from
            ** the instance declaration 
            */
        for (i = 1; i < num_unconstrained + 1; i++) {
            new_typeclass_info[i] = (MR_Word) copy_type_info(
                (MR_TypeInfo *)(&typeclass_info[i]), lower_limit, upper_limit);
        }
            /*
            ** Next, copy all the typeclass infos: both the ones for
            ** constraints on the instance declaration (instance
            ** constraints), and the ones for constraints on the
            ** typeclass declaration (superclass constraints).  
            */
        for (i = num_unconstrained + 1; 
            i <= num_unconstrained + num_instance_constraints + num_super; 
            i++) 
        {
            new_typeclass_info[i] = (MR_Word) copy_typeclass_info(
                &typeclass_info[i], lower_limit, upper_limit);
        }

            /*
            ** Then, copy all the type infos for types in the
            ** head of the type class declaration.
            */
        for (i = num_unconstrained + num_instance_constraints + num_super + 1;
            i <= num_unconstrained + num_instance_constraints + num_super
                + num_arg_typeinfos;
            i++)
        {
            new_typeclass_info[i] = (MR_Word) copy_type_info(
                (MR_TypeInfo *) &typeclass_info[i], lower_limit, upper_limit);
        }
        leave_forwarding_pointer(typeclass_info_ptr,
            (MR_Word) new_typeclass_info);
        return (MR_Word) new_typeclass_info;
    } else {
        found_forwarding_pointer(typeclass_info);
        return (MR_Word) typeclass_info;
    }
}
