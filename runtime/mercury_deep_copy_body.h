// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2005, 2007, 2012 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// The internals of deep copy.
//
// Functions such as "copy", "copy_arg", "copy_type_info", "in_range",
// etc can be #defined to whatever functions are needed for a particular
// copying application.

// Prototypes.

static  MR_Word             copy_arg(const MR_Word *parent_data_ptr,
                                MR_Word data,
                                const MR_DuFunctorDesc *functor_descriptor,
                                const MR_TypeInfoParams type_params,
                                const MR_PseudoTypeInfo arg_pseudotype_info,
                                const MR_Word *lower_limit,
                                const MR_Word *upper_limit);
static  MR_TypeInfo         copy_type_info(MR_TypeInfo type_info,
                                const MR_Word *lower_limit,
                                const MR_Word *upper_limit);
static  MR_PseudoTypeInfo   copy_pseudo_type_info(
                                MR_PseudoTypeInfo pseudo_type_info,
                                const MR_Word *lower_limit,
                                const MR_Word *upper_limit);
static  MR_Word             copy_typeclass_info(MR_Word typeclass_info,
                                const MR_Word *lower_limit,
                                const MR_Word *upper_limit);
#ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
static MR_AllocSiteInfoPtr  maybe_attrib(MR_Word *data_value);
#else
#define maybe_attrib(x)     NULL
#endif

// We need to make sure that we don't clobber any part of the closure
// which might be used by the collector for tracing stack frames of
// closure wrapper functions. So we store the forwarding pointer for closure
// in the MR_closure_code field (which is not used by the collector),
// rather than at offset zero (where it would clobber the closure layout,
// which is used by the collector).

#define CLOSURE_FORWARDING_PTR_OFFSET                                   \
    (offsetof(MR_Closure, MR_closure_code) / sizeof(MR_Word))

// We must not clobber type_infos or typeclass_infos with forwarding pointers,
// since they may be referenced by the garbage collector during collection.
// Unfortunately in this case there is no spare field which we can use.
// So we allocate an extra word before the front of the object (see the code
// for new_object in compiler/mlds_to_c.m), and use that for the forwarding
// pointer. Hence the offsets here are -1, meaning one word before the start
// of the object.

#define TYPEINFO_FORWARDING_PTR_OFFSET -1
#define TYPECLASSINFO_FORWARDING_PTR_OFFSET -1

// RETURN_IF_OUT_OF_RANGE(MR_Word tagged_pointer, MR_Word *pointer,
//          int forwarding_pointer_offset, rettype):
//
// Check if `pointer' is either out of range, or has already been processed,
// and if so, return (from the function that called this macro) with the
// appropriate value.
//
// If the pointer is out of range, we return the original tagged pointer
// value unchanged. If the pointer has already been processed, then return
// the forwarding pointer that was saved in the object, which will be stored
// at pointer[forwarding_pointer_offset].

#define RETURN_IF_OUT_OF_RANGE(tagged_pointer, pointer, offset, rettype) \
        do {                                                             \
            if (!in_range(pointer)) {                                    \
                found_out_of_range_pointer(pointer);                     \
                return (rettype) (tagged_pointer);                       \
            }                                                            \
            if_forwarding_pointer((pointer),                             \
                return (rettype) (pointer)[offset]);                     \
        } while (0)

MR_Word
copy(MR_Word data, MR_TypeInfo type_info,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_Word             new_data;
    MR_TypeCtorInfo     type_ctor_info;
    MR_DuTypeLayout     du_type_layout;

try_again:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error(MR_STRINGIFY(copy) ": term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {

    case MR_TYPECTOR_REP_ENUM:
    case MR_TYPECTOR_REP_ENUM_USEREQ:
    case MR_TYPECTOR_REP_FOREIGN_ENUM:
    case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
        return data;    // Just a copy of the actual item.

    case MR_TYPECTOR_REP_DUMMY:
        // The value we are asked to copy is a dummy, and the return value
        // won't be looked at either, so what we return doesn't really matter.
        return data;

    case MR_TYPECTOR_REP_DU:
    case MR_TYPECTOR_REP_DU_USEREQ:
        {
            MR_Word               *data_value;
            const MR_DuPtagLayout *ptag_layout;
            int                   ptag;

            du_type_layout = MR_type_ctor_layout(type_ctor_info).MR_layout_du;
            ptag = MR_tag(data);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
            case MR_SECTAG_LOCAL:
                return data;    // Just a copy of the actual item.

            // case MR_SECTAG_REMOTE:
            // case MR_SECTAG_NONE:
            // case MR_SECTAG_NONE_DIRECT_ARG:
                // The code we want to execute for the MR_SECTAG_REMOTE
                // and MR_SECTAG_NONE cases is very similar. However,
                // speed is important here, and we don't want to check
                // the secondary tag location multiple times at run-time.
                // So we define the code for these two cases as a macro,
                // `MR_handle_sectag_remote_or_none(have_sectag)',
                // and invoke it twice below, with constant values for the
                // `have_sectag' argument. This ensures that the C
                // preprocessor will duplicate the code and the C compiler
                // will then optimize away the tests at compile time.
                //
                // Likewise, we are careful to avoid testing
                // `exist_info != NULL' multiple times at run-time.
                // This requires two copies of the MR_maybe_copy_sectag() code,
                // which is why we define that as a macro too.

#define MR_maybe_copy_sectag(have_sectag)                                      \
        do {                                                                   \
            /* This `if' will get evaluated at compile time. */                \
            if (!have_sectag) {                                                \
                cur_slot = 0;                                                  \
            } else {                                                           \
                MR_field(0, new_data, 0) = sectag;                             \
                cur_slot = 1;                                                  \
            }                                                                  \
        } while (0)

#define MR_handle_sectag_remote_or_none(have_sectag)                           \
        do {                                                                   \
            data_value = (MR_Word *) MR_body(data, ptag);                      \
            RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);              \
            {                                                                  \
                const MR_DuFunctorDesc  *functor_desc;                         \
                const MR_DuArgLocn      *arg_locns;                            \
                const MR_DuExistInfo    *exist_info;                           \
                MR_AllocSiteInfoPtr     attrib;                                \
                int                     sectag;                                \
                int                     cell_size;                             \
                int                     cur_slot;                              \
                int                     arity;                                 \
                int                     i;                                     \
                                                                               \
                /* This `if' will get evaluated at compile time. */            \
                if (!have_sectag) {                                            \
                    sectag = 0;                                                \
                } else {                                                       \
                    sectag = data_value[0];                                    \
                }                                                              \
                                                                               \
                functor_desc = ptag_layout->MR_sectag_alternatives             \
                    [sectag];                                                  \
                arity = functor_desc->MR_du_functor_orig_arity;                \
                arg_locns = functor_desc->MR_du_functor_arg_locns;             \
                exist_info = functor_desc->MR_du_functor_exist_info;           \
                                                                               \
                /* This `if' will get evaluated at compile time. */            \
                if (have_sectag) {                                             \
                    cell_size = 1;                                             \
                } else {                                                       \
                    cell_size = 0;                                             \
                }                                                              \
                cell_size += MR_cell_size_for_args(arity, arg_locns);          \
                cell_size += MR_SIZE_SLOT_SIZE;                                \
                                                                               \
                if (exist_info == NULL) {                                      \
                    attrib = maybe_attrib(data_value);                         \
                    MR_offset_incr_saved_hp(new_data, MR_SIZE_SLOT_SIZE,       \
                        cell_size, attrib, NULL);                              \
                                                                               \
                    MR_copy_size_slot(0, new_data, ptag, data);                \
                    MR_maybe_copy_sectag(have_sectag);                         \
                } else {                                                       \
                    int num_ti_plain;                                          \
                    int num_tci;                                               \
                                                                               \
                    num_ti_plain = exist_info->MR_exist_typeinfos_plain;       \
                    num_tci = exist_info->MR_exist_tcis;                       \
                    cell_size += num_ti_plain + num_tci;                       \
                                                                               \
                    attrib = maybe_attrib(data_value);                         \
                    MR_offset_incr_saved_hp(new_data, MR_SIZE_SLOT_SIZE,       \
                        cell_size, attrib, NULL);                              \
                                                                               \
                    MR_copy_size_slot(0, new_data, ptag, data);                \
                    MR_maybe_copy_sectag(have_sectag);                         \
                                                                               \
                    for (i = 0; i < num_ti_plain; i++) {                       \
                        MR_field(0, new_data, cur_slot) = (MR_Word)            \
                            copy_type_info((MR_TypeInfo)                       \
                                data_value[cur_slot],                          \
                                lower_limit, upper_limit);                     \
                        cur_slot++;                                            \
                    }                                                          \
                                                                               \
                    for (i = 0; i < num_tci; i++) {                            \
                        MR_field(0, new_data, cur_slot) = (MR_Word)            \
                            copy_typeclass_info(data_value[cur_slot],          \
                                lower_limit, upper_limit);                     \
                        cur_slot++;                                            \
                    }                                                          \
                }                                                              \
                                                                               \
                for (i = 0; i < arity; i++) {                                  \
                    if (arg_locns != NULL) {                                   \
                        /*                                                     \
                        ** The meanings of the various special values          \
                        ** of MR_arg_bits are documented next to the           \
                        ** definition of the MR_DuArgLocn type                 \
                        ** in mercury_type_info.h.                             \
                        */                                                     \
                        if (arg_locns[i].MR_arg_bits == 0) {                   \
                            /*                                                 \
                            ** The usual case of a full word argument.         \
                            ** Only full word args may contain pointers,       \
                            ** so only they may need to be copied recursively. \
                            ** This case is handled below, by code that        \
                            ** gets executed even if arg_locns == NULL.        \
                            */                                                 \
                        } else if (arg_locns[i].MR_arg_bits > 0) {             \
                            /*                                                 \
                            ** Copy words holding packed arguments             \
                            ** when we encounter the first one.                \
                            ** (The first packed argument is an enum.)         \
                            */                                                 \
                            if (arg_locns[i].MR_arg_shift == 0) {              \
                                MR_field(0, new_data, cur_slot) =              \
                                    data_value[cur_slot];                      \
                                cur_slot++;                                    \
                            }                                                  \
                            continue;                                          \
                        } else if (arg_locns[i].MR_arg_bits >= -3) {           \
                            /* Double precision float, int64 or uint64. */     \
                            MR_field(0, new_data, cur_slot) =                  \
                                data_value[cur_slot];                          \
                            MR_field(0, new_data, cur_slot + 1) =              \
                                data_value[cur_slot + 1];                      \
                            cur_slot += 2;                                     \
                            continue;                                          \
                        } else if (arg_locns[i].MR_arg_bits >= -9) {           \
                            /*                                                 \
                            ** Copy words holding packed arguments             \
                            ** when we encounter the first one.                \
                            ** (The first packed argument is a small int.)     \
                            */                                                 \
                            if (arg_locns[i].MR_arg_shift == 0) {              \
                                MR_field(0, new_data, cur_slot) =              \
                                    data_value[cur_slot];                      \
                                cur_slot++;                                    \
                            }                                                  \
                            continue;                                          \
                        } else if (arg_locns[i].MR_arg_bits == -10) {          \
                            /* Dummy argument; occupies zero bits. */          \
                            continue;                                          \
                        } else {                                               \
                            MR_fatal_error("MR_arg_bits < -10");               \
                        }                                                      \
                    }                                                          \
                                                                               \
                    if (MR_arg_type_may_contain_var(functor_desc, i)) {        \
                        MR_Word *parent_data = (MR_Word *) new_data;           \
                        if (have_sectag) {                                     \
                            /* Skip past the secondary tag. */                 \
                            parent_data++;                                     \
                        }                                                      \
                        MR_field(0, new_data, cur_slot) =                      \
                            copy_arg(parent_data, data_value[cur_slot],        \
                                functor_desc,                                  \
                                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(        \
                                    type_info),                                \
                                functor_desc->MR_du_functor_arg_types[i],      \
                                lower_limit, upper_limit);                     \
                    } else {                                                   \
                        MR_field(0, new_data, cur_slot) =                      \
                            copy(data_value[cur_slot],                         \
                                MR_pseudo_type_info_is_ground(                 \
                                functor_desc->MR_du_functor_arg_types[i]),     \
                                lower_limit, upper_limit);                     \
                    }                                                          \
                    cur_slot++;                                                \
                }                                                              \
                                                                               \
                new_data = (MR_Word) MR_mkword(ptag, new_data);                \
                leave_forwarding_pointer(data_value, 0, new_data);             \
            }                                                                  \
        } while (0)

            case MR_SECTAG_REMOTE:
                // See comments above.
                MR_handle_sectag_remote_or_none(MR_TRUE);
                return new_data;

            case MR_SECTAG_NONE:
                // See comments above.
                MR_handle_sectag_remote_or_none(MR_FALSE);
                return new_data;

            case MR_SECTAG_NONE_DIRECT_ARG:
                // This code is a cut-down and specialized version
                // of the code for MR_SECTAG_NONE.

                data_value = (MR_Word *) MR_body(data, ptag);
                RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);
                {
                    const MR_DuFunctorDesc  *functor_desc;
                    const MR_DuArgLocn      *arg_locns;
                    const MR_DuExistInfo    *exist_info;
                    int                     arity;

                    functor_desc = ptag_layout->MR_sectag_alternatives[0];
                    arity = functor_desc->MR_du_functor_orig_arity;
                    arg_locns = functor_desc->MR_du_functor_arg_locns;
                    exist_info = functor_desc->MR_du_functor_exist_info;
                    if (arity != 1) {
                        MR_fatal_error("arity != 1 in direct arg tag functor");
                    }
                    if (arg_locns != NULL) {
                        MR_fatal_error("arg_locns in direct arg tag functor");
                    }
                    if (exist_info != NULL) {
                        MR_fatal_error("exist_info in direct arg tag functor");
                    }

                    new_data = copy((MR_Word) data_value,
                        MR_pseudo_type_info_is_ground(
                            functor_desc->MR_du_functor_arg_types[0]),
                        lower_limit, upper_limit);

                    new_data = (MR_Word) MR_mkword(ptag, new_data);
                    // We cannot (and shouldn't need to) leave a forwarding
                    // pointer for the whole term that is separate from the
                    // forwarding pointer for the argument.

                }
                return new_data;

            case MR_SECTAG_VARIABLE:
                MR_fatal_error("copy(): attempt to copy variable");

            default:
                MR_fatal_error("copy(): unknown sectag_locn");

            } // end switch on sectag_locn
        }
        break;

    case MR_TYPECTOR_REP_NOTAG:
    case MR_TYPECTOR_REP_NOTAG_USEREQ:
        return copy_arg(NULL, data, NULL,
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
            MR_notag_functor_arg_type, lower_limit, upper_limit);

    case MR_TYPECTOR_REP_NOTAG_GROUND:
    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        type_info = MR_pseudo_type_info_is_ground(
            MR_type_ctor_layout(type_ctor_info).MR_layout_notag
            ->MR_notag_functor_arg_type);
        goto try_again;

    case MR_TYPECTOR_REP_EQUIV:
        return copy_arg(NULL, data, NULL,
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv,
            lower_limit, upper_limit);

    case MR_TYPECTOR_REP_EQUIV_GROUND:
        type_info = MR_pseudo_type_info_is_ground(
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
        goto try_again;

    case MR_TYPECTOR_REP_INT:  // fallthru
    case MR_TYPECTOR_REP_UINT: // fallthru
    case MR_TYPECTOR_REP_INT8:  // fallthru
    case MR_TYPECTOR_REP_UINT8: // fallthru
    case MR_TYPECTOR_REP_INT16:  // fallthru
    case MR_TYPECTOR_REP_UINT16: // fallthru
    case MR_TYPECTOR_REP_INT32:  // fallthru
    case MR_TYPECTOR_REP_UINT32: // fallthru
    case MR_TYPECTOR_REP_CHAR:
        return data;
    
    case MR_TYPECTOR_REP_INT64:
        #if defined(MR_BOXED_INT64S)
            {
                MR_Word    *data_value;

                assert(MR_tag(data) == 0);
                data_value = (MR_Word *) MR_body(data, MR_mktag(0));

                RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

                {
                    MR_restore_transient_hp();
#ifdef MR_HIGHLEVEL_CODE
                    // We can't use MR_int64_to_word, since it uses MR_hp,
                    // which in grade hlc.par.gc will be a reference to
                    // thread-local storage that we haven't allocated.

                    new_data = (MR_Word) MR_box_int64(MR_unbox_int64(data));
#else
                    new_data = MR_int64_to_word(MR_word_to_int64(data));
#endif
                    MR_save_transient_hp();
                    leave_forwarding_pointer(data_value, 0, new_data);
                }
            }
        #else
            new_data = data;
        #endif
        return new_data;
    
    case MR_TYPECTOR_REP_UINT64:
        #if defined(MR_BOXED_INT64S)
            {
                MR_Word    *data_value;

                assert(MR_tag(data) == 0);
                data_value = (MR_Word *) MR_body(data, MR_mktag(0));

                RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

                {
                    MR_restore_transient_hp();
#ifdef MR_HIGHLEVEL_CODE
                    // We can't use MR_uint64_to_word, since it uses MR_hp,
                    // which in grade hlc.par.gc will be a reference to
                    // thread-local storage that we haven't allocated.

                    new_data = (MR_Word) MR_box_uint64(MR_unbox_uint64(data));
#else
                    new_data = MR_uint64_to_word(MR_word_to_uint64(data));
#endif
                    MR_save_transient_hp();
                    leave_forwarding_pointer(data_value, 0, new_data);
                }
            }
        #else
            new_data = data;
        #endif
        return new_data;

    case MR_TYPECTOR_REP_FLOAT:
        #ifdef MR_BOXED_FLOAT
            {
                MR_Word    *data_value;

                assert(MR_tag(data) == 0);
                data_value = (MR_Word *) MR_body(data, MR_mktag(0));

                RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

                {
                    MR_restore_transient_hp();
#ifdef MR_HIGHLEVEL_CODE
                    // We can't use MR_float_to_word, since it uses MR_hp,
                    // which in grade hlc.par.gc will be a reference to
                    // thread-local storage that we haven't allocated.

                    new_data = (MR_Word) MR_box_float(MR_unbox_float(data));
#else
                    new_data = MR_float_to_word(MR_word_to_float(data));
#endif
                    MR_save_transient_hp();
                    leave_forwarding_pointer(data_value, 0, new_data);
                }
            }
        #else
            new_data = data;
        #endif
        return new_data;

    case MR_TYPECTOR_REP_STRING:
        {
            // Not all Mercury strings are aligned; in particular,
            // string constants containing the empty string may be
            // allocated unaligned storage by the C compiler.
            // So we can't do `assert(MR_tag(data) == 0)' here.

            RETURN_IF_OUT_OF_RANGE(data, (MR_Word *) data, 0, MR_Word);

            {
                MR_String           new_string;
                MR_AllocSiteInfoPtr attrib;
                attrib = maybe_attrib((MR_Word *) data);
                MR_make_aligned_string_copy_saved_hp(new_string,
                        (MR_String) data, attrib);
                new_data = (MR_Word) new_string;
                leave_forwarding_pointer(data, 0, new_data);
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_FUNC:
    case MR_TYPECTOR_REP_PRED:
        {
            MR_Word    *data_value;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            RETURN_IF_OUT_OF_RANGE(data, data_value,
                    CLOSURE_FORWARDING_PTR_OFFSET, MR_Word);

            // Closures have the structure given by the MR_Closure type.
            //
            // Their type_infos have a pointer to type_ctor_info for
            // pred/0 or func/0, the number of argument typeinfos,
            // and then the argument typeinfos themselves.

            {
                MR_Unsigned         num_r_args;
                MR_Unsigned         num_f_args;
                MR_Unsigned         num_args;
                MR_Unsigned         r_offset;
                MR_Unsigned         f_offset;
                MR_Unsigned         i;
                MR_Closure          *old_closure;
                MR_Closure          *new_closure;
                MR_Word             new_closure_word;
                MR_Closure_Layout   *closure_layout;
                MR_TypeInfo         *type_info_arg_vector;
                MR_AllocSiteInfoPtr attrib;

                old_closure = (MR_Closure *) data_value;
                closure_layout = old_closure->MR_closure_layout;
                num_r_args = MR_closure_num_hidden_r_args(old_closure);
                num_f_args = MR_closure_num_hidden_f_args(old_closure);
                num_args = num_r_args + num_f_args;

                // Create new closure.
                attrib = maybe_attrib(data_value);
                MR_offset_incr_saved_hp(new_closure_word, 0, num_args + 3,
                    attrib, NULL);
                new_closure = (MR_Closure *) new_closure_word;

                // Copy the fixed fields.
                new_closure->MR_closure_layout = closure_layout;
                new_closure->MR_closure_code = old_closure->MR_closure_code;
                new_closure->MR_closure_num_hidden_args_rf =
                    old_closure->MR_closure_num_hidden_args_rf;

                // Fill in the pseudo_typeinfos in the closure layout
                // with the values from the closure.

                type_info_arg_vector = MR_materialize_closure_type_params(
                    old_closure);

                // Copy the arguments.
                r_offset = 0;
                f_offset = num_r_args;

                for (i = 0; i < num_args; i++) {
                    MR_PseudoTypeInfo   arg_pti;
                    MR_Unsigned         offset;

                    arg_pti =
                        closure_layout->MR_closure_arg_pseudo_type_info[i];
#ifdef MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
                    if (MR_unify_pseudo_type_info_float(arg_pti)) {
                        offset = f_offset++;
                    } else {
                        offset = r_offset++;
                    }
#else
                    offset = i;
#endif
                    new_closure->MR_closure_hidden_args_0[offset] =
                        copy_arg(NULL,
                            old_closure->MR_closure_hidden_args_0[offset],
                            NULL, type_info_arg_vector, arg_pti,
                            lower_limit, upper_limit);
                }

                if (type_info_arg_vector != NULL) {
                    MR_free(type_info_arg_vector);
                }

                new_data = (MR_Word) new_closure;
                leave_forwarding_pointer(data, CLOSURE_FORWARDING_PTR_OFFSET,
                    new_data);
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_TUPLE:
        {
            MR_Word    *data_value;
            int     arity, i;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

            {
                MR_Word *new_data_ptr;
                MR_TypeInfo *arg_typeinfo_vector;
                MR_AllocSiteInfoPtr attrib;

                arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);

                if (arity == 0) {
                    new_data = (MR_Word) NULL;
                } else {
                    // Allocate space for the new tuple.
                    attrib = maybe_attrib(data_value);
                    MR_offset_incr_saved_hp(new_data, MR_SIZE_SLOT_SIZE,
                        MR_SIZE_SLOT_SIZE + arity, attrib, NULL);
                    MR_copy_size_slot(0, new_data, 0, data);
                    new_data_ptr = (MR_Word *) new_data;

                    arg_typeinfo_vector =
                        MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
                    for (i = 0; i < arity; i++) {
                       // Type_infos are counted from one.
                       new_data_ptr[i] = copy(data_value[i],
                            (const MR_TypeInfo) arg_typeinfo_vector[i + 1],
                            lower_limit, upper_limit);
                    }
                    leave_forwarding_pointer(data, 0, new_data);
                }
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_SUBGOAL:
        MR_fatal_error("Cannot copy a subgoal type");

    case MR_TYPECTOR_REP_VOID:
        MR_fatal_error("Cannot copy a void type");

    case MR_TYPECTOR_REP_ARRAY:
        {
            MR_Word *data_value;
            int     i;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

            {
                MR_ArrayType *new_array;
                MR_ArrayType *old_array;
                MR_Integer array_size;
                MR_AllocSiteInfoPtr attrib;

                old_array = (MR_ArrayType *) data_value;
                array_size = old_array->size;
                attrib = maybe_attrib(data_value);
                MR_offset_incr_saved_hp(new_data, 0, array_size + 1,
                    attrib, NULL);
                new_array = (MR_ArrayType *) new_data;
                new_array->size = array_size;
                for (i = 0; i < array_size; i++) {
                    new_array->elements[i] = copy_arg(NULL,
                        old_array->elements[i], NULL,
                        MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                        (const MR_PseudoTypeInfo) 1, lower_limit, upper_limit);
                }
                leave_forwarding_pointer(data, 0, new_data);
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_BITMAP:
        {
            MR_Word *data_value;

            assert(MR_tag(data) == 0);
            data_value = (MR_Word *) MR_body(data, MR_mktag(0));

            RETURN_IF_OUT_OF_RANGE(data, data_value, 0, MR_Word);

            {
                MR_BitmapPtr new_array;
                MR_BitmapPtr old_array;

                old_array = (MR_BitmapPtr) data_value;
                MR_allocate_bitmap_saved_hp(new_array, old_array->num_bits,
                    NULL);
                MR_copy_bitmap(new_array, old_array);
                new_data = (MR_Word) new_array;
                leave_forwarding_pointer(data, 0, new_data);
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_TYPEINFO:
    case MR_TYPECTOR_REP_TYPEDESC:
        return (MR_Word) copy_type_info((MR_TypeInfo) data,
            lower_limit, upper_limit);

    case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
        return (MR_Word) copy_pseudo_type_info((MR_PseudoTypeInfo) data,
            lower_limit, upper_limit);

    case MR_TYPECTOR_REP_TYPECTORINFO:
        // Type_ctor_infos are always pointers to static data.
        return data;

    case MR_TYPECTOR_REP_TYPECTORDESC:
        // Type_ctor_descs are always either encoded integers,
        // or pointers to static data.

        return data;

    case MR_TYPECTOR_REP_TYPECLASSINFO:
        return (MR_Word) copy_typeclass_info(data,
            lower_limit, upper_limit);

    case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        // Base_typeclass_infos are always pointers to static data.
        return data;

    case MR_TYPECTOR_REP_STABLE_C_POINTER: // fallthru
    case MR_TYPECTOR_REP_C_POINTER:
        {
            MR_Word *data_value;
            int     data_tag;

            // XXX simplify: tag should be zero.
            data_tag = MR_tag(data);
            data_value = (MR_Word *) MR_body(data, data_tag);

            if (in_range(data_value)) {
                // This error occurs if we try to copy() a
                // `c_pointer' type that points to memory allocated
                // on the Mercury heap.

                MR_fatal_error("Cannot copy a c_pointer type");
            } else {
                new_data = data;
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_SUCCIP: // fallthru
    case MR_TYPECTOR_REP_REDOIP:
        // Code addresses are never relocated.
        return data;

    case MR_TYPECTOR_REP_HP:
        assert(MR_tag(data) == 0);
        if (in_range((MR_Word *) data)) {
            MR_fatal_error("Sorry, not implemented: "
                "copying saved heap pointer");
        } else {
            new_data = data;
        }
        return new_data;

    case MR_TYPECTOR_REP_CURFR: // fallthru
    case MR_TYPECTOR_REP_MAXFR: // fallthru
    case MR_TYPECTOR_REP_REDOFR:
        // We do not modify the layout of the nondet stack.
        return data;

    case MR_TYPECTOR_REP_TRAIL_PTR:
    case MR_TYPECTOR_REP_TICKET:
        // XXX We do not yet compress the trail when doing gc.
        return data;

    case MR_TYPECTOR_REP_REFERENCE:
        {
            MR_Word *ref;
            MR_Word *new_ref;
            MR_AllocSiteInfoPtr attrib;

            assert(MR_tag(data) == 0);
            ref = (MR_Word *) MR_body(data, MR_mktag(0));

            RETURN_IF_OUT_OF_RANGE(data, ref, 0, MR_Word);

            attrib = maybe_attrib(ref);
            MR_offset_incr_saved_hp(new_data, 0, 1, attrib, NULL);
            new_ref = (MR_Word *) new_data;
            *new_ref = copy_arg(NULL, *ref, NULL,
                        MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                        (const MR_PseudoTypeInfo) 1, lower_limit, upper_limit);
            leave_forwarding_pointer(data, 0, new_data);
        }
        return new_data;

    case MR_TYPECTOR_REP_STABLE_FOREIGN:
        // By definition, stable foreign values are never relocated.
        return data;

    case MR_TYPECTOR_REP_FOREIGN:
        {
            MR_Word *data_value;

            data_value = (MR_Word *) MR_strip_tag(data);

            // Foreign types that are not pointers should not have
            // MR_TYPECTOR_REP_FOREIGN; instead, they should have
            // MR_TYPECTOR_REP_STABLE_FOREIGN.

            if (lower_limit != NULL && !in_range(data_value)) {
                // If the foreign value does not point into the area of
                // the heap that we are copying, then it is safe to
                // leave it unchanged.
                //
                // It is important to allow these cases, when doing partial
                // copies (as occurs with accurate GC or solutions),
                // since they include the common cases of pointer types
                // that point to the C heap, global data, or stack data.
                // io__stream is a particularly important example.
                //
                // However, when doing complete copies (lower_limit == NULL),
                // we should not allow shallow copying of foreign types,
                // because in cases where the foreign type is (or represents)
                // a pointer of some kind, that might violate unique mode
                // correctness. That's why we check lower_limit != NULL above.

                new_data = data;
            } else {
                // The foreign value points into the Mercury heap.
                // It might be a foreign pointer to a Mercury heap
                // value; or it might be a pointer to a foreign struct
                // which MR_MAYBE_BOX_FOREIGN_TYPE() has copied to the
                // Mercury heap; or it might be a non-pointer type
                // whose bit pattern happens to point to the heap.
                //
                // We don't know how to copy it, so we have to abort.

                char    *buf;
                int     len;

                len = strlen(type_ctor_info->MR_type_ctor_module_name) +
                        strlen(type_ctor_info->MR_type_ctor_name) + 100;
                buf = (char *) MR_malloc(len);
                sprintf(buf, "Cannot copy foreign type %s.%s",
                        type_ctor_info->MR_type_ctor_module_name,
                        type_ctor_info->MR_type_ctor_name);
                MR_fatal_error(buf);
            }
        }
        return new_data;

    case MR_TYPECTOR_REP_UNUSED1:
    case MR_TYPECTOR_REP_UNUSED2:
    case MR_TYPECTOR_REP_UNKNOWN:
        MR_fatal_error("Unknown layout type in deep copy");
    }

    MR_fatal_error(MR_STRINGIFY(copy) ": unexpected fallthrough");
}

// copy_arg is like copy() except that it takes a pseudo_type_info
// (namely arg_pseudo_type_info) rather than a type_info.
// The pseudo_type_info may contain type variables,
// which refer to arguments of the term_type_info.
//
// It also takes a pointer to the data of the parent of this piece of data
// and a functor descriptor for the parent in case the data being copied is
// existentially quantified.

static MR_Word
copy_arg(const MR_Word *parent_data_ptr, MR_Word data,
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

    new_data = copy(data, new_type_info, lower_limit, upper_limit);
    MR_deallocate(allocated_memory_cells);

    return new_data;
}

static MR_TypeInfo
copy_type_info(MR_TypeInfo type_info,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    // Most changes here should also be done in copy_pseudo_type_info below.

    RETURN_IF_OUT_OF_RANGE((MR_Word) type_info, (MR_Word *) type_info,
        TYPEINFO_FORWARDING_PTR_OFFSET, MR_TypeInfo);

    {
        MR_TypeCtorInfo type_ctor_info;
        MR_Word         *new_type_info_arena;
        MR_Word         new_type_info_arena_word;
        MR_TypeInfo     *type_info_args;
        MR_TypeInfo     *new_type_info_args;
        int             arity;
        int             i;
        int             forwarding_pointer_size;
        MR_AllocSiteInfoPtr attrib;

        // Note that we assume type_ctor_infos will always be
        // allocated statically, so we never copy them.

        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        // Optimize a special case: if there are no arguments,
        // we don't need to construct a type_info; instead,
        // we can just return the type_ctor_info.

        if ((MR_Word) type_info == (MR_Word) type_ctor_info) {
            return (MR_TypeInfo) type_ctor_info;
        }

        // Compute how many words to reserve for the forwarding pointer.
#ifdef MR_NATIVE_GC
        forwarding_pointer_size = 1;
#else
        forwarding_pointer_size = 0;
#endif

        if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
            arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
            type_info_args =
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
            attrib = maybe_attrib((MR_Word *) type_info);
            MR_offset_incr_saved_hp(new_type_info_arena_word,
                forwarding_pointer_size,
                MR_var_arity_type_info_size(arity) + forwarding_pointer_size,
                attrib, NULL
            );
            new_type_info_arena = (MR_Word *) new_type_info_arena_word;
            MR_fill_in_var_arity_type_info(new_type_info_arena,
                type_ctor_info, arity, new_type_info_args);
        } else {
            arity = type_ctor_info->MR_type_ctor_arity;
            type_info_args = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
            attrib = maybe_attrib((MR_Word *) type_info);
            MR_offset_incr_saved_hp(new_type_info_arena_word,
                forwarding_pointer_size,
                MR_fixed_arity_type_info_size(arity) + forwarding_pointer_size,
                attrib, NULL
            );
            new_type_info_arena = (MR_Word *) new_type_info_arena_word;
            MR_fill_in_fixed_arity_type_info(new_type_info_arena,
                type_ctor_info, new_type_info_args);
        }

        for (i = 1; i <= arity; i++) {
            new_type_info_args[i] = copy_type_info(type_info_args[i],
                lower_limit, upper_limit);
        }

        leave_forwarding_pointer((MR_Word) type_info,
            TYPEINFO_FORWARDING_PTR_OFFSET, (MR_Word) new_type_info_arena);
        return (MR_TypeInfo) new_type_info_arena;
    }
}

static MR_PseudoTypeInfo
copy_pseudo_type_info(MR_PseudoTypeInfo pseudo_type_info,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    // Most changes here should also be done in copy_type_info above.

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
        return pseudo_type_info;
    }

    RETURN_IF_OUT_OF_RANGE((MR_Word) pseudo_type_info,
        (MR_Word *) pseudo_type_info, TYPEINFO_FORWARDING_PTR_OFFSET,
        MR_PseudoTypeInfo);

    {
        MR_TypeCtorInfo     type_ctor_info;
        MR_Word             *new_pseudo_type_info_arena;
        MR_Word             new_pseudo_type_info_arena_word;
        MR_PseudoTypeInfo   *pseudo_type_info_args;
        MR_PseudoTypeInfo   *new_pseudo_type_info_args;
        int                 arity;
        int                 i;
        int                 forwarding_pointer_size;
        MR_AllocSiteInfoPtr attrib;

        // Note that we assume type_ctor_infos will always be
        // allocated statically, so we never copy them.

        type_ctor_info =
            MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo_type_info);

        // Optimize a special case: if there are no arguments,
        // we don't need to construct a pseudo_type_info; instead,
        // we can just return the type_ctor_info.

        if ((MR_Word) pseudo_type_info == (MR_Word) type_ctor_info) {
            return (MR_PseudoTypeInfo) type_ctor_info;
        }

        // Compute how many words to reserve for the forwarding pointer.
#ifdef MR_NATIVE_GC
        forwarding_pointer_size = 1;
#else
        forwarding_pointer_size = 0;
#endif

        if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
            arity = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo_type_info);
            pseudo_type_info_args =
                MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pseudo_type_info);
            attrib = maybe_attrib((MR_Word *) pseudo_type_info);
            MR_offset_incr_saved_hp(new_pseudo_type_info_arena_word,
                forwarding_pointer_size,
                MR_var_arity_pseudo_type_info_size(arity)
                    + forwarding_pointer_size,
                attrib, NULL
            );
            new_pseudo_type_info_arena = (MR_Word *)
                new_pseudo_type_info_arena_word;
            MR_fill_in_var_arity_pseudo_type_info(new_pseudo_type_info_arena,
                type_ctor_info, arity, new_pseudo_type_info_args);
        } else {
            arity = type_ctor_info->MR_type_ctor_arity;
            pseudo_type_info_args =
                MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pseudo_type_info);
            attrib = maybe_attrib((MR_Word *) pseudo_type_info);
            MR_offset_incr_saved_hp(new_pseudo_type_info_arena_word,
                forwarding_pointer_size,
                MR_fixed_arity_pseudo_type_info_size(arity)
                    + forwarding_pointer_size,
                attrib, NULL
            );
            new_pseudo_type_info_arena = (MR_Word *)
                new_pseudo_type_info_arena_word;
            MR_fill_in_fixed_arity_pseudo_type_info(new_pseudo_type_info_arena,
                type_ctor_info, new_pseudo_type_info_args);
        }

        for (i = 1; i <= arity; i++) {
            new_pseudo_type_info_args[i] =
                copy_pseudo_type_info(pseudo_type_info_args[i],
                    lower_limit, upper_limit);
        }

        leave_forwarding_pointer((MR_Word) pseudo_type_info,
            TYPEINFO_FORWARDING_PTR_OFFSET,
            (MR_Word) new_pseudo_type_info_arena);
        return (MR_PseudoTypeInfo) new_pseudo_type_info_arena;
    }
}

static MR_Word
copy_typeclass_info(MR_Word typeclass_info_param,
    const MR_Word *lower_limit, const MR_Word *upper_limit)
{
    MR_Word *typeclass_info = (MR_Word *) typeclass_info_param;

    RETURN_IF_OUT_OF_RANGE(typeclass_info_param, typeclass_info,
        TYPECLASSINFO_FORWARDING_PTR_OFFSET, MR_Word);

    {
        MR_Word *base_typeclass_info;
        MR_Word *new_typeclass_info;
        MR_Word new_typeclass_info_word;
        int     num_arg_typeinfos;
        int     num_super;
        int     num_instance_constraints;
        int     num_unconstrained;
        int     i;
        int     forwarding_pointer_size;

        // Note that we assume base_typeclass_infos will always be
        // allocated statically, so we never copy them.

        base_typeclass_info = (MR_Word *) *typeclass_info;

        // Compute how many words to reserve for the forwarding pointer.
#ifdef MR_NATIVE_GC
        forwarding_pointer_size = 1;
#else
        forwarding_pointer_size = 0;
#endif

        num_instance_constraints =
            MR_typeclass_info_num_instance_constraints(typeclass_info);
        num_unconstrained =
            MR_typeclass_info_num_extra_instance_args(typeclass_info)
                - num_instance_constraints;
        num_super = MR_typeclass_info_num_superclasses(typeclass_info);
        num_arg_typeinfos = MR_typeclass_info_num_params(typeclass_info);
        MR_offset_incr_saved_hp(new_typeclass_info_word,
            forwarding_pointer_size,
            forwarding_pointer_size + 1 // for basetypeclass_info
            + num_instance_constraints + num_super + num_arg_typeinfos,
            NULL, NULL);
        new_typeclass_info = (MR_Word *) new_typeclass_info_word;

        new_typeclass_info[0] = (MR_Word) base_typeclass_info;

        // First, copy typeinfos for unconstrained tvars from
        // the instance declaration.

        for (i = 1; i < num_unconstrained + 1; i++) {
            new_typeclass_info[i] = (MR_Word) copy_type_info(
                (MR_TypeInfo) typeclass_info[i], lower_limit, upper_limit);
        }
        // Next, copy all the typeclass infos: both the ones for constraints
        // on the instance declaration (instance constraints), and the ones
        // for constraints on the typeclass declaration
        // (superclass constraints).

        for (i = num_unconstrained + 1;
            i <= num_unconstrained + num_instance_constraints + num_super;
            i++)
        {
            new_typeclass_info[i] = (MR_Word) copy_typeclass_info(
                typeclass_info[i], lower_limit, upper_limit);
        }

        // Then, copy all the type infos for types in the
        // head of the type class declaration.

        for (i = num_unconstrained + num_instance_constraints + num_super + 1;
            i <= num_unconstrained + num_instance_constraints + num_super
                + num_arg_typeinfos;
            i++)
        {
            new_typeclass_info[i] = (MR_Word) copy_type_info(
                (MR_TypeInfo) typeclass_info[i], lower_limit, upper_limit);
        }
        leave_forwarding_pointer(typeclass_info,
                TYPECLASSINFO_FORWARDING_PTR_OFFSET,
                (MR_Word) new_typeclass_info);
        return (MR_Word) new_typeclass_info;
    }
}

// Try to return the allocation identifier for the given object, or NULL.
// If present, allocation identifiers always occupy the first word of an
// allocated object, with the Mercury cell starting at the second word.

#ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
static MR_AllocSiteInfoPtr
maybe_attrib(MR_Word *data_value)
{
    MR_Word *base;

    // Strings are not always aligned, for example.
    if (MR_tag(data_value) == 0) {
        base = GC_base(data_value);
        if (&base[1] == data_value) {
            return (MR_AllocSiteInfoPtr) *base;
        }
    }

    return NULL;
}
#endif
