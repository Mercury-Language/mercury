// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007, 2011 The University of Melbourne.
// Copyright (C) 2016-2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This files defines the bodies of the various variants of the
// MR_table_type() function.
//
// NOTE: Any changes to this function will probably also have to be reflected
// in the places listed in mercury_type_info.h.

    MR_TypeCtorInfo type_ctor_info;
    MR_TrieNode     table_next;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    if (DEBUG && MR_tabledebug) {
        printf("ENTRY %p %lx, data rep: %d\n",
            table, (long) data, MR_type_ctor_rep(type_ctor_info));
    }

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_table_type: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {

    case MR_TYPECTOR_REP_ENUM:
    case MR_TYPECTOR_REP_ENUM_USEREQ:
        MR_TABLE_ENUM(STATS, DEBUG, BACK, table_next, table,
            MR_type_ctor_num_functors(type_ctor_info), data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_FOREIGN_ENUM:
    case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
        MR_TABLE_FOREIGN_ENUM(STATS, DEBUG, BACK, table_next, table,
            data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_DUMMY:
        // If we are ever asked to table a value of a dummy type, we treat
        // it mostly as an enum, with the exception being that we ignore
        // the actual value to be tabled (since it contains garbage) and
        // substitute the constant zero, which ought to be the enum value
        // assigned to the type's only function symbol.
        //
        // We would like the compiler to simply not insert any arguments
        // of dummy types into tables. Unfortunately, while the compiler
        // can filter out any dummy arguments whose type is statically
        // known, it cannot do so for arguments whose type becomes known
        // only at runtime.

        MR_TABLE_ENUM(STATS, DEBUG, BACK, table_next, table, 1, 0);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_DU:
    case MR_TYPECTOR_REP_DU_USEREQ:
    {
        MR_MemoryList           allocated_memory_cells = NULL;
        const MR_DuPtagLayout   *ptag_layout;
        const MR_DuFunctorDesc  *functor_desc;
        const MR_DuArgLocn      *arg_locns;
        const MR_DuExistInfo    *exist_info;
        int                     ptag;
        MR_Word                 sectag;
        int                     tagword_arity;
        int                     full_arity;
        // arg_vector is meaningful iff full_arity > tagword_arity.
        MR_Word                 *arg_vector;
        // tagword_arity is meaningful iff tagword_arity > 0.
        MR_Word                 tagword;
        MR_Word                 direct_arg;
        int                     meta_args;
        int                     i;

        ptag = MR_tag(data);
        MR_index_or_search_ptag_layout(ptag, ptag_layout);

        switch (ptag_layout->MR_sectag_locn) {

        case MR_SECTAG_NONE:
            // We can index MR_sectag_alternatives for MR_SECTAG_NONE.
            functor_desc = ptag_layout->MR_sectag_alternatives[0];
            full_arity = functor_desc->MR_du_functor_orig_arity;
            tagword_arity = 0;
            arg_vector = (MR_Word *) MR_body(data, ptag);
            break;

        case MR_SECTAG_NONE_DIRECT_ARG:
            // We can index MR_sectag_alternatives for
            // MR_SECTAG_NONE_DIRECT_ARG.
            functor_desc = ptag_layout->MR_sectag_alternatives[0];
            full_arity = functor_desc->MR_du_functor_orig_arity;
            tagword_arity = 0;
            direct_arg = MR_body(data, ptag);
            arg_vector = &direct_arg;
            break;

        case MR_SECTAG_LOCAL_REST_OF_WORD:
            sectag = MR_unmkbody(data);
            MR_index_or_search_sectag_functor(ptag_layout, sectag,
                functor_desc);
            full_arity = functor_desc->MR_du_functor_orig_arity;
            tagword_arity = 0;
            tagword = data;
            MR_table_assert(full_arity == 0);
            MR_table_assert(functor_desc->MR_du_functor_exist_info == NULL);
            break;

        case MR_SECTAG_LOCAL_BITS:
            sectag = MR_unmkbody(data) &
                ((1 << ptag_layout->MR_sectag_numbits) - 1);
            MR_index_or_search_sectag_functor(ptag_layout, sectag,
                functor_desc);
            full_arity = functor_desc->MR_du_functor_orig_arity;
            tagword_arity = full_arity;
            tagword = data;
            MR_table_assert(functor_desc->MR_du_functor_exist_info == NULL);
            break;

        case MR_SECTAG_REMOTE_FULL_WORD:
            sectag = MR_field(ptag, data, 0);
            MR_index_or_search_sectag_functor(ptag_layout, sectag,
                functor_desc);
            full_arity = functor_desc->MR_du_functor_orig_arity;
            tagword_arity = 0;
            arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
            break;

        case MR_SECTAG_REMOTE_BITS:
            tagword = MR_field(ptag, data, 0);
            sectag = tagword &
                ((1 << ptag_layout->MR_sectag_numbits) - 1);
            MR_index_or_search_sectag_functor(ptag_layout, sectag,
                functor_desc);
            full_arity = functor_desc->MR_du_functor_orig_arity;
            if (functor_desc->MR_du_functor_arg_locns == NULL) {
                tagword_arity = 0;
            } else {
                // Find the index of the first argument (if any) that is
                // not stored next to the sectag (i.e. whose offset is not -1).
                tagword_arity = full_arity;
                for (i = 0; i < full_arity; i++) {
                    if (functor_desc->MR_du_functor_arg_locns[i].
                        MR_arg_offset != -1)
                    {
                        tagword_arity = i;
                        break;
                    }
                }
            }
            arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
            tagword = arg_vector[-1];
            MR_table_assert(functor_desc->MR_du_functor_exist_info == NULL);
            break;

        case MR_SECTAG_VARIABLE:
            MR_fatal_error("MR_table_type(): unexpected variable");

        default:
            MR_fatal_error("MR_table_type(): unknown sectag_locn");

        }

        MR_TABLE_DU(STATS, DEBUG, BACK, table_next, table,
            MR_type_ctor_num_functors(type_ctor_info),
            functor_desc->MR_du_functor_ordinal);
        table = table_next;

        exist_info = functor_desc->MR_du_functor_exist_info;
        if (exist_info != NULL) {
            int                     num_ti_plain;
            int                     num_ti_in_tci;
            int                     num_tci;
            const MR_DuExistLocn    *locns;

            num_ti_plain = exist_info->MR_exist_typeinfos_plain;
            num_ti_in_tci = exist_info->MR_exist_typeinfos_in_tci;
            num_tci = exist_info->MR_exist_tcis;
            locns = exist_info->MR_exist_typeinfo_locns;

            for (i = 0; i < num_ti_plain + num_ti_in_tci; i++) {
                MR_table_record_exist_lookup();
                if (locns[i].MR_exist_offset_in_tci < 0) {
                    MR_TABLE_TYPEINFO(STATS, DEBUG, BACK,
                        table_next, table, (MR_TypeInfo)
                        arg_vector[locns[i].MR_exist_arg_num]);
                    table = table_next;
                } else {
                    MR_TABLE_TYPEINFO(STATS, DEBUG, BACK,
                        table_next, table, (MR_TypeInfo)
                        MR_typeclass_info_param_type_info(
                            arg_vector[locns[i].MR_exist_arg_num],
                            locns[i].MR_exist_offset_in_tci));
                    table = table_next;
                }
            }
            meta_args = num_ti_plain + num_tci;
        } else {
            meta_args = 0;
        }

        arg_locns = functor_desc->MR_du_functor_arg_locns;
        for (i = 0; i < tagword_arity; i++) {
            MR_TypeInfo arg_type_info;
            MR_Word     arg_value;

            arg_type_info = MR_get_arg_type_info_alloc(type_info, functor_desc,
                arg_vector, i, allocated_memory_cells);

            MR_get_tagword_arg_value(arg_locns[i], tagword, arg_value);

            MR_table_record_arg_lookup();
            MR_TABLE_ANY(STATS, DEBUG, BACK, "du arg tw", table_next, table,
                arg_type_info, arg_value);
            table = table_next;
        }

        for (i = tagword_arity; i < full_arity; i++) {
            MR_TypeInfo         arg_type_info;
            MR_Word             arg_value;
            // We ignore word_size_arg_ptr, but its presence is required
            // by a macro.
            MR_Word             *word_size_arg_ptr;

            arg_type_info = MR_get_arg_type_info_alloc(type_info, functor_desc,
                arg_vector, i, allocated_memory_cells);

            if (functor_desc->MR_du_functor_arg_locns != NULL) {
                MR_get_non_tagword_arg_value(arg_locns[i],
                    (arg_vector + meta_args), arg_value, word_size_arg_ptr);
            } else {
                arg_value = arg_vector[meta_args + i];
            }

            MR_table_record_arg_lookup();
            MR_TABLE_ANY(STATS, DEBUG, BACK, "du arg", table_next, table,
                arg_type_info, arg_value);
            table = table_next;
        }

        MR_deallocate(allocated_memory_cells);
        return table;
    }

    case MR_TYPECTOR_REP_NOTAG:
    case MR_TYPECTOR_REP_NOTAG_USEREQ:
    {
        MR_MemoryList       allocated_memory_cells = NULL;
        MR_TypeInfo         eqv_type_info;

        eqv_type_info = MR_make_type_info(
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                MR_notag_functor_arg_type, &allocated_memory_cells);
        MR_TABLE_ANY(STATS, DEBUG, BACK, "notag arg",
            table_next, table, eqv_type_info, data);
        table = table_next;
        MR_deallocate(allocated_memory_cells);
        return table;
    }

    case MR_TYPECTOR_REP_NOTAG_GROUND:
    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        MR_TABLE_ANY(STATS, DEBUG, BACK, "notag ground arg", table_next, table,
            MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                MR_notag_functor_arg_type), data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_EQUIV:
    {
        MR_MemoryList       allocated_memory_cells = NULL;
        MR_TypeInfo         eqv_type_info;

        eqv_type_info = MR_make_type_info(
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv,
            &allocated_memory_cells);
        MR_TABLE_ANY(STATS, DEBUG, BACK, "equiv", table_next, table,
            eqv_type_info, data);
        table = table_next;
        MR_deallocate(allocated_memory_cells);
        return table;
    }

    case MR_TYPECTOR_REP_EQUIV_GROUND:
        MR_TABLE_ANY(STATS, DEBUG, BACK, "equiv ground", table_next, table,
            MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv),
            data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_INT:
        MR_TABLE_INT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_UINT:
        MR_TABLE_UINT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_INT8:
        MR_TABLE_INT8(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_UINT8:
        MR_TABLE_UINT8(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_INT16:
        MR_TABLE_INT16(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_UINT16:
        MR_TABLE_UINT16(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_INT32:
        MR_TABLE_INT32(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_UINT32:
        MR_TABLE_UINT32(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_INT64:
        MR_TABLE_INT64(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_UINT64:
        MR_TABLE_UINT64(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_CHAR:
        MR_TABLE_CHAR(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_FLOAT:
        MR_TABLE_FLOAT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_STRING:
        MR_TABLE_STRING(STATS, DEBUG, BACK, table_next, table,
            (MR_String) data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_BITMAP:
         MR_TABLE_BITMAP(STATS, DEBUG, BACK, table_next, table,
            (MR_ConstBitmapPtr) data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_FUNC:
    case MR_TYPECTOR_REP_PRED:
    {
        // XXX tabling of the closures by tabling their code address
        // and arguments is not yet implemented, due to the overhead
        // of figuring out the closure argument types.

#if 0
        MR_closure  closure;
        MR_Word     num_hidden_args;
        int         i;

        closure = (MR_Closure *) data;
        num_hidden_args = closure->MR_closure_num_hidden_args;
        MR_TABLE_INT(STATS, DEBUG, BACK, table_next, table,
            closure->MR_closure_code);
        table = table_next;
        for (i = 1; i <= num_hidden_args; i++) {
            MR_TABLE_ANY(STATS, DEBUG, BACK, "closure arg",
                table_next, table,
                <type_info for hidden closure argument number i>,
                closure->MR_closure_hidden_args(i));
            table = table_next;
        }
#else
        // Instead, we use the following rather simplistic means of
        // tabling closures: we just table based on the closure
        // address.

        MR_TABLE_INT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
#endif

        return table;
    }

    case MR_TYPECTOR_REP_TUPLE:
    {
        MR_Word     *data_value;
        MR_TypeInfo *arg_type_info_vector;
        int         arity;
        int         i;

        data_value = (MR_Word *) data;
        arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
        arg_type_info_vector = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
        for (i = 0; i < arity; i++) {
            // Type_infos are counted starting at one.
            MR_TABLE_ANY(STATS, DEBUG, BACK, "tuple arg", table_next, table,
                arg_type_info_vector[i + 1], data_value[i]);
            table = table_next;
        }

        return table;
    }

    case MR_TYPECTOR_REP_SUBGOAL:
        MR_fatal_error("Cannot table a subgoal");

    case MR_TYPECTOR_REP_VOID:
        MR_fatal_error("Cannot table a void type");

    case MR_TYPECTOR_REP_C_POINTER:
        MR_fatal_error("Attempt to table a C_POINTER");

    case MR_TYPECTOR_REP_STABLE_C_POINTER:
        // This works because a stable C pointer guarantees that the
        // data structures pointed to, indirectly as well as directly,
        // will remain stable until the program exits.

        MR_TABLE_INT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_STABLE_FOREIGN:
        // This works because a stable foreign type guarantees that the
        // data structures pointed to, indirectly as well as directly,
        // will remain stable until the program exits.

        MR_TABLE_INT(STATS, DEBUG, BACK, table_next, table, data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_TYPEINFO:
    case MR_TYPECTOR_REP_TYPEDESC:
        MR_TABLE_TYPEINFO(STATS, DEBUG, BACK, table_next, table,
            (MR_TypeInfo) data);
        table = table_next;
        return table;

    case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
        MR_fatal_error("Attempt to table a pseudo_type_desc");

    case MR_TYPECTOR_REP_TYPECTORINFO:
        MR_fatal_error("Attempt to table a type_ctor_info");

    case MR_TYPECTOR_REP_TYPECTORDESC:
        MR_fatal_error("Attempt to table a type_ctor_desc");

    case MR_TYPECTOR_REP_TYPECLASSINFO:
        MR_fatal_error("Attempt to table a type_class_info");

    case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        MR_fatal_error("Attempt to table a base_type_class_info");

    case MR_TYPECTOR_REP_ARRAY:
    {
        MR_TypeInfo     new_type_info;
        MR_MemoryList   allocated_memory_cells = NULL;
        MR_ArrayType    *array;
        MR_Integer      array_size;
        int             i;

        array = (MR_ArrayType *) data;
        array_size = array->size;

        new_type_info = MR_make_type_info(
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            (MR_PseudoTypeInfo) 1, &allocated_memory_cells);

        for (i = 0; i < array_size; i++) {
            MR_TABLE_ANY(STATS, DEBUG, BACK, "array element",
                table_next, table, new_type_info, array->elements[i]);
            table = table_next;
        }

        MR_deallocate(allocated_memory_cells);
        return table;
    }

    case MR_TYPECTOR_REP_SUCCIP:
        MR_fatal_error("Attempt to table a saved succip");

    case MR_TYPECTOR_REP_HP:
        MR_fatal_error("Attempt to table a saved hp");

    case MR_TYPECTOR_REP_CURFR:
        MR_fatal_error("Attempt to table a saved curfr");

    case MR_TYPECTOR_REP_MAXFR:
        MR_fatal_error("Attempt to table a saved maxfr");

    case MR_TYPECTOR_REP_REDOFR:
        MR_fatal_error("Attempt to table a saved redofr");

    case MR_TYPECTOR_REP_REDOIP:
        MR_fatal_error("Attempt to table a saved redoip");

    case MR_TYPECTOR_REP_TRAIL_PTR:
        MR_fatal_error("Attempt to table a saved trail pointer");

    case MR_TYPECTOR_REP_TICKET:
        MR_fatal_error("Attempt to table a saved ticket");

    case MR_TYPECTOR_REP_FOREIGN:
        MR_fatal_error("Attempt to table a value of a foreign type");

    case MR_TYPECTOR_REP_REFERENCE:
        MR_fatal_error("Attempt to table a value of a reference type");

    case MR_TYPECTOR_REP_UNUSED1:
    case MR_TYPECTOR_REP_UNUSED2:
    case MR_TYPECTOR_REP_UNKNOWN:
        MR_fatal_error("Unknown layout tag in table_any");

    }

    MR_fatal_error(func ": unexpected fallthrough");
