/*
** Copyright (C) 1997-1999 The University of Melbourne.
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
static	Word	copy_arg(maybeconst Word *data_ptr, const Word *type_info,
			const Word *arg_type_info, const Word *lower_limit,
			const Word *upper_limit);
static	Word	*copy_type_info(maybeconst Word *type_info,
			const Word *lower_limit, const Word *upper_limit);

Word 
copy(maybeconst Word *data_ptr, const Word *type_info, 
         const Word *lower_limit, const Word *upper_limit)
{
    Word *type_ctor_info, *type_ctor_layout, *type_ctor_functors;
    Word functors_indicator;
    Word layout_entry, *entry_value, *data_value;
    enum MR_DataRepresentation data_rep;
    int data_tag; 
    Word new_data, data;

    data = *data_ptr;

    data_tag = tag(data);
    data_value = (Word *) body(data, data_tag);

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    type_ctor_layout = MR_TYPE_CTOR_INFO_GET_TYPE_CTOR_LAYOUT(type_ctor_info);
    layout_entry = type_ctor_layout[data_tag];

    type_ctor_functors = MR_TYPE_CTOR_INFO_GET_TYPE_CTOR_FUNCTORS(type_ctor_info);
    functors_indicator = MR_TYPE_CTOR_FUNCTORS_INDICATOR(type_ctor_functors);

    entry_value = (Word *) strip_tag(layout_entry);

    data_rep = MR_categorize_data(functors_indicator, layout_entry);

    switch (data_rep) {
        case MR_DATAREP_ENUM:                   /* fallthru */
        case MR_DATAREP_COMPLICATED_CONST:
            new_data = data;	/* just a copy of the actual item */
        break;

        case MR_DATAREP_COMPLICATED: {
            Word secondary_tag;
            Word *new_entry;
            Word *argument_vector, *type_info_vector;
            int arity, i;

            /*
            ** if the vector containing the secondary tags and the
            ** arguments is in range, copy it.
            */
            if (in_range(data_value)) {
                secondary_tag = *data_value;
                argument_vector = data_value + 1;

                new_entry = MR_TYPE_CTOR_LAYOUT_COMPLICATED_VECTOR_GET_SIMPLE_VECTOR(
			entry_value, secondary_tag);
                arity = new_entry[TYPE_CTOR_LAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = new_entry + TYPE_CTOR_LAYOUT_SIMPLE_ARGS_OFFSET;

                /* allocate space for new args, and secondary tag */
                incr_saved_hp(new_data, arity + 1);

                /* copy secondary tag */
                field(0, new_data, 0) = secondary_tag;

                /* copy arguments */
                for (i = 0; i < arity; i++) {
                    field(0, new_data, i + 1) = copy_arg(
                        &argument_vector[i], type_info,
                        (Word *) type_info_vector[i], lower_limit,
                        upper_limit);
                }

                /* tag this pointer */
                new_data = (Word) mkword(data_tag, new_data);
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
            break;
        }

        case MR_DATAREP_SIMPLE: {
            int arity, i;
            Word *argument_vector, *type_info_vector;
            argument_vector = data_value;

            /* If the argument vector is in range, copy the arguments */
            if (in_range(argument_vector)) {
                arity = entry_value[TYPE_CTOR_LAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = entry_value + TYPE_CTOR_LAYOUT_SIMPLE_ARGS_OFFSET;

                /* allocate space for new args. */
                incr_saved_hp(new_data, arity);

                /* copy arguments */
                for (i = 0; i < arity; i++) {
                    field(0, new_data, i) = copy_arg(&argument_vector[i],
                        type_info, (Word *) type_info_vector[i], lower_limit,
                        upper_limit);
                }
                /* tag this pointer */
                new_data = (Word) mkword(data_tag, new_data);
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
            break;
        }

        case MR_DATAREP_NOTAG:
            new_data = copy_arg(data_ptr, type_info, 
                    (Word *) *MR_TYPE_CTOR_LAYOUT_NO_TAG_VECTOR_ARGS(entry_value),
                    lower_limit, upper_limit);
            break;

        case MR_DATAREP_EQUIV: 
            new_data = copy_arg(data_ptr, type_info, 
                (const Word *) MR_TYPE_CTOR_LAYOUT_EQUIV_TYPE((Word *)
                        entry_value), lower_limit, upper_limit);
            break;

        case MR_DATAREP_EQUIV_VAR:
            new_data = copy(data_ptr, (Word *) type_info[(Word) entry_value],
                    lower_limit, upper_limit);
            break;

        case MR_DATAREP_INT:
        case MR_DATAREP_CHAR:
            new_data = data;
            break;

        case MR_DATAREP_FLOAT:
            #ifdef BOXED_FLOAT
                if (in_range(data_value)) {
                    incr_saved_hp(new_data, FLOAT_WORDS);
                    field(0, new_data, 0) = *data_value;
                    leave_forwarding_pointer(data_ptr, new_data);
                } else {
                    new_data = data;
                    found_forwarding_pointer(data);
                }
            #else
                new_data = data;
            #endif
            break;

        case MR_DATAREP_STRING:
            if (in_range(data_value)) {
                incr_saved_hp_atomic(new_data, 
                    (strlen((String) data_value) + sizeof(Word)) 
                                / sizeof(Word));
                strcpy((String) new_data, (String) data_value);
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
            break;

        case MR_DATAREP_PRED: {
            /*
            ** predicate closures store the number of curried arguments
            ** as their first argument, the Code * as their second, and
            ** then the arguments
            **
            ** Their type-infos have a pointer to type_ctor_info for
            ** pred/0, arity, and then argument typeinfos.
            */
            if (in_range(data_value)) {
                int args, i;
                Word *new_closure;

                /* get number of curried arguments */
                args = data_value[0];

                /* create new closure */
                incr_saved_hp(LVALUE_CAST(Word, new_closure), args + 2);

                /* copy number of arguments */
                new_closure[0] = args;

                /* copy pointer to code for closure */
                new_closure[1] = data_value[1];

#if 0		
		/*
		** XXX THIS IS WRONG.  We don't have any information
		** about the types of the things in closures.
		** The pred type only tells us about the arguments
		** which have not yet been applied, not the ones
		** in the closure.
		*/
                /* copy arguments */
                for (i = 0; i < args; i++) {
                    new_closure[i + 2] = copy(&data_value[i + 2],
                        (const Word *) 
                        type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                        lower_limit, upper_limit);
                }
#else
		fatal_error("sorry, not implemented: cannot copy closure");
#endif
                new_data = (Word) new_closure;
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
        }
            break;
        
        case MR_DATAREP_UNIV: 
            /* if the univ is stored in range, copy it */ 
            if (in_range(data_value)) {
                Word *new_data_ptr;

                /* allocate space for a univ */
                incr_saved_hp(new_data, 2);
                new_data_ptr = (Word *) new_data;
		/*
		** Copy the fields across.
		** Note: we must copy the data before the type_info,
		** because when copying the data, we need the type_info
		** to still contain the type rather than just holding
		** a forwarding pointer.
		*/
                new_data_ptr[UNIV_OFFSET_FOR_DATA] = copy(
                        &data_value[UNIV_OFFSET_FOR_DATA], 
                        (const Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                        lower_limit, upper_limit);
                new_data_ptr[UNIV_OFFSET_FOR_TYPEINFO] = 
                    (Word) copy_type_info( 
                        &data_value[UNIV_OFFSET_FOR_TYPEINFO],
                        lower_limit, upper_limit);
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
            break;

        case MR_DATAREP_VOID:
            fatal_error("Cannot copy a void type");
            break;

        case MR_DATAREP_ARRAY: {
            int i;

            if (in_range(data_value)) {
                MR_ArrayType *new_array;
                MR_ArrayType *old_array;
                Integer array_size;

                old_array = (MR_ArrayType *) data_value;
                array_size = old_array->size;
                new_array = MR_make_array(array_size);
                new_array->size = array_size;
                for (i = 0; i < array_size; i++) {
                    new_array->elements[i] = copy_arg(
                        &old_array->elements[i], type_info, 
                        (const Word *) 1, lower_limit, upper_limit);
                }
                new_data = (Word) new_array;
                leave_forwarding_pointer(data_ptr, new_data);
            } else {
                new_data = data;
                found_forwarding_pointer(data);
            }
            break;
        }

        case MR_DATAREP_TYPEINFO:
            new_data = (Word) copy_type_info(data_ptr,
                lower_limit, upper_limit);
            break;

        case MR_DATAREP_C_POINTER:
            if (in_range(data_value)) {
                /*
                ** This error occurs if we try to copy() a
                ** `c_pointer' type that points to memory allocated
                ** on the Mercury heap.
                */
                fatal_error("Cannot copy a c_pointer type");
            } else {
                new_data = data;
            }
            break;
            
        case MR_DATAREP_UNKNOWN: /* fallthru */
        default:
            fatal_error("Unknown layout type in deep copy");
            break;
    }

    return new_data;
}

/*
** copy_arg is like copy() except that it takes a
** pseudo_type_info (namely arg_pseudo_type_info) rather than
** a type_info.  The pseudo_type_info may contain type variables,
** which refer to arguments of the term_type_info.
*/
static Word
copy_arg(maybeconst Word *data_ptr, const Word *term_type_info,
		const Word *arg_pseudo_type_info, const Word *lower_limit,
		const Word *upper_limit)
{
	MR_MemoryList allocated_memory_cells;
	Word *new_type_info;
	Word new_data;

	allocated_memory_cells = NULL;
	new_type_info = MR_make_type_info(term_type_info, arg_pseudo_type_info,
					&allocated_memory_cells);
	new_data = copy(data_ptr, new_type_info, lower_limit, upper_limit);
	MR_deallocate(allocated_memory_cells);

	return new_data;
}


static Word *
copy_type_info(maybeconst Word *type_info_ptr, const Word *lower_limit,
		const Word *upper_limit)
{
	Word *type_info = (Word *) *type_info_ptr;

	if (in_range(type_info)) {
		Word *type_ctor_info;
		Word *new_type_info;
		Integer arity, offset, i;

		/*
		** Note that we assume type_ctor_infos will always be
		** allocated statically, so we never copy them.
		*/

		type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO((Word *)
			type_info);
		/*
		** optimize special case: if there's no arguments,
		** we don't need to construct a type_info; instead,
		** we can just return the type_ctor_info.
		*/
		if (type_info == type_ctor_info) {
			return type_ctor_info;
		}
		if (MR_TYPE_CTOR_INFO_IS_HO(type_ctor_info)) {
			arity = MR_TYPEINFO_GET_HIGHER_ARITY(type_info);
			incr_saved_hp(LVALUE_CAST(Word, new_type_info),
				arity + 2);
			new_type_info[0] = (Word) type_ctor_info;
			new_type_info[1] = arity;
			offset = 2;
		} else {
			arity = MR_TYPE_CTOR_INFO_GET_TYPE_ARITY(type_ctor_info);
			incr_saved_hp(LVALUE_CAST(Word, new_type_info),
				arity + 1);
			new_type_info[0] = (Word) type_ctor_info;
			offset = 1;
		}
		for (i = offset; i < arity + offset; i++) {
			new_type_info[i] = (Word) copy_type_info(&type_info[i],
				lower_limit, upper_limit);
		}
		leave_forwarding_pointer(type_info_ptr, (Word) new_type_info);
		return new_type_info;
	} else {
		found_forwarding_pointer(type_info);
		return type_info;
	}
}
