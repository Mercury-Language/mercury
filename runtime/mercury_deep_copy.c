/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the deep_copy() function.
*/

#include "mercury_imp.h"
#include "mercury_deep_copy.h"
#include "mercury_type_info.h"
#include "mercury_memory.h"

#define in_range(X)	((X) >= lower_limit && (X) <= upper_limit)

/*
** Prototypes.
*/
static Word deep_copy_arg(Word data, Word *type_info, Word *arg_type_info,
	Word *lower_limit, Word *upper_limit);
static Word * deep_copy_type_info(Word *type_info,
	Word *lower_limit, Word *upper_limit);

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);
MR_DECLARE_STRUCT(mercury_data___base_type_info_func_0);

/*
** deep_copy(): see mercury_deep_copy.h for documentation.
**
** Due to the depth of the control here, we'll use 4 space indentation.
**
** NOTE : changes to this function will probably also have to be reflected 
** in the function std_util::ML_expand() and mercury_table_any.c
*/
Word 
deep_copy(Word data, Word *type_info, Word *lower_limit, Word *upper_limit)
{
    Word *base_type_info, *base_type_layout, *base_type_functors;
    Word functors_indicator;
    Word layout_entry, *entry_value, *data_value;
    enum MR_DataRepresentation data_rep;
    int data_tag; 
    Word new_data;
	
    data_tag = tag(data);
    data_value = (Word *) body(data, data_tag);

    base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
    base_type_layout = MR_BASE_TYPEINFO_GET_TYPELAYOUT(base_type_info);
    layout_entry = base_type_layout[data_tag];

    base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(base_type_info);
    functors_indicator = MR_TYPEFUNCTORS_INDICATOR(base_type_functors);

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
                new_entry = (Word *) entry_value[secondary_tag +1];
                arity = new_entry[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = new_entry + TYPELAYOUT_SIMPLE_ARGS_OFFSET;

                    /* allocate space for new args, and secondary tag */
                incr_saved_hp(new_data, arity + 1);

                    /* copy secondary tag */
                field(0, new_data, 0) = secondary_tag;

                    /* copy arguments */
                for (i = 0; i < arity; i++) {
                    field(0, new_data, i + 1) = deep_copy_arg(
                        argument_vector[i], type_info,
                        (Word *) type_info_vector[i], lower_limit,
                        upper_limit);
                }

                    /* tag this pointer */
                new_data = (Word) mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
            break;
        }

        case MR_DATAREP_SIMPLE: {
            int arity, i;
	    Word *argument_vector, *type_info_vector;
            argument_vector = data_value;

                /* If the argument vector is in range, copy the arguments */
            if (in_range(argument_vector)) {
                arity = entry_value[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = entry_value + TYPELAYOUT_SIMPLE_ARGS_OFFSET;

                    /* allocate space for new args. */
                incr_saved_hp(new_data, arity);

                    /* copy arguments */
                for (i = 0; i < arity; i++) {
                    field(0, new_data, i) = deep_copy_arg(argument_vector[i],
                        type_info, (Word *) type_info_vector[i], lower_limit,
                        upper_limit);
                }
                    /* tag this pointer */
                new_data = (Word) mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
            break;
        }

        case MR_DATAREP_NOTAG:
            new_data = deep_copy_arg(data, type_info, 
                    (Word *) *MR_TYPELAYOUT_NO_TAG_VECTOR_ARGS(entry_value),
                    lower_limit, upper_limit);
            break;

        case MR_DATAREP_EQUIV: 
            new_data = deep_copy_arg(data, type_info, 
                    (Word *) MR_TYPELAYOUT_EQUIV_TYPE((Word *) entry_value),
                    lower_limit, upper_limit);
            break;

        case MR_DATAREP_EQUIV_VAR:
            new_data = deep_copy(data, (Word *) type_info[(Word) entry_value],
                    lower_limit, upper_limit);
            break;

        case MR_DATAREP_INT:
        case MR_DATAREP_CHAR:
            new_data = data;
            break;

        case MR_DATAREP_FLOAT:
			#ifdef BOXED_FLOAT
                if (in_range(data_value)) {
                        /*
                        ** force a deep copy by converting to float
                        ** and back
                        */
                    new_data = float_to_word(word_to_float(data));
                } else {
                    new_data = data;
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
            } else {
                new_data = data;
            }
            break;

        case MR_DATAREP_PRED: {
                /*
                ** predicate closures store the number of curried
                ** arguments as their first argument, the
                ** Code * as their second, and then the
                ** arguments
                **
                ** Their type-infos have a pointer to
                ** base_type_info for pred/0, arity, and then
                ** argument typeinfos.
                **/
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

                    /* copy arguments */
                for (i = 0; i < args; i++) {
                    new_closure[i + 2] = deep_copy(data_value[i + 2],
                        (Word *) type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                        lower_limit, upper_limit);
                }
            new_data = (Word) new_closure;
            } else {
                new_data = data;
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
                new_data_ptr[UNIV_OFFSET_FOR_TYPEINFO] = 
                    (Word) deep_copy_type_info( (Word *)
                    data_value[UNIV_OFFSET_FOR_TYPEINFO],
                    lower_limit, upper_limit);
                new_data_ptr[UNIV_OFFSET_FOR_DATA] = deep_copy(
                    data_value[UNIV_OFFSET_FOR_DATA], 
                    (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                    lower_limit, upper_limit);
            } else {
                new_data = data;
            }
            break;

        case MR_DATAREP_VOID:
            fatal_error("Cannot deep copy a void type");
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
                    new_array->elements[i] = deep_copy_arg(
                        old_array->elements[i], type_info, 
                        (Word *) 1, lower_limit, upper_limit);
                }
                new_data = (Word) new_array;
            } else {
                new_data = data;
            }
            break;
        }

        case MR_DATAREP_TYPEINFO:
            new_data = (Word) deep_copy_type_info(data_value,
                lower_limit, upper_limit);
            break;

        case MR_DATAREP_C_POINTER:
            if (in_range(data_value)) {
                /*
                ** This error occurs if we try to deep_copy() a
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
} /* end deep_copy() */

/*
** deep_copy_arg is like deep_copy() except that it takes a
** pseudo_type_info (namely arg_pseudo_type_info) rather than
** a type_info.  The pseudo_type_info may contain type variables,
** which refer to arguments of the term_type_info.
*/
static Word
deep_copy_arg(Word data, Word *term_type_info, Word *arg_pseudo_type_info,
		Word *lower_limit, Word *upper_limit)
{
	MR_MemoryList allocated_memory_cells;
	Word *new_type_info;
	Word new_data;

	allocated_memory_cells = NULL;
	new_type_info = MR_make_type_info(term_type_info, arg_pseudo_type_info,
					&allocated_memory_cells);
	new_data = deep_copy(data, new_type_info, lower_limit, upper_limit);
	MR_deallocate(allocated_memory_cells);

	return new_data;
}


Word *
deep_copy_type_info(Word *type_info, Word *lower_limit, Word *upper_limit)
{
	if (in_range(type_info)) {
		Word *base_type_info;
		Word *new_type_info;
		Integer arity, i;

		/* XXX this doesn't handle higher-order types properly */

		base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
		arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);
		incr_saved_hp(LVALUE_CAST(Word, new_type_info), arity + 1);
		new_type_info[0] = type_info[0];
		for (i = 1; i < arity + 1; i++) {
			new_type_info[i] = (Word) deep_copy_type_info(
				(Word *) type_info[i],
				lower_limit, upper_limit);
		}
		return new_type_info;
	} else {
		return type_info;
	}
}


#define SWAP(val1, val2, type)		\
	do {				\
		type swap_tmp;		\
		swap_tmp = (val1);	\
		(val1) = (val2);	\
		(val2) = swap_tmp;	\
	} while (0)

#ifndef CONSERVATIVE_GC
/*
** MR_make_long_lived(): see mercury_deep_copy.h for documentation.
*/
Word
MR_make_long_lived(Word term, Word *type_info, Word *lower_limit)
{
	Word result;

	restore_transient_registers();	/* Because we play with MR_hp */

	if (lower_limit < MR_heap_zone->bottom ||
			lower_limit > MR_heap_zone->top) {
		lower_limit = MR_heap_zone->bottom;
	}

	/* temporarily swap the heap with the global heap */
	SWAP(MR_heap_zone, MR_global_heap_zone, MemoryZone *);
	SWAP(MR_hp, MR_global_hp, Word *);

	/* copy values from the heap to the global heap */
	save_transient_registers();
	result = deep_copy(term, type_info, lower_limit,
			MR_global_heap_zone->top);
	restore_transient_registers();

	/* swap the heap and global heap back again */
	SWAP(MR_heap_zone, MR_global_heap_zone, MemoryZone *);
	SWAP(MR_hp, MR_global_hp, Word *);

	save_transient_registers();	/* Because we played with MR_hp */

	return result;
}
#endif	/* not CONSERVATIVE_GC */

