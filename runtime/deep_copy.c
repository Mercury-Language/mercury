/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the deep_copy() function.
*/

#include "imp.h"
#include "deep_copy.h"
#include "type_info.h"

#define in_range(X)	((X) >= lower_limit && (X) <= upper_limit)

/*
** Prototypes.
*/
static Word get_base_type_layout_entry(Word data, Word *type_info);
static Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	bool *allocated);

/*
** Due to the depth of the control here, we'll use 4 space indentation.
*/
Word deep_copy(Word data, Word *type_info, Word *lower_limit, Word *upper_limit)
{
    Word layout_entry, *entry_value, *data_value;
    int data_tag, entry_tag; 

    int arity, i;
    bool allocated;
    Word *argument_vector, *type_info_vector, *new_type_info;

    Word new_data;
	
    data_tag = tag(data);
    data_value = (Word *) body(data, data_tag);

    layout_entry = get_base_type_layout_entry(data_tag, type_info);

    entry_tag = tag(layout_entry);
    entry_value = (Word *) body(layout_entry, entry_tag);

    switch(entry_tag) {

        case TYPELAYOUT_CONST_TAG:      /* and COMP_CONST_TAG */

            /* Some builtins need special treatment */
            if ((Word) entry_value <= TYPELAYOUT_MAX_VARINT) {
                int builtin_type = unmkbody(entry_value);

                switch(builtin_type) {

                    case TYPELAYOUT_UNASSIGNED_VALUE:
                        fatal_error("Attempt to use an UNASSIGNED tag "
                            "in deep_copy");
                    break;

                    case TYPELAYOUT_UNUSED_VALUE:
                        fatal_error("Attempt to use an UNUSED tag "
                            "in deep_copy");
                    break;

                    case TYPELAYOUT_STRING_VALUE:
                        if (in_range(data_value)) {
                            incr_saved_hp_atomic(new_data, 
                                (strlen((String) data_value) + sizeof(Word)) 
                                / sizeof(Word));
                            strcpy((String) new_data, (String) data_value);
                        } else {
                            new_data = data;
                        }
                    break;

                    case TYPELAYOUT_FLOAT_VALUE:
                        new_data = data;
                    break;

                    case TYPELAYOUT_INT_VALUE:
                        new_data = data;
                    break;

                    case TYPELAYOUT_CHARACTER_VALUE:
                        new_data = data;
                    break;

                    case TYPELAYOUT_UNIV_VALUE: 
                            /* if the univ is stored in range, copy it */ 
                        if (in_range(data_value)) {
                            Word *new_data_ptr;

                                /* allocate space for a univ */
                            incr_saved_hp(new_data, 2);
                            new_data_ptr = (Word *) new_data;
                            new_data_ptr[UNIV_OFFSET_FOR_TYPEINFO] = 
                                data_value[UNIV_OFFSET_FOR_TYPEINFO];
                            new_data_ptr[UNIV_OFFSET_FOR_DATA] = deep_copy(
                                data_value[UNIV_OFFSET_FOR_DATA], 
                                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                                lower_limit, upper_limit);
                        } else {
                            new_data = data;
                        }
                    break;

                    case TYPELAYOUT_PREDICATE_VALUE:
                    {
                        /* predicate closures store the number of curried
                         * arguments as their first argument, the
                         * Code * as their second, and then the
                         * arguments
                         *
                         * Their type-infos have a pointer to
                         * base_type_info for pred/0, arity, and then
                         * argument typeinfos.
                         */
                        int args;
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
                                (Word *)
                                type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                                lower_limit, upper_limit);
                        }
                        new_data = (Word) new_closure;
                    }
                    break;

                    default:
                        fatal_error("Invalid tag value in deep_copy");
                    break;
                }
            } else {
                    /* a constant or enumeration */
                new_data = data;	/* just a copy of the actual item */
            }
        break;

        case TYPELAYOUT_SIMPLE_TAG: 

            argument_vector = data_value;

                /* If the argument vector is in range, copy the
                 * arguments.
                 */
            if (in_range(argument_vector)) {
                arity = entry_value[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = entry_value + TYPELAYOUT_SIMPLE_ARGS_OFFSET;

                    /* allocate space for new args. */
                incr_saved_hp(new_data, arity);

                    /* copy arguments */
                for (i = 0; i < arity; i++) {
                    new_type_info = make_type_info(type_info,
                        (Word *) type_info_vector[i], &allocated);
                    field(0, new_data, i) = 
                        deep_copy(argument_vector[i],
                            new_type_info, lower_limit, upper_limit); 
                    if (allocated) { 
                        free(new_type_info);
                    }
                }
                    /* tag this pointer */
                new_data = mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
        break;

        case TYPELAYOUT_COMPLICATED_TAG:
        {
            Word secondary_tag;
            Word *new_entry;

                /* if the vector containing the secondary
                 * tags and the arguments is in range, 
                 * copy it.
                 */
            if (in_range(data_value)) {
                secondary_tag = *data_value;
                argument_vector = data_value + 1;
                new_entry = (Word *) entry_value[secondary_tag +1];
                arity = new_entry[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = new_entry + 
                    TYPELAYOUT_SIMPLE_ARGS_OFFSET;

                /* allocate space for new args, and 
                 * secondary tag 
                 */
                incr_saved_hp(new_data, arity + 1);

                    /* copy secondary tag */
                field(0, new_data, 0) = secondary_tag;

                    /* copy arguments */
                for (i = 0; i < arity; i++) {
                    new_type_info = make_type_info(type_info,
                        (Word *) type_info_vector[i], &allocated);
                    field(0, new_data, i + 1) = 
                        deep_copy(argument_vector[i], 
                            new_type_info, lower_limit, 
                            upper_limit);
                    if (allocated) {
                        free(new_type_info);
                    }
                }

                /* tag this pointer */
                new_data = mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
        }
        break;

        case_TYPELAYOUT_EQUIV_TAG:
            /* note: we treat no_tag types just like equivalences */

            if ((Word) entry_value < TYPELAYOUT_MAX_VARINT) {
                deep_copy(data, (Word *) type_info[(Word) entry_value],
                    lower_limit, upper_limit);
            } else {
                new_type_info = make_type_info(type_info, 
                    entry_value, &allocated);
                new_data = deep_copy(data, new_type_info, 
                    lower_limit, upper_limit);
                if (allocated) {
                    free(new_type_info);
                }
            }
        break;

        default:
            fatal_error("Unknown layout tag in deep copy");
        break;
    }

    return new_data;
}

Word get_base_type_layout_entry(Word data_tag, Word *type_info)
{
	Word *base_type_info, *base_type_layout;

	base_type_info = (Word *) type_info[0];

	if (base_type_info == 0) {
		base_type_info = type_info;
	}

	base_type_layout = (Word *) base_type_info[OFFSET_FOR_BASE_TYPE_LAYOUT];

	return base_type_layout[data_tag];
}


	/* 
	 * Given a type_info (term_type_info) which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving the values of the type parameters of this type,
	 * and a pseudo-type_info (arg_pseudo_type_info), which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving EITHER
	 * 	- the values of the type parameters of this type,
	 * or	- an indication of the type parameter of the
	 * 	  term_type_info that should be substituted here
	 *
	 * This returns a fully instantiated type_info, a version of the
	 * arg_pseudo_type_info with all the type variables filled in.
	 * If there are no type variables to fill in, we return the
	 * arg_pseudo_type_info, unchanged. Otherwise, we allocate
	 * memory using malloc().  If memory is allocated, the boolean
	 * argument (passed by reference) is set to TRUE, otherwise it is
	 * set to FALSE.  It is the caller's responsibility to check whether 
	 * the call to make_type_info allocated memory, and if so, free
	 * it.
	 *
	 * This code could be tighter. In general, we want to
	 * handle our own allocations rather than using malloc().
	 * Also, we might be able to do only one traversal.
	 *
	 * NOTE: If you are changing this code, you might also need
	 * to change the code in create_type_info in library/std_util.m,
	 * which does much the same thing, only allocating on the 
	 * heap instead of using malloc.
	 */

Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	bool *allocated) 
{
	int arity, i;
	Word base_type_info;
	Word *type_info;

	*allocated = FALSE;

		/* The arg_pseudo_type_info might be a polymorphic variable */

	if ((Word) arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
		return (Word *) term_type_info[(Word) arg_pseudo_type_info];
	}


	base_type_info = arg_pseudo_type_info[0];

		/* no arguments - optimise common case */
	if (base_type_info == 0) {
		return arg_pseudo_type_info;
	} else {
		arity = ((Word *) base_type_info)[0];
	}

	for (i = arity; i > 0; i--) {
		if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
			break;
		}
	}

		/* 
		 * See if any of the arguments were polymorphic.
		 * If so, substitute.
		 */
	if (i > 0) {
		type_info = checked_malloc(arity * sizeof(Word));
		*allocated = TRUE;
		for (i = 0; i <= arity; i++) {
			if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
				type_info[i] = term_type_info[arg_pseudo_type_info[i]];
				if (type_info[i] < TYPELAYOUT_MAX_VARINT) {
					fatal_error("Error! Can't instantiate type variable.");
				}
			} else {
				type_info[i] = arg_pseudo_type_info[i];
			}
		}
		return type_info;
	} else {
		return arg_pseudo_type_info;
	}
}
