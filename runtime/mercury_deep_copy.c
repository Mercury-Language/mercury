/*
** Copyright (C) 1997 The University of Melbourne.
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
static Word * deep_copy_type_info(Word *type_info,
	Word *lower_limit, Word *upper_limit);

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);
MR_DECLARE_STRUCT(mercury_data___base_type_info_func_0);

/*
** Due to the depth of the control here, we'll use 4 space indentation.
*/
Word 
deep_copy(Word data, Word *type_info, Word *lower_limit, Word *upper_limit)
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

                    case TYPELAYOUT_PREDICATE_VALUE:
                    {
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
                            int args;
                            Word *new_closure;

                            /* get number of curried arguments */
                            args = data_value[0];

                            /* create new closure */
                            incr_saved_hp(LVALUE_CAST(Word, new_closure),
				args + 2);

                            /* copy number of arguments */
                            new_closure[0] = args;

                            /* copy pointer to code for closure */
                            new_closure[1] = data_value[1];

                            /* copy arguments */
                            for (i = 0; i < args; i++) {
                                new_closure[i + 2] = deep_copy(
				    data_value[i + 2],
                                    (Word *) type_info[i +
					TYPEINFO_OFFSET_FOR_PRED_ARGS],
                                    lower_limit, upper_limit);
                            }
                            new_data = (Word) new_closure;
			} else {
			    new_data = data;
			}
                        break;
                    }

                    case TYPELAYOUT_VOID_VALUE:
                        fatal_error("Attempt to use a VOID tag in deep_copy");
                        break;

                    case TYPELAYOUT_ARRAY_VALUE:
                        if (in_range(data_value)) {
			    MR_ArrayType *new_array;
			    MR_ArrayType *old_array;
			    Integer array_size;

			    old_array = (MR_ArrayType *) data_value;
			    array_size = old_array->size;
			    new_array = MR_make_array(array_size);
			    new_array->size = array_size;
			    for (i = 0; i < array_size; i++) {
				new_array->elements[i] = old_array->elements[i];
			    }
			    new_data = (Word) new_array;
			} else {
			    new_data = data;
			}
			break;

                    case TYPELAYOUT_TYPEINFO_VALUE:
			new_data = (Word) deep_copy_type_info(data_value,
			    lower_limit, upper_limit);
                        break;

                    case TYPELAYOUT_C_POINTER_VALUE:
                        if (in_range(data_value)) {
			    /*
			    ** This error occurs if we try to deep_copy() a
			    ** `c_pointer' type that points to memory allocated
			    ** on the Mercury heap.
			    */
                            fatal_error("Attempt to use a C_POINTER tag "
				    "in deep_copy");
                        } else {
                            new_data = data;
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

                /*
		** If the argument vector is in range, copy the
                ** arguments.
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
                new_data = (Word) mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
            break;

        case TYPELAYOUT_COMPLICATED_TAG:
        {
            Word secondary_tag;
            Word *new_entry;

                /*
		** if the vector containing the secondary
                ** tags and the arguments is in range, 
                ** copy it.
                */
            if (in_range(data_value)) {
                secondary_tag = *data_value;
                argument_vector = data_value + 1;
                new_entry = (Word *) entry_value[secondary_tag +1];
                arity = new_entry[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
                type_info_vector = new_entry + 
                    TYPELAYOUT_SIMPLE_ARGS_OFFSET;

                /*
		** allocate space for new args, and 
                ** secondary tag 
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
                new_data = (Word) mkword(data_tag, new_data);
            } else {
                new_data = data;
            }
            break;
        }

        case TYPELAYOUT_EQUIV_TAG:
            /* note: we treat no_tag types just like equivalences */

            if ((Word) entry_value < TYPELAYOUT_MAX_VARINT) {
                new_data = deep_copy(data,
		    (Word *) type_info[(Word) entry_value],
                    lower_limit, upper_limit);
            } else {
		/*
		** offset 0 is no-tag indicator
		** offset 1 is the pseudo-typeinfo
		** (as per comments in base_type_layout.m)
		** XXX should avoid use of hard-coded offset `1' here
		*/
                new_type_info = make_type_info(type_info, 
                    (Word *) entry_value[1], &allocated);
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
} /* end deep_copy() */

Word 
get_base_type_layout_entry(Word data_tag, Word *type_info)
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
	** Given a type_info (term_type_info) which contains a
	** base_type_info pointer and possibly other type_infos
	** giving the values of the type parameters of this type,
	** and a pseudo-type_info (arg_pseudo_type_info), which contains a
	** base_type_info pointer and possibly other type_infos
	** giving EITHER
	** 	- the values of the type parameters of this type,
	** or	- an indication of the type parameter of the
	** 	  term_type_info that should be substituted here
	**
	** This returns a fully instantiated type_info, a version of the
	** arg_pseudo_type_info with all the type variables filled in.
	** If there are no type variables to fill in, we return the
	** arg_pseudo_type_info, unchanged. Otherwise, we allocate
	** memory using malloc().  If memory is allocated, the boolean
	** argument (passed by reference) is set to TRUE, otherwise it is
	** set to FALSE.  It is the caller's responsibility to check whether 
	** the call to make_type_info allocated memory, and if so, free
	** it.
	**
	** This code could be tighter. In general, we want to
	** handle our own allocations rather than using malloc().
	** Also, we might be able to do only one traversal.
	**
	** NOTE: If you are changing this code, you might also need
	** to change the code in create_type_info in library/std_util.m,
	** which does much the same thing, only allocating on the 
	** heap instead of using malloc.
	*/

Word *
make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	bool *allocated) 
{
	int arity, i, extra_args;
	Word *base_type_info;
	Word *type_info;

	*allocated = FALSE;

		/* 
		** The arg_pseudo_type_info might be a polymorphic variable,
		** is so - substitute.
		*/

	if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {
		return (Word *) term_type_info[(Word) arg_pseudo_type_info];
	}

	base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(arg_pseudo_type_info);

		/* no arguments - optimise common case */
	if (base_type_info == arg_pseudo_type_info) {
		return arg_pseudo_type_info;
	} 

        if (MR_BASE_TYPEINFO_IS_HO(base_type_info)) {
                arity = MR_TYPEINFO_GET_HIGHER_ARITY(arg_pseudo_type_info);
                extra_args = 2;
        } else {
                arity = MR_BASE_TYPEINFO_GET_TYPE_ARITY(base_type_info);
                extra_args = 1;
        }

		/*
                ** Check for type variables -- if there are none,
                ** we don't need to create a new type_info.
                */
	for (i = arity + extra_args - 1; i >= extra_args; i--) {
		if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info[i])) {
			break;
		}
	}

		/*
		** Do we need to create a new type_info?
		*/ 
	if (i >= extra_args) {
		type_info = checked_malloc((arity + extra_args) * sizeof(Word));
		*allocated = TRUE;

			/*
			** Copy any preliminary arguments to the type_info 
			** (this means the base_type_info and possibly 
			** arity for higher order terms).
			*/ 
                for (i = 0; i < extra_args; i++) {
                        type_info[i] = arg_pseudo_type_info[i];
                }

			/*
			**  Copy type arguments, substituting for any
			**  type variables.
			*/ 
		for (i = extra_args; i < arity + extra_args; i++) {
			if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info[i])) {
				type_info[i] = term_type_info[
					arg_pseudo_type_info[i]];
				if (type_info[i] < TYPELAYOUT_MAX_VARINT) {
					fatal_error("make_type_info: "
						"unbound type variable.");
				}
			} else {
				type_info[i] = arg_pseudo_type_info[i];
			}
		}
		return type_info;
	} else {
		return arg_pseudo_type_info;
	}
} /* end make_type_info() */

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
		new_type_info = make_many(Word, arity + 1);
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
