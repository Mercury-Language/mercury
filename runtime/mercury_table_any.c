/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the mercury_table_type() function.
*/

#include "mercury_imp.h"
#include "mercury_tabling.h"
#include "mercury_table_any.h"
#include "mercury_type_info.h"
#include <stdio.h>

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);
MR_DECLARE_STRUCT(mercury_data___base_type_info_func_0);

/*
** Due to the depth of the control here, we'll use 4 space indentation.
**
** NOTE : changes to this function will probably also have to be reflected
** in mercury_deep_copy.c and std_util::ML_expand().
*/

TrieNode
MR_table_type(TrieNode table, Word *type_info, Word data)
{
    Word *base_type_info, *base_type_layout, *base_type_functors;
    Word layout_for_tag, *layout_vector_for_tag, *data_value;
    enum MR_DataRepresentation data_rep;
    int data_tag, entry_tag;

    MR_MemoryList allocated_memory_cells = NULL;

    data_tag = tag(data);
    data_value = (Word *) body(data, data_tag);

    base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
    base_type_layout = MR_BASE_TYPEINFO_GET_TYPELAYOUT(base_type_info);
    base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(base_type_info);

    layout_for_tag = base_type_layout[data_tag];
    layout_vector_for_tag = (Word *) strip_tag(layout_for_tag);

    data_rep = MR_categorize_data(MR_TYPEFUNCTORS_INDICATOR(base_type_functors),
		    layout_for_tag);

#ifdef	MR_TABLE_DEBUG
    if (MR_tabledebug) {
	printf("ENTRY %p %x, data rep: %d\n", table, data, data_rep);
    }
#endif	/* MR_TABLE_DEBUG */

    switch (data_rep) {
        case MR_DATAREP_ENUM: {
	    int functors = MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
				layout_vector_for_tag);
	    MR_DEBUG_TABLE_ENUM(table, functors, data);
            break;
        }
        case MR_DATAREP_COMPLICATED_CONST: {
	    int functors = MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
				layout_vector_for_tag);
	    MR_DEBUG_TABLE_TAG(table, data_tag);
	    MR_DEBUG_TABLE_ENUM(table, functors, unmkbody(data));
            break;
        }
        case MR_DATAREP_SIMPLE: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;

            argument_vector = data_value;

            arity = layout_vector_for_tag[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector = &layout_vector_for_tag[
		    		TYPELAYOUT_SIMPLE_ARGS_OFFSET];

	    MR_DEBUG_TABLE_TAG(table, data_tag);

                 /* copy arguments */
            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);

                MR_DEBUG_TABLE_ANY(table, new_type_info, argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_COMPLICATED: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;
            Word secondary_tag, num_sharers, *new_layout_vector;

            secondary_tag = *data_value;
            argument_vector = data_value + 1;

            num_sharers = MR_TYPELAYOUT_COMPLICATED_VECTOR_NUM_SHARERS(
            			layout_vector_for_tag);
            new_layout_vector =
                MR_TYPELAYOUT_COMPLICATED_VECTOR_GET_SIMPLE_VECTOR(
                    layout_vector_for_tag, secondary_tag);
            arity = new_layout_vector[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector =
		    &new_layout_vector[TYPELAYOUT_SIMPLE_ARGS_OFFSET];

	    MR_DEBUG_TABLE_TAG(table, data_tag);
	    MR_DEBUG_TABLE_ENUM(table, num_sharers, secondary_tag);

            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);

                MR_DEBUG_TABLE_ANY(table, new_type_info, argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_NOTAG: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) *MR_TYPELAYOUT_NO_TAG_VECTOR_ARGS(
		    layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) MR_TYPELAYOUT_EQUIV_TYPE(layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV_VAR:
            MR_DEBUG_TABLE_ANY(table,
		(Word *) type_info[(Word) layout_vector_for_tag], data);
            break;

        case MR_DATAREP_INT:
            MR_DEBUG_TABLE_INT(table, data);
            break;

        case MR_DATAREP_CHAR:
            MR_DEBUG_TABLE_CHAR(table, data);
            break;

        case MR_DATAREP_FLOAT:
            MR_DEBUG_TABLE_FLOAT(table, data);
            break;

        case MR_DATAREP_STRING:
            MR_DEBUG_TABLE_STRING(table, data);
            break;

        case MR_DATAREP_PRED: {
            int i;
            Word args = data_value[0];

            MR_DEBUG_TABLE_STRING(table, args);
            MR_DEBUG_TABLE_STRING(table, data_value[1]);

            for (i = 0; i < args; i++) {
        	MR_DEBUG_TABLE_ANY(table,
                    (Word *) type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                    data_value[i+2]);
            }
            break;
        }
        case MR_DATAREP_UNIV:
            MR_DEBUG_TABLE_TYPEINFO(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO]);
            MR_DEBUG_TABLE_ANY(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                data_value[UNIV_OFFSET_FOR_DATA]);
            break;

        case MR_DATAREP_VOID:
            fatal_error("Cannot table a void type");
            break;

        case MR_DATAREP_ARRAY: {
            int i;
            MR_ArrayType *array;
            Word *new_type_info;
            Integer array_size;

            array = (MR_ArrayType *) data_value;
            array_size = array->size;

            new_type_info = MR_make_type_info(type_info, (Word *) 1,
                &allocated_memory_cells);

            for (i = 0; i < array_size; i++) {
        	MR_DEBUG_TABLE_ANY(table, new_type_info, array->elements[i]);
            }
            break;
        }
        case MR_DATAREP_TYPEINFO:
            MR_DEBUG_TABLE_TYPEINFO(table, (Word *) data_value);
            break;

        case MR_DATAREP_C_POINTER:
            fatal_error("Attempt to use a C_POINTER tag in table");
            break;

        case MR_DATAREP_UNKNOWN: /* fallthru */
        default:
            fatal_error("Unknown layout tag in table_any");
            break;
    }

    MR_deallocate(allocated_memory_cells);

    return table;
} /* end table_any() */
