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
MR_table_type(Word *type_info, Word data, TrieNode table)
{
    Word *base_type_info, *base_type_layout, *base_type_functors;
    Word functors_indicator;
    Word layout_entry, *entry_value, *data_value;
    enum MR_DataRepresentation data_rep;
    int data_tag, entry_tag; 

    MR_MemoryList allocated_memory_cells = NULL;

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
        case MR_DATAREP_ENUM: {
            Word functors = 
                    MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(entry_value);
            table = (Word**) MR_TABLE_ENUM(table, functors, data);
            break;
        }
        case MR_DATAREP_COMPLICATED_CONST: {
            Word functors = 
                    MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(entry_value);
            table = (Word**) MR_TABLE_TAG(table, data_tag);
            table = (Word**) MR_TABLE_ENUM(table, functors, (Word) data_value);
            break;
        }
        case MR_DATAREP_SIMPLE: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;
                                           
            argument_vector = data_value;

            arity = entry_value[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector = entry_value + TYPELAYOUT_SIMPLE_ARGS_OFFSET;

            table = (Word**) MR_TABLE_TAG(table, data_tag);

                 /* copy arguments */
            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);
                
                table = (Word**) MR_TABLE_ANY(table, new_type_info,
                    argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_COMPLICATED: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;
            Word secondary_tag, num_sharers, *new_entry;

            secondary_tag = *data_value;
            argument_vector = data_value + 1;
            new_entry = (Word *) entry_value[secondary_tag +1];
            arity = new_entry[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector = new_entry + 
                TYPELAYOUT_SIMPLE_ARGS_OFFSET;

            table = (Word**) MR_TABLE_TAG(table, data_tag);
       
            num_sharers = MR_TYPELAYOUT_COMPLICATED_VECTOR_NUM_SHARERS(
                    entry_value);
            table = (Word**) MR_TABLE_ENUM(table, num_sharers, secondary_tag);
            
            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);
                
                table = (Word**) MR_TABLE_ANY(table, new_type_info, 
                    argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_NOTAG: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) *MR_TYPELAYOUT_NO_TAG_VECTOR_ARGS(entry_value),
                &allocated_memory_cells);
            table = (Word**) MR_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) MR_TYPELAYOUT_EQUIV_TYPE(entry_value),
                &allocated_memory_cells);
            table = (Word**) MR_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV_VAR:
            table = (Word**) MR_TABLE_ANY(table,  
                (Word *) type_info[(Word) entry_value], data);
            break;

        case MR_DATAREP_INT:
            table = (Word**) MR_TABLE_INT(table, data);
            break;

        case MR_DATAREP_CHAR:
            table = (Word**) MR_TABLE_CHAR(table, data);
            break;

        case MR_DATAREP_FLOAT:
            table = (Word**) MR_TABLE_FLOAT(table, data);
            break;

        case MR_DATAREP_STRING:
            table = (Word**) MR_TABLE_STRING(table, data);
            break;

        case MR_DATAREP_PRED: {
            int i;
            Word args = data_value[0];

            table = (Word **) MR_TABLE_WORD(table, args);
            table = (Word **) MR_TABLE_WORD(table, data_value[1]);
            
            for (i = 0; i < args; i++) {
                table = (Word **) MR_TABLE_ANY(table, 
                    (Word *) type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                    data_value[i+2]);
            }
            break;
        }
        case MR_DATAREP_UNIV:
            table = (Word**) MR_TABLE_TYPE_INFO(table,
                data_value[UNIV_OFFSET_FOR_TYPEINFO]);
            table = (Word**) MR_TABLE_ANY(table,
                data_value[UNIV_OFFSET_FOR_TYPEINFO],
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
                table = (Word**) MR_TABLE_ANY(table, new_type_info,
                    array->elements[i]);
            }
            break;
        }
        case MR_DATAREP_TYPEINFO:
            table = (Word**) MR_TABLE_TYPE_INFO(table, data_value);
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

