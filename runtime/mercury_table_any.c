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

/*
** Prototypes.
*/
static Word get_base_type_layout_entry(Word data, Word *type_info);

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
    Word layout_entry, *entry_value, *data_value;
    int data_tag, entry_tag; 

    int arity, i;
    MR_MemoryList allocated_memory_cells = NULL;
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
                            "in table_any");
                        break;

                    case TYPELAYOUT_UNUSED_VALUE:
                        fatal_error("Attempt to use an UNUSED tag "
                            "in table_any");
                        break;

                    case TYPELAYOUT_STRING_VALUE:
                        table = (Word**) MR_TABLE_STRING(table, data);
                        break;

                    case TYPELAYOUT_FLOAT_VALUE:
                        table = (Word**) MR_TABLE_FLOAT(table, data);
                        break;

                    case TYPELAYOUT_INT_VALUE:
                        table = (Word**) MR_TABLE_INT(table, data);
                        break;

                    case TYPELAYOUT_CHARACTER_VALUE:
                        table = (Word**) MR_TABLE_CHAR(table, data);
                        break;

                    case TYPELAYOUT_UNIV_VALUE:
                        table = (Word**) MR_TABLE_TYPE_INFO(table,
                            data_value[UNIV_OFFSET_FOR_TYPEINFO]);
                        table = (Word**) MR_TABLE_ANY(table,
                            data_value[UNIV_OFFSET_FOR_TYPEINFO],
                            data_value[UNIV_OFFSET_FOR_DATA]);
                        break;

                    case TYPELAYOUT_PREDICATE_VALUE:
                    {
                        Word args = data_value[0];

                        table = (Word **) MR_TABLE_WORD(table, args);
                        table = (Word **) MR_TABLE_WORD(table, data_value[1]);
            
                        for (i = 0; i < args; i++) {
                            table = (Word **) MR_TABLE_ANY(table, 
                            (Word *) type_info[i + 
                                TYPEINFO_OFFSET_FOR_PRED_ARGS],
                                data_value[i+2]);
                        }
                    }
                    case TYPELAYOUT_VOID_VALUE:
                        fatal_error("Attempt to use a VOID tag in table_any");
                        break;

                    case TYPELAYOUT_ARRAY_VALUE:
                    {
                        MR_ArrayType *array;
                        Integer array_size;        
        
                        array = (MR_ArrayType *) data_value;
                        array_size = array->size;
            
                        new_type_info = MR_make_type_info(type_info,
                            (Word *) 1, &allocated_memory_cells);
            
                        for (i = 0; i < array_size; i++) {
                            table = (Word**) MR_TABLE_ANY(table, 
                                new_type_info,
                                array->elements[i]);
                        }
                        break;
                    }
                    case TYPELAYOUT_TYPEINFO_VALUE:
                        table = (Word**) MR_TABLE_TYPE_INFO(table, data_value);
                        break;

                    case TYPELAYOUT_C_POINTER_VALUE:
                        fatal_error("Attempt to use a C_POINTER tag "
                            "in table");
                        break;

                    default:
                        fatal_error("Invalid tag value in table_any");
                        break;
                }
            } else {
                if (MR_TYPELAYOUT_ENUM_VECTOR_IS_ENUM(entry_value)) {
                    Word functors = 
                        MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(entry_value);
                    table = (Word**) MR_TABLE_ENUM(table, functors, data);
                } else {
                    Word functors = 
                        MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(entry_value);
                    table = (Word**) MR_TABLE_TAG(table, data_tag);
                    table = (Word**) MR_TABLE_ENUM(table, functors, 
                        (Word) data_value);
                }
            }
            break;

        case TYPELAYOUT_SIMPLE_TAG: 

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

        case TYPELAYOUT_COMPLICATED_TAG:
        {
            Word secondary_tag;
            Word num_sharers;
            Word *new_entry;

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
        case TYPELAYOUT_EQUIV_TAG:
            /* note: we treat no_tag types just like equivalences */
            if ((Word) entry_value < TYPELAYOUT_MAX_VARINT) {
                table = (Word**) MR_TABLE_ANY(table,  
                    (Word *) type_info[(Word) entry_value], data);
            } else {
            /*
            ** offset 0 is no-tag indicator
            ** offset 1 is the pseudo-typeinfo
            ** (as per comments in base_type_layout.m)
            ** XXX should avoid use of hard-coded offset `1' here
            */
                new_type_info = MR_make_type_info(type_info, 
                    (Word *) entry_value[1], &allocated_memory_cells);
                
                table = (Word**) MR_TABLE_ANY(table, new_type_info, data);
            }
            break;

        default:
            fatal_error("Unknown layout tag in table_any");
            break;
    }

    MR_deallocate(allocated_memory_cells);    
    
    return table;
} /* end table_any() */

static Word 
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

