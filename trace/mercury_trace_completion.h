// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2006 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_completion.h
//
// Command line completion for mdb.

#ifndef MR_TRACE_COMPLETION_H
#define MR_TRACE_COMPLETION_H

#include "mercury_std.h"    // for MR_bool
#include <stdlib.h>         // for size_t

// Called by Readline when the user requests completion.
// Examine Readline's input buffer to work out which completers
// should be used, then apply them.
// Readline passes zero for `state' on the first call, and non-zero
// on subsequent calls.

extern  char    *MR_trace_line_completer(const char *word, int state);

// A MR_Completer is called multiple times with the partial word
// to complete, the length of the word and some completer-specific data.
// The completer returns either the next possible completion (which
// must be allocated with MR_malloc), or NULL if there are no more
// completions.

typedef void    *MR_CompleterData;
typedef char    *(*MR_Completer)(const char *word, size_t word_len,
                    MR_CompleterData *data);

// Release the memory held by the completer data.
typedef void    (*MR_FreeCompleterData)(MR_CompleterData data);

typedef struct MR_CompleterList_Struct {
    MR_Completer                    MR_completer_func;
    MR_CompleterData                MR_completer_func_data;
    MR_FreeCompleterData            MR_free_completer_func_data;
    struct MR_CompleterList_Struct  *MR_completer_list_next;
} MR_CompleterList;

typedef MR_CompleterList    *(* MR_MakeCompleter)(const char *word,
                                size_t word_len);

// No completions.
extern  MR_CompleterList    *MR_trace_null_completer(const char *word,
                                size_t word_len);

// Complete on either a procedure specification, or filename:linenumber.
extern  MR_CompleterList    *MR_trace_break_completer(const char *word,
                                size_t word_len);

// Use Readline's filename completer.
extern  MR_CompleterList    *MR_trace_filename_completer(const char *word,
                                size_t word_len);

// Construct a MR_CompleterList with the given arguments.
// The MR_completer_list_next field of the structure will be NULL.

extern  MR_CompleterList    *MR_new_completer_elem(MR_Completer completer,
                                MR_CompleterData data,
                                MR_FreeCompleterData free_data);

// Used where the completer data is not malloc'ed.
extern  void                MR_trace_no_free(MR_CompleterData);

// Complete on the labels of the elements of a sorted array.
// A function of type MR_GetSlotName is used to get the label of the
// element at the given index in the array.

typedef char                *(*MR_GetSlotName)(int slot);

extern  MR_CompleterList    *MR_trace_sorted_array_completer(const char *word,
                                size_t word_length, int array_size,
                                MR_GetSlotName get_slot_name);

// Apply a filter to the output of a completer.
// Functions of type MR_CompleterFilter return MR_TRUE if the given
// string should be included in the list of completions.

typedef MR_bool            (*MR_CompleterFilter)(const char *completion,
                                MR_CompleterData *data);

extern MR_CompleterList     *MR_trace_filter_completer(
                                MR_CompleterFilter filter,
                                MR_CompleterData data,
                                MR_FreeCompleterData free_data,
                                MR_CompleterList *list);

// Apply a mapping function to the output of a completer.
// The MR_Completer_Map function may destructively update its input
// string, and must MR_free it if it is not returned as the result.

typedef char                *(*MR_Completer_Map)(char *completion,
                                MR_CompleterData *data);
extern MR_CompleterList     *MR_trace_map_completer(MR_Completer_Map map_func,
                                MR_CompleterData data,
                                MR_FreeCompleterData free_data,
                                MR_CompleterList *list);

#endif // MR_TRACE_COMPLETION_H
