/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_completion.h
**
** Command line completion for mdb.
*/

#ifndef MR_TRACE_COMPLETION_H
#define MR_TRACE_COMPLETION_H

#include "mercury_std.h"	/* for MR_bool */
#include <stdlib.h>		/* for size_t */

/*
** Called by Readline when the user requests completion.
** Examine Readline's input buffer to work out which completers
** should be used, then apply them.
** Readline passes zero for `state' on the first call, and non-zero
** on subsequent calls.
*/
extern	char	*MR_trace_line_completer(const char *word, int state);

/*
** A MR_Completer is called multiple times with the partial word
** to complete, the length of the word and some completer-specific data.
** The completer returns either the next possible completion (which
** must be allocated with MR_malloc), or NULL if there are no more
** completions.
*/
typedef void *MR_Completer_Data;
typedef char	*(*MR_Completer)(const char *word, size_t word_len,
					MR_Completer_Data *data);

/* Release the memory held by the completer data. */
typedef void	(*MR_Free_Completer_Data)(MR_Completer_Data data);

typedef struct MR_Completer_List_Struct {
	MR_Completer			MR_completer_func;
	MR_Completer_Data		MR_completer_func_data;
	MR_Free_Completer_Data		MR_free_completer_func_data;
	struct MR_Completer_List_Struct	*MR_completer_list_next;
} MR_Completer_List;

typedef MR_Completer_List *(*MR_Make_Completer)(const char *word,
							size_t word_len);
/* No completions. */
extern	MR_Completer_List *MR_trace_null_completer(const char *word,
				size_t word_len);

/* Use Readline's filename completer. */
extern	MR_Completer_List *MR_trace_filename_completer(const char *word,
				size_t word_len);

/*
** Construct a MR_Completer_List with the given arguments.
** The MR_completer_list_next field of the structure will be NULL.
*/
extern	MR_Completer_List *MR_new_completer_elem(MR_Completer completer,
                		MR_Completer_Data data,
				MR_Free_Completer_Data free_data);

/* Used where the completer data is not malloc'ed. */
extern	void	MR_trace_no_free(MR_Completer_Data);

/*
** Complete on the labels of the elements of a sorted array.
** A function of type MR_Get_Slot_Name is used to get the label of the
** element at the given index in the array.
*/
typedef char *(*MR_Get_Slot_Name)(int slot);
extern	MR_Completer_List *MR_trace_sorted_array_completer(const char *word,
				size_t word_length, int array_size,
				MR_Get_Slot_Name get_slot_name);

/*
** Apply a filter to the output of a completer.
** Functions of type MR_Completer_Filter return MR_TRUE if the given
** string should be included in the list of completions.
*/
typedef MR_bool (*MR_Completer_Filter)(const char *completion,
				MR_Completer_Data *data);
extern MR_Completer_List *MR_trace_filter_completer(MR_Completer_Filter filter,
				MR_Completer_Data data,
				MR_Free_Completer_Data free_data,
				MR_Completer_List *list);

/*
** Apply a mapping function to the output of a completer.
** The MR_Completer_Map function may destructively update its input
** string, and must MR_free it if it is not returned as the result.
*/
typedef char *(*MR_Completer_Map)(char *completion, MR_Completer_Data *data);
extern MR_Completer_List *MR_trace_map_completer(MR_Completer_Map map_func,
				MR_Completer_Data data,
				MR_Free_Completer_Data free_data,
				MR_Completer_List *list);

#endif /* MR_TRACE_COMPLETION_H */
