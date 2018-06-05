// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2005-2006 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_trace_completion.c
//
// Main author: stayl
//
// Command line completion for mdb.

#include "mercury_memory.h"
#include "mercury_std.h"
#include "mercury_array_macros.h"
#include "mercury_trace_completion.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_tables.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MR_NO_USE_READLINE
  #ifdef MR_HAVE_READLINE_READLINE
    #include <readline/readline.h>
  #else
    extern char     *rl_line_buffer;
    extern int      rl_point;
    extern char     *filename_completion_function(const char *word, int state);
  #endif
#endif

static  char                *MR_prepend_string(char *completion,
                                MR_CompleterData *data);

static  char                *MR_trace_completer_list_next(const char *word,
                                size_t word_len, MR_CompleterList **list);
static  void                MR_trace_free_completer_list(
                                MR_CompleterList *completer_list);

static  char                *MR_trace_sorted_array_completer_next(
                                const char *word, size_t word_length,
                                MR_CompleterData *data);

static  MR_CompleterList    *MR_trace_string_array_completer(
                                const char *const *strings);
static  char                *MR_trace_string_array_completer_next(
                                const char *word, size_t word_len,
                                MR_CompleterData *data);

static  int                 MR_compare_source_file_lines(
                                const void *ptr1, const void *ptr2);
static  void                MR_insert_module_into_source_file_line_table(
                                const MR_ModuleLayout *module);

static  char                *MR_trace_filename_completer_next(const char *word,
                                size_t word_len, MR_CompleterData *);

static  char                *MR_trace_filter_completer_next(const char *word,
                                size_t word_len, MR_CompleterData *);
static  void                MR_trace_free_filter_completer_data(
                                MR_CompleterData data);

static  char                *MR_trace_map_completer_next(const char *word,
                                size_t word_len, MR_CompleterData *);
static  void                MR_trace_free_map_completer_data(
                                MR_CompleterData data);

////////////////////////////////////////////////////////////////////////////
// Called by Readline when the user requests completion.
// Examine Readline's input buffer to work out which completers
// should be used, then apply them.
// Readline passes zero for `state' on the first call, and non-zero
// on subsequent calls.

char *
MR_trace_line_completer(const char *passed_word, int state)
{
#ifdef MR_NO_USE_READLINE
    return NULL;
#else
    static MR_CompleterList     *completer_list;
    static char                 *word;
    static size_t               word_len;
    char                        *completion;

    // If `state' is 0, this is the first call for this `word',
    // so set up the list of completers.

    if (state == 0) {
        char    *line;
        char    *command_end;
        char    *command_start;
        char    *insertion_point;
        char    *semicolon;

        MR_trace_free_completer_list(completer_list);
        completer_list = NULL;
        if (word != NULL) {
            MR_free(word);
        }

        line = rl_line_buffer;
        insertion_point = rl_line_buffer + rl_point;

        // There may be multiple commands in the line.
        // Skip to the one we are trying to complete.

        semicolon = strchr(line, ';');
        while (semicolon != NULL && semicolon < insertion_point) {
            line = semicolon + 1;
            semicolon = strchr(line, ';');
        }

        // Skip space or a number at the beginning of the command.
        while (line < insertion_point &&
            (MR_isspace(*line) || MR_isdigit(*line)))
        {
            line++;
        }

        // Find the end of the command.
        command_start = line;
        command_end = line;
        while (command_end < insertion_point && !MR_isspace(*command_end)) {
            command_end++;
        }

        if (command_end == insertion_point) {
            // We are completing the command itself.
            int                 num_digits;
            char                *digits;
            MR_CompleterList    *command_completer;
            MR_CompleterList    *alias_completer;

            // Strip off any number preceding the command
            // (it will need to be added back later).

            num_digits = 0;
            while (MR_isdigit(passed_word[num_digits])) {
                num_digits++;
            }
            word = MR_copy_string(passed_word + num_digits);
            word_len = strlen(word);

            // Set up completers for commands and aliases.
            command_completer = MR_trace_command_completer(word, word_len);
            alias_completer = MR_trace_alias_completer(word, word_len);

            completer_list = command_completer;
            completer_list->MR_completer_list_next = alias_completer;

            // Add back the preceding number to the completions.
            if (num_digits != 0) {
                digits = MR_malloc(num_digits + 1);
                strncpy(digits, passed_word, num_digits);
                digits[num_digits] = '\0';
                completer_list = MR_trace_map_completer(MR_prepend_string,
                    digits, MR_free_func, completer_list);
            }
        } else {
            // We are completing an argument of the command.
            #define MR_MAX_COMMAND_NAME_LEN 256
            char                command[MR_MAX_COMMAND_NAME_LEN];
            char                *expanded_command;
            int                 command_len;
            char                **words;
            int                 word_count;
            MR_MakeCompleter    command_completer;
            const char *const   *command_fixed_args;
            MR_CompleterList    *arg_completer;

            command_len = command_end - command_start;
            if (command_len >= MR_MAX_COMMAND_NAME_LEN) {
                // The string is too long to be a command.
                return NULL;
            } else {
                strncpy(command, command_start, command_len);
                command[command_len] = '\0';
            }

            // Expand aliases.
            if (MR_trace_lookup_alias(command, &words, &word_count)) {
                if (word_count == 0) {
                    return NULL;
                } else {
                    expanded_command = words[0];
                }
            } else {
                expanded_command = command;
            }

            if (! MR_trace_command_completion_info(expanded_command,
                &command_completer, &command_fixed_args))
            {
                return NULL;
            }

            // Set up a completer for the fixed argument strings.
            completer_list = NULL;
            if (command_fixed_args != NULL) {
                completer_list = MR_trace_string_array_completer(
                    command_fixed_args);
            }

            word = MR_copy_string(passed_word);
            word_len = strlen(word);

            // Set up a completer for the non-fixed argument strings.
            arg_completer = (*command_completer)(word, word_len);
            if (completer_list == NULL) {
                completer_list = arg_completer;
            } else {
                completer_list->MR_completer_list_next = arg_completer;
            }
        }
    }

    completion = MR_trace_completer_list_next(word, word_len, &completer_list);
    if (completion == NULL) {
        MR_trace_free_completer_list(completer_list);
        MR_free(word);
        word = NULL;
    }

    return completion;
#endif // ! MR_NO_USE_READLINE
}

static char *
MR_prepend_string(char *string, MR_CompleterData *data)
{
    char    *string_to_prepend;
    size_t  string_to_prepend_len;
    char    *result;

    string_to_prepend = (char *) *data;
    string_to_prepend_len = strlen(string_to_prepend);
    result = MR_malloc(string_to_prepend_len + strlen(string) + 1);
    strcpy(result, string_to_prepend);
    strcpy(result + string_to_prepend_len, string);
    MR_free(string);
    return result;
}

////////////////////////////////////////////////////////////////////////////

static char *
MR_trace_completer_list_next(const char *word, size_t word_len,
    MR_CompleterList **list)
{
    MR_CompleterList    *current_completer;
    char                *result;

    if (list == NULL) {
        return NULL;
    }

    while (1) {
        current_completer = *list;
        if (current_completer == NULL) {
            return NULL;
        }
        result = (current_completer->MR_completer_func)(word, word_len,
            &(current_completer->MR_completer_func_data));
        if (result != NULL) {
            return result;
        } else {
            *list = current_completer->MR_completer_list_next;
            (current_completer->MR_free_completer_func_data)(
                current_completer->MR_completer_func_data);
            MR_free(current_completer);
        }
    }
}

static void
MR_trace_free_completer_list(MR_CompleterList *completer_list)
{
    MR_CompleterList    *tmp_list;

    while (completer_list != NULL) {
        tmp_list = completer_list;
        completer_list = completer_list->MR_completer_list_next;
        (tmp_list->MR_free_completer_func_data)(
            tmp_list->MR_completer_func_data);
        MR_free(tmp_list);
    }
}

////////////////////////////////////////////////////////////////////////////
// No completions.

MR_CompleterList *
MR_trace_null_completer(const char *word, size_t word_len)
{
    return NULL;
}

////////////////////////////////////////////////////////////////////////////
// Complete on the labels of a sorted array.

typedef struct {
    MR_GetSlotName  MR_sorted_array_get_slot_name;
    int             MR_sorted_array_current_offset;
    int             MR_sorted_array_size;
} MR_SortedArrayCompleterData;

MR_CompleterList *
MR_trace_sorted_array_completer(const char *word, size_t word_length,
    int array_size, MR_GetSlotName get_slot_name)
{
    MR_CompleterList            *completer;
    MR_bool                     found;
    int                         slot;
    MR_SortedArrayCompleterData *data;

    // Find the slot containing the first possible match, optimizing for the
    // common case where we are finding all elements in the array.

    if (word_length == 0) {
        found = (array_size != 0);
        slot = 0;
    } else {
        MR_find_first_match(array_size, slot, found,
            strncmp(get_slot_name(slot), word, word_length));
    }

    if (found) {
        data = MR_NEW(MR_SortedArrayCompleterData);
        data->MR_sorted_array_get_slot_name = get_slot_name;
        data->MR_sorted_array_current_offset = slot;
        data->MR_sorted_array_size = array_size;
        completer = MR_new_completer_elem(MR_trace_sorted_array_completer_next,
            (MR_CompleterData) data, MR_free_func);
    } else {
        completer = NULL;
    }
    return completer;
}

static char *
MR_trace_sorted_array_completer_next(const char *word,
    size_t word_length, MR_CompleterData *completer_data)
{
    MR_SortedArrayCompleterData *data;
    char                        *completion;

    data = (MR_SortedArrayCompleterData *) *completer_data;

    if (data->MR_sorted_array_current_offset < data->MR_sorted_array_size) {
        completion = data->MR_sorted_array_get_slot_name(
            data->MR_sorted_array_current_offset);
        if (MR_strneq(completion, word, word_length)) {
            data->MR_sorted_array_current_offset++;
            return MR_copy_string(completion);
        } else {
            return NULL;
        }
    } else {
        return NULL;
    }
}

////////////////////////////////////////////////////////////////////////////
// Complete on the elements of an unsorted array of strings.

typedef struct MR_StringArrayCompleterData_struct {
    char    **MR_string_array;
    int     MR_string_array_current_offset;
} MR_StringArrayCompleterData;

// Complete on a NULL terminated array of strings.
// The strings will not be `free'd.

static MR_CompleterList *
MR_trace_string_array_completer(const char *const *strings)
{
    MR_StringArrayCompleterData *data;

    data = MR_NEW(MR_StringArrayCompleterData);
    data->MR_string_array = (char **) strings;
    data->MR_string_array_current_offset = 0;
    return MR_new_completer_elem(&MR_trace_string_array_completer_next,
        (MR_CompleterData) data, MR_free_func);
}

static char *
MR_trace_string_array_completer_next(const char *word, size_t word_len,
    MR_CompleterData *data)
{
    MR_StringArrayCompleterData *completer_data;
    char                        *result;

    completer_data = (MR_StringArrayCompleterData *) *data;

    while (1) {
        result = completer_data->MR_string_array[
            completer_data->MR_string_array_current_offset];
        completer_data->MR_string_array_current_offset++;
        if (result == NULL) {
            return NULL;
        } else {
            if (strncmp(result, word, word_len) == 0) {
                return MR_copy_string(result);
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////
// Complete on either a procedure specification, or a filename:linenumber pair.
//
// We construct the sorted array of filename:linenumber pairs by first
// adding all such pairs implied by the module layout structures of the Mercury
// modules that have debugging information to MR_source_file_lines,
// and then sorting that array. We cannot use the macros defined in
// mercury_array_util.h to keep the array sorted *during* the insertion
// process, because the array can gen very big (more than 150,000 entries
// for the Mercury compiler itself), and performing O(n) calls to
// MR_prepare_insert_into_sorted, whose complexity is itself O(n),
// would yield a quadratic algorithm.' We do the sorting using qsort,
// whose expected complexity is O(n log n). (I don't expect its worst case
// O(n^2) performance to ever occur in practice.)
//
// Since the MR_source_file_lines array is not sorted while we insert into it,
// we have no cheap way to search the entirety of the so-far filled in parts
// of the array to see whether the string we are about to add to it already
// occurs in it. However, we can stop most duplicates from ever getting into
// the array by simply checking if the string we are about to add is for
// the same line number in the same file as the previous string, and we do
// a final pass over the array after it is sorted, squeezing out the remaining
// duplicates, which are now guaranteed to be adjacent to each other.

#define INIT_SOURCE_FILE_LINE_TABLE_SIZE         10

static const char   **MR_source_file_lines;
static unsigned     MR_source_file_line_next = 0;
static unsigned     MR_source_file_line_max  = 0;
static MR_bool      MR_source_file_lines_initialized = MR_FALSE;

#define INIT_SOURCE_FILE_LINE_CHARS_SIZE    100
#define LINE_NUM_MAX_CHARS                  20

static char         *MR_source_file_line_chars;
static unsigned     MR_source_file_line_char_next = 0;
static unsigned     MR_source_file_line_char_max  = 0;

static void
MR_insert_module_into_source_file_line_table(const MR_ModuleLayout *module)
{
    int     num_files;
    int     cur_file;

    num_files = module->MR_ml_filename_count;
    for (cur_file = 0; cur_file < num_files; cur_file++) {
        const MR_ModuleFileLayout   *file;
        MR_ConstString              file_name;
        int                         file_name_len;
        int                         num_lines;
        int                         cur_line;

        file = module->MR_ml_module_file_layout[cur_file];
        file_name = file->MR_mfl_filename;
        file_name_len = strlen(file_name);

        MR_ensure_big_enough(file_name_len + LINE_NUM_MAX_CHARS + 2,
            MR_source_file_line_char, char,
            INIT_SOURCE_FILE_LINE_CHARS_SIZE);
        strcpy(MR_source_file_line_chars, file_name);
        MR_source_file_line_chars[file_name_len] = ':';

        num_lines = file->MR_mfl_label_count;
        // The +1 is for the sentinel.
        MR_ensure_big_enough(MR_source_file_line_next + num_lines + 1,
            MR_source_file_line, const char *,
            INIT_SOURCE_FILE_LINE_TABLE_SIZE);
        for (cur_line = 0; cur_line < num_lines; cur_line++) {
            int line_num = file->MR_mfl_label_lineno[cur_line];

            // Almost all duplicates that end up in MR_source_file_lines
            // get there from consecutive entries for the same line number
            // in the same module file layout structure. Look for this
            // relatively common case, and avoid adding the redundant entry
            // to the array if we find it.

            if (cur_line > 0 &&
                line_num == file->MR_mfl_label_lineno[cur_line - 1])
            {
                // The string we would add would be the same as the string
                // for the previous line number.
                continue;
            }

            snprintf(&MR_source_file_line_chars[file_name_len + 1],
                LINE_NUM_MAX_CHARS, "%d", line_num);
            MR_source_file_lines[MR_source_file_line_next] =
                strdup(MR_source_file_line_chars);
            MR_source_file_line_next++;
        }
    }
}

static int
MR_compare_source_file_lines(const void *ptr1, const void *ptr2)
{
    char * const    *sptr1 = (char * const *) ptr1;
    char * const    *sptr2 = (char * const *) ptr2;
    return strcmp(*sptr1, *sptr2);
}

MR_CompleterList *
MR_trace_break_completer(const char *word, size_t word_len)
{
    MR_CompleterList    *completer_list;
    MR_CompleterList    *cur_completer_list;

    completer_list = MR_trace_proc_spec_completer(word, word_len);

    if (MR_strneq(word, "pred*", 5) || MR_strneq(word, "func*", 5)) {
        // These prefixes say that the breakpoint is on a named procedure,
        // not on a filename/linenumber pair.

        return completer_list;
    }

    if (! MR_source_file_lines_initialized) {
        int module_num;
        int last;
        int i;

        for (module_num = 0; module_num < MR_module_info_next; module_num++) {
            MR_insert_module_into_source_file_line_table(
                MR_module_infos[module_num]);
        }

        qsort(MR_source_file_lines, MR_source_file_line_next,
            sizeof(const char *), MR_compare_source_file_lines);

        // Squeeze out any duplicate entries, which are now guaranteed
        // to be adjacent to each other.

        last = 0;
        for (i = 1; i < MR_source_file_line_next; i++) {
            if (MR_streq(MR_source_file_lines[i], MR_source_file_lines[last])) {
                free((void *) MR_source_file_lines[i]);
            } else {
                ++last;
                MR_source_file_lines[last] = MR_source_file_lines[i];
            }
        }

        // Add the NULL entry as the sentinel.
        ++last;
        MR_source_file_lines[last] = NULL;

        MR_source_file_line_next = last + 1;
        MR_source_file_lines_initialized = MR_TRUE;
    }

    cur_completer_list = completer_list;
    while (cur_completer_list->MR_completer_list_next != NULL) {
        cur_completer_list = cur_completer_list->MR_completer_list_next;
    }

    cur_completer_list->MR_completer_list_next =
        MR_trace_string_array_completer(MR_source_file_lines);

    return completer_list;
}

////////////////////////////////////////////////////////////////////////////
// Use Readline's filename completer.

MR_CompleterList *
MR_trace_filename_completer(const char *word, size_t word_len)
{
    return MR_new_completer_elem(&MR_trace_filename_completer_next,
        (MR_CompleterData) 0, MR_trace_no_free);
}

static char *
MR_trace_filename_completer_next(const char *word, size_t word_len,
    MR_CompleterData *data)
{
#ifdef MR_NO_USE_READLINE
    return NULL;
#else
    MR_Integer  state;

    state = (MR_Integer) *data;
    *data = (MR_CompleterData) 1;
    return filename_completion_function((char *) word, (int) state);
#endif // ! MR_NO_USE_READLINE
}

////////////////////////////////////////////////////////////////////////////
// Apply a filter to the output of a completer.

typedef struct {
    MR_CompleterFilter      MR_filter_func;
    MR_CompleterData        MR_filter_data;
    MR_FreeCompleterData    MR_filter_free_data;
    MR_CompleterList        *MR_filter_list;
} MR_Filter_Completer_Data;

MR_CompleterList *
MR_trace_filter_completer(MR_CompleterFilter filter,
    MR_CompleterData filter_data, MR_FreeCompleterData free_filter_data,
    MR_CompleterList *list)
{
    MR_Filter_Completer_Data    *data;

    data = MR_NEW(MR_Filter_Completer_Data);
    data->MR_filter_func = filter;
    data->MR_filter_data = filter_data;
    data->MR_filter_free_data = free_filter_data;
    data->MR_filter_list = list;
    return MR_new_completer_elem(MR_trace_filter_completer_next,
        (MR_CompleterData) data, MR_trace_free_filter_completer_data);
}

static char *
MR_trace_filter_completer_next(const char *word, size_t word_len,
    MR_CompleterData *completer_data)
{
    MR_Filter_Completer_Data    *data;
    char                        *completion;

    data = (MR_Filter_Completer_Data *) *completer_data;
    while (1) {
        completion = MR_trace_completer_list_next(word, word_len,
            &data->MR_filter_list);
        if (completion == NULL) {
            return NULL;
        } else if (data->MR_filter_func(completion, &(data->MR_filter_data))) {
            return completion;
        } else {
            MR_free(completion);
        }
    }
}

static void
MR_trace_free_filter_completer_data(MR_CompleterData completer_data)
{
    MR_Filter_Completer_Data    *data;

    data = (MR_Filter_Completer_Data *) completer_data;
    data->MR_filter_free_data(data->MR_filter_data);
    MR_trace_free_completer_list(data->MR_filter_list);
    MR_free(data);
}

////////////////////////////////////////////////////////////////////////////
// Apply a mapping function to the output of a completer.

typedef struct {
    MR_Completer_Map        MR_map_func;
    MR_CompleterData        MR_map_data;
    MR_FreeCompleterData    MR_map_free_data;
    MR_CompleterList        *MR_map_list;
} MR_MapCompleterData;

MR_CompleterList *
MR_trace_map_completer(MR_Completer_Map map, MR_CompleterData map_data,
    MR_FreeCompleterData free_data, MR_CompleterList *list)
{
    MR_MapCompleterData *data;

    data = MR_NEW(MR_MapCompleterData);
    data->MR_map_func = map;
    data->MR_map_data = map_data;
    data->MR_map_free_data = free_data;
    data->MR_map_list = list;
    return MR_new_completer_elem(MR_trace_map_completer_next,
        (MR_CompleterData) data, MR_trace_free_map_completer_data);
}

static char *
MR_trace_map_completer_next(const char *word, size_t word_len,
    MR_CompleterData *completer_data)
{
    MR_MapCompleterData *data;
    char                *completion;

    data = (MR_MapCompleterData *) *completer_data;
    completion = MR_trace_completer_list_next(word, word_len,
        &data->MR_map_list);
    if (completion == NULL) {
        return NULL;
    } else {
        return data->MR_map_func(completion, &(data->MR_map_data));
    }
}

static void
MR_trace_free_map_completer_data(MR_CompleterData completer_data)
{
    MR_MapCompleterData *data;

    data = (MR_MapCompleterData *) completer_data;
    data->MR_map_free_data(data->MR_map_data);
    MR_trace_free_completer_list(data->MR_map_list);
    MR_free(data);
}

////////////////////////////////////////////////////////////////////////////

MR_CompleterList *
MR_new_completer_elem(MR_Completer completer, MR_CompleterData data,
    MR_FreeCompleterData free_data)
{
    MR_CompleterList    *result;

    result = MR_NEW(MR_CompleterList);
    result->MR_completer_func = completer;
    result->MR_completer_func_data = data;
    result->MR_free_completer_func_data = free_data;
    result->MR_completer_list_next = NULL;
    return result;
}

void
MR_trace_no_free(MR_CompleterData data)
{
}
