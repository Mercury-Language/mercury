/*---------------------------------------------------------------------------*/

/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** File: mdemangle.c
** Author: fjh
**
** A mercury symbol demangler.
** This is used to convert error messages from the linker back
** into a form that users can understand.
**
** This is implemented in C to minimize startup time and memory usage.
**
** BEWARE:
** This code is duplicated in profiler/demangle.m and profiler/mdemangle.m.
** Any changes here will need to be duplicated there and vice versa.
*/

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "mercury_std.h"

static void demangle(char *name);
static const char *strip_module_name(char **start_ptr, char *end,
		const char *trailing_context[]);
static bool check_for_suffix(char *start, char *position, const char *suffix,
		int sizeof_suffix, int *mode_num2);
static char *fix_mangled_ascii(char *str, char **end);
static bool fix_mangled_special_case(char *str, char **end);
static bool cut_at_double_underscore(char **str, char *end);
static bool cut_trailing_integer(char *str, char **end, int *num);
static bool cut_trailing_underscore_integer(char *str, char **end, int *num);
static bool strip_prefix(char **str, const char *prefix);

/*
** Bloody SunOS 4.x doesn't have memmove()...
** Using memcpy() may not work, but it doesn't really matter
** if the demangler doesn't work 100% correctly on SunOS 4.x.
*/
#ifndef HAVE_MEMMOVE
#define memmove memcpy
#endif

int 
main(int argc, char **argv)
{
	if (argc > 1) {
		/*
		** invoke demangle() on each command line argument
		*/
		int i;
		for (i = 1; i < argc; i++) {
			demangle(argv[i]);
			putchar('\n');
		}
	} else {
		/*
		** copy stdin to stdout, calling demangle() for
		** every valid C identifier in the input
		*/
		for (;;) {
			char buf[1000];
			size_t len = 0;
			int c = getchar();
			while (c != EOF && (isalnum(c) || c == '_')) {
				if (len >= sizeof(buf) - 1)
					break;
				buf[len++] = c;
				c = getchar();
			}
			if (len > 0) {
				buf[len] = '\0'; 
				demangle(buf);
				fflush(stdout);
			}
			if (c == EOF)
				break;
			putchar(c);
		}
	}
	return 0;
}

/*
** demangle() - convert a mangled Mercury identifier into 
** human-readable form and then print it to stdout
*/

static void 
demangle(char *name)
{
	static const char entry[]   = "_entry_";
	static const char mercury[] = "mercury__";
	static const char func_prefix[] = "fn__"; /* added for functions */
	static const char unify[]   = "__Unify___";
	static const char compare[] = "__Compare___";
	static const char mindex[]  = "__Index___";
	/* we call it `mindex' rather than `index' to
	   avoid a naming conflict with strchr's alter ego index() */

	static const char introduced[]  = "IntroducedFrom__";
	static const char deforestation[]  = "DeforestationIn__";
	static const char pred[]  = "pred__";
	static const char func[]  = "func__";

	static const char ua_suffix[] = "__ua"; /* added by unused_args.m */
	static const char ua_suffix2[] = "__uab"; /* added by unused_args.m */
	static const char ho_suffix[] = "__ho"; /* added by higher_order.m */

	static const char mercury_data[] = "mercury_data_";
	static const char base_type_layout[] = "base_type_layout_";
	static const char base_type_info[] = "base_type_info_";
	static const char base_type_functors[] = "base_type_functors_";
	static const char common[] = "common";

	static const char * trailing_context_1[] = {
		introduced,
		deforestation,
		NULL
	};

	static const char * trailing_context_2[] = {
		base_type_layout,
		base_type_info,
		base_type_functors,
		common,
		NULL
	};

	char *start = name;
	const char *module = "";	/* module name */
	char *end = name + strlen(name);
	char *position;		/* current position in string */
	int mode_num;
	int mode_num2;
	int arity;
	const char *pred_or_func; /* either "predicate" or "function" */
	bool unused_args = FALSE; /* does this proc have any unused arguments */
	bool higher_order = FALSE; /* has this proc been specialized */
	int internal = -1;
	int lambda_line = 0;
	int lambda_seq_number = 0;
	char *lambda_pred_name = NULL;
	const char *lambda_kind = NULL;
	enum { ORDINARY, UNIFY, COMPARE, INDEX, LAMBDA, DEFORESTATION }
		category;
	enum { COMMON, INFO, LAYOUT, FUNCTORS } data_category;

	/*
	** skip any leading underscore inserted by the C compiler
	** (but don't skip it if it came from the `_entry_' prefix)
	*/
	if (*start == '_' && strncmp(start, entry, strlen(entry)) != 0) {
		start++;
	}

	/*
	** skip the `_entry_' prefix, if any
	*/
	strip_prefix(&start, entry);

	/*
	** strip off the `mercury__' prefix
	*/

	if (!strip_prefix(&start, mercury)) {
		goto not_plain_mercury;
	}

/*
** Code for dealing with predicate symbols.
*/

	/*
	** strip off the `fn__' prefix, if any
	*/
	if (strip_prefix(&start, func_prefix)) {
		pred_or_func = "function";
	} else {
		pred_or_func = "predicate";
	}

	/*
	** Get integer from end of string (it might be the mode number,
	** it might be the internal label number). We'll assume its mode
	** number for the moment.
	*/

	if (!cut_trailing_integer(start, &end, &mode_num)) {
		goto wrong_format;
	}

	if (end == start) goto wrong_format;

	/*
	** if we got to an `i', that means it is an internal
	** label of the form `mercury__append_3_0_i1'
	** in that case, save the internal label number and then
	** get the mode number
	*/
	if (*--end == 'i') {
		internal = mode_num;
		if (end == start || *--end != '_') goto wrong_format;

		if (!cut_trailing_underscore_integer(start, &end, &mode_num)) {
			goto wrong_format;
		}
	}

	/*
	** scan back past the arity number and then parse it
	*/

	if (!cut_trailing_underscore_integer(start, &end, &arity)) {
		goto wrong_format;
	}

	/*
	** Now start processing from the start of the string again.
	** Check whether the start of the string matches the name of
	** one of the special compiler-generated predicates; if so,
	** set the `category' to the appropriate value and then
	** skip past the prefix.
	*/

	if (strip_prefix(&start, unify)) {
		category = UNIFY;
	} else if (strip_prefix(&start, compare)) {
		category = COMPARE;
		if (mode_num != 0) goto wrong_format;
	} else if (strip_prefix(&start, mindex)) {
		category = INDEX;
		if (mode_num != 0) goto wrong_format;
	} else {
		category = ORDINARY;
	}

	/*
	** Fix any ascii codes mangled in the predicate name
	*/
	start = fix_mangled_ascii(start, &end);

	/*
	** Process the mangling introduced by unused_args.m.
	** This involves stripping off the `__ua<m>' or `__uab<m>' added to 
	** the end of the predicate/function name, where m is the mode number.
	*/ 

	position = end;	/* save end of name */		

	do {
		if (position == start) goto wrong_format;
		position--;
	} while (isdigit((unsigned char)*position));
		/* get the mode number */
	
	if (check_for_suffix(start, position, ua_suffix,
			sizeof(ua_suffix), &mode_num2)) {
		unused_args = TRUE;
		end = position + 1 - (sizeof(ua_suffix) - 1);
		mode_num = mode_num2 % 10000;
	} else if (check_for_suffix(start, position, ua_suffix2,
			sizeof(ua_suffix2), &mode_num2)) {
		unused_args = TRUE;
		end = position + 1 - (sizeof(ua_suffix2) - 1);
		mode_num = mode_num2 % 10000;
	}

	/*
	** Process the mangling introduced by higher_order.m.
	** This involves stripping off the `__ho<n>' where
	** n is a unique identifier for this specialized version
	*/

	position = end;

	do {
		if (position == start) goto wrong_format;
		position--;
	} while (isdigit((unsigned char)*position));
	if (check_for_suffix(start, position, ho_suffix,
			sizeof(ho_suffix), &mode_num2)) {
		end = position + 1 - (sizeof(ho_suffix) - 1);
		higher_order = TRUE;
	}

	/*
	** Cut off the string before the start of the arity number,
	** and the unused_args and specialization information,
	** i.e. at the end of the predicate name or type name.
	*/
	*end = '\0';

	/*
	** Make sure special predicates with unused_args 
	** are reported correctly.
	*/

	if (unused_args && category != ORDINARY) {
		if (!cut_trailing_integer(start, &end, &arity)) {
			goto wrong_format;
		}
	}

	module = strip_module_name(&start, end, trailing_context_1);

	/*
	** look for "IntroducedFrom" or "DeforestationIn"
	*/
	if (category == ORDINARY) {
		if (strip_prefix(&start, introduced)) {
			category = LAMBDA;
		} else if (strip_prefix(&start, deforestation)) {
			category = DEFORESTATION;
		}
	}

	if (category == LAMBDA || category == DEFORESTATION) {
		if (strip_prefix(&start, pred)) {
			lambda_kind = "pred";
		} else if (strip_prefix(&start, func)) {
			lambda_kind = "func";
		} else {
			goto wrong_format;
		}
		lambda_pred_name = start;
		if (!cut_at_double_underscore(&start, end)) {
			goto wrong_format;
		}
		lambda_line = 0;
		while (start < end && MR_isdigit(*start)) {
			lambda_line = lambda_line * 10 + (*start - '0');
			start++;
		}
		if (!cut_at_double_underscore(&start, end)) {
			goto wrong_format;
		}
		lambda_seq_number = 0;
		while (start < end && MR_isdigit(*start)) {
			lambda_seq_number = lambda_seq_number * 10 +
				(*start - '0');
			start++;
		}
	}

	/*
	** Now, finally, we can print the demangled symbol name
	*/
	printf("<");
	switch(category) {
	case UNIFY:
		printf("unification predicate for type '%s:%s'/%d mode %d",
			module, start, arity, mode_num);
		break;
	case COMPARE:
		printf("compare/3 predicate for type '%s:%s'/%d",
			module, start, arity);
		break;
	case INDEX:
		printf("index/2 predicate for type '%s:%s'/%d",
			module, start, arity);
		break;
	case LAMBDA:
		printf("%s goal (#%d) from '%s' in module '%s' line %d",
			lambda_kind, lambda_seq_number,
			lambda_pred_name, module, lambda_line);
		break;
	case DEFORESTATION:
		printf("deforestation procedure (#%d) from '%s' in module '%s' line %d",
			lambda_seq_number, lambda_pred_name,
			module, lambda_line);
		break;
	default:
		if (*module == '\0') {
			printf("%s '%s'/%d mode %d",
				pred_or_func, start, arity, mode_num);
		} else {
			printf("%s '%s:%s'/%d mode %d",
				pred_or_func, module, start, arity, mode_num);
		}
	}
	if (higher_order) {
		printf(" (specialized)");
	}
	if (unused_args) {
		printf(" (minus unused args)");
	}
	if (internal != -1) {
		printf(" label %d", internal);
	}
	printf(">");
	return;

/* 
** Code to deal with mercury_data items.
*/

not_plain_mercury:

	if (!strip_prefix(&start, mercury_data)) {
		goto wrong_format;
	}

	module = strip_module_name(&start, end, trailing_context_2);

	if (strip_prefix(&start, base_type_info)) {
		data_category = INFO;
		if (!cut_trailing_underscore_integer(start, &end, &arity)) {
			goto wrong_format;
		}
	} else if (strip_prefix(&start, base_type_layout)) {
		data_category = LAYOUT;
		if (!cut_trailing_underscore_integer(start, &end, &arity)) {
			goto wrong_format;
		}
	} else if (strip_prefix(&start, base_type_functors)) {
		data_category = FUNCTORS;
		if (!cut_trailing_underscore_integer(start, &end, &arity)) {
			goto wrong_format;
		}
	} else if (strip_prefix(&start, common)) {
		data_category = COMMON;
		if (!cut_trailing_underscore_integer(start, &end, &arity)) {
			goto wrong_format;
		}
	} else {
		goto wrong_format;
	}

	start = fix_mangled_ascii(start, &end);

	switch (data_category) {

	case INFO:
		if (*module == '\0') {
			printf("<base type_info for type '%s'/%d>",
				start, arity);
		} else {
			printf("<base type_info for type '%s:%s'/%d>",
				module, start, arity);
		}
		break;
	case LAYOUT:
		if (*module == '\0') {
			printf("<type layout for type '%s'/%d>",
				start, arity);
		} else {
			printf("<type layout for type '%s:%s'/%d>",
				module, start, arity);
		}
		break;
	case FUNCTORS:
		if (*module == '\0') {
			printf("<type functors for type '%s'/%d>",
				start, arity);
		} else {
			printf("<type functors for type '%s:%s'/%d>",
				module, start, arity);
		}
		break;
	case COMMON:
		printf("<shared constant number %d for module %s>",
			arity, module);
		break;

	default:
		goto wrong_format;

	}
	return;

wrong_format:
	printf("%s", name);
	return;
} /* end demangle() */

	/*
	** Remove a module name prefix.
	** Just keep munching up double-underscores until we
	** get to something that matches the specified trailing context,
	** at which point we stop, or until there are no double-underscores
	** left.
	*/
static const char *
strip_module_name(char **start_ptr, char *end, const char *trailing_context[])
{
	const char *module;		/* module name */
	char *module_end;		/* end of the module name */
	char *next_double_underscore;
	char *start;

	start = *start_ptr;

	/*
	** Strip off the module name
	*/
	module = start;
	module_end = start;
	while ((next_double_underscore = strstr(start, "__")) != NULL) {
		int len, i;

		/*
		** Check for special cases
		*/
		bool stop = FALSE;
		for (i = 0; trailing_context[i] != NULL; i++) {
			if (strncmp(start,
				trailing_context[i],
				strlen(trailing_context[i])) == 0)
			{
				stop = TRUE;
			}
		}
		if (stop) break;

		len = next_double_underscore - start;
		if (module != module_end) {
			/*
			** append a module qualifier, and
			** shift the module name into the right place
			*/
			*module_end = ':';
			module_end++;
			memmove(module_end, start, len);
		}
		module_end += len;

		start = next_double_underscore + 2;
	}
	if (module == module_end) {
		module = "";
	} else {
		*module_end = '\0';
	}

	*start_ptr = start;
	return module;
}

	/*
	** Remove the prefix from a string, if it has 
	** it. 
	** Returns TRUE if it has that prefix, and newstr will
	** then point to the rest of that string.
	** If the string doesn't have that prefix, newstr will
	** be unchanged, and the function will return FALSE.
	*/
static bool 
strip_prefix(char **str, const char *prefix) 
{
	int len;

	len = strlen(prefix);

	if (strncmp(*str, prefix, len) == 0) {
		*str += len;
		return TRUE;
	}
	return FALSE;
}

	/*
	** Remove trailing integer (at the supplied `real_end' of the
	** string), and return it in the int pointed to by `num'.   True
	** is returned if there is an integer at the end, false if not.
	** If false is returned, the string will not be cut.
	** `real_end' is updated with the new end of the string
	*/
static bool 
cut_trailing_integer(char *str, char **real_end, int *num) 
{
	char *end = *real_end;

	do { 
		if (end == str) return FALSE;
		end--;
	} while (isdigit((unsigned char)*end));

	if (sscanf(end + 1, "%d", num) != 1) {
		return FALSE;
	}
	*++end = '\0';
	*real_end = end;

	return TRUE;
}

	/*
	** Same as cut_trailing_integer, but move end back past
	** the underscore as well. If cut_trailing_underscore_integer
	** returns TRUE, the `real_end' will be moved back before the
	** underscore and the integer. If it returns FALSE, the
	** `real_end' is unchanged.
	*/
static bool 
cut_trailing_underscore_integer(char *str, char **real_end, 
	int *num) 
{
	char *end = *real_end;

	if (!cut_trailing_integer(str, &end, num)) {
		return FALSE;
	}
	if (end == str || *(--end) != '_') {
		return FALSE;
	}
	*end = '\0';
	*real_end = end;
	return TRUE;
}

	/*
	** Scan for `__' and cut the string at there (replace first
	** `_' with `\0', return the part of the string after the `__').
	** Returns TRUE if `__' was found, FALSE otherwise.
	*/

static bool
cut_at_double_underscore(char **start, char *end) 
{
	char *str = *start;

	while (*str != '_' || *(str + 1) != '_') {
		if (str == end) {
			return FALSE;
		}
		str++;
	}

	*str = '\0';
	*start = str + 2;
	return TRUE;
}

	/*
	** The compiler changes all names starting with `f_' so that
	** they start with `f__' instead, and uses names starting with
	** `f_' for mangled names which are either descriptions (such
	** as `f_greater_than' for `>') or sequences of decimal
	** reprententations of ASCII codes separated by underscores.
	** If the name starts with `f__', we must change it back to
	** start with `f_'.  Otherwise, if it starts with `f_' we must
	** convert the mnemonic or list of ASCII codes back into an
	** identifier.
	*/

static char *
fix_mangled_ascii(char *str, char **real_end)
{
	char *end = *real_end;

	/*
	** If it starts with `f__', replace that with `f_'.
	*/
	if (strncmp(str, "f__" , 3) == 0) {
		str++;
		*str = 'f';
		return str;
	}

	/*
	** If it starts with `f_' followed by a mnemonic description,
	** then replace that with its unmangled version
	*/
	if (strncmp(str, "f_", 2) == 0 &&
		fix_mangled_special_case(str, real_end))
	{
		return str;
	}

	/*
	** Otherwise, if it starts with `f_' we must convert the list of
	** ASCII codes back into an identifier.
	*/
	if (strncmp(str, "f_", 2) == 0) {
		char buf[1000];
		char *num = str + 2;
		int count = 0;
		while (num < end) {
			char *next_num = num;
			while (isdigit((unsigned char)*next_num)) {
				next_num++;
			}
			if (*next_num != '_' && *next_num != '\0') 
				break;
			*next_num = '\0';
			buf[count++] = atoi(num);
			num = next_num + 1;
		}
			/* copy anything after the mangled string */
		while (num < end) {
			buf[count++] = *num++;
		}
		buf[count] = '\0';
		strcpy(str, buf);
		*real_end = str + count;
	}
	return str;
}

static bool
fix_mangled_special_case(char *str, char **real_end)
{
	static const struct {
		const char *mangled_name;
		const char *unmangled_name;
	} translations[] = {
		/*
		** Beware: we assume that the unmangled name is always
		** shorter than the mangled name.
		*/
		{ "f_not_equal", "\\=" },
		{ "f_greater_or_equal", ">=" },
		{ "f_less_or_equal", "=<" },
		{ "f_equal", "=" },
		{ "f_less_than", "<" },
		{ "f_greater_than", ">" },
		{ "f_plus", "+" },
		{ "f_times", "*" },
		{ "f_minus", "-" },
		{ "f_slash", "/" },
		{ "f_comma", "," },
		{ "f_semicolon", ";" },
		{ "f_cut", "!" }
	};
	const int num_translations =
		sizeof(translations) / sizeof(translations[0]);

	int i;

	/*
	** check for the special cases listed in the table above.
	*/
	for (i = 0; i < num_translations; i++) {
		const char *mangled = translations[i].mangled_name;
		size_t mangled_len = strlen(mangled);
		if (strncmp(str, mangled, mangled_len) == 0) {
			const char *unmangled = translations[i].unmangled_name;
			size_t unmangled_len = strlen(unmangled);
			size_t leftover_len = strlen(str) - mangled_len;

			assert(unmangled_len <= mangled_len);

			strcpy(str, unmangled);
			memmove(str + unmangled_len, str + mangled_len,
				leftover_len + 1);

			*real_end = str + unmangled_len + leftover_len;
			return TRUE;
		}
	}
	return FALSE;
}

static bool 
check_for_suffix(char *start, char *position, const char *suffix,
		int sizeof_suffix, int *mode_num2)
{
	const int suffix_len = sizeof_suffix - 1;

	return (
		position - suffix_len >= start 
		&& sscanf(position + 1, "%d", mode_num2) == 1
		&& strncmp(position - suffix_len + 1, suffix, suffix_len) == 0
	);
}
	
/*---------------------------------------------------------------------------*/
