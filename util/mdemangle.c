/*---------------------------------------------------------------------------*/

/*
** Copyright (C) 1995 University of Melbourne.
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
*/

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "std.h"

static void demangle(char *name);

int main(int argc, char **argv)
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
			int len = 0;
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
**
** Sorry, the following code is awful.
** It ought to be rewritten in a language with
** better string-handling facilities than C!
*/

static void demangle(char *name) {
	static const char entry[]   = "entry_";
	static const char mercury[] = "mercury__";
	static const char func_prefix[] = "fn__"; /* added for functions */
	static const char unify[]   = "__Unify___";
	static const char compare[] = "__Compare___";
	static const char mindex[]  = "__Index___";
	/* we call it `mindex' rather than `index' to
	   avoid a naming conflict with strchr's alter ego index() */

	static const char ua_suffix[] = "__ua"; /* added by unused_args.m */
	static const char ho_suffix[] = "__ho"; /* added by higher_order.m */

	char *start = name;
	char *module = NULL;	/* module name of type for special pred */
	char *end = name + strlen(name);
	char *position;		/* current position in string */
	int mode_num;
	int mode_num2;
	int arity;
	const char *pred_or_func; /* either "predicate" or "function" */
	bool unused_args = FALSE; /* does this proc have any unused arguments */
	bool higher_order = FALSE; /* has this proc been specialized */
	int internal = -1;
	enum { ORDINARY, UNIFY, COMPARE, INDEX } category;

	/*
	** skip any leading underscore inserted by the C compiler
	*/
	if (*start == '_') start++;

	/*
	** skip the `entry_' prefix, if any
	*/
	if (strncmp(start, entry, sizeof(entry) - 1) == 0) {
		start += sizeof(entry) - 1;
	}

	/*
	** strip off the `mercury__' prefix
	*/
	if (strncmp(start, mercury, sizeof(mercury) - 1) == 0) {
		start += sizeof(mercury) - 1;
	} else {
		goto wrong_format;
	}

	/*
	** strip off the `fn__' prefix, if any
	*/
	if (strncmp(start, func_prefix, sizeof(func_prefix) - 1) == 0) {
		start += sizeof(func_prefix) - 1;
		pred_or_func = "function";
	} else {
		pred_or_func = "predicate";
	}

	/*
	** now start working from the end of the string
	** scan backwards past the number at the end
	*/
	do {
		if (end == start) goto wrong_format;
		end--;
	} while (isdigit((unsigned char)*end));

	/*
	** if we got to an `i', that means it is an internal
	** label of the form `mercury__append_3_0_i1'
	** in that case, save the internal label number and then
	** scan back past the mode number
	*/
	if (*end == 'i') {
		if (sscanf(end + 1, "%d", &internal) != 1) goto wrong_format;
		if (*--end != '_') goto wrong_format;
		do {
			if (end == start) goto wrong_format;
			end--;
		} while (isdigit((unsigned char)*end));
	}

	/*
	** parse the mode number
	*/
	if (sscanf(end + 1, "%d", &mode_num) != 1) goto wrong_format;

	/*
	** scan back past the arity number and then parse it
	*/
	do {
		if (end == start) goto wrong_format;
		end--;
	} while (isdigit((unsigned char)*end));
	if (*end != '_') goto wrong_format;
	if (sscanf(end + 1, "%d", &arity) != 1) goto wrong_format;

	/*
	** Process the mangling introduced by unused_args.m.
	** This involves stripping off the `__ua<m>' added to the
	** end of the predicate/function name, where m is the mode number.
	*/ 

	position = end;	/* save end of name */		

	do {
		if (position == start) goto wrong_format;
		position--;
	} while (isdigit((unsigned char)*position));
		/* get the mode number */
	if (position + 1 - sizeof(ua_suffix) > start 
		&& sscanf(position + 1, "%d", &mode_num2) == 1
		&& strncmp(position + 2 - sizeof(ua_suffix),
			ua_suffix, sizeof(ua_suffix) - 1) == 0) { 

		end = position + 2 - sizeof(ua_suffix);
		mode_num = mode_num2 % 10000;
		unused_args = TRUE;
	}

	/*
	** Process the mangling introduced by higher_order.m.
	** This involves stripping off the `__ho<n>_<a>' where
	** n is a unique identifier for this specialized version
	** and a is the original arity of the predicate.
	** 
	*/

	position = end;

	do {
		if (position == start) goto wrong_format;
		position--;
	} while (isdigit((unsigned char)*position));
	if (strncmp(position + 2 - sizeof(ho_suffix),
		ho_suffix, sizeof(ho_suffix) - 1) == 0) {
		
		end = position + 2 - sizeof(ho_suffix);
		higher_order = TRUE;
	}

	/*
	** Cut off the string before the start of the arity number,
	** and the unused_args and specialization information,
	** i.e. at the end of the predicate name or type name.
	*/
	*end = '\0';

	/*
	** Now start processing from the start of the string again.
	** Check whether the start of the string matches the name of
	** one of the special compiler-generated predicates; if so,
	** set the `category' to the appropriate value and then
	** skip past the prefix.
	*/
	if (strncmp(start, unify, sizeof(unify) - 1) == 0) {
		start += sizeof(unify) - 1;
		category = UNIFY;
	} else if (strncmp(start, compare, sizeof(compare) - 1) == 0) {
		start += sizeof(compare) - 1;
		category = COMPARE;
		if (mode_num != 0) goto wrong_format;
	} else if (strncmp(start, mindex, sizeof(mindex) - 1) == 0) {
		start += sizeof(mindex) - 1;
		category = INDEX;
		if (mode_num != 0) goto wrong_format;
	} else {
		category = ORDINARY;
	}

	/*
	** Separate the module name from the type name for the compiler
	** generated predicates.
	*/
	if (category != ORDINARY) {
		module = start++;

		/* Find the __ separating the module name from the type name. */
		while (*start != '_' || *(start + 1) != '_') {
			start++;
		}

		*start = '\0';
		start += 2;
	}

	/*
	** Make sure special predicates with unused_args 
	** are reported correctly.
	*/

	if (unused_args && category != ORDINARY) {
		do { 
			if (end == start) goto wrong_format;
			end--;
		} while (isdigit((unsigned char)*end));
		if (sscanf(end + 1, "%d", &arity) != 1) {
			goto wrong_format;
		}
		*end = '\0';
	}

	/*
	** The compiler changes all names starting with `f_' so that
	** they start with `f__' instead, and uses names starting with
	** `f_' for mangled names which are sequences of decimal
	** reprententations of ASCII codes separated by underscores.
	** If the name starts with `f__', we must change it back to
	** start with `f_'.  Otherwise, if it starts with `f_' we must
	** convert the list of ASCII codes back into an identifier.
	** 
	*/

	if (strncmp(start, "f__" , 3) == 0) {
		start++;
		*start = 'f';
	} else if (strncmp(start, "f_", 2) == 0) {
		char buf[1000];
		char *num = start + 2;
		int count = 0;
		while (num < end) {
			char *next_num = num;
			while (isdigit((unsigned char)*next_num)) {
				next_num++;
			}
			if (*next_num != '_' && *next_num != '\0') break;
			*next_num = '\0';
			buf[count++] = atoi(num);
			num = next_num + 1;
		}
		buf[count] = '\0';
		strcpy(start, buf);
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
		printf("index/3 predicate for type '%s:%s'/%d", module,
			start, arity);
		break;
	default:
		printf("%s '%s'/%d mode %d",
			pred_or_func, start, arity, mode_num);
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

wrong_format:
	printf("%s", name);
	return;
}

/*---------------------------------------------------------------------------*/
