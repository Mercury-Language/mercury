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

static void demangle(char *name);

int main(int argc, char **argv)
{
	char buf[1000];

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
** It ought to be rewritten (preferably in a language
** with better string-handling facilities than C!)
*/
static void demangle(char *name) {
	static const char entry[] = "entry_";
	static const char mercury[] = "mercury__";
	static const char unify[] = "__Unify___";
	static const char compare[] = "__Compare___";
	static const char index[] = "__Index___";
	char *start = name;
	char *end = name + strlen(name);
	char buf[1000];
	int mode_num;
	int arity;
	int internal = -1;
	enum { ORDINARY, UNIFY, COMPARE, INDEX } category;

	if (strncmp(start, entry, sizeof(entry) - 1) == 0) {
		start += sizeof(entry) - 1;
	}
	if (strncmp(start, mercury, sizeof(mercury) - 1) == 0) {
		start += sizeof(mercury) - 1;
	} else {
		goto wrong_format;
	}
	do {
		if (end == start) goto wrong_format;
		end--;
	} while (isdigit(*end));
	if (*end == 'i') {
		if (sscanf(end + 1, "%d", &internal) != 1) goto wrong_format;
		if (*--end != '_') goto wrong_format;
		do {
			if (end == start) goto wrong_format;
			end--;
		} while (isdigit(*end));
	}
	if (sscanf(end + 1, "%d", &mode_num) != 1) goto wrong_format;
	do {
		if (end == start) goto wrong_format;
		end--;
	} while (*end != '_');
	if (sscanf(end + 1, "%d", &arity) != 1) goto wrong_format;
	*end = '\0';
	if (strncmp(start, unify, sizeof(unify) - 1) == 0) {
		start += sizeof(unify) - 1;
		category = UNIFY;
	} else if (strncmp(start, compare, sizeof(compare) - 1) == 0) {
		start += sizeof(compare) - 1;
		category = COMPARE;
		if (mode_num != 0) goto wrong_format;
	} else if (strncmp(start, index, sizeof(index) - 1) == 0) {
		start += sizeof(index) - 1;
		category = INDEX;
		if (mode_num != 0) goto wrong_format;
	} else {
		category = ORDINARY;
	}
	if (strncmp(start, "f__" , 3) == 0) {
		start++;
		*start = 'f';
	} else if (strncmp(start, "f_", 2) == 0) {
		char *num = start + 2;
		int count = 0;
		while (num < end) {
			char *next_num = num;
			while (isdigit(*next_num)) {
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

	printf("<");
	switch(category) {
	case UNIFY:
		printf("unification predicate for type '%s'/%d mode %d",
			start, arity, mode_num);
		break;
	case COMPARE:
		printf("compare/3 predicate for type '%s'/%d",
			start, arity);
		break;
	case INDEX:
		printf("index/3 predicate for type '%s'/%d", start, arity);
		break;
	default:
		printf("predicate '%s'/%d mode %d", start, arity, mode_num);
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
