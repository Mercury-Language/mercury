/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** File: info_to_mdb.c
** Author: zs
**
** This tool takes as its input a dump (via the `info' program) of the
** nodes in the Mercury user's guide that describe debugging commands,
** and generates from them the mdb commands that register with mdb
** the online documentation of those commands.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mercury_std.h"

#define	MAXLINELEN	160

static	bool	is_empty(const char *line);
static	bool	is_all_same_char(const char *line, const char what);
static	bool	is_command(const char *line, bool *is_concept);
static	bool	diff_command(const char *line, const char *command);
static	void	print_command_line(const char *line, bool is_concept);

static	char	*get_next_line(FILE *infp);
static	void	put_line_back(char *line);

#define	concept_char(c)		(MR_isupper(c) ? tolower(c) : \
					(MR_isspace(c) ? '_' : (c)))

int
main(int argc, char **argv)
{
	FILE	*infp;
	char	*line;
	char	command[MAXLINELEN];
	int	slot = 0;
	int	len;
	int	i;
	bool	is_concept;
	bool	next_concept;

	if (argc != 3) {
		printf("usage: info_to_mdb section_name section_info_file\n");
		exit(EXIT_FAILURE);
	}

	if ((infp = fopen(argv[2], "r")) == NULL) {
		printf("cannot read %s\n", argv[2]);
		exit(EXIT_FAILURE);
	}

	/* skip the top part of the node, up to and including */
	/* the underlined heading */

	while ((line = get_next_line(infp)) != NULL) {
		if (is_all_same_char(line, '-') || is_all_same_char(line, '='))
		{
			break;
		}
	}

	while (TRUE) {
		line = get_next_line(infp);
		while (line != NULL && is_empty(line)) {
			line = get_next_line(infp);
		}

		if (line == NULL) {
			return 0;
		}

		if (! is_command(line, &is_concept)) {
			continue;
		}

		slot += 100;
		printf("document %s %d ", argv[1], slot);
		if (is_concept) {
			for (i = 0; line[i] != '\n'; i++) {
				command[i] = concept_char(line[i]);
				putchar(concept_char(line[i]));
			}
			putchar('\n');
			command[i] = '\0';
		} else {
			for (i = 1; MR_isalnumunder(line[i]); i++) {
				command[i-1] = line[i];
				putchar(line[i]);
			}
			putchar('\n');
			command[i-1] = '\0';
		}

		print_command_line(line, is_concept);

		while ((line = get_next_line(infp)) != NULL) {
			if (is_command(line, &next_concept)) {
				if (diff_command(line, command)) {
					put_line_back(line);
					break;
				} else {
					print_command_line(line, next_concept);
				}
			} else {
				printf("%s", line);
			}
		}

		printf("end\n");
	}
}

static bool
is_empty(const char *line)
{
	int	i = 0;

	while (line[i] != '\0') {
		if (!MR_isspace(line[i])) {
			return FALSE;
		}

		i++;
	}

	return TRUE;
}

static bool
is_all_same_char(const char *line, const char what)
{
	int	i = 0;

	while (line[i] != '\0') {
		if (line[i] != what && line[i] != '\n') {
			return FALSE;
		}

		i++;
	}

	return i > 1;
}

static bool
is_command(const char *line, bool *is_concept)
{
	int	len;

	len = strlen(line);
	if ((line[0] == '`') && (line[len-2] == '\'')) {
		*is_concept = FALSE;
		return TRUE;
	} else if (MR_isupper(line[0]) && MR_isupper(line[1])) {
		*is_concept = TRUE;
		return TRUE;
	} else {
		return FALSE;
	}
}

static bool
diff_command(const char *line, const char *command)
{
	int	i;

	for (i = 0; command[i] != '\0' && line[i + 1] == command[i]; i++)
		;

	if (command[i] == '\0' && ! MR_isalnumunder(line[i+1])) {
		return FALSE;
	} else {
		return TRUE;
	}
}

static void
print_command_line(const char *line, bool is_concept)
{
	int	len;
	int	i;

	if (is_concept) {
		for (i = 0; line[i] != '\n'; i++) {
			putchar(concept_char(line[i]));
		}
		putchar('\n');
	} else {
		len = strlen(line);
		for (i = 1; i < len - 2; i++) {
			putchar(line[i]);
		}
		putchar('\n');
	}
}

static	char	*putback_line = NULL;
static	char	line_buf[MAXLINELEN];

static char *
get_next_line(FILE *infp)
{
	char	*tmp;

	if (putback_line != NULL) {
		tmp = putback_line;
		putback_line = NULL;
		return tmp;
	} else {
		if (fgets(line_buf, MAXLINELEN, infp) == NULL) {
			return NULL;
		} else {
			/* printf("read %s", line_buf); */
			return line_buf;
		}
	}
}

static void
put_line_back(char *line)
{
	if (putback_line != NULL) {
		printf("trying to put back more than one line\n");
		exit(EXIT_FAILURE);
	}

	putback_line = line;
}
