/*
** Copyright (C) 1998-1999 The University of Melbourne.
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

static	MR_bool	is_empty(const char *line);
static	MR_bool	is_all_same_char(const char *line, const char what);
static	MR_bool	is_command(const char *line, MR_bool *is_concept);
static	void	get_command(const char *line, char *command);
static	void	print_command_line(const char *line, MR_bool is_concept);

static	char	*get_next_line(FILE *infp);
static	void	put_line_back(char *line);

#define	concept_char(c)		(MR_isspace(c) ? '_' : (c))

int
main(int argc, char **argv)
{
	char	*filename;
	char	*category;	/* mdb help category */
	FILE	*infp;
	char	*line;
	int	num_lines;
	char	command[MAXLINELEN];
	char	next_command[MAXLINELEN];
	int	slot = 0;
	MR_bool	is_concept;
	MR_bool	next_concept;

	if (argc != 3) {
		fprintf(stderr,
			"usage: info_to_mdb category_name section_info_file\n");
		exit(EXIT_FAILURE);
	}
	category = argv[1];
	filename = argv[2];

	if ((infp = fopen(filename, "r")) == NULL) {
		fprintf(stderr, "cannot read `%s'\n", filename);
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

	while (MR_TRUE) {
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
		printf("document %s %d ", category, slot);
		if (is_concept) {
			int	i;
			for (i = 0; line[i + 2] != '\n'; i++) {
				command[i] = concept_char(line[i + 1]);
			}
			command[i] = '\0';
		} else {
			get_command(line, command);
		}
		puts(command);

		print_command_line(line, is_concept);

		num_lines = 0; 
		while ((line = get_next_line(infp)) != NULL) {
			if (is_command(line, &next_concept)) {
				get_command(line, next_command);
				if (strcmp(command, next_command) != 0) {
					/*
					** Sometimes several commands
					** are documented together, e.g.
					**
					** cmd1 args...
					** cmd2 args...
					** cmd3 args...
					**	description...
					**
					** It's difficult for us to handle
					** that case properly here, so we
					** just insert cross references
					** ("cmd1: see cmd2", "cmd2: see cmd3",
					** etc.)
					*/
					if (num_lines == 0) {
						printf("    See help for "
							"`%s'.\n",
							next_command);
					}
					put_line_back(line);
					break;
				} else {
					print_command_line(line, next_concept);
				}
			} else {
				printf("%s", line);
			}
			num_lines++;
		}

		printf("end\n");
	}
}

static MR_bool
is_empty(const char *line)
{
	int	i = 0;

	while (line[i] != '\0') {
		if (!MR_isspace(line[i])) {
			return MR_FALSE;
		}

		i++;
	}

	return MR_TRUE;
}

static MR_bool
is_all_same_char(const char *line, const char what)
{
	int	i = 0;

	while (line[i] != '\0') {
		if (line[i] != what && line[i] != '\n') {
			return MR_FALSE;
		}

		i++;
	}

	return i > 1;
}

static MR_bool
is_command(const char *line, MR_bool *is_concept)
{
	int	len;

	len = strlen(line);
	if ((line[0] == '`') && (line[len-2] == '\'')) {
		*is_concept = MR_FALSE;
		return MR_TRUE;
	} else if ((line[0] == '_') && (line[len-2] == '_')) {
		*is_concept = MR_TRUE;
		return MR_TRUE;
	} else {
		return MR_FALSE;
	}
}

static void
get_command(const char *line, char *command)
{
	int	i;

	for (i = 0; MR_isalnumunder(line[i + 1]); i++) {
		command[i] = line[i + 1];
	}
	command[i] = '\0';
}

static void
print_command_line(const char *line, MR_bool is_concept)
{
	int	len;
	int	i;

	len = strlen(line);
	for (i = 1; i < len - 2; i++) {
		putchar(is_concept ? concept_char(line[i]) : line[i]);
	}
	putchar('\n');
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
		fprintf(stderr, "trying to put back more than one line\n");
		exit(EXIT_FAILURE);
	}

	putback_line = line;
}
