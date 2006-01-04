/*
** Copyright (C) 2005-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING in the Mercury distribution.
**
** Main author: Ian MacLarty (maclarty@cs.mu.oz.au).
**
** A C version of mdb for use with the Windows installer package.
** We use a C program instead of a batch file so that we
** can remove the quotes from MERCURY_CONFIG_DIR before
** setting MERCURY_DEBUGGER_INIT and so that an arbitrary number
** of arguments can be supported.
** We use a C program instead of a Mercury program, because the
** statically linked Mercury program would be too big.
*/

#include <stdio.h>
#include <stdlib.h>

static	void	delete_quotes(char* str);

int main(int argc, char **argv)
{
	char*	mercury_config_dir;
	char*	mercury_options;
	char	set_debugger_init[8196];
	char	set_mercury_options[8196];
	char	command_string[8196];
	int	command_string_pos;
	int	arg_num;

	mercury_config_dir = getenv("MERCURY_CONFIG_DIR");
	if (mercury_config_dir == NULL) {
		fprintf(stderr, "MERCURY_CONFIG_DIR not set.\n");
		exit(EXIT_FAILURE);
	}
	delete_quotes(mercury_config_dir);
	sprintf(set_debugger_init, "MERCURY_DEBUGGER_INIT=%s\\mdb\\mdbrc",
		mercury_config_dir);
	putenv(set_debugger_init);

	mercury_options = getenv("MERCURY_OPTIONS");
	if (mercury_options == NULL) {
		mercury_options = "";
	}
	sprintf(set_mercury_options, "MERCURY_OPTIONS=-Di %s", mercury_options);
	putenv(set_mercury_options);

	command_string_pos = 0;
	for (arg_num = 1; argv[arg_num] != NULL; arg_num++) {
		strcpy(&command_string[command_string_pos], argv[arg_num]);
		command_string_pos += strlen(argv[arg_num]);
		strcpy(&command_string[command_string_pos], " ");
		command_string_pos++;
	}

	command_string[command_string_pos] = '\0';

	/*
	** We must use system and not execv, because Windows forks off another
	** process and returns to the command prompt with execv.
	*/
	return system(command_string);
}

static	void
delete_quotes(char* str)
{
	int	read_pos;
	int	write_pos;
	int	len;

	len = strlen(str);
	write_pos = 0;
	for (read_pos = 0; read_pos < len; read_pos++) {
		if (str[read_pos] != '\"') {
			str[write_pos] = str[read_pos];
			write_pos++;
		}
	}
	str[write_pos] = '\0';
}
