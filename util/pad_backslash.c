//---------------------------------------------------------------------------//
// vim: sw=4 ts=4 expandtab
//---------------------------------------------------------------------------//
//
// Copyright (C) 2006 The University of Melbourne.
// This file may only be copied under the terms of the GNU General
// Public License - see the file COPYING in the Mercury distribution.
//
// File: pad_backslash.c
// Author: zs
//
// Given an input stream in which some lines end with backslashes, pad those
// lines with spaces so that the backslashes are in column 77.
//
//---------------------------------------------------------------------------//

#include    <stdio.h>
#include    <stdlib.h>
#include    <unistd.h>

// options and arguments, set by parse_options().

static  int     target_column = 77;

static  void    usage(void);
static  void    process(FILE *fp);

/*---------------------------------------------------------------------------*/

int
main(int argc, char **argv)
{
    int opt;
    int num_files;

    while ((opt = getopt(argc, argv, "c:")) != EOF) {
        switch (opt) {

            case 'c':
                if (sscanf(optarg, "%d", &target_column) != 1) {
                    usage();
                }

                break;

        default:
            usage();
        }
    }

    num_files = argc - optind;
    if (num_files == 0) {
        process(stdin);
    } else if (num_files == 1) {
        FILE    *fp;

        fp = fopen(argv[optind], "r");
        if (fp != NULL) {
            process(fp);
            (void) fclose(fp);
        } else {
            perror(argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    return EXIT_SUCCESS;
}

static void
usage(void)
{
    fputs("Usage: pad_backslash [-c column] [inputfile]\n", stderr);
    exit(EXIT_FAILURE);
}

static void
process(FILE *fp)
{
    int cur_column;
    int cur_char;
    int next_char;

    cur_column = 1;
    while ((cur_char = getc(fp)) != EOF) {
        if (cur_char == '\n') {
            putchar(cur_char);
            cur_column = 1;
        } else if (cur_char == '\\') {
            next_char = getc(fp);
            if (next_char == '\n') {
                while (cur_column < target_column) {
                    putchar(' ');
                    cur_column++;
                }

                putchar(cur_char);
                putchar(next_char);
                cur_column = 1;
            } else {
                putchar(cur_char);
                putchar(next_char);
                cur_column++;
            }
        } else if (cur_char == '\t') {
            while (cur_column % 8 != 0) {
                putchar(' ');
                cur_column++;
            }
        } else {
            putchar(cur_char);
            cur_column++;
        }
    }
}
