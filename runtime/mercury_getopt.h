/*
Copyright © 2005-2014 Rich Felker, et al.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef MERCURY_GETOPT_H
#define MERCURY_GETOPT_H

#ifdef __cplusplus
extern "C" {
#endif

int MR_getopt(int, char * const [], const char *);
extern char *MR_optarg;
extern int MR_optind, MR_opterr, MR_optopt, MR_optreset;

struct MR_option {
	const char *name;
	int has_arg;
	int *flag;
	int val;
};

int MR_getopt_long(int, char *const *, const char *, const struct MR_option *, int *);
int MR_getopt_long_only(int, char *const *, const char *, const struct MR_option *, int *);

#define MR_no_argument        0
#define MR_required_argument  1
#define MR_optional_argument  2

#ifdef GETOPT_IMPL
void MR__getopt_msg(const char *a, const char *b, const char *c, size_t l);
#endif

#ifdef __cplusplus
}
#endif

#endif
