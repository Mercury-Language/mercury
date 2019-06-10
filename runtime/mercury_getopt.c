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

#include <string.h>
#include <stdio.h>
#define GETOPT_IMPL
#include "mercury_getopt.h"

char *MR_optarg;
int MR_optind=1, MR_opterr=1, MR_optopt, MR__optpos, MR_optreset=0;

#define optpos MR__optpos

void MR__getopt_msg(const char *a, const char *b, const char *c, size_t l)
{
	FILE *f = stderr;
	if (
		fputs(a, f)>=0
		&& fwrite(b, strlen(b), 1, f)
		&& putc('`', f)
		&& fwrite(c, 1, l, f)==l
		&& fputs("'\n", f)>=0
	) { }
}

int MR_getopt(int argc, char * const argv[], const char *optstring)
{
	int i;
	int c, d;
	int k, l;
	char *optchar;

	if (!MR_optind || MR_optreset) {
		MR_optreset = 0;
		MR__optpos = 0;
		MR_optind = 1;
	}

	if (MR_optind >= argc || !argv[MR_optind])
		return -1;

	if (argv[MR_optind][0] != '-') {
		if (optstring[0] == '-') {
			MR_optarg = argv[MR_optind++];
			return 1;
		}
		return -1;
	}

	if (!argv[MR_optind][1])
		return -1;

	if (argv[MR_optind][1] == '-' && !argv[MR_optind][2])
		return MR_optind++, -1;

	if (!optpos) optpos++;
	c = argv[MR_optind][optpos];
	k = 1;
	optchar = argv[MR_optind]+optpos;
	optpos += k;

	if (!argv[MR_optind][optpos]) {
		MR_optind++;
		optpos = 0;
	}

	if (optstring[0] == '-' || optstring[0] == '+')
		optstring++;

	i = 0;
	d = 0;
	do {
		d = optstring[i];
		l = d ? 1 : 0;
		if (l>0) i+=l; else i++;
	} while (l && d != c);

	if (d != c || c == ':') {
		MR_optopt = c;
		if (optstring[0] != ':' && MR_opterr)
			MR__getopt_msg(argv[0], ": unrecognized option: ", optchar, k);
		return '?';
	}
	if (optstring[i] == ':') {
		MR_optarg = 0;
		if (optstring[i+1] != ':' || optpos) {
			MR_optarg = argv[MR_optind++] + optpos;
			optpos = 0;
		}
		if (MR_optind > argc) {
			MR_optopt = c;
			if (optstring[0] == ':') return ':';
			if (MR_opterr) MR__getopt_msg(argv[0],
				": option requires an argument: ",
				optchar, k);
			return '?';
		}
	}
	return c;
}
