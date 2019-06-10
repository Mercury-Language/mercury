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

#include <stdlib.h>
#include <string.h>
#define GETOPT_IMPL
#include "mercury_getopt.h"

extern int MR__optpos;

static void permute(char *const *argv, int dest, int src)
{
	char **av = (char **)argv;
	char *tmp = av[src];
	int i;
	for (i=src; i>dest; i--)
		av[i] = av[i-1];
	av[dest] = tmp;
}

static int __getopt_long_core(int argc, char *const *argv, const char *optstring, const struct MR_option *longopts, int *idx, int longonly);

static int __getopt_long(int argc, char *const *argv, const char *optstring, const struct MR_option *longopts, int *idx, int longonly)
{
	int ret, skipped, resumed;
	if (!MR_optind || MR_optreset) {
		MR_optreset = 0;
		MR__optpos = 0;
		MR_optind = 1;
	}
	if (MR_optind >= argc || !argv[MR_optind]) return -1;
	skipped = MR_optind;
	if (optstring[0] != '+' && optstring[0] != '-') {
		int i;
		for (i=MR_optind; ; i++) {
			if (i >= argc || !argv[i]) return -1;
			if (argv[i][0] == '-' && argv[i][1]) break;
		}
		MR_optind = i;
	}
	resumed = MR_optind;
	ret = __getopt_long_core(argc, argv, optstring, longopts, idx, longonly);
	if (resumed > skipped) {
		int i, cnt = MR_optind-resumed;
		for (i=0; i<cnt; i++)
			permute(argv, skipped, MR_optind-1);
		MR_optind = skipped + cnt;
	}
	return ret;
}

static int __getopt_long_core(int argc, char *const *argv, const char *optstring, const struct MR_option *longopts, int *idx, int longonly)
{
	MR_optarg = 0;
	if (longopts && argv[MR_optind][0] == '-' &&
		((longonly && argv[MR_optind][1] && argv[MR_optind][1] != '-') ||
		 (argv[MR_optind][1] == '-' && argv[MR_optind][2])))
	{
		int colon = optstring[optstring[0]=='+'||optstring[0]=='-']==':';
		int i, cnt, match;
		char *arg, *opt, *start = argv[MR_optind]+1;
		for (cnt=i=0; longopts[i].name; i++) {
			const char *name = longopts[i].name;
			opt = start;
			if (*opt == '-') opt++;
			while (*opt && *opt != '=' && *opt == *name)
				name++, opt++;
			if (*opt && *opt != '=') continue;
			arg = opt;
			match = i;
			if (!*name) {
				cnt = 1;
				break;
			}
			cnt++;
		}
		if (cnt==1 && longonly && arg-start == 1) {
			int l = arg-start;
			for (i=0; optstring[i]; i++) {
				int j;
				for (j=0; j<l && start[j]==optstring[i+j]; j++);
				if (j==l) {
					cnt++;
					break;
				}
			}
		}
		if (cnt==1) {
			i = match;
			opt = arg;
			MR_optind++;
			if (*opt == '=') {
				if (!longopts[i].has_arg) {
					MR_optopt = longopts[i].val;
					if (colon || !MR_opterr)
						return '?';
					MR__getopt_msg(argv[0],
						": option does not take an argument: ",
						longopts[i].name,
						strlen(longopts[i].name));
					return '?';
				}
				MR_optarg = opt+1;
			} else if (longopts[i].has_arg == MR_required_argument) {
				if (!(MR_optarg = argv[MR_optind])) {
					MR_optopt = longopts[i].val;
					if (colon) return ':';
					if (!MR_opterr) return '?';
					MR__getopt_msg(argv[0],
						": option requires an argument: ",
						longopts[i].name,
						strlen(longopts[i].name));
					return '?';
				}
				MR_optind++;
			}
			if (idx) *idx = i;
			if (longopts[i].flag) {
				*longopts[i].flag = longopts[i].val;
				return 0;
			}
			return longopts[i].val;
		}
		if (argv[MR_optind][1] == '-') {
			MR_optopt = 0;
			if (!colon && MR_opterr)
				MR__getopt_msg(argv[0], cnt ?
					": option is ambiguous: " :
					": unrecognized option: ",
					argv[MR_optind]+2,
					strlen(argv[MR_optind]+2));
			MR_optind++;
			return '?';
		}
	}
	return MR_getopt(argc, argv, optstring);
}

int MR_getopt_long(int argc, char *const *argv, const char *optstring, const struct MR_option *longopts, int *idx)
{
	return __getopt_long(argc, argv, optstring, longopts, idx, 0);
}

int MR_getopt_long_only(int argc, char *const *argv, const char *optstring, const struct MR_option *longopts, int *idx)
{
	return __getopt_long(argc, argv, optstring, longopts, idx, 1);
}
