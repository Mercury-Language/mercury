%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% debug.m: Debuging predicates for the mercury profiler
%
% Main author: petdr.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module debug.

:- interface.

:- import_module io.

:- pred output_cliques(list(set(string)), io__state, io__state).
:- mode output_cliques(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set, list, string.
:- import_module getopt, options, globals.
:- import_module std_util, require.

%-----------------------------------------------------------------------------%


% output_cliques
%	Used to check that the topological ordering is being done correctly.
output_cliques([]) --> [].
output_cliques([C | Cs]) -->
	io__write_string("================================\n"),
	{ set__to_sorted_list(C, M) },
	print_list(M),
	output_cliques(Cs).


:- pred print_list(list(string), io__state, io__state).
:- mode	print_list(in, di, uo) is det.

print_list([]) --> [].
print_list([X | Xs]) -->
	io__write_string(X),
	io__write_string("\n"),
	print_list(Xs).


%-----------------------------------------------------------------------------%
