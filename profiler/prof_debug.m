%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% prof_debug.m: Debuging predicates for the mercury profiler
%
% Main author: petdr.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prof_debug.
:- interface.
:- import_module set, list, string, io, assoc_list.

:- pred output_cliques(list(set(string))::in, io::di, io::uo) is det.

:- pred output_propagate_info(set(string)::in, assoc_list(string, int)::in,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

%-----------------------------------------------------------------------------%


% output_cliques
%	Used to check that the topological ordering is being done correctly.
output_cliques([]) --> [].
output_cliques([C | Cs]) -->
	io__write_string("================================\n"),
	{ set__to_sorted_list(C, M) },
	print_list(M),
	output_cliques(Cs).

output_propagate_info(Clique, Parents) -->
        io__write_string("************************\n"),
        io__write_string("Clique\n"),
        print_set(Clique),
        io__write_string("\nParents\n"),
        print_assoc_list(Parents).


:- pred print_set(set(string), io__state, io__state).
:- mode	print_set(in, di, uo) is det.

print_set(Set) -->
	{ set__to_sorted_list(Set, List) },
	print_list(List).


:- pred print_assoc_list(assoc_list(string,int), io__state, io__state).
:- mode	print_assoc_list(in, di, uo) is det.

print_assoc_list([]) --> [].
print_assoc_list([ A - B | Xs]) -->
	io__write_string(A),
	io__write_string("\t-\t"),
	io__write_int(B),
	io__write_string("\n"),
	print_assoc_list(Xs).

:- pred print_list(list(string), io__state, io__state).
:- mode	print_list(in, di, uo) is det.

print_list([]) --> [].
print_list([X | Xs]) -->
	io__write_string(X),
	io__write_string("\n"),
	print_list(Xs).

%-----------------------------------------------------------------------------%
:- end_module prof_debug.
%-----------------------------------------------------------------------------%
