%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2004-2006 The University of Melbourne.
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

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module set.
:- import_module string.

    % output_cliques:
    % Used to check that the topological ordering is being done correctly.
    %
:- pred output_cliques(list(set(string))::in, io::di, io::uo) is det.

:- pred output_propagate_info(set(string)::in, assoc_list(string, int)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.

%-----------------------------------------------------------------------------%

output_cliques([], !IO).
output_cliques([C | Cs], !IO) :-
    io.write_string("================================\n", !IO),
    set.to_sorted_list(C, M),
    print_list(M, !IO),
    output_cliques(Cs, !IO).

output_propagate_info(Clique, Parents, !IO) :-
    io.write_string("************************\n", !IO),
    io.write_string("Clique\n", !IO),
    print_set(Clique, !IO),
    io.write_string("\nParents\n", !IO),
    print_assoc_list(Parents, !IO).

:- pred print_set(set(string)::in, io::di, io::uo) is det.

print_set(Set, !IO) :-
    set.to_sorted_list(Set, List),
    print_list(List, !IO).

:- pred print_assoc_list(assoc_list(string,int)::in, io::di, io::uo) is det.

print_assoc_list([], !IO).
print_assoc_list([ A - B | Xs], !IO) :-
    io.write_string(A, !IO),
    io.write_string("\t-\t", !IO),
    io.write_int(B, !IO),
    io.write_string("\n", !IO),
    print_assoc_list(Xs, !IO).

:- pred print_list(list(string)::in, io::di, io::uo) is det.

print_list([], !IO).
print_list([X | Xs], !IO) :-
    io.write_string(X, !IO),
    io.write_string("\n", !IO),
    print_list(Xs, !IO).

%-----------------------------------------------------------------------------%
:- end_module prof_debug.
%-----------------------------------------------------------------------------%
