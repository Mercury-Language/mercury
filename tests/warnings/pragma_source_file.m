%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pragma_source_file.

:- interface.

:- import_module int.
:- import_module list.

:- pred my_append(list(int), list(int), list(int)).
:- mode my_append(in, in, out) is det.

:- implementation.

:- pragma source_file("foo.m").
#1
my_append([], L, L) :-
#2
    L = L2.
#10
my_append([H | T], L, [H | NT]) :-
    X = L,
    my_append(T, L, NT).
