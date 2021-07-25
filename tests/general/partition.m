%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test - a previous version of the compiler
% got an internal compiler error when compiling this file.
% (Thanks to Bart Demoen for this test.)

/* Running this program yields
213
4

*** Mercury runtime: caught segmentation violation ***
cause: address not mapped to object
PC at signal: 120476 (1d69c)
address involved: 8
exiting from signal handler
*/

:- module partition.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module solutions.

main(!IO) :-
    solutions(bug, List),
    (
        List = [],
        io.write_string("No solution\n", !IO)
    ;
        List = [_ | _],
        print_solnlist(List, !IO)
    ).

:- pred print_solnlist(list(pair(list(int)))::in, io::di, io::uo)
    is det.

print_solnlist([], !IO).
print_solnlist([Le - Gr | Rest], !IO) :-
    print_intlist(Le, !IO),
    print_intlist(Gr, !IO),
    io.nl(!IO),
    print_solnlist(Rest, !IO).

:- pred bug(pair(list(int))::out) is nondet.

bug(Le - Gr) :-
    part(3, [4, 2, 1, 3], Le, Gr).

:- pred part(int::in, list(int)::in, list(int)::out, list(int)::out) is nondet.

part(_X, [], [], []).
part(X, [Y | L], [Y | Le], Gr):-
    Y =< X,
    part(X, L, Le, Gr).
part(X, [Y | L], Le, [Y | Gr]):-
    Y > X,
    part(X, L, Le, Gr).

:- pred print_intlist(list(int)::in, io::di, io::uo) is det.

print_intlist([], !IO) :-
    io.nl(!IO).
print_intlist([X | L], !IO) :-
    io.write_int(X, !IO),
    print_intlist(L, !IO).

:- end_module partition.
