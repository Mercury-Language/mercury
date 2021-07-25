%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module special_term_dep.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

% Term dependencies in the presence of calls to special predicates.

:- implementation.
:- import_module list.

main(!IO) :-
    test1(!IO),          % compare
    test2(!IO).          % unify

%---------------------------------------------------------------------------%

:- pred test1(io::di, io::uo) is det.

test1(!IO) :-
    p(L),
    io.write_line(L, !IO).

:- pred p(list(int)).
:- mode p(out) is det.

p(L) :-
    pa(L0),
    ( if compare('>', L0, [1]) then
        L = L0
    else
        L = []
    ).

:- pred pa(list(int)::out) is det.

pa([2, 3]).

%---------------------------------------------------------------------------%

:- pred test2(io::di, io::uo) is det.

test2(!IO) :-
    ( if q([1, 2], L) then
        io.write_line(L, !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred q(list(int)::in, list(int)::out) is semidet.

q(A, B) :-
    qa(A),
    qb(A, B).

:- pred qa(list(int)::out) is det.

qa([1, 2]).

:- pred qb(list(int)::in, list(int)::out) is det.

qb(_, [3]).

%---------------------------------------------------------------------------%

