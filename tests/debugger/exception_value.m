%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exception_value.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module pair.

main(!IO) :-
    test1(Res1),
    print(Res1, !IO),
    io.nl(!IO),
    test2(Res2),
    print(Res2, !IO),
    io.nl(!IO).

:- pred test1(exception_result(int)::out) is cc_multi.
test1(Res) :-
    try(p, Res).

:- pred test2(exception_result(int)::out) is cc_multi.
test2(Res) :-
    try(q, Res).

:- pred p(int::out) is det.

p(_) :-
    throw("p exception").

:- pred q(int::out) is det.

q(_) :-
    throw("q oops" - [1, 2, 3]).
