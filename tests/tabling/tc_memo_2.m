%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tc_memo_2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module std_util.

:- pragma require_feature_set([memo]).

main(!IO) :-
    ( if
        tc(1, 4),
        tc(3, 4)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred tc(int::in, int::out) is nondet.
:- pragma memo(tc/2).

tc(A, B) :-
    edge(A, C),
    (
        B = C
    ;
        tc(C, B)
    ).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(3, 4).
