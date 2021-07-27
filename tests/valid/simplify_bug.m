%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test:
% When compiling this file with `mc -O-1', Mercury 0.6
% got an internal compiler error ("nondet code in det/semidet context").

:- module simplify_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.
:- import_module std_util.

main(!IO) :-
    ( if nasty([]) then
        main(!IO),
        main(!IO)
    else
        true
    ).

:- pred nasty(list(T)::in) is nondet.

nasty(_) :-
    ( if semidet_succeed then
        list.append(X, _, []),
        X \= [],
        e(X)
    else
        semidet_succeed
    ).

:- pred e(list(T)::in) is erroneous.
:- pragma no_inline(e/1).

e(_) :-
    error("e/1").
