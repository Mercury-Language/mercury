%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% In some versions of the compiler, this results in a compiler abort.
% The problem is that when q and r are inlined in p, and the switch in r
% is pushed into the disjunction in q, the alternative for a is rightly noted
% as not being able to succeed, but the fail goal inserted after X=a has the
% determinism erroneous instead of fail. Since fail is represented internally
% as an empty disjunction, this violates an invariant required by the code
% generator (that every det disjunction must have an alternative that always
% succeeds).

:- module fail_detism.
:- interface.
:- import_module io.
:- import_module string.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    p(X),
    io.write_string(X, !IO),
    io.write_string("\n", !IO).

:- pred p(string::out) is cc_multi.

p(X1) :-
    ( if q(X), r(X) then
        X1 = X
    else
        X1 = "none"
    ).

:- pred q(string::out) is multi.
q("a").
q("b").
q("c").

:- pred r(string::in) is semidet.
r("b").
r("c").
