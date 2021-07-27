% vim: ts=4 sw=4 et ft=mercury
%
% This a regression test for Mantis bug #128. The bug was that the loop
% invariants pass considered the unification that constructed a partially
% instantiated term (_L3 - comma in parse_enum0) to be an invariant goal,
% and attempted to hoist it out of parse_enum0's loop. Due to the free
% variable inside the term, this yielded a compiler abort.

:- module bug128.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- type tokens == assoc_list(int, token).

:- type token
    --->    comma
    ;       symbol(string)
    ;       eof.

main(!IO) :-
    write_string("Hello, world!\n", !IO).

:- type value
    --->    value(int, value0).

:- type value0
    --->    value_sym(string).

:- type parse_res(T)
    --->    ok(T)
    ;       error.

:- pred parse_enum0(int::in, assoc_list(string, maybe(value))::in,
    parse_res(value)::out, tokens::in, tokens::out) is det.

parse_enum0(L, Vs0, Res, !Ts) :-
    ( if next(_L2 - symbol(Sym), !Ts) then
        ( if next(_L3 - comma, !Ts) then
            Vs = [Sym - no | Vs0],
            parse_enum0(L, Vs, Res, !Ts)
        else
            Res = error
        )
    else
        Res = error
    ).

:- pred next(pair(int, token)::out, tokens::in, tokens::out) is det.

next(L - T, [L - T | Ts], Ts).
next(0 - eof, [], []).
