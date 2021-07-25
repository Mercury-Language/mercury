%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for the switch pruning code in simplify.m.

:- module prune_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    f(int)
    ;       g(int)
    ;       h(int).

main(!IO) :-
    create_switch_var(SwitchedOn),
    ( if
        (
            SwitchedOn = f(Int)
        ;
            SwitchedOn = h(Int)
        )
    then
        io.write_int(Int, !IO)
    else
        io.write_string("Failed", !IO)
    ).

:- pred create_switch_var(t::out) is det.

create_switch_var(Var) :-
    create_switch_var_2(Var).

:- pred create_switch_var_2(t :: (free >> bound(f(ground);g(ground)))) is det.
:- pragma no_inline(create_switch_var_2/1).

create_switch_var_2(f(1)).
