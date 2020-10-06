%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% Previously a singleton switch could be replaced by the case goal, possibly
% with a functor test beforehand, but that's only true if the case arm is
% applicable to only a single functor.
%

:- module simplify_multi_arm_switch.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- type fruit
    --->    apple
    ;       pear
    ;       banana
    ;       plum.

:- pred squishy(fruit::in) is semidet.
:- pragma no_inline(squishy/1).

squishy(Fruit) :-
    (
        ( Fruit = apple
        ; Fruit = pear
        ),
        IsSquishy = no
    ;
        ( Fruit = banana
        ; Fruit = plum
        ),
        IsSquishy = yes
    ),
    IsSquishy = yes.

main(!IO) :-
    ( if squishy(apple) then
        io.write_string("bug!\n", !IO)
    else
        io.write_string("ok\n", !IO)
    ).
