%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%

:- module bug402.
:- interface.

:- type foo
    --->    foo
    ;       bar
    ;       baaz.

:- pred convert_1(foo::in, foo::out) is semidet.
:- pred convert_2(foo::in, foo::out) is semidet.

:- implementation.

convert_1(!Value) :-
    convert_2(!.Value, !:Value),
    require_complete_switch [!.Value]
    (
        !.Value = foo,
        !:Value = bar
    ;
        !.Value = bar,
        false
    ;
        !.Value = baaz,
        !:Value = foo
    ).

convert_2(!Value) :-
    require_switch_arms_semidet [!.Value]
    (
        !.Value = foo,
        !:Value = bar
    ;
        !.Value = bar,
        false
    ;
        !.Value = baaz,
        !:Value = foo
    ).
