%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module non_term_user_special.

:- interface.

:- import_module list.

:- type myset(T)
    --->    myset(list(T))
    where
        equality   is my_set_equals,
        comparison is my_set_compare.

:- pred my_set_equals(myset(T)::in, myset(T)::in) is semidet.

:- pred my_set_compare(builtin.comparison_result::uo, myset(T)::in,
    myset(T)::in) is det.

:- implementation.

my_set_equals(_, _) :-
    loop.

:- pred loop is semidet.

loop :- a.

:- pred a is semidet.
:- pred b is semidet.

a :-
    b.
b :-
    a.

my_set_compare(Res, _, _) :-
    ( if loop then
        Res = (=)
    else
        Res = (=)
    ).
