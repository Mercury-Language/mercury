%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module call_impure_in_opt_helper_1.

:- interface.

:- impure pred intermod_impure(int::out) is det.

:- implementation.

intermod_impure(Int) :-
    impure intermod_impure_2(Int).

:- impure pred intermod_impure_2(int::out) is det.

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C",
    intermod_impure_2(Int::out),
    [will_not_call_mercury],
"
    printf(""Output from impure predicate\\n"");
    Int = 2;
").
:- pragma foreign_proc("Java",
    intermod_impure_2(Int::out),
    [will_not_call_mercury],
"
    System.out.println(""Output from impure predicate\\n"");
    Int = 2;
").
