%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_user_compare.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- implementation.

:- type citrus =< fruit
    --->    orange
    ;       lemon
    where   equality is citrus_equal,
            comparison is citrus_compare.

:- pred citrus_equal(citrus::in, citrus::in) is semidet.

citrus_equal(_, _) :-
    semidet_true.

:- pred citrus_compare(comparison_result::uo, citrus::in, citrus::in) is det.

citrus_compare(=, _, _).
