%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multidet_test.

:- interface.

:- pred test_pred is det.

:- implementation.

:- import_module std_util.

test_pred :-
    test_pred_2.

:- pred test_pred_2 is multi.

test_pred_2 :-
    (
        semidet_succeed
    ;
        true
    ).
