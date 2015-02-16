%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multidet_test.

:- import_module std_util.

:- implementation.

:- pred test_pred is det.

test_pred :-
    test_pred_2.

:- pred test_pred_2 is multidet.

test_pred_2 :-
    (
        semidet_succeed
    ;
        true
    ).
