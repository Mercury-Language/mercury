:- module multidet_test.

:- import_module std_util.

:- implementation.

:- pred test_pred is det.

test_pred :- test_pred_2.

:- pred test_pred_2 is multidet.

test_pred_2 :-
        (
                semidet_succeed
        ;
                true
        ).
