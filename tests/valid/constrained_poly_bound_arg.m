:- module constrained_poly_bound_arg.

:- interface.

:- type val_closure(X1)
        ---> val_closure(pred(X1)).

:- type closure_list(X2)
        ---> nil
	;    list(X2,val_closure(closure_list(X2))).

:- inst val_closure(X3)
        == bound( error3__val_closure(pred(out(X3)) is det) ).

:- inst closure_list(X4)
        == bound( error3__nil
 	        ; error3__list(X4,val_closure(closure_list(X4)))
 		).
:- implementation.

:- pred eval(X5,X5).
:- mode eval(in(X6 =< ground),out(X6 =< ground)) is det.
eval(X,X).

:- pred m(closure_list(X8), closure_list(X8)).
:- mode m(in(closure_list(X9 =< ground)), out(closure_list(X9 =< ground)))
	is det.
m(CCL,RCL) :-
  eval(CCL,RCL).
