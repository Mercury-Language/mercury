:- module undef_lambda_mode.

:- interface.

:- pred test(pred(int)) is det.
:- mode test(pred(in) is semidet) is det.

:- implementation.

test((pred(X::junk) is semidet :- X = 1)).

