:- module undef_lambda_mode.

:- interface.

:- pred test(pred(int)) is det.
:- mode test(pred(in) is semidet) is det.

:- implementation.

test(lambda([X::junk] is semidet, X = 1)).

