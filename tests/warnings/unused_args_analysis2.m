:- module unused_args_analysis2.

:- interface.

:- pred p2(int::in, int::out) is det.

:- implementation.

p2(_, 1).
