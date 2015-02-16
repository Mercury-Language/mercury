%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ref_to_implicit_comma.
:- interface.

:- pred test is det.

:- implementation.

test :- fail.
   test,
   test,
   test.
