% Regression test.  The compiler incorrectly computed the instmap delta of
% generic calls with no solutions.

:- module instmap_generic_failure.
:- interface.

:- import_module list.

:- pred int_to_ascii(pred(int, int), int, list(int), list(int)).
:- mode int_to_ascii(in(pred(in, out) is failure), in, in, out) is semidet.

:- implementation.

:- import_module int.

int_to_ascii(ConvertNonAscii, U, Old, New) :-
    ( U < 128 ->
        New = [U | Old]
    ;
        ConvertNonAscii(U, S),  % failure
        New = [S | Old]
    ).
