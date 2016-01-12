%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The mode declarations of a predicate (or function) should be in
% the same section as its predicate (or function) declaration.

:- module mode_decl_in_wrong_section.

:- interface.

:- pred p(int, int).
:- mode p(in, out) is det.

:- mode f(in) = out is det.

:- implementation.

:- import_module int.

:- mode p(in, in) is semidet.

:- func f(int) = int.
:- mode f(in) = in is semidet.

p(A, B) :-
    B = A + 1.

f(A) = B :-
    B = A + 1.
