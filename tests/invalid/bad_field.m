%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% When a program contains a reference to a nonexistent function,
% and the error is due to a unknown name (as opposed to a missing
% or incorrect module qualifier, or an incorrect arity), we try to
% include in the error message a reminder about any function names
% that are similar to the erroneous name.
%
% This test case tests whether, when we construct the list of possible
% similar-enough names to choose the closest from, we include in this list
% the names of the automatically generated field get functions.
%

:- module bad_field.

:- interface.

:- type t
    --->    t(
                abc_fieldname   :: int
            ).

:- pred p(t::in, int::out) is det.

:- implementation.

:- import_module int.

p(T, N) :-
    % Should be either "N0 = abc_fieldname(T)", or N0 = T ^ abc_fieldname".
    % Without the fix committed on 2025 oct 21, we suggested that
    % "ac_fieldname" should be replaced by "fieldname", due to
    % "abc_fieldname" not being included in the list of possible replacements.
    N0 = ac_fieldname(T),
    N = fieldname(N0).

:- func fieldname(int) = int.

fieldname(A) = A + 1.
