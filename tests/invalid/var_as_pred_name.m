%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The term parser turns "X(a, b)" into "`'(X, a, b)". This would leads us
% to generate confusing error messages for invalid declarations and clauses
% such as those below if we didn't detect them, and handle them specially.

:- module var_as_pred_name.
:- interface.

:- func ABC(string) = int.

:- pred XYZ(string, int).
:- mode XYZ(in, out) is det.

:- implementation.

:- import_module string.

ABC(S) = N :-
    string.length(S, N).

XYZ(S, N) :-
    string.length(S, N).
