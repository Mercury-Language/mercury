%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The term parser turns "X(a, b)" into "`'(X, a, b)". This would leads us
% to generate confusing error messages for invalid declarations and clauses
% such as those below if we didn't detect them, and handle them specially.
%

:- module var_as_pred_name.
:- interface.

:- func A(string) = int.
:- mode B(di) = uo.
:- mode C(in) = out is det.
:- func D(string::in) = (int::out).
:- func E(string::in) = (int::out) is det.

:- pred R(string, int).
:- mode S(in, out).
:- mode T(in, out) is det.
:- pred U(string::in, int::out).
:- pred V(string::in, int::out) is det.

:- implementation.

:- import_module string.

X(S) = N :-
    string.length(S, N).

Y(S, N) :-
    string.length(S, N).
