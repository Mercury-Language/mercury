%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test the error messages we generate for situations in which there is a
% mismatch between (a) the declared arity or arities of a predicate or
% function, and (b) their actual arity or arities in their defining clause(s).
%
%---------------------------------------------------------------------------%

:- module bad_pred_arity.
:- interface.

:- pred p1(int::in,
    int::out, string::out) is det.
:- pred p1(int::in, float::in, string::in,
    int::out, string::out) is det.

:- pred p2.

:- pred p3(int::in, float::in, int::in, float::in, string::in, string::in,
    int::out, string::out) is det.

:- func f1(int, int) = string.
:- func f1(int, float, string, int) = string.

:- func f2 = string.
:- func f2(int, int) = string.
:- func f2(int, float, string, int) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module string.

p1(I, S, I + 1, S ++ "abc").

p1.

p2(N, M, Str, Res) :-
    Res = string.int_to_string(M) ++ Str ++ string.int_to_string(N).

p3(N, M, Str, Res) :-
    Res = string.int_to_string(M) ++ Str ++ string.int_to_string(N).

p3(N, M, Str, yes, Res) :-
    Res = string.int_to_string(M) ++ Str ++ string.int_to_string(N).

f1(M, Str, N) = string.int_to_string(M) ++ Str ++ string.int_to_string(N).

f1 = "abc".

f2(M, _, N) = M * N.

%---------------------------------------------------------------------------%
