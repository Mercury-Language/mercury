%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module no_method.
:- interface.

:- import_module io.

:- typeclass class_a(T) where [
    func func_a(T, T, int) = T
].

:- typeclass class_b(T) where [
    func func_b(T, T, int) = T
].

:- typeclass class_c(T) where [
    func func_c(T, T, int) = T
].

:- instance class_a(string).
:- instance class_b(string).
:- instance class_c(string).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

:- instance class_a(string) where [
    func(func_a/3) is one_str           % one_str has fewer args than expected
].

:- instance class_b(string) where [
    func(func_b/3) is two_str           % two_str has correct arity
].

:- instance class_c(string) where [
    func(func_c/3) is three_str         % three_str has more args than expected
].

:- func one_str(string, int) = string.
:- func two_str(string, string, int) = string.
:- func three_str(string, string, string, int) = string.

one_str(S, _) = S.
two_str(S, _, _) = S.
three_str(S, _, _, _) = S.

%---------------------------------------------------------------------------%
:- end_module no_method.
%---------------------------------------------------------------------------%
