%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for use of zero-arity higher-order function terms.
%
% Author: fjh

:- module nullary_ho_func_error.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- func pi = float.

pi = 3.14159.

main(!IO) :-
    io.print("apply_nullary_func(pi) = ", !IO),
    % this would be legal:
    % print(apply_nullary_func((func) = pi)), nl.
    % this one is not:
    io.print_line(apply_nullary_func(pi), !IO).

:- func apply_nullary_func((func) = T) = T.
:- mode apply_nullary_func(in((func) = out is det)) = out is det.

apply_nullary_func(F) = apply(F).
