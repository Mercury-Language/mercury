%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test ensures that functions can be declared with a determinism,
% but without the modes. The compiler should give them the default
% modes of `in' for the arguments and `out' for the return value.
%
% We test :- func definitions, lambda expressions and type class methods.

:- module func_default_modes.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- func bar(int) = (int) is semidet.

:- typeclass tc(T) where [
    func method1(T) = int is semidet
].

:- instance tc(int) where [
    func(method1/1) is bar
].

main(!IO) :-
    A = (func(X) = Y is semidet :- X = 4, Y = 7),
    B = (func(X) = (Y) is semidet :- Y = method1(X)),
    io.write_line(list.filter_map(A, [1, 2, 3, 4, 5, 6]), !IO),
    io.write_line(list.filter_map(bar, [1, 2, 3, 4, 5, 6]), !IO),
    io.write_line(list.filter_map(B, [1, 2, 3, 4, 5, 6]), !IO).

bar(4) = 7.
