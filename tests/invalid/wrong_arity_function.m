%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% When generating an error message for the use of a function symbol with
% the wrong arity and listing the possible arities, the compiler must report
% the arities of functions of the same name, as well as the arities of the
% data constructors of the same name. Prior to 2016 jun 22, it was not
% not doing that.
%

:- module wrong_arity_function.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    % The message for this error should include
    % - the arity 1 data constructor "f" in string.poly_type, and
    % - the arity 2 function "f" defined below
    % in the main part, and
    % - the arity 2 function "f" defined below,
    % - the arity 3 predicate "f" defined below, and
    % - the arity 4 predicate "f" defined below
    % in the "if you are trying to construct a closure" part.
    X = f(1, 2, 3, 4, 5),
    io.format("X = %d\n", [i(X)], !IO).

:- func f(int, int) = int.

f(A, B) = A + B.

:- pred f(int::in, int::in, int::out) is det.

f(A, B, AB) :-
    AB = A + B.

:- pred f(int::in, int::in, int::in, int::out) is det.

f(A, B, C, ABC) :-
    ABC = A + B + C.
