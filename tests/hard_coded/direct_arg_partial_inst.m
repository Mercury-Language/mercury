%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A construction of a direct argument functor with a free argument was
% completely ignored. This test case caused a compiler abort during LLDS
% code generation.
%

:- module direct_arg_partial_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type fruit
    --->    lemon(struct)   % direct arg functor
    ;       apple(struct)   % direct arg functor
    ;       orange(string).

:- type struct
    --->    struct(int, int, int, int, int).

:- pred foo(list(fruit)::in, string::out) is det.

foo(Xs, R) :-
    ( if
        % X was not recorded in the var_state_map.
        X = apple(_),
        % Compiler tries to flush X to the stack.
        list.member(X, Xs)
    then
        R = "found"
    else
        R = "not found"
    ).

main(!IO) :-
    ListA = [apple(struct(1, 2, 3, 4, 5))],
    foo(ListA, ResultA),
    io.write_string(ResultA, !IO),
    io.nl(!IO),

    ListB = [lemon(struct(1, 2, 3, 4, 5))],
    foo(ListB, ResultB),
    io.write_string(ResultB, !IO),
    io.nl(!IO).
