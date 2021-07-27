%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% A regression test.
% This tests a case where the compiler marked cells as being compile time
% garbage collectable, where references to that cell existed in other
% data structures.
%---------------------------------------------------------------------------%

:- module interpret.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module map.
:- import_module float.
:- import_module int.
:- import_module require.

:- type element
    --->    float(float)
    ;       int(int).

:- type operation
    --->    addf
    ;       addi
    ;       float(float)
    ;       lookup
    ;       pop.

:- type stack == list(element).
:- type env == map(int, element).

main(!IO) :-
    Env = map.set(map.init, 1, int(5)),
    Stack0 = [int(1)],
    Ops = [lookup, float(3.14)],
    interpret(Ops, Env, Stack0, Stack1),

    % This list must be of at least length two to as the first cell
    % is correctly marked as not being cgc'able.
    ( if Stack1 = [float(_), int(X0)] then
        X = X0
    else
        error("incorrect stack")
    ),

    % XXX If int(X0) is incorrectly being marked as cgc'able,
    % then P will reuse it's memory and hence the later map.lookup
    % will return int(3) instead of int(5).
    P = int(3),
    io.write_line(P, !IO),
    map.lookup(Env, 1, Q),
    ( if Q = int(X) then
        io.write_string("Element of map hasn't changed.\n", !IO)
    else
        io.write_string("BEEP! BEEP! Map changed!!!.\n", !IO)
    ).

:- pred interpret(list(operation)::in, env::in, stack::in, stack::out) is det.

interpret([], _, Stack, Stack).
interpret([Op | Ops], Env, Stack0, Stack) :-
    do_op(Op, Env, Stack0, Stack1),
    interpret(Ops, Env, Stack1, Stack).

:- pred do_op(operation::in, env::in, stack::in, stack::out) is det.

do_op(float(F), _Env, Stack, [float(F) | Stack]).
do_op(addi, _Env, Stack0, Stack) :-
    ( if Stack0 = [int(A), int(B) | Stack1] then
        Stack = [int(A+B) | Stack1]
    else
        throw(Stack0)
    ).
do_op(addf, _Env, Stack0, Stack) :-
    ( if Stack0 = [float(A), float(B) | Stack1] then
        Stack = [float(A+B) | Stack1]
    else
        error("addi: wrong arguments")
    ).
do_op(lookup, Env, Stack0, Stack) :-
    ( if Stack0 = [int(Loc) | Stack1] then
        % Here we create an alias between the Env variable,
        % and the elements in the stack.
        map.lookup(Env, Loc, Element),
        Stack = [Element | Stack1]
    else
        error("lookup: wrong arguments")
    ).
do_op(pop, _Env, Stack0, Stack) :-
    ( if Stack0 = [_ | Stack1] then
        Stack = Stack1
    else
        error("pop: no arguments on the stack")
    ).
