%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case contributed by C4Cypher.
%
% This is a test case for a rare bug in the LLDS optimizer.
% The symptom was a compiler abort when processing the is_bar predicate,
% which is a switch that cover all the barN function symbols of the foo type.
%
% The bug happened because of the following sequence of events.
%
% - The code generator generates a computed goto for on the primary tag
%   of the one input arg, with one of the labels leading to another computed
%   goto on the remote secondary tag.
%
% - The early optimization phases convert this structure to a simple test:
%   if the primary tag is zero, go to a label that assigns 0 to r1 and returns,
%   and otherwise, go to a label that assigns 1 to r1 and returns.
%
% - The second invocation of jump optimization converts this structure
%   to code that assigns the value of the C expression "tag(r1) != 0" to r1,
%   and returns. Crucially, it left behind two separate kinds of dead code.
%   The first of the original semidet epilogs got left as code behind an
%   unconditional jump, while the second was left as code after a label
%   that had no more jumps targeting it.
%
% - Everything above was perfectly acceptable. What was not acceptable,
%   and what caused the bug, was that the optimize_middle predicate in
%   optimize.m, after invoking jump optimization for the second time,
%   did not call the passes that clean up such dead code. This dead code
%   then caused the compiler abort by triggering a sanity check during
%   the use_local_vars pass.
%
%---------------------------------------------------------------------------%

:- module gh133.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------%

:- type foo
    --->    some [T] exist_foo(T)
    ;       bar01(int)
    ;       bar02(int)
    ;       bar03(int)
    ;       bar04(int)
    ;       bar05(int)
    ;       bar06(int)
    ;       bar07(int)
    ;       bar08(int)
    ;       bar09(int)
    ;       bar10(int)
    ;       bar11(int)
    ;       bar12(int).

:- type bar =< foo
    --->    bar01(int)
    ;       bar02(int)
    ;       bar03(int)
    ;       bar04(int)
    ;       bar05(int)
    ;       bar06(int)
    ;       bar07(int)
    ;       bar08(int)
    ;       bar09(int)
    ;       bar10(int)
    ;       bar11(int)
    ;       bar12(int).

:- inst bar
    --->    bar01(ground)
    ;       bar02(ground)
    ;       bar03(ground)
    ;       bar04(ground)
    ;       bar05(ground)
    ;       bar06(ground)
    ;       bar07(ground)
    ;       bar08(ground)
    ;       bar09(ground)
    ;       bar10(ground)
    ;       bar11(ground)
    ;       bar12(ground).

:- pred is_bar(foo::(ground >> bar)) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    ( if
        X:foo = bar01(5),
        is_bar(X),
        Y:bar = coerce(X)
    then
        io.format("Test succeeded. Y = %s\n", [s(string(Y))], !IO)
    else
        io.write_string("Test failed.\n", !IO)
    ).

is_bar(T) :-
    ( T = bar01(_)
    ; T = bar02(_)
    ; T = bar03(_)
    ; T = bar04(_)
    ; T = bar05(_)
    ; T = bar06(_)
    ; T = bar07(_)
    ; T = bar08(_)
    ; T = bar09(_)
    ; T = bar10(_)
    ; T = bar11(_)
    ; T = bar12(_)
    ).
