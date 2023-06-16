%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test. Versions of the compiler before 2023 april 19
% generated a quite misleading error message for the bug in test_pred.
%
% The bug is that the switch on Str has an arm that is explicitly for
% "Str = k", but this test is *also* present in the first switch arm,
% which is now for a, b and k. This can come about naturally when
%
% - k used to be treated the same way as a and b;
% - the programmer decides to give k its own switch arm, but
% - then forgets to delete the "Str = k" from its original switch arm.
%
% (That is exactly what happened with the original code that inspired
% this cut-down test case.)
%
% The original version of the compiler generated two errors for this bug:
%
%   Warning: this disjunct will never have any solutions.
%   Error: the goal inside the require_complete_switch [Str] scope is
%     not a switch on `Str'.
%
% The contexts were the position of the first Str = k in the code
% (the line marked below as THE PROBLEM LINE), and the position of the
% first require_complete_switch scope respectively. These are confusing
% because
%
% - there is not supposed to be a disjunct at THE PROBLEM LINE, since
%   switch detection is supposed to transform that disjunct into a switch arm,
%   and
% - the body of the require_complete_switch [Str] scope IS in fact a switch
%   on Str.
%
% The reason why these error messages are misleading is that despite
% the contexts, they do not refer to that piece of code at all. They refer
% to a *copy* of that code. When switch detection sees that there are
% *two* switch arms for Str = k, it creates a separate arm of the switch
% on Str for the Str = k case, and sets the code of this arm to be
% a two-way disjunction containing the code that came after the two
% occurrences of Str = k. This meant that the second disjunct was
% "Msg = arg_k", and the first disjunct was this:
%
%   BaseMsg = "arm_ab_",
%   require_complete_switch [Str]
%   (
%       Str = "a",
%       Msg = BaseMsg ++ "arm_a"
%   ;
%       Str = "b",
%       Msg = BaseMsg ++ "arm_b"
%   )
%
% The compiler recognized that *none* of those two switch arms of this *inner*
% switch is actually reachable in the Str = k branch of the *outer* switch
% on Str, so it replaced this switch with fail, which meant that this disjunct
% couldn't succeed (hence the first error message). The fact that it replaced
% the switch with fail but kept the require_complete_switch scope led to the
% second error message.
%
% So both the error messages the compiler generates are justified, including
% the line numbers attached to them. What made them misleading was the lack
% of any reference to the fact that the error occurs only inside the Str = k
% arm of the outer switch. The fix for that is to print this context.

:- module require_complete_nested_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    ( if test_pred("abc", Msg) then
        io.format("Success: %s\n", [s(Msg)], !IO)
    else
        io.format("Failure\n", [], !IO)
    ).

:- pred test_pred(string::in, string::out) is semidet.

test_pred(Str, Msg) :-
    (
        ( Str = "a"
        ; Str = "b"
        ; Str = "k" % THE PROBLEM LINE
        ),
        BaseMsg = "arm_ab_",
        require_complete_switch [Str]
        (
            Str = "a",
            Msg = BaseMsg ++ "arm_a"
        ;
            Str = "b",
            Msg = BaseMsg ++ "arm_b"
        )
    ;
        Str = "k",
        Msg = "arg_k"
    ;
        ( Str = "x"
        ; Str = "y"
        ),
        Msg = "arg_x_y"
    ).
