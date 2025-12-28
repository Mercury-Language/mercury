%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% The first predicate in this test case is a simplified version of
% the message_type_to_string predicate in deep_profiler/message.m
% (as of 2025 nov 13).
%
% This test predicate looks at what happens when switch detection converts
% that predicate's nested disjunction not into a three-way switch
% (its traditional output) but into two nested switches, with each switch
% replacing one of the original disjunctions. (The outer switch having
% two arms, the first arm handling both msg_a and msg_b, with the second arm
% handling msg_c, and the inner switch having two arms for msg_a and msg_b
% respectively.)
%
% Both translations are valid, and in almost all situations, they are
% indistinguishable. The one exception found by a bootcheck was this predicate.
% The issue is that with the two-switch version, the call to string.format
% is *outside* the inner switch, meaning it has to handle both the msg_a
% and msg_b cases. These have different format strings, so the result is
% this message:
%
%   Warning: unknown format string in call to predicate `string.format'/3.
%
% and the inability of opt_format_call.m to optimize the call.
%
% The second predicate tests a different instance of the same problem,
% an instance that was raised during the discussion of the problem on m-dev.
% This version always got a warning from the compiler, with or without
% the recent change to switch_detection.m. This warning was shut up
% by the commit that added this test case.
%
%---------------------------------------------------------------------------%

:- module format_after_switch.
:- interface.

:- import_module cord.

%---------------------------------------------------------------------------%

:- type message_type
    --->    msg_a(int)
    ;       msg_b(int)
    ;       msg_c.

:- func message_type_to_string_a(message_type) = cord(string).

:- func message_type_to_string_b(message_type) = cord(string).

%---------------------------------------------------------------------------%

:- implementation.
:- import_module list.
:- import_module string.

message_type_to_string_a(MessageType) = Cord :-
    (
        (
            MessageType = msg_a(Num),
            Template = "A %d"
        ;
            MessageType = msg_b(Num),
            Template = "B %d"
        ),
        string.format(Template, [i(Num)], Str)
    ;
        MessageType = msg_c,
        Str = "C"
    ),
    Cord = singleton(Str).

message_type_to_string_b(MessageType) = Cord :-
    (
        MessageType = msg_a(Num),
        Template = "A %d"
    ;
        MessageType = msg_b(Num),
        Template = "B %d"
    ;
        MessageType = msg_c,
        Num = 42,
        Template = "C %d"
    ),
    string.format(Template, [i(Num)], Str),
    Cord = singleton(Str).

%---------------------------------------------------------------------------%
