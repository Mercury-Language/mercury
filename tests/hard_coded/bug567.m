%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests for the presence of Mantis bug #567. The bug symptom
% was that the output of this program, which was expected to be
%
% header(field_name("Date"),
%   header_value("Tue, 7 Nov 2023 12:29:43 +1100"))
% header(field_name("Message-ID"),
%   header_value("<20231107122943.GB219@example>"))
%
% but the actual output was
%
% header(field_name("Message-ID"),
%   header_value("Tue, 7 Nov 2023 12:29:43 +1100"))
% header(field_name("Message-ID"),
%   header_value("<20231107122943.GB219@example>"))
%
% (We show each header term on two lines to avoid going over 80 columns.)
%
% The cause of the bug was an interaction between
%
% - the optimization that merges two consective switches on the same variable,
%   specifically, the two switches on Prepare in make_headers below, and
%
% - the optimization that replaces sequences of unifications that construct
%   ground terms with a single unification whose right hand side is a
%   ground_term_const.
%
% The bug was in the simplify_conj predicate in simplify_goal_conj.m.
% The timeline of the bug was:
%
% - simplify_conj calls simplify_goal on the first switch on Prepare.
%   This call replaces the code constructing the term "field_name("Date")"
%   with a reference to a new ground_term_const. The presence of this
%   new ground_term_const is recorded in the updated !:Common returned by
%   simplify_goal.
%
% - simplify_conj sees that the next goal is also a switch on Prepare.
%   It merges the updated form of the first switch on Prepare with the
%   original form of this second switch on Prepare.
%
% - simplify_conj recursively invokes itself on a list of goals that includes
%   the merged switch, specifying the value of !Common from *before*
%   simplifying HeadGoal0 into HeadGoal1. This leaves the ground_term_const
%   in the merged switch, copied from HeadGoal1, dangling. This is the bug.
%
% - We get the symptom we get because the ground_term_const id that
%   the simplification of HeadGoal0 into HeadGoal1 allocated to the term
%   "field_name("Date")" gets allocated to hold the term
%   "field_name("Message-ID")" by the recursive call.
%
%---------------------------------------------------------------------------%

:- module bug567.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- type header
    --->    header(field_name, header_value).

:- type field_name
    --->    field_name(string).

:- type header_value
    --->    header_value(string).

:- type message_id
    --->    message_id(string).

%---------------------------------------------------------------------------%

main(!IO) :-
    Date = header_value("Tue, 7 Nov 2023 12:29:43 +1100"),
    MessageId = message_id("20231107122943.GB219@example"),
    make_headers(prepare_send, Date, MessageId, Headers),
    list.foldl(io.print_line, Headers, !IO).

:- type prepare_temp
    --->    prepare_send
    ;       prepare_edit
    ;       prepare_postpone.

:- pred make_headers(prepare_temp::in, header_value::in, message_id::in,
    list(header)::out) is det.

make_headers(Prepare, Date, MessageId, Headers) :-
    some [!Acc] (
        !:Acc = [],
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            cons(header(field_name("Date"), Date), !Acc)
        ;
            Prepare = prepare_edit
        ),
        (
            Prepare = prepare_send,
            cons(header(field_name("Message-ID"),
                wrap_angle_brackets(MessageId)), !Acc)
        ;
            ( Prepare = prepare_edit
            ; Prepare = prepare_postpone
            )
        ),
        list.reverse(!.Acc, Headers)
    ).

:- func wrap_angle_brackets(message_id) = header_value.

wrap_angle_brackets(message_id(MessageId)) =
    header_value("<" ++ MessageId ++ ">").
