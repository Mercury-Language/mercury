%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program tests for the presence of Mantis bug #570.
%
% For the description of what led to this bug, see the comment mentioning
% this test case in compiler/simplify_goal_conj.m.
%

:- module bug570.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
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
    Value = header_value("value"),
    make_headers(prepare_send, Value, Headers),
    list.foldl(io.print_line, Headers, !IO).

:- type prepare_temp
    --->    prepare_send
    ;       prepare_edit
    ;       prepare_postpone.

:- pred make_headers(prepare_temp::in, header_value::in, list(header)::out)
    is det.

make_headers(Prepare, Value, Headers) :-
    some [!Acc] (
        !:Acc = [],
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            cons(header(field_name("Date"), Value), !Acc)
        ;
            Prepare = prepare_edit
        ),
        prevent_merge_switches(!Acc),
        (
            Prepare = prepare_send,
            cons(header(field_name("Message-ID"), Value), !Acc)
        ;
            ( Prepare = prepare_edit
            ; Prepare = prepare_postpone
            )
        ),
        prevent_merge_switches(!Acc),
        (
            ( Prepare = prepare_send
            ; Prepare = prepare_postpone
            ),
            SkipEmpty = yes
        ;
            Prepare = prepare_edit,
            SkipEmpty = no
        ),
        maybe_cons(SkipEmpty, header(field_name("Subject"), Value), !Acc),
        Headers = !.Acc
    ).

:- pred prevent_merge_switches(list(header)::in, list(header)::out) is det.
:- pragma no_inline(pred(prevent_merge_switches/2)).

prevent_merge_switches(!Acc).

:- pred maybe_cons(bool::in, header::in, list(header)::in, list(header)::out)
    is det.
:- pragma no_inline(pred(maybe_cons/4)).

maybe_cons(_, X, Xs, [X | Xs]).
