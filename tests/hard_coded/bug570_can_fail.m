%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A version of the bug570 test case that shows why the bug cannot occur
% directly in user-written code. (The explanation for this is available
% in a comment mentioning this file in compiler/simplify_goal_conj.m.)
%

:- module bug570_can_fail.
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
    test(prepare_send, Value, !IO),
    io.nl(!IO),
    test(prepare_edit, Value, !IO),
    io.nl(!IO),
    test(prepare_postpone, Value, !IO).

:- pred test(prepare_temp::in, header_value::in, io::di, io::uo) is det.

test(Prepare, Value, !IO) :-
    io.format("Prepare: %s\n", [s(string.string(Prepare))], !IO),
    ( if make_headers(Prepare, Value, Headers) then
        list.foldl(io.print_line, Headers, !IO)
    else
        io.write_string("no headers\n", !IO)
    ).

:- type prepare_temp
    --->    prepare_send
    ;       prepare_edit
    ;       prepare_postpone.

:- pred make_headers(prepare_temp::in, header_value::in, list(header)::out)
    is semidet.

make_headers(Prepare, Value, Headers) :-
    some [!Acc] (
        !:Acc = [],
        (
            Prepare = prepare_send,
            cons(header(field_name("Message-ID"), Value), !Acc)
        ;
            Prepare = prepare_postpone
        ),
        (
            Prepare = prepare_send,
            SkipEmpty = yes
        ;
            ( Prepare = prepare_postpone
            ; Prepare = prepare_edit        % This gets a warning, which is OK.
            ),
            SkipEmpty = no
        ),
        maybe_cons(SkipEmpty, !Acc),
        Headers = !.Acc
    ).

:- pred maybe_cons(bool::in, list(header)::in, list(header)::out)
    is det.

maybe_cons(SkipEmpty, !Acc) :-
    (
        SkipEmpty = no,
        !:Acc = [header(field_name("skip_empty"), header_value("no")) | !.Acc]
    ;
        SkipEmpty = yes,
        !:Acc = [header(field_name("skip_empty"), header_value("yes")) | !.Acc]
    ).
