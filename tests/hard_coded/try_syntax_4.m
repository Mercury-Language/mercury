%-----------------------------------------------------------------------------%
% Try goal with I/O

:- module try_syntax_4.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type action
    --->    act_succeed
    ;       act_throw.

main(!IO) :-
    io.write_string("Begin non-throwing test:\n", !IO),
    run_test(act_succeed, !IO),
    io.nl(!IO),
    io.write_string("Begin throwing test:\n", !IO),
    run_test(act_throw, !IO).

:- pred run_test(action::in, io::di, io::uo) is cc_multi.

run_test(Action, !IO) :-
    (try [io(!IO)] (
        do_some_io("checkpoint (a)\n", !IO),
        Output = "some output",
        (
            Action = act_succeed
        ;
            Action = act_throw,
            throw(90210)
        ),
        do_some_io("checkpoint (b)\n", !IO)
    )

    then
        do_some_io("checkpoint (c)\n", !IO),
        X = Output

    % Else branch not allowed when using I/O.

    catch E:int ->
        X = "caught int: " ++ string(E),
        io.write_string("checkpoint (d)\n", !IO)

    catch E:string ->
        X = "caught string: " ++ E,
        io.write_string("checkpoint (e)\n", !IO)

    catch_any Other ->
        X = "caught: " ++ string(Other),
        io.write_string("checkpoint (f)\n", !IO)
    ),
    io.write_string(X, !IO),
    io.nl(!IO).

:- pred do_some_io(string::in, io::di, io::uo) is det.

do_some_io(String, !IO) :-
    io.write_string(String, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
