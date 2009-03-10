%-----------------------------------------------------------------------------%
% Try goal without I/O, can fail.

:- module try_syntax_1.
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
    ;       act_fail
    ;       act_throw_int
    ;       act_throw_string
    ;       act_throw_primate.

:- type primate
    --->    stupid_monkey
    ;       stupid_human.

main(!IO) :-
    io.write_string("Should succeed: ", !IO),
    run_test(act_succeed, !IO),

    io.write_string("Should fail: ", !IO),
    run_test(act_fail, !IO),

    io.write_string("Should throw an int: ", !IO),
    run_test(act_throw_int, !IO),

    io.write_string("Should throw a string: ", !IO),
    run_test(act_throw_string, !IO),

    io.write_string("Should throw a primate: ", !IO),
    run_test(act_throw_primate, !IO).

:- pred run_test(action::in, io::di, io::uo) is cc_multi.

run_test(Action, !IO) :-
    Input = input,
    (try [] (
        Output = Input ++ "-coated",
        (
            Action = act_succeed
        ;
            Action = act_fail,
            fail
        ;
            Action = act_throw_int,
            throw(90210)
        ;
            Action = act_throw_string,
            throw("up")
        ;
            Action = act_throw_primate,
            throw(stupid_monkey)
        )
    )
    then
        X = Output : string
    else
        X = "failed"
    catch E ->
        X = "caught int: " ++ string.from_int(E)
    catch E ->
        X = "caught string: " ++ E
    catch_any Other ->
        X = "caught other type: " ++ string(Other)
    ),
    io.write_string(X, !IO),
    io.nl(!IO).

:- func input = string.
:- pragma no_inline(input/0).

input = "sugar".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
