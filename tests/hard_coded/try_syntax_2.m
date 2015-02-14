%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Try goal without I/O, cannot fail.

:- module try_syntax_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.

%---------------------------------------------------------------------------%

:- type action
    --->    act_succeed
    ;       act_throw.

main(!IO) :-
    io.write_string("Should succeed: ", !IO),
    run_test(act_succeed, !IO),

    io.write_string("Should throw: ", !IO),
    run_test(act_throw, !IO).

:- pred run_test(action::in, io::di, io::uo) is cc_multi.

run_test(Action, !IO) :-
    Input = input,
    ( try [] (
        Output = Input ++ "-coated",
        (
            Action = act_succeed
        ;
            Action = act_throw,
            throw("up")
        )
    )
    then
        X = Output
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
