%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test implementation-defined literals, e.g. $file.

:- module impl_def_literal.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module impl_def_literal.sub.
:- import_module string.

main(!IO) :-
    io.write_string($file, !IO),
    io.nl(!IO),
    io.write_int($line, !IO),
    io.nl(!IO),
    io.write_string($module, !IO),
    io.nl(!IO),
    io.write_string($pred, !IO),
    io.nl(!IO),
    io.write_string(a_function, !IO),
    io.nl(!IO),
    a_lambda(FromLambda),
    io.write_string(FromLambda, !IO),
    io.nl(!IO),
    a_try_goal(FromTryGoal),
    io.write_string(FromTryGoal, !IO),
    io.nl(!IO),
    an_atomic_goal(FromAtomicGoal, !IO),
    io.write_string(FromAtomicGoal, !IO),
    io.nl(!IO),

    fun_with_lines(!IO),
    fun_with_lines_2(!IO),

    % We don't actually write out the grade string so as not to make the
    % expected output grade-dependent.
    ( if string.length($grade) = 0 then
        io.write_string("huh?\n", !IO)
    else
        io.write_string("have $grade\n", !IO)
    ),

    in_submodule(!IO),

    % Test literals in instance methods.
    tc_p(1, 'a', tt(0), !IO),

    tc_f(tt(0), 'b', 2) = F1,
    io.write_string(F1, !IO),
    io.nl(!IO),

    tc_p("a", "b", "c", !IO),

    tc_f("a", "b", "c") = F2,
    io.write_string(F2, !IO),
    io.nl(!IO).

:- func a_function = string.

a_function = $pred.

:- pred a_lambda(string::out) is det.

a_lambda(String) :-
    Pred = (pred($pred::out) is det),
    Pred(String).

:- pred a_try_goal(string::out) is cc_multi.

a_try_goal(String) :-
    (try []
        String = $pred
    then
        true
    ).

:- pred an_atomic_goal(string::out, io::di, io::uo) is det.

an_atomic_goal(String, !IO) :-
    atomic [outer(!IO), inner(!STM)] (
        String = $pred
    ).

:- pred fun_with_lines(io::di, io::uo) is det.

fun_with_lines(!IO) :-
    X = $line,
    Y = $line,
    ( if X = Y then
        io.write_string("fun_with_lines: equal\n", !IO)
    else
        io.write_string("fun_with_lines: unequal\n", !IO)
    ).

:- pred fun_with_lines_2(io::di, io::uo) is det.

fun_with_lines_2(!IO) :-
    % The user probably expects the two occurrences of $line to be replaced
    % by two different numbers. That didn't happen in the past, but
    % now it does.
    ( if
        $line =
        $line
    then
        io.write_string("fun_with_lines_2: equal\n", !IO)
    else
        io.write_string("fun_with_lines_2: unequal\n", !IO)
    ).

:- typeclass tc(A, B, C) where [
    pred tc_p(A::in, B::in, C::in, io::di, io::uo) is det,
    func tc_f(C, B, A) = string
].

%---------------------------------------------------------------------------%

    :- module sub.
    :- interface.

    :- pred in_submodule(io::di, io::uo) is det.

    :- type tt(T)
        --->    tt(T).

    :- instance tc(int, character, tt(T)).
    :- instance tc(string, string, string).

    :- implementation.

    in_submodule(!IO) :-
        io.write_string($module, !IO),
        io.nl(!IO),
        io.write_string($pred, !IO),
        io.nl(!IO),
        io.write_string($file, !IO),
        io.nl(!IO),
#10101
        io.write_int($line, !IO),
        io.nl(!IO).

    :- instance tc(int, character, tt(T)) where [
        ( tc_p(_, _, _, !IO) :-
            io.write_string($pred, !IO),
            io.nl(!IO)
        ),
        ( tc_f(_, _, _) = $pred )
    ].

    :- instance tc(string, string, string) where [
        pred(tc_p/5) is string_p,
        func(tc_f/3) is string_f
    ].

    :- pred string_p(string::in, string::in, string::in, io::di, io::uo)
        is det.

    string_p(_, _, _, !IO) :-
        io.write_string($pred, !IO),
        io.nl(!IO).

    :- func string_f(string, string, string) = string.

    string_f(_, _, _) = $pred.

    :- end_module sub.
