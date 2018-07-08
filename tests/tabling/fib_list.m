%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fib_list.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module assoc_list.
:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials([1, 4], !IO).

:- pred perform_trials(list(int)::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    % io.write(N, !IO),
    % io.write_string(": ", !IO),
    % io.write_int(Time, !IO),
    % io.write_string("ms vs ", !IO),
    % io.write_int(MTime, !IO),
    % io.write_string("ms\n", !IO),
    ( if
        (
            Time > 10 * MTime,
            MTime > 0   % untabled takes ten times as long
        ;
            Time > 100, % untabled takes at least 100 ms
            MTime < 1   % while tabled takes at most 1 ms
        )
    then
        io.write_string("tabling works\n", !IO)
    else if
        Time > 10000        % untabled takes at least 10 seconds
    then
        io.write_string("tabling does not appear to work\n", !IO)
    else
        % We couldn't get a measurable result with N,
        % and it looks like we can afford a bigger trial
        perform_trials(add_digits(N, [3]), !IO)
    ).

:- pred trial(list(int)::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(list(int)::in, list(int)::out) is det.

fib(N, F) :-
    ( if digits_to_num(N) < 2 then
        F = num_to_digits(1)
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        fib(subtract_digits(N, One), F1),
        fib(subtract_digits(N, Two), F2),
        F = add_digits(F1, F2)
    ).

:- pred mfib(list(int)::in, list(int)::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( if digits_to_num(N) < 2 then
        F = num_to_digits(1)
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        mfib(subtract_digits(N, One), F1),
        mfib(subtract_digits(N, Two), F2),
        F = add_digits(F1, F2)
    ).

:- func add_digits(list(int), list(int)) = list(int).

add_digits(S1, S2) =
    num_to_digits(digits_to_num(S1) + digits_to_num(S2)).

:- func subtract_digits(list(int), list(int)) = list(int).

subtract_digits(S1, S2) =
    num_to_digits(digits_to_num(S1) - digits_to_num(S2)).

:- func digits_to_num(list(int)) = int.

digits_to_num(Digits) = Num :-
    list.reverse(Digits, RevDigits),
    Num = digits_to_num_2(RevDigits).

:- func digits_to_num_2(list(int)) = int.

digits_to_num_2([]) = 0.
digits_to_num_2([Last | Rest]) =
    10 * digits_to_num_2(Rest) + Last.

:- func num_to_digits(int) = list(int).

num_to_digits(Int) = Digits :-
    ( if Int < 10 then
        Digits = [Int]
    else
        Last = Int mod 10,
        Rest = Int // 10,
        list.append(num_to_digits(Rest), [Last], Digits)
    ).
