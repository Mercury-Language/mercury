%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module specified.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.

main(!IO) :-
    perform_trials(aplp_vs_vplp, [1, 4], 14, 3, 0, 0, !IO),
    perform_trials(apli_vs_vpli, [1, 4], 14, 3, 0, 0, !IO),
    perform_trials(vvll_vs_vpll, [4, 4, 4], 444, 30, 0, 0, !IO).

:- type trial_type
    --->    aplp_vs_vplp
    ;       apli_vs_vpli
    ;       vvll_vs_vpll.

:- pred perform_trials(trial_type::in, list(int)::in, int::in, int::in,
    int::in, int::in, io::di, io::uo) is cc_multi.

perform_trials(TrialType, ListN, IntN, Incr, NumDouble0, NumTrials0, !IO) :-
    trial(TrialType, ListN, IntN, Time, MTime, !IO),
    % io.write(TrialType, !IO),
    % io.write_string(" ", !IO),
    % io.write(IntN, !IO),
    % io.write_string(": ", !IO),
    % io.write_int(Time, !IO),
    % io.write_string("ms vs ", !IO),
    % io.write_int(MTime, !IO),
    % io.write_string("ms\n", !IO),
    ( if
        MTime > 10,
        Time > MTime * 2
    then
        NumDouble = NumDouble0 + 1
    else
        NumDouble = 0
    ),
    ( if
        (
            Time > 10 * MTime,
            MTime > 0   % "should be slower" version takes ten times as long
        ;
            Time > 100, % "should be slower" version takes at least 100 ms
            MTime < 1   % while "should be faster" version takes at most 1 ms
        ;
            NumDouble >= 10
                        % The "should be faster" version has been at least
                        % double the speed of the "should be slower" version
                        % for the last ten trials.
        )
    then
        io.write(TrialType, !IO),
        io.write_string(": tabling works\n", !IO)
    else if
        (
            Time > 10000        % "should be slower" takes at least 10 seconds
        ;
            NumTrials0 > 1000
        )
    then
        io.write(TrialType, !IO),
        io.write_string(": tabling does not appear to work\n", !IO)
    else
        % We couldn't get a measurable result with N,
        % and it looks like we can afford a bigger trial
        perform_trials(TrialType,
            add_digits(ListN, num_to_digits(Incr)), IntN + Incr, Incr,
            NumDouble, NumTrials0 + 1, !IO)
    ).

:- pred trial(trial_type::in, list(int)::in, int::in, int::out, int::out,
    io::di, io::uo) is cc_multi.

trial(TrialType, ListN, IntN, Time, MTime, !IO) :-
    (
        TrialType = aplp_vs_vplp,
        benchmark_det(ap_lp_fib_test, ListN - [42], Res, 1, Time),
        benchmark_det(vp_lp_fib_test, ListN - [42], MRes, 1, MTime)
    ;
        TrialType = apli_vs_vpli,
        benchmark_det(ap_li_fib_test, ListN - IntN, Res, 1, Time),
        benchmark_det(vp_li_fib_test, ListN - IntN, MRes, 1, MTime)
    ;
        TrialType = vvll_vs_vpll,
        table_reset_for_vv_ll_fib_3(!IO),
        table_reset_for_vp_ll_fib_3(!IO),
        benchmark_det(vv_ll_fib_test, ListN - ListN, Res, 1, Time),
        benchmark_det(vp_ll_fib_test, ListN - ListN, MRes, 1, MTime)
    ),
    require(unify(Res, MRes), "tabling produces wrong answer").

%---------------------------------------------------------------------------%

:- pred ap_lp_fib_test(pair(list(int), T)::in, list(int)::out) is det.

ap_lp_fib_test(N - Dummy, F) :-
    ap_lp_fib(N, Dummy, F).

:- pred vp_lp_fib_test(pair(list(int), T)::in, list(int)::out) is det.

vp_lp_fib_test(N - Dummy, F) :-
    vp_lp_fib(N, Dummy, F).

:- pred ap_li_fib_test(pair(list(int), int)::in, list(int)::out) is det.

ap_li_fib_test(N - CopyN, F) :-
    ap_li_fib(N, CopyN, F).

:- pred vp_li_fib_test(pair(list(int), int)::in, list(int)::out) is det.

vp_li_fib_test(N - CopyN, F) :-
    vp_li_fib(N, CopyN, F).

:- pred vp_ll_fib_test(pair(list(int), list(int))::in, list(int)::out) is det.

vp_ll_fib_test(N - CopyN, F) :-
    vp_ll_fib(N, CopyN, F).

:- pred vv_ll_fib_test(pair(list(int), list(int))::in, list(int)::out) is det.

vv_ll_fib_test(N - CopyN, F) :-
    vv_ll_fib(N, CopyN, F).

%---------------------------------------------------------------------------%

:- pred ap_lp_fib(list(int)::in, T::in, list(int)::out) is det.
:- pragma memo(ap_lp_fib(in, in, out),
    [allow_reset, statistics, specified([addr, promise_implied, addr])]).

ap_lp_fib(N, Dummy, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        F = num_to_digits(1)
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        ap_lp_fib(subtract_digits(N, One), Dummy, F1),
        ap_lp_fib(subtract_digits(N, Two), Dummy, F2),
        F = add_digits(F1, F2)
    ).

:- pred vp_lp_fib(list(int)::in, T::in, list(int)::out) is det.
:- pragma memo(vp_lp_fib/3,
    [allow_reset, statistics, specified([value, implied, output])]).

vp_lp_fib(N, Dummy, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        F = num_to_digits(1)
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        vp_lp_fib(subtract_digits(N, One), Dummy, F1),
        vp_lp_fib(subtract_digits(N, Two), Dummy, F2),
        F = add_digits(F1, F2)
    ).

:- pred ap_li_fib(list(int)::in, int::in, list(int)::out) is det.
:- pragma memo(ap_li_fib(in, in, out),
    [allow_reset, statistics, specified([addr, promise_implied])]).

ap_li_fib(N, CopyN, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        ( if RawN = CopyN then
            F = num_to_digits(1)
        else
            error("ap_li_fib")
        )
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        ap_li_fib(subtract_digits(N, One), RawN - 1, F1),
        ap_li_fib(subtract_digits(N, Two), RawN - 2, F2),
        F = add_digits(F1, F2)
    ).

:- pred vp_li_fib(list(int)::in, int::in, list(int)::out) is det.
:- pragma memo(vp_li_fib/3,
    [allow_reset, statistics, specified([value, promise_implied, output])]).

vp_li_fib(N, CopyN, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        ( if RawN = CopyN then
            F = num_to_digits(1)
        else
            error("vp_li_fib")
        )
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        vp_li_fib(subtract_digits(N, One), CopyN - 1, F1),
        vp_li_fib(subtract_digits(N, Two), CopyN - 2, F2),
        F = add_digits(F1, F2)
    ).

:- pred vp_ll_fib(list(int)::in, list(int)::in, list(int)::out) is det.
:- pragma memo(vp_ll_fib/3,
    [allow_reset, statistics, specified([output, value, promise_implied])]).

vp_ll_fib(N, CopyN, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        ( if RawN = digits_to_num(CopyN) then
            F = num_to_digits(1)
        else
            error("vp_ll_fib")
        )
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        vp_ll_fib(subtract_digits(N, One), subtract_digits(N, One), F1),
        vp_ll_fib(subtract_digits(N, Two), subtract_digits(N, Two), F2),
        F = add_digits(F1, F2)
    ).

:- pred vv_ll_fib(list(int)::in, list(int)::in, list(int)::out) is det.
:- pragma memo(vv_ll_fib/3,
    [allow_reset, statistics, specified([value, value, output, output])]).

vv_ll_fib(N, CopyN, F) :-
    RawN = digits_to_num(N),
    ( if RawN < 2 then
        ( if RawN = digits_to_num(CopyN) then
            F = num_to_digits(1)
        else
            error("vv_ll_fib")
        )
    else
        One = num_to_digits(1),
        Two = num_to_digits(2),
        vv_ll_fib(subtract_digits(N, One), subtract_digits(N, One), F1),
        vv_ll_fib(subtract_digits(N, Two), subtract_digits(N, Two), F2),
        F = add_digits(F1, F2)
    ).

%---------------------------------------------------------------------------%

:- func add_digits(list(int), list(int)) = list(int).

add_digits(S1, S2) =
    num_to_digits(digits_to_num(S1) + digits_to_num(S2)).

:- func mul_digits(list(int), list(int)) = list(int).

mul_digits(S1, S2) =
    num_to_digits(digits_to_num(S1) * digits_to_num(S2)).

:- func subtract_digits(list(int), list(int)) = list(int).

subtract_digits(S1, S2) =
    num_to_digits(digits_to_num(S1) - digits_to_num(S2)).

:- func digits_to_num(list(int)) = int.

digits_to_num(Digits) = Num :-
    list.reverse(Digits, RevDigits),
    Num = digits_to_num_2(RevDigits).

:- func digits_to_num_2(list(int)) = int.
:- pragma memo(digits_to_num_2/1,
    [specified([addr, output], hidden_arg_addr, hidden_arg_addr)]).

digits_to_num_2([]) = 0.
digits_to_num_2([Last | Rest]) =
    10 * digits_to_num_2(Rest) + Last.

:- func num_to_digits(int) = list(int).
:- pragma memo(num_to_digits/1,
    [specified([value, output], voodoo)]).

num_to_digits(Int) = Digits :-
    ( if Int < 10 then
        Digits = [Int]
    else
        Last = Int mod 10,
        Rest = Int // 10,
        list.append(num_to_digits(Rest), [Last], Digits)
    ).
