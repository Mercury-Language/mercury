%-----------------------------------------------------------------------------%
% qsort using C implementation of Mersenne-Twister pseudo-RNG
%

:- module qsort_mt.

:- interface.

:- impure pred main(io::di, io::uo) is det.

:- import_module io.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mt.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    % Generate 100000 random numbers.
    rand_list(123, 100000, Rands),
    %io.print("rand done\n", !IO),

    io.command_line_arguments(Args, !IO),
    (
        (
            Args = [ModePrime, RepeatsStr, TimeFileNamePrime],
            string.to_int(RepeatsStr, RepeatsPrime)
        ;
            Args = [ModePrime, RepeatsStr],
            string.to_int(RepeatsStr, RepeatsPrime),
            TimeFileNamePrime = ""
        ;
            Args = [ModePrime],
            RepeatsPrime = 1,
            TimeFileNamePrime = ""
        )
    ->
        Mode = ModePrime,
        Repeats = RepeatsPrime,
        TimeFileName = TimeFileNamePrime
    ;
        error("bad command")
    ),
    impure gettimeofday(Start),
    run_n_tests(Repeats, Mode, Rands, Sort),
    impure gettimeofday(End),
    trace [compile_time(flag("checksort"))] (
        ( if  check_is_sorted(Sort) then
            true
        else
            error("not sorted")
        )
    ),
    % io.write_int(length(Sort), !IO),
    % take_upto(10, Sort, T), io.print(T, !IO),
    io.open_output(TimeFileName, Result, !IO),
    (
        Result = ok(Stream),
        io.write_int(Stream, End - Start, !IO),
        io.nl(Stream, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

:- pred run_n_tests(int::in, string::in, list(int)::in, list(int)::out) is det.

run_n_tests(N, Mode, Rands, Sort) :-
    ( Mode = "app_seq" ->
        qsortapp_seq(Rands, ThisSort)
    ; Mode = "acc_seq" ->
        qsortacc_seq(Rands, [], ThisSort)

    ; Mode = "app_par_deep" ->
        qsortapp_par_deep(Rands, ThisSort)
    ; Mode = "app_par_limit" ->
        qsortapp_par_limit(Rands, ThisSort)

    ; Mode = "acc_par_deep" ->
        qsortacc_par_deep(Rands, [], ThisSort)
    ; Mode = "acc_par_limit" ->
        qsortacc_par_limit(Rands, [], ThisSort)

    ; Mode = "app_par_shallow" ->
        qsortapp_par_shallow(Rands, ThisSort)
    ; Mode = "app_par_shallow2" ->
        qsortapp_par_shallow2(Rands, ThisSort)
    ; Mode = "app_par_shallow3" ->
        qsortapp_par_shallow3(Rands, ThisSort)
    ; Mode = "app_par_shallow4" ->
        qsortapp_par_shallow4(Rands, ThisSort)

    ; Mode = "appacc_par_shallow" ->
        qsortappacc_par_shallow(Rands, ThisSort)
    ; Mode = "appacc_par_shallow2" ->
        qsortappacc_par_shallow2(Rands, ThisSort)
    ; Mode = "appacc_par_shallow3" ->
        qsortappacc_par_shallow3(Rands, ThisSort)
    ; Mode = "appacc_par_shallow4" ->
        qsortappacc_par_shallow4(Rands, ThisSort)

    ; Mode = "acc_par_shallow" ->
        qsortacc_par_shallow(Rands, [], ThisSort)
    ; Mode = "acc_par_shallow2" ->
        qsortacc_par_shallow2(Rands, [], ThisSort)
    ; Mode = "acc_par_shallow3" ->
        qsortacc_par_shallow3(Rands, [], ThisSort)
    ; Mode = "acc_par_shallow4" ->
        qsortacc_par_shallow4(Rands, [], ThisSort)

    ; Mode = "acc2_par_shallow" ->
        qsortacc2_par_shallow(Rands, [], ThisSort)
    ; Mode = "acc2_par_shallow2" ->
        qsortacc2_par_shallow2(Rands, [], ThisSort)
    ; Mode = "acc2_par_shallow3" ->
        qsortacc2_par_shallow3(Rands, [], ThisSort)
    ; Mode = "acc2_par_shallow4" ->
        qsortacc2_par_shallow4(Rands, [], ThisSort)

    ;
        error("bad mode")
    ),
    NextN = N - 1,
    ( NextN =< -0 ->
        Sort = ThisSort
    ;
        run_n_tests(NextN, Mode, Rands, Sort)
    ).

:- impure pred gettimeofday(int::out) is det.
:- pragma foreign_proc("C",
    gettimeofday(T::out),
    [thread_safe, will_not_call_mercury],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    T = tv.tv_sec * 1000000L + tv.tv_usec;
").

%-----------------------------------------------------------------------------%

:- pred rand_list(int::in, int::in, list(int)::out) is det.

rand_list(Seed, Num, List) :-
    init_genrand(Seed, MT0),
    rand_list_2(Num, [], List, MT0, _MT).

:- pred rand_list_2(int::in, list(int)::in, list(int)::out,
    mt::di, mt::uo) is det.

rand_list_2(N, Acc0, Acc, !MT) :-
    ( if N > 0 then
        genrand_int32(Rand, !MT),
        rand_list_2(N-1, [Rand | Acc0], Acc, !MT)
    else
        Acc = Acc0
    ).

%-----------------------------------------------------------------------------%

:- pred qsortapp_seq(list(int)::in, list(int)::out) is det.

qsortapp_seq([], []).
qsortapp_seq([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_seq(Left0, Left),
    qsortapp_seq(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pred qsortacc_seq(list(int)::in, list(int)::in, list(int)::out) is det.

qsortacc_seq([], Acc, Acc).
qsortacc_seq([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortacc_seq(Right0, Acc0, Right),
    qsortacc_seq(Left0, [Pivot | Right], Acc).

%-----------------------------------------------------------------------------%

:- pred qsortapp_par_deep(list(int)::in, list(int)::out) is det.

qsortapp_par_deep([], []).
qsortapp_par_deep([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortapp_par_deep(Left0, Left)
    & qsortapp_par_deep(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortacc_par_deep(list(int)::in, list(int)::in, list(int)::out) is det.

qsortacc_par_deep([], Acc, Acc).
qsortacc_par_deep([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_par_deep(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc_par_deep(Left0, PivotRight, Acc)
    ).

:- func min_par_limit_length = int.
min_par_limit_length = 5000.

:- pred qsortapp_par_limit(list(int)::in, list(int)::out) is det.
% :- pragma promise_pure(qsortapp_par_limit/2).

qsortapp_par_limit([], []).
qsortapp_par_limit([Pivot | T], List) :-
%     impure gettimeofday(P0),
    partition_lr(Pivot, T,
        [], Left0, [], Right0,
        0, LeftCount, 0, RightCount),
%     impure gettimeofday(P1),
    (
        ( LeftCount > min_par_limit_length ->
            qsortapp_par_limit(Left0, Left)
        ;
            qsortapp_seq(Left0, Left)
        )
    &
        ( RightCount > min_par_limit_length ->
            qsortapp_par_limit(Right0, Right)
        ;
            qsortapp_seq(Right0, Right)
        )
    ),
%     impure gettimeofday(P2),
    append(Left, [Pivot | Right], List),
%     impure gettimeofday(P3),
%     trace [io(!IO)] (
%         io.print({P1-P0, P2-P1, P3-P2}, !IO),
%         io.nl(!IO)
%     ).
    true.

:- func min_acc_par_limit_length = int.
min_acc_par_limit_length = 10000.

:- pred qsortacc_par_limit(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc_par_limit([], Acc, Acc).
qsortacc_par_limit([Pivot | T], Acc0, Acc) :-
    partition_lr(Pivot, T,
        [], Left0, [], Right0,
        0, LeftCount, 0, RightCount),
    (
        LeftCount > min_acc_par_limit_length,
        RightCount > min_acc_par_limit_length
    ->
        (
            qsortacc_par_limit(Right0, Acc0, Right),
            PivotRight = [Pivot | Right]
        &
            qsortacc_par_limit(Left0, PivotRight, Acc)
        )
    ;
        qsortacc_seq(Right0, Acc0, Right),
        qsortacc_seq(Left0, [Pivot | Right], Acc)
    ).

%-----------------------------------------------------------------------------%

:- pred qsortapp_par_shallow(list(int)::in, list(int)::out) is det.

qsortapp_par_shallow([], []).
qsortapp_par_shallow([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortapp_seq(Left0, Left)
    & qsortapp_seq(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortapp_par_shallow2(list(int)::in, list(int)::out) is det.

qsortapp_par_shallow2([], []).
qsortapp_par_shallow2([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortapp_par_shallow(Left0, Left)
    & qsortapp_par_shallow(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortapp_par_shallow3(list(int)::in, list(int)::out) is det.

qsortapp_par_shallow3([], []).
qsortapp_par_shallow3([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortapp_par_shallow2(Left0, Left)
    & qsortapp_par_shallow2(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortapp_par_shallow4(list(int)::in, list(int)::out) is det.

qsortapp_par_shallow4([], []).
qsortapp_par_shallow4([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortapp_par_shallow3(Left0, Left)
    & qsortapp_par_shallow3(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

%-----------------------------------------------------------------------------%

:- pred qsortappacc_par_shallow(list(int)::in, list(int)::out) is det.

qsortappacc_par_shallow([], []).
qsortappacc_par_shallow([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortacc_seq(Left0, [], Left)
    & qsortacc_seq(Right0, [], Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortappacc_par_shallow2(list(int)::in, list(int)::out) is det.

qsortappacc_par_shallow2([], []).
qsortappacc_par_shallow2([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortappacc_par_shallow(Left0, Left)
    & qsortappacc_par_shallow(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortappacc_par_shallow3(list(int)::in, list(int)::out) is det.

qsortappacc_par_shallow3([], []).
qsortappacc_par_shallow3([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortappacc_par_shallow2(Left0, Left)
    & qsortappacc_par_shallow2(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

:- pred qsortappacc_par_shallow4(list(int)::in, list(int)::out) is det.

qsortappacc_par_shallow4([], []).
qsortappacc_par_shallow4([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    ( qsortappacc_par_shallow3(Left0, Left)
    & qsortappacc_par_shallow3(Right0, Right)
    ),
    append(Left, [Pivot | Right], List).

%-----------------------------------------------------------------------------%

:- pred qsortacc_par_shallow(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc_par_shallow([], Acc, Acc).
qsortacc_par_shallow([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_seq(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc_seq(Left0, PivotRight, Acc)
    ).

:- pred qsortacc_par_shallow2(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc_par_shallow2([], Acc, Acc).
qsortacc_par_shallow2([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_par_shallow(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc_par_shallow(Left0, PivotRight, Acc)
    ).

:- pred qsortacc_par_shallow3(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc_par_shallow3([], Acc, Acc).
qsortacc_par_shallow3([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_par_shallow2(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc_par_shallow2(Left0, PivotRight, Acc)
    ).

:- pred qsortacc_par_shallow4(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc_par_shallow4([], Acc, Acc).
qsortacc_par_shallow4([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_par_shallow3(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc_par_shallow3(Left0, PivotRight, Acc)
    ).

%-----------------------------------------------------------------------------%

:- pred qsortacc2_par_shallow(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc2_par_shallow([], Acc, Acc).
qsortacc2_par_shallow([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc_seq(Right0, [], Right),
        PivotRight = [Pivot | Right],
        list.append(PivotRight, Acc0, PivotRightAcc)
    &
        qsortacc_seq(Left0, PivotRightAcc, Acc)
    ).

:- pred qsortacc2_par_shallow2(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc2_par_shallow2([], Acc, Acc).
qsortacc2_par_shallow2([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc2_par_shallow(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc2_par_shallow(Left0, PivotRight, Acc)
    ).

:- pred qsortacc2_par_shallow3(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc2_par_shallow3([], Acc, Acc).
qsortacc2_par_shallow3([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc2_par_shallow2(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc2_par_shallow2(Left0, PivotRight, Acc)
    ).

:- pred qsortacc2_par_shallow4(list(int)::in, list(int)::in, list(int)::out)
    is det.

qsortacc2_par_shallow4([], Acc, Acc).
qsortacc2_par_shallow4([Pivot | T], Acc0, Acc) :-
    partition(Pivot, T, [], Left0, [], Right0),
    (
        qsortacc2_par_shallow3(Right0, Acc0, Right),
        PivotRight = [Pivot | Right]
    &
        qsortacc2_par_shallow3(Left0, PivotRight, Acc)
    ).

%-----------------------------------------------------------------------------%

    % qsort without append
    %
% :- pred qsort_acc(list(int)::in, list(int)::out) is det.
% :- pred qsort_acc(list(int)::in, list(int)::in, list(int)::out) is det.
%
% qsort_acc(U, S) :-
%     qsort_acc(U, [], S).
%
% qsort_acc([], Acc, Acc).
% qsort_acc([Pivot | T], Acc, List) :-
%     partition(Pivot, T, [], Left0, [], Right0, 0, N),
%     ( if N > gran then
%         qsort_acc(Right0, Acc, Right) &
%         qsort_acc(Left0, [Pivot | Right], List)
%     else
%         qsort_acc(Right0, Acc, Right),
%         qsort_acc(Left0, [Pivot | Right], List)
%     ).

% :- pred qsort_acc1(list(int)::in, list(int)::out) is det.
% :- pred qsort_acc1(list(int)::in, list(int)::in, list(int)::out) is det.
%
% qsort_acc1(U, S) :-
%     qsort_acc1(U, [], S).
%
% qsort_acc1([], Acc, Acc).
% qsort_acc1([Pivot | T], Acc, List) :-
%     partition(Pivot, T, [], Left0, [], Right0, 0, N),
%     ( if N > gran then
%         (
%             qsort_acc1(Right0, Acc, Right),
%             PivotRight = [Pivot | Right]
%         &
%             qsort_acc1(Left0, PivotRight, List)
%         )
%     else
%         qsort_acc1(Right0, Acc, Right),
%         qsort_acc1(Left0, [Pivot | Right], List)
%     ).

% :- pred qsort_acc1b(list(int)::in, list(int)::out) is det.
% :- pred qsort_acc1b(list(int)::in, list(int)::in, list(int)::out) is det.
%
% qsort_acc1b(U, S) :-
%     qsort_acc1b(U, [], S).
%
% qsort_acc1b([], Acc, Acc).
% qsort_acc1b([Pivot | T], Acc, List) :-
%     partition(Pivot, T, [], Left0, [], Right0, 0, N),
%     (
%         qsort_acc1b(Right0, Acc, Right),
%         PivotRight = [Pivot | Right]
%     &
%         qsort_acc1b(Left0, PivotRight, List)
%     ).

    % With granularity control, but simpler granularity control.
    %
% :- pred qsort_acc1c(list(int)::in, list(int)::out) is det.
% :- pred qsort_acc1c(list(int)::in, list(int)::in, list(int)::out) is det.
%
% qsort_acc1c(U, S) :-
%     qsort_acc1c(U, [], S).
%
% qsort_acc1c([], Acc, Acc).
% qsort_acc1c([Pivot | T], Acc, List) :-
%     partition_lr(Pivot, T, [], Left0, [], Right0, 0, L, 0, R),
%     (
%         L > 5000,
%         R > 5000
%     ->
%         (
%             qsort_acc1c(Right0, Acc, Right),
%             PivotRight = [Pivot | Right]
%         &
%             qsort_acc1c(Left0, PivotRight, List)
%         )
%     ;
%         qsort_acc1c(Right0, Acc, Right),
%         qsort_acc1c(Left0, [Pivot | Right], List)
%     ).

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).

:- pred partition_lr(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out, int::in, int::out, int::in, int::out)
    is det.

partition_lr(_Pivot, [], Left, Left, Right, Right, L, L, R, R).
partition_lr(Pivot, [H | T], Left0, Left, Right0, Right, L0, L, R0, R) :-
    ( if H < Pivot then
        partition_lr(Pivot, T, [H | Left0], Left, Right0, Right, L0+1,L, R0,R)
    else
        partition_lr(Pivot, T, Left0, Left, [H | Right0], Right, L0,L, R0+1,R)
    ).

%-----------------------------------------------------------------------------%

:- pred check_is_sorted(list(int)::in) is semidet.
:- pred check_is_sorted(int::in, list(int)::in) is semidet.

check_is_sorted([]).
check_is_sorted([H | T]) :-
    check_is_sorted(H, T).

check_is_sorted(_, []).
check_is_sorted(H, [A | As]) :-
    H =< A,
    check_is_sorted(A, As).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
