%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint64_conversion.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module benchmarking.
:- import_module float.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module require.
:- import_module string.
:- import_module uint64.
:- import_module unit.

%---------------------------------------------------------------------------%

main(!IO) :-
    NumRepeats = 100,
    NumElements = 10_000_000,
    io.format("n = %d; repeats = %d, grade = %s\n",
        [i(NumElements), i(NumRepeats), s($grade)], !IO),
    randomly_fill_array(NumElements, Array, SeedA, SeedB, SeedC, !IO),
    io.format("seed: a = %u, b = %u, c = %u\n",
        [u64(SeedA), u64(SeedB), u64(SeedC)], !IO),
    benchmark_det_io(run_test(std_uint64_to_string), Array, _, !IO,
        NumRepeats, TimeStd),
    io.format(" Std: %dms\n", [i(TimeStd)], !IO),
    benchmark_det_io(run_test(alt1_uint64_to_string), Array, _, !IO,
        NumRepeats, TimeAlt1),
    io.format("Alt1: %dms ratio: %.2f\n",
        [i(TimeAlt1), f(float(TimeStd) / float(TimeAlt1))], !IO),
    benchmark_det_io(run_test(alt2_uint64_to_string), Array, _, !IO,
        NumRepeats, TimeAlt2),
    io.format("Alt2: %dms ratio: %.2f\n",
        [i(TimeAlt2), f(float(TimeStd) / float(TimeAlt2))], !IO).

%---------------------------------------------------------------------------%

:- pred randomly_fill_array(int::in, array(uint64)::array_uo,
    uint64::out, uint64::out, uint64::out, io::di, io::uo) is det.

randomly_fill_array(Size, Array, A, B, C, !IO) :-
    open_system_rng(MaybeSysRNG, !IO),
    (
        MaybeSysRNG = ok(SysRNG)
    ;
        MaybeSysRNG = error(Error),
        error(string(Error))
    ),
    system_rng.generate_uint64(SysRNG, A, !IO),
    system_rng.generate_uint64(SysRNG, B, !IO),
    system_rng.generate_uint64(SysRNG, C, !IO),
    close_system_rng(SysRNG, !IO),
    sfc64.seed(A, B, C, Params, State0),
    array.generate_foldl(Size, generate_int(Params), Array, State0, _State).

:- pred generate_int(sfc64.params::in, int::in, uint64::out,
    sfc64.ustate::di, sfc64.ustate::uo) is det.

generate_int(Params, _I, N, !State) :-
    random.generate_uint64(Params, N, !State).

%---------------------------------------------------------------------------%

:- pred run_test((func(uint64) = string)::in(func(in) = uo is det),
    array(uint64)::in, unit::out, io::di, io::uo) is det.

run_test(Func, Array, unit, !IO) :-
    array.foldl(do_test(Func), Array, !IO).

%---------------------------------------------------------------------------%

:- pred do_test((func(uint64) = string)::in(func(in) = uo is det),
    uint64::in, io::di, io::uo) is det.

do_test(Func, N, !IO) :-
    S = Func(N),
    consume_string(S, !IO).

%---------------------------------------------------------------------------%

:- pragma no_inline(pred(consume_string/3)).
:- pred consume_string(string::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    consume_string(S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // S
").
:- pragma foreign_proc("C#",
    consume_string(S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // S
").
:- pragma foreign_proc("Java",
    consume_string(S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // S
").

%---------------------------------------------------------------------------%
%
% Std implementation using sprintf().
%

:- func std_uint64_to_string(uint64::in) = (string::uo) is det.
:- pragma foreign_proc("C",
    std_uint64_to_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[21]; // 20 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIu64, U64);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

%---------------------------------------------------------------------------%

:- func alt1_uint64_to_string(uint64::in) = (string::uo) is det.
:- pragma foreign_proc("C",
    alt1_uint64_to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int num_digits;
    if (U < UINT64_C(10)) {
        num_digits = 1;
    } else if (U < UINT64_C(100)) {
        num_digits = 2;
    } else if (U < UINT64_C(1000)) {
        num_digits = 3;
    } else if (U < UINT64_C(10000)) {
        num_digits = 4;
    } else if (U < UINT64_C(100000)) {
        num_digits = 5;
    } else if (U < UINT64_C(1000000)) {
        num_digits = 6;
    } else if (U < UINT64_C(10000000)) {
        num_digits = 7;
    } else if (U < UINT64_C(100000000)) {
        num_digits = 8;
    } else if (U < UINT64_C(1000000000)) {
        num_digits = 9;
    } else if (U < UINT64_C(10000000000)) {
        num_digits = 10;
    } else if (U < UINT64_C(100000000000)) {
        num_digits = 11;
    } else if (U < UINT64_C(1000000000000)) {
        num_digits = 12;
    } else if (U < UINT64_C(10000000000000)) {
        num_digits = 13;
    } else if (U < UINT64_C(100000000000000)) {
        num_digits = 14;
    } else if (U < UINT64_C(1000000000000000)) {
        num_digits = 15;
    } else if (U < UINT64_C(10000000000000000)) {
        num_digits = 16;
    } else if (U < UINT64_C(100000000000000000)) {
        num_digits = 17;
    } else if (U < UINT64_C(1000000000000000000)) {
        num_digits = 18;
    } else if (U < UINT64_C(10000000000000000000)) {
        num_digits = 19;
    } else {
        num_digits = 20;
    }

    MR_allocate_aligned_string_msg(S, num_digits, MR_ALLOC_ID);
    int i = num_digits;
    S[i] = '\\0';
    i--;
    do {
        S[i] = \"0123456789\"[U % 10];
        i--;
    } while ( U /= 10 );
").

%---------------------------------------------------------------------------%

% Same as alt1 except it uses Andrei Alexandrescu's digit counting method.
% See: <https://www.facebook.com/notes/10158791579037200/>
% This is biased in favour of small numbers, it doesn't really help for
% this benchmark.
%
:- func alt2_uint64_to_string(uint64::in) = (string::uo) is det.
:- pragma foreign_proc("C",
    alt2_uint64_to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint64_t v = U;
    int num_digits = 1;
    for (;;) {
        if (v < 10) { break; }
        if (v < 100) { num_digits += 1; break; }
        if (v < 1000) { num_digits += 2; break; }
        if (v < 10000) { num_digits += 3; break; }
        v /= UINT64_C(10000);
        num_digits += 4;
    }

    MR_allocate_aligned_string_msg(S, num_digits, MR_ALLOC_ID);
    int i = num_digits;
    S[i] = '\\0';
    i--;
    do {
        S[i] = \"0123456789\"[U % 10];
        i--;
    } while ( U /= 10 );
").

%---------------------------------------------------------------------------%
:- end_module uint64_conversion.
%---------------------------------------------------------------------------%
