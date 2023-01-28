%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module speedtest_bitset.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module fat_sparse_bitset.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module random.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module sparse_bitset.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        (
            Args = [],
            Size = 20000u,
            Repeat = 1000
        ;
            Args = [SizeStr],
            string.to_uint(SizeStr, Size),
            Repeat = 1000
        ;
            Args = [SizeStr, RepeatStr],
            string.to_uint(SizeStr, Size),
            string.to_int(RepeatStr, Repeat)
        )
    then
        init_random(MaybeRNG, !IO),
        (
            MaybeRNG = ok(R0),
            build_random_list(Size, 2u, [], BenchList, R0, _R),
            run_benchmarks(Repeat, BenchList, !IO)
        ;
            MaybeRNG = error(_),
            io.write_string("init_random failed\n", !IO)
        )
    else
        io.write_string("usage: speedtest_bitset [Size [Repeat]]\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- type bench
    --->    bench(uint, uint, list(uint)).
            % stride, #elems, random list with that stride and #elems

:- pred build_random_list(uint::in, uint::in,
    list(bench)::in, list(bench)::out, R::in, R::out) is det <= random(R).

build_random_list(Size, Stride, !BenchList, !R) :-
    NumElems = Size / Stride,
    ( if NumElems =< 64u then
        true
    else
        build_random_list_acc(Size, NumElems, [], RandomList, !R),
        Bench = bench(Stride, NumElems, RandomList),
        !:BenchList = [Bench | !.BenchList],
        build_random_list(Size, Stride * 2u, !BenchList, !R)
    ).

:- pred build_random_list_acc(uint::in, uint::in,
    list(uint)::in, list(uint)::out, R::in, R::out) is det <= random(R).

build_random_list_acc(Size, NumElemsLeft, !RandomList, !R) :-
    ( if NumElemsLeft = 0u then
        true
    else
        random.uniform_uint_in_range(0u, Size, Random, !R),
        !:RandomList = [Random | !.RandomList],
        build_random_list_acc(Size, NumElemsLeft - 1u, !RandomList, !R)
    ).

%---------------------------------------------------------------------------%

:- pred run_benchmarks(int::in, list(bench)::in, io::di, io::uo) is cc_multi.

run_benchmarks(_Repeat, [], !IO).
run_benchmarks(Repeat, [Bench | Benches], !IO) :-
    Bench = bench(Stride, NumElems, RandomList),
    io.write_string("--------------------\n\n", !IO),
    io.format("stride:  %u\n", [u(Stride)], !IO),
    io.format("#elems:  %u\n", [u(NumElems)], !IO),
    io.format("#repeat: %d\n", [i(Repeat)], !IO),

    benchmark_det(bench_sparse_old, RandomList, _SparseOldSet, Repeat,
        TimeSparseOld),
    benchmark_det(bench_sparse, RandomList, SparseSet, Repeat,
        TimeSparse),
    benchmark_det(bench_fat_sparse_old, RandomList, _FatSparseOldSet, Repeat,
        TimeFatSparseOld),
    benchmark_det(bench_fat_sparse, RandomList, FatSparseSet, Repeat,
        TimeFatSparse),

    sparse_bitset.to_sorted_list(SparseSet, SparseSortedList),
    fat_sparse_bitset.to_sorted_list(FatSparseSet, FatSparseSortedList),
    list.sort_and_remove_dups(RandomList, SortedList),
    ( if
        SparseSortedList = SortedList,
        FatSparseSortedList = SortedList
    then
        AvgSparseOld = float(TimeSparseOld) / float(Repeat),
        AvgSparse = float(TimeSparse) / float(Repeat),
        AvgFatSparseOld = float(TimeFatSparseOld) / float(Repeat),
        AvgFatSparse = float(TimeFatSparse) / float(Repeat),

        io.format("sparse old:     %12.5f ms\n", [f(AvgSparseOld)], !IO),
        io.format("sparse:         %12.5f ms\n", [f(AvgSparse)], !IO),
        io.format("fat sparse old: %12.5f ms\n", [f(AvgFatSparseOld)], !IO),
        io.format("fat sparse:     %12.5f ms\n", [f(AvgFatSparse)], !IO)
    else
        io.write_string("SORTED LIST MISMATCH\n", !IO)
    ),
    io.nl(!IO),
    io.flush_output(!IO),

    run_benchmarks(Repeat, Benches, !IO).

:- pred bench_sparse_old(list(uint)::in,
    sparse_bitset(uint)::out) is det.

bench_sparse_old(RandomList, Set) :-
    sparse_bitset.old_list_to_set(RandomList, Set).

:- pred bench_sparse(list(uint)::in,
    sparse_bitset(uint)::out) is det.

bench_sparse(RandomList, Set) :-
    sparse_bitset.list_to_set(RandomList, Set).

:- pred bench_fat_sparse_old(list(uint)::in,
    fat_sparse_bitset(uint)::out) is det.

bench_fat_sparse_old(RandomList, Set) :-
    fat_sparse_bitset.old_list_to_set(RandomList, Set).

:- pred bench_fat_sparse(list(uint)::in,
    fat_sparse_bitset(uint)::out) is det.

bench_fat_sparse(RandomList, Set) :-
    fat_sparse_bitset.list_to_set(RandomList, Set).

%---------------------------------------------------------------------------%

:- some [R] pred init_random(maybe_error(R)::out, io::di, io::uo) is det
    => random(R).

init_random(Res, !IO) :-
    open_system_rng(MaybeRNG, !IO),
    (
        MaybeRNG = ok(SystemRNG),
        system_rng.generate_uint64(SystemRNG, A, !IO),
        system_rng.generate_uint64(SystemRNG, B, !IO),
        system_rng.generate_uint64(SystemRNG, C, !IO),
        close_system_rng(SystemRNG, !IO),
        sfc64.seed(A, B, C, Params, UState),
        R = make_shared_random(Params, UState),
        Res = ok(R)
    ;
        MaybeRNG = error(Error),
        Res = error(Error)
    ).

%---------------------------------------------------------------------------%
