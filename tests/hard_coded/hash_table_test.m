%-----------------------------------------------------------------------------%

:- module hash_table_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module hash_table.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        A = "1000000",
        B = "0.9"
    ;
        Args = [A],
        B = "0.9"
    ;
        Args = [A, B | _]
    ),
    Max = string.det_to_int(A),
    MaxOccupancy = string.det_to_float(B),
    some [!HT] (
        !:HT = hash_table.new(int_hash, 1, MaxOccupancy),

        io.write_string("Inserting elements\n", !IO),
        int.fold_up(do_insert, 0, Max - 1, !HT),
        trace [io(!IO), runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        io.write_string("Looking up elements\n", !IO),
        int.fold_up(do_lookup, 0, Max - 1, !HT),
        trace [io(!IO), runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        unsafe_hash_table_cast(!HT),

        NumOccupants0 = hash_table.num_occupants(!.HT),
        ( NumOccupants0 = Max ->
            true
        ;
            error("num_occupants failed")
        ),

        Half = Max / 2,
        io.write_string("Deleting some elements\n", !IO),
        int.fold_up(do_delete, 0, Half - 1, !HT),
        trace [io(!IO), runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        unsafe_hash_table_cast(!HT),

        NumOccupants = hash_table.num_occupants(!.HT),
        ( NumOccupants = Max - Half ->
            true
        ;
            error("num_occupants failed")
        ),

        AL = hash_table.to_assoc_list(!.HT),
        ( list.length(AL) = NumOccupants ->
            true
        ;
            error("to_assoc_list failed")
        ),

        io.write_string("Replacing elements\n", !IO),
        int.fold_up(do_replace_neg, 0, Max - 1, !HT),
        trace [io(!IO), runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        io.write_string("Looking up elements\n", !IO),
        int.fold_up(do_lookup_neg, 0, Max - 1, !HT),
        trace [io(!IO), runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        _ = !.HT
    ).

:- pred do_insert(int::in, hash_table(int, int)::hash_table_di,
    hash_table(int, int)::hash_table_uo) is det.

do_insert(I, !HT) :-
    hash_table.det_insert(I, I, !HT).

:- pred do_lookup(int::in, hash_table(int, int)::hash_table_di,
    hash_table(int, int)::hash_table_uo) is det.

do_lookup(I, !HT) :-
    V = hash_table.lookup(!.HT, I),
    ( I = V ->
        true
    ;
        error("do_lookup failed")
    ).

:- pred do_lookup_neg(int::in, hash_table(int, int)::hash_table_di,
    hash_table(int, int)::hash_table_uo) is det.

do_lookup_neg(I, !HT) :-
    V = hash_table.lookup(!.HT, I),
    ( -I = V ->
        true
    ;
        error("do_lookup failed")
    ).

:- pred do_delete(int::in, hash_table(int, int)::hash_table_di,
    hash_table(int, int)::hash_table_uo) is det.

do_delete(I, !HT) :-
    hash_table.delete(I, !HT).

:- pred do_replace_neg(int::in, hash_table(int, int)::hash_table_di,
    hash_table(int, int)::hash_table_uo) is det.

do_replace_neg(I, !HT) :-
    hash_table.set(I, -I, !HT).

:- pred unsafe_hash_table_cast(hash_table(T, T)::in,
    hash_table(T, T)::out(hash_table)) is det.

:- pragma foreign_proc("C",
    unsafe_hash_table_cast(HT0::in, HT::out(hash_table)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    HT = HT0;
").

:- pragma foreign_proc("Java",
    unsafe_hash_table_cast(HT0::in, HT::out(hash_table)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    HT = HT0;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
