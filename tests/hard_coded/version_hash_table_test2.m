%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module version_hash_table_test2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

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
        !:HT = version_hash_table.init(int.hash, 1, MaxOccupancy),

        io.write_string("Inserting elements\n", !IO),
        int.fold_up(do_insert, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        io.write_string("Looking up elements\n", !IO),
        int.fold_up(do_lookup, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        NumOccupants0 = version_hash_table.num_occupants(!.HT),
        ( NumOccupants0 = Max ->
            true
        ;
            error("num_occupants failed")
        ),

        Half = Max / 2,
        io.write_string("Deleting some elements\n", !IO),
        int.fold_up(do_delete, 0, Half - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        NumOccupants = version_hash_table.num_occupants(!.HT),
        ( NumOccupants = Max - Half ->
            true
        ;
            error("num_occupants failed")
        ),

        AL = version_hash_table.to_assoc_list(!.HT),
        ( list.length(AL) = NumOccupants ->
            true
        ;
            error("to_assoc_list failed")
        ),

        io.write_string("Replacing elements\n", !IO),
        int.fold_up(do_replace_neg, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        io.write_string("Looking up elements\n", !IO),
        int.fold_up(do_lookup_neg, 0, Max - 1, !HT),
        trace [runtime(env("HASH_TABLE_STATS"))] (
            impure report_stats
        ),

        _ = !.HT
    ).

:- pred do_insert(int::in, version_hash_table(int, int)::in,
    version_hash_table(int, int)::out) is det.

do_insert(I, !HT) :-
    version_hash_table.det_insert(I, I, !HT).

:- pred do_lookup(int::in, version_hash_table(int, int)::in,
    version_hash_table(int, int)::out) is det.

do_lookup(I, !HT) :-
    V = version_hash_table.lookup(!.HT, I),
    ( I = V ->
        true
    ;
        error("do_lookup failed")
    ).

:- pred do_lookup_neg(int::in, version_hash_table(int, int)::in,
    version_hash_table(int, int)::out) is det.

do_lookup_neg(I, !HT) :-
    V = version_hash_table.lookup(!.HT, I),
    ( -I = V ->
        true
    ;
        error("do_lookup failed")
    ).

:- pred do_delete(int::in, version_hash_table(int, int)::in,
    version_hash_table(int, int)::out) is det.

do_delete(I, !HT) :-
    version_hash_table.delete(I, !HT).

:- pred do_replace_neg(int::in, version_hash_table(int, int)::in,
    version_hash_table(int, int)::out) is det.

do_replace_neg(I, !HT) :-
    version_hash_table.set(I, -I, !HT).
