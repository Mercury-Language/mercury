%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: table_statistics.m.
% Author: zs.
% Stability: low.
%
% This file is automatically imported, as if via `use_module', into every
% module that contains a `pragma memo' that asks the compiler to create
% a predicate for returning statistics about the memo table. It defines
% the data structure that this predicate will return, and some operations
% on this data structure.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module table_statistics.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- type proc_table_statistics
    --->    proc_table_statistics(
                call_table_stats            :: table_stats_curr_prev,
                maybe_answer_table_stats    :: maybe(table_stats_curr_prev)
            ).

:- type table_stats_curr_prev
    --->    table_stats_curr_prev(
                current_stats               :: table_stats,
                stats_at_last_call          :: table_stats
            ).

:- type table_stats
    --->    table_stats(
                num_lookups                 :: int,
                num_lookups_is_dupl         :: int,
                step_statistics             :: list(table_step_stats)
            ).

    % The definition of this type be an enum whose implementation matches
    % the type MR_TableTrieStep in runtime/mercury_tabling.h. It should also
    % be kept in sync with the type table_trie_step in hlds_pred.m.
    %
:- type table_step_kind
    --->    table_step_dummy
    ;       table_step_int
    ;       table_step_char
    ;       table_step_string
    ;       table_step_float
    ;       table_step_enum
    ;       table_step_foreign_enum
    ;       table_step_general
    ;       table_step_general_addr
    ;       table_step_general_poly
    ;       table_step_general_poly_addr
    ;       table_step_typeinfo
    ;       table_step_typeclassinfo
    ;       table_step_promise_implied
    ;       table_step_int8
    ;       table_step_uint8
    ;       table_step_int16
    ;       table_step_uint16
    ;       table_step_int32
    ;       table_step_uint32
    ;       table_step_int64
    ;       table_step_uint64.

:- type table_step_stats
    --->    table_step_stats(
                table_step_var_name                 :: string,
                table_step_num_lookups              :: int,
                table_step_num_lookups_is_dupl      :: int,
                table_step_detail                   :: table_step_stat_details
            ).

:- type table_step_stat_details
    --->    step_stats_none
    ;       step_stats_start(
                start_num_node_allocs               :: int,
                start_num_node_bytes                :: int
            )
    ;       step_stats_enum(
                enum_num_node_allocs                :: int,
                enum_num_node_bytes                 :: int
            )
    ;       step_stats_hash(
                hash_num_table_allocs               :: int,
                hash_num_table_bytes                :: int,
                hash_num_link_chunk_allocs          :: int,
                hash_num_link_chunk_bytes           :: int,
                hash_num_num_key_compares_not_dupl  :: int,
                hash_num_num_key_compares_dupl      :: int,
                hash_num_resizes                    :: int,
                hash_resizes_num_old_entries        :: int,
                hash_resizes_num_new_entries        :: int
            )
    ;       step_stats_du(
                du_num_node_allocs                  :: int,
                du_num_node_bytes                   :: int,
                du_num_arg_lookups                  :: int,
                du_num_exist_lookups                :: int,

                du_enum_num_node_allocs             :: int,
                du_enum_num_node_bytes              :: int,

                du_hash_num_table_allocs            :: int,
                du_hash_num_table_bytes             :: int,
                du_hash_num_link_chunk_allocs       :: int,
                du_hash_num_link_chunk_bytes        :: int,
                du_hash_num_num_key_compares_not_dupl :: int,
                du_hash_num_num_key_compares_dupl   :: int,
                du_hash_num_resizes                 :: int,
                du_hash_resizes_num_old_entries     :: int,
                du_hash_resizes_num_new_entries     :: int
            )
    ;       step_stats_poly(
                poly_du_num_node_allocs             :: int,
                poly_du_num_node_bytes              :: int,
                poly_du_num_arg_lookups             :: int,
                poly_du_num_exist_lookups           :: int,

                poly_enum_num_node_allocs           :: int,
                poly_enum_num_node_bytes            :: int,

                poly_hash_num_table_allocs          :: int,
                poly_hash_num_table_bytes           :: int,
                poly_hash_num_link_chunk_allocs     :: int,
                poly_hash_num_link_chunk_bytes      :: int,
                poly_hash_num_num_key_compares_not_dupl :: int,
                poly_hash_num_num_key_compares_dupl :: int,
                poly_hash_num_resizes               :: int,
                poly_hash_resizes_num_old_entries   :: int,
                poly_hash_resizes_num_new_entries   :: int
            ).

:- func table_stats_difference(table_stats, table_stats) = table_stats.

:- pred write_table_stats(table_stats::in, io::di, io::uo) is det.
:- pred write_table_stats(io.text_output_stream::in, table_stats::in,
    io::di, io::uo) is det.

    % In grades that don't support tabling, all calls to get tabling stats
    % will return these dummy statistics.
    %
:- func dummy_proc_table_statistics = proc_table_statistics.

%---------------------------------------------------------------------------%

:- implementation.
:- interface.

% The compiler, when implementing the "statistics" attribute of a
% tabling pragma, can introduce calls to MR_get_tabling_stats.
% This means that this C function is effectively part of the interface
% of this module. However, this fact is an implementation detail
% that is of concern only to implementors, which is why we do not
% want it includes in the automaticallu generated documentation
% of this module.
:- pragma foreign_import_module("C", table_statistics).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module table_builtin.

%---------------------------------------------------------------------------%

:- pred get_tabling_stats(ml_proc_table_info::in, proc_table_statistics::out,
    io::di, io::uo) is det.
:- pragma foreign_export("C", get_tabling_stats(in, out, di, uo),
    "MR_get_tabling_stats").

get_tabling_stats(Info, Statistics, !IO) :-
    get_proc_info_direct_fields(Info, HasAnswerTable, NumInputs, NumOutputs,
        InputStepDescsPtr, OutputStepDescsPtr,
        CurCallStatsPtr, PrevCallStatsPtr,
        CurAnswerStatsPtr, PrevAnswerStatsPtr, !IO),
    get_one_table_stats(InputStepDescsPtr, CurCallStatsPtr, NumInputs,
        CurCallStats, !IO),
    get_one_table_stats(InputStepDescsPtr, PrevCallStatsPtr, NumInputs,
        PrevCallStats, !IO),
    CallStats = table_stats_curr_prev(CurCallStats, PrevCallStats),
    copy_current_stats_to_prev(CurCallStatsPtr, PrevCallStatsPtr,
        NumInputs, !IO),
    ( if HasAnswerTable > 0 then
        get_one_table_stats(OutputStepDescsPtr, CurAnswerStatsPtr, NumOutputs,
            CurAnswerStats, !IO),
        get_one_table_stats(OutputStepDescsPtr, PrevAnswerStatsPtr, NumOutputs,
            PrevAnswerStats, !IO),
        AnswerStats = table_stats_curr_prev(CurAnswerStats, PrevAnswerStats),
        copy_current_stats_to_prev(CurAnswerStatsPtr, PrevAnswerStatsPtr,
            NumOutputs, !IO),
        MaybeAnswerStats = yes(AnswerStats)
    else
        MaybeAnswerStats = no
    ),
    Statistics = proc_table_statistics(CallStats, MaybeAnswerStats).

:- type ml_table_step_desc_ptr --->   ml_table_step_desc_ptr(c_pointer).
:- pragma foreign_type("C", ml_table_step_desc_ptr, "const MR_TableStepDesc *",
    [can_pass_as_mercury_type]).

:- type ml_table_stats_ptr --->   ml_table_stats_ptr(c_pointer).
:- pragma foreign_type("C", ml_table_stats_ptr, "MR_TableStats *",
    [can_pass_as_mercury_type]).

:- pred get_proc_info_direct_fields(ml_proc_table_info::in,
    int::out, int::out, int::out,
    ml_table_step_desc_ptr::out, ml_table_step_desc_ptr::out,
    ml_table_stats_ptr::out, ml_table_stats_ptr::out,
    ml_table_stats_ptr::out, ml_table_stats_ptr::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_proc_info_direct_fields(Info::in,
        HasAnswerTable::out, NumInputs::out, NumOutputs::out,
        InputStepDescsPtr::out, OutputStepDescsPtr::out,
        CurCallStatsPtr::out, PrevCallStatsPtr::out,
        CurAnswerStatsPtr::out, PrevAnswerStatsPtr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness],
"
    HasAnswerTable = ( Info->MR_pt_has_answer_table ? 1 : 0 );
    NumInputs = Info->MR_pt_num_inputs;
    NumOutputs = Info->MR_pt_num_outputs;
    InputStepDescsPtr = Info->MR_pt_steps_desc[MR_TABLE_CALL_TABLE];
    OutputStepDescsPtr = Info->MR_pt_steps_desc[MR_TABLE_ANSWER_TABLE];
    CurCallStatsPtr = &(Info->
        MR_pt_stats[MR_TABLE_CALL_TABLE][MR_TABLE_STATS_CURR]);
    PrevCallStatsPtr = &(Info->
        MR_pt_stats[MR_TABLE_CALL_TABLE][MR_TABLE_STATS_PREV]);
    CurAnswerStatsPtr = &(Info->
        MR_pt_stats[MR_TABLE_ANSWER_TABLE][MR_TABLE_STATS_CURR]);
    PrevAnswerStatsPtr = &(Info->
        MR_pt_stats[MR_TABLE_ANSWER_TABLE][MR_TABLE_STATS_PREV]);
").

:- pred get_one_table_stats(ml_table_step_desc_ptr::in, ml_table_stats_ptr::in,
    int::in, table_stats::out, io::di, io::uo) is det.

get_one_table_stats(StepDescsPtr, StatsPtr, NumSteps, Stats, !IO) :-
    get_one_table_overall_stats(StatsPtr, NumLookups, NumLookupsIsDupl, !IO),
    get_one_table_stats_step_loop(StepDescsPtr, StatsPtr,
        0, NumSteps, [], StepStats, !IO),
    Stats = table_stats(NumLookups, NumLookupsIsDupl, StepStats).

:- pred get_one_table_overall_stats(ml_table_stats_ptr::in,
    int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_one_table_overall_stats(StatsPtr::in,
        NumLookups::out, NumLookupsIsDupl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness],
"
    NumLookups = StatsPtr->MR_ts_num_lookups;
    NumLookupsIsDupl = StatsPtr->MR_ts_num_lookups_is_dupl;
").

:- pred get_one_table_stats_step_loop(ml_table_step_desc_ptr::in,
    ml_table_stats_ptr::in, int::in, int::in,
    list(table_step_stats)::in, list(table_step_stats)::out,
    io::di, io::uo) is det.

get_one_table_stats_step_loop(StepDescsPtr, StatsPtr, CurStep, NumSteps,
        !StepStats, !IO) :-
    ( if CurStep >= NumSteps then
        true
    else
        get_one_table_stats_step_loop(StepDescsPtr, StatsPtr,
            CurStep + 1, NumSteps, !StepStats, !IO),
        get_one_table_step_stats(StepDescsPtr, StatsPtr, CurStep, StepStats,
            !IO),
        !:StepStats = [StepStats | !.StepStats]
    ).

:- pred get_one_table_step_stats(ml_table_step_desc_ptr::in,
    ml_table_stats_ptr::in, int::in, table_step_stats::out, io::di, io::uo)
    is det.

get_one_table_step_stats(StepDescsPtr, StatsPtr, StepNum, Stats, !IO) :-
    get_one_table_step_stat_details(StepDescsPtr, StatsPtr, StepNum, VarName,
        NumLookups, NumLookupsIsDupl, KindInt,
        HashTableAllocs, HashTableBytes,
        HashLinkChunkAllocs, HashLinkChunkBytes,
        HashKeyComparesNotDupl, HashKeyComparesIsDupl,
        HashResizes, HashResizeOldEntries, HashResizeNewEntries,
        EnumNodeAllocs, EnumNodeBytes,
        DuNodeAllocs, DuNodeBytes, DuArgLookups, DuExistLookups,
        StartAllocs, StartBytes, !IO),
    ( if KindInt = 0 then                    % MR_TABLE_STATS_DETAIL_HASH
        ( if
            EnumNodeAllocs = 0,
            EnumNodeBytes = 0,
            DuNodeAllocs = 0,
            DuNodeBytes = 0,
            DuArgLookups = 0,
            DuExistLookups = 0,
            StartAllocs = 0,
            StartBytes = 0
        then
            Details = step_stats_hash(HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries)
        else
            unexpected($pred, "extra counts for hash")
        )
    else if KindInt = 1 then                    % MR_TABLE_STATS_DETAIL_ENUM
        ( if
            HashTableAllocs = 0,
            HashTableBytes = 0,
            HashLinkChunkAllocs = 0,
            HashLinkChunkBytes = 0,
            HashKeyComparesNotDupl = 0,
            HashKeyComparesIsDupl = 0,
            HashResizes = 0,
            HashResizeOldEntries = 0,
            HashResizeNewEntries = 0,
            DuNodeAllocs = 0,
            DuNodeBytes = 0,
            DuArgLookups = 0,
            DuExistLookups = 0,
            StartAllocs = 0,
            StartBytes = 0
        then
            Details = step_stats_enum(EnumNodeAllocs, EnumNodeBytes)
        else
            unexpected($pred, "extra counts for enum")
        )
    else if KindInt = 2 then                    % MR_TABLE_STATS_DETAIL_START
        ( if
            HashTableAllocs = 0,
            HashTableBytes = 0,
            HashLinkChunkAllocs = 0,
            HashLinkChunkBytes = 0,
            HashKeyComparesNotDupl = 0,
            HashKeyComparesIsDupl = 0,
            HashResizes = 0,
            HashResizeOldEntries = 0,
            HashResizeNewEntries = 0,
            EnumNodeAllocs = 0,
            EnumNodeBytes = 0,
            DuNodeAllocs = 0,
            DuNodeBytes = 0,
            DuArgLookups = 0,
            DuExistLookups = 0
        then
            Details = step_stats_start(StartAllocs, StartBytes)
        else
            unexpected($pred, "extra counts for start")
        )
    else if KindInt = 3 then                    % MR_TABLE_STATS_DETAIL_DU
        ( if
            StartAllocs = 0,
            StartBytes = 0
        then
            Details = step_stats_du(DuNodeAllocs, DuNodeBytes,
                DuArgLookups, DuExistLookups, EnumNodeAllocs, EnumNodeBytes,
                HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries)
        else
            unexpected($pred, "extra counts for du")
        )
    else if KindInt = 4 then                    % MR_TABLE_STATS_DETAIL_POLY
        ( if
            StartAllocs = 0,
            StartBytes = 0
        then
            Details = step_stats_poly(DuNodeAllocs, DuNodeBytes,
                DuArgLookups, DuExistLookups, EnumNodeAllocs, EnumNodeBytes,
                HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries)
        else
            unexpected($pred, "extra counts for poly")
        )
    else if KindInt = 5 then                    % MR_TABLE_STATS_DETAIL_NONE
        ( if
            HashTableAllocs = 0,
            HashTableBytes = 0,
            HashLinkChunkAllocs = 0,
            HashKeyComparesNotDupl = 0,
            HashKeyComparesIsDupl = 0,
            HashResizes = 0,
            HashResizeOldEntries = 0,
            HashResizeNewEntries = 0,
            EnumNodeAllocs = 0,
            EnumNodeBytes = 0,
            DuNodeAllocs = 0,
            DuNodeBytes = 0,
            DuArgLookups = 0,
            DuExistLookups = 0,
            StartAllocs = 0,
            StartBytes = 0
        then
            Details = step_stats_none
        else
            unexpected($pred, "extra counts for none")
        )
    else
        unexpected($pred, "unexpected detail kind")
    ),
    Stats = table_step_stats(VarName, NumLookups, NumLookupsIsDupl, Details).

:- pred get_one_table_step_stat_details(ml_table_step_desc_ptr::in,
    ml_table_stats_ptr::in, int::in,
    string::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out,
    int::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_one_table_step_stat_details(StepDescsPtr::in, StatsStructPtr::in,
        StepNum::in, VarName::out, NumLookups::out, NumLookupsIsDupl::out,
        KindInt::out,
        HashTableAllocs::out, HashTableBytes::out,
        HashLinkChunkAllocs::out, HashLinkChunkBytes::out,
        HashKeyComparesNotDupl::out, HashKeyComparesIsDupl::out,
        HashResizes::out, HashResizeOldEntries::out, HashResizeNewEntries::out,
        EnumNodeAllocs::out,EnumNodeBytes::out,
        DuNodeAllocs::out, DuNodeBytes::out,
        DuArgLookups::out, DuExistLookups::out,
        StartAllocs::out, StartBytes::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness],
"
    const MR_TableStepStats *ptr;

    ptr = &(StatsStructPtr->MR_ts_steps[StepNum]);

    // The casts are to discard const.
    VarName = (MR_String) (MR_Integer) StepDescsPtr[StepNum].MR_tsd_var_name;

    NumLookups =                ptr->MR_tss_num_lookups;
    NumLookupsIsDupl =          ptr->MR_tss_num_lookups_is_dupl;

    KindInt = (MR_Integer)      ptr->MR_tss_detail_kind;

    HashTableAllocs =           ptr->MR_tss_hash_num_table_allocs;
    HashTableBytes =            ptr->MR_tss_hash_num_table_alloc_bytes;
    HashLinkChunkAllocs =       ptr->MR_tss_hash_num_link_chunk_allocs;
    HashLinkChunkBytes =        ptr->MR_tss_hash_num_link_chunk_alloc_bytes;
    HashKeyComparesNotDupl =    ptr->MR_tss_hash_num_key_compares_not_dupl;
    HashKeyComparesIsDupl =     ptr->MR_tss_hash_num_key_compares_dupl;
    HashResizes =               ptr->MR_tss_hash_num_resizes;
    HashResizeOldEntries =      ptr->MR_tss_hash_resize_old_entries;
    HashResizeNewEntries =      ptr->MR_tss_hash_resize_new_entries;

    EnumNodeAllocs =            ptr->MR_tss_enum_num_node_allocs;
    EnumNodeBytes =             ptr->MR_tss_enum_num_node_alloc_bytes;

    DuNodeAllocs =              ptr->MR_tss_du_num_node_allocs;
    DuNodeBytes =               ptr->MR_tss_du_num_node_alloc_bytes;
    DuArgLookups =              ptr->MR_tss_du_num_arg_lookups;
    DuExistLookups =            ptr->MR_tss_du_num_exist_lookups;

    StartAllocs =               ptr->MR_tss_start_num_allocs;
    StartBytes =                ptr->MR_tss_start_num_alloc_bytes;
").

:- pred copy_current_stats_to_prev(ml_table_stats_ptr::in,
    ml_table_stats_ptr::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    copy_current_stats_to_prev(CurPtr::in, PrevPtr::in, NumSteps::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness],
"
    MR_TableStepStats   *cur;
    MR_TableStepStats   *prev;
    int                 i;

    PrevPtr->MR_ts_num_lookups = CurPtr->MR_ts_num_lookups;
    PrevPtr->MR_ts_num_lookups_is_dupl = CurPtr->MR_ts_num_lookups_is_dupl;

    for (i = 0; i < NumSteps; i++) {
        cur = &(CurPtr->MR_ts_steps[i]);
        prev = &(PrevPtr->MR_ts_steps[i]);

        prev->MR_tss_num_lookups =
            cur->MR_tss_num_lookups;
        prev->MR_tss_num_lookups_is_dupl =
            cur->MR_tss_num_lookups_is_dupl;

        prev->MR_tss_hash_num_table_allocs =
            cur->MR_tss_hash_num_table_allocs;
        prev->MR_tss_hash_num_table_alloc_bytes =
            cur->MR_tss_hash_num_table_alloc_bytes;
        prev->MR_tss_hash_num_link_chunk_allocs =
            cur->MR_tss_hash_num_link_chunk_allocs;
        prev->MR_tss_hash_num_link_chunk_alloc_bytes =
            cur->MR_tss_hash_num_link_chunk_alloc_bytes;
        prev->MR_tss_hash_num_key_compares_not_dupl =
            cur->MR_tss_hash_num_key_compares_not_dupl;
        prev->MR_tss_hash_num_key_compares_dupl =
            cur->MR_tss_hash_num_key_compares_dupl;
        prev->MR_tss_hash_num_resizes =
            cur->MR_tss_hash_num_resizes;
        prev->MR_tss_hash_resize_old_entries =
            cur->MR_tss_hash_resize_old_entries;
        prev->MR_tss_hash_resize_new_entries =
            cur->MR_tss_hash_resize_new_entries;

        prev->MR_tss_enum_num_node_allocs =
            cur->MR_tss_enum_num_node_allocs;
        prev->MR_tss_enum_num_node_alloc_bytes =
            cur->MR_tss_enum_num_node_alloc_bytes;

        prev->MR_tss_du_num_node_allocs =
            cur->MR_tss_du_num_node_allocs;
        prev->MR_tss_du_num_node_alloc_bytes =
            cur->MR_tss_du_num_node_alloc_bytes;
        prev->MR_tss_du_num_arg_lookups =
            cur->MR_tss_du_num_arg_lookups;
        prev->MR_tss_du_num_exist_lookups =
            cur->MR_tss_du_num_exist_lookups;

        prev->MR_tss_start_num_allocs =
            cur->MR_tss_start_num_allocs;
        prev->MR_tss_start_num_alloc_bytes =
            cur->MR_tss_start_num_alloc_bytes;
    }
").

%---------------------------------------------------------------------------%

table_stats_difference(StatsA, StatsB) = StatsDiff :-
    StatsA = table_stats(LookupsA, LookupsIsDuplA, StepsA),
    StatsB = table_stats(LookupsB, LookupsIsDuplB, StepsB),

    LookupsDiff = LookupsA - LookupsB,
    LookupsIsDuplDiff = LookupsIsDuplA - LookupsIsDuplB,
    StepsDiff = table_step_stats_diff(StepsA, StepsB),

    StatsDiff = table_stats(LookupsDiff, LookupsIsDuplDiff, StepsDiff).

:- func table_step_stats_diff(list(table_step_stats), list(table_step_stats))
    = list(table_step_stats).

table_step_stats_diff([], []) = [].
table_step_stats_diff([_ | _], []) = func_error("mismatched table stats").
table_step_stats_diff([], [_ | _]) = func_error("mismatched table stats").
table_step_stats_diff([StepA | StepsA], [StepB | StepsB])
        = [StepDiff | StepDiffs] :-
    StepA = table_step_stats(VarNameA, LookupsA, LookupsIsDuplA, DetailsA),
    StepB = table_step_stats(VarNameB, LookupsB, LookupsIsDuplB, DetailsB),

    require(unify(VarNameA, VarNameB),
        "table_step_stats_diff: mismatches in variable name"),
    LookupsDiff = LookupsA - LookupsB,
    LookupsIsDuplDiff = LookupsIsDuplA - LookupsIsDuplB,
    ( if
        table_step_stats_detail_diff(DetailsA, DetailsB, DetailsDiffPrime)
    then
        DetailsDiff = DetailsDiffPrime
    else
        unexpected($pred, "mismatches in details")
    ),

    StepDiff = table_step_stats(VarNameA, LookupsDiff, LookupsIsDuplDiff,
        DetailsDiff),
    StepDiffs = table_step_stats_diff(StepsA, StepsB).

:- pred table_step_stats_detail_diff(table_step_stat_details::in,
    table_step_stat_details::in, table_step_stat_details::out) is semidet.

table_step_stats_detail_diff(DetailsA, DetailsB, DetailsDiff) :-
    (
        DetailsA = step_stats_none,
        DetailsB = step_stats_none,
        DetailsDiff = step_stats_none
    ;
        DetailsA = step_stats_start(StartAllocsA, StartBytesA),
        DetailsB = step_stats_start(StartAllocsB, StartBytesB),
        DetailsDiff = step_stats_start(StartAllocsA - StartAllocsB,
            StartBytesA - StartBytesB)
    ;
        DetailsA = step_stats_enum(EnumNodeAllocsA, EnumNodeBytesA),
        DetailsB = step_stats_enum(EnumNodeAllocsB, EnumNodeBytesB),
        DetailsDiff = step_stats_enum(EnumNodeAllocsA - EnumNodeAllocsB,
            EnumNodeBytesA - EnumNodeBytesB)
    ;
        DetailsA = step_stats_hash(HashTableAllocsA, HashTableBytesA,
            HashLinkChunkAllocsA, HashLinkChunkBytesA,
            HashKeyComparesNotDuplA, HashKeyComparesIsDuplA,
            HashResizesA, HashResizeOldEntriesA, HashResizeNewEntriesA),
        DetailsB = step_stats_hash(HashTableAllocsB, HashTableBytesB,
            HashLinkChunkAllocsB, HashLinkChunkBytesB,
            HashKeyComparesNotDuplB, HashKeyComparesIsDuplB,
            HashResizesB, HashResizeOldEntriesB, HashResizeNewEntriesB),
        DetailsDiff = step_stats_hash(HashTableAllocsA - HashTableAllocsB,
            HashTableBytesA - HashTableBytesB,
            HashLinkChunkAllocsA - HashLinkChunkAllocsB,
            HashLinkChunkBytesA - HashLinkChunkBytesB,
            HashKeyComparesNotDuplA - HashKeyComparesNotDuplB,
            HashKeyComparesIsDuplA - HashKeyComparesIsDuplB,
            HashResizesA - HashResizesB,
            HashResizeOldEntriesA - HashResizeOldEntriesB,
            HashResizeNewEntriesA - HashResizeNewEntriesB)
    ;
        DetailsA = step_stats_du(DuNodeAllocsA, DuNodeBytesA,
            DuArgLookupsA, DuExistLookupsA, EnumNodeAllocsA, EnumNodeBytesA,
            HashTableAllocsA, HashTableBytesA,
            HashLinkChunkAllocsA, HashLinkChunkBytesA,
            HashKeyComparesNotDuplA, HashKeyComparesIsDuplA,
            HashResizesA, HashResizeOldEntriesA, HashResizeNewEntriesA),
        DetailsB = step_stats_du(DuNodeAllocsB, DuNodeBytesB,
            DuArgLookupsB, DuExistLookupsB, EnumNodeAllocsB, EnumNodeBytesB,
            HashTableAllocsB, HashTableBytesB,
            HashLinkChunkAllocsB, HashLinkChunkBytesB,
            HashKeyComparesNotDuplB, HashKeyComparesIsDuplB,
            HashResizesB, HashResizeOldEntriesB, HashResizeNewEntriesB),
        DetailsDiff = step_stats_du(DuNodeAllocsA - DuNodeAllocsB,
            DuNodeBytesA - DuNodeBytesB,
            DuArgLookupsA - DuArgLookupsB,
            DuExistLookupsA - DuExistLookupsB,
            EnumNodeAllocsA - EnumNodeAllocsB,
            EnumNodeBytesA - EnumNodeBytesB,
            HashTableAllocsA - HashTableAllocsB,
            HashTableBytesA - HashTableBytesB,
            HashLinkChunkAllocsA - HashLinkChunkAllocsB,
            HashLinkChunkBytesA - HashLinkChunkBytesB,
            HashKeyComparesNotDuplA - HashKeyComparesNotDuplB,
            HashKeyComparesIsDuplA - HashKeyComparesIsDuplB,
            HashResizesA - HashResizesB,
            HashResizeOldEntriesA - HashResizeOldEntriesB,
            HashResizeNewEntriesA - HashResizeNewEntriesB)
    ;
        DetailsA = step_stats_poly(DuNodeAllocsA, DuNodeBytesA,
            DuArgLookupsA, DuExistLookupsA, EnumNodeAllocsA, EnumNodeBytesA,
            HashTableAllocsA, HashTableBytesA,
            HashLinkChunkAllocsA, HashLinkChunkBytesA,
            HashKeyComparesNotDuplA, HashKeyComparesIsDuplA,
            HashResizesA, HashResizeOldEntriesA, HashResizeNewEntriesA),
        DetailsB = step_stats_poly(DuNodeAllocsB, DuNodeBytesB,
            DuArgLookupsB, DuExistLookupsB, EnumNodeAllocsB, EnumNodeBytesB,
            HashTableAllocsB, HashTableBytesB,
            HashLinkChunkAllocsB, HashLinkChunkBytesB,
            HashKeyComparesNotDuplB, HashKeyComparesIsDuplB,
            HashResizesB, HashResizeOldEntriesB, HashResizeNewEntriesB),
        DetailsDiff = step_stats_poly(DuNodeAllocsA - DuNodeAllocsB,
            DuNodeBytesA - DuNodeBytesB,
            DuArgLookupsA - DuArgLookupsB,
            DuExistLookupsA - DuExistLookupsB,
            EnumNodeAllocsA - EnumNodeAllocsB,
            EnumNodeBytesA - EnumNodeBytesB,
            HashTableAllocsA - HashTableAllocsB,
            HashTableBytesA - HashTableBytesB,
            HashLinkChunkAllocsA - HashLinkChunkAllocsB,
            HashLinkChunkBytesA - HashLinkChunkBytesB,
            HashKeyComparesNotDuplA - HashKeyComparesNotDuplB,
            HashKeyComparesIsDuplA - HashKeyComparesIsDuplB,
            HashResizesA - HashResizesB,
            HashResizeOldEntriesA - HashResizeOldEntriesB,
            HashResizeNewEntriesA - HashResizeNewEntriesB)
    ).

%---------------------------------------------------------------------------%

write_table_stats(Stats, !IO) :-
    io.output_stream(OutStream, !IO),
    write_table_stats(OutStream, Stats, !IO).

write_table_stats(OutStream, Stats, !IO) :-
    Stats = table_stats(Lookups, LookupsIsDupl, Steps),

    LookupsNotDupl = Lookups - LookupsIsDupl,
    LookupsStr = string.int_to_string_thousands(Lookups),
    LookupsIsDuplStr = string.int_to_string_thousands(LookupsIsDupl),
    LookupsNotDuplStr = string.int_to_string_thousands(LookupsNotDupl),

    io.format(OutStream,
        "number of lookups:                            %9s\n",
        [s(LookupsStr)], !IO),
    ( if Lookups > 0 then
        FractionIsDuplStr = percentage_str(LookupsIsDupl, Lookups),
        FractionNotDuplStr = percentage_str(LookupsNotDupl, Lookups),

        io.format(OutStream,
            "number of successful lookups (old calls):     %9s %9s\n",
            [s(LookupsIsDuplStr), s(FractionIsDuplStr)], !IO),
        io.format(OutStream,
            "number of unsuccessful lookups (new calls):   %9s %9s\n",
            [s(LookupsNotDuplStr), s(FractionNotDuplStr)], !IO),
        io.write_string(OutStream,
            "statistics for the individual steps:\n", !IO),
        list.foldl2(write_table_step_stats_loop(OutStream), Steps, 1, _, !IO)
    else
        true
    ).

:- pred write_table_step_stats_loop(io.text_output_stream::in,
    table_step_stats::in, int::in, int::out, io::di, io::uo) is det.

write_table_step_stats_loop(OutStream, Step, !StepNum, !IO) :-
    write_table_step_stats(OutStream, Step, !.StepNum, !IO),
    !:StepNum = !.StepNum + 1.

:- pred write_table_step_stats_header(io.text_output_stream::in,
    string::in, int::in, string::in, int::in, int::in, io::di, io::uo) is det.

write_table_step_stats_header(OutStream, VarName, StepNum, KindStr,
        Lookups, LookupsIsDupl, !IO) :-
    io.format(OutStream,
        "\nstep %d, variable %s: %s\n",
        [i(StepNum), s(VarName), s(KindStr)], !IO),

    LookupsNotDupl = Lookups - LookupsIsDupl,
    LookupsStr = string.int_to_string_thousands(Lookups),
    LookupsIsDuplStr = string.int_to_string_thousands(LookupsIsDupl),
    LookupsNotDuplStr = string.int_to_string_thousands(LookupsNotDupl),

    io.format(OutStream,
        "  number of lookups:                          %9s\n",
        [s(LookupsStr)], !IO),
    ( if Lookups > 0 then
        FractionIsDuplStr = percentage_str(LookupsIsDupl, Lookups),
        FractionNotDuplStr = percentage_str(LookupsNotDupl, Lookups),

        io.format(OutStream,
            "  number of successful lookups:               %9s %9s\n",
            [s(LookupsIsDuplStr), s(FractionIsDuplStr)], !IO),
        io.format(OutStream,
            "  number of unsuccessful lookups:             %9s %9s\n",
            [s(LookupsNotDuplStr), s(FractionNotDuplStr)], !IO)
    else
        true
    ).

:- pred write_table_step_stats_start(io.text_output_stream::in,
    int::in, int::in, io::di, io::uo) is det.

write_table_step_stats_start(OutStream, StartAllocs, StartBytes, !IO) :-
    StartAllocsStr = string.int_to_string_thousands(StartAllocs),
    StartBytesStr = string.int_to_string_thousands(StartBytes),
    io.format(OutStream,
        "  number of array (re)allocations:            %9s\n",
        [s(StartAllocsStr)], !IO),
    io.format(OutStream,
        "  number of bytes (re)allocationed:           %9s\n",
        [s(StartBytesStr)], !IO).

:- pred write_table_step_stats_enum(io.text_output_stream::in,
    int::in, int::in, io::di, io::uo) is det.

write_table_step_stats_enum(OutStream, EnumNodeAllocs, EnumNodeBytes, !IO) :-
    EnumNodeAllocsStr = string.int_to_string_thousands(EnumNodeAllocs),
    EnumNodeBytesStr = string.int_to_string_thousands(EnumNodeBytes),
    io.format(OutStream,
        "  number of enum node allocations:            %9s\n",
        [s(EnumNodeAllocsStr)], !IO),
    io.format(OutStream,
        "  number of bytes allocated for enum nodes:   %9s\n",
        [s(EnumNodeBytesStr)], !IO).

:- pred write_table_step_stats_hash(io.text_output_stream::in,
    int::in, int::in, int::in, int::in, int::in, int::in,
    int::in, int::in, int::in, io::di, io::uo) is det.

write_table_step_stats_hash(OutStream, HashTableAllocs, HashTableBytes,
        HashLinkChunkAllocs, HashLinkChunkBytes,
        HashKeyComparesNotDupl, HashKeyComparesIsDupl,
        HashResizes, HashResizeOldEntries, HashResizeNewEntries, !IO) :-
    HashTableAllocsStr =
        string.int_to_string_thousands(HashTableAllocs),
    HashTableBytesStr =
        string.int_to_string_thousands(HashTableBytes),
    HashLinkChunkAllocsStr =
        string.int_to_string_thousands(HashLinkChunkAllocs),
    HashLinkChunkBytesStr =
        string.int_to_string_thousands(HashLinkChunkBytes),
    HashKeyComparesNotDuplStr =
        string.int_to_string_thousands(HashKeyComparesNotDupl),
    HashKeyComparesIsDuplStr =
        string.int_to_string_thousands(HashKeyComparesIsDupl),
    HashResizesStr =
        string.int_to_string_thousands(HashResizes),
    HashResizeOldEntriesStr =
        string.int_to_string_thousands(HashResizeOldEntries),
    HashResizeNewEntriesStr =
        string.int_to_string_thousands(HashResizeNewEntries),
    io.format(OutStream,
        "  number of hash table allocations:           %9s\n",
        [s(HashTableAllocsStr)], !IO),
    io.format(OutStream,
        "  number of bytes allocated for hash tables:  %9s\n",
        [s(HashTableBytesStr)], !IO),
    io.format(OutStream,
        "  number of bulk hash link allocations:       %9s\n",
        [s(HashLinkChunkAllocsStr)], !IO),
    io.format(OutStream,
        "  number of bytes allocated for hash links:   %9s\n",
        [s(HashLinkChunkBytesStr)], !IO),
    io.format(OutStream,
        "  number of key compares when unsuccessful:   %9s\n",
        [s(HashKeyComparesNotDuplStr)], !IO),
    io.format(OutStream,
        "  number of key compares when successful:     %9s\n",
        [s(HashKeyComparesIsDuplStr)], !IO),
    io.format(OutStream,
        "  number of hash table resizes:               %9s\n",
        [s(HashResizesStr)], !IO),
    ( if HashResizes > 0 then
        io.format(OutStream,
            "  number of old entries in resizes:           %9s\n",
            [s(HashResizeOldEntriesStr)], !IO),
        io.format(OutStream,
            "  number of new entries in resizes:           %9s\n",
            [s(HashResizeNewEntriesStr)], !IO)
    else
        true
    ).

:- pred write_table_step_stats_du(io.text_output_stream::in,
    int::in, int::in, int::in, int::in, io::di, io::uo) is det.

write_table_step_stats_du(OutStream, DuNodeAllocs, DuNodeBytes,
        DuArgLookups, DuExistLookups, !IO) :-
    DuNodeAllocsStr = string.int_to_string_thousands(DuNodeAllocs),
    DuNodeBytesStr = string.int_to_string_thousands(DuNodeBytes),
    DuArgLookupsStr = string.int_to_string_thousands(DuArgLookups),
    DuExistLookupsStr = string.int_to_string_thousands(DuExistLookups),
    io.format(OutStream,
        "  number of du functor node allocations:      %9s\n",
        [s(DuNodeAllocsStr)], !IO),
    io.format(OutStream,
        "  number of bytes allocated for du functors:  %9s\n",
        [s(DuNodeBytesStr)], !IO),
    io.format(OutStream,
        "  number of du functor argument lookups:      %9s\n",
        [s(DuArgLookupsStr)], !IO),
    ( if DuExistLookups > 0 then
        io.format(OutStream,
            "  number of du existential type lookups:      %9s\n",
            [s(DuExistLookupsStr)], !IO)
    else
        true
    ).

:- pred write_table_step_stats(io.text_output_stream::in,
    table_step_stats::in, int::in, io::di, io::uo) is det.

write_table_step_stats(OutStream, Step, StepNum, !IO) :-
    Step = table_step_stats(VarName, Lookups, LookupsIsDupl, Details),
    (
        Details = step_stats_none,
        write_table_step_stats_header(OutStream, VarName, StepNum, "none",
            Lookups, LookupsIsDupl, !IO)
    ;
        Details = step_stats_start(StartAllocs, StartBytes),
        write_table_step_stats_header(OutStream, VarName, StepNum,
            "expandable array", Lookups, LookupsIsDupl, !IO),
        ( if Lookups > 0 then
            write_table_step_stats_start(OutStream,
                StartAllocs, StartBytes, !IO)
        else
            true
        )
    ;
        Details = step_stats_enum(EnumNodeAllocs, EnumNodeBytes),
        write_table_step_stats_header(OutStream, VarName, StepNum,
            "enum trie", Lookups, LookupsIsDupl, !IO),
        ( if Lookups > 0 then
            write_table_step_stats_enum(OutStream,
                EnumNodeAllocs, EnumNodeBytes, !IO)
        else
            true
        )
    ;
        Details = step_stats_hash(HashTableAllocs, HashTableBytes,
            HashLinkChunkAllocs, HashLinkChunkBytes,
            HashKeyComparesNotDupl, HashKeyComparesIsDupl,
            HashResizes, HashResizeOldEntries, HashResizeNewEntries),
        write_table_step_stats_header(OutStream, VarName, StepNum,
            "hash table", Lookups, LookupsIsDupl, !IO),
        ( if Lookups > 0 then
            write_table_step_stats_hash(OutStream,
                HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries, !IO)
        else
            true
        )
    ;
        (
            Details = step_stats_du(DuNodeAllocs, DuNodeBytes,
                DuArgLookups, DuExistLookups, EnumNodeAllocs, EnumNodeBytes,
                HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries),
            KindStr = "discriminated union nested trie",
            MustHaveDu = yes
        ;
            Details = step_stats_poly(DuNodeAllocs, DuNodeBytes,
                DuArgLookups, DuExistLookups, EnumNodeAllocs, EnumNodeBytes,
                HashTableAllocs, HashTableBytes,
                HashLinkChunkAllocs, HashLinkChunkBytes,
                HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                HashResizes, HashResizeOldEntries, HashResizeNewEntries),
            KindStr = "polymorphic table",
            MustHaveDu = no
        ),
        write_table_step_stats_header(OutStream, VarName, StepNum, KindStr,
            Lookups, LookupsIsDupl, !IO),
        ( if Lookups > 0 then
            ( if DuNodeAllocs > 0 then
                write_table_step_stats_du(OutStream, DuNodeAllocs, DuNodeBytes,
                    DuArgLookups, DuExistLookups, !IO)
            else
                (
                    MustHaveDu = no
                ;
                    MustHaveDu = yes,
                    error("write_table_step_stats: no du stats")
                )
            ),
            ( if EnumNodeAllocs > 0 then
                write_table_step_stats_enum(OutStream,
                    EnumNodeAllocs, EnumNodeBytes, !IO)
            else
                true
            ),
            ( if HashTableAllocs > 0 then
                write_table_step_stats_hash(OutStream,
                    HashTableAllocs, HashTableBytes,
                    HashLinkChunkAllocs, HashLinkChunkBytes,
                    HashKeyComparesNotDupl, HashKeyComparesIsDupl,
                    HashResizes, HashResizeOldEntries, HashResizeNewEntries,
                    !IO)
            else
                true
            )
        else
            true
        )
    ).

:- func percentage_str(int, int) = string.

percentage_str(A, B) = PercentageStr :-
    Percentage = float(100) * float(A) / float(B),
    string.format("(%.2f%%)", [f(Percentage)], PercentageStr).

%---------------------------------------------------------------------------%

dummy_proc_table_statistics = ProcTableStatistics :-
    TableStats = table_stats(0, 0, []),
    CallTableStats = table_stats_curr_prev(TableStats, TableStats),
    ProcTableStatistics = proc_table_statistics(CallTableStats, no).

%---------------------------------------------------------------------------%
