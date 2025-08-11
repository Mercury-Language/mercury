%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023-2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: edit_distance.m.
% Stability: high.
%
% This module computes the edit distance between two sequences of items.
% Its job is both similar to, and distinct from, the job of edit_seq.m.
% It is similar in that both modules work out the simplest, cheapest way
% to transform one sequence into another. It is distinct because the two
% modules aim to solve different problems.
%
% - edit_seq.m aims to solve the problem of displaying the difference
%   between two given sequences in a way that makes their differences
%   as easy to understand as possible.
%
% - edit_distance.m aims to solve the problem of finding, in a pool of
%   candidate sequences, the candidate that is closest to a given query
%   sequence.
%
% Doing a good job with the second problem requires a mechanism that
% allows callers to specify that a transposition of two elements (such as
% replacing "bc" with "cb", thus transforming e.g. "abcd" into "acbd")
% has a different cost than deleting one element in one place in the sequence
% and inserting it back at another place. This mechanism does not help
% with the first problem at all (since the simplest way to display
% a transposition *is* as a delete/insert pair), and in fact its presence
% in the system would unnecessarily complicate the algorithm.
%
% Technically, this module computes Damerau-Levenshtein distances,
% while edit_seq.m computes Levenshtein distances. (The difference between
% the two is that only the former considers transpositions to be single
% operations.).
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module edit_distance.
:- interface.

:- import_module char.
:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type edit_params(T)
    --->    edit_params(
                % The cost of delete, insert, replace and transpose operations
                % respectively. Only the *relative* values of the costs matter;
                % if these are fixed, their *absolute* values are irrelevant
                % (unless they are so high that they cause arithmetic
                % overflows).
                %
                % For replacement operations, the cost may depend on what
                % is being replaced by what. The intended use case is
                % specifying that replacements that change a letter into
                % the same letter in a different case (as char.to_lower
                % or char.to_upper would do) are cheaper than other kinds of
                % replacements.
                cost_of_delete      :: uint,
                cost_of_insert      :: uint,
                cost_of_replace     :: (func(T, T) = uint),
                cost_of_transpose   :: uint
            ).

    % find_edit_distance(Params, SeqA, SeqB, Distance):
    %
    % Compute Distance as the sum of the costs of the operations
    % needed to transform SeqA into SeqB, where the cost of each kind of
    % edit operation is specified by Params.
    %
:- pred find_edit_distance(edit_params(T)::in, list(T)::in, list(T)::in,
    uint::out) is det.

%---------------------------------------------------------------------------%

    % find_closest_seqs(Params, SourceSeq, TargetSeqs,
    %    BestEditDistance, HeadBestCloseSeq, TailBestCloseSeqs):
    %
    % Given a source sequence SourceSeq, find the sequence in TargetSeqs
    % that has the best (meaning smallest) edit distance from SourceSeq.
    % Return that target sequence as HeadBestCloseSeq, and its edit distance
    % in BestEditDistance. If there are any other sequences in TargetSeqs
    % that also have the same edit distance from SourceSeq, return them
    % in TailBestCloseSeqs. [HeadBestCloseSeq | TailBestCloseSeqs] will contain
    % target sequences in the same order as TargetSeqs.
    %
    % Note that TargetSeqs must be a nonempty list, i.e. it cannot be [].
    % However, it is ok for one of its elements to be an empty sequence.
    %
:- pred find_closest_seqs(edit_params(T)::in, list(T)::in, list(list(T))::in,
    uint::out, list(T)::out, list(list(T))::out) is det.

    % find_best_close_enough_seqs(Params, SourceSeq, TargetSeqs,
    %   MaxEditDistance, BestEditDistance,
    %   HeadBestCloseSeq, TailBestCloseSeqs):
    %
    % This is a version of find_closest_seqs that takes MaxEditDistance
    % as an input, and which ignores any target sequence whose edit distance
    % from SourceSeq is known to exceed MaxEditDistance.
    %
    % If all elements of TargetSeqs have edit distances from SourceSeq
    % that are greater than MaxEditDistance, then this predicate will fail.
    % If this is not the case, i.e. if there are some target sequences
    % whose distance is less than or equal to MaxEditDistance, this predicate
    % will succeed, and will do the same job as find_closest_seqs, but it is
    % intended to do so more quickly, the efficiency gain coming from stopping
    % the computation of exact edit distances as soon as it becomes known
    % that the exact distance would exceed MaxEditDistance.
    %
:- pred find_best_close_enough_seqs(edit_params(T)::in, list(T)::in,
    list(list(T))::in, uint::in, uint::out,
    list(T)::out, list(list(T))::out) is semidet.

%---------------------%

    % find_closest_strings(Params, SourceStr, TargetStrs,
    %    BestEditDistance, HeadBestCloseStr, TailBestCloseStrs):
    %
    % This is an instance of find_closest_seqs that takes care of the
    % necessary conversions between strings and sequences of characters.
    %
:- pred find_closest_strings(edit_params(char)::in, string::in,
    list(string)::in, uint::out, string::out, list(string)::out) is det.

    % find_best_close_enough_strings(Params, SourceStr, TargetStrs,
    %    MaxEditDistance,
    %   BestEditDistance, HeadBestCloseStr, TailBestCloseStrs):
    %
    % This is an instance of find_best_close_enough_seqs that takes care of the
    % necessary conversions between strings and sequences of characters.
    %
:- pred find_best_close_enough_strings(edit_params(char)::in, string::in,
    list(string)::in, uint::in, uint::out, string::out, list(string)::out)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module cord.
:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The code in this module is based on spellcheck.cc in gcc.
%

find_edit_distance(Params, SeqA, SeqB, Cost) :-
    find_edit_distance_ceiling(Params, SeqA, SeqB, no, Cost).

%---------------------------------------------------------------------------%

find_closest_seqs(Params, SourceSeq, TargetSeqs,
        BestCost, HeadBestSeq, TailBestSeqs) :-
    (
        TargetSeqs = [],
        unexpected($pred,
            "Calling find_closest_seqs on an empty list of target sequences" ++
            " does not make sense")
    ;
        TargetSeqs = [HeadTargetSeq | TailTargetSeqs],
        find_edit_distance(Params, SourceSeq, HeadTargetSeq, HeadCost),
        find_closest_seqs_loop(Params, SourceSeq, TailTargetSeqs,
            HeadCost, BestCost, cord.singleton(HeadTargetSeq), BestSeqCord),
        BestSeqList = cord.list(BestSeqCord),
        list.det_head_tail(BestSeqList, HeadBestSeq, TailBestSeqs)
    ).

find_best_close_enough_seqs(Params, SourceSeq, TargetSeqs,
        MaxCost, BestCost, HeadBestSeq, TailBestSeqs) :-
    (
        TargetSeqs = [],
        unexpected($pred,
            "Calling find_best_close_enough_seqs on an empty list" ++
            " of target sequences does not make sense")
    ;
        TargetSeqs = [_ | _],
        find_first_close_enough_seq(Params, SourceSeq, TargetSeqs,
            MaxCost, FirstCost, FirstTargetSeq, RestTargetSeqs),
        find_closest_seqs_loop(Params, SourceSeq, RestTargetSeqs,
            FirstCost, BestCost, cord.singleton(FirstTargetSeq), BestSeqCord),
        BestSeqList = cord.list(BestSeqCord),
        list.det_head_tail(BestSeqList, HeadBestSeq, TailBestSeqs)
    ).

%---------------------------------------------------------------------------%

find_closest_strings(Params, SourceStr, TargetStrs,
        BestCost, HeadBestStr, TailBestStrs) :-
    string.to_char_list(SourceStr, SourceCharSeq),
    list.map(string.to_char_list, TargetStrs, TargetCharSeqs),
    find_closest_seqs(Params, SourceCharSeq, TargetCharSeqs,
        BestCost, HeadBestCharSeq, TailBestCharSeqs),
    string.from_char_list(HeadBestCharSeq, HeadBestStr),
    list.map(string.from_char_list, TailBestCharSeqs, TailBestStrs).

find_best_close_enough_strings(Params, SourceStr, TargetStrs, MaxCost,
        BestCost, HeadBestStr, TailBestStrs) :-
    string.to_char_list(SourceStr, SourceCharSeq),
    list.map(string.to_char_list, TargetStrs, TargetCharSeqs),
    find_best_close_enough_seqs(Params, SourceCharSeq, TargetCharSeqs, MaxCost,
        BestCost, HeadBestCharSeq, TailBestCharSeqs),
    string.from_char_list(HeadBestCharSeq, HeadBestStr),
    list.map(string.from_char_list, TailBestCharSeqs, TailBestStrs).

%---------------------------------------------------------------------------%

:- pred find_first_close_enough_seq(edit_params(T)::in, list(T)::in,
    list(list(T))::in, uint::in, uint::out, list(T)::out, list(list(T))::out)
    is semidet.

find_first_close_enough_seq(Params, SourceSeq,
        [HeadTargetSeq | TailTargetSeqs], MaxCost,
        FirstCost, FirstTargetSeq, RestTargetSeqs) :-
    find_edit_distance_ceiling(Params, SourceSeq, HeadTargetSeq,
        yes(MaxCost), HeadCost),
    ( if HeadCost =< MaxCost then
        FirstCost = HeadCost,
        FirstTargetSeq = HeadTargetSeq,
        RestTargetSeqs = TailTargetSeqs
    else
        find_first_close_enough_seq(Params, SourceSeq, TailTargetSeqs, MaxCost,
            FirstCost, FirstTargetSeq, RestTargetSeqs)
    ).

:- pred find_closest_seqs_loop(edit_params(T)::in, list(T)::in,
    list(list(T))::in, uint::in, uint::out,
    cord(list(T))::in, cord(list(T))::out) is det.

find_closest_seqs_loop(_, _, [], !Cost, !CostSeqCord).
find_closest_seqs_loop(Params, SourceSeq, [HeadTargetSeq | TailTargetSeqs],
        !Cost, !CostSeqCord) :-
    find_edit_distance_ceiling(Params, SourceSeq, HeadTargetSeq,
        yes(!.Cost), HeadCost),
    ( if HeadCost < !.Cost then
        % Update the best known cost, ...
        !:Cost = HeadCost,
        % ... and throw away all the target sequences
        % with the *old* best known cost.
        !:CostSeqCord = cord.singleton(HeadTargetSeq),
        find_closest_seqs_loop(Params, SourceSeq, TailTargetSeqs,
            !Cost, !CostSeqCord)
    else if HeadCost = !.Cost then
        % Keep the best known cost the same, and add this target sequence
        % to the list of target sequences at this cost.
        cord.snoc(HeadTargetSeq, !CostSeqCord),
        find_closest_seqs_loop(Params, SourceSeq, TailTargetSeqs,
            !Cost, !CostSeqCord)
    else
        find_closest_seqs_loop(Params, SourceSeq, TailTargetSeqs,
            !Cost, !CostSeqCord)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This version of find_edit_distance allows the caller to specify
    % a cost ceiling, beyond which the cost does not matter (because the
    % caller intends to throw away any edits whose cost is beyond the ceiling).
    % Once the minimum cost in a row exceeds the ceiling (if there is one),
    % we stop computing further rows, because the cost can only increase
    % from then on. In such cases, the cost we return will be inaccurate,
    % but this is ok; since it will be beyond the ceiling, our caller
    % will throw the edit away.
    %
:- pred find_edit_distance_ceiling(edit_params(T)::in,
    list(T)::in, list(T)::in, maybe(uint)::in, uint::out) is det.

find_edit_distance_ceiling(Params, SeqA, SeqB, MaybeCeiling, Cost) :-
    % The gcc code on which the implementation of this predicate is based
    % is included at the bottom of this module in a big comment. It has been
    % reformatted to follow *our* code style for C (though the code is in C++).
    trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
        io.output_stream(CurStream, !IO),
        io.format(CurStream, "\nfind_edit_distance(%s, %s)\n",
            [s(string.string(SeqA)), s(string.string(SeqB))], !IO)
    ),

    list.length(SeqA, LenIntA),
    list.length(SeqB, LenIntB),
    LenA = uint.det_from_int(LenIntA),
    LenB = uint.det_from_int(LenIntB),
    ItemMapA = array.from_list(SeqA),
    ItemMapB = array.from_list(SeqB),

    InsertCost = Params ^ cost_of_insert,
    DeleteCost = Params ^ cost_of_delete,

    ( if LenA = 0u then
        Cost = LenB * InsertCost
    else if LenB = 0u then
        Cost = LenA * DeleteCost
    else
        % The original comment from the gcc code was:
        %
        %   We effectively build a matrix where each (i, j) contains
        %   the distance between the prefix strings s[0:j] and t[0:i].
        %   Rather than actually build an (len_t + 1) * (len_s + 1) matrix,
        %   we simply keep track of the last two rows, v_one_ago and v_two_ago,
        %   and a new row, v_next.
        %
        % In this Mercury version, we use
        %
        % - ItemMapA  to represent s
        % - ItemMapB  to represent t
        % - LenA      to represent len_s
        % - LenB      to represent len_t
        % - RowTwoAgo to represent v_two_ago
        % - RowOneAgo to represent v_one_ago
        % - RowNext   to represent v_next
        %
        % Just as in the gcc code i iterates from 0 up to len_t-1,
        % RowNum here iterates from 0u up to LenB - 1u.
        % And just as in the gcc code j iterates from 0 up to len_s-1,
        % J here iterates from 0u up to LenA - 1u.
        %
        % The Mercury compiler uses this predicate mostly when constructing
        % "did you mean" addendums for diagnostic messages reporting
        % failed name lookups. In modules that are large themselves, or
        % (more likely) in modules that import many other modules, the set of
        % names to which we want to compare the name whose lookup failed
        % can be quite large. In such situations, the code constructing
        % the error message will invoke find_edit_distance thousands of times.
        %
        % The original Mercury version of this code used maps to implement
        % both ItemMap{A,B} and Row{OneAgo,TwoAgo,Next}. In some cases,
        % this caused the construction of "did you mean" addendums to take
        % several seconds (much more time than than was needed by the *rest*
        % of the compiler), which was annoying.
        %
        % The two main reasons of this slowdown were almost certainly
        %
        % - the logN factor in the complexity of pretty much all operations
        %   on maps, and
        %
        % - the allocation of logN heap cells on every update of every element
        %   of every map.
        %
        % Our fix for the first cause is to switch the representation of
        % ItemMap{A,B} and Row{OneAgo,TwoAgo,Next} to arrays.
        % This also helps with the second cause, but we can do better still
        % by only ever using the *same* three arrays, meaning that we don't
        % allocate any memory in build_rows at all.
        RowTwoAgo = array.init(uint.cast_to_int(LenA + 1u), 0u),
        init_delete_cost_row(DeleteCost, 0u, LenA, RowOneAgoList),
        RowOneAgo = array.from_list(RowOneAgoList),
        RowSpare = array.init(uint.cast_to_int(LenA + 1u), 0u),

        build_rows(Params, LenA, LenB, ItemMapA, ItemMapB, MaybeCeiling,
            0u, RowTwoAgo, RowOneAgo, RowSpare, FinalRow),
        array.lookup(FinalRow, uint.cast_to_int(LenA), Cost)
    ).

%---------------------%

    % The first row is for the case of an empty target string,
    % which we can reach by deleting every character in the source string.
    %
:- pred init_delete_cost_row(uint::in, uint::in, uint::in, list(uint)::out)
    is det.

init_delete_cost_row(DeleteCost, ColNum, MaxColNum, Row) :-
    ( if ColNum =< MaxColNum then
        init_delete_cost_row(DeleteCost, ColNum + 1u, MaxColNum, RowTail),
        Row = [ColNum * DeleteCost | RowTail]
    else
        Row = []
    ).

%---------------------%

:- pred build_rows(edit_params(T)::in, uint::in, uint::in,
    array(T)::in, array(T)::in, maybe(uint)::in, uint::in,
    array(uint)::array_di, array(uint)::array_di,
    array(uint)::array_di, array(uint)::array_uo) is det.

build_rows(Params, LenA, LenB, ItemMapA, ItemMapB, MaybeCeiling,
        RowNum, RowTwoAgo0, RowOneAgo0, RowSpare, FinalRow) :-
    trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
        io.output_stream(CurStream, !IO),
        dump_row(CurStream, RowNum, RowOneAgo0, !IO)
    ),
    ( if RowNum < LenB then
        RowNext0 = RowSpare,
        % The initial column is for the case of an empty source string;
        % we can reach prefixes of the target string of length i (RowNum)
        % by inserting i characters.
        InsertCost = Params ^ cost_of_insert,
        InsertCostInRow = (RowNum + 1u) * InsertCost,
        array.set(0, InsertCostInRow, RowNext0, RowNext1),

        MinCostInRow0 = InsertCostInRow,
        build_columns(Params, LenA, LenB, ItemMapA, ItemMapB, RowNum, 0u,
            RowTwoAgo0, RowTwoAgo, RowOneAgo0, RowOneAgo, RowNext1, RowNext,
            MinCostInRow0, MinCostInRow),
        ( if
            MaybeCeiling = yes(Ceiling),
            MinCostInRow > Ceiling
        then
            FinalRow = RowNext
        else
            build_rows(Params, LenA, LenB, ItemMapA, ItemMapB, MaybeCeiling,
                RowNum + 1u, RowOneAgo, RowNext, RowTwoAgo, FinalRow)
        )
    else
        FinalRow = RowOneAgo0
    ).

    % build_columns(Params, LenA, LenB, ItemMapA, ItemMapB, RowNum, J,
    %   !RowTwoAgo, !RowOneAgo, !RowNext):
    %
    % Compute the slots of the !:RowNext starting at ColNum (J+1)
    % by considering their neighbors to the north, west and northwest.
    %
    % Obviously, this means that this predicate updates !RowNext.
    % It also has array_{di,uo} pairs of arguments for !RowTwoAgo
    % and !RowOneAgo, but this predicate leaves those two state variables
    % unchanged. We need to return the "new" values of !:RowTwoAgo
    % and !.RowOneAgo to our caller to allow the mode checker to see
    % that these arrays are still unique, which allows our caller
    % to reuse the same three arrays over and over, thus avoiding
    % ever allocating a fourth array.
    %
:- pred build_columns(edit_params(T)::in, uint::in, uint::in,
    array(T)::in, array(T)::in, uint::in, uint::in,
    array(uint)::array_di, array(uint)::array_uo,
    array(uint)::array_di, array(uint)::array_uo,
    array(uint)::array_di, array(uint)::array_uo,
    uint::in, uint::out) is det.

build_columns(Params, LenA, LenB, ItemMapA, ItemMapB, RowNum, J,
        !RowTwoAgo, !RowOneAgo, !RowNext, !MinCostInRow) :-
    ( if J < LenA then
        ColNum = J + 1u,
        % Note that "here", the position from which are looking,
        % is RowNext(ColNum). This means that
        %
        % - left        is RowNext(J)
        % - up          is RowOneAgo(J + 1u), i.e. RowOneAgo(ColNum)
        % - diag        is RowOneAgo(J)
        % - trans_diag  is RowTwoAgo(J - 1u)

        array.lookup(!.RowOneAgo, uint.cast_to_int(J), DiagCost),
        I = RowNum,
        array.lookup(ItemMapA, uint.cast_to_int(J), CurAJ),
        array.lookup(ItemMapB, uint.cast_to_int(I), CurBI),
        ( if CurAJ = CurBI then
            MinCost = DiagCost,
            trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
                io.output_stream(CurStream, !IO),
                io.format(CurStream, "nc = %u; ", [u(MinCost)], !IO)
            )
        else
            ReplacementCostFunc = Params ^ cost_of_replace,
            ReplacementCost = DiagCost + ReplacementCostFunc(CurAJ, CurBI),

            array.lookup(!.RowNext, uint.cast_to_int(J), LeftCost),
            DeleteCost = LeftCost + Params ^ cost_of_delete,

            array.lookup(!.RowOneAgo, uint.cast_to_int(ColNum), UpCost),
            InsertCost = UpCost + Params ^ cost_of_insert,

            MinCost0 = min(InsertCost, min(DeleteCost, ReplacementCost)),

            ( if
                I > 0u,
                J > 0u,
                array.lookup(ItemMapB, uint.cast_to_int(I - 1u), CurAJ),
                array.lookup(ItemMapA, uint.cast_to_int(J - 1u), CurBI)
            then
                array.lookup(!.RowTwoAgo, uint.cast_to_int(J - 1u),
                    TransDiagCost),
                TransposeCost = TransDiagCost + Params ^ cost_of_transpose,
                MinCost = min(MinCost0, TransposeCost),
                trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
                    io.output_stream(CurStream, !IO),
                    io.format(CurStream, "i%u d%u r%u t%u = %u; ",
                        [u(InsertCost), u(DeleteCost), u(ReplacementCost),
                        u(TransposeCost), u(MinCost)], !IO)
                )
            else
                MinCost = MinCost0,
                trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
                    io.output_stream(CurStream, !IO),
                    io.format(CurStream, "i%u d%u r%u = %u; ",
                        [u(InsertCost), u(DeleteCost), u(ReplacementCost),
                        u(MinCost)], !IO)
                )
            )
        ),
        array.set(uint.cast_to_int(ColNum), MinCost, !RowNext),
        ( if MinCost < !.MinCostInRow then
            !:MinCostInRow = MinCost
        else
            true
        ),
        build_columns(Params, LenA, LenB, ItemMapA, ItemMapB,
            RowNum, J + 1u, !RowTwoAgo, !RowOneAgo, !RowNext, !MinCostInRow)
    else
        trace [compile_time(flag("debug_edit_distance")), io(!IO)] (
            io.output_stream(CurStream, !IO),
            io.nl(CurStream, !IO)
        )
    ).

%---------------------%

:- pred dump_row(io.text_output_stream::in, uint::in, array(uint)::in,
    io::di, io::uo) is det.

dump_row(OutStream, RowNum, RowMap, !IO) :-
    io.format(OutStream, "row #%2u: [", [u(RowNum)], !IO),
    array.to_list(RowMap, RowAL),
    dump_columns(OutStream, 0u, RowAL, !IO),
    io.write_string(OutStream, "]\n", !IO),
    io.flush_output(OutStream, !IO).

:- pred dump_columns(io.text_output_stream::in, uint::in, list(uint)::in,
    io::di, io::uo) is det.

dump_columns(_, _, [], !IO).
dump_columns(OutStream, ColNum, [Value | Values], !IO) :-
    io.format(OutStream, "%2u: %3u, ", [u(ColNum), u(Value)], !IO),
    dump_columns(OutStream, ColNum + 1u, Values, !IO).

%---------------------------------------------------------------------------%
%
% edit_distance_t
% get_edit_distance (const char *s, int len_s, const char *t, int len_t)
% {
%     const bool debug = false;
%
%     if (debug) {
%         printf ("s: \"%s\" (len_s=%i)\n", s, len_s);
%         printf ("t: \"%s\" (len_t=%i)\n", t, len_t);
%     }
%
%     if (len_s == 0) {
%         return BASE_COST * len_t;
%     }
%     if (len_t == 0) {
%         return BASE_COST * len_s;
%     }
%
%     // We effectively build a matrix where each (i, j) contains
%     // the distance between the prefix strings s[0:j] and t[0:i].
%     // Rather than actually build an (len_t + 1) * (len_s + 1) matrix,
%     // we simply keep track of the last two rows, v_one_ago and v_two_ago,
%     // and a new row, v_next.
%     edit_distance_t *v_two_ago = new edit_distance_t[len_s + 1];
%     edit_distance_t *v_one_ago = new edit_distance_t[len_s + 1];
%     edit_distance_t *v_next =    new edit_distance_t[len_s + 1];
%
%     // The first row is for the case of an empty target string,
%     // which we can reach by deleting every character in the source string.
%     for (int i = 0; i < len_s + 1; i++) {
%         v_one_ago[i] = i * BASE_COST;
%     }
%
%     // Build successive rows.
%     for (int i = 0; i < len_t; i++) {
%         if (debug) {
%             printf ("i:%i v_one_ago = ", i);
%             for (int j = 0; j < len_s + 1; j++) {
%                 printf ("%i ", v_one_ago[j]);
%             }
%             printf ("\n");
%         }
%
%         // The initial column is for the case of an empty source string;
%         // we can reach prefixes of the target string of length i
%         // by inserting i characters.
%         v_next[0] = (i + 1) * BASE_COST;
%
%         // Build the rest of the row by considering neighbors
%         // to the north, west and northwest.
%         for (int j = 0; j < len_s; j++) {
%             edit_distance_t cost;
%
%             if (s[j] == t[i]) {
%                 cost = 0;
%             } else if (TOLOWER (s[j]) == TOLOWER (t[i])) {
%                 cost = CASE_COST;
%             } else {
%                 cost = BASE_COST;
%             }
%
%             edit_distance_t deletion     = v_next[j] + BASE_COST;
%             edit_distance_t insertion    = v_one_ago[j + 1] + BASE_COST;
%             edit_distance_t substitution = v_one_ago[j] + cost;
%             edit_distance_t cheapest = MIN (deletion, insertion);
%             cheapest = MIN (cheapest, substitution);
%             if (i > 0 && j > 0 && s[j] == t[i - 1] && s[j - 1] == t[i]) {
%                 edit_distance_t transposition = v_two_ago[j - 1] + BASE_COST;
%                 cheapest = MIN (cheapest, transposition);
%             }
%             v_next[j + 1] = cheapest;
%         }
%
%         // Prepare to move on to next row.
%         for (int j = 0; j < len_s + 1; j++) {
%             v_two_ago[j] = v_one_ago[j];
%             v_one_ago[j] = v_next[j];
%         }
%     }
%
%     if (debug) {
%         printf ("final v_next = ");
%         for (int j = 0; j < len_s + 1; j++) {
%             printf ("%i ", v_next[j]);
%         }
%         printf ("\n");
%     }
%
%     edit_distance_t result = v_next[len_s];
%     delete[] v_two_ago;
%     delete[] v_one_ago;
%     delete[] v_next;
%     return result;
% }
%
%---------------------------------------------------------------------------%
:- end_module edit_distance.
%---------------------------------------------------------------------------%
