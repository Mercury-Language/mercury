%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: edit_seq.m.
% Stability: medium.
%
% This module finds an edit sequence, which means that given two sequences
% of items, it finds the shortest sequence of edit operations (deletes,
% inserts and/or replaces) that will transform the first sequence
% into the second.
%
% The code is a naive implementation of the Wagner-Fischer algorithm,
% which is documented on its own wikipedia page.
%
% Given two lists of length M and N, its complexity is O(MN) in both
% time and space, so it is suitable for use only on reasonably short lists.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module edit_seq.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Given two item sequences A and B, the edit sequence is the sequence
    % of edit operations that transforms sequence A into sequence B.
    %
    % Item numbers start at 1. The item numbers in edit operations reflect
    % the *original* position of the relevant item, i.e. they are not affected
    % by any edit operations that take place before that position.
    %
:- type edit_seq(T) == list(edit(T)).
:- type edit(T)
    --->    delete(int)
            % Delete item #N in sequence A.

    ;       insert(int, T)
            % Insert the given item after item #N in sequence A.

    ;       replace(int, T).
            % Replace item #N in sequence A with the given item.

:- type edit_params
    --->    edit_params(
                % The cost of delete, insert and replace operations
                % respectively. Only the *relative* values of the costs matter;
                % if these are fixed, their *absolute* values are irrelevant
                % (unless they are so high that they cause arithmetic
                % overflows).
                cost_of_delete      :: int,
                cost_of_insert      :: int,
                cost_of_replace     :: int
            ).

    % find_shortest_edit_seq(Params, SeqA, SeqB, Edits):
    %
    % Compute Edits as the cheapest sequence of edit operations
    % that will transform SeqA into SeqB, where the cost of each kind of
    % edit operation is specified by Params.
    %
:- pred find_shortest_edit_seq(edit_params::in, list(T)::in, list(T)::in,
    edit_seq(T)::out) is det.

%---------------------------------------------------------------------------%

    % A diff_seq represents a unified diff such as the output of "diff -u".
    % Each line (or in general, one item) in it can be an item from SeqA
    % that is left unchanged, an item from SeqA that is to be deleted, or
    % an item (from SeqB) that is to be inserted.
:- type diff_seq(T) == list(diff(T)).
:- type diff(T)
    --->    unchanged(T)
    ;       deleted(T)
    ;       inserted(T).

    % Given an edit sequence computed by find_shortest_edit_seq, return
    % the unified diff representing that edit sequence.
    %
    % The main difference between the edit sequence and the diff sequence
    % is that given several consecutive replace edits, a naive representation
    % of those edit operations would output interleaved pairs of items
    % to be deleted and inserted, while the diff sequence would output
    % *all* of the items to be deleted by those replace operations *before*
    % printing the insertions of their replacements.
    %
:- pred find_diff_seq(list(T)::in, edit_seq(T)::in, diff_seq(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

find_shortest_edit_seq(Params, SeqA, SeqB, Edits) :-
    % The Wagner-Fischer algorithm. One difference from the algorithm's
    % wikipedia page is that we iterate over the table in row major order,
    % not column major. Since the algorithm is symmetric about the table's
    % diagonal, this should not matter. The other difference is that we don't
    % just compute the edit *distance*; we compute the edit *sequence* as well.
    %
    % Note that the complexity of this algorithm is O(MN) where
    % M and N are the lengths of SeqA and SeqB.
    list.length(SeqA, LenA),
    list.length(SeqB, LenB),
    some [!Table] (
        init_table(!:Table),
        add_entry(0, 0, entry(cord.init, 0), !Table),
        init_row_zero_inserts(Params, 1, SeqB, cord.init, !Table),
        init_col_zero_deletes(Params, 1, SeqA, cord.init, !Table),
        process_rows(Params, 1, SeqA, SeqB, !Table),
        lookup_entry(!.Table, LenA, LenB, CornerEntry),
        Edits = cord.list(CornerEntry ^ e_edits)
    ).

:- pred init_row_zero_inserts(edit_params::in, int::in, list(T)::in,
    cord(edit(T))::in, dynprog_table(T)::in, dynprog_table(T)::out) is det.

init_row_zero_inserts(_Params, _ColNum, [], _CurEdits, !Table).
init_row_zero_inserts(Params, ColNum, [HeadSeqB | TailSeqB], PrevEdits,
        !Table) :-
    CurEdits = cord.snoc(PrevEdits, insert(1, HeadSeqB)),
    Entry = entry(CurEdits, ColNum * Params ^ cost_of_insert),
    add_entry(0, ColNum, Entry, !Table),
    init_row_zero_inserts(Params, ColNum + 1, TailSeqB, CurEdits, !Table).

:- pred init_col_zero_deletes(edit_params::in, int::in, list(T)::in,
    cord(edit(T))::in, dynprog_table(T)::in, dynprog_table(T)::out) is det.

init_col_zero_deletes(_Params, _RowNum, [], _CurEdits, !Table).
init_col_zero_deletes(Params, RowNum, [_HeadSeqA | TailSeqA],
        PrevEdits, !Table) :-
    CurEdits = cord.snoc(PrevEdits, delete(RowNum)),
    Entry = entry(CurEdits, RowNum * Params ^ cost_of_delete),
    add_entry(RowNum, 0, Entry, !Table),
    init_col_zero_deletes(Params, RowNum + 1, TailSeqA, CurEdits, !Table).

:- pred process_rows(edit_params::in, int::in, list(T)::in, list(T)::in,
    dynprog_table(T)::in, dynprog_table(T)::out) is det.

process_rows(_Params, _RowNum, [], _SeqB, !Table).
process_rows(Params, RowNum, [HeadSeqA | TailSeqA], SeqB, !Table) :-
    process_columns(Params, RowNum, HeadSeqA, 1, SeqB, !Table),
    process_rows(Params, RowNum + 1, TailSeqA, SeqB, !Table).

:- pred process_columns(edit_params::in, int::in, T::in, int::in, list(T)::in,
    dynprog_table(T)::in, dynprog_table(T)::out) is det.

process_columns(_Params, _RowNum, _RowA, _ColNum, [], !Table).
process_columns(Params, RowNum, RowA, ColNum, [HeadSeqB | TailSeqB], !Table) :-
    process_entry(Params, RowNum, RowA, ColNum, HeadSeqB, !Table),
    process_columns(Params, RowNum, RowA, ColNum + 1, TailSeqB, !Table).

:- pred process_entry(edit_params::in, int::in, T::in, int::in, T::in,
    dynprog_table(T)::in, dynprog_table(T)::out) is det.

process_entry(Params, RowNum, A, ColNum, B, !Table) :-
    ( if A = B then
        lookup_entry(!.Table, RowNum - 1, ColNum - 1, Entry)
    else
        lookup_entry(!.Table, RowNum - 1, ColNum, EntryUp),
        lookup_entry(!.Table, RowNum, ColNum - 1, EntryLeft),
        lookup_entry(!.Table, RowNum - 1, ColNum - 1, EntryDiag),
        EntryUp = entry(EditsUp, CostUp0),
        EntryLeft = entry(EditsLeft, CostLeft0),
        EntryDiag = entry(EditsDiag, CostDiag0),
        CostUp = CostUp0 + Params ^ cost_of_delete,
        CostLeft = CostLeft0 + Params ^ cost_of_insert,
        CostDiag = CostDiag0 + Params ^ cost_of_replace,
        % The order of the tests here can be important when the parameters
        % favour delete/insert pairs over replace operations.
        %
        % By preferring insert here, we create delete(R)/insert(R, ...) pairs.
        % If we preferred delete here, we would create insert(R, ...)/delete(R)
        % pairs, which find_diff_cord would not be able to handle.
        % (In the left and up predecessors, we would have just an insert
        % or a delete; the choice between insert and delete exists only
        % on the *second* operation of the pair.)
        ( if CostLeft =< CostUp, CostLeft =< CostDiag then
            % We can transform SeqA[1 .. RowNum] into SeqB[1 .. ColNum-1]
            % in CostLeft0 steps.
            % We can thus transfrom SeqA[1 .. RowNum] into SeqB[1 .. ColNum]
            % in CostLeft0 + ConstInsert steps by inserting B after #RowNum
            % in SeqA.
            Edits = cord.snoc(EditsLeft, insert(RowNum, B)),
            Entry = entry(Edits, CostLeft)
        else if CostUp =< CostLeft, CostUp =< CostDiag then
            % We can transform SeqA[1 .. RowNum-1] into SeqB[1 .. ColNum]
            % in CostUp0 steps.
            % We can thus transform SeqA[1 .. RowNum] into SeqB[1 .. ColNum]
            % in CostUp0 + CostDelete steps by deleting item #RowNum from SeqA.
            Edits = cord.snoc(EditsUp, delete(RowNum)),
            Entry = entry(Edits, CostUp)
        else
            % We can transfrom SeqA[1 .. RowNum-1] into SeqB[1 .. ColNum-1]
            % in CostDiag0 steps.
            % We can thus transfrom SeqA[1 .. RowNum] into SeqB[1 .. ColNum]
            % in CostDiag0 + CostReplace steps by replacing item #RowNum
            % in SeqA by B.
            Edits = cord.snoc(EditsDiag, replace(RowNum, B)),
            Entry = entry(Edits, CostDiag)
        )
    ),
    add_entry(RowNum, ColNum, Entry, !Table).

%---------------------------------------------------------------------------%
%
% The dynamic programming table.
%
% The row numbers #R must fall into the range [0, LenA].
% The column numbers #C must fall into the range [0, LenB].
%
% The entry at row #R, column #C contains both the shortest sequence of
% edit operations required to transform the first #R items of SeqA
% into the first #C items of SeqB, and the cost of that sequence according to
% the parameters, which give the cost of each basic edit operation.
%

:- type dynprog_table(T) == map(int, map(int, dynprog_entry(T))).

:- type dynprog_entry(T)
    --->    entry(
                e_edits     ::  cord(edit(T)),
                e_cost      ::  int
            ).

:- pred init_table(dynprog_table(T)::out) is det.

init_table(Table) :-
    map.init(Table).

:- pred lookup_entry(dynprog_table(T)::in, int::in, int::in,
    dynprog_entry(T)::out) is det.

lookup_entry(Table, RowNum, ColNum, Entry) :-
    map.lookup(Table, RowNum, Row),
    map.lookup(Row, ColNum, Entry).

:- pred add_entry(int::in, int::in, dynprog_entry(T)::in,
    dynprog_table(T)::in, dynprog_table(T)::out) is det.

add_entry(RowNum, ColNum, Entry, !Table) :-
    ( if map.search(!.Table, RowNum, Row0) then
        map.det_insert(ColNum, Entry, Row0, Row),
        map.det_update(RowNum, Row, !Table)
    else
        Row = map.singleton(ColNum, Entry),
        map.det_insert(RowNum, Row, !Table)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

find_diff_seq(SeqA, Edits, DiffSeq) :-
    Deletes0 = cord.init,
    Inserts0 = cord.init,
    DiffCord0 = cord.init,
    find_diff_cord(1, SeqA, Edits, Deletes0, Inserts0, DiffCord0, DiffCord),
    DiffSeq = cord.list(DiffCord).

:- type diff_cord(T) == cord(diff(T)).

:- pred find_diff_cord(int::in, list(T)::in,
    edit_seq(T)::in, diff_cord(T)::in, diff_cord(T)::in,
    diff_cord(T)::in, diff_cord(T)::out) is det.

find_diff_cord(CurA, SeqA, [Edit | Edits], !.Deletes, !.Inserts, !Diffs) :-
    (
        Edit = delete(A),
        uncons(SeqA, HeadA, TailA),
        ( if A = CurA then
            !:Deletes = cord.snoc(!.Deletes, deleted(HeadA)),
            find_diff_cord(CurA + 1, TailA, Edits,
                !.Deletes, !.Inserts, !Diffs)
        else
            flush_deletes_inserts(!Deletes, !Inserts, !Diffs),
            !:Diffs = cord.snoc(!.Diffs, unchanged(HeadA)),
            find_diff_cord(CurA + 1, TailA, [Edit | Edits],
                !.Deletes, !.Inserts, !Diffs)
        )
    ;
        Edit = insert(A, Item),
        % The insert(A, Item) operation means inserting Item *after* item A
        % in SeqA. We implement this as inserting Item before item A+1
        % *if* there is an item A+1, and before the end of the list otherwise.
        ( if A+1 = CurA then
            !:Inserts = cord.snoc(!.Inserts, inserted(Item)),
            find_diff_cord(CurA, SeqA, Edits,
                !.Deletes, !.Inserts, !Diffs)
        else
            (
                SeqA = [],
                !:Inserts = cord.snoc(!.Inserts, inserted(Item)),
                find_diff_cord(CurA, SeqA, Edits,
                    !.Deletes, !.Inserts, !Diffs)
            ;
                SeqA = [HeadA | TailA],
                flush_deletes_inserts(!Deletes, !Inserts, !Diffs),
                !:Diffs = cord.snoc(!.Diffs, unchanged(HeadA)),
                find_diff_cord(CurA + 1, TailA, [Edit | Edits],
                    !.Deletes, !.Inserts, !Diffs)
            )
        )
    ;
        Edit = replace(A, Item),
        uncons(SeqA, HeadA, TailA),
        ( if CurA = A then
            !:Deletes = cord.snoc(!.Deletes, deleted(HeadA)),
            !:Inserts = cord.snoc(!.Inserts, inserted(Item)),
            find_diff_cord(CurA + 1, TailA, Edits,
                !.Deletes, !.Inserts, !Diffs)
        else
            flush_deletes_inserts(!Deletes, !Inserts, !Diffs),
            !:Diffs = cord.snoc(!.Diffs, unchanged(HeadA)),
            find_diff_cord(CurA + 1, TailA, [Edit | Edits],
                !.Deletes, !.Inserts, !Diffs)
        )
    ).
find_diff_cord(_, SeqA, [], !.Deletes, !.Inserts, !Diffs) :-
    flush_deletes_inserts(!.Deletes, _, !.Inserts, _, !Diffs),
    LeftOvers = list.map(func(I) = unchanged(I), SeqA),
    !:Diffs = !.Diffs ++ cord.from_list(LeftOvers).

:- pred uncons(list(T)::in, T::out, list(T)::out) is det.

uncons([], _, _) :-
    unexpected($pred, "empty list").
uncons([Head | Tail], Head, Tail).

:- pred flush_deletes_inserts(
    diff_cord(T)::in, diff_cord(T)::out,
    diff_cord(T)::in, diff_cord(T)::out,
    diff_cord(T)::in, diff_cord(T)::out) is det.

flush_deletes_inserts(!Deletes, !Inserts, !Diffs) :-
    !:Diffs = !.Diffs ++ !.Deletes ++ !.Inserts,
    !:Deletes = cord.init,
    !:Inserts = cord.init.

%---------------------------------------------------------------------------%
:- end_module edit_seq.
%---------------------------------------------------------------------------%
