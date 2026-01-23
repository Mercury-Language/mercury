%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2019, 2022, 2024-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_array2d.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: medium.
%
% Two-dimensional rectangular (i.e. not ragged) version arrays.
%
% See the header comments in version_array.m for more details about version
% structures.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_array2d.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % A version_array2d is a two-dimensional version array which stores
    % the array in row-major order, meaning that first it stores
    % the elements of the first row in left-to-right order, then the elements
    % of the second row in the same order, and so on.
    %
:- type version_array2d(T).

    % version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]):
    %
    % Given a list of NumRows (M) elements, each of which is a list of
    % NumColumns (N) elements, returns the two-dimensional arrays
    % with NumRows rows and NumColumns columns.
    %
    % Throws an exception if the sublists are not all the same length.
    %
    % When given the empty list as input, it sets not just NumRows
    % but also NumColumns to zero.
    %
:- func version_array2d(list(list(T))) = version_array2d(T).

    % init(NumRows, NumColumns, DefaultValue):
    % uinit(NumRows, NumColumns, DefaultValue):
    %
    % Returns the two-dimensional array with the given numbers of rows and
    % columns in which every element is DefaultValue.
    %
    % Throws an exception if either NumRows or NumColumns is negative.
    %
:- func init(int, int, T) = version_array2d(T).
:- func uinit(uint, uint, T) = version_array2d(T).

    % bounds(VersionArray2d, NumRows, NumColumns):
    % ubounds(VersionArray2d, NumRows, NumColumns):
    %
    % Given VersionArray2d, returns the number of rows and columns it has.
    %
:- pred bounds(version_array2d(T)::in, int::out, int::out) is det.
:- pred ubounds(version_array2d(T)::in, uint::out, uint::out) is det.

    % in_bounds(VersionArray2d, RowNum, ColumnNum):
    % in_ubounds(VersionArray2d, RowNum, ColumnNum):
    %
    % Succeeds if-and-only-if both RowNum and ColumnNum are in-bounds
    % for VersionArray2d. If the sizes of the two dimensions of VersionArray2d
    % are NumRows and NumColumns respectively, this means that this predicate
    % succeeds if-and-only-if both 0 =< RowNum < NumRows and
    % 0 =< ColumnNum < NumColumns are true.
    %
:- pred in_bounds(version_array2d(T)::in, int::in, int::in) is semidet.
:- pred in_ubounds(version_array2d(T)::in, uint::in, uint::in) is semidet.

    % lookup(VersionArray2d, RowNum, ColumnNum, Value):
    % ulookup(VersionArray2d, RowNum, ColumnNum, Value):
    % VersionArray2d ^ elem(RowNum, ColumnNum) = Value:
    % VersionArray2d ^ uelem(RowNum, ColumnNum) = Value:
    %
    % Return the value at the given row and column numbers in VersionArray2d.
    % Note that both row and column numbers start from zero.
    %
    % Throw an exception if either RowNum or ColumnNum is out of bounds.
    %
:- pred lookup(version_array2d(T)::in, int::in, int::in, T::out) is det.
:- pred ulookup(version_array2d(T)::in, uint::in, uint::in, T::out) is det.
:- func elem(int, int, version_array2d(T)) = T.
:- func uelem(uint, uint, version_array2d(T)) = T.

    % set(RowNum, ColumnNum, X, !VersionArray2d):
    % uset(RowNum, ColumnNum, X, !VersionArray2d):
    % ( !.VersionArray2d ^ elem(RowNum, ColumnNum) := X ) = !:VersionArray2d:
    % ( !.VersionArray2d ^ uelem(RowNum, ColumnNum) := X ) = !:VersionArray2d:
    %
    % Return a version of the initial version_array2d that contains
    % all the same values, with the exception that the element at the
    % given row and column number is set to X.
    %
    % Throw an exception if either RowNum or ColumnNum is out of bounds.
    %
:- pred set(int::in, int::in, T::in,
    version_array2d(T)::in, version_array2d(T)::out) is det.
:- pred uset(uint::in, uint::in, T::in,
    version_array2d(T)::in, version_array2d(T)::out) is det.
:- func 'elem :='(int, int, version_array2d(T), T) = version_array2d(T).
:- func 'uelem :='(uint, uint, version_array2d(T), T) = version_array2d(T).

    % lists(version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]])) =
    %     [[X11, ..., X1N], ..., [XM1, ..., XMN]]
    %
:- func lists(version_array2d(T)) = list(list(T)).

    % copy(OldVersionArray2d) = VersionArray2d:
    %
    % Returns a copy of OldVersionArray2d that has O(1) access time
    % to all its elements.
    %
:- func copy(version_array2d(T)) = version_array2d(T).

    % resize(OldVersionArray2d, NumRows, NumColumns, DefaultValue)
    %   = VersionArray2d:
    % uresize(OldVersionArray2d, NumRows, NumColumns, DefaultValue)
    %   = VersionArray2d:
    %
    % Returns a copy of OldVersionArray2d that is resized to
    % NumRows * NumColumns. Items with coordinates that exist in
    % OldVersionArray2d are copied from OldVersionArray2d; all other items
    % are initialised to DefaultValue.
    %
    % Throws an exception if either NumRows < 0 or NumColumns < 0.
    %
:- func resize(version_array2d(T), int, int, T) = version_array2d(T).
:- func uresize(version_array2d(T), uint, uint, T) = version_array2d(T).

    % unsafe_rewind(OldVersionArray2d) = VersionArray2d
    %
    % Returns a new 2d version array that has O(1) access time to all its
    % elements, at the cost of rendering the contents of OldVersionArray2d
    % and all its descendants undefined.
    %
    % Call this function *only* if you are absolutely certain that
    % there are no remaining live references to either OldVersionArray2d
    % or to any of its descendants.
    %
:- func unsafe_rewind(version_array2d(T)) = version_array2d(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module version_array.

%---------------------------------------------------------------------------%

    % version_array2d(NumRows, NumColumns, Array)
    %
:- type version_array2d(T)
    --->    version_array2d(int, int, version_array(T)).

%---------------------------------------------------------------------------%

version_array2d(Rows) = VersionArray2d :-
    (
        Rows = [],
        VersionArray2d = version_array2d(0, 0, version_array([]))
    ;
        Rows = [FirstRow | _],
        list.length(Rows, NumRows),
        list.length(FirstRow, FirstRowNumColumns),
        ( if
            all [Row] (
                list.member(Row, Rows)
            =>
                list.length(Row) = FirstRowNumColumns
            )
        then
            VersionArray = version_array(list.condense(Rows)),
            VersionArray2d =
                version_array2d(NumRows, FirstRowNumColumns, VersionArray)
        else
            error($pred, "non-rectangular list of lists")
        )
    ).

%---------------------------------------------------------------------------%

init(NumRows, NumColumns, InitValue) = VersionArray2d :-
    ( if NumRows >= 0, NumColumns >= 0 then
        VersionArray = version_array.init(NumRows * NumColumns, InitValue),
        VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray)
    else
        error($pred, "bounds must be non-negative")
    ).

uinit(NumRows, NumColumns, InitValue) = VersionArray2d :-
    NumRowsI = uint.cast_to_int(NumRows),
    NumColumnsI = uint.cast_to_int(NumColumns),
    VersionArray2d = init(NumRowsI, NumColumnsI, InitValue).

%---------------------------------------------------------------------------%

bounds(version_array2d(NumRows, NumColumns, _A), NumRows, NumColumns).

ubounds(version_array2d(NumRowsI, NumColumnsI, _A), NumRows, NumColumns) :-
    NumRows = uint.cast_from_int(NumRowsI),
    NumColumns = uint.cast_from_int(NumColumnsI).

%---------------------------------------------------------------------------%

in_bounds(version_array2d(NumRows, NumColumns, _A), RowNum, ColumnNum) :-
    0 =< RowNum, RowNum < NumRows,
    0 =< ColumnNum, ColumnNum < NumColumns.

in_ubounds(version_array2d(NumRowsI, NumColumnsI, _A), RowNum, ColumnNum) :-
    RowNumI = uint.cast_to_int(RowNum),
    ColumnNumI = uint.cast_to_int(ColumnNum),
    RowNumI < NumRowsI,
    ColumnNumI < NumColumnsI.

%---------------------------------------------------------------------------%

lookup(VersionArray2d, RowNum, ColumnNum, Value) :-
    VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray),
    ( if not (0 =< RowNum, RowNum < NumRows) then
        out_of_bounds_error("version_array2d.lookup", "row",
            RowNum, NumRows)
    else if not (0 =< ColumnNum, ColumnNum < NumColumns) then
        out_of_bounds_error("version_array2d.lookup", "column",
            ColumnNum, NumColumns)
    else
        Slot = RowNum * NumColumns + ColumnNum,
        version_array.lookup(VersionArray, Slot, Value)
    ).

ulookup(VersionArray2d, RowNum, ColumnNum, Value) :-
    VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray),
    RowNumI = uint.cast_to_int(RowNum),
    ColumnNumI = uint.cast_to_int(ColumnNum),
    ( if not (RowNumI < NumRows) then
        out_of_bounds_error("version_array2d.ulookup", "row",
            RowNumI, NumRows)
    else if not (ColumnNumI < NumColumns) then
        out_of_bounds_error("version_array2d.ulookup", "column",
            ColumnNumI, NumColumns)
    else
        Slot = RowNumI * NumColumns + ColumnNumI,
        version_array.lookup(VersionArray, Slot, Value)
    ).

elem(RowNum, ColumnNum, VersionArray2d) = Value :-
    lookup(VersionArray2d, RowNum, ColumnNum, Value).

uelem(RowNum, ColumnNum, VersionArray2d) = Value :-
    ulookup(VersionArray2d, RowNum, ColumnNum, Value).

%---------------------------------------------------------------------------%

set(RowNum, ColumnNum, NewValue, !VersionArray2d) :-
    !.VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray0),
    ( if not (0 =< RowNum, RowNum < NumRows) then
        out_of_bounds_error("version_array2d.set", "row",
            RowNum, NumRows)
    else if not (0 =< ColumnNum, ColumnNum < NumColumns) then
        out_of_bounds_error("version_array2d.set", "column",
            ColumnNum, NumColumns)
    else
        SlotNum = RowNum * NumColumns + ColumnNum,
        version_array.set(SlotNum, NewValue, VersionArray0, VersionArray),
        !:VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray)
    ).

uset(RowNum, ColumnNum, NewValue, !VersionArray2d) :-
    !.VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray0),
    RowNumI = uint.cast_to_int(RowNum),
    ColumnNumI = uint.cast_to_int(ColumnNum),
    ( if not (RowNumI < NumRows) then
        out_of_bounds_error("version_array2d.uset", "row",
            RowNumI, NumRows)
    else if not (ColumnNumI < NumColumns) then
        out_of_bounds_error("version_array2d.uset", "column",
            ColumnNumI, NumColumns)
    else
        SlotNum = RowNumI * NumColumns + ColumnNumI,
        version_array.set(SlotNum, NewValue, VersionArray0, VersionArray),
        !:VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray)
    ).

'elem :='(RowNum, ColumnNum, !.VersionArray2d, NewValue) = !:VersionArray2d :-
    set(RowNum, ColumnNum, NewValue, !VersionArray2d).

'uelem :='(RowNum, ColumnNum, !.VersionArray2d, NewValue) = !:VersionArray2d :-
    uset(RowNum, ColumnNum, NewValue, !VersionArray2d).

%---------------------------------------------------------------------------%

lists(VersionArray2d) = List :-
    VersionArray2d = version_array2d(NumRows, NumColumns, VersionArray),
    List = lists_2((NumRows * NumColumns) - 1, NumColumns - 1, NumColumns,
        VersionArray, [], []),
    acc_rev_rows(VersionArray, NumRows, NumColumns, NumRows - 1, [], ListB),
    expect(unify(List, ListB), $pred,
        "lists results mismatch" ++ string.string(List) ++
        " vs " ++ string.string(ListB)).

    % XXX This predicate should be replaced by two predicates.
    % One should copy the elements of ONE row, looping over the columns,
    % while the other should loop over all the rows.
    %
:- func lists_2(int, int, int, version_array(T), list(T),
    list(list(T))) = list(list(T)).

lists_2(IJ, J, N, VA, Xs, Xss) =
    ( if 0 =< IJ then
        ( if 0 =< J then
            lists_2(IJ - 1, J - 1, N, VA, [VA ^ elem(IJ) | Xs], Xss       )
        else
            lists_2(IJ,     N - 1, N, VA, [],                   [Xs | Xss])
        )
    else
        [Xs | Xss]
    ).

:- pred acc_rev_rows(version_array(T)::in, int::in, int::in, int::in,
    list(list(T))::in, list(list(T))::out) is det.

acc_rev_rows(VersionArray, NumRows, NumColumns, RowNum, !Rows) :-
    ( if 0 =< RowNum then
        RowBase = RowNum * NumColumns,
        acc_rev_columns(VersionArray, RowBase, NumColumns, NumColumns - 1,
            [], Row),
        !:Rows = [Row | !.Rows],
        acc_rev_rows(VersionArray, NumRows, NumColumns, RowNum - 1, !Rows)
    else
        true
    ).

:- pred acc_rev_columns(version_array(T)::in, int::in, int::in, int::in,
    list(T)::in, list(T)::out) is det.

acc_rev_columns(VersionArray, RowBase, NumColumns, ColumnNum, !ItemsInRow) :-
    ( if 0 =< ColumnNum then
        lookup(VersionArray, RowBase + ColumnNum, Item),
        !:ItemsInRow = [Item | !.ItemsInRow],
        acc_rev_columns(VersionArray, RowBase, NumColumns, ColumnNum - 1,
            !ItemsInRow)
    else
        true
    ).

%---------------------------------------------------------------------------%

copy(VersionArray2d) = CopyVersionArray2d :-
    VersionArray2d = version_array2d(NumRows, NumColumns, VA),
    CopyVersionArray2d = version_array2d(NumRows, NumColumns, copy(VA)).

%---------------------------------------------------------------------------%

resize(OldVersionArray2d, NewNumRows, NewNumColumns, DefaultValue)
        = !:VersionArray2d :-
    !:VersionArray2d = init(NewNumRows, NewNumColumns, DefaultValue),
    bounds(OldVersionArray2d, OldNumRows, OldNumColumns),
    % We cannot copy more rows or columns from OldVersionArray2d
    % than it actually has. Any slots in !:NewNumColumns whose
    % row and/or column numbers do not exist in OldVersionArray2d
    % will have been set to DefaultValue by the call to init above.
    CopyNumRows =    min(OldNumRows, NewNumRows),
    CopyNumColumns = min(OldNumColumns, NewNumColumns),
    resize_copy_loop(0, 0, CopyNumRows, CopyNumColumns,
        OldVersionArray2d, !VersionArray2d).

    % XXX This predicate could be replaced by two predicates,
    % with one predicate copying ONE row, looping over the columns,
    % while the other loops over the rows themselves.
    %
:- pred resize_copy_loop(int::in, int::in, int::in, int::in,
    version_array2d(T)::in,
    version_array2d(T)::in, version_array2d(T)::out) is det.

resize_copy_loop(RowNum, ColumnNum, CopyNumRows, CopyNumColumns,
        OldVersionArray2d, !VersionArray2d) :-
    ( if RowNum >= CopyNumRows then
        % We have copied all the rows that exist in OldVersionArray2d.
        % Any rows in !VersionArray2d that are not in OldVersionArray2d
        % will have been set to DefaultValue by our caller.
        true
    else if ColumnNum >= CopyNumColumns then
        % We have copied all the columns in this row that exist in
        % OldVersionArray2d. Any later columns in in !VersionArray2d
        % that are not in OldVersionArray2d will have been set
        % to DefaultValue by our caller.
        resize_copy_loop(RowNum + 1, 0, CopyNumRows, CopyNumColumns,
            OldVersionArray2d, !VersionArray2d)
    else
        lookup(OldVersionArray2d, RowNum, ColumnNum, Item),
        set(RowNum, ColumnNum, Item, !VersionArray2d),
        resize_copy_loop(RowNum, ColumnNum + 1, CopyNumRows, CopyNumColumns,
            OldVersionArray2d, !VersionArray2d)
    ).

uresize(OldVersionArray2d, NewNumRows, NewNumColumns, DefaultValue)
        = VersionArray2d :-
    VersionArray2d = resize(OldVersionArray2d,
        uint.cast_to_int(NewNumRows), uint.cast_to_int(NewNumColumns),
        DefaultValue).

%---------------------------------------------------------------------------%

unsafe_rewind(version_array2d(NumRows, NumColumns, VersionArray)) =
    version_array2d(NumRows, NumColumns, unsafe_rewind(VersionArray)).

%---------------------------------------------------------------------------%

    % Throw an exception indicating an array bounds error.
    %
:- pred out_of_bounds_error(string::in, string::in, int::in, int::in)
    is erroneous.

out_of_bounds_error(PredName, RowOrColumn, Index, Max) :-
    % Note: we deliberately do not include the array element type name in the
    % error message here, for performance reasons: using the type name could
    % prevent the compiler from optimizing away the construction of the
    % type_info in the caller, because it would prevent unused argument
    % elimination.
    string.format("%s: %s index %d not in range [0, %d]",
        [s(PredName), s(RowOrColumn), i(Index), i(Max)], Msg),
    throw(index_out_of_bounds(Msg)).

%---------------------------------------------------------------------------%
:- end_module version_array2d.
%---------------------------------------------------------------------------%
