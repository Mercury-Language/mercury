%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: array2d.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: medium-low.
%
% Two-dimensional rectangular (i.e. not ragged) array ADT.
%
% XXX The same caveats re: uniqueness of arrays apply to array2ds.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module array2d.
:- interface.

:- import_module array.
:- import_module list.

%---------------------------------------------------------------------------%

    % An array2d is a two-dimensional array stored in row-major order
    % (that is, the elements of the first row in left-to-right
    % order, followed by the elements of the second row and so forth.)
    %
:- type array2d(T).

:- inst array2d for array2d/1
    --->    array2d(ground, ground, array).

    % XXX These are work-arounds until we get nested uniqueness working.
    %
:- mode array2d_di == di(array2d).
:- mode array2d_ui == in(array2d).
:- mode array2d_uo == out(array2d).

    % init(NumRows, NumColumns, Elem):
    % Creates a 2d array with the given numbers of rows and columns
    % whose every element is set to Elem.
    %
    % Throws an exception if either NumRows or NumColumns is negative.
    %
:- func init(int, int, T) = array2d(T).
:- mode init(in, in, in) = array2d_uo is det.

    % array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]) constructs an array2d
    % of size M * N, with the special case that bounds(array2d([]), 0, 0).
    %
    % In other words, the elements of the top level list each represent
    % one row, and each row is itself a list of the values in the columns
    % of that row.
    %
    % Throws an exception unless all rows have the same number of columns.
    %
:- func array2d(list(list(T))) = array2d(T).
:- mode array2d(in) = array2d_uo is det.

    % A synonym for the array2d function above.
    %
:- func from_lists(list(list(T))) = array2d(T).
:- mode from_lists(in) = array2d_uo is det.

    % from_array(NumRows, NumColumns, Array) constructs an array2d
    % of size NumRows * NumColumns where the elements are taken from Array
    % in row-major order, i.e. the element at row R column C is taken from
    % Array at index (R * NumColumns + C). Indices start from zero.
    %
    % Throws an exception if NumRows < 0 or NumColumns < 0, or if
    % the number of elements in Array does not equal NumRows * NumColumns.
    %
:- func from_array(int, int, array(T)) = array2d(T).
:- mode from_array(in, in, array_di) = array2d_uo is det.

    % is_empty(Array):
    % True iff Array contains zero elements.
    %
:- pred is_empty(array2d(T)).
% :- mode is_empty(array2d_ui) is semidet.
:- mode is_empty(in) is semidet.

    % bounds(Array, NumRows, NumColumns):
    %
    % Returns the number of rows and columns in the given 2d array.
    %
:- pred bounds(array2d(T), int, int).
% :- mode bounds(array2d_ui, out, out) is det.
:- mode bounds(in, out, out) is det.

    % in_bounds(Array, R, C):
    %
    % Succeeds if and only if 0 =< C < NumRows, 0 =< C < NumColumns.
    %
:- pred in_bounds(array2d(T), int, int).
% :- mode in_bounds(array2d_ui, in, in) is semidet.
:- mode in_bounds(in, in, in) is semidet.

    % lookup(Array, R, C):
    %
    % Given a 2d array Array with NumRows rows and NumColumns columns,
    % return the element at row R and column C. Indices start at zero.
    %
    % This function requires 0 =< R < NumRows and 0 =< C < NumColumns.
    % If this requirement is not satisfied, this function will throw
    % an exception.
    %
:- func lookup(array2d(T), int, int) = T.
% :- mode lookup(array2d_ui, in, in) = out is det.
:- mode lookup(in, in, in) = out is det.
:- pred lookup(array2d(T), int, int, T).
% :- mode lookup(array2d_ui, in, in, out) is det.
:- mode lookup(in, in, in, out) is det.
:- func array2d(T) ^ elem(int, int) = T.
% :- mode array2d_ui ^ elem(in, in) = out is det.
:- mode in ^ elem(in, in) = out is det.

    % unsafe_lookup(Array, R, C):
    %
    % Given a 2d array Array with NumRows rows and NumColumns columns,
    % return the element at row R and column C. Indices start at zero.
    %
    % This function requires 0 =< R < NumRows and 0 =< C < NumColumns.
    % If this requirement is not satisfied, the behavior of this function
    % is undefined.
    %
:- func unsafe_lookup(array2d(T), int, int) = T.
% :- mode unsafe_lookup(array2d_ui, in, in) = out is det.
:- mode unsafe_lookup(in, in, in) = out is det.
:- pred unsafe_lookup(array2d(T), int, int, T).
% :- mode unsafe_lookup(array2d_ui, in, in, out) is det.
:- mode unsafe_lookup(in, in, in, out) is det.
:- func array2d(T) ^ unsafe_elem(int, int) = T.
% :- mode array2d_ui ^ unsafe_elem(in, in) = out is det.
:- mode in ^ unsafe_elem(in, in) = out is det.

    % set(R, C, NewElem, Array0, Array):
    %
    % Return Array, which differs from Array0 only in that
    % the value at row R and column C is NewElem.
    %
    % Throws an exception unless 0 =< R < NumRows, 0 =< C < NumColumns.
    %
:- pred set(int, int, T, array2d(T), array2d(T)).
:- mode set(in, in, in, array2d_di, array2d_uo) is det.
:- func (array2d(T) ^ elem(int, int) := T) = array2d(T).
:- mode (array2d_di ^ elem(in, in) := in) = array2d_uo is det.

    % unsafe_set(R, C, NewElem, Array0, Array):
    %
    % Return Array, which differs from Array0 only in that
    % the value at row R and column C is NewElem.
    %
    % The behavior is defined only if 0 =< R < NumRows, 0 =< C < NumColumns.
    %
:- pred unsafe_set(int, int, T, array2d(T), array2d(T)).
:- mode unsafe_set(in, in, in, array2d_di, array2d_uo) is det.
:- func (array2d(T) ^ unsafe_elem(int, int) := T ) = array2d(T).
:- mode (array2d_di ^ unsafe_elem(in, in) := in) = array2d_uo is det.


    % lists(Array):
    %
    % Return the contents of the given 2d array as a list of rows,
    % with each row containing the values in its columns.
    %
    % This function is the converse of from_lists.
    % For every Array, from_lists(lists(Array) = Array,
    % and for every Lists for from_lists(Lists) does not throw
    % an exception, lists(from_lists(Lists) = Lists.
    %
:- func lists(array2d(T)) = list(list(T)).
% :- mode lists(array2d_ui) = out is det.
:- mode lists(in) = out is det.

    % fill(Item, !Array):
    % Sets every element of the array to Item.
    %
:- pred fill(T::in, array2d(T)::array2d_di, array2d(T)::array2d_uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.

:- interface.

    % This should be abstract, but needs to be exported for insts.
    %
:- type array2d(T)
    --->    array2d(
                % The number of rows.
                int,

                % The number of columns.
                int,

                % The contents of the 2d array, flattened out.
                % It stores the element at row R and column C at 
                % index (R * NumColumns) + C in the flattened array.
                array(T)
            ).

:- implementation.


%---------------------------------------------------------------------------%

init(NumRows, NumColumns, Elem) =
    ( if NumRows >= 0, NumColumns >= 0 then
        array2d(NumRows, NumColumns, array.init(NumRows * NumColumns, Elem))
    else
        func_error($pred, "bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

array2d(Rows) = from_lists(Rows).

from_lists([]) = array2d(0, 0, make_empty_array).
from_lists(Rows @ [FirstRow | _]) = Array :-
    NumRows = list.length(Rows),
    NumColumns = list.length(FirstRow),
    ( if
        all [Row] (
            list.member(Row, Rows)
        =>
            list.length(Row) = NumColumns
        )
    then
        A = array(list.condense(Rows)),
        Array = array2d(NumRows, NumColumns, A)
    else
        error($pred, "non-rectangular list of lists")
    ).

from_array(NumRows, NumColumns, Array) = Array2d :-
    ( if
        NumRows >= 0,
        NumColumns >= 0
    then
        array.size(Array, Size),
        compare(Result, Size, NumRows * NumColumns),
        (
            Result = (=),
            Array2d = array2d(NumRows, NumColumns, Array)
        ;
            Result = (>),
            error($pred, "too many elements")
        ;
            Result = (<),
            error($pred, "too few elements")
        )
    else
        error($pred, " bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

is_empty(array2d(_, _, A)) :-
    array.is_empty(A).

%---------------------------------------------------------------------------%

bounds(array2d(NumRows, NumColumns, _A), NumRows, NumColumns).

%---------------------------------------------------------------------------%

in_bounds(array2d(NumRows, NumColumns, _A), R, C) :-
    0 =< R, R < NumRows,
    0 =< C, C < NumColumns.

%---------------------------------------------------------------------------%

lookup(Array, R, C) = Elem :-
    ( if in_bounds(Array, R, C) then
        Elem = unsafe_lookup(Array, R, C)
    else
        error($pred, "indices out of bounds")
    ).

lookup(Array, R, C, Elem) :-
    Elem = lookup(Array, R, C).

Array ^ elem(R, C) =
    lookup(Array, R, C).

%---------------------------------------------------------------------------%

unsafe_lookup(Array, R, C) = Elem :-
    Array = array2d(_NumRows, NumColumns, A),
    array.unsafe_lookup(A, (R * NumColumns) + C, Elem).

unsafe_lookup(Array, R, C, Elem) :-
    Elem = unsafe_lookup(Array, R, C).

Array ^ unsafe_elem(R, C) =
    unsafe_lookup(Array, R, C).

%---------------------------------------------------------------------------%

set(R, C, Value, !Array) :-
    ( if in_bounds(!.Array, R, C) then
        unsafe_set(R, C, Value, !Array)
    else
        error($pred, "indices out of bounds")
    ).

( Array0 ^ elem(R, C) := Value ) = Array :-
    set(R, C, Value, Array0, Array).

%---------------------------------------------------------------------------%

unsafe_set(R, C, Value, !Array) :-
    !.Array = array2d(NumRows, NumColumns, A0),
    array.unsafe_set((R * NumColumns) + C, Value, A0, A),
    !:Array = array2d(NumRows, NumColumns, A).

( Array0 ^ unsafe_elem(R, C) := Value ) = Array :-
    unsafe_set(R, C, Value, Array0, Array).

%---------------------------------------------------------------------------%

lists(array2d(NumRows, NumColumns, A)) = Rows :-
    get_rows(NumRows - 1, NumColumns, A, [], Rows).

:- pred get_rows(int, int, array(T), list(list(T)), list(list(T))).
% :- mode get_rows(in, in, array_ui, in, out) is det.
:- mode get_rows(in, in, in, in, out) is det.

get_rows(RowNum, NumColumns, A, !Rows) :-
    ( if RowNum >= 0 then
        get_columns(RowNum, NumColumns - 1, NumColumns, A, [], Columns),
        !:Rows = [Columns | !.Rows],
        get_rows(RowNum - 1, NumColumns, A, !Rows)
    else
        true
    ).

:- pred get_columns(int, int, int, array(T), list(T), list(T)).
% :- mode get_columns(in, in, in, array_ui, in, out) is det.
:- mode get_columns(in, in, in, in, in, out) is det.

get_columns(RowNum, ColumnNum, NumColumns, A, !Columns) :-
    ( if ColumnNum >= 0 then
        array.unsafe_lookup(A, (RowNum * NumColumns) + ColumnNum, Elem),
        !:Columns = [Elem | !.Columns],
        get_columns(RowNum, ColumnNum - 1, NumColumns, A, !Columns)
    else
        true
    ).

%---------------------------------------------------------------------------%

fill(Item, Array0, Array) :-
    Array0 = array2d(NumRows, NumColumns, A0),
    array.fill(Item, A0, A),
    Array = array2d(NumRows, NumColumns, A).

%---------------------------------------------------------------------------%
:- end_module array2d.
%---------------------------------------------------------------------------%
