%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2019, 2022, 2024-2025 The Mercury team.
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

    % A version_array2d is a two-dimensional version array stored in row-major
    % order (that is, the elements of the first row in left-to-right order,
    % followed by the elements of the second row, and so on.)
    %
:- type version_array2d(T).

    % version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]) constructs
    % a 2d version array of size M * N, with the special case that
    % bounds(version_array2d([]), 0, 0).
    %
    % An exception is thrown if the sublists are not all the same length.
    %
:- func version_array2d(list(list(T))) = version_array2d(T).

    % init(M, N, X) = version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]])
    % where each XIJ = X.
    %
    % An exception is thrown if M < 0 or N < 0.
    %
:- func init(int, int, T) = version_array2d(T).

    % bounds(version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]), M, N)
    %
:- pred bounds(version_array2d(T)::in, int::out, int::out) is det.

    % in_bounds(version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]), I, J)
    % succeeds if-and-only-if 0 =< I < M, 0 =< J < N.
    %
:- pred in_bounds(version_array2d(T)::in, int::in, int::in) is semidet.

    % lookup(VA2D, RowNum, ColNum, Value):
    % VA2D ^ elem(RowNum, ColNum) = Value:
    %
    % Return the value at the given row and column numbers.
    % Note that both row and column numbers start from zero.
    %
    % Throw an exception if either RowNum or ColNum is out of bounds.
    %
:- pred lookup(version_array2d(T)::in, int::in, int::in, T::out) is det.
:- func elem(int, int, version_array2d(T)) = T.

    % set(RowNum, ColNum, X, !VA2D):
    % ( !.VA2D ^ elem(RowNum, ColNum) := X ) = !:VA2D:
    %
    % Return a version of the initial version_array2d that contains
    % all the same values, with the exception that the element at the
    % given row and column number is set to X.
    %
    % Throw an exception if either RowNum or ColNum is out of bounds.
    %
:- pred set(int::in, int::in, T::in,
    version_array2d(T)::in, version_array2d(T)::out) is det.
:- func 'elem :='(int, int, version_array2d(T), T) = version_array2d(T).

    % lists(version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN])) =
    %     [[X11, ..., X1N], ..., [XM1, ..., XMN]]
    %
:- func lists(version_array2d(T)) = list(list(T)).

    % copy(VA2D) returns a copy of VA2D with O(1) access times.
    %
:- func copy(version_array2d(T)) = version_array2d(T).

    % resize(VA2D, M, N, X) returns a copy of VA2D resized to M * N.
    % Items with coordinates in common are copied from VA2D; other
    % items are initialised to X.
    %
    % An exception is thrown if M < 0 or N < 0.
    %
:- func resize(version_array2d(T), int, int, T) = version_array2d(T).

    % unsafe_rewind(VA2D) returns a new 2d version array with O(1) access
    % times, at the cost of rendering VA2D and its descendants undefined.
    % Only call this function if you are absolutely certain there are no
    % remaining live references to VA2D or any descendent of VA2D.
    %
:- func unsafe_rewind(version_array2d(T)) = version_array2d(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module version_array.

%---------------------------------------------------------------------------%

    % version_array2d(NumRows, NumCols, Array)
    %
:- type version_array2d(T)
    --->    version_array2d(int, int, version_array(T)).

%---------------------------------------------------------------------------%

version_array2d(Rows) = VA2D :-
    (
        Rows = [],
        VA2D = version_array2d(0, 0, version_array([]))
    ;
        Rows = [FirstRow | _],
        list.length(Rows, NumRows),
        list.length(FirstRow, FirstRowNumCols),
        ( if
            all [Row] (
                list.member(Row, Rows)
            =>
                list.length(Row) = FirstRowNumCols
            )
        then
            VA = version_array(list.condense(Rows)),
            VA2D = version_array2d(NumRows, FirstRowNumCols, VA)
        else
            error($pred, "non-rectangular list of lists")
        )
    ).

%---------------------------------------------------------------------------%

init(NumRows, NumCols, InitValue) = VA2D :-
    ( if NumRows >= 0, NumCols >= 0 then
        VA = version_array.init(NumRows * NumCols, InitValue),
        VA2D = version_array2d(NumRows, NumCols, VA)
    else
        error($pred, "bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

bounds(version_array2d(NumRows, NumCols, _A), NumRows, NumCols).

%---------------------------------------------------------------------------%

in_bounds(version_array2d(NumRows, NumCols, _A), RowNum, ColNum) :-
    0 =< RowNum, RowNum < NumRows,
    0 =< ColNum, ColNum < NumCols.

%---------------------------------------------------------------------------%

lookup(VA2D, RowNum, ColNum, Value) :-
    VA2D = version_array2d(_NumRows, NumCols, VA),
    version_array.lookup(VA, RowNum * NumCols + ColNum, Value).

elem(RowNum, ColNum, VA2D) = Value :-
    lookup(VA2D, RowNum, ColNum, Value).

%---------------------------------------------------------------------------%

set(RowNum, ColNum, NewValue, !VA2D) :-
    !.VA2D = version_array2d(NumRows, NumCols, VA0),
    version_array.set(RowNum * NumCols + ColNum, NewValue, VA0, VA),
    !:VA2D = version_array2d(NumRows, NumCols, VA).

'elem :='(RowNum, ColNum, !.VA2D, NewValue) = !:VA2D :-
    set(RowNum, ColNum, NewValue, !VA2D).

%---------------------------------------------------------------------------%

lists(version_array2d(M, N, VA)) = lists_2((M * N) - 1, N - 1, N, VA, [], []).

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

%---------------------------------------------------------------------------%

copy(VA2D) = CopyA2D :-
    VA2D = version_array2d(NumRows, NumCols, VA),
    CopyA2D = version_array2d(NumRows, NumCols, copy(VA)).

%---------------------------------------------------------------------------%

resize(VA2D0, M, N, X) = VA2D :-
    VA2D1 = init(M, N, X),
    bounds(VA2D0, M0, N0),
    % XXX By allowing M1 != M and N1 != N, these calls to min *contradict*
    % the documentation of resize.
    M1    = min(M0, M),
    N1    = min(N0, N),
    VA2D  = resize_2(0, 0, M1, N1, VA2D0, VA2D1).

    % XXX This predicate should be replaced by two predicates.
    % One should copy and resize ONE row, looping over the columns,
    % while the other should copy and resize the rows themselves.
    %
:- func resize_2(int, int, int, int, version_array2d(T),
    version_array2d(T)) = version_array2d(T).

resize_2(I, J, M, N, SrcVA2D, DestVA2D) =
    ( if I >= M then
        DestVA2D
    else if J >= N then
        resize_2(I + 1, 0, M, N, SrcVA2D, DestVA2D)
    else
        resize_2(I, J + 1, M, N, SrcVA2D,
            DestVA2D ^ elem(I, J) := SrcVA2D ^ elem(I, J))
    ).

%---------------------------------------------------------------------------%

unsafe_rewind(version_array2d(M, N, VA)) =
    version_array2d(M, N, unsafe_rewind(VA)).

%---------------------------------------------------------------------------%
:- end_module version_array2d.
%---------------------------------------------------------------------------%
