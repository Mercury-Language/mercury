% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_array2d.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: medium-low.
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
    % succeeds iff 0 =< I < M, 0 =< J < N.
    %
:- pred in_bounds(version_array2d(T)::in, int::in, int::in) is semidet.

    % version_array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]) ^ elem(I, J) = X
    % where X is the J+1th element of the I+1th row (i.e. indices start from
    % zero.)
    %
    % An exception is thrown unless 0 =< I < M, 0 =< J < N.
    %
:- func version_array2d(T) ^ elem(int, int) = T.

    % ( VA2D0 ^ elem(I, J) := X ) = VA2D
    % where VA2D ^ elem(II, JJ) = X                    if I = II, J = JJ
    % and   VA2D ^ elem(II, JJ) = VA2D0 ^ elem(II, JJ) otherwise.
    %
    % An exception is thrown unless 0 =< I < M, 0 =< J < N.
    %
    % A predicate version is also provided.
    %
:- func ( version_array2d(T) ^ elem(int, int) := T  ) = version_array2d(T).
:- pred set(int::in, int::in, T::in,
    version_array2d(T)::in, version_array2d(T)::out) is det.

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
:- import_module string.
:- import_module version_array.

%---------------------------------------------------------------------------%


    % version_array2d(Rows, Cols, Array)
    %
:- type version_array2d(T) ---> version_array2d(int, int, version_array(T)).

%---------------------------------------------------------------------------%

version_array2d(      []      ) = version_array2d(0, 0, version_array([])).
version_array2d(Xss @ [Xs | _]) = VA2D :-
    M = length(Xss),
    N = length(Xs),
    A = version_array(condense(Xss)),
    ( if all [Ys] ( member(Ys, Xss) => length(Ys) = N ) then
        VA2D = version_array2d(M, N, A)
    else
        error("version_array2d.version_array2d/1: " ++
            "non-rectangular list of lists")
    ).

%---------------------------------------------------------------------------%

init(M, N, X) =
    ( if M >= 0, N >= 0 then
        version_array2d(M, N, version_array.init(M * N, X))
    else
        func_error("version_array2d.new: bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

bounds(version_array2d(M, N, _A), M, N).

%---------------------------------------------------------------------------%

in_bounds(version_array2d(M, N, _A), I, J) :-
    0 =< I, I < M,
    0 =< J, J < N.

%---------------------------------------------------------------------------%

version_array2d(_M, N, VA) ^ elem(I, J) = VA ^ elem(I * N + J).

%---------------------------------------------------------------------------%

( version_array2d(M, N, VA) ^ elem(I, J) := X ) =
    version_array2d(M, N, VA ^ elem(I * N + J) := X).

set(I, J, X, VA2D, VA2D ^ elem(I, J) := X).

%---------------------------------------------------------------------------%

lists(version_array2d(M, N, VA)) = lists_2((M * N) - 1, N - 1, N, VA, [], []).

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

copy(version_array2d(M, N, VA)) = version_array2d(M, N, copy(VA)).

%---------------------------------------------------------------------------%

resize(VA2D0, M, N, X) = VA2D :-
    VA2D1 = init(M, N, X),
    bounds(VA2D0, M0, N0),
    M1    = min(M0, M),
    N1    = min(N0, N),
    VA2D  = resize_2(0, 0, M1, N1, VA2D0, VA2D1).

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
%---------------------------------------------------------------------------%
