%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
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

    % A array2d is a two-dimensional array stored in row-major order
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

    % init(M, N, X) = array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]])
    % where each XIJ = X.  An exception is thrown if M < 0 or N < 0.
    %
:- func init(int, int, T) = array2d(T).
:- mode init(in, in, in) = array2d_uo is det.

    % array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]) constructs a array2d
    % of size M * N, with the special case that bounds(array2d([]), 0, 0).
    %
    % An exception is thrown if the sublists are not all the same length.
    %
:- func array2d(list(list(T))) = array2d(T).
:- mode array2d(in) = array2d_uo is det.

    % is_empty(Array):
    % True iff Array contains zero elements.
    %
:- pred is_empty(array2d(T)).
%:- mode is_empty(array2d_ui) is semidet.
:- mode is_empty(in) is semidet.

    % A synonym for the above.
    %
:- func from_lists(list(list(T))) = array2d(T).
:- mode from_lists(in) = array2d_uo is det.

    % bounds(array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]), M, N)
    %
:- pred bounds(array2d(T), int, int).
%:- mode bounds(array2d_ui, out, out) is det.
:- mode bounds(in,       out, out) is det.

    % in_bounds(array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]), I, J)
    % succeeds iff 0 =< I < M, 0 =< J < N.
    %
:- pred in_bounds(array2d(T), int, int).
%:- mode in_bounds(array2d_ui, in,  in ) is semidet.
:- mode in_bounds(in,       in,  in ) is semidet.

    % array2d([[X11, ..., X1N], ..., [XM1, ..., XMN]]) ^ elem(I, J) = X
    % where X is the J+1th element of the I+1th row (that is, indices
    % start from zero.)
    %
    % An exception is thrown unless 0 =< I < M, 0 =< J < N.
    %
:- func array2d(T) ^ elem(int, int) = T.
%:- mode array2d_ui ^ elem(in,  in ) = out is det.
:- mode in       ^ elem(in,  in ) = out is det.

    % T ^ unsafe_elem(I, J) is the same as T ^ elem(I, J) except that
    % behaviour is undefined if not in_bounds(T, I, J).
    %
:- func array2d(T) ^ unsafe_elem(int, int) = T.
%:- mode array2d_ui ^ unsafe_elem(in,  in ) = out is det.
:- mode in       ^ unsafe_elem(in,  in ) = out is det.

    % ( T0 ^ elem(I, J) := X ) = T
    % where T ^ elem(II, JJ) = X                 if I = II, J = JJ
    % and   T ^ elem(II, JJ) = T0 ^ elem(II, JJ) otherwise.
    %
    % An exception is thrown unless 0 =< I < M, 0 =< J < N.
    %
:- func ( array2d(T) ^ elem(int, int) := T  ) = array2d(T).
:- mode ( array2d_di ^ elem(in,  in)  := in ) = array2d_uo is det.

    % Pred version of the above.
    %
:- pred set(int, int, T,  array2d(T), array2d(T)).
:- mode set(in,  in,  in, array2d_di, array2d_uo) is det.

    % T ^ unsafe_elem(I, J) := X is the same as T ^ elem(I, J) := X except
    % that behaviour is undefined if not in_bounds(T, I, J).
    %
:- func ( array2d(T) ^ unsafe_elem(int, int) := T  ) = array2d(T).
:- mode ( array2d_di ^ unsafe_elem(in,  in)  := in ) = array2d_uo is det.

    % Pred version of the above.
    %
:- pred unsafe_set(int, int, T,  array2d(T), array2d(T)).
:- mode unsafe_set(in,  in,  in, array2d_di, array2d_uo) is det.

    % lists(array2d([[X11, ..., X1N], ..., [XM1, ..., XMN])) =
    %     [[X11, ..., X1N], ..., [XM1, ..., XMN]]
    %
:- func lists(array2d(T)) = list(list(T)).
%:- mode lists(array2d_ui) = out is det.
:- mode lists(in        ) = out is det.

    % fill(Item, !Array2d):
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
                int,        % rows
                int,        % cols
                array(T)    % array
            ).

:- implementation.


%---------------------------------------------------------------------------%

init(M, N, X) =
    ( if    M >= 0, N >= 0
      then  array2d(M, N, array.init(M * N, X))
      else  func_error("array2d.init: bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

array2d([]) = array2d(0, 0, make_empty_array).
array2d(Xss @ [Xs | _]) = T :-
    M = length(Xss),
    N = length(Xs),
    A = array(condense(Xss)),
    T = ( if    all [Ys] ( member(Ys, Xss) => length(Ys) = N )
          then  array2d(M, N, A)
          else  func_error("array2d.array2d/1: non-rectangular list of lists")
        ).

is_empty(array2d(_, _, A)) :-
    array.is_empty(A).

from_lists(Xss) = array2d(Xss).

%---------------------------------------------------------------------------%

bounds(array2d(M, N, _A), M, N).

%---------------------------------------------------------------------------%

in_bounds(array2d(M, N, _A), I, J) :-
    0 =< I, I < M,
    0 =< J, J < N.

%---------------------------------------------------------------------------%

T ^ elem(I, J) =
    ( if    in_bounds(T, I, J)
      then  T ^ unsafe_elem(I, J)
      else  func_error("array2d.elem: indices out of bounds")
    ).

%---------------------------------------------------------------------------%

array2d(_M, N, A) ^ unsafe_elem(I, J) = A ^ unsafe_elem(I * N + J).

%---------------------------------------------------------------------------%

( T ^ elem(I, J) := X ) =
    ( if    in_bounds(T, I, J)
      then  T ^ unsafe_elem(I, J) := X
      else  func_error("array2d.'elem :=': indices out of bounds")
    ).

set(I, J, X, A, A ^ elem(I, J) := X).

%---------------------------------------------------------------------------%

( array2d(M, N, A) ^ unsafe_elem(I, J) := X ) =
    array2d(M, N, A ^ unsafe_elem(I * N + J) := X).

unsafe_set(I, J, X, A, A ^ unsafe_elem(I, J) := X).

%---------------------------------------------------------------------------%

lists(array2d(M, N, A)) = lists_2((M * N) - 1, N - 1, N, A, [], []).

:- func lists_2(int, int, int, array(T), list(T), list(list(T))) =
            list(list(T)).
%:- mode lists_2(in,  in,  in,  array_ui, in,      in           ) = out is det.
:- mode lists_2(in,  in,  in,  in,       in,      in           ) = out is det.

lists_2(IJ, J, N, A, Xs, Xss) =
    ( if 0 =< IJ then
        ( if    0 =< J
          then  lists_2(IJ - 1, J - 1, N, A, [A ^ elem(IJ) | Xs], Xss       )
          else  lists_2(IJ,     N - 1, N, A, [],                  [Xs | Xss])
        )
      else
         [Xs | Xss]
     ).

%---------------------------------------------------------------------------%

fill(Item, A0, A) :-
    A0 = array2d(M, N, Array0),
    array.fill(Item, Array0, Array),
    A = array2d(M, N, Array).

%---------------------------------------------------------------------------%
:- end_module array2d.
%---------------------------------------------------------------------------%
