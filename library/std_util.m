%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2008 The University of Melbourne.
% Copyright (C) 2016, 2018, 2020, 2022-2023 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: std_util.m.
% Main author: fjh.
% Stability: high.
%
% This file contains higher-order programming constructs and other
% useful standard utilities.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module std_util.
:- interface.

%---------------------------------------------------------------------------%
%
% General purpose higher-order programming constructs
%

    % compose(F, G, X) = F(G(X))
    %
    % Function composition.
    %
:- func compose(func(T2) = T3, func(T1) = T2, T1) = T3.

    % converse(F, X, Y) = F(Y, X).
    %
:- func converse(func(T1, T2) = T3, T2, T1) = T3.

    % pow(F, N, X) = F^N(X)
    %
    % Function exponentiation.
    % Throws an exception if N is negative.
    %
:- func pow(func(T) = T, int, T) = T.

    % The identity function.
    %
:- func id(T) = T.

%---------------------------------------------------------------------------%

    % isnt(Pred, X) <=> not Pred(X)
    %
    % This is useful in higher order programming, e.g.
    %   Odds  = list.filter(odd, Xs)
    %   Evens = list.filter(isnt(odd), Xs)
    %
:- pred isnt(pred(T)::in(pred(in) is semidet), T::in) is semidet.

    % negate(Pred) <=> not Pred
    %
    % This is useful in higher order programming, e.g.
    %   expect(negate(Pred), ...)
    %
:- pred negate((pred)::in((pred) is semidet)) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

compose(F, G, X) =
    F(G(X)).

converse(F, X, Y) =
    F(Y, X).

pow(F, N, X) =
    ( if N < 0 then
        func_error($pred, "N is negative")
    else
        do_pow(F, N, X)
    ).

:- func do_pow(func(T) = T, int, T) = T.

do_pow(F, N, X) =
    ( if N = 0 then X else do_pow(F, N - 1, F(X)) ).

id(X) = X.

isnt(P, X) :-
    not P(X).

negate(P) :-
    not P.

%---------------------------------------------------------------------------%
:- end_module std_util.
%---------------------------------------------------------------------------%
