%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2008 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury Team.
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

:- import_module maybe.

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

    % maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
    % input semideterministically. If calling the closure with the input
    % X succeeds, Y is bound to `yes(Z)' where Z is the output of the
    % call, or to `no' if the call fails.
    %
    % Use maybe.pred_to_maybe instead.
:- pragma obsolete(maybe_pred/3).
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

    % Use maybe.pred_to_maybe instead.
:- pragma obsolete(maybe_func/2).
:- func maybe_func(func(T1) = T2, T1) = maybe(T2).
:- mode maybe_func(func(in) = out is semidet, in) = out is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

compose(F, G, X) =
    F(G(X)).

converse(F, X, Y) =
    F(Y, X).

pow(F, N, X) =
    ( if N = 0 then X else pow(F, N - 1, F(X)) ).

id(X) = X.

isnt(P, X) :-
    not P(X).

negate(P) :-
    not P.

maybe_pred(Pred, X, Y) :-
    Y = ( if Pred(X, Z) then yes(Z) else no ).

maybe_func(PF, X) =
    ( if Y = PF(X) then yes(Y) else no ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
