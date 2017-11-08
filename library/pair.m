%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pair.m.
% Main author: fjh.
% Stability: high.
%
% The "pair" type.  Useful for many purposes.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module pair.
:- interface.

:- type pair(T1, T2)
    --->    (T1 - T2).
:- type pair(T) ==  pair(T, T).

:- inst pair(I1, I2) for pair/2
    --->    (I1 - I2).
:- inst pair(I) == pair(I, I).

    % Return the first element of the pair.
    %
:- func fst(pair(X, Y)) = X.
:- pred fst(pair(X, Y)::in, X::out) is det.

    % Return the second element of the pair.
    %
:- func snd(pair(X, Y)) = Y.
:- pred snd(pair(X, Y)::in, Y::out) is det.

:- func pair(T1, T2) = pair(T1, T2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

fst(X - _Y) = X.
fst(P, X) :-
    X = fst(P).

snd(_X - Y) = Y.
snd(P, X) :-
    X = snd(P).

pair(X, Y) = X - Y.

%---------------------------------------------------------------------------%
:- end_module pair.
%---------------------------------------------------------------------------%
