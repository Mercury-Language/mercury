%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018, 2024 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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
:- func fst(pair(T1, T2)) = T1.
:- pred fst(pair(T1, T2)::in, T1::out) is det.

    % Return the second element of the pair.
    %
:- func snd(pair(T1, T2)) = T2.
:- pred snd(pair(T1, T2)::in, T2::out) is det.

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
