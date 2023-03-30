%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%
%
% File:        misc.m
% Main author: conway
%
% Provides miscellaneous functionality required by user.m and basics.m
%
%---------------------------------------------------------------------------%

:- module mcurses.misc.
:- interface.

:- import_module array.
:- import_module int.

  % for(Accumulator, Max, Closure, StoreIn, StoreOut)
  % perform an operation much like a `for loop' in imperative languages. For
  % every value of Accumulator =< Max, call Closure with the current value of
  % Accumulator.
  %
  % Example:
  %
  % main -->
  %     for(0, 5, (pred(Num::in, di, uo) is det -->
  %             io.print(Num)
  %     )).
  %
  % Would print "012345".
:- pred for(int, int, pred(int, T, T), T, T).
:- mode for(in, in, pred(in, in, out) is det, in, out) is det.
:- mode for(in, in, pred(in, in, out) is semidet, in, out) is semidet.
:- mode for(in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode for(in, in, pred(in, array_di, array_uo) is det,
    array_di, array_uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

for(Min, Max, Pred, Acc0, Acc) :-
    ( if Min =< Max then
        Pred(Min, Acc0, Acc1),
        for(Min + 1, Max, Pred, Acc1, Acc)
    else
        Acc = Acc0
    ).
