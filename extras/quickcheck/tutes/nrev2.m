%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module nrev2.

:- interface.
:- import_module list. 

:- pred nrev2(list(T), list(T)).
:- mode nrev2(in, out) is det.

:- implementation.

nrev2([], []).
nrev2([X|Xs], Ys):- 
		nrev2(Xs, Reversed), 
		list__append(Reversed, [X], Ys).
