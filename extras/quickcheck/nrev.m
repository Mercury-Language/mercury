%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module nrev.

:- interface.
:- import_module list. 

:- func nrev(list(T)) = list(T).
:- mode nrev(in) = out is det.

:- implementation.

nrev([]) = [].
nrev([X|Xs]) = Ys :- 
		list__append(nrev(Xs), [X], Ys).
