%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module mymax.

:- interface.
:- import_module int.

:- func mymax(int, int) = int.
:- mode mymax(in, in) = out is det.

:- implementation.

mymax(X, Y) = Z :-
	(if	Y >= X
	 then	
		Z = Y
	 else
		Z = X
	).
