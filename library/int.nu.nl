%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: int.nu.nl.
% Main author: fjh.
%
% This file provides a Prolog implementation of `int__to_float'.
%
%-----------------------------------------------------------------------------%

int__to_float(Int, Float) :-
	% we could use `Float is float(Int)',
	% but the following is more portable (e.g. works on SWI-Prolog)
	Float is Int + 0.0.

%-----------------------------------------------------------------------------%
