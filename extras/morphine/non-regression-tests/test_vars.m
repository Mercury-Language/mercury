%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.

:- module test_vars.

:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, std_util.

:- pred p(pair(int)::out) is det.
  
main(IO, IO) :- 
	p(_). 
 
p(3-8).
