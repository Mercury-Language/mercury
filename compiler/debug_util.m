:- module debug_util.

% main author: ksiew

% This module is mostly junk;
% it contains miscellaneous utility predicates used by debugger.m.
% We should probably throw both it and debugger.m away.
%	- fjh.

:- interface.
:- import_module hlds, io.

%  modules in compiler directory
:- import_module hlds_out, prog_io, mercury_to_mercury.
:- import_module mode_util, prog_util.
%  modules in library directory
:- import_module map, list, term, std_util, varset, require, string.

:- type highinfo ---> highinfo(module_info,list(var)).

:- pred do_termvar2termconst(varset,term,term).
:- mode do_termvar2termconst(in,in,out) is det.

:- pred do_termvar2termconst2(varset,term,term).
:- mode do_termvar2termconst2(in,in,out) is det.

:- pred do_termvar2termconst3(varset,list(term),list(term)).
:- mode do_termvar2termconst3(in,in,out) is det.

:- pred what_term(term,io__state,io__state).
:- mode what_term(in,di,uo) is det.

:- pred do_lookup_termvarname(varset,term,string).
:- mode do_lookup_termvarname(in,in,out) is det.

:- pred do_lookup_varname(varset,var,string).
:- mode do_lookup_varname(in,in,out) is det.

:- pred do_lookup_var(varset,var,term).
:- mode do_lookup_var(in,in,out) is det.

:- pred termvar2termfunctor(varset,term,term).
:- mode termvar2termfunctor(in,in,out) is det.

:- implementation.

%:- pred do_termvar2termconst(varset,term,term).
%:- mode do_termvar2termconst(in,in,out) is det.
do_termvar2termconst(VarSet,Tin,Tout) :-
	( Tin = term_variable(Var),
	  ( varset__lookup_var(VarSet,Var,TermValue) ->
	    do_termvar2termconst(VarSet,TermValue,Tout)
	  ;
	    ( varset__lookup_name(VarSet,Var,VarName) ->
	      string__append(VarName," is not in varset",Str)
	    ;
	      string__append("","Var have no name and not in varset",Str)
	    ),
	    term_context_init(Dummy),
	    Tout = term_functor(term_string(Str),[],Dummy)
	  )
	; Tin = term_functor(_,_,_),
	  do_termvar2termconst2(VarSet,Tin,Tout)
	).


%:- pred do_termvar2termconst2(varset,term,term).
%:- mode do_termvar2termconst2(in,in,out) is det.
do_termvar2termconst2(VarSet,Tin,Tout) :-
	( Tin = term_functor(A,B,C),
	  ( A = term_atom(_),
	    do_termvar2termconst3(VarSet,B,Bout),
	    Tout = term_functor(A,Bout,C)
	  ;
	    A = term_integer(_),
	    Tout = Tin
	  ;
	    A = term_string(_),
	    Tout = Tin
	  ;
	    A = term_float(_),
	    Tout = Tin
	  )
	;
	  Tin = term_variable(_),
	  error("Error in do_termvar2termconst2. got in impossible area\n")
	).

%:- pred do_termvar2termconst3(varset,list(term),list(term)).
%:- mode do_termvar2termconst3(in,in,out) is det.
do_termvar2termconst3(_VarSet,[],[]).

do_termvar2termconst3(VarSet,[A|As],Lout) :-
	do_termvar2termconst(VarSet,A,Aout),
	do_termvar2termconst3(VarSet,As,Asout),
	list__append([Aout],Asout,Lout).


%:- pred what_term(term,io__state,io__state).
%:- mode what_term(in,di,uo) is det.
what_term(Term) -->
	( { Term = term_variable(Var) },
	  io__write_string("term_variable\n")
	;
	  { Term = term_functor(A,_B,_C) },
	  io__write_string("term_functor\n"),
	  ( { A = term_atom(AS) },
	    io__write_string("term_atom("),
	    io__write_string(AS),
	    io__write_string(")\n")
	  ;
	    { A = term_integer(I) },
	    io__write_string("term_integer("),
	    io__write_int(I),
	    io__write_string(")/n")
	  ;
	    { A = term_string(S) },
	    io__write_string("term_string("),
	    io__write_string(S),
	    io__write_string(")/n")
	  ;
	    { A = term_float(F) },
	    io__write_string("term_float("),
	    io__write_float(F),
	    io__write_string(")/n")
	  ) 
	).

%:- pred do_lookup_termvarname(varset,term,string).
%:- mode do_lookup_termvarname(in,in,out) is det.
do_lookup_termvarname(VarSet,Tin,Name) :-
	( Tin = term_variable(Var),
	  ( varset__lookup_name(VarSet,Var,Name0) ->
	    Name = Name0
	  ;
	    Name = "noname"
	  )
	;
	  Tin = term_functor(_,_,_),
	  Name = "not a var"
	).

%:- pred do_lookup_varname(varset,var,string).
%:- mode do_lookup_varname(in,in,out) is det.
do_lookup_varname(VarSet,Var,Name) :-
	( varset__lookup_name(VarSet,Var,Name0) ->
	  Name = Name0
	;
	  Name = "noname"
	).
 

%:- pred do_lookup_var(varset,var,term).
%:- mode do_lookup_var(in,in,out) is det.
do_lookup_var(VarSet,Var,Term) :-
	( varset__lookup_var(VarSet,Var,T) ->
	  Term = T
	;
	  term_context_init(Dummy),
	  Term = term_functor(term_integer(1),[],Dummy),
	  error("do_lookup_var: No such var in varset\n")
	).

%:- pred termvar2termfunctor(varset,term,term).
%:- mode termvar2termfunctor(in,in,out) is det.
termvar2termfunctor(VarSet,Tin,Tout) :-
	( Tin = term_variable(Var),
	  ( varset__lookup_var(VarSet,Var,TermValue) ->
	    termvar2termfunctor(VarSet,TermValue,Tout)
	  ;
	    ( varset__lookup_name(VarSet,Var,VarName) ->
	      string__append(VarName," is not in varset",Str)
	    ;
	      string__append("","Var have no name and not in varset",Str)
	    ),
	    term_context_init(Dummy),
	    Tout = term_functor(term_string(Str),[],Dummy)
	  )
	; Tin = term_functor(_,_,_),
	  Tout = Tin
	).
