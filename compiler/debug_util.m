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
	( Tin = term__variable(Var),
	  ( varset__lookup_var(VarSet,Var,TermValue) ->
	    do_termvar2termconst(VarSet,TermValue,Tout)
	  ;
	    ( varset__lookup_name(VarSet,Var,VarName) ->
	      string__append(VarName," is not in varset",Str)
	    ;
	      string__append("","Var have no name and not in varset",Str)
	    ),
	    term__context_init(Dummy),
	    Tout = term__functor(term__string(Str),[],Dummy)
	  )
	; Tin = term__functor(_,_,_),
	  do_termvar2termconst2(VarSet,Tin,Tout)
	).


%:- pred do_termvar2termconst2(varset,term,term).
%:- mode do_termvar2termconst2(in,in,out) is det.
do_termvar2termconst2(VarSet,Tin,Tout) :-
	( Tin = term__functor(A,B,C),
	  ( A = term__atom(_),
	    do_termvar2termconst3(VarSet,B,Bout),
	    Tout = term__functor(A,Bout,C)
	  ;
	    A = term__integer(_),
	    Tout = Tin
	  ;
	    A = term__string(_),
	    Tout = Tin
	  ;
	    A = term__float(_),
	    Tout = Tin
	  )
	;
	  Tin = term__variable(_),
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
	( { Term = term__variable(Var) },
	  io__write_string("term__variable\n")
	;
	  { Term = term__functor(A,_B,_C) },
	  io__write_string("term__functor\n"),
	  ( { A = term__atom(AS) },
	    io__write_string("term__atom("),
	    io__write_string(AS),
	    io__write_string(")\n")
	  ;
	    { A = term__integer(I) },
	    io__write_string("term__integer("),
	    io__write_int(I),
	    io__write_string(")/n")
	  ;
	    { A = term__string(S) },
	    io__write_string("term__string("),
	    io__write_string(S),
	    io__write_string(")/n")
	  ;
	    { A = term__float(F) },
	    io__write_string("term__float("),
	    io__write_float(F),
	    io__write_string(")/n")
	  ) 
	).

%:- pred do_lookup_termvarname(varset,term,string).
%:- mode do_lookup_termvarname(in,in,out) is det.
do_lookup_termvarname(VarSet,Tin,Name) :-
	( Tin = term__variable(Var),
	  ( varset__lookup_name(VarSet,Var,Name0) ->
	    Name = Name0
	  ;
	    Name = "noname"
	  )
	;
	  Tin = term__functor(_,_,_),
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
	  term__context_init(Dummy),
	  Term = term__functor(term__integer(1),[],Dummy),
	  error("do_lookup_var: No such var in varset\n")
	).

%:- pred termvar2termfunctor(varset,term,term).
%:- mode termvar2termfunctor(in,in,out) is det.
termvar2termfunctor(VarSet,Tin,Tout) :-
	( Tin = term__variable(Var),
	  ( varset__lookup_var(VarSet,Var,TermValue) ->
	    termvar2termfunctor(VarSet,TermValue,Tout)
	  ;
	    ( varset__lookup_name(VarSet,Var,VarName) ->
	      string__append(VarName," is not in varset",Str)
	    ;
	      string__append("","Var have no name and not in varset",Str)
	    ),
	    term__context_init(Dummy),
	    Tout = term__functor(term__string(Str),[],Dummy)
	  )
	; Tin = term__functor(_,_,_),
	  Tout = Tin
	).
