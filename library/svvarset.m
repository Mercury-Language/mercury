%---------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: svvarset.m.

% This file provides an interface to the 'varset' ADT that is conducive to the
% user of state variable notation.  The predicates here do the same thing as
% their counterparts in the varset module; the only difference is the order of
% the arguments.

%--------------------------------------------------------------------------%

:- module svvarset.
:- interface.
:- import_module list, set, string, term, varset.
	
	% Create a new variable.
	%
:- pred svvarset__new_var(var(T)::out, varset(T)::in, varset(T)::out) is det.
	
	% Create a new named variable.
	% 
:- pred svvarset__new_named_var(string::in, var(T)::out,
	varset(T)::in, varset(T)::out) is det.
	
	% Create a new named variable with a unique (w.r.t. the
	% varset) number appended to the name.
	% 
:- pred svvarset__new_uniquely_named_var(string::in, var(T)::out,
	varset(T)::in, varset(T)::out) is det.
	
	% Create multiple new variables.
	% 
:- pred svvarset__new_vars(int::in, list(var(T))::out, varset(T)::in,
	varset(T)::out) is det.

	% Delete the name and value for a variable.
	%
:- pred svvarset__delete_var(var(T)::in, varset(T)::in, varset(T)::out) is det.
	
	% Delete the names and values for a list of variables.
	%
:- pred svvarset__delete_vars(list(var(T))::in, varset(T)::in, varset(T)::out)
	is det.
	
	% Set the name of a variable.
	%
:- pred svvarset__name_var(var(T)::in, string::in, varset(T)::in, varset(T)::out)
	is det.
	
	% Bind a value to a variable.
	% This will overwrite any existing binding.
	%
:- pred svvarset__bind_var(var(T)::in, term(T)::in, varset(T)::in,
	varset(T)::out) is det.
	
	% Bind a set of terms to a set of variables.
	%
:- pred svvarset__bind_vars(substitution(T)::in, varset(T)::in, varset(T)::out)
	is det.
	
	% Given a varset and a set of variables, remove the names
	% and values of any other variables stored in the varset.
	%
:- pred svvarset__select(set(var(T))::in, varset(T)::in, varset(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

svvarset__new_var(Var, Varset0, Varset) :-
	varset__new_var(Varset0, Var, Varset).

svvarset__new_named_var(Name, Var, Varset0, Varset) :-
	varset__new_named_var(Varset0, Name, Var, Varset).
	
svvarset__new_uniquely_named_var(Name, Var, Varset0, Varset) :-
	varset__new_uniquely_named_var(Varset0, Name, Var, Varset).

svvarset__new_vars(NumVars, NewVars, Varset0, Varset) :-
	varset__new_vars(Varset0, NumVars, NewVars, Varset).

svvarset__delete_var(Var, Varset0, Varset) :-
	varset__delete_var(Varset0, Var, Varset).

svvarset__delete_vars(Vars, Varset0, Varset) :-
	varset__delete_vars(Varset0, Vars, Varset).
	
svvarset__name_var(Id, Name, Varset0, Varset) :-
	varset__name_var(Varset0, Id, Name, Varset).

svvarset__bind_var(Id, Val, Varset0, Varset) :-
	varset__bind_var(Varset0, Id, Val, Varset).

svvarset__bind_vars(Subst, Varset0, Varset) :-
	varset__bind_vars(Varset0, Subst, Varset).
	
svvarset__select(Vars, Varset0, Varset) :-
	varset__select(Varset0, Vars, Varset).

%-----------------------------------------------------------------------------%
:- end_module svvarset.
%-----------------------------------------------------------------------------%
