%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file provides facilities for manipulating logical terms.
% It provides the 'varset' ADT. A varset is a set of variables.
% (These variables are object-level variables, and are represented
% as ground terms, so it might help to think of them as "variable ids"
% rather than variables.)
% Associated with each id there can be both a name and a value (binding).
%
% Currently ids are implemented as integers and bindings as maps,
% but we should re-implement this using addresses as ids (if and
% when NU-Prolog supports the necessary primitive operations).
%
% Design problem: this is slightly over-specialized.
% Really, there should be a general "graph" data type for manipulating
% arbitrary graphs and varset should be implemented using that.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module varset.
:- export_pred	varset__init, varset__new_var, varset__name_var,
		varset__lookup_name, varset__bind_var, varset__lookup_var.

:- import_module integer, string, map.

:- type var_id	==	integer.
:- type varset	--->	varset(var_id, map(var_id, string), map(var_id, term)).

%-----------------------------------------------------------------------------%

	% initialize a varset
:- pred varset__init(varset).
varset__init(varset(0,Names,Vals)) :-
	map__init(Names),
	map__init(Vals).

%-----------------------------------------------------------------------------%

	% create a new variable
:- pred varset__new_var(varset, var_id, varset).
varset__new_var(varset(MaxId0,Names,Vals), MaxId0, varset(MaxId,Names,Vals)) :-
	MaxId is MaxId0 + 1.

%-----------------------------------------------------------------------------%

	% set the name of a variable
:- pred varset__name_var(varset, var_id, string, varset).
varset__name_var(varset(MaxId, Names0, Vals), Id, Name,
		varset(MaxId, Names, Vals)) :-
	map__search_insert(Names0, Id, Name, Names).

%-----------------------------------------------------------------------------%

	% lookup the name of a variable
:- pred varset__name(varset, var_id, string).
varset__lookup_name(varset(_, Names, _), Id, Name) :-
	map__search(Names, Id, Name).

%-----------------------------------------------------------------------------%

	% bind a value to a variable
:- pred varset__bind_var(varset, var_id, term, varset).
varset__bind_var(varset(MaxId, Names, Vals0), Id, Val,
		varset(MaxId, Names, Vals)) :-
	map__search_insert(Vals0, Id, Val, Vals).

%-----------------------------------------------------------------------------%

	% lookup the value of a variable
:- pred varset__lookup_var(varset, var_id, term).
varset__lookup_var(varset(_, _, Vals), Id, Val) :-
	map__search(Vals, Id, Val).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
