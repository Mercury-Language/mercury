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
:- interface.

:- type var_id.
:- type varset.

	% initialize a varset
:- pred varset__init(varset).
:- mode varset__init(output).

	% create a new variable
:- pred varset__new_var(varset, var_id, varset).
:- mode varset__new_var(input, output, output).

	% set the name of a variable
	% (if there is already a variable with the same name "Foo",
	% then try naming it "Foo'", or "Foo''", or "Foo'''", etc. until
	% an unused name is found.)
:- pred varset__name_var(varset, var_id, string, varset).
:- mode varset__name_var(input, input, input, output).

	% lookup the name of a variable
:- pred varset__lookup_name(varset, var_id, string).
:- mode varset__lookup_name(input, input, output).

	% bind a value to a variable
:- pred varset__bind_var(varset, var_id, term, varset).
:- mode varset__bind_var(input, input, input, output).

	% lookup the value of a variable
:- pred varset__lookup_var(varset, var_id, term).
:- mode varset__lookup_var(input, input, output).

	% Combine two different varsets, renaming apart.
	% For efficiency, the biggest one should be the
	% first parameter, as this is O(size of second parameter).
:- pred varset__merge(varset, varset, varset).
:- mode varset__merge(input, input, output).
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, string, map, term.

:- type var_id	==	variable.
:- type varset	--->	varset(var_id, map(var_id, string), map(var_id, term)).

%-----------------------------------------------------------------------------%

varset__init(varset(0,Names,Vals)) :-
	map__init(Names),
	map__init(Vals).

%-----------------------------------------------------------------------------%

varset__new_var(varset(MaxId0,Names,Vals), MaxId0, varset(MaxId,Names,Vals)) :-
	MaxId is MaxId0 + 1.

%-----------------------------------------------------------------------------%

	% XXX efficiency problem: this is O(number of names in VarSet0),
	% because of the map__inverse_search.  We could avoid this
	% problem by storing both the map and the inverse map.

varset__name_var(VarSet0, Id, Name, VarSet) :-
	VarSet0 = varset(MaxId, Names0, Vals),
	(if some []
		(some [OtherId] map__inverse_search(Names0, Name, OtherId))
	then
		string__append(Name, "'", Name2),
		varset__name_var(VarSet0, Id, Name2, VarSet)
	else
		map__search_insert(Names0, Id, Name, Names),
		VarSet = varset(MaxId, Names, Vals)
	).

%-----------------------------------------------------------------------------%

varset__lookup_name(varset(_, Names, _), Id, Name) :-
	map__search(Names, Id, Name).

%-----------------------------------------------------------------------------%

varset__bind_var(varset(MaxId, Names, Vals0), Id, Val,
		varset(MaxId, Names, Vals)) :-
	map__search_insert(Vals0, Id, Val, Vals).

%-----------------------------------------------------------------------------%

varset__lookup_var(varset(_, _, Vals), Id, Val) :-
	map__search(Vals, Id, Val).

%-----------------------------------------------------------------------------%

	% This also has the same efficiency problem because it
	% calls varset__name_var.

varset__merge(VarSet0, varset(MaxId, Names, Vals),
		VarSet) :-
	varset__merge_2(0, MaxId, Names, Vals, VarSet0, VarSet).

:- pred varset__merge_2(var_id, var_id, map(var_id, string), map(var_id, term),
			varset, varset).
:- mode varset__merge_2(input, input, input, input, input, output).

varset__merge_2(N, Max, Names, Vals, VarSet0, VarSet) :-
	(if
		N = Max
	then
		VarSet = VarSet0
	else
		varset__new_var(VarSet0, VarId, VarSet1),
		(if some [Name]
			map__search(Names, N, Name)
		then
			varset__name_var(VarSet1, VarId, Name, VarSet2)
		else
			VarSet2 = VarSet1
		),
		N1 is N + 1,
		varset__merge_2(N1, Max, Names, Vals, VarSet2, VarSet)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
