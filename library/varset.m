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
% [But at the moment, the rest of the code is only using varsets to store
% names, not values.]
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
:- import_module string, term.

:- type varset.

:- type var_id	==	variable.

	% construct an empty varset.
:- pred varset__init(varset).
:- mode varset__init(output).

	% check whether a varset is empty.
:- pred varset__is_empty(varset).
:- mode varset__is_empty(input).

	% create a new variable
:- pred varset__new_var(varset, variable, varset).
:- mode varset__new_var(input, output, output).

	% set the name of a variable
	% (if there is already a variable with the same name "Foo",
	% then try naming it "Foo'", or "Foo''", or "Foo'''", etc. until
	% an unused name is found.)
:- pred varset__name_var(varset, variable, string, varset).
:- mode varset__name_var(input, input, input, output).

	% lookup the name of a variable
:- pred varset__lookup_name(varset, variable, string).
:- mode varset__lookup_name(input, input, output).

	% bind a value to a variable
:- pred varset__bind_var(varset, variable, term, varset).
:- mode varset__bind_var(input, input, input, output).

	% lookup the value of a variable
:- pred varset__lookup_var(varset, variable, term).
:- mode varset__lookup_var(input, input, output).

	% Combine two different varsets, renaming apart:
	% varset__merge(VarSet0, NewVarSet, Terms0, VarSet, Terms) is
	% true iff VarSet is the varset that results from joining
	% VarSet0 to a suitably renamed version of NewVarSet,
	% and Terms is Terms0 renamed accordingly.
	% (Any bindings in NewVarSet are ignored.)

:- pred varset__merge(varset, varset, list(term), varset, list(term)).
:- mode varset__merge(input, input, input, output, output).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map.

:- type varset		--->	varset(
					var_supply,
					map(variable, string),
					map(variable, term)
				).

%-----------------------------------------------------------------------------%

varset__init(varset(VarSupply, Names, Vals)) :-
	term__init_var_supply(VarSupply),
	map__init(Names),
	map__init(Vals).

%-----------------------------------------------------------------------------%

varset__is_empty(varset(VarSupply, _, _)) :-
	term__init_var_supply(VarSupply).

%-----------------------------------------------------------------------------%

varset__new_var(varset(MaxId0,Names,Vals), Var, varset(MaxId,Names,Vals)) :-
	term__create_var(MaxId0, Var, MaxId).

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

	% We scan through the second varset, introducing a fresh
	% variable into the first varset for each var in the
	% second, and building up a substitution which maps
	% the variables in the second varset into the corresponding
	% fresh variable in the first varset.  We then apply
	% this substition to the list of terms.

varset__merge(VarSet0, varset(MaxId, Names, Vals), TermList0,
		VarSet, TermList) :-
	map__init(Subst0),
	varset__merge_2(0, MaxId, Names, Vals, VarSet0, Subst0, VarSet, Subst),
	term__apply_substitution_to_list(TermList0, Subst, TermList).

:- pred varset__merge_2(variable, variable, map(variable, string),
	map(variable, term), varset, substitution, varset, substitution).
:- mode varset__merge_2(input, input, input, input, input, input,
	output, output).

varset__merge_2(N, Max, Names, Vals, VarSet0, Subst0, VarSet, Subst) :-
	( N = Max ->
		VarSet = VarSet0,
		Subst0 = Subst
	;
		varset__new_var(VarSet0, VarId, VarSet1),
		(if some [Name]
			map__search(Names, N, Name)
		then
			varset__name_var(VarSet1, VarId, Name, VarSet2)
		else
			VarSet2 = VarSet1
		),
		map__insert(Subst0, VarN, term_variable(VarId), Subst1),
		map__create_var(N, VarN, N1),
		varset__merge_2(N1, Max, Names, Vals, VarSet2, Subst1,
				VarSet, Subst)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
