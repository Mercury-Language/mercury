%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file provides facilities for manipulating collections of
% variables and terms.
% It provides the 'varset' ADT. A varset is a set of variables.
% (These variables are object-level variables, and are represented
% as ground terms, so it might help to think of them as "variable ids"
% rather than variables.)
% Associated with each variable there can be both a name and a value (binding).
% [But at the moment, the rest of the code is only using varsets to store
% names, not values.]
%
% There may be some design flaws in the relationship between varset.nl,
% term.nl, and graph.nl.  Once we have implemented unique modes and
% destructive assignment, we will need to rethink the design;  we may
% end up modifying these modules considerably, or we may end up
% making new single-threaded versions of these modules.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module varset.
:- interface.
:- import_module string, term.

:- type varset.

	% construct an empty varset.
:- pred varset__init(varset).
:- mode varset__init(out).

	% check whether a varset is empty.
:- pred varset__is_empty(varset).
:- mode varset__is_empty(in).

	% create a new variable
:- pred varset__new_var(varset, var, varset).
:- mode varset__new_var(in, out, out).

	% return a list of all the variables in a varset
:- pred varset__vars(varset, list(var)).
:- mode varset__vars(in, out).

	% set the name of a variable
	% (if there is already a variable with the same name "Foo",
	% then try naming it "Foo'", or "Foo''", or "Foo'''", etc. until
	% an unused name is found.)
:- pred varset__name_var(varset, var, string, varset).
:- mode varset__name_var(in, in, in, out).

	% lookup the name of a variable
:- pred varset__lookup_name(varset, var, string).
:- mode varset__lookup_name(in, in, out).

	% bind a value to a variable
:- pred varset__bind_var(varset, var, term, varset).
:- mode varset__bind_var(in, in, in, out).

	% lookup the value of a variable
:- pred varset__lookup_var(varset, var, term).
:- mode varset__lookup_var(in, in, out).

	% Combine two different varsets, renaming apart:
	% varset__merge(VarSet0, NewVarSet, Terms0, VarSet, Terms) is
	% true iff VarSet is the varset that results from joining
	% VarSet0 to a suitably renamed version of NewVarSet,
	% and Terms is Terms0 renamed accordingly.
	% (Any bindings in NewVarSet are ignored.)

:- pred varset__merge(varset, varset, list(term), varset, list(term)).
:- mode varset__merge(in, in, in, out, out).

	% As above, except return the substitution directly
	% rather than applying it to a list of terms.

:- pred varset__merge_subst(varset, varset, varset, substitution).
:- mode varset__merge_subst(in, in, in, out).


%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map.

:- type varset		--->	varset(
					var_supply,
					map(var, string),
					map(var, term)
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

varset__vars(varset(MaxId0,_,_), L) :-
	term__init_var_supply(V0),
	varset__vars_2(V0, MaxId0, [], L1),
	reverse(L1, L).

:- pred varset__vars_2(var_supply, var_supply, list(var),
			list(var)).
:- mode varset__vars_2(in, in, in, out).

varset__vars_2(N, Max, L0, L) :-
	(N = Max ->
		L = L0
	;
		term__create_var(N, V, N1),
		varset__vars_2(N1, Max, [V|L0], L)
	).

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

varset__merge(VarSet0, VarSet1, TermList0, VarSet, TermList) :-
	varset__merge_subst(VarSet0, VarSet1, VarSet, Subst),
	term__apply_substitution_to_list(TermList0, Subst, TermList).

varset__merge_subst(VarSet0, varset(MaxId, Names, Vals), VarSet, Subst) :-
	term__init_var_supply(N),
	map__init(Subst0),
	varset__merge_subst_2(N, MaxId, Names, Vals, VarSet0, Subst0,
				VarSet, Subst).

:- pred varset__merge_subst_2(var_supply, var_supply, map(var, string),
	map(var, term), varset, substitution, varset, substitution).
:- mode varset__merge_subst_2(in, in, in, in, in, in,
	out, out).

varset__merge_subst_2(N, Max, Names, Vals, VarSet0, Subst0, VarSet, Subst) :-
	( N = Max ->
		VarSet = VarSet0,
		Subst0 = Subst
	;
		varset__new_var(VarSet0, VarId, VarSet1),
		term__create_var(N, VarN, N1),
		(
			%some [Name]
			map__search(Names, VarN, Name)
		->
			varset__name_var(VarSet1, VarId, Name, VarSet2)
		;
			VarSet2 = VarSet1
		),
		map__insert(Subst0, VarN, term_variable(VarId), Subst1),
		varset__merge_subst_2(N1, Max, Names, Vals, VarSet2, Subst1,
				VarSet, Subst)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
