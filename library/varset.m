%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: varset.m.
% Main author: fjh.
% Stability: low.
%
% This file provides facilities for manipulating collections of
% variables and terms.
% It provides the 'varset' ADT. A varset is a set of variables.
% (These variables are object-level variables, and are represented
% as ground terms, so it might help to think of them as "variable ids"
% rather than variables.)
% Associated with each variable there can be both a name and a value
% (binding).
%
% There may be some design flaws in the relationship between varset.m,
% term.m, and graph.m.  Once we have implemented unique modes and
% destructive assignment, we will need to rethink the design;  we may
% end up modifying these modules considerably, or we may end up
% making new single-threaded versions of these modules.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module varset.
:- interface.
:- import_module std_util, term, list, map, set, assoc_list.

:- type varset(T).

:- type varset	==	varset(generic).

	% construct an empty varset.
:- pred varset__init(varset(T)).
:- mode varset__init(out) is det.

:- func varset__init = varset(T).

	% check whether a varset is empty.
:- pred varset__is_empty(varset(T)).
:- mode varset__is_empty(in) is semidet.

	% create a new variable
:- pred varset__new_var(varset(T), var(T), varset(T)).
:- mode varset__new_var(in, out, out) is det.

	% create a new named variable
:- pred varset__new_named_var(varset(T), string, var(T), varset(T)).
:- mode varset__new_named_var(in, in, out, out) is det.

	% create a new named variable with a unique (w.r.t. the
	% varset) number appended to the name.
:- pred varset__new_uniquely_named_var(varset(T), string, var(T), varset(T)).
:- mode varset__new_uniquely_named_var(in, in, out, out) is det.

	% create a new variable, and maybe give it a name
:- pred varset__new_maybe_named_var(varset(T), maybe(string),
	var(T), varset(T)).
:- mode varset__new_maybe_named_var(in, in, out, out) is det.

	% create multiple new variables
:- pred varset__new_vars(varset(T), int, list(var(T)), varset(T)).
:- mode varset__new_vars(in, in, out, out) is det.

	% delete the name and value for a variable
:- pred varset__delete_var(varset(T), var(T), varset(T)).
:- mode varset__delete_var(in, in, out) is det.

:- func varset__delete_var(varset(T), var(T)) = varset(T).

	% delete the names and values for a list of variables
:- pred varset__delete_vars(varset(T), list(var(T)), varset(T)).
:- mode varset__delete_vars(in, in, out) is det.

:- func varset__delete_vars(varset(T), list(var(T))) = varset(T).

	% return a list of all the variables in a varset
:- pred varset__vars(varset(T), list(var(T))).
:- mode varset__vars(in, out) is det.

:- func varset__vars(varset(T)) = list(var(T)).

	% set the name of a variable
:- pred varset__name_var(varset(T), var(T), string, varset(T)).
:- mode varset__name_var(in, in, in, out) is det.

:- func varset__name_var(varset(T), var(T), string) = varset(T).

	% lookup the name of a variable;
	% create one if it doesn't have one using V_ as a prefix
:- pred varset__lookup_name(varset(T), var(T), string).
:- mode varset__lookup_name(in, in, out) is det.

:- func varset__lookup_name(varset(T), var(T)) = string.

	% lookup the name of a variable;
	% create one if it doesn't have one using the specified prefix
:- pred varset__lookup_name(varset(T), var(T), string, string).
:- mode varset__lookup_name(in, in, in, out) is det.

:- func varset__lookup_name(varset(T), var(T), string) = string.

	% lookup the name of a variable;
	% fail if it doesn't have one
:- pred varset__search_name(varset(T), var(T), string).
:- mode varset__search_name(in, in, out) is semidet.

	% bind a value to a variable
	% (will overwrite any existing binding).
:- pred varset__bind_var(varset(T), var(T), term(T), varset(T)).
:- mode varset__bind_var(in, in, in, out) is det.

:- func varset__bind_var(varset(T), var(T), term(T)) = varset(T).

	% bind a set of terms to a set of variables.
:- pred varset__bind_vars(varset(T), substitution(T), varset(T)).
:- mode varset__bind_vars(in, in, out) is det.

:- func varset__bind_vars(varset(T), substitution(T)) = varset(T).

	% lookup the value of a variable
:- pred varset__search_var(varset(T), var(T), term(T)).
:- mode varset__search_var(in, in, out) is semidet.

	% get the bindings for all the bound variables.
:- pred varset__lookup_vars(varset(T), substitution(T)).
:- mode varset__lookup_vars(in, out) is det.

:- func varset__lookup_vars(varset(T)) = substitution(T).

	% Combine two different varsets, renaming apart:
	% varset__merge(VarSet0, NewVarSet, Terms0, VarSet, Terms) is
	% true iff VarSet is the varset that results from joining
	% VarSet0 to a suitably renamed version of NewVarSet,
	% and Terms is Terms0 renamed accordingly.
	% (Any bindings in NewVarSet are ignored.)

:- pred varset__merge(varset(T), varset(T), list(term(T)),
		varset(T), list(term(T))).
:- mode varset__merge(in, in, in, out, out) is det.

	% As above, except return the substitution directly
	% rather than applying it to a list of terms.

:- pred varset__merge_subst(varset(T), varset(T), varset(T), substitution(T)).
:- mode varset__merge_subst(in, in, out, out) is det.

	% Same as varset__merge, except that the names of variables
	% in NewVarSet are not included in the final varset.
	% This is useful if varset__create_name_var_map needs
	% to be used on the resulting varset.

:- pred varset__merge_without_names(varset(T), varset(T), list(term(T)),
		varset(T), list(term(T))).
:- mode varset__merge_without_names(in, in, in, out, out) is det.

	% As above, except return the substitution directly
	% rather than applying it to a list of terms.

:- pred varset__merge_subst_without_names(varset(T),
		varset(T), varset(T), substitution(T)).
:- mode varset__merge_subst_without_names(in, in, out, out) is det.

	% get the bindings for all the bound variables.
:- pred varset__get_bindings(varset(T), substitution(T)).
:- mode varset__get_bindings(in, out) is det.

:- func varset__get_bindings(varset(T)) = substitution(T).

	% set the bindings for all the bound variables.
:- pred varset__set_bindings(varset(T), substitution(T), varset(T)).
:- mode varset__set_bindings(in, in, out) is det.

:- func varset__set_bindings(varset(T), substitution(T)) = varset(T).

	% Create a map from names to variables.
	% Each name is mapped to only one variable, even if a name is
	% shared by more than one variable. Therefore this predicate
	% is only really useful if it is already known that no two
	% variables share the same name.
:- pred varset__create_name_var_map(varset(T), map(string, var(T))).
:- mode varset__create_name_var_map(in, out) is det.

:- func varset__create_name_var_map(varset(T)) = map(string, var(T)).

	% Return an association list giving the name of each variable.
	% Every variable has an entry in the returned association list,
	% even if it shares its name with another variable.
:- pred varset__var_name_list(varset(T), assoc_list(var(T), string)).
:- mode varset__var_name_list(in, out) is det.

:- func varset__var_name_list(varset(T)) = assoc_list(var(T), string).

	% Given a list of variable and varset in which some variables have
	% no name but some other variables may have the same name,
	% return another varset in which every variable has a unique name.
	% If necessary, names will have suffixes added on the end;
	% the second argument gives the suffix to use.
:- pred varset__ensure_unique_names(list(var(T)),
		string, varset(T), varset(T)).
:- mode varset__ensure_unique_names(in, in, in, out) is det.

:- func varset__ensure_unique_names(list(var(T)),
		string, varset(T)) = varset(T).

	% Given a varset and a set of variables, remove the names
	% and values of any other variables stored in the varset.
:- pred varset__select(varset(T), set(var(T)), varset(T)).
:- mode varset__select(in, in, out) is det.

:- func varset__select(varset(T), set(var(T))) = varset(T).

	% Given a varset and a list of variables, construct a new varset
	% containing one variable for each one in the list (and no others).
	% Also return a substitution mapping the selected variables in the
	% original varset into variables in the new varset. The relative
	% ordering of variables in the original varset is maintained.
:- pred varset__squash(varset(T), list(var(T)),
		varset(T), map(var(T), var(T))).
:- mode varset__squash(in, in, out, out) is det.

	% Coerce the types of the variables in a varset.
:- pred varset__coerce(varset(T), varset(U)).
:- mode varset__coerce(in, out) is det.

:- func varset__coerce(varset(T)) = varset(U).

:- implementation.
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, int, list, map, assoc_list.
:- import_module set, require, string.

:- type varset(T)	--->	varset(
					var_supply(T),
					map(var(T), string),
					map(var(T), term(T))
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

varset__new_var(varset(MaxId0, Names, Vals), Var,
		varset(MaxId, Names, Vals)) :-
	term__create_var(MaxId0, Var, MaxId).

varset__new_named_var(varset(MaxId0, Names0, Vals), Name, Var,
		varset(MaxId, Names, Vals)) :-
	term__create_var(MaxId0, Var, MaxId),
	map__set(Names0, Var, Name, Names).

varset__new_uniquely_named_var(varset(MaxId0, Names0, Vals), Name, Var,
		varset(MaxId, Names, Vals)) :-
	term__create_var(MaxId0, Var, MaxId),
	N = term__var_id(Var),
	map__set(Names0, Var, string__format("%s_%d", [s(Name), i(N)]), Names).

varset__new_maybe_named_var(varset(MaxId0, Names0, Vals), MaybeName, Var,
		varset(MaxId, Names, Vals)) :-
	term__create_var(MaxId0, Var, MaxId),
	(
		MaybeName = no,
		Names = Names0
	;
		MaybeName = yes(Name),
		map__set(Names0, Var, Name, Names)
	).

varset__new_vars(Varset0, NumVars, NewVars, Varset) :-
	varset__new_vars_2(Varset0, NumVars, [], NewVars, Varset).

:- pred varset__new_vars_2(varset(T), int, list(var(T)),
		list(var(T)), varset(T)).
:- mode varset__new_vars_2(in, in, in, out, out) is det.

varset__new_vars_2(Varset0, NumVars, NewVars0, NewVars, Varset) :-
	(
		NumVars > 0
	->
		NumVars1 is NumVars - 1,
		varset__new_var(Varset0, Var, Varset1),
		varset__new_vars_2(Varset1, NumVars1, [Var | NewVars0],
							NewVars, Varset)
	;
		NumVars = 0
	->
		NewVars = NewVars0,
		Varset = Varset0
	;
		error("varset__new_vars - invalid call")
	).
		
%-----------------------------------------------------------------------------%

varset__delete_var(varset(MaxId, Names0, Vals0), Var,
		varset(MaxId, Names, Vals)) :-
	map__delete(Names0, Var, Names),
	map__delete(Vals0, Var, Vals).

%-----------------------------------------------------------------------------%

varset__delete_vars(Varset, [], Varset).
varset__delete_vars(Varset0, [Var | Vars], Varset) :-
	varset__delete_var(Varset0, Var, Varset1),
	varset__delete_vars(Varset1, Vars, Varset).

%-----------------------------------------------------------------------------%

varset__vars(varset(MaxId0, _, _), L) :-
	term__init_var_supply(V0),
	varset__vars_2(V0, MaxId0, [], L1),
	list__reverse(L1, L).

:- pred varset__vars_2(var_supply(T), var_supply(T), list(var(T)),
			list(var(T))).
:- mode varset__vars_2(in, in, in, out) is det.

varset__vars_2(N, Max, L0, L) :-
	(N = Max ->
		L = L0
	;
		term__create_var(N, V, N1),
		varset__vars_2(N1, Max, [V|L0], L)
	).

%-----------------------------------------------------------------------------%

varset__name_var(VarSet0, Id, Name, VarSet) :-
	VarSet0 = varset(MaxId, Names0, Vals),
	map__set(Names0, Id, Name, Names),
	VarSet = varset(MaxId, Names, Vals).

%-----------------------------------------------------------------------------%

varset__lookup_name(Varset, Id, Name) :-
	( varset__search_name(Varset, Id, Name0) ->
		Name = Name0
	;
		term__var_to_int(Id, VarNum),
		string__int_to_string(VarNum, NumStr),
		string__append("V_", NumStr, Name)
	).

varset__lookup_name(Varset, Id, Prefix, Name) :-
	( varset__search_name(Varset, Id, Name0) ->
		Name = Name0
	;
		term__var_to_int(Id, VarNum),
		string__int_to_string(VarNum, NumStr),
		string__append(Prefix, NumStr, Name)
	).

varset__search_name(varset(_, Names, _), Id, Name) :-
	map__search(Names, Id, Name0),
	Name = Name0.
/* This part is useful during debugging when you need to
   be able to distinguish different variables with the same name.
	(
		map__member(Names, Id1, Name0),
		Id1 \= Id
	->
		term__var_to_int(Id, Int),
		string__format("%s__%d",[s(Name0),i(Int)], Name)
	;
		Name = Name0
	).
*/

%-----------------------------------------------------------------------------%

varset__bind_var(varset(MaxId, Names, Vals0), Id, Val,
		varset(MaxId, Names, Vals)) :-
	map__set(Vals0, Id, Val, Vals).

%-----------------------------------------------------------------------------%

varset__bind_vars(Varset0, Subst, Varset) :-
	map__to_assoc_list(Subst, VarTermList),
	varset__bind_vars_2(VarTermList, Varset0, Varset).

:- pred varset__bind_vars_2(assoc_list(var(T), term(T)), varset(T), varset(T)).
:- mode varset__bind_vars_2(in, in, out) is det.

varset__bind_vars_2([], Varset, Varset).
varset__bind_vars_2([V - T | Rest], Varset0, Varset) :-
	varset__bind_var(Varset0, V, T, Varset1),
	varset__bind_vars_2(Rest, Varset1, Varset).

%-----------------------------------------------------------------------------%

varset__search_var(varset(_, _, Vals), Id, Val) :-
	map__search(Vals, Id, Val).

%-----------------------------------------------------------------------------%

varset__lookup_vars(varset(_, _, Subst), Subst).

%-----------------------------------------------------------------------------%

varset__get_bindings(varset(_, _, Subst), Subst).

varset__set_bindings(varset(C, N, _), S, varset(C, N, S)).

%-----------------------------------------------------------------------------%

	% We scan through the second varset, introducing a fresh
	% variable into the first varset for each var in the
	% second, and building up a substitution which maps
	% the variables in the second varset into the corresponding
	% fresh variable in the first varset.  We then apply
	% this substition to the list of terms.

varset__merge(VarSet0, VarSet1, TermList0, VarSet, TermList) :-
	IncludeNames = yes,
	varset__merge_subst(IncludeNames, VarSet0, VarSet1, VarSet, Subst),
	term__apply_substitution_to_list(TermList0, Subst, TermList).

varset__merge_without_names(VarSet0, VarSet1, TermList0, VarSet, TermList) :-
	IncludeNames = no,
	varset__merge_subst(IncludeNames,
		VarSet0, VarSet1, VarSet, Subst),
	term__apply_substitution_to_list(TermList0, Subst, TermList).

varset__merge_subst(VarSet0, varset(MaxId, Names, Vals),
			VarSet, Subst) :-
	IncludeNames = yes,	
	varset__merge_subst(IncludeNames, VarSet0, varset(MaxId, Names, Vals),
			VarSet, Subst).

varset__merge_subst_without_names(VarSet0, varset(MaxId, Names, Vals),
			VarSet, Subst) :-
	IncludeNames = no,	
	varset__merge_subst(IncludeNames, VarSet0, varset(MaxId, Names, Vals),
			VarSet, Subst).

:- pred varset__merge_subst(bool, varset(T), varset(T), varset(T),
		substitution(T)).
:- mode varset__merge_subst(in, in, in, out, out) is det.

varset__merge_subst(IncludeNames, VarSet0, varset(MaxId, Names, Vals),
			VarSet, Subst) :-
	term__init_var_supply(N),
	map__init(Subst0),
	varset__merge_subst_2(IncludeNames, N, MaxId, Names, Vals,
			VarSet0, Subst0, VarSet, Subst).

:- pred varset__merge_subst_2(bool, var_supply(T),
	var_supply(T), map(var(T), string),
	map(var(T), term(T)), varset(T), substitution(T),
	varset(T), substitution(T)).
:- mode varset__merge_subst_2(in, in, in, in, in, in, in, out, out) is det.

varset__merge_subst_2(IncludeNames, N, Max, Names, Vals,
		VarSet0, Subst0, VarSet, Subst) :-
	( N = Max ->
		VarSet = VarSet0,
		Subst0 = Subst
	;
		varset__new_var(VarSet0, VarId, VarSet1),
		term__create_var(N, VarN, N1),
		(
			IncludeNames = yes,
			map__search(Names, VarN, Name)
		->
			varset__name_var(VarSet1, VarId, Name, VarSet2)
		;
			VarSet2 = VarSet1
		),
		map__set(Subst0, VarN, term__variable(VarId), Subst1),
		varset__merge_subst_2(IncludeNames, N1, Max, Names,
				Vals, VarSet2, Subst1, VarSet, Subst)
	).

%-----------------------------------------------------------------------------%

varset__create_name_var_map(varset(_, VarNameIndex, _), NameVarIndex) :-
	map__keys(VarNameIndex, Vars),
	map__values(VarNameIndex, Names),
	map__from_corresponding_lists(Names, Vars, NameVarIndex).

%-----------------------------------------------------------------------------%

varset__var_name_list(varset(_, VarNameIndex, _), VarNameList) :-
	map__to_assoc_list(VarNameIndex, VarNameList).

%-----------------------------------------------------------------------------%

varset__ensure_unique_names(AllVars, Suffix,
		varset(Supply, VarNameMap0, Values),
		varset(Supply, VarNameMap, Values)) :-
	set__init(UsedNames),
	map__init(VarNameMap1),
	varset__ensure_unique_names_2(AllVars, Suffix, UsedNames, VarNameMap0,
		VarNameMap1, VarNameMap).

:- pred varset__ensure_unique_names_2(list(var(T)), string, set(string),
	map(var(T), string), map(var(T), string), map(var(T), string)).
:- mode varset__ensure_unique_names_2(in, in, in, in, in, out) is det.

varset__ensure_unique_names_2([], _, _, _, VarNameMap, VarNameMap).
varset__ensure_unique_names_2([Var | Vars], Suffix, UsedNames0,
		OldVarNameMap, VarNameMap0, VarNameMap) :-
	( map__search(OldVarNameMap, Var, OldName) ->
		( set__member(OldName, UsedNames0) ->
			term__var_to_int(Var, VarNum),
			string__int_to_string(VarNum, NumStr),
			string__append("_", NumStr, NumSuffix),
			string__append(OldName, NumSuffix, TrialName)
		;
			TrialName = OldName
		)
	;
		term__var_to_int(Var, VarNum),
		string__int_to_string(VarNum, NumStr),
		string__append("Var_", NumStr, TrialName)
	),
	varset__ensure_unique_names_3(TrialName, Suffix, UsedNames0, FinalName),
	set__insert(UsedNames0, FinalName, UsedNames1),
	map__det_insert(VarNameMap0, Var, FinalName, VarNameMap1),
	varset__ensure_unique_names_2(Vars, Suffix, UsedNames1, OldVarNameMap,
		VarNameMap1, VarNameMap).

:- pred varset__ensure_unique_names_3(string, string, set(string), string).
:- mode varset__ensure_unique_names_3(in, in, in, out) is det.

varset__ensure_unique_names_3(Trial0, Suffix, UsedNames, Final) :-
	( set__member(Trial0, UsedNames) ->
		string__append(Trial0, Suffix, Trial1),
		varset__ensure_unique_names_3(Trial1, Suffix, UsedNames, Final)
	;
		Final = Trial0
	).

%-----------------------------------------------------------------------------%

varset__select(varset(Supply, VarNameMap0, Values0), Vars,
		varset(Supply, VarNameMap, Values)) :-
	map__select(VarNameMap0, Vars, VarNameMap),
	map__select(Values0, Vars, Values).

%-----------------------------------------------------------------------------%

varset__squash(OldVarSet, KeptVars, NewVarSet, Subst) :-
	%
	% Create a new varset with the same number of variables. 
	%
	list__length(KeptVars, NumVars),
	varset__init(NewVarSet0),
	varset__new_vars(NewVarSet0, NumVars, 
		NewVars0, NewVarSet1),
	%
	% We need to sort the fresh variables, to
	% ensure that the substitution that we create below
	% does not alter the relative ordering of the variables
	%
	list__sort(NewVars0, NewVars),

	%
	% Copy the variable names across from the old
	% varset to the new varset.
	%
	varset__var_name_list(OldVarSet, VarNames),
	map__from_corresponding_lists(KeptVars, NewVars, Subst),
	copy_var_names(VarNames, Subst, NewVarSet1, NewVarSet).

:- pred copy_var_names(assoc_list(var(T), string), map(var(T), var(T)),
		varset(T), varset(T)).
:- mode copy_var_names(in, in, in, out) is det.

copy_var_names([], _Subst, NewVarSet, NewVarSet).
copy_var_names([OldVar - Name | Rest], Subst, NewVarSet0, NewVarSet) :-
	( map__search(Subst, OldVar, NewVar) ->
		varset__name_var(NewVarSet0, NewVar, Name, NewVarSet1)
	;
		NewVarSet1 = NewVarSet0
	),
	copy_var_names(Rest, Subst, NewVarSet1, NewVarSet).

%-----------------------------------------------------------------------------%

varset__coerce(A, B) :-
	% Normally calls to this predicate should only be
	% generated by the compiler, but type coercion by 
	% copying was taking about 3% of the compiler's runtime.
	private_builtin__unsafe_type_cast(A, B).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%	Function forms added.

varset__init = VS :-
	varset__init(VS).

varset__delete_var(VS1, V) = VS2 :-
	varset__delete_var(VS1, V, VS2).

varset__delete_vars(VS1, Vs) = VS2 :-
	varset__delete_vars(VS1, Vs, VS2).

varset__vars(VS) = Vs :-
	varset__vars(VS, Vs).

varset__name_var(VS1, V, S) = VS2 :-
	varset__name_var(VS1, V, S, VS2).

varset__lookup_name(VS, V) = S :-
	varset__lookup_name(VS, V, S).

varset__lookup_name(VS1, V, S) = S2 :-
	varset__lookup_name(VS1, V, S, S2).

varset__bind_var(VS1, V, T) = VS2 :-
	varset__bind_var(VS1, V, T, VS2).

varset__bind_vars(VS1, S) = VS2 :-
	varset__bind_vars(VS1, S, VS2).

varset__lookup_vars(VS) = S :-
	varset__lookup_vars(VS, S).

varset__get_bindings(VS) = S :-
	varset__get_bindings(VS, S).

varset__set_bindings(VS1, S) = VS2 :-
	varset__set_bindings(VS1, S, VS2).

varset__create_name_var_map(VS) = M :-
	varset__create_name_var_map(VS, M).

varset__var_name_list(VS) = AL :-
	varset__var_name_list(VS, AL).

varset__ensure_unique_names(Vs, S1, VS1) = VS2 :-
	varset__ensure_unique_names(Vs, S1, VS1, VS2).

varset__select(VS1, S) = VS2 :-
	varset__select(VS1, S, VS2).

varset__coerce(VS1) = VS2 :-
	varset__coerce(VS1, VS2).

