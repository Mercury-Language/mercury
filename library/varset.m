%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2002-2005 The University of Melbourne.
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

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module term.

:- type varset(T).

:- type varset  ==  varset(generic).

    % Construct an empty varset.
    %
:- func varset__init = varset(T).
:- pred varset__init(varset(T)::out) is det.

    % Check whether a varset is empty.
    %
:- pred varset__is_empty(varset(T)::in) is semidet.

    % Create a new variable.
    %
:- pred varset__new_var(varset(T)::in, var(T)::out, varset(T)::out) is det.

    % Create a new named variable.
    %
:- pred varset__new_named_var(varset(T)::in, string::in, var(T)::out,
    varset(T)::out) is det.

    % Create a new named variable with a unique (w.r.t. the
    % varset) number appended to the name.
    %
:- pred varset__new_uniquely_named_var(varset(T)::in, string::in, var(T)::out,
    varset(T)::out) is det.

    % Create a new variable, and maybe give it a name.
    %
:- pred varset__new_maybe_named_var(varset(T)::in, maybe(string)::in,
    var(T)::out, varset(T)::out) is det.

    % Create multiple new variables.
    %
:- pred varset__new_vars(varset(T)::in, int::in, list(var(T))::out,
    varset(T)::out) is det.

    % Delete the name and value for a variable.
    %
:- func varset__delete_var(varset(T), var(T)) = varset(T).
:- pred varset__delete_var(varset(T)::in, var(T)::in, varset(T)::out) is det.

    % Delete the names and values for a list of variables.
    %
:- func varset__delete_vars(varset(T), list(var(T))) = varset(T).
:- pred varset__delete_vars(varset(T)::in, list(var(T))::in, varset(T)::out)
    is det.

    % Return a list of all the variables in a varset.
    %
:- func varset__vars(varset(T)) = list(var(T)).
:- pred varset__vars(varset(T)::in, list(var(T))::out) is det.

    % Set the name of a variable.
    %
:- func varset__name_var(varset(T), var(T), string) = varset(T).
:- pred varset__name_var(varset(T)::in, var(T)::in, string::in, varset(T)::out)
    is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using V_ as a prefix.
    %
:- func varset__lookup_name(varset(T), var(T)) = string.
:- pred varset__lookup_name(varset(T)::in, var(T)::in, string::out) is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using the specified prefix
    %
:- func varset__lookup_name(varset(T), var(T), string) = string.
:- pred varset__lookup_name(varset(T)::in, var(T)::in, string::in, string::out)
    is det.

    % Lookup the name of a variable;
    % fail if it doesn't have one
    %
:- pred varset__search_name(varset(T)::in, var(T)::in, string::out) is semidet.

    % Bind a value to a variable.
    % This will overwrite any existing binding.
    %
:- func varset__bind_var(varset(T), var(T), term(T)) = varset(T).
:- pred varset__bind_var(varset(T)::in, var(T)::in, term(T)::in,
    varset(T)::out) is det.

    % Bind a set of terms to a set of variables.
    %
:- func varset__bind_vars(varset(T), substitution(T)) = varset(T).
:- pred varset__bind_vars(varset(T)::in, substitution(T)::in, varset(T)::out)
    is det.

    % Lookup the value of a variable.
    %
:- pred varset__search_var(varset(T)::in, var(T)::in, term(T)::out) is semidet.

    % Get the bindings for all the bound variables.
    %
:- func varset__lookup_vars(varset(T)) = substitution(T).
:- pred varset__lookup_vars(varset(T)::in, substitution(T)::out) is det.

    % Combine two different varsets, renaming apart:
    % varset__merge(VarSet0, NewVarSet, VarSet, Subst) is
    % true iff VarSet is the varset that results from joining
    % a suitably renamed version of NewVarSet to VarSet0.
    % (Any bindings in NewVarSet are ignored.)
    % Subst is a substitution which maps the variables in NewVarSet
    % into the corresponding fresh variable in VarSet.
    %
:- pred varset__merge_subst(varset(T)::in, varset(T)::in, varset(T)::out,
    substitution(T)::out) is det.

    % varset__merge(VarSet0, NewVarSet, Terms0, VarSet, Terms):
    % As varset__merge_subst, except instead of returning the substitution,
    % this predicate applies it to the given list of terms.
    %
:- pred varset__merge(varset(T)::in, varset(T)::in, list(term(T))::in,
    varset(T)::out, list(term(T))::out) is det.

    % Same as varset__merge_subst, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if varset__create_name_var_map needs
    % to be used on the resulting varset.
    %
:- pred varset__merge_subst_without_names(varset(T)::in,
    varset(T)::in, varset(T)::out, substitution(T)::out) is det.

    % Same as varset__merge, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if varset__create_name_var_map needs
    % to be used on the resulting varset.
    %
:- pred varset__merge_without_names(varset(T)::in, varset(T)::in,
    list(term(T))::in, varset(T)::out, list(term(T))::out) is det.

    % Get the bindings for all the bound variables.
    %
:- func varset__get_bindings(varset(T)) = substitution(T).
:- pred varset__get_bindings(varset(T)::in, substitution(T)::out) is det.

    % Set the bindings for all the bound variables.
    %
:- func varset__set_bindings(varset(T), substitution(T)) = varset(T).
:- pred varset__set_bindings(varset(T)::in, substitution(T)::in,
    varset(T)::out) is det.

    % Create a map from names to variables.
    % Each name is mapped to only one variable, even if a name is
    % shared by more than one variable. Therefore this predicate
    % is only really useful if it is already known that no two
    % variables share the same name.
    %
:- func varset__create_name_var_map(varset(T)) = map(string, var(T)).
:- pred varset__create_name_var_map(varset(T)::in, map(string, var(T))::out)
    is det.

    % Return an association list giving the name of each variable.
    % Every variable has an entry in the returned association list,
    % even if it shares its name with another variable.
    %
:- func varset__var_name_list(varset(T)) = assoc_list(var(T), string).
:- pred varset__var_name_list(varset(T)::in, assoc_list(var(T), string)::out)
    is det.

    % Given a list of variable and varset in which some variables have
    % no name but some other variables may have the same name,
    % return another varset in which every variable has a unique name.
    % If necessary, names will have suffixes added on the end;
    % the second argument gives the suffix to use.
    %
:- func varset__ensure_unique_names(list(var(T)), string, varset(T))
    = varset(T).
:- pred varset__ensure_unique_names(list(var(T))::in,
    string::in, varset(T)::in, varset(T)::out) is det.

    % Given a varset and a set of variables, remove the names
    % and values of any other variables stored in the varset.
    %
:- func varset__select(varset(T), set(var(T))) = varset(T).
:- pred varset__select(varset(T)::in, set(var(T))::in, varset(T)::out) is det.

    % Given a varset and a list of variables, construct a new varset
    % containing one variable for each one in the list (and no others).
    % Also return a substitution mapping the selected variables in the
    % original varset into variables in the new varset. The relative
    % ordering of variables in the original varset is maintained.
    %
:- pred varset__squash(varset(T)::in, list(var(T))::in,
    varset(T)::out, map(var(T), var(T))::out) is det.

    % Coerce the types of the variables in a varset.
    %
:- func varset__coerce(varset(T)) = varset(U).
:- pred varset__coerce(varset(T)::in, varset(U)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Returns the highest numbered variable returned from this varset's
    % var_supply.
    %
:- func varset__max_var(varset(T)) = var(T).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

:- type varset(T)
    --->    varset(
                var_supply  :: var_supply(T),
                var_names   :: map(var(T), string),
                var_values  :: map(var(T), term(T))
            ).

%-----------------------------------------------------------------------------%

varset__init(varset(VarSupply, Names, Values)) :-
    term__init_var_supply(VarSupply),
    map__init(Names),
    map__init(Values).

%-----------------------------------------------------------------------------%

varset__is_empty(varset(VarSupply, _, _)) :-
    term__init_var_supply(VarSupply).

%-----------------------------------------------------------------------------%

varset__new_var(VarSet0, Var, VarSet) :-
    MaxId0 = VarSet0 ^ var_supply,
    term__create_var(MaxId0, Var, MaxId),
    VarSet = VarSet0 ^ var_supply := MaxId.

varset__new_named_var(varset(MaxId0, Names0, Values), Name, Var,
        varset(MaxId, Names, Values)) :-
    term__create_var(MaxId0, Var, MaxId),
    map__set(Names0, Var, Name, Names).

varset__new_uniquely_named_var(varset(MaxId0, Names0, Values), Name, Var,
        varset(MaxId, Names, Values)) :-
    term__create_var(MaxId0, Var, MaxId),
    N = term__var_id(Var),
    map__set(Names0, Var, string__format("%s_%d", [s(Name), i(N)]), Names).

varset__new_maybe_named_var(varset(MaxId0, Names0, Values), MaybeName, Var,
        varset(MaxId, Names, Values)) :-
    term__create_var(MaxId0, Var, MaxId),
    (
        MaybeName = no,
        Names = Names0
    ;
        MaybeName = yes(Name),
        map__set(Names0, Var, Name, Names)
    ).

varset__new_vars(VarSet0, NumVars, NewVars, VarSet) :-
    varset__new_vars_2(VarSet0, NumVars, [], NewVars, VarSet).

:- pred varset__new_vars_2(varset(T)::in, int::in, list(var(T))::in,
    list(var(T))::out, varset(T)::out) is det.

varset__new_vars_2(VarSet0, NumVars, NewVars0, NewVars, VarSet) :-
    ( NumVars > 0 ->
        NumVars1 = NumVars - 1,
        varset__new_var(VarSet0, Var, VarSet1),
        varset__new_vars_2(VarSet1, NumVars1, [Var | NewVars0],
            NewVars, VarSet)
    ; NumVars = 0 ->
        NewVars = NewVars0,
        VarSet = VarSet0
    ;
        error("varset__new_vars - invalid call")
    ).

%-----------------------------------------------------------------------------%

varset__delete_var(varset(MaxId, Names0, Values0), Var,
        varset(MaxId, Names, Values)) :-
    map__delete(Names0, Var, Names),
    map__delete(Values0, Var, Values).

%-----------------------------------------------------------------------------%

varset__delete_vars(VarSet, [], VarSet).
varset__delete_vars(VarSet0, [Var | Vars], VarSet) :-
    varset__delete_var(VarSet0, Var, VarSet1),
    varset__delete_vars(VarSet1, Vars, VarSet).

%-----------------------------------------------------------------------------%

varset__vars(VarSet0, Vars) :-
    MaxId0 = VarSet0 ^ var_supply,
    term__init_var_supply(N0),
    RevVars = varset__vars_2(N0, MaxId0, []),
    list__reverse(RevVars, Vars).

:- func varset__vars_2(var_supply(T), var_supply(T), list(var(T)))
    = list(var(T)).

varset__vars_2(N, Max, RevVars0) = RevVars :-
    ( N = Max ->
        RevVars = RevVars0
    ;
        term__create_var(N, Var, N1),
        RevVars = varset__vars_2(N1, Max, [Var | RevVars0])
    ).

%-----------------------------------------------------------------------------%

varset__name_var(VarSet0, Id, Name, VarSet) :-
    Names0 = VarSet0 ^ var_names,
    map__set(Names0, Id, Name, Names),
    VarSet = VarSet0 ^ var_names := Names.

%-----------------------------------------------------------------------------%

varset__lookup_name(VarSet, Id, Name) :-
    ( varset__search_name(VarSet, Id, Name0) ->
        Name = Name0
    ;
        term__var_to_int(Id, VarNum),
        string__int_to_string(VarNum, NumStr),
        string__append("V_", NumStr, Name)
    ).

varset__lookup_name(VarSet, Id, Prefix, Name) :-
    ( varset__search_name(VarSet, Id, Name0) ->
        Name = Name0
    ;
        term__var_to_int(Id, VarNum),
        string__int_to_string(VarNum, NumStr),
        string__append(Prefix, NumStr, Name)
    ).

varset__search_name(varset(_, Names, _), Id, Name) :-
    map__search(Names, Id, Name0),
    Name = Name0.
% This part is useful during debugging when you need to
% be able to distinguish different variables with the same name.
%   (
%       map__member(Names, Id1, Name0),
%       Id1 \= Id
%   ->
%       term__var_to_int(Id, Int),
%       string__format("%s__%d",[s(Name0),i(Int)], Name)
%   ;
%       Name = Name0
%   ).

%-----------------------------------------------------------------------------%

varset__bind_var(VarSet0, Id, Val, VarSet) :-
    Values0 = VarSet0 ^ var_values,
    map__set(Values0, Id, Val, Values),
    VarSet = VarSet0 ^ var_values := Values.

%-----------------------------------------------------------------------------%

varset__bind_vars(VarSet0, Subst, VarSet) :-
    map__to_assoc_list(Subst, VarTermList),
    varset__bind_vars_2(VarTermList, VarSet0, VarSet).

:- pred varset__bind_vars_2(assoc_list(var(T), term(T))::in, varset(T)::in,
    varset(T)::out) is det.

varset__bind_vars_2([], VarSet, VarSet).
varset__bind_vars_2([V - T | Rest], VarSet0, VarSet) :-
    varset__bind_var(VarSet0, V, T, VarSet1),
    varset__bind_vars_2(Rest, VarSet1, VarSet).

%-----------------------------------------------------------------------------%

varset__search_var(VarSet, Id, Val) :-
    Values = VarSet ^ var_values,
    map__search(Values, Id, Val).

%-----------------------------------------------------------------------------%

varset__lookup_vars(VarSet, VarSet ^ var_values).

%-----------------------------------------------------------------------------%

varset__get_bindings(VarSet, VarSet ^ var_values).

varset__set_bindings(VarSet, Values, VarSet ^ var_values := Values).

%-----------------------------------------------------------------------------%

    % We scan through the second varset, introducing a fresh
    % variable into the first varset for each var in the
    % second, and building up a substitution which maps
    % the variables in the second varset into the corresponding
    % fresh variable in the first varset.  We then apply
    % this substitution to the list of terms.

varset__merge(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    IncludeNames = yes,
    varset__merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst),
    term__apply_substitution_to_list(TermList0, Subst, TermList).

varset__merge_without_names(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    IncludeNames = no,
    varset__merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst),
    term__apply_substitution_to_list(TermList0, Subst, TermList).

varset__merge_subst(VarSetA, VarSetB, VarSet, Subst) :-
    IncludeNames = yes,
    varset__merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst).

varset__merge_subst_without_names(VarSetA, VarSetB, VarSet, Subst) :-
    IncludeNames = no,
    varset__merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst).

:- pred varset__merge_subst(bool::in, varset(T)::in, varset(T)::in,
    varset(T)::out, substitution(T)::out) is det.

varset__merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst) :-
    VarSetB = varset(MaxId, Names, Values),
    term__init_var_supply(N),
    map__init(Subst0),
    varset__merge_subst_2(IncludeNames, N, MaxId, Names, Values,
        VarSetA, VarSet, Subst0, Subst).

:- pred varset__merge_subst_2(bool::in, var_supply(T)::in,
    var_supply(T)::in, map(var(T), string)::in,
    map(var(T), term(T))::in, varset(T)::in, varset(T)::out,
    substitution(T)::in, substitution(T)::out) is det.

varset__merge_subst_2(IncludeNames, N, Max, Names, Values, !VarSet, !Subst) :-
    ( N = Max ->
        true
    ;
        varset__new_var(!.VarSet, VarId, !:VarSet),
        term__create_var(N, VarN, N1),
        (
            IncludeNames = yes,
            map__search(Names, VarN, Name)
        ->
            varset__name_var(!.VarSet, VarId, Name, !:VarSet)
        ;
            true
        ),
        map__set(!.Subst, VarN, term__variable(VarId), !:Subst),
        varset__merge_subst_2(IncludeNames, N1, Max, Names, Values,
            !VarSet, !Subst)
    ).

%-----------------------------------------------------------------------------%

varset__create_name_var_map(VarSet, NameVars) :-
    VarNames = VarSet ^ var_names,
    map__keys(VarNames, Vars),
    map__values(VarNames, Names),
    map__from_corresponding_lists(Names, Vars, NameVars).

%-----------------------------------------------------------------------------%

varset__var_name_list(VarSet, VarNameList) :-
    VarNames = VarSet ^ var_names,
    map__to_assoc_list(VarNames, VarNameList).

%-----------------------------------------------------------------------------%

varset__ensure_unique_names(AllVars, Suffix, VarSet0, VarSet) :-
    VarNames0 = VarSet0 ^ var_names,
    varset__ensure_unique_names_2(AllVars, Suffix, set__init, VarNames0,
        map__init, VarNames),
    VarSet = VarSet0 ^ var_names := VarNames.

:- pred varset__ensure_unique_names_2(list(var(T))::in, string::in,
    set(string)::in, map(var(T), string)::in, map(var(T), string)::in,
    map(var(T), string)::out) is det.

varset__ensure_unique_names_2([], _, _, _, VarNames, VarNames).
varset__ensure_unique_names_2([Var | Vars], Suffix, UsedNames0, OldVarNames,
        VarNames0, VarNames) :-
    ( map__search(OldVarNames, Var, OldName) ->
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
    append_suffix_until_unique(TrialName, Suffix, UsedNames0, FinalName),
    set__insert(UsedNames0, FinalName, UsedNames1),
    map__det_insert(VarNames0, Var, FinalName, VarNames1),
    varset__ensure_unique_names_2(Vars, Suffix, UsedNames1, OldVarNames,
        VarNames1, VarNames).

:- pred append_suffix_until_unique(string::in, string::in, set(string)::in,
    string::out) is det.

append_suffix_until_unique(Trial0, Suffix, UsedNames, Final) :-
    ( set__member(Trial0, UsedNames) ->
        string__append(Trial0, Suffix, Trial1),
        append_suffix_until_unique(Trial1, Suffix, UsedNames, Final)
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
    % Create a new varset with the same number of variables.
    list__length(KeptVars, NumVars),
    varset__init(NewVarSet0),
    varset__new_vars(NewVarSet0, NumVars,
        NewVars0, NewVarSet1),

    % We need to sort the fresh variables, to ensure that the substitution
    % that we create below does not alter the relative ordering of the
    % variables.
    list__sort(NewVars0, NewVars),

    % Copy the variable names across from the old varset to the new varset.
    varset__var_name_list(OldVarSet, VarNames),
    map__from_corresponding_lists(KeptVars, NewVars, Subst),
    copy_var_names(VarNames, Subst, NewVarSet1, NewVarSet).

:- pred copy_var_names(assoc_list(var(T), string)::in, map(var(T), var(T))::in,
    varset(T)::in, varset(T)::out) is det.

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
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin__unsafe_type_cast(A, B).

%-----------------------------------------------------------------------------%

varset__max_var(varset(VarSupply, _, _)) = term__var_supply_max_var(VarSupply).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

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
