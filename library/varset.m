%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: varset.m.
% Main author: fjh.
% Stability: low.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module varset.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type varset(T).

:- type varset  ==  varset(generic).

    % Construct an empty varset.
    %
:- func varset.init = varset(T).
:- pred varset.init(varset(T)::out) is det.

    % Check whether a varset is empty.
    %
:- pred varset.is_empty(varset(T)::in) is semidet.

    % Create a new variable.
    %
:- pred varset.new_var(varset(T)::in, var(T)::out, varset(T)::out) is det.

    % Create a new named variable.
    %
:- pred varset.new_named_var(varset(T)::in, string::in, var(T)::out,
    varset(T)::out) is det.

    % Create a new named variable with a unique (w.r.t. the
    % varset) number appended to the name.
    %
:- pred varset.new_uniquely_named_var(varset(T)::in, string::in, var(T)::out,
    varset(T)::out) is det.

    % Create a new variable, and maybe give it a name.
    %
:- pred varset.new_maybe_named_var(varset(T)::in, maybe(string)::in,
    var(T)::out, varset(T)::out) is det.

    % Create multiple new variables.
    %
:- pred varset.new_vars(varset(T)::in, int::in, list(var(T))::out,
    varset(T)::out) is det.

    % Delete the name and value for a variable.
    %
:- func varset.delete_var(varset(T), var(T)) = varset(T).
:- pred varset.delete_var(varset(T)::in, var(T)::in, varset(T)::out) is det.

    % Delete the names and values for a list of variables.
    %
:- func varset.delete_vars(varset(T), list(var(T))) = varset(T).
:- pred varset.delete_vars(varset(T)::in, list(var(T))::in, varset(T)::out)
    is det.

    % Return a list of all the variables in a varset.
    %
:- func varset.vars(varset(T)) = list(var(T)).
:- pred varset.vars(varset(T)::in, list(var(T))::out) is det.

    % Set the name of a variable.
    %
:- func varset.name_var(varset(T), var(T), string) = varset(T).
:- pred varset.name_var(varset(T)::in, var(T)::in, string::in, varset(T)::out)
    is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using V_ as a prefix.
    %
:- func varset.lookup_name(varset(T), var(T)) = string.
:- pred varset.lookup_name(varset(T)::in, var(T)::in, string::out) is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using the specified prefix
    %
:- func varset.lookup_name(varset(T), var(T), string) = string.
:- pred varset.lookup_name(varset(T)::in, var(T)::in, string::in, string::out)
    is det.

    % Lookup the name of a variable;
    % fail if it doesn't have one
    %
:- pred varset.search_name(varset(T)::in, var(T)::in, string::out) is semidet.

    % Bind a value to a variable.
    % This will overwrite any existing binding.
    %
:- func varset.bind_var(varset(T), var(T), term(T)) = varset(T).
:- pred varset.bind_var(varset(T)::in, var(T)::in, term(T)::in,
    varset(T)::out) is det.

    % Bind a set of terms to a set of variables.
    %
:- func varset.bind_vars(varset(T), substitution(T)) = varset(T).
:- pred varset.bind_vars(varset(T)::in, substitution(T)::in, varset(T)::out)
    is det.

    % Lookup the value of a variable.
    %
:- pred varset.search_var(varset(T)::in, var(T)::in, term(T)::out) is semidet.

    % Get the bindings for all the bound variables.
    %
:- func varset.lookup_vars(varset(T)) = substitution(T).
:- pred varset.lookup_vars(varset(T)::in, substitution(T)::out) is det.

    % Combine two different varsets, renaming apart:
    % varset.merge(VarSet0, NewVarSet, VarSet, Subst) is
    % true iff VarSet is the varset that results from joining
    % a suitably renamed version of NewVarSet to VarSet0.
    % (Any bindings in NewVarSet are ignored.)
    % Subst is a substitution which maps the variables in NewVarSet
    % into the corresponding fresh variable in VarSet.
    %
:- pred varset.merge_subst(varset(T)::in, varset(T)::in, varset(T)::out,
    substitution(T)::out) is det.

    % varset.merge(VarSet0, NewVarSet, Terms0, VarSet, Terms):
    % As varset.merge_subst, except instead of returning the substitution,
    % this predicate applies it to the given list of terms.
    %
:- pred varset.merge(varset(T)::in, varset(T)::in, list(term(T))::in,
    varset(T)::out, list(term(T))::out) is det.

    % Same as varset.merge_subst, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if varset.create_name_var_map needs
    % to be used on the resulting varset.
    %
:- pred varset.merge_subst_without_names(varset(T)::in,
    varset(T)::in, varset(T)::out, substitution(T)::out) is det.

    % Same as varset.merge, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if varset.create_name_var_map needs
    % to be used on the resulting varset.
    %
:- pred varset.merge_without_names(varset(T)::in, varset(T)::in,
    list(term(T))::in, varset(T)::out, list(term(T))::out) is det.

    % Get the bindings for all the bound variables.
    %
:- func varset.get_bindings(varset(T)) = substitution(T).
:- pred varset.get_bindings(varset(T)::in, substitution(T)::out) is det.

    % Set the bindings for all the bound variables.
    %
:- func varset.set_bindings(varset(T), substitution(T)) = varset(T).
:- pred varset.set_bindings(varset(T)::in, substitution(T)::in,
    varset(T)::out) is det.

    % Create a map from names to variables.
    % Each name is mapped to only one variable, even if a name is
    % shared by more than one variable. Therefore this predicate
    % is only really useful if it is already known that no two
    % variables share the same name.
    %
:- func varset.create_name_var_map(varset(T)) = map(string, var(T)).
:- pred varset.create_name_var_map(varset(T)::in, map(string, var(T))::out)
    is det.

    % Return an association list giving the name of each variable.
    % Every variable has an entry in the returned association list,
    % even if it shares its name with another variable.
    %
:- func varset.var_name_list(varset(T)) = assoc_list(var(T), string).
:- pred varset.var_name_list(varset(T)::in, assoc_list(var(T), string)::out)
    is det.

    % Given a list of variable and varset in which some variables have
    % no name but some other variables may have the same name,
    % return another varset in which every variable has a unique name.
    % If necessary, names will have suffixes added on the end;
    % the second argument gives the suffix to use.
    %
:- func varset.ensure_unique_names(list(var(T)), string, varset(T))
    = varset(T).
:- pred varset.ensure_unique_names(list(var(T))::in,
    string::in, varset(T)::in, varset(T)::out) is det.

    % Given a varset and a set of variables, remove the names
    % and values of any other variables stored in the varset.
    %
:- func varset.select(varset(T), set(var(T))) = varset(T).
:- pred varset.select(varset(T)::in, set(var(T))::in, varset(T)::out) is det.

    % Given a varset and a list of variables, construct a new varset
    % containing one variable for each one in the list (and no others).
    % Also return a substitution mapping the selected variables in the
    % original varset into variables in the new varset. The relative
    % ordering of variables in the original varset is maintained.
    %
:- pred varset.squash(varset(T)::in, list(var(T))::in,
    varset(T)::out, map(var(T), var(T))::out) is det.

    % Coerce the types of the variables in a varset.
    %
:- func varset.coerce(varset(T)) = varset(U).
:- pred varset.coerce(varset(T)::in, varset(U)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Returns the highest numbered variable returned from this varset's
    % var_supply.
    %
:- func varset.max_var(varset(T)) = var(T).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
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

varset.init(varset(VarSupply, Names, Values)) :-
    term.init_var_supply(VarSupply),
    map.init(Names),
    map.init(Values).

%-----------------------------------------------------------------------------%

varset.is_empty(varset(VarSupply, _, _)) :-
    term.init_var_supply(VarSupply).

%-----------------------------------------------------------------------------%

varset.new_var(VarSet0, Var, VarSet) :-
    MaxId0 = VarSet0 ^ var_supply,
    term.create_var(MaxId0, Var, MaxId),
    VarSet = VarSet0 ^ var_supply := MaxId.

varset.new_named_var(varset(MaxId0, Names0, Values), Name, Var,
        varset(MaxId, Names, Values)) :-
    term.create_var(MaxId0, Var, MaxId),
    map.set(Names0, Var, Name, Names).

varset.new_uniquely_named_var(varset(MaxId0, Names0, Values), Name, Var,
        varset(MaxId, Names, Values)) :-
    term.create_var(MaxId0, Var, MaxId),
    N = term.var_id(Var),
    map.set(Names0, Var, string.format("%s_%d", [s(Name), i(N)]), Names).

varset.new_maybe_named_var(varset(MaxId0, Names0, Values), MaybeName, Var,
        varset(MaxId, Names, Values)) :-
    term.create_var(MaxId0, Var, MaxId),
    (
        MaybeName = no,
        Names = Names0
    ;
        MaybeName = yes(Name),
        map.set(Names0, Var, Name, Names)
    ).

varset.new_vars(VarSet0, NumVars, NewVars, VarSet) :-
    varset.new_vars_2(VarSet0, NumVars, [], NewVars, VarSet).

:- pred varset.new_vars_2(varset(T)::in, int::in, list(var(T))::in,
    list(var(T))::out, varset(T)::out) is det.

varset.new_vars_2(VarSet0, NumVars, NewVars0, NewVars, VarSet) :-
    ( NumVars > 0 ->
        NumVars1 = NumVars - 1,
        varset.new_var(VarSet0, Var, VarSet1),
        varset.new_vars_2(VarSet1, NumVars1, [Var | NewVars0],
            NewVars, VarSet)
    ; NumVars = 0 ->
        NewVars = NewVars0,
        VarSet = VarSet0
    ;
        error("varset.new_vars - invalid call")
    ).

%-----------------------------------------------------------------------------%

varset.delete_var(varset(MaxId, Names0, Values0), Var,
        varset(MaxId, Names, Values)) :-
    map.delete(Names0, Var, Names),
    map.delete(Values0, Var, Values).

%-----------------------------------------------------------------------------%

varset.delete_vars(VarSet, [], VarSet).
varset.delete_vars(VarSet0, [Var | Vars], VarSet) :-
    varset.delete_var(VarSet0, Var, VarSet1),
    varset.delete_vars(VarSet1, Vars, VarSet).

%-----------------------------------------------------------------------------%

varset.vars(VarSet0, Vars) :-
    MaxId0 = VarSet0 ^ var_supply,
    term.init_var_supply(N0),
    RevVars = varset.vars_2(N0, MaxId0, []),
    list.reverse(RevVars, Vars).

:- func varset.vars_2(var_supply(T), var_supply(T), list(var(T)))
    = list(var(T)).

varset.vars_2(N, Max, RevVars0) = RevVars :-
    ( N = Max ->
        RevVars = RevVars0
    ;
        term.create_var(N, Var, N1),
        RevVars = varset.vars_2(N1, Max, [Var | RevVars0])
    ).

%-----------------------------------------------------------------------------%

varset.name_var(VarSet0, Id, Name, VarSet) :-
    Names0 = VarSet0 ^ var_names,
    map.set(Names0, Id, Name, Names),
    VarSet = VarSet0 ^ var_names := Names.

%-----------------------------------------------------------------------------%

varset.lookup_name(VarSet, Id, Name) :-
    ( varset.search_name(VarSet, Id, Name0) ->
        Name = Name0
    ;
        term.var_to_int(Id, VarNum),
        string.int_to_string(VarNum, NumStr),
        string.append("V_", NumStr, Name)
    ).

varset.lookup_name(VarSet, Id, Prefix, Name) :-
    ( varset.search_name(VarSet, Id, Name0) ->
        Name = Name0
    ;
        term.var_to_int(Id, VarNum),
        string.int_to_string(VarNum, NumStr),
        string.append(Prefix, NumStr, Name)
    ).

varset.search_name(varset(_, Names, _), Id, Name) :-
    map.search(Names, Id, Name0),
    Name = Name0.
% This part is useful during debugging when you need to
% be able to distinguish different variables with the same name.
%   (
%       map.member(Names, Id1, Name0),
%       Id1 \= Id
%   ->
%       term.var_to_int(Id, Int),
%       string.format("%s.%d",[s(Name0),i(Int)], Name)
%   ;
%       Name = Name0
%   ).

%-----------------------------------------------------------------------------%

varset.bind_var(VarSet0, Id, Val, VarSet) :-
    Values0 = VarSet0 ^ var_values,
    map.set(Values0, Id, Val, Values),
    VarSet = VarSet0 ^ var_values := Values.

%-----------------------------------------------------------------------------%

varset.bind_vars(VarSet0, Subst, VarSet) :-
    map.to_assoc_list(Subst, VarTermList),
    varset.bind_vars_2(VarTermList, VarSet0, VarSet).

:- pred varset.bind_vars_2(assoc_list(var(T), term(T))::in, varset(T)::in,
    varset(T)::out) is det.

varset.bind_vars_2([], VarSet, VarSet).
varset.bind_vars_2([V - T | Rest], VarSet0, VarSet) :-
    varset.bind_var(VarSet0, V, T, VarSet1),
    varset.bind_vars_2(Rest, VarSet1, VarSet).

%-----------------------------------------------------------------------------%

varset.search_var(VarSet, Id, Val) :-
    Values = VarSet ^ var_values,
    map.search(Values, Id, Val).

%-----------------------------------------------------------------------------%

varset.lookup_vars(VarSet, VarSet ^ var_values).

%-----------------------------------------------------------------------------%

varset.get_bindings(VarSet, VarSet ^ var_values).

varset.set_bindings(VarSet, Values, VarSet ^ var_values := Values).

%-----------------------------------------------------------------------------%

    % We scan through the second varset, introducing a fresh
    % variable into the first varset for each var in the
    % second, and building up a substitution which maps
    % the variables in the second varset into the corresponding
    % fresh variable in the first varset.  We then apply
    % this substitution to the list of terms.

varset.merge(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    IncludeNames = yes,
    varset.merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst),
    term.apply_substitution_to_list(TermList0, Subst, TermList).

varset.merge_without_names(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    IncludeNames = no,
    varset.merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst),
    term.apply_substitution_to_list(TermList0, Subst, TermList).

varset.merge_subst(VarSetA, VarSetB, VarSet, Subst) :-
    IncludeNames = yes,
    varset.merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst).

varset.merge_subst_without_names(VarSetA, VarSetB, VarSet, Subst) :-
    IncludeNames = no,
    varset.merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst).

:- pred varset.merge_subst(bool::in, varset(T)::in, varset(T)::in,
    varset(T)::out, substitution(T)::out) is det.

varset.merge_subst(IncludeNames, VarSetA, VarSetB, VarSet, Subst) :-
    VarSetB = varset(MaxId, Names, Values),
    term.init_var_supply(N),
    map.init(Subst0),
    varset.merge_subst_2(IncludeNames, N, MaxId, Names, Values,
        VarSetA, VarSet, Subst0, Subst).

:- pred varset.merge_subst_2(bool::in, var_supply(T)::in,
    var_supply(T)::in, map(var(T), string)::in,
    map(var(T), term(T))::in, varset(T)::in, varset(T)::out,
    substitution(T)::in, substitution(T)::out) is det.

varset.merge_subst_2(IncludeNames, N, Max, Names, Values, !VarSet, !Subst) :-
    ( N = Max ->
        true
    ;
        varset.new_var(!.VarSet, VarId, !:VarSet),
        term.create_var(N, VarN, N1),
        (
            IncludeNames = yes,
            map.search(Names, VarN, Name)
        ->
            varset.name_var(!.VarSet, VarId, Name, !:VarSet)
        ;
            true
        ),
        map.set(!.Subst, VarN, term.variable(VarId), !:Subst),
        varset.merge_subst_2(IncludeNames, N1, Max, Names, Values,
            !VarSet, !Subst)
    ).

%-----------------------------------------------------------------------------%

varset.create_name_var_map(VarSet, NameVars) :-
    VarNames = VarSet ^ var_names,
    map.keys(VarNames, Vars),
    map.values(VarNames, Names),
    map.from_corresponding_lists(Names, Vars, NameVars).

%-----------------------------------------------------------------------------%

varset.var_name_list(VarSet, VarNameList) :-
    VarNames = VarSet ^ var_names,
    map.to_assoc_list(VarNames, VarNameList).

%-----------------------------------------------------------------------------%

varset.ensure_unique_names(AllVars, Suffix, VarSet0, VarSet) :-
    VarNames0 = VarSet0 ^ var_names,
    varset.ensure_unique_names_2(AllVars, Suffix, set.init, VarNames0,
        map.init, VarNames),
    VarSet = VarSet0 ^ var_names := VarNames.

:- pred varset.ensure_unique_names_2(list(var(T))::in, string::in,
    set(string)::in, map(var(T), string)::in, map(var(T), string)::in,
    map(var(T), string)::out) is det.

varset.ensure_unique_names_2([], _, _, _, VarNames, VarNames).
varset.ensure_unique_names_2([Var | Vars], Suffix, UsedNames0, OldVarNames,
        VarNames0, VarNames) :-
    ( map.search(OldVarNames, Var, OldName) ->
        ( set.member(OldName, UsedNames0) ->
            term.var_to_int(Var, VarNum),
            string.int_to_string(VarNum, NumStr),
            string.append("_", NumStr, NumSuffix),
            string.append(OldName, NumSuffix, TrialName)
        ;
            TrialName = OldName
        )
    ;
        term.var_to_int(Var, VarNum),
        string.int_to_string(VarNum, NumStr),
        string.append("Var_", NumStr, TrialName)
    ),
    append_suffix_until_unique(TrialName, Suffix, UsedNames0, FinalName),
    set.insert(UsedNames0, FinalName, UsedNames1),
    map.det_insert(VarNames0, Var, FinalName, VarNames1),
    varset.ensure_unique_names_2(Vars, Suffix, UsedNames1, OldVarNames,
        VarNames1, VarNames).

:- pred append_suffix_until_unique(string::in, string::in, set(string)::in,
    string::out) is det.

append_suffix_until_unique(Trial0, Suffix, UsedNames, Final) :-
    ( set.member(Trial0, UsedNames) ->
        string.append(Trial0, Suffix, Trial1),
        append_suffix_until_unique(Trial1, Suffix, UsedNames, Final)
    ;
        Final = Trial0
    ).

%-----------------------------------------------------------------------------%

varset.select(varset(Supply, VarNameMap0, Values0), Vars,
        varset(Supply, VarNameMap, Values)) :-
    map.select(VarNameMap0, Vars, VarNameMap),
    map.select(Values0, Vars, Values).

%-----------------------------------------------------------------------------%

varset.squash(OldVarSet, KeptVars, NewVarSet, Subst) :-
    % Create a new varset with the same number of variables.
    list.length(KeptVars, NumVars),
    varset.init(NewVarSet0),
    varset.new_vars(NewVarSet0, NumVars,
        NewVars0, NewVarSet1),

    % We need to sort the fresh variables, to ensure that the substitution
    % that we create below does not alter the relative ordering of the
    % variables.
    list.sort(NewVars0, NewVars),

    % Copy the variable names across from the old varset to the new varset.
    varset.var_name_list(OldVarSet, VarNames),
    map.from_corresponding_lists(KeptVars, NewVars, Subst),
    copy_var_names(VarNames, Subst, NewVarSet1, NewVarSet).

:- pred copy_var_names(assoc_list(var(T), string)::in, map(var(T), var(T))::in,
    varset(T)::in, varset(T)::out) is det.

copy_var_names([], _Subst, NewVarSet, NewVarSet).
copy_var_names([OldVar - Name | Rest], Subst, NewVarSet0, NewVarSet) :-
    ( map.search(Subst, OldVar, NewVar) ->
        varset.name_var(NewVarSet0, NewVar, Name, NewVarSet1)
    ;
        NewVarSet1 = NewVarSet0
    ),
    copy_var_names(Rest, Subst, NewVarSet1, NewVarSet).

%-----------------------------------------------------------------------------%

varset.coerce(A, B) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(A, B).

%-----------------------------------------------------------------------------%

varset.max_var(varset(VarSupply, _, _)) = term.var_supply_max_var(VarSupply).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

varset.init = VS :-
    varset.init(VS).

varset.delete_var(VS1, V) = VS2 :-
    varset.delete_var(VS1, V, VS2).

varset.delete_vars(VS1, Vs) = VS2 :-
    varset.delete_vars(VS1, Vs, VS2).

varset.vars(VS) = Vs :-
    varset.vars(VS, Vs).

varset.name_var(VS1, V, S) = VS2 :-
    varset.name_var(VS1, V, S, VS2).

varset.lookup_name(VS, V) = S :-
    varset.lookup_name(VS, V, S).

varset.lookup_name(VS1, V, S) = S2 :-
    varset.lookup_name(VS1, V, S, S2).

varset.bind_var(VS1, V, T) = VS2 :-
    varset.bind_var(VS1, V, T, VS2).

varset.bind_vars(VS1, S) = VS2 :-
    varset.bind_vars(VS1, S, VS2).

varset.lookup_vars(VS) = S :-
    varset.lookup_vars(VS, S).

varset.get_bindings(VS) = S :-
    varset.get_bindings(VS, S).

varset.set_bindings(VS1, S) = VS2 :-
    varset.set_bindings(VS1, S, VS2).

varset.create_name_var_map(VS) = M :-
    varset.create_name_var_map(VS, M).

varset.var_name_list(VS) = AL :-
    varset.var_name_list(VS, AL).

varset.ensure_unique_names(Vs, S1, VS1) = VS2 :-
    varset.ensure_unique_names(Vs, S1, VS1, VS2).

varset.select(VS1, S) = VS2 :-
    varset.select(VS1, S, VS2).

varset.coerce(VS1) = VS2 :-
    varset.coerce(VS1, VS2).
