%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2002-2007, 2009-2011 The University of Melbourne.
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
% There may be some design flaws in the relationship between varset.m, and
% term.m.  Once we have implemented unique modes and destructive assignment, we
% will need to rethink the design;  we may end up modifying these modules
% considerably, or we may end up making new single-threaded versions of these
% modules.
% 
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module varset.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

:- type varset(T).

:- type varset  ==  varset(generic).

    % Construct an empty varset.
    %
:- func init = varset(T).
:- pred init(varset(T)::out) is det.

    % Check whether a varset is empty.
    %
:- pred is_empty(varset(T)::in) is semidet.

    % Create a new variable.
    %
:- pred new_var(var(T)::out, varset(T)::in, varset(T)::out) is det.

    % Create a new named variable.
    %
:- pred new_named_var(string::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create a new named variable with a unique (w.r.t. the
    % varset) number appended to the name.
    %
:- pred new_uniquely_named_var(string::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create a new variable, and maybe give it a name.
    %
:- pred new_maybe_named_var(maybe(string)::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create multiple new variables.
    %
:- pred new_vars(int::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

    % Delete the name and value for a variable.
    %
:- func delete_var(varset(T), var(T)) = varset(T).
:- pred delete_var(var(T)::in, varset(T)::in, varset(T)::out) is det.

    % Delete the names and values for a list of variables.
    %
:- func delete_vars(varset(T), list(var(T))) = varset(T).
:- pred delete_vars(list(var(T))::in, varset(T)::in, varset(T)::out)
    is det.

    % Delete the names and values for a sorted list of variables.
    % (If the list is not sorted, the result will be either an abort
    % or incorrect output.)
    %
:- func delete_sorted_vars(varset(T), list(var(T))) = varset(T).
:- pred delete_sorted_vars(list(var(T))::in,
    varset(T)::in, varset(T)::out) is det.

    % Return a list of all the variables in a varset.
    %
:- func vars(varset(T)) = list(var(T)).
:- pred vars(varset(T)::in, list(var(T))::out) is det.

    % Set the name of a variable.
    %
:- func name_var(varset(T), var(T), string) = varset(T).
:- pred name_var(var(T)::in, string::in,
    varset(T)::in, varset(T)::out) is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using V_ as a prefix.
    %
:- func lookup_name(varset(T), var(T)) = string.
:- pred lookup_name(varset(T)::in, var(T)::in, string::out) is det.

    % Lookup the name of a variable;
    % create one if it doesn't have one using the specified prefix
    %
:- func lookup_name(varset(T), var(T), string) = string.
:- pred lookup_name(varset(T)::in, var(T)::in, string::in, string::out)
    is det.

    % Lookup the name of a variable;
    % fail if it doesn't have one
    %
:- pred search_name(varset(T)::in, var(T)::in, string::out) is semidet.

    % Bind a value to a variable.
    % This will overwrite any existing binding.
    %
:- func bind_var(varset(T), var(T), term(T)) = varset(T).
:- pred bind_var(var(T)::in, term(T)::in,
    varset(T)::in, varset(T)::out) is det.

    % Bind a set of terms to a set of variables.
    %
:- func bind_vars(varset(T), substitution(T)) = varset(T).
:- pred bind_vars(substitution(T)::in,
    varset(T)::in, varset(T)::out) is det.

    % Lookup the value of a variable.
    %
:- pred search_var(varset(T)::in, var(T)::in, term(T)::out) is semidet.

    % Get the bindings for all the bound variables.
    %
:- func lookup_vars(varset(T)) = substitution(T).
:- pred lookup_vars(varset(T)::in, substitution(T)::out) is det.

    % Combine two different varsets, renaming apart:
    % merge_renaming(VarSet0, NewVarSet, VarSet, Subst) is true
    % iff VarSet is the varset that results from joining a suitably renamed
    % version of NewVarSet to VarSet0. (Any bindings in NewVarSet are ignored.)
    % Renaming map the variables in NewVarSet into the corresponding
    % fresh variable in VarSet.
    %
:- pred merge_renaming(varset(T)::in, varset(T)::in, varset(T)::out,
    renaming(T)::out) is det.

    % Does the same job as merge_renaming, but returns the renaming
    % as a general substitution in which all the terms in the range happen
    % to be variables.
    %
    % Consider using merge_renaming instead.
    %
:- pred merge_subst(varset(T)::in, varset(T)::in, varset(T)::out,
    substitution(T)::out) is det.
:- pragma obsolete(merge_subst/4).

    % merge(VarSet0, NewVarSet, Terms0, VarSet, Terms):
    %
    % As merge_renaming, except instead of returning the renaming,
    % this predicate applies it to the given list of terms.
    %
:- pred merge(varset(T)::in, varset(T)::in, list(term(T))::in,
    varset(T)::out, list(term(T))::out) is det.

    % Same as merge_renaming, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if create_name_var_map needs to be used
    % on the resulting varset.
    %
:- pred merge_renaming_without_names(varset(T)::in,
    varset(T)::in, varset(T)::out, renaming(T)::out) is det.

    % Same as merge_subst, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if create_name_var_map needs to be used
    % on the resulting varset.
    %
    % Consider using merge_renaming_without_names instead.
    %
:- pred merge_subst_without_names(varset(T)::in,
    varset(T)::in, varset(T)::out, substitution(T)::out) is det.
:- pragma obsolete(merge_subst_without_names/4).

    % Same as merge, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if create_name_var_map needs to be used
    % on the resulting varset.
    %
:- pred merge_without_names(varset(T)::in, varset(T)::in,
    list(term(T))::in, varset(T)::out, list(term(T))::out) is det.

    % Get the bindings for all the bound variables.
    %
:- func get_bindings(varset(T)) = substitution(T).
:- pred get_bindings(varset(T)::in, substitution(T)::out) is det.

    % Set the bindings for all the bound variables.
    %
:- func set_bindings(varset(T), substitution(T)) = varset(T).
:- pred set_bindings(varset(T)::in, substitution(T)::in,
    varset(T)::out) is det.

    % Create a map from names to variables.
    % Each name is mapped to only one variable, even if a name is
    % shared by more than one variable. Therefore this predicate
    % is only really useful if it is already known that no two
    % variables share the same name.
    %
:- func create_name_var_map(varset(T)) = map(string, var(T)).
:- pred create_name_var_map(varset(T)::in, map(string, var(T))::out)
    is det.

    % Return an association list giving the name of each variable.
    % Every variable has an entry in the returned association list,
    % even if it shares its name with another variable.
    %
:- func var_name_list(varset(T)) = assoc_list(var(T), string).
:- pred var_name_list(varset(T)::in, assoc_list(var(T), string)::out)
    is det.

    % Given a list of variable and varset in which some variables have
    % no name but some other variables may have the same name,
    % return another varset in which every variable has a unique name.
    % If necessary, names will have suffixes added on the end;
    % the second argument gives the suffix to use.
    %
:- func ensure_unique_names(list(var(T)), string, varset(T))
    = varset(T).
:- pred ensure_unique_names(list(var(T))::in,
    string::in, varset(T)::in, varset(T)::out) is det.

    % Given a varset and a set of variables, remove the names
    % and values of any other variables stored in the varset.
    %
:- func select(varset(T), set(var(T))) = varset(T).
:- pred select(set(var(T))::in, varset(T)::in, varset(T)::out) is det.

    % Given a varset and a list of variables, construct a new varset
    % containing one variable for each one in the list (and no others).
    % Also return a substitution mapping the selected variables in the
    % original varset into variables in the new varset. The relative
    % ordering of variables in the original varset is maintained.
    %
:- pred squash(varset(T)::in, list(var(T))::in,
    varset(T)::out, renaming(T)::out) is det.

    % Coerce the types of the variables in a varset.
    %
:- func coerce(varset(T)) = varset(U).
:- pred coerce(varset(T)::in, varset(U)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Returns the highest numbered variable returned from this varset's
    % var_supply.
    %
:- func max_var(varset(T)) = var(T).

:- func num_allocated(varset(T)) = int.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.

:- type varset(T)
    --->    varset(
                var_supply  :: var_supply(T),
                var_names   :: map(var(T), string),
                var_values  :: map(var(T), term(T))
            ).

%---------------------------------------------------------------------------%

varset.init = VS :-
    varset.init(VS).

varset.init(varset(VarSupply, Names, Values)) :-
    term.init_var_supply(VarSupply),
    map.init(Names),
    map.init(Values).

%---------------------------------------------------------------------------%

varset.is_empty(varset(VarSupply, _, _)) :-
    term.init_var_supply(VarSupply).

%---------------------------------------------------------------------------%

varset.new_var(Var, !VarSet) :-
    MaxId0 = !.VarSet ^ var_supply,
    term.create_var(Var, MaxId0, MaxId),
    !VarSet ^ var_supply := MaxId.

varset.new_named_var(Name, Var,
        varset(!.MaxId, !.Names, Values), varset(!:MaxId, !:Names, Values)) :-
    term.create_var(Var, !MaxId),
    map.set(Var, Name, !Names).

varset.new_uniquely_named_var(Name, Var,
        varset(!.MaxId, !.Names, Values), varset(!:MaxId, !:Names, Values)) :-
    term.create_var(Var, !MaxId),
    N = term.var_id(Var),
    map.set(Var, string.format("%s_%d", [s(Name), i(N)]), !Names).

varset.new_maybe_named_var(MaybeName, Var,
        varset(!.MaxId, !.Names, Values), varset(!:MaxId, !:Names, Values)) :-
    term.create_var(Var, !MaxId),
    (
        MaybeName = no
    ;
        MaybeName = yes(Name),
        map.set(Var, Name, !Names)
    ).

varset.new_vars(NumVars, NewVars, !VarSet) :-
    varset.new_vars_2(NumVars, [], RevNewVars, !VarSet),
    % Return the new variables in order.
    list.reverse(RevNewVars, NewVars).

:- pred varset.new_vars_2(int::in, list(var(T))::in,
    list(var(T))::out, varset(T)::in, varset(T)::out) is det.

varset.new_vars_2(NumVars, !RevNewVars, !VarSet) :-
    ( NumVars > 0 ->
        varset.new_var(Var, !VarSet),
        !:RevNewVars = [Var | !.RevNewVars],
        varset.new_vars_2(NumVars - 1, !RevNewVars, !VarSet)
    ; NumVars = 0 ->
        true
    ;
        error("varset.new_vars - invalid call")
    ).

%---------------------------------------------------------------------------%

varset.delete_var(DeleteVar, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete(DeleteVar, Names0, Names),
    map.delete(DeleteVar, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

%---------------------------------------------------------------------------%

varset.delete_vars(DeleteVars, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete_list(DeleteVars, Names0, Names),
    map.delete_list(DeleteVars, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

varset.delete_sorted_vars(DeleteVars, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete_sorted_list(DeleteVars, Names0, Names),
    map.delete_sorted_list(DeleteVars, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

%---------------------------------------------------------------------------%

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
        term.create_var(Var, N, NPrime),
        RevVars = varset.vars_2(NPrime, Max, [Var | RevVars0])
    ).

%---------------------------------------------------------------------------%

varset.name_var(Id, Name, !VarSet) :-
    Names0 = !.VarSet ^ var_names,
    map.set(Id, Name, Names0, Names),
    !VarSet ^ var_names := Names.

%---------------------------------------------------------------------------%

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
    map.search(Names, Id, Name).

%---------------------------------------------------------------------------%

varset.bind_var(Id, Val, !VarSet) :-
    Values0 = !.VarSet ^ var_values,
    map.set(Id, Val, Values0, Values),
    !VarSet ^ var_values := Values.

%---------------------------------------------------------------------------%

varset.bind_vars(Subst, !VarSet) :-
    map.to_assoc_list(Subst, VarTermList),
    varset.bind_vars_2(VarTermList, !VarSet).

:- pred varset.bind_vars_2(assoc_list(var(T), term(T))::in, varset(T)::in,
    varset(T)::out) is det.

varset.bind_vars_2([], !VarSet).
varset.bind_vars_2([V - T | Rest], !VarSet) :-
    varset.bind_var(V, T, !VarSet),
    varset.bind_vars_2(Rest, !VarSet).

%---------------------------------------------------------------------------%

varset.search_var(VarSet, Id, Val) :-
    Values = VarSet ^ var_values,
    map.search(Values, Id, Val).

%---------------------------------------------------------------------------%

varset.lookup_vars(VarSet, VarSet ^ var_values).

%---------------------------------------------------------------------------%

varset.get_bindings(VarSet, VarSet ^ var_values).

varset.set_bindings(VarSet, Values, VarSet ^ var_values := Values).

%---------------------------------------------------------------------------%

    % We scan through the second varset, introducing a fresh variable
    % into the first varset for each var in the second, and building up
    % a renaming which maps the variables in the second varset into
    % the corresponding fresh variable in the first varset. We then apply
    % this renaming to the list of terms.

varset.merge(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    varset.merge_renaming(VarSetA, VarSetB, VarSet, Renaming),
    term.apply_renaming_to_list(TermList0, Renaming, TermList).

varset.merge_without_names(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    varset.merge_renaming_without_names(VarSetA, VarSetB, VarSet, Renaming),
    term.apply_renaming_to_list(TermList0, Renaming, TermList).

%---------------------------------------------------------------------------%
%
% The structure of this code is identical to the structure of the code
% in the next block.

varset.merge_renaming(VarSetA, VarSetB, VarSet, Renaming) :-
    VarSetB = varset(SupplyB, NamesB, _ValuesB),
    term.init_var_supply(SupplyB0),
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    map.init(Renaming0),
    varset.merge_renaming_2(SupplyB0, SupplyB, NamesB,
        SupplyA, Supply, NamesA, Names, Renaming0, Renaming),
    VarSet = varset(Supply, Names, ValuesA).

:- pred varset.merge_renaming_2(var_supply(T)::in, var_supply(T)::in,
    map(var(T), string)::in,
    var_supply(T)::in, var_supply(T)::out,
    map(var(T), string)::in, map(var(T), string)::out,
    renaming(T)::in, renaming(T)::out) is det.

varset.merge_renaming_2(!.SupplyB, MaxSupplyB, NamesB,
        !Supply, !Names, !Renaming) :-
    ( !.SupplyB = MaxSupplyB ->
        true
    ;
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        ( map.search(NamesB, VarB, NameB) ->
            map.det_insert(Var, NameB, !Names)
        ;
            true
        ),
        map.det_insert(VarB, Var, !Renaming),
        varset.merge_renaming_2(!.SupplyB, MaxSupplyB, NamesB,
            !Supply, !Names, !Renaming)
    ).

varset.merge_renaming_without_names(VarSetA, VarSetB, VarSet, Renaming) :-
    VarSetB = varset(SupplyB, _NamesB, _ValuesB),
    term.init_var_supply(SupplyB0),
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    map.init(Renaming0),
    varset.merge_renaming_without_names_2(SupplyB0, SupplyB,
        SupplyA, Supply, Renaming0, Renaming),
    VarSet = varset(Supply, NamesA, ValuesA).

:- pred varset.merge_renaming_without_names_2(var_supply(T)::in,
    var_supply(T)::in, var_supply(T)::in, var_supply(T)::out,
    renaming(T)::in, renaming(T)::out) is det.

varset.merge_renaming_without_names_2(!.SupplyB, MaxSupplyB,
        !Supply, !Renaming) :-
    ( !.SupplyB = MaxSupplyB ->
        true
    ;
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        map.det_insert(VarB, Var, !Renaming),
        varset.merge_renaming_without_names_2(!.SupplyB, MaxSupplyB,
            !Supply, !Renaming)
    ).

%---------------------------------------------------------------------------%
%
% The structure of this code is identical to the structure of the code
% in the previous block.

varset.merge_subst(VarSetA, VarSetB, VarSet, Subst) :-
    VarSetB = varset(SupplyB, NamesB, _ValuesB),
    term.init_var_supply(SupplyB0),
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    map.init(Subst0),
    varset.merge_subst_2(SupplyB0, SupplyB, NamesB,
        SupplyA, Supply, NamesA, Names, Subst0, Subst),
    VarSet = varset(Supply, Names, ValuesA).

:- pred varset.merge_subst_2(var_supply(T)::in, var_supply(T)::in,
    map(var(T), string)::in,
    var_supply(T)::in, var_supply(T)::out,
    map(var(T), string)::in, map(var(T), string)::out,
    substitution(T)::in, substitution(T)::out) is det.

varset.merge_subst_2(!.SupplyB, MaxSupplyB, NamesB,
        !Supply, !Names, !Subst) :-
    ( !.SupplyB = MaxSupplyB ->
        true
    ;
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        ( map.search(NamesB, VarB, NameB) ->
            map.det_insert(Var, NameB, !Names)
        ;
            true
        ),
        Replacement = term.variable(Var, context_init),
        map.det_insert(VarB, Replacement, !Subst),
        varset.merge_subst_2(!.SupplyB, MaxSupplyB, NamesB,
            !Supply, !Names, !Subst)
    ).

varset.merge_subst_without_names(VarSetA, VarSetB, VarSet, Subst) :-
    VarSetB = varset(SupplyB, _NamesB, _ValuesB),
    term.init_var_supply(SupplyB0),
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    map.init(Subst0),
    varset.merge_subst_without_names_2(SupplyB0, SupplyB,
        SupplyA, Supply, Subst0, Subst),
    VarSet = varset(Supply, NamesA, ValuesA).

:- pred varset.merge_subst_without_names_2( var_supply(T)::in,
    var_supply(T)::in, var_supply(T)::in, var_supply(T)::out,
    substitution(T)::in, substitution(T)::out) is det.

varset.merge_subst_without_names_2(!.SupplyB, MaxSupplyB, !Supply, !Subst) :-
    ( !.SupplyB = MaxSupplyB ->
        true
    ;
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        Replacement = term.variable(Var, context_init),
        map.det_insert(VarB, Replacement, !Subst),
        varset.merge_subst_without_names_2(!.SupplyB, MaxSupplyB,
            !Supply, !Subst)
    ).

%---------------------------------------------------------------------------%

varset.create_name_var_map(VarSet, NameVars) :-
    VarNames = VarSet ^ var_names,
    map.keys(VarNames, Vars),
    map.values(VarNames, Names),
    map.from_corresponding_lists(Names, Vars, NameVars).

%---------------------------------------------------------------------------%

varset.var_name_list(VarSet, VarNameList) :-
    VarNames = VarSet ^ var_names,
    map.to_assoc_list(VarNames, VarNameList).

%---------------------------------------------------------------------------%

varset.ensure_unique_names(AllVars, Suffix, !VarSet) :-
    VarNames0 = !.VarSet ^ var_names,
    varset.ensure_unique_names_2(AllVars, Suffix, set.init, VarNames0,
        map.init, VarNames),
    !VarSet ^ var_names := VarNames.

:- pred varset.ensure_unique_names_2(list(var(T))::in, string::in,
    set(string)::in, map(var(T), string)::in, map(var(T), string)::in,
    map(var(T), string)::out) is det.

varset.ensure_unique_names_2([], _, _, _, !VarNames).
varset.ensure_unique_names_2([Var | Vars], Suffix, !.UsedNames, OldVarNames,
        !VarNames) :-
    ( map.search(OldVarNames, Var, OldName) ->
        ( set.member(OldName, !.UsedNames) ->
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
    append_suffix_until_unique(TrialName, Suffix, !.UsedNames, FinalName),
    set.insert(FinalName, !UsedNames),
    map.det_insert(Var, FinalName, !VarNames),
    varset.ensure_unique_names_2(Vars, Suffix, !.UsedNames, OldVarNames,
        !VarNames).

:- pred append_suffix_until_unique(string::in, string::in, set(string)::in,
    string::out) is det.

append_suffix_until_unique(Trial0, Suffix, UsedNames, Final) :-
    ( set.member(Trial0, UsedNames) ->
        string.append(Trial0, Suffix, Trial1),
        append_suffix_until_unique(Trial1, Suffix, UsedNames, Final)
    ;
        Final = Trial0
    ).

%---------------------------------------------------------------------------%

varset.select(Vars, VarSet0, VarSet) :-
    VarSet0 = varset(Supply, VarNameMap0, Values0),
    map.select(VarNameMap0, Vars, VarNameMap),
    map.select(Values0, Vars, Values),
    VarSet = varset(Supply, VarNameMap, Values).

%---------------------------------------------------------------------------%

varset.squash(OldVarSet, KeptVars, NewVarSet, Subst) :-
    % Create a new varset with the same number of variables.
    list.length(KeptVars, NumVars),
    varset.init(NewVarSet0),
    varset.new_vars(NumVars, NewVars0, NewVarSet0, NewVarSet1),

    % We need to sort the fresh variables, to ensure that the substitution
    % that we create below does not alter the relative ordering of the
    % variables.
    list.sort(NewVars0, NewVars),

    % Copy the variable names across from the old varset to the new varset.
    varset.var_name_list(OldVarSet, VarNames),
    map.from_corresponding_lists(KeptVars, NewVars, Subst),
    copy_var_names(VarNames, Subst, NewVarSet1, NewVarSet).

:- pred copy_var_names(assoc_list(var(T), string)::in, renaming(T)::in,
    varset(T)::in, varset(T)::out) is det.

copy_var_names([], _Subst, !NewVarSet).
copy_var_names([OldVar - Name | Rest], Subst, !NewVarSet) :-
    ( map.search(Subst, OldVar, NewVar) ->
        varset.name_var(NewVar, Name, !NewVarSet)
    ;
        true
    ),
    copy_var_names(Rest, Subst, !NewVarSet).

%---------------------------------------------------------------------------%

varset.coerce(A, B) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(A, B).

%---------------------------------------------------------------------------%

varset.max_var(varset(VarSupply, _, _)) = term.var_supply_max_var(VarSupply).

varset.num_allocated(varset(VarSupply, _, _)) =
    term.var_supply_num_allocated(VarSupply).

%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

varset.delete_var(!.VS, V) = !:VS :-
    varset.delete_var(V, !VS).

varset.delete_vars(!.VS, Vs) = !:VS :-
    varset.delete_vars(Vs, !VS).

varset.delete_sorted_vars(!.VS, Vs) = !:VS :-
    varset.delete_sorted_vars(Vs, !VS).

varset.vars(VS) = Vs :-
    varset.vars(VS, Vs).

varset.name_var(!.VS, V, S) = !:VS :-
    varset.name_var(V, S, !VS).

varset.lookup_name(VS, V) = S :-
    varset.lookup_name(VS, V, S).

varset.lookup_name(VS1, V, S) = S2 :-
    varset.lookup_name(VS1, V, S, S2).

varset.bind_var(!.VS, V, T) = !:VS :-
    varset.bind_var(V, T, !VS).

varset.bind_vars(!.VS, S) = !:VS :-
    varset.bind_vars(S, !VS).

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

varset.select(!.VS, S) = !:VS :-
    varset.select(S, !VS).

varset.coerce(!.VS) = !:VS :-
    varset.coerce(!VS).

%---------------------------------------------------------------------------%
%:- end_module varset.
%---------------------------------------------------------------------------%
