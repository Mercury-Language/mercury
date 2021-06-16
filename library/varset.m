%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2002-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: varset.m.
% Main author: fjh.
% Stability: low.
%
% This file provides facilities for manipulating collections of variables.
% through the 'varset' ADT. These variables are object-level variables,
% and are represented as ground terms, so it might help to think of them
% as "variable ids" rather than Prolog-style variables.
%
% A varset may record a name and/or a value (binding) with each variable.
%
% Many situations require dealing with several distinct sets of variables
% that should never be mixed together. For example, a compiler may handle
% both program variables and type variables, and it does not make sense
% to a have a single varset containing both program variables and type
% variables. The varsets provided by this module are thus parameterized;
% the compiler can use e.g. varset(prog_var_type) to hold program variables
% and varset(tvar_type) to hold type variables. Since all operations on
% two or more varsets require agreement on the argument of the varset/1
% type constructor, any accidental mixup of different instances of varset/1
% is guaranteed to be caught by the compiler.
%
% In situations in which this is not a concern, programmers may use
% the standard generic varset instance.
%
% Note that there are some design flaws in the relationship between
% varset.m and term.m. There is too much coupling between the two,
% which may and should be fixed later, e.g. by merging the two modules.
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
:- type varset == varset(generic).

%---------------------%

    % Construct an empty varset.
    %
:- func init = varset(T).
:- pred init(varset(T)::out) is det.

    % Check whether a varset is empty.
    %
:- pred is_empty(varset(T)::in) is semidet.

%---------------------%

    % Create a new variable.
    %
:- pred new_var(var(T)::out, varset(T)::in, varset(T)::out) is det.

    % Create a new named variable.
    %
:- pred new_named_var(string::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create a new named variable with a unique (w.r.t. the varset) number
    % appended to the name.
    %
:- pred new_uniquely_named_var(string::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create a new variable, and maybe give it a name.
    %
:- pred new_maybe_named_var(maybe(string)::in, var(T)::out,
    varset(T)::in, varset(T)::out) is det.

    % Create multiple new variables.
    % Throws an exception if a negative number of new variables
    % is requested.
    %
:- pred new_vars(int::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

%---------------------%

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
    % (If the list is not sorted, the predicate or function will
    % either throw an exception or return incorrect output.)
    %
:- func delete_sorted_vars(varset(T), list(var(T))) = varset(T).
:- pred delete_sorted_vars(list(var(T))::in,
    varset(T)::in, varset(T)::out) is det.

%---------------------%

    % Return a list of all the variables in a varset.
    %
:- func vars(varset(T)) = list(var(T)).
:- pred vars(varset(T)::in, list(var(T))::out) is det.

%---------------------%

    % Set the name of a variable.
    %
:- func name_var(varset(T), var(T), string) = varset(T).
:- pred name_var(var(T)::in, string::in,
    varset(T)::in, varset(T)::out) is det.

    % Unset the name of a variable.
    %
:- func unname_var(varset(T), var(T)) = varset(T).
:- pred unname_var(var(T)::in, varset(T)::in, varset(T)::out) is det.

    % Lookup the name of a variable;
    % If it doesn't have one, return a default name consisting of two parts:
    % "V_" as a prefix, followed by a unique number. This is meant to evoke
    % "variable number N".
    %
:- func lookup_name(varset(T), var(T)) = string.
:- pred lookup_name(varset(T)::in, var(T)::in, string::out) is det.

    % Lookup the name of a variable;
    % if it doesn't have one, create one using the specified prefix.
    %
:- func lookup_name(varset(T), var(T), string) = string.
:- pred lookup_name(varset(T)::in, var(T)::in, string::in, string::out)
    is det.

    % Lookup the name of a variable;
    % fail if it doesn't have one.
    %
:- pred search_name(varset(T)::in, var(T)::in, string::out) is semidet.

%---------------------%

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

%---------------------%

    % Get the bindings for all the bound variables.
    %
    % NOTE_TO_IMPLEMENTORS Redundant; identical to get_bindings.
:- func lookup_vars(varset(T)) = substitution(T).
:- pred lookup_vars(varset(T)::in, substitution(T)::out) is det.

    % Get the bindings for all the bound variables.
    %
:- func get_bindings(varset(T)) = substitution(T).
:- pred get_bindings(varset(T)::in, substitution(T)::out) is det.

    % Set the bindings for all the bound variables.
    %
    % NOTE_TO_IMPLEMENTORS The argument order is not conducive
    % NOTE_TO_IMPLEMENTORS to the use of state variables.
:- func set_bindings(varset(T), substitution(T)) = varset(T).
:- pred set_bindings(varset(T)::in, substitution(T)::in,
    varset(T)::out) is det.

%---------------------%

    % Combine two different varsets, renaming apart:
    % merge_renaming(VarSet0, NewVarSet, VarSet, Renaming) is true
    % iff VarSet is the varset that results from joining a suitably renamed
    % version of NewVarSet to VarSet0. (Any bindings in NewVarSet are ignored.)
    % Renaming will map each variable in NewVarSet to the corresponding
    % fresh variable in VarSet.
    %
:- pred merge_renaming(varset(T)::in, varset(T)::in, varset(T)::out,
    renaming(T)::out) is det.

    % Same as merge_renaming, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if create_name_var_map needs to be used
    % on the resulting varset.
    %
:- pred merge_renaming_without_names(varset(T)::in,
    varset(T)::in, varset(T)::out, renaming(T)::out) is det.

    % merge(VarSet0, NewVarSet, Terms0, VarSet, Terms):
    %
    % As merge_renaming, except instead of returning the renaming,
    % this predicate applies it to the given list of terms.
    %
:- pred merge(varset(T)::in, varset(T)::in, list(term(T))::in,
    varset(T)::out, list(term(T))::out) is det.

    % Same as merge, except that the names of variables
    % in NewVarSet are not included in the final varset.
    % This is useful if create_name_var_map needs to be used
    % on the resulting varset.
    %
:- pred merge_without_names(varset(T)::in, varset(T)::in,
    list(term(T))::in, varset(T)::out, list(term(T))::out) is det.

%---------------------%

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

    % Unname all variables whose explicitly given names have the form
    % of the default names used by lookup_name, i.e. "V_" followed by
    % an integer.
    %
    % This predicate is intended to be used in situations where
    % a term has been read in after being written out. The process of
    % writing out the term forces requires every variable to given
    % a name that can be written out, even variables that until then
    % did not have names. If these variables are given names of the default
    % form, then, after the written-out term is read back in, this predicate
    % will recreate the original varset, including the variables without names.
    %
:- pred undo_default_names(varset(T)::in, varset(T)::out) is det.

%---------------------%

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

init = VarSet :-
    varset.init(VarSet).

init(VarSet) :-
    term.init_var_supply(VarSupply),
    map.init(Names),
    map.init(Values),
    VarSet = varset(VarSupply, Names, Values).

%---------------------------------------------------------------------------%

is_empty(varset(VarSupply, _, _)) :-
    term.init_var_supply(VarSupply).

%---------------------------------------------------------------------------%

new_var(Var, !VarSet) :-
    MaxId0 = !.VarSet ^ var_supply,
    term.create_var(Var, MaxId0, MaxId),
    !VarSet ^ var_supply := MaxId.

new_named_var(Name, Var, !VarSet) :-
    !.VarSet = varset(MaxId0, Names0, Values),
    term.create_var(Var, MaxId0, MaxId),
    map.set(Var, Name, Names0, Names),
    !:VarSet = varset(MaxId, Names, Values).

new_uniquely_named_var(Name, Var, !VarSet) :-
    !.VarSet = varset(MaxId0, Names0, Values),
    term.create_var(Var, MaxId0, MaxId),
    N = term.var_to_int(Var),
    map.set(Var, string.format("%s_%d", [s(Name), i(N)]), Names0, Names),
    !:VarSet = varset(MaxId, Names, Values).

new_maybe_named_var(MaybeName, Var, !VarSet) :-
    !.VarSet = varset(MaxId0, Names0, Values),
    term.create_var(Var, MaxId0, MaxId),
    (
        MaybeName = no,
        Names = Names0
    ;
        MaybeName = yes(Name),
        map.set(Var, Name, Names0, Names)
    ),
    !:VarSet = varset(MaxId, Names, Values).

new_vars(NumVars, NewVars, !VarSet) :-
    new_vars_loop(NumVars, [], RevNewVars, !VarSet),
    % Return the new variables in order.
    list.reverse(RevNewVars, NewVars).

:- pred new_vars_loop(int::in, list(var(T))::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

new_vars_loop(NumVars, !RevNewVars, !VarSet) :-
    ( if NumVars > 0 then
        varset.new_var(Var, !VarSet),
        !:RevNewVars = [Var | !.RevNewVars],
        varset.new_vars_loop(NumVars - 1, !RevNewVars, !VarSet)
    else if NumVars = 0 then
        true
    else
        unexpected($pred, "invalid call")
    ).

%---------------------------------------------------------------------------%

delete_var(!.VarSet, DeleteVar) = !:VarSet :-
    varset.delete_var(DeleteVar, !VarSet).

delete_var(DeleteVar, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete(DeleteVar, Names0, Names),
    map.delete(DeleteVar, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

%---------------------------------------------------------------------------%

delete_vars(!.VarSet, DeleteVars) = !:VarSet :-
    varset.delete_vars(DeleteVars, !VarSet).

delete_vars(DeleteVars, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete_list(DeleteVars, Names0, Names),
    map.delete_list(DeleteVars, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

delete_sorted_vars(!.VarSet, DeleteVars) = !:VarSet :-
    varset.delete_sorted_vars(DeleteVars, !VarSet).

delete_sorted_vars(DeleteVars, !VarSet) :-
    !.VarSet = varset(MaxId, Names0, Values0),
    map.delete_sorted_list(DeleteVars, Names0, Names),
    map.delete_sorted_list(DeleteVars, Values0, Values),
    !:VarSet = varset(MaxId, Names, Values).

%---------------------------------------------------------------------------%

vars(VarSet) = Vars :-
    varset.vars(VarSet, Vars).

vars(VarSet, Vars) :-
    MaxId = VarSet ^ var_supply,
    term.init_var_supply(N0),
    vars_loop(N0, MaxId, [], RevVars),
    list.reverse(RevVars, Vars).

:- pred vars_loop(var_supply(T)::in, var_supply(T)::in,
    list(var(T))::in, list(var(T))::out) is det.

vars_loop(Cur, Max, !RevVars) :-
    ( if Cur = Max then
        true
    else
        term.create_var(Var, Cur, Next),
        !:RevVars = [Var | !.RevVars],
        vars_loop(Next, Max, !RevVars)
    ).

%---------------------------------------------------------------------------%

name_var(!.VarSet, Var, Name) = !:VarSet :-
    varset.name_var(Var, Name, !VarSet).

name_var(Var, Name, !VarSet) :-
    Names0 = !.VarSet ^ var_names,
    map.set(Var, Name, Names0, Names),
    !VarSet ^ var_names := Names.

unname_var(!.VarSet, Var) = !:VarSet :-
    varset.unname_var(Var, !VarSet).

unname_var(Var, !VarSet) :-
    Names0 = !.VarSet ^ var_names,
    map.delete(Var, Names0, Names),
    !VarSet ^ var_names := Names.

%---------------------------------------------------------------------------%

lookup_name(VarSet, Var) = Name :-
    varset.lookup_name(VarSet, Var, Name).

lookup_name(VarSet, Var, Name) :-
    ( if varset.search_name(VarSet, Var, NamePrime) then
        Name = NamePrime
    else
        term.var_to_int(Var, VarNum),
        Name = "V_" ++ string.int_to_string(VarNum)
    ).

lookup_name(VarSet, Id, Prefix) = Name :-
    varset.lookup_name(VarSet, Id, Prefix, Name).

lookup_name(VarSet, Id, Prefix, Name) :-
    ( if varset.search_name(VarSet, Id, NamePrime) then
        Name = NamePrime
    else
        term.var_to_int(Id, VarNum),
        Name = Prefix ++ string.int_to_string(VarNum)
    ).

search_name(VarSet, Var, Name) :-
    VarSet = varset(_, Names, _),
    map.search(Names, Var, Name).

%---------------------------------------------------------------------------%

bind_var(!.VarSet, Var, Value) = !:VarSet :-
    varset.bind_var(Var, Value, !VarSet).

bind_var(Var, Value, !VarSet) :-
    Values0 = !.VarSet ^ var_values,
    map.set(Var, Value, Values0, Values),
    !VarSet ^ var_values := Values.

%---------------------------------------------------------------------------%

bind_vars(!.VarSet, Subst) = !:VarSet :-
    varset.bind_vars(Subst, !VarSet).

bind_vars(Subst, !VarSet) :-
    map.to_assoc_list(Subst, VarsValues),
    bind_vars_loop(VarsValues, !VarSet).

:- pred bind_vars_loop(assoc_list(var(T), term(T))::in, varset(T)::in,
    varset(T)::out) is det.

bind_vars_loop([], !VarSet).
bind_vars_loop([Var - Value | VarsValues], !VarSet) :-
    bind_var(Var, Value, !VarSet),
    bind_vars_loop(VarsValues, !VarSet).

%---------------------------------------------------------------------------%

search_var(VarSet, Var, Value) :-
    Values = VarSet ^ var_values,
    map.search(Values, Var, Value).

%---------------------------------------------------------------------------%

lookup_vars(VarSet) = Values :-
    varset.lookup_vars(VarSet, Values).

lookup_vars(VarSet, Values) :-
    Values = VarSet ^ var_values.

%---------------------------------------------------------------------------%

get_bindings(VarSet) = Values :-
    varset.get_bindings(VarSet, Values).

get_bindings(VarSet, Values) :-
    Values = VarSet ^ var_values.

set_bindings(!.VarSet, Values) = !:VarSet :-
    varset.set_bindings(!.VarSet, Values, !:VarSet).

set_bindings(!.VarSet, Values, !:VarSet) :-
    !VarSet ^ var_values := Values.

%---------------------------------------------------------------------------%
%
% We have two near-identical copies of code implementing two related
% but nevertheless distinct exported predicates:
%
% - merge_renaming
% - merge_renaming_without_names
%
% The code of the two must be distinct from each other if we don't want
% to pay the cost of passing around name maps unnecessarily.
%
% The approach we follow in the simple implementation of each of the above
% exported predicates is that we scan through the second varset, introducing
% a fresh variable into the first varset for each var in the second,
% and building up a renaming which maps the variables in the second varset
% into the corresponding fresh variable in the first varset.
%
% The fast implementation each of the above exported predicates does the
% same conceptual job, but with differences in detail.
%
% 1 Instead of inserting oldvar-newvar pairs into the rename map,
%   it constructs a sorted flat assoc list to be turned into a map.
%   If the two varsets have M and N vars respectively, then the simple
%   algorithm is O(N log N), while the fast one is O(N).
%
% 2 Instead of looking up the name of each variable in the second varset
%   in a map, it flattens the second varset's name map and iterates
%   over the resulting list of var-name pairs. Again, the simple algorithm
%   is O(N log N), while the fast one is O(N).
%
% 3 Instead of inserting newvar-name pairs into the updated name map,
%   it constructs a sorted flat assoc list for the new entries.
%   Constructing the updated name map requires flattening the old name map,
%   appending the new entries to it, and turning the result into a map.
%   The simple algorithm is O(N log (M+N)), since it does N insertions
%   into a map whose final size is O(M+N), while the fast algorithm is
%   O(M + N). This *may* yield a slowdown if M is much bigger than N.
%
% As its name says, merge_renaming_without_names does not update the name map,
% so the slowdown mentioned in point 3 cannot occur for them. This is why
% it has a separate heuristic.
%

:- pred use_simple_loop(int::in, int::in) is semidet.
:- pragma inline(pred(use_simple_loop/2)).

use_simple_loop(NumAllocatedA, NumAllocatedB) :-
    % XXX The numbers in this heuristic should to be determined by benchmarks.
    (
        % If NumAllocatedB (the N in the complexity analysis above)
        % is small, then the asymptotic complexity does not matter as much
        % as the constant factors.
        NumAllocatedB =< 5
    ;
        % Avoid flattening NamesA if it does not buy us enough speed on NamesB.
        NumAllocatedA >= NumAllocatedB * 16
    ).

:- pred use_simple_loop_without_names(int::in, int::in) is semidet.
:- pragma inline(pred(use_simple_loop_without_names/2)).

use_simple_loop_without_names(_NumAllocatedA, NumAllocatedB) :-
    % XXX The numbers in this heuristic should to be determined by benchmarks.
    % If NumAllocatedB (the N in the complexity analysis above)
    % is small, then the asymptotic complexity does not matter as much
    % as the constant factors.
    NumAllocatedB =< 5.

%---------------------%

merge_renaming(VarSetA, VarSetB, VarSet, Renaming) :-
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    VarSetB = varset(SupplyB, NamesB, _ValuesB),
    NumAllocatedA = var_supply_num_allocated(SupplyA),
    NumAllocatedB = var_supply_num_allocated(SupplyB),
    ( if use_simple_loop(NumAllocatedA, NumAllocatedB) then
        term.init_var_supply(SupplyB0),
        map.init(Renaming0),
        simple_merge_renaming_loop(SupplyB0, SupplyB, NamesB,
            SupplyA, Supply, NamesA, Names, Renaming0, Renaming)
    else
        map.to_sorted_assoc_list(NamesA, NamesListA),
        map.to_sorted_assoc_list(NamesB, NamesListB),
        fast_merge_renaming_loop(1, NumAllocatedB, NumAllocatedA, NumAllocated,
            NamesListB, [], RevNamesListSuffix, [], RevRenamingList),
        Supply = force_construct_var_supply(NumAllocated),
        list.reverse(RevNamesListSuffix, NamesListSuffix),
        NamesList = NamesListA ++ NamesListSuffix,
        map.from_sorted_assoc_list(NamesList, Names),
        map.from_rev_sorted_assoc_list(RevRenamingList, Renaming)
    ),
    VarSet = varset(Supply, Names, ValuesA).

:- pred simple_merge_renaming_loop(var_supply(T)::in, var_supply(T)::in,
    map(var(T), string)::in,
    var_supply(T)::in, var_supply(T)::out,
    map(var(T), string)::in, map(var(T), string)::out,
    renaming(T)::in, renaming(T)::out) is det.

simple_merge_renaming_loop(!.SupplyB, MaxSupplyB, NamesB,
        !Supply, !Names, !Renaming) :-
    ( if !.SupplyB = MaxSupplyB then
        true
    else
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        ( if map.search(NamesB, VarB, NameB) then
            map.det_insert(Var, NameB, !Names)
        else
            true
        ),
        map.det_insert(VarB, Var, !Renaming),
        disable_warning [suspicious_recursion] (
            simple_merge_renaming_loop(!.SupplyB, MaxSupplyB, NamesB,
                !Supply, !Names, !Renaming)
        )
    ).

:- pred fast_merge_renaming_loop(int::in, int::in, int::in, int::out,
    assoc_list(var(T), string)::in,
    assoc_list(var(T), string)::in, assoc_list(var(T), string)::out,
    assoc_list(var(T), var(T))::in, assoc_list(var(T), var(T))::out) is det.

fast_merge_renaming_loop(CurVarNumB, NumAllocatedB, !NumAllocated,
        VarNamesB, !RevNamesListSuffix, !RevRenaming) :-
    (
        VarNamesB = [],
        fast_merge_renaming_loop_leftover(CurVarNumB, NumAllocatedB,
            !NumAllocated, !RevRenaming)
    ;
        VarNamesB = [HeadVarB - HeadNameB | TailVarNamesB],
        !:NumAllocated = !.NumAllocated + 1,
        Var = force_construct_var(!.NumAllocated),
        VarB = force_construct_var(CurVarNumB),
        !:RevRenaming = [VarB - Var | !.RevRenaming],
        ( if CurVarNumB = var_to_int(HeadVarB) then
            !:RevNamesListSuffix = [Var - HeadNameB | !.RevNamesListSuffix],
            NextVarNamesB = TailVarNamesB
        else
            NextVarNamesB = VarNamesB
        ),
        fast_merge_renaming_loop(CurVarNumB + 1, NumAllocatedB, !NumAllocated,
            NextVarNamesB, !RevNamesListSuffix, !RevRenaming)
    ).

    % A version of fast_merge_renaming_loop specialized for the case
    % where there are no names left from VarSetB.
    %
:- pred fast_merge_renaming_loop_leftover(int::in, int::in, int::in, int::out,
    assoc_list(var(T), var(T))::in, assoc_list(var(T), var(T))::out) is det.

fast_merge_renaming_loop_leftover(CurVarNumB, NumAllocatedB,
        !NumAllocated, !RevRenaming) :-
    ( if CurVarNumB > NumAllocatedB then
        true
    else
        !:NumAllocated = !.NumAllocated + 1,
        Var = force_construct_var(!.NumAllocated),
        VarB = force_construct_var(CurVarNumB),
        !:RevRenaming = [VarB - Var | !.RevRenaming],
        fast_merge_renaming_loop_leftover(CurVarNumB + 1, NumAllocatedB,
            !NumAllocated, !RevRenaming)
    ).

%---------------------%

merge_renaming_without_names(VarSetA, VarSetB, VarSet, Renaming) :-
    VarSetA = varset(SupplyA, NamesA, ValuesA),
    VarSetB = varset(SupplyB, _NamesB, _ValuesB),
    NumAllocatedA = var_supply_num_allocated(SupplyA),
    NumAllocatedB = var_supply_num_allocated(SupplyB),
    ( if use_simple_loop_without_names(NumAllocatedA, NumAllocatedB) then
        term.init_var_supply(SupplyB0),
        map.init(Renaming0),
        simple_merge_renaming_without_names_loop(SupplyB0, SupplyB,
            SupplyA, Supply, Renaming0, Renaming)
    else
        fast_merge_renaming_without_names_loop(1, NumAllocatedB,
            NumAllocatedA, NumAllocated, [], RevRenamingList),
        Supply = force_construct_var_supply(NumAllocated),
        map.from_rev_sorted_assoc_list(RevRenamingList, Renaming)
    ),
    VarSet = varset(Supply, NamesA, ValuesA).

:- pred simple_merge_renaming_without_names_loop(var_supply(T)::in,
    var_supply(T)::in, var_supply(T)::in, var_supply(T)::out,
    renaming(T)::in, renaming(T)::out) is det.

simple_merge_renaming_without_names_loop(!.SupplyB, MaxSupplyB,
        !Supply, !Renaming) :-
    ( if !.SupplyB = MaxSupplyB then
        true
    else
        term.create_var(Var, !Supply),
        term.create_var(VarB, !SupplyB),
        map.det_insert(VarB, Var, !Renaming),
        disable_warning [suspicious_recursion] (
            simple_merge_renaming_without_names_loop(!.SupplyB, MaxSupplyB,
                !Supply, !Renaming)
        )
    ).

:- pred fast_merge_renaming_without_names_loop(int::in, int::in,
    int::in, int::out,
    assoc_list(var(T), var(T))::in, assoc_list(var(T), var(T))::out) is det.

fast_merge_renaming_without_names_loop(CurVarNumB, NumAllocatedB,
        !NumAllocated, !RevRenaming) :-
    ( if CurVarNumB > NumAllocatedB then
        true
    else
        !:NumAllocated = !.NumAllocated + 1,
        Var = force_construct_var(!.NumAllocated),
        VarB = force_construct_var(CurVarNumB),
        !:RevRenaming = [VarB - Var | !.RevRenaming],
        fast_merge_renaming_without_names_loop(CurVarNumB + 1, NumAllocatedB,
            !NumAllocated, !RevRenaming)
    ).

%---------------------------------------------------------------------------%

merge(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    varset.merge_renaming(VarSetA, VarSetB, VarSet, Renaming),
    term.apply_renaming_in_terms(Renaming, TermList0, TermList).

merge_without_names(VarSetA, VarSetB, TermList0, VarSet, TermList) :-
    varset.merge_renaming_without_names(VarSetA, VarSetB, VarSet, Renaming),
    term.apply_renaming_in_terms(Renaming, TermList0, TermList).

%---------------------------------------------------------------------------%

create_name_var_map(VarSet) = NameVars :-
    varset.create_name_var_map(VarSet, NameVars).

create_name_var_map(VarSet, NameVars) :-
    VarNames = VarSet ^ var_names,
    map.keys(VarNames, Vars),
    map.values(VarNames, Names),
    map.from_corresponding_lists(Names, Vars, NameVars).

%---------------------------------------------------------------------------%

var_name_list(VarSet) = VarNameList :-
    varset.var_name_list(VarSet, VarNameList).

var_name_list(VarSet, VarNameList) :-
    VarNames = VarSet ^ var_names,
    map.to_assoc_list(VarNames, VarNameList).

%---------------------------------------------------------------------------%

ensure_unique_names(AllVars, Suffix, !.VarSet) = !:VarSet :-
    varset.ensure_unique_names(AllVars, Suffix, !VarSet).

ensure_unique_names(AllVars, Suffix, !VarSet) :-
    VarNames0 = !.VarSet ^ var_names,
    varset.ensure_unique_names_loop(AllVars, Suffix, set.init, VarNames0,
        map.init, VarNames),
    !VarSet ^ var_names := VarNames.

:- pred ensure_unique_names_loop(list(var(T))::in, string::in,
    set(string)::in, map(var(T), string)::in, map(var(T), string)::in,
    map(var(T), string)::out) is det.

ensure_unique_names_loop([], _, _, _, !VarNames).
ensure_unique_names_loop([Var | Vars], Suffix, !.UsedNames,
        OldVarNames, !VarNames) :-
    ( if map.search(OldVarNames, Var, OldName) then
        ( if set.member(OldName, !.UsedNames) then
            term.var_to_int(Var, VarNum),
            TrialName = OldName ++ "_" ++ string.int_to_string(VarNum)
        else
            TrialName = OldName
        )
    else
        term.var_to_int(Var, VarNum),
        TrialName = "Var_" ++ string.int_to_string(VarNum)
    ),
    append_suffix_until_unique(TrialName, Suffix, !.UsedNames, FinalName),
    set.insert(FinalName, !UsedNames),
    map.det_insert(Var, FinalName, !VarNames),
    ensure_unique_names_loop(Vars, Suffix, !.UsedNames,
        OldVarNames, !VarNames).

:- pred append_suffix_until_unique(string::in, string::in, set(string)::in,
    string::out) is det.

append_suffix_until_unique(Trial0, Suffix, UsedNames, Final) :-
    ( if set.member(Trial0, UsedNames) then
        string.append(Trial0, Suffix, Trial1),
        append_suffix_until_unique(Trial1, Suffix, UsedNames, Final)
    else
        Final = Trial0
    ).

%---------------------------------------------------------------------------%

undo_default_names(VarSet0, VarSet) :-
    VarSet0 = varset(Supply0, Names0, Values0),
    map.to_sorted_assoc_list(Names0, NamesList0),
    undo_default_names_loop(NamesList0, [], RevNamesList),
    map.from_rev_sorted_assoc_list(RevNamesList, Names),
    VarSet = varset(Supply0, Names, Values0).

:- pred undo_default_names_loop(assoc_list(var(T), string)::in,
    assoc_list(var(T), string)::in, assoc_list(var(T), string)::out) is det.

undo_default_names_loop([], !RevNamesList).
undo_default_names_loop([Var - Name | VarNames], !RevNamesList) :-
    ( if
        % Does the variable name have the default form used by lookup_name?
        remove_prefix("V_", Name, NameAfterV),
        string.to_int(NameAfterV, _IntAfterV)
    then
        % Yes, so delete the name by simply not adding it to the list.
        true
    else
        !:RevNamesList = [Var - Name | !.RevNamesList]
    ),
    undo_default_names_loop(VarNames, !RevNamesList).

%---------------------------------------------------------------------------%

select(!.VarSet, Vars) = !:VarSet :-
    varset.select(Vars, !VarSet).

select(Vars, !VarSet) :-
    !.VarSet = varset(Supply, VarNameMap0, Values0),
    map.select(VarNameMap0, Vars, VarNameMap),
    map.select(Values0, Vars, Values),
    !:VarSet = varset(Supply, VarNameMap, Values).

%---------------------------------------------------------------------------%

squash(OldVarSet, KeptVars, NewVarSet, Subst) :-
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
    ( if map.search(Subst, OldVar, NewVar) then
        varset.name_var(NewVar, Name, !NewVarSet)
    else
        true
    ),
    copy_var_names(Rest, Subst, !NewVarSet).

%---------------------------------------------------------------------------%

coerce(!.VarSet) = !:VarSet :-
    varset.coerce(!VarSet).

coerce(!VarSet) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(!VarSet).

%---------------------------------------------------------------------------%

max_var(varset(VarSupply, _, _)) = term.var_supply_max_var(VarSupply).

num_allocated(varset(VarSupply, _, _)) =
    term.var_supply_num_allocated(VarSupply).

%---------------------------------------------------------------------------%
%:- end_module varset.
%---------------------------------------------------------------------------%
