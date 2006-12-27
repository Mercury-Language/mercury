%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pred_table.m.
% Main authors: fjh, conway.
%
% This module defines the part of the High Level Data Structure or HLDS
% that allows the compiler to look up predicates by name (qualified,
% unqualified or some mixture) and/or arity.
%
%-----------------------------------------------------------------------------%

:- module hlds.pred_table.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module multi_map.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module svmulti_map.

%-----------------------------------------------------------------------------%

:- interface.

:- type predicate_table.

:- type pred_table  ==  map(pred_id, pred_info).

    % Various predicates for accessing the predicate_table type.
    % The predicate_table holds information about the predicates
    % and functions defined in this module or imported from other modules.
    % The primary key for this table is the `pred_id', but there
    % are also secondary indexes on each of name, name+arity, and
    % module+name+arity, for both functions and predicates.

    % Initialize the predicate table.
    %
:- pred predicate_table_init(predicate_table::out) is det.

    % Balance all the binary trees in the predicate table
    %
:- pred predicate_table_optimize(predicate_table::in, predicate_table::out)
    is det.

    % Get the pred_id->pred_info map.
    %
:- pred predicate_table_get_preds(predicate_table::in, pred_table::out) is det.

    % Restrict the predicate table to the list of predicates. This predicate
    % should only be used when the set of predicates to restrict the table to
    % is significantly smaller than the predicate_table size, as rather than
    % removing entries from the table it builds a new table from scratch.
    %
:- pred predicate_table_restrict(partial_qualifier_info::in,
    list(pred_id)::in, predicate_table::in, predicate_table::out) is det.

    % Set the pred_id->pred_info map.
    % NB You shouldn't modify the keys in this table, only
    % use predicate_table_insert, predicate_table_remove_predid and
    % predicate_table_remove_predicate.
    %
:- pred predicate_table_set_preds(pred_table::in,
    predicate_table::in, predicate_table::out) is det.

    % Get a list of all the valid predids in the predicate_table.
    %
:- pred predicate_table_get_predids(predicate_table::in, list(pred_id)::out)
    is det.

    % Remove a pred_id from the valid list.
    %
:- pred predicate_table_remove_predid(pred_id::in,
    predicate_table::in, predicate_table::out) is det.
:- pred predicate_table_remove_predicate(pred_id::in,
    predicate_table::in, predicate_table::out) is det.

    % Reverse the order of pred_ids stored in the predicate_table.
    %
:- pred predicate_table_reverse_predids(predicate_table::in,
    predicate_table::out) is det.

    % Search the table for (a) predicates or functions (b) predicates only
    % or (c) functions only matching this (possibly module-qualified) sym_name.
    %
:- pred predicate_table_search_sym(predicate_table::in, is_fully_qualified::in,
    sym_name::in, list(pred_id)::out) is semidet.

:- pred predicate_table_search_pred_sym(predicate_table::in,
    is_fully_qualified::in, sym_name::in, list(pred_id)::out) is semidet.

:- pred predicate_table_search_func_sym(predicate_table::in,
    is_fully_qualified::in, sym_name::in, list(pred_id)::out) is semidet.

    % Search the table for (a) predicates or functions (b) predicates only
    % or (c) functions only matching this (possibly module-qualified)
    % sym_name & arity.
    %
:- pred predicate_table_search_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, arity::in, list(pred_id)::out)
    is semidet.

:- pred predicate_table_search_pred_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, arity::in, list(pred_id)::out)
    is semidet.

:- pred predicate_table_search_func_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, arity::in, list(pred_id)::out)
    is semidet.

    % Search the table for (a) predicates or functions
    % (b) predicates only or (c) functions only matching this name.
    %
:- pred predicate_table_search_name(predicate_table::in, string::in,
    list(pred_id)::out) is semidet.

:- pred predicate_table_search_pred_name(predicate_table::in, string::in,
    list(pred_id)::out) is semidet.

:- pred predicate_table_search_func_name(predicate_table::in, string::in,
    list(pred_id)::out) is semidet.

    % Search the table for (a) predicates or functions (b) predicates only
    % or (c) functions only matching this name & arity. When searching for
    % functions, the arity used is the arity of the function, not the arity
    % N+1 predicate that it gets converted to.
    %
:- pred predicate_table_search_name_arity(predicate_table::in, string::in,
    arity::in, list(pred_id)::out) is semidet.

:- pred predicate_table_search_pred_name_arity(predicate_table::in, string::in,
    arity::in, list(pred_id)::out) is semidet.

:- pred predicate_table_search_func_name_arity(predicate_table::in, string::in,
    arity::in, list(pred_id)::out) is semidet.

    % Search the table for (a) predicates or functions (b) predicates only
    % or (c) functions only matching this module, name & arity. When searching
    % for functions, the arity used is the arity of the function, not the arity
    % N+1 predicate that it gets converted to.
    %
    % Note that in cases (b) and (c) it was previously the case that there
    % could only be one matching pred_id, since each predicate or function
    % could be uniquely identified by its module, name, arity, and category
    % (function/predicate). However this is no longer true, due to nested
    % modules. (For example, `pred foo:bar/2' might match both
    % `pred mod1:foo:bar/2' and `pred mod2:foo:bar/2'). I hope it doesn't
    % break anything too badly...
    %
    % (`m_n_a' here is short for "module, name, arity".)

    % Is the item known to be fully qualified? If so, a search for
    % `pred foo.bar/2' will not match `pred baz.foo.bar/2'.
:- type is_fully_qualified
    --->    is_fully_qualified
    ;       may_be_partially_qualified.

:- pred predicate_table_search_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, arity::in,
    list(pred_id)::out) is semidet.

:- pred predicate_table_search_pred_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, arity::in,
    list(pred_id)::out) is semidet.

:- pred predicate_table_search_func_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, arity::in,
    list(pred_id)::out) is semidet.

    % Search the table for predicates or functions matching this pred_or_func
    % category, module, name, and arity. When searching for functions, the
    % arity used is the arity of the predicate that the function gets converted
    % to, i.e. the arity of the function plus one.
    % NB. This is opposite to what happens with the search predicates
    % declared above!!
    %
:- pred predicate_table_search_pf_m_n_a(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, module_name::in, string::in,
    arity::in, list(pred_id)::out) is semidet.

    % Search the table for predicates or functions matching this pred_or_func
    % category, name, and arity. When searching for functions, the arity used
    % is the arity of the predicate that the function gets converted to,
    % i.e. the arity of the function plus one.
    % NB. This is opposite to what happens with the search predicates
    % declared above!!
    %
:- pred predicate_table_search_pf_name_arity(predicate_table::in,
    pred_or_func::in, string::in, arity::in, list(pred_id)::out)
    is semidet.

    % Search the table for predicates or functions matching this pred_or_func
    % category, sym_name, and arity. When searching for functions, the arity
    % used is the arity of the predicate that the function gets converted to,
    % i.e. the arity of the function plus one.
    % NB. This is opposite to what happens with the search predicates
    % declared above!!
    %
:- pred predicate_table_search_pf_sym_arity(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, sym_name::in, arity::in,
    list(pred_id)::out) is semidet.

    % Search the table for predicates or functions matching
    % this pred_or_func category and sym_name.
    %
:- pred predicate_table_search_pf_sym(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, sym_name::in,
    list(pred_id)::out) is semidet.

    % predicate_table_insert(PredTable0, PredInfo,
    %   NeedQual, PartialQualInfo, PredId, PredTable):
    %
    % Insert PredInfo into PredTable0 and assign it a new pred_id.
    % You should check beforehand that the pred doesn't already occur
    % in the table.
    %
:- pred predicate_table_insert_qual(pred_info::in, need_qualifier::in,
    partial_qualifier_info::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

    % Equivalent to predicate_table_insert_qual/6, except that only the
    % fully-qualified version of the predicate will be inserted into the
    % predicate symbol table. This is useful for creating compiler-generated
    % predicates which will only ever be accessed via fully-qualified names.
    %
:- pred predicate_table_insert(pred_info::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

    % Find a predicate which matches the given name and argument types.
    % Abort if there is no matching pred.
    % Abort if there are multiple matching preds.
    % 
:- pred resolve_pred_overloading(module_info::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, sym_name::in, sym_name::out, pred_id::out)
    is det.

    % Find a predicate or function from the list of pred_ids which matches the
    % given name and argument types.  If the constraint_search argument is
    % provided then also check that the class context is consistent with what
    % is expected.  Fail if there is no matching pred.  Abort if there are
    % multiple matching preds.
    %
:- pred find_matching_pred_id(module_info::in, list(pred_id)::in, tvarset::in,
    list(mer_type)::in, maybe(constraint_search)::in(maybe(constraint_search)),
    pred_id::out, sym_name::out) is semidet.

    % A means to check that the required constraints are available, without
    % knowing in advance how many are required.
    %
:- type constraint_search == pred(int, list(prog_constraint)).
:- inst constraint_search == (pred(in, out) is semidet).

    % Get the pred_id and proc_id matching a higher-order term with
    % the given argument types, aborting with an error if none is found.
    %
:- pred get_pred_id_and_proc_id(is_fully_qualified::in, sym_name::in,
    pred_or_func::in, tvarset::in, list(mer_type)::in, module_info::in,
    pred_id::out, proc_id::out) is det.

    % Get the pred_id matching a higher-order term with
    % the given argument types, failing if none is found.
    %
:- pred get_pred_id(is_fully_qualified::in, sym_name::in, pred_or_func::in,
    tvarset::in, list(mer_type)::in, module_info::in, pred_id::out) is semidet.

    % Given a pred_id, return the single proc_id, aborting
    % if there are no modes or more than one mode.
    %
:- pred get_proc_id(module_info::in, pred_id::in, proc_id::out) is det.

:- type mode_no
    --->    only_mode           % The pred must have exactly one mode.
    ;       mode_no(int).       % The Nth mode, counting from 0.

:- pred lookup_builtin_pred_proc_id(module_info::in, module_name::in,
    string::in, pred_or_func::in, arity::in, mode_no::in,
    pred_id::out, proc_id::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type predicate_table
    --->    predicate_table(
                preds               :: pred_table,
                                    % Map from pred_id to pred_info.

                next_pred_id        :: pred_id,
                                    % The next available pred_id.

                pred_ids            :: list(pred_id),
                                    % The keys of the pred_table - cached
                                    % here for efficiency.

                accessibility_table :: accessibility_table,
                                    % How is the predicate accessible?

                % Indexes on predicates

                pred_name_index     :: name_index,
                                    % Map from pred name to pred_id.

                pred_name_arity_index :: name_arity_index,
                                    % Map from pred name & arity to pred_id.

                pred_module_name_arity_index :: module_name_arity_index,
                                    % Map from pred module, name & arity
                                    % to pred_id.

                % Indexes on functions

                func_name_index     :: name_index,
                                    % Map from func name to pred_id.

                func_name_arity_index :: name_arity_index,
                                    % Map from func name & arity to pred_id.

                func_module_name_arity_index :: module_name_arity_index
                                    % Map from func module, name & arity
                                    % to pred_id.
            ).

:- type accessibility_table == map(pred_id, name_accessibility).

:- type name_accessibility
    --->    access(
                % Is this predicate accessible by its unqualified name.
                accessible_by_unqualifed_name       :: bool,

                % Is this predicate accessible by any partially qualified
                % names.
                accessible_by_partially_qualified_names :: bool
            ).

:- type name_index  == map(string, list(pred_id)).

:- type name_arity_index == map(name_arity, list(pred_id)).
:- type name_arity ---> string / arity.

    % First search on module and name, then search on arity. The two levels
    % are needed because typecheck.m needs to be able to search on module
    % and name only for higher-order terms.
:- type module_name_arity_index ==
    map(pair(module_name, string), map(arity, list(pred_id))).

predicate_table_init(PredicateTable) :-
    PredicateTable = predicate_table(Preds, NextPredId, PredIds,
        AccessibilityTable,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index),
    map.init(Preds),
    NextPredId = hlds_pred.initial_pred_id,
    PredIds = [],
    map.init(AccessibilityTable),
    map.init(Pred_N_Index),
    map.init(Pred_NA_Index),
    map.init(Pred_MNA_Index),
    map.init(Func_N_Index),
    map.init(Func_NA_Index),
    map.init(Func_MNA_Index).

predicate_table_optimize(PredicateTable0, PredicateTable) :-
    PredicateTable0 = predicate_table(A, B, C, D,
        Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
        Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
    map.optimize(Pred_N_Index0, Pred_N_Index),
    map.optimize(Pred_NA_Index0, Pred_NA_Index),
    map.optimize(Pred_MNA_Index0, Pred_MNA_Index),
    map.optimize(Func_N_Index0, Func_N_Index),
    map.optimize(Func_NA_Index0, Func_NA_Index),
    map.optimize(Func_MNA_Index0, Func_MNA_Index),
    PredicateTable = predicate_table(A, B, C, D,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index).

predicate_table_get_preds(PredicateTable, PredicateTable ^ preds).

predicate_table_set_preds(Preds, PredicateTable,
    PredicateTable ^ preds := Preds).

predicate_table_get_predids(PredicateTable, PredicateTable ^ pred_ids).

predicate_table_remove_predid(PredId, PredicateTable0, PredicateTable) :-
    list.delete_all(PredicateTable0 ^ pred_ids, PredId, PredIds),
    PredicateTable = PredicateTable0 ^ pred_ids := PredIds.

predicate_table_remove_predicate(PredId, PredicateTable0, PredicateTable) :-
    PredicateTable0 = predicate_table(Preds0, NextPredId, PredIds0,
        AccessibilityTable0,
        PredN0, PredNA0, PredMNA0, FuncN0, FuncNA0, FuncMNA0),
    list.delete_all(PredIds0, PredId, PredIds),
    map.det_remove(Preds0, PredId, PredInfo, Preds),
    map.det_remove(AccessibilityTable0, PredId, _, AccessibilityTable),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        IsPredOrFunc = predicate,
        predicate_table_remove_from_index(Module, Name, Arity, PredId,
            PredN0, PredN, PredNA0, PredNA, PredMNA0, PredMNA),
        PredicateTable = predicate_table(Preds, NextPredId, PredIds,
            AccessibilityTable,
            PredN, PredNA, PredMNA, FuncN0, FuncNA0, FuncMNA0)
    ;
        IsPredOrFunc = function,
        FuncArity = Arity - 1,
        predicate_table_remove_from_index(Module, Name, FuncArity,
            PredId, FuncN0, FuncN, FuncNA0, FuncNA,
            FuncMNA0, FuncMNA),
        PredicateTable = predicate_table(Preds, NextPredId, PredIds,
            AccessibilityTable,
            PredN0, PredNA0, PredMNA0, FuncN, FuncNA, FuncMNA)
    ).

:- pred predicate_table_remove_from_index(module_name::in, string::in, int::in,
    pred_id::in, name_index::in, name_index::out,
    name_arity_index::in, name_arity_index::out,
    module_name_arity_index::in, module_name_arity_index::out) is det.

predicate_table_remove_from_index(Module, Name, Arity, PredId,
        !N, !NA, !MNA) :-
    do_remove_from_index(Name, PredId, !N),
    do_remove_from_index(Name / Arity, PredId, !NA),
    do_remove_from_m_n_a_index(Module, Name, Arity, PredId, !MNA).

:- pred do_remove_from_index(T::in, pred_id::in,
    map(T, list(pred_id))::in, map(T, list(pred_id))::out) is det.

do_remove_from_index(T, PredId, Index0, Index) :-
    ( map.search(Index0, T, NamePredIds0) ->
        list.delete_all(NamePredIds0, PredId, NamePredIds),
        (
            NamePredIds = [],
            map.delete(Index0, T, Index)
        ;
            NamePredIds = [_ | _],
            map.det_update(Index0, T, NamePredIds, Index)
        )
    ;
        Index = Index0
    ).

:- pred do_remove_from_m_n_a_index(module_name::in, string::in, int::in,
    pred_id::in, module_name_arity_index::in, module_name_arity_index::out)
    is det.

do_remove_from_m_n_a_index(Module, Name, Arity, PredId, MNA0, MNA) :-
    map.lookup(MNA0, Module - Name, Arities0),
    map.lookup(Arities0, Arity, PredIds0),
    list.delete_all(PredIds0, PredId, PredIds),
    (
        PredIds = [],
        map.delete(Arities0, Arity, Arities),
        ( map.is_empty(Arities) ->
            map.delete(MNA0, Module - Name, MNA)
        ;
            map.det_update(MNA0, Module - Name, Arities, MNA)
        )
    ;
        PredIds = [_ | _],
        map.det_update(Arities0, Arity, PredIds, Arities),
        map.det_update(MNA0, Module - Name, Arities, MNA)
    ).

%-----------------------------------------------------------------------------%

predicate_table_reverse_predids(PredicateTable0, PredicateTable) :-
    list.reverse(PredicateTable0 ^ pred_ids, PredIds),
    PredicateTable = PredicateTable0 ^ pred_ids := PredIds.

%-----------------------------------------------------------------------------%

predicate_table_search_sym(PredicateTable, may_be_partially_qualified,
        unqualified(Name), PredIdList) :-
    predicate_table_search_name(PredicateTable, Name, PredIdList).
predicate_table_search_sym(PredicateTable, IsFullyQualified,
        qualified(Module, Name), PredIdList) :-
    predicate_table_search_module_name(PredicateTable, IsFullyQualified,
        Module, Name, PredIdList),
    PredIdList = [_ | _].

predicate_table_search_pred_sym(PredicateTable, may_be_partially_qualified,
        unqualified(Name), PredIdList) :-
    predicate_table_search_pred_name(PredicateTable, Name, PredIdList).
predicate_table_search_pred_sym(PredicateTable, IsFullyQualified,
        qualified(Module, Name), PredIdList) :-
    predicate_table_search_pred_module_name(PredicateTable,
        IsFullyQualified, Module, Name, PredIdList),
    PredIdList = [_ | _].

predicate_table_search_func_sym(PredicateTable, may_be_partially_qualified,
        unqualified(Name), PredIdList) :-
    predicate_table_search_func_name(PredicateTable, Name, PredIdList).
predicate_table_search_func_sym(PredicateTable, IsFullyQualified,
        qualified(Module, Name), PredIdList) :-
    predicate_table_search_func_module_name(PredicateTable,
        IsFullyQualified, Module, Name, PredIdList),
    PredIdList = [_ | _].

%-----------------------------------------------------------------------------%

predicate_table_search_sym_arity(PredicateTable, IsFullyQualified,
        qualified(Module, Name), Arity, PredIdList) :-
    predicate_table_search_m_n_a(PredicateTable,
        IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_sym_arity(PredicateTable, may_be_partially_qualified,
        unqualified(Name), Arity, PredIdList) :-
    predicate_table_search_name_arity(PredicateTable, Name, Arity, PredIdList).

predicate_table_search_pred_sym_arity(PredicateTable, IsFullyQualified,
        qualified(Module, Name), Arity, PredIdList) :-
    predicate_table_search_pred_m_n_a(PredicateTable,
        IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_pred_sym_arity(PredicateTable,
        may_be_partially_qualified, unqualified(Name),
        Arity, PredIdList) :-
    predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
        PredIdList).

predicate_table_search_func_sym_arity(PredicateTable, IsFullyQualified,
        qualified(Module, Name), Arity, PredIdList) :-
    predicate_table_search_func_m_n_a(PredicateTable,
        IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_func_sym_arity(PredicateTable,
        may_be_partially_qualified, unqualified(Name),
        Arity, PredIdList) :-
    predicate_table_search_func_name_arity(PredicateTable, Name, Arity,
        PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_search_name(PredicateTable, Name, PredIds) :-
    ( predicate_table_search_pred_name(PredicateTable, Name, PredPredIds0) ->
        PredPredIds = PredPredIds0
    ;
        PredPredIds = []
    ),
    ( predicate_table_search_func_name(PredicateTable, Name, FuncPredIds0) ->
        FuncPredIds = FuncPredIds0
    ;
        FuncPredIds = []
    ),
    list.append(FuncPredIds, PredPredIds, PredIds),
    PredIds = [_ | _].

predicate_table_search_pred_name(PredicateTable, PredName, PredIds) :-
    map.search(PredicateTable ^ pred_name_index, PredName, PredIds).

predicate_table_search_func_name(PredicateTable, FuncName, PredIds) :-
    map.search(PredicateTable ^ func_name_index, FuncName, PredIds).

%-----------------------------------------------------------------------------%

:- pred predicate_table_search_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is semidet.

predicate_table_search_module_name(PredicateTable, IsFullyQualified,
        Module, Name, PredIds) :-
    (
        predicate_table_search_pred_module_name(PredicateTable,
            IsFullyQualified, Module, Name, PredPredIds0)
    ->
        PredPredIds = PredPredIds0
    ;
        PredPredIds = []
    ),
    (
        predicate_table_search_func_module_name(PredicateTable,
            IsFullyQualified, Module, Name, FuncPredIds0)
    ->
        FuncPredIds = FuncPredIds0
    ;
        FuncPredIds = []
    ),
    list.append(FuncPredIds, PredPredIds, PredIds),
    PredIds = [_ | _].

:- pred predicate_table_search_pred_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is semidet.

predicate_table_search_pred_module_name(PredicateTable, IsFullyQualified,
        Module, PredName, PredIds) :-
    Pred_MNA_Index = PredicateTable ^ pred_module_name_arity_index,
    map.search(Pred_MNA_Index, Module - PredName, Arities),
    map.values(Arities, PredIdLists),
    list.condense(PredIdLists, PredIds0),
    maybe_filter_pred_ids_matching_module(IsFullyQualified,
        Module, PredicateTable, PredIds0, PredIds).

:- pred predicate_table_search_func_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is semidet.

predicate_table_search_func_module_name(PredicateTable, IsFullyQualified,
        Module, FuncName, PredIds) :-
    Func_MNA_Index = PredicateTable ^ func_module_name_arity_index,
    map.search(Func_MNA_Index, Module - FuncName, Arities),
    map.values(Arities, PredIdLists),
    list.condense(PredIdLists, PredIds0),
    maybe_filter_pred_ids_matching_module(IsFullyQualified,
        Module, PredicateTable, PredIds0, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_name_arity(PredicateTable, Name, Arity, PredIds) :-
    (
        predicate_table_search_pred_name_arity(PredicateTable,
            Name, Arity, PredPredIds0)
    ->
        PredPredIds = PredPredIds0
    ;
        PredPredIds = []
    ),
    (
        predicate_table_search_func_name_arity(PredicateTable,
            Name, Arity, FuncPredIds0)
    ->
        FuncPredIds = FuncPredIds0
    ;
        FuncPredIds = []
    ),
    list.append(FuncPredIds, PredPredIds, PredIds),
    PredIds = [_ | _].

predicate_table_search_pred_name_arity(PredicateTable, PredName, Arity,
        PredIds) :-
    PredNameArityIndex = PredicateTable ^ pred_name_arity_index,
    map.search(PredNameArityIndex, PredName / Arity, PredIds).

predicate_table_search_func_name_arity(PredicateTable, FuncName, Arity,
        PredIds) :-
    FuncNameArityIndex = PredicateTable ^ func_name_arity_index,
    map.search(FuncNameArityIndex, FuncName / Arity, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_m_n_a(PredicateTable, IsFullyQualified,
        Module, Name, Arity, PredIds) :-
    (
        predicate_table_search_pred_m_n_a(PredicateTable,
            IsFullyQualified, Module, Name, Arity, PredPredIds0)
    ->
        PredPredIds = PredPredIds0
    ;
        PredPredIds = []
    ),
    (
        predicate_table_search_func_m_n_a(PredicateTable,
            IsFullyQualified, Module, Name, Arity, FuncPredIds0)
    ->
        FuncPredIds = FuncPredIds0
    ;
        FuncPredIds = []
    ),
    list.append(FuncPredIds, PredPredIds, PredIds),
    PredIds = [_ | _].

predicate_table_search_pred_m_n_a(PredicateTable, IsFullyQualified,
        Module, PredName, Arity, !:PredIds) :-
    P_MNA_Index = PredicateTable ^ pred_module_name_arity_index,
    map.search(P_MNA_Index, Module - PredName, ArityIndex),
    map.search(ArityIndex, Arity, !:PredIds),
    maybe_filter_pred_ids_matching_module(IsFullyQualified, Module,
        PredicateTable, !PredIds).

predicate_table_search_func_m_n_a(PredicateTable, IsFullyQualified,
        Module, FuncName, Arity, !:PredIds) :-
    F_MNA_Index = PredicateTable ^ func_module_name_arity_index,
    map.search(F_MNA_Index, Module - FuncName, ArityIndex),
    map.search(ArityIndex, Arity, !:PredIds),
    maybe_filter_pred_ids_matching_module(IsFullyQualified, Module,
        PredicateTable, !PredIds).

:- pred maybe_filter_pred_ids_matching_module(is_fully_qualified::in,
    module_name::in, predicate_table::in,
    list(pred_id)::in, list(pred_id)::out) is det.

maybe_filter_pred_ids_matching_module(may_be_partially_qualified, _, _,
        !PredIds).
maybe_filter_pred_ids_matching_module(is_fully_qualified, ModuleName,
        PredicateTable, !PredIds) :-
    predicate_table_get_preds(PredicateTable, Preds),
    list.filter(pred_id_matches_module(Preds, ModuleName), !PredIds).

:- pred pred_id_matches_module(pred_table::in, module_name::in, pred_id::in)
    is semidet.

pred_id_matches_module(Preds, ModuleName, PredId) :-
    map.lookup(Preds, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo).

%-----------------------------------------------------------------------------%

predicate_table_search_pf_m_n_a(PredicateTable, IsFullyQualified,
        predicate, Module, Name, Arity, PredIds) :-
    predicate_table_search_pred_m_n_a(PredicateTable, IsFullyQualified,
        Module, Name, Arity, PredIds).
predicate_table_search_pf_m_n_a(PredicateTable, IsFullyQualified,
        function, Module, Name, Arity, PredIds) :-
    FuncArity = Arity - 1,
    predicate_table_search_func_m_n_a(PredicateTable, IsFullyQualified,
        Module, Name, FuncArity, PredIds).

predicate_table_search_pf_name_arity(PredicateTable, predicate, Name, Arity,
        PredIds) :-
    predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
        PredIds).
predicate_table_search_pf_name_arity(PredicateTable, function, Name, Arity,
        PredIds) :-
    FuncArity = Arity - 1,
    predicate_table_search_func_name_arity(PredicateTable, Name, FuncArity,
        PredIds).

predicate_table_search_pf_sym_arity(PredicateTable, IsFullyQualified,
        PredOrFunc, qualified(Module, Name), Arity, PredIdList) :-
    predicate_table_search_pf_m_n_a(PredicateTable,
        IsFullyQualified, PredOrFunc, Module, Name, Arity, PredIdList).
predicate_table_search_pf_sym_arity(PredicateTable, may_be_partially_qualified,
        PredOrFunc, unqualified(Name), Arity, PredIdList) :-
    predicate_table_search_pf_name_arity(PredicateTable, PredOrFunc,
        Name, Arity, PredIdList).

predicate_table_search_pf_sym(PredicateTable, IsFullyQualified, predicate,
        SymName, PredIdList) :-
    predicate_table_search_pred_sym(PredicateTable, IsFullyQualified,
        SymName, PredIdList).
predicate_table_search_pf_sym(PredicateTable, IsFullyQualified,
        function, SymName, PredIdList) :-
    predicate_table_search_func_sym(PredicateTable, IsFullyQualified,
        SymName, PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_restrict(PartialQualInfo, PredIds, OrigPredicateTable,
        PredicateTable) :-
    predicate_table_reset(OrigPredicateTable, PredicateTable0),
    predicate_table_get_preds(OrigPredicateTable, Preds),
    AccessibilityTable = OrigPredicateTable ^ accessibility_table,

    % Note that we use foldr here rather than foldl, so that the PredIds list
    % in the predicate table is the same as the PredIds list argument here
    % (if we used foldl, it would get reversed, since each new predicate
    % inserted into the table gets its pred_id added at the start of the list).
    list.foldr(reinsert_for_restrict(PartialQualInfo, Preds,
        AccessibilityTable), PredIds, PredicateTable0, PredicateTable).

:- pred reinsert_for_restrict(partial_qualifier_info::in, pred_table::in,
    accessibility_table::in, pred_id::in,
    predicate_table::in, predicate_table::out) is det.

reinsert_for_restrict(PartialQualInfo, Preds, AccessibilityTable, PredId,
        !PredicateTable) :-
    PredInfo = map.lookup(Preds, PredId),
    Access = map.lookup(AccessibilityTable, PredId),
    Access = access(Unqualified, PartiallyQualified),
    (
        Unqualified = yes,
        NeedQual = may_be_unqualified
    ;
        Unqualified = no,
        NeedQual = must_be_qualified
    ),
    (
        PartiallyQualified = yes,
        MaybeQualInfo = yes(PartialQualInfo)
    ;
        PartiallyQualified = no,
        MaybeQualInfo = no
    ),
    do_predicate_table_insert(yes(PredId), PredInfo, NeedQual, MaybeQualInfo,
        _, !PredicateTable).

:- pred predicate_table_reset(predicate_table::in, predicate_table::out)
    is det.

predicate_table_reset(PredicateTable0, PredicateTable) :-
    NextPredId = PredicateTable0 ^ next_pred_id,
    PredicateTable = predicate_table(map.init, NextPredId, [], map.init,
        map.init, map.init, map.init, map.init, map.init, map.init).

%-----------------------------------------------------------------------------%

predicate_table_insert(PredInfo, PredId, !PredicateTable) :-
    do_predicate_table_insert(no, PredInfo, must_be_qualified, no, PredId,
        !PredicateTable).

predicate_table_insert_qual(PredInfo, NeedQual, QualInfo, PredId,
        !PredicateTable) :-
    do_predicate_table_insert(no, PredInfo, NeedQual, yes(QualInfo), PredId,
        !PredicateTable).

:- pred do_predicate_table_insert(maybe(pred_id)::in, pred_info::in,
    need_qualifier::in, maybe(partial_qualifier_info)::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

do_predicate_table_insert(MaybePredId, PredInfo, NeedQual, MaybeQualInfo,
        PredId, !PredicateTable) :-
    !.PredicateTable = predicate_table(Preds0, NextPredId0, PredIds0,
        AccessibilityTable0,
        Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
        Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    (
        MaybePredId = yes(PredId),
        NextPredId = NextPredId0
    ;
        % Allocate a new pred_id.
        MaybePredId = no,
        PredId = NextPredId0,
        hlds_pred.next_pred_id(PredId, NextPredId)
    ),
    % Insert the pred_id into either the function or predicate indices,
    % as appropriate.
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        PredOrFunc = predicate,
        predicate_table_do_insert(Module, Name, Arity,
            NeedQual, MaybeQualInfo, PredId,
            AccessibilityTable0, AccessibilityTable,
            Pred_N_Index0, Pred_N_Index,
            Pred_NA_Index0, Pred_NA_Index,
            Pred_MNA_Index0, Pred_MNA_Index),

        Func_N_Index = Func_N_Index0,
        Func_NA_Index = Func_NA_Index0,
        Func_MNA_Index = Func_MNA_Index0
    ;
        PredOrFunc = function,
        FuncArity = Arity - 1,
        predicate_table_do_insert(Module, Name, FuncArity,
            NeedQual, MaybeQualInfo, PredId,
            AccessibilityTable0, AccessibilityTable,
            Func_N_Index0, Func_N_Index,
            Func_NA_Index0, Func_NA_Index,
            Func_MNA_Index0, Func_MNA_Index),

        Pred_N_Index = Pred_N_Index0,
        Pred_NA_Index = Pred_NA_Index0,
        Pred_MNA_Index = Pred_MNA_Index0
    ),

    % Insert the pred_id into the pred_id list.
    PredIds = [PredId | PredIds0],

    % Save the pred_info for this pred_id.
    map.det_insert(Preds0, PredId, PredInfo, Preds),

    !:PredicateTable = predicate_table(Preds, NextPredId, PredIds,
        AccessibilityTable,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index).

:- pred predicate_table_do_insert(module_name::in, string::in, arity::in,
    need_qualifier::in, maybe(partial_qualifier_info)::in, pred_id::in,
    accessibility_table::in, accessibility_table::out,
    name_index::in, name_index::out,
    name_arity_index::in, name_arity_index::out,
    module_name_arity_index::in, module_name_arity_index::out) is det.

predicate_table_do_insert(Module, Name, Arity, NeedQual, MaybeQualInfo,
        PredId, !AccessibilityTable, !N_Index, !NA_Index, !MNA_Index) :-
    (
        NeedQual = may_be_unqualified,
        % Insert the unqualified name into the name index.
        svmulti_map.set(Name, PredId, !N_Index),

        % Insert the unqualified name/arity into the name/arity index.
        NA = Name / Arity,
        svmulti_map.set(NA, PredId, !NA_Index),

        AccessibleByUnqualifiedName = yes
    ;
        NeedQual = must_be_qualified,
        AccessibleByUnqualifiedName = no
    ),
    (
        MaybeQualInfo = yes(QualInfo),

        % Insert partially module-qualified versions of the name into the
        % module.name/arity index.
        get_partial_qualifiers(Module, QualInfo, PartialQuals),
        list.foldl((pred(AncModule::in, MNAs0::in, MNAs::out) is det :-
                insert_into_mna_index(AncModule, Name, Arity, PredId,
                    MNAs0, MNAs)
            ), PartialQuals, !MNA_Index),

        AccessibleByPartiallyQualifiedNames = yes
    ;
        MaybeQualInfo = no,
        AccessibleByPartiallyQualifiedNames = no
    ),
    % Insert the fully-qualified name into the module.name/arity index.
    insert_into_mna_index(Module, Name, Arity, PredId, !MNA_Index),
    Access = access(AccessibleByUnqualifiedName,
        AccessibleByPartiallyQualifiedNames),
    svmap.set(PredId, Access, !AccessibilityTable).

:- pred insert_into_mna_index(module_name::in, string::in, arity::in,
    pred_id::in, module_name_arity_index::in,
    module_name_arity_index::out) is det.

insert_into_mna_index(Module, Name, Arity, PredId, !MNA_Index) :-
    ( map.search(!.MNA_Index, Module - Name, MN_Arities0) ->
        multi_map.set(MN_Arities0, Arity, PredId, MN_Arities),
        svmap.det_update(Module - Name, MN_Arities, !MNA_Index)
    ;
        map.init(MN_Arities0),
        map.det_insert(MN_Arities0, Arity, [PredId], MN_Arities),
        svmap.det_insert(Module - Name, MN_Arities, !MNA_Index)
    ).

%-----------------------------------------------------------------------------%

resolve_pred_overloading(ModuleInfo, CallerMarkers, ArgTypes, TVarSet,
        PredName0, PredName, PredId) :-
    % Note: calls to preds declared in `.opt' files should always be
    % module qualified, so they should not be considered
    % when resolving overloading.

    module_info_get_predicate_table(ModuleInfo, PredTable),
    (
        predicate_table_search_pred_sym(PredTable,
            calls_are_fully_qualified(CallerMarkers), PredName0, PredIds0)
    ->
        PredIds = PredIds0
    ;
        PredIds = []
    ),

    % Check if there any of the candidate pred_ids have argument/return types
    % which subsume the actual argument/return types of this function call.
    (
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ArgTypes,
            no, PredId1, PredName1)
    ->
        PredId = PredId1,
        PredName = PredName1
    ;
        % If there is no matching predicate for this call, then this predicate
        % must have a type error which should have been caught by typechecking.
        unexpected(this_file, "type error in pred call: no matching pred")
    ).

find_matching_pred_id(ModuleInfo, [PredId | PredIds], TVarSet, ArgTypes,
        MaybeConstraintSearch, ThePredId, PredName) :-
    (
        % Lookup the argument types of the candidate predicate
        % (or the argument types + return type of the candidate function).
        %
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_arg_types(PredInfo, PredTVarSet, PredExistQVars0,
            PredArgTypes0),
        pred_info_get_tvar_kinds(PredInfo, PredKindMap),

        arg_type_list_subsumes(TVarSet, ArgTypes, PredTVarSet, PredKindMap,
            PredExistQVars0, PredArgTypes0),

        (
            MaybeConstraintSearch = no
        ;
            MaybeConstraintSearch = yes(ConstraintSearch),
            % Lookup the universal constraints on the condidate predicate.
            pred_info_get_class_context(PredInfo, ProgConstraints),
            ProgConstraints = constraints(UnivConstraints, _),
            list.length(UnivConstraints, NumConstraints),
            ConstraintSearch(NumConstraints, ProvenConstraints),
            univ_constraints_match(ProvenConstraints, UnivConstraints)
        )
    ->
        % We've found a matching predicate.
        % Was there was more than one matching predicate/function?

        PName = pred_info_name(PredInfo),
        Module = pred_info_module(PredInfo),
        PredName = qualified(Module, PName),
        (
            find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ArgTypes,
                MaybeConstraintSearch, _OtherPredId, _)
        ->
            % XXX this should report an error properly, not
            % via error/1
            unexpected(this_file, "Type error in predicate call: " ++
                "unresolvable predicate overloading. " ++
                "You need to use an explicit module qualifier. " ++
                "Compile with -V to find out where.")
        ;
            ThePredId = PredId
        )
    ;
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ArgTypes,
            MaybeConstraintSearch, ThePredId, PredName)
    ).

    % Check that the universal constraints proven in the caller match the
    % constraints on the callee.
    %
    % XXX we should rename apart the callee constraints and check that the
    % proven constraints are instances of them.  This would give us better
    % overloading resolution.  For the moment, we just check that the names
    % and arities match, which is sufficient to prevent any compiler aborts
    % in later stages.
    %
:- pred univ_constraints_match(list(prog_constraint)::in,
    list(prog_constraint)::in) is semidet.

univ_constraints_match([], []).
univ_constraints_match([ProvenConstraint | ProvenConstraints],
        [CalleeConstraint | CalleeConstraints]) :-
    ProvenConstraint = constraint(Name, ProvenArgs),
    list.length(ProvenArgs, Arity),
    CalleeConstraint = constraint(Name, CalleeArgs),
    list.length(CalleeArgs, Arity),
    univ_constraints_match(ProvenConstraints, CalleeConstraints).

get_pred_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
        ArgTypes, ModuleInfo, PredId) :-
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    list.length(ArgTypes, Arity),
    (
        predicate_table_search_pf_sym_arity(PredicateTable, IsFullyQualified,
            PredOrFunc, SymName, Arity, PredIds),
        % Resolve overloading using the argument types.
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ArgTypes, no,
            PredId0, _PredName)
    ->
        PredId = PredId0
    ;
        % Undefined/invalid pred or func.
        fail
    ).

get_pred_id_and_proc_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
        ArgTypes, ModuleInfo, PredId, ProcId) :-
    (
        get_pred_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
            ArgTypes, ModuleInfo, PredId0)
    ->
        PredId = PredId0
    ;
        % Undefined/invalid pred or func. The type-checker should ensure
        % that this never happens
        list.length(ArgTypes, Arity),
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        NameStr = sym_name_to_string(SymName),
        string.int_to_string(Arity, ArityString),
        string.append_list(["get_pred_id_and_proc_id: ",
            "undefined/invalid ", PredOrFuncStr,
            "\n`", NameStr, "/", ArityString, "'"], Msg),
        unexpected(this_file, Msg)
    ),
    get_proc_id(ModuleInfo, PredId, ProcId).

get_proc_id(ModuleInfo, PredId, ProcId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    ( ProcIds = [ProcId0] ->
        ProcId = ProcId0
    ;
        Name = pred_info_name(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        string.int_to_string(Arity, ArityString),
        (
            ProcIds = [],
            string.append_list([
                "cannot take address of ", PredOrFuncStr,
                "\n`", Name, "/", ArityString, "' with no modes.\n",
                "(Sorry, confused by earlier errors -- bailing out.)"],
                Message)
        ;
            ProcIds = [_ | _],
            string.append_list([
                "sorry, not implemented: ",
                "taking address of ", PredOrFuncStr,
                "\n`", Name, "/", ArityString, "' with multiple modes.\n",
                "(use an explicit lambda expression instead)"],
                Message)
        ),
        unexpected(this_file, Message)
    ).

lookup_builtin_pred_proc_id(Module, ModuleName, ProcName, PredOrFunc,
        Arity, ModeNo, PredId, ProcId) :-
    module_info_get_predicate_table(Module, PredTable),
    (
        (
            PredOrFunc = predicate,
            predicate_table_search_pred_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, Arity, [PredId0])
        ;
            PredOrFunc = function,
            predicate_table_search_func_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, Arity, [PredId0])
        )
    ->
        PredId = PredId0
    ;
        % Some of the table builtins are polymorphic, and for them we need
        % to subtract one from the arity to take into account the type_info
        % argument. XXX The caller should supply us with the exact arity.
        % Guessing how many of the arguments are typeinfos and/or
        % typeclass_infos, as this code here does, is error-prone as well as
        % inefficient.
        (
            PredOrFunc = predicate,
            predicate_table_search_pred_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, Arity - 1, [PredId0])
        ;
            PredOrFunc = function,
            predicate_table_search_func_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, Arity - 1, [PredId0])
        )
    ->
        PredId = PredId0
    ;
        string.int_to_string(Arity, ArityS),
        string.append_list(["can't locate ", ProcName, "/", ArityS],
            ErrorMessage),
        unexpected(this_file, ErrorMessage)
    ),
    module_info_pred_info(Module, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    (
        ModeNo = only_mode,
        ( ProcIds = [ProcId0] ->
            ProcId = ProcId0
        ;
            unexpected(this_file,
                string.format("expected single mode for %s/%d",
                    [s(ProcName), i(Arity)]))
        )
    ;
        ModeNo = mode_no(N),
        ( list.index0(ProcIds, N, ProcId0) ->
            ProcId = ProcId0
        ;
            unexpected(this_file,
                string.format("there is no mode %d for %s/%d",
                    [i(N), s(ProcName), i(Arity)]))
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "pred_table.m".

%-----------------------------------------------------------------------------%
