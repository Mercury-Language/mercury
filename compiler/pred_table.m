%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pred_table.m.
% Main authors: fjh, conway.
%
% This module defines the part of the High Level Data Structure or HLDS
% that allows the compiler to look up predicates by name (qualified,
% unqualified or some mixture) and/or arity.
%
%---------------------------------------------------------------------------%

:- module hlds.pred_table.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set_tree234.

%---------------------------------------------------------------------------%
%
% The data structure that this module manages is the predicate_table.
% This contains the pred_table, which maps pred_ids to pred_infos, but
% it also contains several different kinds of index structures that
% allow predicates and functions to be looked up by various combinations
% of name, module qualifier on the name, and arity.
%
% NOTE Almost all of this module manages the indexes, so the name
% of the module really should be predicate_table.m, not pred_table.m.
%
% NOTE On the other hand, it would be even better if the names of those
% two data structures actually gave a clue to the difference between them ...
%

:- type predicate_table.

:- type pred_id_table == map(pred_id, pred_info).

%---------------------------------------------------------------------------%
%
% The basic operations that create and update the predicate_table.
%

    % Various predicates for accessing the predicate_table type.
    % The predicate_table holds information about the predicates
    % and functions defined in this module or imported from other modules.
    % The primary key for this table is the `pred_id', but there
    % are also secondary indexes on each of name, name+arity, and
    % module+name+arity, for both functions and predicates.

    % Initialize the predicate table.
    %
:- pred predicate_table_init(predicate_table::out) is det.

    % predicate_table_insert(PredTable0, PredInfo,
    %   NeedQual, PartialQualInfo, PredId, PredTable):
    %
    % Insert PredInfo into PredTable0 and assign it a new pred_id.
    % You should either check beforehand that the predicate doesn't
    % already occur in the table, or ensure it by construction.
    %
:- pred predicate_table_insert_qual(pred_info::in, need_qualifier::in,
    partial_qualifier_info::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

    % Equivalent to predicate_table_insert_qual/6, except that only the
    % fully qualified version of the predicate will be inserted into the
    % predicate symbol table. This is useful for creating compiler-generated
    % predicates which will only ever be accessed via fully qualified names.
    %
:- pred predicate_table_insert(pred_info::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

    % Delete the given predicate, not just from the pred_id_table, but also
    % from all the indexes.
    %
:- pred predicate_table_remove_predicate(pred_id::in,
    predicate_table::in, predicate_table::out) is det.

%---------------------------------------------------------------------------%
%
% Maintenance of the list of valid predicates.
%

    % Get a set of all the valid pred_ids in the predicate_table.
    % (Predicates whose definition contains a type error, etc.
    % get removed from this list, so that later passes can rely
    % on the predicates in this list being type-correct, etc.)
    %
:- pred predicate_table_get_valid_pred_id_set(predicate_table::in,
    set_tree234(pred_id)::out) is det.

    % Remove one or more pred_ids from the valid list.
    %
:- pred predicate_table_make_pred_id_invalid(pred_id::in,
    predicate_table::in, predicate_table::out) is det.
:- pred predicate_table_make_pred_ids_invalid(list(pred_id)::in,
    predicate_table::in, predicate_table::out) is det.

%---------------------------------------------------------------------------%
%
% Operations on the predicate_table as a whole.
%

    % Balance all the binary trees in the predicate table
    %
:- pred predicate_table_optimize(predicate_table::in, predicate_table::out)
    is det.

    % Restrict the predicate table to the list of predicates. This predicate
    % should only be used when the set of predicates to restrict the table to
    % is significantly smaller than the predicate_table size, as rather than
    % removing entries from the table it builds a new table from scratch.
    %
:- pred predicate_table_restrict(partial_qualifier_info::in,
    list(pred_id)::in, predicate_table::in, predicate_table::out) is det.

%---------------------------------------------------------------------------%
%
% Raw access to the pred_id_table.
%

    % Get the pred_id->pred_info map.
    %
:- pred predicate_table_get_pred_id_table(predicate_table::in,
    pred_id_table::out) is det.

    % Set the pred_id->pred_info map.
    % NB You shouldn't modify the keys in this table, only
    % use predicate_table_insert{,_qual}, predicate_table_remove_predicate,
    % and predicate_table_make_pred_id_invalid.
    %
:- pred predicate_table_set_pred_id_table(pred_id_table::in,
    predicate_table::in, predicate_table::out) is det.

%---------------------------------------------------------------------------%
%
% Lookups in the predicate_table.
%

    % Search the table for (sym) predicates or functions (pred) predicates only
    % or (func) functions only matching this (possibly module-qualified)
    % sym_name. When searching for functions, the arity used is the arity of
    % the function itself, not the arity N+1 predicate that it gets
    % converted to.
    %
:- pred predicate_table_lookup_sym(predicate_table::in, is_fully_qualified::in,
    sym_name::in, list(pred_id)::out) is det.
:- pred predicate_table_lookup_pred_sym(predicate_table::in,
    is_fully_qualified::in, sym_name::in, list(pred_id)::out) is det.
:- pred predicate_table_lookup_func_sym(predicate_table::in,
    is_fully_qualified::in, sym_name::in, list(pred_id)::out) is det.

    % Search the table for
    % (in the sym_arity version) predicates or functions,
    % (in the pred_sym_arity version) predicates only, or
    % (in the func_sym_arity version) functions
    % only matching this (possibly module-qualified) sym_name and arity.
    % When searching for functions, the arity used is the arity of the
    % function itself, not the arity N+1 predicate that it gets converted to.
    %
:- pred predicate_table_lookup_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, user_arity::in, list(pred_id)::out)
    is det.
:- pred predicate_table_lookup_pred_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, user_arity::in, list(pred_id)::out)
    is det.
:- pred predicate_table_lookup_func_sym_arity(predicate_table::in,
    is_fully_qualified::in, sym_name::in, user_arity::in, list(pred_id)::out)
    is det.

    % These do the same job as the predicates without the "_one" suffix,
    % but they are intended to be used in situations where we know
    % that there should be exactly one match. If the number of matches
    % is any number other than one, we throw an exception.
    %
    % This works only if we are looking up predicates or functions
    % in modules whose contents we control, i.e. Mercury standard
    % library modules.)
    %
:- pred predicate_table_lookup_pred_sym_arity_one(predicate_table::in,
    is_fully_qualified::in, sym_name::in, user_arity::in, pred_id::out) is det.
:- pred predicate_table_lookup_func_sym_arity_one(predicate_table::in,
    is_fully_qualified::in, sym_name::in, user_arity::in, pred_id::out) is det.

    % Search the table for (name) predicates or functions
    % (pred_name) predicates only or (func_name) functions only
    % matching this name.
    %
:- pred predicate_table_lookup_name(predicate_table::in, string::in,
    list(pred_id)::out) is det.
:- pred predicate_table_lookup_pred_name(predicate_table::in, string::in,
    list(pred_id)::out) is det.
:- pred predicate_table_lookup_func_name(predicate_table::in, string::in,
    list(pred_id)::out) is det.

    % Search the table for (name) predicates or functions (pred_name)
    % predicates only or (func_name) functions only matching this name & arity.
    % When searching for functions, the arity used is the arity of the
    % function itself, not the arity N+1 predicate that it gets converted to.
    %
:- pred predicate_table_lookup_name_arity(predicate_table::in, string::in,
    user_arity::in, list(pred_id)::out) is det.
:- pred predicate_table_lookup_pred_name_arity(predicate_table::in, string::in,
    user_arity::in, list(pred_id)::out) is det.
:- pred predicate_table_lookup_func_name_arity(predicate_table::in, string::in,
    user_arity::in, list(pred_id)::out) is det.

    % Is the item known to be fully qualified? If so, a search for
    % `pred foo.bar/2' will not match `pred baz.foo.bar/2'.
:- type is_fully_qualified
    --->    is_fully_qualified
    ;       may_be_partially_qualified.

    % Search the table for (mna) predicates or functions (pred_mna) predicates
    % only or (func_mna) functions only matching this module, name & arity.
    % When searching for functions, the arity used is the arity of the
    % function itself, not the arity N+1 predicate that it gets converted to.
    % (`m_n_a' here is short for "module, name, arity".)
    %
    % Note that in cases (pred_mna) and (func_mna), it was once true that
    % there could only be one matching pred_id, since each predicate or
    % function could be uniquely identified by its module, name, arity,
    % and category (function/predicate). However this is no longer true,
    % due to nested modules. For example, `pred foo.bar/2' might match both
    % `pred mod1.foo.bar/2' and `pred mod2.foo.bar/2'.
    %
:- pred predicate_table_lookup_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, user_arity::in,
    list(pred_id)::out) is det.
:- pred predicate_table_lookup_pred_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, user_arity::in,
    list(pred_id)::out) is det.
:- pred predicate_table_lookup_func_m_n_a(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in, user_arity::in,
    list(pred_id)::out) is det.

    % Search the table for predicates or functions matching this pred_or_func
    % category, module, name, and arity. When searching for functions, the
    % arity used is the arity of the predicate that the function gets converted
    % to, i.e. the arity of the function plus one.
    % NOTE This is opposite to what happens with the lookup predicates
    % declared above!!
    %
:- pred predicate_table_lookup_pf_m_n_a(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, module_name::in, string::in,
    pred_form_arity::in, list(pred_id)::out) is det.

    % Search the table for predicates or functions matching this pred_or_func
    % category, name, and arity. When searching for functions, the arity used
    % is the arity of the predicate that the function gets converted to,
    % i.e. the arity of the function plus one.
    % NOTE This is opposite to what happens with the lookup predicates
    % declared above!!
    %
:- pred predicate_table_lookup_pf_name_arity(predicate_table::in,
    pred_or_func::in, string::in, pred_form_arity::in,
    list(pred_id)::out) is det.

    % Search the table for predicates or functions matching this pred_or_func
    % category, sym_name, and arity. When searching for functions, the arity
    % used is the arity of the predicate that the function gets converted to,
    % i.e. the arity of the function plus one.
    % NOTE This is opposite to what happens with the lookup predicates
    % declared above!!
    %
:- pred predicate_table_lookup_pf_sym_arity(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, sym_name::in,
    pred_form_arity::in, list(pred_id)::out) is det.

    % Search the table for predicates or functions matching
    % this pred_or_func category and sym_name.
    %
:- pred predicate_table_lookup_pf_sym(predicate_table::in,
    is_fully_qualified::in, pred_or_func::in, sym_name::in,
    list(pred_id)::out) is det.

%---------------------------------------------------------------------------%

    % Return a list of the predicates or functions with the given name
    % regardless of whether they are supposed to be accessible without
    % module qualification.
    %
:- pred predicate_table_lookup_pf_raw_name(predicate_table::in,
    pred_or_func::in, string::in, list(pred_id)::out) is det.

%---------------------------------------------------------------------------%

    % Find a predicate which matches the given name and argument types.
    % Abort if there is no matching pred.
    % Abort if there are multiple matching preds.
    %
:- pred resolve_pred_overloading(module_info::in, pred_markers::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in, prog_context::in,
    sym_name::in, sym_name::out, pred_id::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % A means to check that the required constraints are available, without
    % knowing in advance how many are required.
    %
:- type constraint_search == pred(int, list(prog_constraint)).
:- inst constraint_search == (pred(in, out) is semidet).

    % Find a predicate or function from the list of pred_ids which matches the
    % given name and argument types. If the constraint_search argument is
    % provided then also check that the class context is consistent with what
    % is expected. Fail if there is no matching pred. Abort if there are
    % multiple matching preds.
    %
:- pred find_matching_pred_id(module_info::in, list(pred_id)::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in,
    maybe(constraint_search)::in(maybe(constraint_search)), prog_context::in,
    pred_id::out, sym_name::out, list(error_spec)::out) is semidet.

%---------------------------------------------------------------------------%

    % Get the pred_id matching a higher-order term with
    % the given argument types, failing if none is found.
    %
:- pred get_pred_id_by_types(is_fully_qualified::in, sym_name::in,
    pred_or_func::in, tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in, module_info::in, prog_context::in,
    pred_id::out, list(error_spec)::out) is semidet.

    % Get the pred_id and proc_id matching a higher-order term with
    % the given argument types, aborting with an error if none is found.
    %
:- pred get_pred_id_and_proc_id_by_types(is_fully_qualified::in, sym_name::in,
    pred_or_func::in, tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in, module_info::in, prog_context::in,
    pred_id::out, proc_id::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Given a pred_id, return the single proc_id, aborting
    % if there are no modes or more than one mode.
    %
:- pred get_single_proc_id(module_info::in, pred_id::in, proc_id::out) is det.

%---------------------------------------------------------------------------%

:- type mode_no
    --->    only_mode           % The pred must have exactly one mode.
    ;       mode_no(int).       % The Nth mode, counting from 0.

:- pred lookup_builtin_pred_proc_id(module_info::in, module_name::in,
    string::in, pred_or_func::in, user_arity::in, mode_no::in,
    pred_id::out, proc_id::out) is det.

%---------------------------------------------------------------------------%

:-pred get_next_pred_id(predicate_table::in, pred_id::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module string.

:- type predicate_table
    --->    predicate_table(
                % Map from pred_id to pred_info.
                pt_pred_id_table                :: pred_id_table,

                % The next available pred_id.
                pt_next_pred_id                 :: pred_id,

                % The set of pred ids that may be processed further.
                % Every pred_id in valid_pred_ids must be a key in preds,
                % but it is ok for a key in pt_pred_id_table not to be in
                % pt_valid_pred_ids.
                pt_valid_pred_ids               :: set_tree234(pred_id),

                % Maps each pred_id to its accessibility by (partially)
                % unqualified names.
                pt_accessibility_table          :: accessibility_table,

                % Indexes on predicates and on functions.
                % - Map from pred/func name to pred_id.
                % - Map from pred/func name & arity to pred_id.
                % - Map from module, pred/func name & arity to pred_id.
                pt_pred_name_index              :: name_index,
                pt_pred_name_arity_index        :: name_arity_index,
                pt_pred_module_name_arity_index :: module_name_arity_index,
                pt_func_name_index              :: name_index,
                pt_func_name_arity_index        :: name_arity_index,
                pt_func_module_name_arity_index :: module_name_arity_index
            ).

:- type accessibility_table == map(pred_id, name_accessibility).

:- type name_accessibility
    --->    access(
                % Is this predicate accessible by its unqualified name?
                accessible_by_unqualified_name          :: bool,

                % Is this predicate accessible by any partially qualified
                % names?
                accessible_by_partially_qualified_names :: bool
            ).

:- type name_index == map(string, list(pred_id)).

:- type name_user_arity
    --->    name_user_arity(string, user_arity).
:- type name_arity_index == map(name_user_arity, list(pred_id)).

:- type module_and_name
    --->    module_and_name(module_name, string).

    % First search on module and name, then search on arity. We need these
    % two levels because typecheck.m, when processing higher order terms,
    % sees only an initial subsequence of the arguments, and does not know
    % the full arity.
    %
:- type module_name_arity_index ==
    map(module_and_name, multi_map(user_arity, pred_id)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

predicate_table_init(PredicateTable) :-
    map.init(PredIdTable),
    NextPredId = hlds_pred.initial_pred_id,
    ValidPredIds = set_tree234.init,
    map.init(AccessibilityTable),
    map.init(Pred_N_Index),
    map.init(Pred_NA_Index),
    map.init(Pred_MNA_Index),
    map.init(Func_N_Index),
    map.init(Func_NA_Index),
    map.init(Func_MNA_Index),
    PredicateTable = predicate_table(PredIdTable, NextPredId,
        ValidPredIds, AccessibilityTable,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index).

%---------------------%

predicate_table_insert_qual(PredInfo, NeedQual, QualInfo, PredId,
        !PredicateTable) :-
    do_predicate_table_insert(no, PredInfo, NeedQual, yes(QualInfo), PredId,
        !PredicateTable).

predicate_table_insert(PredInfo, PredId, !PredicateTable) :-
    do_predicate_table_insert(no, PredInfo, must_be_qualified, no, PredId,
        !PredicateTable).

:- pred do_predicate_table_insert(maybe(pred_id)::in, pred_info::in,
    need_qualifier::in, maybe(partial_qualifier_info)::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

do_predicate_table_insert(MaybePredId, PredInfo, NeedQual, MaybeQualInfo,
        PredId, !PredicateTable) :-
    !.PredicateTable = predicate_table(PredIdTable0, NextPredId0,
        ValidPredIds0, AccessibilityTable0,
        Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
        Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    UserArity = pred_info_user_arity(PredInfo),
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
        PredOrFunc = pf_predicate,
        predicate_table_do_insert(Module, Name, UserArity,
            NeedQual, MaybeQualInfo, PredId,
            AccessibilityTable0, AccessibilityTable,
            Pred_N_Index0, Pred_N_Index,
            Pred_NA_Index0, Pred_NA_Index,
            Pred_MNA_Index0, Pred_MNA_Index),
        Func_N_Index = Func_N_Index0,
        Func_NA_Index = Func_NA_Index0,
        Func_MNA_Index = Func_MNA_Index0
    ;
        PredOrFunc = pf_function,
        predicate_table_do_insert(Module, Name, UserArity,
            NeedQual, MaybeQualInfo, PredId,
            AccessibilityTable0, AccessibilityTable,
            Func_N_Index0, Func_N_Index,
            Func_NA_Index0, Func_NA_Index,
            Func_MNA_Index0, Func_MNA_Index),
        Pred_N_Index = Pred_N_Index0,
        Pred_NA_Index = Pred_NA_Index0,
        Pred_MNA_Index = Pred_MNA_Index0
    ),
    % Save the pred_info for this pred_id.
    map.det_insert(PredId, PredInfo, PredIdTable0, PredIdTable),
    set_tree234.insert(PredId, ValidPredIds0, ValidPredIds),
    !:PredicateTable = predicate_table(PredIdTable, NextPredId,
        ValidPredIds, AccessibilityTable,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index).

:- pred predicate_table_do_insert(module_name::in, string::in, user_arity::in,
    need_qualifier::in, maybe(partial_qualifier_info)::in, pred_id::in,
    accessibility_table::in, accessibility_table::out,
    name_index::in, name_index::out,
    name_arity_index::in, name_arity_index::out,
    module_name_arity_index::in, module_name_arity_index::out) is det.

predicate_table_do_insert(Module, Name, UserArity, NeedQual, MaybeQualInfo,
        PredId, !AccessibilityTable, !N_Index, !NA_Index, !MNA_Index) :-
    (
        NeedQual = may_be_unqualified,
        % Insert the unqualified name into the name index.
        multi_map.add(Name, PredId, !N_Index),

        % Insert the unqualified name/arity into the name/arity index.
        NameArity = name_user_arity(Name, UserArity),
        multi_map.add(NameArity, PredId, !NA_Index),

        AccessibleByUnqualifiedName = yes
    ;
        NeedQual = must_be_qualified,
        AccessibleByUnqualifiedName = no
    ),
    (
        MaybeQualInfo = yes(QualInfo),
        % Insert partially module-qualified versions of the name into the
        % module.name/arity index.
        get_partial_qualifiers(mq_not_used_in_interface, Module, QualInfo,
            PartialQuals),
        list.foldl(insert_into_mna_index(Name, UserArity, PredId),
            PartialQuals, !MNA_Index),
        AccessibleByPartiallyQualifiedNames = yes
    ;
        MaybeQualInfo = no,
        AccessibleByPartiallyQualifiedNames = no
    ),
    % Insert the fully qualified name into the module.name/arity index.
    insert_into_mna_index(Name, UserArity, PredId, Module, !MNA_Index),
    Access = access(AccessibleByUnqualifiedName,
        AccessibleByPartiallyQualifiedNames),
    map.set(PredId, Access, !AccessibilityTable).

:- pred insert_into_mna_index(string::in, user_arity::in, pred_id::in,
    module_name::in, module_name_arity_index::in, module_name_arity_index::out)
    is det.

insert_into_mna_index(Name, UserArity, PredId, Module, !MNA_Index) :-
    ModuleAndName = module_and_name(Module, Name),
    ( if map.search(!.MNA_Index, ModuleAndName, MN_Arities0) then
        multi_map.add(UserArity, PredId, MN_Arities0, MN_Arities),
        map.det_update(ModuleAndName, MN_Arities, !MNA_Index)
    else
        MN_Arities = map.singleton(UserArity, [PredId]),
        map.det_insert(ModuleAndName, MN_Arities, !MNA_Index)
    ).

%---------------------%

predicate_table_remove_predicate(PredId, PredicateTable0, PredicateTable) :-
    PredicateTable0 = predicate_table(PredIdTable0, NextPredId,
        ValidPredIds0, AccessibilityTable0,
        PredN0, PredNA0, PredMNA0, FuncN0, FuncNA0, FuncMNA0),
    set_tree234.delete(PredId, ValidPredIds0, ValidPredIds),
    map.det_remove(PredId, PredInfo, PredIdTable0, PredIdTable),
    map.det_remove(PredId, _, AccessibilityTable0, AccessibilityTable),
    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    UserArity = pred_info_user_arity(PredInfo),
    (
        IsPredOrFunc = pf_predicate,
        predicate_table_remove_from_index(Module, Name, UserArity, PredId,
            PredN0, PredN, PredNA0, PredNA, PredMNA0, PredMNA),
        PredicateTable = predicate_table(PredIdTable, NextPredId,
            ValidPredIds, AccessibilityTable,
            PredN, PredNA, PredMNA, FuncN0, FuncNA0, FuncMNA0)
    ;
        IsPredOrFunc = pf_function,
        predicate_table_remove_from_index(Module, Name, UserArity, PredId,
            FuncN0, FuncN, FuncNA0, FuncNA, FuncMNA0, FuncMNA),
        PredicateTable = predicate_table(PredIdTable, NextPredId,
            ValidPredIds, AccessibilityTable,
            PredN0, PredNA0, PredMNA0, FuncN, FuncNA, FuncMNA)
    ).

:- pred predicate_table_remove_from_index(module_name::in,
    string::in, user_arity::in, pred_id::in,
    name_index::in, name_index::out,
    name_arity_index::in, name_arity_index::out,
    module_name_arity_index::in, module_name_arity_index::out) is det.

predicate_table_remove_from_index(Module, Name, UserArity, PredId,
        !N, !NA, !MNA) :-
    do_remove_from_index(Name, PredId, !N),
    do_remove_from_index(name_user_arity(Name, UserArity), PredId, !NA),
    do_remove_from_m_n_a_index(module_and_name(Module, Name), UserArity,
        PredId, !MNA).

:- pred do_remove_from_index(T::in, pred_id::in,
    map(T, list(pred_id))::in, map(T, list(pred_id))::out) is det.

do_remove_from_index(T, PredId, !Index) :-
    ( if map.search(!.Index, T, NamePredIds0) then
        list.delete_all(NamePredIds0, PredId, NamePredIds),
        (
            NamePredIds = [],
            map.delete(T, !Index)
        ;
            NamePredIds = [_ | _],
            map.det_update(T, NamePredIds, !Index)
        )
    else
        true
    ).

:- pred do_remove_from_m_n_a_index(module_and_name::in, user_arity::in,
    pred_id::in, module_name_arity_index::in, module_name_arity_index::out)
    is det.

do_remove_from_m_n_a_index(ModuleAndName, UserArity, PredId, !MNA) :-
    map.lookup(!.MNA, ModuleAndName, UserArities0),
    map.lookup(UserArities0, UserArity, PredIds0),
    list.delete_all(PredIds0, PredId, PredIds),
    (
        PredIds = [],
        map.delete(UserArity, UserArities0, UserArities),
        ( if map.is_empty(UserArities) then
            map.delete(ModuleAndName, !MNA)
        else
            map.det_update(ModuleAndName, UserArities, !MNA)
        )
    ;
        PredIds = [_ | _],
        map.det_update(UserArity, PredIds, UserArities0, UserArities),
        map.det_update(ModuleAndName, UserArities, !MNA)
    ).

%---------------------------------------------------------------------------%

predicate_table_get_valid_pred_id_set(PredicateTable, ValidPredIds) :-
    ValidPredIds = PredicateTable ^ pt_valid_pred_ids.

predicate_table_make_pred_id_invalid(InvalidPredId, !PredicateTable) :-
    ValidPredIds0 = !.PredicateTable ^ pt_valid_pred_ids,
    set_tree234.delete(InvalidPredId, ValidPredIds0, ValidPredIds),
    !PredicateTable ^ pt_valid_pred_ids := ValidPredIds.

predicate_table_make_pred_ids_invalid(InvalidPredIds, !PredicateTable) :-
    ValidPredIds0 = !.PredicateTable ^ pt_valid_pred_ids,
    set_tree234.delete_list(InvalidPredIds, ValidPredIds0, ValidPredIds),
    !PredicateTable ^ pt_valid_pred_ids := ValidPredIds.

%---------------------------------------------------------------------------%

predicate_table_optimize(PredicateTable0, PredicateTable) :-
    PredicateTable0 = predicate_table(PredIdTable, NextPredId,
        ValidPredIds, AccessibilityTable,
        Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
        Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
    map.optimize(Pred_N_Index0, Pred_N_Index),
    map.optimize(Pred_NA_Index0, Pred_NA_Index),
    map.optimize(Pred_MNA_Index0, Pred_MNA_Index),
    map.optimize(Func_N_Index0, Func_N_Index),
    map.optimize(Func_NA_Index0, Func_NA_Index),
    map.optimize(Func_MNA_Index0, Func_MNA_Index),
    PredicateTable = predicate_table(PredIdTable, NextPredId,
        ValidPredIds, AccessibilityTable,
        Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
        Func_N_Index, Func_NA_Index, Func_MNA_Index).

%---------------------%

predicate_table_restrict(PartialQualInfo, ToKeepPredIds, OrigPredicateTable,
        !:PredicateTable) :-
    OrigPredIdTable = OrigPredicateTable ^ pt_pred_id_table,
    OrigAccessibilityTable = OrigPredicateTable ^ pt_accessibility_table,
    predicate_table_reset(OrigPredicateTable, !:PredicateTable),
    list.foldl(
        reinsert_for_restrict(PartialQualInfo,
            OrigPredIdTable, OrigAccessibilityTable),
        ToKeepPredIds, !PredicateTable).

:- pred predicate_table_reset(predicate_table::in, predicate_table::out)
    is det.

predicate_table_reset(PredicateTable0, PredicateTable) :-
    NextPredId = PredicateTable0 ^ pt_next_pred_id,
    PredicateTable = predicate_table(map.init, NextPredId,
        set_tree234.init, map.init,
        map.init, map.init, map.init, map.init, map.init, map.init).

:- pred reinsert_for_restrict(partial_qualifier_info::in, pred_id_table::in,
    accessibility_table::in, pred_id::in,
    predicate_table::in, predicate_table::out) is det.

reinsert_for_restrict(PartialQualInfo, PredIdTable, AccessibilityTable, PredId,
        !PredicateTable) :-
    map.lookup(PredIdTable, PredId, PredInfo),
    map.lookup(AccessibilityTable, PredId, Access),
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

%---------------------------------------------------------------------------%

predicate_table_get_pred_id_table(PT, X) :-
    X = PT ^ pt_pred_id_table.
predicate_table_set_pred_id_table(X, !PT) :-
    !PT ^ pt_pred_id_table := X.

%---------------------------------------------------------------------------%

predicate_table_lookup_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_name(PredicateTable, Name, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_module_name(PredicateTable, IsFullyQualified,
            Module, Name, PredIds)
    ).

predicate_table_lookup_pred_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_pred_name(PredicateTable, Name, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_pred_module_name(PredicateTable,
            IsFullyQualified, Module, Name, PredIds)
    ).

predicate_table_lookup_func_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_func_name(PredicateTable, Name, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_func_module_name(PredicateTable,
            IsFullyQualified, Module, Name, PredIds)
    ).

%---------------------------------------------------------------------------%

predicate_table_lookup_sym_arity(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_name_arity(PredicateTable, Name, UserArity,
                PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_m_n_a(PredicateTable,
            IsFullyQualified, Module, Name, UserArity, PredIds)
    ).

predicate_table_lookup_pred_sym_arity(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_pred_name_arity(PredicateTable,
                Name, UserArity, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_pred_m_n_a(PredicateTable,
            IsFullyQualified, Module, Name, UserArity, PredIds)
    ).

predicate_table_lookup_func_sym_arity(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredIds) :-
    (
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_func_name_arity(PredicateTable,
                Name, UserArity, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ;
        SymName = qualified(Module, Name),
        predicate_table_lookup_func_m_n_a(PredicateTable,
            IsFullyQualified, Module, Name, UserArity, PredIds)
    ).

predicate_table_lookup_pred_sym_arity_one(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredId) :-
    predicate_table_lookup_pred_sym_arity(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredIds),
    (
        PredIds = [PredId]
    ;
        PredIds = [],
        unexpected($pred, "no match")
    ;
        PredIds = [_, _ | _],
        unexpected($pred, "more than one match")
    ).

predicate_table_lookup_func_sym_arity_one(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredId) :-
    predicate_table_lookup_func_sym_arity(PredicateTable, IsFullyQualified,
        SymName, UserArity, PredIds),
    (
        PredIds = [PredId]
    ;
        PredIds = [],
        unexpected($pred, "no match")
    ;
        PredIds = [_, _ | _],
        unexpected($pred, "more than one match")
    ).

%---------------------------------------------------------------------------%

predicate_table_lookup_name(PredicateTable, Name, PredIds) :-
    predicate_table_lookup_pred_name(PredicateTable, Name, PredPredIds),
    predicate_table_lookup_func_name(PredicateTable, Name, FuncPredIds),
    PredIds = FuncPredIds ++ PredPredIds.

predicate_table_lookup_pred_name(PredicateTable, PredName, PredIds) :-
    PredNameIndex = PredicateTable ^ pt_pred_name_index,
    ( if map.search(PredNameIndex, PredName, PredIdsPrime) then
        PredIds = PredIdsPrime
    else
        PredIds = []
    ).

predicate_table_lookup_func_name(PredicateTable, FuncName, PredIds) :-
    FuncNameIndex = PredicateTable ^ pt_func_name_index,
    ( if map.search(FuncNameIndex, FuncName, PredIdsPrime) then
        PredIds = PredIdsPrime
    else
        PredIds = []
    ).

%---------------------------------------------------------------------------%

:- pred predicate_table_lookup_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is det.

predicate_table_lookup_module_name(PredicateTable, IsFullyQualified,
        Module, Name, PredIds) :-
    predicate_table_lookup_pred_module_name(PredicateTable,
        IsFullyQualified, Module, Name, PredPredIds),
    predicate_table_lookup_func_module_name(PredicateTable,
        IsFullyQualified, Module, Name, FuncPredIds),
    PredIds = FuncPredIds ++ PredPredIds.

:- pred predicate_table_lookup_pred_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is det.

predicate_table_lookup_pred_module_name(PredicateTable, IsFullyQualified,
        Module, PredName, PredIds) :-
    Pred_MNA_Index = PredicateTable ^ pt_pred_module_name_arity_index,
    ModuleAndName = module_and_name(Module, PredName),
    ( if map.search(Pred_MNA_Index, ModuleAndName, Arities) then
        map.values(Arities, PredIdLists),
        list.condense(PredIdLists, PredIds0),
        maybe_filter_pred_ids_matching_module(IsFullyQualified,
            Module, PredicateTable, PredIds0, PredIds)
    else
        PredIds = []
    ).

:- pred predicate_table_lookup_func_module_name(predicate_table::in,
    is_fully_qualified::in, module_name::in, string::in,
    list(pred_id)::out) is det.

predicate_table_lookup_func_module_name(PredicateTable, IsFullyQualified,
        Module, FuncName, PredIds) :-
    Func_MNA_Index = PredicateTable ^ pt_func_module_name_arity_index,
    ModuleAndName = module_and_name(Module, FuncName),
    ( if map.search(Func_MNA_Index, ModuleAndName, Arities) then
        map.values(Arities, PredIdLists),
        list.condense(PredIdLists, PredIds0),
        maybe_filter_pred_ids_matching_module(IsFullyQualified,
            Module, PredicateTable, PredIds0, PredIds)
    else
        PredIds = []
    ).

%---------------------------------------------------------------------------%

predicate_table_lookup_name_arity(PredicateTable, Name, UserArity, PredIds) :-
    predicate_table_lookup_pred_name_arity(PredicateTable,
        Name, UserArity, PredPredIds),
    predicate_table_lookup_func_name_arity(PredicateTable,
        Name, UserArity, FuncPredIds),
    PredIds = FuncPredIds ++ PredPredIds.

predicate_table_lookup_pred_name_arity(PredicateTable, PredName, UserArity,
        PredIds) :-
    PredNameArityIndex = PredicateTable ^ pt_pred_name_arity_index,
    NameArity = name_user_arity(PredName, UserArity),
    ( if map.search(PredNameArityIndex, NameArity, PredIdsPrime) then
        PredIds = PredIdsPrime
    else
        PredIds = []
    ).

predicate_table_lookup_func_name_arity(PredicateTable, FuncName, UserArity,
        PredIds) :-
    FuncNameArityIndex = PredicateTable ^ pt_func_name_arity_index,
    NameArity = name_user_arity(FuncName, UserArity),
    ( if map.search(FuncNameArityIndex, NameArity, PredIdsPrime) then
        PredIds = PredIdsPrime
    else
        PredIds = []
    ).

%---------------------------------------------------------------------------%

predicate_table_lookup_m_n_a(PredicateTable, IsFullyQualified,
        Module, Name, UserArity, PredIds) :-
    predicate_table_lookup_pred_m_n_a(PredicateTable,
        IsFullyQualified, Module, Name, UserArity, PredPredIds),
    predicate_table_lookup_func_m_n_a(PredicateTable,
        IsFullyQualified, Module, Name, UserArity, FuncPredIds),
    PredIds = FuncPredIds ++ PredPredIds.

predicate_table_lookup_pred_m_n_a(PredicateTable, IsFullyQualified,
        Module, PredName, UserArity, !:PredIds) :-
    P_MNA_Index = PredicateTable ^ pt_pred_module_name_arity_index,
    ModuleAndName = module_and_name(Module, PredName),
    ( if
        map.search(P_MNA_Index, ModuleAndName, ArityIndex),
        map.search(ArityIndex, UserArity, !:PredIds)
    then
        maybe_filter_pred_ids_matching_module(IsFullyQualified, Module,
            PredicateTable, !PredIds)
    else
        !:PredIds = []
    ).

predicate_table_lookup_func_m_n_a(PredicateTable, IsFullyQualified,
        Module, FuncName, UserArity, !:PredIds) :-
    F_MNA_Index = PredicateTable ^ pt_func_module_name_arity_index,
    ModuleAndName = module_and_name(Module, FuncName),
    ( if
        map.search(F_MNA_Index, ModuleAndName, ArityIndex),
        map.search(ArityIndex, UserArity, !:PredIds)
    then
        maybe_filter_pred_ids_matching_module(IsFullyQualified, Module,
            PredicateTable, !PredIds)
    else
        !:PredIds = []
    ).

:- pred maybe_filter_pred_ids_matching_module(is_fully_qualified::in,
    module_name::in, predicate_table::in,
    list(pred_id)::in, list(pred_id)::out) is det.

maybe_filter_pred_ids_matching_module(may_be_partially_qualified, _, _,
        !PredIds).
maybe_filter_pred_ids_matching_module(is_fully_qualified, ModuleName,
        PredicateTable, !PredIds) :-
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    list.filter(pred_id_matches_module(PredIdTable, ModuleName), !PredIds).

:- pred pred_id_matches_module(pred_id_table::in, module_name::in, pred_id::in)
    is semidet.

pred_id_matches_module(PredIdTable, ModuleName, PredId) :-
    map.lookup(PredIdTable, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo).

%---------------------------------------------------------------------------%

predicate_table_lookup_pf_m_n_a(PredicateTable, IsFullyQualified,
        PredOrFunc, Module, Name, PredFormArity, PredIds) :-
    (
        PredOrFunc = pf_predicate,
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        predicate_table_lookup_pred_m_n_a(PredicateTable, IsFullyQualified,
            Module, Name, UserArity, PredIds)
    ;
        PredOrFunc = pf_function,
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        predicate_table_lookup_func_m_n_a(PredicateTable, IsFullyQualified,
            Module, Name, UserArity, PredIds)
    ).

predicate_table_lookup_pf_name_arity(PredicateTable,
        PredOrFunc, Name, PredFormArity, PredIds) :-
    (
        PredOrFunc = pf_predicate,
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        predicate_table_lookup_pred_name_arity(PredicateTable, Name, UserArity,
            PredIds)
    ;
        PredOrFunc = pf_function,
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        predicate_table_lookup_func_name_arity(PredicateTable, Name, UserArity,
            PredIds)
    ).

predicate_table_lookup_pf_sym_arity(PredicateTable, IsFullyQualified,
        PredOrFunc, SymName, PredFormArity, PredIds) :-
    (
        SymName = qualified(Module, Name),
        predicate_table_lookup_pf_m_n_a(PredicateTable, IsFullyQualified,
            PredOrFunc, Module, Name, PredFormArity, PredIds)
    ;
        SymName = unqualified(Name),
        (
            IsFullyQualified = may_be_partially_qualified,
            predicate_table_lookup_pf_name_arity(PredicateTable,
                PredOrFunc, Name, PredFormArity, PredIds)
        ;
            IsFullyQualified = is_fully_qualified,
            PredIds = []
        )
    ).

predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified, PredOrFunc,
        SymName, PredIds) :-
    (
        PredOrFunc = pf_predicate,
        predicate_table_lookup_pred_sym(PredicateTable, IsFullyQualified,
            SymName, PredIds)
    ;
        PredOrFunc = pf_function,
        predicate_table_lookup_func_sym(PredicateTable, IsFullyQualified,
            SymName, PredIds)
    ).

%---------------------------------------------------------------------------%

predicate_table_lookup_pf_raw_name(PredicateTable, PredOrFunc, Name,
        PredIds) :-
    % We add *all* pred_ids to the {pred,func}_module_name_arity_index.
    % We do *not* a pred_id to the other indexes if it comes from a module
    % that the current module got access to via a "use_module" declaration.
    % Since we want to return all pred_ids with the given name,
    % the only indexes we can use are {pred,func}_module_name_arity_index.
    (
        PredOrFunc = pf_predicate,
        MNAIndex = PredicateTable ^ pt_pred_module_name_arity_index
    ;
        PredOrFunc = pf_function,
        MNAIndex = PredicateTable ^ pt_func_module_name_arity_index
    ),
    module_name_arity_index_search(MNAIndex, Name, PredIds).

:- pred module_name_arity_index_search(module_name_arity_index::in,
    string::in, list(pred_id)::out) is det.

module_name_arity_index_search(MNAIndex, Name, PredIds) :-
    map.foldl(add_pred_ids_to_list_if_name_matches(Name), MNAIndex,
        [], PredIds).

:- pred add_pred_ids_to_list_if_name_matches(string::in,
    module_and_name::in, multi_map(user_arity, pred_id)::in,
    list(pred_id)::in, list(pred_id)::out) is det.

add_pred_ids_to_list_if_name_matches(Name, KeyMN, UserArityMap, !PredIds) :-
    KeyMN = module_and_name(_KeyModuleName, KeyName),
    ( if Name = KeyName then
        !:PredIds = multi_map.values(UserArityMap) ++ !.PredIds
    else
        true
    ).

%---------------------------------------------------------------------------%

resolve_pred_overloading(ModuleInfo, CallerMarkers, TVarSet, ExistQTVars,
        ArgTypes, ExternalTypeParams, Context, PredSymName0, PredSymName,
        PredId, Specs) :-
    % Note: calls to preds declared in `.opt' files should always be
    % module qualified, so they should not be considered
    % when resolving overloading.
    module_info_get_predicate_table(ModuleInfo, PredTable),
    IsFullyQualified = calls_are_fully_qualified(CallerMarkers),
    predicate_table_lookup_pred_sym(PredTable, IsFullyQualified,
        PredSymName0, PredIds),
    % Check if there any of the candidate pred_ids have argument/return types
    % which subsume the actual argument/return types of this function call.
    ( if
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ExistQTVars,
            ArgTypes, ExternalTypeParams, no, Context, PredIdPrime,
            PredSymNamePrime, SpecsPrime)
    then
        PredId = PredIdPrime,
        PredSymName = PredSymNamePrime,
        Specs = SpecsPrime
    else
        % If there is no matching predicate for this call, then this predicate
        % must have a type error which should have been caught by typechecking.
        unexpected($pred, "type error in pred call: no matching pred")
    ).

%---------------------------------------------------------------------------%

find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ExistQTVars,
        ArgTypes, ExternalTypeParams, MaybeConstraintSearch, Context,
        ThePredId, ThePredSymName, Specs) :-
    find_matching_pred_ids(ModuleInfo, TVarSet, ExistQTVars,
        ArgTypes, ExternalTypeParams, MaybeConstraintSearch, Context,
        PredIds, MatchingPredIdsInfos),
    (
        MatchingPredIdsInfos = [],
        fail
    ;
        MatchingPredIdsInfos =
            [ThePredId - ThePredInfo | TailMatchingPredIdsInfos],
        % We have found a matching predicate.
        pred_info_get_sym_name(ThePredInfo, ThePredSymName),
        % Was there was more than one matching predicate/function?
        (
            TailMatchingPredIdsInfos = [],
            Specs = []
        ;
            TailMatchingPredIdsInfos = [_ | _],
            GetPredDesc =
                ( func(_ - PI) = Piece :-
                    pred_info_get_pf_sym_name_arity(PI, PFSNA),
                    Piece = qual_pf_sym_name_pred_form_arity(PFSNA)
                ),
            PredDescPieces = list.map(GetPredDesc, MatchingPredIdsInfos),
            intersperse_list_last([suffix(","), nl],
                [suffix(","), words("and"), nl], PredDescPieces,
                PredDescSepPieces),
            Pieces = [words("Error: unresolved predicate overloading."), nl,
                words("The matches are"), nl_indent_delta(1)] ++
                PredDescSepPieces ++ [suffix("."), nl_indent_delta(-1),
                words("You need to use an explicit module qualifier"),
                words("to select the one you intend to refer to."), nl,
                words("Proceeding on the assumption that"),
                words("the intended match is the first."),
                words("If this assumption is incorrect, other error messages"),
                words("may be reported for this predicate or function"),
                words("solely because of this wrong assumption."), nl],
            % In the frequent case (for us, at least) where the ambiguity
            % is caused by moving a predicate from one module of the standard
            % library to another but leaving a forwarding predicate behind,
            % the choices will be equivalent, and there will be *no* avalanche
            % errors caused by our assumption. However, it is better to warn
            % about avalanche errors that won't happen than to not warn
            % about avalanche errors that *do* happen.
            Spec = simplest_spec($pred, severity_error, phase_type_check,
                Context, Pieces),
            Specs = [Spec]
        )
    ).

:- pred find_matching_pred_ids(module_info::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in,
    maybe(constraint_search)::in(maybe(constraint_search)), prog_context::in,
    list(pred_id)::in, assoc_list(pred_id, pred_info)::out) is det.

find_matching_pred_ids(_, _, _, _, _, _, _, [], []).
find_matching_pred_ids(ModuleInfo, TVarSet, ExistQTVars, ArgTypes,
        ExternalTypeParams, MaybeConstraintSearch, Context,
        [PredId | PredIds], MatchingPredIdsInfos) :-
    find_matching_pred_ids(ModuleInfo, TVarSet, ExistQTVars, ArgTypes,
        ExternalTypeParams, MaybeConstraintSearch, Context,
        PredIds, TailMatchingPredIdsInfos),
    ( if
        % Lookup the argument types of the candidate predicate
        % (or the argument types + return type of the candidate function).
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_arg_types(PredInfo, PredTVarSet, PredExistQVars0,
            PredArgTypes0),
        pred_info_get_tvar_kind_map(PredInfo, PredKindMap),
        arg_type_list_subsumes(TVarSet, ExistQTVars, ArgTypes,
            ExternalTypeParams, PredTVarSet, PredKindMap, PredExistQVars0,
            PredArgTypes0),
        (
            MaybeConstraintSearch = no
        ;
            MaybeConstraintSearch = yes(ConstraintSearch),
            % Lookup the universal constraints on the candidate predicate.
            pred_info_get_class_context(PredInfo, ProgConstraints),
            ProgConstraints = constraints(UnivConstraints, _),
            list.length(UnivConstraints, NumConstraints),
            ConstraintSearch(NumConstraints, ProvenConstraints),
            univ_constraints_match(ProvenConstraints, UnivConstraints)
        )
    then
        MatchingPredIdsInfos = [PredId - PredInfo | TailMatchingPredIdsInfos]
    else
        MatchingPredIdsInfos = TailMatchingPredIdsInfos
    ).

    % Check that the universal constraints proven in the caller match the
    % constraints on the callee.
    %
    % XXX We should rename apart the callee constraints and check that the
    % proven constraints are instances of them. This would give us better
    % overloading resolution. For the moment, we just check that the names
    % and arities match, which is sufficient to prevent any compiler aborts
    % in later stages.
    %
:- pred univ_constraints_match(list(prog_constraint)::in,
    list(prog_constraint)::in) is semidet.

univ_constraints_match([], []).
univ_constraints_match([ProvenConstraint | ProvenConstraints],
        [CalleeConstraint | CalleeConstraints]) :-
    ProvenConstraint = constraint(ClassName, ProvenArgTypes),
    list.length(ProvenArgTypes, Arity),
    CalleeConstraint = constraint(ClassName, CalleeArgTypes),
    list.length(CalleeArgTypes, Arity),
    univ_constraints_match(ProvenConstraints, CalleeConstraints).

%---------------------%

get_pred_id_by_types(IsFullyQualified, SymName, PredOrFunc, TVarSet,
        ExistQTVars, ArgTypes, ExternalTypeParams, ModuleInfo, Context,
        PredId, Specs) :-
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredFormArity = arg_list_arity(ArgTypes),
    predicate_table_lookup_pf_sym_arity(PredicateTable, IsFullyQualified,
        PredOrFunc, SymName, PredFormArity, PredIds),
    ( if
        % Resolve overloading using the argument types.
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ExistQTVars,
            ArgTypes, ExternalTypeParams, no, Context, PredIdPrime,
            _PredSymName, SpecsPrime)
    then
        PredId = PredIdPrime,
        Specs = SpecsPrime
    else
        % Undefined/invalid pred or func.
        fail
    ).

get_pred_id_and_proc_id_by_types(IsFullyQualified, SymName, PredOrFunc,
        TVarSet, ExistQTVars, ArgTypes, ExternalTypeParams, ModuleInfo,
        Context, PredId, ProcId, Specs) :-
    ( if
        get_pred_id_by_types(IsFullyQualified, SymName, PredOrFunc, TVarSet,
            ExistQTVars, ArgTypes, ExternalTypeParams, ModuleInfo, Context,
            PredIdPrime, SpecsPrime)
    then
        PredId = PredIdPrime,
        Specs = SpecsPrime
    else
        % Undefined/invalid pred or func. The type-checker should ensure
        % that this never happens.
        list.length(ArgTypes, Arity),
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        NameStr = sym_name_to_string(SymName),
        string.int_to_string(Arity, ArityString),
        string.append_list(["undefined/invalid ", PredOrFuncStr,
            "\n`", NameStr, "/", ArityString, "'"], Msg),
        unexpected($pred, Msg)
    ),
    get_single_proc_id(ModuleInfo, PredId, ProcId).

%---------------------------------------------------------------------------%

get_single_proc_id(ModuleInfo, PredId, ProcId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    ( if ProcIds = [ProcId0] then
        ProcId = ProcId0
    else
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
        unexpected($pred, Message)
    ).

%---------------------------------------------------------------------------%

lookup_builtin_pred_proc_id(Module, ModuleName, ProcName, PredOrFunc,
        UserArity, ModeNo, PredId, ProcId) :-
    module_info_get_predicate_table(Module, PredTable),
    UserArity = user_arity(UserArityInt),
    ( if
        (
            PredOrFunc = pf_predicate,
            predicate_table_lookup_pred_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, UserArity, PredIds)
        ;
            PredOrFunc = pf_function,
            predicate_table_lookup_func_m_n_a(PredTable, is_fully_qualified,
                ModuleName, ProcName, UserArity, PredIds)
        ),
        PredIds = [PredIdPrime]
    then
        PredId = PredIdPrime
    else
        unexpected($pred,
            string.format("can't locate %s.%s/%d",
                [s(sym_name_to_string(ModuleName)), s(ProcName),
                i(UserArityInt)]))
    ),
    module_info_pred_info(Module, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    (
        ModeNo = only_mode,
        ( if ProcIds = [ProcId0] then
            ProcId = ProcId0
        else
            unexpected($pred,
                string.format("expected single mode for %s.%s/%d",
                    [s(sym_name_to_string(ModuleName)),
                    s(ProcName), i(UserArityInt)]))
        )
    ;
        ModeNo = mode_no(N),
        ( if list.index0(ProcIds, N, ProcId0) then
            ProcId = ProcId0
        else
            unexpected($pred,
                string.format("there is no mode %d for %s.%s/%d",
                    [i(N), s(sym_name_to_string(ModuleName)),
                    s(ProcName), i(UserArityInt)]))
        )
    ).

%---------------------------------------------------------------------------%

get_next_pred_id(PredTable, NextPredId) :-
    NextPredId = PredTable ^ pt_next_pred_id.

%---------------------------------------------------------------------------%
:- end_module hlds.pred_table.
%---------------------------------------------------------------------------%
