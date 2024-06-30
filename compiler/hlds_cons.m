%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: cons.m.
% Main authors: fjh, conway.
%
% This module defines the data structures we use to hold information
% about function symbols.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_cons.
:- interface.

:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The symbol table for constructors.
%

    % The symbol table for constructors. This table is used by the typechecker
    % to look up the type of functors/constants.
    %
:- type cons_table.

    % A cons_defn is the definition of a constructor (i.e. a constant
    % or a functor) for a particular type.
    %
:- type hlds_cons_defn
    --->    hlds_cons_defn(
                % The result type, i.e. the type constructor to which this
                % cons_defn belongs.
                cons_type_ctor      :: type_ctor,

                % These three fields are copies of the first three fields
                % of the hlds_type_defn for cons_type_ctor. They specify
                % the tvarset from which the type variables in the argument
                % types come, the type constructor's type parameter list,
                % and the kinds of the type variables.
                %
                % These copies are here because code that wants to process
                % the types of the cons_id's arguments (the cons_args field)
                % typically needs to know them. Having a copy here avoids
                % a lookup of the type definition.
                cons_type_tvarset   :: tvarset,
                cons_type_params    :: list(type_param),
                cons_type_kinds     :: tvar_kind_map,

                % Any existential type variables and class constraints.
                % It is an invariant that this will be no_exist_constraints
                % if the list of arguments is empty.
                cons_maybe_exist    :: maybe_cons_exist_constraints,

                % The field names and types of the arguments of this functor
                % (if any).
                cons_args           :: list(constructor_arg),

                % The location of this constructor definition in the
                % original source code.
                cons_context        :: prog_context
            ).

%---------------------------------------------------------------------------%
%
% Creating and populating the cons_table.
%

:- func init_cons_table = cons_table.

    % Insert the given hlds_cons_defn into the cons_table as the definition
    % for one or more user du_ctors. These du_ctors should represent the full
    % range of possible qualifications of the *same* cons_id, from unqualified
    % through all forms of possible qualification to fully qualified,
    % and with both the actual type_ctor and the dummy type_ctor.
    % The first argument should be the fully qualified version with
    % the actual type_ctor.
    %
:- pred insert_into_cons_table(du_ctor::in, list(du_ctor)::in,
    hlds_cons_defn::in, cons_table::in, cons_table::out) is det.

%---------------------------------------------------------------------------%
%
% Updating the cons table as a whole.
%

    % Transform every hlds_cons_defn in the cons_table using the
    % given predicate.
    %
:- pred replace_cons_defns_in_cons_table(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    cons_table::in, cons_table::out) is det.

:- pred cons_table_optimize(cons_table::in, cons_table::out) is det.

%---------------------------------------------------------------------------%
%
% Searching the cons table for a given user cons_id.
%

    % Does the given user cons_id occur in the cons_table? If yes, return
    % the list of possible matching hlds_cons_defns, which will be
    % for occurrences of the same cons_id in different type definitions.
    %
:- pred search_cons_table(cons_table::in, du_ctor::in,
    list(hlds_cons_defn)::out) is semidet.

    % Does the given user cons_id occur in the cons_table? This does the same
    % job as search_cons_table, but without constructing and returning the
    % list of possible matching hlds_cons_defns.
    %
:- pred is_known_data_cons(cons_table::in, du_ctor::in) is semidet.

    % Does the given user cons_id occur in the definition of the given type
    % constructor in the cons_table? If yes, return its definition in that
    % type; otherwise, fail.
    %
:- pred search_cons_table_of_type_ctor(cons_table::in, type_ctor::in,
    du_ctor::in, hlds_cons_defn::out) is semidet.

    % Does the given user cons_id occur in the definition of the given type
    % constructor in the cons_table? If yes, return its definition in that
    % type; otherwise, abort.
    %
:- pred lookup_cons_table_of_type_ctor(cons_table::in, type_ctor::in,
    du_ctor::in, hlds_cons_defn::out) is det.

%---------------------------------------------------------------------------%
%
% Searching the cons table for things other than a cons_id.
%

    % Return the list of arities with which the given sym_name occurs
    % in the cons_table.
    %
:- pred return_cons_arities(cons_table::in, sym_name::in, list(int)::out)
    is det.

:- pred return_cons_defns_with_given_name(cons_table::in, string::in,
    list(hlds_cons_defn)::out) is det.

%---------------------------------------------------------------------------%
%
% Returning the entire contents of the cons table.
%

    % Return all the constructor definitions in the cons_table.
    %
:- pred get_all_cons_defns(cons_table::in,
    assoc_list(du_ctor, hlds_cons_defn)::out) is det.

    % Return all the names in the cons_table.
    %
:- pred cons_table_names(cons_table::in, set(string)::out) is det.

%---------------------------------------------------------------------------%
%
% The table for field names.
%

:- type ctor_field_table == map(sym_name, list(hlds_ctor_field_defn)).

:- type hlds_ctor_field_defn
    --->    hlds_ctor_field_defn(
                % The context of the field definition.
                field_context   :: prog_context,

                field_status    :: type_status,

                % The type containing the field.
                field_type_ctor :: type_ctor,

                % The constructor containing the field.
                field_du_ctor   :: du_ctor,

                % Argument number (counting from 1).
                field_arg_num   :: int
            ).

    % Field accesses are expanded into inline unifications by post_typecheck.m
    % after typechecking has worked out which field is being referred to.
    %
    % Function declarations and clauses are not generated for these because
    % it would be difficult to work out how to mode them.
    %
    % Users can supply type and mode declarations, for example to export
    % a field of an abstract data type or to allow taking the address
    % of a field access function.
    %
:- type field_access_type
    --->    get
    ;       set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module varset.

    % Maps the raw, unqualified name of a functor to information about
    % all the functors with that name.
:- type cons_table == map(string, inner_cons_table).

    % Every visible constructor will have exactly one entry in the list,
    % and this entry lists all the du_ctors by which that constructor
    % may be known. The du_ctors listed for a given constructor may give
    % fully, partially or not-at-all qualified versions of the symbol name,
    % they must agree on the arity, and may give the actual type_ctor
    % or the standard dummy type_ctor. The main cons_id must give the
    % fully qualified symname and the right type_ctor.
    %
    % The order of the list is not meaningful.
:- type inner_cons_table == list(inner_cons_entry).
:- type inner_cons_entry
    --->    inner_cons_entry(
                ice_fully_qual_data_ctor    :: du_ctor,
                ice_other_data_ctors        :: list(du_ctor),
                ice_cons_defn               :: hlds_cons_defn
            ).

%---------------------------------------------------------------------------%

init_cons_table = map.init.

insert_into_cons_table(MainDuCtor, OtherDuCtors,
        ConsDefn, !ConsTable) :-
    MainDuCtor = du_ctor(MainSymName, _, _),
    MainName = unqualify_name(MainSymName),
    Entry = inner_cons_entry(MainDuCtor, OtherDuCtors, ConsDefn),
    ( if map.search(!.ConsTable, MainName, InnerConsEntries0) then
        InnerConsEntries = [Entry | InnerConsEntries0],
        map.det_update(MainName, InnerConsEntries, !ConsTable)
    else
        InnerConsEntries = [Entry],
        map.det_insert(MainName, InnerConsEntries, !ConsTable)
    ).

%---------------------------------------------------------------------------%

replace_cons_defns_in_cons_table(Replace, !ConsTable) :-
    map.map_values_only(replace_cons_defns_in_inner_cons_table(Replace),
        !ConsTable).

:- pred replace_cons_defns_in_inner_cons_table(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    inner_cons_table::in, inner_cons_table::out) is det.

replace_cons_defns_in_inner_cons_table(Replace, !InnerConsTable) :-
    list.map(replace_cons_defns_in_inner_cons_entry(Replace), !InnerConsTable).

:- pred replace_cons_defns_in_inner_cons_entry(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    inner_cons_entry::in, inner_cons_entry::out) is det.

replace_cons_defns_in_inner_cons_entry(Replace, !Entry) :-
    ConsDefn0 = !.Entry ^ ice_cons_defn,
    Replace(ConsDefn0, ConsDefn),
    !Entry ^ ice_cons_defn := ConsDefn.

%---------------------%

cons_table_optimize(!ConsTable) :-
    map.optimize(!ConsTable).

%---------------------------------------------------------------------------%

search_cons_table(ConsTable, DuCtor, ConsDefns) :-
    DuCtor = du_ctor(SymName, _, _),
    Name = unqualify_name(SymName),
    map.search(ConsTable, Name, InnerConsTable),
    % After post-typecheck, all calls should specify the main cons_id
    % of the searched-for (single) constructor definition. Searching
    % the main du_ctors is sufficient for such calls, and since there are
    % many fewer main du_ctors than other du_ctors, it is fast as well.
    %
    % I (zs) don't think replacing a list with a different structure would
    % help, since these lists should be very short.
    ( if
        search_inner_main_du_ctors(InnerConsTable, DuCtor, MainConsDefn)
    then
        ConsDefns = [MainConsDefn]
    else
        % Before and during typecheck, we may need to look up constructors
        % using du_ctors that may not be even partially module qualified,
        % and which will contain a dummy type_ctor. That is why we search
        % the other du_ctors as well.
        %
        % After post-typecheck, we should get here only if there is a bug
        % in the compiler, since
        %
        % - at that time, all du_ctors should be module-qualified and should
        %   have non-dummy type-ctors,
        %
        % - this means that searches that find what they are looking for
        %   will take the then-branch above, and
        %
        % - there should be no searches that fail, because mention of
        %   an unknown cons_id in the program is a type error, which means
        %   the compiler should never get to the passes following
        %   post-typecheck.
        search_inner_other_du_ctors(InnerConsTable, DuCtor, ConsDefns),
        % Do not return empty lists; let the call fail in that case.
        ConsDefns = [_ | _]
    ).

:- pred search_inner_main_du_ctors(list(inner_cons_entry)::in,
    du_ctor::in, hlds_cons_defn::out) is semidet.

search_inner_main_du_ctors([Entry | Entries], DuCtor, ConsDefn) :-
    ( if DuCtor = Entry ^ ice_fully_qual_data_ctor then
        ConsDefn = Entry ^ ice_cons_defn
    else
        search_inner_main_du_ctors(Entries, DuCtor, ConsDefn)
    ).

:- pred search_inner_other_du_ctors(list(inner_cons_entry)::in,
    du_ctor::in, list(hlds_cons_defn)::out) is det.

search_inner_other_du_ctors([], _DuCtor, []).
search_inner_other_du_ctors([Entry | Entries], DuCtor, !:ConsDefns) :-
    search_inner_other_du_ctors(Entries, DuCtor, !:ConsDefns),
    ( if list.member(DuCtor, Entry ^ ice_other_data_ctors) then
        !:ConsDefns = [Entry ^ ice_cons_defn | !.ConsDefns]
    else
        true
    ).

%---------------------%

is_known_data_cons(ConsTable, DuCtor) :-
    DuCtor = du_ctor(SymName, _, _),
    Name = unqualify_name(SymName),
    map.search(ConsTable, Name, InnerConsTable),
    is_known_data_cons_inner(InnerConsTable, DuCtor).

:- pred is_known_data_cons_inner(list(inner_cons_entry)::in,
    du_ctor::in) is semidet.

is_known_data_cons_inner([Entry | Entries], DuCtor) :-
    ( if
        ( DuCtor = Entry ^ ice_fully_qual_data_ctor
        ; list.member(DuCtor, Entry ^ ice_other_data_ctors)
        )
    then
        true
    else
        is_known_data_cons_inner(Entries, DuCtor)
    ).

%---------------------%

search_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor, ConsDefn) :-
    DuCtor = du_ctor(SymName, _, _),
    Name = unqualify_name(SymName),
    map.search(ConsTable, Name, InnerConsTable),
    search_inner_du_ctors_type_ctor(InnerConsTable, TypeCtor, DuCtor,
        ConsDefn).

:- pred search_inner_du_ctors_type_ctor(list(inner_cons_entry)::in,
    type_ctor::in, du_ctor::in, hlds_cons_defn::out) is semidet.

search_inner_du_ctors_type_ctor([Entry | Entries], TypeCtor, DuCtor,
        ConsDefn) :-
    EntryConsDefn = Entry ^ ice_cons_defn,
    ( if
        % If a type has two functors with the same name but different arities,
        % then it is possible for the TypeCtor test to succeed and the DuCtor
        % tests to fail (due to the arity mismatch). In such cases, we need
        % to search the rest of the list.
        EntryConsDefn ^ cons_type_ctor = TypeCtor,
        ( DuCtor = Entry ^ ice_fully_qual_data_ctor
        ; list.member(DuCtor, Entry ^ ice_other_data_ctors)
        )
    then
        ConsDefn = EntryConsDefn
    else
        search_inner_du_ctors_type_ctor(Entries, TypeCtor, DuCtor, ConsDefn)
    ).

%---------------------%

lookup_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor, ConsDefn) :-
    ( if
        search_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor,
            ConsDefnPrime)
    then
        ConsDefn = ConsDefnPrime
    else
        unexpected($pred, "lookup failed")
    ).

%---------------------------------------------------------------------------%

return_cons_arities(ConsTable, SymName, Arities) :-
    Name = unqualify_name(SymName),
    ( if map.search(ConsTable, Name, InnerConsTable) then
        return_cons_arities_inner(InnerConsTable, SymName, [], Arities0),
        list.sort_and_remove_dups(Arities0, Arities)
    else
        Arities = []
    ).

:- pred return_cons_arities_inner(list(inner_cons_entry)::in,
    sym_name::in, list(int)::in, list(int)::out) is det.

return_cons_arities_inner([], _, !Arities).
return_cons_arities_inner([Entry | Entries], SymName, !Arities) :-
    MainDuCtor = Entry ^ ice_fully_qual_data_ctor,
    OtherDuCtors = Entry ^ ice_other_data_ctors,
    DuCtors = [MainDuCtor | OtherDuCtors],
    return_cons_arities_inner_du_ctors(DuCtors, SymName, !Arities),
    return_cons_arities_inner(Entries, SymName, !Arities).

:- pred return_cons_arities_inner_du_ctors(list(du_ctor)::in,
    sym_name::in, list(int)::in, list(int)::out) is det.

return_cons_arities_inner_du_ctors([], _, !Arities).
return_cons_arities_inner_du_ctors([DuCtor | DuCtors], SymName, !Arities) :-
    DuCtor = du_ctor(ThisSymName, ThisArity, _),
    ( if ThisSymName = SymName then
        !:Arities = [ThisArity | !.Arities]
    else
        true
    ),
    return_cons_arities_inner_du_ctors(DuCtors, SymName, !Arities).

%---------------------%

return_cons_defns_with_given_name(ConsTable, Name, ConsDefns) :-
    ( if map.search(ConsTable, Name, InnerConsTable) then
        accumulate_hlds_cons_defns(InnerConsTable, [], ConsDefns)
    else
        ConsDefns = []
    ).

:- pred accumulate_hlds_cons_defns(list(inner_cons_entry)::in,
    list(hlds_cons_defn)::in, list(hlds_cons_defn)::out) is det.

accumulate_hlds_cons_defns([], !ConsDefns).
accumulate_hlds_cons_defns([Entry | Entries], !ConsDefns) :-
    !:ConsDefns = [Entry ^ ice_cons_defn | !.ConsDefns],
    accumulate_hlds_cons_defns(Entries, !ConsDefns).

%---------------------------------------------------------------------------%

get_all_cons_defns(ConsTable, AllConsDefns) :-
    map.foldl_values(accumulate_all_inner_cons_defns, ConsTable,
        [], AllConsDefns).

:- pred accumulate_all_inner_cons_defns(inner_cons_table::in,
    assoc_list(du_ctor, hlds_cons_defn)::in,
    assoc_list(du_ctor, hlds_cons_defn)::out) is det.

accumulate_all_inner_cons_defns(InnerConsTable, !AllConsDefns) :-
    list.map(project_inner_cons_entry, InnerConsTable, InnerConsList),
    !:AllConsDefns = InnerConsList ++ !.AllConsDefns.

:- pred project_inner_cons_entry(inner_cons_entry::in,
    pair(du_ctor, hlds_cons_defn)::out) is det.

project_inner_cons_entry(Entry, Pair) :-
    Entry = inner_cons_entry(MainDuCtor, _OtherDuCtors, ConsDefn),
    Pair = MainDuCtor - ConsDefn.

%---------------------%

cons_table_names(ConsTable, Names) :-
    map.keys_as_set(ConsTable, Names).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_cons.
%---------------------------------------------------------------------------%
