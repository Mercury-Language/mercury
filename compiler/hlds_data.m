%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_data.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with issues related
% to data and its representation: function symbols, types, insts, modes.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_data.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % The symbol table for constructors. This table is used by the type-checker
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

                % Existential type variables and class constraints.
                cons_exist_tvars    :: existq_tvars,
                cons_constraints    :: list(prog_constraint),

                % The field names and types of the arguments of this functor
                % (if any).
                cons_args           :: list(constructor_arg),

                % The location of this constructor definition in the
                % original source code.
                cons_context        :: prog_context
            ).

:- func init_cons_table = cons_table.

:- pred insert_into_cons_table(cons_id::in, list(cons_id)::in,
    hlds_cons_defn::in, cons_table::in, cons_table::out) is det.

:- pred search_cons_table(cons_table::in, cons_id::in,
    list(hlds_cons_defn)::out) is semidet.

:- pred search_cons_table_of_type_ctor(cons_table::in, type_ctor::in,
    cons_id::in, hlds_cons_defn::out) is semidet.

:- pred lookup_cons_table_of_type_ctor(cons_table::in, type_ctor::in,
    cons_id::in, hlds_cons_defn::out) is det.

:- pred get_all_cons_defns(cons_table::in,
    assoc_list(cons_id, hlds_cons_defn)::out) is det.

:- pred return_cons_arities(cons_table::in, sym_name::in, list(int)::out)
    is det.

:- pred replace_cons_defns_in_cons_table(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    cons_table::in, cons_table::out) is det.

:- pred cons_table_optimize(cons_table::in, cons_table::out) is det.

:- implementation.

    % Maps the raw, unqualified name of a functor to information about
    % all the functors with that name.
:- type cons_table == map(string, inner_cons_table).

    % Every visible constructor will have exactly one entry in the list,
    % and this entry lists all the cons_ids by which that constructor
    % may be known. The cons_ids listed for a given constructor may give
    % fully, partially or not-at-all qualified versions of the symbol name,
    % they must agree on the arity, and may give the actual type_ctor
    % or the standard dummy type_ctor. The main cons_id must give the
    % fully qualified symname and the right type_ctor.
    %
    % The order of the list is not meaningful.
:- type inner_cons_table == list(inner_cons_entry).
:- type inner_cons_entry
    --->    inner_cons_entry(
                ice_fully_qual_cons_id      :: cons_id,
                ice_other_cons_ids          :: list(cons_id),
                ice_cons_defn               :: hlds_cons_defn
            ).

init_cons_table = map.init.

insert_into_cons_table(MainConsId, OtherConsIds, ConsDefn, !ConsTable) :-
    ( if MainConsId = cons(MainSymName, _, _) then
        MainName = unqualify_name(MainSymName),
        Entry = inner_cons_entry(MainConsId, OtherConsIds, ConsDefn),
        ( if map.search(!.ConsTable, MainName, InnerConsEntries0) then
            InnerConsEntries = [Entry | InnerConsEntries0],
            map.det_update(MainName, InnerConsEntries, !ConsTable)
        else
            InnerConsEntries = [Entry],
            map.det_insert(MainName, InnerConsEntries, !ConsTable)
        )
    else
        unexpected($module, $pred, "MainConsId is not cons")
    ).

search_cons_table(ConsTable, ConsId, ConsDefns) :-
    ConsId = cons(SymName, _, _),
    Name = unqualify_name(SymName),
    map.search(ConsTable, Name, InnerConsTable),

    % After post-typecheck, all calls should specify the main cons_id
    % of the searched-for (single) constructor definition. Searching
    % the main cons_ids is sufficient for such calls, and since there are
    % many fewer main cons_ids than other cons_ids, it is fast as well.
    %
    % I (zs) don't think replacing a list with a different structure would
    % help, since these lists should be very short.

    ( if search_inner_main_cons_ids(InnerConsTable, ConsId, MainConsDefn) then
        ConsDefns = [MainConsDefn]
    else
        % Before and during typecheck, we may need to look up constructors
        % using cons_ids that may not be even partially module qualified,
        % and which will contain a dummy type_ctor. That is why we search
        % the other cons_ids as well.
        %
        % After post-typecheck, we should get here only if there is a bug
        % in the compiler, since
        %
        % - at that time, all cons_ids should be module-qualified and should
        %   have non-dummy type-ctors,
        %
        % - this means that searches that find what they are looking for
        %   will take the then-branch above, and
        %
        % - there should be no searches that fail, because mention of
        %   an unknown cons_id in the program is a type error, which means
        %   the compiler should never get to the passes following
        %   post-typecheck.

        search_inner_other_cons_ids(InnerConsTable, ConsId, ConsDefns),
        % Do not return empty lists; let the call fail in that case.
        ConsDefns = [_ | _]
    ).

:- pred search_inner_main_cons_ids(list(inner_cons_entry)::in, cons_id::in,
    hlds_cons_defn::out) is semidet.

search_inner_main_cons_ids([Entry | Entries], ConsId, ConsDefn) :-
    ( if ConsId = Entry ^ ice_fully_qual_cons_id then
        ConsDefn = Entry ^ ice_cons_defn
    else
        search_inner_main_cons_ids(Entries, ConsId, ConsDefn)
    ).

:- pred search_inner_other_cons_ids(list(inner_cons_entry)::in, cons_id::in,
    list(hlds_cons_defn)::out) is det.

search_inner_other_cons_ids([], _ConsId, []).
search_inner_other_cons_ids([Entry | Entries], ConsId, !:ConsDefns) :-
    search_inner_other_cons_ids(Entries, ConsId, !:ConsDefns),
    ( if list.member(ConsId, Entry ^ ice_other_cons_ids) then
        !:ConsDefns = [Entry ^ ice_cons_defn | !.ConsDefns]
    else
        true
    ).

search_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn) :-
    ConsId = cons(SymName, _, _),
    Name = unqualify_name(SymName),
    map.search(ConsTable, Name, InnerConsTable),
    search_inner_cons_ids_type_ctor(InnerConsTable, TypeCtor, ConsId,
        ConsDefn).

:- pred search_inner_cons_ids_type_ctor(list(inner_cons_entry)::in,
    type_ctor::in, cons_id::in, hlds_cons_defn::out) is semidet.

search_inner_cons_ids_type_ctor([Entry | Entries], TypeCtor, ConsId,
        ConsDefn) :-
    EntryConsDefn = Entry ^ ice_cons_defn,
    ( if
        % If a type has two functors with the same name but different arities,
        % then it is possible for the TypeCtor test to succeed and the ConsId
        % tests to fail (due to the arity mismatch). In such cases, we need
        % to search the rest of the list.

        EntryConsDefn ^ cons_type_ctor = TypeCtor,
        ( ConsId = Entry ^ ice_fully_qual_cons_id
        ; list.member(ConsId, Entry ^ ice_other_cons_ids)
        )
    then
        ConsDefn = EntryConsDefn
    else
        search_inner_cons_ids_type_ctor(Entries, TypeCtor, ConsId, ConsDefn)
    ).

lookup_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn) :-
    ( if
        search_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId,
            ConsDefnPrime)
    then
        ConsDefn = ConsDefnPrime
    else
        unexpected($module, $pred, "lookup failed")
    ).

get_all_cons_defns(ConsTable, AllConsDefns) :-
    map.foldl(accumulate_all_inner_cons_defns, ConsTable, [], AllConsDefns).

:- pred accumulate_all_inner_cons_defns(string::in, inner_cons_table::in,
    assoc_list(cons_id, hlds_cons_defn)::in,
    assoc_list(cons_id, hlds_cons_defn)::out) is det.

accumulate_all_inner_cons_defns(_Name, InnerConsTable, !AllConsDefns) :-
    list.map(project_inner_cons_entry, InnerConsTable, InnerConsList),
    !:AllConsDefns = InnerConsList ++ !.AllConsDefns.

:- pred project_inner_cons_entry(inner_cons_entry::in,
    pair(cons_id, hlds_cons_defn)::out) is det.

project_inner_cons_entry(Entry, Pair) :-
    Entry = inner_cons_entry(MainConsId, _OtherConsIds, ConsDefn),
    Pair = MainConsId - ConsDefn.

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
    MainConsId = Entry ^ ice_fully_qual_cons_id,
    OtherConsIds = Entry ^ ice_other_cons_ids,
    return_cons_arities_inner_cons_ids([MainConsId | OtherConsIds], SymName,
        !Arities),
    return_cons_arities_inner(Entries, SymName, !Arities).

:- pred return_cons_arities_inner_cons_ids(list(cons_id)::in,
    sym_name::in, list(int)::in, list(int)::out) is det.

return_cons_arities_inner_cons_ids([], _, !Arities).
return_cons_arities_inner_cons_ids([ConsId | ConsIds], SymName, !Arities) :-
    ( if ConsId = cons(ThisSymName, ThisArity, _) then
        ( if ThisSymName = SymName then
            !:Arities = [ThisArity | !.Arities]
        else
            true
        )
    else
        unexpected($module, $pred, "ConsId is not cons")
    ),
    return_cons_arities_inner_cons_ids(ConsIds, SymName, !Arities).

replace_cons_defns_in_cons_table(Replace, !ConsTable) :-
    map.map_values_only(replace_cons_defns_in_inner_cons_table(Replace),
        !ConsTable).

:- pred replace_cons_defns_in_inner_cons_table(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    inner_cons_table::in, inner_cons_table::out) is det.

replace_cons_defns_in_inner_cons_table(Replace, !InnerConsTable) :-
    list.map(replace_cons_defns_in_inner_cons_entry(Replace),
        !InnerConsTable).

:- pred replace_cons_defns_in_inner_cons_entry(
    pred(hlds_cons_defn, hlds_cons_defn)::in(pred(in, out) is det),
    inner_cons_entry::in, inner_cons_entry::out) is det.

replace_cons_defns_in_inner_cons_entry(Replace, !Entry) :-
    ConsDefn0 = !.Entry ^ ice_cons_defn,
    Replace(ConsDefn0, ConsDefn),
    !Entry ^ ice_cons_defn := ConsDefn.

cons_table_optimize(!ConsTable) :-
    map.optimize(!ConsTable).

%---------------------------------------------------------------------------%

:- interface.

:- type ctor_field_table == map(sym_name, list(hlds_ctor_field_defn)).

:- type hlds_ctor_field_defn
    --->    hlds_ctor_field_defn(
                % The context of the field definition.
                field_context   :: prog_context,

                field_status    :: type_status,

                % The type containing the field.
                field_type_ctor :: type_ctor,

                % The constructor containing the field.
                field_cons_id   :: cons_id,

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

:- interface.

    % The symbol table for types. Conceptually, it is a map from type_ctors
    % to hlds_type_defns, but the implementation may be different for
    % efficiency.
    %
:- type type_table.

:- func init_type_table = type_table.

:- pred add_type_ctor_defn(type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out) is det.

:- pred replace_type_ctor_defn(type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out) is det.

:- pred search_type_ctor_defn(type_table::in, type_ctor::in,
    hlds_type_defn::out) is semidet.
:- pred lookup_type_ctor_defn(type_table::in, type_ctor::in,
    hlds_type_defn::out) is det.

:- pred get_all_type_ctor_defns(type_table::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

:- pred foldl_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T)::
        in(pred(in, in, in, out) is det),
    type_table::in, T::in, T::out) is det.

:- pred foldl2_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T, U, U)::
        in(pred(in, in, in, out, in, out) is det),
    type_table::in, T::in, T::out, U::in, U::out) is det.

:- pred foldl3_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T, U, U, V, V)::
        in(pred(in, in, in, out, in, out, in, out) is det),
    type_table::in, T::in, T::out, U::in, U::out, V::in, V::out) is det.

:- pred map_foldl_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, hlds_type_defn, T, T)::
        in(pred(in, in, out, in, out) is det),
    type_table::in, type_table::out, T::in, T::out) is det.

%---------------------------------------------------------------------------%

    % Have we reported an error for this type definition yet?
:- type type_defn_prev_errors
    --->    type_defn_no_prev_errors
    ;       type_defn_prev_errors.

    % This is how type, modes and constructors are represented. The parts that
    % are not defined here (i.e. type_param, constructor, type, inst and mode)
    % are represented in the same way as in parse tree, and are defined there.
    %
    % An hlds_type_defn holds the information about a type definition.
:- type hlds_type_defn.

:- pred create_hlds_type_defn(tvarset::in, list(type_param)::in,
    tvar_kind_map::in, hlds_type_body::in, bool::in,
    type_status::in, need_qualifier::in, type_defn_prev_errors::in,
    prog_context::in, hlds_type_defn::out) is det.

:- pred get_type_defn_tvarset(hlds_type_defn::in, tvarset::out) is det.
:- pred get_type_defn_tparams(hlds_type_defn::in, list(type_param)::out)
    is det.
:- pred get_type_defn_kind_map(hlds_type_defn::in, tvar_kind_map::out) is det.
:- pred get_type_defn_body(hlds_type_defn::in, hlds_type_body::out) is det.
:- pred get_type_defn_status(hlds_type_defn::in, type_status::out) is det.
:- pred get_type_defn_in_exported_eqv(hlds_type_defn::in, bool::out) is det.
:- pred get_type_defn_ctors_need_qualifier(hlds_type_defn::in,
    need_qualifier::out) is det.
:- pred get_type_defn_prev_errors(hlds_type_defn::in,
    type_defn_prev_errors::out) is det.
:- pred get_type_defn_context(hlds_type_defn::in, prog_context::out) is det.

:- pred set_type_defn_body(hlds_type_body::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_tvarset(tvarset::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_status(type_status::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_in_exported_eqv(bool::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_prev_errors(type_defn_prev_errors::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.

    % An `hlds_type_body' holds the body of a type definition:
    % du = discriminated union, eqv_type = equivalence type (a type defined
    % to be equivalent to some other type), and solver_type.
    %
:- type hlds_type_body
    --->    hlds_du_type(
                % The ctors for this type.
                du_type_ctors               :: list(constructor),

                % Their tag values.
                du_type_cons_tag_values     :: cons_tag_values,

                du_type_cheaper_tag_test    :: maybe_cheaper_tag_test,

                % Is this type an enumeration or a dummy type?
                du_type_kind                :: du_type_kind,

                % User-defined equality and comparison preds.
                du_type_usereq              :: maybe(unify_compare),

                % Direct argument functors.
                du_direct_arg_ctors         :: maybe(list(sym_name_and_arity)),

                % Is there a `:- pragma reserve_tag' pragma for this type?
                du_type_reserved_tag        :: uses_reserved_tag,

                % Does the type representation use a reserved address?
                du_type_reserved_addr       :: uses_reserved_address,

                % Are there `:- pragma foreign' type declarations for
                % this type?
                du_type_is_foreign_type     :: maybe(foreign_type_body)
            )
    ;       hlds_eqv_type(mer_type)
    ;       hlds_foreign_type(foreign_type_body)
    ;       hlds_solver_type(type_details_solver)
    ;       hlds_abstract_type(type_details_abstract).

:- type maybe_cheaper_tag_test
    --->    no_cheaper_tag_test
    ;       cheaper_tag_test(
                more_expensive_cons_id  :: cons_id,
                more_expensive_cons_tag :: cons_tag,
                less_expensive_cons_id  :: cons_id,
                less_expensive_cons_tag :: cons_tag
            ).

:- type du_type_kind
    --->    du_type_kind_mercury_enum
    ;       du_type_kind_foreign_enum(
                dtkfe_language      :: foreign_language
            )
    ;       du_type_kind_direct_dummy
    ;       du_type_kind_notag(
                % A notag type is a dummy type if and only if the type it wraps
                % is a dummy type.
                dtkn_functor_name   :: sym_name,
                dtkn_arg_type       :: mer_type,
                dtkn_maybe_arg_name :: maybe(string)
            )
    ;       du_type_kind_general.

:- type foreign_type_body
    --->    foreign_type_body(
                c       :: foreign_type_lang_body(c_foreign_type),
                java    :: foreign_type_lang_body(java_foreign_type),
                csharp  :: foreign_type_lang_body(csharp_foreign_type),
                erlang  :: foreign_type_lang_body(erlang_foreign_type)
            ).

:- type foreign_type_lang_body(T) == maybe(foreign_type_lang_data(T)).

    % Foreign types may have user-defined equality and comparison
    % preds, but not solver_type_details.
    %
:- type foreign_type_lang_data(T)
    --->    foreign_type_lang_data(
                T,
                maybe(unify_compare),
                foreign_type_assertions
            ).

    % The `cons_tag_values' type stores the information on how a discriminated
    % union type is represented. For each functor in the d.u. type, it gives
    % a cons_tag which specifies how that functor and its arguments are
    % represented.
    %
    % XXX At the moment, every value is in the map several times.
    % One reason for duplicating values is to be able to get at them
    % with the sym_name in the cons_id key being fully qualified, unqualified,
    % or anything in between. Another reason is the need to get at them
    % with the cons_id containing the standard dummy type_ctor before
    % post-typecheck, and with the actual correct type_ctor after
    % post-typecheck.
    %
    % The key in the map should be just a string/arity pair.
    %
:- type cons_tag_values == map(cons_id, cons_tag).

    % A cons_id together with its tag.
    %
:- type tagged_cons_id
    --->    tagged_cons_id(cons_id, cons_tag).

    % Return the tag inside a tagged_cons_id.
    %
:- func project_tagged_cons_id_tag(tagged_cons_id) = cons_tag.

    % A `cons_tag' specifies how a functor and its arguments (if any) are
    % represented. Currently all values are represented as a single word;
    % values which do not fit into a word are represented by a (possibly
    % tagged) pointer to memory on the heap.
    %
:- type cons_tag
    --->    string_tag(string)
            % Strings are represented using the MR_string_const() macro;
            % in the current implementation, Mercury strings are represented
            % just as C null-terminated strings.

    ;       float_tag(float)
            % Floats are represented using the MR_float_to_word(),
            % MR_word_to_float(), and MR_float_const() macros. The default
            % implementation of these is to use boxed double-precision floats.

    ;       int_tag(int)
            % This means the constant is represented just as a word containing
            % the specified integer value. This is used for enumerations and
            % character constants as well as for int constants.

    ;       uint_tag(uint)
            % This means the constant is represented just as a word containing
            % the specified unsigned integer value. This is used for uint
            % constants.

            % XXX FIXED SIZE INT
    ;       int8_tag(int)
    ;       uint8_tag(int)
    ;       int16_tag(int)
    ;       uint16_tag(int)
    ;       int32_tag(int)
    ;       uint32_tag(int)

    ;       foreign_tag(foreign_language, string)
            % This means the constant is represented by the string which is
            % embedded directly in the target language. This is used for
            % foreign enumerations, i.e. those enumeration types that are the
            % subject of a foreign_enum pragma.

    ;       closure_tag(pred_id, proc_id, lambda_eval_method)
            % Higher-order pred closures tags. These are represented as
            % a pointer to an argument vector. For closures with
            % lambda_eval_method `normal', the first two words of the argument
            % vector hold the number of args and the address of the procedure
            % respectively. The remaining words hold the arguments.

    ;       type_ctor_info_tag(module_name, string, arity)
            % This is how we refer to type_ctor_info structures represented
            % as global data. The args are the name of the module the type
            % is defined in, and the name of the type, and its arity.

    ;       base_typeclass_info_tag(module_name, class_id, string)
            % This is how we refer to base_typeclass_info structures
            % represented as global data. The first argument is the name
            % of the module containing the instance declaration, the second
            % is the class name and arity, while the third is the string which
            % uniquely identifies the instance declaration (it is made from
            % the type of the arguments to the instance decl).

    ;       type_info_const_tag(int)
    ;       typeclass_info_const_tag(int)

    ;       ground_term_const_tag(int, cons_tag)

    ;       tabling_info_tag(pred_id, proc_id)
            % This is how we refer to the global structures containing
            % tabling pointer variables and related data. The word just
            % contains the address of the global struct.

    ;       deep_profiling_proc_layout_tag(pred_id, proc_id)
            % This is for constants representing procedure descriptions for
            % deep profiling.

    ;       table_io_entry_tag(pred_id, proc_id)
            % This is for constants representing the structure that allows us
            % to decode the contents of the answer block containing the
            % headvars of I/O primitives.

    ;       single_functor_tag
            % This is for types with a single functor (and possibly also some
            % constants represented using reserved addresses -- see below).
            % For these types, we don't need any tags. We just store a pointer
            % to the argument vector.

    ;       unshared_tag(tag_bits)
            % This is for constants or functors which can be distinguished
            % with just a primary tag. An "unshared" tag is one which fits
            % on the bottom of a pointer (i.e. two bits for 32-bit
            % architectures, or three bits for 64-bit architectures), and is
            % used for just one functor. For constants we store a tagged zero,
            % for functors we store a tagged pointer to the argument vector.

    ;       direct_arg_tag(tag_bits)
            % This is for functors which can be distinguished with just a
            % primary tag. The primary tag says which of the type's functors
            % (which must have arity 1) this word represents. However, the
            % body of the word is not a pointer to a cell holding the argument;
            % it IS the value of that argument, which must be an untagged
            % pointer to a cell.

    ;       shared_remote_tag(tag_bits, int)
            % This is for functors or constants which require more than just
            % a primary tag. In this case, we use both a primary and a
            % secondary tag. Several functors share the primary tag and are
            % distinguished by the secondary tag. The secondary tag is stored
            % as the first word of the argument vector. (If it is a constant,
            % then in this case there is an argument vector of size 1 which
            % just holds the secondary tag.)

    ;       shared_local_tag(tag_bits, int)
            % This is for constants which require more than a two-bit tag.
            % In this case, we use both a primary and a secondary tag,
            % but this time the secondary tag is stored in the rest of the
            % main word rather than in the first word of the argument vector.

    ;       no_tag
            % This is for types with a single functor of arity one. In this
            % case, we don't need to store the functor, and instead we store
            % the argument directly.

    ;       reserved_address_tag(reserved_address)
            % This is for constants represented as null pointers, or as
            % other reserved values in the address space.

    ;       shared_with_reserved_addresses_tag(list(reserved_address),
                cons_tag).
            % This is for constructors of discriminated union types where
            % one or more of the *other* constructors for that type is
            % represented as a reserved address. Any semidet deconstruction
            % against a constructor represented as a
            % shared_with_reserved_addresses cons_tag must check that
            % the value isn't any of the reserved addresses before testing
            % for the constructor's own cons_tag.

:- type reserved_address
    --->    null_pointer
            % This is for constants which are represented as a null pointer.

    ;       small_pointer(int)
            % This is for constants which are represented as a small integer,
            % cast to a pointer.

    ;       reserved_object(type_ctor, sym_name, arity).
            % This is for constants which are represented as the address
            % of a specially reserved global variable.

    % The type `tag_bits' holds a primary tag value.
    %
:- type tag_bits == int.        % actually only 2 (or maybe 3) bits

    % Return the primary tag, if any, for a cons_tag.
    % A return value of `no' means the primary tag is unknown.
    % A return value of `yes(N)' means the primary tag is N.
    % (`yes(0)' also corresponds to the case where there no primary tag.)
    %
:- func get_primary_tag(cons_tag) = maybe(int).

    % Return the secondary tag, if any, for a cons_tag.
    % A return value of `no' means there is no secondary tag.
    %
:- func get_secondary_tag(cons_tag) = maybe(int).

    % The type definitions for no_tag types have information mirrored in a
    % separate table for faster lookups. mode_util.mode_to_arg_mode makes
    % heavy use of type_util.type_is_no_tag_type.
    %
:- type no_tag_type
    --->    no_tag_type(
                list(type_param),   % Formal type parameters.
                sym_name,           % Constructor name.
                mer_type              % Argument type.
            ).

    % A type_ctor essentially contains three components. The raw name
    % of the type constructor, its module qualification, and its arity.
    % I (zs) tried replacing this table with a two-stage map (from raw name
    % to a subtable that itself mapped the full type_ctor to no_tag_type,
    % in an attempt to make the main part of the looked use cheaper
    % comparisons, on just raw strings. However, this change effectively led
    % to no change in performance.
:- type no_tag_type_table == map(type_ctor, no_tag_type).

:- func get_maybe_cheaper_tag_test(hlds_type_body) = maybe_cheaper_tag_test.

    % The atomic variants of the Boehm gc allocator calls (e.g.
    % GC_malloc_atomic instead of GC_malloc) may yield slightly faster code
    % since atomic blocks are not scanned for included pointers. However,
    % this makes them safe to use *only* if the block allocated this way
    % can never contain any pointer the Boehm collector would be interested
    % in tracing. In particular, even if the cell initially contains no
    % pointers, we must still use may_not_use_atomic_alloc for it if the cell
    % could possibly be reused later by compile-time garbage collection.
    %
:- type may_use_atomic_alloc
    --->    may_use_atomic_alloc
    ;       may_not_use_atomic_alloc.

    % Check asserted properties of a foreign type.
    %
:- pred asserted_can_pass_as_mercury_type(foreign_type_assertions::in)
    is semidet.
:- pred asserted_stable(foreign_type_assertions::in) is semidet.
:- pred asserted_word_aligned_pointer(foreign_type_assertions::in) is semidet.

:- implementation.

project_tagged_cons_id_tag(TaggedConsId) = Tag :-
    TaggedConsId = tagged_cons_id(_, Tag).

get_primary_tag(Tag) = MaybePrimaryTag :-
    (
        % In some of the cases where we return `no' here,
        % it would probably be OK to return `yes(0)'.
        % But it's safe to be conservative...
        ( Tag = int_tag(_)
        ; Tag = uint_tag(_)
        ; Tag = int8_tag(_)
        ; Tag = uint8_tag(_)
        ; Tag = int16_tag(_)
        ; Tag = uint16_tag(_)
        ; Tag = int32_tag(_)
        ; Tag = uint32_tag(_)
        ; Tag = float_tag(_)
        ; Tag = string_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = no_tag
        ; Tag = reserved_address_tag(_)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ),
        MaybePrimaryTag = no
    ;
        Tag = ground_term_const_tag(_, SubTag),
        MaybePrimaryTag = get_primary_tag(SubTag)
    ;
        Tag = single_functor_tag,
        MaybePrimaryTag = yes(0)
    ;
        ( Tag = unshared_tag(PrimaryTag)
        ; Tag = direct_arg_tag(PrimaryTag)
        ; Tag = shared_remote_tag(PrimaryTag, _SecondaryTag)
        ; Tag = shared_local_tag(PrimaryTag, _SecondaryTag)
        ),
        MaybePrimaryTag = yes(PrimaryTag)
    ;
        Tag = shared_with_reserved_addresses_tag(_RAs, InnerTag),
        MaybePrimaryTag = get_primary_tag(InnerTag)
    ).

get_secondary_tag(Tag) = MaybeSecondaryTag :-
    (
        ( Tag = int_tag(_)
        ; Tag = uint_tag(_)
        ; Tag = int8_tag(_)
        ; Tag = uint8_tag(_)
        ; Tag = int16_tag(_)
        ; Tag = uint16_tag(_)
        ; Tag = int32_tag(_)
        ; Tag = uint32_tag(_)
        ; Tag = float_tag(_)
        ; Tag = string_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = no_tag
        ; Tag = reserved_address_tag(_)
        ; Tag = unshared_tag(_PrimaryTag)
        ; Tag = direct_arg_tag(_PrimaryTag)
        ; Tag = single_functor_tag
        ),
        MaybeSecondaryTag = no
    ;
        Tag = ground_term_const_tag(_, SubTag),
        MaybeSecondaryTag = get_secondary_tag(SubTag)
    ;
        ( Tag = shared_remote_tag(_PrimaryTag, SecondaryTag)
        ; Tag = shared_local_tag(_PrimaryTag, SecondaryTag)
        ),
        MaybeSecondaryTag = yes(SecondaryTag)
    ;
        Tag = shared_with_reserved_addresses_tag(_RAs, InnerTag),
        MaybeSecondaryTag = get_secondary_tag(InnerTag)
    ).

get_maybe_cheaper_tag_test(TypeBody) = CheaperTagTest :-
    (
        TypeBody = hlds_du_type(_, _, CheaperTagTest, _, _, _, _, _, _)
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(_)
        ),
        CheaperTagTest = no_cheaper_tag_test
    ).

asserted_can_pass_as_mercury_type(foreign_type_assertions(Set)) :-
    (
        set.contains(Set, foreign_type_can_pass_as_mercury_type)
    ;
        set.contains(Set, foreign_type_word_aligned_pointer)
    ).

asserted_stable(Assertions) :-
    Assertions = foreign_type_assertions(Set),
    set.contains(Set, foreign_type_stable),
    asserted_can_pass_as_mercury_type(Assertions).

asserted_word_aligned_pointer(foreign_type_assertions(Set)) :-
    set.contains(Set, foreign_type_word_aligned_pointer).

%---------------------------------------------------------------------------%

:- type type_table == map(string, type_ctor_table).

:- type type_ctor_table == map(type_ctor, hlds_type_defn).

init_type_table = map.init.

add_type_ctor_defn(TypeCtor, TypeDefn, !TypeTable) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    ( if map.search(!.TypeTable, Name, TypeCtorTable0) then
        map.det_insert(TypeCtor, TypeDefn, TypeCtorTable0, TypeCtorTable),
        map.det_update(Name, TypeCtorTable, !TypeTable)
    else
        TypeCtorTable = map.singleton(TypeCtor, TypeDefn),
        map.det_insert(Name, TypeCtorTable, !TypeTable)
    ).

replace_type_ctor_defn(TypeCtor, TypeDefn, !TypeTable) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.lookup(!.TypeTable, Name, TypeCtorTable0),
    map.det_update(TypeCtor, TypeDefn, TypeCtorTable0, TypeCtorTable),
    map.det_update(Name, TypeCtorTable, !TypeTable).

search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.search(TypeTable, Name, TypeCtorTable),
    map.search(TypeCtorTable, TypeCtor, TypeDefn).

lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.lookup(TypeTable, Name, TypeCtorTable),
    map.lookup(TypeCtorTable, TypeCtor, TypeDefn).

get_all_type_ctor_defns(TypeTable, TypeCtorsDefns) :-
    map.foldl(get_all_type_ctor_defns_2, TypeTable, [], TypeCtorsDefns).

:- pred get_all_type_ctor_defns_2(string::in, type_ctor_table::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

get_all_type_ctor_defns_2(_Name, TypeCtorTable, !TypeCtorsDefns) :-
    map.to_assoc_list(TypeCtorTable, NameTypeCtorsDefns),
    !:TypeCtorsDefns = NameTypeCtorsDefns ++ !.TypeCtorsDefns.

foldl_over_type_ctor_defns(Pred, TypeTable, !Acc) :-
    map.foldl(foldl_over_type_ctor_defns_2(Pred), TypeTable, !Acc).

:- pred foldl_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T)::
        in(pred(in, in, in, out) is det),
    string::in, type_ctor_table::in, T::in, T::out) is det.

foldl_over_type_ctor_defns_2(Pred, _Name, TypeCtorTable, !Acc) :-
    map.foldl(Pred, TypeCtorTable, !Acc).

foldl2_over_type_ctor_defns(Pred, TypeTable, !AccA, !AccB) :-
    map.foldl2(foldl2_over_type_ctor_defns_2(Pred), TypeTable, !AccA, !AccB).

:- pred foldl2_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T, U, U)::
        in(pred(in, in, in, out, in, out) is det),
    string::in, type_ctor_table::in, T::in, T::out, U::in, U::out) is det.

foldl2_over_type_ctor_defns_2(Pred, _Name, TypeCtorTable, !AccA, !AccB) :-
    map.foldl2(Pred, TypeCtorTable, !AccA, !AccB).

foldl3_over_type_ctor_defns(Pred, TypeTable, !AccA, !AccB, !AccC) :-
    map.foldl3(foldl3_over_type_ctor_defns_2(Pred), TypeTable, !AccA, !AccB,
        !AccC).

:- pred foldl3_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T, U, U, V, V)::
        in(pred(in, in, in, out, in, out, in, out) is det),
    string::in, type_ctor_table::in, T::in, T::out, U::in, U::out,
        V::in, V::out) is det.

foldl3_over_type_ctor_defns_2(Pred, _Name, TypeCtorTable, !AccA, !AccB,
        !AccC) :-
    map.foldl3(Pred, TypeCtorTable, !AccA, !AccB, !AccC).

map_foldl_over_type_ctor_defns(Pred, !TypeTable, !Acc) :-
    map.map_foldl(map_foldl_over_type_ctor_defns_2(Pred), !TypeTable, !Acc).

:- pred map_foldl_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, hlds_type_defn, T, T)::
        in(pred(in, in, out, in, out) is det),
    string::in, type_ctor_table::in, type_ctor_table::out,
    T::in, T::out) is det.

map_foldl_over_type_ctor_defns_2(Pred, _Name, !TypeCtorTable, !Acc) :-
    map.map_foldl(Pred, !TypeCtorTable, !Acc).

%---------------------------------------------------------------------------%

:- type hlds_type_defn
    --->    hlds_type_defn(
                % Note that the first three of these fields are duplicated
                % in the hlds_cons_defns of the data constructors of the type
                % (if any).

                % Names of the type variables, if any.
                type_defn_tvarset           :: tvarset,

                % Formal type parameters.
                type_defn_params            :: list(type_param),

                % The kinds of the formal parameters.
                type_defn_kinds             :: tvar_kind_map,

                % The definition of the type.
                type_defn_body              :: hlds_type_body,

                % Does the type constructor appear on the right hand side
                % of a type equivalence defining a type that is visible from
                % outside this module? If yes, equiv_type_hlds may generate
                % references to this type constructor's unify and compare preds
                % from other modules even if the type is otherwise local
                % to the module, so we can't make their implementations private
                % to the module.
                %
                % Meaningful only after the equiv_type_hlds pass.
                type_defn_in_exported_eqv   :: bool,

                % Is the type defined in this module, and if yes,
                % is it exported.
                type_defn_status            :: type_status,

                % Do uses of the type's constructors need to be qualified?
                type_defn_ctors_need_qualifier :: need_qualifier,

                % Have we reported an error for this type definition yet?
                % If yes, then don't emit any more errors for it, since they
                % are very likely to be due to the compiler's incomplete
                % recovery from the previous error.
                type_defn_prev_errors       :: type_defn_prev_errors,

                % The location of this type definition in the original
                % source code.
                type_defn_context           :: prog_context
            ).

create_hlds_type_defn(Tvarset, Params, Kinds, TypeBody, InExportedEqv,
        TypeStatus, NeedQual, PrevErrors, Context, Defn) :-
    Defn = hlds_type_defn(Tvarset, Params, Kinds, TypeBody, InExportedEqv,
        TypeStatus, NeedQual, PrevErrors, Context).

get_type_defn_tvarset(Defn, X) :-
    X = Defn ^ type_defn_tvarset.
get_type_defn_tparams(Defn, X) :-
    X = Defn ^ type_defn_params.
get_type_defn_kind_map(Defn, X) :-
    X = Defn ^ type_defn_kinds.
get_type_defn_body(Defn, X) :-
    X = Defn ^ type_defn_body.
get_type_defn_status(Defn, X) :-
    X = Defn ^ type_defn_status.
get_type_defn_in_exported_eqv(Defn, X) :-
    X = Defn ^ type_defn_in_exported_eqv.
get_type_defn_ctors_need_qualifier(Defn, X) :-
    X = Defn ^ type_defn_ctors_need_qualifier.
get_type_defn_prev_errors(Defn, X) :-
    X = Defn ^ type_defn_prev_errors.
get_type_defn_context(Defn, X) :-
    X = Defn ^ type_defn_context.

set_type_defn_body(X, !Defn) :-
    !Defn ^ type_defn_body := X.
set_type_defn_tvarset(X, !Defn) :-
    !Defn ^ type_defn_tvarset := X.
set_type_defn_status(X, !Defn) :-
    !Defn ^ type_defn_status := X.
set_type_defn_in_exported_eqv(X, !Defn) :-
    !Defn ^ type_defn_in_exported_eqv := X.
set_type_defn_prev_errors(X, !Defn) :-
    !Defn ^ type_defn_prev_errors := X.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % An inst that is defined to be equivalent to a bound inst may be
    % declared by the programmer to be for a particular type constructor.
:- type inst_for_type_ctor
    --->    iftc_not_applicable
            % The inst is not defined to be equivalent to a bound inst.

    ;       iftc_applicable_declared(type_ctor)
            % The inst is defined to be equivalent to a bound inst,
            % and it is declared to be for this type constructor.
            % This requires that all the top level cons_ids in the bound inst
            % be function symbols of the given type constructor. Later,
            % it will also require that this inst be applied only to values
            % of this type.

    ;       iftc_applicable_not_known
            % The inst is defined to be equivalent to a bound inst.
            % It is not declared to be for a specific type constructor,
            % and the list of type constructors that its cons_ids match
            % is not (yet) known.

    ;       iftc_applicable_known(list(type_ctor))
            % The inst is defined to be equivalent to a bound inst.
            % It is not declared to be for a specific type constructor,
            % but the list of type constructors that its cons_ids match
            % is known to be the given list of type constructors.

    ;       iftc_applicable_error.
            % The inst is defined to be equivalent to a bound inst.
            % It is not declared to be for a specific type constructor,

    % An `hlds_inst_defn' holds the information we need to store
    % about inst definitions such as
    %   :- inst list_skel(I) = bound([] ; [I | list_skel(I)].
    %
:- type hlds_inst_defn
    --->    hlds_inst_defn(
                % The names of the inst parameters (if any).
                inst_varset             :: inst_varset,

                % The inst parameters (if any). ([I] in the above example.)
                inst_params             :: list(inst_var),

                % The definition of this inst.
                inst_body               :: hlds_inst_body,

                % If this inst is equivalent to a bound inst, is it
                % specified to be applicable only to a specific
                % type constructor? If not, is it known to be compatible
                % with a known set of type constructors? (This means
                % having top level function symbols that all come from
                % the type constructor.)
                inst_for_type           :: inst_for_type_ctor,

                % The location in the source code of this inst definition.
                inst_context            :: prog_context,

                % So intermod.m can tell whether to output this inst.
                inst_status             :: inst_status
            ).

:- type hlds_inst_body
    --->    eqv_inst(mer_inst)
            % This inst is equivalent to some other inst.

    ;       abstract_inst.
            % This inst is just a forward declaration; the real definition
            % will be filled in later.
            % (XXX Abstract insts are not really supported.)

:- type user_inst_table ==          map(inst_id, hlds_inst_defn).

:- type maybe_inst
    --->    inst_unknown
    ;       inst_known(mer_inst).

:- type maybe_inst_det
    --->    inst_det_unknown
    ;       inst_det_known(mer_inst, determinism).

:- type unify_inst_table.
:- type merge_inst_table.
:- type ground_inst_table.
:- type any_inst_table.
:- type shared_inst_table.
:- type mostly_uniq_inst_table.

:- pred lookup_unify_inst(unify_inst_table::in,
    unify_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_merge_inst(merge_inst_table::in,
    merge_inst_info::in, maybe_inst::out) is det.
:- pred lookup_ground_inst(ground_inst_table::in,
    ground_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_any_inst(any_inst_table::in,
    any_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_shared_inst(shared_inst_table::in,
    inst_name::in, maybe_inst::out) is det.
:- pred lookup_mostly_uniq_inst(mostly_uniq_inst_table::in,
    inst_name::in, maybe_inst::out) is det.

:- pred search_insert_unify_inst(
    unify_inst_info::in, maybe(maybe_inst_det)::out,
    unify_inst_table::in, unify_inst_table::out) is det.
:- pred search_insert_merge_inst(
    merge_inst_info::in, maybe(maybe_inst)::out,
    merge_inst_table::in, merge_inst_table::out) is det.
:- pred search_insert_ground_inst(
    ground_inst_info::in, maybe(maybe_inst_det)::out,
    ground_inst_table::in, ground_inst_table::out) is det.
:- pred search_insert_any_inst(
    any_inst_info::in, maybe(maybe_inst_det)::out,
    any_inst_table::in, any_inst_table::out) is det.
:- pred search_insert_shared_inst(
    inst_name::in, maybe(maybe_inst)::out,
    shared_inst_table::in, shared_inst_table::out) is det.
:- pred search_insert_mostly_uniq_inst(
    inst_name::in, maybe(maybe_inst)::out,
    mostly_uniq_inst_table::in, mostly_uniq_inst_table::out) is det.

:- pred det_update_unify_inst(unify_inst_info::in, maybe_inst_det::in,
    unify_inst_table::in, unify_inst_table::out) is det.
:- pred det_update_merge_inst(merge_inst_info::in, maybe_inst::in,
    merge_inst_table::in, merge_inst_table::out) is det.
:- pred det_update_ground_inst(ground_inst_info::in, maybe_inst_det::in,
    ground_inst_table::in, ground_inst_table::out) is det.
:- pred det_update_any_inst(any_inst_info::in, maybe_inst_det::in,
    any_inst_table::in, any_inst_table::out) is det.
:- pred det_update_shared_inst(inst_name::in, maybe_inst::in,
    shared_inst_table::in, shared_inst_table::out) is det.
:- pred det_update_mostly_uniq_inst(inst_name::in, maybe_inst::in,
    mostly_uniq_inst_table::in, mostly_uniq_inst_table::out) is det.

:- pred unify_insts_to_sorted_pairs(unify_inst_table::in,
    assoc_list(unify_inst_info, maybe_inst_det)::out) is det.
:- pred merge_insts_to_sorted_pairs(merge_inst_table::in,
    assoc_list(merge_inst_info, maybe_inst)::out) is det.
:- pred ground_insts_to_sorted_pairs(ground_inst_table::in,
    assoc_list(ground_inst_info, maybe_inst_det)::out) is det.
:- pred any_insts_to_sorted_pairs(any_inst_table::in,
    assoc_list(any_inst_info, maybe_inst_det)::out) is det.
:- pred shared_insts_to_sorted_pairs(shared_inst_table::in,
    assoc_list(inst_name, maybe_inst)::out) is det.
:- pred mostly_uniq_insts_to_sorted_pairs(mostly_uniq_inst_table::in,
    assoc_list(inst_name, maybe_inst)::out) is det.

:- pred unify_insts_from_sorted_pairs(
    assoc_list(unify_inst_info, maybe_inst_det)::in,
    unify_inst_table::out) is det.
:- pred merge_insts_from_sorted_pairs(
    assoc_list(merge_inst_info, maybe_inst)::in,
    merge_inst_table::out) is det.
:- pred ground_insts_from_sorted_pairs(
    assoc_list(ground_inst_info, maybe_inst_det)::in,
    ground_inst_table::out) is det.
:- pred any_insts_from_sorted_pairs(
    assoc_list(any_inst_info, maybe_inst_det)::in,
    any_inst_table::out) is det.
:- pred shared_insts_from_sorted_pairs(
    assoc_list(inst_name, maybe_inst)::in,
    shared_inst_table::out) is det.
:- pred mostly_uniq_insts_from_sorted_pairs(
    assoc_list(inst_name, maybe_inst)::in,
    mostly_uniq_inst_table::out) is det.

%---------------------------------------------------------------------------%

    % The symbol table for insts.
    %
:- type inst_table.

:- pred inst_table_init(inst_table::out) is det.

:- pred inst_table_get_user_insts(inst_table::in, user_inst_table::out) is det.
:- pred inst_table_get_unify_insts(inst_table::in, unify_inst_table::out)
    is det.
:- pred inst_table_get_merge_insts(inst_table::in, merge_inst_table::out)
    is det.
:- pred inst_table_get_ground_insts(inst_table::in, ground_inst_table::out)
    is det.
:- pred inst_table_get_any_insts(inst_table::in, any_inst_table::out) is det.
:- pred inst_table_get_shared_insts(inst_table::in, shared_inst_table::out)
    is det.
:- pred inst_table_get_mostly_uniq_insts(inst_table::in,
    mostly_uniq_inst_table::out) is det.

:- pred inst_table_set_user_insts(user_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_unify_insts(unify_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_merge_insts(merge_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_ground_insts(ground_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_any_insts(any_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_shared_insts(shared_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_mostly_uniq_insts(mostly_uniq_inst_table::in,
    inst_table::in, inst_table::out) is det.

:- implementation.

%---------------------------------------------------------------------------%
%
% I (zs) have tried making the merge_inst_table a two-stage table,
% i.e. being map(mer_inst, map(mer_inst, maybe_inst)), in the hope
% of making lookups faster, but it led to a slowdown, not a speedup.
% The main reason was the extra cost of insertions. While you can always
% use a search_insert operation on the inner map, you can do it on
% the outer map only if the first inst does not occur in the outer map.
% If it does, then you are *modifying* an existing entry, not inserting
% a new one, and you can't modify it without knowing what it is. Therefore
% in the common case, a search_insert on the whole merge_inst_table
% requires first a search_insert on the outer table, and when the search
% part of that succeeds, a search_insert on the inner table and then
% a straight *update* on the outer map. The main performance problem is
% the need for this update.
%
% I expect (though I have not tested it) that the same problem would arise
% if we turned the subtables of the unify_inst_table into two-stage maps.

:- type inst_pair
    --->    inst_pair(mer_inst, mer_inst).

:- type unify_inst_table
    --->    unify_inst_table(
                uit_live_real   ::  map(inst_pair, maybe_inst_det),
                uit_live_fake   ::  map(inst_pair, maybe_inst_det),
                uit_dead_real   ::  map(inst_pair, maybe_inst_det),
                uit_dead_fake   ::  map(inst_pair, maybe_inst_det)
            ).

:- type merge_inst_table ==         map(merge_inst_info, maybe_inst).
:- type ground_inst_table ==        map(ground_inst_info, maybe_inst_det).
:- type any_inst_table ==           map(any_inst_info, maybe_inst_det).
:- type shared_inst_table ==        map(inst_name, maybe_inst).
:- type mostly_uniq_inst_table ==   map(inst_name, maybe_inst).

%---------------------------------------------------------------------------%

lookup_unify_inst(UnifyInstTable, UnifyInstInfo, MaybeInstDet) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable = UnifyInstTable ^ uit_live_real,
        map.lookup(LiveRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable = UnifyInstTable ^ uit_live_fake,
        map.lookup(LiveFakeTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable = UnifyInstTable ^ uit_dead_real,
        map.lookup(DeadRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable = UnifyInstTable ^ uit_dead_fake,
        map.lookup(DeadFakeTable, InstPair, MaybeInstDet)
    ).

lookup_merge_inst(MergeInstTable, MergeInstInfo, MaybeInst) :-
    map.lookup(MergeInstTable, MergeInstInfo, MaybeInst).

lookup_ground_inst(GroundInstTable, GroundInstInfo, MaybeInstDet) :-
    map.lookup(GroundInstTable, GroundInstInfo, MaybeInstDet).

lookup_any_inst(AnyInstTable, AnyInstInfo, MaybeInstDet) :-
    map.lookup(AnyInstTable, AnyInstInfo, MaybeInstDet).

lookup_shared_inst(SharedInstTable, InstName, MaybeInst) :-
    map.lookup(SharedInstTable, InstName, MaybeInst).

lookup_mostly_uniq_inst(MostlyUniqInstTable, InstName, MaybeInst) :-
    map.lookup(MostlyUniqInstTable, InstName, MaybeInst).

%---------------------------------------------------------------------------%

search_insert_unify_inst(UnifyInstInfo, MaybeMaybeInstDet, !UnifyInstTable) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable0 = !.UnifyInstTable ^ uit_live_real,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            LiveRealTable0, LiveRealTable),
        !UnifyInstTable ^ uit_live_real := LiveRealTable
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable0 = !.UnifyInstTable ^ uit_live_fake,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            LiveFakeTable0, LiveFakeTable),
        !UnifyInstTable ^ uit_live_fake := LiveFakeTable
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable0 = !.UnifyInstTable ^ uit_dead_real,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            DeadRealTable0, DeadRealTable),
        !UnifyInstTable ^ uit_dead_real := DeadRealTable
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable0 = !.UnifyInstTable ^ uit_dead_fake,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            DeadFakeTable0, DeadFakeTable),
        !UnifyInstTable ^ uit_dead_fake := DeadFakeTable
    ).

search_insert_merge_inst(MergeInstInfo, MaybeMaybeInst, !MergeInstTable) :-
    map.search_insert(MergeInstInfo, inst_unknown, MaybeMaybeInst,
        !MergeInstTable).

search_insert_ground_inst(GroundInstInfo, MaybeMaybeInstDet,
        !GroundInstTable) :-
    map.search_insert(GroundInstInfo, inst_det_unknown, MaybeMaybeInstDet,
        !GroundInstTable).

search_insert_any_inst(AnyInstInfo, MaybeMaybeInstDet, !AnyInstTable) :-
    map.search_insert(AnyInstInfo, inst_det_unknown, MaybeMaybeInstDet,
        !AnyInstTable).

search_insert_shared_inst(InstName, MaybeMaybeInst, !SharedInstTable) :-
    map.search_insert(InstName, inst_unknown, MaybeMaybeInst,
        !SharedInstTable).

search_insert_mostly_uniq_inst(InstName, MaybeMaybeInst,
        !MostlyUniqInstTable) :-
    map.search_insert(InstName, inst_unknown, MaybeMaybeInst,
        !MostlyUniqInstTable).

%---------------------------------------------------------------------------%

det_update_unify_inst(UnifyInstInfo, MaybeInstDet, !UnifyInstTable) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable0 = !.UnifyInstTable ^ uit_live_real,
        map.det_update(InstPair, MaybeInstDet, LiveRealTable0, LiveRealTable),
        !UnifyInstTable ^ uit_live_real := LiveRealTable
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable0 = !.UnifyInstTable ^ uit_live_fake,
        map.det_update(InstPair, MaybeInstDet, LiveFakeTable0, LiveFakeTable),
        !UnifyInstTable ^ uit_live_fake := LiveFakeTable
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable0 = !.UnifyInstTable ^ uit_dead_real,
        map.det_update(InstPair, MaybeInstDet, DeadRealTable0, DeadRealTable),
        !UnifyInstTable ^ uit_dead_real := DeadRealTable
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable0 = !.UnifyInstTable ^ uit_dead_fake,
        map.det_update(InstPair, MaybeInstDet, DeadFakeTable0, DeadFakeTable),
        !UnifyInstTable ^ uit_dead_fake := DeadFakeTable
    ).

det_update_merge_inst(MergeInstInfo, MaybeInst, !MergeInstTable) :-
    map.det_update(MergeInstInfo, MaybeInst, !MergeInstTable).

det_update_ground_inst(GroundInstInfo, MaybeInstDet, !GroundInstTable) :-
    map.det_update(GroundInstInfo, MaybeInstDet, !GroundInstTable).

det_update_any_inst(AnyInstInfo, MaybeInstDet, !AnyInstTable) :-
    map.det_update(AnyInstInfo, MaybeInstDet, !AnyInstTable).

det_update_shared_inst(InstName, MaybeInst, !SharedInstTable) :-
    map.det_update(InstName, MaybeInst, !SharedInstTable).

det_update_mostly_uniq_inst(InstName, MaybeInst, !MostlyUniqInstTable) :-
    map.det_update(InstName, MaybeInst, !MostlyUniqInstTable).

%---------------------------------------------------------------------------%

unify_insts_to_sorted_pairs(UnifyInstTable, AssocList) :-
    UnifyInstTable = unify_inst_table(LiveRealTable, LiveFakeTable,
        DeadRealTable, DeadFakeTable),
    map.to_assoc_list(LiveRealTable, LiveRealPairInsts),
    map.to_assoc_list(LiveFakeTable, LiveFakePairInsts),
    map.to_assoc_list(DeadRealTable, DeadRealPairInsts),
    map.to_assoc_list(DeadFakeTable, DeadFakePairInsts),
    some [!RevAssocList] (
        % The order in which we generate the four sublists corresponds to
        % the order in which we take them in unify_insts_from_sorted_pairs.
        !:RevAssocList = [],
        accumulate_unify_insts(is_live, real_unify, LiveRealPairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_live, fake_unify, LiveFakePairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_dead, real_unify, DeadRealPairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_dead, fake_unify, DeadFakePairInsts,
            !RevAssocList),
        list.reverse(!.RevAssocList, AssocList)
    ).

:- pred accumulate_unify_insts(is_live::in, unify_is_real::in,
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(unify_inst_info, maybe_inst_det)::in,
    assoc_list(unify_inst_info, maybe_inst_det)::out) is det.

accumulate_unify_insts(_IsLive, _IsReal, [], !RevAssocList).
accumulate_unify_insts(IsLive, IsReal, [PairMaybeInst | PairMaybeInsts],
        !RevAssocList) :-
    PairMaybeInst = inst_pair(InstA, InstB) - MaybeInst,
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    !:RevAssocList = [UnifyInstInfo - MaybeInst | !.RevAssocList],
    accumulate_unify_insts(IsLive, IsReal, PairMaybeInsts, !RevAssocList).

%---------------------%

merge_insts_to_sorted_pairs(MergeInstTable, AssocList) :-
    map.to_sorted_assoc_list(MergeInstTable, AssocList).

ground_insts_to_sorted_pairs(GroundInstTable, AssocList) :-
    map.to_sorted_assoc_list(GroundInstTable, AssocList).

any_insts_to_sorted_pairs(AnyInstTable, AssocList) :-
    map.to_sorted_assoc_list(AnyInstTable, AssocList).

shared_insts_to_sorted_pairs(SharedInstTable, AssocList) :-
    map.to_sorted_assoc_list(SharedInstTable, AssocList).

mostly_uniq_insts_to_sorted_pairs(MostlyUniqInstTable, AssocList) :-
    map.to_sorted_assoc_list(MostlyUniqInstTable, AssocList).

%---------------------------------------------------------------------------%

unify_insts_from_sorted_pairs(AssocList0, UnifyInstTable) :-
    % The order in which we take the four sublists corresponds to
    % the order in which we generate them in unify_insts_to_sorted_pairs.
    unify_inst_subtable_from_sorted_pairs(is_live, real_unify,
        AssocList0, AssocList1, [], RevLiveRealAssocList),
    unify_inst_subtable_from_sorted_pairs(is_live, fake_unify,
        AssocList1, AssocList2, [], RevLiveFakeAssocList),
    unify_inst_subtable_from_sorted_pairs(is_dead, real_unify,
        AssocList2, AssocList3, [], RevDeadRealAssocList),
    unify_inst_subtable_from_sorted_pairs(is_dead, fake_unify,
        AssocList3, AssocList4, [], RevDeadFakeAssocList),
    expect(unify(AssocList4, []), $module, $pred, "AssocList4 != []"),
    map.from_rev_sorted_assoc_list(RevLiveRealAssocList, LiveRealTable),
    map.from_rev_sorted_assoc_list(RevLiveFakeAssocList, LiveFakeTable),
    map.from_rev_sorted_assoc_list(RevDeadRealAssocList, DeadRealTable),
    map.from_rev_sorted_assoc_list(RevDeadFakeAssocList, DeadFakeTable),
    UnifyInstTable = unify_inst_table(LiveRealTable, LiveFakeTable,
        DeadRealTable, DeadFakeTable).

:- pred unify_inst_subtable_from_sorted_pairs(is_live::in, unify_is_real::in,
    assoc_list(unify_inst_info, maybe_inst_det)::in,
    assoc_list(unify_inst_info, maybe_inst_det)::out,
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(inst_pair, maybe_inst_det)::out) is det.

unify_inst_subtable_from_sorted_pairs(_ExpLive, _ExpReal,
        [], [], !RevSubTablePairs).
unify_inst_subtable_from_sorted_pairs(ExpLive, ExpReal,
        [Pair | Pairs], LeftOverPairs, !RevSubTablePairs) :-
    Pair = UnifyInstInfo - MaybeInstDet,
    UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
    ( if Live = ExpLive, Real = ExpReal then
        InstPair = inst_pair(InstA, InstB),
        !:RevSubTablePairs = [InstPair - MaybeInstDet | !.RevSubTablePairs],
        unify_inst_subtable_from_sorted_pairs(ExpLive, ExpReal,
            Pairs, LeftOverPairs, !RevSubTablePairs)
    else
        LeftOverPairs = [Pair | Pairs]
    ).

%---------------------%

merge_insts_from_sorted_pairs(AssocList, MergeInstTable) :-
    map.from_sorted_assoc_list(AssocList, MergeInstTable).

ground_insts_from_sorted_pairs(AssocList, GroundInstTable) :-
    map.from_sorted_assoc_list(AssocList, GroundInstTable).

any_insts_from_sorted_pairs(AssocList, AnyInstTable) :-
    map.from_sorted_assoc_list(AssocList, AnyInstTable).

shared_insts_from_sorted_pairs(AssocList, SharedInstTable) :-
    map.from_sorted_assoc_list(AssocList, SharedInstTable).

mostly_uniq_insts_from_sorted_pairs(AssocList, MostlyUniqInstTable) :-
    map.from_sorted_assoc_list(AssocList, MostlyUniqInstTable).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type inst_table
    --->    inst_table(
                inst_table_user         :: user_inst_table,
                inst_table_unify        :: unify_inst_table,
                inst_table_merge        :: merge_inst_table,
                inst_table_ground       :: ground_inst_table,
                inst_table_any          :: any_inst_table,
                inst_table_shared       :: shared_inst_table,
                inst_table_mostly_uniq  :: mostly_uniq_inst_table
            ).

inst_table_init(InstTable) :-
    map.init(UserInsts),
    UnifyInsts = unify_inst_table(map.init, map.init, map.init, map.init),
    map.init(MergeInsts),
    map.init(GroundInsts),
    map.init(SharedInsts),
    map.init(AnyInsts),
    map.init(NondetLiveInsts),
    InstTable = inst_table(UserInsts, UnifyInsts, MergeInsts, GroundInsts,
        AnyInsts, SharedInsts, NondetLiveInsts).

inst_table_get_user_insts(InstTable, X) :-
    X = InstTable ^ inst_table_user.
inst_table_get_unify_insts(InstTable, X) :-
    X = InstTable ^ inst_table_unify.
inst_table_get_merge_insts(InstTable, X) :-
    X = InstTable ^ inst_table_merge.
inst_table_get_ground_insts(InstTable, X) :-
    X = InstTable ^ inst_table_ground.
inst_table_get_any_insts(InstTable, X) :-
    X = InstTable ^ inst_table_any.
inst_table_get_shared_insts(InstTable, X) :-
    X = InstTable ^ inst_table_shared.
inst_table_get_mostly_uniq_insts(InstTable, X) :-
    X = InstTable ^ inst_table_mostly_uniq.

inst_table_set_user_insts(X, !InstTable) :-
    !InstTable ^ inst_table_user := X.
inst_table_set_unify_insts(X, !InstTable) :-
    !InstTable ^ inst_table_unify := X.
inst_table_set_merge_insts(X, !InstTable) :-
    !InstTable ^ inst_table_merge := X.
inst_table_set_ground_insts(X, !InstTable) :-
    !InstTable ^ inst_table_ground := X.
inst_table_set_any_insts(X, !InstTable) :-
    !InstTable ^ inst_table_any := X.
inst_table_set_shared_insts(X, !InstTable) :-
    !InstTable ^ inst_table_shared := X.
inst_table_set_mostly_uniq_insts(X, !InstTable) :-
    !InstTable ^ inst_table_mostly_uniq := X.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % The symbol table for modes.
    %
:- type mode_table.
:- type mode_defns == map(mode_id, hlds_mode_defn).

    % A hlds_mode_defn stores the information about a mode
    % definition such as
    %   :- mode out == free >> ground.
    % or
    %   :- mode in(I) == I >> I.
    % or
    %   :- mode in_list_skel == in(list_skel).
    %
:- type hlds_mode_defn
    --->    hlds_mode_defn(
                % The names of the inst parameters (if any).
                mode_varset     :: inst_varset,

                % The list of the inst parameters (if any).
                % (e.g. [I] for the second example above.)
                mode_params     :: list(inst_var),

                % The definition of this mode.
                mody_body       :: hlds_mode_body,

                % The location of this mode definition in the source code.
                mode_context    :: prog_context,

                % So intermod.m can tell whether to output this mode.
                mode_status     :: mode_status
            ).

    % The only sort of mode definitions allowed are equivalence modes.
    %
:- type hlds_mode_body
    --->    eqv_mode(mer_mode).  % This mode is equivalent to some other mode.

    % Given a mode table get the mode_id - hlds_mode_defn map.
    %
:- pred mode_table_get_mode_defns(mode_table::in, mode_defns::out) is det.

    % Insert a mode_id and corresponding hlds_mode_defn into the mode_table.
    % Fail if the mode_id is already present in the table.
    %
:- pred mode_table_insert(mode_id::in, hlds_mode_defn::in,
    mode_table::in, mode_table::out) is semidet.

:- pred mode_table_init(mode_table::out) is det.

    % Optimize the mode table for lookups.
    %
:- pred mode_table_optimize(mode_table::in, mode_table::out) is det.

:- implementation.

:- type mode_table == mode_defns.

mode_table_get_mode_defns(ModeDefns, ModeDefns).

mode_table_insert(ModeId, ModeDefn, !ModeDefns) :-
    map.insert(ModeId, ModeDefn, !ModeDefns).

mode_table_init(map.init).

mode_table_optimize(!ModeDefns) :-
    map.optimize(!ModeDefns).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

:- type class_table == map(class_id, hlds_class_defn).

    % Information about a single `typeclass' declaration.
    %
:- type hlds_class_defn
    --->    hlds_class_defn(
                classdefn_status            :: typeclass_status,

                % SuperClasses.
                classdefn_supers            :: list(prog_constraint),

                % Functional dependencies.
                classdefn_fundeps           :: hlds_class_fundeps,

                % All ancestors which have fundeps on them.
                classdefn_fundep_ancestors  :: list(prog_constraint),

                % ClassVars.
                classdefn_vars              :: list(tvar),

                % Kinds of class_vars.
                classdefn_kinds             :: tvar_kind_map,

                % The interface from the original declaration, used by
                % intermod.m to % write out the interface for a local typeclass
                % to the `.opt' file.
                classdefn_interface         :: class_interface,

                % Methods.
                classdefn_hlds_interface    :: hlds_class_interface,

                % VarNames.
                classdefn_tvarset           :: tvarset,

                % Location of declaration.
                classdefn_context           :: prog_context
            ).

    % In the HLDS, functional dependencies are represented using
    % argument positions (counting from 1) rather than type variables.
    % We know that there will be a one-one correspondence since
    % typeclass parameters must be distinct variables, and using
    % argument positions is more efficient.
    %
:- type hlds_class_fundeps == list(hlds_class_fundep).
:- type hlds_class_fundep
    --->    fundep(
                domain      :: set(hlds_class_argpos),
                range       :: set(hlds_class_argpos)
            ).

:- type hlds_class_argpos == int.

:- func restrict_list_elements(set(hlds_class_argpos), list(T)) = list(T).

:- type hlds_class_interface == list(pred_proc_id).

    % For each class, we keep track of a list of its instances, since there
    % can be more than one instance of each class. Each visible instance
    % is assigned a unique identifier (integers beginning from one).
    % The position in the list of instances corresponds to the instance_id.
    %
:- type instance_table == map(class_id, list(hlds_instance_defn)).

:- type instance_id == int.

    % Information about a single `instance' declaration.
    % The order of fields is intended to put the list of hlds_instance_defns
    % of a class into a stable order when the hlds_instance_defns are sorted.
    % ("Stable" meaning that changing *only* the order of the instance
    % declarations in a source module should leave the contents of the
    % .opt file unchanged.)
    %
:- type hlds_instance_defn
    --->    hlds_instance_defn(
                % Module of the instance declaration.
                instdefn_module         :: module_name,

                % The class types. The original types field is used only
                % for error checking.
                instdefn_types          :: list(mer_type),
                instdefn_orig_types     :: list(mer_type),

                % Import status of the instance declaration.
                % XXX This can be set to abstract_imported even if
                % the instance is NOT imported.
                instdefn_status         :: instance_status,

                % Context of declaration.
                instdefn_context        :: prog_context,

                % Constraints on the instance declaration.
                instdefn_constraints    :: list(prog_constraint),

                % Methods
                instdefn_body           :: instance_body,

                % After check_typeclass, we will know the pred_ids and proc_ids
                % of all the methods.
                instdefn_hlds_interface :: maybe(hlds_class_interface),

                % VarNames
                instdefn_tvarset        :: tvarset,

                % "Proofs" of how to build the typeclass_infos for the
                % superclasses of this class (that is, the constraints
                % on the class declaration), for this instance.
                instdefn_proofs         :: constraint_proof_map
            ).

    % Return the value of the MR_typeclass_info_num_extra_instance_args field
    % in the base_typeclass_info of the given instance.
    %
:- pred num_extra_instance_args(hlds_instance_defn::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

restrict_list_elements(Elements, List) = RestrictedList :-
    set.to_sorted_list(Elements, SortedElements),
    restrict_list_elements_2(SortedElements, 1, List, RestrictedList).

:- pred restrict_list_elements_2(list(hlds_class_argpos)::in,
    hlds_class_argpos::in, list(T)::in, list(T)::out) is det.

restrict_list_elements_2(_, _, [], []).
restrict_list_elements_2([], _, [_ | _], []).
restrict_list_elements_2([Posn | Posns], Index, [X | Xs], RestrictedXs) :-
    ( if Index = Posn then
        restrict_list_elements_2(Posns, Index + 1, Xs, TailRestrictedXs),
        RestrictedXs = [X | TailRestrictedXs]
    else if Index < Posn then
        restrict_list_elements_2([Posn | Posns], Index + 1, Xs, RestrictedXs)
    else
        restrict_list_elements_2(Posns, Index + 1, [X | Xs], RestrictedXs)
    ).

num_extra_instance_args(InstanceDefn, NumExtra) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule,
        InstanceTypes, _OrigInstanceTypes, _ImportStatus, _TermContext,
        InstanceConstraints, _Body, _PredProcIds, _Varset, _SuperClassProofs),
    type_vars_list(InstanceTypes, TypeVars),
    get_unconstrained_tvars(TypeVars, InstanceConstraints, Unconstrained),
    list.length(InstanceConstraints, NumConstraints),
    list.length(Unconstrained, NumUnconstrained),
    NumExtra = NumConstraints + NumUnconstrained.

%---------------------------------------------------------------------------%

:- interface.

    % Identifiers for constraints which are unique across a given type_assign.
    % Integers in these values refer to the position in the list of constraints
    % at that location, beginning from 1.
    %
    % Only identifiers for constraints appearing directly on a goal are needed
    % at the moment, so there is no way to represent the appropriate identifier
    % for the superclass of such a constraint.
    %
    % XXX A more robust and efficient solution would be to allocate
    % unique integers to the constraints as they are encountered, and
    % store the allocated integer in the relevant hlds_goal_expr.
    %
:- type constraint_id
    --->    constraint_id(
                % Assumed or unproven.
                constraint_type,

                % The id of the atomic goal which is constrained.
                goal_id,

                % The position of the constraint.
                int
            ).

:- type constraint_type
    --->    unproven
    ;       assumed.

    % The identifier of a constraint is stored along with the constraint.
    % Each value of this type may have more than one identifier because
    % if two constraints in a context are equivalent, then we merge them
    % together in order to not have to prove the same constraint twice.
    %
:- type hlds_constraint
    --->    hlds_constraint(
                list(constraint_id),
                class_name,
                list(mer_type)
            ).

:- type hlds_constraints
    --->    hlds_constraints(
                % Unproven constraints. These are the constraints that we must
                % prove (that is, universal constraints from the goal being
                % checked, or existential constraints on the head).
                hcs_unproven    :: list(hlds_constraint),

                % Assumed constraints. These are constraints we can use in
                % proofs (that is, existential constraints from the goal being
                % checked, or universal constraints on the head).
                hcs_assumed     :: list(hlds_constraint),

                % Constraints that are known to be redundant. This includes
                % constraints that have already been proved as well as
                % constraints that are ancestors of other unproven, assumed
                % or redundant constraints. Not all such constraints are
                % included, only those which may be used for the purposes
                % of improvement.
                hcs_redundant   :: redundant_constraints,

                % Ancestors of assumed constraints.
                hcs_ancestors   :: ancestor_constraints
            ).

    % Redundant constraints are partitioned by class, which helps us
    % process them more efficiently.
    %
:- type redundant_constraints == map(class_id, set(hlds_constraint)).

    % Constraints which are ancestors of assumed constraints, along with the
    % list of constraints (following the class hierarchy) which leads to
    % the assumed constraint. The assumed constraint is the last item in the
    % list.
    %
    % Note that if there are two possible lists for the same constraint, we
    % always keep the shorter one.
    %
:- type ancestor_constraints == map(prog_constraint, list(prog_constraint)).

    % During type checking we fill in a constraint_map which gives
    % the constraint that corresponds to each identifier. This is used
    % by the polymorphism translation to retrieve details of constraints.
    %
:- type constraint_map == map(constraint_id, prog_constraint).

    % `Proof' of why a constraint is redundant.
:- type constraint_proof
    --->    apply_instance(instance_id)
            % Apply the instance decl with the given identifier. Note that
            % we don't store the actual hlds_instance_defn for two reasons:
            %
            % - That would require storing a renamed version of the
            %   constraint_proofs for *every* use of an instance declaration.
            %   This wouldn't even get GCed for a long time because it
            %   would be stored in the pred_info.
            %
            % - The superclass proofs stored in the hlds_instance_defn would
            %   need to store all the constraint_proofs for all its ancestors.
            %   This would require the class relation to be topologically
            %   sorted before checking the instance declarations.

    ;       superclass(prog_constraint).
            % The constraint is redundant because of the following class's
            % superclass declaration.

    % The constraint_proof_map is a map which for each type class constraint
    % records how/why that constraint was satisfied. This information is used
    % to determine how to construct the typeclass_info for that constraint.
    %
:- type constraint_proof_map == map(prog_constraint, constraint_proof).

:- pred empty_hlds_constraints(hlds_constraints::out) is det.

:- pred init_hlds_constraint_list(list(prog_constraint)::in,
    list(hlds_constraint)::out) is det.

:- pred make_head_hlds_constraints(class_table::in, tvarset::in,
    prog_constraints::in, hlds_constraints::out) is det.

:- pred make_body_hlds_constraints(class_table::in, tvarset::in, goal_id::in,
    prog_constraints::in, hlds_constraints::out) is det.

    % make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
    %   AssumedConstraints, Constraints):
    %
    % ClassTable is the class_table for the module. TVarSet is the tvarset
    % for the predicate this class context is for. UnprovenConstraints is
    % a list of constraints which will need to be proven (that is, universal
    % constraints in the body or existential constraints in the head).
    % AssumedConstraints is a list of constraints that may be used in proofs
    % (that is, existential constraints in the body or universal constraints
    % in the head).
    %
:- pred make_hlds_constraints(class_table::in, tvarset::in,
    list(hlds_constraint)::in, list(hlds_constraint)::in,
    hlds_constraints::out) is det.

:- pred make_hlds_constraint_list(list(prog_constraint)::in,
    constraint_type::in, goal_id::in, list(hlds_constraint)::out) is det.

:- pred merge_hlds_constraints(hlds_constraints::in, hlds_constraints::in,
    hlds_constraints::out) is det.

:- pred retrieve_prog_constraints(hlds_constraints::in, prog_constraints::out)
    is det.

:- pred retrieve_prog_constraint_list(list(hlds_constraint)::in,
    list(prog_constraint)::out) is det.

:- pred retrieve_prog_constraint(hlds_constraint::in, prog_constraint::out)
    is det.

:- pred matching_constraints(hlds_constraint::in, hlds_constraint::in)
    is semidet.

:- pred compare_hlds_constraints(hlds_constraint::in, hlds_constraint::in,
    comparison_result::out) is det.

:- pred update_constraint_map(hlds_constraint::in, constraint_map::in,
    constraint_map::out) is det.

:- pred update_redundant_constraints(class_table::in, tvarset::in,
    list(hlds_constraint)::in,
    redundant_constraints::in, redundant_constraints::out) is det.

:- pred lookup_hlds_constraint_list(constraint_map::in, constraint_type::in,
    goal_id::in, int::in, list(prog_constraint)::out) is det.

:- pred search_hlds_constraint_list(constraint_map::in, constraint_type::in,
    goal_id::in, int::in, list(prog_constraint)::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

empty_hlds_constraints(Constraints) :-
    Constraints = hlds_constraints([], [], map.init, map.init).

init_hlds_constraint_list(ProgConstraints, Constraints) :-
    list.map(init_hlds_constraint, ProgConstraints, Constraints).

:- pred init_hlds_constraint(prog_constraint::in, hlds_constraint::out) is det.

init_hlds_constraint(Constraint, HLDSConstraint) :-
    Constraint = constraint(ClassName, ArgTypes),
    HLDSConstraint = hlds_constraint([], ClassName, ArgTypes).

make_head_hlds_constraints(ClassTable, TVarSet, ProgConstraints,
        Constraints) :-
    ProgConstraints = constraints(UnivConstraints, ExistConstraints),
    GoalId = goal_id_for_head_constraints,
    make_hlds_constraint_list(UnivConstraints, assumed, GoalId,
        AssumedConstraints),
    make_hlds_constraint_list(ExistConstraints, unproven, GoalId,
        UnprovenConstraints),
    make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
        AssumedConstraints, Constraints).

make_body_hlds_constraints(ClassTable, TVarSet, GoalId, ProgConstraints,
        Constraints) :-
    ProgConstraints = constraints(UnivConstraints, ExistConstraints),
    make_hlds_constraint_list(UnivConstraints, unproven, GoalId,
        UnprovenConstraints),
    make_hlds_constraint_list(ExistConstraints, assumed, GoalId,
        AssumedConstraints),
    make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
        AssumedConstraints, Constraints).

make_hlds_constraints(ClassTable, TVarSet, Unproven, Assumed, Constraints) :-
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Unproven, map.init, Redundant0),
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Assumed, Redundant0, Redundant),
    list.foldl(update_ancestor_constraints(ClassTable, TVarSet),
        Assumed, map.init, Ancestors),
    Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

make_hlds_constraint_list(ProgConstraints, ConstraintType, GoalId,
        Constraints) :-
    make_hlds_constraint_list_2(ProgConstraints, ConstraintType, GoalId,
        1, Constraints).

:- pred make_hlds_constraint_list_2(list(prog_constraint)::in,
    constraint_type::in, goal_id::in, int::in, list(hlds_constraint)::out)
    is det.

make_hlds_constraint_list_2([], _, _, _, []).
make_hlds_constraint_list_2([ProgConstraint | ProgConstraints], ConstraintType,
        GoalId, CurArgNum, [HLDSConstraint | HLDSConstraints]) :-
    ProgConstraint = constraint(ClassName, ArgTypes),
    Id = constraint_id(ConstraintType, GoalId, CurArgNum),
    HLDSConstraint = hlds_constraint([Id], ClassName, ArgTypes),
    make_hlds_constraint_list_2(ProgConstraints, ConstraintType,
        GoalId, CurArgNum + 1, HLDSConstraints).

merge_hlds_constraints(ConstraintsA, ConstraintsB, Constraints) :-
    ConstraintsA = hlds_constraints(UnprovenA, AssumedA,
        RedundantA, AncestorsA),
    ConstraintsB = hlds_constraints(UnprovenB, AssumedB,
        RedundantB, AncestorsB),
    Unproven = UnprovenA ++ UnprovenB,
    Assumed = AssumedA ++ AssumedB,
    map.union(set.union, RedundantA, RedundantB, Redundant),
    map.union(pick_shorter_list, AncestorsA, AncestorsB, Ancestors),
    Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred pick_shorter_list(list(T)::in, list(T)::in, list(T)::out) is det.

pick_shorter_list(As, Bs, Cs) :-
    ( if is_shorter(As, Bs) then
        Cs = As
    else
        Cs = Bs
    ).

:- pred is_shorter(list(T)::in, list(T)::in) is semidet.

is_shorter([], _).
is_shorter([_ | As], [_ | Bs]) :-
    is_shorter(As, Bs).

retrieve_prog_constraints(Constraints, ProgConstraints) :-
    Constraints = hlds_constraints(Unproven, Assumed, _, _),
    retrieve_prog_constraint_list(Unproven, UnivProgConstraints),
    retrieve_prog_constraint_list(Assumed, ExistProgConstraints),
    ProgConstraints = constraints(UnivProgConstraints, ExistProgConstraints).

retrieve_prog_constraint_list(Constraints, ProgConstraints) :-
    list.map(retrieve_prog_constraint, Constraints, ProgConstraints).

retrieve_prog_constraint(Constraint, ProgConstraint) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    ProgConstraint = constraint(ClassName, ArgTypes).

matching_constraints(ConstraintA, ConstraintB) :-
    ConstraintA = hlds_constraint(_IdsA, ClassName, ArgTypes),
    ConstraintB = hlds_constraint(_IdsB, ClassName, ArgTypes).

compare_hlds_constraints(ConstraintA, ConstraintB, CmpRes) :-
    ConstraintA = hlds_constraint(_IdsA, ClassNameA, ArgTypesA),
    ConstraintB = hlds_constraint(_IdsB, ClassNameB, ArgTypesB),
    compare(NameCmpRes, ClassNameA, ClassNameB),
    (
        NameCmpRes = (=),
        compare(CmpRes, ArgTypesA, ArgTypesB)
    ;
        ( NameCmpRes = (<)
        ; NameCmpRes = (>)
        ),
        CmpRes = NameCmpRes
    ).

update_constraint_map(HLDSConstraint, !ConstraintMap) :-
    HLDSConstraint = hlds_constraint(Ids, ClassName, ArgTypes),
    ProgConstraint = constraint(ClassName, ArgTypes),
    list.foldl(update_constraint_map_2(ProgConstraint), Ids, !ConstraintMap).

:- pred update_constraint_map_2(prog_constraint::in, constraint_id::in,
    constraint_map::in, constraint_map::out) is det.

update_constraint_map_2(ProgConstraint, ConstraintId, !ConstraintMap) :-
    map.set(ConstraintId, ProgConstraint, !ConstraintMap).

update_redundant_constraints(ClassTable, TVarSet, Constraints, !Redundant) :-
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Constraints, !Redundant).

:- pred update_redundant_constraints_2(class_table::in, tvarset::in,
    hlds_constraint::in, redundant_constraints::in,
    redundant_constraints::out) is det.

update_redundant_constraints_2(ClassTable, TVarSet, Constraint, !Redundant) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    map.lookup(ClassTable, ClassId, ClassDefn),
    ClassAncestors0 = ClassDefn ^ classdefn_fundep_ancestors,
    list.map(init_hlds_constraint, ClassAncestors0, ClassAncestors),
    (
        % Optimize the simple case.
        ClassAncestors = []
    ;
        ClassAncestors = [_ | _],
        ClassTVarSet = ClassDefn ^ classdefn_tvarset,
        ClassParams = ClassDefn ^ classdefn_vars,

        % We can ignore the resulting tvarset, since any new variables
        % will become bound when the arguments are bound. (This follows
        % from the fact that constraints on class declarations can only use
        % variables that appear in the head of the declaration.)

        tvarset_merge_renaming(TVarSet, ClassTVarSet, _, Renaming),
        apply_variable_renaming_to_constraint_list(Renaming,
            ClassAncestors, RenamedAncestors),
        apply_variable_renaming_to_tvar_list(Renaming, ClassParams,
            RenamedParams),
        map.from_corresponding_lists(RenamedParams, ArgTypes, Subst),
        apply_subst_to_constraint_list(Subst, RenamedAncestors, Ancestors),
        list.foldl(add_redundant_constraint, Ancestors, !Redundant)
    ).

:- pred add_redundant_constraint(hlds_constraint::in,
    redundant_constraints::in, redundant_constraints::out) is det.

add_redundant_constraint(Constraint, !Redundant) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    ( if map.search(!.Redundant, ClassId, Constraints0) then
        set.insert(Constraint, Constraints0, Constraints),
        map.det_update(ClassId, Constraints, !Redundant)
    else
        Constraints = set.make_singleton_set(Constraint),
        map.det_insert(ClassId, Constraints, !Redundant)
    ).

lookup_hlds_constraint_list(ConstraintMap, ConstraintType, GoalId, Count,
        Constraints) :-
    ( if
        search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId,
            Count, [], ConstraintsPrime)
    then
        Constraints = ConstraintsPrime
    else
        unexpected($module, $pred, "not found")
    ).

search_hlds_constraint_list(ConstraintMap, ConstraintType, GoalId, Count,
        Constraints) :-
    search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId,
        Count, [], Constraints).

:- pred search_hlds_constraint_list_2(constraint_map::in, constraint_type::in,
    goal_id::in, int::in, list(prog_constraint)::in,
    list(prog_constraint)::out) is semidet.

search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId, Count,
        !Constraints) :-
    ( if Count = 0 then
        true
    else
        ConstraintId = constraint_id(ConstraintType, GoalId, Count),
        map.search(ConstraintMap, ConstraintId, Constraint),
        !:Constraints = [Constraint | !.Constraints],
        search_hlds_constraint_list_2(ConstraintMap, ConstraintType,
            GoalId, Count - 1, !Constraints)
    ).

%---------------------------------------------------------------------------%

    % Search the superclasses of the given constraint for a potential proof,
    % and add it to the map if no better proof exists already.
    %
:- pred update_ancestor_constraints(class_table::in, tvarset::in,
    hlds_constraint::in, ancestor_constraints::in, ancestor_constraints::out)
    is det.

update_ancestor_constraints(ClassTable, TVarSet, HLDSConstraint, !Ancestors) :-
    retrieve_prog_constraint(HLDSConstraint, Constraint),
    update_ancestor_constraints_2(ClassTable, TVarSet, [], Constraint,
        !Ancestors).

:- pred update_ancestor_constraints_2(class_table::in, tvarset::in,
    list(prog_constraint)::in, prog_constraint::in,
    ancestor_constraints::in, ancestor_constraints::out) is det.

update_ancestor_constraints_2(ClassTable, TVarSet, Descendants0, Constraint,
        !Ancestors) :-
    Constraint = constraint(ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    map.lookup(ClassTable, ClassId, ClassDefn),

    % We can ignore the resulting tvarset, since any new variables
    % will become bound when the arguments are bound. (This follows
    % from the fact that constraints on class declarations can only use
    % variables that appear in the head of the declaration.)

    tvarset_merge_renaming(TVarSet, ClassDefn ^ classdefn_tvarset, _,
        Renaming),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        ClassDefn ^ classdefn_supers, RenamedSupers),
    apply_variable_renaming_to_tvar_list(Renaming, ClassDefn ^ classdefn_vars,
        RenamedParams),
    map.from_corresponding_lists(RenamedParams, ArgTypes, Subst),
    apply_subst_to_prog_constraint_list(Subst, RenamedSupers, Supers),

    Descendants = [Constraint | Descendants0],
    list.foldl(update_ancestor_constraints_3(ClassTable, TVarSet, Descendants),
        Supers, !Ancestors).

:- pred update_ancestor_constraints_3(class_table::in, tvarset::in,
    list(prog_constraint)::in, prog_constraint::in,
    ancestor_constraints::in, ancestor_constraints::out) is det.

update_ancestor_constraints_3(ClassTable, TVarSet, Descendants, Constraint,
        !Ancestors) :-
    ( if
        map.search(!.Ancestors, Constraint, OldDescendants),
        is_shorter(OldDescendants, Descendants)
    then
        % We don't want to update the ancestors because we already have a
        % better path. The same will apply for all superclasses, so we
        % don't traverse any further.
        true
    else
        map.set(Constraint, Descendants, !Ancestors),
        update_ancestor_constraints_2(ClassTable, TVarSet, Descendants,
            Constraint, !Ancestors)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % A table that records all the assertions in the system.
    % An assertion is a goal that will always evaluate to true,
    % subject to the constraints imposed by the quantifiers.
    %
    % ie :- promise all [A] some [B] (B > A)
    %
    % The above assertion states that for all possible values of A,
    % there will exist at least one value, B, such that B is greater than A.
    %
:- type assert_id.
:- type assertion_table.

:- pred assertion_table_init(assertion_table::out) is det.

:- pred assertion_table_add_assertion(pred_id::in, assert_id::out,
    assertion_table::in, assertion_table::out) is det.

:- pred assertion_table_lookup(assertion_table::in, assert_id::in,
    pred_id::out) is det.

:- pred assertion_table_pred_ids(assertion_table::in,
    list(pred_id)::out) is det.

:- implementation.

:- type assert_id == int.
:- type assertion_table
    --->    assertion_table(assert_id, map(assert_id, pred_id)).

assertion_table_init(assertion_table(0, AssertionMap)) :-
    map.init(AssertionMap).

assertion_table_add_assertion(Assertion, Id, !AssertionTable) :-
    !.AssertionTable = assertion_table(Id, AssertionMap0),
    map.det_insert(Id, Assertion, AssertionMap0, AssertionMap),
    !:AssertionTable = assertion_table(Id + 1, AssertionMap).

assertion_table_lookup(AssertionTable, Id, Assertion) :-
    AssertionTable = assertion_table(_MaxId, AssertionMap),
    map.lookup(AssertionMap, Id, Assertion).

assertion_table_pred_ids(assertion_table(_, AssertionMap), PredIds) :-
    map.values(AssertionMap, PredIds).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % A table recording exclusivity declarations (i.e. promise_exclusive
    % and promise_exclusive_exhaustive).
    %
    % e.g. :- all [X]
    %       promise_exclusive
    %       some [Y] (
    %           p(X, Y)
    %       ;
    %           q(X)
    %       ).
    %
    % promises that only one of p(X, Y) and q(X) can succeed at a time,
    % although whichever one succeeds may have multiple solutions. See
    % notes/promise_ex.html for details of the declarations.

    % An exclusive_id is the pred_id of an exclusivity declaration,
    % and is useful in distinguishing between the arguments of the
    % operations below on the exclusive_table.
    %
:- type exclusive_id    ==  pred_id.
:- type exclusive_ids   ==  list(pred_id).

:- type exclusive_table.

    % Initialise the exclusive_table.
    %
:- pred exclusive_table_init(exclusive_table::out) is det.

    % Search the exclusive table and return the list of exclusivity
    % declarations that use the predicate given by pred_id.
    %
:- pred exclusive_table_search(exclusive_table::in, pred_id::in,
    exclusive_ids::out) is semidet.

    % As for search, but aborts if no exclusivity declarations are found.
    %
:- pred exclusive_table_lookup(exclusive_table::in, pred_id::in,
    exclusive_ids::out) is det.

    % Optimises the exclusive_table.
    %
:- pred exclusive_table_optimize(exclusive_table::in, exclusive_table::out)
    is det.

    % Add to the exclusive table that pred_id is used in the exclusivity
    % declaration exclusive_id.
    %
:- pred exclusive_table_add(pred_id::in, exclusive_id::in,
    exclusive_table::in, exclusive_table::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type exclusive_table == multi_map(pred_id, exclusive_id).

exclusive_table_init(ExclusiveTable) :-
    multi_map.init(ExclusiveTable).

exclusive_table_lookup(ExclusiveTable, PredId, ExclusiveIds) :-
    multi_map.lookup(ExclusiveTable, PredId, ExclusiveIds).

exclusive_table_search(ExclusiveTable, Id, ExclusiveIds) :-
    multi_map.search(ExclusiveTable, Id, ExclusiveIds).

exclusive_table_optimize(!ExclusiveTable) :-
    multi_map.optimize(!ExclusiveTable).

exclusive_table_add(ExclusiveId, PredId, !ExclusiveTable) :-
    multi_map.set(PredId, ExclusiveId, !ExclusiveTable).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_data.
%---------------------------------------------------------------------------%
