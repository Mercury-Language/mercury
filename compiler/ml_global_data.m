%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_global_data.m.
% Main author: zs.
%
% This module is part of the MLDS code generator. It handles the generation
% of data structures that are "born global", i.e. they belong to the generated
% module as a whole, not to any particular function in it.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_global_data.
:- interface.

:- import_module backend_libs.rtti.
:- import_module ml_backend.mlds.

:- import_module list.
:- import_module map.

    % This abstract type represents the MLDS code generator's repository of
    % data structures that are "born global", i.e. the ones for which we
    % known from the start that they will be defined at file scope.
    %
:- type ml_global_data.

    % Initialize the ml_global_data structure to a value that represents
    % no global data structures known yet.
    %
:- func ml_global_data_init = ml_global_data.

    % ml_global_data_get_global_defns(GlobalData, RevFlatCellDefns,
    %   RevFlatRttiDefns, RevMaybeNonFlatDefns):
    %
    % Get all the global definitions implicit in the argument. Each group
    % of global definitions with shared characteristics are returned in a
    % separate argument.
    %
:- pred ml_global_data_get_global_defns(ml_global_data::in,
    list(mlds_defn)::out, list(mlds_defn)::out, list(mlds_defn)::out) is det.

    % ml_global_data_get_all_global_defns(GlobalData, Defns):
    %
    % Get all the global definitions implicit in the argument, in an order
    % which is likely to be reasonably good. Note that this order may still
    % require forward declarations.
    %
:- pred ml_global_data_get_all_global_defns(ml_global_data::in,
    list(mlds_defn)::out) is det.

    % This type maps the names of rtti data structures that have already been
    % generated to the rval that refers to that data structure, and its type.
    %
    % The code generator looks up this map whenever it is needs a reference to
    % an RTTI data structure that may or may not have been generated before.
    % If it finds the id of that data structure in this map, it uses the
    % corresponding value *without* generating the duplicate definition.
    % Of course, this requires the code generator to add the original
    % definition of the data structure to the ml_globals_data whenever it adds
    % new entries to this map.
    %
    % At the moment, the only data structures that are potentially duplicated
    % are the representations of type_infos and pseudo_type_infos, so only
    % these rtti_ids will be in this map.
    %
:- type ml_rtti_rval_type_map == map(rtti_id, ml_rval_and_type).
:- type ml_rval_and_type
    --->    ml_rval_and_type(
                mlds_rval,
                mlds_type
            ).

    % Return the component of the given ml_global_data that contains
    % information about potentially duplicated RTTI global definitions.
    %
:- pred ml_global_data_get_pdup_rval_type_map(ml_global_data::in,
    ml_rtti_rval_type_map::out) is det.

:- pred ml_global_data_get_unique_const_num(int::out,
    ml_global_data::in, ml_global_data::out) is det.

    % Set the list of unique maybe-nonflat definitions to the given list.
    % Intended for use by code that transforms the previously current list
    % of unique maybe-nonflat definitions.
    %
:- pred ml_global_data_set_rev_maybe_nonflat_defns(list(mlds_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Map the given rtti_id to the given ml_rval_and_type, and record the
    % generation of the given global data definitions.
    %
:- pred ml_global_data_add_pdup_rtti_id(rtti_id::in, ml_rval_and_type::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Add to the global data structure one or more definitions that are
    % "unique by construction". Some of these are guaranteed to be flat
    % definitions (definitions for which ml_elim_nested is an identity
    % operation), while some have no such guarantee.
    %
:- pred ml_global_data_add_flat_cell_defn(mlds_defn::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_add_flat_rtti_defn(mlds_defn::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_add_flat_rtti_defns(list(mlds_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_add_maybe_nonflat_defns(list(mlds_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module svmap.

:- type ml_global_data
    --->    ml_global_data(
                mdg_pdup_rval_type_map          :: ml_rtti_rval_type_map,
                mdg_const_counter               :: counter,
                mdg_rev_flat_cell_defns         :: list(mlds_defn),
                mdg_rev_flat_rtti_defns         :: list(mlds_defn),
                mdg_rev_maybe_nonflat_defns     :: list(mlds_defn)
            ).

%-----------------------------------------------------------------------------%

ml_global_data_init = GlobalData :-
    GlobalData = ml_global_data(map.init, counter.init(1), [], [], []).

ml_global_data_get_global_defns(GlobalData, RevFlatCellDefns, RevFlatRttiDefns,
        RevMaybeNonFlatDefns) :-
    GlobalData = ml_global_data(_PDupRvalTypeMap, _ConstCounter,
        RevFlatCellDefns, RevFlatRttiDefns, RevMaybeNonFlatDefns).

ml_global_data_get_all_global_defns(GlobalData, Defns) :-
    GlobalData = ml_global_data(_PDupRvalTypeMap, _ConstCounter,
        RevFlatCellDefns, RevFlatRttiDefns, RevMaybeNonFlatDefns),
    % RevFlatRttiDefns are type_ctor_infos and the like, while
    % RevNonFlatDefns are type_infos and pseudo_type_infos.
    % They refer to each other, so neither order is obviously better.
    %
    % RevFlatCellDefns can refer to either of the previous two groups,
    % which cannot refer back, so RevFlatCellDefns should definitely be listed
    % last.
    Defns = list.reverse(RevFlatRttiDefns) ++
        list.reverse(RevMaybeNonFlatDefns) ++
        list.reverse(RevFlatCellDefns).

%-----------------------------------------------------------------------------%
%
% Access predicates for the ml_global_data type.
%

:- pred ml_global_data_get_const_counter(ml_global_data::in,
    counter::out) is det.
:- pred ml_global_data_get_rev_flat_cell_defns(ml_global_data::in,
    list(mlds_defn)::out) is det.
:- pred ml_global_data_get_rev_flat_rtti_defns(ml_global_data::in,
    list(mlds_defn)::out) is det.
:- pred ml_global_data_get_rev_maybe_nonflat_defns(ml_global_data::in,
    list(mlds_defn)::out) is det.

:- pred ml_global_data_set_pdup_rval_type_map(ml_rtti_rval_type_map::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_const_counter(counter::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_rev_flat_cell_defns(list(mlds_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_rev_flat_rtti_defns(list(mlds_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

ml_global_data_get_pdup_rval_type_map(GlobalData,
    GlobalData ^ mdg_pdup_rval_type_map).
ml_global_data_get_const_counter(GlobalData,
    GlobalData ^ mdg_const_counter).
ml_global_data_get_rev_flat_cell_defns(GlobalData,
    GlobalData ^ mdg_rev_flat_cell_defns).
ml_global_data_get_rev_flat_rtti_defns(GlobalData,
    GlobalData ^ mdg_rev_flat_rtti_defns).
ml_global_data_get_rev_maybe_nonflat_defns(GlobalData,
    GlobalData ^ mdg_rev_maybe_nonflat_defns).

ml_global_data_set_pdup_rval_type_map(PDupRvalTypeMap, !GlobalData) :-
    !GlobalData ^ mdg_pdup_rval_type_map := PDupRvalTypeMap.
ml_global_data_set_const_counter(ConstCounter, !GlobalData) :-
    !GlobalData ^ mdg_const_counter := ConstCounter.
ml_global_data_set_rev_flat_cell_defns(Defns, !GlobalData) :-
    !GlobalData ^ mdg_rev_flat_cell_defns := Defns.
ml_global_data_set_rev_flat_rtti_defns(Defns, !GlobalData) :-
    !GlobalData ^ mdg_rev_flat_rtti_defns := Defns.
ml_global_data_set_rev_maybe_nonflat_defns(Defns, !GlobalData) :-
    !GlobalData ^ mdg_rev_maybe_nonflat_defns := Defns.

%-----------------------------------------------------------------------------%

ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData) :-
    ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap0),
    svmap.det_insert(RttiId, RvalType, PDupRvalTypeMap0, PDupRvalTypeMap),
    ml_global_data_set_pdup_rval_type_map(PDupRvalTypeMap, !GlobalData).

ml_global_data_get_unique_const_num(ConstNum, !GlobalData) :-
    ml_global_data_get_const_counter(!.GlobalData, ConstCounter0),
    counter.allocate(ConstNum, ConstCounter0, ConstCounter),
    ml_global_data_set_const_counter(ConstCounter, !GlobalData).

ml_global_data_add_flat_cell_defn(Defn, !GlobalData) :-
    ml_global_data_get_rev_flat_cell_defns(!.GlobalData, RevDefns0),
    RevDefns = [Defn | RevDefns0],
    ml_global_data_set_rev_flat_cell_defns(RevDefns, !GlobalData).

ml_global_data_add_flat_rtti_defn(Defn, !GlobalData) :-
    ml_global_data_get_rev_flat_rtti_defns(!.GlobalData, RevDefns0),
    RevDefns = [Defn | RevDefns0],
    ml_global_data_set_rev_flat_rtti_defns(RevDefns, !GlobalData).

ml_global_data_add_flat_rtti_defns(Defns, !GlobalData) :-
    ml_global_data_get_rev_flat_rtti_defns(!.GlobalData, RevDefns0),
    RevDefns = list.reverse(Defns) ++ RevDefns0,
    ml_global_data_set_rev_flat_rtti_defns(RevDefns, !GlobalData).

ml_global_data_add_maybe_nonflat_defns(Defns, !GlobalData) :-
    ml_global_data_get_rev_maybe_nonflat_defns(!.GlobalData, RevDefns0),
    RevDefns = list.reverse(Defns) ++ RevDefns0,
    ml_global_data_set_rev_maybe_nonflat_defns(RevDefns, !GlobalData).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_global_data.
%-----------------------------------------------------------------------------%
