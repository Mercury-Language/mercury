%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007, 2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: erlang_rtti.m.
% Authors: petdr, zs.
%
% Definitions of data structures for representing run-time type information
% in the Erlang backend.
%
% Note we only define new types for from where the RTTI differs
% from what is defined in rtti.m.
%
% In the context of the MLDS backend erlang_rtti.m is the equivalent of
% rtti.m, while erl_rtti.m is the equivalent to rtti_to_mlds.m
%
% These types have to be kept in sync with the corresponding types in
% library/erlang_rtti_implementation.m
%
%-----------------------------------------------------------------------------%

:- module backend_libs.erlang_rtti.
:- interface.

:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%
%
% The data structures representing type constructors
%

    % A type_ctor_data structure contains all the information that the
    % runtime system needs to know about a type constructor.
    %
:- type erlang_type_ctor_data
    --->    erlang_type_ctor_data(
                etcr_version        :: uint8,
                etcr_module_name    :: module_name,
                etcr_type_name      :: string,
                etcr_arity          :: uint16,

                %
                % It is possible that the type doesn't have
                % a unify or compare predicate.
                % eg one cannot unify higher-order types.
                %
                etcr_unify          :: maybe(rtti_proc_label),
                etcr_compare        :: maybe(rtti_proc_label),
                etcr_rep_details    :: erlang_type_ctor_details
            ).

    % A erlang_type_ctor_details structure contains all the information that
    % the runtime system needs to know about the data representation scheme
    % used by a type constructor.
    %
    % XXX Later on we may want to handle the enum and notag du types
    % specially.
    % Enum is for types that define only constants.
    % Notag is for types that define only one unary functor.
    %
:- type erlang_type_ctor_details

    --->    erlang_du(
                % The function symbols are listed in declaration order.
                edu_functors        :: list(erlang_du_functor)
            )

    ;       erlang_dummy(
                edummy_name         :: string
            )

                % Mercury lists are represented as erlang lists
    ;       erlang_list

                %
    ;       erlang_array

    ;       erlang_eqv(
                                    % XXX why is it a pseudo type info
                eeqv_type           :: rtti_maybe_pseudo_type_info
            )

                % Builtin Mercury types
    ;       erlang_builtin(
                ebuiltin_ctor       :: builtin_ctor
            )


                % Types used just in the implementation.
    ;       erlang_impl_artifact(
                eimpl_ctor          :: erlang_impl_ctor
            )

    ;       erlang_foreign
    .

:- type erlang_du_functor
    --->    erlang_du_functor(
                edu_name            :: string,
                edu_orig_arity      :: uint16,

                    % The declaration order of the functor.
                edu_ordinal         :: uint32,

                    % The lexicographic order of the functor.
                edu_lex             :: uint32,

                    % erlang atom which represents the functor
                    % currently encoded version of name
                    % in the future maybe name_arity
                edu_rep             :: erlang_atom_raw,
                edu_arg_infos       :: list(du_arg_info),
                edu_exist_info      :: maybe(exist_info),
                edu_subtype_info    :: functor_subtype_info
            ).

:- type erlang_atom_raw
    ---> erlang_atom_raw(string).

    % The list of type constructors that are used behind the scenes by
    % the Mercury implementation.
    %
:- type erlang_impl_ctor
    --->    erlang_impl_ctor_type_info
    ;       erlang_impl_ctor_type_ctor_info
    ;       erlang_impl_ctor_typeclass_info
    ;       erlang_impl_ctor_base_typeclass_info

                % The following are introduced in
                % private_builtin and table_builtin
                % but should never be used.
                %
    ;       erlang_impl_ctor_hp
    ;       erlang_impl_ctor_subgoal
    ;       erlang_impl_ctor_ticket
    .

%-----------------------------------------------------------------------------%
%
% The data structures representing the top-level global data structures
% generated by the Mercury compiler.  These are all generated read-only.

:- type erlang_rtti_data
    --->    erlang_rtti_data_type_ctor_info(
                erlang_type_ctor_data
            )
    ;       erlang_rtti_data_type_info(
                rtti_type_info
            )
    ;       erlang_rtti_data_pseudo_type_info(
                rtti_pseudo_type_info
            )
    ;       erlang_rtti_data_base_typeclass_info(
                tc_name,        % identifies the type class
                module_name,    % module containing instance decl.
                string,         % encodes the names and arities of the
                                % types in the instance declaration
                base_typeclass_info
            )
    ;       erlang_rtti_data_type_class_decl(
                tc_decl
            )
    ;       erlang_rtti_data_type_class_instance(
                tc_instance
            ).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.erlang_rtti.
%-----------------------------------------------------------------------------%
