%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks that declarations for abstract types also have a
% corresponding definition somewhere in the module.
%
%---------------------------------------------------------------------------%

:- module check_hlds.check_for_missing_type_defns.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%---------------------------------------------------------------------------%

    % Check that every abstract type in the module has at least one definition
    % in either the interface or implementation of the module.
    %
    % Note that a type may have several definitions, e.g. some foreign
    % definitions and a default Mercury definition.
    %
:- pred check_for_missing_type_defns(module_info::in, list(error_spec)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.

check_for_missing_type_defns(ModuleInfo, Specs) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(check_for_missing_type_defns_in_type, TypeTable,
        [], Specs).

:- pred check_for_missing_type_defns_in_type(type_ctor::in, hlds_type_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_missing_type_defns_in_type(TypeCtor, TypeDefn, !Specs) :-
    ( if
        get_type_defn_status(TypeDefn, TypeStatus),
        type_status_defined_in_this_module(TypeStatus) = yes,
        get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_abstract_type(_)
    then
        % We expect the builtin types character, float, int, uint and string to
        % have abstract declarations with no definitions. The following types
        % from the type_desc module also only have abstract declarations:
        %
        %   - type_desc/0
        %   - pseudo_type_desc/0
        %   - type_ctor_desc/0
        %
        % We do not emit an error for these types. In addition, we also don't
        % bother checking for corresponding definitions in any of the builtin
        % modules in the standard library.

        TypeCtor = type_ctor(SymName, Arity),
        BuiltinTypeCtors = builtin_type_ctors_with_no_hlds_type_defn,
        ( if
            sym_name_get_module_name(SymName, ModuleName),
            not any_mercury_builtin_module(ModuleName),

            % Several of the type defineds in type_desc do not have
            % Mercury definitions.
            not ModuleName = unqualified("type_desc"),
            not list.member(TypeCtor, BuiltinTypeCtors),

            % If we have previously reported an error for this type,
            % then the definition may have been present, though erroneous.
            get_type_defn_prev_errors(TypeDefn, type_defn_no_prev_errors)
        then
            get_type_defn_context(TypeDefn, TypeContext),
            Pieces = [words("Error: abstract declaration for type"),
                unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                words("has no corresponding definition."), nl],
            Spec = simplest_spec($pred, severity_error, phase_type_check,
                TypeContext, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.check_for_missing_type_defns.
%---------------------------------------------------------------------------%
