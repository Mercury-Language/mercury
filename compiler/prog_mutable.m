%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_mutable.m.
% Main authors: rafe, juliensf.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.

%-----------------------------------------------------------------------------%
%
% The names we construct for the auxiliary predicates of a mutable.
%

:- func mutable_lock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unlock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_get_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_set_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_get_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_set_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_secret_set_pred_sym_name(module_name, string) = sym_name.
:- func mutable_init_pred_sym_name(module_name, string) = sym_name.
:- func mutable_pre_init_pred_sym_name(module_name, string) = sym_name.

%-----------------------------------------------------------------------------%

    % This predicate decides which of the publicly visible auxiliary predicates
    % we should generate for a mutable.
    %
    % This same decisions for the private aux predicates are made by
    % compute_needed_private_mutable_aux_preds in add_mutable_aux_preds.m.
    %
:- pred compute_needed_public_mutable_aux_preds(mutable_var_attributes::in,
    list(mutable_pred_kind)::out) is det.

    % make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst, Context,
    %   Kind, PredDecl):
    %
    % Create the predicate declaration for the given Kind of mutable auxiliry
    % predicate for a mutable with the given MutableName, which has the
    % given Type, Inst and Context.
    %
:- pred make_mutable_aux_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, prog_context::in, mutable_pred_kind::in,
    item_pred_decl_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.

:- import_module maybe.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

mutable_lock_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "lock_" ++ Name).

mutable_unlock_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "unlock_" ++ Name).

mutable_unsafe_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "unsafe_get_" ++ Name).

mutable_unsafe_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "unsafe_set_" ++ Name).

mutable_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "get_" ++ Name).

mutable_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "set_" ++ Name).

mutable_secret_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "secret_initialization_only_set_" ++ Name).

mutable_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "initialise_mutable_" ++ Name).

mutable_pre_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "pre_initialise_mutable_" ++ Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

compute_needed_public_mutable_aux_preds(MutAttrs, PublicAuxPreds) :-
    % The logic we use here is duplicated in define_main_get_set_preds
    % in add_mutable_aux_preds.m. The comment there explains why.
    IsConstant = mutable_var_constant(MutAttrs),
    AttachToIO = mutable_var_attach_to_io_state(MutAttrs),
    (
        IsConstant = mutable_constant,
        % We create the "get" access predicate, which is pure since
        % it always returns the same value, but we must also create
        % a secret "set" predicate for use by the initialization code.
        GetSetPreds =
            [mutable_pred_constant_get, mutable_pred_constant_secret_set]
    ;
        IsConstant = mutable_not_constant,
        % Create the standard, non-pure access predicates. These are
        % always created for non-constant mutables, even if the
        % `attach_to_io_state' attribute has been specified.
        StdGetSetPreds = [mutable_pred_std_get, mutable_pred_std_set],

        % If requested, create pure access predicates using
        % the I/O state as well.
        (
            AttachToIO = mutable_dont_attach_to_io_state,
            GetSetPreds = StdGetSetPreds
        ;
            AttachToIO = mutable_attach_to_io_state,
            IOGetSetPreds = [mutable_pred_io_get, mutable_pred_io_set],
            GetSetPreds = StdGetSetPreds ++ IOGetSetPreds
        )
    ),
    PublicAuxPreds = GetSetPreds.

%-----------------------------------------------------------------------------%

make_mutable_aux_pred_decl(ModuleName, Name, Type, Inst, Context, Kind,
        PredDecl) :-
    (
        Kind = mutable_pred_pre_init,
        PredSymName = mutable_pre_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_init,
        PredSymName = mutable_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_lock,
        PredSymName = mutable_lock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_unlock,
        PredSymName = mutable_unlock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_unsafe_get,
        PredSymName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        Purity = purity_semipure
    ;
        Kind = mutable_pred_unsafe_set,
        PredSymName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        Purity = purity_impure
    ;
        Kind = mutable_pred_std_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        Purity = purity_semipure
    ;
        Kind = mutable_pred_std_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        Purity = purity_impure
    ;
        Kind = mutable_pred_constant_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        Purity = purity_pure
    ;
        Kind = mutable_pred_constant_secret_set,
        PredSymName = mutable_secret_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        Purity = purity_impure
    ;
        Kind = mutable_pred_io_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))]
            ++ io_state_pair,
        Purity = purity_pure
    ;
        Kind = mutable_pred_io_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))]
            ++ io_state_pair,
        Purity = purity_pure
    ),
    WithType = maybe.no,
    WithMode = maybe.no,
    Origin = compiler_origin_mutable(ModuleName, Name, Kind),
    CompilerAttrs = item_compiler_attributes(Origin),
    MaybeAttrs = item_origin_compiler(CompilerAttrs),
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),
    SeqNum = -1,
    PredDecl = item_pred_decl_info(PredSymName, pf_predicate, ArgTypesAndModes,
        WithType, WithMode, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum).

:- func io_state_pair = list(type_and_mode).

io_state_pair =
    [type_and_mode(io_state_type, di_mode),
    type_and_mode(io_state_type, uo_mode)].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_mutable.
%-----------------------------------------------------------------------------%
