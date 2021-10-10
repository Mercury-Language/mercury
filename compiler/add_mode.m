%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: add_mode.m.
%
% This submodule of make_hlds handles the declarations of new insts and modes.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_mode.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred module_add_inst_defn(inst_status::in, item_inst_defn_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_check_inst_defn(module_info::in, item_inst_defn_info::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred module_add_mode_defn(mode_status::in, item_mode_defn_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_check_mode_defn(module_info::in, item_mode_defn_info::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_inst_mode.
:- import_module hlds.make_hlds_error.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module bool.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

module_add_inst_defn(InstStatus, ItemInstDefnInfo, !ModuleInfo, !Specs) :-
    ItemInstDefnInfo = item_inst_defn_info(InstName, InstParams, MaybeForType,
        MaybeAbstractInstDefn, VarSet, Context, _SeqNum),
    (
        MaybeAbstractInstDefn = abstract_inst_defn
        % We use abstract inst definitions only for module qualification;
        % we never add them to the HLDS.
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
        % Add the definition of this inst to the HLDS inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_user_insts(InstTable0, UserInstTable0),
        insts_add(VarSet, InstName, InstParams, MaybeForType, InstDefn,
            Context, InstStatus, UserInstTable0, UserInstTable, !Specs),
        inst_table_set_user_insts(UserInstTable, InstTable0, InstTable),
        module_info_set_inst_table(InstTable, !ModuleInfo)
    ).

:- pred insts_add(inst_varset::in, sym_name::in, list(inst_var)::in,
    maybe(type_ctor)::in, inst_defn::in, prog_context::in,
    inst_status::in, user_inst_table::in, user_inst_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

insts_add(VarSet, InstSymName, InstParams, MaybeForType, eqv_inst(EqvInst),
        Context, InstStatus, !UserInstTable, !Specs) :-
    list.length(InstParams, InstArity),
    InstCtor = inst_ctor(InstSymName, InstArity),
    (
        EqvInst = bound(_, _, _),
        (
            MaybeForType = no,
            IFTC = iftc_applicable_not_known,
            Here = inst_status_defined_in_this_module(InstStatus),
            (
                Here = yes,
                ShortInstSymName = unqualified(unqualify_name(InstSymName)),
                Pieces = [words("Warning: inst"),
                    qual_sym_name_arity(
                        sym_name_arity(ShortInstSymName, InstArity)),
                    words("includes references to function symbols,"),
                    words("but does not declare what type constructor"),
                    words("it is for."), nl],
                Option = warn_insts_with_functors_without_type,
                Spec = conditional_spec($pred, Option, yes,
                    severity_warning, phase_parse_tree_to_hlds,
                    [simplest_msg(Context, Pieces)]),
                !:Specs = [Spec | !.Specs]
            ;
                Here = no
            )
        ;
            MaybeForType = yes(ForType),
            IFTC = iftc_applicable_declared(ForType)
        )
    ;
        ( EqvInst = any(_, _)
        ; EqvInst = free
        ; EqvInst = free(_)
        ; EqvInst = ground(_, _)
        ; EqvInst = not_reached
        ; EqvInst = inst_var(_)
        ; EqvInst = constrained_inst_vars(_, _)
        ; EqvInst = defined_inst(_)
        ; EqvInst = abstract_inst(_, _)
        ),
        IFTC = iftc_not_applicable,
        ( if
            MaybeForType = yes(_ForType),
            inst_status_defined_in_this_module(InstStatus) = yes
        then
            ShortInstSymName = unqualified(unqualify_name(InstSymName)),
            Pieces = [words("Error: inst"),
                qual_sym_name_arity(
                    sym_name_arity(ShortInstSymName, InstArity)),
                words("is specified to be for a given type constructor,"),
                words("but it is not defined to be equivalent to a"),
                quote("bound"), words("inst."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ),
    InstDefn = hlds_inst_defn(VarSet, InstParams, eqv_inst(EqvInst), IFTC,
        Context, InstStatus),
    ( if map.insert(InstCtor, InstDefn, !UserInstTable) then
        true
    else
        % If abstract insts are implemented, this will need to change
        % to update the hlds_inst_defn to the non-abstract inst.

        InstStatus = inst_status(InstModeStatus),
        ReportDup = should_report_duplicate_inst_or_mode(InstModeStatus),
        (
            ReportDup = no
        ;
            ReportDup = yes,
            map.lookup(!.UserInstTable, InstCtor, OrigInstDefn),
            OrigContext = OrigInstDefn ^ inst_context,
            Extras = [],
            report_multiple_def_error(InstSymName, InstArity, "inst",
                Context, OrigContext, Extras, !Specs)
        )
    ).

%---------------------%

module_check_inst_defn(ModuleInfo, ItemInstDefnInfo,
        !FoundInvalidInstOrMode, !Specs) :-
    ItemInstDefnInfo = item_inst_defn_info(InstName, InstParams, _MaybeForType,
        _MaybeAbstractInstDefn, _VarSet, Context, _SeqNum),
    % Check if the inst is infinitely recursive (at the top level).
    module_info_get_inst_table(ModuleInfo, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    InstArity = list.length(InstParams),
    InstCtor = inst_ctor(InstName, InstArity),
    TestArgs = list.duplicate(InstArity, not_reached),
    check_for_cyclic_inst(UserInstTable, InstCtor, InstCtor, TestArgs, [],
        Context, FoundInvalidInst, !Specs),
    (
        FoundInvalidInst = no
    ;
        FoundInvalidInst = yes,
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ).

    % Check if the inst is infinitely recursive (at the top level).
    %
:- pred check_for_cyclic_inst(user_inst_table::in, inst_ctor::in,
    inst_ctor::in, list(mer_inst)::in, list(inst_ctor)::in, prog_context::in,
    bool::out, list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_inst(UserInstTable, OrigInstCtor, InstCtor0, Args0,
        Expansions0, Context, InvalidMode, !Specs) :-
    ( if list.member(InstCtor0, Expansions0) then
        report_circular_inst_equiv_error(OrigInstCtor, InstCtor0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    else
        ( if
            map.search(UserInstTable, InstCtor0, InstDefn),
            InstDefn = hlds_inst_defn(_, Params, Body, _, _, _),
            Body = eqv_inst(EqvInst0),
            inst_substitute_arg_list(Params, Args0, EqvInst0, EqvInst),
            EqvInst = defined_inst(user_inst(Name, Args))
        then
            Arity = list.length(Args),
            InstCtor = inst_ctor(Name, Arity),
            Expansions = [InstCtor0 | Expansions0],
            check_for_cyclic_inst(UserInstTable, OrigInstCtor, InstCtor, Args,
                Expansions, Context, InvalidMode, !Specs)
        else
            InvalidMode = no
        )
    ).

:- func should_report_duplicate_inst_or_mode(new_instmode_status) = bool.

should_report_duplicate_inst_or_mode(InstModeStatus) = ReportDup :-
    (
        InstModeStatus = instmode_defined_in_this_module(_),
        ReportDup = yes
    ;
        InstModeStatus = instmode_defined_in_other_module(InstModeImport),
        (
            ( InstModeImport = instmode_import_plain
            ; InstModeImport = instmode_import_abstract
            ),
            ReportDup = yes
        ;
            InstModeImport = instmode_import_opt,
            ReportDup = no
        )
    ).

%---------------------------------------------------------------------------%

module_add_mode_defn(ModeStatus, ItemModeDefnInfo, !ModuleInfo, !Specs) :-
    ItemModeDefnInfo = item_mode_defn_info(Name, Params, MaybeAbstractModeDefn,
        VarSet, Context, _SeqNum),
    (
        MaybeAbstractModeDefn = abstract_mode_defn
        % We use abstract mode definitions only for module qualification;
        % we never add them to the HLDS.
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
        module_info_get_mode_table(!.ModuleInfo, ModeTable0),
        modes_add(VarSet, Name, Params, ModeDefn, Context, ModeStatus,
            ModeTable0, ModeTable, !Specs),
        module_info_set_mode_table(ModeTable, !ModuleInfo)
    ).

:- pred modes_add(inst_varset::in, sym_name::in, list(inst_var)::in,
    mode_defn::in, prog_context::in, mode_status::in,
    mode_table::in, mode_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

modes_add(VarSet, Name, Params, ModeBody, Context, ModeStatus,
        !ModeTable, !Specs) :-
    list.length(Params, Arity),
    ModeCtor = mode_ctor(Name, Arity),
    ModeBody = eqv_mode(EqvMode),
    HldsModeBody = hlds_mode_body(EqvMode),
    ModeDefn = hlds_mode_defn(VarSet, Params, HldsModeBody, Context,
        ModeStatus),
    ( if mode_table_insert(ModeCtor, ModeDefn, !ModeTable) then
        true
    else
        ModeStatus = mode_status(InstModeStatus),
        ReportDup = should_report_duplicate_inst_or_mode(InstModeStatus),
        (
            ReportDup = no
        ;
            ReportDup = yes,
            mode_table_get_mode_defns(!.ModeTable, ModeDefns),
            map.lookup(ModeDefns, ModeCtor, OrigModeDefn),
            OrigModeDefn = hlds_mode_defn(_, _, _, OrigContext, _),
            Extras = [],
            report_multiple_def_error(Name, Arity, "mode",
                Context, OrigContext, Extras, !Specs)
        )
    ).

%---------------------%

module_check_mode_defn(ModuleInfo, ItemModeDefnInfo,
        !FoundInvalidInstOrMode, !Specs) :-
    module_info_get_mode_table(ModuleInfo, ModeTable),
    ItemModeDefnInfo = item_mode_defn_info(Name, Params,
        _MaybeAbstractModeDefn, _VarSet, Context, _SeqNum),
    list.length(Params, Arity),
    ModeCtor = mode_ctor(Name, Arity),
    Expansions0 = [],
    check_for_cyclic_mode(ModeTable, ModeCtor, ModeCtor, Expansions0,
        Context, FoundInvalidMode, !Specs),
    (
        FoundInvalidMode = no
    ;
        FoundInvalidMode = yes,
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ).

    % Check if the mode is infinitely recursive at the top level.
    %
:- pred check_for_cyclic_mode(mode_table::in, mode_ctor::in, mode_ctor::in,
    list(mode_ctor)::in, prog_context::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_mode(ModeTable, OrigModeCtor, ModeCtor0, Expansions0, Context,
        InvalidMode, !Specs) :-
    ( if list.member(ModeCtor0, Expansions0) then
        report_circular_mode_equiv_error(OrigModeCtor, ModeCtor0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    else
        mode_table_get_mode_defns(ModeTable, ModeDefns),
        ( if
            map.search(ModeDefns, ModeCtor0, ModeDefn),
            ModeDefn = hlds_mode_defn(_, _, Body, _, _),
            Body = hlds_mode_body(EqvMode),
            EqvMode = user_defined_mode(Name, Args)
        then
            Arity = list.length(Args),
            ModeCtor = mode_ctor(Name, Arity),
            Expansions = [ModeCtor0 | Expansions0],
            check_for_cyclic_mode(ModeTable, OrigModeCtor, ModeCtor,
                Expansions, Context, InvalidMode, !Specs)
        else
            InvalidMode = no
        )
    ).

:- pred report_circular_inst_equiv_error(inst_ctor::in, inst_ctor::in,
    list(inst_ctor)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_circular_inst_equiv_error(OrigInstCtor, InstCtor, Expansions, Context,
        !Specs) :-
    report_circular_equiv_error("inst", "insts",
        inst_ctor_to_circ_id(OrigInstCtor), inst_ctor_to_circ_id(InstCtor),
        list.map(inst_ctor_to_circ_id, Expansions),
        Context, !Specs).

:- pred report_circular_mode_equiv_error(mode_ctor::in, mode_ctor::in,
    list(mode_ctor)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_circular_mode_equiv_error(OrigModeCtor, ModeCtor, Expansions, Context,
        !Specs) :-
    report_circular_equiv_error("mode", "modes",
        mode_ctor_to_circ_id(OrigModeCtor), mode_ctor_to_circ_id(ModeCtor),
        list.map(mode_ctor_to_circ_id, Expansions),
        Context, !Specs).

:- type circ_id
    --->    circ_id(sym_name, arity).

:- func inst_ctor_to_circ_id(inst_ctor) = circ_id.
:- func mode_ctor_to_circ_id(mode_ctor) = circ_id.

inst_ctor_to_circ_id(inst_ctor(SymName, Arity)) = circ_id(SymName, Arity).
mode_ctor_to_circ_id(mode_ctor(SymName, Arity)) = circ_id(SymName, Arity).

:- pred report_circular_equiv_error(string::in, string::in,
    circ_id::in, circ_id::in, list(circ_id)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_circular_equiv_error(One, Several, OrigId, Id, Expansions, Context,
        !Specs) :-
    ( if Id = OrigId then
        % Report an error message of the form
        %   Error: circular equivalence <kind> foo/0.
        % or
        %   Error: circular equivalence <kind>s foo/0 and bar/1.
        % or
        %   Error: circular equivalence <kind>s foo/0, bar/1,
        %   and baz/2.
        % where <kind> is either "inst" or "mode".

        Kinds = choose_number(Expansions, One, Several),
        ExpansionPieces = list.map(
            ( func(circ_id(SymName, Arity)) =
                qual_sym_name_arity(sym_name_arity(SymName, Arity))
            ),
            Expansions),
        Pieces = [words("Error: circular equivalence"), fixed(Kinds)]
            ++ component_list_to_pieces("and", ExpansionPieces) ++
            [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        % We have an inst `OrigId' which is not itself circular,
        % but which is defined in terms of `Id' which is circular.
        % Don't bother reporting it now -- it should have already been
        % reported when we processed the definition of Id.
        true
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mode.
%---------------------------------------------------------------------------%
