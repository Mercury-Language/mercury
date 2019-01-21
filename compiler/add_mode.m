%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_mode.m.
%
% This submodule of make_hlds handles the declarations of new insts and modes.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_mode.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module list.

:- pred module_add_inst_defn(item_inst_defn_info::in, inst_status::in,
    bool::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_add_mode_defn(item_mode_defn_info::in, mode_status::in,
    bool::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds_error.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module map.
:- import_module maybe.
:- import_module require.

%----------------------------------------------------------------------------%

module_add_inst_defn(ItemInstDefnInfo, InstStatus, InvalidInst, !ModuleInfo,
        !Specs) :-
    ItemInstDefnInfo = item_inst_defn_info(InstName, InstParams, MaybeForType,
        InstDefn, VarSet, Context, _SeqNum),
    % Add the definition of this inst to the HLDS inst table.
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_user_insts(InstTable0, UserInstTable0),
    insts_add(VarSet, InstName, InstParams, MaybeForType, InstDefn, Context,
        InstStatus, UserInstTable0, UserInstTable, !Specs),
    inst_table_set_user_insts(UserInstTable, InstTable0, InstTable),
    module_info_set_inst_table(InstTable, !ModuleInfo),

    % Check if the inst is infinitely recursive (at the top level).
    InstArity = list.length(InstParams),
    InstId = inst_id(InstName, InstArity),
    TestArgs = list.duplicate(InstArity, not_reached),
    check_for_cyclic_inst(UserInstTable, InstId, InstId, TestArgs, [], Context,
        InvalidInst, !Specs).

:- pred insts_add(inst_varset::in, sym_name::in, list(inst_var)::in,
    maybe(type_ctor)::in, inst_defn::in, prog_context::in,
    inst_status::in, user_inst_table::in, user_inst_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

insts_add(_, _, _, _, abstract_inst, _, _, !UserInstTable, !Specs) :-
    % XXX handle abstract insts
    sorry($pred, "abstract insts not implemented").
insts_add(VarSet, InstSymName, InstParams, MaybeForType, eqv_inst(EqvInst),
        Context, InstStatus, !UserInstTable, !Specs) :-
    list.length(InstParams, InstArity),
    InstId = inst_id(InstSymName, InstArity),
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
                    qual_sym_name_and_arity(
                        sym_name_arity(ShortInstSymName, InstArity)),
                    words("includes references to function symbols,"),
                    words("but does not declare what type constructor"),
                    words("it is for."), nl],
                Option = warn_insts_with_functors_without_type,
                Msg = simple_msg(Context,
                    [option_is_set(Option, yes, [always(Pieces)])]),
                Severity =
                    severity_conditional(Option, yes, severity_warning, no),
                Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
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
                qual_sym_name_and_arity(
                    sym_name_arity(ShortInstSymName, InstArity)),
                words("is specified to be for a given type constructor,"),
                words("but it is not defined to be equivalent to a"),
                quote("bound"), words("inst."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ),
    InstDefn = hlds_inst_defn(VarSet, InstParams, eqv_inst(EqvInst), IFTC,
        Context, InstStatus),
    ( if map.insert(InstId, InstDefn, !UserInstTable) then
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
            map.lookup(!.UserInstTable, InstId, OrigInstDefn),
            OrigContext = OrigInstDefn ^ inst_context,
            Extras = [],
            report_multiple_def_error(InstSymName, InstArity, "inst",
                Context, OrigContext, Extras, !Specs)
        )
    ).

    % Check if the inst is infinitely recursive (at the top level).
    %
:- pred check_for_cyclic_inst(user_inst_table::in, inst_id::in, inst_id::in,
    list(mer_inst)::in, list(inst_id)::in, prog_context::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_inst(UserInstTable, OrigInstId, InstId0, Args0, Expansions0,
        Context, InvalidMode, !Specs) :-
    ( if list.member(InstId0, Expansions0) then
        report_circular_inst_equiv_error(OrigInstId, InstId0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    else
        ( if
            map.search(UserInstTable, InstId0, InstDefn),
            InstDefn = hlds_inst_defn(_, Params, Body, _, _, _),
            Body = eqv_inst(EqvInst0),
            inst_substitute_arg_list(Params, Args0, EqvInst0, EqvInst),
            EqvInst = defined_inst(user_inst(Name, Args))
        then
            Arity = list.length(Args),
            InstId = inst_id(Name, Arity),
            Expansions = [InstId0 | Expansions0],
            check_for_cyclic_inst(UserInstTable, OrigInstId, InstId, Args,
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
            ( InstModeImport = instmode_import_plain(_)
            ; InstModeImport = instmode_import_abstract
            ),
            ReportDup = yes
        ;
            InstModeImport = instmode_import_opt,
            ReportDup = no
        )
    ).

%-----------------------------------------------------------------------------%

module_add_mode_defn(ItemModeDefnInfo, ModeStatus, InvalidMode, !ModuleInfo,
        !Specs) :-
    ItemModeDefnInfo = item_mode_defn_info(Name, Params, ModeDefn, VarSet,
        Context, _SeqNum),
    module_info_get_mode_table(!.ModuleInfo, ModeTable0),
    modes_add(VarSet, Name, Params, ModeDefn, Context, ModeStatus, InvalidMode,
        ModeTable0, ModeTable, !Specs),
    module_info_set_mode_table(ModeTable, !ModuleInfo).

:- pred modes_add(inst_varset::in, sym_name::in, list(inst_var)::in,
    mode_defn::in, prog_context::in, mode_status::in, bool::out,
    mode_table::in, mode_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

modes_add(VarSet, Name, Args, ModeBody, Context, ModeStatus, InvalidMode,
        !ModeTable, !Specs) :-
    list.length(Args, Arity),
    ModeId = mode_id(Name, Arity),
    ModeBody = eqv_mode(EqvMode),
    HldsModeBody = hlds_mode_body(EqvMode),
    ModeDefn = hlds_mode_defn(VarSet, Args, HldsModeBody, Context, ModeStatus),
    ( if mode_table_insert(ModeId, ModeDefn, !ModeTable) then
        true
    else
        ModeStatus = mode_status(InstModeStatus),
        ReportDup = should_report_duplicate_inst_or_mode(InstModeStatus),
        (
            ReportDup = no
        ;
            ReportDup = yes,
            mode_table_get_mode_defns(!.ModeTable, ModeDefns),
            map.lookup(ModeDefns, ModeId, OrigModeDefn),
            OrigModeDefn = hlds_mode_defn(_, _, _, OrigContext, _),
            Extras = [],
            report_multiple_def_error(Name, Arity, "mode",
                Context, OrigContext, Extras, !Specs)
        )
    ),
    Expansions0 = [],
    check_for_cyclic_mode(!.ModeTable, ModeId, ModeId, Expansions0, Context,
        InvalidMode, !Specs).

    % Check if the mode is infinitely recursive at the top level.
    %
:- pred check_for_cyclic_mode(mode_table::in, mode_id::in, mode_id::in,
    list(mode_id)::in, prog_context::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_mode(ModeTable, OrigModeId, ModeId0, Expansions0, Context,
        InvalidMode, !Specs) :-
    ( if list.member(ModeId0, Expansions0) then
        report_circular_mode_equiv_error(OrigModeId, ModeId0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    else
        mode_table_get_mode_defns(ModeTable, ModeDefns),
        ( if
            map.search(ModeDefns, ModeId0, ModeDefn),
            ModeDefn = hlds_mode_defn(_, _, Body, _, _),
            Body = hlds_mode_body(EqvMode),
            EqvMode = user_defined_mode(Name, Args)
        then
            Arity = list.length(Args),
            ModeId = mode_id(Name, Arity),
            Expansions = [ModeId0 | Expansions0],
            check_for_cyclic_mode(ModeTable, OrigModeId, ModeId, Expansions,
                Context, InvalidMode, !Specs)
        else
            InvalidMode = no
        )
    ).

:- pred report_circular_inst_equiv_error(inst_id::in, inst_id::in,
    list(inst_id)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_circular_inst_equiv_error(OrigInstId, InstId, Expansions, Context,
        !Specs) :-
    report_circular_equiv_error("inst", "insts",
        inst_id_to_circ_id(OrigInstId), inst_id_to_circ_id(InstId),
        list.map(inst_id_to_circ_id, Expansions),
        Context, !Specs).

:- pred report_circular_mode_equiv_error(mode_id::in, mode_id::in,
    list(mode_id)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_circular_mode_equiv_error(OrigModeId, ModeId, Expansions, Context,
        !Specs) :-
    report_circular_equiv_error("mode", "modes",
        mode_id_to_circ_id(OrigModeId), mode_id_to_circ_id(ModeId),
        list.map(mode_id_to_circ_id, Expansions),
        Context, !Specs).

:- type circ_id
    --->    circ_id(sym_name, arity).

:- func inst_id_to_circ_id(inst_id) = circ_id.
:- func mode_id_to_circ_id(mode_id) = circ_id.

inst_id_to_circ_id(inst_id(SymName, Arity)) = circ_id(SymName, Arity).
mode_id_to_circ_id(mode_id(SymName, Arity)) = circ_id(SymName, Arity).

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
                qual_sym_name_and_arity(sym_name_arity(SymName, Arity))
            ),
            Expansions),
        Pieces = [words("Error: circular equivalence"), fixed(Kinds)]
            ++ component_list_to_pieces("and", ExpansionPieces) ++
            [suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        % We have an inst `OrigId' which is not itself circular,
        % but which is defined in terms of `Id' which is circular.
        % Don't bother reporting it now -- it should have already been
        % reported when we processed the definition of Id.
        true
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mode.
%----------------------------------------------------------------------------%
