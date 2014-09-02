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
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module list.

:- pred module_add_inst_defn(item_inst_defn_info::in, bool::out,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_add_mode_defn(item_mode_defn_info::in, bool::out,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module map.
:- import_module require.

%----------------------------------------------------------------------------%

module_add_inst_defn(ItemInstDefnInfo, InvalidMode, ItemStatus, !ModuleInfo,
        !Specs) :-
    ItemStatus = item_status(Status, _NeedQual),
    ItemInstDefnInfo = item_inst_defn_info(VarSet, Name, Params, InstDefn,
        Cond, Context, _SeqNum),
    % Add the definition of this inst to the HLDS inst table.
    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_user_insts(InstTable0, Insts0),
    insts_add(VarSet, Name, Params, InstDefn, Cond, Context, Status,
        Insts0, Insts, !Specs),
    inst_table_set_user_insts(Insts, InstTable0, InstTable),
    module_info_set_inst_table(InstTable, !ModuleInfo),

    % Check if the inst is infinitely recursive (at the top level).
    Arity = list.length(Params),
    InstId = inst_id(Name, Arity),
    TestArgs = list.duplicate(Arity, not_reached),
    check_for_cyclic_inst(Insts, InstId, InstId, TestArgs, [], Context,
        InvalidMode, !Specs).

:- pred insts_add(inst_varset::in, sym_name::in,
    list(inst_var)::in, inst_defn::in, condition::in, prog_context::in,
    import_status::in, user_inst_table::in, user_inst_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

insts_add(_, _, _, abstract_inst, _, _, _, !Insts, !Specs) :-
    % XXX handle abstract insts
    sorry($module, $pred, "abstract insts not implemented").
insts_add(VarSet, Name, Args, eqv_inst(Body), _Cond, Context, Status, !Insts,
        !Specs) :-
    list.length(Args, Arity),
    InstId = inst_id(Name, Arity),
    I = hlds_inst_defn(VarSet, Args, eqv_inst(Body), Context, Status),
    ( user_inst_table_insert(InstId, I, !Insts) ->
        true
    ;
        % If abstract insts are implemented, this will need to change
        % to update the hlds_inst_defn to the non-abstract inst.

        % XXX we should record each error using
        %    module_info_incr_errors
        user_inst_table_get_inst_defns(!.Insts, InstDefns),
        map.lookup(InstDefns, InstId, OrigI),
        OrigI = hlds_inst_defn(_, _, _, OrigContext, _),
        multiple_def_error(Status, Name, Arity, "inst", Context, OrigContext,
            [], !Specs)
    ).

    % Check if the inst is infinitely recursive (at the top level).
    %
:- pred check_for_cyclic_inst(user_inst_table::in, inst_id::in, inst_id::in,
    list(mer_inst)::in, list(inst_id)::in, prog_context::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_inst(UserInstTable, OrigInstId, InstId0, Args0, Expansions0,
        Context, InvalidMode, !Specs) :-
    ( list.member(InstId0, Expansions0) ->
        report_circular_inst_equiv_error(OrigInstId, InstId0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    ;
        user_inst_table_get_inst_defns(UserInstTable, InstDefns),
        (
            map.search(InstDefns, InstId0, InstDefn),
            InstDefn = hlds_inst_defn(_, Params, Body, _, _),
            Body = eqv_inst(EqvInst0),
            inst_substitute_arg_list(Params, Args0, EqvInst0, EqvInst),
            EqvInst = defined_inst(user_inst(Name, Args))
        ->
            Arity = list.length(Args),
            InstId = inst_id(Name, Arity),
            Expansions = [InstId0 | Expansions0],
            check_for_cyclic_inst(UserInstTable, OrigInstId, InstId, Args,
                Expansions, Context, InvalidMode, !Specs)
        ;
            InvalidMode = no
        )
    ).

%-----------------------------------------------------------------------------%

module_add_mode_defn(ItemModeDefnInfo, InvalidMode, ItemStatus, !ModuleInfo,
        !Specs) :-
    ItemModeDefnInfo = item_mode_defn_info(VarSet, Name, Params, ModeDefn,
        Cond, Context, _SeqNum),
    ItemStatus = item_status(Status, _NeedQual),
    module_info_get_mode_table(!.ModuleInfo, Modes0),
    modes_add(VarSet, Name, Params, ModeDefn, Cond, Context, Status,
        Modes0, Modes, InvalidMode, !Specs),
    module_info_set_mode_table(Modes, !ModuleInfo).

:- pred modes_add(inst_varset::in, sym_name::in, list(inst_var)::in,
    mode_defn::in, condition::in, prog_context::in, import_status::in,
    mode_table::in, mode_table::out, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

modes_add(VarSet, Name, Args, eqv_mode(Body), _Cond, Context, Status,
        !Modes, InvalidMode, !Specs) :-
    list.length(Args, Arity),
    ModeId = mode_id(Name, Arity),
    I = hlds_mode_defn(VarSet, Args, eqv_mode(Body), Context, Status),
    ( mode_table_insert(ModeId, I, !Modes) ->
        true
    ;
        mode_table_get_mode_defns(!.Modes, ModeDefns),
        map.lookup(ModeDefns, ModeId, OrigI),
        OrigI = hlds_mode_defn(_, _, _, OrigContext, _),
        % XXX We should record each error using module_info_incr_errors.
        multiple_def_error(Status, Name, Arity, "mode", Context, OrigContext,
            [], !Specs)
    ),
    check_for_cyclic_mode(!.Modes, ModeId, ModeId, [], Context, InvalidMode,
        !Specs).

    % Check if the mode is infinitely recursive at the top level.
    %
:- pred check_for_cyclic_mode(mode_table::in, mode_id::in, mode_id::in,
    list(mode_id)::in, prog_context::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_mode(ModeTable, OrigModeId, ModeId0, Expansions0, Context,
        InvalidMode, !Specs) :-
    ( list.member(ModeId0, Expansions0) ->
        report_circular_mode_equiv_error(OrigModeId, ModeId0, Expansions0,
            Context, !Specs),
        InvalidMode = yes
    ;
        mode_table_get_mode_defns(ModeTable, ModeDefns),
        (
            map.search(ModeDefns, ModeId0, ModeDefn),
            ModeDefn = hlds_mode_defn(_, _, Body, _, _),
            Body = eqv_mode(EqvMode),
            EqvMode = user_defined_mode(Name, Args)
        ->
            Arity = list.length(Args),
            ModeId = mode_id(Name, Arity),
            Expansions = [ModeId0 | Expansions0],
            check_for_cyclic_mode(ModeTable, OrigModeId, ModeId, Expansions,
                Context, InvalidMode, !Specs)
        ;
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
    ( Id = OrigId ->
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
            (func(circ_id(SymName, Arity)) =
                sym_name_and_arity(SymName / Arity)),
            Expansions),
        Pieces = [words("Error: circular equivalence"), fixed(Kinds)]
            ++ component_list_to_pieces(ExpansionPieces) ++ [suffix(".")],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        % We have an inst `OrigId' which is not itself circular,
        % but which is defined in terms of `Id' which is circular.
        % Don't bother reporting it now -- it have already been
        % reported when we processed the definition of Id.
        true
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mode.
%----------------------------------------------------------------------------%
