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
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------%

:- pred module_add_inst_defn(inst_status::in, item_inst_defn_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred module_add_mode_defn(mode_status::in, item_mode_defn_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred check_inst_defns(module_info::in, ims_list(item_inst_defn_info)::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred check_mode_defns(module_info::in, ims_list(item_mode_defn_info)::in,
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
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

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
            report_multiply_defined("inst", InstSymName, user_arity(InstArity),
                Context, OrigContext, [], !Specs)
        )
    ).

%---------------------%

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
            report_multiply_defined("mode", Name, user_arity(Arity),
                Context, OrigContext, [], !Specs)
        )
    ).

%---------------------%

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

check_inst_defns(ModuleInfo, ImsSubLists, !FoundInvalidInstOrMode, !Specs) :-
    find_eqv_cycles_in_insts(ModuleInfo, ImsSubLists, set.init, Cycles),
    ( if set.is_empty(Cycles) then
        true
    else
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode,
        list.map(cycle_to_error_spec(ModuleInfo, iom_inst),
            set.to_sorted_list(Cycles), CycleSpecs),
        !:Specs = CycleSpecs ++ !.Specs
    ).

check_mode_defns(ModuleInfo, ImsSubLists, !FoundInvalidInstOrMode, !Specs) :-
    find_eqv_cycles_in_modes(ModuleInfo, ImsSubLists, set.init, Cycles),
    ( if set.is_empty(Cycles) then
        true
    else
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode,
        list.map(cycle_to_error_spec(ModuleInfo, iom_mode),
            set.to_sorted_list(Cycles), CycleSpecs),
        !:Specs = CycleSpecs ++ !.Specs
    ).

%---------------------%

:- pred find_eqv_cycles_in_insts(module_info::in,
    ims_list(item_inst_defn_info)::in,
    set(cycle)::in, set(cycle)::out) is det.

find_eqv_cycles_in_insts(_, [], !Cycles).
find_eqv_cycles_in_insts(ModuleInfo, [ImsSubList | ImsSubLists], !Cycles) :-
    ImsSubList = ims_sub_list(_ItemMercuryStatus, InstDefns),
    list.foldl(find_eqv_cycles_in_inst(ModuleInfo), InstDefns, !Cycles),
    find_eqv_cycles_in_insts(ModuleInfo, ImsSubLists, !Cycles).

:- pred find_eqv_cycles_in_modes(module_info::in,
    ims_list(item_mode_defn_info)::in,
    set(cycle)::in, set(cycle)::out) is det.

find_eqv_cycles_in_modes(_, [], !Cycles).
find_eqv_cycles_in_modes(ModuleInfo, [ImsSubList | ImsSubLists], !Cycles) :-
    ImsSubList = ims_sub_list(_ItemMercuryStatus, ModeDefns),
    list.foldl(find_eqv_cycles_in_mode(ModuleInfo), ModeDefns, !Cycles),
    find_eqv_cycles_in_modes(ModuleInfo, ImsSubLists, !Cycles).

%---------------------%

:- pred find_eqv_cycles_in_inst(module_info::in, item_inst_defn_info::in,
    set(cycle)::in, set(cycle)::out) is det.

find_eqv_cycles_in_inst(ModuleInfo, ItemInstDefnInfo, !Cycles) :-
    ItemInstDefnInfo = item_inst_defn_info(InstName, InstParams, _MaybeForType,
        _MaybeAbstractInstDefn, _VarSet, _Context, _SeqNum),
    module_info_get_inst_table(ModuleInfo, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    list.length(InstParams, InstArity),
    InstCtor = inst_ctor(InstName, InstArity),
    TestArgs = list.duplicate(InstArity, not_reached),
    map.init(Expansions0),
    check_for_cyclic_inst(UserInstTable, InstCtor, InstCtor, TestArgs,
        Expansions0, !Cycles).

:- pred find_eqv_cycles_in_mode(module_info::in, item_mode_defn_info::in,
    set(cycle)::in, set(cycle)::out) is det.

find_eqv_cycles_in_mode(ModuleInfo, ItemModeDefnInfo, !Cycles) :-
    ItemModeDefnInfo = item_mode_defn_info(ModeName, ModeParams,
        _MaybeAbstractModeDefn, _VarSet, _Context, _SeqNum),
    module_info_get_mode_table(ModuleInfo, ModeTable),
    mode_table_get_mode_defns(ModeTable, ModeDefns),
    list.length(ModeParams, ModeArity),
    ModeCtor = mode_ctor(ModeName, ModeArity),
    map.init(Expansions0),
    check_for_cyclic_mode(ModeDefns, ModeCtor, ModeCtor,
        Expansions0, !Cycles).

%---------------------%

:- pred check_for_cyclic_inst(user_inst_table::in,
    inst_ctor::in, inst_ctor::in, list(mer_inst)::in,
    expansions::in, set(cycle)::in, set(cycle)::out) is det.

check_for_cyclic_inst(UserInstTable, OrigInstCtor, InstCtor0, Args0,
        Expansions0, !Cycles) :-
    InstCtor0 = inst_ctor(SymName0, Arity0),
    SNA0 = sym_name_arity(SymName0, Arity0),
    ( if map.search(Expansions0, SNA0, _OldContext) then
        ( if OrigInstCtor = InstCtor0 then
            map.to_sorted_assoc_list(Expansions0, ExpansionsAL0),
            set.insert(cycle(ExpansionsAL0), !Cycles)
        else
            % Both OrigInstCtor and InstCtor0 suffer from needing infinite
            % expansion, but the circularity we have just detected does
            % NOT involve OrigInstCtor.
            true
        )
    else
        ( if
            map.search(UserInstTable, InstCtor0, InstDefn),
            InstDefn = hlds_inst_defn(_, Params, Body, _, Context, _),
            Body = eqv_inst(EqvInst0),
            inst_substitute_arg_list(Params, Args0, EqvInst0, EqvInst),
            EqvInst = defined_inst(user_inst(SymName, Args))
        then
            list.length(Args, Arity),
            InstCtor = inst_ctor(SymName, Arity),
            map.det_insert(SNA0, Context, Expansions0, Expansions1),
            check_for_cyclic_inst(UserInstTable, OrigInstCtor, InstCtor, Args,
                Expansions1, !Cycles)
        else
            true
        )
    ).

:- pred check_for_cyclic_mode(mode_defns::in, mode_ctor::in, mode_ctor::in,
    expansions::in, set(cycle)::in, set(cycle)::out) is det.

check_for_cyclic_mode(ModeDefns, OrigModeCtor, ModeCtor0,
        Expansions0, !Cycles) :-
    ModeCtor0 = mode_ctor(SymName0, Arity0),
    SNA0 = sym_name_arity(SymName0, Arity0),
    ( if map.search(Expansions0, SNA0, _OldContext) then
        ( if ModeCtor0 = OrigModeCtor then
            map.to_sorted_assoc_list(Expansions0, ExpansionsAL0),
            set.insert(cycle(ExpansionsAL0), !Cycles)
        else
            % Both OrigModeCtor and ModeCtor0 suffer from needing infinite
            % expansion, but the circularity we have just detected does
            % NOT involve OrigModeCtor.
            true
        )
    else
        ( if
            map.search(ModeDefns, ModeCtor0, ModeDefn),
            ModeDefn = hlds_mode_defn(_, _, Body, Context, _),
            Body = hlds_mode_body(EqvMode),
            EqvMode = user_defined_mode(SymName, Args)
        then
            list.length(Args, Arity),
            ModeCtor = mode_ctor(SymName, Arity),
            map.det_insert(SNA0, Context, Expansions0, Expansions1),
            check_for_cyclic_mode(ModeDefns, OrigModeCtor, ModeCtor,
                Expansions1, !Cycles)
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

    % The inst_ctors or mode_ctors we have seen so far, together
    % with the contexts of their definitions.
:- type expansions == map(sym_name_arity, prog_context).

    % A cycle is an expansion that starts and ends at the same inst_ctor
    % or mode_ctor.
    %
    % We store it as an assoc_list, not a map, both because we need it
    % in the form of a list when generating the error message, and because
    % unlike maps, two assoc_lists are be semantically the same
    % if and only if they are also syntactially the same.
:- type cycle
    --->    cycle(assoc_list(sym_name_arity, prog_context)).

:- type inst_or_mode
    --->    iom_inst
    ;       iom_mode.

:- pred cycle_to_error_spec(module_info::in, inst_or_mode::in, cycle::in,
    error_spec::out) is det.

cycle_to_error_spec(ModuleInfo, InstOrMode, Cycle, Spec) :-
    (
        InstOrMode = iom_inst,
        InstOrModeWord = "inst",
        AnInstOrModeWord = "an inst"
    ;
        InstOrMode = iom_mode,
        InstOrModeWord = "mode",
        AnInstOrModeWord = "a mode"
    ),
    Cycle = cycle(SNAsContexts),
    module_info_get_name(ModuleInfo, ModuleName),
    list.filter(sna_context_is_for_module(ModuleName), SNAsContexts,
        LocalSNAsContexts, OtherSNAsContexts),
    (
        LocalSNAsContexts = [],
        (
            OtherSNAsContexts = [],
            unexpected($pred, "cycle has no entries")
        ;
            OtherSNAsContexts = [HeadOtherSNAContext | TailOtherSNAsContexts],
            HeadOtherSNAContext = _HeadSNA - HeadContext,
            other_sna_and_context_to_piece(HeadOtherSNAContext, HeadSNAPiece),
            list.map(other_sna_and_context_to_piece,
                TailOtherSNAsContexts, LaterSNAPieces),
            ContextMsgs = []
        )
    ;
        LocalSNAsContexts = [HeadLocalSNAContext | TailLocalSNAsContexts],
        HeadLocalSNAContext = _HeadSNA - HeadContext,
        local_sna_and_context_to_piece_and_msg(ModuleInfo, InstOrMode,
            HeadLocalSNAContext, HeadSNAPiece, _HeadMsg),
        list.map2(
            local_sna_and_context_to_piece_and_msg(ModuleInfo, InstOrMode),
            TailLocalSNAsContexts, TailLocalSNAPieces, ContextMsgs),
        list.map(other_sna_and_context_to_piece,
            OtherSNAsContexts, OtherSNAPieces),
        LaterSNAPieces = TailLocalSNAPieces ++ OtherSNAPieces
    ),
    PreludePieces = [words("Error:"),
        words(InstOrModeWord), words("name"), HeadSNAPiece,
        words("expands to"), words(AnInstOrModeWord),
        words("containing itself")],
    ConsequencePieces = [words("which means that"),
        words("processing any reference to it"),
        words("would require an infinite sequence of expansions."), nl],
    (
        LaterSNAPieces = [],
        HeadPieces = PreludePieces ++ [suffix(",")] ++ ConsequencePieces
    ;
        LaterSNAPieces = [LaterSNAPiece],
        HeadPieces = PreludePieces ++
            [words("through"), LaterSNAPiece, suffix(",")]
            ++ ConsequencePieces
    ;
        LaterSNAPieces = [_, _ | _],
        LaterSNAPieceLists = list.map(make_singleton_list, LaterSNAPieces),
        CyclePieces = component_list_to_line_pieces(LaterSNAPieceLists,
            [suffix(","), nl]),
        HeadPieces = PreludePieces ++ [words("through"), nl]
            ++ CyclePieces ++ ConsequencePieces
    ),
    HeadMsg = simplest_msg(HeadContext, HeadPieces),
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
        [HeadMsg | ContextMsgs]).

:- pred sna_context_is_for_module(module_name::in,
    pair(sym_name_arity, prog_context)::in) is semidet.

sna_context_is_for_module(ModuleName, SNA - _Context) :-
    SNA = sym_name_arity(SymName, _Arity),
    SymName = qualified(ModuleName, _).

:- pred local_sna_and_context_to_piece_and_msg(module_info::in,
    inst_or_mode::in, pair(sym_name_arity, prog_context)::in,
    format_component::out, error_msg::out) is det.

local_sna_and_context_to_piece_and_msg(ModuleInfo, InstOrMode, SNA - Context,
        SNAPiece, Msg) :-
    % Module qualify the names of local insts or modes *only* if
    % those names could be confused with insts or modes of the same name
    % in the builtin module. (Error messages from the Mercury compiler
    % normally module qualify every reference that is to an entity
    % that is not either in the current module or the builtin module.)
    SNA = sym_name_arity(SymName, Arity),
    Name = unqualify_name(SymName),
    BuiltinSymName = qualified(mercury_public_builtin_module, Name),
    (
        InstOrMode = iom_inst,
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        BuiltinInstCtor = inst_ctor(BuiltinSymName, Arity),
        ( if map.search(UserInstTable, BuiltinInstCtor, _) then
            SNAPiece = qual_sym_name_arity(SNA)
        else
            SNAPiece = unqual_sym_name_arity(SNA)
        )
    ;
        InstOrMode = iom_mode,
        module_info_get_mode_table(ModuleInfo, ModeTable),
        mode_table_get_mode_defns(ModeTable, ModeDefns),
        BuiltinModeCtor = mode_ctor(BuiltinSymName, Arity),
        ( if map.search(ModeDefns, BuiltinModeCtor, _ModeDefn) then
            SNAPiece = qual_sym_name_arity(SNA)
        else
            SNAPiece = unqual_sym_name_arity(SNA)
        )
    ),
    MsgPieces = [words("The definition of"), SNAPiece, words("is here."), nl],
    Msg = simplest_msg(Context, MsgPieces).

:- pred other_sna_and_context_to_piece(pair(sym_name_arity, prog_context)::in,
    format_component::out) is det.

other_sna_and_context_to_piece(SNA - _Context, SNAPiece) :-
    SNAPiece = qual_sym_name_arity(SNA).

:- func make_singleton_list(T) = list(T).

make_singleton_list(X) = [X].

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mode.
%---------------------------------------------------------------------------%
