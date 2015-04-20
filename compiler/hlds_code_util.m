%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_code_util.m.
%
% Various utilities routines for use during HLDS generation.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_code_util.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Find out how a function symbol (constructor) is represented
    % in the given type.
    %
:- func cons_id_to_tag(module_info, cons_id) = cons_tag.

    % Given a list of types, mangle the names so into a string which
    % identifies them. The types must all have their top level functor
    % bound, with any arguments free variables.
    %
:- pred make_instance_string(list(mer_type)::in, string::out) is det.

    % Given a type_ctor, return the cons_id that represents its type_ctor_info.
    %
:- func type_ctor_info_cons_id(type_ctor) = cons_id.

    % Given a type_ctor, return the cons_id that represents its type_ctor_info.
    %
:- func base_typeclass_info_cons_id(instance_table,
    prog_constraint, int, list(mer_type)) = cons_id.

    % Succeeds iff this inst is one that can be used in a valid
    % mutable declaration.
    %
:- pred is_valid_mutable_inst(module_info::in, mer_inst::in) is semidet.

%-----------------------------------------------------------------------------%

    % Find the procedure with argmodes which match the ones we want.
    %
:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info)::in,
    list(mer_mode)::in, module_info::in, proc_id::out) is semidet.

    % Find the procedure with declared argmodes which match the ones we want.
    % If there was no mode declaration, then use the inferred argmodes.
    % Allow for a renaming between the inst vars.
    %
:- pred get_procedure_matching_declmodes_with_renaming(
    assoc_list(proc_id, proc_info)::in, list(mer_mode)::in,
    module_info::in, proc_id::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred.
:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

cons_id_to_tag(ModuleInfo, ConsId) = Tag:-
    (
        ConsId = int_const(Int),
        Tag = int_tag(Int)
    ;
        ConsId = float_const(Float),
        Tag = float_tag(Float)
    ;
        ConsId = char_const(Char),
        char.to_int(Char, CharCode),
        Tag = int_tag(CharCode)
    ;
        ConsId = string_const(String),
        Tag = string_tag(String)
    ;
        ConsId = impl_defined_const(_),
        unexpected($module, $pred, "implementation_defined_const")
    ;
        ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = closure_tag(PredId, ProcId, EvalMethod)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
        Tag = type_ctor_info_tag(ModuleName, TypeName, Arity)
    ;
        ConsId = base_typeclass_info_const(ModuleName, ClassName,
            _Instance, EncodedArgs),
        Tag = base_typeclass_info_tag(ModuleName, ClassName, EncodedArgs)
    ;
        ( ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ),
        Tag = unshared_tag(0)
    ;
        ConsId = type_info_const(TIConstNum),
        Tag = type_info_const_tag(TIConstNum)
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        Tag = typeclass_info_const_tag(TCIConstNum)
    ;
        ConsId = ground_term_const(ConstNum, SubConsId),
        SubConsTag = cons_id_to_tag(ModuleInfo, SubConsId),
        Tag = ground_term_const_tag(ConstNum, SubConsTag)
    ;
        ConsId = tabling_info_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = tabling_info_tag(PredId, ProcId)
    ;
        ConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = deep_profiling_proc_layout_tag(PredId, ProcId)
    ;
        ConsId = table_io_entry_desc(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = table_io_entry_tag(PredId, ProcId)
    ;
        ConsId = tuple_cons(Arity),
        % Tuples do not need a tag. Note that unary tuples are not treated
        % as no_tag types. There is no reason why they couldn't be, it is
        % just not worth the effort.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, TargetLang),
        (
            ( TargetLang = target_c
            ; TargetLang = target_erlang
            ),
            ( Arity = 0 ->
                Tag = int_tag(0)
            ;
                Tag = single_functor_tag
            )
        ;
            % For these target languages, converting arity-zero tuples into
            % dummy integer tags results in invalid code being generated.
            ( TargetLang = target_il
            ; TargetLang = target_csharp
            ; TargetLang = target_java
            ),
            Tag = single_functor_tag
        )
    ;
        ConsId = cons(_Name, _Arity, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(_, ConsTagTable, _, _, _, _, _, _, _),
            map.lookup(ConsTagTable, ConsId, Tag)
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_, _)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected($module, $pred, "type is not d.u. type")
        )
    ).

%-----------------------------------------------------------------------------%

make_instance_string(InstanceTypes, InstanceString) :-
    % Note that for historical reasons, builtin types are treated as being
    % unqualified (`int') rather than being qualified (`builtin.int')
    % at this point.
    list.map(type_to_string, InstanceTypes, InstanceStrings),
    string.append_list(InstanceStrings, InstanceString).

:- pred type_to_string(mer_type::in, string::out) is det.

type_to_string(Type, String) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeName, TypeArity),
    TypeNameString = sym_name_to_string_sep(TypeName, "__"),
    string.int_to_string(TypeArity, TypeArityString),
    String = TypeNameString ++ "__arity" ++ TypeArityString ++ "__".

%-----------------------------------------------------------------------------%

type_ctor_info_cons_id(TypeCtor) = ConsId :-
    type_ctor_module_name_arity(TypeCtor, ModuleName, Name, Arity),
    ConsId = type_ctor_info_const(ModuleName, Name, Arity).

base_typeclass_info_cons_id(InstanceTable, Constraint, InstanceNum,
        InstanceTypes) = ConsId :-
    Constraint = constraint(ClassName, ConstraintArgTypes),
    ClassId = class_id(ClassName, list.length(ConstraintArgTypes)),
    map.lookup(InstanceTable, ClassId, InstanceList),
    list.det_index1(InstanceList, InstanceNum, InstanceDefn),
    InstanceModuleName = InstanceDefn ^ instance_module,
    make_instance_string(InstanceTypes, InstanceString),
    ConsId = base_typeclass_info_const(InstanceModuleName, ClassId,
        InstanceNum, InstanceString).

%----------------------------------------------------------------------------%

is_valid_mutable_inst(ModuleInfo, Inst) :-
    set.init(Expansions),
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions).

:- pred is_valid_mutable_inst_2(module_info::in, mer_inst::in,
    set(inst_name)::in) is semidet.

is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions0) :-
    (
        ( Inst = any(Uniq, _)
        ; Inst = ground(Uniq, _)
        ),
        Uniq = shared
    ;
        Inst = bound(shared, _, BoundInsts),
        are_valid_mutable_bound_insts(ModuleInfo, BoundInsts, Expansions0)
    ;
        Inst = defined_inst(InstName),
        ( if not set.member(InstName, Expansions0) then
            set.insert(InstName, Expansions0, Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            is_valid_mutable_inst_2(ModuleInfo, SubInst, Expansions)
        else
            true
        )
    ).

:- pred are_valid_mutable_bound_insts(module_info::in, list(bound_inst)::in,
    set(inst_name)::in) is semidet.

are_valid_mutable_bound_insts(_ModuleInfo, [], _Expansions0).
are_valid_mutable_bound_insts(ModuleInfo, [BoundInst | BoundInsts],
        Expansions0) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    are_valid_mutable_insts(ModuleInfo, ArgInsts, Expansions0),
    are_valid_mutable_bound_insts(ModuleInfo, BoundInsts, Expansions0).

:- pred are_valid_mutable_insts(module_info::in, list(mer_inst)::in,
    set(inst_name)::in) is semidet.

are_valid_mutable_insts(_ModuleInfo, [], _Expansions0).
are_valid_mutable_insts(ModuleInfo, [Inst | Insts], Expansions0) :-
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions0),
    are_valid_mutable_insts(ModuleInfo, Insts, Expansions0).

%----------------------------------------------------------------------------%

get_procedure_matching_argmodes(Procs, Modes0, ModuleInfo, ProcId) :-
    list.map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_argmodes_2(assoc_list(proc_id, proc_info)::in,
    list(mer_mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes_2([P | Procs], Modes, ModuleInfo, OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_get_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, OurProcId)
    ).

:- pred mode_list_matches(list(mer_mode)::in, list(mer_mode)::in,
    module_info::in) is semidet.

mode_list_matches([], [], _).
mode_list_matches([Mode1 | Modes1], [Mode2 | Modes2], ModuleInfo) :-
    % Use mode_get_insts_semidet instead of mode_get_insts to avoid
    % aborting if there are undefined modes.
    % XXX
    mode_get_insts_semidet(ModuleInfo, Mode1, Inst1, Inst2),
    mode_get_insts_semidet(ModuleInfo, Mode2, Inst1, Inst2),
    mode_list_matches(Modes1, Modes2, ModuleInfo).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

get_procedure_matching_declmodes_with_renaming(Procs, Modes0,
        ModuleInfo, ProcId) :-
    list.map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_declmodes_with_renaming_2(Procs, Modes,
        ModuleInfo, ProcId).

:- pred get_procedure_matching_declmodes_with_renaming_2(
    assoc_list(proc_id, proc_info)::in, list(mer_mode)::in,
    module_info::in, proc_id::out) is semidet.

get_procedure_matching_declmodes_with_renaming_2([P | Procs], Modes,
        ModuleInfo, OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_declared_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches_with_renaming(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_declmodes_with_renaming_2(Procs, Modes,
            ModuleInfo, OurProcId)
    ).

:- type inst_var_renaming == map(inst_var, inst_var).
:- type inst_var_renamings == list(inst_var_renaming).

    % Succeeds if two lists of modes match allowing for a renaming
    % of inst variables between the two lists.
    %
:- pred mode_list_matches_with_renaming(list(mer_mode)::in,
    list(mer_mode)::in, module_info::in) is semidet.

mode_list_matches_with_renaming(ModesA, ModesB, ModuleInfo) :-
    mode_list_matches_with_renaming(ModesA, ModesB, _, ModuleInfo).

:- pred mode_list_matches_with_renaming(list(mer_mode)::in,
    list(mer_mode)::in, inst_var_renaming::out, module_info::in)
    is semidet.

mode_list_matches_with_renaming(ModesA, ModesB, Renaming, ModuleInfo) :-
    mode_list_matches_with_renaming_2(ModesA, ModesB, [], Renamings,
        ModuleInfo),
    list.foldl(merge_inst_var_renamings, Renamings, map.init, Renaming).

:- pred mode_list_matches_with_renaming_2(
    list(mer_mode)::in, list(mer_mode)::in,
    inst_var_renamings::in, inst_var_renamings::out,
    module_info::in) is semidet.

mode_list_matches_with_renaming_2([], [], !Renaming, _).
mode_list_matches_with_renaming_2([ModeA | ModesA], [ModeB | ModesB],
        !Substs, ModuleInfo) :-
    % We use mode_get_insts_semidet instead of mode_get_insts to avoid
    % aborting if there are undefined modes. (Undefined modes get
    % reported later).

    mode_get_insts_semidet(ModuleInfo, ModeA, InstAInitial, InstAFinal),
    mode_get_insts_semidet(ModuleInfo, ModeB, InstBInitial, InstBFinal),
    match_insts_with_renaming(ModuleInfo, InstAInitial, InstBInitial,
        InitialSubst),
    match_insts_with_renaming(ModuleInfo, InstAFinal, InstBFinal,
        FinalSubst),
    list.append([InitialSubst, FinalSubst], !Substs),
    mode_list_matches_with_renaming_2(ModesA, ModesB, !Substs, ModuleInfo).

:- pred match_corresponding_inst_lists_with_renaming(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

match_corresponding_inst_lists_with_renaming(_, [], [], !Renaming).
match_corresponding_inst_lists_with_renaming(ModuleInfo, [A | As], [B | Bs],
        !Renaming) :-
    match_insts_with_renaming(ModuleInfo, A, B, Renaming0),
    merge_inst_var_renamings(Renaming0, !Renaming),
    match_corresponding_inst_lists_with_renaming(ModuleInfo, As, Bs,
        !Renaming).

:- pred match_corresponding_bound_inst_lists_with_renaming(module_info::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

match_corresponding_bound_inst_lists_with_renaming(_, [], [], !Renaming).
match_corresponding_bound_inst_lists_with_renaming(ModuleInfo,
        [A | As], [B | Bs], !Renaming) :-
    A = bound_functor(ConsId, ArgsA),
    B = bound_functor(ConsId, ArgsB),
    match_corresponding_inst_lists_with_renaming(ModuleInfo, ArgsA, ArgsB,
        map.init, Renaming0),
    merge_inst_var_renamings(Renaming0, !Renaming),
    match_corresponding_bound_inst_lists_with_renaming(ModuleInfo, As, Bs,
        !Renaming).

:- pred match_insts_with_renaming(module_info::in, mer_inst::in, mer_inst::in,
    map(inst_var, inst_var)::out) is semidet.

match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    (
        InstA = not_reached,
        InstB = not_reached,
        map.init(Renaming)
    ;
        InstA = free,
        InstB = free,
        map.init(Renaming)
    ;
        InstA = free(Type),
        InstB = free(Type),
        map.init(Renaming)
    ;
        InstA = any(Uniq, HOInstInfoA),
        InstB = any(Uniq, HOInstInfoB),
        match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
            Renaming)
    ;
        InstA = ground(Uniq, HOInstInfoA),
        InstB = ground(Uniq, HOInstInfoB),
        match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
            Renaming)
    ;
        InstA = bound(Uniq, _, BoundInstsA),
        InstB = bound(Uniq, _, BoundInstsB),
        match_corresponding_bound_inst_lists_with_renaming(ModuleInfo,
            BoundInstsA, BoundInstsB, map.init, Renaming)
    ;
        InstA = inst_var(VarA),
        InstB = inst_var(VarB),
        Renaming = map.singleton(VarA, VarB)
    ;
        InstA = constrained_inst_vars(InstVarSetA, SpecInstA),
        InstB = constrained_inst_vars(InstVarSetB, SpecInstB),

        % We will deal with the specified inst first.
        match_insts_with_renaming(ModuleInfo, SpecInstA, SpecInstB, Renaming0),
        ListVarA = set.to_sorted_list(InstVarSetA),
        ListVarB = set.to_sorted_list(InstVarSetB),
        (
            ListVarA = [VarA0],
            ListVarB = [VarB0]
        ->
            VarA = VarA0,
            VarB = VarB0
        ;
            unexpected($module, $pred, "non-singleton sets")
        ),
        ( map.search(Renaming0, VarA, SpecVarB) ->
            % If VarA was already in the renaming then check that it is
            % consistent with the renaming from the set of inst vars.
            VarB = SpecVarB,
            Renaming = Renaming0
        ;
            map.insert(VarA, VarB, Renaming0, Renaming)
        )
    ;
        InstA = defined_inst(InstNameA),
        InstB = defined_inst(InstNameB),
        match_inst_names_with_renaming(ModuleInfo, InstNameA, InstNameB,
            Renaming)
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        match_corresponding_inst_lists_with_renaming(ModuleInfo, ArgsA, ArgsB,
            map.init, Renaming)
    ).

:- pred match_ho_inst_infos_with_renaming(module_info::in, ho_inst_info::in,
    ho_inst_info::in, map(inst_var, inst_var)::out) is semidet.

match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
        Renaming) :-
    (
        HOInstInfoA = none,
        HOInstInfoB = none,
        Renaming = map.init
    ;
        HOInstInfoA = higher_order(PredInstInfoA),
        HOInstInfoB = higher_order(PredInstInfoB),
        PredInstInfoA = pred_inst_info(PredOrFunc, ModesA, _, Detism),
        PredInstInfoB = pred_inst_info(PredOrFunc, ModesB, _, Detism),
        mode_list_matches_with_renaming(ModesA, ModesB, Renaming, ModuleInfo)
    ).

:- pred match_inst_names_with_renaming(module_info::in,
    inst_name::in, inst_name::in, inst_var_renaming::out) is semidet.

match_inst_names_with_renaming(ModuleInfo, InstNameA, InstNameB, Renaming) :-
    (
        InstNameA = user_inst(Name, ArgsA),
        InstNameB = user_inst(Name, ArgsB),
        match_corresponding_inst_lists_with_renaming(ModuleInfo,
            ArgsA, ArgsB, map.init, Renaming)
    ;
        % XXX The rest of these are introduced by the compiler, it doesn't
        % look like they need any special treatment.
        ( InstNameA = unify_inst(_, _, _, _)
        ; InstNameA = merge_inst(_, _)
        ; InstNameA = ground_inst(_, _, _, _)
        ; InstNameA = any_inst(_, _, _, _)
        ; InstNameA = shared_inst(_)
        ; InstNameA = mostly_uniq_inst(_)
        ),
        InstNameB = InstNameA,
        Renaming = map.init
    ).

:- pred merge_inst_var_renamings(inst_var_renaming::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

merge_inst_var_renamings(RenamingA, RenamingB, Result) :-
    map.union(merge_common_inst_vars, RenamingA, RenamingB, Result).

:- pred merge_common_inst_vars(inst_var::in, inst_var::in, inst_var::out)
    is semidet.

merge_common_inst_vars(A, A, A).

%----------------------------------------------------------------------------%
:- end_module hlds.hlds_code_util.
%----------------------------------------------------------------------------%
