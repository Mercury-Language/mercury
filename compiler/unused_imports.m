%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2015-2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: unused_imports.m.
% Main author: petdr.
%
% This module scans the current module determining all the modules which are
% used in the module and then compares that with the set of modules imported
% and reports those modules which are unused.
%
% It also determines which imports aren't used in the interface, and thus
% should only be in the implementation.
%
% XXX The analysis carried out here can break in the presence of procedures
% that have both foreign language clauses and default Mercury clauses.
% The problem is that some of module imports may only be required by
% the Mercury clauses but these are thrown away if we use the foreign clauses.
% As a result the module imports may appear to be unused.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.unused_imports.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

    % This predicate issues a warning for each import_module
    % which is not directly used in this module, plus those
    % which are in the interface but should be in the implementation.
    %
:- pred warn_about_unused_imports(module_info::in, list(error_spec)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

warn_about_unused_imports(ModuleInfo, Specs) :-
    module_info_get_name(ModuleInfo, ModuleName),
    find_all_non_warn_modules(ModuleInfo, UsedModules),

    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
    map.to_assoc_list(AvailModuleMap, ModuleAvails),
    get_avail_modules_anywhere_interface(ModuleAvails,
        cord.init, AvailAnywhereCord,
        cord.init, AvailInterfaceCord),
    set.sorted_list_to_set(cord.list(AvailAnywhereCord),
        AvailAnywhereModules),
    set.sorted_list_to_set(cord.list(AvailInterfaceCord),
        AvailInterfaceModules),

    UsedInInterface = UsedModules ^ int_used_modules,
    UsedInImplementation = UsedModules ^ imp_used_modules,
    UsedAnywhere = set.union(UsedInInterface, UsedInImplementation),

    % The unused imports is simply the set of all imports minus all the
    % used modules.
    UnusedAnywhereImports = set.difference(AvailAnywhereModules, UsedAnywhere),

    % Determine the modules imported in the interface but not used in
    % the interface.
    UnusedInterfaceImports =
        set.difference(AvailInterfaceModules, UsedInInterface),

    map.foldl(
        maybe_warn_about_avail(ModuleName,
            UnusedAnywhereImports, UnusedInterfaceImports),
        AvailModuleMap, [], Specs).

:- pred get_avail_modules_anywhere_interface(
    assoc_list(module_name, avail_module_entry)::in,
    cord(module_name)::in, cord(module_name)::out,
    cord(module_name)::in, cord(module_name)::out) is det.

get_avail_modules_anywhere_interface([],
        !AvailAnywhereCord, !AvailInterfaceCord).
get_avail_modules_anywhere_interface([ModuleEntry | ModuleEntries],
        !AvailAnywhereCord, !AvailInterfaceCord) :-
    ModuleEntry = ModuleName - Entry,
    Entry = avail_module_entry(Section, _ImportOrUse, _Context),
    !:AvailAnywhereCord = cord.snoc(!.AvailAnywhereCord, ModuleName),
    (
        Section = ms_interface,
        !:AvailInterfaceCord = cord.snoc(!.AvailInterfaceCord, ModuleName)
    ;
        Section = ms_implementation
    ),
    get_avail_modules_anywhere_interface(ModuleEntries,
        !AvailAnywhereCord, !AvailInterfaceCord).

:- pred maybe_warn_about_avail(module_name::in,
    set(module_name)::in, set(module_name)::in,
    module_name::in, avail_module_entry::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_avail(TopModuleName,
        UnusedAnywhereImports, UnusedInterfaceImports,
        ModuleName, AvailEntry, !Specs) :-
    AvailEntry = avail_module_entry(Section, ImportOrUse, Avails),
    list.sort(compare_avails, Avails, SortedAvails),
    (
        SortedAvails = []
    ;
        SortedAvails = [HeadAvail | _],
        HeadAvail = avail_module(_, _, HeadContext),
        maybe_generate_redundant_avail_warnings(ModuleName, SortedAvails,
            [], !Specs),
        ( if
            set.member(ModuleName, UnusedAnywhereImports)
        then
            AnywhereSpec = generate_unused_warning(TopModuleName,
                ModuleName, ImportOrUse, HeadContext, aoi_anywhere),
            !:Specs = [AnywhereSpec | !.Specs],
            AnywhereWarning = yes
        else
            AnywhereWarning = no
        ),
        % Do not generate a report that a module is unused in the interface
        % if we have generated a report that it is unused *anywhere*.
        ( if
            Section = ms_interface,
            set.member(ModuleName, UnusedInterfaceImports),
            AnywhereWarning = no
        then
            InterfaceSpec = generate_unused_warning(TopModuleName,
                ModuleName, ImportOrUse, HeadContext, aoi_interface),
            !:Specs = [InterfaceSpec | !.Specs]
        else
            true
        )
    ).

:- pred compare_avails(avail_module::in, avail_module::in,
    comparison_result::out) is det.

compare_avails(AvailA, AvailB, Result) :-
    % Put interface before implementation, and import before use,
    % so that less general entries (entries that grant fewer permissions)
    % are always *after* more general entries.
    AvailA = avail_module(SectionA, ImportOrUseA, ContextA),
    AvailB = avail_module(SectionB, ImportOrUseB, ContextB),
    (
        SectionA = ms_interface,
        SectionB = ms_implementation,
        Result = (>)
    ;
        SectionA = ms_implementation,
        SectionB = ms_interface,
        Result = (>)
    ;
        ( SectionA = ms_interface, SectionB = ms_interface
        ; SectionA = ms_implementation, SectionB = ms_implementation
        ),
        (
            ImportOrUseA = import_decl,
            ImportOrUseB = use_decl,
            Result = (>)
        ;
            ImportOrUseA = use_decl,
            ImportOrUseB = import_decl,
            Result = (>)
        ;
            ( ImportOrUseA = import_decl, ImportOrUseB = import_decl
            ; ImportOrUseA = use_decl, ImportOrUseB = use_decl
            ),
            compare(Result, ContextA, ContextB)
        )
    ).

:- pred maybe_generate_redundant_avail_warnings(module_name::in,
    list(avail_module)::in, list(avail_module)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_generate_redundant_avail_warnings(_ModuleName, [], _, !Specs).
maybe_generate_redundant_avail_warnings(ModuleName, [Avail | Avails],
        !.PrevAvails, !Specs) :-
    % XXX UNUSED_IMPORT Harmonize the operation of this predicate with
    % the operation of warn_unused_interface_import
    % in module_qual.qual_errors.m.
    list.foldl(add_msg_if_avail_as_general(ModuleName, Avail), !.PrevAvails,
        [], PrevMsgs),
    (
        PrevMsgs = [],
        % O(n^2), but the usual value of n is *very* small.
        !:PrevAvails = !.PrevAvails ++ [Avail]
    ;
        PrevMsgs = [_ | _],
        Avail = avail_module(_Section, ImportOrUse, Context),
        DeclName = import_or_use_decl_name(ImportOrUse),
        MainPieces = [words("This"), decl(DeclName), words("declaration"),
            words("for"), qual_sym_name(ModuleName),
            words("is redundant."), nl],
        MainMsg = simple_msg(Context, [always(MainPieces)]),
        Spec = error_spec(severity_informational, phase_code_gen,
            [MainMsg | PrevMsgs]),
        !:Specs = [Spec | !.Specs]
    ),
    maybe_generate_redundant_avail_warnings(ModuleName, Avails,
        !.PrevAvails, !Specs).

    % add_msg_if_avail_as_general(ModuleName, ThisAvail, PrevAvail, !Msgs):
    %
    % If PrevEntry is at least as general as ThisEntry, add a message
    % about PrevEntry being a previous import or use declaration
    % for ModuleName to !Msgs.
    %
:- pred add_msg_if_avail_as_general(module_name::in,
    avail_module::in, avail_module::in,
    list(error_msg)::in, list(error_msg)::out) is det.

add_msg_if_avail_as_general(ModuleName, ThisAvail, PrevAvail, !Msgs) :-
    ThisAvail = avail_module(ThisSection, ThisImportOrUse, _ThisContext),
    PrevAvail = avail_module(PrevSection, PrevImportOrUse, PrevContext),
    ( if
        (
            % Does this entry grant extra permissions about where ModuleName
            % may be used?
            PrevSection = ms_implementation,
            ThisSection = ms_interface
        ;
            % Does this entry grant extra permissions about the use of
            % ModuleName without explicit qualification?
            PrevImportOrUse = use_decl,
            ThisImportOrUse = import_decl
        )
    then
        true
    else
        DeclName = import_or_use_decl_name(PrevImportOrUse),
        Pieces = [words("This is the location of the previous"),
            decl(DeclName), words("declaration"),
            words("for module"), qual_sym_name(ModuleName),
            words("that makes this one redundant."), nl],
        Msg = simple_msg(PrevContext, [always(Pieces)]),
        !:Msgs = [Msg | !.Msgs]
    ).

:- type anywhere_or_interface
    --->    aoi_anywhere
    ;       aoi_interface.

:- func generate_unused_warning(module_name, module_name, import_or_use,
    prog_context, anywhere_or_interface) = error_spec.

generate_unused_warning(TopModuleName, UnusedModuleName, ImportOrUse,
        Context, AnywhereOrInterface) = Spec :-
    (
        AnywhereOrInterface = aoi_anywhere,
        DeclInTheLocn = "",
        NotUsedInTheLocn = "anywhere in the module"
    ;
        AnywhereOrInterface = aoi_interface,
        DeclInTheLocn = "in the interface",
        NotUsedInTheLocn = "in the interface"
    ),
    ImportOrUseDeclName = import_or_use_decl_name(ImportOrUse),
    Pieces = [words("In module" ), qual_sym_name(TopModuleName),
        suffix(":"), nl,
        words("warning: module"), qual_sym_name(UnusedModuleName),
        words("has a"), decl(ImportOrUseDeclName),
        words("declaration"), words(DeclInTheLocn), suffix(","),
        words("but is not used"), words(NotUsedInTheLocn), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_code_gen, [Msg]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Scan each entity in the module_info, and record the module defining
    % that entity as being used in the appropriate section of the current
    % module (interface or implementation).
    %
    % Also include in the returned set the modules that we shouldn't
    % generate unused module warnings about because the user cannot
    % eliminate their import.
    %
:- pred find_all_non_warn_modules(module_info::in, used_modules::out) is det.

find_all_non_warn_modules(ModuleInfo, !:UsedModules) :-
    % Each parent module of the current module has imports that are
    % inherited by this module, so we have to add the used modules
    % of the parents to the set of used modules, as an import in the parent
    % may only be consumed by the parent.
    module_info_get_used_modules(ModuleInfo, !:UsedModules),
    UsedModulesInit = !.UsedModules,

    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(type_used_modules, TypeTable, !UsedModules),
    UsedModulesTypeCtor = !.UsedModules,

    module_info_get_inst_table(ModuleInfo, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    map.foldl(user_inst_used_modules, UserInstTable, !UsedModules),
    UsedModulesUserInst = !.UsedModules,

    module_info_get_mode_table(ModuleInfo, ModeTable),
    mode_table_get_mode_defns(ModeTable, ModeDefns),
    map.foldl(mode_used_modules, ModeDefns, !UsedModules),
    UsedModulesMode = !.UsedModules,

    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    list.foldl(const_struct_used_modules, ConstStructs, !UsedModules),
    UsedModulesConstStruct = !.UsedModules,

    module_info_get_preds(ModuleInfo, PredTable),
    map.foldl(pred_info_used_modules, PredTable, !UsedModules),
    UsedModulesPredInfo = !.UsedModules,

    module_info_get_class_table(ModuleInfo, ClassTable),
    map.foldl(class_used_modules, ClassTable, !UsedModules),
    UsedModulesClass = !.UsedModules,

    module_info_get_name(ModuleInfo, ThisModuleName),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.foldl(class_instances_used_modules(ThisModuleName),
        InstanceTable, !UsedModules),
    UsedModulesInstance = !.UsedModules,

    % We do not want to generate unused module warnings for implicitly
    % imported modules, since the user cannot prevent their imports.
    % We therefore include them as "used" modules.
    % XXX We can do better in two different ways.
    % (1) Instead of considering *all* builtin modules to be used,
    % we could record the set of modules that we *actually* implicitly import,
    % and add only *those* to !UsedModules.
    % (2) While the usual message about the module being unused is not
    % appropriate for implicitly imported modules, we may want to report
    % a warning if any module that the compiler imports implicitly
    % is *also* imported *explicitly* by the user.
    ImplicitImports = all_builtin_modules,
    list.foldl(record_module_and_ancestors_as_used(visibility_public),
        ImplicitImports, !UsedModules),
    UsedModulesBuiltin = !.UsedModules,

    UsedModulesHistory = [
        "initial" - UsedModulesInit,
        "type_ctor_defns" - UsedModulesTypeCtor,
        "user_insts" - UsedModulesUserInst,
        "modes" - UsedModulesMode,
        "pred_infos" - UsedModulesPredInfo,
        "const_structs" - UsedModulesConstStruct,
        "classes" - UsedModulesClass,
        "instances" - UsedModulesInstance,
        "builtin" - UsedModulesBuiltin
    ],

    trace [compile_time(flag("dump_used_modules_history")), io(!IO)] (
        dump_used_modules_history(set.init, set.init, UsedModulesHistory, !IO)
    ).

:- pred dump_used_modules_history(set(module_name)::in, set(module_name)::in,
    assoc_list(string, used_modules)::in, io::di, io::uo) is det.

dump_used_modules_history(_, _, [], !IO).
dump_used_modules_history(!.IntUsed, !.ImpUsed, [Head | Tail], !IO) :-
    Head = HeadStr - used_modules(HeadInt, HeadImp),
    set.difference(HeadInt, !.IntUsed, NewHeadInt),
    set.difference(HeadImp, !.ImpUsed, NewHeadImp),
    io.format("modules added at stage %s\n", [s(HeadStr)], !IO),
    ( if set.is_empty(NewHeadInt) then
        true
    else
        io.write_string("interface:\n", !IO),
        set.to_sorted_list(NewHeadInt, NewHeadIntList),
        io.write(NewHeadIntList, !IO),
        io.nl(!IO)
    ),
    ( if set.is_empty(NewHeadImp) then
        true
    else
        io.write_string("implementation:\n", !IO),
        set.to_sorted_list(NewHeadImp, NewHeadImpList),
        io.write(NewHeadImpList, !IO),
        io.nl(!IO)
    ),

    set.union(HeadInt, !IntUsed),
    set.union(HeadImp, !ImpUsed),
    dump_used_modules_history(!.IntUsed, !.ImpUsed, Tail, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred type_used_modules(type_ctor::in, hlds_type_defn::in,
    used_modules::in, used_modules::out) is det.

type_used_modules(_TypeCtor, TypeDefn, !UsedModules) :-
    get_type_defn_status(TypeDefn, TypeStatus),
    get_type_defn_body(TypeDefn, TypeBody),

    DefinedInThisModule = type_status_defined_in_this_module(TypeStatus),
    (
        DefinedInThisModule = yes,
        Visibility = type_visibility(TypeStatus),
        (
            TypeBody = hlds_du_type(Ctors, _, _, _),
            list.foldl(ctor_used_modules(Visibility), Ctors, !UsedModules)
        ;
            TypeBody = hlds_eqv_type(EqvType),
            mer_type_used_modules(Visibility, EqvType, !UsedModules)
        ;
            ( TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            )
        )
    ;
        DefinedInThisModule = no
    ).

:- pred ctor_used_modules(item_visibility::in, constructor::in,
    used_modules::in, used_modules::out) is det.

ctor_used_modules(Visibility, Ctor, !UsedModules) :-
    Ctor = ctor(_, MaybeExistConstraints, _, Args, _, _),
    (
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(_, Constraints, _, _),
        list.foldl(prog_constraint_used_modules(Visibility), Constraints,
            !UsedModules)
    ),
    list.foldl(
        ( pred(Arg::in, !.M::in, !:M::out) is det :-
            mer_type_used_modules(Visibility, Arg ^ arg_type, !M)
        ), Args, !UsedModules).

:- pred prog_constraint_used_modules(item_visibility::in, prog_constraint::in,
    used_modules::in, used_modules::out) is det.

prog_constraint_used_modules(Visibility, Constraint, !UsedModules) :-
    Constraint = constraint(ClassName, ArgTypes),
    record_sym_name_module_as_used(Visibility, ClassName, !UsedModules),
    list.foldl(mer_type_used_modules(Visibility), ArgTypes, !UsedModules).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred user_inst_used_modules(inst_id::in, hlds_inst_defn::in,
    used_modules::in, used_modules::out) is det.

user_inst_used_modules(_InstId, InstDefn, !UsedModules) :-
    InstStatus = InstDefn ^ inst_status,
    DefinedInThisModule = inst_status_defined_in_this_module(InstStatus),
    (
        DefinedInThisModule = yes,
        Visibility = inst_visibility(InstStatus),
        InstDefn ^ inst_body = eqv_inst(Inst),
        mer_inst_used_modules(Visibility, Inst, !UsedModules)
    ;
        DefinedInThisModule = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mode_used_modules(mode_id::in, hlds_mode_defn::in,
    used_modules::in, used_modules::out) is det.

mode_used_modules(mode_id(Name, _Arity), ModeDefn, !UsedModules) :-
    ModeStatus = ModeDefn ^ mode_status,
    DefinedInThisModule = mode_status_defined_in_this_module(ModeStatus),
    (
        DefinedInThisModule = yes,
        Visibility = mode_visibility(ModeStatus),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        ModeDefn ^ mody_body = hlds_mode_body(Mode),
        mer_mode_used_modules(Visibility, Mode, !UsedModules)
    ;
        DefinedInThisModule = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred class_used_modules(class_id::in, hlds_class_defn::in,
    used_modules::in, used_modules::out) is det.

class_used_modules(class_id(Name, _Arity), ClassDefn, !UsedModules) :-
    TypeClassStatus = ClassDefn ^ classdefn_status,
    DefinedInThisModule =
        typeclass_status_defined_in_this_module(TypeClassStatus),
    (
        DefinedInThisModule = yes,
        Visibility = typeclass_visibility(TypeClassStatus),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        list.foldl(prog_constraint_used_modules(Visibility),
            ClassDefn ^ classdefn_supers, !UsedModules)
    ;
        DefinedInThisModule = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred class_instances_used_modules(module_name::in,
    class_id::in, list(hlds_instance_defn)::in,
    used_modules::in, used_modules::out) is det.

class_instances_used_modules(ThisModuleName,
        ClassId, InstanceDefns, !UsedModules) :-
    list.foldl(instance_used_modules(ThisModuleName, ClassId),
        InstanceDefns, !UsedModules).

:- pred instance_used_modules(module_name::in,
    class_id::in, hlds_instance_defn::in,
    used_modules::in, used_modules::out) is det.

instance_used_modules(ThisModuleName, ClassId, InstanceDefn, !UsedModules) :-
    ClassId = class_id(ClassName, ClassArity),
    InstanceDefn = hlds_instance_defn(InstanceModuleName,
        Types, _OriginalTypes, InstanceStatus, _Context, Constraints, _Body,
        _MaybePredProcIds, _VarSet, _ProofMap),

    trace [compile_time(flag("dump_used_modules_history")), io(!IO)] (
        io.format("instance_used_modules: class id %s/%d, instance in %s\n",
            [s(sym_name_to_string(ClassName)), i(ClassArity),
            s(sym_name_to_string(InstanceModuleName))], !IO)
    ),

    ( if ThisModuleName = InstanceModuleName then
        Visibility = instance_visibility(InstanceStatus),
        record_sym_name_module_as_used(Visibility, ClassName, !UsedModules),
        list.foldl(mer_type_used_modules(Visibility), Types, !UsedModules),

        list.foldl(prog_constraint_used_modules(Visibility), Constraints,
            !UsedModules)

        % The methods of the class are stored in the pred_table,
        % and thus should have been processed by pred_info_used_modules.
    else
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred const_struct_used_modules(pair(int, const_struct)::in,
    used_modules::in, used_modules::out) is det.

const_struct_used_modules(_ConstNum - ConstStruct, !UsedModules) :-
    % Every const_struct in the const_struct_db was put there because
    % it is used in module. None of the uses can be in the interface.

    ConstStruct = const_struct(ConsId, ConstStructArgs, Type, Inst),
    cons_id_used_modules(visibility_private, ConsId, !UsedModules),
    list.foldl(const_struct_arg_used_modules, ConstStructArgs, !UsedModules),
    mer_type_used_modules(visibility_private, Type, !UsedModules),
    mer_inst_used_modules(visibility_private, Inst, !UsedModules).

:- pred const_struct_arg_used_modules(const_struct_arg::in,
    used_modules::in, used_modules::out) is det.

const_struct_arg_used_modules(ConstStructArg, !UsedModules) :-
    (
        ConstStructArg = csa_const_struct(_ConstNum)
    ;
        ConstStructArg = csa_constant(ConsId, Type),
        cons_id_used_modules(visibility_private, ConsId, !UsedModules),
        mer_type_used_modules(visibility_private, Type, !UsedModules)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pred_info_used_modules(pred_id::in, pred_info::in,
    used_modules::in, used_modules::out) is det.

pred_info_used_modules(PredId, PredInfo, !UsedModules) :-
    pred_info_get_status(PredInfo, PredStatus),
    DefinedInThisModule = pred_status_defined_in_this_module(PredStatus),
    (
        DefinedInThisModule = yes,

        trace [compile_time(flag("dump_used_modules_history")), io(!IO)] (
            io.format("examining pred_info of pred_id %d\n",
                [i(pred_id_to_int(PredId))], !IO)
        ),

        Visibility = pred_visibility(PredStatus),

        pred_info_get_class_context(PredInfo, Constraints),
        Constraints = constraints(UnivConstraints, ExistConstraints),
        list.foldl(prog_constraint_used_modules(Visibility),
            UnivConstraints, !UsedModules),
        list.foldl(prog_constraint_used_modules(Visibility),
            ExistConstraints, !UsedModules),

        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl(proc_info_used_modules(Visibility), ProcTable, !UsedModules),

        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_used_modules(ClausesInfo, !UsedModules)
    ;
        DefinedInThisModule = no,

        trace [compile_time(flag("dump_used_modules_history")), io(!IO)] (
            io.format("NOT examining pred_info of pred_id %d\n",
                [i(pred_id_to_int(PredId))], !IO)
        )
    ).

:- pred proc_info_used_modules(item_visibility::in, proc_id::in, proc_info::in,
    used_modules::in, used_modules::out) is det.

proc_info_used_modules(Visibility, _ProcId, ProcInfo, !UsedModules) :-
    % In some rare cases, the type of a variable can refer to a module
    % that is used nowhere else in the module, not even in the types of
    % the arguments of the procedure. The instance method predicate
    % for the function method named "analyses" defined in mmc_analysis.m
    % demonstrates such a case. In this case, the function returns a value
    % of an tuple type with existentially-typed slots, and some of the
    % clauses of the function fill in the existentially-typed slots
    % in the tuple with references to types that are defined in modules
    % that not used anywhere else in mmc_analysis.m.
    %
    % Looking through the types of all the variables is significantly slower
    % than looking through just the types of the arguments, but the above
    % shows that it is needed for correctness. Looking through types
    % in all the proc_infos in a pred_info should not be a problem,
    % given that the average number of proc_infos per pred_info typically
    % hovers in the 1.01-to-1.2 range.
    proc_info_get_vartypes(ProcInfo, VarTypes),
    foldl_var_types(mer_type_used_modules(Visibility), VarTypes,
        !UsedModules),

    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(Modes),
        list.foldl(mer_mode_used_modules(Visibility), Modes, !UsedModules)
    ;
        MaybeArgModes = no
    ).

:- pred clauses_info_used_modules(clauses_info::in,
    used_modules::in, used_modules::out) is det.

clauses_info_used_modules(ClausesInfo, !UsedModules) :-
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    list.foldl(clause_used_modules, Clauses, !UsedModules).

:- pred clause_used_modules(clause::in,
    used_modules::in, used_modules::out) is det.

clause_used_modules(Clause, !UsedModules) :-
    hlds_goal_used_modules(Clause ^ clause_body, !UsedModules).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred hlds_goal_used_modules(hlds_goal::in,
    used_modules::in, used_modules::out) is det.

hlds_goal_used_modules(Goal, !UsedModules) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = unify(_, RHS, _, _, _),
        unify_rhs_used_modules(RHS, !UsedModules)
    ;
        GoalExpr = plain_call(_, _, _, _, _, SymName),
        record_sym_name_module_as_used(visibility_private, SymName,
            !UsedModules),
        Name = unqualify_name(SymName),
        ( if Name = "format" then
            record_format_modules_as_used(!UsedModules)
        else
            true
        )
    ;
        GoalExpr = generic_call(Call, _, _, _, _),
        (
            Call = class_method(_, _, ClassId, CallId),
            ClassId = class_id(ClassName, _),
            CallId = simple_call_id(_, MethodName, _),
            record_sym_name_module_as_used(visibility_private, ClassName,
                !UsedModules),
            record_sym_name_module_as_used(visibility_private, MethodName,
                !UsedModules)
        ;
            ( Call = higher_order(_, _, _, _)
            ; Call = event_call(_)
            ; Call = cast(_)
            )
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        list.foldl(hlds_goal_used_modules, Goals, !UsedModules)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(case_used_modules, Cases, !UsedModules)
    ;
        % Even for from_ground_term_construct scopes, we need to check
        % which modules are referenced by the cons_ids inside.
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_, SubGoal)
        ),
        hlds_goal_used_modules(SubGoal, !UsedModules)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        hlds_goal_used_modules(Cond, !UsedModules),
        hlds_goal_used_modules(Then, !UsedModules),
        hlds_goal_used_modules(Else, !UsedModules)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = bi_implication(GoalA, GoalB),
            hlds_goal_used_modules(GoalA, !UsedModules),
            hlds_goal_used_modules(GoalB, !UsedModules)
        ;
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            hlds_goal_used_modules(MainGoal, !UsedModules),
            list.foldl(hlds_goal_used_modules, OrElseGoals, !UsedModules)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            hlds_goal_used_modules(SubGoal, !UsedModules)
        )
    ).

:- pred case_used_modules(case::in, used_modules::in, used_modules::out)
    is det.

case_used_modules(Case, !UsedModules) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    cons_id_used_modules(visibility_private, MainConsId, !UsedModules),
    list.foldl(cons_id_used_modules(visibility_private), OtherConsIds,
        !UsedModules),
    hlds_goal_used_modules(Goal, !UsedModules).

:- pred unify_rhs_used_modules(unify_rhs::in,
    used_modules::in, used_modules::out) is det.

unify_rhs_used_modules(RHS, !UsedModules) :-
    (
        RHS = rhs_var(_)
    ;
        RHS = rhs_functor(ConsId, _, _),
        cons_id_used_modules(visibility_private, ConsId, !UsedModules)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal),
        hlds_goal_used_modules(Goal, !UsedModules)
    ).

:- pred cons_id_used_modules(item_visibility::in, cons_id::in,
    used_modules::in, used_modules::out) is det.

cons_id_used_modules(Visibility, ConsId, !UsedModules) :-
    (
        ( ConsId = cons(SymName, _, _)
        ; ConsId = type_info_cell_constructor(type_ctor(SymName, _))
        ),
        record_sym_name_module_as_used(Visibility, SymName, !UsedModules)
    ;
        ( ConsId = type_ctor_info_const(ModuleName, _, _)
        ; ConsId = base_typeclass_info_const(ModuleName, _, _, _)
        ),
        record_module_and_ancestors_as_used(Visibility, ModuleName,
            !UsedModules)
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
        ; ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = int64_const(_)
        ; ConsId = uint64_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mer_type_used_modules(item_visibility::in, mer_type::in,
    used_modules::in, used_modules::out) is det.

mer_type_used_modules(Visibility, Type, !UsedModules) :-
    (
        Type = type_variable(_, _)
    ;
        Type = defined_type(Name, Args, _),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        list.foldl(mer_type_used_modules(Visibility), Args, !UsedModules)
    ;
        Type = builtin_type(BuiltinType),
        (
            ( BuiltinType = builtin_type_int(_)
            ; BuiltinType = builtin_type_float
            ; BuiltinType = builtin_type_string
            )
            % You don't need to import int.m, float.m or string.m to use these.
        ;
            BuiltinType = builtin_type_char,
            % You *do* need to import char.m to use these.
            CharModuleName = mercury_std_lib_module_name(unqualified("char")),
            record_module_and_ancestors_as_used(Visibility, CharModuleName,
                !UsedModules)
        )
    ;
        Type = higher_order_type(_, Args, HOInstInfo, _, _),
        list.foldl(mer_type_used_modules(Visibility), Args, !UsedModules),
        ho_inst_info_used_modules(Visibility, HOInstInfo, !UsedModules)
    ;
        Type = tuple_type(ArgTypes, _),
        list.foldl(mer_type_used_modules(Visibility), ArgTypes, !UsedModules)
    ;
        Type = apply_n_type(_, ArgTypes, _),
        list.foldl(mer_type_used_modules(Visibility), ArgTypes, !UsedModules)
    ;
        Type = kinded_type(ArgType, _),
        mer_type_used_modules(Visibility, ArgType, !UsedModules)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mer_mode_used_modules(item_visibility::in, mer_mode::in,
    used_modules::in, used_modules::out) is det.

mer_mode_used_modules(Visibility, Mode, !UsedModules) :-
    (
        Mode = from_to_mode(Inst0, Inst),
        mer_inst_used_modules(Visibility, Inst0, !UsedModules),
        mer_inst_used_modules(Visibility, Inst, !UsedModules)
    ;
        Mode = user_defined_mode(Name, Insts),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        list.foldl(mer_inst_used_modules(Visibility), Insts, !UsedModules)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mer_inst_used_modules(item_visibility::in, mer_inst::in,
    used_modules::in, used_modules::out) is det.

mer_inst_used_modules(Visibility, Inst, !UsedModules) :-
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = inst_var(_)
        )
    ;
        ( Inst = ground(_, HOInstInfo)
        ; Inst = any(_, HOInstInfo)
        ),
        ho_inst_info_used_modules(Visibility, HOInstInfo, !UsedModules)
    ;
        Inst = free(Type),
        mer_type_used_modules(Visibility, Type, !UsedModules)
    ;
        Inst = bound(_, _InstResults, BoundInsts),
        % Anything appearing in InstResults should also appear in BoundInsts.
        list.foldl(bound_inst_info_used_modules(Visibility), BoundInsts,
            !UsedModules)
    ;
        Inst = constrained_inst_vars(_InstVars, SubInst),
        mer_inst_used_modules(Visibility, SubInst, !UsedModules)
    ;
        Inst = defined_inst(InstName),
        inst_name_used_modules(Visibility, InstName, !UsedModules)
    ;
        Inst = abstract_inst(Name, ArgInsts),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        list.foldl(mer_inst_used_modules(Visibility), ArgInsts, !UsedModules)
    ).

%-----------------------------------------------------------------------------%

:- pred bound_inst_info_used_modules(item_visibility::in, bound_inst::in,
    used_modules::in, used_modules::out) is det.

bound_inst_info_used_modules(Visibility, BoundFunctor, !UsedModules) :-
    BoundFunctor = bound_functor(ConsId, Insts),
    cons_id_used_modules(Visibility, ConsId, !UsedModules),
    list.foldl(mer_inst_used_modules(Visibility), Insts, !UsedModules).

:- pred ho_inst_info_used_modules(item_visibility::in,
    ho_inst_info::in, used_modules::in, used_modules::out) is det.

ho_inst_info_used_modules(Visibility, HOInstInfo, !UsedModules) :-
    (
        HOInstInfo = higher_order(pred_inst_info(_, Modes, _, _)),
        list.foldl(mer_mode_used_modules(Visibility), Modes, !UsedModules)
    ;
        HOInstInfo = none_or_default_func
    ).

%-----------------------------------------------------------------------------%

:- pred inst_name_used_modules(item_visibility::in, inst_name::in,
    used_modules::in, used_modules::out) is det.

inst_name_used_modules(Visibility, InstName, !UsedModules) :-
    (
        InstName = user_inst(Name, Insts),
        record_sym_name_module_as_used(Visibility, Name, !UsedModules),
        list.foldl(mer_inst_used_modules(Visibility), Insts, !UsedModules)
    ;
        ( InstName = merge_inst(InstA, InstB)
        ; InstName = unify_inst(_, _, InstA, InstB)
        ),
        mer_inst_used_modules(Visibility, InstA, !UsedModules),
        mer_inst_used_modules(Visibility, InstB, !UsedModules)
    ;
        ( InstName = ground_inst(SubInstName, _, _, _)
        ; InstName = any_inst(SubInstName, _, _, _)
        ; InstName = shared_inst(SubInstName)
        ; InstName = mostly_uniq_inst(SubInstName)
        ),
        inst_name_used_modules(Visibility, SubInstName, !UsedModules)
    ;
        InstName = typed_ground(_, Type),
        mer_type_used_modules(Visibility, Type, !UsedModules)
    ;
        InstName = typed_inst(Type, SubInstName),
        mer_type_used_modules(Visibility, Type, !UsedModules),
        inst_name_used_modules(Visibility, SubInstName, !UsedModules)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Determine if the given import_status implies item is visibility_public
% (in the interface) or visibility_private (in the implementation).
%

:- func type_visibility(type_status) = item_visibility.

type_visibility(TypeStatus) = Visibility :-
    Exported = type_status_is_exported_to_non_submodules(TypeStatus),
    Visibility = exported_to_visibility(Exported).

:- func inst_visibility(inst_status) = item_visibility.

inst_visibility(InstStatus) = Visibility :-
    Exported = inst_status_is_exported_to_non_submodules(InstStatus),
    Visibility = exported_to_visibility(Exported).

:- func mode_visibility(mode_status) = item_visibility.

mode_visibility(ModeStatus) = Visibility :-
    Exported = mode_status_is_exported_to_non_submodules(ModeStatus),
    Visibility = exported_to_visibility(Exported).

:- func typeclass_visibility(typeclass_status) = item_visibility.

typeclass_visibility(TypeClassStatus) = Visibility :-
    Exported = typeclass_status_is_exported_to_non_submodules(TypeClassStatus),
    Visibility = exported_to_visibility(Exported).

:- func instance_visibility(instance_status) = item_visibility.

instance_visibility(InstanceStatus) = Visibility :-
    Exported = instance_status_is_exported_to_non_submodules(InstanceStatus),
    Visibility = exported_to_visibility(Exported).

:- func pred_visibility(pred_status) = item_visibility.

pred_visibility(PredStatus) = Visibility :-
    Exported = pred_status_is_exported_to_non_submodules(PredStatus),
    Visibility = exported_to_visibility(Exported).

:- func exported_to_visibility(bool) = item_visibility.

exported_to_visibility(Exported) = Visibility :-
    (
        Exported = yes,
        Visibility = visibility_public
    ;
        Exported = no,
        Visibility = visibility_private
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.unused_imports.
%-----------------------------------------------------------------------------%
