%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module creates the initial HLDS from the augmented parse tree
% of a module.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.

:- import_module list.

%---------------------------------------------------------------------------%

    % do_parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo,
    %   TypeEqvMap, UsedModules, QualInfo, InvalidTypes, InvalidModes,
    %   HLDS, Specs):
    %
    % Given MQInfo (returned by module_qual.m) and TypeEqvMap and UsedModules
    % (both returned by equiv_type.m), convert AugCompUnit to HLDS.
    % Return any errors found in Specs.
    % Return InvalidTypes = yes if we found undefined types.
    % Return InvalidModes = yes if we found undefined or cyclic insts or modes.
    % QualInfo is an abstract type that check_typeclass.m will later pass
    % to produce_instance_method_clauses.
    %
:- pred do_parse_tree_to_hlds(aug_compilation_unit::in, globals::in,
    string::in, mq_info::in, type_eqv_map::in, used_modules::in,
    qual_info::out, found_invalid_type::out, found_invalid_inst_or_mode::out,
    module_info::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.

:- import_module hlds.add_pred.
:- import_module hlds.add_special_pred.
:- import_module hlds.default_func_mode.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_mode.
:- import_module hlds.make_hlds.add_mutable_aux_preds.
:- import_module hlds.make_hlds.add_pragma.
:- import_module hlds.make_hlds.add_solver.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item_stats.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module bool.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

do_parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo0,
        TypeEqvMap, UsedModules, !:QualInfo,
        !:FoundInvalidType, !:FoundInvalidInstOrMode, !:ModuleInfo, !:Specs) :-
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleNameContext = ParseTreeModuleSrc ^ ptms_module_name_context,
    get_implicit_avail_needs_in_aug_compilation_unit(Globals, AugCompUnit,
        ImplicitlyUsedModules),
    mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
    module_info_init(Globals, ModuleName, ModuleNameContext, DumpBaseFileName,
        UsedModules, ImplicitlyUsedModules, PQInfo, no, !:ModuleInfo),

    % Optionally gather statistics about the items in the compilation unit.
    trace [compile_time(flag("item_stats")), io(!IO)] (
        % We *append* statistics to a file, rather than simply *writing*
        % them to a file, so that we can gather statistics from a sequence of
        % Mercury compiler invocations, such as those in a bootcheck.
        % The file should be created empty before the start of the sequence,
        % and should be appended to by one Mercury compiler invocation
        % at a time. (We don't do any locking, so if more than one invocation
        % tries to append to the file at the same time, the resulting output
        % will be malformed.)
        %
        % You may of course change the name of the file, if you wish.
        %
        % A statistics file resulting from one or more compiler invocations
        % may be analyzed using tools/item_stats.

        io.open_append("/tmp/ITEM_STATS", Res, !IO),
        (
            Res = error(_)
        ;
            Res = ok(Stream),
            gather_and_write_item_stats(Stream, AugCompUnit, !IO),
            io.close_output(Stream, !IO)
        )
    ),

    map.init(DirectArgMap),
    TypeRepnDec = type_repn_decision_data(TypeRepnMap, DirectArgMap,
        ForeignEnums, ForeignExportEnums),
    module_info_set_type_repn_dec(TypeRepnDec, !ModuleInfo),

    TypeSpecs = ParseTreeModuleSrc ^ ptms_type_specs,
    InstModeSpecs = ParseTreeModuleSrc ^ ptms_inst_mode_specs,
    !:Specs = TypeSpecs ++ InstModeSpecs,

    IsInvalidTypeSpec =
        ( pred(Spec::in) is semidet :-
            extract_spec_phase(Spec, Phase),
            Phase = phase_type_inst_mode_check_invalid_type
        ),
    IsInvalidInstModeSpec =
        ( pred(Spec::in) is semidet :-
            extract_spec_phase(Spec, Phase),
            Phase = phase_type_inst_mode_check_invalid_inst_mode
        ),
    list.filter(IsInvalidTypeSpec, TypeSpecs, InvalidTypeSpecs),
    list.filter(IsInvalidInstModeSpec, InstModeSpecs, InvalidInstModeSpecs),
    TypeErrors = contains_errors(Globals, InvalidTypeSpecs),
    InstModeErrors = contains_errors(Globals, InvalidInstModeSpecs),
    (
        TypeErrors = no,
        !:FoundInvalidType = did_not_find_invalid_type
    ;
        TypeErrors = yes,
        !:FoundInvalidType = found_invalid_type
    ),
    (
        InstModeErrors = no,
        !:FoundInvalidInstOrMode = did_not_find_invalid_inst_or_mode
    ;
        InstModeErrors = yes,
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ),

    % We used to add items to the HLDS in three passes.
    % Roughly,
    %
    % - pass 1 added type, inst and mode definitions and predicate
    %   declarations to the module,
    % - pass 3 added the definitions of predicates, including not just
    %   clauses but also clause-like pragmas such as foreign_procs,
    %   to the module, while
    % - pass 2 did the tasks that had to be done before pass 3 started
    %   but which needed access to *all* the declarations added by pass 1,
    %   not just the ones processed so far.
    %
    % We now add items to the HLDS by item kind: first one kind of item,
    % then another, then another. The general order remains roughly the same
    % as before, but we now also impose an order *within* what used to be
    % a pass. For example, we add all type definitions before we add
    % any inst definitions, since inst definitions may now refer to type
    % constructors.
    %
    % The constraints of what we have to add before what are documented below.

    separate_items_in_aug_comp_unit(AugCompUnit, Avails, FIMs,
        TypeDefnsAbstract, TypeDefnsMercury, TypeDefnsForeign,
        InstDefns, ModeDefns, PredDecls, ModeDecls,
        Promises, Typeclasses, Instances, Initialises, Finalises, Mutables,
        TypeRepnMap, ForeignEnums, ForeignExportEnums,
        PragmasDecl, PragmasDeclTypeSpec,
        PragmasDeclTermInfo, PragmasDeclTerm2Info,
        PragmasDeclSharing, PragmasDeclReuse,
        PragmasImpl,
        PragmasGenUnusedArgs, PragmasGenExceptions,
        PragmasGenTrailing, PragmasGenMMTabling,
        Clauses, IntBadClauses),

    % The old pass 1.

    % Record the import_module and use_module declarations.
    add_item_avails(Avails, !ModuleInfo),

    % Record type definitions.
    %
    % We first add the abstract type definitions, then the nonabstract ones,
    % since this should simplify the processing of type constructors that
    % have both abstract and nonabstract definitions. Among the nonabstract
    % type definitions, we process the ones that give Mercury definitions
    % before the ones that give foreign language definitions, since the code
    % that adds foreign language definitions can give the right error message
    % only in that case.
    %
    % The definition of a solver type requires the declaration and the
    % definition of several auxiliary predicates, for representation changes
    % and maybe for initialization. When add_type_defn defines a solver type,
    % it returns the declarations of the auxiliary predicates that implement
    % that solver type, and if the solver type is defined in the current
    % module, then they also return the foreign procs and mutables that
    % implement those auxiliary predicates. We add these items to the HLDS
    % later, at the time when we add items of the same kinds to the HLDS.
    % (The mutables implement the constraint store.)
    some [!RevSolverPredDecls, !RevSolverForeignProcs, !RevSolverMutables]
    (
        !:RevSolverPredDecls = [],
        !:RevSolverForeignProcs = [],
        !:RevSolverMutables = [],
        % XXX TYPE_REPN
        % The XXX comments below are irrelevant if
        % separate_items_in_aug_comp_unit returns a type_ctor_checked_map.
        % XXX TYPE_REPN
        % - group parse_tree_{du,eqv,solver}_type together in their type;
        % - have separate_items_in_aug_comp_unit return the definitions of
        %   (a) abstract, (b) du/eqv/solver and (c) foreign types in lists
        %   whose elements are of different, specialized types; and
        % - call specialized versions of add_type_defn in each foldl5 below.
        % XXX TYPE_REPN Consider treating du/eqv/solver type defns separately,
        % but beware: the current code of maybe_make_abstract_type_defn,
        % while usually returning abstract types, sometimes returns Mercury
        % types.
        add_type_defns(TypeDefnsAbstract,
            !ModuleInfo, !FoundInvalidType, !Specs, !RevSolverPredDecls,
            !RevSolverForeignProcs, !RevSolverMutables),
        add_type_defns(TypeDefnsMercury,
            !ModuleInfo, !FoundInvalidType, !Specs, !RevSolverPredDecls,
            !RevSolverForeignProcs, !RevSolverMutables),
        add_type_defns(TypeDefnsForeign,
            !ModuleInfo, !FoundInvalidType, !Specs, !RevSolverPredDecls,
            !RevSolverForeignProcs, !RevSolverMutables),
        % The code of add_type_defns adds new entries to the *fronts* of
        % !RevSolverPredDecls, !RevSolverForeignProcs and !RevSolverMutables,
        % because doing so avoids quadratic behavior. However, this means
        % that e.g. a pred decl for a solver type from a later type defn
        % could come before a pred decl from an earlier type defn.
        % This is a problem if the two type definitions are actually the same,
        % the first copy being from a module's .intN file and the second copy
        % being from the module's .opt file. The reason why this is a problem
        % is that we suppress error messages about duplicate pred decls
        % only if the duplicate (i.e. the copy we see second) is from
        % a .opt file.
        %
        % The make_hlds_separate_items.m gives us lists of type defns
        % in which type_defns from .intN files will always precede
        % type_defns from .opt files. To ensure that this property also holds
        % for the pred decls of any auxiliary predicates needed for the
        % solver types among them, we must restore the original order.
        list.reverse(!.RevSolverPredDecls, SolverPredDecls),
        list.reverse(!.RevSolverForeignProcs, SolverForeignProcs),
        list.reverse(!.RevSolverMutables, SolverMutables)
    ),

    % We process inst definitions after all type definitions because
    % the processing of a type-specific inst may require access not just
    % to the definition of *that* type, but of any other type that occurs
    % in its definition, either directly or indirectly.
    %
    % Checking insts for circularity cannot be guaranteed to catch all
    % circularities until all insts have been added to the HLDS.
    % For example, in tests/invalid/circ_inst5.m, the circularity between
    % the inst_ctors c/0 and c/1 is detected only if the definition of
    % the inst_ctor i/1 has already been added to the inst table, but
    % i/1 does not refer to either c/0 or c1, and is not itself circular,
    % so its later addition cannot be expected to catch the circularity
    % between c/0 and c/1.
    %
    % We definitely do want to guarantee that we catch and report all
    % circular insts *now*, because later compiler passes *assume* that
    % this has been done, and thus employ recursive algorithms on insts
    % that do not check for infinite recursion. Any circularity we miss here
    % is thus very likely to cause a later pass to enter an infinite loop.
    add_inst_defns(InstDefns,
        !ModuleInfo, !Specs),
    check_inst_defns(!.ModuleInfo, InstDefns,
        !FoundInvalidInstOrMode, !Specs),

    % Mode definitions may refer to user defined insts. Since we have already
    % seen all inst definitions, we could check whether the newly defined
    % mode refers to an undefined inst, but we do not (yet) do so.
    add_mode_defns(ModeDefns,
        !ModuleInfo, !Specs),
    check_mode_defns(!.ModuleInfo, ModeDefns,
        !FoundInvalidInstOrMode, !Specs),

    % A predicate declaration defines the type of the arguments of a predicate,
    % and may give the modes of those arguments. Since we have already seen
    % all type, inst and mode definitions, we could check whether the newly
    % defined predicate refers to an undefined type, inst, or mode, but
    % we do not (yet) do so.
    add_pred_decls(PredDecls,
        !ModuleInfo, !Specs),
    add_pred_decls(SolverPredDecls,
        !ModuleInfo, !Specs),

    % We need to process the mode declaration of a predicate
    % after we have seen the (type) declaration of the predicate.
    % In the past, this required the predicate declaration to precede
    % the mode declaration in the source code, but now, the order
    % in the source code does not matter.
    %
    % Note that mode declarations embedded in predmode declarations
    % have already been added to the HLDS by add_pred_decl.
    add_mode_decls(ModeDecls,
        !ModuleInfo, !Specs),

    % Every mutable has its own set of access predicates. Declare them
    % (using predmode declarations, which specify their types, modes and
    % determinism) whether the mutable definition was a standalone item
    % or part of the definition of a solver type.
    %
    % We have to do this after we add types and insts to the HLDS,
    % so we can check the types and insts in the mutable for validity.
    % XXX Currently, we check only the inst for validity.
    AllMutables = Mutables ++ SolverMutables,
    module_info_get_user_init_pred_target_names(!.ModuleInfo,
        InitPredTargetNames0),
    implement_mutables_if_local(!.ModuleInfo, AllMutables,
        MutablePredDecls, MutableClauses, MutableForeignProcs,
        MutableForeignDeclCodes, MutableForeignBodyCodes,
        RevPragmaFPEInfos1, InitPredTargetNames0, InitPredTargetNames1,
        !Specs),
    add_pred_decls(MutablePredDecls,
        !ModuleInfo, !Specs),

    % Record the definitions of typeclasses.
    % This will add the pred and mode declarations of the methods inside them
    % to the HLDS.
    add_typeclass_defns(Typeclasses,
        !ModuleInfo, !Specs),

    % The old pass 2.

    % Now that we have processed all mode declarations, we have a reliable
    % record of which declared predicates and functions have NO mode
    % declaration. For functions, this is not an error, since Mercury
    % specifies a implicit mode declaration for them. maybe_add_default_mode
    % adds that implicit mode declaration in such circumstances.
    % XXX It should also generate error messages for PREDICATES that have
    % no mode declaration.
    maybe_add_default_modes(PredDecls,
        !ModuleInfo),
    % The items in MutablePredDecls do not have default modes to add.

    % Record instance definitions.
    add_instance_defns(Instances,
        !ModuleInfo, !Specs),

    % Implement several kinds of pragmas, the ones in the subtype
    % defined by the pragma_pass_2 inst.

    % Record imports of foreign-language modules for use by the target code
    % we may generate.
    list.foldl(module_add_item_fim, FIMs,
        !ModuleInfo),

    % Since all declared predicates are in now the HLDS, we could check
    % all type definitions that define type-specific unify and/or compare
    % predicates whether the predicate names they give refer to predicates
    % that (a) exist, and (b) have the right signature. However, we currently
    % don't do that. Any such errors are discovered when we type and mode
    % check that automatically created unify and compare predicates, whose
    % bodies call the user-specified predicate names.
    % XXX PASS STRUCTURE Maybe we *should* check that here, since doing so
    % would allow us to generate better error messages.
    (
        !.FoundInvalidType = did_not_find_invalid_type,
        % Add constructors for du types to the HLDS, check subtype definitions,
        % and check that Mercury types defined solely by foreign types have a
        % definition that works for the target backend.
        %
        % This must be done after adding all type definitions and all
        % `:- pragma foreign_type' declarations. If there were errors
        % in foreign type type declarations, doing this may cause
        % a compiler abort.
        module_info_get_type_table(!.ModuleInfo, TypeTable0),
        foldl3_over_type_ctor_defns(
            add_du_ctors_check_subtype_check_foreign_type(TypeTable0),
            TypeTable0, !FoundInvalidType, !ModuleInfo, !Specs)
    ;
        !.FoundInvalidType = found_invalid_type
    ),

    % Add the special preds for the builtin types which don't have a
    % type declaration, hence no hlds_type_defn is generated for them.
    ( if
        ModuleName = mercury_public_builtin_module,
        compiler_generated_rtti_for_builtins(!.ModuleInfo)
    then
        list.foldl(add_builtin_type_ctor_special_preds_in_builtin_module,
            builtin_type_ctors_with_no_hlds_type_defn, !ModuleInfo)
    else
        true
    ),

    % Balance any data structures that need it.
    module_info_optimize(!ModuleInfo),

    % The old pass 3.
    init_qual_info(MQInfo0, TypeEqvMap, !:QualInfo),

    % Add clauses to their predicates.
    add_clauses(Clauses,
        !ModuleInfo, !QualInfo, !Specs),
    % Add clauses that define the auxiliary predicates
    % that implement some of the auxiliary predicates of mutables.
    add_clauses(MutableClauses,
        !ModuleInfo, !QualInfo, !Specs),
    % Add clauses that record promises.
    % XXX CLEANUP For each promise, we add a clause, find that there is
    % no predicate declaration for the predicate that the clause is for,
    % remember that oh yeah, this is normal for promises, and then add
    % the implicitly-specified predicate declaration. It would be much simpler
    % to just construct and add the predicate declaration first.
    add_promises(Promises,
        !ModuleInfo, !QualInfo, !Specs),

    % Remember attempts to define predicates in the interface.
    module_info_set_int_bad_clauses(IntBadClauses, !ModuleInfo),

    % Add foreign proc definitions
    % - for the auxiliary predicates that implement solver types, and
    % - for the rest of the auxiliary predicates that implement mutables.
    add_pragma_foreign_procs(SolverForeignProcs,
        !ModuleInfo, !Specs),
    add_pragma_foreign_procs(MutableForeignProcs,
        !ModuleInfo, !Specs),

    % Check that the predicates listed in `:- initialise' and `:- finalise'
    % declarations exist and have the correct signature, construct
    % pragma foreign_export declarations for them, and record their exported
    % names in the module_info, so that we can generate code to call them
    % at initialisation/finalisation time.
    %
    % InitPredTargetNames1 is set by the call to
    % implement_mutables_if_local above.
    module_info_get_user_final_pred_target_names(!.ModuleInfo,
        FinalPredTargetNames1),
    add_initialises(!.ModuleInfo, Initialises,
        RevPragmaFPEInfos1, RevPragmaFPEInfos2,
        InitPredTargetNames1, InitPredTargetNames, !Specs),
    add_finalises(!.ModuleInfo, Finalises,
        RevPragmaFPEInfos2, RevPragmaFPEInfos,
        FinalPredTargetNames1, FinalPredTargetNames, !Specs),
    module_info_set_user_init_pred_target_names(InitPredTargetNames,
        !ModuleInfo),
    module_info_set_user_final_pred_target_names(FinalPredTargetNames,
        !ModuleInfo),

    % Implement all pragmas.
    %
    % Once upon a time, we processed many pragmas in pass 2.
    % We had three different reasons for doing this for three different
    % kinds of pragmas.
    %
    % 1: We had to do this for foreign_enum pragmas, since these may affect
    %    type representations, and *that* may affect many other things.
    %    However, these now have their own item kind, which are processed
    %    at the very top above.
    %
    % 2: We once also had to do this for foreign_decl pragmas, but the
    %    separation of user and aux foreign decls has removed that requirement
    %    (see the comments on the module_info fields containing foreign_decls
    %    in hlds_module.m).
    %
    % 3: We wanted to do this also for pragmas that mark predicates and/or
    %    functions as external, so that we could catch and report attempts
    %    to add clauses for any predicates and/or functions so marked, However,
    %    we have no code to actually do this. If we ever *do* want to implement
    %    this check, it can easily be done in this pass: just emit the error
    %    message if a predicate or function being marked has some clauses.
    %
    % We can now add all the remaining pragmas to the HLDS at the same time.
    % This times does have to be after we have processed all predicate
    % and mode declarations, since several pragmas do refer to predicates
    % or to modes of predicates.
    add_decl_pragmas(PragmasDecl,
        !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas_type_spec(PragmasDeclTypeSpec,
        !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas_term_info(PragmasDeclTermInfo,
        !ModuleInfo, !Specs),
    add_decl_pragmas_term2_info(PragmasDeclTerm2Info,
        !ModuleInfo, !Specs),
    add_decl_pragmas_sharing(PragmasDeclSharing,
        !ModuleInfo, !Specs),
    add_decl_pragmas_reuse(PragmasDeclReuse,
        !ModuleInfo, !Specs),

    % We want to process tabled pragmas *after* any inline pragmas
    % (which are also impl pragmas), so that we can detect and report
    % the problem if a predicate or function has both an inline pragma
    % and a tabled pragma.
    add_impl_pragmas(PragmasImpl, [], RevPragmasTabled,
        !ModuleInfo, !QualInfo, !Specs),
    list.reverse(RevPragmasTabled, PragmasTabled),
    add_impl_pragmas_tabled(PragmasTabled,
        !ModuleInfo, !QualInfo, !Specs),

    list.reverse(RevPragmaFPEInfos, PragmaFPEInfos),
    list.foldl2(add_pragma_info_foreign_proc_export, PragmaFPEInfos,
        !ModuleInfo, !Specs),

    list.foldl(module_add_foreign_decl_code_aux, MutableForeignDeclCodes,
        !ModuleInfo),
    list.foldl(module_add_foreign_body_code, MutableForeignBodyCodes,
        !ModuleInfo),

    list.foldl2(add_gen_pragma_unused_args, PragmasGenUnusedArgs,
        !ModuleInfo, !Specs),
    list.foldl2(add_gen_pragma_exceptions, PragmasGenExceptions,
        !ModuleInfo, !Specs),
    list.foldl2(add_gen_pragma_trailing, PragmasGenTrailing,
        !ModuleInfo, !Specs),
    list.foldl2(add_gen_pragma_mm_tabling, PragmasGenMMTabling,
        !ModuleInfo, !Specs),

    % Check that the declarations for field extraction and update functions
    % are sensible, and generate error messages for the ones that aren't.
    % We can do this only after we have processed every predicate declaration,
    % as well as everything that affects either the type table or the
    % constructor table.
    check_preds_if_field_access_function(!.ModuleInfo, PredDecls, !Specs),

    ModuleItemVersionNumbers =
        AugCompUnit ^ acu_module_item_version_numbers_map,
    map.foldl(add_module_item_version_numbers, ModuleItemVersionNumbers,
        !QualInfo),

    qual_info_get_mq_info(!.QualInfo, MQInfo),
    mq_info_get_found_undef_type(MQInfo, MQUndefType),
    mq_info_get_found_undef_inst(MQInfo, MQUndefInst),
    mq_info_get_found_undef_mode(MQInfo, MQUndefMode),
    mq_info_get_found_undef_typeclass(MQInfo, MQUndefTypeClass),
    ( if
        ( MQUndefType = found_undef_type
        ; MQUndefTypeClass = found_undef_typeclass
        )
    then
        !:FoundInvalidType = found_invalid_type
    else
        true
    ),
    ( if
        ( MQUndefInst = found_undef_inst
        ; MQUndefMode = found_undef_mode
        )
    then
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred add_builtin_type_ctor_special_preds_in_builtin_module(type_ctor::in,
    module_info::in, module_info::out) is det.

add_builtin_type_ctor_special_preds_in_builtin_module(TypeCtor, !ModuleInfo) :-
    varset.init(TVarSet),
    term.context_init(Context),
    % These predicates are local only in the public builtin module,
    % but we *get here* only if we are compiling the public builtin module.
    TypeStatus = type_status(status_local),
    construct_type(TypeCtor, [], Type),
    % You cannot construct clauses to unify or compare values of an abstract
    % type. The abstract body tells the callee to generate code for
    % a builtin type.
    Body = hlds_abstract_type(abstract_type_general),
    % XXX For some reason, when generating C# code, if we don't add
    % the special preds now, we don't add them ever, which can cause compiler
    % aborts when we try to generate the type's type_ctor_info structure
    % (since we need to put references to the type's unify and compare
    % predicates into that structure.)
    add_special_pred_decl_defns_for_type_eagerly(TVarSet,
        Type, TypeCtor, Body, TypeStatus, Context, !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_item_avails(ims_list(item_avail)::in,
    module_info::in, module_info::out) is det.

add_item_avails([], !ModuleInfo).
add_item_avails([ImsList | ImsLists], !ModuleInfo) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Avails),
    list.foldl(add_item_avail(ItemMercuryStatus), Avails, !ModuleInfo),
    add_item_avails(ImsLists, !ModuleInfo).

:- pred add_item_avail(item_mercury_status::in, item_avail::in,
    module_info::in, module_info::out) is det.

add_item_avail(ItemMercuryStatus, Avail, !ModuleInfo) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = use_decl
    ),
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_anywhere,
            Section = ms_interface
        ;
            ( ItemExport = item_export_nowhere
            ; ItemExport = item_export_only_submodules
            ),
            Section = ms_implementation
        ),
        module_add_avail_module_name(ModuleName, Section, ImportOrUse,
            yes(Context), !ModuleInfo)
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ItemImport = item_import_int_concrete(ImportLocn),
            (
                ImportLocn = import_locn_ancestor_int0_interface,
                module_add_avail_module_name(ModuleName, ms_interface,
                    ImportOrUse, no, !ModuleInfo),
                % A module that is imported by an ancestor may be used
                % in that ancestor even if it is not used in this module.
                % We therefore record it as "used" to avoid reporting
                % a warning that may be incorrect.
                %
                % If the import is not used in the ancestor's interface,
                % we should be able to generate a warning for that
                % when we compile the ancestor. XXX We do not currently
                % do this.
                module_info_add_parent_to_used_modules(ModuleName, !ModuleInfo)
            ;
                ImportLocn = import_locn_ancestor_int0_implementation,
                module_add_avail_module_name(ModuleName, ms_implementation,
                    ImportOrUse, no, !ModuleInfo),
                % A module that is imported by an ancestor may be used
                % in that ancestor even if it is not used in this module.
                % We therefore record it as "used" to avoid reporting
                % a warning that may be incorrect.
                %
                % If the import is not used in the ancestor itself,
                % we should be able to generate a warning for that
                % when we compile the ancestor. XXX We do not currently
                % do this.
                module_info_add_parent_to_used_modules(ModuleName, !ModuleInfo)
            ;
                ( ImportLocn = import_locn_interface
                ; ImportLocn = import_locn_implementation
                ; ImportLocn = import_locn_import_by_ancestor
                ),
                % XXX Given that we get here only if ItemMercuryStatus says
                % the item is from another module, the import cannot be
                % in either the interface or the implementation section
                % of *this* module, so neither import_locn_interface
                % nor import_locn_implementation should be possible
                % without misuse of those import_locns.
                module_add_indirectly_imported_module_name(ModuleName,
                    !ModuleInfo)
            )
        ;
            ( ItemImport = item_import_int_abstract
            ; ItemImport = item_import_opt_int
            ),
            module_add_indirectly_imported_module_name(ModuleName, !ModuleInfo)
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_type_defns(sec_list(item_type_defn_info)::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    ims_list(item_foreign_proc)::in, ims_list(item_foreign_proc)::out,
    sec_list(item_mutable_info)::in, sec_list(item_mutable_info)::out) is det.

add_type_defns([], !ModuleInfo, !FoundInvalidType,
        !Specs, !RevPredDecls, !RevForeignProcs, !RevMutables).
add_type_defns([SecList | SecLists], !ModuleInfo, !FoundInvalidType,
        !Specs, !RevPredDecls, !RevForeignProcs, !RevMutables) :-
    SecList = sec_sub_list(SectionInfo, TypeDefns),
    SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
    item_mercury_status_to_type_status(ItemMercuryStatus, TypeStatus),
    list.foldl6(add_type_defn(SectionInfo, TypeStatus), TypeDefns,
        !ModuleInfo, !FoundInvalidType, !Specs,
        !RevPredDecls, !RevForeignProcs, !RevMutables),
    add_type_defns(SecLists, !ModuleInfo, !FoundInvalidType,
        !Specs, !RevPredDecls, !RevForeignProcs, !RevMutables).

:- pred add_type_defn(sec_info::in, type_status::in, item_type_defn_info::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    ims_list(item_foreign_proc)::in, ims_list(item_foreign_proc)::out,
    sec_list(item_mutable_info)::in, sec_list(item_mutable_info)::out) is det.

add_type_defn(SectionInfo, TypeStatus, TypeDefnInfo,
        !ModuleInfo, !FoundInvalidType, !Specs,
        !RevPredDecls, !RevForeignProcs, !RevMutables) :-
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    TypeDefnInfo = item_type_defn_info(SymName, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    (
        TypeDefn = parse_tree_solver_type(Detailssolver),
        Detailssolver =
            type_details_solver(SolverTypeDetails, _MaybeUserEqComp),
        % If this is a solver type, then we need to also declare and define
        % the compiler generated construction function and deconstruction
        % predicate for the special constrained data constructor.
        % Before switch detection, we turn calls to these functions/predicates
        % into ordinary constructions/deconstructions, but preserve the
        % corresponding impurity annotations.
        %
        % Our caller will add !:PredDecls, !:ForeignProcs and !:Mutables
        % to the HLDS (roughly) when it processes other predicate declarations,
        % foreign procs and mutables.
        SolverAuxPredInfo = solver_aux_pred_info(SymName, TypeParams,
            TypeVarSet, SolverTypeDetails, Context),
        get_solver_type_aux_pred_decls(SolverAuxPredInfo, PredDeclInfos),
        PredDeclList = sec_sub_list(SectionInfo, PredDeclInfos),
        !:RevPredDecls = [PredDeclList | !.RevPredDecls],
        (
            ItemMercuryStatus = item_defined_in_this_module(_),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, Target),
            get_solver_type_aux_pred_defns(Target, SolverAuxPredInfo,
                ForeignProcInfos),
            ForeignProcList =
                ims_sub_list(ItemMercuryStatus, ForeignProcInfos),
            !:RevForeignProcs = [ForeignProcList | !.RevForeignProcs],

            MutableItems = SolverTypeDetails ^ std_mutable_items,
            MutableList = sec_sub_list(SectionInfo, MutableItems),
            !:RevMutables = [MutableList | !.RevMutables]
        ;
            ItemMercuryStatus = item_defined_in_other_module(_)
        )
    ;
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_sub_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        )
    ),
    module_add_type_defn(TypeStatus, NeedQual, TypeDefnInfo,
        !ModuleInfo, !FoundInvalidType, !Specs).

%---------------------------------------------------------------------------%

:- pred add_inst_defns(ims_list(item_inst_defn_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_inst_defns([], !ModuleInfo, !Specs).
add_inst_defns([ImsSubList | ImsSubLists], !ModuleInfo, !Specs) :-
    ImsSubList = ims_sub_list(ItemMercuryStatus, InstDefns),
    item_mercury_status_to_inst_status(ItemMercuryStatus, InstStatus),
    list.foldl2(module_add_inst_defn(InstStatus), InstDefns,
        !ModuleInfo, !Specs),
    add_inst_defns(ImsSubLists, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_mode_defns(ims_list(item_mode_defn_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mode_defns([], !ModuleInfo, !Specs).
add_mode_defns([ImsSubList | ImsSubLists], !ModuleInfo, !Specs) :-
    ImsSubList = ims_sub_list(ItemMercuryStatus, ModeDefns),
    item_mercury_status_to_mode_status(ItemMercuryStatus, ModeStatus),
    list.foldl2(module_add_mode_defn(ModeStatus), ModeDefns,
        !ModuleInfo, !Specs),
    add_mode_defns(ImsSubLists, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pred_decls(sec_list(item_pred_decl_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_decls([], !ModuleInfo, !Specs).
add_pred_decls([SecSubList | SecSubLists], !ModuleInfo, !Specs) :-
    SecSubList = sec_sub_list(SectionInfo, PredDecls),
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.map_foldl2(
        module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual),
        PredDecls, _MaybePredProcIds, !ModuleInfo, !Specs),
    add_pred_decls(SecSubLists, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_mode_decls(ims_list(item_mode_decl_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mode_decls([], !ModuleInfo, !Specs).
add_mode_decls([SecSubList | SecSubLists], !ModuleInfo, !Specs) :-
    SecSubList = ims_sub_list(ItemMercuryStatus, ModeDecls),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.map_foldl2(
        module_add_mode_decl(not_part_of_predmode, is_not_a_class_method,
            ItemMercuryStatus, PredStatus),
        ModeDecls, _PredProcIds, !ModuleInfo, !Specs),
    add_mode_decls(SecSubLists, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred maybe_add_default_modes(sec_list(item_pred_decl_info)::in,
    module_info::in, module_info::out) is det.

maybe_add_default_modes([], !ModuleInfo).
maybe_add_default_modes([SecSubList | SecSubLists], !ModuleInfo) :-
    SecSubList = sec_sub_list(_SectionInfo, Items),
    list.foldl(maybe_add_default_mode, Items, !ModuleInfo),
    maybe_add_default_modes(SecSubLists, !ModuleInfo).

:- pred maybe_add_default_mode(item_pred_decl_info::in,
    module_info::in, module_info::out) is det.

maybe_add_default_mode(PredDecl, !ModuleInfo) :-
    PredDecl = item_pred_decl_info(PredSymName, PredOrFunc, TypesAndModes,
        _, _, _, _, _, _, _, _, _, _, _),
    % Add default modes for function declarations, if necessary.
    PredName = unqualify_name(PredSymName),
    ( if PredName = "" then
        % We didn't add the predicate declaration itself above,
        % so we cannot possibly add anything to it now.
        true
    else
        (
            PredOrFunc = pf_predicate
        ;
            PredOrFunc = pf_function,
            PredFormArity = arg_list_arity(TypesAndModes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            module_info_get_predicate_table(!.ModuleInfo, PredTable0),
            predicate_table_lookup_func_sym_arity(PredTable0,
                is_fully_qualified, PredSymName, UserArity, PredIds),
            (
                PredIds = [_ | _],
                predicate_table_get_pred_id_table(PredTable0, PredIdTable0),
                maybe_add_default_func_modes(PredIds,
                    PredIdTable0, PredIdTable),
                predicate_table_set_pred_id_table(PredIdTable,
                    PredTable0, PredTable),
                module_info_set_predicate_table(PredTable, !ModuleInfo)
            ;
                PredIds = [],
                unexpected($pred, "can't find func declaration")
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_clauses(ims_list(item_clause_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_clauses([], !ModuleInfo, !QualInfo, !Specs).
add_clauses([ImsList | ImsLists], !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    ClauseType = clause_not_for_promise,
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.foldl3(module_add_clause(PredStatus, ClauseType), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_clauses(ImsLists, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_promises(ims_list(item_promise_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promises([], !ModuleInfo, !QualInfo, !Specs).
add_promises([ImsList | ImsLists], !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.foldl3(add_promise(PredStatus), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_promises(ImsLists, !ModuleInfo, !QualInfo, !Specs).

:- pred add_promise(pred_status::in, item_promise_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promise(PredStatus, PromiseInfo, !ModuleInfo, !QualInfo, !Specs) :-
    PromiseInfo = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
        Context, SeqNum),
    % Promise declarations are recorded as a predicate with a goal_type
    % of goal_type_promise(X), where X is a promise_type. This allows us
    % to leverage off all the other checks in the compiler that operate
    % on predicates.
    %
    % :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
    %
    % becomes
    %
    % promise__lineno__filename(A, B, R) :-
    %   ( R = A + B <=> R = B + A ).
    %
    % The double underscores in the compiler-generated name would be turned
    % into module qualifications if the name were provided by the user,
    % guarding against accidental name clashes.
    %
    % XXX We should be testing Goal here to see if it has the form of
    % one of the supported kinds of promises, recording it in a
    % promise-kind-specific format if it is, and rejecting it with warning
    % if it is not.
    (
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        % Extra error checking for promise ex declarations.
        check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !Specs)
    ;
        PromiseType = promise_type_true
    ),
    ClauseType = clause_for_promise(PromiseType),

    term.context_line(Context, Line),
    term.context_file(Context, File),
    string.format("%s__%d__%s",
        [s(prog_out.promise_to_string(PromiseType)), i(Line), s(File)], Name),
    module_info_get_name(!.ModuleInfo, ModuleName),
    PromisePredSymName = qualified(ModuleName, Name),

    % If the outermost universally quantified variables are placed in the head
    % of the dummy predicate, the typechecker will avoid warning about unbound
    % type variables, as this implicitly adds a universal quantification of the
    % type variables needed.
    term.var_list_to_term_list(UnivVars, HeadVars),
    ClauseInfo = item_clause_info(pf_predicate, PromisePredSymName, HeadVars,
        VarSet, ok2(Goal, []), Context, SeqNum),
    module_add_clause(PredStatus, ClauseType, ClauseInfo,
        !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_initialises(module_info::in, ims_list(item_initialise_info)::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_initialises(_, [], !RevPragmaFPEInfos, !PredTargetNames, !Specs).
add_initialises(ModuleInfo, [ImsList | ImsLists],
        !RevPragmaFPEInfos, !PredTargetNames, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_initialise(ModuleInfo, ItemMercuryStatus),
        Items, !RevPragmaFPEInfos, !PredTargetNames, !Specs),
    add_initialises(ModuleInfo, ImsLists,
        !RevPragmaFPEInfos, !PredTargetNames, !Specs).

:- pred add_initialise(module_info::in, item_mercury_status::in,
    item_initialise_info::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_initialise(ModuleInfo, ItemMercuryStatus, Initialise,
        !RevPragmaFPEInfos, !PredTargetNames, !Specs) :-
    Initialise = item_initialise_info(SymName, Arity, Origin, Context, SeqNum),
    (
        ItemMercuryStatus = item_defined_in_this_module(_),
        (
            Origin = item_origin_user,
            implement_initialise_finalise(ModuleInfo, iof_init,
                SymName, Arity, Context, SeqNum,
                !RevPragmaFPEInfos, !PredTargetNames, !Specs)
        ;
            Origin = item_origin_compiler(_CompilerAttrs),
            unexpected($pred, "bad introduced initialise declaration")
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % It is OK if the initialise is defined in a parent module,
        % and we get to see it (though it SHOULD be kept out of .int0 files),
        % but we should NOT implement it.
    ).

:- pred add_finalises(module_info::in, ims_list(item_finalise_info)::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_finalises(_, [], !RevPragmaFPEInfos, !PredTargetNames, !Specs).
add_finalises(ModuleInfo, [ImsList | ImsLists],
        !RevPragmaFPEInfos, !PredTargetNames, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_finalise(ModuleInfo, ItemMercuryStatus),
        Items, !RevPragmaFPEInfos, !PredTargetNames, !Specs),
    add_finalises(ModuleInfo, ImsLists,
        !RevPragmaFPEInfos, !PredTargetNames, !Specs).

:- pred add_finalise(module_info::in, item_mercury_status::in,
    item_finalise_info::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_finalise(ModuleInfo, ItemMercuryStatus, FinaliseInfo,
        !RevPragmaFPEInfos, !PredTargetNames, !Specs) :-
    FinaliseInfo = item_finalise_info(SymName, Arity, Origin, Context, SeqNum),
    (
        ItemMercuryStatus = item_defined_in_this_module(_),
        (
            Origin = item_origin_user,
            implement_initialise_finalise(ModuleInfo, iof_final,
                SymName, Arity, Context, SeqNum,
                !RevPragmaFPEInfos, !PredTargetNames, !Specs)
        ;
            Origin = item_origin_compiler(_),
            unexpected($pred, "bad introduced finalise declaration")
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % It is OK if the initialise is defined in a parent module,
        % and we get to see it (though it SHOULD be kept out of .int0 files),
        % but we should NOT implement it.
    ).

%---------------------%

:- type init_or_final
    --->    iof_init
    ;       iof_final.

:- pred implement_initialise_finalise(module_info::in, init_or_final::in,
    sym_name::in, user_arity::in, prog_context::in, item_seq_num::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::in,
    list(item_pragma_info(pragma_info_foreign_proc_export))::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_initialise_finalise(ModuleInfo, InitOrFinal, SymName, UserArity,
        Context, SeqNum, !RevPragmaFPEInfos, !PredTargetNames, !Specs) :-
    % To implement an `:- initialise pred.' or `:- finalise pred' declaration
    % for C backends, we need to:
    %
    % (1) construct a new C function name, CName, to use to export pred,
    % (2) add the export pragma that does this,
    % (3) record the pred/cname pair in the ModuleInfo so that
    %     code generation can ensure CName is called during module
    %     initialisation/finalisation

    module_info_get_predicate_table(ModuleInfo, PredTable),
    UserArity = user_arity(UserArityInt),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, UserArity, PredIds),
    ( InitOrFinal = iof_init,  DeclName = "initialise"
    ; InitOrFinal = iof_final, DeclName = "finalise"
    ),
    (
        PredIds = [],
        Pieces = [words("Error:"),
            qual_sym_name_arity(sym_name_arity(SymName, UserArityInt)),
            words("used in"), decl(DeclName), words("declaration"),
            words("does not have a corresponding"),
            decl("pred"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [PredId],
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ( if is_valid_init_or_final_pred(PredInfo, ExpectedHeadModes) then
            module_info_get_name(ModuleInfo, ModuleName),
            (
                InitOrFinal = iof_init,
                NameIoF = "init",
                Origin = compiler_origin_initialise
            ;
                InitOrFinal = iof_final,
                NameIoF = "final",
                Origin = compiler_origin_finalise
            ),
            new_user_init_or_final_pred_target_name(ModuleName, NameIoF,
                SeqNum, SymName, UserArity, TargetName, !PredTargetNames),
            module_info_get_globals(ModuleInfo, Globals),
            make_pragma_foreign_proc_export(Globals, SymName,
                ExpectedHeadModes, TargetName, Origin, Context, PragmaFPEInfo),
            !:RevPragmaFPEInfos = [PragmaFPEInfo | !.RevPragmaFPEInfos]
        else
            Pieces = [words("Error:"),
                qual_sym_name_arity(sym_name_arity(SymName, UserArityInt)),
                words("used in"), decl(DeclName), words("declaration"),
                words("has an invalid signature."), nl,
                words("A signature is valid only if it has"),
                words("one of these two forms:"),
                nl_indent_delta(1),
                quote(":- pred <predname>(io::di, io::uo) is <detism>."), nl,
                quote(":- impure pred <predname> is <detism>."),
                nl_indent_delta(-1),
                words("where"), quote("<detism>"), words("is either"),
                quote("det"), words("or"), quote("cc_multi"), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Error:"),
            qual_sym_name_arity(sym_name_arity(SymName, UserArityInt)),
            words("used in"), decl(DeclName), words("declaration"),
            words("has multiple"), decl("pred"), words("declarations."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

:- pred is_valid_init_or_final_pred(pred_info::in, list(mer_mode)::out)
    is semidet.

is_valid_init_or_final_pred(PredInfo, ExpectedHeadModes) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_proc_table(PredInfo, ProcTable),
    ProcInfos = map.values(ProcTable),
    (
        ArgTypes = [Arg1Type, Arg2Type],
        type_is_io_state(Arg1Type),
        type_is_io_state(Arg2Type),
        ExpectedHeadModes = [di_mode, uo_mode],
        ExpectedPurity = purity_pure
    ;
        ArgTypes = [],
        ExpectedHeadModes = [],
        ExpectedPurity = purity_impure
    ),
    some [ProcInfo] (
        list.member(ProcInfo, ProcInfos),
        proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeHeadModes),
        MaybeHeadModes = yes(HeadModes),
        HeadModes = ExpectedHeadModes,
        proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
        MaybeDetism = yes(Detism),
        ( Detism = detism_det ; Detism = detism_cc_multi )
    ),
    pred_info_get_purity(PredInfo, Purity),
    Purity = ExpectedPurity.

:- pred make_pragma_foreign_proc_export(globals::in, sym_name::in,
    list(mer_mode)::in, string::in, compiler_origin::in, prog_context::in,
    item_pragma_info(pragma_info_foreign_proc_export)::out) is det.

make_pragma_foreign_proc_export(Globals, SymName, HeadModes, CName,
        Origin, Context, PragmaFPEInfo) :-
    Attrs = item_compiler_attributes(Origin),
    PEOrigin = item_origin_compiler(Attrs),
    globals.get_target(Globals, CompilationTarget),
    ExportLang = target_lang_to_foreign_export_lang(CompilationTarget),
    PredNameModesPF = proc_pf_name_modes(pf_predicate, SymName, HeadModes),
    % Since the pragma is not coming from the user, we won't be
    % generating any error messages for it, which means that
    % the varset won't be used.
    varset.init(VarSet),
    FPEInfo = pragma_info_foreign_proc_export(PEOrigin, ExportLang,
        PredNameModesPF, CName, VarSet),
    PragmaFPEInfo = item_pragma_info(FPEInfo, Context, item_no_seq_num).

%---------------------------------------------------------------------------%

:- pred add_module_item_version_numbers(module_name::in,
    module_item_version_numbers::in, qual_info::in, qual_info::out) is det.

add_module_item_version_numbers(ModuleName, ModuleItemVersionNumbers,
        !QualInfo) :-
    % Record the version numbers for each imported module
    % if smart recompilation is enabled.
    apply_to_recompilation_info(
        update_module_item_version_numbers(ModuleName,
            ModuleItemVersionNumbers),
        !QualInfo).

:- pred update_module_item_version_numbers(module_name::in,
    module_item_version_numbers::in,
    recompilation_info::in, recompilation_info::out) is det.

update_module_item_version_numbers(ModuleName, ModuleItemVersionNumbers,
        !RecompInfo) :-
    VersionNumbersMap0 = !.RecompInfo ^ recomp_version_numbers,
    map.set(ModuleName, ModuleItemVersionNumbers,
        VersionNumbersMap0, VersionNumbersMap),
    !RecompInfo ^ recomp_version_numbers := VersionNumbersMap.

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.
%---------------------------------------------------------------------------%
