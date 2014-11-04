%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mlds_to_il.m - Convert MLDS to IL.
% Main author: trd, petdr.
%
% This module generates IL from MLDS.  Currently it's pretty tuned
% towards generating assembler -- to generate code using
% Reflection::Emit it is likely some changes will need to be made.
%
% Currently non-det environments are represented using a high-level data
% representation (classes with typed fields), while all other data structures
% are represented using a low-level data representation (arrays of
% System.Object).  This is for historical reasons -- the MLDS high-level-data
% support wasn't available when it was needed.  Eventually we should
% move to a completely high-level data representation as the current
% representation is pretty inefficient.
%
% The IL backend TO-DO list:
%
% [ ] advanced name mangling:
%   - optionally only mangle names when it is absolutely necessary
%   (Partly done; we now mangle names less often than we used to.
%   The only way to mangle less would be to use a context-sensitive
%   name mangling algorithm, which may not be a good idea.)
% [ ] Type classes
%   - now work, but...
%   - type class hierarchies don't work due to unimplemented pragma
%     foreign code.
%   - should be implemented as interfaces
% [ ] RTTI (io.write -- about half the work required for this is done)
% [ ] High-level RTTI data
% [ ] Test unused mode (we seem to create a byref for it)
% [ ] auto dependency generation for IL and assembler
% [ ] build environment improvements
%       (support libraries/packages/namespaces better)
% [ ] verifiable code
%   [ ] verifiable function pointers
% [ ] omit empty cctors
% [ ] Computed gotos need testing.
% [ ] nested modules need testing
% [ ] Fix issues with abstract types so that we can implement C
%     pointers as MR_Box rather than MR_Word.
% [ ] When generating target_code, sometimes we output more calls than
%     we should (this can occur in nondet C code).
% [ ] ml_gen_call_current_success_cont_indirectly should be merged with
%   similar code for doing copy-in/copy-out.
% [ ] Add an option to do overflow checking.
% [ ] Should replace hard-coded of int32 with a more abstract name such
%     as `mercury_int_il_type'.
% [ ] Implement `pragma foreign_export' for C#.
%
% XXX We should rename this module to mlds_to_ilds, since that is what
%     it actually does.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_il.
:- interface.

:- import_module hlds.hlds_pred.    % for `pred_proc_id'.
:- import_module libs.globals.      % for `foreign_language'.
:- import_module ml_backend.ilasm.
:- import_module ml_backend.ilds.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Generate IL assembly from MLDS.
    %
    % This is where all the action is for the IL backend.
    %
:- pred generate_il(globals::in, mlds::in, list(il_decl)::out,
    set(foreign_language)::out) is det.

%-----------------------------------------------------------------------------%

    % The following predicates are exported so that we can get type
    % conversions and name mangling consistent between the C# output
    % (currently in mlds_to_managed.m) and IL output (in this file).
    %
    % XXX we should reduce the dependencies here to a bare minimum.
    %
:- func params_to_il_signature(il_data_rep, mlds_module_name,
    mlds_func_params) = signature.

    % Generate an identifier for a pred label, to be used in C#.
    %
:- pred predlabel_to_csharp_id(mlds_pred_label::in, proc_id::in,
    maybe(mlds_func_sequence_num)::in, ilds.id::out) is det.

    % Generate an IL identifier for a MLDS var.
    %
:- pred mangle_mlds_var(mlds_var::in, ilds.id::out) is det.

    % This type stores information affecting our IL data representation.
    %
:- type il_data_rep
    --->    il_data_rep(
                highlevel_data  :: bool,        % Do we use high-level data?
                il_envptr_type  :: il_type      % What IL type do we use for
                                                % mlds_generic_env_ptr_type?
            ).

:- pred get_il_data_rep(globals::in, il_data_rep::out) is det.

    % Get the corresponding ILDS type for an MLDS type
    % (this depends on which representation you happen to be using).
    %
:- func mlds_type_to_ilds_type(il_data_rep, mlds_type) = il_type.

    % Get the corresponding ILDS class name for an MLDS type
    % (this depends on which representation you happen to be using).
    %
:- func mlds_type_to_ilds_class_name(il_data_rep, mlds_type) =
    ilds.class_name.

    % Turn a proc name into an IL class_name and a method name.
    %
:- pred mangle_mlds_proc_label(mlds_qualified_proc_label::in,
    maybe(mlds_func_sequence_num)::in, ilds.class_name::out, ilds.id::out)
    is det.

    % class_name(Module, Name) returns a class name representing
    % Name in the module Module.
    %
:- func class_name(mlds_module_name, string) = ilds.class_name.

    % Return the class_name for the generic class.
    %
:- func il_generic_class_name = ilds.class_name.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module hlds.code_model.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module cord.
:- import_module counter.
:- import_module deconstruct.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

    % We build up lists of instructions using a tree to make
    % insertion easy.
    %
:- type instr_tree == cord(instr).

    % The state of the il code generator.
    %
:- type il_info
    --->    il_info(
                % file-wide attributes (all static)
                module_name         :: mlds_module_name,
                assembly_name       :: ilds.id,
                imports             :: mlds_imports,
                file_foreign_langs  :: set(foreign_language),
                                    % file foreign code

                il_data_rep         :: il_data_rep,
                debug_il_asm        :: bool,        % --debug-il-asm
                verifiable_code     :: bool,        % --verifiable-code
                il_byref_tailcalls  :: bool,        % --il-byref-tailcalls
                support_ms_clr      :: bool,        % --support-ms-clr
                support_rotor_clr   :: bool,        % --support-rotor-clr

                % class-wide attributes (all accumulate)
                alloc_instrs        :: instr_tree,
                                    % .cctor allocation instructions
                init_instrs         :: instr_tree,
                                    % .cctor init instructions
                class_members       :: list(class_member),
                                    % class methods and fields
                has_main            :: has_main,
                                    % class contains main
                class_foreign_langs :: set(foreign_language),
                                    % class foreign code
                field_names         :: field_names_set,

                % method-wide attributes (accumulating)
                locals              :: locals_map,
                                    % The current locals
                instr_tree          :: instr_tree,
                                    % The instruction tree (unused)
                label_counter       :: counter,     % the label counter
                block_counter       :: counter,     % the block counter
                method_foreign_lang :: maybe(foreign_language),
                                    % method contains foreign code

                % method-wide attributes (static)
                arguments           :: arguments_map,   % The arguments
                method_name         :: member_name,     % current method name
                csharp_method_name  :: member_name,
                                    % current C# method name
                signature           :: signature        % current return type
            ).

:- type locals_map == map(ilds.id, mlds_type).
:- type arguments_map == assoc_list(ilds.id, mlds_type).
:- type mlds_vartypes == map(ilds.id, mlds_type).
:- type field_names_set == set(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_il(Globals, MLDS, ILAsm, ForeignLangs) :-
    globals.get_maybe_il_version_number(Globals, MaybeVersionNumber),
    (
        MaybeVersionNumber = yes(VersionNumber),
        VersionNumber = il_version_number(Major, Minor, Build, Revision),
        Version = version(Major, Minor, Build, Revision),
        generate_il(Globals, MLDS, Version, ILAsm, ForeignLangs)
    ;
        MaybeVersionNumber = no,
        ILAsm = [],
        ForeignLangs = set.init
    ).

%-----------------------------------------------------------------------------%

:- pred generate_il(globals::in, mlds::in, assembly_decl::in,
    list(il_decl)::out, set(foreign_language)::out) is det.

generate_il(Globals, MLDS0, Version, ILAsm, ForeignLangs) :-
    % XXX initialise declarations NYI for IL backend
    il_transform_mlds(MLDS0, MLDS),
    MLDS = mlds(MercuryModuleName, ForeignCode, Imports, GlobalData, Defns0,
        _, _, _),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap, GlobalDefns),
    expect(map.is_empty(ScalarCellGroupMap), $module, $pred,
        "nonempty ScalarCellGroupMap"),
    expect(map.is_empty(VectorCellGroupMap), $module, $pred,
        "nonempty VectorCellGroupMap"),
    Defns = GlobalDefns ++ Defns0,

    ModuleName = mercury_module_name_to_mlds(MercuryModuleName),
    AssemblyName =
        sym_name_to_string(mlds_module_name_to_sym_name(ModuleName)),
    get_il_data_rep(Globals, ILDataRep),
    globals.lookup_bool_option(Globals, debug_il_asm, DebugIlAsm),
    globals.lookup_bool_option(Globals, verifiable_code, VerifiableCode),
    globals.lookup_bool_option(Globals, il_byref_tailcalls, ByRefTailCalls),
    globals.lookup_bool_option(Globals, il_sign_assembly, SignAssembly),
    globals.lookup_bool_option(Globals, separate_assemblies,
        SeparateAssemblies),
    globals.lookup_bool_option(Globals, support_ms_clr, MsCLR),
    globals.lookup_bool_option(Globals, support_rotor_clr, RotorCLR),

    IlInfo0 = il_info_init(ModuleName, AssemblyName, Imports, ILDataRep,
        DebugIlAsm, VerifiableCode, ByRefTailCalls, MsCLR, RotorCLR),

    % Generate code for all the methods.
    list.map_foldl(mlds_defn_to_ilasm_decl, Defns, ILDecls, IlInfo0, IlInfo),

    list.filter(has_foreign_code_defined(ForeignCode),
        [lang_csharp], ForeignCodeLangs),

    ForeignLangs = IlInfo ^ file_foreign_langs `union`
        set.list_to_set(ForeignCodeLangs),

    ClassName = mlds_module_name_to_class_name(ModuleName),
    ClassName = structured_name(_, NamespaceName, _),

    % Make this module an assembly unless it is in the standard library.
    % Standard library modules all go in the one assembly in a separate step
    % during the build (using AL.EXE).
    PackageName = mlds_module_name_to_package_name(ModuleName),
    ( sym_name_prefix(PackageName) = "mercury" ->
        ThisAssembly = [],
        AssemblerRefs = Imports
    ;
        % If the package name is qualified then the we have a sub-module
        % which shouldn't be placed in its own assembly provided we have
        % --no-separate-assemblies.
        (
            PackageName = qualified(_, _),
            SeparateAssemblies = no
        ->
            ThisAssembly = []
        ;
            ThisAssembly = [ildecl_assembly(AssemblyName)]
        ),

        % XXX At a later date we should make foreign code behave like
        % a submodule.
        %
        % If not in the library, but we have foreign code, declare the foreign
        % module as an assembly we reference.
        list.map(
            (pred(F::in, I::out) is det :-
                mangle_foreign_code_module(F, ModuleName, N),
                I = mercury_import(compiler_visible_interface, N)
            ),
            set.to_sorted_list(ForeignLangs),
            ForeignCodeAssemblerRefs),
        AssemblerRefs = list.append(ForeignCodeAssemblerRefs, Imports)
    ),
    generate_extern_assembly(AssemblyName, Version, SignAssembly,
        SeparateAssemblies, AssemblerRefs, ExternAssemblies),
    Namespace = ildecl_namespace(NamespaceName, ILDecls),
    ILAsm = ThisAssembly ++ ExternAssemblies ++ [Namespace].

get_il_data_rep(Globals, ILDataRep) :-
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    ILEnvPtrType = choose_il_envptr_type(Globals),
    ILDataRep = il_data_rep(HighLevelData, ILEnvPtrType).

:- pred has_foreign_code_defined(map(foreign_language, mlds_foreign_code)::in,
    foreign_language::in) is semidet.

has_foreign_code_defined(ForeignCodeMap, Lang) :-
    ForeignCode = map.search(ForeignCodeMap, Lang),
    ForeignCode = mlds_foreign_code(Decls, Imports, Codes, Exports),
    ( Decls = [_ | _]
    ; Imports = [_ | _]
    ; Codes = [_ | _]
    ; Exports = [_ | _]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Move all the top level methods and data definitions into the wrapper
    % class, and then fix all the references so that they refer to their new
    % names.
    %
:- pred il_transform_mlds(mlds::in, mlds::out) is det.

il_transform_mlds(MLDS0, MLDS) :-
    MLDS0 = mlds(ModuleName, ForeignCodeMap, TopLevelImports,
        GlobalData0, Defns0, InitPreds, FinalPreds, ExportedEnums),

    map.values(ForeignCodeMap, ForeignCodes),
    ForeignCodeExportLists =
        list.map(project_foreign_code_export, ForeignCodes),
    ForeignCodeExports = list.condense(ForeignCodeExportLists),

    % Generate the exports for this file, they will be placed into
    % class methods inside the wrapper class.
    list.map(mlds_export_to_mlds_defn, ForeignCodeExports, ExportDefns),

    % We take all the definitions out of the global data field of the MLDS.
    ml_global_data_get_all_global_defns(GlobalData0,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap, GlobalDefns),
    expect(map.is_empty(ScalarCellGroupMap), $module, $pred,
        "nonempty ScalarCellGroupMap"),
    expect(map.is_empty(VectorCellGroupMap), $module, $pred,
        "nonempty VectorCellGroupMap"),
    Defns1 = GlobalDefns ++ Defns0 ++ ExportDefns,
    GlobalData = ml_global_data_init(do_not_use_common_cells,
        do_not_have_unboxed_floats),

    IsFunctionOrData =
        (pred(D::in) is semidet :-
            ( D = mlds_defn(_, _, _, mlds_function(_, _, _, _, _))
            ; D = mlds_defn(_, _, _, mlds_data(_, _, _))
            )
        ),
    list.filter(IsFunctionOrData, Defns1, MercuryCodeDefns, OtherDefns),

    WrapperClass = wrapper_class(list.map(rename_defn, MercuryCodeDefns)),
    % XXX We are we renaming OtherDefns? Its definitions are not being wrapped
    % in a class.
    WrappedOtherDefns = list.map(rename_defn, OtherDefns),
    % Note that ILASM requires that the type definitions in WrappedOtherDefns
    % must precede the references to those types in WrapperClass.
    Defns = WrappedOtherDefns ++ [WrapperClass],

    MLDS = mlds(ModuleName, ForeignCodeMap, TopLevelImports,
        GlobalData, Defns, InitPreds, FinalPreds, ExportedEnums).

:- func project_foreign_code_export(mlds_foreign_code) =
    list(mlds_pragma_export).

project_foreign_code_export(mlds_foreign_code(_, _, _, Exports)) = Exports.

:- func wrapper_class(list(mlds_defn)) = mlds_defn.

wrapper_class(Members) =
    mlds_defn(
        entity_export(wrapper_class_name),
        mlds_make_context(term.context_init),
        ml_gen_type_decl_flags,
        mlds_class(mlds_class_defn(mlds_package, [], [], [], [], [], Members))
    ).

%-----------------------------------------------------------------------------%
%
% Rename the relevant components of the definition (such as qualified var
% names) to reflect the wrapper class we are adding around the definition.
%

:- func rename_defn(mlds_defn) = mlds_defn.

rename_defn(Defn0) = Defn :-
    Defn0 = mlds_defn(Name, Context, Flags, Entity0),
    (
        Entity0 = mlds_data(Type, Initializer, GCStatement),
        Entity = mlds_data(Type,
            rename_initializer(Initializer),
            rename_gc_statement(GCStatement))
    ;
        Entity0 = mlds_function(MaybePredProcId, Params, FunctionBody0,
            Attributes, EnvVarNames),
        (
            FunctionBody0 = body_defined_here(Stmt),
            FunctionBody = body_defined_here(rename_statement(Stmt))
        ;
            FunctionBody0 = body_external,
            FunctionBody = body_external
        ),
        Entity = mlds_function(MaybePredProcId, Params, FunctionBody,
            Attributes, EnvVarNames)
    ;
        Entity0 = mlds_class(ClassDefn0),
        ClassDefn0 = mlds_class_defn(Kind, Imports, Inherits, Implements,
            TypeParams, Ctors0, Members0),
        Ctors = list.map(rename_defn, Ctors0),
        Members = list.map(rename_defn, Members0),
        ClassDefn = mlds_class_defn(Kind, Imports, Inherits, Implements,
            TypeParams, Ctors, Members),
        Entity = mlds_class(ClassDefn)
    ),
    Defn = mlds_defn(Name, Context, Flags, Entity).

:- func rename_maybe_statement(maybe(statement)) = maybe(statement).

rename_maybe_statement(no) = no.
rename_maybe_statement(yes(Stmt)) = yes(rename_statement(Stmt)).

:- func rename_gc_statement(mlds_gc_statement) = mlds_gc_statement.

rename_gc_statement(gc_no_stmt) = gc_no_stmt.
rename_gc_statement(gc_trace_code(Stmt))
    = gc_trace_code(rename_statement(Stmt)).
rename_gc_statement(gc_initialiser(Stmt))
    = gc_initialiser(rename_statement(Stmt)).

:- func rename_statement(statement) = statement.

rename_statement(statement(ml_stmt_block(Defns, Stmts), Context))
    = statement(ml_stmt_block(list.map(rename_defn, Defns),
        list.map(rename_statement, Stmts)), Context).
rename_statement(statement(ml_stmt_while(Kind, Rval, Loop), Context))
    = statement(ml_stmt_while(Kind, rename_rval(Rval), rename_statement(Loop)),
        Context).
rename_statement(statement(ml_stmt_if_then_else(Rval, Then, MaybeElse),
        Context)) =
    statement(ml_stmt_if_then_else(rename_rval(Rval),
        rename_statement(Then),
        rename_maybe_statement(MaybeElse)), Context).
rename_statement(statement(ml_stmt_switch(Type, Rval, Range, Cases, Default0),
        Context))
    = statement(ml_stmt_switch(Type, rename_rval(Rval), Range,
        list.map(rename_switch_case, Cases), Default), Context) :-
    (
        Default0 = default_is_unreachable,
        Default = default_is_unreachable
    ;
        Default0 = default_do_nothing,
        Default = default_do_nothing
    ;
        Default0 = default_case(Stmt),
        Default = default_case(rename_statement(Stmt))
    ).
rename_statement(statement(ml_stmt_label(Label), Context))
    = statement(ml_stmt_label(Label), Context).
rename_statement(statement(ml_stmt_goto(Label), Context))
    = statement(ml_stmt_goto(Label), Context).
rename_statement(statement(ml_stmt_computed_goto(Rval, Labels), Context))
    = statement(ml_stmt_computed_goto(rename_rval(Rval), Labels), Context).

rename_statement(statement(
        ml_stmt_call(Signature, Rval, MaybeThis0, Args, Results, TailCall),
        Context))
    = statement(ml_stmt_call(Signature, rename_rval(Rval),
        MaybeThis, list.map(rename_rval, Args),
        list.map(rename_lval, Results), TailCall), Context) :-
    (
        MaybeThis0 = yes(Self),
        MaybeThis = yes(rename_rval(Self))
    ;
        MaybeThis0 = no,
        MaybeThis = no
    ).

rename_statement(statement(ml_stmt_return(Vals), Context))
    = statement(ml_stmt_return(Vals), Context).
rename_statement(statement(ml_stmt_try_commit(Lval, Try, Handler), Context))
    = statement(ml_stmt_try_commit(rename_lval(Lval), rename_statement(Try),
        rename_statement(Handler)), Context).
rename_statement(statement(ml_stmt_do_commit(Rval), Context))
    = statement(ml_stmt_do_commit(rename_rval(Rval)), Context).
rename_statement(statement(ml_stmt_atomic(Stmt), Context))
    = statement(ml_stmt_atomic(rename_atomic(Stmt)), Context).

:- func rename_switch_case(mlds_switch_case) = mlds_switch_case.

rename_switch_case(Case0) = Case :-
    Case0 = mlds_switch_case(FirstCond0, LaterConds0, Stmt0),
    FirstCond = rename_cond(FirstCond0),
    LaterConds = list.map(rename_cond, LaterConds0),
    Stmt = rename_statement(Stmt0),
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- func rename_cond(mlds_case_match_cond) = mlds_case_match_cond.

rename_cond(match_value(Rval)) = match_value(rename_rval(Rval)).
rename_cond(match_range(RvalA, RvalB))
    = match_range(rename_rval(RvalA), rename_rval(RvalB)).

:- func rename_atomic(mlds_atomic_statement) = mlds_atomic_statement.

rename_atomic(comment(S)) = comment(S).
rename_atomic(assign(L, R)) = assign(rename_lval(L), rename_rval(R)).
rename_atomic(assign_if_in_heap(L, R)) =
    assign(rename_lval(L), rename_rval(R)).
rename_atomic(delete_object(O)) = delete_object(rename_rval(O)).
rename_atomic(new_object(L, Tag, ExplicitSecTag, Type, MaybeSize, Ctxt, Args,
        Types, MayUseAtomic, AllocId))
    = new_object(rename_lval(L), Tag, ExplicitSecTag, Type, MaybeSize,
        Ctxt, list.map(rename_rval, Args), Types, MayUseAtomic, AllocId).
rename_atomic(gc_check) = gc_check.
rename_atomic(mark_hp(L)) = mark_hp(rename_lval(L)).
rename_atomic(restore_hp(R)) = restore_hp(rename_rval(R)).
rename_atomic(trail_op(T)) = trail_op(T).
rename_atomic(inline_target_code(L, Cs)) = inline_target_code(L, Cs).
rename_atomic(outline_foreign_proc(F, Vs, Ls, S))
    = outline_foreign_proc(F, Vs, Ls, S).

:- func rename_rval(mlds_rval) = mlds_rval.

rename_rval(ml_lval(Lval)) = ml_lval(rename_lval(Lval)).
rename_rval(ml_mkword(Tag, Rval)) = ml_mkword(Tag, rename_rval(Rval)).
rename_rval(ml_const(Const)) = ml_const(rename_const(Const)).
rename_rval(ml_unop(Op, Rval)) = ml_unop(Op, rename_rval(Rval)).
rename_rval(ml_binop(Op, RvalA, RvalB)) =
    ml_binop(Op, rename_rval(RvalA), rename_rval(RvalB)).
rename_rval(ml_mem_addr(Lval)) = ml_mem_addr(rename_lval(Lval)).
rename_rval(ml_scalar_common(ScalarCommon)) = ml_scalar_common(ScalarCommon).
rename_rval(ml_vector_common_row(VectorCommon, RowRval)) =
    ml_vector_common_row(VectorCommon, rename_rval(RowRval)).
rename_rval(ml_self(Type)) = ml_self(Type).

:- func rename_const(mlds_rval_const) = mlds_rval_const.

rename_const(mlconst_true) = mlconst_true.
rename_const(mlconst_false) = mlconst_false.
rename_const(mlconst_int(I)) = mlconst_int(I).
rename_const(mlconst_enum(I, T)) = mlconst_enum(I, T).
rename_const(mlconst_char(C)) = mlconst_char(C).
rename_const(mlconst_foreign(L, F, T)) = mlconst_foreign(L, F, T).
rename_const(mlconst_float(F)) = mlconst_float(F).
rename_const(mlconst_string(S)) = mlconst_string(S).
rename_const(mlconst_multi_string(S)) = mlconst_multi_string(S).
rename_const(mlconst_named_const(NC)) = mlconst_named_const(NC).
rename_const(mlconst_code_addr(C)) = mlconst_code_addr(rename_code_addr(C)).
rename_const(mlconst_data_addr(A)) = mlconst_data_addr(rename_data_addr(A)).
rename_const(mlconst_null(T)) = mlconst_null(T).

:- func rename_code_addr(mlds_code_addr) = mlds_code_addr.

rename_code_addr(code_addr_proc(Label, Signature))
    = code_addr_proc(rename_proc_label(Label), Signature).
rename_code_addr(code_addr_internal(Label, Seq, Signature))
    = code_addr_internal(rename_proc_label(Label), Seq, Signature).

rename_proc_label(qual(Module, _QualKind, Name))
    = qual(mlds_append_wrapper_class(Module), type_qual, Name).

:- func rename_lval(mlds_lval) = mlds_lval.

rename_lval(ml_field(Tag, Address, FieldName, FieldType, PtrType))
    = ml_field(Tag, rename_rval(Address),
        rename_field_id(FieldName), FieldType, PtrType).
rename_lval(ml_mem_ref(Rval, Type)) = ml_mem_ref(rename_rval(Rval), Type).
rename_lval(ml_global_var_ref(Ref)) = ml_global_var_ref(Ref).
rename_lval(ml_var(Var, Type)) = ml_var(rename_mlds_var(Var, Type), Type).

:- func rename_field_id(mlds_field_id) = mlds_field_id.

rename_field_id(ml_field_offset(Rval)) = ml_field_offset(rename_rval(Rval)).
rename_field_id(ml_field_named(Name, Type)) = ml_field_named(Name, Type).

:- func rename_initializer(mlds_initializer) = mlds_initializer.

rename_initializer(init_obj(Rval)) = init_obj(rename_rval(Rval)).
rename_initializer(init_struct(Type, Inits))
    = init_struct(Type, list.map(rename_initializer, Inits)).
rename_initializer(init_array(Inits))
    = init_array(list.map(rename_initializer, Inits)).
rename_initializer(no_initializer) = no_initializer.

    % We need to append a wrapper class qualifier so that we access
    % the RTTI fields correctly.
    %
:- func rename_data_addr(mlds_data_addr) = mlds_data_addr.

rename_data_addr(data_addr(ModuleName, Name))
    = data_addr(mlds_append_wrapper_class(ModuleName), Name).

    % We need to append a wrapper class qualifier so that we refer to the
    % methods of the wrapper class.
    %
:- func rename_proc_label(mlds_qualified_proc_label)
    = mlds_qualified_proc_label.

    % Again append a wrapper class qualifier to the var name.
    %
:- func rename_mlds_var(mlds_var, mlds_type) = mlds_var.

rename_mlds_var(qual(ModuleName, _QualKind, Name), _Type)
    = qual(mlds_append_wrapper_class(ModuleName), type_qual, Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mlds_defn_to_ilasm_decl(mlds_defn::in, il_decl::out,
    il_info::in, il_info::out) is det.

mlds_defn_to_ilasm_decl(mlds_defn(Name, Context, Flags0, Data), Decl, !Info) :-
    % IL supports top-level (i.e. "global") function definitions and
    % data definitions, but they're not part of the CLS.
    % Since they are not part of the CLS, we don't generate them,
    % and so there's no need to handle them here.
    (
        Data = mlds_data(_Type, _Init, _GC),
        sorry($module, $pred, "top level data definition!")
    ;
        Data = mlds_function(_MaybePredProcId, _Params, _MaybeStmts, _Attrs,
            _EnvVarNames),
        sorry($module, $pred, "top level function definition!")
    ;
        Data = mlds_class(ClassDefn),
        il_info_new_class(ClassDefn, !Info),

        generate_class_body(Name, Context, ClassDefn, ClassName, EntityName,
            Extends, Interfaces, MethodsAndFieldsAndCtors, !Info),

        % Only the wrapper class needs to have the initialization instructions
        % executed by the class constructor.
        ( EntityName = wrapper_class_name ->
            Imports = !.Info ^ imports,
            InitInstrs = cord.list(!.Info ^ init_instrs),
            AllocInstrs = cord.list(!.Info ^ alloc_instrs),

            % Generate a field that records whether we have finished
            % RTTI initialization.
            generate_rtti_initialization_field(ClassName,
                AllocDoneFieldRef, AllocDoneField),

            % Generate a class constructor.
            make_class_constructor_class_member(AllocDoneFieldRef,
                Imports, AllocInstrs, InitInstrs, CCtor, !Info),

            % The declarations in this class.
            MethodDecls = [AllocDoneField, CCtor | MethodsAndFieldsAndCtors]
        ;
            MethodDecls = MethodsAndFieldsAndCtors
        ),
        % XXX Needed to work around a bug where private classes aren't
        % accessible from classes in the same assembly when that assembly
        % is created by al.exe. This occurs for nondet environment classes
        % in the mercury std library.
        ( ClassName = structured_name(assembly("mercury"), _, _) ->
            Flags = set_access(Flags0, acc_public)
        ;
            Flags = Flags0
        ),
        Decl = ildecl_class(decl_flags_to_classattrs(Flags), EntityName,
            Extends, Interfaces, MethodDecls)
    ).

:- pred generate_class_body(mlds_entity_name::in, mlds_context::in,
    mlds_class_defn::in, ilds.class_name::out, ilds.id::out,
    extends::out, implements::out, list(class_member)::out,
    il_info::in, il_info::out) is det.

generate_class_body(Name, Context, ClassDefn, ClassName, EntityName, Extends,
        Interfaces, ClassMembers, !Info) :-
    EntityName = entity_name_to_ilds_id(Name),
    ClassDefn = mlds_class_defn(Kind, _Imports, Inherits, Implements,
        _TypeParams, Ctors0, Members),
    Parent - Extends = generate_parent_and_extends(!.Info ^ il_data_rep,
        Kind, Inherits),
    Interfaces = implements(
        list.map(interface_id_to_class_name, Implements)),
    ClassName = class_name(!.Info ^ module_name, EntityName),
    list.map_foldl(generate_method(ClassName, no), Members,
        MethodsAndFields, !Info),
    Ctors = maybe_add_empty_ctor(Ctors0, Kind, Context),
    list.map_foldl(generate_method(ClassName, yes(Parent)), Ctors,
        IlCtors, !Info),
    ClassMembers = IlCtors ++ MethodsAndFields.

    % For IL, every class needs a constructor, otherwise you can't use the
    % newobj instruction to allocate instances of the class. So if a class
    % doesn't already have one, we add an empty one.
    %
:- func maybe_add_empty_ctor(list(mlds_defn), mlds_class_kind, mlds_context) =
    list(mlds_defn).

maybe_add_empty_ctor(Ctors0, Kind, Context) = Ctors :-
    (
        Kind = mlds_class,
        Ctors0 = []
    ->
        % Generate an empty block for the body of the constructor.
        Stmt = statement(ml_stmt_block([], []), Context),

        Attributes = [],
        EnvVarNames = set.init,
        Ctor = mlds_function(no, mlds_func_params([], []),
            body_defined_here(Stmt), Attributes, EnvVarNames),
        CtorFlags = init_decl_flags(acc_public, per_instance, non_virtual,
            overridable, modifiable, concrete),

        CtorDefn = mlds_defn(entity_export(".ctor"), Context, CtorFlags, Ctor),
        Ctors = [CtorDefn]
    ;
        Ctors = Ctors0
    ).

:- func generate_parent_and_extends(il_data_rep, mlds_class_kind,
    list(mlds_class_id)) = pair(ilds.class_name, extends).

generate_parent_and_extends(DataRep, Kind, Inherits) = Parent - Extends :-
    (
        Inherits = [],
        (
            Kind = mlds_struct,
            Parent = il_generic_valuetype_name,
            Extends = extends(Parent)
        ;
            Kind = mlds_enum,
            Parent = il_generic_enum_name,
            Extends = extends(Parent)
        ;
            ( Kind = mlds_class
            ; Kind = mlds_package
            ; Kind = mlds_interface
            ),
            Parent = il_generic_class_name,
            Extends = extends_nothing
        )
    ;
        Inherits = [Parent0 | Rest],
        (
            Rest = [],
            Parent = mlds_type_to_ilds_class_name(DataRep, Parent0),
            Extends = extends(Parent)
        ;
            Rest = [_ | _],
            unexpected($module, $pred, "multiple inheritance not supported")
        )
    ).

class_name(Module, Name)
    = append_toplevel_class_name(mlds_module_name_to_class_name(Module), Name).

:- func decl_flags_to_classattrs(mlds_decl_flags) = list(ilasm.classattr).

decl_flags_to_classattrs(Flags) =
        list.condense([Access, decl_flags_to_classattrs_2(Flags)]) :-
    AccessFlag = access(Flags),
    (
        AccessFlag = acc_public,
        Access = [public]
    ;
        AccessFlag = acc_protected,
        unexpected($module, $pred, "protected access flag")
    ;
        AccessFlag = acc_private,
        Access = [private]
    ;
        AccessFlag = acc_default,
        % To make members of the private class accessible to other types
        % in the assembly, set their access to be default or public.
        Access = [private]
    ;
        AccessFlag = acc_local,
        unexpected($module, $pred, "local access flag")
    ).

:- func decl_flags_to_nestedclassattrs(mlds_decl_flags) =
    list(ilasm.classattr).

decl_flags_to_nestedclassattrs(Flags)
        = list.condense([Access, decl_flags_to_classattrs_2(Flags)]) :-
    AccessFlag = access(Flags),
    (
        AccessFlag = acc_public,
        Access = [nestedpublic]
    ;
        AccessFlag = acc_protected,
        Access = [nestedfamily]
    ;
        AccessFlag = acc_private,
        Access = [nestedprivate]
    ;
        AccessFlag = acc_default,
        Access = [nestedassembly]
    ;
        AccessFlag = acc_local,
        unexpected($module, $pred, "local access flag")
    ).

:- func decl_flags_to_classattrs_2(mlds_decl_flags) = list(ilasm.classattr).

decl_flags_to_classattrs_2(Flags) = ClassAttrs :-
    OverridabilityFlag = overridability(Flags),
    (
        OverridabilityFlag = overridable,
        Overridability = []
    ;
        OverridabilityFlag = sealed,
        Overridability = [sealed]
    ),
    AbstractnessFlag = abstractness(Flags),
    (
        AbstractnessFlag = concrete,
        Abstractness = []
    ;
        AbstractnessFlag = abstract,
        Abstractness = [abstract]
    ),
    ClassAttrs = list.condense([Overridability, Abstractness]).

:- func decl_flags_to_methattrs(mlds_decl_flags) = list(ilasm.methattr).

decl_flags_to_methattrs(Flags)
        = list.condense([Access, PerInstance, Virtuality,
            Overridability, Abstractness]) :-
    AccessFlag = access(Flags),
    (
        AccessFlag = acc_public,
        Access = [public]
    ;
        AccessFlag = acc_protected,
        Access = [family]
    ;
        AccessFlag = acc_private,
        Access = [private]
    ;
        AccessFlag = acc_default,
        Access = [assembly]
    ;
        AccessFlag = acc_local,
        unexpected($module, $pred, "local access flag")
    ),
    PerInstanceFlag = per_instance(Flags),
    (
        PerInstanceFlag = one_copy,
        PerInstance = [static]
    ;
        PerInstanceFlag = per_instance,
        PerInstance = []
    ),
    VirtualityFlag = virtuality(Flags),
    (
        VirtualityFlag = non_virtual,
        Virtuality = []
    ;
        VirtualityFlag = virtual,
        Virtuality = [virtual]
    ),
    OverridabilityFlag = overridability(Flags),
    (
        OverridabilityFlag = overridable,
        Overridability = []
    ;
        OverridabilityFlag = sealed,
        Overridability = [final]
    ),
    AbstractnessFlag = abstractness(Flags),
    (
        AbstractnessFlag = concrete,
        Abstractness = []
    ;
        AbstractnessFlag = abstract,
        Abstractness = [abstract]
    ).

:- func decl_flags_to_fieldattrs(mlds_decl_flags) = list(ilasm.fieldattr).

decl_flags_to_fieldattrs(Flags)
        = list.condense([Access, PerInstance, Constness]) :-
    AccessFlag = access(Flags),
    (
        AccessFlag = acc_public,
        Access = [public]
    ;
        AccessFlag = acc_protected,
        Access = [family]
    ;
        AccessFlag = acc_private,
        Access = [private]
    ;
        AccessFlag = acc_default,
        Access = [assembly]
    ;
        AccessFlag = acc_local,
        % Access = [private]
        unexpected($module, $pred, "local access flag")
    ),
    PerInstanceFlag = per_instance(Flags),
    (
        PerInstanceFlag = one_copy,
        PerInstance = [static]
    ;
        PerInstanceFlag = per_instance,
        PerInstance = []
    ),
    ConstnessFlag = constness(Flags),
    (
        ConstnessFlag = modifiable,
        Constness = []
    ;
        ConstnessFlag = const,
        Constness = [initonly]
    ).

:- func entity_name_to_ilds_id(mlds_entity_name) = ilds.id.

entity_name_to_ilds_id(entity_export(Name)) = Name.
entity_name_to_ilds_id(entity_function(PredLabel, ProcId, MaybeSeqNum, _))
        = Name :-
    predlabel_to_ilds_id(PredLabel, ProcId, MaybeSeqNum, Name).
entity_name_to_ilds_id(entity_type(Name, Arity))
    = string.format("%s_%d", [s(Name), i(Arity)]).
entity_name_to_ilds_id(entity_data(DataName)) = Name :-
    mangle_dataname(DataName, Name).

:- func interface_id_to_class_name(mlds_interface_id) = ilds.class_name.

interface_id_to_class_name(_) = Result :-
    % XXX
    ( semidet_succeed ->
        sorry($module, $pred, "NYI")
    ;
        Result = structured_name(assembly("XXX"), [], [])
    ).

%-----------------------------------------------------------------------------%

:- pred generate_method(ilds.class_name::in, maybe(ilds.class_name)::in,
    mlds_defn::in, class_member::out, il_info::in, il_info::out) is det.

generate_method(ClassName, _, mlds_defn(Name, Context, Flags, Entity),
        ClassMember, !Info) :-
    Entity = mlds_data(Type, DataInitializer, _GCStatement),

    FieldName = entity_name_to_ilds_id(Name),

    Attrs = decl_flags_to_fieldattrs(Flags),

    % Generate instructions to initialize this data. There are two sorts of
    % instructions, instructions to allocate the data structure, and
    % instructions to initialize it. See the comments about class constructors
    % to find out why we do this.
    data_initializer_to_instrs(DataInitializer, Type, AllocInstrsTree,
        InitInstrTree, !Info),

    % Make a field reference for the field.
    DataRep = !.Info ^ il_data_rep,
    ILType = mlds_type_to_ilds_type(DataRep, Type),
    FieldRef = make_fieldref(ILType, ClassName, FieldName),

    % If we had to allocate memory, the code we generate looks like this:
    %
    %   // allocation for foo
    %   ... allocation instructions ...
    %   stsfld thisclass::foo
    %
    %
    %   // initializer for foo
    %   ldsfld thisclass::foo
    %   ... initialization code ...
    %   pop
    %
    % The final pop is necessary because the init code will leave the field
    % on the stack, but we don't need it anymore (and we already set the field
    % when we allocated it).
    %
    % If no memory had to be allocated, the code is a bit simpler.
    %
    %   // allocation for foo
    %   nothing here!
    %
    %   // initializer for foo
    %   ... initialization code ...
    %   stsfld thisclass::foo
    %
    % Note that here we have to set the field.

    ( cord.is_empty(AllocInstrsTree) ->
        StoreAllocTree = empty,
        StoreInitTree = singleton(stsfld(FieldRef)),
        LoadTree = empty
    ;
        StoreAllocTree = singleton(stsfld(FieldRef)),
        StoreInitTree = singleton(pop),
        LoadTree = singleton(ldsfld(FieldRef))
    ),

    % Add a store after the alloc instrs (if necessary)
    AllocInstrs = cord.list(
        context_node(Context) ++
        comment_node(string.append("allocation for ", FieldName)) ++
        AllocInstrsTree ++
        StoreAllocTree),

    % Add a load before the init instrs (if necessary)
    InitInstrs = cord.list(
        context_node(Context) ++
        comment_node(string.append("initializer for ", FieldName)) ++
        LoadTree ++
        InitInstrTree ++
        StoreInitTree),

    % Add these instructions to the lists of allocation/initialization
    % instructions. They will be put into the class constructor later.
    il_info_add_alloc_instructions(AllocInstrs, !Info),
    il_info_add_init_instructions(InitInstrs, !Info),

    MaybeOffset = no,
    Initializer = none,

    ClassMember = member_field(Attrs, ILType, FieldName, MaybeOffset,
        Initializer).

generate_method(_, IsCons, mlds_defn(Name, Context, Flags, Entity),
        ClassMember, !Info) :-
    Entity = mlds_function(_MaybePredProcId, Params, MaybeStatement,
        Attributes, EnvVarNames),

    expect(set.is_empty(EnvVarNames), $module, $pred, "EnvVarNames"),

    il_info_get_module_name(!.Info, ModuleName),

%   XXX We formerly returned a list of definitions, so we could put
%   this term in a comment term, so we cannot currently do this.
%
%   % Generate a term (we use it to emit the complete method definition
%   % as a comment, which is nice for debugging).
%   term.type_to_term(defn(Name, Context, Flags, Entity), _MLDSDefnTerm),

    % Generate the signature
    Params = mlds_func_params(Args, Returns),
    ILArgs = list.map(mlds_arg_to_il_arg, Args),
    DataRep = !.Info ^ il_data_rep,
    ILSignature = params_to_il_signature(DataRep, ModuleName, Params),

    % Generate the name
    (
        IsCons = yes(ParentClass),
        MemberName = ctor,
        CSharpMemberName = ctor,
        CtorInstrs = [load_this,
            call(methoddef(call_conv(yes, default), void,
            class_member_name(ParentClass, ctor), []))]
    ;
        IsCons = no,
        (
            Name = entity_function(PredLabel, ProcId, MaybeSeqNum, _PredId),
            predlabel_to_ilds_id(PredLabel, ProcId, MaybeSeqNum, MemberName0),
            predlabel_to_csharp_id(PredLabel, ProcId, MaybeSeqNum,
                    CSharpMemberName0),
            MemberName = id(MemberName0),
            CSharpMemberName = id(CSharpMemberName0)
        ;
            Name = entity_export(ExportName),
            MemberName = id(ExportName),
            CSharpMemberName = id(ExportName)
        ;
            ( Name = entity_type(_, _)
            ; Name = entity_data(_)
            ),
            unexpected($module, $pred, "IL procedure is not a function")
        ),
        CtorInstrs = []
    ),

    Attrs = decl_flags_to_methattrs(Flags),

    % Initialize the IL info with this method info.
    il_info_new_method(ILArgs, ILSignature, MemberName, CSharpMemberName,
            !Info),

    % Start a new block, which we will use to wrap up the entire method.
    il_info_get_next_block_id(BlockId, !Info),

    % Generate the code of the statement.
    (
        MaybeStatement = body_defined_here(Statement),
        statement_to_il(Statement, InstrsTree1, !Info),
        % Need to insert a ret for functions returning void (MLDS doesn't).
        (
            Returns = [],
            MaybeRet = singleton(ret)
        ;
            Returns = [_ | _],
            MaybeRet = empty
        )
    ;
        MaybeStatement = body_external,

        % XXX The external reference must currently reside in the
        % C# file associated with this file.  This is very hackish.
        ForeignLangs = !.Info ^ file_foreign_langs,
        !Info ^ file_foreign_langs :=
            set.insert(ForeignLangs, lang_csharp),

        mangle_dataname_module(no, ModuleName, NewModuleName),
        ClassName = mlds_module_name_to_class_name(NewModuleName),

        ILSignature = signature(_, ILRetType, ILParams),
        TypeParams = il_method_params_to_il_types(ILParams),

        list.map_foldl(
            (pred(_::in, Instr::out, Num::in, Num+1::out) is det :-
                Instr = ldarg(index(Num))
            ), TypeParams, LoadInstrs, 0, _),
        InstrsTree1 =
            comment_node("external -- call handwritten version") ++
            from_list(LoadInstrs) ++
            singleton(call(get_static_methodref(ClassName,
                CSharpMemberName, ILRetType, TypeParams))),
        MaybeRet = singleton(ret)
    ),

    % Retrieve the locals, put them in the enclosing scope.
    il_info_get_locals_list(!.Info, Locals),
    InstrsTree2 =
        context_node(Context) ++
        from_list(CtorInstrs) ++
        context_node(Context) ++
        singleton(start_block(bt_scope(Locals), BlockId)) ++
        InstrsTree1 ++
        MaybeRet ++
        singleton(end_block(bt_scope(Locals), BlockId)),

    % If this is main, add the entrypoint, set a flag, wrap the code
    % in an exception handler and call the initialization instructions
    % in the cctor of this module.
    (
        Name = entity_function(MainPredLabel, _ProcId, no, _),
        MainPredLabel = mlds_user_pred_label(pf_predicate, no, "main", 2,
            model_det, no)
    ->
        EntryPoint = [entrypoint],
        !Info ^ has_main := has_main,

        il_info_get_next_block_id(InnerTryBlockId, !Info),
        il_info_get_next_block_id(OuterTryBlockId, !Info),
        il_info_get_next_block_id(InnerCatchBlockId, !Info),
        il_info_get_next_block_id(OuterCatchBlockId, !Info),
        il_info_make_next_label(DoneLabel, !Info),

        % Replace all the returns with leave instructions; as a side effect,
        % this means that we can no longer have any tail calls, so replace them
        % with nops.
        RenameRets = (func(I) =
            (if (I = ret) then
                leave(label_target(DoneLabel))
            else if (I = tailcall) then
                nop
            else
                I
            )
        ),

        UnivSymName = qualified(unqualified("univ"), "univ"),
        UnivMercuryType = defined_type(UnivSymName, [], kind_star),
        UnivMLDSType = mercury_type(UnivMercuryType,
            ctor_cat_user(cat_user_general),
            non_foreign_type(UnivMercuryType)),
        UnivType = mlds_type_to_ilds_type(DataRep, UnivMLDSType),

        MercuryExceptionClassName = mercury_runtime_name(["Exception"]),

        ExceptionClassName = structured_name(il_system_assembly_name,
            ["System", "Exception"], []),

        FieldRef = make_fieldref(UnivType, MercuryExceptionClassName,
            "mercury_exception"),

        ConsoleWriteName = class_member_name(
            structured_name(il_system_assembly_name,
                ["System", "Console"], []),
            id("Write")),

        UncaughtExceptionName = class_member_name(
            mercury_library_wrapper_class_name(["exception"]),
                id("ML_report_uncaught_exception")),

        WriteString = methoddef(call_conv(no, default),
            void, ConsoleWriteName, [il_string_type]),
        WriteUncaughtException = methoddef(call_conv(no, default),
            void, UncaughtExceptionName, [UnivType]),
        WriteObject = methoddef(call_conv(no, default),
            void, ConsoleWriteName, [il_generic_type]),

        % A code block to catch any exception at all.

        CatchAnyException =
            from_list([
                start_block(bt_catch(ExceptionClassName), OuterCatchBlockId),
                ldstr("\nUncaught system exception: \n"),
                call(WriteString),
                call(WriteObject),
                ldc(int32, i(1)),
                call(il_set_exit_code),
                leave(label_target(DoneLabel)),
                end_block(bt_catch(ExceptionClassName), OuterCatchBlockId)
            ]),

        % Code to catch Mercury exceptions.
        CatchUserException =
            from_list([
                start_block(bt_catch(MercuryExceptionClassName),
                    InnerCatchBlockId),
                ldfld(FieldRef),
                call(WriteUncaughtException),

                ldc(int32, i(1)),
                call(il_set_exit_code),

                leave(label_target(DoneLabel)),
                end_block(bt_catch(MercuryExceptionClassName),
                    InnerCatchBlockId)
            ]),

        % Wrap an exception handler around the main code. This allows us
        % to debug programs remotely without a window popping up asking
        % how you wish to debug. Pressing the cancel button on this window
        % is a bit difficult remotely.
        %
        % Inside this exception handler, we catch any exceptions and
        % print them.
        %
        % We nest the Mercury exception handler so that any exceptions thrown
        % in ML_report_uncaught_exception will be caught by the outer
        % (more general) exception handler.
        %
        % try {
        %   try {
        %       ... main instructions ...
        %   }
        %   catch (mercury.runtime.Exception me) {
        %       ML_report_uncaught_exception(me);
        %       System.Environment.ExitCode = 1;
        %   }
        % }
        % catch (System.Exception e) {
        %   System.Console.Write(e);
        %   System.Environment.ExitCode = 1;
        % }

        InstrsTree =
            from_list([
                % outer try block
                start_block(bt_try, OuterTryBlockId),

                % inner try block
                start_block(bt_try, InnerTryBlockId)
            ]) ++
            cord.map(RenameRets, InstrsTree2) ++
            from_list([
                leave(label_target(DoneLabel)),
                end_block(bt_try, InnerTryBlockId)
            ]) ++
            % inner catch block
            CatchUserException ++
            from_list([
                leave(label_target(DoneLabel)),
                end_block(bt_try, OuterTryBlockId)
            ]) ++
            % outer catch block
            CatchAnyException ++
            from_list([
                label(DoneLabel),
                ret
            ])
    ;
        EntryPoint = [],
        InstrsTree = InstrsTree2
    ),

    % Generate the entire method contents.
    DebugIlAsm = !.Info ^ debug_il_asm,
    VerifiableCode = !.Info ^ verifiable_code,
    MethodBody = make_method_defn(DebugIlAsm, VerifiableCode,
        InstrsTree),
    CustomAttributes = attributes_to_custom_attributes(DataRep,
        Attributes),
    list.condense([EntryPoint, CustomAttributes, MethodBody],
        MethodContents),

    ClassMember = member_method(methodhead(Attrs, MemberName,
        ILSignature, []), MethodContents).

generate_method(_, _, mlds_defn(Name, Context, Flags, Entity), ClassMember,
        !Info) :-
    Entity = mlds_class(ClassDefn),
    generate_class_body(Name, Context, ClassDefn, _ClassName, EntityName,
        Extends, Interfaces, ClassMembers, !Info),
    ClassMember = member_nested_class(decl_flags_to_nestedclassattrs(Flags),
        EntityName, Extends, Interfaces, ClassMembers).

%-----------------------------------------------------------------------------%

:- func attributes_to_custom_attributes(il_data_rep, list(mlds_attribute))
    = list(method_body_decl).

attributes_to_custom_attributes(DataRep, Attrs) =
    list.map(attribute_to_custom_attribute(DataRep), Attrs).

:- func attribute_to_custom_attribute(il_data_rep, mlds_attribute)
    = method_body_decl.

attribute_to_custom_attribute(DataRep, custom(MLDSType))
        = custom(CustomDecl) :-
    ClassName = mlds_type_to_ilds_class_name(DataRep, MLDSType),
    MethodRef = get_constructor_methoddef(ClassName, []),
    CustomDecl = custom_decl(methodref(MethodRef), no, no_initalizer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % MLDS exports are converted into forwarding functions, which are
    % marked as public, are given the specified name, and simply call to
    % the "exported" function.
    %
    % They will be placed inside the "mercury_code" wrapper class with
    % all the other procedures.
    %
    % XXX Much of this code should be generalized and turned into a
    % more general routine for generating MLDS forwarding functions.
    % We could use almost the same approach for outline_foreign_code
    % to generate the forwarding function.
    %
:- pred mlds_export_to_mlds_defn(mlds_pragma_export::in, mlds_defn::out)
    is det.

mlds_export_to_mlds_defn(ExportDefn, Defn) :-
    ExportDefn = ml_pragma_export(Lang, ExportName, EntityName, Params,
        _UnivQTVars, Context),
    EntityName = qual(ModuleName, _QualKind, UnqualName),
    expect(unify(Lang, lang_il), $module, $pred,
        "export for language other than IL."),
    Params = mlds_func_params(Inputs, RetTypes),
    list.map_foldl(
        (pred(RT::in, RV - Lval::out, N0::in, N0 + 1::out) is det :-
            VN = mlds_var_name("returnval" ++ int_to_string(N0), no),
            % We don't need to worry about tracing variables for
            % accurate GC in the IL back-end -- the .NET runtime
            % system itself provides accurate GC.
            GCStatement = gc_no_stmt,
            RV = ml_gen_mlds_var_decl_init(mlds_data_var(VN), RT,
                no_initializer, GCStatement, Context),
            Lval = ml_var(qual(ModuleName, module_qual, VN), RT)
        ), RetTypes, ReturnVars, 0, _),

    EntNameToVarName = (func(EntName) = VarName :-
        ( EntName = entity_data(mlds_data_var(VarName0)) ->
            VarName = qual(ModuleName, module_qual, VarName0)
        ;
            unexpected($module, $pred,
                "exported method has argument without var name")
        )
    ),
    ArgTypes = mlds_get_arg_types(Inputs),
    ArgRvals = list.map(
        (func(mlds_argument(EntName, Type, _GCStatement)) =
                ml_lval(ml_var(VarName, Type)) :-
            VarName = EntNameToVarName(EntName)
        ), Inputs),
    ReturnVarDecls = assoc_list.keys(ReturnVars),
    ReturnLvals = assoc_list.values(ReturnVars),
    ReturnRvals = list.map((func(X) = ml_lval(X)), ReturnLvals),

    Signature = mlds_func_signature(ArgTypes, RetTypes),
    (
        UnqualName = entity_function(PredLabel, ProcId, _MaybeSeq, _PredId),
        CodeRval = ml_const(mlconst_code_addr(code_addr_proc(
            qual(ModuleName, module_qual, mlds_proc_label(PredLabel, ProcId)),
            Signature)))
    ;
        ( UnqualName = entity_type(_, _)
        ; UnqualName = entity_data(_)
        ; UnqualName = entity_export(_)
        ),
        unexpected($module, $pred, "exported entity is not a function")
    ),

    % XXX Should we look for tail calls?
    CallStatement = statement(
        ml_stmt_call(Signature, CodeRval, no, ArgRvals, ReturnLvals,
            ordinary_call), Context),
    ReturnStatement = statement(ml_stmt_return(ReturnRvals), Context),

    Statement = statement(ml_stmt_block(ReturnVarDecls,
        ( ReturnRvals = [] ->
            [CallStatement]
        ;
            [CallStatement, ReturnStatement]
        )
    ), Context),

    Attributes = [],
    EnvVarNames = set.init,
    DefnEntity = mlds_function(no, Params, body_defined_here(Statement),
        Attributes, EnvVarNames),

    Flags = init_decl_flags(acc_public, one_copy, non_virtual, overridable,
        const, concrete),
    Defn = mlds_defn(entity_export(ExportName), Context, Flags, DefnEntity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Code for generating initializers.
%

    % Generate initializer code from an MLDS defn. We are only expecting
    % data defns at this point (local vars), not functions or classes.
    %
:- pred generate_defn_initializer(mlds_defn::in,
    instr_tree::in, instr_tree::out, il_info::in, il_info::out) is det.

generate_defn_initializer(mlds_defn(Name, Context, _DeclFlags, Entity),
        !Tree, !Info) :-
    (
        Name = entity_data(DataName),
        Entity = mlds_data(MLDSType, Initializer, _GCStatement)
    ->
        ( Initializer = no_initializer ->
            true
        ;
            ( DataName = mlds_data_var(VarName) ->
                il_info_get_module_name(!.Info, ModuleName),
                Lval = ml_var(qual(ModuleName, module_qual, VarName),
                    MLDSType),
                get_load_store_lval_instrs(Lval,
                    LoadMemRefInstrs, StoreLvalInstrs, !Info),
                NameString = mangle_mlds_var_name(VarName)
            ;
                LoadMemRefInstrs = throw_unimplemented(
                    "initializer_for_non_var_data_name"),
                StoreLvalInstrs = empty,
                NameString = "unknown"
            ),
            data_initializer_to_instrs(Initializer, MLDSType,
                AllocInstrs, InitInstrs, !Info),
            Comment = "initializer for " ++ NameString,
            !:Tree =
                !.Tree ++
                context_node(Context) ++
                comment_node(Comment) ++
                LoadMemRefInstrs ++
                AllocInstrs ++
                InitInstrs ++
                StoreLvalInstrs
        )
    ;
        unexpected($module, $pred, "defn not data(...) in block")
    ).

    % Initialize this value, leave it on the stack.
    % XXX the code generator doesn't box these values
    % we need to look ahead at them and box them appropriately.
    %
:- pred data_initializer_to_instrs(mlds_initializer::in, mlds_type::in,
    instr_tree::out, instr_tree::out, il_info::in, il_info::out) is det.

data_initializer_to_instrs(init_obj(Rval), _Type, empty, InitInstrs,
        !Info) :-
    load(Rval, InitInstrs, !Info).

    % MLDS structures initializers are assumed to be initialized like
    % structures in C, which means nested elements are actually laid out
    % flat in the structure.
    %
    % So we flatten structures, and then process them as arrays
    % (this may have to be re-visited if used to initialise high-level data).
    %
data_initializer_to_instrs(init_struct(_StructType, InitList0), Type,
        AllocInstrs, InitInstrs, !Info) :-
    InitList = flatten_inits(InitList0),
    data_initializer_to_instrs(init_array(InitList), Type,
        AllocInstrs, InitInstrs, !Info).

    % Put the array allocation in AllocInstrs.
    % For sub-initializations, we don't worry about keeping AllocInstrs
    % and InitInstrs apart, since we are only interested in top level
    % allocations.
data_initializer_to_instrs(init_array(InitList), Type,
        AllocInstrs, InitInstrs, !Info) :-
    % Figure out the array element type.
    DataRep = !.Info ^ il_data_rep,
    ( Type = mlds_array_type(ElemType0) ->
        ElemType = ElemType0,
        ILElemType = mlds_type_to_ilds_type(DataRep, ElemType)
    ;
        % XXX We assume struct fields have type mlds_generic_type
        % This is probably wrong for --high-level-data.
        ElemType = mlds_generic_type,
        ILElemType = il_generic_type
    ),
    ILElemType = il_type(_, ILElemSimpleType),

    % To initialize an array, we generate the following code:
    %   ldc <length of array>
    %   newarr <array element type>
    %
    % Then, for each element in the array:
    %   dup
    %   ldc <index of this element in the array>
    %   ... allocation instructions ...
    %   ... initialization instructions ...
    %   box the value (if necessary)
    %   stelem <array element type>
    %
    % The initialization will leave the array on the stack.
    %
    AllocInstrs = from_list([
        ldc(int32, i(list.length(InitList))),
        newarr(ILElemType)
    ]),
    AddInitializer =
        (pred(Init0::in, X0 - Tree0::in, (X0 + 1) - Tree::out,
                Info0::in, Info::out) is det :-
            % we may need to box the arguments
            % XXX is this right?
            ( ElemType = mlds_generic_type ->
                maybe_box_initializer(Init0, Init)
            ;
                Init = Init0
            ),
            data_initializer_to_instrs(Init, ElemType,
                ATree1, ITree1, Info0, Info),
            Tree = Tree0 ++ from_list([dup, ldc(int32, i(X0))]) ++
                ATree1 ++ ITree1 ++ singleton(stelem(ILElemSimpleType))
        ),
    list.foldl2(AddInitializer, InitList, 0 - empty, _ - InitInstrs, !Info).
data_initializer_to_instrs(no_initializer, _, empty, empty, !Info).

    % If we are initializing an array or struct, we need to box
    % all the things inside it.
    %
:- pred maybe_box_initializer(mlds_initializer::in, mlds_initializer::out)
    is det.

    % nothing to do
maybe_box_initializer(no_initializer, no_initializer).
    % array already boxed
maybe_box_initializer(init_array(X), init_array(X)).
    % struct already boxed
maybe_box_initializer(init_struct(Type, X), init_struct(Type, X)).
    % single items need to be boxed
maybe_box_initializer(init_obj(Rval), init_obj(NewRval)) :-
    rval_to_type(Rval, BoxType),
    NewRval = ml_unop(box(BoxType), Rval).

    % Code to flatten nested intializers.
    %
:- func flatten_inits(list(mlds_initializer)) = list(mlds_initializer).

flatten_inits(Inits) = list.condense(list.map(flatten_init, Inits)).

:- func flatten_init(mlds_initializer) = list(mlds_initializer).

flatten_init(I) = Inits :-
    ( I = init_struct(_Type, Inits0) ->
        Inits = flatten_inits(Inits0)
    ; I = init_array(Inits0) ->
        Inits = flatten_inits(Inits0)
    ;
        Inits = [I]
    ).

%-----------------------------------------------------------------------------%
%
% Convert basic MLDS statements into IL.
%

:- pred statements_to_il(list(statement)::in, instr_tree::out,
    il_info::in, il_info::out) is det.

statements_to_il([], empty, !Info).
statements_to_il([HeadStmt | TailStmts], HeadCode ++ TailCode, !Info) :-
    statement_to_il(HeadStmt, HeadCode, !Info),
    statements_to_il(TailStmts, TailCode, !Info).

:- pred statement_to_il(statement::in, instr_tree::out,
    il_info::in, il_info::out) is det.

statement_to_il(statement(BlockStmt, Context), Instrs, !Info) :-
    BlockStmt = ml_stmt_block(Defns, Statements),
    il_info_get_module_name(!.Info, ModuleName),
    il_info_get_next_block_id(BlockId, !Info),
    list.map(defn_to_local(ModuleName), Defns, Locals),
    il_info_add_locals(Locals, !Info),
    list.foldl2(generate_defn_initializer, Defns, empty,
        InitInstrsTree, !Info),
    statements_to_il(Statements, BlockInstrs, !Info),
    DataRep = !.Info ^ il_data_rep,
    list.map((pred((K - V)::in, (K - W)::out) is det :-
        W = mlds_type_to_ilds_type(DataRep, V)), Locals, ILLocals),
    Scope = bt_scope(ILLocals),
    Instrs =
        context_node(Context) ++
        singleton(start_block(Scope, BlockId)) ++
        InitInstrsTree ++
        comment_node("block body") ++
        BlockInstrs ++
        singleton(end_block(Scope, BlockId)),
    il_info_remove_locals(Locals, !Info).

statement_to_il(statement(ml_stmt_atomic(Atomic), Context), Instrs, !Info) :-
    atomic_statement_to_il(Atomic, AtomicInstrs, !Info),
    Instrs = context_node(Context) ++ AtomicInstrs.

statement_to_il(statement(CallStmt, Context), Instrs, !Info) :-
    CallStmt = ml_stmt_call(Sig, Function, _This, Args, Returns, CallKind),
    VerifiableCode = !.Info ^ verifiable_code,
    ByRefTailCalls = !.Info ^ il_byref_tailcalls,
    MsCLR = !.Info ^ support_ms_clr,
    RotorCLR = !.Info ^ support_rotor_clr,
    DataRep = !.Info ^ il_data_rep,
    TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig),
    ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig),
    CallerSig = !.Info ^ signature,
    CallerSig = signature(_, CallerReturnParam, _),
    (
        ( CallKind = tail_call ; CallKind = no_return_call ),
        % If --verifiable-code is enabled, and the arguments contain one
        % or more byrefs, then don't emit the "tail." prefix, unless
        % --il-byref-tailcalls is set.
        \+ (
            VerifiableCode = yes,
            some [Ref] (
                list.member(Ref, TypeParams),
                ( Ref = il_type(_, '&'(_))
                ; Ref = il_type(_, '*'(_))
                ; Ref = il_type(_, refany)
                )
            ),
            ByRefTailCalls = no
        ),
        % If --verifiable-code is enabled, then we must not output the "tail."
        % prefix unless the callee return type is compatible with the caller
        % return type.
        \+ (
            VerifiableCode = yes,
            ReturnParam \= CallerReturnParam
        ),
        % In the MS CLR implementation the callee and caller return type
        % of a tail call must be compatible even when we are using
        % unverifiable code.
        \+ (
            MsCLR = yes,
            ReturnParam \= CallerReturnParam
        ),
        % The ROTOR implementation only allows "tail." annotations on direct
        % calls (tail.call), not indirect calls (calli).
        \+ (
            RotorCLR = yes,
            Function \= ml_const(_)
        )
    ->
        TailCallInstrs = [tailcall],
        % For calls marked with "tail.", we need a `ret' instruction
        % immediately after the call (this is in fact needed for correct IL,
        % not just for verifiability).
        RetInstrs = [ret],
        ReturnsStoredInstrs = empty,
        LoadMemRefInstrs = empty
    ;
        % For non-tail calls, we might have to load a memory reference
        % before the call so we can store the result into the memory reference
        % after the call.
        TailCallInstrs = [],
        RetInstrs = [],
        get_all_load_store_lval_instrs(Returns,
            LoadMemRefInstrs, ReturnsStoredInstrs, !Info)
    ),
    list.map_foldl(load, Args, ArgsLoadInstrsTrees, !Info),
    ArgsLoadInstrs = cord_list_to_cord(ArgsLoadInstrsTrees),
    ( Function = ml_const(Const) ->
        FunctionLoadInstrs = empty,
        const_rval_to_function(Const, MemberName),
        Instrs0 = [call(methoddef(call_conv(no, default),
            ReturnParam, MemberName, TypeParams))]
    ;
        load(Function, FunctionLoadInstrs, !Info),
        MakeMethodParam = (func(MethodType) = MethodParam :-
            MethodParam = il_method_param(MethodType, no)
        ),
        ParamsList = list.map(MakeMethodParam, TypeParams),
        Instrs0 = [calli(signature(call_conv(no, default),
            ReturnParam, ParamsList))]
    ),
    Instrs =
        context_node(Context) ++
        comment_node("call") ++
        LoadMemRefInstrs ++
        ArgsLoadInstrs ++
        FunctionLoadInstrs ++
        from_list(TailCallInstrs) ++
        from_list(Instrs0) ++
        from_list(RetInstrs) ++
        ReturnsStoredInstrs.

statement_to_il(statement(IfThenElseStmt, Context), Instrs, !Info) :-
    IfThenElseStmt = ml_stmt_if_then_else(Condition, ThenCase, ElseCase),
    generate_condition(Condition, ConditionInstrs, ElseLabel, !Info),
    il_info_make_next_label(DoneLabel, !Info),
    statement_to_il(ThenCase, ThenInstrs, !Info),
    maybe_map_fold(statement_to_il, ElseCase, empty, ElseInstrs, !Info),
    Instrs =
        context_node(Context) ++
        comment_node("if then else") ++
        ConditionInstrs ++
        comment_node("then case") ++
        ThenInstrs ++
        singleton(br(label_target(DoneLabel))) ++
        singleton(label(ElseLabel)) ++
        comment_node("else case") ++
        ElseInstrs ++
        comment_node("end if then else") ++
        singleton(label(DoneLabel)).

statement_to_il(statement(SwitchStmt, _Context), _Instrs, !Info) :-
    SwitchStmt = ml_stmt_switch(_Type, _Val, _Range, _Cases, _Default),
    % The IL back-end only supports computed_gotos and if-then-else chains;
    % the MLDS code generator should either avoid generating MLDS switches,
    % or should transform them into computed_gotos or if-then-else chains.
    unexpected($module, $pred, "`switch' not supported").

statement_to_il(statement(WhileStmt, Context), Instrs, !Info) :-
    WhileStmt = ml_stmt_while(Kind, Condition, Body),
    generate_condition(Condition, ConditionInstrs, EndLabel, !Info),
    il_info_make_next_label(StartLabel, !Info),
    statement_to_il(Body, BodyInstrs, !Info),
    (
        Kind = may_loop_zero_times,
        Instrs =
            context_node(Context) ++
            comment_node("while") ++
            singleton(label(StartLabel)) ++
            ConditionInstrs ++
            BodyInstrs ++
            singleton(br(label_target(StartLabel))) ++
            singleton(label(EndLabel))
    ;
        Kind = loop_at_least_once,
        % XXX This generates a branch over branch which is suboptimal.
        Instrs =
            context_node(Context) ++
            comment_node("while (actually do ... while)") ++
            singleton(label(StartLabel)) ++
            BodyInstrs ++
            ConditionInstrs ++
            singleton(br(label_target(StartLabel))) ++
            singleton(label(EndLabel))
    ).

statement_to_il(statement(ml_stmt_return(Rvals), Context), Instrs, !Info) :-
    (
        Rvals = [],
        unexpected($module, $pred, "empty list of return values")
    ;
        Rvals = [Rval],
        load(Rval, LoadInstrs, !Info),
        Instrs =
            context_node(Context) ++
            LoadInstrs ++
            singleton(ret)
    ;
        Rvals = [_, _ | _],
        % MS IL doesn't support multiple return values
        sorry($module, $pred, "multiple return values")
    ).

statement_to_il(statement(ml_stmt_label(Label), Context), Instrs, !Info) :-
    string.format("label %s", [s(Label)], Comment),
    Instrs = from_list([
        comment(Comment),
        context_instr(Context),
        label(Label)
    ]).

statement_to_il(statement(GotoLabelStmt, Context), Instrs, !Info) :-
    GotoLabelStmt = ml_stmt_goto(goto_label(Label)),
    string.format("goto %s", [s(Label)], Comment),
    Instrs = from_list([
        comment(Comment),
        context_instr(Context),
        br(label_target(Label))
    ]).

statement_to_il(statement(ml_stmt_goto(goto_break), _Context), _Instrs,
        !Info) :-
    sorry($module, $pred, "break").

statement_to_il(statement(ml_stmt_goto(goto_continue), _Context), _Instrs,
        !Info) :-
    sorry($module, $pred, "continue").

statement_to_il(statement(DoCommitStmt, Context), Instrs, !Info) :-
    DoCommitStmt = ml_stmt_do_commit(_Ref),

    % For commits, we use exception handling.
    %
    % For a do_commit instruction, we generate code equivalent
    % to the following C#/Java code:
    %
    %   throw new mercury.runtime.Commit();
    %
    % In IL the code looks like this:
    %
    %   newobj  instance void
    %       ['mercury']'mercury'.'runtime'.'Commit'::.ctor()
    %   throw
    %
    NewObjInstr = newobj_constructor(il_commit_class_name, []),
    Instrs =
        context_node(Context) ++
        comment_node("do_commit/1") ++
        singleton(NewObjInstr) ++
        singleton(throw).

statement_to_il(statement(TryCommitStmt, Context), Instrs, !Info) :-
    TryCommitStmt = ml_stmt_try_commit(_Ref, GoalToTry, CommitHandlerGoal),

    % For commits, we use exception handling.
    %
    % For try_commit instructions, we generate IL code
    % of the following form:
    %
    %   .try {
    %       <GoalToTry>
    %       leave label1
    %   } catch commit_type {
    %       pop // discard the exception object
    %       <CommitHandlerGoal>
    %       leave label1
    %   }
    %   label1:
    %
    il_info_get_next_block_id(TryBlockId, !Info),
    statement_to_il(GoalToTry, GoalInstrsTree, !Info),
    il_info_get_next_block_id(CatchBlockId, !Info),
    statement_to_il(CommitHandlerGoal, HandlerInstrsTree, !Info),
    il_info_make_next_label(DoneLabel, !Info),

    ClassName = il_commit_class_name,
    Instrs =
        context_node(Context) ++
        comment_node("try_commit/3") ++

        singleton(start_block(bt_try, TryBlockId)) ++
        GoalInstrsTree ++
        singleton(leave(label_target(DoneLabel))) ++
        singleton(end_block(bt_try, TryBlockId)) ++

        singleton(start_block(bt_catch(ClassName), CatchBlockId)) ++
        comment_node("discard the exception object") ++
        singleton(pop) ++
        HandlerInstrsTree ++
        singleton(leave(label_target(DoneLabel))) ++
        singleton(end_block(bt_catch(ClassName), CatchBlockId)) ++
        singleton(label(DoneLabel)).

statement_to_il(statement(ComputedGotoStmt, Context), Instrs, !Info) :-
    ComputedGotoStmt = ml_stmt_computed_goto(Rval, MLDSLabels),
    load(Rval, RvalLoadInstrs, !Info),
    Targets = list.map(func(L) = label_target(L), MLDSLabels),
    Instrs =
        context_node(Context) ++
        comment_node("computed goto") ++
        RvalLoadInstrs ++
        singleton(switch(Targets)).

:- pred atomic_statement_to_il(mlds_atomic_statement::in, instr_tree::out,
    il_info::in, il_info::out) is det.

atomic_statement_to_il(gc_check, singleton(Instr), !Info) :-
    Instr = comment("gc check -- not relevant for this backend").
atomic_statement_to_il(mark_hp(_), singleton(Instr), !Info) :-
    Instr = comment("mark hp -- not relevant for this backend").
atomic_statement_to_il(restore_hp(_), singleton(Instr), !Info) :-
    Instr = comment("restore hp -- not relevant for this backend").

atomic_statement_to_il(outline_foreign_proc(Lang, _, ReturnLvals, _Code),
        Instrs, !Info) :-
    il_info_get_module_name(!.Info, ModuleName),
    (
        !.Info ^ method_foreign_lang = no,
        Info0 = !.Info,
        !Info ^ method_foreign_lang := yes(Lang),
        !Info ^ file_foreign_langs :=
            set.insert(Info0 ^ file_foreign_langs, Lang),
        mangle_foreign_code_module(Lang, ModuleName, OutlineLangModuleName),
        ClassName = mlds_module_name_to_class_name(OutlineLangModuleName),
        signature(_, RetType, Params) = !.Info ^ signature,

        (
            ReturnLvals = [],
            % If there is a return type, but no return value, it must be
            % a semidet predicate so put it in SUCCESS_INDICATOR.
            % XXX It would be better to get the code generator
            % to tell us this is the case directly.
            LoadInstrs = empty,
            ( RetType = void ->
                StoreInstrs = empty
            ;
                StoreInstrs = singleton(stloc(name("SUCCESS_INDICATOR")))
            )
        ;
            ReturnLvals = [ReturnLval],
            get_load_store_lval_instrs(ReturnLval, LoadInstrs, StoreInstrs,
                !Info)
        ;
            ReturnLvals = [_, _ | _],
            sorry($module, $pred, "multiple return values")
        ),
        MethodName = !.Info ^ csharp_method_name,
        TypeParams = il_method_params_to_il_types(Params),
        list.map_foldl((pred(_::in, Instr::out,
            Num::in, Num + 1::out) is det :-
                Instr = ldarg(index(Num))),
            TypeParams, LoadArgInstrs, 0, _),
        Instrs =
            comment_node("outline foreign proc -- call handwritten version") ++
            LoadInstrs ++
            from_list(LoadArgInstrs) ++
            singleton(call(get_static_methodref(ClassName, MethodName, RetType,
                TypeParams))) ++
            StoreInstrs
    ;
        !.Info ^ method_foreign_lang = yes(_),
        Instrs = comment_node("outline foreign proc -- already called")
    ).

atomic_statement_to_il(inline_target_code(ml_target_il, Code), Instrs,
        !Info) :-
    Instrs = inline_code_to_il_asm(Code).
atomic_statement_to_il(inline_target_code(ml_target_c, _Code), _Instrs,
        !Info) :-
    unexpected($module, $pred, "ml_target_c").
atomic_statement_to_il(inline_target_code(ml_target_csharp, _Code), _Instrs,
        !Info) :-
    unexpected($module, $pred, "ml_target_csharp").
atomic_statement_to_il(inline_target_code(ml_target_java, _Code), _Instrs,
        !Info) :-
    unexpected($module, $pred, "ml_target_java").
atomic_statement_to_il(inline_target_code(ml_target_gnu_c, _), _, !Info) :-
    unexpected($module, $pred, "ml_target_gnu_c").

    % NOTE: for the MLDS backends trail ops are currently implemented by
    % the HLDS->HLDS transformation in add_trail_ops.m.  If we encounter
    % an MLDS trail op it's an error.
    %
atomic_statement_to_il(trail_op(_), _, _, _) :-
    unexpected($module, $pred, "trail ops").

atomic_statement_to_il(assign(Lval, Rval), Instrs, !Info) :-
    % Do assignments by loading the rval and storing to the lval.
    load(Rval, LoadRvalInstrs, !Info),
    get_load_store_lval_instrs(Lval, LoadMemRefInstrs, StoreLvalInstrs,
        !Info),
    Instrs =
        comment_node("assign") ++
        LoadMemRefInstrs ++
        LoadRvalInstrs ++
        StoreLvalInstrs.

atomic_statement_to_il(assign_if_in_heap(_, _), _, !Info) :-
    sorry($module, $pred, "assign_if_in_heap").

atomic_statement_to_il(comment(Comment), Instrs, !Info) :-
    Instrs = singleton(comment(Comment)).

atomic_statement_to_il(delete_object(_Target), Instrs, !Info) :-
    % XXX We assume the code generator knows what it is doing and is only
    % going to delete real objects (e.g. reference types). It would perhaps
    % be prudent to check the type of delete_object (if it had one) to
    % make sure.

    % We implement delete_object by storing null in the lval, which hopefully
    % gives the garbage collector a good solid hint that this storage is
    % no longer required.
    %
    % XXX commented out because `delete_object' was changed to take an rval
    % instead of an lval
    %
    % get_load_store_lval_instrs(Target, LoadInstrs, StoreInstrs, !Info),
    % Instrs = LoadInstrs ++ singleton(ldnull) ++ StoreInstrs.
    Instrs = empty.

atomic_statement_to_il(NewObject, Instrs, !Info) :-
    NewObject = new_object(Target0, _MaybeTag, ExplicitSecTag, Type,
        Size, MaybeCtorName, Args, ArgTypes, _MayUseAtomic, _AllocId),
    (
        ExplicitSecTag = yes,
        unexpected($module, $pred, "new_object has explicit secondary tag")
    ;
        ExplicitSecTag = no
    ),
    DataRep = !.Info ^ il_data_rep,
    (
        (
            Type = mlds_generic_env_ptr_type
        ;
            Type = mlds_class_type(_, _, mlds_class)
        ;
            DataRep ^ highlevel_data = yes,
            Type = mercury_type(MercuryType, ctor_cat_user(_), _),
            \+ type_needs_lowlevel_rep(target_il, MercuryType)
        )
    ->
        % If this is a class, we should call the constructor. (This is needed
        % for nondet environment classes, and also for high-level data.)
        % We generate code of the form:
        %
        %   ... load memory reference ...
        %   // new object (call constructor)
        %   ... load each argument ...
        %   call ClassName::.ctor
        %   ... store to memory reference ...

        ClassName0 = mlds_type_to_ilds_class_name(DataRep, Type),
        (
            MaybeCtorName = yes(QualifiedCtorName),
            QualifiedCtorName = qual(_, _, ctor_id(CtorName, CtorArity)),
            CtorType =
                entity_name_to_ilds_id(entity_type(CtorName, CtorArity)),
            ClassName = append_nested_class_name(ClassName0, [CtorType])
        ;
            MaybeCtorName = no,
            ClassName = ClassName0
        ),
        ILArgTypes = list.map(mlds_type_to_ilds_type(DataRep), ArgTypes),
        list.map_foldl(load, Args, ArgsLoadInstrsTrees, !Info),
        ArgsLoadInstrs = cord_list_to_cord(ArgsLoadInstrsTrees),

        % If the new object is being assigned to private_builtin.dummy_var
        % then we need to cast it to il_generic_type.
        (
            Target0 = ml_var(qual(MLDS_Module, QualKind, VarName), _),
            VarName = mlds_var_name("dummy_var", _),
            PrivateBuiltin = mercury_private_builtin_module,
            MLDS_PrivateBuiltin = mercury_module_name_to_mlds(PrivateBuiltin),
            mlds_append_wrapper_class(MLDS_PrivateBuiltin) = MLDS_Module
        ->
            MaybeCastInstrs = singleton(castclass(il_generic_type)),
            Target = ml_var(qual(MLDS_Module, QualKind, VarName),
                mlds_generic_type)
        ;
            MaybeCastInstrs = empty,
            Target = Target0
        ),
        get_load_store_lval_instrs(Target, LoadMemRefInstrs,
            StoreLvalInstrs, !Info),
        CallCtor = newobj_constructor(ClassName, ILArgTypes),
        Instrs =
            LoadMemRefInstrs ++
            comment_node("new object (call constructor)") ++
            ArgsLoadInstrs ++
            singleton(CallCtor) ++
            MaybeCastInstrs ++
            StoreLvalInstrs
    ;
        % Otherwise this is a generic mercury object -- we use an array
        % of System::Object to represent it.
        %
        %   ... load memory reference ...
        %   // new object
        %   ldc <size of array>
        %   newarr
        %
        % And then for each array element:
        %
        %   dup
        %   ldc <array index>
        %   ... load rval ...
        %   stelem System::Object
        %
        % Finally, after all the array elements have been set:
        %
        %   ... store to memory reference ...
        %
        % Note that the MLDS code generator is responsible for boxing/unboxing
        % the arguments if needed.

        % Load each rval.
        % XXX We do almost exactly the same code when initializing array
        % data structures -- we should reuse that code.
        LoadInArray =
            (pred(Rval::in, I::out, Arg0::in, Arg::out) is det :-
                Arg0 = Index - S0,
                I0 = singleton(dup),
                load(ml_const(mlconst_int(Index)), I1, S0, S1),

                % XXX the MLDS code generator is meant to be responsible for
                % boxing the args, but when compiled with the highlevel_data
                % where we have overridden the type to use a lowlevel
                % representation it doesn't get this right.
                rval_to_type(Rval, RvalType),
                ILRvalType = mlds_type_to_ilds_type(DataRep, RvalType),
                ( already_boxed(ILRvalType) ->
                    NewRval = Rval
                ;
                    NewRval = ml_unop(box(RvalType), Rval)
                ),

                load(NewRval, I2, S1, S),
                I3 = singleton(stelem(il_generic_simple_type)),
                I = I0 ++ I1 ++ I2 ++ I3,
                Arg = (Index + 1) - S
            ),
        list.map_foldl(LoadInArray, Args, ArgsLoadInstrsTrees,
            0 - !.Info, _ - !:Info),
        ArgsLoadInstrs = cord_list_to_cord(ArgsLoadInstrsTrees),

        % Get the instructions to load and store the target.
        get_load_store_lval_instrs(Target0, LoadMemRefInstrs, StoreLvalInstrs,
            !Info),
        (
            Size = yes(SizeInWordsRval0),
            SizeInWordsRval = SizeInWordsRval0
        ;
            Size = no,
            % XXX Do we need to handle this case?
            % I think it's needed for --high-level-data.
            unexpected($module, $pred, "unknown size in MLDS new_object")
        ),
        load(SizeInWordsRval, LoadSizeInstrs, !Info),

        Instrs =
            LoadMemRefInstrs ++
            comment_node("new object") ++
            LoadSizeInstrs ++
            singleton(newarr(il_generic_type)) ++
            ArgsLoadInstrs ++
            StoreLvalInstrs
    ).

:- func inline_code_to_il_asm(list(target_code_component)) = instr_tree.

inline_code_to_il_asm([]) = empty.
inline_code_to_il_asm([T | Ts]) = Instrs ++ Rest :-
    (
        T = user_target_code(Code, MaybeContext, Attrs),
        ( yes(max_stack_size(N)) = get_max_stack_attribute(Attrs) ->
            (
                MaybeContext = yes(Context),
                Instrs0 = context_node(mlds_make_context(Context))
            ;
                MaybeContext = no,
                Instrs0 = empty
            ),
            Instrs = Instrs0 ++ singleton(il_asm_code(Code, N))
        ;
            unexpected($module, $pred, "max_stack_size not set")
        )
    ;
        T = raw_target_code(Code, Attrs),
        MaybeMaxStack = get_max_stack_attribute(Attrs),
        (
            MaybeMaxStack = yes(max_stack_size(N)),
            Instrs = singleton(il_asm_code(Code, N))
        ;
            MaybeMaxStack = no,
            unexpected($module, $pred, "max_stack_size not set")
        )
    ;
        T = target_code_input(_),
        Instrs = empty
    ;
        T = target_code_output(_),
        Instrs = empty
    ;
        T = target_code_type(_),
        Instrs = empty
    ;
        T = target_code_name(_),
        Instrs = empty
    ;
        T = target_code_alloc_id(_),
        unexpected($module, $pred, "target_code_alloc_id not implemented")
    ),
    Rest = inline_code_to_il_asm(Ts).

:- func get_max_stack_attribute(target_code_attributes) =
    maybe(target_code_attribute).

get_max_stack_attribute([]) = no.
get_max_stack_attribute([X | _Xs]) = yes(X) :- X = max_stack_size(_).

:- pred get_all_load_store_lval_instrs(list(mlds_lval)::in,
    instr_tree::out, instr_tree::out, il_info::in, il_info::out) is det.

get_all_load_store_lval_instrs([], empty, empty, !Info).
get_all_load_store_lval_instrs([Lval | Lvals],
        LoadMemRefNode ++ LoadMemRefTree,
        StoreLvalNode ++ StoreLvalTree, !Info) :-
    get_load_store_lval_instrs(Lval, LoadMemRefNode, StoreLvalNode, !Info),
    get_all_load_store_lval_instrs(Lvals, LoadMemRefTree, StoreLvalTree,
        !Info).

    % Some lvals need to be loaded before you load the rval.
    % XXX It would be much better if this took the lval and the rval and
    % just gave you a single tree. Instead it gives you the "before" tree
    % and the "after" tree and asks you to sandwich the rval in between.
    % The predicate `store' should probably take the lval and the rval
    % and do all of this at once.
    %
:- pred get_load_store_lval_instrs(mlds_lval::in,
    instr_tree::out, instr_tree::out,
    il_info::in, il_info::out) is det.

get_load_store_lval_instrs(Lval, LoadMemRefInstrs, StoreLvalInstrs, !Info) :-
    DataRep = !.Info ^ il_data_rep,
    ( Lval = ml_mem_ref(Rval0, MLDS_Type) ->
        load(Rval0, LoadMemRefInstrs, !Info),
        SimpleType = mlds_type_to_ilds_simple_type(DataRep, MLDS_Type),
        StoreLvalInstrs = singleton(stind(SimpleType))
    ; Lval = ml_field(_MaybeTag, FieldRval, FieldNum, FieldType, ClassType) ->
        ClassILType = mlds_type_to_ilds_type(DataRep, ClassType),
        ( ClassILType = il_type(_, '[]'(_, _)) ->
            (
                FieldNum = ml_field_offset(OffsetRval),
                FieldILType = mlds_type_to_ilds_simple_type(DataRep,
                    FieldType),
                load(FieldRval, LoadArrayRval, !Info),
                load(OffsetRval, LoadIndexRval, !Info),
                LoadMemRefInstrs = LoadArrayRval ++ LoadIndexRval,
                StoreLvalInstrs = singleton(stelem(FieldILType))
            ;
                FieldNum = ml_field_named(_, _),
                unexpected($module, $pred,
                    "ml_field_named for a type with an array representation.")
            )
        ;
            get_fieldref(DataRep, FieldNum, FieldType, ClassType, FieldRef,
                CastClassInstrs),
            load(FieldRval, LoadMemRefInstrs0, !Info),
            LoadMemRefInstrs = LoadMemRefInstrs0 ++ CastClassInstrs,
            StoreLvalInstrs = singleton(stfld(FieldRef))
        )
    ;
        LoadMemRefInstrs = empty,
        store(Lval, StoreLvalInstrs, !Info)
    ).

%-----------------------------------------------------------------------------%
%
% Load and store.
%
% NOTE: Be very careful calling store directly. You probably want to call
% get_load_store_lval_instrs to generate the prelude part (which will load
% any memory reference that need to be loaded) and the store part (while will
% store the rval into the pre-loaded lval), and then sandwich the calculation
% of the rval in between the two.

:- pred load(mlds_rval::in, instr_tree::out, il_info::in, il_info::out) is det.

load(Rval, Instrs, !Info) :-
    (
        Rval = ml_lval(Lval),
        DataRep = !.Info ^ il_data_rep,
        (
            Lval = ml_var(Var, VarType),
            mangle_mlds_var(Var, MangledVarStr),
            ( is_local(MangledVarStr, !.Info) ->
                Instrs = singleton(ldloc(name(MangledVarStr)))
            ; is_argument(MangledVarStr, !.Info) ->
                Instrs = singleton(ldarg(name(MangledVarStr)))
            ; is_local_field(Var, VarType, !.Info, FieldRef) ->
                Instrs = singleton(ldsfld(FieldRef))
            ;
                FieldRef = make_static_fieldref(DataRep, Var, VarType),
                Instrs = singleton(ldsfld(FieldRef))
            )
        ;
            Lval = ml_field(_MaybeTag, BaseRval, FieldNum, FieldType,
                ClassType),
            load(BaseRval, BaseRvalLoadInstrs, !Info),
            ( FieldNum = ml_field_offset(OffSet) ->
                SimpleFieldType = mlds_type_to_ilds_simple_type(DataRep,
                    FieldType),
                load(OffSet, OffSetLoadInstrs, !Info),
                CastClassInstrs = empty,
                LoadInstruction = ldelem(SimpleFieldType)
            ;
                get_fieldref(DataRep, FieldNum, FieldType, ClassType, FieldRef,
                    CastClassInstrs),
                LoadInstruction = ldfld(FieldRef),
                OffSetLoadInstrs = empty
            ),
            Instrs =
                BaseRvalLoadInstrs ++
                CastClassInstrs ++
                OffSetLoadInstrs ++
                singleton(LoadInstruction)
        ;
            Lval = ml_mem_ref(BaseRval, MLDS_Type),
            SimpleType = mlds_type_to_ilds_simple_type(DataRep, MLDS_Type),
            load(BaseRval, BaseRvalLoadInstrs, !Info),
            Instrs = BaseRvalLoadInstrs ++ singleton(ldind(SimpleType))
        ;
            Lval = ml_global_var_ref(_),
            Instrs = throw_unimplemented("load lval mem_ref")
        )
    ;
        Rval = ml_mkword(_Tag, _Rval),
        Instrs = comment_node("unimplemented load rval mkword")
    ;
        Rval = ml_const(Const),
        % XXX check these, what should we do about multi strings,
        % characters, etc.
        DataRep = !.Info ^ il_data_rep,
        % True and false are just the integers 1 and 0.
        (
            Const = mlconst_true,
            Instrs = singleton(ldc(bool, i(1)))
        ;
            Const = mlconst_false,
            Instrs = singleton(ldc(bool, i(0)))
        ;
            Const = mlconst_string(Str),
            Instrs = singleton(ldstr(Str))
        ;
            ( Const = mlconst_int(Int)
            ; Const = mlconst_enum(Int, _)
            ; Const = mlconst_char(Int)
            ),
            Instrs = singleton(ldc(int32, i(Int)))
        ;
            Const = mlconst_foreign(_Lang, _F, _T),
            sorry($module, $pred, "NYI IL backend and foreign tags.")
        ;
            Const = mlconst_float(Float),
            Instrs = singleton(ldc(float64, f(Float)))
        ;
            Const = mlconst_multi_string(_MultiString),
            Instrs = throw_unimplemented("load multi_string_const")
        ;
            Const = mlconst_named_const(_NamedConst),
            Instrs = throw_unimplemented("load named_const")
        ;
            Const = mlconst_code_addr(CodeAddr),
            MethodRef = code_addr_constant_to_methodref(DataRep, CodeAddr),
            Instrs = singleton(ldftn(MethodRef))
        ;
            Const = mlconst_data_addr(DataAddr),
            data_addr_constant_to_fieldref(DataAddr, FieldRef),
            Instrs = singleton(ldsfld(FieldRef))
        ;
            Const = mlconst_null(_MLDSType),
            % We might consider loading an integer for null function types.
            Instrs = singleton(ldnull)
        )
    ;
        Rval = ml_unop(Unop, RvalA),
        load(RvalA, RvalALoadInstrs, !Info),
        unaryop_to_il(Unop, RvalA, UnOpInstrs, !Info),
        Instrs = RvalALoadInstrs ++ UnOpInstrs
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        load(RvalA, RvalALoadInstrs, !Info),
        load(RvalB, RvalBLoadInstrs, !Info),
        binaryop_to_il(BinOp, BinaryOpInstrs, !Info),
        Instrs = RvalALoadInstrs ++ RvalBLoadInstrs ++ BinaryOpInstrs
    ;
        Rval = ml_mem_addr(Lval),
        DataRep = !.Info ^ il_data_rep,
        (
            Lval = ml_var(Var, VarType),
            mangle_mlds_var(Var, MangledVarStr),
            ( is_local(MangledVarStr, !.Info) ->
                Instrs = singleton(ldloca(name(MangledVarStr)))
            ; is_argument(MangledVarStr, !.Info) ->
                Instrs = singleton(ldarga(name(MangledVarStr)))
            ; is_local_field(Var, VarType, !.Info, FieldRef) ->
                Instrs = singleton(ldsfld(FieldRef))
            ;
                FieldRef = make_static_fieldref(DataRep, Var, VarType),
                Instrs = singleton(ldsfld(FieldRef))
            )
        ;
            Lval = ml_field(_MaybeTag, BaseRval, FieldNum, FieldType,
                ClassType),
            get_fieldref(DataRep, FieldNum, FieldType, ClassType,
                FieldRef, CastClassInstrs),
            load(BaseRval, BaseRvalLoadInstrs, !Info),
            Instrs =
                BaseRvalLoadInstrs ++
                CastClassInstrs ++
                singleton(ldflda(FieldRef))
        ;
            Lval = ml_mem_ref(_, _),
            % XXX Implement this.
            Instrs = throw_unimplemented("load mem_addr lval mem_ref")
        ;
            Lval = ml_global_var_ref(_),
            Instrs = throw_unimplemented("load mem_addr lval global_var_ref")
        )
    ;
        Rval = ml_scalar_common(_),
        Instrs = throw_unimplemented("load scalar_common")
    ;
        Rval = ml_vector_common_row(_, _),
        Instrs = throw_unimplemented("load vector_common_row")
    ;
        Rval = ml_self(_),
        Instrs = singleton(ldarg(index(0)))
    ).

:- pred store(mlds_lval::in, instr_tree::out, il_info::in, il_info::out)
    is det.

store(Lval, Instrs, !Info) :-
    (
        Lval = ml_field(_MaybeTag, Rval, FieldNum, FieldType, ClassType),
        DataRep = !.Info ^ il_data_rep,
        get_fieldref(DataRep, FieldNum, FieldType, ClassType,
            FieldRef, CastClassInstrs),
        load(Rval, RvalLoadInstrs, !Info),
        Instrs =
            CastClassInstrs ++
            RvalLoadInstrs ++
            singleton(stfld(FieldRef))
    ;
        Lval = ml_mem_ref(_Rval, _Type),
        % You always need load the reference first, then the value,
        % then stind it. There's no swap instruction. Annoying, eh?
        unexpected($module, $pred, "store into mem_ref")
    ;
        Lval = ml_global_var_ref(_),
        unexpected($module, $pred, "store into global_var_ref")
    ;
        Lval = ml_var(Var, VarType),
        DataRep = !.Info ^ il_data_rep,
        mangle_mlds_var(Var, MangledVarStr),
        ( is_local(MangledVarStr, !.Info) ->
            Instrs = singleton(stloc(name(MangledVarStr)))
        ; is_argument(MangledVarStr, !.Info) ->
            Instrs = singleton(starg(name(MangledVarStr)))
        ;
            FieldRef = make_static_fieldref(DataRep, Var, VarType),
            Instrs = singleton(stsfld(FieldRef))
        )
    ).

%-----------------------------------------------------------------------------%
%
% Convert binary and unary operations to IL.
%

:- pred unaryop_to_il(mlds_unary_op::in, mlds_rval::in, instr_tree::out,
    il_info::in, il_info::out) is det.

    % Once upon a time the MLDS code generator generated primary tag tests
    % (but we don't use primary tags).
    % If we make mktag return its operand (since it will always be
    % called with 0 as its operand), and we make tag return 0, it will
    % always succeed in the tag test (which is good, with tagbits = 0
    % we want to always succeed all primary tag tests).

unaryop_to_il(std_unop(mktag), _, comment_node("mktag (a no-op)"), !Info).
unaryop_to_il(std_unop(tag), _, Instrs, !Info) :-
    load(ml_const(mlconst_int(0)), Instrs, !Info).
unaryop_to_il(std_unop(unmktag), _, comment_node("unmktag (a no-op)"), !Info).
unaryop_to_il(std_unop(strip_tag),_,comment_node("strip_tag (a no-op)"),
        !Info).
unaryop_to_il(std_unop(mkbody), _, comment_node("mkbody (a no-op)"), !Info).
unaryop_to_il(std_unop(unmkbody), _, comment_node("unmkbody (a no-op)"),
        !Info).
unaryop_to_il(std_unop(bitwise_complement), _, singleton(bitwise_not), !Info).

    % Might want to revisit this and define not to be only valid on 1 or 0,
    % then we can use ldc.i4.1 and xor, which might be more efficient.
unaryop_to_il(std_unop(logical_not), _,
        from_list([ldc(int32, i(1)), clt(unsigned)]), !Info).
unaryop_to_il(std_unop(hash_string), _,
        singleton(call(il_mercury_string_hash)), !Info).
unaryop_to_il(std_unop(hash_string2), _, _, !Info) :-
    unexpected($module, $pred, "hash_string2").
unaryop_to_il(std_unop(hash_string3), _, _, !Info) :-
    unexpected($module, $pred, "hash_string3").

    % XXX Should detect casts to System.Array from array types
    % and ignore them, as they are not necessary.
unaryop_to_il(cast(DestType), SrcRval, Instrs, !Info) :-
    DataRep = !.Info ^ il_data_rep,
    DestILType = mlds_type_to_ilds_type(DataRep, DestType),
    rval_to_type(SrcRval, SrcType),
    SrcILType = mlds_type_to_ilds_type(DataRep, SrcType),

    % We need to handle casts to/from "refany" specially --
    % IL has special instructions for those
    (
        % Is it a cast to refany?
        DestILType = il_type(_, refany)
    ->
        (
            % Is it from refany?
            SrcILType = il_type(_, refany)
        ->
            % Cast from refany to refany is a NOP.
            Instrs = empty
        ;
            % Cast to refany: use "mkrefany" instruction.
            ( SrcILType = il_type(_Qual, '&'(ReferencedType)) ->
                Instrs = singleton(mkrefany(ReferencedType))
            ;
                unexpected($module, $pred, "cast from non-ref type to refany")
            )
        )
    ;
        % Is it a cast from refany?
        SrcRval = ml_lval(_),
        rval_to_type(SrcRval, SrcType),
        SrcILType = mlds_type_to_ilds_type(DataRep, SrcType),
        SrcILType = il_type(_, refany)
    ->
        % Cast from refany: use "refanyval" instruction.
        ( DestILType = il_type(_Qual, '&'(ReferencedType)) ->
            Instrs = singleton(refanyval(ReferencedType))
        ;
            unexpected($module, $pred, "cast from non-ref type to refany")
        )
    ;
        % We need to handle casts to/from unmanaged pointers specially --
        % .castclass doesn't work for those.  These casts are generated
        % by ml_elim_nested.m for the environment pointers.  If we're
        % using unmanaged pointers, then this must be unverifiable code.
        % We don't need to use any explicit conversion in the IL
        %
        % XXX Currently ilds uses `native_uint' for unmanaged pointers,
        % because that's what IL does, but we should probably define a
        % separate ilds type for this.

        ( DestILType = il_type(_, native_uint)
        ; SrcILType = il_type(_, native_uint)
        )
    ->
        Instrs = empty
    ;
        % If we are casting from an unboxed type to a boxed type,
        % we should box it first, and then cast.

        already_boxed(DestILType)
    ->
        ( already_boxed(SrcILType) ->
            ( SrcType = DestType ->
                Instrs = empty
            ;
                % Cast one boxed type to another boxed type.
                Instrs = singleton(castclass(DestILType))
            )
        ;
            % Convert an unboxed type to a boxed type: box it first, then cast.
            Instrs =
                convert_to_object(SrcILType) ++
                singleton(castclass(DestILType))
        )
    ;
        ( already_boxed(SrcILType) ->
            (
                SrcType = mercury_type(_, TypeCtorCategory, _),
                % XXX Consider whether this is the right way to handle
                % type_infos, type_ctor_infos, typeclass_infos and
                % base_typeclass_infos.
                ( TypeCtorCategory = ctor_cat_user(_)
                ; is_introduced_type_info_type_category(TypeCtorCategory) = yes
                )
            ->
                % XXX We should look into a nicer way to generate MLDS
                % so we don't need to do this.
                % XXX This looks wrong for --high-level-data.
                % -fjh.
                Instrs =
                    comment_node("loading out of an MR_Word") ++
                    singleton(ldc(int32, i(0))) ++
                    singleton(ldelem(il_generic_simple_type)) ++
                    comment_node("turning a cast into an unbox") ++
                    convert_from_object(DestILType)
            ;
                % XXX It would be nicer if the MLDS used an unbox to do this.
                Instrs =
                    comment_node("turning a cast into an unbox") ++
                    convert_from_object(DestILType)
            )
        ;
            DestILType = il_type(_, DestSimpleType),
            Instrs =
                comment_node("cast between value types") ++
                singleton(conv(DestSimpleType))
        )
    ).

unaryop_to_il(box(UnboxedType), _, Instrs, !Info) :-
    DataRep = !.Info ^ il_data_rep,
    UnboxedILType = mlds_type_to_ilds_type(DataRep, UnboxedType),
    ( already_boxed(UnboxedILType) ->
        % It is already boxed, so we don't need to do anything.
        Instrs = empty
    ;
        Instrs = convert_to_object(UnboxedILType)
    ).

unaryop_to_il(unbox(UnboxedType), Rval, Instrs, !Info) :-
    DataRep = !.Info^ il_data_rep,
    rval_to_type(Rval, RvalType),
    UnboxedILType = mlds_type_to_ilds_type(DataRep, UnboxedType),
    ( already_boxed(UnboxedILType) ->
        ( RvalType = UnboxedType ->
            % We already have the correct type.
            Instrs = empty
        ;
            % We have a different boxed type.
            Instrs = singleton(castclass(UnboxedILType))
        )
    ;
        Instrs = convert_from_object(UnboxedILType)
    ).

:- pred already_boxed(il_type::in) is semidet.

already_boxed(il_type(_, object)).
already_boxed(il_type(_, string)).
already_boxed(il_type(_, refany)).
already_boxed(il_type(_, class(_))).
already_boxed(il_type(_, interface(_))).
already_boxed(il_type(_, '[]'(_, _))).
already_boxed(il_type(_, '&'(_))).
already_boxed(il_type(_, '*'(_))).

:- pred binaryop_to_il(binary_op::in, instr_tree::out,
    il_info::in, il_info::out) is det.

binaryop_to_il(int_add, singleton(I), !Info) :-
    I = add(nocheckoverflow, signed).

binaryop_to_il(int_sub, singleton(I), !Info) :-
    I = sub(nocheckoverflow, signed).

binaryop_to_il(int_mul, singleton(I), !Info) :-
    I = mul(nocheckoverflow, signed).

binaryop_to_il(int_div, singleton(I), !Info) :-
    I = div(signed).

binaryop_to_il(int_mod, singleton(I), !Info) :-
    I = rem(signed).

binaryop_to_il(unchecked_left_shift, singleton(I), !Info) :-
    I = shl.

binaryop_to_il(unchecked_right_shift, singleton(I), !Info) :-
    I = shr(signed).

binaryop_to_il(bitwise_and, singleton(I), !Info) :-
    I = bitwise_and.

binaryop_to_il(bitwise_or, singleton(I), !Info) :-
    I = bitwise_or.

binaryop_to_il(bitwise_xor, singleton(I), !Info) :-
    I = bitwise_xor.

binaryop_to_il(logical_and, singleton(I), !Info) :-
    % XXX
    I = bitwise_and.

binaryop_to_il(logical_or, singleton(I), !Info) :-
    % XXX
    I = bitwise_or.

binaryop_to_il(eq, singleton(I), !Info) :-
    I = ceq.

binaryop_to_il(ne, from_list(Instrs), !Info) :-
    Instrs = [
        ceq,
        ldc(int32, i(0)),
        ceq
    ].

binaryop_to_il(body, _, !Info) :-
    unexpected($module, $pred, "body").

binaryop_to_il(float_word_bits, _, !Info) :-
    unexpected($module, $pred, "float_word_bits").

binaryop_to_il(float_from_dword, _, !Info) :-
    unexpected($module, $pred, "float_from_dword").

binaryop_to_il(array_index(ElemType), singleton(I), !Info) :-
    DataRep = !.Info ^ il_data_rep,
    MLDS_Type = ml_gen_array_elem_type(ElemType),
    ILSimpleType = mlds_type_to_ilds_simple_type(DataRep, MLDS_Type),
    I = ldelem(ILSimpleType).

    % String operations.
binaryop_to_il(str_eq, from_list([
        call(il_string_equals)
    ]), !Info).
binaryop_to_il(str_ne, from_list([
        call(il_string_equals),
        ldc(int32, i(0)),
        ceq
    ]), !Info).
binaryop_to_il(str_lt, from_list([
        call(il_string_compare),
        ldc(int32, i(0)),
        clt(signed)
    ]), !Info).
binaryop_to_il(str_gt, from_list([
        call(il_string_compare),
        ldc(int32, i(0)),
        cgt(signed)
    ]), !Info).
binaryop_to_il(str_le, from_list([
        call(il_string_compare),
        ldc(int32, i(1)), clt(signed)
    ]), !Info).
binaryop_to_il(str_ge, from_list([
        call(il_string_compare),
        ldc(int32, i(-1)),
        cgt(signed)
    ]), !Info).
binaryop_to_il(str_cmp, _, !Info) :-
    unexpected($module, $pred, "str_cmp").

    % Integer comparison
binaryop_to_il(int_lt, singleton(clt(signed)), !Info).
binaryop_to_il(int_gt, singleton(cgt(signed)), !Info).
binaryop_to_il(int_le, from_list([cgt(signed), ldc(int32, i(0)), ceq]), !Info).
binaryop_to_il(int_ge, from_list([clt(signed), ldc(int32, i(0)), ceq]), !Info).
binaryop_to_il(unsigned_le, from_list([cgt(unsigned), ldc(int32, i(0)), ceq]),
    !Info).

    % Floating pointer operations.
binaryop_to_il(float_plus, singleton(I), !Info) :-
    I = add(nocheckoverflow, signed).
binaryop_to_il(float_minus, singleton(I), !Info) :-
    I = sub(nocheckoverflow, signed).
binaryop_to_il(float_times, singleton(I), !Info) :-
    I = mul(nocheckoverflow, signed).
binaryop_to_il(float_divide, singleton(I), !Info) :-
    I = div(signed).
binaryop_to_il(float_eq, singleton(I), !Info) :-
    I = ceq.
binaryop_to_il(float_ne, from_list(Instrs), !Info) :-
    Instrs = [
        ceq,
        ldc(int32, i(0)),
        ceq
    ].
binaryop_to_il(float_lt, singleton(clt(signed)), !Info).
binaryop_to_il(float_gt, singleton(cgt(signed)), !Info).
binaryop_to_il(float_le, from_list([cgt(signed), ldc(int32, i(0)), ceq]),
        !Info).
binaryop_to_il(float_ge, from_list([clt(signed), ldc(int32, i(0)), ceq]),
        !Info).

binaryop_to_il(compound_eq, _, !Info) :-
    unexpected($module, $pred, "compound_eq").
binaryop_to_il(compound_lt, _, !Info) :-
    unexpected($module, $pred, "compound_lt").

%-----------------------------------------------------------------------------%
%
% Generate code for conditional statements
%
% For most conditionals, we simply load the rval and branch to the else
% case if it is false.
%
%   load rval
%   brfalse elselabel
%
% For eq and ne binops, this will generate something a bit wasteful, e.g.
%
%   load operand1
%   load operand2
%   ceq
%   brfalse elselabel
%
% We try to avoid generating a comparison result on the stack and then
% comparing it to false.  Instead we load the operands and
% branch/compare all at once.  E.g.
%
%   load operand1
%   load operand2
%   bne.unsigned elselabel
%
% Perhaps it would be better to just generate the default code and let
% the peephole optimizer pick this one up.  Since it's pretty easy
% to detect I've left it here for now.

:- pred generate_condition(mlds_rval::in, instr_tree::out, string::out,
    il_info::in, il_info::out) is det.

generate_condition(Rval, Instrs, ElseLabel, !Info) :-
    il_info_make_next_label(ElseLabel, !Info),
    (
        Rval = ml_binop(eq, Operand1, Operand2)
    ->
        load(Operand1, Op1Instr, !Info),
        load(Operand2, Op2Instr, !Info),
        OpInstr = singleton(bne(unsigned, label_target(ElseLabel))),
        Instrs = Op1Instr ++ Op2Instr ++ OpInstr
    ;
        Rval = ml_binop(ne, Operand1, Operand2)
    ->
        load(Operand1, Op1Instr, !Info),
        load(Operand2, Op2Instr, !Info),
        OpInstr = singleton(beq(label_target(ElseLabel))),
        Instrs = Op1Instr ++ Op2Instr ++ OpInstr
    ;
        load(Rval, RvalLoadInstrs, !Info),
        ExtraInstrs = singleton(brfalse(label_target(ElseLabel))),
        Instrs = RvalLoadInstrs ++ ExtraInstrs
    ).

%-----------------------------------------------------------------------------%
%
% Get a function name for a code_addr_const rval.
%
% XXX This predicate should be narrowed down to the cases that actually
% make sense.

    % Convert an rval into a function we can call.
    %
:- pred const_rval_to_function(mlds_rval_const::in, class_member_name::out)
    is det.

const_rval_to_function(Const, MemberName) :-
    ( Const = mlconst_code_addr(CodeConst) ->
        (
            CodeConst = code_addr_proc(ProcLabel, _Sig),
            mangle_mlds_proc_label(ProcLabel, no, ClassName, ProcLabelStr)
        ;
            CodeConst = code_addr_internal(ProcLabel, SeqNum, _Sig),
            mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName,
                ProcLabelStr)
        ),
        MemberName = class_member_name(ClassName, id(ProcLabelStr))
    ;
        unexpected($module, $pred, "const is not a code address")
    ).

%-----------------------------------------------------------------------------
%
% Class constructors (.cctors) are used to initialise the runtime.
% This currently consists of initialising the RTTI and calling
% mercury.runtime.init_runtime.
%
% The RTTI is stored in static fields of the class.

    % .cctors can be called at practically any time by the runtime
    % system, but must be called before a static field is loaded
    % (the runtime will ensure this happens).
    % Since all the static fields in RTTI reference other RTTI static
    % fields, we could run into problems if we load a field from another
    % class before we initialize it.  Often the RTTI in one module will
    % refer to another, creating exactly this cross-referencing problem.
    % To avoid problems, we initialize them in 3 passes (passes 2 to 4
    % below).
    %
    % Here is the structure of the .cctor that we generate.
    %
    % 1. We call mercury.runtime.responsible_for_initialising_runtime
    %    to determine whether this is the first mercury .cctor called.
    %
    % 2. We allocate all the RTTI data structures but leave them blank.
    %    When this is complete we set a flag to say we have completed this
    %    pass.  After this pass is complete, it is safe for any other
    %    module to reference our data structures.
    %
    % 3. We call all the .cctors for RTTI data structures that we
    %    import.  We do this because we can't load fields from them until we
    %    know they have been allocated.
    %
    % 4. We fill in the RTTI info in the already allocated structures.
    %
    % 5. If responsible_for_initialising_runtime returned true, then we
    %    call the initialise runtime function now all the RTTI is
    %    initialised.
    %
    % To ensure that pass 3 doesn't cause looping, the first thing done
    % in all .cctors is a check to see if the flag is set.  If it is, we
    % return immediately (we have already been called and our
    % initialization is either complete or at pass 3).
    %
    % Here is a skeleton of the il that we will generate.
    %
    %   // Are we responsible for initialising the runtime.
    %   call bool [mercury]mercury.runtime::
    %           responsible_for_initialising_runtime()
    %
    %   // if (rtti_initialized) return;
    %   ldsfld rtti_initialized
    %       brfalse done_label
    %   pop // pop the responsible_for_initialising_runtime bool
    %   ret
    %   done_label:
    %
    %   // rtti_initialized = true
    %   ldc.i4.1
    %   stsfld rtti_initialized
    %
    %   // allocate RTTI data structures.
    %   <allocation instructions generated by field initializers>
    %
    %   // call .cctors
    %   call    someclass::.cctor
    %   call    someotherclass::.cctor
    %   ... etc ...
    %
    %   // fill in fields of RTTI data structures
    %   <initialization instructions generated by field initializers>
    %
    %   // Maybe initialise the runtime
    %   call void [mercury]mercury.runtime::init_runtime(bool)
    %
:- pred make_class_constructor_class_member(fieldref::in, mlds_imports::in,
    list(instr)::in, list(instr)::in, class_member::out,
    il_info::in, il_info::out) is det.

make_class_constructor_class_member(DoneFieldRef, Imports, AllocInstrs,
        InitInstrs, Method, !Info) :-
    Method = member_method(methodhead([public, static], cctor,
        signature(call_conv(no, default), void, []), []), MethodDecls),
    ResponsibleInitRuntimeInstrs = responsible_for_init_runtime_instrs,
    RuntimeInitInstrs = runtime_initialization_instrs,
    test_rtti_initialization_field(DoneFieldRef, TestInstrs, !Info),
    set_rtti_initialization_field(DoneFieldRef, SetInstrs, !Info),
    CCtorCalls = list.filter_map(
        (func(I::in) = (C::out) is semidet :-
            I = mercury_import(compiler_visible_interface, ImportName),
            C = call_class_constructor(
                class_name(ImportName, wrapper_class_name))
        ), Imports),
    AllInstrs = list.condense([ResponsibleInitRuntimeInstrs, TestInstrs,
        AllocInstrs, SetInstrs, CCtorCalls, InitInstrs, RuntimeInitInstrs,
        [ret]]),
    MethodDecls = [instrs(AllInstrs)].

:- pred test_rtti_initialization_field(fieldref::in, list(instr)::out,
    il_info::in, il_info::out) is det.

test_rtti_initialization_field(FieldRef, Instrs, !Info) :-
    il_info_make_next_label(DoneLabel, !Info),
    Instrs = [ldsfld(FieldRef), brfalse(label_target(DoneLabel)),
        pop, ret, label(DoneLabel)].

:- pred set_rtti_initialization_field(fieldref::in, list(instr)::out,
    il_info::in, il_info::out) is det.

set_rtti_initialization_field(FieldRef, Instrs, !Info) :-
    Instrs = [ldc(int32, i(1)), stsfld(FieldRef)].

:- pred generate_rtti_initialization_field(ilds.class_name::in,
    fieldref::out, class_member::out) is det.

generate_rtti_initialization_field(ClassName, AllocDoneFieldRef,
        AllocDoneField) :-
    AllocDoneFieldName = "rtti_initialized",
    AllocDoneField = member_field([public, static], il_type([], bool),
        AllocDoneFieldName, no, none),
    AllocDoneFieldRef = make_fieldref(il_type([], bool),
        ClassName, AllocDoneFieldName).

%-----------------------------------------------------------------------------
%
% Conversion of MLDS types to IL types.

:- func mlds_inherits_to_ilds_inherits(il_data_rep, list(mlds_type))
    = ilasm.extends.

mlds_inherits_to_ilds_inherits(DataRep, Inherits) = Extends :-
    (
        Inherits = [],
        Extends = extends_nothing
    ;
        Inherits = [InheritType],
        Extends = extends(mlds_type_to_ilds_class_name(DataRep, InheritType))
    ;
        Inherits = [_, _ | _],
        unexpected($module, $pred, "multiple inheritance not supported")
    ).

:- pred mlds_signature_to_ilds_type_params(il_data_rep::in,
    mlds_func_signature::in, list(il_type)::out) is det.

mlds_signature_to_ilds_type_params(DataRep,
        mlds_func_signature(Args, _Returns), Params) :-
    Params = list.map(mlds_type_to_ilds_type(DataRep), Args).

:- func mlds_arg_to_il_arg(mlds_argument) = pair(ilds.id, mlds_type).

mlds_arg_to_il_arg(mlds_argument(EntityName, Type, _GCStatement)) =
        Id - Type :-
    mangle_entity_name(EntityName, Id).

:- func mlds_signature_to_ilds_type_params(il_data_rep, mlds_func_signature)
    = list(il_type).

mlds_signature_to_ilds_type_params(DataRep,
        mlds_func_signature(Args, _Returns)) =
    list.map(mlds_type_to_ilds_type(DataRep), Args).

:- func mlds_signature_to_il_return_param(il_data_rep, mlds_func_signature)
    = ret_type.

mlds_signature_to_il_return_param(DataRep, mlds_func_signature(_, Returns))
        = Param :-
    (
        Returns = [],
        Param = void
    ;
        Returns = [ReturnType],
        SimpleType = mlds_type_to_ilds_simple_type(DataRep, ReturnType),
        Param = simple_type(SimpleType)
    ;
        Returns = [_, _ | _],
        % IL doesn't support multiple return values
        sorry($module, $pred, "multiple return values")
    ).

params_to_il_signature(DataRep, ModuleName, FuncParams) = ILSignature :-
    ILInputTypes = list.map(input_param_to_ilds_type(DataRep, ModuleName),
        Inputs),
    FuncParams = mlds_func_params(Inputs, Outputs),
    (
        Outputs = [],
        Param = void
    ;
        Outputs = [ReturnType],
        SimpleType = mlds_type_to_ilds_simple_type(DataRep, ReturnType),
        Param = simple_type(SimpleType)
    ;
        Outputs = [_, _ | _],
        % IL doesn't support multiple return values.
        sorry($module, $pred, "multiple return values")
    ),
    ILSignature = signature(call_conv(no, default), Param, ILInputTypes).

:- func input_param_to_ilds_type(il_data_rep, mlds_module_name, mlds_argument)
    = il_method_param.

input_param_to_ilds_type(DataRep, _ModuleName, Arg) = MethodParam :-
    Arg = mlds_argument(EntityName, MldsType, _GCStatement),
    mangle_entity_name(EntityName, Id),
    ILType = mlds_type_to_ilds_type(DataRep, MldsType),
    MethodParam = il_method_param(ILType, yes(Id)).

:- func mlds_type_to_ilds_simple_type(il_data_rep, mlds_type)
    = ilds.simple_type.

mlds_type_to_ilds_simple_type(DataRep, MLDSType) = SimpleType :-
    il_type(_, SimpleType) = mlds_type_to_ilds_type(DataRep, MLDSType).

    % XXX Make sure all the types are converted correctly.

mlds_type_to_ilds_type(_, mlds_rtti_type(_RttiName)) = il_object_array_type.

    % This is a placeholder only.
mlds_type_to_ilds_type(_, mlds_tabling_type(_Id)) = il_object_array_type.

mlds_type_to_ilds_type(DataRep, mlds_mercury_array_type(ElementType)) =
    ( ElementType = mercury_type(_, ctor_cat_variable, _) ->
        il_generic_array_type
    ;
        il_type([], '[]'(mlds_type_to_ilds_type(DataRep, ElementType), []))
    ).

mlds_type_to_ilds_type(DataRep, mlds_array_type(ElementType)) =
    il_type([], '[]'(mlds_type_to_ilds_type(DataRep, ElementType), [])).

mlds_type_to_ilds_type(_, mlds_mostly_generic_array_type(_)) = _ :-
    sorry($module, $pred, "mlds_mostly_generic_array_type").

    % XXX Should be checked.
mlds_type_to_ilds_type(_, mlds_type_info_type) = il_generic_type.

    % This is tricky. It could be an integer, or it could be a System.Array.
mlds_type_to_ilds_type(_, mlds_pseudo_type_info_type) = il_generic_type.

    % IL has a pretty fuzzy idea about function types. We treat them
    % as integers for now
    % XXX This means the code is not verifiable.
mlds_type_to_ilds_type(_, mlds_func_type(_)) = il_type([], int32).

mlds_type_to_ilds_type(_, mlds_generic_type) = il_generic_type.

    % XXX Using int32 here means the code is not verifiable
    % see comments about function types above.
mlds_type_to_ilds_type(_, mlds_cont_type(_ArgTypes)) = il_type([], int32).

mlds_type_to_ilds_type(_, mlds_class_type(Class, Arity, Kind)) =
        il_type([], SimpleType) :-
    ClassName = mlds_class_name_to_ilds_class_name(Class, Arity),
    SimpleType = mlds_class_to_ilds_simple_type(Kind, ClassName).

mlds_type_to_ilds_type(_, mlds_commit_type) = il_commit_type.

mlds_type_to_ilds_type(ILDataRep, mlds_generic_env_ptr_type) =
    ILDataRep^il_envptr_type.

mlds_type_to_ilds_type(_, mlds_native_bool_type) = il_type([], bool).

mlds_type_to_ilds_type(_, mlds_native_char_type) = il_type([], char).

    % These two following choices are arbitrary -- IL has native integer
    % and float types too. It's not clear whether there is any benefit
    % in mapping to them instead -- it all depends what the indended use
    % of mlds_native_int_type and mlds_native_float_type is.
    % Any mapping other than int32 would have to be examined to see
    % whether it is going to be compatible with int32.
mlds_type_to_ilds_type(_, mlds_native_int_type) = il_type([], int32).

mlds_type_to_ilds_type(_, mlds_native_float_type) = il_type([], float64).

mlds_type_to_ilds_type(_, mlds_foreign_type(ForeignType))
        = il_type([], Class) :-
    (
        ForeignType = il(il_type(RefOrVal, Assembly, Type)),
        sym_name_to_class_name(Type, ForeignClassName),
        (
            RefOrVal = reference,
            Class = class(structured_name(assembly(Assembly),
                ForeignClassName, []))
        ;
            RefOrVal = value,
            Class = valuetype(structured_name(assembly(Assembly),
                ForeignClassName, []))
        )
    ;
        ForeignType = c(_),
        unexpected($module, $pred, "c foreign type")
    ;
        ForeignType = java(_),
        unexpected($module, $pred, "java foreign type")
    ;
        ForeignType = csharp(_),
        unexpected($module, $pred, "csharp foreign type")
    ;
        ForeignType = erlang(_),
        unexpected($module, $pred, "erlang foreign type")
    ).

mlds_type_to_ilds_type(ILDataRep, mlds_ptr_type(MLDSType)) =
    il_type([], '&'(mlds_type_to_ilds_type(ILDataRep, MLDSType))).

mlds_type_to_ilds_type(ILDataRep, mercury_type(MercuryType, TypeCategory, _)) =
    mlds_mercury_type_to_ilds_type(ILDataRep, MercuryType, TypeCategory).

mlds_type_to_ilds_type(_, mlds_unknown_type) = _ :-
    unexpected($module, $pred, "unknown_type").

    % Get the corresponding ILDS type for an MLDS mercury type
    % (this depends on which representation you happen to be using).
    % The entry for the void type is a dummy; there shouldn't be values
    % of type void, so the type is moot.
    %
:- func mlds_mercury_type_to_ilds_type(il_data_rep, mer_type,
    type_ctor_category) = il_type.

mlds_mercury_type_to_ilds_type(DataRep, MercuryType, CtorCat) = ILType :-
    (
        ( CtorCat = ctor_cat_builtin(cat_builtin_int)
        ; CtorCat = ctor_cat_void
        ),
        ILType = il_type([], int32)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        ILType = il_type([], char)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        ILType = il_type([], float64)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        ILType = il_string_type
    ;
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_enum(_)
        ),
        ILType = il_object_array_type
    ;
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ),
        ILType = il_generic_type
    ;
        % We should handle ctor_cat_user(cat_user_direct_dummy) specially.
        ( CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(_)
        ),
        (
            DataRep ^ highlevel_data = yes,
            \+ type_needs_lowlevel_rep(target_il, MercuryType)
        ->
            ILType = mercury_type_to_highlevel_class_type(MercuryType)
        ;
            ILType = il_object_array_type
        )
    ).

:- func mlds_class_to_ilds_simple_type(mlds_class_kind, ilds.class_name) =
    ilds.simple_type.

mlds_class_to_ilds_simple_type(Kind, ClassName) = SimpleType :-
    ( Kind = mlds_package,     SimpleType = class(ClassName)
    ; Kind = mlds_class,       SimpleType = class(ClassName)
    ; Kind = mlds_interface,   SimpleType = class(ClassName)
    ; Kind = mlds_struct,      SimpleType = valuetype(ClassName)
    ; Kind = mlds_enum,        SimpleType = valuetype(ClassName)
    ).

:- func mercury_type_to_highlevel_class_type(mer_type) = il_type.

mercury_type_to_highlevel_class_type(MercuryType) = ILType :-
    type_to_ctor_det(MercuryType, TypeCtor),
    ml_gen_type_name(TypeCtor, ClassName, Arity),
    ILType = il_type([], class(
        mlds_class_name_to_ilds_class_name(ClassName, Arity))).

:- func mlds_class_name_to_ilds_class_name(mlds_class, arity) =
    ilds.class_name.

mlds_class_name_to_ilds_class_name(QualClassName, Arity) = IldsClassName :-
    QualClassName = qual(MldsModuleName, _QualKind, MldsClassName0),
    MldsClassName = string.format("%s_%d", [s(MldsClassName0), i(Arity)]),
    IldsClassName = append_toplevel_class_name(
        mlds_module_name_to_class_name(MldsModuleName), MldsClassName).

mlds_type_to_ilds_class_name(DataRep, MldsType) =
    get_ilds_type_class_name(mlds_type_to_ilds_type(DataRep, MldsType)).

:- func get_ilds_type_class_name(il_type) = ilds.class_name.

get_ilds_type_class_name(ILType) = ClassName :-
    (
        ( ILType = il_type(_, class(ClassName0))
        ; ILType = il_type(_, valuetype(ClassName0))
        )
    ->
        ClassName = ClassName0
    ;
        unexpected($module, $pred, "type not a class")
    ).

%-----------------------------------------------------------------------------
%
% Name mangling.

:- type il_mangle_name
    --->    mangle_for_il
            % Names that are to be used only in IL are able to include
            % spaces, punctuation and other special characters, because they
            % are in quotes.

    ;       mangle_for_csharp.
            % Names that are to be used in C# (typically because they are
            % foreign procedures) must be mangled in the same way as for C.

    % Create a mangled predicate identifier, suitable for use in IL.
    %
:- pred predlabel_to_ilds_id(mlds_pred_label::in, proc_id::in,
    maybe(mlds_func_sequence_num)::in, ilds.id::out) is det.

predlabel_to_ilds_id(PredLabel, ProcId, MaybeSeqNum, Id) :-
    predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, mangle_for_il, Id).

predlabel_to_csharp_id(PredLabel, ProcId, MaybeSeqNum, Id) :-
    predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, mangle_for_csharp, Id).

    % XXX We may need to do different name mangling for CLS compliance
    % than we would otherwise need.
    %
    % We mangle as follows:
    %   - Problem:
    %     Two preds or funcs with different arities in Mercury
    %     end up having the same types and arities in IL, e.g.
    %     because one of them takes io.state arguments which
    %     get omitted in IL.
    %
    %     To avoid this we append _<arity> to every procedure
    %     name.
    %
    %   - Problem:
    %     A semidet pred returns its success value, and so has
    %     the same return type (bool) as a function.
    %
    %     To avoid this, we mangle all semidet predicates
    %     to indicate that they are a pred by appending _p.
    %
    %   - Problem:
    %     A function with modes other than the default (in, in,
    %     in = out) may clash with a predicate which has the
    %     same types and modes.
    %
    %     In addition, a function may clash with a predicate which
    %     has the same types and modes and has been introduced
    %     by inlining.These will then differ only in return type,
    %     which is forbidden.
    %
    %     To avoid this, we mangle all functions by adding _f
    %     to the procedure name.
    %
    %   - Problem:
    %     A predicate or function with more than one mode.
    %
    %     To avoid this, we mangle all modes > 0 by adding
    %     _m<modenum> to the procedure name.
    %
    %   - We append the sequence number (if there is one) as
    %     _i<seqnum>.
    %
    %   - We prepend the module name (if there is one) as
    %     <modulename>_.
    %
    % So the mangled name is:
    % (<modulename>_)<procname>_<arity>(_f|_p)(_m<modenum>)(_i<seqnum>)
    %
    % Where parentheses indicate optional components.
    %
    % Since each optional component (except the modulename) is after
    % the mandatory arity, and the components have unique prefixes,
    % it isn't possible to generate names that conflict with user
    % names.
    %
    % XXX I think that it may be possible to have conflicts with
    % user names in the case where there is a <modulename>. - fjh
    %
:- pred predlabel_to_id(mlds_pred_label::in, proc_id::in,
    maybe(mlds_func_sequence_num)::in, il_mangle_name::in,
    ilds.id::out) is det.

predlabel_to_id(mlds_user_pred_label(PredOrFunc, MaybeModuleName, Name, Arity,
        CodeModel, _NonOutputFunc), ProcId, MaybeSeqNum, MangleType, Id) :-
    (
        MaybeModuleName = yes(ModuleName),
        mlds_to_il.sym_name_to_string(ModuleName, MStr),
        string.format("%s_", [s(MStr)], MaybeModuleStr)
    ;
        MaybeModuleName = no,
        MaybeModuleStr = ""
    ),
    (
        PredOrFunc = pf_predicate,
        ( CodeModel = model_semi ->
            PredOrFuncStr = "_p"
        ;
            PredOrFuncStr = ""
        )
    ;
        PredOrFunc = pf_function,
        PredOrFuncStr = "_f"
    ),
    proc_id_to_int(ProcId, ProcIdInt),
    ( ProcIdInt = 0 ->
        MaybeProcIdInt = ""
    ;
        string.format("_m%d", [i(ProcIdInt)], MaybeProcIdInt)
    ),
    (
        MaybeSeqNum = yes(SeqNum),
        string.format("_i%d", [i(SeqNum)], MaybeSeqNumStr)
    ;
        MaybeSeqNum = no,
        MaybeSeqNumStr = ""
    ),
    MangledName = mangle_pred_name(Name, MangleType),
    string.format("%s%s_%d%s%s%s", [
        s(MaybeModuleStr), s(MangledName),
        i(Arity), s(PredOrFuncStr), s(MaybeProcIdInt),
        s(MaybeSeqNumStr)], Id).

predlabel_to_id(mlds_special_pred_label(PredName, MaybeModuleName, TypeName,
        Arity), ProcId, MaybeSeqNum, MangleType, Id) :-
    proc_id_to_int(ProcId, ProcIdInt),
    (
        MaybeModuleName = yes(ModuleName),
        mlds_to_il.sym_name_to_string(ModuleName, MStr),
        string.format("%s_", [s(MStr)], MaybeModuleStr)
    ;
        MaybeModuleName = no,
        MaybeModuleStr = ""
    ),
    (
        MaybeSeqNum = yes(SeqNum),
        string.format("_%d", [i(SeqNum)], MaybeSeqNumStr)
    ;
        MaybeSeqNum = no,
        MaybeSeqNumStr = ""
    ),
    MangledName = mangle_pred_name(PredName, MangleType),
    string.format("special_%s%s_%s_%d_%d%s",
        [s(MaybeModuleStr), s(MangledName), s(TypeName), i(Arity),
            i(ProcIdInt), s(MaybeSeqNumStr)], Id).

:- func mangle_pred_name(string, il_mangle_name) = string.

mangle_pred_name(PredName, mangle_for_il) = PredName.
mangle_pred_name(PredName, mangle_for_csharp) = MangledName :-
    ( string.is_all_alnum_or_underscore(PredName) ->
        MangledName = PredName
    ;
        MangledName = convert_to_valid_c_identifier(PredName)
    ).

    % If an mlds_var is not an argument or a local, what is it?
    % We assume the given variable is a static field;
    % either a compiler-generated static,
    % or possibly a handwritten RTTI reference or a
    % reference to some hand-written code in the
    % modulename__csharp_code.mercury_code class.
    %
:- func make_static_fieldref(il_data_rep, mlds_var, mlds_type) = fieldref.

make_static_fieldref(DataRep, Var, VarType) = FieldRef :-
    Var = qual(ModuleName, _QualKind, VarName),
    mangle_mlds_var(Var, MangledVarStr),
    mangle_dataname_module(yes(mlds_data_var(VarName)),
        ModuleName, NewModuleName),
    ClassName = mlds_module_name_to_class_name(NewModuleName),
    FieldRef = make_fieldref(mlds_type_to_ilds_type(DataRep, VarType),
        ClassName, MangledVarStr).

:- pred mangle_foreign_code_module(foreign_language::in,
    mlds_module_name::in, mlds_module_name::out) is det.

mangle_foreign_code_module(Lang, ModuleName0, ModuleName) :-
    LangStr = simple_foreign_language_string(Lang),
    PackageName0 = mlds_module_name_to_package_name(ModuleName0),
    (
        PackageName0 = qualified(Q, M0),
        M = string.format("%s__%s_code", [s(M0), s(LangStr)]),
        PackageName = qualified(Q, M)
    ;
        PackageName0 = unqualified(M0),
        M = string.format("%s__%s_code", [s(M0), s(LangStr)]),
        PackageName = unqualified(M)
    ),
    SymName0 = mlds_module_name_to_sym_name(ModuleName0),
    % Check to see whether or not the name has already been qualified
    % with the wrapper class. If not, qualify it.
    ( SymName0 = qualified(SymName1, wrapper_class_name) ->
        (
            SymName1 = qualified(SQ, SM0),
            SM = string.format("%s__%s_code", [s(SM0), s(LangStr)]),
            SymName2 = qualified(SQ, SM)
        ;
            SymName1 = unqualified(SM0),
            SM = string.format("%s__%s_code", [s(SM0), s(LangStr)]),
            SymName2 = unqualified(SM)
        ),
        SymName = qualified(SymName2, wrapper_class_name)
    ;
        (
            SymName0 = qualified(SQ, SM0),
            SM = string.format("%s__%s_code", [s(SM0), s(LangStr)]),
            SymName = qualified(qualified(SQ, SM), wrapper_class_name)
        ;
            SymName0 = unqualified(SM0),
            SM = string.format("%s__%s_code", [s(SM0), s(LangStr)]),
            SymName = qualified(unqualified(SM), wrapper_class_name)
        )
    ),
    ModuleName = mercury_module_and_package_name_to_mlds(PackageName, SymName).

    % When generating references to RTTI, we need to mangle the module name
    % if the RTTI is defined in C code by hand. If no data_name is provided,
    % always do the mangling.
    %
:- pred mangle_dataname_module(maybe(mlds_data_name)::in,
    mlds_module_name::in, mlds_module_name::out) is det.

mangle_dataname_module(no, !ModuleName) :-
    mangle_foreign_code_module(lang_csharp, !ModuleName).

mangle_dataname_module(yes(DataName), !ModuleName) :-
    (
        SymName = mlds_module_name_to_sym_name(!.ModuleName),
        SymName = qualified(qualified(unqualified("mercury"),
            LibModuleName0), wrapper_class_name),
        DataName = mlds_data_var(_),
        LibModuleName0 = "private_builtin",
        CodeString = "__csharp_code"
    ->
        string.append(LibModuleName0, CodeString, LibModuleName),
        !:ModuleName = mercury_module_name_to_mlds(
            qualified(qualified(unqualified("mercury"),
            LibModuleName), wrapper_class_name))
    ;
        true
    ).

:- pred mangle_dataname(mlds_data_name::in, string::out) is det.

mangle_dataname(DataName, Name) :-
    (
        DataName = mlds_data_var(MLDSVarName),
        Name = mangle_mlds_var_name(MLDSVarName)
    ;
        DataName = mlds_scalar_common_ref(_),
        sorry($module, $pred, "unimplemented: mangling mlds_scalar_common_ref")
    ;
        DataName = mlds_rtti(RttiId),
        rtti.id_to_c_identifier(RttiId, Name)
    ;
        DataName = mlds_module_layout,
        sorry($module, $pred, "unimplemented: mangling mlds_module_layout")
    ;
        DataName = mlds_proc_layout(_),
        sorry($module, $pred, "unimplemented: mangling mlds_proc_layout")
    ;
        DataName = mlds_internal_layout(_, _),
        sorry($module, $pred, "unimplemented: mangling mlds_internal_layout")
    ;
        DataName = mlds_tabling_ref(_, _),
        sorry($module, $pred, "unimplemented: mangling mlds_tabling_ref")
    ).

    % We turn procedures into methods of classes.
mangle_mlds_proc_label(qual(ModuleName, _, mlds_proc_label(PredLabel, ProcId)),
        MaybeSeqNum, ClassName, PredStr) :-
    ClassName = mlds_module_name_to_class_name(ModuleName),
    predlabel_to_ilds_id(PredLabel, ProcId, MaybeSeqNum, PredStr).

:- pred mangle_entity_name(mlds_entity_name::in, string::out) is det.

mangle_entity_name(entity_type(_TypeName, _), _MangledName) :-
    unexpected($module, $pred, "can't mangle type names").
mangle_entity_name(entity_data(DataName), MangledName) :-
    mangle_dataname(DataName, MangledName).
mangle_entity_name(entity_function(_, _, _, _), _MangledName) :-
    unexpected($module, $pred, "can't mangle function names").
mangle_entity_name(entity_export(_), _MangledName) :-
    unexpected($module, $pred, "can't mangle export names").

    % Any valid Mercury identifier will be fine here too.
    % We quote all identifiers before we output them, so
    % even funny characters should be fine.
mangle_mlds_var(qual(_ModuleName, _, VarName), Str) :-
    Str = mangle_mlds_var_name(VarName).

:- func mangle_mlds_var_name(mlds_var_name) = string.

mangle_mlds_var_name(mlds_var_name(Name, yes(Num))) =
    string.format("%s_%d", [s(Name), i(Num)]).
mangle_mlds_var_name(mlds_var_name(Name, no)) = Name.

:- pred mlds_to_il.sym_name_to_string(sym_name::in, string::out) is det.

mlds_to_il.sym_name_to_string(SymName, String) :-
    mlds_to_il.sym_name_to_string(SymName, ".", String).

:- pred mlds_to_il.sym_name_to_string(sym_name::in, string::in, string::out)
    is det.

mlds_to_il.sym_name_to_string(SymName, Separator, String) :-
    mlds_to_il.sym_name_to_string_2(SymName, Separator, [], Parts),
    string.append_list(Parts, String).

:- pred mlds_to_il.sym_name_to_string_2(sym_name::in, string::in,
    list(string)::in, list(string)::out) is det.

mlds_to_il.sym_name_to_string_2(qualified(ModuleSpec,Name), Separator,
        !Strs) :-
    mlds_to_il.sym_name_to_string_2(ModuleSpec, Separator, !Strs),
    !:Strs = [Separator, Name | !.Strs].
mlds_to_il.sym_name_to_string_2(unqualified(Name), _, !Strs) :-
    !:Strs = [Name | !.Strs].

    % Turn an MLDS module name into a class_name name.
    %
:- func mlds_module_name_to_class_name(mlds_module_name) = ilds.class_name.

mlds_module_name_to_class_name(MldsModuleName) =
        structured_name(AssemblyName, ClassName, []) :-
    SymName = mlds_module_name_to_sym_name(MldsModuleName),
    sym_name_to_class_name(SymName, ClassName),
    AssemblyName = mlds_module_name_to_assembly_name(MldsModuleName).

:- func mlds_module_name_to_assembly_name(mlds_module_name) = assembly_name.

mlds_module_name_to_assembly_name(MldsModuleName) = AssemblyName :-
    SymName = mlds_module_name_to_sym_name(MldsModuleName),
    PackageSymName = mlds_module_name_to_package_name(MldsModuleName),
    sym_name_to_class_name(SymName, ClassName),
    (
        ClassName = ["mercury" | _]
    ->
        AssemblyName = assembly("mercury")
    ;
        % Foreign code currently resides in it's own assembly even if it is
        % in a sub-module.
        PackageSymName = qualified(_, Name),
        ( string.remove_suffix(Name, "__csharp_code", _)
        ; string.remove_suffix(Name, "__cpp_code", _)
        )
    ->
        mlds_to_il.sym_name_to_string(PackageSymName, PackageString),
        AssemblyName = assembly(PackageString)
    ;
        mlds_to_il.sym_name_to_string(PackageSymName, PackageString),
        (
            PackageSymName = unqualified(_),
            AssemblyName = assembly(PackageString)
        ;
            PackageSymName = qualified(_, _),
            AssemblyName = module(PackageString,
                outermost_qualifier(PackageSymName))
        )
    ).

:- pred sym_name_to_class_name(sym_name::in, list(ilds.id)::out) is det.

sym_name_to_class_name(SymName, Ids) :-
    sym_name_to_class_name_2(SymName, Ids0),
    list.reverse(Ids0, Ids).

:- pred sym_name_to_class_name_2(sym_name::in, list(ilds.id)::out) is det.

sym_name_to_class_name_2(qualified(ModuleSpec, Name), [Name | Modules]) :-
    sym_name_to_class_name_2(ModuleSpec, Modules).
sym_name_to_class_name_2(unqualified(Name), [Name]).

:- func sym_name_prefix(sym_name) = string.
sym_name_prefix(qualified(ModuleSpec, _)) = sym_name_prefix(ModuleSpec).
sym_name_prefix(unqualified(Name)) = Name.

%-----------------------------------------------------------------------------%
%
% Predicates for checking various attributes of variables
%

:- pred is_argument(ilds.id::in, il_info::in) is semidet.

is_argument(VarName, Info) :-
    list.member(VarName - _, Info ^ arguments).

:- pred is_local(ilds.id::in, il_info::in) is semidet.

is_local(VarName, Info) :-
    map.contains(Info ^ locals, VarName).

:- pred is_local_field(mlds_var::in, mlds_type::in, il_info::in,
    fieldref::out) is semidet.

is_local_field(Var, VarType, Info, FieldRef) :-
    mangle_mlds_var(Var, VarName),
    set.member(VarName, Info ^ field_names),
    Var = qual(ModuleName, _, _),
    ClassName = mlds_module_name_to_class_name(ModuleName),
    FieldRef = make_fieldref(
        mlds_type_to_ilds_type(Info ^ il_data_rep, VarType),
        ClassName, VarName).

%-----------------------------------------------------------------------------%
%
% Preds and funcs to find the types of rvals
%

    % This gives us the type of an rval. This type is an MLDS type,
    % but is with respect to the IL representation (that is, we map code
    % address and data address constants to the MLDS version of their IL
    % representation). This is so you can generate appropriate box rvals for
    % rval_consts.
    %
:- pred rval_to_type(mlds_rval::in, mlds_type::out) is det.

rval_to_type(ml_lval(ml_var(_, Type)), Type).
rval_to_type(ml_lval(ml_field(_, _, _, Type, _)), Type).
rval_to_type(ml_lval(ml_mem_ref(_, Type)), Type).
rval_to_type(ml_lval(ml_global_var_ref(_)), _) :-
    sorry($module, $pred, "global_var_ref").
rval_to_type(ml_mkword(_, _), _) :-
    unexpected($module, $pred, "mkword").
rval_to_type(ml_unop(Unop, _), Type) :-
    (
        Unop = box(_),
        Type = mlds_generic_type
    ;
        Unop = unbox(UnboxType),
        Type = UnboxType
    ;
        Unop = cast(CastType),
        Type = CastType
    ;
        Unop = std_unop(StdUnop),
        functor(StdUnop, canonicalize, StdUnopStr, _Arity),
        sorry($module, $pred, "unop: " ++ StdUnopStr)
    ).
rval_to_type(ml_binop(_, _, _), _) :-
    sorry($module, $pred, "binop").
rval_to_type(ml_mem_addr(_), _) :-
    sorry($module, $pred, "mem_addr").
rval_to_type(ml_scalar_common(ScalarCommon), Type) :-
    ScalarCommon = ml_scalar_common(_, Type, _, _).
rval_to_type(ml_vector_common_row(VectorCommon, _), Type) :-
    VectorCommon = ml_vector_common(_, Type, _, _, _).
rval_to_type(ml_self(Type), Type).
rval_to_type(ml_const(Const), Type) :-
    Type = rval_const_to_type(Const).

:- func rval_const_to_type(mlds_rval_const) = mlds_type.

rval_const_to_type(mlconst_data_addr(_)) = mlds_array_type(mlds_generic_type).
rval_const_to_type(mlconst_code_addr(_))
        = mlds_func_type(mlds_func_params([], [])).
rval_const_to_type(mlconst_int(_)) = ml_int_type.
rval_const_to_type(mlconst_enum(_, MLDS_Type)) = MLDS_Type.
rval_const_to_type(mlconst_char(_)) = ml_char_type.
rval_const_to_type(mlconst_foreign(_, _, _))
        = sorry($module, $pred, "IL backend and foreign tag").
rval_const_to_type(mlconst_float(_)) = MLDSType :-
    FloatType = builtin_type(builtin_type_float),
    MLDSType = mercury_type(FloatType, ctor_cat_builtin(cat_builtin_float),
        non_foreign_type(FloatType)).
rval_const_to_type(mlconst_false) = mlds_native_bool_type.
rval_const_to_type(mlconst_true) = mlds_native_bool_type.
rval_const_to_type(mlconst_string(_)) = ml_string_type.
rval_const_to_type(mlconst_multi_string(_)) = ml_string_type.
rval_const_to_type(mlconst_named_const(_))
        = sorry($module, $pred, "IL backend and named const").
rval_const_to_type(mlconst_null(MldsType)) = MldsType.

%-----------------------------------------------------------------------------%

:- func code_addr_constant_to_methodref(il_data_rep, mlds_code_addr) =
    methodref.

code_addr_constant_to_methodref(DataRep, code_addr_proc(ProcLabel, Sig))
        = MethodRef :-
    mangle_mlds_proc_label(ProcLabel, no, ClassName, ProcLabelStr),
    ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig),
    TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig),
    MemberName = class_member_name(ClassName, id(ProcLabelStr)),
    MethodRef = methoddef(call_conv(no, default), ReturnParam,
        MemberName, TypeParams).

code_addr_constant_to_methodref(DataRep,
        code_addr_internal(ProcLabel, SeqNum, Sig)) = MethodRef :-
    mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName, ProcLabelStr),
    TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig),
    ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig),
    MemberName = class_member_name(ClassName, id(ProcLabelStr)),
    MethodRef = methoddef(call_conv(no, default), ReturnParam,
        MemberName, TypeParams).

    % Assumed to be a field of a class.
    %
:- pred data_addr_constant_to_fieldref(mlds_data_addr::in, fieldref::out)
    is det.

data_addr_constant_to_fieldref(data_addr(ModuleName, DataName), FieldRef) :-
    mangle_dataname(DataName, FieldName),
    mangle_dataname_module(yes(DataName), ModuleName, NewModuleName),
    ClassName = mlds_module_name_to_class_name(NewModuleName),
    FieldRef = make_fieldref(il_object_array_type, ClassName, FieldName).

%-----------------------------------------------------------------------------%

    % When we generate mercury terms using classes, we should use this
    % to reference the fields of the class. Note this pred will handle
    % named or offsets. It assumes that an offset is transformed into "f<num>".
    % XXX Should move towards using this code for *all* field name
    % creation and referencing.
    % XXX We remove byrefs from fields here. Perhaps we ought to do this
    % in a separate pass.  See defn_to_class_decl which does the same thing
    % when creating the fields.
    %
:- pred get_fieldref(il_data_rep::in, mlds_field_id::in, mlds_type::in,
    mlds_type::in, fieldref::out, instr_tree::out) is det.

get_fieldref(DataRep, FieldNum, FieldType, ClassType0, FieldRef,
        CastClassInstrs) :-
    ( ClassType0 = mlds_ptr_type(ClassType1) ->
        ClassType = ClassType1
    ;
        ClassType = ClassType0
    ),
    FieldILType0 = mlds_type_to_ilds_type(DataRep, FieldType),
    ( FieldILType0 = il_type(_, '&'(FieldILType1)) ->
        FieldILType = FieldILType1
    ;
        FieldILType = FieldILType0
    ),
    (
        FieldNum = ml_field_offset(OffsetRval),
        ClassName = mlds_type_to_ilds_class_name(DataRep, ClassType),
        ( OffsetRval = ml_const(mlconst_int(Num)) ->
            string.format("f%d", [i(Num)], FieldId)
        ;
            sorry($module, $pred, "offsets for non-mlconst_int rvals")
        ),
        CastClassInstrs = empty
    ;
        FieldNum = ml_field_named(qual(ModuleName, _, FieldId), _CtorType),
        % The MLDS doesn't record which qualifiers are class qualifiers
        % and which are namespace qualifiers... we first generate
        % a name for the CtorClass as if it wasn't nested, and then
        % we call fixup_class_qualifiers to make it correct.
        % XXX This is a bit of a hack.  It would be nicer for the
        % MLDS to keep the information around.
        CtorClassName = mlds_module_name_to_class_name(ModuleName),
        PtrClassName = mlds_type_to_ilds_class_name(DataRep, ClassType),
        ClassName = fixup_class_qualifiers(CtorClassName, PtrClassName),
        ( PtrClassName = CtorClassName ->
            CastClassInstrs = empty
        ;
            CastClassInstrs = singleton(
                castclass(il_type([], class(ClassName))))
        )
    ),
    FieldRef = make_fieldref(FieldILType, ClassName, FieldId).

    % The CtorClass will be nested inside the base class.
    % But when we initially generate the name, we don't
    % know that it is nested.  This routine fixes up the
    % CtorClassName by moving the nested parts into the
    % third field of the structured_name.
    %
:- func fixup_class_qualifiers(ilds.class_name, ilds.class_name) =
    ilds.class_name.

fixup_class_qualifiers(CtorClassName0, PtrClassName) = CtorClassName :-
    PtrClassName = structured_name(PtrAssembly, PtrClass, PtrNested),
    CtorClassName0 = structured_name(CtorAssembly, CtorClass, CtorNested),
    (
        % Some sanity checks.
        PtrAssembly = CtorAssembly,
        PtrNested = [],
        CtorNested = []
    ->
        % The part of the prefix which CtorClass shares with PtrClass
        % will be the outermost class name; the remainder of CtorClass,
        % if any, will be a nested class within.
        % (XXX This relies on the way that ml_type_gen.m generates
        % the nested MLDS classes for discriminated unions.)
        common_prefix(CtorClass, PtrClass, OuterClass, NestedClasses, _),
        CtorClassName = structured_name(CtorAssembly, OuterClass,
            NestedClasses)
    ;
        unexpected($module, $pred, "condition failed")
    ).

    % common_prefix(List1, List2, Prefix, Tail1, Tail2):
    %   List1 = Prefix ++ Tail1,
    %   List2 = Prefix ++ Tail2.
:- pred common_prefix(list(T)::in, list(T)::in, list(T)::out, list(T)::out,
    list(T)::out) is det.

common_prefix([],       Ys,     [],       [],       Ys).
common_prefix([X | Xs], [],     [],       [X | Xs], []).
common_prefix([X | Xs], [Y | Ys], Prefix, TailXs,   TailYs) :-
    ( X = Y ->
        common_prefix(Xs, Ys, Prefix1, TailXs, TailYs),
        Prefix = [X | Prefix1]
    ;
        TailXs = [X | Xs],
        TailYs = [Y | Ys],
        Prefix = []
    ).

%-----------------------------------------------------------------------------%

:- pred defn_to_local(mlds_module_name::in, mlds_defn::in,
    pair(ilds.id, mlds_type)::out) is det.

defn_to_local(ModuleName, Defn, Id - MLDSType) :-
    Defn = mlds_defn(Name, _Context, _DeclFlags, Entity),
    (
        Name = entity_data(DataName),
        Entity = mlds_data(MLDSType0, _Initializer, _GCStatement)
    ->
        mangle_dataname(DataName, MangledDataName),
        mangle_mlds_var(qual(ModuleName, module_qual,
            mlds_var_name(MangledDataName, no)), Id),
        MLDSType0 = MLDSType
    ;
        unexpected($module, $pred, "definition name was not data/1")
    ).

%-----------------------------------------------------------------------------%
%
% These functions are for converting to/from generic objects.
%

:- func convert_to_object(il_type) = instr_tree.

convert_to_object(Type) = singleton(box(ValueType)) :-
    Type = il_type(_, SimpleType),
    ValueType = simple_type_to_valuetype(SimpleType).

:- func convert_from_object(il_type) = instr_tree.

convert_from_object(Type) = from_list([unbox(Type), ldobj(Type)]).

:- func simple_type_to_valuetype(simple_type) = il_type.
simple_type_to_valuetype(int8) =
    il_type([], valuetype(il_system_name(["SByte"]))).
simple_type_to_valuetype(int16) =
    il_type([], valuetype(il_system_name(["Int16"]))).
simple_type_to_valuetype(int32) =
    il_type([], valuetype(il_system_name(["Int32"]))).
simple_type_to_valuetype(int64) =
    il_type([], valuetype(il_system_name(["Int64"]))).
simple_type_to_valuetype(uint8) =
    il_type([], valuetype(il_system_name(["Byte"]))).
simple_type_to_valuetype(uint16) =
    il_type([], valuetype(il_system_name(["UInt16"]))).
simple_type_to_valuetype(uint32) =
    il_type([], valuetype(il_system_name(["UInt32"]))).
simple_type_to_valuetype(uint64) =
    il_type([], valuetype(il_system_name(["UInt64"]))).
simple_type_to_valuetype(float32) =
    il_type([], valuetype(il_system_name(["Single"]))).
simple_type_to_valuetype(float64) =
    il_type([], valuetype(il_system_name(["Double"]))).
simple_type_to_valuetype(bool) =
    il_type([], valuetype(il_system_name(["Boolean"]))).
simple_type_to_valuetype(char) =
    il_type([], valuetype(il_system_name(["Char"]))).
simple_type_to_valuetype(object) = _ :-
    % il_type([], valuetype(il_system_name(["Object"]))).
    unexpected($module, $pred, "no value class for System.Object").
simple_type_to_valuetype(string) = _ :-
    % il_type([], valuetype(il_system_name(["String"]))).
    unexpected($module, $pred, "no value class for System.String").
simple_type_to_valuetype(refany) = _ :-
    unexpected($module, $pred, "no value class for refany").
simple_type_to_valuetype(class(_)) = _ :-
    unexpected($module, $pred, "no value class for class").
simple_type_to_valuetype(valuetype(Name)) =
    il_type([], valuetype(Name)).
simple_type_to_valuetype(interface(_)) = _ :-
    unexpected($module, $pred, "no value class for interface").
simple_type_to_valuetype('[]'(_, _)) = _ :-
    unexpected($module, $pred, "no value class for array").
simple_type_to_valuetype('&'( _)) = _ :-
    unexpected($module, $pred, "no value class for '&'").
simple_type_to_valuetype('*'(_)) = _ :-
    unexpected($module, $pred, "no value class for '*'").
simple_type_to_valuetype(native_float) = _ :-
    unexpected($module, $pred, "no value class for native float").
simple_type_to_valuetype(native_int) = _ :-
    unexpected($module, $pred, "no value class for native int").
simple_type_to_valuetype(native_uint) = _ :-
    unexpected($module, $pred, "no value class for native uint").

%-----------------------------------------------------------------------------%

:- func il_bool_type = il_type.

il_bool_type = simple_type_to_valuetype(bool).

%-----------------------------------------------------------------------------%
%
% The mapping of the string type.
%

:- func il_string_equals = methodref.

il_string_equals = get_static_methodref(il_string_class_name, id("Equals"),
    simple_type(bool), [il_string_type, il_string_type]).

:- func il_string_compare = methodref.

il_string_compare = get_static_methodref(il_string_class_name, id("Compare"),
    simple_type(int32), [il_string_type, il_string_type]).

    % Note that we need to use the hash function from the Mercury standard
    % library, rather than the one from the .NET BCL (Base Class Library),
    % because it must match the one used by the Mercury compiler when
    % computing the hash tables for string switches.
    %
:- func il_mercury_string_hash = methodref.

il_mercury_string_hash = get_static_methodref(mercury_string_class_name,
    id("hash_2"), simple_type(int32), [il_string_type]).

:- func il_string_class_name = ilds.class_name.

il_string_class_name = il_system_name(["String"]).

:- func il_string_simple_type = simple_type.

il_string_simple_type = class(il_string_class_name).

:- func il_string_type = il_type.

il_string_type = il_type([], il_string_simple_type).

:- func mercury_string_class_name = ilds.class_name.

mercury_string_class_name = mercury_library_name(StringClass) :-
    sym_name_to_class_name(qualified(unqualified("string"),
        wrapper_class_name), StringClass).

%-----------------------------------------------------------------------------%
%
% The mapping of the generic type (used like MR_Box)
%

:- func il_generic_type = il_type.

il_generic_type = il_type([], il_generic_simple_type).

:- func il_generic_simple_type = simple_type.

il_generic_simple_type = class(il_generic_class_name).

il_generic_class_name = il_system_name(["Object"]).

    % Return the class name for System.ValueType.
    %
:- func il_generic_valuetype_name = ilds.class_name.

il_generic_valuetype_name = il_system_name(["ValueType"]).

    % Return the class name for System.Enum
    %
:- func il_generic_enum_name = ilds.class_name.

il_generic_enum_name = il_system_name(["Enum"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the object array type (used like MR_Word).
%
    % il_object_array_type means array of System.Object.
    %
:- func il_object_array_type = il_type.

il_object_array_type = il_type([], '[]'(il_generic_type, [])).

%-----------------------------------------------------------------------------%
%
% The mapping of the library array type (array(T))
%

    % il_generic_array_type means array of System.Object.
    %
:- func il_generic_array_type = il_type.

il_generic_array_type = il_type([], class(il_system_name(["Array"]))).

%-----------------------------------------------------------------------------%
%
% The class that performs conversion operations
%

:- func il_conversion_class_name = ilds.class_name.

il_conversion_class_name = mercury_runtime_name(["Convert"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the exception type.
%

:- func il_exception_type = il_type.

il_exception_type = il_type([], il_exception_simple_type).

:- func il_exception_simple_type = simple_type.

il_exception_simple_type = class(il_exception_class_name).

:- func il_exception_class_name = ilds.class_name.

il_exception_class_name = mercury_runtime_name(["Exception"]).

%-----------------------------------------------------------------------------%

    % The System.Environment.set_ExitCode method (the "setter" for the
    % System.Environment.ExitCode property). We use this to set a non-zero
    % exit status when the main method exits due to an uncaught exception.
    %
:- func il_set_exit_code = methodref.

il_set_exit_code = get_static_methodref(system_environment_class_name,
    id("set_ExitCode"), void, [il_type([], int32)]).

:- func system_environment_class_name = ilds.class_name.

system_environment_class_name = il_system_name(["Environment"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the generic environment pointer type.
%

% Unfortunately the .NET CLR doesn't have any verifiable way of creating a
% generic pointer to an environment, unless you allocate them on the heap.
% Using "refany" (a.k.a. "typedref") *almost* works, except that we need
% to be able to put these pointers in environment structs, and the CLR
% doesn't allow that (see ECMA CLI Partition 1, 8.6.1.3 "Local Signatures").
% So we only do that if the --il-refany-fields option is set.
% If it is not set, then handle_options.m will ensure that we allocate
% the environments on the heap if verifiable code is requested.

% For unverifiable code we allocate environments on the stack and use
% unmanaged pointers.

:- func choose_il_envptr_type(globals) = il_type.

choose_il_envptr_type(Globals) = ILType :-
    globals.lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
    globals.lookup_bool_option(Globals, verifiable_code, Verifiable),
    ( OnHeap = yes ->
        % Use an object reference type.
        ILType = il_heap_envptr_type
    ; Verifiable = yes ->
        % Use "refany", the generic managed pointer type
        ILType = il_type([], refany)
    ;
        % Use unmanaged pointers
        ILType = il_type([], native_uint)
        % XXX We should introduce an ILDS type for unmanaged pointers,
        % rather than using native_uint; that's what IL does, but it sucks
        % -- we should delay the loss of type information to the last possible
        % moment, i.e. when writing out IL.
    ).

:- func il_heap_envptr_type = il_type.

il_heap_envptr_type = il_type([], il_heap_envptr_simple_type).

:- func il_heap_envptr_simple_type = simple_type.

il_heap_envptr_simple_type = class(il_heap_envptr_class_name).

:- func il_heap_envptr_class_name = ilds.class_name.

il_heap_envptr_class_name = mercury_runtime_name(["Environment"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the commit type
%

:- func il_commit_type = il_type.

il_commit_type = il_type([], il_commit_simple_type).

:- func il_commit_simple_type = simple_type.

il_commit_simple_type = class(il_commit_class_name).

:- func il_commit_class_name = ilds.class_name.

il_commit_class_name = mercury_runtime_name(["Commit"]).

%-----------------------------------------------------------------------------

    % Qualify a name with "[mercury]mercury.".
    %
:- func mercury_library_name(ilds.namespace_qual_name) = ilds.class_name.

mercury_library_name(Name) =
    structured_name(assembly("mercury"), ["mercury" | Name], []).

    % Qualify a name with "[mercury]mercury." and add the wrapper class
    % name on the end.
    %
:- func mercury_library_wrapper_class_name(ilds.namespace_qual_name) =
    ilds.class_name.

mercury_library_wrapper_class_name(Name) =
    structured_name(assembly("mercury"),
        ["mercury" | Name] ++ [wrapper_class_name], []).

%-----------------------------------------------------------------------------

    % Qualifiy a name with "[mercury]mercury.runtime.".
    %
:- func mercury_runtime_name(ilds.namespace_qual_name) = ilds.class_name.

mercury_runtime_name(Name) =
    structured_name(assembly("mercury"), ["mercury", "runtime" | Name], []).

%-----------------------------------------------------------------------------

    % Qualifiy a name with "[mscorlib]System.".
    %
:- func il_system_name(ilds.namespace_qual_name) = ilds.class_name.

il_system_name(Name) =
    structured_name(il_system_assembly_name,
        [il_system_namespace_name | Name], []).

:- func il_system_assembly_name = assembly_name.

il_system_assembly_name = assembly("mscorlib").

:- func il_system_namespace_name = string.

il_system_namespace_name = "System".

%-----------------------------------------------------------------------------

    % Generate extern decls for any assembly we reference.
    %
:- pred generate_extern_assembly(string::in, assembly_decl::in,
    bool::in, bool::in, mlds_imports::in, list(il_decl)::out) is det.

generate_extern_assembly(CurrentAssembly, Version, SignAssembly,
        SeparateAssemblies, Imports, AllDecls) :-
    Gen = (pred(Import::in, GenDecls::out) is semidet :-
        (
            Import = mercury_import(compiler_visible_interface, ImportName),
            (
                SignAssembly = yes,
                AsmDecls = mercury_strong_name_assembly_decls
            ;
                SignAssembly = no,
                AsmDecls = []
            )
        ;
            Import = foreign_import(ForeignImportName),
            ForeignImportName = il_assembly_name(ImportName),
            PackageName = mlds_module_name_to_package_name( ImportName),
            ForeignPackageStr = sym_name_to_string(PackageName),
            ( string.prefix(ForeignPackageStr, "System") ->
                AsmDecls = dotnet_system_assembly_decls(Version)
            ;
                AsmDecls = []
            )
        ),
        AsmName = mlds_module_name_to_assembly_name(ImportName),
        (
            AsmName = assembly(Assembly),
            Assembly \= "mercury",
            GenDecls = [ildecl_extern_assembly(Assembly, AsmDecls)]
        ;
            AsmName = module(ModuleName, Assembly),
            (
                SeparateAssemblies = no,
                ( Assembly = CurrentAssembly ->
                    ModuleStr = ModuleName ++ ".dll",
                    GenDecls = [ildecl_file(ModuleStr),
                        ildecl_extern_module(ModuleStr)]
                ;
                    Assembly \= "mercury",
                    GenDecls = [ildecl_extern_assembly(Assembly, AsmDecls)]
                )
            ;
                SeparateAssemblies = yes,
                GenDecls = [ildecl_extern_assembly(ModuleName, AsmDecls)]
            )
        )
    ),
    list.filter_map(Gen, Imports, Decls0),
    list.sort_and_remove_dups(list.condense(Decls0), Decls),
    AllDecls = [
        ildecl_extern_assembly("mercury", [
            version(0, 0, 0, 0),
            public_key_token([
                int8(0x22), int8(0x8C), int8(0x16), int8(0x7D),
                int8(0x12), int8(0xAA), int8(0x0B), int8(0x0B)
            ])
        ]),
        ildecl_extern_assembly("mscorlib",
            dotnet_system_assembly_decls(Version)) | Decls].

:- func dotnet_system_assembly_decls(assembly_decl) = list(assembly_decl).

dotnet_system_assembly_decls(Version)
    = [
        Version,
        public_key_token([
            int8(0xb7), int8(0x7a), int8(0x5c), int8(0x56),
            int8(0x19), int8(0x34), int8(0xE0), int8(0x89)
        ])
    ].

:- func mercury_strong_name_assembly_decls = list(assembly_decl).

mercury_strong_name_assembly_decls
    = [
        version(0, 0, 0, 0),
        public_key_token([
            int8(0x22), int8(0x8C), int8(0x16), int8(0x7D),
            int8(0x12), int8(0xAA), int8(0x0B), int8(0x0B)
        ])
    ].

%-----------------------------------------------------------------------------

:- func make_method_defn(bool, bool, instr_tree) = method_defn.

make_method_defn(DebugIlAsm, VerifiableCode, InstrTree) = MethodDecls :-
    (
        DebugIlAsm = yes,
        Add = 1
    ;
        DebugIlAsm = no,
        Add = 0
    ),
    Instrs = cord.list(InstrTree),
    MaxStack = maxstack(int32(calculate_max_stack(Instrs) + Add)),
    % .zeroinit (which initializes all variables to zero) is required for
    % verifiable code. But if we're generating non-verifiable code, then
    % we can skip it. The code that the Mercury compiler generates doesn't
    % require it, and omitting it may lead to slightly faster code.
    (
        VerifiableCode = yes,
        MethodDecls = [MaxStack, zeroinit, instrs(Instrs)]
    ;
        VerifiableCode = no,
        MethodDecls = [MaxStack, instrs(Instrs)]
    ).

%-----------------------------------------------------------------------------
%
% Some useful functions for generating IL fragments
%

:- func load_this = instr.

load_this = ldarg(index(0)).

:- func call_class_constructor(ilds.class_name) = instr.

call_class_constructor(CtorMemberName) =
    call(get_static_methodref(CtorMemberName, cctor, void, [])).

:- func call_constructor(ilds.class_name) = instr.

call_constructor(CtorMemberName) =
    call(get_constructor_methoddef(CtorMemberName, [])).

:- func throw_unimplemented(string) = instr_tree.

throw_unimplemented(String) =
    from_list([
        ldstr(String),
        newobj(get_instance_methodref(il_exception_class_name,
            ctor, void, [il_string_type])),
        throw]
    ).

:- func newobj_constructor(ilds.class_name, list(il_type)) = instr.

newobj_constructor(CtorMemberName, ArgTypes) =
    newobj(get_constructor_methoddef(CtorMemberName, ArgTypes)).

:- func get_constructor_methoddef(ilds.class_name, list(il_type))
    = methodref.

get_constructor_methoddef(CtorMemberName, ArgTypes) =
    get_instance_methodref(CtorMemberName, ctor, void, ArgTypes).

:- func get_instance_methodref(ilds.class_name, member_name, ret_type,
    list(il_type)) = methodref.

get_instance_methodref(ClassName, MethodName, RetType, TypeParams) =
    methoddef(call_conv(yes, default), RetType,
        class_member_name(ClassName, MethodName), TypeParams).

:- func get_static_methodref(ilds.class_name, member_name, ret_type,
    list(il_type)) = methodref.

get_static_methodref(ClassName, MethodName, RetType, TypeParams) =
    methoddef(call_conv(no, default), RetType,
        class_member_name(ClassName, MethodName), TypeParams).

:- func make_constructor_class_member(method_defn) = class_member.

make_constructor_class_member(MethodDecls) = Member :-
    MethodHead = methodhead([], ctor, signature(call_conv(no, default),
        void, []), []),
    Member = member_method(MethodHead, MethodDecls).

:- func make_fieldref(il_type, ilds.class_name, ilds.id) = fieldref.

make_fieldref(ILType, ClassName, Id) =
    fieldref(ILType, class_member_name(ClassName, id(Id))).

:- func responsible_for_init_runtime_instrs = list(instr).

responsible_for_init_runtime_instrs = [
    call(get_static_methodref(runtime_init_module_name,
        responsible_for_init_runtime_name, simple_type(bool), []))
    ].

:- func runtime_initialization_instrs = list(instr).

runtime_initialization_instrs = [
    call(get_static_methodref(runtime_init_module_name,
        runtime_init_method_name, void, [il_bool_type]))
    ].

:- func runtime_init_module_name = ilds.class_name.

runtime_init_module_name =
    structured_name(assembly("mercury"),
        ["mercury", "runtime", "Init"], []).

:- func runtime_init_method_name = ilds.member_name.

runtime_init_method_name = id("init_runtime").

:- func responsible_for_init_runtime_name = ilds.member_name.

responsible_for_init_runtime_name = id("responsible_for_initialising_runtime").

%-----------------------------------------------------------------------------%
%
% Predicates for manipulating il_info
%

:- func il_info_init(mlds_module_name, ilds.id, mlds_imports,
    il_data_rep, bool, bool, bool, bool, bool) = il_info.

il_info_init(ModuleName, AssemblyName, Imports, ILDataRep,
        DebugIlAsm, VerifiableCode, ByRefTailCalls, MsCLR, RotorCLR) =
    il_info(ModuleName, AssemblyName, Imports, set.init, ILDataRep,
        DebugIlAsm, VerifiableCode, ByRefTailCalls, MsCLR, RotorCLR,
        empty, empty, [], no_main, set.init, set.init,
        map.init, empty, counter.init(1), counter.init(1), no,
        Args, MethodName, CSharpMethodName, DefaultSignature) :-
    Args = [],
    DefaultSignature = signature(call_conv(no, default), void, []),
    MethodName = id(""),
    CSharpMethodName = id("").

:- pred il_info_new_class(mlds_class_defn::in, il_info::in, il_info::out)
    is det.

il_info_new_class(ClassDefn, !Info) :-
    ClassDefn = mlds_class_defn(_, _, _, _, _, _, Members),
    list.filter_map(
        (pred(M::in, S::out) is semidet :-
            M = mlds_defn(Name, _, _, mlds_data(_, _, _)),
            S = entity_name_to_ilds_id(Name)
        ), Members, FieldNames),
    !Info ^ alloc_instrs := empty,
    !Info ^ init_instrs := empty,
    !Info ^ class_members := [],
    !Info ^ has_main := no_main,
    !Info ^ class_foreign_langs := set.init,
    !Info ^ field_names := set.list_to_set(FieldNames).

    % Reset the il_info for processing a new method.
    %
:- pred il_info_new_method(arguments_map::in, signature::in, member_name::in,
    member_name::in, il_info::in, il_info::out) is det.

il_info_new_method(ILArgs, ILSignature, MethodName, CSharpMethodName,
        !Info) :-
    Info0 = !.Info,
    (
        !.Info ^ method_foreign_lang = yes(SomeLang),
        !Info ^ file_foreign_langs :=
            set.insert(Info0 ^ file_foreign_langs, SomeLang),
        !Info ^ class_foreign_langs :=
            set.insert(Info0 ^ class_foreign_langs, SomeLang)
    ;
        !.Info ^ method_foreign_lang = no
    ),
    !Info ^ locals := map.init,
    !Info ^ instr_tree := empty,
    !Info ^ label_counter := counter.init(1),
    !Info ^ block_counter := counter.init(1),
    !Info ^ method_foreign_lang := no,
    !Info ^ arguments := ILArgs,
    !Info ^ method_name := MethodName,
    !Info ^ csharp_method_name := CSharpMethodName,
    !Info ^ signature := ILSignature.

:- pred il_info_set_arguments(assoc_list(ilds.id, mlds_type)::in,
    il_info::in, il_info::out) is det.

il_info_set_arguments(Arguments, !Info) :-
    !Info ^ arguments := Arguments.

:- pred il_info_get_arguments(il_info::in, arguments_map::out) is det.

il_info_get_arguments(Info, Arguments) :-
    Arguments = Info ^ arguments.

:- pred il_info_get_mlds_type(ilds.id::in, mlds_type::out,
    il_info::in, il_info::out) is det.

il_info_get_mlds_type(Id, Type, !Info) :-
    ( map.search(!.Info ^ locals, Id, Type0) ->
        Type = Type0
    ; assoc_list.search(!.Info ^ arguments, Id, Type0) ->
        Type = Type0
    ;
        % XXX If it isn't a local or an argument, it can only be a
        % "global variable" -- used by RTTI.
        Type = mlds_type_for_rtti_global
    ).

    % RTTI creates global variables -- these all happen to be of
    % type mlds_native_int_type.
    %
:- func mlds_type_for_rtti_global = mlds_type.

mlds_type_for_rtti_global = mlds_native_int_type.

:- pred il_info_set_modulename(mlds_module_name::in,
    il_info::in, il_info::out) is det.

il_info_set_modulename(ModuleName, !Info) :-
    !Info ^ module_name := ModuleName.

:- pred il_info_add_locals(assoc_list(ilds.id, mlds_type)::in,
    il_info::in, il_info::out) is det.

il_info_add_locals(NewLocals, !Info) :-
    !Info ^ locals :=
        map.det_insert_from_assoc_list(!.Info ^ locals, NewLocals).

:- pred il_info_remove_locals(assoc_list(ilds.id, mlds_type)::in,
    il_info::in, il_info::out) is det.

il_info_remove_locals(RemoveLocals, !Info) :-
    assoc_list.keys(RemoveLocals, Keys),
    map.delete_list(Keys, !.Info ^ locals, NewLocals),
    !Info ^ locals := NewLocals.

:- pred il_info_add_class_member(list(class_member)::in,
    il_info::in, il_info::out) is det.

il_info_add_class_member(ClassMembers, !Info) :-
    !Info ^ class_members :=
        list.append(ClassMembers, !.Info ^ class_members).

:- pred il_info_add_instructions(list(instr)::in,
    il_info::in, il_info::out) is det.

il_info_add_instructions(NewInstrs, !Info) :-
    !Info ^ instr_tree :=
        !.Info ^ instr_tree ++ from_list(NewInstrs).

:- pred il_info_add_init_instructions(list(instr)::in,
    il_info::in, il_info::out) is det.

il_info_add_init_instructions(NewInstrs, !Info) :-
    !Info ^ init_instrs :=
        !.Info ^ init_instrs ++ from_list(NewInstrs).

:- pred il_info_add_alloc_instructions(list(instr)::in,
    il_info::in, il_info::out) is det.

il_info_add_alloc_instructions(NewInstrs, !Info) :-
    !Info ^ alloc_instrs :=
        !.Info ^ alloc_instrs ++ from_list(NewInstrs).

:- pred il_info_get_instructions(il_info::in, cord(instr)::out) is det.

il_info_get_instructions(Info, Instrs) :-
    Instrs = Info ^ instr_tree.

:- pred il_info_get_locals_list(il_info::in,
    assoc_list(ilds.id, il_type)::out) is det.

il_info_get_locals_list(Info, Locals) :-
    DataRep = Info ^ il_data_rep,
    map.map_values_only((pred(V::in, W::out) is det :-
        W = mlds_type_to_ilds_type(DataRep, V)),
        Info ^ locals, LocalsMap),
    map.to_assoc_list(LocalsMap, Locals).

:- pred il_info_get_module_name(il_info::in, mlds_module_name::out) is det.

il_info_get_module_name(Info, ModuleName) :-
    ModuleName = Info ^ module_name.

:- pred il_info_get_next_block_id(blockid::out, il_info::in, il_info::out)
    is det.

il_info_get_next_block_id(N, !Info) :-
    counter.allocate(N, !.Info ^ block_counter, NewCounter),
    !Info ^ block_counter := NewCounter.

:- pred il_info_get_next_label_num(int::out, il_info::in, il_info::out) is det.

il_info_get_next_label_num(N, !Info) :-
    counter.allocate(N, !.Info ^ label_counter, NewCounter),
    !Info ^ label_counter := NewCounter.

:- pred il_info_make_next_label(ilds.label::out, il_info::in, il_info::out)
    is det.

il_info_make_next_label(Label, !Info) :-
    il_info_get_next_label_num(LabelNnum, !Info),
    string.format("l%d", [i(LabelNnum)], Label).

%-----------------------------------------------------------------------------%

    % Use this to make comments into trees easily.
    %
:- func comment_node(string) = instr_tree.

comment_node(S) = singleton(comment(S)).

    % Use this to make contexts into trees easily.
    %
:- func context_node(mlds_context) = instr_tree.

context_node(Context) = singleton(context_instr(Context)).

:- func context_instr(mlds_context) = instr.

context_instr(Context) = context(FileName, LineNumber) :-
    ProgContext = mlds_get_prog_context(Context),
    term.context_file(ProgContext, FileName),
    term.context_line(ProgContext, LineNumber).

    % Maybe fold T into U, and map it to V.
    % U remains untouched if T is `no'.
    %
:- pred maybe_map_fold(pred(T, V, U, U)::in(pred(in, out, in, out) is det),
    maybe(T)::in, V::in, V::out, U::in, U::out) is det.

maybe_map_fold(_, no, !V, !U).
maybe_map_fold(P, yes(T), _, !:V, !U) :-
    P(T, !:V, !U).

:- func il_method_params_to_il_types(list(il_method_param)) = list(il_type).

il_method_params_to_il_types([]) = [].
il_method_params_to_il_types([ il_method_param(Type, _) | Params]) =
        [ Type | Types ] :-
    Types = il_method_params_to_il_types(Params).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_il.
%-----------------------------------------------------------------------------%
