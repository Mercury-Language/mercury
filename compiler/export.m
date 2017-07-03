%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: export.m.
% Main author: dgj, juliensf.
%
% This module defines predicates to produce the functions which are
% exported to a foreign language via a `pragma foreign_export' declaration.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.export.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module io.

%-----------------------------------------------------------------------------%

    % From the module_info, get a list of foreign_export_decls, each of which
    % holds information about the declaration of a foreign function named in a
    % `pragma foreign_export' declaration, which is used to allow a call to
    % be made to a Mercury procedure from the foreign language.
    %
:- pred get_foreign_export_decls(module_info::in, foreign_export_decls::out)
    is det.

    % From the module_info, get a list of foreign_export_defns, each of which
    % is a string containing the foreign code for defining a foreign function
    % named in a `pragma foreign_export' decl.
    %
:- pred get_foreign_export_defns(module_info::in, foreign_export_defns::out)
    is det.

    % Produce an interface file containing declarations for the exported
    % foreign functions (if required in this foreign language).
    %
    % This procedure is used for both the MLDS and LLDS back-ends.
    %
:- pred produce_header_file(module_info::in, foreign_export_decls::in,
    module_name::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Utilities for generating C code which interfaces with Mercury.
% The {MLDS,LLDS}->C backends and fact tables use this code.

    % Generate C code to convert an rval (represented as a string), from
    % a C type to a Mercury C type (i.e. convert strings and floats to
    % words) and return the resulting C code as a string.
    %
:- pred convert_type_to_mercury(string::in, mer_type::in, arg_loc::in,
    string::out) is det.

    % Generate C code to convert an rval (represented as a string), from
    % a Mercury C type to a C type (i.e. convert words to strings and
    % floats if required) and return the resulting C code as a string.
    %
:- pred convert_type_from_mercury(arg_loc::in, string::in, mer_type::in,
    string::out) is det.

    % Succeeds iff the given C type is known by the compiler to be an integer
    % or pointer type the same size as MR_Word.
    %
:- pred c_type_is_word_sized_int_or_ptr(string::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

get_foreign_export_decls(ModuleInfo, ForeignExportDecls) :-
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),

    module_info_get_foreign_decl_codes(ModuleInfo, ForeignDeclCodeCord),
    ForeignDeclCodes = cord.list(ForeignDeclCodeCord),

    module_info_get_pragma_exported_procs(ModuleInfo, ExportedProcsCord),
    get_foreign_export_decls_loop(ModuleInfo, Preds,
        cord.list(ExportedProcsCord), ExportDecls),

    ForeignExportDecls = foreign_export_decls(ForeignDeclCodes, ExportDecls).

:- pred get_foreign_export_decls_loop(module_info::in, pred_table::in,
    list(pragma_exported_proc)::in, list(foreign_export_decl)::out) is det.

get_foreign_export_decls_loop(_, _, [], []).
get_foreign_export_decls_loop(ModuleInfo, Preds,
        [HeadExportedProc | TailExportedProcs],
        [HeadExportDecl | TailExportDecls]) :-
    HeadExportedProc = pragma_exported_proc(Lang, PredId, ProcId, ExportName,
        _Context),
    (
        Lang = lang_c,
        get_export_info_for_lang_c(ModuleInfo, Preds, PredId, ProcId,
            _HowToDeclare, RetType, _DeclareReturnVal, _FailureAction,
            _SuccessAction, HeadArgInfoTypes),
        get_argument_declarations_for_lang_c(ModuleInfo, no, HeadArgInfoTypes,
            ArgDecls)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($module, $pred,  ":- pragma foreign_export for non-C backends.")
    ),
    HeadExportDecl = foreign_export_decl(Lang, RetType, ExportName, ArgDecls),
    get_foreign_export_decls_loop(ModuleInfo, Preds,
        TailExportedProcs, TailExportDecls).

%-----------------------------------------------------------------------------%

get_foreign_export_defns(ModuleInfo, ExportedProcsCode) :-
    module_info_get_pragma_exported_procs(ModuleInfo, ExportedProcsCord),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    export_procs_to_c(ModuleInfo, Preds,
        cord.list(ExportedProcsCord), ExportedProcsCode).

:- pred export_procs_to_c(module_info::in, pred_table::in,
    list(pragma_exported_proc)::in, list(foreign_export_defn)::out) is det.

export_procs_to_c(_ModuleInfo, _Preds, [], []).
export_procs_to_c(ModuleInfo, Preds,
        [ExportedProc | ExportedProcs], [ExportDefn | ExportDefns]) :-
    export_proc_to_c(ModuleInfo, Preds, ExportedProc, ExportDefn),
    export_procs_to_c(ModuleInfo, Preds, ExportedProcs, ExportDefns).

    % For each exported procedure, produce a C function.
    % The code we generate is in the form
    %
    % MR_declare_entry(<label of called proc>); /* or MR_declare_static */
    %
    %   /* Start with a declaration to avoid C compiler warnings. */
    % #if SEMIDET
    %   MR_bool
    % #elif FUNCTION
    %   MR_Word
    % #else
    %   void
    % #endif
    % <function name>(MR_Word Mercury__Argument1,
    %       MR_Word *Mercury__Argument2...);
    %       /* MR_Word for input, MR_Word* for output */
    %
    % #if SEMIDET
    %   MR_bool
    % #elif FUNCTION
    %   MR_Word
    % #else
    %   void
    % #endif
    % <function name>(MR_Word Mercury__Argument1,
    %       MR_Word *Mercury__Argument2...)
    %       /* MR_Word for input, MR_Word* for output */
    % {
    % #if MR_NUM_REAL_REGS > 0
    %   MR_Word c_regs[MR_NUM_REAL_REGS];
    % #endif
    % #if FUNCTION
    %   MR_Word retval;
    % #endif
    % #if MR_THREAD_SAFE
    %   MR_bool must_finalize_engine;
    % #endif
    % #if MR_DEEP_PROFILING
    %   MR_CallSiteDynamic *saved_call_site_addr = MR_current_callback_site;
    %   MR_CallSiteDynamic *saved_csd;
    % #endif
    %
    %       /* save the registers that our C caller may be using */
    %   MR_save_regs_to_mem(c_regs);
    %
    %       /*
    %       ** Start a new Mercury engine inside this POSIX thread,
    %       ** if necessary (the C code may be multi-threaded itself).
    %       */
    %
    % #if MR_THREAD_SAFE
    %   must_finalize_engine = MR_init_thread(MR_use_now);
    % #endif
    %
    % #if MR_DEEP_PROFILING
    %   saved_csd = MR_current_call_site_dynamic;
    %   MR_setup_callback(MR_ENTRY(<label of called proc>));
    % #endif
    %       /*
    %       ** Restore Mercury's registers that were saved as we entered C
    %       ** from Mercury. For single threaded programs, the process must
    %       ** always start in Mercury, so that we can MR_init_engine() etc.
    %       ** For multi-threaded MR_init_thread (above) takes care of
    %       ** making a new engine if required.
    %       */
    %   MR_restore_registers();
    %   <copy input arguments from Mercury__Arguments into registers>
    %       /* Save the registers which may be clobbered      */
    %       /* by the C function call MR_call_engine().       */
    %   MR_save_transient_registers();
    %
    %   (void) MR_call_engine(MR_ENTRY(<label of called proc>),
    %           MR_FALSE);
    %
    %       /* Restore the registers which may have been clobbered */
    %       /* by the return from the C function MR_call_engine(). */
    %   MR_restore_transient_registers();
    % #if MR_DEEP_PROFILING
    %   MR_current_call_site_dynamic = saved_csd;
    %   MR_current_callback_site = saved_call_site_addr;
    % #endif
    % #if SEMIDET
    %   if (!MR_r1) {
    %       MR_restore_regs_from_mem(c_regs);
    %       return MR_FALSE;
    %   }
    % #elif FUNCTION
    %   <copy return value register into retval>
    % #endif
    %   <copy output args from registers into *Mercury__Arguments>
    % #if MR_THREAD_SAFE
    %   if (must_finalize_engine) {
    %       MR_finalize_thread_engine();
    %   }
    % #endif
    %   MR_restore_regs_from_mem(c_regs);
    % #if SEMIDET
    %   return MR_TRUE;
    % #elif FUNCTION
    %   return retval;
    % #endif
    % }
    %
:- pred export_proc_to_c(module_info::in, pred_table::in,
    pragma_exported_proc::in, foreign_export_defn::out) is det.

export_proc_to_c(ModuleInfo, Preds, ExportedProc, ExportDefn) :-
    ExportedProc = pragma_exported_proc(Lang, PredId, ProcId, CFunction,
        _Context),
    expect(unify(Lang, lang_c), $module, $pred,
        "foreign language other than C"),
    get_export_info_for_lang_c(ModuleInfo, Preds, PredId, ProcId,
        DeclareString, CRetType, MaybeDeclareRetval, MaybeFail, MaybeSucceed,
        ArgInfoTypes),
    get_argument_declarations_for_lang_c(ModuleInfo, yes, ArgInfoTypes,
        ArgDecls),

    % Work out which arguments are input, and which are output, and copy
    % to/from the Mercury registers.
    pass_input_args(ModuleInfo, 0, ArgInfoTypes, PassInputArgs),
    retrieve_output_args(ModuleInfo, 0, ArgInfoTypes, RetrieveOutputArgs),

    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    ProcLabelString = proc_label_to_c_string(ProcLabel, yes),

    string.append_list([
        "\n",
        DeclareString, "(", ProcLabelString, ");\n",
        "\n",
        CRetType, "\n",
        CFunction, "(", ArgDecls, ");\n",
        "\n",
        CRetType, "\n",
        CFunction, "(", ArgDecls, ")\n{\n",
        "#if MR_NUM_REAL_REGS > 0\n",
        "\tMR_Word c_regs[MR_NUM_REAL_REGS];\n",
        "#endif\n",
        "#if MR_THREAD_SAFE\n",
        "\tMR_bool must_finalize_engine;\n",
        "#endif\n",
        "#if MR_DEEP_PROFILING\n",
        "\tMR_CallSiteDynList **saved_cur_callback;\n",
        "\tMR_CallSiteDynamic *saved_cur_csd;\n",
        "#endif\n",
        MaybeDeclareRetval,
        "\n",
        "\tMR_save_regs_to_mem(c_regs);\n",
        "#if MR_THREAD_SAFE\n",
        "\tmust_finalize_engine = MR_init_thread(MR_use_now);\n",
        "#endif\n",
        "#if MR_DEEP_PROFILING\n",
        "\tsaved_cur_callback = MR_current_callback_site;\n",
        "\tsaved_cur_csd = MR_current_call_site_dynamic;\n",
        "\tMR_setup_callback(MR_ENTRY(", ProcLabelString, "));\n",
        "#endif\n",
        "\tMR_restore_registers();\n",
        PassInputArgs,
        "\tMR_save_transient_registers();\n",
        "\t(void) MR_call_engine(MR_ENTRY(",
            ProcLabelString, "), MR_FALSE);\n",
        "\tMR_restore_transient_registers();\n",
        "#if MR_DEEP_PROFILING\n",
        "\tMR_current_call_site_dynamic = saved_cur_csd;\n",
        "\tMR_current_callback_site = saved_cur_callback;\n",
        "#endif\n",
        MaybeFail,
        RetrieveOutputArgs,
        "#if MR_THREAD_SAFE\n",
        "\tif (must_finalize_engine) {\n",
        "\t\t MR_finalize_thread_engine();\n",
        "\t}\n",
        "#endif\n",
        "\tMR_restore_regs_from_mem(c_regs);\n",
        MaybeSucceed,
        "}\n\n"],
        Code),
    ExportDefn = foreign_export_defn(Code).

    % get_export_info_for_lang_c(Preds, PredId, ProcId, Globals,
    %   DeclareString, CRetType,
    %   MaybeDeclareRetval, MaybeFail, MaybeSuccess, ArgInfoTypes):
    %
    % For a given procedure, figure out the information about that procedure
    % that is needed to export it:
    % - how to declare the procedure's entry label;
    % - the C return type, and the C declaration for the variable
    %   holding the return value (if any);
    % - the actions on success and failure; and
    % - the argument locations/modes/types.
    %
:- pred get_export_info_for_lang_c(module_info::in, pred_table::in,
    pred_id::in, proc_id::in, string::out, string::out, string::out,
    string::out, string::out, assoc_list(arg_info, mer_type)::out) is det.

get_export_info_for_lang_c(ModuleInfo, Preds, PredId, ProcId,
        HowToDeclareLabel, CRetType, MaybeDeclareRetval, MaybeFail,
        MaybeSucceed, ArgInfoTypes) :-
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_status(PredInfo, Status),
    ( if
        (
            procedure_is_exported(ModuleInfo, PredInfo, ProcId)
        ;
            pred_status_defined_in_this_module(Status) = no
        )
    then
        HowToDeclareLabel = "MR_declare_entry"
    else
        HowToDeclareLabel = "MR_declare_static"
    ),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_maybe_arg_info(ProcInfo, MaybeArgInfos),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    (
        MaybeArgInfos = yes(ArgInfos0),
        ArgInfos = ArgInfos0
    ;
        MaybeArgInfos = no,
        generate_proc_arg_info(Markers, ArgTypes, ModuleInfo, ProcInfo,
            NewProcInfo),
        proc_info_arg_info(NewProcInfo, ArgInfos)
    ),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    assoc_list.from_corresponding_lists(ArgInfos, ArgTypes, ArgInfoTypes0),

    % Figure out what the C return type should be, and build the `return'
    % instructions (if any).
    (
        CodeModel = model_det,
        ( if
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgInfoTypes0, ArgInfoTypes1,
                arg_info(RetArgLoc, RetArgMode) - RetType),
            RetArgMode = top_out,
            check_dummy_type(ModuleInfo, RetType) = is_not_dummy_type
        then
            ExportRetType = foreign.to_exported_type(ModuleInfo, RetType),
            CRetType = exported_type_to_string(lang_c, ExportRetType),
            arg_loc_to_string(RetArgLoc, RetArgString0),
            convert_type_from_mercury(RetArgLoc, RetArgString0, RetType,
                RetArgString),
            MaybeDeclareRetval = "\t" ++ CRetType ++ " return_value;\n",
            % We need to unbox non-word-sized foreign types
            % before returning them to C code
            ExportRetTypeIsForeign = foreign.is_foreign_type(ExportRetType),
            (
                ExportRetTypeIsForeign = yes(_),
                SetReturnValue = "\tMR_MAYBE_UNBOX_FOREIGN_TYPE("
                    ++ CRetType ++ ", " ++ RetArgString
                    ++ ", return_value);\n"
            ;
                ExportRetTypeIsForeign = no,
                SetReturnValue = "\treturn_value = " ++ RetArgString ++ ";\n"
            ),
            MaybeFail = SetReturnValue,
            MaybeSucceed = "\treturn return_value;\n",
            ArgInfoTypes2 = ArgInfoTypes1
        else
            CRetType = "void",
            MaybeDeclareRetval = "",
            MaybeFail = "",
            MaybeSucceed = "",
            ArgInfoTypes2 = ArgInfoTypes0
        )
    ;
        CodeModel = model_semi,
        % We treat semidet functions the same as semidet predicates, which
        % means that for Mercury functions the Mercury return value becomes
        % the last argument, and the C return value is a bool that is used
        % to indicate success or failure.
        CRetType = "MR_bool",
        MaybeDeclareRetval = "",
        string.append_list([
            "\tif (!MR_r1) {\n",
            "\t\tMR_restore_regs_from_mem(c_regs);\n",
            "\treturn MR_FALSE;\n",
            "\t}\n"], MaybeFail),
        MaybeSucceed = "\treturn MR_TRUE;\n",
        ArgInfoTypes2 = ArgInfoTypes0
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "Attempt to export model_non procedure.")
    ),
    list.filter(include_arg(ModuleInfo), ArgInfoTypes2, ArgInfoTypes).

    % include_arg(ArgInfoType):
    %
    % Succeeds iff the specified argument should be included in
    % the arguments of the exported C function.
    %
:- pred include_arg(module_info::in, pair(arg_info, mer_type)::in) is semidet.

include_arg(ModuleInfo, arg_info(_Loc, Mode) - Type) :-
    Mode \= top_unused,
    check_dummy_type(ModuleInfo, Type) = is_not_dummy_type.

    % get_argument_declarations(Args, NameThem, DeclString):
    %
    % Build a string to declare the argument types (and if NameThem = yes,
    % the argument names) of a C function.
    %
:- pred get_argument_declarations_for_lang_c(module_info::in, bool::in,
    assoc_list(arg_info, mer_type)::in, string::out) is det.

get_argument_declarations_for_lang_c(_, _, [], "void").
get_argument_declarations_for_lang_c(ModuleInfo, NameThem, [X | Xs],
        ArgsDecl) :-
    get_argument_declarations_nonvoid(ModuleInfo, NameThem, 0, [X | Xs],
        ArgsDecl).

:- pred get_argument_declarations_nonvoid(module_info::in, bool::in, int::in,
    assoc_list(arg_info, mer_type)::in, string::out) is det.

get_argument_declarations_nonvoid(_, _, _, [], "").
get_argument_declarations_nonvoid(ModuleInfo, NameThem, LastArgNum, [AT | ATs],
        ArgsDecl) :-
    AT = ArgInfo - Type,
    CurArgNum = LastArgNum + 1,
    get_argument_declaration(ModuleInfo, NameThem, CurArgNum, ArgInfo, Type,
        TypeString, ArgName),
    HeadArgsDecl = TypeString ++ ArgName,
    (
        ATs = [],
        ArgsDecl = HeadArgsDecl
    ;
        ATs = [_ | _],
        get_argument_declarations_nonvoid(ModuleInfo, NameThem, CurArgNum, ATs,
            TailArgsDecl),
        ArgsDecl = HeadArgsDecl ++ ", " ++ TailArgsDecl
    ).

:- pred get_argument_declaration(module_info::in, bool::in, int::in,
    arg_info::in, mer_type::in, string::out, string::out) is det.

get_argument_declaration(ModuleInfo, NameThem, ArgNum, ArgInfo, Type,
        TypeString, ArgName) :-
    ArgInfo = arg_info(_Loc, Mode),
    (
        NameThem = yes,
        string.int_to_string(ArgNum, ArgNumString),
        string.append(" Mercury__argument", ArgNumString, ArgName)
    ;
        NameThem = no,
        ArgName = ""
    ),
    TypeString0 = mercury_exported_type_to_string(ModuleInfo, lang_c, Type),
    (
        Mode = top_out,
        % output variables are passed as pointers
        TypeString = TypeString0 ++ " *"
    ;
        ( Mode = top_in
        ; Mode = top_unused
        ),
        TypeString = TypeString0
    ).

:- pred pass_input_args(module_info::in, int::in,
    assoc_list(arg_info, mer_type)::in, string::out) is det.

pass_input_args(_, _, [], "").
pass_input_args(ModuleInfo, LastArgNum, [AT | ATs], PassInputArgs) :-
    AT = ArgInfo - Type,
    ArgInfo = arg_info(ArgLoc, Mode),
    CurArgNum = LastArgNum + 1,
    (
        Mode = top_in,
        ArgName0 = "Mercury__argument" ++ string.int_to_string(CurArgNum),
        arg_loc_to_string(ArgLoc, ArgLocString),
        convert_type_to_mercury(ArgName0, Type, ArgLoc, ArgName),
        ExportType = foreign.to_exported_type(ModuleInfo, Type),
        % We need to box non-word-sized foreign types
        % before passing them to Mercury code.
        ExportTypeIsForeign = foreign.is_foreign_type(ExportType),
        (
            ExportTypeIsForeign = yes(_),
            CType = exported_type_to_string(lang_c, ExportType),
            PassHeadInputArg = "\tMR_MAYBE_BOX_FOREIGN_TYPE(" ++
                CType ++ ", " ++ ArgName ++ ", " ++ ArgLocString ++ ");\n"
        ;
            ExportTypeIsForeign = no,
            PassHeadInputArg =
                "\t" ++ ArgLocString ++ " = " ++ ArgName ++ ";\n"
        )
    ;
        Mode = top_out,
        PassHeadInputArg = ""
    ;
        Mode = top_unused,
        PassHeadInputArg = ""
    ),
    pass_input_args(ModuleInfo, CurArgNum, ATs, PassTailInputArgs),
    PassInputArgs = PassHeadInputArg ++ PassTailInputArgs.

:- pred retrieve_output_args(module_info::in, int::in,
    assoc_list(arg_info, mer_type)::in, string::out) is det.

retrieve_output_args(_, _, [], "").
retrieve_output_args(ModuleInfo, LastArgNum, [AT | ATs], RetrieveOutputArgs) :-
    AT = ArgInfo - Type,
    ArgInfo = arg_info(ArgLoc, Mode),
    CurArgNum = LastArgNum + 1,
    (
        Mode = top_in,
        RetrieveHeadOutputArg = ""
    ;
        Mode = top_out,
        ArgName = "Mercury__argument" ++ string.int_to_string(CurArgNum),
        arg_loc_to_string(ArgLoc, ArgLocString0),
        convert_type_from_mercury(ArgLoc, ArgLocString0, Type, ArgLocString),
        ExportType = foreign.to_exported_type(ModuleInfo, Type),
        % We need to unbox non-word-sized foreign types
        % before returning them to C code
        ExportTypeIsForeign = foreign.is_foreign_type(ExportType),
        (
            ExportTypeIsForeign = yes(_),
            CType = exported_type_to_string(lang_c, ExportType),
            RetrieveHeadOutputArg = "\tMR_MAYBE_UNBOX_FOREIGN_TYPE(" ++
                CType ++ ", " ++ ArgLocString ++ ", * " ++ ArgName ++ ");\n"
        ;
            ExportTypeIsForeign = no,
            RetrieveHeadOutputArg =
                "\t*" ++ ArgName ++ " = " ++ ArgLocString ++ ";\n"
        )
    ;
        Mode = top_unused,
        RetrieveHeadOutputArg = ""
    ),
    retrieve_output_args(ModuleInfo, CurArgNum, ATs, RetrieveTailOutputArgs),
    RetrieveOutputArgs = RetrieveHeadOutputArg ++ RetrieveTailOutputArgs.

    % Convert an argument location to a string representing a C code fragment
    % that names it.
    %
:- pred arg_loc_to_string(arg_loc::in, string::out) is det.

arg_loc_to_string(reg(RegType, RegNum), RegName) :-
    % XXX this should reuse llds_out_data.reg_to_string
    (
        RegType = reg_r,
        % XXX This magic number can't be good.
        ( if RegNum > 32 then
            RegName = "MR_r(" ++ int_to_string(RegNum) ++ ")"
        else
            RegName = "MR_r" ++ int_to_string(RegNum)
        )
    ;
        RegType = reg_f,
        RegName = "MR_f(" ++ int_to_string(RegNum) ++ ")"
    ).

convert_type_to_mercury(Rval, Type, TargetArgLoc, ConvertedRval) :-
    (
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_string,
            ConvertedRval = "(MR_Word) " ++ Rval
        ;
            BuiltinType = builtin_type_float,
            (
                TargetArgLoc = reg(reg_r, _),
                ConvertedRval = "MR_float_to_word(" ++ Rval ++ ")"
            ;
                TargetArgLoc = reg(reg_f, _),
                ConvertedRval = Rval
            )
        ;
            BuiltinType = builtin_type_char,
            % We need to explicitly cast to MR_UnsignedChar
            % to avoid problems with C compilers for which `char'
            % is signed.
            ConvertedRval = "(MR_UnsignedChar) " ++ Rval
        ;
            BuiltinType = builtin_type_int(_),
            ConvertedRval = Rval
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        ConvertedRval = Rval
    ).

convert_type_from_mercury(SourceArgLoc, Rval, Type, ConvertedRval) :-
    (
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_string,
            ConvertedRval = "(MR_String) " ++ Rval
        ;
            BuiltinType = builtin_type_float,
            (
                SourceArgLoc = reg(reg_r, _),
                ConvertedRval = "MR_word_to_float(" ++ Rval ++ ")"
            ;
                SourceArgLoc = reg(reg_f, _),
                ConvertedRval = Rval
            )
        ;
            ( BuiltinType = builtin_type_int(_)
            ; BuiltinType = builtin_type_char
            ),
            ConvertedRval = Rval
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        ConvertedRval = Rval
    ).

%-----------------------------------------------------------------------------%
%
% Code to create the .mh files.
%

produce_header_file(ModuleInfo, ForeignExportDecls, ModuleName, !IO) :-
    % We always produce a .mh file because with intermodule optimization
    % enabled, the .o file depends on all the .mh files of the imported
    % modules. so we need to produce a .mh file even if it contains nothing.
    module_info_get_globals(ModuleInfo, Globals),
    HeaderExt = ".mh",
    module_name_to_file_name(Globals, do_create_dirs, HeaderExt,
        ModuleName, FileName, !IO),
    MaybeThisFileName = yes(FileName),
    io.open_output(FileName ++ ".tmp", Result, !IO),
    (
        Result = ok(FileStream),
        module_name_to_file_name(Globals, do_not_create_dirs, ".m",
            ModuleName, SourceFileName, !IO),
        library.version(Version, Fullarch),
        io.write_strings(FileStream, [
            "/*\n",
            "** Automatically generated from `", SourceFileName, "'\n",
            "** by the Mercury compiler,\n",
            "** version ", Version, "\n",
            "** configured for ", Fullarch, ".\n",
            "** Do not edit.\n",
            "*/\n"], !IO),
        MangledModuleName = sym_name_mangle(ModuleName),
        string.to_upper(MangledModuleName, UppercaseModuleName),
        GuardMacroName = UppercaseModuleName ++ "_MH",
        io.write_strings(FileStream, [
            "#ifndef ", GuardMacroName, "\n",
            "#define ", GuardMacroName, "\n",
            "\n",
            "#ifdef __cplusplus\n",
            "extern ""C"" {\n",
            "#endif\n",
            "\n",
            "#ifdef MR_HIGHLEVEL_CODE\n",
            "#include ""mercury.h""\n",
            "#else\n",
            "  #ifndef MERCURY_HDR_EXCLUDE_IMP_H\n",
            "  #include ""mercury_imp.h""\n",
            "  #endif\n",
            "#endif\n",
            "#ifdef MR_DEEP_PROFILING\n",
            "#include ""mercury_deep_profiling.h""\n",
            "#endif\n",
            "\n"], !IO),

        module_info_get_exported_enums(ModuleInfo, ExportedEnums),
        list.filter(exported_enum_is_for_c, ExportedEnums, CExportedEnums),

        ForeignExportDecls =
            foreign_export_decls(ForeignDeclCodes, CExportDecls),
        list.filter(foreign_decl_code_is_for_lang(lang_c),
            ForeignDeclCodes, CForeignDeclCodes),

        ( if
            CExportedEnums = [],
            CForeignDeclCodes = []
        then
            % The two folds below won't output anything.
            % There is no point in printing guards around nothing.
            true
        else
            MaybeSetLineNumbers = lookup_line_numbers(Globals,
                line_numbers_for_c_headers),
            io.write_strings(FileStream, [
                "#ifndef ", decl_guard(ModuleName), "\n",
                "#define ", decl_guard(ModuleName), "\n"], !IO),
            list.foldl(
                output_exported_c_enum(FileStream, MaybeSetLineNumbers,
                    MaybeThisFileName),
                CExportedEnums, !IO),
            list.foldl(
                output_foreign_decl(FileStream, MaybeSetLineNumbers,
                    MaybeThisFileName, SourceFileName,
                    yes(foreign_decl_is_exported)),
                CForeignDeclCodes, !IO),
            io.write_string(FileStream, "\n#endif\n", !IO)
        ),

        write_export_decls(FileStream, CExportDecls, !IO),
        io.write_strings(FileStream, [
            "\n",
            "#ifdef __cplusplus\n",
            "}\n",
            "#endif\n",
            "\n",
            "#endif /* ", GuardMacroName, " */\n"], !IO),
        io.close_output(FileStream, !IO),
        % rename "<ModuleName>.mh.tmp" to "<ModuleName>.mh".
        update_interface(Globals, FileName, !IO)
    ;
        Result = error(_),
        io.progname_base("export.m", ProgName, !IO),
        io.write_string("\n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": can't open `", !IO),
        io.write_string(FileName ++ ".tmp", !IO),
        io.write_string("' for output\n", !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred write_export_decls(io.text_output_stream::in,
    list(foreign_export_decl)::in, io::di, io::uo) is det.

write_export_decls(_Stream, [], !IO).
write_export_decls(Stream, [ExportDecl | ExportDecls], !IO) :-
    ExportDecl = foreign_export_decl(Lang, CRetType, CFunction, ArgDecls),
    (
        Lang = lang_c,
        % Output the function header.
        io.write_string(Stream, CRetType, !IO),
        io.write_string(Stream, " ", !IO),
        io.write_string(Stream, CFunction, !IO),
        io.write_string(Stream, "(", !IO),
        io.write_string(Stream, ArgDecls, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($module, $pred, "foreign languages other than C unimplemented")
    ),
    write_export_decls(Stream, ExportDecls, !IO).

:- pred output_foreign_decl(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in, string::in,
    maybe(foreign_decl_is_local)::in, foreign_decl_code::in,
    io::di, io::uo) is det.

output_foreign_decl(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        SourceFileName, MaybeDesiredIsLocal, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, IsLocal, LiteralOrInclude, Context),
    expect(unify(Lang, lang_c), $module, $pred, "Lang != lang_c"),
    ( if
        (
            MaybeDesiredIsLocal = no
        ;
            MaybeDesiredIsLocal = yes(DesiredIsLocal),
            DesiredIsLocal = IsLocal
        )
    then
        output_foreign_literal_or_include(Stream, MaybeSetLineNumbers,
            MaybeThisFileName, SourceFileName, LiteralOrInclude, Context, !IO)
    else
        true
    ).

:- pred output_foreign_literal_or_include(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in, string::in,
    foreign_literal_or_include::in, prog_context::in, io::di, io::uo) is det.

output_foreign_literal_or_include(Stream, MaybeSetLineNumbers,
        MaybeThisFileName, SourceFileName, LiteralOrInclude, Context, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        term.context_file(Context, File),
        term.context_line(Context, Line),
        c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line,
            !IO),
        io.write_string(Stream, Code, !IO)
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, IncludePath, 1,
            !IO),
        write_include_file_contents(Stream, IncludePath, !IO)
    ),
    io.nl(Stream, !IO),
    c_util.maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        !IO).

%-----------------------------------------------------------------------------%
%
% Code for writing out foreign exported enumerations.
%

% For C/C++ we emit a #defined constant for constructors exported from an
% enumeration.

:- pred exported_enum_is_for_c(exported_enum_info::in) is semidet.

exported_enum_is_for_c(ExportedEnumInfo) :-
    ExportedEnumInfo = exported_enum_info(Lang, _, _, _, _, _),
    Lang = lang_c.

:- pred output_exported_c_enum(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in,
    exported_enum_info::in, io::di, io::uo) is det.

output_exported_c_enum(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        ExportedEnumInfo, !IO) :-
    ExportedEnumInfo = exported_enum_info(Lang, Context, TypeCtor,
        NameMapping, Ctors, TagValues),
    expect(unify(Lang, lang_c), $module, $pred, "Lang != lang_c"),
    list.foldl(
        foreign_const_name_and_tag(TypeCtor, NameMapping, TagValues),
        Ctors, cord.init, ForeignNamesAndTagsCord),
    ForeignNamesAndTags = cord.list(ForeignNamesAndTagsCord),
    term.context_file(Context, File),
    term.context_line(Context, Line),
    c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line, !IO),
    output_exported_enum_constname_tags(Stream, ForeignNamesAndTags, !IO),
    c_util.maybe_reset_line_num(Stream, MaybeSetLineNumbers,
        MaybeThisFileName, !IO).

    % The tags for exported enumerations will either be integers (for normal
    % enumerations) or strings (for foreign enumerations.)
    %
:- type exported_enum_tag_rep
    --->    ee_tag_rep_int(int)
    ;       ee_tag_rep_string(string).

:- pred output_exported_enum_constname_tags(io.text_output_stream::in,
    list(pair(string, exported_enum_tag_rep))::in, io::di, io::uo) is det.

output_exported_enum_constname_tags(_Stream, [], !IO).
output_exported_enum_constname_tags(Stream, [ConstNameTag | ConstNameTags],
        !IO) :-
    output_exported_enum_constname_tag(Stream, ConstNameTag, !IO),
    output_exported_enum_constname_tags(Stream, ConstNameTags, !IO).

:- pred output_exported_enum_constname_tag(io.text_output_stream::in,
    pair(string, exported_enum_tag_rep)::in, io::di, io::uo) is det.

output_exported_enum_constname_tag(Stream, ConstName - Tag, !IO) :-
    (
        Tag = ee_tag_rep_int(RawIntTag),
        io.format(Stream, "#define %s %d\n", [s(ConstName), i(RawIntTag)], !IO)
    ;
        Tag = ee_tag_rep_string(RawStrTag),
        io.format(Stream, "#define %s %s\n", [s(ConstName), s(RawStrTag)], !IO)
    ).

:- pred foreign_const_name_and_tag(type_ctor::in, map(sym_name, string)::in,
    cons_tag_values::in, constructor::in,
    cord(pair(string, exported_enum_tag_rep))::in,
    cord(pair(string, exported_enum_tag_rep))::out) is det.

foreign_const_name_and_tag(TypeCtor, Mapping, TagValues, Ctor,
        !NamesAndTagsCord) :-
    Ctor = ctor(_, _, QualifiedCtorName, _Args, Arity, _),
    ConsId = cons(QualifiedCtorName, Arity, TypeCtor),
    map.lookup(TagValues, ConsId, TagVal),
    (
        TagVal = int_tag(IntTag),
        Tag = ee_tag_rep_int(IntTag)
    ;
        TagVal = foreign_tag(_ForeignLang, ForeignTag),
        Tag = ee_tag_rep_string(ForeignTag)
    ;
        ( TagVal = string_tag(_)
        ; TagVal = uint_tag(_)
        ; TagVal = int8_tag(_)
        ; TagVal = uint8_tag(_)
        ; TagVal = int16_tag(_)
        ; TagVal = uint16_tag(_)
        ; TagVal = int32_tag(_)
        ; TagVal = uint32_tag(_)
        ; TagVal = float_tag(_)
        ; TagVal = closure_tag(_, _, _)
        ; TagVal = type_ctor_info_tag(_, _, _)
        ; TagVal = base_typeclass_info_tag(_, _, _)
        ; TagVal = type_info_const_tag(_)
        ; TagVal = typeclass_info_const_tag(_)
        ; TagVal = ground_term_const_tag(_, _)
        ; TagVal = tabling_info_tag(_, _)
        ; TagVal = deep_profiling_proc_layout_tag(_, _)
        ; TagVal = table_io_entry_tag(_, _)
        ; TagVal = single_functor_tag
        ; TagVal = unshared_tag(_)
        ; TagVal = direct_arg_tag(_)
        ; TagVal = shared_remote_tag(_, _)
        ; TagVal = shared_local_tag(_, _)
        ; TagVal = no_tag
        ; TagVal = reserved_address_tag(_)
        ; TagVal = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected($module, $pred, "enum constant requires an int tag")
    ),
    % Sanity check.
    expect(unify(Arity, 0), $module, $pred, "enum constant arity != 0"),
    UnqualifiedCtorName = unqualified(unqualify_name(QualifiedCtorName)),
    map.lookup(Mapping, UnqualifiedCtorName, ForeignName),
    !:NamesAndTagsCord = cord.snoc(!.NamesAndTagsCord, ForeignName - Tag).

%-----------------------------------------------------------------------------%

c_type_is_word_sized_int_or_ptr("MR_Word").
c_type_is_word_sized_int_or_ptr("MR_TypeInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeCtorInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeClassInfo").
c_type_is_word_sized_int_or_ptr("MR_BaseTypeclassInfo").

%-----------------------------------------------------------------------------%
:- end_module backend_libs.export.
%-----------------------------------------------------------------------------%
