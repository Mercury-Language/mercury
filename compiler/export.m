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
:- import_module mdbcomp.prim_data.
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
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
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

get_foreign_export_decls(HLDS, ForeignExportDecls) :-
    module_info_get_predicate_table(HLDS, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),

    module_info_get_foreign_decl(HLDS, RevForeignDecls),
    ForeignDecls = list.reverse(RevForeignDecls),

    module_info_get_pragma_exported_procs(HLDS, ExportedProcs),
    module_info_get_globals(HLDS, Globals),
    get_foreign_export_decls_2(Preds, ExportedProcs, Globals, HLDS,
        ExportDecls),

    ForeignExportDecls = foreign_export_decls(ForeignDecls, ExportDecls).

:- pred get_foreign_export_decls_2(pred_table::in,
    list(pragma_exported_proc)::in, globals::in, module_info::in,
    list(foreign_export_decl)::out) is det.

get_foreign_export_decls_2(_Preds, [], _, _, []).
get_foreign_export_decls_2(Preds, [E | ExportedProcs], Globals,
        ModuleInfo, ExportDecls) :-
    E = pragma_exported_proc(Lang, PredId, ProcId, ExportName, _Ctxt),
    (
        Lang = lang_c,
        get_export_info_for_lang_c(Preds, PredId, ProcId, Globals, ModuleInfo,
            _HowToDeclare, RetType, _DeclareReturnVal, _FailureAction,
            _SuccessAction, HeadArgInfoTypes),
            get_argument_declarations_for_lang_c(HeadArgInfoTypes, no,
                ModuleInfo, ArgDecls)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry($module, $pred,  ":- pragma foreign_export for non-C backends.")
    ),
    ExportDecl = foreign_export_decl(Lang, RetType, ExportName, ArgDecls),
    get_foreign_export_decls_2(Preds, ExportedProcs, Globals, ModuleInfo,
        ExportDecls0),
    ExportDecls = [ExportDecl | ExportDecls0].

%-----------------------------------------------------------------------------%

get_foreign_export_defns(ModuleInfo, ExportedProcsCode) :-
    module_info_get_pragma_exported_procs(ModuleInfo, ExportedProcs),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    to_c(Preds, ExportedProcs, ModuleInfo, ExportedProcsCode).

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
    %       ** start a new Mercury engine inside this POSIX
    %       ** thread, if necessary (the C code may be
    %       ** multi-threaded itself).
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
    %       ** restore Mercury's registers that were saved as
    %       ** we entered C from Mercury.  For single threaded
    %       ** programs the process must always start in Mercury
    %       ** so that we can MR_init_engine() etc.  For
    %       ** multi-threaded MR_init_thread (above) takes care
    %       ** of making a new engine if required.
    %       */
    %   MR_restore_registers();
    %   <copy input arguments from Mercury__Arguments into registers>
    %       /* save the registers which may be clobbered      */
    %       /* by the C function call MR_call_engine().       */
    %   MR_save_transient_registers();
    %
    %   (void) MR_call_engine(MR_ENTRY(<label of called proc>),
    %           MR_FALSE);
    %
    %       /* restore the registers which may have been      */
    %       /* clobbered by the return from the C function    */
    %       /* MR_call_engine()               */
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
:- pred to_c(pred_table::in, list(pragma_exported_proc)::in,
    module_info::in, list(string)::out) is det.

to_c(_Preds, [], _ModuleInfo, []).
to_c(Preds, [E | ExportedProcs], ModuleInfo, ExportedProcsCode) :-
    E = pragma_exported_proc(Lang, PredId, ProcId, C_Function, _Ctxt),
    expect(unify(Lang, lang_c), $module, $pred,
        "foreign language other than C"),
    module_info_get_globals(ModuleInfo, Globals),
    get_export_info_for_lang_c(Preds, PredId, ProcId, Globals, ModuleInfo,
        DeclareString, C_RetType, MaybeDeclareRetval, MaybeFail, MaybeSucceed,
        ArgInfoTypes),
    get_argument_declarations_for_lang_c(ArgInfoTypes, yes, ModuleInfo,
        ArgDecls),
    %
    % Work out which arguments are input, and which are output, and copy
    % to/from the Mercury registers.
    %
    get_input_args(ArgInfoTypes, 0, ModuleInfo, InputArgs),
    copy_output_args(ArgInfoTypes, 0, ModuleInfo, OutputArgs),

    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    ProcLabelString = proc_label_to_c_string(ProcLabel, yes),

    string.append_list([
        "\n",
        DeclareString, "(", ProcLabelString, ");\n",
        "\n",
        C_RetType, "\n",
        C_Function, "(", ArgDecls, ");\n",
        "\n",
        C_RetType, "\n",
        C_Function, "(", ArgDecls, ")\n{\n",
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
        InputArgs,
        "\tMR_save_transient_registers();\n",
        "\t(void) MR_call_engine(MR_ENTRY(",
            ProcLabelString, "), MR_FALSE);\n",
        "\tMR_restore_transient_registers();\n",
    "#if MR_DEEP_PROFILING\n",
    "\tMR_current_call_site_dynamic = saved_cur_csd;\n",
    "\tMR_current_callback_site = saved_cur_callback;\n",
    "#endif\n",
        MaybeFail,
        OutputArgs,
        "#if MR_THREAD_SAFE\n",
        "\tif (must_finalize_engine) {\n",
        "\t\t MR_finalize_thread_engine();\n",
        "\t}\n",
        "#endif\n",
        "\tMR_restore_regs_from_mem(c_regs);\n",
        MaybeSucceed,
        "}\n\n"],
        Code),

    to_c(Preds, ExportedProcs, ModuleInfo, TheRest),
    ExportedProcsCode = [Code | TheRest].

    % get_export_info_for_lang_c(Preds, PredId, ProcId, Globals,
    %   DeclareString, C_RetType,
    %   MaybeDeclareRetval, MaybeFail, MaybeSuccess, ArgInfoTypes):
    %
    % For a given procedure, figure out the information about that procedure
    % that is needed to export it:
    % - how to declare the procedure's entry label,
    % - the C return type, and the C declaration for the variable
    %   holding the return value (if any),
    % - the actions on success and failure, and
    % - the argument locations/modes/types.
    %
:- pred get_export_info_for_lang_c(pred_table::in, pred_id::in, proc_id::in,
    globals::in, module_info::in, string::out, string::out, string::out,
    string::out, string::out, assoc_list(arg_info, mer_type)::out) is det.

get_export_info_for_lang_c(Preds, PredId, ProcId, _Globals, ModuleInfo,
        HowToDeclareLabel, C_RetType, MaybeDeclareRetval, MaybeFail,
        MaybeSucceed, ArgInfoTypes) :-
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, Status),
    (
        (
            procedure_is_exported(ModuleInfo, PredInfo, ProcId)
        ;
            status_defined_in_this_module(Status) = no
        )
    ->
        HowToDeclareLabel = "MR_declare_entry"
    ;
        HowToDeclareLabel = "MR_declare_static"
    ),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_procedures(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_maybe_arg_info(ProcInfo, MaybeArgInfos),
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
        (
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgInfoTypes0, ArgInfoTypes1,
                arg_info(RetArgLoc, RetArgMode) - RetType),
            RetArgMode = top_out,
            check_dummy_type(ModuleInfo, RetType) = is_not_dummy_type
        ->
            Export_RetType = foreign.to_exported_type(ModuleInfo, RetType),
            C_RetType = exported_type_to_string(lang_c, Export_RetType),
            arg_loc_to_string(RetArgLoc, RetArgString0),
            convert_type_from_mercury(RetArgLoc, RetArgString0, RetType,
                RetArgString),
            MaybeDeclareRetval = "\t" ++ C_RetType ++ " return_value;\n",
            % We need to unbox non-word-sized foreign types
            % before returning them to C code
            ( foreign.is_foreign_type(Export_RetType) = yes(_) ->
                SetReturnValue = "\tMR_MAYBE_UNBOX_FOREIGN_TYPE("
                    ++ C_RetType ++ ", " ++ RetArgString
                    ++ ", return_value);\n"
            ;
                SetReturnValue = "\treturn_value = " ++ RetArgString ++ ";\n"
            ),
            MaybeFail = SetReturnValue,
            MaybeSucceed = "\treturn return_value;\n",
            ArgInfoTypes2 = ArgInfoTypes1
        ;
            C_RetType = "void",
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
        C_RetType = "MR_bool",
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
:- pred get_argument_declarations_for_lang_c(
    assoc_list(arg_info, mer_type)::in, bool::in,
    module_info::in, string::out) is det.

get_argument_declarations_for_lang_c([], _, _, "void").
get_argument_declarations_for_lang_c([X | Xs], NameThem, ModuleInfo, Result) :-
    get_argument_declarations_2([X | Xs], 0, NameThem, ModuleInfo, Result).

:- pred get_argument_declarations_2(assoc_list(arg_info, mer_type)::in,
    int::in, bool::in, module_info::in, string::out) is det.

get_argument_declarations_2([], _, _, _, "").
get_argument_declarations_2([AT | ATs], Num0, NameThem, ModuleInfo, Result) :-
    AT = ArgInfo - Type,
    Num = Num0 + 1,
    get_argument_declaration(ArgInfo, Type, Num, NameThem, ModuleInfo,
        TypeString, ArgName),
    (
        ATs = [],
        Result = TypeString ++ ArgName
    ;
        ATs = [_ | _],
        get_argument_declarations_2(ATs, Num, NameThem, ModuleInfo, TheRest),
        Result = TypeString ++ ArgName ++ ", " ++ TheRest
    ).

:- pred get_argument_declaration(arg_info::in, mer_type::in, int::in, bool::in,
    module_info::in, string::out, string::out) is det.

get_argument_declaration(ArgInfo, Type, Num, NameThem, ModuleInfo,
        TypeString, ArgName) :-
    ArgInfo = arg_info(_Loc, Mode),
    (
        NameThem = yes,
        string.int_to_string(Num, NumString),
        string.append(" Mercury__argument", NumString, ArgName)
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

:- pred get_input_args(assoc_list(arg_info, mer_type)::in, int::in,
    module_info::in, string::out) is det.

get_input_args([], _, _, "").
get_input_args([AT | ATs], Num0, ModuleInfo, Result) :-
    AT = ArgInfo - Type,
    ArgInfo = arg_info(ArgLoc, Mode),
    Num = Num0 + 1,
    (
        Mode = top_in,
        string.int_to_string(Num, NumString),
        ArgName0 = "Mercury__argument" ++ NumString,
        arg_loc_to_string(ArgLoc, ArgLocString),
        convert_type_to_mercury(ArgName0, Type, ArgLoc, ArgName),
        Export_Type = foreign.to_exported_type(ModuleInfo, Type),
        % We need to box non-word-sized foreign types
        % before passing them to Mercury code
        ( foreign.is_foreign_type(Export_Type) = yes(_) ->
            C_Type = exported_type_to_string(lang_c, Export_Type),
            string.append_list(["\tMR_MAYBE_BOX_FOREIGN_TYPE(",
                C_Type, ", ", ArgName, ", ", ArgLocString, ");\n"], InputArg)
        ;
            InputArg = "\t" ++ ArgLocString ++ " = " ++ ArgName ++ ";\n"
        )
    ;
        Mode = top_out,
        InputArg = ""
    ;
        Mode = top_unused,
        InputArg = ""
    ),
    get_input_args(ATs, Num, ModuleInfo, TheRest),
    Result = InputArg ++ TheRest.

:- pred copy_output_args(assoc_list(arg_info, mer_type)::in, int::in,
    module_info::in, string::out) is det.

copy_output_args([], _, _, "").
copy_output_args([AT | ATs], Num0, ModuleInfo, Result) :-
    AT = ArgInfo - Type,
    ArgInfo = arg_info(ArgLoc, Mode),
    Num = Num0 + 1,
    (
        Mode = top_in,
        OutputArg = ""
    ;
        Mode = top_out,
        string.int_to_string(Num, NumString),
        string.append("Mercury__argument", NumString, ArgName),
        arg_loc_to_string(ArgLoc, ArgLocString0),
        convert_type_from_mercury(ArgLoc, ArgLocString0, Type, ArgLocString),
        Export_Type = foreign.to_exported_type(ModuleInfo, Type),
        % We need to unbox non-word-sized foreign types
        % before returning them to C code
        ( foreign.is_foreign_type(Export_Type) = yes(_) ->
            C_Type = exported_type_to_string(lang_c, Export_Type),
            string.append_list(["\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", C_Type,
                ", ", ArgLocString, ", * ", ArgName, ");\n"], OutputArg)
        ;
            OutputArg = "\t*" ++ ArgName ++ " = " ++ ArgLocString ++ ";\n"
        )
    ;
        Mode = top_unused,
        OutputArg = ""
    ),
    copy_output_args(ATs, Num, ModuleInfo, TheRest),
    string.append(OutputArg, TheRest, Result).

    % Convert an argument location to a string representing a C code fragment
    % that names it.
    %
:- pred arg_loc_to_string(arg_loc::in, string::out) is det.

arg_loc_to_string(reg(RegType, RegNum), RegName) :-
    % XXX this should reuse llds_out_data.reg_to_string
    (
        RegType = reg_r,
        % XXX This magic number can't be good.
        ( RegNum > 32 ->
            RegName = "MR_r(" ++ int_to_string(RegNum) ++ ")"
        ;
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
            BuiltinType = builtin_type_int,
            ConvertedRval = Rval
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _)
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
            ( BuiltinType = builtin_type_int
            ; BuiltinType = builtin_type_char
            ),
            ConvertedRval = Rval
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        ConvertedRval = Rval
    ).

%-----------------------------------------------------------------------------%
%
% Code to create the .mh files
%

% This procedure is used for both the MLDS and LLDS back-ends.

produce_header_file(ModuleInfo, ForeignExportDecls, ModuleName, !IO) :-
    % We always produce a .mh file because with intermodule optimization
    % enabled the .o file depends on all the .mh files of the imported modules
    % so we always need to produce a .mh file even if it contains nothing.
    ForeignExportDecls = foreign_export_decls(ForeignDecls, C_ExportDecls),
    module_info_get_exported_enums(ModuleInfo, ExportedEnums),
    HeaderExt = ".mh",
    module_info_get_globals(ModuleInfo, Globals),
    module_name_to_file_name(Globals, ModuleName, HeaderExt, do_create_dirs,
        FileName, !IO),
    io.open_output(FileName ++ ".tmp", Result, !IO),
    (
        Result = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
            SourceFileName, !IO),
        library.version(Version, Fullarch),
        io.write_strings([
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
        io.write_strings([
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

        io.write_strings([
            "#ifndef ", decl_guard(ModuleName), "\n",
            "#define ", decl_guard(ModuleName), "\n"], !IO),
        list.foldl(output_exported_enum(ModuleInfo), ExportedEnums, !IO),
        list.foldl(output_foreign_decl(Globals, SourceFileName,
            yes(foreign_decl_is_exported)), ForeignDecls, !IO),
        io.write_string("\n#endif\n", !IO),

        produce_header_file_2(C_ExportDecls, !IO),
        io.write_strings([
            "\n",
            "#ifdef __cplusplus\n",
            "}\n",
            "#endif\n",
            "\n",
            "#endif /* ", GuardMacroName, " */\n"], !IO),
        io.set_output_stream(OutputStream, _, !IO),
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

:- pred produce_header_file_2(list(foreign_export_decl)::in, io::di, io::uo)
    is det.

produce_header_file_2([], !IO).
produce_header_file_2([E | ExportedProcs], !IO) :-
    E = foreign_export_decl(Lang, C_RetType, C_Function, ArgDecls),
    ( 
        Lang = lang_c,
        % Output the function header.
        io.write_string(C_RetType, !IO),
        io.write_string(" ", !IO),
        io.write_string(C_Function, !IO),
        io.write_string("(", !IO),
        io.write_string(ArgDecls, !IO),
        io.write_string(");\n", !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry($module, $pred, "foreign languages other than C unimplemented")
    ),
    produce_header_file_2(ExportedProcs, !IO).

:- pred output_foreign_decl(globals::in, string::in,
    maybe(foreign_decl_is_local)::in, foreign_decl_code::in, io::di, io::uo)
    is det.

output_foreign_decl(Globals, SourceFileName, MaybeDesiredIsLocal, DeclCode,
        !IO) :-
    DeclCode = foreign_decl_code(Lang, IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        (
            MaybeDesiredIsLocal = no
        ;
            MaybeDesiredIsLocal = yes(DesiredIsLocal),
            DesiredIsLocal = IsLocal
        )
    ->
        output_foreign_literal_or_include(Globals, SourceFileName,
            LiteralOrInclude, Context, !IO),
        io.nl(!IO),
        c_util.reset_line_num(Globals, !IO)
    ;
        true
    ).

:- pred output_foreign_literal_or_include(globals::in, string::in,
    foreign_literal_or_include::in, prog_context::in, io::di, io::uo) is det.

output_foreign_literal_or_include(Globals, SourceFileName, LiteralOrInclude,
        Context, !IO) :-
    (
        LiteralOrInclude = literal(Code),
        term.context_file(Context, File),
        term.context_line(Context, Line),
        c_util.set_line_num(Globals, File, Line, !IO),
        io.write_string(Code, !IO)
    ;
        LiteralOrInclude = include_file(IncludeFileName),
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_util.set_line_num(Globals, IncludePath, 1, !IO),
        write_include_file_contents(IncludePath, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code for writing out foreign exported enumerations.
%

% For C/C++ we emit a #defined constant for constructors exported from an
% enumeration.

:- pred output_exported_enum(module_info::in, exported_enum_info::in,
    io::di, io::uo) is det.

output_exported_enum(ModuleInfo, ExportedEnumInfo, !IO) :-
    ExportedEnumInfo = exported_enum_info(Lang, _, _, _),
    (
        Lang = lang_c,
        output_exported_enum_2(ModuleInfo, ExportedEnumInfo, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        )
    ).

:- pred output_exported_enum_2(module_info::in, exported_enum_info::in,
    io::di, io::uo) is det.

output_exported_enum_2(ModuleInfo, ExportedEnumInfo, !IO) :-
    ExportedEnumInfo = exported_enum_info(_Lang, Context, TypeCtor,
        NameMapping),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    get_type_defn_body(TypeDefn, TypeBody),
    (
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_, _)
        ; TypeBody = hlds_abstract_type(_)
        ),
        unexpected($module, $pred, "invalid type for foreign_export_enum")
    ;
        TypeBody = hlds_du_type(Ctors, TagValues, _CheaperTagTest,
            DuTypeKind, _MaybeUserEq, _MaybeDirectArgCtors,
            _ReservedTag, _ReservedAddr, _IsForeignType),
        (
            ( DuTypeKind = du_type_kind_general
            ; DuTypeKind = du_type_kind_notag(_, _, _)
            ),
            unexpected($module, $pred, "d.u. is not an enumeration.")
        ;
            ( DuTypeKind = du_type_kind_mercury_enum
            ; DuTypeKind = du_type_kind_foreign_enum(_)
            ; DuTypeKind = du_type_kind_direct_dummy
            ),
            list.foldl(
                foreign_const_name_and_tag(TypeCtor, NameMapping, TagValues),
                Ctors, [], ForeignNamesAndTags0),
            % We reverse the list so the constants are printed out in order.
            list.reverse(ForeignNamesAndTags0, ForeignNamesAndTags),
            module_info_get_globals(ModuleInfo, Globals),
            term.context_file(Context, File),
            term.context_line(Context, Line),
            c_util.set_line_num(Globals, File, Line, !IO),
            io.write_list(ForeignNamesAndTags, "\n",
                output_exported_enum_3(ModuleInfo), !IO),
            io.nl(!IO),
            c_util.reset_line_num(Globals, !IO)
        )
    ).

    % The tags for exported enumerations will either be integers (for normal
    % enumerations) or strings (for foreign enumerations.)
    %
:- type exported_enum_tag_rep
    --->    ee_tag_rep_int(int)
    ;       ee_tag_rep_string(string).

:- pred output_exported_enum_3(module_info::in,
    pair(string, exported_enum_tag_rep)::in, io::di, io::uo) is det.

output_exported_enum_3(_, ConstName - Tag, !IO) :-
    (
        Tag = ee_tag_rep_int(RawIntTag),
        io.format("#define %s %d", [s(ConstName), i(RawIntTag)], !IO)
    ;
        Tag = ee_tag_rep_string(RawStrTag),
        io.format("#define %s %s", [s(ConstName), s(RawStrTag)], !IO)
    ).

:- pred foreign_const_name_and_tag(type_ctor::in, map(sym_name, string)::in,
    cons_tag_values::in, constructor::in,
    assoc_list(string, exported_enum_tag_rep)::in,
    assoc_list(string, exported_enum_tag_rep)::out) is det.

foreign_const_name_and_tag(TypeCtor, Mapping, TagValues, Ctor,
        !NamesAndTags) :-
    Ctor = ctor(_, _, QualifiedCtorName, Args, _),
    list.length(Args, Arity),
    ConsId = cons(QualifiedCtorName, Arity, TypeCtor),
    map.lookup(TagValues, ConsId, TagVal),
    (
        TagVal = int_tag(IntTag),
        Tag    = ee_tag_rep_int(IntTag)
    ;
        TagVal = foreign_tag(_ForeignLang, ForeignTag),
        Tag    = ee_tag_rep_string(ForeignTag)
    ;
        ( TagVal = string_tag(_)
        ; TagVal = float_tag(_)
        ; TagVal = closure_tag(_, _, _)
        ; TagVal = type_ctor_info_tag(_, _, _)
        ; TagVal = base_typeclass_info_tag(_, _, _)
        ; TagVal = type_info_const_tag(_)
        ; TagVal = typeclass_info_const_tag(_)
        ; TagVal = ground_term_const_tag(_, _)
        ; TagVal = tabling_info_tag(_, _)
        ; TagVal = deep_profiling_proc_layout_tag(_, _)
        ; TagVal = table_io_decl_tag(_, _)
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
    list.cons(ForeignName - Tag, !NamesAndTags).

%-----------------------------------------------------------------------------%

c_type_is_word_sized_int_or_ptr("MR_Word").
c_type_is_word_sized_int_or_ptr("MR_TypeInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeCtorInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeClassInfo").
c_type_is_word_sized_int_or_ptr("MR_BaseTypeclassInfo").

%-----------------------------------------------------------------------------%
:- end_module backend_libs.export.
%-----------------------------------------------------------------------------%
