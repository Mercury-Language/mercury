%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
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
:- pred produce_header_file(io.text_output_stream::in, module_info::in,
    foreign_export_decls::in, module_name::in, io::di, io::uo) is det.

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
:- import_module hlds.hlds_proc_util.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.file.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term_context.

%-----------------------------------------------------------------------------%

get_foreign_export_decls(ModuleInfo, ForeignExportDecls) :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),

    module_info_get_foreign_decl_codes_user(ModuleInfo,
        ForeignDeclCodeUserCord),
    module_info_get_foreign_decl_codes_aux(ModuleInfo,
        ForeignDeclCodeAuxCord),
    ForeignDeclCodes =
        cord.list(ForeignDeclCodeUserCord ++ ForeignDeclCodeAuxCord),

    module_info_get_pragma_exported_procs(ModuleInfo, ExportedProcsCord),
    get_foreign_export_decls_loop(ModuleInfo, PredIdTable,
        cord.list(ExportedProcsCord), ExportDecls),

    ForeignExportDecls = foreign_export_decls(ForeignDeclCodes, ExportDecls).

:- pred get_foreign_export_decls_loop(module_info::in, pred_id_table::in,
    list(pragma_exported_proc)::in, list(foreign_export_decl)::out) is det.

get_foreign_export_decls_loop(_, _, [], []).
get_foreign_export_decls_loop(ModuleInfo, PredIdTable,
        [HeadExportedProc | TailExportedProcs],
        [HeadExportDecl | TailExportDecls]) :-
    HeadExportedProc = pragma_exported_proc(Lang, PredId, ProcId, ExportName,
        _Context),
    (
        Lang = lang_c,
        get_export_info_for_lang_c(ModuleInfo, PredIdTable, PredId, ProcId,
            _HowToDeclare, RetType, _DeclareReturnVal, _FailureAction,
            _SuccessAction, HeadArgInfoTypes),
        get_argument_declarations_for_lang_c(ModuleInfo, no, HeadArgInfoTypes,
            ArgDecls)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred,  ":- pragma foreign_export for non-C backends.")
    ),
    HeadExportDecl = foreign_export_decl(Lang, RetType, ExportName, ArgDecls),
    get_foreign_export_decls_loop(ModuleInfo, PredIdTable,
        TailExportedProcs, TailExportDecls).

%-----------------------------------------------------------------------------%

get_foreign_export_defns(ModuleInfo, ExportedProcsCode) :-
    module_info_get_pragma_exported_procs(ModuleInfo, ExportedProcsCord),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    export_procs_to_c(ModuleInfo, PredIdTable,
        cord.list(ExportedProcsCord), ExportedProcsCode).

:- pred export_procs_to_c(module_info::in, pred_id_table::in,
    list(pragma_exported_proc)::in, list(foreign_export_defn)::out) is det.

export_procs_to_c(_ModuleInfo, _PredIdTable, [], []).
export_procs_to_c(ModuleInfo, PredIdTable,
        [ExportedProc | ExportedProcs], [ExportDefn | ExportDefns]) :-
    export_proc_to_c(ModuleInfo, PredIdTable, ExportedProc, ExportDefn),
    export_procs_to_c(ModuleInfo, PredIdTable, ExportedProcs, ExportDefns).

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
:- pred export_proc_to_c(module_info::in, pred_id_table::in,
    pragma_exported_proc::in, foreign_export_defn::out) is det.

export_proc_to_c(ModuleInfo, PredIdTable, ExportedProc, ExportDefn) :-
    ExportedProc = pragma_exported_proc(Lang, PredId, ProcId, CFunction,
        _Context),
    expect(unify(Lang, lang_c), $pred, "foreign language other than C"),
    get_export_info_for_lang_c(ModuleInfo, PredIdTable, PredId, ProcId,
        DeclareString, CRetType, MaybeDeclareRetval, MaybeFail, MaybeSucceed,
        ArgInfoTypes),
    get_argument_declarations_for_lang_c(ModuleInfo, yes, ArgInfoTypes,
        ArgDecls),

    % Work out which arguments are input, and which are output, and copy
    % to/from the Mercury registers.
    pass_input_args(ModuleInfo, 0, ArgInfoTypes, PassInputArgs),
    retrieve_output_args(ModuleInfo, 0, ArgInfoTypes, RetrieveOutputArgs),

    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    ProcLabelString = proc_label_to_c_string(add_label_prefix, ProcLabel),

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

    % get_export_info_for_lang_c(ModuleInfo, PredIdTable, PredId, ProcId,
    %   HowToDeclareLabel, CRetType, MaybeDeclareRetval,
    %   MaybeFail, MaybeSuccess, ArgInfoTypes):
    %
    % For a given procedure, figure out the information about that procedure
    % that is needed to export it:
    % - how to declare the procedure's entry label;
    % - the C return type, and the C declaration for the variable
    %   holding the return value (if any);
    % - the actions on success and failure; and
    % - the argument locations/modes/types.
    %
:- pred get_export_info_for_lang_c(module_info::in, pred_id_table::in,
    pred_id::in, proc_id::in, string::out, string::out, string::out,
    string::out, string::out, assoc_list(arg_info, mer_type)::out) is det.

get_export_info_for_lang_c(ModuleInfo, PredIdTable, PredId, ProcId,
        HowToDeclareLabel, CRetType, MaybeDeclareRetval,
        MaybeFail, MaybeSucceed, ArgInfoTypes) :-
    map.lookup(PredIdTable, PredId, PredInfo),
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
        generate_proc_arg_info(ModuleInfo, Markers, ArgTypes,
            ProcInfo, NewProcInfo),
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
            is_type_a_dummy(ModuleInfo, RetType) = is_not_dummy_type
        then
            MaybeForeignRetType = is_this_a_foreign_type(ModuleInfo, RetType),
            CRetType = maybe_foreign_type_to_c_string(RetType,
                MaybeForeignRetType),
            arg_loc_to_string(RetArgLoc, RetArgString0),
            convert_type_from_mercury(RetArgLoc, RetArgString0, RetType,
                RetArgString),
            MaybeDeclareRetval = "\t" ++ CRetType ++ " return_value;\n",
            % We need to unbox non-word-sized foreign types
            % before returning them to C code.
            (
                MaybeForeignRetType = yes(_),
                SetReturnValue = "\tMR_MAYBE_UNBOX_FOREIGN_TYPE("
                    ++ CRetType ++ ", " ++ RetArgString
                    ++ ", return_value);\n"
            ;
                MaybeForeignRetType = no,
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
        unexpected($pred, "Attempt to export model_non procedure.")
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
    is_type_a_dummy(ModuleInfo, Type) = is_not_dummy_type.

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
    MaybeForeignType = is_this_a_foreign_type(ModuleInfo, Type),
    TypeString0 = maybe_foreign_type_to_c_string(Type, MaybeForeignType),
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
        MaybeForeignType = is_this_a_foreign_type(ModuleInfo, Type),
        % We need to box non-word-sized foreign types
        % before passing them to Mercury code.
        (
            MaybeForeignType = yes(ForeignType),
            CType = foreign_type_to_c_string(ForeignType),
            PassHeadInputArg = "\tMR_MAYBE_BOX_FOREIGN_TYPE(" ++
                CType ++ ", " ++ ArgName ++ ", " ++ ArgLocString ++ ");\n"
        ;
            MaybeForeignType = no,
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
        MaybeForeignType = is_this_a_foreign_type(ModuleInfo, Type),
        % We need to unbox non-word-sized foreign types
        % before returning them to C code
        (
            MaybeForeignType = yes(ForeignType),
            CType = foreign_type_to_c_string(ForeignType),
            RetrieveHeadOutputArg = "\tMR_MAYBE_UNBOX_FOREIGN_TYPE(" ++
                CType ++ ", " ++ ArgLocString ++ ", * " ++ ArgName ++ ");\n"
        ;
            MaybeForeignType = no,
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

%-----------------------------------------------------------------------------%
%
% Code to create the .mh files.
%

produce_header_file(ProgressStream, ModuleInfo, ForeignExportDecls,
        ModuleName, !IO) :-
    % We always produce a .mh file because with intermodule optimization
    % enabled, the .o file depends on all the .mh files of the imported
    % modules. so we need to produce a .mh file even if it contains nothing.
    module_info_get_globals(ModuleInfo, Globals),
    module_name_to_file_name_create_dirs(Globals, $pred, ext_cur(ext_cur_mh),
        ModuleName, FileName, !IO),
    MaybeThisFileName = yes(FileName),
    TmpFileName = FileName ++ ".tmp",
    io.open_output(TmpFileName, Result, !IO),
    (
        Result = ok(FileStream),
        module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
        library.version(Version, Fullarch),
        io.write_strings(FileStream, [
            "// Automatically generated from `", SourceFileName, "'\n",
            "// by the Mercury compiler,\n",
            "// version ", Version, "\n",
            "// configured for ", Fullarch, ".\n",
            "// Do not edit.\n"
            ], !IO),
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
            CForeignDeclCodeResults = []
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
            list.map_foldl(
                output_foreign_decl(FileStream, MaybeSetLineNumbers,
                    MaybeThisFileName, SourceFileName,
                    yes(foreign_decl_is_exported)),
                CForeignDeclCodes, CForeignDeclCodeResults, !IO),
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

        list.filter_map(maybe_is_error, CForeignDeclCodeResults, Errors),
        (
            Errors = [],
            % Rename "<ModuleName>.mh.tmp" to "<ModuleName>.mh".
            copy_dot_tmp_to_base_file_report_any_error(ProgressStream,
                Globals, ".mh", FileName, _Succeeded, !IO)
        ;
            Errors = [_ | _],
            io.file.remove_file(TmpFileName, _, !IO),
            % report_error sets the exit status.
            list.foldl(report_error(ProgressStream), Errors, !IO)
        )
    ;
        Result = error(_),
        io.progname_base("export.m", ProgName, !IO),
        io.format(ProgressStream, "\n%s: can't open `%s' for output\n",
            [s(ProgName), s(TmpFileName)], !IO),
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
        io.format(Stream, "%s %s(%s);\n",
            [s(CRetType), s(CFunction), s(ArgDecls)], !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred, "foreign languages other than C unimplemented")
    ),
    write_export_decls(Stream, ExportDecls, !IO).

:- pred output_foreign_decl(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in, string::in,
    maybe(foreign_decl_is_local)::in, foreign_decl_code::in, maybe_error::out,
    io::di, io::uo) is det.

output_foreign_decl(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        SourceFileName, MaybeDesiredIsLocal, DeclCode, Res, !IO) :-
    DeclCode = foreign_decl_code(Lang, IsLocal, LiteralOrInclude, Context),
    expect(unify(Lang, lang_c), $pred, "Lang != lang_c"),
    ( if
        (
            MaybeDesiredIsLocal = no
        ;
            MaybeDesiredIsLocal = yes(DesiredIsLocal),
            DesiredIsLocal = IsLocal
        )
    then
        output_foreign_literal_or_include(Stream, MaybeSetLineNumbers,
            MaybeThisFileName, SourceFileName, LiteralOrInclude, Context,
            Res, !IO)
    else
        Res = ok
    ).

:- pred output_foreign_literal_or_include(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in, string::in,
    foreign_literal_or_include::in, prog_context::in, maybe_error::out,
    io::di, io::uo) is det.

output_foreign_literal_or_include(Stream, MaybeSetLineNumbers,
        MaybeThisFileName, SourceFileName, LiteralOrInclude, Context,
        Res, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        File = term_context.context_file(Context),
        Line = term_context.context_line(Context),
        c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line,
            !IO),
        io.write_string(Stream, Code, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, IncludePath, 1,
            !IO),
        write_include_file_contents(Stream, IncludePath, Res, !IO)
    ),
    io.nl(Stream, !IO),
    c_util.maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        !IO).

%-----------------------------------------------------------------------------%

convert_type_to_mercury(RvalStr, Type, TargetArgLoc, ConvertedRvalStr) :-
    % NOTE The predicate convert_arg_type_to_mercury in fact_table.m
    % duplicates the logic of this predicate as it pertains to the
    % types that may occur in fact tables. Changes here may need to be made
    % there as well.
    (
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_string,
            ConvertedRvalStr = "(MR_Word) " ++ RvalStr
        ;
            BuiltinType = builtin_type_float,
            (
                TargetArgLoc = reg(reg_r, _),
                ConvertedRvalStr = "MR_float_to_word(" ++ RvalStr ++ ")"
            ;
                TargetArgLoc = reg(reg_f, _),
                ConvertedRvalStr = RvalStr
            )
        ;
            BuiltinType = builtin_type_char,
            % We need to explicitly cast to MR_UnsignedChar
            % to avoid problems with C compilers for which `char'
            % is signed.
            ConvertedRvalStr = "(MR_UnsignedChar) " ++ RvalStr
        ;
            BuiltinType = builtin_type_int(IntType),
            (
                ( IntType = int_type_int
                ; IntType = int_type_uint
                ; IntType = int_type_int8
                ; IntType = int_type_uint8
                ; IntType = int_type_int16
                ; IntType = int_type_uint16
                ; IntType = int_type_int32
                ; IntType = int_type_uint32
                ),
                ConvertedRvalStr = RvalStr
            ;
                IntType = int_type_int64,
                ConvertedRvalStr = "MR_int64_to_word(" ++ RvalStr ++ ")"
            ;
                IntType = int_type_uint64,
                ConvertedRvalStr = "MR_uint64_to_word(" ++ RvalStr ++ ")"
            )
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        ConvertedRvalStr = RvalStr
    ).

convert_type_from_mercury(SourceArgLoc, RvalStr, Type, ConvertedRvalStr) :-
    % NOTE The predicate convert_arg_type_from_mercury in fact_table.m
    % duplicates the logic of this predicate as it pertains to the
    % types that may occur in fact tables. Changes here may need to be made
    % there as well.
    (
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_string,
            ConvertedRvalStr = "(MR_String) " ++ RvalStr
        ;
            BuiltinType = builtin_type_float,
            (
                SourceArgLoc = reg(reg_r, _),
                ConvertedRvalStr = "MR_word_to_float(" ++ RvalStr ++ ")"
            ;
                SourceArgLoc = reg(reg_f, _),
                ConvertedRvalStr = RvalStr
            )
        ;
            BuiltinType = builtin_type_int(IntType),
            (
                ( IntType = int_type_int
                ; IntType = int_type_uint
                ; IntType = int_type_int8
                ; IntType = int_type_uint8
                ; IntType = int_type_int16
                ; IntType = int_type_uint16
                ; IntType = int_type_int32
                ; IntType = int_type_uint32
                ),
                ConvertedRvalStr = RvalStr
            ;
                IntType = int_type_int64,
                ConvertedRvalStr = "MR_word_to_int64(" ++ RvalStr ++ ")"
            ;
                IntType = int_type_uint64,
                ConvertedRvalStr = "MR_word_to_uint64(" ++ RvalStr ++ ")"
            )
        ;
            BuiltinType = builtin_type_char,
            ConvertedRvalStr = RvalStr
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        ConvertedRvalStr = RvalStr
    ).

%-----------------------------------------------------------------------------%
%
% Code for writing out foreign exported enumerations.
%

% For C/C++ we emit a #defined constant for constructors exported from an
% enumeration.

:- pred exported_enum_is_for_c(exported_enum_info::in) is semidet.

exported_enum_is_for_c(ExportedEnumInfo) :-
    ExportedEnumInfo ^ eei_language = lang_c.

:- pred output_exported_c_enum(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in,
    exported_enum_info::in, io::di, io::uo) is det.

output_exported_c_enum(Stream, MaybeSetLineNumbers, MaybeThisFileName,
        ExportedEnumInfo, !IO) :-
    ExportedEnumInfo = exported_enum_info(_TypeCtor, CtorRepns, Lang,
        NameMapping, Context),
    expect(unify(Lang, lang_c), $pred, "Lang != lang_c"),
    list.foldl(foreign_const_name_and_tag(NameMapping),
        CtorRepns, cord.init, ForeignNamesAndTagsCord),
    ForeignNamesAndTags = cord.list(ForeignNamesAndTagsCord),
    File = term_context.context_file(Context),
    Line = term_context.context_line(Context),
    c_util.maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line, !IO),
    output_exported_enum_constname_tags(Stream, ForeignNamesAndTags, !IO),
    c_util.maybe_reset_line_num(Stream, MaybeSetLineNumbers,
        MaybeThisFileName, !IO).

    % Values of this type associate the foreign name of an exported
    % enum constructor with the foreign tag of that constructor.
    % The tag will either be an integer (for enumerations whose
    % representations are decided by the Mercury compiler) or strings
    % (for enumerations whose representations are decide by a
    % foreign_enum pragma).
    %
:- type exported_enum_name_and_tag_rep
    --->    ee_name_and_tag_rep_int(string, int)
    ;       ee_name_and_tag_rep_string(string, string).

:- pred output_exported_enum_constname_tags(io.text_output_stream::in,
    list(exported_enum_name_and_tag_rep)::in, io::di, io::uo) is det.

output_exported_enum_constname_tags(_Stream, [], !IO).
output_exported_enum_constname_tags(Stream, [NameAndTag | NameAndTags], !IO) :-
    output_exported_enum_constname_tag(Stream, NameAndTag, !IO),
    output_exported_enum_constname_tags(Stream, NameAndTags, !IO).

:- pred output_exported_enum_constname_tag(io.text_output_stream::in,
    exported_enum_name_and_tag_rep::in, io::di, io::uo) is det.

output_exported_enum_constname_tag(Stream, NameAndTag, !IO) :-
    (
        NameAndTag = ee_name_and_tag_rep_int(Name, RawIntTag),
        io.format(Stream, "#define %s %d\n", [s(Name), i(RawIntTag)], !IO)
    ;
        NameAndTag = ee_name_and_tag_rep_string(Name, RawStrTag),
        io.format(Stream, "#define %s %s\n", [s(Name), s(RawStrTag)], !IO)
    ).

:- pred foreign_const_name_and_tag(map(string, string)::in,
    constructor_repn::in,
    cord(exported_enum_name_and_tag_rep)::in,
    cord(exported_enum_name_and_tag_rep)::out) is det.

foreign_const_name_and_tag(Mapping, CtorRepn, !NamesAndTagsCord) :-
    CtorRepn = ctor_repn(_, _, SymName, ConsTag, _, Arity, _),
    expect(unify(Arity, 0), $pred, "enum constant arity != 0"),
    Name = unqualify_name(SymName),
    map.lookup(Mapping, Name, ForeignName),
    (
        ConsTag = int_tag(IntTag),
        (
            IntTag = int_tag_int(Int),
            NameAndTag = ee_name_and_tag_rep_int(ForeignName, Int)
        ;
            ( IntTag = int_tag_uint(_)
            ; IntTag = int_tag_int8(_)
            ; IntTag = int_tag_uint8(_)
            ; IntTag = int_tag_int16(_)
            ; IntTag = int_tag_uint16(_)
            ; IntTag = int_tag_int32(_)
            ; IntTag = int_tag_uint32(_)
            ; IntTag = int_tag_int64(_)
            ; IntTag = int_tag_uint64(_)
            ),
            unexpected($pred, "enum constant requires an int tag")
        )
    ;
        ConsTag = dummy_tag,
        NameAndTag = ee_name_and_tag_rep_int(ForeignName, 0)
    ;
        ConsTag = foreign_tag(_ForeignLang, ForeignTag),
        NameAndTag = ee_name_and_tag_rep_string(ForeignName, ForeignTag)
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = local_args_tag(_)
        ; ConsTag = remote_args_tag(_)
        ; ConsTag = no_tag
        ),
        unexpected($pred, "enum constant requires an int tag")
    ),
    !:NamesAndTagsCord = cord.snoc(!.NamesAndTagsCord, NameAndTag).

%-----------------------------------------------------------------------------%

c_type_is_word_sized_int_or_ptr("MR_Word").
c_type_is_word_sized_int_or_ptr("MR_TypeInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeCtorInfo").
c_type_is_word_sized_int_or_ptr("MR_TypeClassInfo").
c_type_is_word_sized_int_or_ptr("MR_BaseTypeclassInfo").

%-----------------------------------------------------------------------------%
:- end_module backend_libs.export.
%-----------------------------------------------------------------------------%
