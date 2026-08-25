%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: fact_table_gen.m.
% Main author: dmo.
%
% This module is the last to execute of the three modules that together
% implement fact tables.
%
% First, fact_table_check.m checks whether the argument types and modes
% of the selected predicate allow it to be implemented by fact tables.
% (This is because fact tables support omly a small subset of Mercury.)
%
% Second, fact_table_compile.m checks each fact in the fact table file
% whether it is type- and mode-correct, and compiles the facts that
% pass these tests into big tables in the .c file that implements
% the fact table.
%
% Third, this module generates the foreign_proc definitions for
% each procedure of the fact table predicate. These foreign_procs
% serve as the interface between those tables and the rest of the
% Mercury program.
%
%---------------------------------------------------------------------------%

:- module ll_backend.fact_table_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_proc.
:- import_module hlds.pred_proc_id.
:- import_module ll_backend.fact_table_check.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.

%---------------------------------------------------------------------------%

    % fact_table_generate_c_code_for_proc(ModuleInfo, PredSymName,
    %   ProcId, PrimaryProcId, ProcInfo, GenInfo, VarSet, PragmaVars,
    %   ProcCode, ExtraCode):
    %
    % Generate C code to look up a fact table in a given mode.
    % ProcCode is the C code for the procedure, ExtraCode is extra C code
    % that should be included in the module. VarSet is the varset of
    % the foreign_proc our caller should construct, and PragmaVars
    % are its arguments.
    %
    % Model_non foreign_procs were not supported by the compiler when this
    % code was written. To get around this, the C_ProcCode generated for
    % model_non code pops off the stack frame that is automatically created
    % by the compiler and jumps to the code contained in ExtraCode.
    % ExtraCode declares the required labels and creates a new stack frame
    % with the required number of framevars. It then does all the work required
    % to look up the fact table.
    %
:- pred fact_table_generate_c_code_for_proc(module_info::in, sym_name::in,
    proc_id::in, proc_id::in, proc_info::in, fact_table_gen_info::in,
    prog_varset::out, list(pragma_var)::out, string::out, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_proc_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.

:- import_module char.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fact_table_generate_c_code_for_proc(ModuleInfo, PredSymName,
        ProcId, PrimaryProcId, ProcInfo, GenInfo, VarSet, PragmaVars,
        ProcCode, ExtraCode) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, fact_table_max_array_size,
        FactTableArraySize),

    proc_info_interface_determinism(ProcInfo, Determinism),
    PredSymNameStr = sym_name_mangle(PredSymName),
    GenInfo = fact_table_gen_info(FactArgInfos, FactTableProcMap, _, _),
    Types = list.map((func(fact_arg_info(Type, _, _)) = Type), FactArgInfos),
    map.lookup(FactTableProcMap, ProcId, FactTableProcInfo),
    FactTableProcInfo = fact_table_proc_info(FactTableVars, ModeClass, VarSet),
    PragmaVars =
        list.map((func(fact_table_var(_, _, _, PV)) = PV), FactTableVars),
    (
        ModeClass = all_out,
        (
            Determinism = detism_multi,
            generate_multi_code(ModuleInfo, FactTableArraySize, PredSymNameStr,
                ProcId, Types, FactTableVars, ProcCode, ExtraCode)
        ;
            Determinism = detism_cc_multi,
            generate_cc_multi_code(PredSymNameStr, FactTableVars, ProcCode),
            ExtraCode = ""
        ;
            ( Determinism = detism_det
            ; Determinism = detism_semi
            ; Determinism = detism_non
            ; Determinism = detism_cc_non
            ; Determinism = detism_failure
            ; Determinism = detism_erroneous
            ),
            generate_dummy_code(FactTableVars, ProcCode, ExtraCode)
        )
    ;
        ModeClass = all_in,
        (
            Determinism = detism_semi,
            generate_semidet_all_in_code(FactTableArraySize, PredSymNameStr,
                ProcId, Types, FactTableVars, ProcCode),
            ExtraCode = ""
        ;
            ( Determinism = detism_det
            ; Determinism = detism_multi
            ; Determinism = detism_non
            ; Determinism = detism_cc_multi
            ; Determinism = detism_cc_non
            ; Determinism = detism_failure
            ; Determinism = detism_erroneous
            ),
            generate_dummy_code(FactTableVars, ProcCode, ExtraCode)
        )
    ;
        ModeClass = in_out,
        (
            ( Determinism = detism_semi
            ; Determinism = detism_cc_non
            ),
            generate_semidet_in_out_code(FactTableArraySize, PredSymNameStr,
                ProcId, Types, FactTableVars, ProcCode),
            ExtraCode = ""
        ;
            Determinism = detism_non,
            ( if ProcId = PrimaryProcId then
                generate_primary_nondet_code(ModuleInfo, FactTableArraySize,
                    PredSymNameStr, ProcId, Types, FactTableVars,
                    ProcCode, ExtraCode)
            else
                generate_secondary_nondet_code(ModuleInfo, FactTableArraySize,
                    PredSymNameStr, ProcId, Types, FactTableVars,
                    ProcCode, ExtraCode)
            )
        ;
            ( Determinism = detism_det
            ; Determinism = detism_multi
            ; Determinism = detism_cc_multi
            ; Determinism = detism_failure
            ; Determinism = detism_erroneous
            ),
            generate_dummy_code(FactTableVars, ProcCode, ExtraCode)
        )
    ).

    % Generate contents for a dummy implementation of a fact table.
    % Used when there is a determinism error in a procedure, which
    % should be reported during determinism analysis when the inferred
    % determinism we recorded above is compared to the declared determinism.
    % So all we need to do here is return some C code that does nothing.
    %
:- pred generate_dummy_code(list(fact_table_var)::in,
    string::out, string::out) is det.

generate_dummy_code(FactTableVars, ProcCode, ExtraCode) :-
    % List the variables in the C code to stop the compiler giving
    % a warning about them not being there.
    fact_table_vars_to_names_string(FactTableVars, NamesString),
    string.format("/* %s */", [s(NamesString)], ProcCode),
    ExtraCode = "".

%---------------------------------------------------------------------------%
%
% Implement detism_multi procedures.
%

:- pred generate_multi_code(module_info::in, int::in, string::in, proc_id::in,
    list(fact_arg_type)::in, list(fact_table_var)::in,
    string::out, string::out) is det.

generate_multi_code(ModuleInfo, FactTableArraySize, PredName, ProcId,
        Types, FactTableVars, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PredName, ProcId, FactTableVars,
        ExtraCodeLabel, ProcCode),
    ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
    MR_init_entry(%s);
    MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
    MR_mkframe(""%s/%d"", 1, MR_LABEL(%s_i1));
    MR_framevar(1) = (MR_Integer) 0;
    MR_GOTO(MR_LABEL(%s_i1));
MR_define_label(%s_i1);
    if (MR_framevar(1) >= %s) MR_fail();
    {
        // declare argument vars
%s
        MR_Word ind = MR_framevar(1), tmp;
        // lookup fact table
%s
        // save output args to registers
%s
    }
    MR_framevar(1)++;
    MR_succeed();
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
    %s_module();
}

    ",

    string.format("mercury__%s_fact_table_num_facts",
        [s(PredName)], NumFactsVar),
    list.length(FactTableVars, Arity),
    generate_argument_vars_code(ModuleInfo, Types, FactTableVars,
        ArgDeclCode, _InputCode, OutputCode, _, _, _),
    generate_fact_lookup_code(FactTableArraySize, PredName,
        Types, FactTableVars, 1, FactLookupCode),

    string.format(ExtraCodeTemplate, [
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(PredName),
        i(Arity),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(NumFactsVar),
        s(ArgDeclCode),
        s(FactLookupCode),
        s(OutputCode),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel)],
        ExtraCode).

:- pred generate_nondet_proc_code(string::in, proc_id::in,
    list(fact_table_var)::in, string::out, string::out) is det.

generate_nondet_proc_code(PredName, ProcId, FactTableVars,
        ExtraCodeLabel, ProcCode) :-
    ProcCodeTemplate =  "

    // Mention arguments %s to stop the compiler giving a warning.
    //
    // Pop off the nondet stack frame that the pragma c_code generates
    // then jump to the code where the work is actually done.

    MR_maxfr_word = MR_prevfr_slot_word(MR_curfr);
    MR_curfr_word = MR_succfr_slot_word(MR_curfr);
    {
        MR_declare_entry(%s);
        MR_GOTO(MR_ENTRY(%s));
    }
    ",

    list.length(FactTableVars, Arity),
    proc_id_to_int(ProcId, ProcIdInt),
    string.format("mercury__%s_%d_%d_xx",
        [s(PredName), i(Arity), i(ProcIdInt)], ExtraCodeLabel),
    fact_table_vars_to_names_string(FactTableVars, NamesString),
    string.format(ProcCodeTemplate, [s(NamesString), s(ExtraCodeLabel),
        s(ExtraCodeLabel)], ProcCode).

%---------------------------------------------------------------------------%
%
% Implement detism_cc_multi procedures.
%

    % For cc_multi output mode, just return the first fact in the table.
    %
:- pred generate_cc_multi_code(string::in, list(fact_table_var)::in,
    string::out) is det.

generate_cc_multi_code(PredName, FactTableVars, ProcCode) :-
    string.format("mercury__%s_fact_table", [s(PredName)], StructName),
    generate_cc_multi_code_loop(StructName, FactTableVars, 1, "", ProcCode).

:- pred generate_cc_multi_code_loop(string::in,
    list(fact_table_var)::in, int::in, string::in, string::out) is det.

generate_cc_multi_code_loop(_, [], _, !ProcCode).
generate_cc_multi_code_loop(StructName, [FactTableVar | FactTableVars], ArgNum,
        !ProcCode) :-
    FactTableVar = fact_table_var(VarName, _, _, _),
    string.format("\t\t%s = %s[0][0].V_%d;\n", [s(VarName), s(StructName),
        i(ArgNum)], ArgAssignCode),
    !:ProcCode = !.ProcCode ++ ArgAssignCode,
    generate_cc_multi_code_loop(StructName, FactTableVars, ArgNum + 1,
        !ProcCode).

%---------------------------------------------------------------------------%
%
% Implement detism_non procedures.
%

    % Generate code for the nondet mode with the primary key.
    %
:- pred generate_primary_nondet_code(module_info::in, int::in,
    string::in, proc_id::in, list(fact_arg_type)::in, list(fact_table_var)::in,
    string::out, string::out) is det.

generate_primary_nondet_code(ModuleInfo, FactTableArraySize, PredName, ProcId,
        Types, FactTableVars, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PredName, ProcId, FactTableVars,
        ExtraCodeLabel, ProcCode),
    ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
    MR_init_entry(%s);
    MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
    MR_mkframe(""%s/%d"", %d, MR_LABEL(%s_i1));
    {
        // create argument vars
%s
        // declare local variables
%s
        // copy registers to input arg vars
%s
        // copy registers to framevars
%s
        // lookup hash table
%s
    success_code_%s:
        // lookup fact table
%s
        // save output args to registers
%s
        MR_framevar(1) = ind + 1;
        MR_succeed();
    failure_code_%s:
        MR_fail();
    }
MR_define_label(%s_i1);
    if (MR_framevar(1) >= %s)
        MR_fail();
    {
        // create argument vars
%s
        int ind = MR_framevar(1);
        // copy framevars to registers
%s
        // copy registers to input arg vars
%s
        // test fact table entry
%s
        // lookup fact table
%s
        // save output args to registers
%s
    }
    MR_framevar(1)++;
    MR_succeed();
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
    %s_module();
}

    ",

    generate_argument_vars_code(ModuleInfo, Types, FactTableVars,
        ArgDeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        NumFrameVars),
    generate_decl_code(PredName, ProcId, DeclCode),
    proc_id_to_int(ProcId, ProcIdInt),
    string.format("%s_%d", [s(PredName), i(ProcIdInt)], LabelName),
    generate_hash_code(FactTableArraySize, PredName, LabelName, 0,
        Types, FactTableVars, 1, HashCode),
    generate_fact_lookup_code(FactTableArraySize, PredName,
        Types, FactTableVars, 1, FactLookupCode),
    generate_fact_test_code(FactTableArraySize, PredName,
        Types, FactTableVars, FactTestCode),

    string.format("mercury__%s_fact_table_num_facts",
        [s(PredName)], NumFactsVar),
    list.length(FactTableVars, Arity),

    string.format(ExtraCodeTemplate, [
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(PredName),
        i(Arity),
        i(NumFrameVars),
        s(ExtraCodeLabel),
        s(ArgDeclCode),
        s(DeclCode),
        s(InputCode),
        s(SaveRegsCode),
        s(HashCode),
        s(LabelName),
        s(FactLookupCode),
        s(OutputCode),
        s(LabelName),
        s(ExtraCodeLabel),
        s(NumFactsVar),
        s(ArgDeclCode),
        s(GetRegsCode),
        s(InputCode),
        s(FactTestCode),
        s(FactLookupCode),
        s(OutputCode),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel)
        ],
        ExtraCode).

    % Generate code for a nondet mode using a secondary key.
    %
:- pred generate_secondary_nondet_code(module_info::in, int::in, string::in,
    proc_id::in, list(fact_arg_type)::in, list(fact_table_var)::in,
    string::out, string::out) is det.

generate_secondary_nondet_code(ModuleInfo, FactTableArraySize, PredName,
        ProcId, Types, FactTableVars, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PredName, ProcId, FactTableVars,
        ExtraCodeLabel, ProcCode),
    ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
    MR_init_entry(%s);
    MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
    MR_mkframe(""%s/%d"", 4, MR_LABEL(%s_i1));
    {
        // create argument vars
%s
        // declare local variables
%s
        // copy registers to input arg vars
%s
        // lookup hash table
%s
    success_code_%s:
        // lookup fact table
%s
        // save output args to registers
%s
        if (hashval == -1) MR_succeed_discard();
        MR_framevar(1) = hashval;
        MR_framevar(2) = (MR_Word) current_table;
        MR_framevar(3) = (MR_Word) keytype;
        MR_framevar(4) = current_key;
        MR_succeed();
    failure_code_%s:
        MR_fail();
    }
MR_define_label(%s_i1);
    {
        // create argument vars
%s
        MR_Integer  hashval = MR_framevar(1);
        MR_Word     ind;
        void        *current_table = (void *) MR_framevar(2);
        char        keytype = (char) MR_framevar(3);

        // lookup hash table
        switch(keytype)
        {
            case 's':
%s
                break;
            case 'i':
%s
                break;
            case 'f':
%s
                break;
            default:
                MR_fatal_error(
                    ""fact table hash lookup: nondet stack corrupted?"");
        }
    success_code_%s:
        // lookup fact table
%s
        // save output args to registers
%s
        if (hashval == -1) MR_succeed_discard();
        MR_framevar(1) = hashval;
        MR_succeed();
    failure_code_%s:
        MR_fail();
    }
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
    %s_module();
}

    ",

    generate_argument_vars_code(ModuleInfo, Types, FactTableVars,
        ArgDeclCode, InputCode, OutputCode, _SaveRegsCode, _GetRegsCode,
        _NumFrameVars),
    generate_decl_code(PredName, ProcId, DeclCode),
    proc_id_to_int(ProcId, ProcIdInt),
    string.format("%s_%d", [s(PredName), i(ProcIdInt)], LabelName),
    string.append(LabelName, "_2", LabelName2),
    generate_hash_code(FactTableArraySize, PredName, LabelName, 0,
        Types, FactTableVars, 1, HashCode),

    StringVarName = "(char *) MR_framevar(4)",
    IntVarName = "MR_framevar(4)",
    FloatVarName = "MR_word_to_float(MR_framevar(4))",
    generate_hash_lookup_code(StringVarName, LabelName2, 0,
        string_equals, 's', do_not_test_keys, StringHashLookupCode),
    generate_hash_lookup_code(IntVarName, LabelName2, 1,
        plain_equals, 'i', do_not_test_keys, IntHashLookupCode),
    generate_hash_lookup_code(FloatVarName, LabelName2, 2,
        plain_equals, 'f', do_not_test_keys, FloatHashLookupCode),
    generate_fact_lookup_code(FactTableArraySize, PredName,
        Types, FactTableVars, 1, FactLookupCode),
    list.length(FactTableVars, Arity),

    string.format(ExtraCodeTemplate, [
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(PredName),
        i(Arity),
        s(ExtraCodeLabel),
        s(ArgDeclCode),
        s(DeclCode),
        s(InputCode),
        s(HashCode),
        s(LabelName),
        s(FactLookupCode),
        s(OutputCode),
        s(LabelName),
        s(ExtraCodeLabel),
        s(ArgDeclCode),
        s(StringHashLookupCode),
        s(IntHashLookupCode),
        s(FloatHashLookupCode),
        s(LabelName2),
        s(FactLookupCode),
        s(OutputCode),
        s(LabelName2),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel),
        s(ExtraCodeLabel)
        ],
        ExtraCode).

%---------------------------------------------------------------------------%
%
% Implement detism_semi procedures.
%

    % Generate semidet code for all_in mode.
    %
:- pred generate_semidet_all_in_code(int::in, string::in, proc_id::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, string::out) is det.

generate_semidet_all_in_code(FactTableArraySize, PredName, ProcId,
        Types, FactTableVars, ProcCode) :-
    generate_decl_code(PredName, ProcId, DeclCode),

    proc_id_to_int(ProcId, ProcIdInt),
    string.format("%s_%d", [s(PredName), i(ProcIdInt)], LabelName),
    generate_hash_code(FactTableArraySize, PredName, LabelName, 0,
        Types, FactTableVars, 1, HashCode),

    SuccessCodeTemplate = "
        success_code_%s:
            SUCCESS_INDICATOR = MR_TRUE;
            goto skip_%s;
        failure_code_%s:
            SUCCESS_INDICATOR = MR_FALSE;
        skip_%s:
            ;
    ",
    string.format(SuccessCodeTemplate, [s(LabelName), s(LabelName),
        s(LabelName), s(LabelName)], SuccessCode),

    ProcCode = "\t{\n" ++ DeclCode ++ HashCode ++ SuccessCode ++ "\t}\n".

%---------------------%

    % Generate code for semidet and cc_nondet in_out modes. Lookup key in
    % hash table and if found return first match. If not found, fail.
    %
:- pred generate_semidet_in_out_code(int::in, string::in, proc_id::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, string::out) is det.

generate_semidet_in_out_code(FactTableArraySize, PredName, ProcId,
        Types, FactTableVars, ProcCode):-
    generate_decl_code(PredName, ProcId, DeclCode),

    proc_id_to_int(ProcId, ProcIdInt),
    string.format("%s_%d", [s(PredName), i(ProcIdInt)], LabelName),
    generate_hash_code(FactTableArraySize, PredName, LabelName, 0,
        Types, FactTableVars, 1, HashCode),

    SuccessCodeTemplate = "
        success_code_%s:
            SUCCESS_INDICATOR = MR_TRUE;
    ",
    string.format(SuccessCodeTemplate, [s(LabelName)], SuccessCode),

    generate_fact_lookup_code(FactTableArraySize, PredName,
        Types, FactTableVars, 1, FactLookupCode),

    FailCodeTemplate = "
            goto skip_%s;
        failure_code_%s:
            SUCCESS_INDICATOR = MR_FALSE;
        skip_%s:
            ;
    ",
    string.format(FailCodeTemplate, [s(LabelName), s(LabelName),
        s(LabelName)], FailCode),

    ProcCode = "\t{\n" ++ DeclCode ++ HashCode ++ SuccessCode
        ++ FactLookupCode ++ FailCode ++ "\t}\n".

%---------------------------------------------------------------------------%
%
% Some service procedures used by the generate_*_code predicates above.
%

:- pred generate_decl_code(string::in, proc_id::in, string::out) is det.

generate_decl_code(Name, ProcId, DeclCode) :-
    DeclCodeTemplate = "
            MR_Integer hashval, hashsize;
            MR_Word ind;
            void *current_table;
            char keytype = '\\0';
            MR_Word current_key, tmp;

            // Initialise current_table to the top level hash table
            // for this ProcId.
            current_table =
                &mercury__%s_fact_table_hash_table_%d_0;

    ",
    proc_id_to_int(ProcId, ProcIdInt),
    string.format(DeclCodeTemplate, [s(Name), i(ProcIdInt)], DeclCode).

    % Generate code to calculate hash values and lookup the hash tables.
    %
:- pred generate_hash_code(int::in, string::in, string::in, int::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    string::out) is det.

generate_hash_code(_, _, _, _, [], [], _, "").
generate_hash_code(_, _, _, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
generate_hash_code(_, _, _, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
generate_hash_code(FactTableArraySize, PredName, LabelName, LabelNum,
        [Type | Types], [FactTableVar | FactTableVars], ArgNum, Code) :-
    FactTableVar = fact_table_var(VarName, Mode, _, _),
    NextArgNum = ArgNum + 1,
    (
        Mode = fully_in,
        (
            Type = fact_arg_type_int,
            generate_hash_int_code(FactTableArraySize, PredName, VarName,
                LabelName, LabelNum, Types, FactTableVars, NextArgNum, ArgCode)
        ;
            Type = fact_arg_type_float,
            generate_hash_float_code(FactTableArraySize, PredName, VarName,
                LabelName, LabelNum, Types, FactTableVars, NextArgNum, ArgCode)
        ;
            Type = fact_arg_type_string,
            generate_hash_string_code(FactTableArraySize, PredName, VarName,
                LabelName, LabelNum, Types, FactTableVars, NextArgNum, ArgCode)
        ),
        generate_hash_code(FactTableArraySize, PredName, LabelName,
            LabelNum + 1, Types, FactTableVars, NextArgNum, ArgsCode),
        Code = ArgCode ++ ArgsCode
    ;
        Mode = fully_out,
        % Skip non-input arguments.
        generate_hash_code(FactTableArraySize, PredName, LabelName, LabelNum,
            Types, FactTableVars, NextArgNum, Code)
    ).

:- pred generate_hash_int_code(int::in, string::in, string::in,
    string::in, int::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    string::out) is det.

generate_hash_int_code(FactTableArraySize, PredName, VarName,
        LabelName, LabelNum, Types, FactTableVars, ArgNum, Code) :-
    TestKeys =
        test_keys(FactTableArraySize, PredName, Types, FactTableVars, ArgNum),
    generate_hash_lookup_code(VarName, LabelName, LabelNum,
        plain_equals, 'i', TestKeys, HashLookupCode),
    CodeTemplate = "

        // calculate hash value for an integer
        hashsize = ((struct MR_fact_table_hash_table_i *) current_table)
            ->size;
        hashval = (%s >= 0 ? %s : -%s) %% hashsize;

        current_key = %s;

        // lookup the hash table
        %s

    ",
    string.format(CodeTemplate,
        [s(VarName), s(VarName), s(VarName), s(VarName), s(HashLookupCode)],
        Code).

:- pred generate_hash_float_code(int::in, string::in, string::in,
    string::in, int::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    string::out) is det.

generate_hash_float_code(FactTableArraySize, PredName, VarName,
        LabelName, LabelNum, Types, FactTableVars, ArgNum, Code) :-
    TestKeys =
        test_keys(FactTableArraySize, PredName, Types, FactTableVars, ArgNum),
    generate_hash_lookup_code(VarName, LabelName, LabelNum,
        plain_equals, 'f', TestKeys, HashLookupCode),
    CodeTemplate = "

        // calculate hash value for a float
        hashsize = ((struct MR_fact_table_hash_table_f *) current_table)
            ->size;
        hashval = MR_hash_float(%s);
        hashval = (hashval >= 0 ? hashval : -hashval) %% hashsize;

        current_key = MR_float_to_word(%s);

        // lookup the hash table
        %s

    ",
    string.format(CodeTemplate,
        [s(VarName), s(VarName), s(HashLookupCode)], Code).

:- pred generate_hash_string_code(int::in, string::in, string::in,
    string::in, int::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    string::out) is det.

generate_hash_string_code(FactTableArraySize, PredName, VarName,
        LabelName, LabelNum, Types, FactTableVars, ArgNum, Code) :-
    TestKeys =
        test_keys(FactTableArraySize, PredName, Types, FactTableVars, ArgNum),
    generate_hash_lookup_code(VarName, LabelName, LabelNum,
        string_equals, 's', TestKeys, HashLookupCode),
    CodeTemplate = "

        hashsize = ((struct MR_fact_table_hash_table_s *) current_table)->size;

        // calculate hash value for a string
        {
            char *p;
            hashval = 0;
            for (p = %s ; *p != '\\0' ; p++) {
                hashval = (*p + 31 * hashval) %% hashsize;
            }
        }

        current_key = (MR_Word) %s;

        // lookup the hash table
        %s

    ",
    string.format(CodeTemplate,
        [s(VarName), s(VarName), s(HashLookupCode)], Code).

%---------------------%

:- type comparison_kind
    --->    plain_equals
    ;       string_equals.

:- inst key_char for char/0
    --->    ('s')
    ;       ('i')
    ;       ('f').

:- type maybe_test_keys
    --->    do_not_test_keys
    ;       test_keys(
                int,                    % The fact_table_size parameter.
                string,                 % predicate name
                list(fact_arg_type),
                list(fact_table_var),
                int                     % The argument number.
            ).

    % Generate code to lookup the key in the hash table.
    % KeyType should be 's', 'i' or 'f' for string, int or float,
    % respectively. CompareTemplate should be a template for testing for
    % equality for the type given, e.g. "%s == %s" for ints,
    % "strcmp(%s, %s) == 0" for strings.
    %
:- pred generate_hash_lookup_code(string::in, string::in, int::in,
    comparison_kind::in, char::in(key_char), maybe_test_keys::in,
    string::out) is det.

generate_hash_lookup_code(VarName, LabelName, LabelNum,
        ComparisonKind, KeyType, TestKeys, HashLookupCode) :-
    string.format("((struct MR_fact_table_hash_table_%c *) current_table)"
        ++ "->table[hashval]", [c(KeyType)], HashTableEntry),
    HashTableKey = HashTableEntry ++ ".key",
    (
        ComparisonKind = plain_equals,
        string.format("%s == %s", [s(HashTableKey), s(VarName)],
            CompareString)
    ;
        ComparisonKind = string_equals,
        string.format("strcmp(%s, %s) == 0", [s(HashTableKey), s(VarName)],
            CompareString)
    ),

    HashLookupCodeTemplate = "

        do {
            if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) != 0 && %s) {
                ind = (MR_Word) %s.index;
                goto found_%s_%d;
            }
        } while ((hashval = %s.next) != -1);

        // key not found
        goto failure_code_%s;

    found_%s_%d:

        if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) == 1) {
            ind = MR_FACT_TABLE_HASH_INDEX(ind);

            // check that any remaining input arguments match
            %s
            keytype = '%c';
            hashval = %s.next;
            goto success_code_%s;
        }

        current_table = (void *) MR_FACT_TABLE_HASH_POINTER(ind);

    ",
    (
        TestKeys = test_keys(FactTableArraySize, PredName, Types,
            FactTableVars, ArgNum),
        FactTableName = "mercury__" ++ PredName ++ "_fact_table",
        generate_test_condition_code(FactTableArraySize, FactTableName,
            Types, FactTableVars, ArgNum, have_not_seen_input_arg, CondCode),
        ( if CondCode = "" then
            TestCode = ""
        else
            TestCodeTemplate = "if (%s\t\t\t) goto failure_code_%s;\n",
            string.format(TestCodeTemplate, [s(CondCode), s(LabelName)],
                TestCode)
        )
    ;
        TestKeys = do_not_test_keys,
        TestCode = ""
    ),

    string.format(HashLookupCodeTemplate,
        [s(HashTableEntry), s(CompareString),
        s(HashTableEntry), s(LabelName), i(LabelNum),
        s(HashTableEntry), s(LabelName), s(LabelName), i(LabelNum),
        s(HashTableEntry), s(TestCode), c(KeyType),
        s(HashTableEntry), s(LabelName)],
        HashLookupCode).

%---------------------%

    % Generate code to lookup the fact table with a given index
    %
:- pred generate_fact_lookup_code(int::in, string::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    string::out) is det.

generate_fact_lookup_code(_, _, [], [], _, "").
generate_fact_lookup_code(_, _, [_ | _], [], _, _) :-
    unexpected($pred, "too many pragma vars").
generate_fact_lookup_code(_, _, [], [_ | _], _, _) :-
    unexpected($pred, "too many types").
generate_fact_lookup_code(FactTableArraySize, PredName,
        [Type | Types], [FactTableVar | FactTableVars], ArgNum, Code) :-
    FactTableVar = fact_table_var(VarName, Mode, MakeUnique, _),
    (
        Mode = fully_out,
        TableEntryTemplate = "mercury__%s_fact_table[ind/%d][ind%%%d].V_%d",
        string.format(TableEntryTemplate,
            [s(PredName), i(FactTableArraySize), i(FactTableArraySize),
                i(ArgNum)],
            TableEntry),
        (
            Type = fact_arg_type_string,
            (
                MakeUnique = do_not_make_unique,
                % Cast MR_ConstString -> MR_Word -> MR_String to avoid gcc
                % warning "assignment discards `const'".
                Template = "\t\tMR_make_aligned_string(%s, " ++
                    "(MR_String) (MR_Word) %s);\n",
                string.format(Template, [s(VarName), s(TableEntry)], ArgCode)
            ;
                MakeUnique = make_unique,
                % Unique modes need to allow destructive update,
                % so we need to make a copy of the string on the heap.
                Template =
                    "       MR_incr_hp_atomic(tmp,
                                (strlen(%s) + sizeof(MR_Word))
                                    / sizeof(MR_Word));
                            %s = (MR_String) tmp;
                            strcpy(%s, %s);
                    ",
                string.format(Template,
                    [s(TableEntry), s(VarName), s(VarName), s(TableEntry)],
                    ArgCode)
            )
        ;
            ( Type = fact_arg_type_int
            ; Type = fact_arg_type_float
            ),
            Template = "\t\t%s = %s;\n",
            string.format(Template, [s(VarName), s(TableEntry)], ArgCode)
        ),
        generate_fact_lookup_code(FactTableArraySize, PredName,
            Types, FactTableVars, ArgNum + 1, ArgsCode),
        Code = ArgCode ++ ArgsCode
    ;
        Mode = fully_in,
        % Skip non-output arguments.
        generate_fact_lookup_code(FactTableArraySize, PredName,
            Types, FactTableVars, ArgNum + 1, Code)
    ).

    % Generate code to create argument variables and assign them to registers.
    %
:- pred generate_argument_vars_code(module_info::in,
    list(fact_arg_type)::in, list(fact_table_var)::in,
    string::out, string::out, string::out,
    string::out, string::out, int::out) is det.

generate_argument_vars_code(ModuleInfo, FactArgTypes, FactTableVars,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        NumInputArgs) :-
    Types = list.map(
        ( func(FactArgType) = Type :-
            ( FactArgType = fact_arg_type_int,    Type = int_type
            ; FactArgType = fact_arg_type_float,  Type = float_type
            ; FactArgType = fact_arg_type_string, Type = string_type
            )
        ), FactArgTypes),
    Modes = list.map(
        ( func(fact_table_var(_, FactTableMode, _, _)) = Mode :-
            ( FactTableMode = fully_in,  Mode = in_mode
            ; FactTableMode = fully_out, Mode = out_mode
            )
        ), FactTableVars),
    make_standard_arg_infos(ModuleInfo, model_non, Types, Modes, ArgInfos),
    % XXX Starting counting NumInputArgs at 1 looks strange, since
    % we have not seen any input args yet. However, while the code of
    % generate_argument_vars_code_loop also refers to this arg pair
    % as !NumInputArgs, generate_arg_input_code refers to it as FrameVarNum,
    % and uses it accordingly.
    generate_argument_vars_code_loop(FactArgTypes, FactTableVars, ArgInfos,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        1, NumInputArgs).

:- pred generate_argument_vars_code_loop(
    list(fact_arg_type)::in, list(fact_table_var)::in, list(arg_info)::in,
    string::out, string::out, string::out,
    string::out, string::out, int::in, int::out) is det.

generate_argument_vars_code_loop(Types, FactTableVars, ArgInfos,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        !NumInputArgs) :-
    ( if
        Types = [],
        FactTableVars = [],
        ArgInfos = []
    then
        DeclCode = "",
        InputCode = "",
        OutputCode = "",
        SaveRegsCode = "",
        GetRegsCode = ""
    else if
        Types = [Type | TailTypes],
        FactTableVars = [FactTableVar | TailFactTableVars],
        ArgInfos = [ArgInfo | TailArgInfos]
    then
        FactTableVar = fact_table_var(VarName, Mode, _, _),
        ArgInfo = arg_info(Loc, _),
        generate_arg_decl_code(VarName, Type, ArgDeclCode),
        (
            Mode = fully_in,
            !:NumInputArgs = !.NumInputArgs + 1,
            generate_arg_input_code(VarName, Type, Loc, !.NumInputArgs,
                ArgInputCode, ArgSaveRegsCode, ArgGetRegsCode),
            ArgOutputCode = ""
        ;
            Mode = fully_out,
            generate_arg_output_code(VarName, Type, Loc, ArgOutputCode),
            ArgInputCode = "",
            ArgSaveRegsCode = "",
            ArgGetRegsCode = ""
        ),
        generate_argument_vars_code_loop(
            TailTypes, TailFactTableVars, TailArgInfos,
            ArgsDeclCode, ArgsInputCode, ArgsOutputCode, ArgsSaveRegsCode,
            ArgsGetRegsCode, !NumInputArgs),
        DeclCode = ArgDeclCode ++ ArgsDeclCode,
        InputCode = ArgInputCode ++ ArgsInputCode,
        OutputCode = ArgOutputCode ++ ArgsOutputCode,
        SaveRegsCode = ArgSaveRegsCode ++ ArgsSaveRegsCode,
        GetRegsCode = ArgGetRegsCode ++ ArgsGetRegsCode
    else
        unexpected($pred, "list length mismatch")
    ).

:- pred generate_arg_decl_code(string::in, fact_arg_type::in, string::out)
    is det.

generate_arg_decl_code(Name, Type, DeclCode) :-
    ( Type = fact_arg_type_int,    CType = "MR_Integer"
    ; Type = fact_arg_type_float,  CType = "MR_Float"
    ; Type = fact_arg_type_string, CType = "MR_String"
    ),
    string.format("\t\t%s %s;\n", [s(CType), s(Name)], DeclCode).

:- pred generate_arg_input_code(string::in, fact_arg_type::in, arg_loc::in,
    int::in, string::out, string::out, string::out) is det.

generate_arg_input_code(Name, Type, ArgLoc, FrameVarNum,
        InputCode, SaveRegCode, GetRegCode) :-
    ArgLoc = reg(RegType, RegNum),
    (
        RegType = reg_r,
        ConvertToFrameVar = "",
        ConvertFromFrameVar = ""
    ;
        RegType = reg_f,
        ConvertToFrameVar = "MR_float_to_word",
        ConvertFromFrameVar = "MR_word_to_float"
    ),
    RegNameStr = reg_to_string(RegType, RegNum),
    convert_arg_type_from_mercury(ArgLoc, RegNameStr, Type,
        ConvertedRegNameStr),
    string.format("\t\t%s = %s;\n",
        [s(Name), s(ConvertedRegNameStr)], InputCode),
    string.format("\t\tMR_framevar(%d) = %s(%s);\n",
        [i(FrameVarNum), s(ConvertToFrameVar), s(RegNameStr)], SaveRegCode),
    string.format("\t\t%s = %s(MR_framevar(%d));\n",
        [s(RegNameStr), s(ConvertFromFrameVar), i(FrameVarNum)], GetRegCode).

:- pred generate_arg_output_code(string::in, fact_arg_type::in, arg_loc::in,
    string::out) is det.

generate_arg_output_code(Name, Type, ArgLoc, OutputCode) :-
    ArgLoc = reg(RegType, RegNum),
    RegName = reg_to_string(RegType, RegNum),
    convert_arg_type_to_mercury(Name, Type, ArgLoc, ConvertedName),
    Template = "\t\t%s = %s;\n",
    string.format(Template, [s(RegName), s(ConvertedName)], OutputCode).

%---------------------%

:- pred convert_arg_type_to_mercury(string::in, fact_arg_type::in, arg_loc::in,
    string::out) is det.

convert_arg_type_to_mercury(RvalStr, Type, TargetArgLoc, ConvertedRvalStr) :-
    % This code is a version of convert_type_to_mercury that has been cut down
    % to handle only fact_arg_types.
    (
        Type = fact_arg_type_int,
        ConvertedRvalStr = RvalStr
    ;
        Type = fact_arg_type_float,
        (
            TargetArgLoc = reg(reg_r, _),
            ConvertedRvalStr = "MR_float_to_word(" ++ RvalStr ++ ")"
        ;
            TargetArgLoc = reg(reg_f, _),
            ConvertedRvalStr = RvalStr
        )
    ;
        Type = fact_arg_type_string,
        ConvertedRvalStr = "(MR_Word) " ++ RvalStr
    ).

:- pred convert_arg_type_from_mercury(arg_loc::in, string::in,
    fact_arg_type::in, string::out) is det.

convert_arg_type_from_mercury(SourceArgLoc, RvalStr, Type, ConvertedRvalStr) :-
    % This code is a version of convert_type_from_mercury
    % cut down to handle only fact_arg_types.
    (
        Type = fact_arg_type_int,
        ConvertedRvalStr = RvalStr
    ;
        Type = fact_arg_type_float,
        (
            SourceArgLoc = reg(reg_r, _),
            ConvertedRvalStr = "MR_word_to_float(" ++ RvalStr ++ ")"
        ;
            SourceArgLoc = reg(reg_f, _),
            ConvertedRvalStr = RvalStr
        )
    ;
        Type = fact_arg_type_string,
        ConvertedRvalStr = "(MR_String) " ++ RvalStr
    ).

%---------------------%

    % Generate code to test that the fact found matches the input arguments.
    % This is only required for generate_primary_nondet_code. Other procedures
    % can test the key in the hash table against the input arguments.
    %
:- pred generate_fact_test_code(int::in, string::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, string::out) is det.

generate_fact_test_code(FactTableArraySize, PredName,
        Types, FactTableVars, FactTestCode) :-
    FactTableName = "mercury__" ++ PredName ++ "_fact_table",
    generate_test_condition_code(FactTableArraySize, FactTableName,
        Types, FactTableVars, 1, have_not_seen_input_arg, CondCode),
    FactTestCode = "\t\tif(" ++ CondCode ++ "\t\t) MR_fail();\n".

:- type maybe_seen_input_arg
    --->    have_seen_input_arg
    ;       have_not_seen_input_arg.

:- pred generate_test_condition_code(int::in, string::in,
    list(fact_arg_type)::in, list(fact_table_var)::in, int::in,
    maybe_seen_input_arg::in, string::out) is det.

generate_test_condition_code(_, _, [], [], _, _, "").
generate_test_condition_code(_, _, [_ | _], [], _, _, "") :-
    unexpected($pred, "too many PragmaVars").
generate_test_condition_code(_, _, [], [_ | _], _, _, "") :-
    unexpected($pred, "too many ArgTypes").
generate_test_condition_code(FactTableArraySize, FactTableName,
        [Type | Types], [FactTableVar | FactTableVars], ArgNum,
        !.IsFirstInputArg, CondCode) :-
    FactTableVar = fact_table_var(Name, Mode, _, _),
    (
        Mode = fully_in,
        (
            Type = fact_arg_type_string,
            Template = "strcmp(%s[ind/%d][ind%%%d].V_%d, %s) != 0\n",
            string.format(Template, [s(FactTableName), i(FactTableArraySize),
                i(FactTableArraySize), i(ArgNum), s(Name)], ArgCondCode0)
        ;
            ( Type = fact_arg_type_int
            ; Type = fact_arg_type_float
            ),
            Template = "%s[ind/%d][ind%%%d].V_%d != %s\n",
            string.format(Template, [s(FactTableName), i(FactTableArraySize),
                i(FactTableArraySize), i(ArgNum), s(Name)], ArgCondCode0)
        ),
        (
            !.IsFirstInputArg = have_seen_input_arg,
            ArgCondCode = "\t\t|| " ++ ArgCondCode0
        ;
            !.IsFirstInputArg = have_not_seen_input_arg,
            ArgCondCode = ArgCondCode0
        ),
        !:IsFirstInputArg = have_seen_input_arg
    ;
        Mode = fully_out,
        ArgCondCode = ""
    ),
    generate_test_condition_code(FactTableArraySize, FactTableName,
        Types, FactTableVars, ArgNum + 1, !.IsFirstInputArg, ArgsCondCode),
    CondCode = ArgCondCode ++ ArgsCondCode.

%---------------------------------------------------------------------------%

    % fact_table_vars_to_names_string(PragmaVars, NamesString):
    %
    % Create a string containing the names of the vars separated by commas.
    %
:- pred fact_table_vars_to_names_string(list(fact_table_var)::in,
    string::out) is det.

fact_table_vars_to_names_string([], "").
fact_table_vars_to_names_string([FactTableVar  | FactTableVars],
        NamesString) :-
    fact_table_vars_to_names_string(FactTableVars, NamesStringTail),
    FactTableVar = fact_table_var(Name, _, _, _),
    NamesString = Name ++ ", " ++ NamesStringTail.

%---------------------------------------------------------------------------%
:- end_module ll_backend.fact_table_gen.
%---------------------------------------------------------------------------%
