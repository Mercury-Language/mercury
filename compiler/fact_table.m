%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: fact_table.m.
% Main author: dmo.
%
% This module handles compilation of fact tables contained in external files
% that have been declared with a `pragma fact_table' declaration.
%
% The facts are processed one by one. Each fact is read in and type and mode
% checked. If there are no modes with input arguments, the data is written
% out to arrays of C structures as each fact is processed. If there are input
% modes, the input arguments for each mode are written out to a temporary
% sort file -- one sort file per input mode. The output arguments are also
% included in the sort file for the primary input mode. (At the moment,
% the primary input mode is the one with the lowest ProcID number, however
% this may change in the future to select the mode that is likely to give
% the biggest increase in efficiency by being the primary mode).
%
% After all the facts have been read, the sort files are sorted by the Unix
% `sort' program. They are then scanned for duplicate input keys to infer
% the determinisms of each mode.
%
% The sort files are then read back in one by one and hash tables are created
% for each input mode. While the sort file for the primary input mode is
% being read, the output arguments are also read back in and output as C
% arrays in another temporary file. (This file is concatenated to the end
% of the fact table C file after all the hash tables have been created.)
% This means that the output data for identical keys in the primary input
% mode will be grouped together allowing the code that accesses this mode
% to be just pick the next item in the data array when backtracking.
%
% The inferred determinism for each mode is added to the proc_info. If a
% determinism has been declared for the procedure, it will be tested against
% the inferred determinism later on in det_report.m.
%
% XXX All combinations of `in' and `out' arguments are now supported for all
% determinisms. Only the builtin `string', `int' and `float' types are
% supported at the moment.
%
% XXX Cross compilation is not supported for fact tables that are indexed on
% floats.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.fact_table.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % fact_table_compile_facts(PredName, Arity, FileName, PredInfo0,
    %   PredInfo, Context, ModuleInfo, C_HeaderCode, PrimaryProcID):
    %
    % Compile the fact table into a separate .c file.
    %
:- pred fact_table_compile_facts(sym_name::in, arity::in, string::in,
    pred_info::in, pred_info::out, prog_context::in, module_info::in,
    string::out, proc_id::out, io::di, io::uo) is det.

    % fact_table_generate_c_code(PredName, PragmaVars, ProcID,
    %   PrimaryProcID, ProcInfo, ArgTypes, C_ProcCode, C_ExtraCode):
    %
    % Generate c code to lookup a fact table in a given mode. C_ProcCode is the
    % C code for the procedure, C_ExtraCode is extra C code that should be
    % included in the module.
    %
    % Model_non foreign_procs were not supported by the compiler when this
    % code was written. To get around this, the C_ProcCode generated for
    % model_non code pops off the stack frame that is automatically created
    % by the compiler and jumps to the code contained in C_ExtraCode.
    % C_ExtraCode declares the required labels and creates a new stack frame
    % with the required number of framevars. It then does all the work required
    % to look up the fact table.
    %
:- pred fact_table_generate_c_code(sym_name::in, list(pragma_var)::in,
    proc_id::in, proc_id::in, proc_info::in, list(mer_type)::in,
    module_info::in, string::out, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.export.
:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module parser.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.

%-----------------------------------------------------------------------------%

:- type fact_result
    --->    ok
    ;       error.

    % Proc_stream contains information about an open sort file for
    % a particular procedure.
    %
:- type proc_stream
    --->    proc_stream(
                proc_id,            % ID of procedure.
                io.output_stream    % Sort file stream.
            ).

:- type hash_entry
    --->    hash_entry(
                fact_arg,   % Lookup key.
                hash_index, % Pointer to next hash table or index to fact data.
                int         % Position of next entry with same hash value.
            ).

    % Data structure used to build up a hash table before writing it out
    % as a C array.
:- type hash_table
    --->    hash_table(
                int,                    % Size of hash table.
                map(int, hash_entry)
            ).

:- type hash_index
    --->    fact(int)                   % Index into fact table.
    ;       hash_table(int, string).    % Hash table for next arg.

:- type fact_arg == const.

    % Sort_file_line contains the information read in from a sort file
    % after sorting.
:- type sort_file_line
    --->    sort_file_line(
                list(fact_arg),     % Input arguments.
                int,                % Index of fact in original file.
                list(fact_arg)      % Output arguments.
            ).

:- type fact_table_mode_type
    --->    all_in      % Modes of all arguments are input.
    ;       all_out     % Modes of all arguments are output.
    ;       in_out      % Modes are a mixture of input and output.
    ;       other       % Some arguments have modes that are not in or out.
    ;       unknown.

:- type inferred_determinism
    --->    inferred(determinism)   % Determinism has been inferred.
    ;       not_yet                 % Determinism has not yet been inferred.
    ;       error.                  % An error occurred trying to infer
                                    % determinism.

:- type fact_arg_info
    --->    fact_arg_info(
                mer_type,   % Type of the argument.
                bool,       % Is an input argument for some mode.
                bool        % Is an output argument for some mode.
            ).

    % fact_table_size(Globals, FactTableSize):
    %
    % FactTableSize is the maximum size of each array in the fact data table.
    % GCC doesn't cope very well with huge arrays, so we break the fact data
    % table into a number of smaller arrays, each with a maximum size given
    % by FactTableSize, and create an array of pointers to these arrays
    % to access the data. The size should be a power of 2 to make the
    % generated code more efficient.
    %
:- pred fact_table_size(globals::in, int::out) is det.

fact_table_size(Globals, FactTableSize) :-
    globals.lookup_int_option(Globals, fact_table_max_array_size,
        FactTableSize).

%---------------------------------------------------------------------------%

fact_table_compile_facts(PredName, Arity, FileName, !PredInfo, Context,
        ModuleInfo, C_HeaderCode, PrimaryProcID, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    io.see(FileName, SeeResult, !IO),
    (
        SeeResult = ok,
        module_info_get_name(ModuleInfo, ModuleName),
        fact_table_file_name(Globals, ModuleName, FileName, ".c",
            do_create_dirs, OutputFileName, !IO),
        io.open_output(OutputFileName, OpenResult, !IO),
        (
            OpenResult = ok(OutputStream),
            fact_table_compile_facts_2(PredName, Arity, FileName, !PredInfo,
                Context, ModuleInfo, C_HeaderCode, PrimaryProcID,
                OutputFileName, OutputStream, !IO)
        ;
            OpenResult = error(Error),
            print_file_open_error(Globals, yes(Context), FileName, "output",
                Error, !IO),
            C_HeaderCode = "",
            PrimaryProcID = invalid_proc_id
        )
    ;
        SeeResult = error(Error),
        print_file_open_error(Globals, yes(Context), FileName, "input",
            Error, !IO),
        C_HeaderCode = "",
        PrimaryProcID = invalid_proc_id
    ).

:- pred fact_table_compile_facts_2(sym_name::in, arity::in, string::in,
    pred_info::in, pred_info::out, prog_context::in, module_info::in,
    string::out, proc_id::out, string::in, io.output_stream::in,
    io::di, io::uo) is det.

fact_table_compile_facts_2(PredName, Arity, FileName, !PredInfo, Context,
        ModuleInfo, C_HeaderCode, PrimaryProcID, OutputFileName, OutputStream,
        !IO) :-
    pred_info_get_arg_types(!.PredInfo, Types),
    init_fact_arg_infos(Types, FactArgInfos0),
    infer_determinism_pass_1(!PredInfo, Context, ModuleInfo, CheckProcs,
        ExistsAllInMode, WriteHashTables, WriteDataTable,
        FactArgInfos0, FactArgInfos, [], Pass1Errors),
    create_fact_table_header(PredName, !.PredInfo, FactArgInfos,
        C_HeaderCode0, StructName, Pass1Errors, Pass1HeaderErrors),
    module_info_get_globals(ModuleInfo, Globals),
    (
        Pass1HeaderErrors = [],
        io.write_string(OutputStream, fact_table_file_header(FileName), !IO),
        io.write_string(OutputStream, C_HeaderCode0, !IO),
        open_sort_files(CheckProcs, ProcStreams, [], OpenErrors, !IO),
        (
            WriteDataTable = yes,
            (
                CheckProcs = [],
                MaybeOutput = yes(OutputStream - StructName),
                % Outputs opening brace for first fact array.
                write_new_data_array(OutputStream, StructName, 0, !IO),
                WriteDataAfterSorting = no
            ;
                CheckProcs = [_ | _],
                MaybeOutput = no,
                WriteDataAfterSorting = yes
            )
        ;
            WriteDataTable = no,
            MaybeOutput = no,
            WriteDataAfterSorting = no
        ),
        compile_facts(PredName, Arity, !.PredInfo, ModuleInfo, FactArgInfos,
            ProcStreams, MaybeOutput, 0, NumFacts, [], CompileErrors, !IO),
        io.seen(!IO),
        (
            MaybeOutput = yes(_),
            % Outputs closing brace for last fact array.
            write_closing_brace(OutputStream, !IO),
            fact_table_size(Globals, FactTableSize),
            write_fact_table_pointer_array(NumFacts, FactTableSize,
                StructName, OutputStream, C_HeaderCode2, !IO)
        ;
            MaybeOutput = no,
            C_HeaderCode2 = ""

        ),
        close_sort_files(ProcStreams, ProcFiles, !IO),
        OpenCompileErrors = OpenErrors ++ CompileErrors,
        (
            OpenCompileErrors = [],
            pred_info_get_proc_table(!.PredInfo, ProcTable0),
            infer_determinism_pass_2(ProcFiles, Globals, ExistsAllInMode,
                ProcTable0, ProcTable, !IO),
            pred_info_set_proc_table(ProcTable, !PredInfo),
            io.make_temp_file(DataFileNameResult, !IO),
            (
                DataFileNameResult = ok(DataFileName),
                write_fact_table_arrays(ProcFiles, DataFileName, StructName,
                    ProcTable, ModuleInfo, NumFacts, FactArgInfos,
                    WriteHashTables, WriteDataAfterSorting, OutputStream,
                    C_HeaderCode1, PrimaryProcID, !IO),
                write_fact_table_numfacts(OutputStream, PredName, NumFacts,
                    C_HeaderCode3, !IO),
                string.append_list([C_HeaderCode0, C_HeaderCode1,
                    C_HeaderCode2, C_HeaderCode3], C_HeaderCode)
            ;
                DataFileNameResult = error(Error),
                ErrorReport = no - [
                    words("Could not create temporary file:"),
                    quote(error_message(Error)), nl],
                print_error_report(Globals, ErrorReport, !IO),
                C_HeaderCode = C_HeaderCode0,
                PrimaryProcID = invalid_proc_id,
                DataFileName = ""
            )
        ;
            OpenCompileErrors = [_ | _],
            print_error_reports(Globals, OpenCompileErrors, !IO),
            C_HeaderCode = C_HeaderCode0,
            PrimaryProcID = invalid_proc_id,
            DataFileName = ""
        )
    ;
        Pass1HeaderErrors = [_ | _],
        % Either there are no modes declared for this fact table or the
        % `:- pred' or `:- func' declaration had some types that are not
        % supported in fact tables so there is no point trying to type-check
        % all the facts.
        print_error_reports(Globals, Pass1HeaderErrors, !IO),
        C_HeaderCode = C_HeaderCode0,
        PrimaryProcID = invalid_proc_id,
        WriteDataAfterSorting = no,
        DataFileName = ""
    ),
    io.close_output(OutputStream, !IO),
    maybe_append_data_table(Globals, WriteDataAfterSorting, OutputFileName,
        DataFileName, !IO).

%---------------------------------------------------------------------------%

    % Read in facts one by one and check and compile them.
    %
:- pred compile_facts(sym_name::in, arity::in, pred_info::in, module_info::in,
    list(fact_arg_info)::in, list(proc_stream)::in,
    maybe(pair(io.output_stream, string))::in, int::in, int::out,
    error_reports::in, error_reports::out, io::di, io::uo) is det.

compile_facts(PredName, Arity, PredInfo, ModuleInfo, FactArgInfos, ProcStreams,
        MaybeOutput, !NumFacts, !Errors, !IO) :-
    parser.read_term(Result0, !IO),
    (
        Result0 = eof
    ;
        Result0 = error(Message, LineNum),
        io.input_stream_name(FileName, !IO),
        term.context_init(FileName, LineNum, Context),
        add_error_report(Context, [words(Message)], !Errors)
    ;
        Result0 = term(_VarSet, Term),
        module_info_get_globals(ModuleInfo, Globals),
        fact_table_size(Globals, FactTableSize),
        ( if 0 = !.NumFacts mod FactTableSize then
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            (
                VeryVerbose = yes,
                io.format("%% Read fact %d\n", [i(!.NumFacts)], !IO)
            ;
                VeryVerbose = no
            )
        else
            true
        ),

        check_fact_term(PredName, Arity, PredInfo, ModuleInfo, Term,
            FactArgInfos, ProcStreams, MaybeOutput, !.NumFacts, Result,
            !Errors, !IO),
        (
            Result = ok,
            !:NumFacts = !.NumFacts + 1
        ;
            Result = error
        ),
        compile_facts(PredName, Arity, PredInfo, ModuleInfo, FactArgInfos,
            ProcStreams, MaybeOutput, !NumFacts, !Errors, !IO)
    ).

    % Do syntactic and semantic checks on a fact term.
    %
:- pred check_fact_term(sym_name::in, arity::in, pred_info::in,
    module_info::in, prog_term::in, list(fact_arg_info)::in,
    list(proc_stream)::in, maybe(pair(io.output_stream, string))::in,
    int::in, fact_result::out, error_reports::in, error_reports::out,
    io::di, io::uo) is det.

check_fact_term(_, _, _, _, term.variable(_V, _), _, _, _, _, error,
        !Errors, !IO) :-
    io.get_line_number(LineNum, !IO),
    io.input_stream_name(FileName, !IO),
    Context = term.context(FileName, LineNum),
    Msg = "Error: term is not a fact.",
    add_error_report(Context, [words(Msg)], !Errors).
check_fact_term(PredName, Arity0, PredInfo, ModuleInfo,
        term.functor(Const, Terms0, Context), FactArgInfos,
        ProcStreams, MaybeOutput, FactNum, Result, !Errors, !IO) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredString = unqualify_name(PredName),
    ( if Const = term.atom(TopLevel) then
        ( if
            (
                PredOrFunc = pf_predicate,
                TopLevel = PredString,
                Terms = Terms0,
                Arity = Arity0
            ;
                PredOrFunc = pf_function,
                TopLevel = "=",
                Terms0 = [FuncHeadTerm, FuncResultTerm],
                FuncHeadTerm = term.functor(term.atom(PredString), Terms1, _),
                list.append(Terms1, [FuncResultTerm], Terms),
                Arity = Arity0 + 1
            )
        then
            check_fact_term_2(PredOrFunc, Arity, PredInfo, ModuleInfo, Terms,
                Context, FactArgInfos, ProcStreams, MaybeOutput, FactNum,
                Result, !Errors, !IO)
        else
            PFStr = pred_or_func_to_full_str(PredOrFunc),
            string.format("Error: invalid clause for %s `%s/%d'.",
                [s(PFStr), s(PredString), i(Arity0)], Msg),
            add_error_report(Context, [words(Msg)], !Errors),
            Result = error
        )
    else
        add_error_report(Context,
            [words("Error: term is not a fact.")], !Errors),
        Result = error
    ).

:- pred check_fact_term_2(pred_or_func::in, arity::in, pred_info::in,
    module_info::in, list(prog_term)::in, context::in, list(fact_arg_info)::in,
    list(proc_stream)::in, maybe(pair(io.output_stream, string))::in,
    int::in, fact_result::out, error_reports::in, error_reports::out,
    io::di, io::uo) is det.

check_fact_term_2(PredOrFunc, Arity, PredInfo, ModuleInfo, Terms, Context,
        FactArgInfos, ProcStreams, MaybeOutput, FactNum, Result,
        !Errors, !IO) :-
    % Check that arity of the fact is correct.
    list.length(Terms, Len),
    ( if Len = Arity then
        pred_info_get_arg_types(PredInfo, Types),
        check_fact_type_and_mode(Types, Terms, 0, PredOrFunc, Context,
            Result, !Errors),
        pred_info_get_proc_table(PredInfo, ProcTable),
        string.int_to_string(FactNum, FactNumStr),
        write_sort_file_lines(ProcStreams, ProcTable, Terms,
            ModuleInfo, FactNumStr, FactArgInfos, yes, !IO),

        % If there are no in_out modes to the predicate, we need to write out
        % the facts at this point. If there are input modes, the facts are
        % written out later on after being sorted on the first input mode.
        ( if
            MaybeOutput = yes(OutputStream - StructName),
            TermToArg = (
                pred(Term::in, FactArg::out) is semidet :-
                    Term = term.functor(FactArg, _, _)
            ),
            list.map(TermToArg, Terms, FactArgs)
        then
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            fact_table_size(Globals, FactTableSize),
            write_fact_data(VeryVerbose, FactNum, FactTableSize, FactArgs,
                StructName, OutputStream, !IO)
        else
            % If list.map above fails, don't do anything here. The error will
            % have already been reported in check_fact_type_and_mode.
            true
        )
    else
        Msg1 = "Error: fact has wrong number of arguments.",
        string.format("Expecting %d arguments, but fact has %d arguments.",
            [i(Arity), i(Len)], Msg2),
        add_error_report(Context, [words(Msg1), words(Msg2)], !Errors),
        Result = error
    ).

    % Check that the mode of the fact is correct. All terms must be ground
    % and be a constant of the correct type. Only string, int and float are
    % supported at the moment.
    %
:- pred check_fact_type_and_mode(list(mer_type)::in, list(prog_term)::in,
    int::in, pred_or_func::in, prog_context::in, fact_result::out,
    error_reports::in, error_reports::out) is det.

check_fact_type_and_mode(_, [], _, _, _, ok, !Errors).
check_fact_type_and_mode(Types0, [Term | Terms], ArgNum0, PredOrFunc,
        Context0, Result, !Errors) :-
    ArgNum = ArgNum0 + 1,
    (
        Term = term.variable(_, _),
        Msg = "Error: non-ground term in fact.",
        add_error_report(Context0, [words(Msg)], !Errors),
        Result = error
    ;
        Term = term.functor(Functor, Items, Context),
        % We know that string, integer and float constants are
        % ground, but we still need to check that they are
        % the right type for this argument.
        (
            Functor = term.string(_),
            RequiredType = yes(builtin_type_string)
        ;
            Functor = term.integer(_, _, Signedness, _),
            (
                Signedness = signed,
                RequiredType = yes(builtin_type_int)
            ;
                Signedness = unsigned,
                RequiredType = yes(builtin_type_uint)
            )
        ;
            Functor = term.float(_),
            RequiredType = yes(builtin_type_float)
        ;
            Functor = term.atom(_),
            RequiredType = no
        ;
            Functor = term.implementation_defined(_),
            unexpected($module, $pred, "implementation-defined literal")
        ),
        (
            RequiredType = no,
            (
                Items = [_ | _],
                Msg = "Error: compound types are not supported in fact tables."
            ;
                Items = [],
                Msg = "Error: enumeration types are not " ++
                    "yet supported in fact tables."
            ),
            add_error_report(Context, [words(Msg)], !Errors),
            Result = error
        ;
            RequiredType = yes(BuiltinType),
            ( if
                Types0 = [Type | Types],
                Type = builtin_type(BuiltinType)
            then
                check_fact_type_and_mode(Types, Terms, ArgNum,
                    PredOrFunc, Context0, Result, !Errors)
            else
                report_type_error(Context, ArgNum, Terms, PredOrFunc, !Errors),
                Result = error
            )
        )
    ).

:- pred report_type_error(prog_context::in, int::in, list(prog_term)::in,
    pred_or_func::in, error_reports::in, error_reports::out) is det.

report_type_error(Context, ArgNum, RemainingTerms, PredOrFunc, !Errors) :-
    ( if
        % Report a different error message for the return value of a function.
        PredOrFunc = pf_function,
        RemainingTerms = []
    then
        Msg = "Type error in return value of function."
    else
        string.format("Type error in argument %d.", [i(ArgNum)], Msg)
    ),
    add_error_report(Context, [words(Msg)], !Errors).

%---------------------------------------------------------------------------%

:- func fact_table_file_header(string) = string.

fact_table_file_header(FileName) = FileHeader :-
    library.version(Version, Fullarch),
    string.append_list(
        ["/*\n",
        "** Automatically generated from `", FileName, "'\n",
        "** by the Mercury compiler, version ", Version, ",\n",
        "** configured for ", Fullarch, ".\n",
        "** Do not edit.\n",
        "*/\n",
        "\n",
        "#include ""mercury_imp.h""\n\n"],
        FileHeader).

:- pred create_fact_table_header(sym_name::in, pred_info::in,
    list(fact_arg_info)::in, string::out, string::out,
    error_reports::in, error_reports::out) is det.

create_fact_table_header(PredName, PredInfo, FactArgInfos,
        C_HeaderCode, StructName, !Errors) :-

    make_fact_table_identifier(PredName, Identifier),
    StructName = "mercury__" ++ Identifier ++ "_fact_table",

    % Define a struct for a fact table entry.
    pred_info_get_context(PredInfo, Context),  % location of :- pred decl
    create_fact_table_struct(FactArgInfos, 1, Context, StructContents,
        !Errors),
    ( if StructContents = "" then
        StructDef = ""
    else
        StructDef = "struct " ++ StructName ++ "_struct {\n"
            ++ StructContents ++ "};\n\n"
    ),
    HashDef = hash_def,
    string.append(StructDef, HashDef, C_HeaderCode).

    % Define a struct for a hash table entry.
    %
:- func hash_def = string.

hash_def = "
#ifndef MERCURY_FACT_TABLE_HASH_TABLES
#define MERCURY_FACT_TABLE_HASH_TABLES

struct MR_fact_table_hash_table_s {
    MR_Integer size;                            /* size of the hash table */
    struct MR_fact_table_hash_entry_s *table;   /* the actual table */
};

struct MR_fact_table_hash_table_f {
    MR_Integer size;                            /* size of the hash table */
    struct MR_fact_table_hash_entry_f *table;   /* the actual table */
};

struct MR_fact_table_hash_table_i {
    MR_Integer size;                            /* size of the hash table */
    struct MR_fact_table_hash_entry_i *table;   /* the actual table */
};

/* hash table for string keys */
struct MR_fact_table_hash_entry_s {
    MR_ConstString  key;        /* lookup key */
    const MR_Word   *index;     /* index into fact table data array or       */
                                /* pointer to hash table for next argument   */
#if TAGBITS < 2
    short type;                 /* 0 if entry empty,                         */
                                /* 1 if entry is a pointer to the data table */
                                /* 2 if entry is a pointer to another        */
                                /*   hash table                              */
#endif
    int next;                   /* location of next entry with the same hash */
                                /* value */
};

/* hash table for float keys */
struct MR_fact_table_hash_entry_f {
    MR_Float        key;
    const MR_Word   *index;
#if TAGBITS < 2
    short           type;
#endif
    int             next;
};

/* hash table for int keys */
struct MR_fact_table_hash_entry_i {
    MR_Integer      key;
    const MR_Word   *index;
#if TAGBITS < 2
    short           type;
#endif
    int             next;
};

#if TAGBITS >= 2
    #define MR_FACT_TABLE_MAKE_TAGGED_INDEX(i, t)   \
        MR_mkword(MR_mktag(t), MR_mkbody(i))
    #define MR_FACT_TABLE_MAKE_TAGGED_POINTER(p, t) \
        MR_mkword(MR_mktag(t), p)
    #define MR_FACT_TABLE_HASH_ENTRY_TYPE(p)        \
        MR_tag((MR_Word)((p).index))
    #define MR_FACT_TABLE_HASH_INDEX(w)             \
        MR_unmkbody(w)
    #define MR_FACT_TABLE_HASH_POINTER(w)           \
        MR_body(w, MR_tag(w))
#else
    #define MR_FACT_TABLE_MAKE_TAGGED_INDEX(i, t)   \
        ((const MR_Word *) i), (t)
    #define MR_FACT_TABLE_MAKE_TAGGED_POINTER(p, t) \
        ((const MR_Word *) p), (t)
    #define MR_FACT_TABLE_HASH_ENTRY_TYPE(p)       ((p).type)
    #define MR_FACT_TABLE_HASH_INDEX(w)            (w)
    #define MR_FACT_TABLE_HASH_POINTER(w)          (w)
#endif

#endif /* not MERCURY_FACT_TABLE_HASH_TABLES */
".

    % Create a struct for the fact table consisting of any arguments
    % that are output in some mode. Also ensure that are arguments are
    % either string, float or int.
    %
:- pred create_fact_table_struct(list(fact_arg_info)::in, int::in,
    prog_context::in, string::out,
    error_reports::in, error_reports::out) is det.

create_fact_table_struct([], _, _, "", !Errors).
create_fact_table_struct([Info | Infos], I, Context, StructContents,
        !Errors) :-
    create_fact_table_struct(Infos, I + 1, Context, StructContentsTail,
        !Errors),
    Info = fact_arg_info(Type, _IsInput, IsOutput),
    ( if
        (
            Type = builtin_type(builtin_type_string),
            TypeStr = "MR_ConstString"
        ;
            Type = builtin_type(builtin_type_int),
            TypeStr = "MR_Integer"
        ;
            Type = builtin_type(builtin_type_float),
            TypeStr = "MR_Float"
        )
    then
        (
            IsOutput = yes,
            string.format("\t%s V_%d;\n", [s(TypeStr), i(I)], StructField),
            string.append(StructField, StructContentsTail, StructContents)
        ;
            IsOutput = no,
            StructContents = StructContentsTail
        )
    else
        % Report an error for types other than string, int and float.
        % Context is the `:- pred' or `:- func' declaration where the
        % types are declared.
        Msg = "Error: invalid type in fact table: " ++
            "only `string', `int' and `float' types " ++
            "are allowed in fact tables.",
        add_error_report(Context, [words(Msg)], !Errors),
        StructContents = StructContentsTail
    ).

%---------------------------------------------------------------------------%

    % Initialise list of fact argument information. Input and output flags
    % are initialised to `no' and filled in correctly by
    % infer_determinism_pass_1.
    %
:- pred init_fact_arg_infos(list(mer_type)::in, list(fact_arg_info)::out)
    is det.

init_fact_arg_infos([], []).
init_fact_arg_infos([Type | Types], [Info | Infos]) :-
    Info = fact_arg_info(Type, no, no),
    init_fact_arg_infos(Types, Infos).

:- pred fill_in_fact_arg_infos(list(mer_mode)::in, module_info::in,
    list(fact_arg_info)::in, list(fact_arg_info)::out) is det.

fill_in_fact_arg_infos([], _, [], []).
fill_in_fact_arg_infos([_ | _], _, [], _) :-
    unexpected($module, $pred, "too many argmodes").
fill_in_fact_arg_infos([], _, [_ | _], _) :-
    unexpected($module, $pred, "too many fact_arg_infos").
fill_in_fact_arg_infos([Mode | Modes], ModuleInfo, [Info0 | Infos0],
        [Info | Infos]) :-
    Info0 = fact_arg_info(Type, IsInput, _IsOutput),
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        % XXX Info = fact_arg_info(Type, yes, IsOutput)

        % XXX currently the first input mode requires _all_ arguments to be
        % written in the fact data table so it can do lookups on backtracking.
        % This may change if it is found to be less efficient than doing these
        % lookups via the hash table.
        Info = fact_arg_info(Type, yes, yes)
    else if mode_is_fully_output(ModuleInfo, Mode) then
        Info = fact_arg_info(Type, IsInput, yes)
    else
        % This is a mode error that will be reported by
        % infer_proc_determinism_pass_1.
        Info = Info0
    ),
    fill_in_fact_arg_infos(Modes, ModuleInfo, Infos0, Infos).

%---------------------------------------------------------------------------%

    % First pass of determinism inference. (out, out, ..., out) procs are
    % multi and (in, in, .., in) procs are semidet. Return a list of procs
    % containing both in's and out's. These need further analysis later
    % in pass 2.
    %
:- pred infer_determinism_pass_1(pred_info::in, pred_info::out,
    prog_context::in, module_info::in, list(proc_id)::out,
    bool::out, bool::out, bool::out,
    list(fact_arg_info)::in, list(fact_arg_info)::out,
    error_reports::in, error_reports::out) is det.

infer_determinism_pass_1(!PredInfo, Context, ModuleInfo, CheckProcs,
        ExistsAllInMode, WriteHashTables, WriteDataTable,
        !FactArgInfos, !Errors) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    ProcIDs = pred_info_procids(!.PredInfo),
    (
        ProcIDs = [],
        % There are no declared modes, so report an error.
        PredString = pred_info_name(!.PredInfo),
        Arity = pred_info_orig_arity(!.PredInfo),
        string.format("Error: no modes declared for fact table `%s/%d'.\n",
            [s(PredString), i(Arity)], Msg),
        add_error_report(Context, [words(Msg)], !Errors),
        CheckProcs = [],
        ExistsAllInMode = no,
        WriteHashTables = no,
        WriteDataTable = no
    ;
        ProcIDs = [_ | _],
        infer_proc_determinism_pass_1(ProcIDs, ModuleInfo,
            ProcTable0, ProcTable, [], CheckProcs0, !FactArgInfos,
            MaybeAllInProc, WriteHashTables, WriteDataTable, !Errors),

        % If there is an all_in procedure, it needs to be put on the end of
        % the list so a sort file is created for it. This is required when
        % building the hash table, not for determinism inference.
        (
            MaybeAllInProc = yes(ProcID),
            CheckProcs1 = [ProcID | CheckProcs0],
            ExistsAllInMode = yes
        ;
            MaybeAllInProc = no,
            CheckProcs1 = CheckProcs0,
            ExistsAllInMode = no
        ),

        % We need to get the order right for CheckProcs because the first
        % procedure in list is used to derive the primary lookup key.
        list.reverse(CheckProcs1, CheckProcs),
        pred_info_set_proc_table(ProcTable, !PredInfo)
    ).

:- pred infer_proc_determinism_pass_1(list(proc_id)::in, module_info::in,
    proc_table::in, proc_table::out, list(proc_id)::in, list(proc_id)::out,
    list(fact_arg_info)::in, list(fact_arg_info)::out,
    maybe(proc_id)::out, bool::out, bool::out,
    error_reports::in, error_reports::out) is det.

infer_proc_determinism_pass_1([], _, !ProcTable, !CheckProcs, !FactArgInfos,
        no, no, no, !Errors).
infer_proc_determinism_pass_1([ProcID | ProcIDs], ModuleInfo, !ProcTable,
        !CheckProcs, !FactArgInfos, MaybeAllInProc, WriteHashTables,
        WriteDataTable, !Errors) :-
    map.lookup(!.ProcTable, ProcID, ProcInfo0),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    fill_in_fact_arg_infos(ArgModes, ModuleInfo, !FactArgInfos),
    fact_table_mode_type(ArgModes, ModuleInfo, ModeType),
    (
        ModeType = all_in,
        InferredDetism = inferred(detism_semi),
        WriteHashTables0 = yes,
        WriteDataTable0 = no,
        MaybeAllInProc0 = yes(ProcID)
    ;
        ModeType = all_out,
        proc_info_get_declared_determinism(ProcInfo0, MaybeDet),
        ( if
            (
                MaybeDet = yes(detism_cc_multi)
            ;
                MaybeDet = yes(detism_cc_non)
            )
        then
            InferredDetism = inferred(detism_cc_multi)
        else
            InferredDetism = inferred(detism_multi)
        ),
        WriteHashTables0 = no,
        WriteDataTable0 = yes,
        MaybeAllInProc0 = no
    ;
        ModeType = in_out,

        % Don't have enough info to infer determinism yet.
        % Put it off till the second pass.
        InferredDetism = not_yet,
        % Add to list and check in pass 2.
        !:CheckProcs = [ProcID | !.CheckProcs],
        WriteHashTables0 = yes,
        WriteDataTable0 = yes,
        MaybeAllInProc0 = no
    ;
        ModeType = other,           % mode error
        InferredDetism = error,
        proc_info_get_context(ProcInfo0, Context),
        Msg = "Error: only `in' and `out' modes are currently " ++
            "supported in fact tables.",
        add_error_report(Context, [words(Msg)], !Errors),
        WriteHashTables0 = no,
        WriteDataTable0 = no,
        MaybeAllInProc0 = no
    ;
        ModeType = unknown,         % mode error
        InferredDetism = error,
        proc_info_get_context(ProcInfo0, Context),
        Msg = "Error: mode list for procedure is empty.",
        add_error_report(Context, [words(Msg)], !Errors),
        WriteHashTables0 = no,
        WriteDataTable0 = no,
        MaybeAllInProc0 = no
    ),
    ( if InferredDetism = inferred(Determinism) then
        proc_info_set_inferred_determinism(Determinism,
            ProcInfo0, ProcInfo),
        map.det_update(ProcID, ProcInfo, !ProcTable)
    else
        true
    ),
    infer_proc_determinism_pass_1(ProcIDs, ModuleInfo, !ProcTable,
        !CheckProcs, !FactArgInfos, MaybeAllInProc1, WriteHashTables1,
        WriteDataTable1, !Errors),
    (
        MaybeAllInProc0 = yes(_),
        MaybeAllInProc = MaybeAllInProc0
    ;
        MaybeAllInProc0 = no,
        MaybeAllInProc = MaybeAllInProc1
    ),
    bool.or(WriteHashTables0, WriteHashTables1, WriteHashTables),
    bool.or(WriteDataTable0, WriteDataTable1, WriteDataTable).

    % Return the fact_table_mode_type for a procedure.
    %
:- pred fact_table_mode_type(list(mer_mode)::in, module_info::in,
    fact_table_mode_type::out) is det.

fact_table_mode_type([], _, unknown).
fact_table_mode_type([Mode | Modes], ModuleInfo, ModeType) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ModeType0 = all_in
    else if mode_is_fully_output(ModuleInfo, Mode) then
        ModeType0 = all_out
    else
        ModeType0 = other
    ),
    ( if ModeType0 = other then
        ModeType = other
    else
        fact_table_mode_type(Modes, ModuleInfo, ModeType1),
        ( if ModeType1 = unknown then
            ModeType = ModeType0
        else if ModeType1 = other then
            ModeType = other
        else if ModeType1 = ModeType0 then
            ModeType = ModeType0
        else
            ModeType = in_out
        )
    ).

%---------------------------------------------------------------------------%

    % open_sort_files(ProcIDs, ProcStreams):
    %
    % Open a temporary sort file for each proc_id in ProcIDs.
    % Return a list of proc_streams for all the files opened.
    %
:- pred open_sort_files(list(proc_id)::in, list(proc_stream)::out,
    error_reports::in, error_reports::out, io::di, io::uo) is det.

open_sort_files([], [], !Errors, !IO).
open_sort_files([ProcID | ProcIDs], ProcStreams, !Errors, !IO) :-
    open_temp_output(SortFileNameResult, !IO),
    (
        SortFileNameResult = ok({_SortFileName, Stream}),
        open_sort_files(ProcIDs, ProcStreams0, !Errors, !IO),
        ProcStreams = [proc_stream(ProcID, Stream) | ProcStreams0]
    ;
        SortFileNameResult = error(ErrorMessage),
        ProcStreams = [],
        add_error_report([words(ErrorMessage)], !Errors)
    ).

    % close_sort_files(ProcStreams, ProcFiles, !IO):
    %
    % Close the sort file of each procedure, and return its name.
    %
:- pred close_sort_files(list(proc_stream)::in,
    assoc_list(proc_id, string)::out, io::di, io::uo) is det.

close_sort_files([], [], !IO).
close_sort_files([proc_stream(ProcID, Stream) | ProcStreams],
        [ProcID - FileName | ProcFiles], !IO) :-
    io.output_stream_name(Stream, FileName, !IO),
    io.close_output(Stream, !IO),
    close_sort_files(ProcStreams, ProcFiles, !IO).

    % write_sort_file_lines(ProcStreams, ProcTable, Terms):
    %
    % Write out a line to each sort file for this fact. The line is made up
    % of the input arguments of the procedure (the key) followed by the
    % position of the fact in the original input table.
    %
    % Note lines written out here need to be read back in by
    % read_sort_file_line so if any changes are made here, corresponding
    % changes should be made there too.
    %
:- pred write_sort_file_lines(list(proc_stream)::in, proc_table::in,
    list(prog_term)::in, module_info::in, string::in,
    list(fact_arg_info)::in, bool::in, io::di, io::uo) is det.

write_sort_file_lines([], _, _, _, _, _, _, !IO).
write_sort_file_lines([proc_stream(ProcID, Stream) | ProcStreams], ProcTable,
        Terms, ModuleInfo, FactNumStr, FactArgInfos, IsPrimary, !IO) :-
    map.lookup(ProcTable, ProcID, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    assoc_list.from_corresponding_lists(ArgModes, Terms, ModeTerms),
    make_sort_file_key(ModeTerms, ModuleInfo, Key),
    (
        IsPrimary = yes,
        assoc_list.from_corresponding_lists(FactArgInfos, Terms, InfoTerms),
        DataString = make_fact_data_string(InfoTerms)
    ;
        IsPrimary = no,
        DataString = ""
    ),
    io.write_strings(Stream,
        [Key, "~", FactNumStr, "~", DataString, "\n"], !IO),
    write_sort_file_lines(ProcStreams, ProcTable, Terms, ModuleInfo,
        FactNumStr, [], no, !IO).

%---------------------------------------------------------------------------%

    % Create a key for the fact table entry.
    % Arguments are separated by ":".
    % Colons in string literals are replaced by "\c", tildes are replaced
    % by "\t", newlines are replaced by "\n" and backslashes by "\\".
    % This ensures that each possible set of arguments maps to a unique key
    % and guarantees that duplicate keys will be adjacent after sorting
    % with the sort program. The tilde ('~') character is used in the
    % sort file to separate the sort key from the data.
    %
:- pred make_sort_file_key(assoc_list(mer_mode, prog_term)::in,
    module_info::in, string::out) is det.

make_sort_file_key([], _, "").
make_sort_file_key([(Mode - Term) | ModeTerms], ModuleInfo, Key) :-
    ( if
        mode_is_fully_input(ModuleInfo, Mode),
        Term = term.functor(Const, [], _Context)
    then
        KeyPart = make_key_part(Const),
        make_sort_file_key(ModeTerms, ModuleInfo, Key0),
        string.append(":", Key0, Key1), % field separator
        string.append(KeyPart, Key1, Key)
    else
        make_sort_file_key(ModeTerms, ModuleInfo, Key)
    ).

    % Like make_sort_file_key but for the output arguments of the fact.
    %
:- func make_fact_data_string(assoc_list(fact_arg_info, prog_term)) = string.

make_fact_data_string([]) = "".
make_fact_data_string([fact_arg_info(_, _, IsOutput) - Term | InfoTerms]) =
        String :-
    ( if
        IsOutput = yes,
        Term = term.functor(Const, [], _)
    then
        KeyPart = make_key_part(Const),
        String0 = make_fact_data_string(InfoTerms),
        string.append_list([KeyPart, ":", String0], String)
    else
        String = make_fact_data_string(InfoTerms)
    ).

:- func make_key_part(const) = string.

make_key_part(Const) = Key :-
    (
        Const = term.atom(_),
        unexpected($module, $pred, "enumerated types are not supported yet.")
    ;
        Const = term.integer(Base, Integer, Signedness, _Size),
        (
            Signedness = signed,
            ( if source_integer_to_int(Base, Integer, I) then
                % Print the integer in base 36 to reduce the amount of I/O we
                % do.
                Key = string.int_to_base_string(I, 36)
            else
                unexpected($module, $pred, "integer too big")
            )
        ;
            Signedness = unsigned,
            unexpected($module, $pred, "NYI uints and fact tables")
        )
    ;
        Const = term.float(F),
        Key = string.float_to_string(F)
    ;
        Const = term.string(Str),
        string.to_char_list(Str, Chars),
        key_from_chars(Chars, EscapedChars),
        string.from_char_list(EscapedChars, Key)
    ;
        Const = term.implementation_defined(_),
        unexpected($module, $pred, "implementation-defined literal")
    ).

    % Escape all backslashes with a backslash, and replace all newlines
    % with "\n", colons with "\c" and tildes with "\t".
    %
:- pred key_from_chars(list(char)::in, list(char)::out) is det.

key_from_chars(Chars, EscapedChars) :-
    key_from_chars_loop(Chars, cord.init, EscapedCharsCord),
    EscapedChars = cord.to_list(EscapedCharsCord).

:- pred key_from_chars_loop(list(char)::in, cord(char)::in, cord(char)::out)
    is det.

key_from_chars_loop([], !EscapedCharsCord).
key_from_chars_loop([Char | Chars], !EscapedCharsCord) :-
    ( if Char = ('\\') then
        !:EscapedCharsCord = !.EscapedCharsCord ++ cord.from_list(['\\', '\\'])
    else if Char = ('\n') then
        !:EscapedCharsCord = !.EscapedCharsCord ++ cord.from_list(['\\', 'n'])
    else if Char = (':') then
        !:EscapedCharsCord = !.EscapedCharsCord ++ cord.from_list(['\\', 'c'])
    else if Char = ('~') then
        !:EscapedCharsCord = !.EscapedCharsCord ++ cord.from_list(['\\', 't'])
    else
        !:EscapedCharsCord = cord.snoc(!.EscapedCharsCord, Char)
    ),
    key_from_chars_loop(Chars, !EscapedCharsCord).

%---------------------------------------------------------------------------%

    % infer_determinism_pass_2(ProcFiles, Globals, ExistsAllInMode,
    %   !ProcTable, !IO):
    %
    % Run `sort' on each sort file to see if the keys are unique.
    % If they are, the procedure is semidet, otherwise it is nondet.
    % Return the updated proc_table.
    %
:- pred infer_determinism_pass_2(assoc_list(proc_id, string)::in, globals::in,
    bool::in, proc_table::in, proc_table::out, io::di, io::uo) is det.

infer_determinism_pass_2([], _, _, !ProcTable, !IO).
infer_determinism_pass_2([ProcID - FileName | ProcFiles], Globals,
        ExistsAllInMode, !ProcTable, !IO) :-
    map.lookup(!.ProcTable, ProcID, ProcInfo0),
    make_command_string(string.format(
        "LC_ALL=C sort -o %s %s && " ++
        "cut -d'~' -f1 %s | LC_ALL=C sort -cu >/dev/null 2>&1",
        [s(FileName), s(FileName), s(FileName)]), double, Command),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Invoking system command `", !IO),
    maybe_write_string(Verbose, Command, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    io.call_system(Command, Result, !IO),
    maybe_write_string(Verbose, "done.\n", !IO),
    (
        Result = ok(ExitStatus),

        % sort -cu returns 0 if file is sorted and contains no duplicate keys,
        % >=1 if duplicate keys exist.
        ( if
            (
                ExitStatus = 0
            ;
                % This is an all_in mode so it is semidet.
                ExistsAllInMode = yes,
                ProcFiles = []
            )
        then
            % No duplicate keys => procedure is semidet.
            Determinism = detism_semi
        else if
            ExitStatus >= 1
        then
            % Duplicate keys => procedure is nondet.
            proc_info_get_declared_determinism(ProcInfo0, MaybeDet),
            ( if
                ( MaybeDet = yes(detism_cc_multi)
                ; MaybeDet = yes(detism_cc_non)
                )
            then
                Determinism = detism_cc_non
            else
                Determinism = detism_non
            )
        else
            io.progname_base("mercury_compile", ProgName, !IO),
            string.format(
                "%s: an error occurred in the `sort' program "
                ++ "during fact table determinism inference.",
                [s(ProgName)], Msg),
            write_error_pieces_plain(Globals, [words(Msg)], !IO),
            io.set_exit_status(1, !IO),
            Determinism = detism_erroneous
        )
    ;
        Result = error(ErrorCode),
        write_call_system_error_msg(Globals, "sort", ErrorCode, !IO),
        Determinism = detism_erroneous
    ),
    proc_info_set_inferred_determinism(Determinism, ProcInfo0, ProcInfo),
    map.det_update(ProcID, ProcInfo, !ProcTable),
    infer_determinism_pass_2(ProcFiles, Globals, ExistsAllInMode,
        !ProcTable, !IO).

%---------------------------------------------------------------------------%

    % Write out the fact table data arrays and hash tables.
    %
:- pred write_fact_table_arrays(assoc_list(proc_id, string)::in, string::in,
    string::in, proc_table::in, module_info::in, int::in,
    list(fact_arg_info)::in, bool::in, bool::in, io.output_stream::in,
    string::out, proc_id::out, io::di, io::uo) is det.

write_fact_table_arrays(ProcFiles0, DataFileName, StructName, ProcTable,
        ModuleInfo, NumFacts, FactArgInfos, WriteHashTables, WriteDataTable,
        OutputStream, C_HeaderCode, PrimaryProcID, !IO) :-
    (
        % No sort files => there was only and all_out mode
        %   => nothing left to be done here.
        ProcFiles0 = [],
        C_HeaderCode = "",
        % This won't get used anyway.
        PrimaryProcID = hlds_pred.initial_proc_id
    ;
        ProcFiles0 = [PrimaryProcID - FileName | ProcFiles1],
        (
            WriteHashTables = yes,
            (
                % If there we need to build secondary hash tables (i.e. if
                % there is >1 input mode) we need to create a ``FactMap''
                % while writing out the primary table.
                ProcFiles1 = [_ | _],
                CreateFactMap = yes
            ;
                ProcFiles1 = [],
                CreateFactMap = no
            ),
            write_primary_hash_table(PrimaryProcID, FileName, DataFileName,
                StructName, ProcTable, ModuleInfo, OutputStream, FactArgInfos,
                WriteDataTable, NumFacts, CreateFactMap, Result0, FactMap,
                C_HeaderCode0, !IO),
            (
                Result0 = ok,
                write_secondary_hash_tables(ProcFiles1, StructName, ProcTable,
                    ModuleInfo, OutputStream, FactMap, FactArgInfos,
                    "", C_HeaderCode1, !IO),
                C_HeaderCode = C_HeaderCode0 ++ C_HeaderCode1
            ;
                Result0 = error,
                C_HeaderCode = C_HeaderCode0
            )
        ;
            WriteHashTables = no,
            C_HeaderCode = ""
        )
    ).

    % Write out the data for the fact table.
    %
:- pred write_fact_table_data(bool::in, int::in, int::in,
    list(list(fact_arg))::in, string::in, io.output_stream::in,
    io::di, io::uo) is det.

write_fact_table_data(_, _, _, [], _, _, !IO).
write_fact_table_data(VeryVerbose, FactNum, FactTableSize, [Fact | Facts],
        StructName, OutputStream, !IO) :-
    write_fact_data(VeryVerbose, FactNum, FactTableSize, Fact,
        StructName, OutputStream, !IO),
    write_fact_table_data(VeryVerbose, FactNum + 1, FactTableSize, Facts,
        StructName, OutputStream, !IO).

    % Write out the data for a single fact, starting a new array if necessary.
    % Note: this predicate will not write the declaration or opening brace
    % for the first array or the closing brace of the last array.
    %
:- pred write_fact_data(bool::in, int::in, int::in, list(fact_arg)::in,
    string::in, io.output_stream::in, io::di, io::uo) is det.

write_fact_data(VeryVerbose, FactNum, FactTableSize, Args,
        StructName, OutputStream, !IO) :-
    ( if 0 = FactNum mod FactTableSize then
        ( if FactNum = 0 then
            true
        else
            write_closing_brace(OutputStream, !IO),
            write_new_data_array(OutputStream, StructName, FactNum, !IO)
        ),
        (
            VeryVerbose = yes,
            io.format("%% Writing fact %d\n", [i(FactNum)], !IO)
        ;
            VeryVerbose = no
        )
    else
        true
    ),
    io.write_string(OutputStream, "\t{", !IO),
    write_fact_args(OutputStream, Args, !IO),
    io.write_string(OutputStream, " },\n", !IO).

    % Write out the declaration of a new data array followed by " = {\n".
    %
:- pred write_new_data_array(io.output_stream::in, string::in, int::in,
    io::di, io::uo) is det.

write_new_data_array(OutputStream, StructName, FactNum, !IO) :-
    io.format(OutputStream, "const struct %s_struct %s%d[] = {\n",
        [s(StructName), s(StructName), i(FactNum)], !IO).

    % Write out the closing brace of an array.
    %
:- pred write_closing_brace(io.output_stream::in, io::di, io::uo) is det.

write_closing_brace(OutputStream, !IO) :-
    io.write_string(OutputStream, "};\n\n", !IO).

:- pred write_fact_args(io.output_stream::in, list(fact_arg)::in,
    io::di, io::uo) is det.

write_fact_args(_, [], !IO).
write_fact_args(OutputStream, [Arg | Args], !IO) :-
    (
        Arg = term.string(String),
        io.write_string(OutputStream, """", !IO),
        c_util.output_quoted_string(OutputStream, String, !IO),
        io.write_string(OutputStream, """, ", !IO)
    ;
        Arg = term.integer(Base, Integer, Signedness, _),
        (
            Signedness = signed,
            ( if source_integer_to_int(Base, Integer, Int) then
                io.write_int(OutputStream, Int, !IO),
                io.write_string(OutputStream, ", ", !IO)
            else
                unexpected($module, $pred, "integer too big")
            )
        ;
            Signedness = unsigned,
            unexpected($module, $pred, "NYI uints in fact tables")
        )
    ;
        Arg = term.float(Float),
        io.write_float(OutputStream, Float, !IO),
        io.write_string(OutputStream, ", ", !IO)
    ;
        Arg = term.atom(_),
        unexpected($module, $pred, "unsupported type")
    ;
        Arg = term.implementation_defined(_),
        unexpected($module, $pred, "implementation-defined literal")
    ),
    write_fact_args(OutputStream, Args, !IO).

    % If a data table has been created in a separate file, append it to the
    % end of the main output file and then delete it.
    %
:- pred maybe_append_data_table(globals::in, bool::in, string::in, string::in,
    io::di, io::uo) is det.

maybe_append_data_table(_Globals, no, _, _, !IO).
maybe_append_data_table(Globals, yes, OutputFileName, DataFileName, !IO) :-
    make_command_string(string.format("cat %s >>%s",
        [s(DataFileName), s(OutputFileName)]), forward, Command),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Invoking system command `", !IO),
    maybe_write_string(Verbose, Command, !IO),
    maybe_write_string(Verbose, ", ...", !IO),
    io.call_system(Command, Result, !IO),
    maybe_write_string(Verbose, "done.\n", !IO),
    (
        Result = ok(ExitStatus),
        ( if ExitStatus = 0 then
            true
        else
            Msg = "An error occurred while concatenating " ++
                "fact table output files.",
            write_error_pieces_plain(Globals, [words(Msg)], !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        Result = error(ErrorCode),
        write_call_system_error_msg(Globals, "cat", ErrorCode, !IO)
    ),
    delete_temporary_file(Globals, DataFileName, !IO).

    % Write hash tables for the primary key. Create a map from indices in the
    % original input table to the table sorted on the primary key.
    % Write out the data table if required.
    %
:- pred write_primary_hash_table(proc_id::in, string::in, string::in,
    string::in, proc_table::in, module_info::in, io.output_stream::in,
    list(fact_arg_info)::in, bool::in, int::in, bool::in, fact_result::out,
    map(int, int)::out, string::out, io::di, io::uo) is det.

write_primary_hash_table(ProcID, FileName, DataFileName, StructName, ProcTable,
        ModuleInfo, OutputStream, FactArgInfos, WriteDataTable,
        NumFacts, CreateFactMap, Result, FactMap, C_HeaderCode, !IO) :-
    map.init(FactMap0),
    module_info_get_globals(ModuleInfo, Globals),
    io.see(FileName, SeeResult, !IO),
    (
        SeeResult = ok,
        (
            WriteDataTable  = yes,
            io.open_output(DataFileName, OpenResult, !IO),
            (
                OpenResult = ok(DataStream),
                MaybeDataStream = yes(DataStream),
                % output opening brace for first fact array
                write_new_data_array(DataStream, StructName, 0, !IO)
            ;
                OpenResult = error(Error),
                print_file_open_error(Globals, no, DataFileName, "output",
                    Error, !IO),
                MaybeDataStream = no
            )
        ;
            WriteDataTable = no,
            MaybeDataStream = no
        ),
        (
            MaybeDataStream = yes(_),
            proc_id_to_int(ProcID, ProcInt),
            string.format("%s_hash_table_%d_",
                [s(StructName), i(ProcInt)], HashTableName),
            % Note: the type declared here is not necessarily correct.
            % The type is declared just to stop the C compiler emitting
            % warnings.
            string.format("extern struct MR_fact_table_hash_table_i %s0;\n",
                [s(HashTableName)], C_HeaderCode0),
            map.lookup(ProcTable, ProcID, ProcInfo),
            proc_info_get_argmodes(ProcInfo, ArgModes),
            read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
                MaybeFirstFact, !IO),
            (
                MaybeFirstFact = yes(FirstFact),
                build_hash_table(0, 0, HashTableName, StructName, 0, ArgModes,
                    ModuleInfo, FactArgInfos, yes, OutputStream, FirstFact,
                    MaybeDataStream, CreateFactMap, FactMap0, FactMap, !IO),
                Result = ok
            ;
                MaybeFirstFact = no,
                Result = error,
                FactMap = FactMap0
            )
        ;
            MaybeDataStream = no,
            Result = error,
            FactMap = FactMap0,
            C_HeaderCode0 = ""
        ),
        (
            MaybeDataStream = yes(DataStream1),
            % Closing brace for last fact data array.
            write_closing_brace(DataStream1, !IO),
            fact_table_size(Globals, FactTableSize),
            write_fact_table_pointer_array(NumFacts, FactTableSize,
                StructName, DataStream1, C_HeaderCode1, !IO),
            io.close_output(DataStream1, !IO),
            C_HeaderCode = C_HeaderCode0 ++ C_HeaderCode1
        ;
            MaybeDataStream = no,
            C_HeaderCode = C_HeaderCode0
        ),
        io.seen(!IO),
        delete_temporary_file(Globals, FileName, !IO)
    ;
        SeeResult = error(Error),
        print_file_open_error(Globals, no, FileName, "input", Error, !IO),
        Result = error,
        FactMap = FactMap0,
        C_HeaderCode = ""
    ).

    % Build hash tables for non-primary input procs.
    %
:- pred write_secondary_hash_tables(assoc_list(proc_id, string)::in,
    string::in, proc_table::in, module_info::in, io.output_stream::in,
    map(int, int)::in, list(fact_arg_info)::in, string::in, string::out,
    io::di, io::uo) is det.

write_secondary_hash_tables([], _, _, _, _, _, _, !C_HeaderCode, !IO).
write_secondary_hash_tables([ProcID - FileName | ProcFiles], StructName,
        ProcTable, ModuleInfo, OutputStream, FactMap, FactArgInfos,
        !C_HeaderCode, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    io.see(FileName, SeeResult, !IO),
    (
        SeeResult = ok,
        proc_id_to_int(ProcID, ProcInt),
        string.format("%s_hash_table_%d_",
            [s(StructName), i(ProcInt)], HashTableName),
        % Note: the type declared here is not necessarily correct.
        % The type is declared just to stop the C compiler emitting warnings.
        string.format(
            "extern struct MR_fact_table_hash_table_i %s0;\n",
            [s(HashTableName)], New_C_HeaderCode),
        string.append(New_C_HeaderCode, !C_HeaderCode),
        map.lookup(ProcTable, ProcID, ProcInfo),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
            MaybeFirstFact, !IO),
        (
            MaybeFirstFact = yes(FirstFact),
            build_hash_table(0, 0, HashTableName, StructName, 0, ArgModes,
                ModuleInfo, FactArgInfos, no, OutputStream, FirstFact, no, no,
                FactMap, _, !IO),
            io.seen(!IO),
            delete_temporary_file(Globals, FileName, !IO),
            write_secondary_hash_tables(ProcFiles, StructName, ProcTable,
                ModuleInfo, OutputStream, FactMap, FactArgInfos,
                !C_HeaderCode, !IO)
        ;
            MaybeFirstFact = no,
            io.seen(!IO)
        )
    ;
        SeeResult = error(Error),
        print_file_open_error(Globals, no, FileName, "input", Error, !IO)
    ).

:- pred read_sort_file_line(list(fact_arg_info)::in, list(mer_mode)::in,
    module_info::in, maybe(sort_file_line)::out, io::di, io::uo) is det.

read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo, MaybeSortFileLine,
        !IO) :-
    io.read_line(Result, !IO),
    (
        Result = ok(LineChars),
        string.from_char_list(LineChars, LineString),
        split_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
            LineString, SortFileLine),
        MaybeSortFileLine = yes(SortFileLine)
    ;
        Result = eof,
        MaybeSortFileLine = no
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        io.input_stream_name(FileName, !IO),
        string.format("Error reading file `%s':", [s(FileName)], Msg),
        module_info_get_globals(ModuleInfo, Globals),
        write_error_pieces_plain(Globals,
            [words(Msg), nl, words(ErrorMessage)], !IO),
        io.set_exit_status(1, !IO),
        MaybeSortFileLine = no
    ).

    % Build and write out a top level hash table and all the lower level
    % tables connected to it.
    %
:- pred build_hash_table(int::in, int::in, string::in, string::in,
    int::in, list(mer_mode)::in, module_info::in, list(fact_arg_info)::in,
    bool::in, io.output_stream::in, sort_file_line::in,
    maybe(io.output_stream)::in, bool::in,
    map(int, int)::in, map(int, int)::out, io::di, io::uo) is det.

build_hash_table(FactNum, InputArgNum, HashTableName, StructName,
        TableNum, ArgModes, ModuleInfo, Infos, IsPrimaryTable, OutputStream,
        FirstFact, MaybeDataStream, CreateFactMap, !FactMap, !IO) :-
    build_hash_table_2(FactNum, InputArgNum, HashTableName, StructName,
        TableNum, ArgModes, ModuleInfo, Infos, IsPrimaryTable,
        OutputStream, yes(FirstFact), MaybeDataStream, CreateFactMap,
        !FactMap, [], HashList, !IO),
    list.length(HashList, Len),
    module_info_get_globals(ModuleInfo, Globals),
    calculate_hash_table_size(Globals, Len, HashSize),
    hash_table_init(HashSize, HashTable0),
    hash_table_from_list(HashList, HashSize, HashTable0, HashTable),
    write_hash_table(OutputStream, HashTableName, TableNum, HashTable, !IO).

:- pred build_hash_table_2(int::in, int::in, string::in, string::in, int::in,
    list(mer_mode)::in, module_info::in, list(fact_arg_info)::in, bool::in,
    io.output_stream::in,
    maybe(sort_file_line)::in, maybe(io.output_stream)::in,
    bool::in, map(int, int)::in, map(int, int)::out,
    list(hash_entry)::in, list(hash_entry)::out, io::di, io::uo) is det.

build_hash_table_2(_, _, _, _, _, _, _, _, _, _, no, _, _, !FactMap,
        !HashList, !IO).
build_hash_table_2(FactNum, InputArgNum, HashTableName, StructName, !.TableNum,
        ArgModes, ModuleInfo, Infos, IsPrimaryTable, OutputStream,
        yes(FirstFact), MaybeDataStream, CreateFactMap, !FactMap, !HashList,
        !IO) :-
    top_level_collect_matching_facts(FirstFact, MatchingFacts, MaybeNextFact,
        Infos, ArgModes, ModuleInfo, !IO),
    (
        CreateFactMap = yes,
        update_fact_map(FactNum, MatchingFacts, !FactMap)
    ;
        CreateFactMap = no
    ),
    module_info_get_globals(ModuleInfo, Globals),
    (
        MaybeDataStream = yes(DataStream),
        list.map((pred(X::in, Y::out) is det :-
            X = sort_file_line(_, _, Y)
        ), MatchingFacts, OutputData),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        fact_table_size(Globals, FactTableSize),
        write_fact_table_data(VeryVerbose, FactNum, FactTableSize,
            OutputData, StructName, DataStream, !IO)
    ;
        MaybeDataStream = no
    ),
    do_build_hash_table(Globals, FactNum, InputArgNum, HashTableName,
        !TableNum, IsPrimaryTable, OutputStream, MatchingFacts, !.FactMap,
        !HashList, !IO),
    list.length(MatchingFacts, Len),
    NextFactNum = FactNum + Len,
    build_hash_table_2(NextFactNum, InputArgNum, HashTableName, StructName,
        !.TableNum, ArgModes, ModuleInfo, Infos, IsPrimaryTable, OutputStream,
        MaybeNextFact, MaybeDataStream, CreateFactMap, !FactMap, !HashList,
        !IO).

    % Build a lower level hash table. The main difference to build_hash_table
    % (above) is that ``sort file lines'' are read from a list rather than
    % from the actual sort file.
    %
:- pred build_hash_table_lower_levels(globals::in, int::in, int::in,
    string::in, int::in, int::out, bool::in, io.output_stream::in,
    list(sort_file_line)::in, map(int, int)::in, io::di, io::uo) is det.

build_hash_table_lower_levels(Globals, FactNum, InputArgNum, HashTableName,
        TableNum0, TableNum, IsPrimaryTable, OutputStream, Facts, FactMap,
        !IO) :-
    build_hash_table_lower_levels_2(Globals, FactNum, InputArgNum,
        HashTableName, TableNum0, TableNum, IsPrimaryTable, OutputStream,
        Facts, FactMap, [], HashList, !IO),
    list.length(HashList, Len),
    calculate_hash_table_size(Globals, Len, HashSize),
    hash_table_init(HashSize, HashTable0),
    hash_table_from_list(HashList, HashSize, HashTable0, HashTable),
    write_hash_table(OutputStream, HashTableName, TableNum0, HashTable, !IO).

:- pred build_hash_table_lower_levels_2(globals::in, int::in, int::in,
    string::in, int::in, int::out, bool::in, io.output_stream::in,
    list(sort_file_line)::in, map(int, int)::in,
    list(hash_entry)::in, list(hash_entry)::out, io::di, io::uo) is det.

build_hash_table_lower_levels_2(_, _, _, _, !TableNum, _, _, [],
        _, !HashList, !IO).
build_hash_table_lower_levels_2(Globals, FactNum, InputArgNum, HashTableName,
        !TableNum, IsPrimaryTable, OutputStream,
        [Fact | Facts0], FactMap, !HashList, !IO) :-
    lower_level_collect_matching_facts(Fact, Facts0, MatchingFacts,
        Facts1, InputArgNum),
    do_build_hash_table(Globals, FactNum, InputArgNum, HashTableName,
        !TableNum, IsPrimaryTable, OutputStream, MatchingFacts, FactMap,
        !HashList, !IO),
    list.length(MatchingFacts, Len),
    NextFactNum = FactNum + Len,
    build_hash_table_lower_levels_2(Globals, NextFactNum, InputArgNum,
        HashTableName, !TableNum, IsPrimaryTable, OutputStream,
        Facts1, FactMap, !HashList, !IO).

    % This is where most of the actual work is done in building up the
    % hash table.
    %
:- pred do_build_hash_table(globals::in, int::in, int::in, string::in,
    int::in, int::out, bool::in, io.output_stream::in,
    list(sort_file_line)::in, map(int, int)::in,
    list(hash_entry)::in, list(hash_entry)::out, io::di, io::uo) is det.

do_build_hash_table(Globals, FactNum, InputArgNum, HashTableName, !TableNum,
        IsPrimaryTable, OutputStream, Facts, FactMap, !HashList, !IO) :-
    (
        Facts = [],
        unexpected($module, $pred, "no facts")
    ;
        Facts = [Fact | Facts1],
        fact_get_arg_and_index(Fact, InputArgNum, Arg, Index),
        (
            IsPrimaryTable = yes,
            HashIndex = FactNum
        ;
            IsPrimaryTable = no,
            map.lookup(FactMap, Index, HashIndex)
        ),
        ( if
            Facts1 = []
        then
            % If only one matching index, insert a pointer to the fact table
            % entry into the current hash table.
            !:HashList = [hash_entry(Arg, fact(HashIndex), -1) | !.HashList]
        else if
            % See if there are any more input arguments.
            NextInputArgNum = InputArgNum + 1,
            Fact = sort_file_line(InputArgs, _, _),
            N = NextInputArgNum + 1,
            list.drop(N, InputArgs, _)
        then
            !:TableNum = !.TableNum + 1,
            ThisTableNum = !.TableNum,
            build_hash_table_lower_levels(Globals, FactNum, NextInputArgNum,
                HashTableName, !TableNum, IsPrimaryTable, OutputStream,
                Facts, FactMap, !IO),
            !:HashList = [hash_entry(Arg,
                hash_table(ThisTableNum, HashTableName), -1) | !.HashList]
        else if
            IsPrimaryTable = no
        then
            % Insert all matching indexes into the hash table.
            hash_list_insert_many(Facts, IsPrimaryTable, FactMap,
                FactNum, InputArgNum, !HashList)
        else
            % Insert only the first matching index into the hash table.
            !:HashList = [hash_entry(Arg, fact(HashIndex), -1) | !.HashList]
        )
    ).

    % Read lines from the sort file that that have the same first input
    % argument as Fact. Places these lines into MatchingFacts. The first fact
    % in MatchingFacts is always Fact. If an extra fact is read in following
    % the matching facts, it is placed in MaybeNextFact.
    %
:- pred top_level_collect_matching_facts(sort_file_line::in,
    list(sort_file_line)::out, maybe(sort_file_line)::out,
    list(fact_arg_info)::in, list(mer_mode)::in, module_info::in,
    io::di, io::uo) is det.

top_level_collect_matching_facts(Fact, MatchingFacts, MaybeNextFact, Infos,
        ArgModes, ModuleInfo, !IO) :-
    top_level_collect_matching_facts_2(Fact, [], MatchingFacts0,
        MaybeNextFact, Infos, ArgModes, ModuleInfo, !IO),
    list.reverse(MatchingFacts0, MatchingFacts1),
    MatchingFacts = [Fact | MatchingFacts1].

:- pred top_level_collect_matching_facts_2(sort_file_line::in,
    list(sort_file_line)::in, list(sort_file_line)::out,
    maybe(sort_file_line)::out, list(fact_arg_info)::in, list(mer_mode)::in,
    module_info::in, io::di, io::uo) is det.

top_level_collect_matching_facts_2(Fact, !MatchingFacts, MaybeNextFact,
        Infos, ArgModes, ModuleInfo, !IO) :-
    read_sort_file_line(Infos, ArgModes, ModuleInfo, MaybeSortFileLine, !IO),
    (
        MaybeSortFileLine = yes(Fact1),
        ( if
            Fact1 = sort_file_line([Arg1 | _], _, _),
            Fact  = sort_file_line([Arg  | _], _, _)
        then
            ( if Arg = Arg1 then
                top_level_collect_matching_facts_2(Fact,
                    [Fact1 | !.MatchingFacts], !:MatchingFacts, MaybeNextFact,
                    Infos, ArgModes, ModuleInfo, !IO)
            else
                MaybeNextFact = yes(Fact1)
            )
        else
            unexpected($module, $pred, "no input args")
        )
    ;
        MaybeSortFileLine = no,
        MaybeNextFact = no
    ).

    % Same as above, but reads facts from a list instead of from the sort file.
    %
:- pred lower_level_collect_matching_facts(sort_file_line::in,
    list(sort_file_line)::in, list(sort_file_line)::out,
    list(sort_file_line)::out, int::in) is det.

lower_level_collect_matching_facts(Fact, Facts0, Matching, Remaining,
        InputArgNum) :-
    lower_level_collect_matching_facts_2(Fact, Facts0, [], Matching0,
        Remaining, InputArgNum),
    list.reverse(Matching0, Matching1),
    Matching = [Fact | Matching1].

:- pred lower_level_collect_matching_facts_2(sort_file_line::in,
    list(sort_file_line)::in, list(sort_file_line)::in,
    list(sort_file_line)::out, list(sort_file_line)::out, int::in) is det.

lower_level_collect_matching_facts_2(_, [], Matching, Matching, [], _).
lower_level_collect_matching_facts_2(Fact, [Fact0 | Facts0], Matching0,
        Matching, Remaining, InputArgNum) :-
    Fact0 = sort_file_line(InputArgs0, _, _),
    Fact  = sort_file_line(InputArgs,  _, _),
    ( if
        list.drop(InputArgNum, InputArgs0, [Arg0 | _]),
        list.drop(InputArgNum, InputArgs,  [Arg  | _])
    then
        ( if Arg = Arg0 then
            lower_level_collect_matching_facts_2(Fact, Facts0,
                [Fact0 | Matching0], Matching, Remaining, InputArgNum)
        else
            Matching = Matching0,
            Remaining = [Fact0 | Facts0]
        )
    else
        unexpected($module, $pred, "not enough input args")
    ).

:- pred update_fact_map(int::in, list(sort_file_line)::in,
    map(int, int)::in, map(int, int)::out) is det.

update_fact_map(_, [], !FactMap).
update_fact_map(FactNum, [Fact | Facts], !FactMap) :-
    Fact = sort_file_line(_, Index, _),
    map.set(Index, FactNum, !FactMap),
    update_fact_map(FactNum + 1, Facts, !FactMap).

%---------------------------------------------------------------------------%

    % Break up a string into the components of a sort file line.
    %
:- pred split_sort_file_line(list(fact_arg_info)::in, list(mer_mode)::in,
    module_info::in, string::in, sort_file_line::out) is det.

split_sort_file_line(FactArgInfos, ArgModes, ModuleInfo, Line0,
        SortFileLine) :-
    ( if
        string.sub_string_search(Line0, "~", Pos0),
        string.split(Line0, Pos0, InputArgsString, Line1),
        string.first_char(Line1, _, Line2),
        string.sub_string_search(Line2, "~", Pos1),
        string.split(Line2, Pos1, IndexString, Line3),
        string.first_char(Line3, _, Line4),
        string.remove_suffix(Line4, "\n", OutputArgsString),
        string.to_int(IndexString, Index0)
    then
        split_key_to_arg_strings(InputArgsString, InputArgStrings),
        get_input_args_list(FactArgInfos, ArgModes, ModuleInfo,
            InputArgStrings, InputArgs),
        split_key_to_arg_strings(OutputArgsString, OutputArgStrings),
        (
            % Only extract the output arguments if they have actually been
            % written to this sort file.
            OutputArgStrings = [_ | _],
            get_output_args_list(FactArgInfos, OutputArgStrings, OutputArgs)
        ;
            OutputArgStrings = [],
            OutputArgs = []
        ),
        SortFileLine = sort_file_line(InputArgs, Index0, OutputArgs)
    else
        unexpected($module, $pred, "sort file format incorrect")
    ).

    % Split up a string containing a sort file key into a list of strings
    % containing the key arguments. Arguments in the key are separated by `:'.
    %
:- pred split_key_to_arg_strings(string::in, list(string)::out) is det.

split_key_to_arg_strings(Key0, ArgStrings) :-
    ( if Key0 = "" then
        ArgStrings = []
    else
        ( if
            string.sub_string_search(Key0, ":", Pos),
            string.split(Key0, Pos, ArgString, Key1),
            string.first_char(Key1, _, Key2)
        then
            split_key_to_arg_strings(Key2, ArgStrings0),
            ArgStrings = [ArgString | ArgStrings0]
        else
            unexpected($module, $pred, "sort file key format is incorrect")
        )
    ).

:- pred get_input_args_list(list(fact_arg_info)::in, list(mer_mode)::in,
    module_info::in, list(string)::in, list(fact_arg)::out) is det.

get_input_args_list([], [], _, _, []).
get_input_args_list([_ | _], [], _, _, _) :-
    unexpected($module, $pred, "too many fact_arg_infos").
get_input_args_list([], [_ | _], _, _, _) :-
    unexpected($module, $pred, "too many argmodes").
get_input_args_list([Info | Infos], [Mode | Modes], ModuleInfo, ArgStrings0,
        Args) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        (
            ArgStrings0 = [ArgString | ArgStrings],
            Info = fact_arg_info(Type, _, _),
            convert_key_string_to_arg(ArgString, Type, Arg),
            get_input_args_list(Infos, Modes, ModuleInfo, ArgStrings, Args0),
            Args = [Arg | Args0]
        ;
            ArgStrings0 = [],
            unexpected($module, $pred, "not enough ArgStrings")
        )
    else
        % This argument is not input so skip it and try the next one.
        get_input_args_list(Infos, Modes, ModuleInfo, ArgStrings0, Args)
    ).

:- pred get_output_args_list(list(fact_arg_info)::in, list(string)::in,
    list(fact_arg)::out) is det.

get_output_args_list([], _, []).
get_output_args_list([Info | Infos], ArgStrings0, Args) :-
    Info = fact_arg_info(Type, _, IsOutput),
    (
        IsOutput = yes,
        % This is an output argument (for some mode of the predicate).
        (
            ArgStrings0 = [ArgString | ArgStrings],
            convert_key_string_to_arg(ArgString, Type, Arg),
            get_output_args_list(Infos, ArgStrings, Args0),
            Args = [Arg | Args0]
        ;
            ArgStrings0 = [],
            unexpected($module, $pred, "not enough ArgStrings")
        )
    ;
        IsOutput = no,
        % Not an output argument for any mode of the predicate.
        get_output_args_list(Infos, ArgStrings0, Args)
    ).

:- pred convert_key_string_to_arg(string::in, mer_type::in, fact_arg::out)
    is det.

convert_key_string_to_arg(ArgString, Type, Arg) :-
    % XXX UINT - handle uints here too when we support them in fact tables.
    ( if Type = builtin_type(builtin_type_int) then
        ( if string.base_string_to_int(36, ArgString, I) then
            Arg = term.integer(base_10, integer(I), signed, size_word)
        else
            unexpected($module, $pred, "could not convert string to int")
        )
    else if Type = builtin_type(builtin_type_string) then
        string.to_char_list(ArgString, Cs0),
        remove_sort_file_escapes(Cs0, [], Cs1),
        list.reverse(Cs1, Cs),
        string.from_char_list(Cs, S),
        Arg = term.string(S)
    else if Type = builtin_type(builtin_type_float) then
        ( if string.to_float(ArgString, F) then
            Arg = term.float(F)
        else
            unexpected($module, $pred, "could not convert string to float")
        )
    else
        unexpected($module, $pred, "unsupported type")
    ).

    % Remove the escape characters put in the string by make_sort_file_key.
    %
:- pred remove_sort_file_escapes(list(char)::in, list(char)::in,
    list(char)::out) is det.

remove_sort_file_escapes([], Cs, Cs).
remove_sort_file_escapes([C0 | Cs0], In, Out) :-
    ( if C0 = ('\\') then
        (
            Cs0 = [C1 | Cs1],
            ( if C1 = ('\\') then
                C = ('\\')
            else if C1 = ('c') then
                C = (':')
            else if C1 = ('t') then
                C = ('~')
            else if C1 = ('n') then
                C = ('\n')
            else
                unexpected($module, $pred, "something went wrong")
            ),
            remove_sort_file_escapes(Cs1, [C | In], Out)
        ;
            Cs0 = [],
            unexpected($module, $pred, "something went wrong")
        )
    else
        remove_sort_file_escapes(Cs0, [C0 | In], Out)
    ).

:- pred fact_get_arg_and_index(sort_file_line::in, int::in, fact_arg::out,
    int::out) is det.

fact_get_arg_and_index(Fact, InputArgNum, Arg, Index) :-
    Fact = sort_file_line(InputArgs, Index, _),
    ( if list.drop(InputArgNum, InputArgs, [ArgPrime | _]) then
        Arg = ArgPrime
    else
        unexpected($module, $pred, "not enough input args")
    ).

%---------------------------------------------------------------------------%

    % Select a prime number > NumEntries * 100 / PercentFull.
    % The prime number is selected from a list of primes each of which is
    % close to a power of 2 between 2^1 and 2^31.
    %
:- pred calculate_hash_table_size(globals::in, int::in, int::out) is det.

calculate_hash_table_size(Globals, NumEntries, HashTableSize) :-
    globals.lookup_int_option(Globals, fact_table_hash_percent_full,
        PercentFull),
    Primes = [2, 3, 5, 11, 17, 37, 67, 131, 257, 521, 1031, 2053, 4099, 8209,
        16411, 32771, 65537, 131101, 262147, 524309, 1048627, 2097257, 4194493,
        8388949, 16777903, 33555799, 67108879, 134217757, 268435459, 536870923,
        1073741827, 2147483647],
    N = (NumEntries * 100) // PercentFull,
    calculate_hash_table_size_2(N, Primes, HashTableSize).

:- pred calculate_hash_table_size_2(int::in, list(int)::in, int::out) is det.

calculate_hash_table_size_2(_, [], _) :-
    unexpected($module, $pred, "hash table too large (max size 2147483647)").
calculate_hash_table_size_2(N, [P | Ps], H) :-
    ( if P > N then
        H = P
    else
        calculate_hash_table_size_2(N, Ps, H)
    ).

    % Insert an entry in a hash table. If a collision occurrs, find an empty
    % hash slot to place the data in and put a pointer to the new slot in the
    % Next field of the old one. This technique is called ``open-addressing''.
    %
:- pred hash_table_insert(hash_entry::in, int::in,
    hash_table::in, hash_table::out) is det.

hash_table_insert(Entry, HashSize, !HashTable) :-
    Entry = hash_entry(Key, Index, _),
    fact_table_hash(HashSize, Key, HashVal),
    ( if hash_table_search(!.HashTable, HashVal, _) then
        hash_table_insert_2(HashVal, _, Index, Key, !HashTable)
    else
        hash_table_set(HashVal, hash_entry(Key, Index, -1), !HashTable)
    ).

:- pred hash_table_insert_2(int::in, int::out, hash_index::in, fact_arg::in,
    hash_table::in, hash_table::out) is det.

hash_table_insert_2(HashVal, FreeVal, Index0, Key0, !HashTable) :-
    ( if
        hash_table_search(!.HashTable, HashVal, hash_entry(Key1, Index1, Next))
    then
        ( if Next = -1 then
            get_free_hash_slot(!.HashTable, HashVal, FreeVal),
            hash_table_set(FreeVal,
                hash_entry(Key0, Index0, -1), !HashTable),
            hash_table_set(HashVal,
                hash_entry(Key1, Index1, FreeVal), !HashTable)
        else
            hash_table_insert_2(Next, FreeVal, Index0, Key0, !HashTable)
        )
    else
        unexpected($module, $pred, "hash table entry empty")
    ).

    % Probe through the hash table to find a free slot. This will eventually
    % terminate because the hash table size is selected to be larger than
    % the number of entries that need to go in it.
    %
:- pred get_free_hash_slot(hash_table::in, int::in, int::out) is det.

get_free_hash_slot(HashTable, Start, Free) :-
    HashTable = hash_table(Size, _),
    Max = Size - 1,
    get_free_hash_slot_2(HashTable, Start, Max, Free).

:- pred get_free_hash_slot_2(hash_table::in, int::in, int::in, int::out)
    is det.

get_free_hash_slot_2(HashTable, Start, Max, Free) :-
    Next = (Start + 1) mod Max,
    ( if hash_table_search(HashTable, Next, _) then
        get_free_hash_slot_2(HashTable, Next, Max, Free)
    else
        Free = Next
    ).

    % Hash computation predicate.
    % Note: if you change this predicate, you will also need to change
    % the C code that is output to compute the hash value at runtime.
    % This C code is generated in `generate_hash_code'.
    %
:- pred fact_table_hash(int::in, fact_arg::in, int::out) is det.

fact_table_hash(HashSize, Key, HashVal) :-
    ( if
        Key = term.string(String)
    then
        % XXX This method of hashing strings may not work if cross-compiling
        % between systems that have different character representations.
        string.to_char_list(String, Cs),
        list.map((pred(C::in, I::out) is det :- char.to_int(C, I)), Cs, Ns)
    else if
        Key = term.integer(_, Integer, signed, size_word),
        integer.to_int(Integer, Int)
    then
        int.abs(Int, N),
        Ns = [N]
    else if
        Key = term.float(Float)
    then
        % XXX This method of hashing floats may not work cross-compiling
        % between architectures that have different floating-point
        % representations.
        int.abs(float.hash(Float), N),
        Ns = [N]
    else
        unexpected($module, $pred, "unsupported type in key")
    ),
    fact_table_hash_2(HashSize, Ns, 0, HashVal).

:- pred fact_table_hash_2(int::in, list(int)::in, int::in, int::out) is det.

fact_table_hash_2(_, [], !HashVal).
fact_table_hash_2(HashSize, [N | Ns], !HashVal) :-
    !:HashVal = (N + 31 * !.HashVal) mod HashSize,
    fact_table_hash_2(HashSize, Ns, !HashVal).

:- pred hash_list_insert_many(list(sort_file_line)::in, bool::in,
    map(int, int)::in, int::in, int::in,
    list(hash_entry)::in, list(hash_entry)::out) is det.

hash_list_insert_many([], _, _, _, _, !HashList).
hash_list_insert_many([Fact | Facts], IsPrimaryTable, FactMap,
        FactNum, InputArgNum, !HashList) :-
    fact_get_arg_and_index(Fact, InputArgNum, Arg, Index),
    (
        IsPrimaryTable = yes,
        HashIndex = FactNum
    ;
        IsPrimaryTable = no,
        map.lookup(FactMap, Index, HashIndex)
    ),
    !:HashList = [hash_entry(Arg, fact(HashIndex), -1) | !.HashList],
    hash_list_insert_many(Facts, IsPrimaryTable, FactMap, FactNum,
        InputArgNum, !HashList).

:- pred hash_table_init(int::in, hash_table::out) is det.

hash_table_init(Size, HashTable) :-
    map.init(Map),
    HashTable = hash_table(Size, Map).

:- pred hash_table_from_list(list(hash_entry)::in, int::in, hash_table::in,
    hash_table::out) is det.

hash_table_from_list([], _, !HashTable).
hash_table_from_list([Entry | Entrys], HashSize, !HashTable) :-
    hash_table_insert(Entry, HashSize, !HashTable),
    hash_table_from_list(Entrys, HashSize, !HashTable).

:- pred hash_table_search(hash_table::in, int::in, hash_entry::out) is semidet.

hash_table_search(HashTable, Index, Value) :-
    HashTable = hash_table(_, Map),
    map.search(Map, Index, Value).

:- pred hash_table_set(int::in, hash_entry::in,
    hash_table::in, hash_table::out) is det.

hash_table_set(Index, Value, HashTable0, HashTable) :-
    HashTable0 = hash_table(Size, Map0),
    map.set(Index, Value, Map0, Map),
    HashTable = hash_table(Size, Map).

%--------------------------------------------------------------------------%

    % Write out the C code for a hash table.
    %
:- pred write_hash_table(io.output_stream::in, string::in, int::in,
    hash_table::in, io::di, io::uo) is det.

write_hash_table(OutputStream, BaseName, TableNum, HashTable, !IO) :-
    get_hash_table_type(HashTable, TableType),
    string.format("struct MR_fact_table_hash_entry_%c %s%d_data[]",
        [c(TableType), s(BaseName), i(TableNum)], HashTableDataName),
    io.write_strings(OutputStream, [HashTableDataName, " = {\n"], !IO),
    HashTable = hash_table(Size, _),
    MaxIndex = Size - 1,
    write_hash_table_loop(OutputStream, HashTable, 0, MaxIndex, !IO),
    io.write_string(OutputStream, "};\n\n", !IO),
    io.format(OutputStream, "

struct MR_fact_table_hash_table_%c %s%d = {
    %d,
    %s%d_data
};
",
        [c(TableType), s(BaseName), i(TableNum), i(Size),
        s(BaseName), i(TableNum)], !IO).

:- pred write_hash_table_loop(io.text_output_stream::in, hash_table::in,
    int::in, int::in, io::di, io::uo) is det.

write_hash_table_loop(Stream, HashTable, CurrIndex, MaxIndex, !IO) :-
    ( if CurrIndex > MaxIndex then
        true
    else
        io.write_string(Stream, "\t{ ", !IO),
        ( if hash_table_search(HashTable, CurrIndex, HashEntry) then
            HashEntry = hash_entry(Key, Index, Next),
            (
                Key = term.string(String),
                io.write_string(Stream, """", !IO),
                c_util.output_quoted_string(Stream, String, !IO),
                io.write_string(Stream, """", !IO)
            ;
                Key = term.integer(_, Integer, Signedness, _),
                (
                    Signedness = signed,
                    Int = integer.det_to_int(Integer),
                    io.write_int(Stream, Int, !IO)
                ;
                    Signedness = unsigned,
                    unexpected($pred, "NYI uints in fact tables")
                )
            ;
                Key = term.float(Float),
                io.write_float(Stream, Float, !IO)
            ;
                ( Key = term.atom(_)
                ; Key = term.implementation_defined(_)
                ),
                unexpected($pred, "unsupported type")
            ),
            (
                Index = fact(I),
                io.format(Stream,
                    ", MR_FACT_TABLE_MAKE_TAGGED_INDEX(%d, 1), ",
                    [i(I)], !IO)
            ;
                Index = hash_table(I, H),
                io.format(Stream,
                    ", MR_FACT_TABLE_MAKE_TAGGED_POINTER(&%s%d, 2), ",
                    [s(H), i(I)], !IO)
            ),
            io.write_int(Stream, Next, !IO)
        else
            io.write_string(Stream,
                "0, MR_FACT_TABLE_MAKE_TAGGED_POINTER(NULL, 0), -1 ", !IO)
        ),
        io.write_string(Stream, "},\n", !IO),
        write_hash_table_loop(Stream, HashTable, CurrIndex + 1, MaxIndex, !IO)
    ).

    % Return 's' for string, 'i' for int, 'f' for float, 'a' for atom.
    % Don't call this with an empty hash table.
    %
:- pred get_hash_table_type(hash_table::in, char::out) is det.

get_hash_table_type(HashTable, TableType) :-
    HashTable = hash_table(_Size, Map),
    ( if map.is_empty(Map) then
        unexpected($module, $pred, "empty hash table")
    else
        get_hash_table_type_2(Map, 0, TableType)
    ).

:- pred get_hash_table_type_2(map(int, hash_entry)::in, int::in, char::out)
    is det.

get_hash_table_type_2(Map, Index, TableType) :-
    ( if map.search(Map, Index, Entry) then
        Entry = hash_entry(Key, _, _),
        ( if Key = term.string(_) then
            TableType = 's'
        else if Key = term.integer(_, _, _, _) then
            TableType = 'i'
        else if Key = term.float(_) then
            TableType = 'f'
        else if Key = term.atom(_) then
            TableType = 'a'
        else
            unexpected($module, $pred, "invalid term")
        )
    else
        get_hash_table_type_2(Map, Index + 1, TableType)
    ).

%---------------------------------------------------------------------------%

    % Write out the array of pointers to the fact table arrays.
    %
:- pred write_fact_table_pointer_array(int::in, int::in, string::in,
    io.text_output_stream::in, string::out, io::di, io::uo) is det.

write_fact_table_pointer_array(NumFacts, FactTableSize,
        StructName, OutputStream, C_HeaderCode, !IO) :-
    PointerArrayName = "const struct " ++ StructName ++ "_struct *"
        ++ StructName ++ "[]",
    C_HeaderCode = "extern " ++ PointerArrayName ++ ";\n",
    io.write_strings(OutputStream, [PointerArrayName, " = {\n"], !IO),
    write_fact_table_pointer_array_2(0, NumFacts, FactTableSize,
        StructName, OutputStream, !IO),
    io.write_string(OutputStream, "};\n", !IO).

:- pred write_fact_table_pointer_array_2(int::in, int::in, int::in, string::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_fact_table_pointer_array_2(CurrFact, NumFacts, FactTableSize,
        StructName, OutputStream, !IO) :-
    ( if CurrFact >= NumFacts then
        true
    else
        io.format(OutputStream, "\t%s%d,\n",
            [s(StructName), i(CurrFact)], !IO),
        NextFact = CurrFact + FactTableSize,
        write_fact_table_pointer_array_2(NextFact, NumFacts, FactTableSize,
            StructName, OutputStream, !IO)
    ).

:- pred write_fact_table_numfacts(io.text_output_stream::in,
    sym_name::in, int::in, string::out, io::di, io::uo) is det.

write_fact_table_numfacts(OutputStream, PredName, NumFacts, C_HeaderCode,
        !IO) :-
    io.set_output_stream(OutputStream, OldOutputStream, !IO),
    % Write out the size of the fact table.
    make_fact_table_identifier(PredName, Identifier),
    io.write_strings(["const MR_Integer mercury__", Identifier,
        "_fact_table_num_facts = "], !IO),
    io.write_int(NumFacts, !IO),
    io.write_string(";\n\n", !IO),
    C_HeaderCode = "extern const MR_Integer mercury__" ++ Identifier
        ++ "_fact_table_num_facts;\n",
    io.set_output_stream(OldOutputStream, _, !IO).

%---------------------------------------------------------------------------%

:- pred make_fact_table_identifier(sym_name::in, string::out) is det.

make_fact_table_identifier(SymName, Identifier) :-
    Identifier = sym_name_mangle(SymName).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fact_table_generate_c_code(PredName, PragmaVars, ProcID, PrimaryProcID,
        ProcInfo, ArgTypes, ModuleInfo, ProcCode, ExtraCode, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    fact_table_size(Globals, FactTableSize),

    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_interface_determinism(ProcInfo, Determinism),
    fact_table_mode_type(ArgModes, ModuleInfo, ModeType),
    make_fact_table_identifier(PredName, Identifier),
    ( if
        ModeType = all_out,
        Determinism = detism_multi
    then
        generate_multidet_code(Identifier, PragmaVars, ProcID, ArgTypes,
            ModuleInfo, FactTableSize, ProcCode, ExtraCode)
    else if
        ModeType = all_out,
        Determinism = detism_cc_multi
    then
        generate_cc_multi_code(Identifier, PragmaVars, ProcCode),
        ExtraCode = ""
    else if
        ModeType = all_in,
        Determinism = detism_semi
    then
        generate_all_in_code(Identifier, PragmaVars, ProcID, ArgTypes,
            ModuleInfo, FactTableSize, ProcCode),
        ExtraCode = ""
    else if
        ModeType = in_out,
        ( Determinism = detism_semi
        ; Determinism = detism_cc_non
        )
    then
        generate_semidet_in_out_code(Identifier, PragmaVars, ProcID, ArgTypes,
            ModuleInfo, FactTableSize, ProcCode),
        ExtraCode = ""
    else if
        ModeType = in_out,
        Determinism = detism_non,
        ProcID = PrimaryProcID
    then
        generate_primary_nondet_code(Identifier, PragmaVars, ProcID, ArgTypes,
            ModuleInfo, FactTableSize, ProcCode, ExtraCode)
    else if
        ModeType = in_out,
        Determinism = detism_non,
        ProcID \= PrimaryProcID
    then
        generate_secondary_nondet_code(Identifier, PragmaVars, ProcID,
            ArgTypes, ModuleInfo, FactTableSize, ProcCode, ExtraCode)
    else
        % There is a determinism error in this procedure which will be
        % reported later on when the inferred determinism is compared
        % to the declared determinism. So all we need to do here is
        % return some C code that does nothing.

        % List the variables in the C code to stop the compiler giving
        % a warning about them not being there.
        pragma_vars_to_names_string(PragmaVars, NamesString),
        string.format("/* %s */", [s(NamesString)], ProcCode),
        ExtraCode = ""
    ).

%---------------------------------------------------------------------------%

:- pred generate_multidet_code(string::in, list(pragma_var)::in, proc_id::in,
    list(mer_type)::in, module_info::in, int::in, string::out, string::out)
    is det.

generate_multidet_code(PredName, PragmaVars, ProcID, ArgTypes,
        ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
        ProcCode),
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
        /* declare argument vars */
%s
        MR_Word ind = MR_framevar(1), tmp;
        /* lookup fact table */
%s
        /* save output args to registers */
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

    NumFactsVar = "mercury__" ++ PredName ++ "_fact_table_num_facts",
    list.length(PragmaVars, Arity),
    generate_argument_vars_code(PragmaVars, ArgTypes, ModuleInfo,
        ArgDeclCode, _InputCode, OutputCode, _, _, _),
    generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
        FactTableSize, FactLookupCode),

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

:- pred generate_nondet_proc_code(list(pragma_var)::in, string::in,
    proc_id::in, string::out, string::out) is det.

generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
        ProcCode) :-
    ProcCodeTemplate =  "

    /*
    ** Mention arguments %s to stop the compiler giving a warning.
    **
    ** Pop off the nondet stack frame that the pragma c_code generates
    ** then jump to the code where the work is actually done.
    */

    MR_maxfr_word = MR_prevfr_slot_word(MR_curfr);
    MR_curfr_word = MR_succfr_slot_word(MR_curfr);
    {
        MR_declare_entry(%s);
        MR_GOTO(MR_ENTRY(%s));
    }
    ",

    list.length(PragmaVars, Arity),
    proc_id_to_int(ProcID, ProcInt),
    string.format("mercury__%s_%d_%d_xx",
        [s(PredName), i(Arity), i(ProcInt)], ExtraCodeLabel),
    pragma_vars_to_names_string(PragmaVars, NamesString),
    string.format(ProcCodeTemplate, [s(NamesString), s(ExtraCodeLabel),
        s(ExtraCodeLabel)], ProcCode).

    % pragma_vars_to_names_string(PragmaVars, NamesString):
    %
    % Create a string containing the names of the pragma vars separated by
    % a space.
    %
:- pred pragma_vars_to_names_string(list(pragma_var)::in, string::out) is det.

pragma_vars_to_names_string([], "").
pragma_vars_to_names_string([pragma_var(_, Name, _, _) | PVars],
        NamesString) :-
    pragma_vars_to_names_string(PVars, NamesString0),
    string.append_list([Name, ", ", NamesString0], NamesString).

%---------------------------------------------------------------------------%

    % For cc_multi output mode, just return the first fact in the table.
    %
:- pred generate_cc_multi_code(string::in, list(pragma_var)::in, string::out)
    is det.

generate_cc_multi_code(PredName, PragmaVars, ProcCode) :-
    string.append_list(["mercury__", PredName, "_fact_table"], StructName),
    generate_cc_multi_code_2(PragmaVars, StructName, 1, "", ProcCode).

:- pred generate_cc_multi_code_2(list(pragma_var)::in, string::in, int::in,
    string::in, string::out) is det.

generate_cc_multi_code_2([], _, _, !ProcCode).
generate_cc_multi_code_2([pragma_var(_, VarName, _, _) | PragmaVars],
        StructName, ArgNum, !ProcCode) :-
    string.format("\t\t%s = %s[0][0].V_%d;\n", [s(VarName), s(StructName),
        i(ArgNum)], NewProcCode),
    string.append(NewProcCode, !ProcCode),
    generate_cc_multi_code_2(PragmaVars, StructName, ArgNum + 1, !ProcCode).

%---------------------------------------------------------------------------%

    % Generate semidet code for all_in mode.
    %
:- pred generate_all_in_code(string::in, list(pragma_var)::in, proc_id::in,
    list(mer_type)::in, module_info::in, int::in, string::out) is det.

generate_all_in_code(PredName, PragmaVars, ProcID, ArgTypes, ModuleInfo,
        FactTableSize, ProcCode) :-
    generate_decl_code(PredName, ProcID, DeclCode),

    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
        PredName, 1, FactTableSize, HashCode),

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

%---------------------------------------------------------------------------%

    % Generate code for semidet and cc_nondet in_out modes. Lookup key in
    % hash table and if found return first match. If not found, fail.
    %
:- pred generate_semidet_in_out_code(string::in, list(pragma_var)::in,
    proc_id::in, list(mer_type)::in, module_info::in, int::in, string::out)
    is det.

generate_semidet_in_out_code(PredName, PragmaVars, ProcID, ArgTypes,
        ModuleInfo, FactTableSize, ProcCode):-
    generate_decl_code(PredName, ProcID, DeclCode),

    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
        PredName, 1, FactTableSize, HashCode),

    SuccessCodeTemplate = "
        success_code_%s:
            SUCCESS_INDICATOR = MR_TRUE;
    ",
    string.format(SuccessCodeTemplate, [s(LabelName)], SuccessCode),

    generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
        FactTableSize, FactLookupCode),

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
% Some code generation procedures used by various modes.

:- pred generate_decl_code(string::in, proc_id::in, string::out) is det.

generate_decl_code(Name, ProcID, DeclCode) :-
    DeclCodeTemplate = "
            MR_Integer hashval, hashsize;
            MR_Word ind;
            void *current_table;
            char keytype = '\\0';
            MR_Word current_key, tmp;

            /*
            ** Initialise current_table to the top level hash table
            ** for this ProcID.
            */
            current_table =
                &mercury__%s_fact_table_hash_table_%d_0;

    ",
    proc_id_to_int(ProcID, ProcInt),
    string.format(DeclCodeTemplate, [s(Name), i(ProcInt)], DeclCode).

    % Generate code to calculate hash values and lookup the hash tables.
    %
:- pred generate_hash_code(list(pragma_var)::in, list(mer_type)::in,
    module_info::in, string::in, int::in, string::in, int::in, int::in,
    string::out) is det.

generate_hash_code([], [], _, _, _, _, _, _, "").
generate_hash_code([], [_ | _], _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "length mismatch").
generate_hash_code([_ | _], [], _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "length mismatch").
generate_hash_code([pragma_var(_, Name, Mode, _) | PragmaVars], [Type | Types],
        ModuleInfo, LabelName, LabelNum, PredName, ArgNum,
        FactTableSize, C_Code) :-
    NextArgNum = ArgNum + 1,
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ( if Type = builtin_type(builtin_type_int) then
            generate_hash_int_code(Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, ModuleInfo,
                NextArgNum, FactTableSize, C_Code0)
        else if Type = builtin_type(builtin_type_float) then
            generate_hash_float_code(Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, ModuleInfo,
                NextArgNum, FactTableSize, C_Code0)
        else if Type = builtin_type(builtin_type_string) then
            generate_hash_string_code(Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, ModuleInfo,
                NextArgNum, FactTableSize, C_Code0)
        else
            unexpected($module, $pred, "unsupported type")
        ),
        generate_hash_code(PragmaVars, Types, ModuleInfo, LabelName,
            LabelNum + 1, PredName, NextArgNum, FactTableSize,
            C_Code1),
        string.append(C_Code0, C_Code1, C_Code)
    else
        % Skip non-input arguments.
        generate_hash_code(PragmaVars, Types, ModuleInfo, LabelName,
            LabelNum, PredName, NextArgNum, FactTableSize, C_Code)
    ).

:- pred generate_hash_int_code(string::in, string::in, int::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, module_info::in,
    int::in, int::in, string::out) is det.

generate_hash_int_code(Name, LabelName, LabelNum, PredName, PragmaVars,
        Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
    generate_hash_lookup_code(Name, LabelName, LabelNum, plain_equals, 'i',
        yes, PredName, PragmaVars, Types, ModuleInfo, ArgNum,
        FactTableSize, HashLookupCode),
    C_Code_Template = "

        /* calculate hash value for an integer */

        hashsize = ((struct MR_fact_table_hash_table_i *)current_table)
            ->size;

        hashval = (%s >= 0 ? %s : -%s) %% hashsize;

        current_key = %s;

        /* lookup the hash table */
        %s

    ",
    string.format(C_Code_Template, [s(Name), s(Name), s(Name), s(Name),
        s(HashLookupCode)], C_Code).

:- pred generate_hash_float_code(string::in, string::in, int::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, module_info::in,
    int::in, int::in, string::out) is det.

generate_hash_float_code(Name, LabelName, LabelNum, PredName, PragmaVars,
        Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
    generate_hash_lookup_code(Name, LabelName, LabelNum, plain_equals, 'f',
        yes, PredName, PragmaVars, Types, ModuleInfo, ArgNum,
        FactTableSize, HashLookupCode),
    C_Code_Template = "

        /* calculate hash value for a float */

        hashsize = ((struct MR_fact_table_hash_table_f *)current_table)
            ->size;

        hashval = MR_hash_float(%s);
        hashval = (hashval >= 0 ? hashval : -hashval) %% hashsize;

        current_key = MR_float_to_word(%s);

        /* lookup the hash table */
        %s

    ",
    string.format(C_Code_Template, [s(Name), s(Name), s(HashLookupCode)],
        C_Code).

:- pred generate_hash_string_code(string::in, string::in, int::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, module_info::in,
    int::in, int::in, string::out) is det.

generate_hash_string_code(Name, LabelName, LabelNum, PredName, PragmaVars,
        Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
    generate_hash_lookup_code(Name, LabelName, LabelNum,
        string_equals, 's', yes, PredName, PragmaVars,
        Types, ModuleInfo, ArgNum, FactTableSize, HashLookupCode),
    C_Code_Template = "

        hashsize = ((struct MR_fact_table_hash_table_s *) current_table)->size;

        /* calculate hash value for a string */
        {
            char *p;
            hashval = 0;
            for (p = %s ; *p != '\\0' ; p++) {
                hashval = (*p + 31 * hashval) %% hashsize;
            }
        }

        current_key = (MR_Word) %s;

        /* lookup the hash table */
        %s

    ",
    string.format(C_Code_Template, [s(Name), s(Name), s(HashLookupCode)],
        C_Code).

:- type comparison_kind
    --->    plain_equals
    ;       string_equals.

    % Generate code to lookup the key in the hash table.
    % KeyType should be 's', 'i' or 'f' for string, int or float,
    % respectively. CompareTemplate should be a template for testing for
    % equality for the type given, e.g. "%s == %s" for ints,
    % "strcmp(%s, %s) == 0" for strings.
    %
:- pred generate_hash_lookup_code(string::in, string::in, int::in,
    comparison_kind::in, char::in, bool::in, string::in, list(pragma_var)::in,
    list(mer_type)::in, module_info::in, int::in, int::in, string::out) is det.

generate_hash_lookup_code(VarName, LabelName, LabelNum, ComparisonKind,
        KeyType, CheckKeys, PredName, PragmaVars, Types,
        ModuleInfo, ArgNum, FactTableSize, HashLookupCode) :-
    string.format("((struct MR_fact_table_hash_table_%c *) current_table)"
        ++ "->table[hashval]", [c(KeyType)], HashTableEntry),
    string.append(HashTableEntry, ".key", HashTableKey),
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
            if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) != 0 && %s)
            {
                ind = (MR_Word) %s.index;
                goto found_%s_%d;
            }
        } while ((hashval = %s.next) != -1);

        /* key not found */
        goto failure_code_%s;

    found_%s_%d:

        if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) == 1) {
            ind = MR_FACT_TABLE_HASH_INDEX(ind);

            /* check that any remaining input arguments match */
            %s
            keytype = '%c';
            hashval = %s.next;
            goto success_code_%s;
        }

        current_table = (void *) MR_FACT_TABLE_HASH_POINTER(ind);

    ",
    (
        CheckKeys = yes,
        FactTableName = "mercury__" ++ PredName ++ "_fact_table",
        generate_test_condition_code(FactTableName, PragmaVars, Types,
            ModuleInfo, ArgNum, yes, FactTableSize, CondCode),
        ( if CondCode = "" then
            TestCode = ""
        else
            TestCodeTemplate = "if (%s\t\t\t) goto failure_code_%s;\n",
            string.format(TestCodeTemplate, [s(CondCode), s(LabelName)],
                TestCode)
        )
    ;
        CheckKeys = no,
        TestCode = ""
    ),

    string.format(HashLookupCodeTemplate, [s(HashTableEntry),
        s(CompareString), s(HashTableEntry), s(LabelName), i(LabelNum),
        s(HashTableEntry), s(LabelName), s(LabelName), i(LabelNum),
        s(HashTableEntry), s(TestCode), c(KeyType),
        s(HashTableEntry), s(LabelName)],
        HashLookupCode).

    % Generate code to lookup the fact table with a given index
    %
:- pred generate_fact_lookup_code(string::in, list(pragma_var)::in,
    list(mer_type)::in, module_info::in, int::in, int::in, string::out) is det.

generate_fact_lookup_code(_, [], [], _, _, _, "").
generate_fact_lookup_code(_, [_ | _], [], _, _, _, _) :-
    unexpected($module, $pred, "too many pragma vars").
generate_fact_lookup_code(_, [], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "too many types").
generate_fact_lookup_code(PredName,
        [pragma_var(_, VarName, Mode, _) | PragmaVars],
        [Type | Types], ModuleInfo, ArgNum, FactTableSize, C_Code) :-
    NextArgNum = ArgNum + 1,
    ( if mode_is_fully_output(ModuleInfo, Mode) then
        TableEntryTemplate = "mercury__%s_fact_table[ind/%d][ind%%%d].V_%d",
        string.format(TableEntryTemplate,
            [s(PredName), i(FactTableSize), i(FactTableSize), i(ArgNum)],
            TableEntry),
        ( if Type = builtin_type(builtin_type_string) then
            mode_get_insts(ModuleInfo, Mode, _, FinalInst),
            ( if inst_is_not_partly_unique(ModuleInfo, FinalInst) then
                % Cast MR_ConstString -> MR_Word -> MR_String to avoid gcc
                % warning "assignment discards `const'".
                Template = "\t\tMR_make_aligned_string(%s, " ++
                    "(MR_String) (MR_Word) %s);\n",
                string.format(Template, [s(VarName), s(TableEntry)], C_Code0)
            else
                % Unique modes need to allow destructive
                % update so we need to make a copy of the
                % string on the heap.
                Template =
                    "       MR_incr_hp_atomic(tmp,
                                (strlen(%s) + sizeof(MR_Word))
                                    / sizeof(MR_Word));
                            %s = (MR_String) tmp;
                            strcpy(%s, %s);
                    ",
                string.format(Template,
                    [s(TableEntry), s(VarName), s(VarName), s(TableEntry)],
                    C_Code0)
            )
        else
            Template = "\t\t%s = %s;\n",
            string.format(Template, [s(VarName), s(TableEntry)], C_Code0)
        ),
        generate_fact_lookup_code(PredName, PragmaVars, Types,
            ModuleInfo, NextArgNum, FactTableSize, C_Code1),
        string.append(C_Code0, C_Code1, C_Code)
    else
        % Skip non-output arguments.
        generate_fact_lookup_code(PredName, PragmaVars, Types,
            ModuleInfo, NextArgNum, FactTableSize, C_Code)
    ).

%---------------------------------------------------------------------------%
%
% Code for lookup in nondet modes.

    % Generate code for the nondet mode with the primary key.
    %
:- pred generate_primary_nondet_code(string::in, list(pragma_var)::in,
    proc_id::in, list(mer_type)::in, module_info::in, int::in,
    string::out, string::out) is det.

generate_primary_nondet_code(PredName, PragmaVars, ProcID, ArgTypes,
        ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
        ProcCode),
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
        /* create argument vars */
%s
        /* declare local variables */
%s
        /* copy registers to input arg vars */
%s
        /* copy registers to framevars */
%s
        /* lookup hash table */
%s
    success_code_%s:
        /* lookup fact table */
%s
        /* save output args to registers */
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
        /* create argument vars */
%s
        int ind = MR_framevar(1);
        /* copy framevars to registers */
%s
        /* copy registers to input arg vars */
%s
        /* test fact table entry */
%s
        /* lookup fact table */
%s
        /* save output args to registers */
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

    generate_argument_vars_code(PragmaVars, ArgTypes,
        ModuleInfo, ArgDeclCode, InputCode, OutputCode, SaveRegsCode,
        GetRegsCode, NumFrameVars),
    generate_decl_code(PredName, ProcID, DeclCode),
    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
        PredName, 1, FactTableSize, HashCode),
    generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
        FactTableSize, FactLookupCode),
    generate_fact_test_code(PredName, PragmaVars, ArgTypes, ModuleInfo,
        FactTableSize, FactTestCode),

    string.append_list(["mercury__", PredName, "_fact_table_num_facts"],
        NumFactsVar),
    list.length(PragmaVars, Arity),

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

    % Generate code to create argument variables and assign them to registers.
    %
:- pred generate_argument_vars_code(list(pragma_var)::in, list(mer_type)::in,
    module_info::in, string::out, string::out, string::out,
    string::out, string::out, int::out) is det.

generate_argument_vars_code(PragmaVars, Types, ModuleInfo, DeclCode, InputCode,
        OutputCode, SaveRegsCode, GetRegsCode, NumInputArgs) :-
    list.map((pred(X::in, Y::out) is det :- X = pragma_var(_, _, Y, _)),
        PragmaVars, Modes),
    make_standard_arg_infos(Types, Modes, model_non, ModuleInfo, ArgInfos),
    generate_argument_vars_code_2(PragmaVars, ArgInfos, Types, ModuleInfo,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode, 1,
        NumInputArgs).

:- pred generate_argument_vars_code_2(list(pragma_var)::in, list(arg_info)::in,
    list(mer_type)::in, module_info::in, string::out, string::out, string::out,
    string::out, string::out, int::in, int::out) is det.

generate_argument_vars_code_2(PragmaVars0, ArgInfos0, Types0, Module, DeclCode,
        InputCode, OutputCode, SaveRegsCode, GetRegsCode, !NumInputArgs) :-
    ( if
        PragmaVars0 = [],
        ArgInfos0 = [],
        Types0 = []
    then
        DeclCode = "",
        InputCode = "",
        OutputCode = "",
        SaveRegsCode = "",
        GetRegsCode = ""
    else if
        PragmaVars0 = [pragma_var(_, VarName, _, _) | PragmaVars],
        ArgInfos0 = [arg_info(Loc, ArgMode) | ArgInfos],
        Types0 = [Type | Types]
    then
        generate_arg_decl_code(VarName, Type, Module, DeclCode0),
        ( if ArgMode = top_in then
            !:NumInputArgs = !.NumInputArgs + 1,
            generate_arg_input_code(VarName, Type, Loc, !.NumInputArgs,
                InputCode0, SaveRegsCode0, GetRegsCode0),
            OutputCode0 = ""
        else if ArgMode = top_out then
            generate_arg_output_code(VarName, Type, Loc, OutputCode0),
            InputCode0 = "",
            SaveRegsCode0 = "",
            GetRegsCode0 = ""
        else
            unexpected($module, $pred, "invalid mode")
        ),
        generate_argument_vars_code_2(PragmaVars, ArgInfos, Types, Module,
            DeclCode1, InputCode1, OutputCode1, SaveRegsCode1, GetRegsCode1,
            !NumInputArgs),
        DeclCode = DeclCode0 ++ DeclCode1,
        InputCode = InputCode0 ++ InputCode1,
        OutputCode = OutputCode0 ++ OutputCode1,
        SaveRegsCode = SaveRegsCode0 ++ SaveRegsCode1,
        GetRegsCode = GetRegsCode0 ++ GetRegsCode1
    else
        unexpected($module, $pred, "list length mismatch")
    ).

:- pred generate_arg_decl_code(string::in, mer_type::in, module_info::in,
    string::out) is det.

generate_arg_decl_code(Name, Type, Module, DeclCode) :-
    C_Type = mercury_exported_type_to_string(Module, lang_c, Type),
    string.format("\t\t%s %s;\n", [s(C_Type), s(Name)], DeclCode).

:- pred generate_arg_input_code(string::in, mer_type::in, arg_loc::in, int::in,
    string::out, string::out, string::out) is det.

generate_arg_input_code(Name, Type, ArgLoc, FrameVarNum, InputCode,
        SaveRegCode, GetRegCode) :-
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
    RegName = reg_to_string(RegType, RegNum),
    convert_type_from_mercury(ArgLoc, RegName, Type, Converted),
    Template = "\t\t%s = %s;\n",
    string.format(Template, [s(Name), s(Converted)], InputCode),
    string.format("\t\tMR_framevar(%d) = %s(%s);\n",
        [i(FrameVarNum), s(ConvertToFrameVar), s(RegName)], SaveRegCode),
    string.format("\t\t%s = %s(MR_framevar(%d));\n",
        [s(RegName), s(ConvertFromFrameVar), i(FrameVarNum)], GetRegCode).

:- pred generate_arg_output_code(string::in, mer_type::in, arg_loc::in,
    string::out) is det.

generate_arg_output_code(Name, Type, ArgLoc, OutputCode) :-
    ArgLoc = reg(RegType, RegNum),
    RegName = reg_to_string(RegType, RegNum),
    convert_type_to_mercury(Name, Type, ArgLoc, Converted),
    Template = "\t\t%s = %s;\n",
    string.format(Template, [s(RegName), s(Converted)], OutputCode).

    % Generate code to test that the fact found matches the input arguments.
    % This is only required for generate_primary_nondet_code. Other procedures
    % can test the key in the hash table against the input arguments.
    %
:- pred generate_fact_test_code(string::in, list(pragma_var)::in,
    list(mer_type)::in, module_info::in, int::in, string::out) is det.

generate_fact_test_code(PredName, PragmaVars, ArgTypes, ModuleInfo,
        FactTableSize, FactTestCode) :-
    FactTableName = "mercury__" ++ PredName ++ "_fact_table",
    generate_test_condition_code(FactTableName, PragmaVars, ArgTypes,
        ModuleInfo, 1, yes, FactTableSize, CondCode),
    FactTestCode = "\t\tif(" ++ CondCode ++ "\t\t) MR_fail();\n".

:- pred generate_test_condition_code(string::in, list(pragma_var)::in,
    list(mer_type)::in, module_info::in, int::in, bool::in, int::in,
    string::out) is det.

generate_test_condition_code(_, [], [], _, _, _, _, "").
generate_test_condition_code(_, [_ | _], [], _, _, _, _, "") :-
    unexpected($module, $pred, "too many PragmaVars").
generate_test_condition_code(_, [], [_ | _], _, _, _, _, "") :-
    unexpected($module, $pred, "too many ArgTypes").
generate_test_condition_code(FactTableName, [PragmaVar | PragmaVars],
        [Type | Types], ModuleInfo, ArgNum, !.IsFirstInputArg,
        FactTableSize, CondCode) :-
    PragmaVar = pragma_var(_, Name, Mode, _),
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ( if Type = builtin_type(builtin_type_string) then
            Template = "strcmp(%s[ind/%d][ind%%%d].V_%d, %s) != 0\n",
            string.format(Template, [s(FactTableName), i(FactTableSize),
                i(FactTableSize), i(ArgNum), s(Name)], CondCode0)
        else
            Template = "%s[ind/%d][ind%%%d].V_%d != %s\n",
            string.format(Template, [s(FactTableName), i(FactTableSize),
                i(FactTableSize), i(ArgNum), s(Name)], CondCode0)
        ),
        (
            !.IsFirstInputArg = no,
            CondCode1 = "\t\t|| " ++ CondCode0
        ;
            !.IsFirstInputArg = yes,
            CondCode1 = CondCode0
        ),
        !:IsFirstInputArg = no
    else
        CondCode1 = ""
    ),
    generate_test_condition_code(FactTableName, PragmaVars, Types, ModuleInfo,
        ArgNum + 1, !.IsFirstInputArg, FactTableSize, CondCode2),
    CondCode = CondCode1 ++ CondCode2.

    % Generate code for a nondet mode using a secondary key.

:- pred generate_secondary_nondet_code(string::in, list(pragma_var)::in,
    proc_id::in, list(mer_type)::in, module_info::in, int::in,
    string::out, string::out) is det.

generate_secondary_nondet_code(PredName, PragmaVars, ProcID, ArgTypes,
        ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
    generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
        ProcCode),
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
        /* create argument vars */
%s
        /* declare local variables */
%s
        /* copy registers to input arg vars */
%s
        /* lookup hash table */
%s
    success_code_%s:
        /* lookup fact table */
%s
        /* save output args to registers */
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
        /* create argument vars */
%s
        MR_Integer  hashval = MR_framevar(1);
        MR_Word     ind;
        void        *current_table = (void *) MR_framevar(2);
        char        keytype = (char) MR_framevar(3);

        /* lookup hash table */
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
        /* lookup fact table */
%s
        /* save output args to registers */
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

    generate_argument_vars_code(PragmaVars, ArgTypes, ModuleInfo, ArgDeclCode,
        InputCode, OutputCode, _SaveRegsCode, _GetRegsCode, _NumFrameVars),
    generate_decl_code(PredName, ProcID, DeclCode),
    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    string.append(LabelName, "_2", LabelName2),
    generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
        PredName, 1, FactTableSize, HashCode),

    generate_hash_lookup_code("(char *) MR_framevar(4)", LabelName2, 0,
        string_equals, 's', no, "", [], [], ModuleInfo, 0, 0,
        StringHashLookupCode),
    generate_hash_lookup_code("MR_framevar(4)", LabelName2, 1, plain_equals,
        'i', no, "", [], [], ModuleInfo, 0, 0, IntHashLookupCode),
    generate_hash_lookup_code("MR_word_to_float(MR_framevar(4))",
        LabelName2, 2, plain_equals, 'f', no, "", [], [], ModuleInfo,
        0, 0, FloatHashLookupCode),
    generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
        FactTableSize, FactLookupCode),
    list.length(PragmaVars, Arity),

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

    % Delete a file. Report an error message if something goes wrong.
    %
:- pred delete_temporary_file(globals::in, string::in, io::di, io::uo) is det.

delete_temporary_file(Globals, FileName, !IO) :-
    io.remove_file(FileName, Result, !IO),
    (
        Result = ok
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMsg),
        io.progname_base("mercury_compile", ProgName, !IO),
        string.format("%s: error deleting file `%s:",
            [s(ProgName), s(FileName)], Msg),
        Pieces = [words(Msg), nl, words(ErrorMsg), nl],
        write_error_pieces_plain(Globals, Pieces, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred write_call_system_error_msg(globals::in, string::in, io.error::in,
    io::di, io::uo) is det.

write_call_system_error_msg(Globals, Cmd, ErrorCode, !IO) :-
    io.error_message(ErrorCode, ErrorMsg),
    io.progname_base("mercury_compile", ProgName, !IO),
    string.format("%s: error executing system command `%s:",
        [s(ProgName), s(Cmd)], Msg),
    write_error_pieces_plain(Globals, [words(Msg), nl, words(ErrorMsg)], !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

    % We keep error reports in reverse order to keep N calls to
    % add_error_report at complexity N, rather than N*N. We reverse them
    % before printing.

:- type error_report    == pair(maybe(context), list(format_component)).
:- type error_reports   == list(error_report).

:- pred add_error_report(context::in, list(format_component)::in,
    error_reports::in, error_reports::out) is det.

add_error_report(Context, Pieces, !Errors) :-
    !:Errors = [yes(Context) - Pieces | !.Errors].

:- pred add_error_report(list(format_component)::in,
    error_reports::in, error_reports::out) is det.

add_error_report(Pieces, !Errors) :-
    !:Errors = [no - Pieces | !.Errors].

:- pred print_error_reports(globals::in, error_reports::in, io::di, io::uo)
    is det.

print_error_reports(Globals, RevErrors, !IO) :-
    list.reverse(RevErrors, Errors),
    list.foldl(print_error_report(Globals), Errors, !IO).

:- pred print_error_report(globals::in, error_report::in, io::di, io::uo)
    is det.

print_error_report(Globals, MaybeContext - Pieces, !IO) :-
    (
        MaybeContext = yes(Context),
        write_error_pieces(Globals, Context, 0, Pieces, !IO)
    ;
        MaybeContext = no,
        write_error_pieces_plain(Globals, Pieces, !IO)
    ),
    io.set_exit_status(1, !IO).

:- pred print_file_open_error(globals::in, maybe(context)::in, string::in,
    string::in, io.error::in, io::di, io::uo) is det.

print_file_open_error(Globals, MaybeContext, FileName, InOrOut, Error, !IO) :-
    io.error_message(Error, ErrorMsg),
    string.format("Error opening file `%s' for %s:",
        [s(FileName), s(InOrOut)], Msg),
    Pieces = [words(Msg), nl, words(ErrorMsg), nl],
    print_error_report(Globals, MaybeContext - Pieces, !IO).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.fact_table.
%-----------------------------------------------------------------------------%
