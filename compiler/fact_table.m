%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module ll_backend.fact_table.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % fact_table_compile_facts(ModuleInfo, FileName, Context,
    %   C_HeaderCode, PrimaryProcID, !PredInfo, !IO):
    %
    % Compile the fact table for PredInfo into a separate .c file.
    %
:- pred fact_table_compile_facts(module_info::in, string::in, prog_context::in,
    string::out, proc_id::out, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

    % fact_table_generate_c_code(ModuleInfo, PredName, ProcID, PrimaryProcID,
    %   ProcInfo, PragmaVars, ArgTypes, C_ProcCode, C_ExtraCode, !IO):
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
:- pred fact_table_generate_c_code(module_info::in, sym_name::in,
    proc_id::in, proc_id::in, proc_info::in,
    list(pragma_var)::in, list(mer_type)::in, string::out, string::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
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
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_llds.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_foreign.
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

%---------------------------------------------------------------------------%

:- type fact_result
    --->    ok
    ;       error.

    % Proc_stream contains information about an open sort file for
    % a particular procedure.
    %
:- type proc_stream
    --->    proc_stream(
                proc_id,                % ID of procedure.
                io.text_output_stream   % Sort file stream.
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

% XXX CLEANUP We should replace this with a type that has one functor
% for each of the allowed types of argument.
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
    ;       no_args.    % There are no arguments.

:- type inferred_determinism
    --->    inferred(determinism)   % Determinism has been inferred.
    ;       not_yet                 % Determinism has not yet been inferred.
    ;       error.                  % An error occurred trying to infer
                                    % determinism.

:- type fact_arg_info
    --->    fact_arg_info(
                fact_arg_type,      % Type of the argument.
                bool,               % Is an input argument for some mode.
                bool                % Is an output argument for some mode.
            ).

    % XXX UINT - handle uints here too when we support them in fact tables.
:- type fact_arg_type
    --->    fact_arg_type_int
    ;       fact_arg_type_float
    ;       fact_arg_type_string.

%---------------------------------------------------------------------------%

fact_table_compile_facts(ModuleInfo, FileName, Context,
        HeaderCode, PrimaryProcID, !PredInfo, !Specs, !IO) :-
    pred_info_get_arg_types(!.PredInfo, Types),
    init_fact_arg_infos(!.PredInfo, Types, FactArgInfos0, [], ArgTypeSpecs),
    (
        ArgTypeSpecs = [_ | _],
        % If the predicate has argument types that are not suitable for
        % tabling, then there is no point in trying to implement the fact
        % table, since the effort is doomed to failure.
        HeaderCode = "",
        PrimaryProcID = invalid_proc_id,
        !:Specs = ArgTypeSpecs ++ !.Specs
    ;
        ArgTypeSpecs = [],
        module_info_get_globals(ModuleInfo, Globals),
        io.open_input(FileName, FileResult, !IO),
        (
            FileResult = ok(FileStream),
            fact_table_file_name(Globals, $pred, do_create_dirs,
                other_ext(".c"), FileName, OutputFileName, !IO),
            io.open_output(OutputFileName, OpenResult, !IO),
            (
                OpenResult = ok(OutputStream),
                pred_info_get_module_name(!.PredInfo, ModuleName),
                pred_info_get_name(!.PredInfo, PredName),
                PredSymName = qualified(ModuleName, PredName),
                fact_table_size(Globals, FactTableSize),
                compile_fact_table_in_file(FileStream, FileName, OutputStream,
                    FactTableSize, ModuleInfo, PredSymName, FactArgInfos0,
                    Context, HeaderCode, PrimaryProcID, MaybeDataFileName,
                    !PredInfo, !Specs, !IO),
                io.close_output(OutputStream, !IO),
                (
                    MaybeDataFileName = no
                ;
                    MaybeDataFileName = yes(DataFileName),
                    append_data_table(ModuleInfo, OutputFileName, DataFileName,
                        !Specs, !IO)
                )
            ;
                OpenResult = error(Error),
                add_file_open_error(yes(Context), FileName, "output", Error,
                    !Specs, !IO),
                HeaderCode = "",
                PrimaryProcID = invalid_proc_id
            ),
            io.close_input(FileStream, !IO)
        ;
            FileResult = error(Error),
            add_file_open_error(yes(Context), FileName, "input", Error,
                !Specs, !IO),
            HeaderCode = "",
            PrimaryProcID = invalid_proc_id
        )
    ).

:- pred compile_fact_table_in_file(io.text_input_stream::in, string::in,
    io.text_output_stream::in, int::in, module_info::in,
    sym_name::in, list(fact_arg_info)::in, prog_context::in,
    string::out, proc_id::out, maybe(string)::out,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

compile_fact_table_in_file(FileStream, FileName, OutputStream, FactTableSize,
        ModuleInfo, PredSymName, FactArgInfos0, Context,
        HeaderCode, PrimaryProcID, MaybeDataFileName,
        !PredInfo, !Specs, !IO) :-
    infer_determinism_pass_1(ModuleInfo, Context, CheckProcs,
        ExistsAllInMode, WriteHashTables, WriteDataTable,
        FactArgInfos0, FactArgInfos, !PredInfo, [], Pass1Specs),
    create_fact_table_header(PredSymName, FactArgInfos,
        HeaderCode0, StructName),
    (
        Pass1Specs = [],
        io.write_string(OutputStream, fact_table_file_header(FileName), !IO),
        io.write_string(OutputStream, HeaderCode0, !IO),
        open_sort_files(CheckProcs, ProcStreams, [], OpenSpecs, !IO),
        % As the documentation on the fact_table_size predicate says,
        % we impose a limit on the size of the arrays we generate.
        % The data table may need to be broken into pieces to respect
        % this limit.
        %
        % The nice way to do this would be to
        %
        % - generate all the array entries we need,
        % - break them into chunks that respect the maximum size, and then
        % - write out each chunk with its prologue and epilogue, with
        %   the prologue containing the start of the subarray's definition,
        %   including an opening brace, and the prologue containing
        %   the matching close brace and the final semicolon.
        %
        % This two level loop (first over the chunks, then over the entries
        % in each chunk) is nice because there is a natural place to put
        % the prologues and epilogues.
        %
        % Unfortunately, this design also requires the whole array to be
        % in memory at the same time. Since fact tables may be extremely large,
        % we want to avoid this. We therefore adopt a cruder approach using
        % a single loop over the entries. In this approach, most loop
        % iterations just process one entry, but when the entry is the last
        % entry in what *would have been* a chunk, we also output the epilogue,
        % and if there are any more entries after it, then the prologue as
        % well. This requires us to handle the initial prologue here,
        % and possibly the final epilogue as well.
        (
            WriteDataTable = write_data_table,
            (
                CheckProcs = [],
                MaybeOutput = yes(OutputStream - StructName),
                % Outputs opening brace for first fact array.
                write_new_data_array_opening_brace(OutputStream, StructName,
                    0, !IO),
                WriteDataAfterSorting = do_not_write_data_table
            ;
                CheckProcs = [_ | _],
                MaybeOutput = no,
                WriteDataAfterSorting = write_data_table
            )
        ;
            WriteDataTable = do_not_write_data_table,
            MaybeOutput = no,
            WriteDataAfterSorting = do_not_write_data_table
        ),
        get_maybe_progress_output_stream(ModuleInfo, MaybeProgressStream, !IO),
        FactNum0 = 0,
        CompileSpecs0 = [],
        read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
            FactTableSize, ModuleInfo, !.PredInfo, FactArgInfos,
            ProcStreams, MaybeOutput, FactNum0, FactNum,
            CompileSpecs0, CompileSpecs, !IO),
        NumFacts = FactNum,
        (
            MaybeOutput = yes(_),
            % Outputs closing brace for last fact array.
            write_closing_brace(OutputStream, !IO),
            write_fact_table_pointer_array(OutputStream, FactTableSize,
                StructName, NumFacts, HeaderCode2, !IO)
        ;
            MaybeOutput = no,
            HeaderCode2 = ""

        ),
        close_sort_files(ProcStreams, ProcFiles, !IO),
        OpenCompileSpecs = OpenSpecs ++ CompileSpecs,
        (
            OpenCompileSpecs = [],
            pred_info_get_proc_table(!.PredInfo, ProcTable0),
            infer_determinism_pass_2(ModuleInfo, ExistsAllInMode, ProcFiles,
                ProcTable0, ProcTable, !Specs, !IO),
            pred_info_set_proc_table(ProcTable, !PredInfo),
            io.make_temp_file(DataFileNameResult, !IO),
            (
                DataFileNameResult = ok(DataFileName),
                write_fact_table_arrays(OutputStream, FactTableSize,
                    ModuleInfo, ProcFiles, DataFileName, ProcTable,
                    StructName, NumFacts, FactArgInfos,
                    WriteHashTables, WriteDataAfterSorting,
                    HeaderCode1, PrimaryProcID, !Specs, !IO),
                write_fact_table_numfacts(OutputStream, PredSymName, NumFacts,
                    HeaderCode3, !IO),
                HeaderCode = 
                    HeaderCode0 ++ HeaderCode1 ++ HeaderCode2 ++ HeaderCode3,
                MaybeDataFileName = yes(DataFileName)
            ;
                DataFileNameResult = error(Error),
                TmpPieces = [words("Could not create temporary file:"),
                    quote(io.error_message(Error)), nl],
                add_error_pieces(TmpPieces, !Specs),
                HeaderCode = HeaderCode0,
                PrimaryProcID = invalid_proc_id,
                MaybeDataFileName = no
            )
        ;
            OpenCompileSpecs = [_ | _],
            !:Specs = OpenCompileSpecs ++ !.Specs,
            HeaderCode = HeaderCode0,
            PrimaryProcID = invalid_proc_id,
            MaybeDataFileName = no
        )
    ;
        Pass1Specs = [_ | _],
        % Either there are no modes declared for this fact table or the
        % `:- pred' or `:- func' declaration had some types that are not
        % supported in fact tables so there is no point trying to type-check
        % all the facts.
        !:Specs = Pass1Specs ++ !.Specs,
        HeaderCode = HeaderCode0,
        PrimaryProcID = invalid_proc_id,
        MaybeDataFileName = no
    ).

%---------------------------------------------------------------------------%

:- type all_in_mode
    --->    all_in_mode_does_not_exist
    ;       all_in_mode_does_exist.

:- type maybe_write_hash_tables
    --->    do_not_write_hash_tables
    ;       write_hash_tables.

:- type maybe_write_data_table
    --->    do_not_write_data_table
    ;       write_data_table.

    % First pass of determinism inference. (out, out, ..., out) procs are
    % multi and (in, in, .., in) procs are semidet. Return a list of procs
    % containing both in's and out's. These need further analysis later
    % in pass 2.
    %
:- pred infer_determinism_pass_1(module_info::in, prog_context::in,
    list(proc_id)::out, all_in_mode::out,
    maybe_write_hash_tables::out, maybe_write_data_table::out,
    list(fact_arg_info)::in, list(fact_arg_info)::out,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

infer_determinism_pass_1(ModuleInfo, Context, CheckProcs, AllInMode,
        WriteHashTables, WriteDataTable, !FactArgInfos, !PredInfo, !Specs) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    ProcIDs = pred_info_all_procids(!.PredInfo),
    (
        ProcIDs = [],
        % There are no declared modes, so report an error.
        PredPieces = describe_one_pred_info_name(should_not_module_qualify,
            !.PredInfo),
        Pieces = [words("Error: no modes declared for fact table")
            | PredPieces] ++ [suffix("."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs),
        CheckProcs = [],
        AllInMode = all_in_mode_does_not_exist,
        WriteHashTables = do_not_write_hash_tables,
        WriteDataTable = do_not_write_data_table
    ;
        ProcIDs = [_ | _],
        infer_procs_determinism_pass_1(ModuleInfo, ProcIDs, MaybeAllInProc,
            ProcTable0, ProcTable, [], CheckProcs0, !FactArgInfos,
            do_not_write_hash_tables, WriteHashTables,
            do_not_write_data_table, WriteDataTable, !Specs),

        % If there is an all_in procedure, it needs to be put on the end of
        % the list so a sort file is created for it. This is required when
        % building the hash table, not for determinism inference.
        (
            MaybeAllInProc = yes(ProcID),
            CheckProcs1 = [ProcID | CheckProcs0],
            AllInMode = all_in_mode_does_exist
        ;
            MaybeAllInProc = no,
            CheckProcs1 = CheckProcs0,
            AllInMode = all_in_mode_does_not_exist
        ),

        % We need to get the order right for CheckProcs because the first
        % procedure in list is used to derive the primary lookup key.
        list.reverse(CheckProcs1, CheckProcs),
        pred_info_set_proc_table(ProcTable, !PredInfo)
    ).

:- pred infer_procs_determinism_pass_1(module_info::in, list(proc_id)::in,
    maybe(proc_id)::out,
    proc_table::in, proc_table::out,
    list(proc_id)::in, list(proc_id)::out,
    list(fact_arg_info)::in, list(fact_arg_info)::out,
    maybe_write_hash_tables::in, maybe_write_hash_tables::out,
    maybe_write_data_table::in, maybe_write_data_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

infer_procs_determinism_pass_1(_, [], no, !CheckProcs, !FactArgInfos,
        !WriteHashTables, !WriteDataTable, !ProcTable, !Specs).
infer_procs_determinism_pass_1(ModuleInfo, [ProcID | ProcIDs], MaybeAllInProc,
        !ProcTable, !CheckProcs, !FactArgInfos,
        !WriteHashTables, !WriteDataTable, !Specs) :-
    map.lookup(!.ProcTable, ProcID, ProcInfo0),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    fill_in_fact_arg_infos(ModuleInfo, ArgModes, !FactArgInfos),
    compute_fact_table_mode_type(ModuleInfo, ArgModes, ModeType),
    (
        ModeType = all_in,
        InferredDetism = inferred(detism_semi),
        MaybeAllInProc0 = yes(ProcID),
        !:WriteHashTables = write_hash_tables
    ;
        ModeType = all_out,
        proc_info_get_declared_determinism(ProcInfo0, MaybeDet),
        ( if
            MaybeDet = yes(Det),
            ( Det = detism_cc_multi
            ; Det = detism_cc_non
            )
        then
            InferredDetism = inferred(detism_cc_multi)
        else
            InferredDetism = inferred(detism_multi)
        ),
        MaybeAllInProc0 = no,
        !:WriteDataTable = write_data_table
    ;
        ModeType = in_out,

        % Don't have enough info to infer determinism yet.
        % Put it off till the second pass.
        InferredDetism = not_yet,
        % Add to list and check in pass 2.
        MaybeAllInProc0 = no,
        !:CheckProcs = [ProcID | !.CheckProcs],
        !:WriteHashTables = write_hash_tables,
        !:WriteDataTable = write_data_table
    ;
        ModeType = other,           % mode error
        InferredDetism = error,
        MaybeAllInProc0 = no,
        proc_info_get_context(ProcInfo0, Context),
        Pieces = [words("Error: the only modes allowed in fact tables are"),
            quote("in"), words("and"), quote("out"), suffix("."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs)
    ;
        ModeType = no_args,         % mode error
        InferredDetism = error,
        MaybeAllInProc0 = no,
        proc_info_get_context(ProcInfo0, Context),
        Pieces = [words("Error: predicates without arguments"),
            words("are not allowed in fact tables."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs)
    ),
    ( if InferredDetism = inferred(Determinism) then
        proc_info_set_inferred_determinism(Determinism,
            ProcInfo0, ProcInfo),
        map.det_update(ProcID, ProcInfo, !ProcTable)
    else
        true
    ),
    infer_procs_determinism_pass_1(ModuleInfo, ProcIDs, MaybeAllInProc1,
        !ProcTable, !CheckProcs, !FactArgInfos,
        !WriteHashTables, !WriteDataTable, !Specs),
    (
        MaybeAllInProc0 = yes(_),
        MaybeAllInProc = MaybeAllInProc0
    ;
        MaybeAllInProc0 = no,
        MaybeAllInProc = MaybeAllInProc1
    ).

    % Return the fact_table_mode_type for a procedure.
    %
:- pred compute_fact_table_mode_type(module_info::in,
    list(mer_mode)::in, fact_table_mode_type::out) is det.

compute_fact_table_mode_type(ModuleInfo, Modes, ModeType) :-
    compute_mode_classes(ModuleInfo, Modes, Classes),
    list.sort_and_remove_dups(Classes, SortedClasses),
    (
        SortedClasses = [],
        ModeType = no_args
    ;
        SortedClasses = [_ | _],
        % SortedClasses may contain a maximum of three elements,
        % taken from fully_input_mode, fully_output_mode, and mixed_mode.
        ( if list.contains(SortedClasses, mixed_mode) then
            ModeType = other
        else
            % SortedClasses may contain a maximum of two elements,
            % taken from fully_input_mode and fully_output_mode.
            ( if SortedClasses = [fully_input_mode] then
                ModeType = all_in
            else if SortedClasses = [fully_output_mode] then
                ModeType = all_out
            else
                expect(
                    unify(SortedClasses,
                        [fully_input_mode, fully_output_mode]),
                    $pred, "unexpected SortedClasses"),
                ModeType = in_out
            )
        )
    ).

:- type mode_class
    --->    fully_input_mode
    ;       fully_output_mode
    ;       mixed_mode.

:- pred compute_mode_classes(module_info::in,
    list(mer_mode)::in, list(mode_class)::out) is det.

compute_mode_classes(_, [], []).
compute_mode_classes(ModuleInfo, [Mode | Modes], [Class | Classes]) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        Class = fully_input_mode
    else if mode_is_fully_output(ModuleInfo, Mode) then
        Class = fully_output_mode
    else
        Class = mixed_mode
    ),
    compute_mode_classes(ModuleInfo, Modes, Classes).

%---------------------------------------------------------------------------%

    % Initialise list of fact argument information. Input and output flags
    % are initialised to `no' and filled in correctly by
    % infer_determinism_pass_1.
    %
:- pred init_fact_arg_infos(pred_info::in, list(mer_type)::in,
    list(fact_arg_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

init_fact_arg_infos(_, [], [], !Specs).
init_fact_arg_infos(PredInfo, [Type | Types], [Info | Infos], !Specs) :-
    ( if
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_int(int_type_int),
            FactArgTypePrime = fact_arg_type_int
        ;
            BuiltinType = builtin_type_float,
            FactArgTypePrime = fact_arg_type_float
        ;
            BuiltinType = builtin_type_string,
            FactArgTypePrime = fact_arg_type_string
        )
    then
        FactArgType = FactArgTypePrime
    else
        pred_info_get_context(PredInfo, Context),
        Pieces = [words("Error: invalid type in fact table."),
            words("The only types allowed in fact tables are"),
            quote("int"), suffix(","), quote("float"), words("and"),
            quote("string"), suffix("."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs),
        FactArgType = fact_arg_type_int % Dummy; won't be used.
    ),
    Info = fact_arg_info(FactArgType, no, no),
    init_fact_arg_infos(PredInfo, Types, Infos, !Specs).

    % XXX CLEANUP The input to this should NOT be a partially-filled-in
    % fact_arg_info, but a value of a type containing just the filled-in part.
    %
:- pred fill_in_fact_arg_infos(module_info::in, list(mer_mode)::in,
    list(fact_arg_info)::in, list(fact_arg_info)::out) is det.

fill_in_fact_arg_infos(_, [], [], []).
fill_in_fact_arg_infos(_, [_ | _], [], _) :-
    unexpected($pred, "too many argmodes").
fill_in_fact_arg_infos(_, [], [_ | _], _) :-
    unexpected($pred, "too many fact_arg_infos").
fill_in_fact_arg_infos(ModuleInfo, [Mode | Modes],
        [Info0 | Infos0], [Info | Infos]) :-
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
    fill_in_fact_arg_infos(ModuleInfo, Modes, Infos0, Infos).

%---------------------------------------------------------------------------%

    % Read in facts one by one and check and compile them.
    %
:- pred read_in_and_compile_facts(io.text_input_stream::in, string::in,
    maybe(io.text_output_stream)::in, int::in,
    module_info::in, pred_info::in,
    list(fact_arg_info)::in, list(proc_stream)::in,
    maybe(pair(io.text_output_stream, string))::in, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
        FactTableSize, ModuleInfo, PredInfo, FactArgInfos,
        ProcStreams, MaybeOutput, !FactNum, !Specs, !IO) :-
    parser.read_term(FileStream, Result0, !IO),
    (
        Result0 = eof
    ;
        Result0 = error(Message, LineNum),
        term.context_init(FileName, LineNum, Context),
        add_error_context_and_pieces(Context, [words(Message)], !Specs)
    ;
        Result0 = term(_VarSet, Term),
        ( if 0 = !.FactNum mod FactTableSize then
            (
                MaybeProgressStream = yes(ProgressStream),
                io.format(ProgressStream, "%% Read fact %d\n",
                    [i(!.FactNum)], !IO)
            ;
                MaybeProgressStream = no
            )
        else
            true
        ),
        check_fact_term(FileStream, FileName, MaybeProgressStream,
            FactTableSize, ModuleInfo, PredInfo, FactArgInfos,
            !.FactNum, Term, ProcStreams, MaybeOutput, Result, !Specs, !IO),
        (
            Result = ok,
            !:FactNum = !.FactNum + 1
        ;
            Result = error
        ),
        read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
            FactTableSize, ModuleInfo, PredInfo, FactArgInfos,
            ProcStreams, MaybeOutput, !FactNum, !Specs, !IO)
    ).

    % Do syntactic and semantic checks on a fact term.
    %
:- pred check_fact_term(io.text_input_stream::in, string::in,
    maybe(io.text_output_stream)::in, int::in, module_info::in, pred_info::in,
    list(fact_arg_info)::in, int::in, prog_term::in,
    list(proc_stream)::in, maybe(pair(io.text_output_stream, string))::in,
    fact_result::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

check_fact_term(FileStream, FileName, MaybeProgressStream, FactTableSize,
        ModuleInfo, PredInfo, FactArgInfos, FactNum, Term,
        ProcStreams, MaybeOutput, Result, !Specs, !IO) :-
    (
        Term = term.variable(_, _),
        io.get_line_number(FileStream, LineNum, !IO),
        Context = term.context(FileName, LineNum),
        Pieces = [words("Error: term is not a fact."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs),
        Result = error
    ;
        Term = term.functor(Functor, ArgTerms0, Context),
        ( if Functor = term.atom(FunctorAtom) then
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            pred_info_get_name(PredInfo, PredName),
            ( if
                (
                    PredOrFunc = pf_predicate,
                    FunctorAtom = PredName,
                    ArgTerms = ArgTerms0
                ;
                    PredOrFunc = pf_function,
                    FunctorAtom = "=",
                    ArgTerms0 = [BeforeEqualTerm, ResultTerm],
                    BeforeEqualTerm =
                        term.functor(term.atom(PredName), BeforeEqualTerms, _),
                    ArgTerms = BeforeEqualTerms ++ [ResultTerm]
                )
            then
                check_fact_term_2(MaybeProgressStream, FactTableSize,
                    ModuleInfo, PredInfo, FactArgInfos,
                    FactNum, ArgTerms, Context, ProcStreams, MaybeOutput,
                    Result, !Specs, !IO)
            else
                PredPieces = describe_one_pred_info_name(
                    should_not_module_qualify, PredInfo),
                Pieces = [words("Error: invalid clause for") | PredPieces]
                    ++ [suffix("."), nl],
                add_error_context_and_pieces(Context, Pieces, !Specs),
                Result = error
            )
        else
            Pieces = [words("Error: term is not a fact."), nl],
            add_error_context_and_pieces(Context, Pieces, !Specs),
            Result = error
        )
    ).

:- pred check_fact_term_2(maybe(io.text_output_stream)::in, int::in,
    module_info::in, pred_info::in, list(fact_arg_info)::in,
    int::in, list(prog_term)::in, context::in,
    list(proc_stream)::in, maybe(pair(io.text_output_stream, string))::in,
    fact_result::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

check_fact_term_2(MaybeProgressStream, FactTableSize, ModuleInfo, PredInfo,
        FactArgInfos, FactNum, Terms, Context, ProcStreams, MaybeOutput,
        Result, !Specs, !IO) :-
    % Check that arity of the fact is correct.
    pred_info_get_orig_arity(PredInfo, OrigArity),
    list.length(Terms, NumTerms),
    ( if NumTerms = OrigArity then
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_arg_types(PredInfo, Types),
        check_fact_type_and_mode(PredOrFunc, Types, Terms, 0, Context,
            Result, !Specs),
        pred_info_get_proc_table(PredInfo, ProcTable),
        string.int_to_string(FactNum, FactNumStr),
        write_sort_file_lines(ModuleInfo, ProcTable, FactNumStr, Terms,
            is_primary_proc(FactArgInfos), ProcStreams, !IO),
        % If there are no in_out modes to the predicate, we need to write out
        % the facts at this point. If there are input modes, the facts are
        % written out later on after being sorted on the first input mode.
        ( if
            MaybeOutput = yes(OutputStream - StructName),
            TermToArg =
                (
                    pred(Term::in, FactArg::out) is semidet :-
                        Term = term.functor(FactArg, _, _)
                ),
            list.map(TermToArg, Terms, FactArgs)
        then
            write_fact_data(OutputStream, MaybeProgressStream, FactTableSize,
                StructName, FactArgs, FactNum, !IO)
        else
            % If list.map above fails, don't do anything here. The error will
            % have already been reported in check_fact_type_and_mode.
            true
        )
    else
        Pieces = [words("Error: fact has wrong number of arguments."),
            words("Expecting"), int_fixed(OrigArity), words("arguments, but"),
            words("fact has"), int_fixed(NumTerms), words("arguments."), nl],
        add_error_context_and_pieces(Context, Pieces, !Specs),
        Result = error
    ).

    % Check that the mode of the fact is correct. All terms must be ground
    % and be a constant of the correct type. Only string, int and float are
    % supported at the moment.
    %
:- pred check_fact_type_and_mode(pred_or_func::in,
    list(mer_type)::in, list(prog_term)::in, int::in,
    prog_context::in, fact_result::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fact_type_and_mode(_, _, [], _, _, ok, !Specs).
check_fact_type_and_mode(PredOrFunc, Types0, [Term | Terms], ArgNum0,
        Context0, Result, !Specs) :-
    ArgNum = ArgNum0 + 1,
    (
        Term = term.variable(_, _),
        Pieces = [words("Error: non-ground term in fact."), nl],
        add_error_context_and_pieces(Context0, Pieces, !Specs),
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
                RequiredType = yes(builtin_type_int(int_type_int))
            ;
                Signedness = unsigned,
                RequiredType = yes(builtin_type_int(int_type_uint))
            )
        ;
            Functor = term.float(_),
            RequiredType = yes(builtin_type_float)
        ;
            Functor = term.atom(_),
            RequiredType = no
        ;
            Functor = term.implementation_defined(_),
            unexpected($pred, "implementation-defined literal")
        ),
        (
            RequiredType = no,
            (
                Items = [_ | _],
                Pieces = [words("Error: compound types are"),
                    words("not supported in fact tables."), nl]
            ;
                Items = [],
                Pieces = [words("Error: enumeration types are"),
                    words("not supported in fact tables."), nl]
            ),
            add_error_context_and_pieces(Context, Pieces, !Specs),
            Result = error
        ;
            RequiredType = yes(BuiltinType),
            ( if
                Types0 = [Type | Types],
                Type = builtin_type(BuiltinType)
            then
                check_fact_type_and_mode(PredOrFunc, Types, Terms, ArgNum,
                    Context0, Result, !Specs)
            else
                report_type_error(PredOrFunc, ArgNum, Terms, Context, !Specs),
                Result = error
            )
        )
    ).

:- pred report_type_error(pred_or_func::in, int::in, list(prog_term)::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_type_error(PredOrFunc, ArgNum, RemainingTerms, Context, !Specs) :-
    ( if
        % Report a different error message for the return value of a function.
        PredOrFunc = pf_function,
        RemainingTerms = []
    then
        Pieces = [words("Type error in return value of function."), nl]
    else
        Pieces = [words("Type error in argument"), int_fixed(ArgNum),
            suffix("."), nl]
    ),
    add_error_context_and_pieces(Context, Pieces, !Specs).

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

:- pred create_fact_table_header(sym_name::in, list(fact_arg_info)::in,
    string::out, string::out) is det.

create_fact_table_header(PredSymName, FactArgInfos, HeaderCode, StructName) :-
    make_fact_table_identifier(PredSymName, Identifier),
    StructName = "mercury__" ++ Identifier ++ "_fact_table",

    % Define a struct for a fact table entry.
    create_fact_table_struct(FactArgInfos, 1, StructContents),
    ( if StructContents = "" then
        StructDef = ""
    else
        StructDef = "struct " ++ StructName ++ "_struct {\n"
            ++ StructContents ++ "};\n\n"
    ),
    HashDef = hash_def,
    HeaderCode = StructDef ++ HashDef.

    % Create a struct for the fact table consisting of any arguments
    % that are output in some mode. Also ensure that are arguments are
    % either string, float or int.
    %
:- pred create_fact_table_struct(list(fact_arg_info)::in, int::in, string::out)
    is det.

create_fact_table_struct([], _, "").
create_fact_table_struct([Info | Infos], ArgNum, StructContents) :-
    create_fact_table_struct(Infos, ArgNum + 1, StructContentsTail),
    Info = fact_arg_info(Type, _IsInput, IsOutput),
    (
        Type = fact_arg_type_int,
        TypeStr = "MR_Integer"
    ;
        Type = fact_arg_type_float,
        TypeStr = "MR_Float"
    ;
        Type = fact_arg_type_string,
        TypeStr = "MR_ConstString"
    ),
    (
        IsOutput = yes,
        string.format("\t%s V_%d;\n", [s(TypeStr), i(ArgNum)], StructField),
        string.append(StructField, StructContentsTail, StructContents)
    ;
        IsOutput = no,
        StructContents = StructContentsTail
    ).

    % Define a struct for a hash table entry.
    %
:- func hash_def = string.

hash_def = "
#ifndef MERCURY_FACT_TABLE_HASH_TABLES
#define MERCURY_FACT_TABLE_HASH_TABLES

struct MR_fact_table_hash_table_s {
    MR_Integer size;                            // size of the hash table
    struct MR_fact_table_hash_entry_s *table;   // the actual table
};

struct MR_fact_table_hash_table_f {
    MR_Integer size;                            // size of the hash table
    struct MR_fact_table_hash_entry_f *table;   // the actual table
};

struct MR_fact_table_hash_table_i {
    MR_Integer size;                            // size of the hash table
    struct MR_fact_table_hash_entry_i *table;   // the actual table
};

// hash table for string keys
struct MR_fact_table_hash_entry_s {
    MR_ConstString  key;        // lookup key
    const MR_Word   *index;     // index into fact table data array or
                                // pointer to hash table for next argument
#if TAGBITS < 2
    short type;                 // 0 if entry empty,
                                // 1 if entry is a pointer to the data table
                                // 2 if entry is a pointer to another
                                //   hash table
#endif
    int next;                   // location of next entry with the same hash
                                // value
};

// hash table for float keys
struct MR_fact_table_hash_entry_f {
    MR_Float        key;
    const MR_Word   *index;
#if TAGBITS < 2
    short           type;
#endif
    int             next;
};

// hash table for int keys
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

#endif // not MERCURY_FACT_TABLE_HASH_TABLES
".

%---------------------------------------------------------------------------%

    % open_sort_files(ProcIDs, ProcStreams):
    %
    % Open a temporary sort file for each proc_id in ProcIDs.
    % Return a list of proc_streams for all the files opened.
    %
:- pred open_sort_files(list(proc_id)::in, list(proc_stream)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

open_sort_files([], [], !Specs, !IO).
open_sort_files([ProcID | ProcIDs], ProcStreams, !Specs, !IO) :-
    open_temp_output(SortFileNameResult, !IO),
    (
        SortFileNameResult = ok({_SortFileName, Stream}),
        open_sort_files(ProcIDs, ProcStreams0, !Specs, !IO),
        ProcStreams = [proc_stream(ProcID, Stream) | ProcStreams0]
    ;
        SortFileNameResult = error(ErrorMessage),
        ProcStreams = [],
        add_error_pieces([words(ErrorMessage)], !Specs)
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

%---------------------%

:- type maybe_primary_proc
    --->    is_not_primary_proc
    ;       is_primary_proc(list(fact_arg_info)).

    % write_sort_file_lines(ModuleInfo, ProcStreams, ProcTable, Terms):
    %
    % Write out a line to each sort file for this fact. The line is made up
    % of the input arguments of the procedure (the key) followed by the
    % position of the fact in the original input table.
    %
    % Note lines written out here need to be read back in by
    % read_sort_file_line so if any changes are made here, corresponding
    % changes should be made there too.
    %
:- pred write_sort_file_lines(module_info::in, proc_table::in,
    string::in, list(prog_term)::in, maybe_primary_proc::in,
    list(proc_stream)::in, io::di, io::uo) is det.

write_sort_file_lines(_, _, _, _, _, [], !IO).
write_sort_file_lines(ModuleInfo, ProcTable, FactNumStr, Terms,
        IsPrimary, [ProcStream | ProcStreams], !IO) :-
    ProcStream = proc_stream(ProcID, Stream),
    map.lookup(ProcTable, ProcID, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    assoc_list.from_corresponding_lists(ArgModes, Terms, ModeTerms),
    make_sort_file_key(ModuleInfo, ModeTerms, Key),
    (
        IsPrimary = is_primary_proc(FactArgInfos),
        assoc_list.from_corresponding_lists(FactArgInfos, Terms, InfoTerms),
        DataString = make_fact_data_string(InfoTerms)
    ;
        IsPrimary = is_not_primary_proc,
        DataString = ""
    ),
    io.format(Stream, "%s~%s~%s\n",
        [s(Key), s(FactNumStr), s(DataString)], !IO),
    write_sort_file_lines(ModuleInfo, ProcTable, FactNumStr, Terms,
        is_not_primary_proc, ProcStreams, !IO).

    % Create a key for the fact table entry.
    % Arguments are separated by ":".
    % Colons in string literals are replaced by "\c", tildes are replaced
    % by "\t", newlines are replaced by "\n" and backslashes by "\\".
    % This ensures that each possible set of arguments maps to a unique key
    % and guarantees that duplicate keys will be adjacent after sorting
    % with the sort program. The tilde ('~') character is used in the
    % sort file to separate the sort key from the data.
    %
:- pred make_sort_file_key(module_info::in,
    assoc_list(mer_mode, prog_term)::in, string::out) is det.

make_sort_file_key(_ModuleInfo, [], "").
make_sort_file_key(ModuleInfo, [HeadModeTerm | TailModeTerms], Key) :-
    make_sort_file_key(ModuleInfo, TailModeTerms, TailKey),
    HeadModeTerm = Mode - Term,
    ( if
        mode_is_fully_input(ModuleInfo, Mode),
        Term = term.functor(Const, [], _Context)
    then
        HeadKey = make_key_part(Const),
        Key = HeadKey ++ ":" ++ TailKey
    else
        Key = TailKey
    ).

    % Like make_sort_file_key but for the output arguments of the fact.
    %
:- func make_fact_data_string(assoc_list(fact_arg_info, prog_term)) = string.

make_fact_data_string([]) = "".
make_fact_data_string([HeadInfoTerm | TailInfoTerms]) = String :-
    TailString = make_fact_data_string(TailInfoTerms),
    HeadInfoTerm = fact_arg_info(_, _, IsOutput) - Term,
    ( if
        IsOutput = yes,
        Term = term.functor(Const, [], _)
    then
        HeadString = make_key_part(Const),
        String = HeadString ++ ":" ++ TailString
    else
        String = TailString
    ).

:- func make_key_part(const) = string.

make_key_part(Const) = Key :-
    (
        Const = term.atom(_),
        unexpected($pred, "enumerated types are not supported yet.")
    ;
        Const = term.integer(Base, Integer, Signedness, _Size),
        (
            Signedness = signed,
            ( if source_integer_to_int(Base, Integer, I) then
                % Print the integer in base 36 to reduce the amount of I/O
                % that we do.
                Key = string.int_to_base_string(I, 36)
            else
                unexpected($pred, "integer too big")
            )
        ;
            Signedness = unsigned,
            unexpected($pred, "NYI uints and fact tables")
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
        unexpected($pred, "implementation-defined literal")
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
        cord.snoc(Char, !EscapedCharsCord)
    ),
    key_from_chars_loop(Chars, !EscapedCharsCord).

%---------------------------------------------------------------------------%

    % infer_determinism_pass_2(ModuleInfo, ExistsAllInMode, ProcFiles,
    %   !ProcTable, !IO):
    %
    % Run `sort' on each sort file to see if the keys are unique.
    % If they are, the procedure is semidet, otherwise it is nondet.
    % Return the updated proc_table.
    %
:- pred infer_determinism_pass_2(module_info::in, all_in_mode::in,
    assoc_list(proc_id, string)::in, proc_table::in, proc_table::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

infer_determinism_pass_2(_, _, [], !ProcTable, !Specs, !IO).
infer_determinism_pass_2(ModuleInfo, ExistsAllInMode,
        [ProcID - FileName | ProcFiles], !ProcTable, !Specs, !IO) :-
    map.lookup(!.ProcTable, ProcID, ProcInfo0),
    % XXX This sends the output to the *input* file.
    % This works in the usual implementations of sort,
    % since you have to finish reading the input before you can generate
    % any part of the output, but it is not *guaranteed* to work,
    % since sort may *open* the output file before finishing reading.
    % XXX Also, this requires "sort" and "cut" to be installed,
    % but we don't check that. We should have autoconf tests for their
    % presence/absence, and if either is absent, we should generate
    % a more specific error message than what results from the failure
    % of Command.
    string.format(
        "LC_ALL=C sort -o %s %s && " ++
        "cut -d'~' -f1 %s | LC_ALL=C sort -cu >/dev/null 2>&1",
        [s(FileName), s(FileName), s(FileName)], Command0),
    make_command_string(Command0, double, Command),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbose = no,
        io.call_system(Command, Result, !IO)
    ;
        Verbose = yes,
        get_progress_output_stream(ModuleInfo, ProgressStream, !IO),
        io.format(ProgressStream, "%% Invoking system command `%s' ...",
            [s(Command)], !IO),
        io.call_system(Command, Result, !IO),
        io.write_string(ProgressStream, "done.\n", !IO)
    ),
    (
        Result = ok(ExitStatus),
        % sort -cu returns 0 if file is sorted and contains no duplicate keys,
        % >=1 if duplicate keys exist.
        ( if
            (
                ExitStatus = 0
            ;
                % This is an all_in mode so it is semidet.
                ExistsAllInMode = all_in_mode_does_exist,
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
            Pieces =
                [fixed(ProgName), suffix(":"), words("an error occurred"),
                words("in ether the"), quote("sort"),
                words("or the"), quote("cut"), words("program"),
                words("during fact table determinism inference."), nl],
            Spec = error_spec($pred, severity_error, phase_fact_table_check,
                [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs],
            Determinism = detism_erroneous
        )
    ;
        Result = error(ErrorCode),
        add_call_system_error("sort", ErrorCode, !Specs, !IO),
        Determinism = detism_erroneous
    ),
    proc_info_set_inferred_determinism(Determinism, ProcInfo0, ProcInfo),
    map.det_update(ProcID, ProcInfo, !ProcTable),
    infer_determinism_pass_2(ModuleInfo, ExistsAllInMode, ProcFiles,
        !ProcTable, !Specs, !IO).

%---------------------------------------------------------------------------%

    % Write out the fact table data arrays and hash tables.
    %
:- pred write_fact_table_arrays(io.text_output_stream::in, int::in,
    module_info::in, assoc_list(proc_id, string)::in, string::in,
    proc_table::in, string::in, int::in, list(fact_arg_info)::in,
    maybe_write_hash_tables::in, maybe_write_data_table::in,
    string::out, proc_id::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

write_fact_table_arrays(OutputStream, FactTableSize, ModuleInfo,
        ProcFiles, DataFileName, ProcTable, StructName, NumFacts, FactArgInfos,
        WriteHashTables, WriteDataTable, HeaderCode, PrimaryProcID,
        !Specs, !IO) :-
    (
        % No sort files => there was only and all_out mode
        %   => nothing left to be done here.
        ProcFiles = [],
        HeaderCode = "",
        % This won't get used anyway.
        PrimaryProcID = hlds_pred.initial_proc_id
    ;
        ProcFiles = [PrimaryProcID - FileName | TailProcFiles],
        (
            WriteHashTables = write_hash_tables,
            (
                TailProcFiles = [],
                CreateFactMap = do_not_create_fact_map
            ;
                % If there we need to build secondary hash tables (i.e. if
                % there is >1 input mode) we need to create a ``FactMap''
                % while writing out the primary table.
                TailProcFiles = [_ | _],
                CreateFactMap = create_fact_map
            ),
            write_primary_hash_table(OutputStream, FactTableSize, ModuleInfo,
                ProcTable, PrimaryProcID, FileName, DataFileName, StructName,
                FactArgInfos, WriteDataTable, NumFacts,
                CreateFactMap, FactMap, PrimaryResult, PrimaryHeaderCode,
                !Specs, !IO),
            (
                PrimaryResult = ok,
                write_secondary_hash_tables(OutputStream, FactTableSize,
                    ModuleInfo, ProcTable, StructName, FactArgInfos, FactMap,
                    TailProcFiles, "", SecondaryHeadCode, !Specs, !IO),
                HeaderCode = PrimaryHeaderCode ++ SecondaryHeadCode
            ;
                PrimaryResult = error,
                HeaderCode = PrimaryHeaderCode
            )
        ;
            WriteHashTables = do_not_write_hash_tables,
            HeaderCode = ""
        )
    ).

    % Write out the data for the fact table.
    %
:- pred write_fact_table_data(io.text_output_stream::in,
    maybe(io.text_output_stream)::in, int::in, string::in,
    list(list(fact_arg))::in, int::in, io::di, io::uo) is det.

write_fact_table_data(_, _, _, _, [], _, !IO).
write_fact_table_data(OutputStream, MaybeProgressStream, FactTableSize,
        StructName, [Fact | Facts], FactNum, !IO) :-
    write_fact_data(OutputStream, MaybeProgressStream, FactTableSize,
        StructName, Fact, FactNum, !IO),
    write_fact_table_data(OutputStream, MaybeProgressStream, FactTableSize,
        StructName, Facts, FactNum + 1, !IO).

    % Write out the data for a single fact, starting a new array if necessary.
    % Note: this predicate will not write the declaration or opening brace
    % for the first array or the closing brace of the last array.
    %
:- pred write_fact_data(io.text_output_stream::in,
    maybe(io.text_output_stream)::in, int::in, string::in,
    list(fact_arg)::in, int::in, io::di, io::uo) is det.

write_fact_data(OutputStream, MaybeProgressStream, FactTableSize,
        StructName, Args, FactNum, !IO) :-
    ( if 0 = FactNum mod FactTableSize then
        ( if FactNum = 0 then
            true
        else
            write_closing_brace(OutputStream, !IO),
            write_new_data_array_opening_brace(OutputStream, StructName,
                FactNum, !IO)
        ),
        (
            MaybeProgressStream = yes(ProgressStream),
            io.format(ProgressStream,
                "%% Writing fact %d\n", [i(FactNum)], !IO)
        ;
            MaybeProgressStream = no
        )
    else
        true
    ),
    io.write_string(OutputStream, "\t{", !IO),
    write_fact_args(OutputStream, Args, !IO),
    io.write_string(OutputStream, " },\n", !IO).

    % Write out the declaration of a new data array followed by " = {\n".
    %
:- pred write_new_data_array_opening_brace(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

write_new_data_array_opening_brace(OutputStream, StructName, FactNum, !IO) :-
    io.format(OutputStream, "const struct %s_struct %s%d[] = {\n",
        [s(StructName), s(StructName), i(FactNum)], !IO).

    % Write out the closing brace of an array.
    %
:- pred write_closing_brace(io.text_output_stream::in, io::di, io::uo) is det.

write_closing_brace(OutputStream, !IO) :-
    io.write_string(OutputStream, "};\n\n", !IO).

:- pred write_fact_args(io.text_output_stream::in, list(fact_arg)::in,
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
                unexpected($pred, "integer too big")
            )
        ;
            Signedness = unsigned,
            unexpected($pred, "NYI uints in fact tables")
        )
    ;
        Arg = term.float(Float),
        io.write_float(OutputStream, Float, !IO),
        io.write_string(OutputStream, ", ", !IO)
    ;
        Arg = term.atom(_),
        unexpected($pred, "unsupported type")
    ;
        Arg = term.implementation_defined(_),
        unexpected($pred, "implementation-defined literal")
    ),
    write_fact_args(OutputStream, Args, !IO).

    % If a data table has been created in a separate file, append it to the
    % end of the main output file and then delete it.
    %
:- pred append_data_table(module_info::in, string::in, string::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

append_data_table(ModuleInfo, OutputFileName, DataFileName, !Specs, !IO) :-
    make_command_string(string.format("cat %s >>%s",
        [s(DataFileName), s(OutputFileName)]), forward, Command),
    get_maybe_progress_output_stream(ModuleInfo, MaybeProgressStream, !IO),
    (
        MaybeProgressStream = no,
        io.call_system(Command, Result, !IO)
    ;
        MaybeProgressStream = yes(ProgressStream),
        io.format(ProgressStream, "%% Invoking system command `%s' ...",
            [s(Command)], !IO),
        io.call_system(Command, Result, !IO),
        io.write_string(ProgressStream, "done.\n", !IO)
    ),
    (
        Result = ok(ExitStatus),
        ( if ExitStatus = 0 then
            true
        else
            Pieces = [words("An error occurred while concatenating"),
                words("fact table output files."), nl],
            Spec = error_spec($pred, severity_error, phase_fact_table_check,
                [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Result = error(ErrorCode),
        add_call_system_error("cat", ErrorCode, !Specs, !IO)
    ),
    delete_temporary_file(DataFileName, !Specs, !IO).

    % Write hash tables for the primary key. Create a map from indices in the
    % original input table to the table sorted on the primary key.
    % Write out the data table if required.
    %
:- pred write_primary_hash_table(io.text_output_stream::in, int::in,
    module_info::in, proc_table::in, proc_id::in, string::in, string::in,
    string::in, list(fact_arg_info)::in, maybe_write_data_table::in, int::in,
    maybe_create_fact_map::in, map(int, int)::out,
    fact_result::out, string::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

write_primary_hash_table(OutputStream, FactTableSize, ModuleInfo,
        ProcTable, ProcID, FileName, DataFileName, StructName, FactArgInfos,
        WriteDataTable, NumFacts, CreateFactMap, FactMap,
        Result, HeaderCode, !Specs, !IO) :-
    map.init(FactMap0),
    io.open_input(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        (
            WriteDataTable = write_data_table,
            io.open_output(DataFileName, OpenResult, !IO),
            (
                OpenResult = ok(DataStream),
                MaybeDataStream = yes(DataStream),
                % output opening brace for first fact array
                write_new_data_array_opening_brace(DataStream, StructName,
                    0, !IO)
            ;
                OpenResult = error(Error),
                add_file_open_error(no, DataFileName, "output", Error,
                    !Specs, !IO),
                MaybeDataStream = no
            )
        ;
            WriteDataTable = do_not_write_data_table,
            MaybeDataStream = no
        ),
        (
            MaybeDataStream = yes(_),
            proc_id_to_int(ProcID, ProcInt),
            string.format("%s_hash_table_%d_",
                [s(StructName), i(ProcInt)], HashTableName),
            % Note: the type declared here is not necessarily correct.
            % We declare it just to stop the C compiler emitting warnings.
            string.format("extern struct MR_fact_table_hash_table_i %s0;\n",
                [s(HashTableName)], HeaderCode0),
            map.lookup(ProcTable, ProcID, ProcInfo),
            proc_info_get_argmodes(ProcInfo, ArgModes),
            read_sort_file_line(FileStream, FileName, ModuleInfo,
                FactArgInfos, ArgModes, MaybeFirstFact, !Specs, !IO),
            (
                MaybeFirstFact = yes(FirstFact),
                build_hash_table(FileStream, FileName, OutputStream,
                    MaybeDataStream, FactTableSize, ModuleInfo, primary_table,
                    ArgModes, FactArgInfos, StructName, 0, HashTableName, 0,
                    FirstFact, 0, CreateFactMap, FactMap0, FactMap,
                    !Specs, !IO),
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
            HeaderCode0 = ""
        ),
        (
            MaybeDataStream = yes(DataStream1),
            % Closing brace for last fact data array.
            write_closing_brace(DataStream1, !IO),
            write_fact_table_pointer_array(DataStream1, FactTableSize,
                StructName, NumFacts, HeaderCode1, !IO),
            io.close_output(DataStream1, !IO),
            HeaderCode = HeaderCode0 ++ HeaderCode1
        ;
            MaybeDataStream = no,
            HeaderCode = HeaderCode0
        ),
        io.close_input(FileStream, !IO),
        delete_temporary_file(FileName, !Specs, !IO)
    ;
        FileResult = error(Error),
        add_file_open_error(no, FileName, "input", Error, !Specs, !IO),
        Result = error,
        FactMap = FactMap0,
        HeaderCode = ""
    ).

    % Build hash tables for non-primary input procs.
    %
:- pred write_secondary_hash_tables(io.text_output_stream::in, int::in,
    module_info::in, proc_table::in, string::in,
    list(fact_arg_info)::in, map(int, int)::in,
    assoc_list(proc_id, string)::in,
    string::in, string::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

write_secondary_hash_tables(_, _, _, _, _, _, _, [], !HeaderCode, !Specs, !IO).
write_secondary_hash_tables(OutputStream, FactTableSize, ModuleInfo,
        ProcTable, StructName, FactArgInfos, FactMap,
        [ProcID - FileName | ProcFiles], !HeaderCode, !Specs, !IO) :-
    io.open_input(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        proc_id_to_int(ProcID, ProcInt),
        string.format("%s_hash_table_%d_",
            [s(StructName), i(ProcInt)], HashTableName),
        % Note: the type declared here is not necessarily correct.
        % The type is declared just to stop the C compiler emitting warnings.
        string.format(
            "extern struct MR_fact_table_hash_table_i %s0;\n",
            [s(HashTableName)], NewHeaderCode),
        % XXX CLEANUP This prepends to !HeaderCode, when appending
        % would look to be more appropriate.
        string.append(NewHeaderCode, !HeaderCode),
        map.lookup(ProcTable, ProcID, ProcInfo),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        read_sort_file_line(FileStream, FileName, ModuleInfo,
            FactArgInfos, ArgModes, MaybeFirstFact, !Specs, !IO),
        (
            MaybeFirstFact = yes(FirstFact),
            build_hash_table(FileStream, FileName, OutputStream, no,
                FactTableSize, ModuleInfo, not_primary_table,
                ArgModes, FactArgInfos, StructName, 0, HashTableName, 0,
                FirstFact, 0, do_not_create_fact_map, FactMap, _, !Specs, !IO),
            io.close_input(FileStream, !IO),
            delete_temporary_file(FileName, !Specs, !IO),
            write_secondary_hash_tables(OutputStream, FactTableSize,
                ModuleInfo, ProcTable, StructName, FactArgInfos, FactMap,
                ProcFiles, !HeaderCode, !Specs, !IO)
        ;
            MaybeFirstFact = no,
            io.close_input(FileStream, !IO)
        )
    ;
        FileResult = error(Error),
        add_file_open_error(no, FileName, "input", Error, !Specs, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred read_sort_file_line(io.text_input_stream::in, string::in,
    module_info::in, list(fact_arg_info)::in, list(mer_mode)::in,
    maybe(sort_file_line)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_sort_file_line(InputStream, InputFileName, ModuleInfo,
        FactArgInfos, ArgModes, MaybeSortFileLine, !Specs, !IO) :-
    io.read_line(InputStream, Result, !IO),
    (
        Result = ok(LineChars),
        string.from_char_list(LineChars, LineString),
        split_sort_file_line(ModuleInfo, FactArgInfos, ArgModes, LineString,
            SortFileLine),
        MaybeSortFileLine = yes(SortFileLine)
    ;
        Result = eof,
        MaybeSortFileLine = no
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        Pieces =
            [words("Error reading file"), quote(InputFileName),
            suffix(":"), nl,
            words(ErrorMessage), nl],
        Spec = error_spec($pred, severity_error, phase_fact_table_check,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs],
        MaybeSortFileLine = no
    ).

    % Break up a string into the components of a sort file line.
    %
:- pred split_sort_file_line(module_info::in,
    list(fact_arg_info)::in, list(mer_mode)::in, string::in,
    sort_file_line::out) is det.

split_sort_file_line(ModuleInfo, FactArgInfos, ArgModes, Line0,
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
        get_input_args_list(ModuleInfo, FactArgInfos, ArgModes,
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
        unexpected($pred, "sort file format incorrect")
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
            unexpected($pred, "sort file key format is incorrect")
        )
    ).

:- pred get_input_args_list(module_info::in,
    list(fact_arg_info)::in, list(mer_mode)::in, list(string)::in,
    list(fact_arg)::out) is det.

get_input_args_list(_, [], [], _, []).
get_input_args_list(_, [_ | _], [], _, _) :-
    unexpected($pred, "too many fact_arg_infos").
get_input_args_list(_, [], [_ | _], _, _) :-
    unexpected($pred, "too many argmodes").
get_input_args_list(ModuleInfo, [Info | Infos], [Mode | Modes],
        ArgStrings0, Args) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        (
            ArgStrings0 = [ArgString | ArgStrings],
            Info = fact_arg_info(Type, _, _),
            convert_key_string_to_arg(ArgString, Type, Arg),
            get_input_args_list(ModuleInfo, Infos, Modes,
                ArgStrings, ArgsTail),
            Args = [Arg | ArgsTail]
        ;
            ArgStrings0 = [],
            unexpected($pred, "not enough ArgStrings")
        )
    else
        % This argument is not input, so skip it and try the next one.
        get_input_args_list(ModuleInfo, Infos, Modes, ArgStrings0, Args)
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
            unexpected($pred, "not enough ArgStrings")
        )
    ;
        IsOutput = no,
        % Not an output argument for any mode of the predicate.
        get_output_args_list(Infos, ArgStrings0, Args)
    ).

:- pred convert_key_string_to_arg(string::in, fact_arg_type::in, fact_arg::out)
    is det.

convert_key_string_to_arg(ArgString, Type, Arg) :-
    (
        Type = fact_arg_type_int,
        ( if string.base_string_to_int(36, ArgString, I) then
            Arg = term.integer(base_10, integer(I), signed, size_word)
        else
            unexpected($pred, "could not convert string to int")
        )
    ;
        Type = fact_arg_type_float,
        ( if string.to_float(ArgString, F) then
            Arg = term.float(F)
        else
            unexpected($pred, "could not convert string to float")
        )
    ;
        Type = fact_arg_type_string,
        string.to_char_list(ArgString, Chars0),
        remove_sort_file_escapes(Chars0, [], RevChars),
        list.reverse(RevChars, Chars),
        string.from_char_list(Chars, S),
        Arg = term.string(S)
    ).

    % Remove the escape characters put in the string by make_sort_file_key.
    %
:- pred remove_sort_file_escapes(list(char)::in,
    list(char)::in, list(char)::out) is det.

remove_sort_file_escapes([], !RevChars).
remove_sort_file_escapes([C0 | Cs0], !RevChars) :-
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
                unexpected($pred, "something went wrong")
            ),
            !:RevChars = [C | !.RevChars],
            remove_sort_file_escapes(Cs1, !RevChars)
        ;
            Cs0 = [],
            unexpected($pred, "something went wrong")
        )
    else
        !:RevChars = [C0 | !.RevChars],
        remove_sort_file_escapes(Cs0, !RevChars)
    ).


%---------------------------------------------------------------------------%

:- type maybe_primary_table
    --->    not_primary_table
    ;       primary_table.

:- type maybe_create_fact_map
    --->    do_not_create_fact_map
    ;       create_fact_map.

    % Build and write out a top level hash table and all the lower level
    % tables connected to it.
    %
:- pred build_hash_table(io.text_input_stream::in, string::in,
    io.text_output_stream::in, maybe(io.text_output_stream)::in, int::in,
    module_info::in, maybe_primary_table::in,
    list(mer_mode)::in, list(fact_arg_info)::in, string::in,
    int::in, string::in, int::in, sort_file_line::in, int::in,
    maybe_create_fact_map::in, map(int, int)::in, map(int, int)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_hash_table(InputStream, InputFileName, OutputStream, MaybeDataStream,
        FactTableSize, ModuleInfo, IsPrimaryTable, ArgModes, Infos, StructName,
        InputArgNum, HashTableName, TableNum,
        FirstFact, FactNum, CreateFactMap, !FactMap, !Specs, !IO) :-
    build_hash_table_loop(InputStream, InputFileName, OutputStream,
        MaybeDataStream, FactTableSize, ModuleInfo, IsPrimaryTable,
        ArgModes, Infos, StructName, InputArgNum, HashTableName, TableNum,
        FirstFact, FactNum, CreateFactMap, !FactMap, [], HashList,
        !Specs, !IO),
    list.length(HashList, Len),
    module_info_get_globals(ModuleInfo, Globals),
    calculate_hash_table_size(Globals, Len, HashSize),
    hash_table_init(HashSize, HashTable0),
    hash_table_from_list(HashList, HashSize, HashTable0, HashTable),
    write_hash_table(OutputStream, HashTableName, TableNum, HashTable, !IO).

:- pred build_hash_table_loop(io.text_input_stream::in, string::in,
    io.text_output_stream::in, maybe(io.text_output_stream)::in, int::in,
    module_info::in, maybe_primary_table::in,
    list(mer_mode)::in, list(fact_arg_info)::in, string::in,
    int::in, string::in, int::in, sort_file_line::in, int::in,
    maybe_create_fact_map::in, map(int, int)::in, map(int, int)::out,
    list(hash_entry)::in, list(hash_entry)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_hash_table_loop(InputStream, InputFileName, OutputStream,
        MaybeDataStream, FactTableSize, ModuleInfo, IsPrimaryTable,
        ArgModes, Infos, StructName, InputArgNum, HashTableName, !.TableNum,
        FirstFact, FactNum, CreateFactMap, !FactMap, !HashList, !Specs, !IO) :-
    top_level_collect_matching_facts(InputStream, InputFileName, ModuleInfo,
        Infos, ArgModes, FirstFact, MatchingFacts, MaybeNextFact, !Specs, !IO),
    (
        CreateFactMap = create_fact_map,
        update_fact_map(FactNum, MatchingFacts, !FactMap)
    ;
        CreateFactMap = do_not_create_fact_map
    ),
    module_info_get_globals(ModuleInfo, Globals),
    (
        MaybeDataStream = yes(DataStream),
        OutputData = list.map(
            (func(sort_file_line(_, _, OutArgs)) = OutArgs),
            MatchingFacts),
        get_maybe_progress_output_stream(ModuleInfo, MaybeProgressStream,
            !IO),
        write_fact_table_data(DataStream, MaybeProgressStream, FactTableSize,
            StructName, OutputData, FactNum, !IO)
    ;
        MaybeDataStream = no
    ),
    do_build_hash_table(OutputStream, Globals, IsPrimaryTable,
        !.FactMap, FactNum, InputArgNum, HashTableName,
        MatchingFacts, !TableNum, !HashList, !IO),
    (
        MaybeNextFact = no
    ;
        MaybeNextFact = yes(NextFact),
        list.length(MatchingFacts, Len),
        NextFactNum = FactNum + Len,
        build_hash_table_loop(InputStream, InputFileName, OutputStream,
            MaybeDataStream, FactTableSize, ModuleInfo, IsPrimaryTable,
            ArgModes, Infos, StructName, InputArgNum, HashTableName,
            !.TableNum, NextFact, NextFactNum,
            CreateFactMap, !FactMap, !HashList, !Specs, !IO)
    ).

    % Build a lower level hash table. The main difference to build_hash_table
    % (above) is that ``sort file lines'' are read from a list rather than
    % from the actual sort file.
    %
:- pred build_hash_table_lower_levels(io.text_output_stream::in, globals::in,
    maybe_primary_table::in, map(int, int)::in, int::in, string::in,
    list(sort_file_line)::in, int::in, int::in, int::out,
    io::di, io::uo) is det.

build_hash_table_lower_levels(OutputStream, Globals, IsPrimaryTable, FactMap,
        InputArgNum, HashTableName, Facts, FactNum,
        TableNum0, TableNum, !IO) :-
    build_hash_table_lower_levels_loop(OutputStream, Globals, IsPrimaryTable,
        FactMap, InputArgNum, HashTableName, Facts, FactNum,
        TableNum0, TableNum, [], HashList, !IO),
    list.length(HashList, Len),
    calculate_hash_table_size(Globals, Len, HashSize),
    hash_table_init(HashSize, HashTable0),
    hash_table_from_list(HashList, HashSize, HashTable0, HashTable),
    write_hash_table(OutputStream, HashTableName, TableNum0, HashTable, !IO).

:- pred build_hash_table_lower_levels_loop(io.text_output_stream::in,
    globals::in, maybe_primary_table::in, map(int, int)::in, int::in,
    string::in, list(sort_file_line)::in, int::in, int::in, int::out,
    list(hash_entry)::in, list(hash_entry)::out, io::di, io::uo) is det.

build_hash_table_lower_levels_loop(_, _, _, _, _, _,
        [], _, !TableNum, !HashList, !IO).
build_hash_table_lower_levels_loop(OutputStream, Globals, IsPrimaryTable,
        FactMap, InputArgNum, HashTableName,
        [Fact | Facts0], FactNum, !TableNum, !HashList, !IO) :-
    lower_level_collect_matching_facts(InputArgNum, Fact, Facts0,
        MatchingFacts, RemainingFacts),
    do_build_hash_table(OutputStream, Globals, IsPrimaryTable,
        FactMap, FactNum, InputArgNum, HashTableName,
        MatchingFacts, !TableNum, !HashList, !IO),
    list.length(MatchingFacts, Len),
    NextFactNum = FactNum + Len,
    build_hash_table_lower_levels_loop(OutputStream, Globals, IsPrimaryTable,
        FactMap, InputArgNum, HashTableName,
        RemainingFacts, NextFactNum, !TableNum, !HashList, !IO).

    % This is where most of the actual work is done in building up the
    % hash table.
    %
:- pred do_build_hash_table(io.text_output_stream::in, globals::in,
    maybe_primary_table::in, map(int, int)::in, int::in, int::in, string::in,
    list(sort_file_line)::in, int::in, int::out,
    list(hash_entry)::in, list(hash_entry)::out, io::di, io::uo) is det.

do_build_hash_table(OutputStream, Globals, IsPrimaryTable, FactMap,
        FactNum, InputArgNum, HashTableName, Facts,
        !TableNum, !HashList, !IO) :-
    (
        Facts = [],
        unexpected($pred, "no facts")
    ;
        Facts = [Fact | TailFacts],
        fact_get_arg_and_index(Fact, InputArgNum, Arg, Index),
        (
            IsPrimaryTable = primary_table,
            HashIndex = FactNum
        ;
            IsPrimaryTable = not_primary_table,
            map.lookup(FactMap, Index, HashIndex)
        ),
        (
            TailFacts = [],
            % If only one matching index, insert a pointer to the fact table
            % entry into the current hash table.
            !:HashList = [hash_entry(Arg, fact(HashIndex), -1) | !.HashList]
        ;
            TailFacts = [_ | _],
            ( if
                % See if there are any more input arguments.
                NextInputArgNum = InputArgNum + 1,
                Fact = sort_file_line(InputArgs, _, _),
                N = NextInputArgNum + 1,
                list.drop(N, InputArgs, _)
            then
                !:TableNum = !.TableNum + 1,
                ThisTableNum = !.TableNum,
                build_hash_table_lower_levels(OutputStream, Globals,
                    IsPrimaryTable, FactMap, NextInputArgNum,
                    HashTableName, Facts, FactNum, !TableNum,  !IO),
                EntryIndex = hash_table(ThisTableNum, HashTableName),
                !:HashList = [hash_entry(Arg, EntryIndex, -1) | !.HashList]
            else
                (
                    IsPrimaryTable = primary_table,
                    % Insert only the first matching index into the hash table.
                    Entry = hash_entry(Arg, fact(HashIndex), -1),
                    !:HashList = [Entry | !.HashList]
                ;
                    IsPrimaryTable = not_primary_table,
                    % Insert all matching indexes into the hash table.
                    hash_list_insert_many(IsPrimaryTable, FactMap, FactNum,
                        InputArgNum, Facts, !HashList)
                )
            )
        )
    ).

%---------------------%

    % Read lines from the sort file that that have the same first input
    % argument as Fact. Places these lines into MatchingFacts. The first fact
    % in MatchingFacts is always Fact. If an extra fact is read in following
    % the matching facts, it is placed in MaybeNextFact.
    %
:- pred top_level_collect_matching_facts(io.text_input_stream::in, string::in,
    module_info::in, list(fact_arg_info)::in, list(mer_mode)::in,
    sort_file_line::in, list(sort_file_line)::out, maybe(sort_file_line)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

top_level_collect_matching_facts(InputStream, InputFileName, ModuleInfo,
        Infos, ArgModes, Fact, MatchingFacts, MaybeNextFact, !Specs, !IO) :-
    top_level_collect_matching_facts_loop(InputStream, InputFileName,
        ModuleInfo, Infos, ArgModes, Fact, [], RevMatchingFacts, MaybeNextFact,
        !Specs, !IO),
    list.reverse(RevMatchingFacts, MatchingFactsTail),
    MatchingFacts = [Fact | MatchingFactsTail].

:- pred top_level_collect_matching_facts_loop(io.text_input_stream::in,
    string::in, module_info::in, list(fact_arg_info)::in, list(mer_mode)::in,
    sort_file_line::in, list(sort_file_line)::in, list(sort_file_line)::out,
    maybe(sort_file_line)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

top_level_collect_matching_facts_loop(InputStream, InputFileName,
        ModuleInfo, Infos, ArgModes, Fact, !RevMatchingFacts, MaybeNextFact,
        !Specs, !IO) :-
    read_sort_file_line(InputStream, InputFileName, ModuleInfo,
        Infos, ArgModes, MaybeSortFileLine, !Specs, !IO),
    (
        MaybeSortFileLine = yes(Fact1),
        ( if
            Fact1 = sort_file_line([Arg1 | _], _, _),
            Fact  = sort_file_line([Arg  | _], _, _)
        then
            ( if Arg = Arg1 then
                !:RevMatchingFacts = [Fact1 | !.RevMatchingFacts],
                top_level_collect_matching_facts_loop(InputStream,
                    InputFileName, ModuleInfo, Infos, ArgModes,
                    Fact, !RevMatchingFacts, MaybeNextFact, !Specs, !IO)
            else
                MaybeNextFact = yes(Fact1)
            )
        else
            unexpected($pred, "no input args")
        )
    ;
        MaybeSortFileLine = no,
        MaybeNextFact = no
    ).

    % Same as above, but reads facts from a list instead of from the sort file.
    %
:- pred lower_level_collect_matching_facts(int::in, sort_file_line::in,
    list(sort_file_line)::in, list(sort_file_line)::out,
    list(sort_file_line)::out) is det.

lower_level_collect_matching_facts(InputArgNum, Fact, Facts,
        MatchingFacts, RemainingFacts) :-
    Fact = sort_file_line(InputArgs, _, _),
    list.det_index0(InputArgs, InputArgNum, MatchArg),
    lower_level_collect_matching_facts_loop(InputArgNum, MatchArg, Facts,
        [], RevMatchingFacts, RemainingFacts),
    list.reverse(RevMatchingFacts, TailMatchingFacts),
    MatchingFacts = [Fact | TailMatchingFacts].

:- pred lower_level_collect_matching_facts_loop(int::in, fact_arg::in,
    list(sort_file_line)::in, list(sort_file_line)::in,
    list(sort_file_line)::out, list(sort_file_line)::out) is det.

lower_level_collect_matching_facts_loop(_, _, [], !RevMatchingFacts, []).
lower_level_collect_matching_facts_loop(InputArgNum, MatchArg,
        [Fact | Facts], !RevMatchingFacts, RemainingFacts) :-
    Fact = sort_file_line(InputArgs, _, _),
    list.det_index0(InputArgs, InputArgNum, Arg),
    ( if MatchArg = Arg then
        !:RevMatchingFacts = [Fact | !.RevMatchingFacts],
        lower_level_collect_matching_facts_loop(InputArgNum, MatchArg,
            Facts, !RevMatchingFacts, RemainingFacts)
    else
        RemainingFacts = [Fact | Facts]
    ).

:- pred update_fact_map(int::in, list(sort_file_line)::in,
    map(int, int)::in, map(int, int)::out) is det.

update_fact_map(_, [], !FactMap).
update_fact_map(FactNum, [Fact | Facts], !FactMap) :-
    Fact = sort_file_line(_, Index, _),
    map.set(Index, FactNum, !FactMap),
    update_fact_map(FactNum + 1, Facts, !FactMap).

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
    find_first_big_enough_prime(N, Primes, HashTableSize).

:- pred find_first_big_enough_prime(int::in, list(int)::in, int::out) is det.

find_first_big_enough_prime(_, [], _) :-
    unexpected($pred, "hash table too large (max size 2147483647)").
find_first_big_enough_prime(NumSlotsNeeded, [Prime | Primes], Size) :-
    ( if Prime > NumSlotsNeeded then
        Size = Prime
    else
        find_first_big_enough_prime(NumSlotsNeeded, Primes, Size)
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
        hash_table_insert_open_address_loop(HashVal, Key, Index, !HashTable)
    else
        hash_table_set(HashVal, hash_entry(Key, Index, -1), !HashTable)
    ).

:- pred hash_table_insert_open_address_loop(int::in,
    fact_arg::in, hash_index::in, hash_table::in, hash_table::out) is det.

hash_table_insert_open_address_loop(HashVal, Key0, Index0,
        !HashTable) :-
    ( if hash_table_search(!.HashTable, HashVal, OldEntry1) then
        OldEntry1 = hash_entry(Key1, Index1, Next),
        ( if Next = -1 then
            get_free_hash_slot(!.HashTable, HashVal, FreeVal),
            NewEntry = hash_entry(Key0, Index0, -1),
            hash_table_set(FreeVal, NewEntry, !HashTable),
            OldEntry = hash_entry(Key1, Index1, FreeVal),
            hash_table_set(HashVal, OldEntry, !HashTable)
        else
            hash_table_insert_open_address_loop(Next, Key0, Index0, !HashTable)
        )
    else
        unexpected($pred, "hash table entry empty")
    ).

    % Probe through the hash table to find a free slot. This will eventually
    % terminate because the hash table size is selected to be larger than
    % the number of entries that need to go in it.
    %
    % XXX "This will eventually terminate" is not something one enjoys reading
    % in the documentation the implementation of a language feature
    % whose only purpose is to improve compiler performance :-(
    %
:- pred get_free_hash_slot(hash_table::in, int::in, int::out) is det.

get_free_hash_slot(HashTable, Start, Free) :-
    HashTable = hash_table(Size, _),
    Max = Size - 1,
    get_free_hash_slot_loop(HashTable, Start, Max, Free).

:- pred get_free_hash_slot_loop(hash_table::in, int::in, int::in, int::out)
    is det.

get_free_hash_slot_loop(HashTable, Start, Max, Free) :-
    Next = (Start + 1) mod Max,
    ( if hash_table_search(HashTable, Next, _) then
        get_free_hash_slot_loop(HashTable, Next, Max, Free)
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
        unexpected($pred, "unsupported type in key")
    ),
    fact_table_hash_2(HashSize, Ns, 0, HashVal).

:- pred fact_table_hash_2(int::in, list(int)::in, int::in, int::out) is det.

fact_table_hash_2(_, [], !HashVal).
fact_table_hash_2(HashSize, [N | Ns], !HashVal) :-
    !:HashVal = (N + 31 * !.HashVal) mod HashSize,
    fact_table_hash_2(HashSize, Ns, !HashVal).

:- pred hash_list_insert_many(maybe_primary_table::in,
    map(int, int)::in, int::in, int::in ,list(sort_file_line)::in,
    list(hash_entry)::in, list(hash_entry)::out) is det.

hash_list_insert_many(_, _, _, _, [], !HashList).
hash_list_insert_many(IsPrimaryTable, FactMap, FactNum, InputArgNum,
        [Fact | Facts], !HashList) :-
    fact_get_arg_and_index(Fact, InputArgNum, Arg, Index),
    (
        IsPrimaryTable = primary_table,
        HashIndex = FactNum
    ;
        IsPrimaryTable = not_primary_table,
        map.lookup(FactMap, Index, HashIndex)
    ),
    !:HashList = [hash_entry(Arg, fact(HashIndex), -1) | !.HashList],
    hash_list_insert_many(IsPrimaryTable, FactMap, FactNum, InputArgNum,
        Facts, !HashList).

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

%---------------------%

:- pred fact_get_arg_and_index(sort_file_line::in, int::in, fact_arg::out,
    int::out) is det.

fact_get_arg_and_index(Fact, InputArgNum, Arg, Index) :-
    Fact = sort_file_line(InputArgs, Index, _),
    list.det_index0(InputArgs, InputArgNum, Arg).

%---------------------------------------------------------------------------%

    % Write out the C code for a hash table.
    %
:- pred write_hash_table(io.text_output_stream::in, string::in, int::in,
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
    StructDefnTemplate = "

struct MR_fact_table_hash_table_%c %s%d = {
    %d,
    %s%d_data
};
",
    io.format(OutputStream, StructDefnTemplate,
        [c(TableType), s(BaseName), i(TableNum), i(Size),
        s(BaseName), i(TableNum)], !IO).

:- pred write_hash_table_loop(io.text_output_stream::in, hash_table::in,
    int::in, int::in, io::di, io::uo) is det.

write_hash_table_loop(Stream, HashTable, CurIndex, MaxIndex, !IO) :-
    ( if CurIndex > MaxIndex then
        true
    else
        io.write_string(Stream, "\t{ ", !IO),
        ( if hash_table_search(HashTable, CurIndex, HashEntry) then
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
        write_hash_table_loop(Stream, HashTable, CurIndex + 1, MaxIndex, !IO)
    ).

%---------------------%

    % Return 's' for string, 'i' for int, 'f' for float, 'a' for atom.
    % Don't call this with an empty hash table.
    %
:- pred get_hash_table_type(hash_table::in, char::out) is det.

get_hash_table_type(HashTable, TableType) :-
    HashTable = hash_table(_Size, Map),
    ( if map.is_empty(Map) then
        unexpected($pred, "empty hash table")
    else
        get_hash_table_type_loop(Map, 0, TableType)
    ).

:- pred get_hash_table_type_loop(map(int, hash_entry)::in, int::in, char::out)
    is det.

get_hash_table_type_loop(Map, Index, TableType) :-
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
            unexpected($pred, "invalid term")
        )
    else
        get_hash_table_type_loop(Map, Index + 1, TableType)
    ).

%---------------------%

    % Write out the array of pointers to the fact table arrays.
    %
:- pred write_fact_table_pointer_array(io.text_output_stream::in, int::in,
    string::in, int::in, string::out, io::di, io::uo) is det.

write_fact_table_pointer_array(OutputStream, FactTableSize,
        StructName, NumFacts, HeaderCode, !IO) :-
    string.format("const struct %s_struct *%s[]",
        [s(StructName), s(StructName)], PointerArrayName),
    HeaderCode = "extern " ++ PointerArrayName ++ ";\n",
    io.format(OutputStream, "%s = {\n", [s(PointerArrayName)], !IO),
    write_fact_table_pointer_array_loop(OutputStream, FactTableSize,
        StructName, 0, NumFacts, !IO),
    io.write_string(OutputStream, "};\n", !IO).

:- pred write_fact_table_pointer_array_loop(io.text_output_stream::in, int::in,
    string::in, int::in, int::in, io::di, io::uo) is det.

write_fact_table_pointer_array_loop(OutputStream, FactTableSize,
        StructName, CurFact, NumFacts, !IO) :-
    ( if CurFact >= NumFacts then
        true
    else
        io.format(OutputStream, "\t%s%d,\n", [s(StructName), i(CurFact)], !IO),
        NextFact = CurFact + FactTableSize,
        write_fact_table_pointer_array_loop(OutputStream, FactTableSize,
            StructName, NextFact, NumFacts, !IO)
    ).

%---------------------%

:- pred write_fact_table_numfacts(io.text_output_stream::in,
    sym_name::in, int::in, string::out, io::di, io::uo) is det.

write_fact_table_numfacts(OutputStream, PredName, NumFacts, HeaderCode, !IO) :-
    % Write out the size of the fact table.
    make_fact_table_identifier(PredName, Identifier),
    io.format(OutputStream,
        "const MR_Integer mercury__%s_fact_table_num_facts = %d;\n\n",
        [s(Identifier), i(NumFacts)], !IO),
    string.format(
        "extern const MR_Integer mercury__%s_fact_table_num_facts;\n",
        [s(Identifier)], HeaderCode).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fact_table_generate_c_code(ModuleInfo, PredName, ProcID, PrimaryProcID,
        ProcInfo, PragmaVars, ArgTypes, ProcCode, ExtraCode, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    fact_table_size(Globals, FactTableSize),

    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_interface_determinism(ProcInfo, Determinism),
    compute_fact_table_mode_type(ModuleInfo, ArgModes, ModeType),
    make_fact_table_identifier(PredName, Identifier),
    ( if
        ModeType = all_out,
        Determinism = detism_multi
    then
        generate_multi_code(ModuleInfo, FactTableSize, Identifier, ProcID,
            PragmaVars, ArgTypes, ProcCode, ExtraCode)
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
        generate_semidet_all_in_code(ModuleInfo, FactTableSize,
            Identifier, ProcID, PragmaVars, ArgTypes, ProcCode),
        ExtraCode = ""
    else if
        ModeType = in_out,
        ( Determinism = detism_semi
        ; Determinism = detism_cc_non
        )
    then
        generate_semidet_in_out_code(ModuleInfo, FactTableSize,
            Identifier, ProcID, PragmaVars, ArgTypes, ProcCode),
        ExtraCode = ""
    else if
        ModeType = in_out,
        Determinism = detism_non
    then
        ( if ProcID = PrimaryProcID then
            generate_primary_nondet_code(ModuleInfo, FactTableSize,
                Identifier, ProcID, PragmaVars, ArgTypes, ProcCode, ExtraCode)
        else
            generate_secondary_nondet_code(ModuleInfo, FactTableSize,
                Identifier, ProcID, PragmaVars, ArgTypes, ProcCode, ExtraCode)
        )
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
%
% Implement detism_multi procedures.
%

:- pred generate_multi_code(module_info::in, int::in,
    string::in, proc_id::in, list(pragma_var)::in, list(mer_type)::in,
    string::out, string::out) is det.

generate_multi_code(ModuleInfo, FactTableSize, PredName, ProcID,
        PragmaVars, ArgTypes, ProcCode, ExtraCode) :-
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

    NumFactsVar = "mercury__" ++ PredName ++ "_fact_table_num_facts",
    list.length(PragmaVars, Arity),
    generate_argument_vars_code(ModuleInfo, PragmaVars, ArgTypes,
        ArgDeclCode, _InputCode, OutputCode, _, _, _),
    generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, 1, FactLookupCode),

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

    list.length(PragmaVars, Arity),
    proc_id_to_int(ProcID, ProcInt),
    string.format("mercury__%s_%d_%d_xx",
        [s(PredName), i(Arity), i(ProcInt)], ExtraCodeLabel),
    pragma_vars_to_names_string(PragmaVars, NamesString),
    string.format(ProcCodeTemplate, [s(NamesString), s(ExtraCodeLabel),
        s(ExtraCodeLabel)], ProcCode).

%---------------------------------------------------------------------------%
%
% Implement detism_cc_multi procedures.
%

    % For cc_multi output mode, just return the first fact in the table.
    %
:- pred generate_cc_multi_code(string::in, list(pragma_var)::in, string::out)
    is det.

generate_cc_multi_code(PredName, PragmaVars, ProcCode) :-
    % ZZZ
    string.append_list(["mercury__", PredName, "_fact_table"], StructName),
    generate_cc_multi_code_loop(StructName, PragmaVars, 1, "", ProcCode).

:- pred generate_cc_multi_code_loop(string::in, list(pragma_var)::in, int::in,
    string::in, string::out) is det.

generate_cc_multi_code_loop(_, [], _, !ProcCode).
generate_cc_multi_code_loop(StructName, [PragmaVar  | PragmaVars], ArgNum,
        !ProcCode) :-
    % ZZZ cf generate_argument_vars_code
    PragmaVar = pragma_var(_, VarName, _, _),
    string.format("\t\t%s = %s[0][0].V_%d;\n", [s(VarName), s(StructName),
        i(ArgNum)], NewProcCode),
    % ZZZ
    string.append(NewProcCode, !ProcCode),
    generate_cc_multi_code_loop(StructName, PragmaVars, ArgNum + 1, !ProcCode).

%---------------------------------------------------------------------------%
%
% Implement detism_non procedures.
%

    % Generate code for the nondet mode with the primary key.
    %
:- pred generate_primary_nondet_code(module_info::in, int::in,
    string::in, proc_id::in, list(pragma_var)::in, list(mer_type)::in,
    string::out, string::out) is det.

generate_primary_nondet_code(ModuleInfo, FactTableSize, PredName, ProcID,
        PragmaVars, ArgTypes, ProcCode, ExtraCode) :-
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

    generate_argument_vars_code(ModuleInfo, PragmaVars, ArgTypes,
        ArgDeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        NumFrameVars),
    generate_decl_code(PredName, ProcID, DeclCode),
    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(ModuleInfo, FactTableSize, PredName, LabelName, 0,
        PragmaVars, ArgTypes, 1, HashCode),
    generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, 1, FactLookupCode),
    generate_fact_test_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, FactTestCode),

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

    % Generate code for a nondet mode using a secondary key.
    %
:- pred generate_secondary_nondet_code(module_info::in, int::in, string::in,
    proc_id::in, list(pragma_var)::in, list(mer_type)::in,
    string::out, string::out) is det.

generate_secondary_nondet_code(ModuleInfo, FactTableSize, PredName, ProcID,
        PragmaVars, ArgTypes, ProcCode, ExtraCode) :-
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

    generate_argument_vars_code(ModuleInfo, PragmaVars, ArgTypes, ArgDeclCode,
        InputCode, OutputCode, _SaveRegsCode, _GetRegsCode, _NumFrameVars),
    generate_decl_code(PredName, ProcID, DeclCode),
    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    string.append(LabelName, "_2", LabelName2),
    generate_hash_code(ModuleInfo, FactTableSize, PredName, LabelName, 0,
        PragmaVars, ArgTypes, 1, HashCode),

    generate_hash_lookup_code(ModuleInfo,
        "(char *) MR_framevar(4)", LabelName2, 0, string_equals,
        's', do_not_test_keys, "", [], [], 0, 0, StringHashLookupCode),
    generate_hash_lookup_code(ModuleInfo,
        "MR_framevar(4)", LabelName2, 1, plain_equals,
        'i', do_not_test_keys, "", [], [], 0, 0, IntHashLookupCode),
    generate_hash_lookup_code(ModuleInfo,
        "MR_word_to_float(MR_framevar(4))", LabelName2, 2, plain_equals,
        'f', do_not_test_keys, "", [], [], 0, 0, FloatHashLookupCode),
    generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, 1, FactLookupCode),
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
%
% Implement detism_semi procedures.
%

    % Generate semidet code for all_in mode.
    %
:- pred generate_semidet_all_in_code(module_info::in, int::in, string::in,
    proc_id::in, list(pragma_var)::in, list(mer_type)::in, string::out) is det.

generate_semidet_all_in_code(ModuleInfo, FactTableSize, PredName, ProcID,
        PragmaVars, ArgTypes, ProcCode) :-
    generate_decl_code(PredName, ProcID, DeclCode),

    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(ModuleInfo, FactTableSize, PredName, LabelName, 0,
        PragmaVars, ArgTypes, 1, HashCode),

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
:- pred generate_semidet_in_out_code(module_info::in, int::in,
    string::in, proc_id::in, list(pragma_var)::in, list(mer_type)::in,
    string::out) is det.

generate_semidet_in_out_code(ModuleInfo, FactTableSize, PredName, ProcID,
        PragmaVars, ArgTypes, ProcCode):-
    generate_decl_code(PredName, ProcID, DeclCode),

    proc_id_to_int(ProcID, ProcInt),
    string.format("%s_%d", [s(PredName), i(ProcInt)], LabelName),
    generate_hash_code(ModuleInfo, FactTableSize, PredName, LabelName, 0,
        PragmaVars, ArgTypes, 1, HashCode),

    SuccessCodeTemplate = "
        success_code_%s:
            SUCCESS_INDICATOR = MR_TRUE;
    ",
    string.format(SuccessCodeTemplate, [s(LabelName)], SuccessCode),

    generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, 1, FactLookupCode),

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

generate_decl_code(Name, ProcID, DeclCode) :-
    DeclCodeTemplate = "
            MR_Integer hashval, hashsize;
            MR_Word ind;
            void *current_table;
            char keytype = '\\0';
            MR_Word current_key, tmp;

            // Initialise current_table to the top level hash table
            // for this ProcID.
            current_table =
                &mercury__%s_fact_table_hash_table_%d_0;

    ",
    proc_id_to_int(ProcID, ProcInt),
    string.format(DeclCodeTemplate, [s(Name), i(ProcInt)], DeclCode).

    % Generate code to calculate hash values and lookup the hash tables.
    %
:- pred generate_hash_code(module_info::in, int::in, string::in,
    string::in, int::in, list(pragma_var)::in, list(mer_type)::in, int::in,
    string::out) is det.

generate_hash_code(_, _, _, _, _, [], [], _, "").
generate_hash_code(_, _, _, _, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
generate_hash_code(_, _, _, _, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
generate_hash_code(ModuleInfo, FactTableSize, PredName, LabelName, LabelNum,
        [PragmaVar | PragmaVars], [Type | Types], ArgNum, Code) :-
    PragmaVar = pragma_var(_, Name, Mode, _),
    NextArgNum = ArgNum + 1,
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ( if Type = builtin_type(builtin_type_int(int_type_int)) then
            generate_hash_int_code(ModuleInfo, Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, NextArgNum,
                FactTableSize, ArgCode)
        else if Type = builtin_type(builtin_type_float) then
            generate_hash_float_code(ModuleInfo, Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, NextArgNum,
                FactTableSize, ArgCode)
        else if Type = builtin_type(builtin_type_string) then
            generate_hash_string_code(ModuleInfo, Name, LabelName, LabelNum,
                PredName, PragmaVars, Types, NextArgNum,
                FactTableSize, ArgCode)
        else
            unexpected($pred, "unsupported type")
        ),
        generate_hash_code(ModuleInfo, FactTableSize, PredName,
            LabelName, LabelNum + 1, PragmaVars, Types, NextArgNum, ArgsCode),
        Code = ArgCode ++ ArgsCode
    else
        % Skip non-input arguments.
        generate_hash_code(ModuleInfo, FactTableSize, PredName,
            LabelName, LabelNum, PragmaVars, Types, NextArgNum, Code)
    ).

:- pred generate_hash_int_code(module_info::in,string::in, string::in, int::in,
    string::in, list(pragma_var)::in, list(mer_type)::in,
    int::in, int::in, string::out) is det.

generate_hash_int_code(ModuleInfo, Name, LabelName, LabelNum, PredName,
        PragmaVars, Types, ArgNum, FactTableSize, Code) :-
    generate_hash_lookup_code(ModuleInfo, Name, LabelName, LabelNum,
        plain_equals, 'i', test_keys, PredName, PragmaVars, Types, ArgNum,
        FactTableSize, HashLookupCode),
    CodeTemplate = "

        // calculate hash value for an integer

        hashsize = ((struct MR_fact_table_hash_table_i *)current_table)
            ->size;

        hashval = (%s >= 0 ? %s : -%s) %% hashsize;

        current_key = %s;

        // lookup the hash table
        %s

    ",
    string.format(CodeTemplate,
        [s(Name), s(Name), s(Name), s(Name), s(HashLookupCode)], Code).

:- pred generate_hash_float_code(module_info::in, string::in, string::in,
    int::in, string::in, list(pragma_var)::in, list(mer_type)::in,
    int::in, int::in, string::out) is det.

generate_hash_float_code(ModuleInfo, Name, LabelName, LabelNum, PredName,
        PragmaVars, Types, ArgNum, FactTableSize, Code) :-
    generate_hash_lookup_code(ModuleInfo, Name, LabelName, LabelNum,
        plain_equals, 'f', test_keys, PredName, PragmaVars, Types, ArgNum,
        FactTableSize, HashLookupCode),
    CodeTemplate = "

        // calculate hash value for a float

        hashsize = ((struct MR_fact_table_hash_table_f *)current_table)
            ->size;

        hashval = MR_hash_float(%s);
        hashval = (hashval >= 0 ? hashval : -hashval) %% hashsize;

        current_key = MR_float_to_word(%s);

        // lookup the hash table
        %s

    ",
    string.format(CodeTemplate, [s(Name), s(Name), s(HashLookupCode)], Code).

:- pred generate_hash_string_code(module_info::in, string::in, string::in,
    int::in, string::in, list(pragma_var)::in, list(mer_type)::in,
    int::in, int::in, string::out) is det.

generate_hash_string_code(ModuleInfo, Name, LabelName, LabelNum, PredName,
        PragmaVars, Types, ArgNum, FactTableSize, Code) :-
    generate_hash_lookup_code(ModuleInfo, Name, LabelName, LabelNum,
        string_equals, 's', test_keys, PredName, PragmaVars, Types, ArgNum,
        FactTableSize, HashLookupCode),
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
    string.format(CodeTemplate, [s(Name), s(Name), s(HashLookupCode)], Code).

%---------------------%

:- type comparison_kind
    --->    plain_equals
    ;       string_equals.

    % XXX CLEANUP Due to a compiler limitation, we can't specify
    % that KeyType below has mode in(key_char), not simply in.
:- inst key_char for char/0
    --->    ('s')
    ;       ('i')
    ;       ('f').

:- type maybe_test_keys
    --->    do_not_test_keys
    ;       test_keys.

    % Generate code to lookup the key in the hash table.
    % KeyType should be 's', 'i' or 'f' for string, int or float,
    % respectively. CompareTemplate should be a template for testing for
    % equality for the type given, e.g. "%s == %s" for ints,
    % "strcmp(%s, %s) == 0" for strings.
    %
:- pred generate_hash_lookup_code(module_info::in, string::in, string::in,
    int::in, comparison_kind::in, char::in, maybe_test_keys::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, int::in, int::in,
    string::out) is det.

generate_hash_lookup_code(ModuleInfo, VarName, LabelName, LabelNum,
        ComparisonKind, KeyType, TestKeys, PredName, PragmaVars, Types,
        ArgNum, FactTableSize, HashLookupCode) :-
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
            if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) != 0 && %s)
            {
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
        TestKeys = test_keys,
        FactTableName = "mercury__" ++ PredName ++ "_fact_table",
        generate_test_condition_code(FactTableSize, ModuleInfo, FactTableName,
            PragmaVars, Types, ArgNum, have_not_seen_input_arg, CondCode),
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
:- pred generate_fact_lookup_code(module_info::in, int::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, int::in, string::out) is det.

generate_fact_lookup_code(_, _, _, [], [], _, "").
generate_fact_lookup_code(_, _, _, [_ | _], [], _, _) :-
    unexpected($pred, "too many pragma vars").
generate_fact_lookup_code(_, _, _, [], [_ | _], _, _) :-
    unexpected($pred, "too many types").
generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
        [PragmaVar | PragmaVars], [Type | Types], ArgNum, Code) :-
    PragmaVar = pragma_var(_, VarName, Mode, _),
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
                string.format(Template, [s(VarName), s(TableEntry)], ArgCode)
            else
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
        else
            Template = "\t\t%s = %s;\n",
            string.format(Template, [s(VarName), s(TableEntry)], ArgCode)
        ),
        generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
            PragmaVars, Types, NextArgNum, ArgsCode),
        Code = ArgCode ++ ArgsCode
    else
        % Skip non-output arguments.
        generate_fact_lookup_code(ModuleInfo, FactTableSize, PredName,
            PragmaVars, Types, NextArgNum, Code)
    ).

    % Generate code to create argument variables and assign them to registers.
    %
:- pred generate_argument_vars_code(module_info::in,
    list(pragma_var)::in, list(mer_type)::in,
    string::out, string::out, string::out,
    string::out, string::out, int::out) is det.

generate_argument_vars_code(ModuleInfo, PragmaVars, Types, DeclCode, InputCode,
        OutputCode, SaveRegsCode, GetRegsCode, NumInputArgs) :-
    Modes = list.map((func(pragma_var(_, _, Mode, _)) = Mode), PragmaVars),
    make_standard_arg_infos(ModuleInfo, model_non, Types, Modes, ArgInfos),
    generate_argument_vars_code_loop(ModuleInfo, PragmaVars, ArgInfos, Types,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        1, NumInputArgs).

:- pred generate_argument_vars_code_loop(module_info::in,
    list(pragma_var)::in, list(arg_info)::in, list(mer_type)::in,
    string::out, string::out, string::out,
    string::out, string::out, int::in, int::out) is det.

generate_argument_vars_code_loop(ModuleInfo, PragmaVars, ArgInfos, Types,
        DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode,
        !NumInputArgs) :-
    ( if
        PragmaVars = [],
        ArgInfos = [],
        Types = []
    then
        DeclCode = "",
        InputCode = "",
        OutputCode = "",
        SaveRegsCode = "",
        GetRegsCode = ""
    else if
        PragmaVars = [pragma_var(_, VarName, _, _) | TailPragmaVars],
        ArgInfos = [arg_info(Loc, ArgMode) | TailArgInfos],
        Types = [Type | TailTypes]
    then
        generate_arg_decl_code(ModuleInfo, VarName, Type, ArgDeclCode),
        ( if ArgMode = top_in then
            !:NumInputArgs = !.NumInputArgs + 1,
            generate_arg_input_code(VarName, Type, Loc, !.NumInputArgs,
                ArgInputCode, ArgSaveRegsCode, ArgGetRegsCode),
            ArgOutputCode = ""
        else if ArgMode = top_out then
            generate_arg_output_code(VarName, Type, Loc, ArgOutputCode),
            ArgInputCode = "",
            ArgSaveRegsCode = "",
            ArgGetRegsCode = ""
        else
            unexpected($pred, "invalid mode")
        ),
        generate_argument_vars_code_loop(ModuleInfo,
            TailPragmaVars, TailArgInfos, TailTypes,
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

:- pred generate_arg_decl_code(module_info::in, string::in, mer_type::in,
    string::out) is det.

generate_arg_decl_code(ModuleInfo, Name, Type, DeclCode) :-
    CType = exported_type_to_c_string(ModuleInfo, Type),
    string.format("\t\t%s %s;\n", [s(CType), s(Name)], DeclCode).

:- pred generate_arg_input_code(string::in, mer_type::in, arg_loc::in, int::in,
    string::out, string::out, string::out) is det.

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

%---------------------%

    % Generate code to test that the fact found matches the input arguments.
    % This is only required for generate_primary_nondet_code. Other procedures
    % can test the key in the hash table against the input arguments.
    %
:- pred generate_fact_test_code(module_info::in, int::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, string::out) is det.

generate_fact_test_code(ModuleInfo, FactTableSize, PredName,
        PragmaVars, ArgTypes, FactTestCode) :-
    FactTableName = "mercury__" ++ PredName ++ "_fact_table",
    generate_test_condition_code(FactTableSize, ModuleInfo, FactTableName,
        PragmaVars, ArgTypes, 1, have_not_seen_input_arg, CondCode),
    FactTestCode = "\t\tif(" ++ CondCode ++ "\t\t) MR_fail();\n".

:- type maybe_seen_input_arg
    --->    have_seen_input_arg
    ;       have_not_seen_input_arg.

:- pred generate_test_condition_code(int::in, module_info::in, string::in,
    list(pragma_var)::in, list(mer_type)::in, int::in,
    maybe_seen_input_arg::in, string::out) is det.

generate_test_condition_code(_, _, _, [], [], _, _, "").
generate_test_condition_code(_, _, _, [_ | _], [], _, _, "") :-
    unexpected($pred, "too many PragmaVars").
generate_test_condition_code(_, _, _, [], [_ | _], _, _, "") :-
    unexpected($pred, "too many ArgTypes").
generate_test_condition_code(FactTableSize, ModuleInfo, FactTableName,
        [PragmaVar | PragmaVars], [Type | Types], ArgNum, !.IsFirstInputArg,
        CondCode) :-
    PragmaVar = pragma_var(_, Name, Mode, _),
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ( if Type = builtin_type(builtin_type_string) then
            Template = "strcmp(%s[ind/%d][ind%%%d].V_%d, %s) != 0\n",
            string.format(Template, [s(FactTableName), i(FactTableSize),
                i(FactTableSize), i(ArgNum), s(Name)], ArgCondCode0)
        else
            Template = "%s[ind/%d][ind%%%d].V_%d != %s\n",
            string.format(Template, [s(FactTableName), i(FactTableSize),
                i(FactTableSize), i(ArgNum), s(Name)], ArgCondCode0)
        ),
        (
            !.IsFirstInputArg = have_seen_input_arg,
            ArgCondCode = "\t\t|| " ++ ArgCondCode0
        ;
            !.IsFirstInputArg = have_not_seen_input_arg,
            ArgCondCode = ArgCondCode0
        ),
        !:IsFirstInputArg = have_seen_input_arg
    else
        ArgCondCode = ""
    ),
    generate_test_condition_code(FactTableSize, ModuleInfo, FactTableName,
        PragmaVars, Types, ArgNum + 1, !.IsFirstInputArg, ArgsCondCode),
    CondCode = ArgCondCode ++ ArgsCondCode.

%---------------------------------------------------------------------------%

:- pred make_fact_table_identifier(sym_name::in, string::out) is det.

make_fact_table_identifier(SymName, Identifier) :-
    Identifier = sym_name_mangle(SymName).

%---------------------------------------------------------------------------%

    % pragma_vars_to_names_string(PragmaVars, NamesString):
    %
    % Create a string containing the names of the pragma vars separated by
    % a space.
    %
:- pred pragma_vars_to_names_string(list(pragma_var)::in, string::out) is det.

pragma_vars_to_names_string([], "").
pragma_vars_to_names_string([pragma_var(_, Name, _, _) | PragmaVars],
        NamesString) :-
    pragma_vars_to_names_string(PragmaVars, NamesString0),
    string.append_list([Name, ", ", NamesString0], NamesString).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

    % Delete a file. Report an error message if something goes wrong.
    %
:- pred delete_temporary_file(string::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

delete_temporary_file(FileName, !Specs, !IO) :-
    io.remove_file(FileName, Result, !IO),
    (
        Result = ok
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMsg),
        io.progname_base("mercury_compile", ProgName, !IO),
        Pieces = [fixed(ProgName), suffix(":"), words("error deleting file"),
            quote(FileName), suffix(":"), nl,
            words(ErrorMsg), suffix("."), nl],
        Spec = error_spec($pred, severity_error, phase_fact_table_check,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%

:- pred add_call_system_error(string::in, io.error::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

add_call_system_error(Cmd, ErrorCode, !Specs, !IO) :-
    io.error_message(ErrorCode, ErrorMsg),
    io.progname_base("mercury_compile", ProgName, !IO),
    Pieces = [fixed(ProgName), suffix(":"),
        words("error executing system command"), quote(Cmd), suffix(":"), nl,
        words(ErrorMsg), suffix("."), nl],
    Spec = error_spec($pred, severity_error, phase_fact_table_check,
        [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_file_open_error(maybe(context)::in, string::in, string::in,
    io.error::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

add_file_open_error(MaybeContext, FileName, InOrOut, Error, !Specs, !IO) :-
    io.error_message(Error, ErrorMsg),
    io.progname_base("mercury_compile", ProgName, !IO),
    Pieces = [fixed(ProgName), suffix(":"),
        words("error opening file"), quote(FileName),
        words("for"), words(InOrOut), suffix(":"), nl,
        words(ErrorMsg), nl],
    Spec = error_spec($pred, severity_error, phase_fact_table_check,
        [error_msg(MaybeContext, treat_as_first, 0, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_error_context_and_pieces(prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_error_context_and_pieces(Context, Pieces, !Specs) :-
    Spec = simplest_spec($pred, severity_error, phase_fact_table_check,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred add_error_pieces(list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_error_pieces(Pieces, !Specs) :-
    Spec = error_spec($pred, severity_error, phase_fact_table_check,
        [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred get_maybe_progress_output_stream(module_info::in,
    maybe(io.text_output_stream)::out, io::di, io::uo) is det.

get_maybe_progress_output_stream(ModuleInfo, MaybeProgressStream, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no,
        MaybeProgressStream = no
    ;
        VeryVerbose = yes,
        get_progress_output_stream(ModuleInfo, ProgressStream, !IO),
        MaybeProgressStream = yes(ProgressStream)
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.fact_table.
%---------------------------------------------------------------------------%
