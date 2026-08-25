%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: fact_table_compile.m.
% Main author: dmo.
%
% This module handles compilation of fact tables contained in external files
% that have been declared with a `pragma fact_table' declaration, after the
% compatibility of the declaration with the predicate or function it is
% intended to be applied has been verified by fact_table_check.m.
%
% XXX The following description needs significant improvement.
% Besides massively overusing passive voice, it also blurs the distinction
% between "mode of a predicate", meaning "a vector of argument modes",
% and individual "argument mode"s themselves.
%
% The facts are processed one by one. Each fact is read in and type and mode
% checked. If there are no modes with input arguments, the data is written
% out to arrays of C structures as each fact is processed. If there are input
% modes, the input arguments for each mode are written out to a temporary
% sort file -- one sort file per input mode. The output arguments are also
% included in the sort file for the primary input mode. (At the moment,
% the primary input mode is the one with the lowest ProcId number, however
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
% the inferred determinism later on in det_check_proc.m.
%
% XXX All combinations of `in' and `out' arguments are now supported for all
% determinisms. Only the builtin `string', `int' and `float' types are
% supported at the moment.
%
% XXX Cross compilation is not supported for fact tables that are indexed on
% floats.
%
%---------------------------------------------------------------------------%

:- module ll_backend.fact_table_compile.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_proc_id.
:- import_module ll_backend.fact_table_check.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % fact_table_compile_facts(ProgressStream, ModuleInfo, FactTableFileName,
    %   Context, GenInfo, HeaderCode, PrimaryProcId, !PredInfo, !Specs, !IO):
    %
    % Compile the fact table in FactFileName for PredInfo into a separate .c
    % file. Return error specs for any errors discovered while processing the
    % contents of FactTableFileName, such as syntax errors (such as found fact
    % for wrong predicate, found fact with wrong number of arguments, found
    % fact in function form when expecting fact for predicate), type errors
    % (expected int, found float) and mode errors (expected int,
    % found variable).
    %
:- pred fact_table_compile_facts(io.text_output_stream::in, module_info::in,
    string::in, prog_context::in, fact_table_gen_info::in, string::out,
    proc_id::out, pred_info::in, pred_info::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_proc.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.system_cmds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module io.call_system.
:- import_module io.file.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Proc_stream contains information about an open sort file for
    % a particular procedure.
    %
:- type proc_stream
    --->    proc_stream(
                proc_id,                % ID of procedure.
                list(fact_table_mode),  % The modes of the arguments.
                string,                 % The name of the sort file.
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

    % Sort_file_line contains the information read in from a sort file
    % after sorting.
:- type sort_file_line
    --->    sort_file_line(
                list(fact_arg),     % Input arguments.
                int,                % Index of fact in original file.
                list(fact_arg)      % Output arguments.
            ).

:- type fact_arg
    --->    fact_arg_int(int)
    ;       fact_arg_float(float)
    ;       fact_arg_string(string).

:- type inferred_determinism
    --->    inferred(determinism)   % Determinism has been inferred.
    ;       not_yet                 % Determinism has not yet been inferred.
    ;       error.                  % An error occurred trying to infer
                                    % determinism.

%---------------------------------------------------------------------------%

fact_table_compile_facts(ProgressStream, ModuleInfo, FactTableFileName,
        Context, GenInfo, HeaderCode, PrimaryProcId, !PredInfo, !Specs, !IO) :-
    io.open_input(FactTableFileName, FactTableFileResult, !IO),
    (
        FactTableFileResult = ok(FactTableFileStream),
        module_info_get_globals(ModuleInfo, Globals),
        % XXX LEGACY
        fact_table_file_name_return_dirs(Globals, $pred,
            ext_cur_ngs_gs(ext_cur_ngs_gs_target_c), FactTableFileName,
            Dirs, _DirsProposed, OutputFileName, _OutFileNameProposed),
        create_any_dirs_on_path(Dirs, !IO),
        io.open_output(OutputFileName, OpenResult, !IO),
        (
            OpenResult = ok(OutputStream),
            pred_info_get_module_name(!.PredInfo, ModuleName),
            pred_info_get_name(!.PredInfo, PredName),
            PredSymName = qualified(ModuleName, PredName),
            globals.lookup_int_option(Globals, fact_table_max_array_size,
                FactTableArraySize),
            get_maybe_progress_output_stream(ModuleInfo, ProgressStream,
                MaybeProgressStream),
            compile_fact_table_in_file(MaybeProgressStream,
                FactTableFileStream, FactTableFileName, OutputStream,
                FactTableArraySize, ModuleInfo, PredSymName, GenInfo,
                HeaderCode, PrimaryProcId, MaybeDataFileName,
                !PredInfo, !Specs, !IO),
            io.close_output(OutputStream, !IO),
            (
                MaybeDataFileName = no
            ;
                MaybeDataFileName = yes(DataFileName),
                append_data_table(MaybeProgressStream,
                    OutputFileName, DataFileName, !Specs, !IO)
            )
        ;
            OpenResult = error(Error),
            add_file_open_error(yes(Context), FactTableFileName, "output",
                Error, !Specs, !IO),
            HeaderCode = "",
            PrimaryProcId = invalid_proc_id
        ),
        io.close_input(FactTableFileStream, !IO)
    ;
        FactTableFileResult = error(Error),
        pred_info_get_markers(!.PredInfo, PredMarkers0),
        add_marker(marker_fact_table_semantic_errors,
            PredMarkers0, PredMarkers),
        pred_info_set_markers(PredMarkers, !PredInfo),
        add_file_open_error(yes(Context), FactTableFileName, "input",
            Error, !Specs, !IO),
        HeaderCode = "",
        PrimaryProcId = invalid_proc_id
    ).

:- pred compile_fact_table_in_file(maybe(io.text_output_stream)::in,
    io.text_input_stream::in, string::in, io.text_output_stream::in,
    int::in, module_info::in, sym_name::in, fact_table_gen_info::in,
    string::out, proc_id::out, maybe(string)::out,
    pred_info::in, pred_info::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

compile_fact_table_in_file(MaybeProgressStream, FileStream, FileName,
        OutputStream, FactTableArraySize, ModuleInfo, PredSymName, GenInfo,
        HeaderCode, PrimaryProcId, MaybeDataFileName,
        !PredInfo, !Specs, !IO) :-
    infer_determinism_pass_1(GenInfo, WriteHashTables, WriteDataTable,
        !PredInfo),
    create_fact_table_header(PredSymName, FactArgInfos,
        HeaderCode0, StructName),

    GenInfo = fact_table_gen_info(FactArgInfos, FactTableProcMap,
        MaybeAllInProcId, InOutProcIds),
    % We need to get the order right for ProcsToCheck because the first
    % procedure in list is used to derive the primary lookup key.
    % XXX Document what "getting the order right" means ...
    %
    % If there is an all_in procedure, it needs to be put on the end of
    % the list so a sort file is created for it. This is required when
    % building the hash table, not for determinism inference.
    (
        MaybeAllInProcId = yes(AllInProcId),
        ProcsToCheck = InOutProcIds ++ [AllInProcId]
    ;
        MaybeAllInProcId = no,
        ProcsToCheck = InOutProcIds
    ),

    io.write_string(OutputStream, fact_table_file_header(FileName), !IO),
    io.write_string(OutputStream, HeaderCode0, !IO),
    open_sort_files(FactTableProcMap, ProcsToCheck, ProcStreams,
        [], OpenSpecs, !IO),

    % GCC doesn't cope very well with huge arrays, so we break the fact data
    % table into a number of smaller arrays, each with a maximum size given
    % by FactTableArraySize, and create an array of pointers to these arrays
    % to access the data. The size should be a power of 2 to make the
    % generated code more efficient.
    %
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
    % well. This requires us to handle the initial prologue and the final
    % epilogue here, outside the loop.
    %
    % XXX The RIGHT way to fix this would be to use a hybrid approach,
    % which would perform these two actions in a loop:
    %
    % - read in FactTableArraySize facts from the input, provided
    %   that there were that many left in the input,
    % - write out a whole array containing those fact, provided that
    %   it contained at least one fact.
    %
    % However, this way is worth implementing only if this code
    % needs to be modified for some other reason.
    (
        WriteDataTable = write_data_table,
        (
            ProcsToCheck = [],
            MaybeOutput = yes(OutputStream - StructName),
            % Outputs opening brace for first fact array.
            write_new_data_array_opening_brace(OutputStream, StructName,
                0, !IO),
            WriteDataAfterSorting = do_not_write_data_table
        ;
            ProcsToCheck = [_ | _],
            MaybeOutput = no,
            WriteDataAfterSorting = write_data_table
        )
    ;
        WriteDataTable = do_not_write_data_table,
        MaybeOutput = no,
        WriteDataAfterSorting = do_not_write_data_table
    ),
    list.length(FactArgInfos, NumFactArgInfos),
    FactNum0 = 0,
    CompileSpecs0 = [],
    read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
        FactTableArraySize, !.PredInfo, NumFactArgInfos, FactArgInfos,
        ProcStreams, MaybeOutput, FactNum0, FactNum,
        CompileSpecs0, CompileSpecs, !IO),
    NumFacts = FactNum,
    (
        MaybeOutput = yes(_),
        % Outputs closing brace for last fact array.
        write_closing_brace(OutputStream, !IO),
        write_fact_table_pointer_array(OutputStream, FactTableArraySize,
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
        infer_determinism_pass_2(MaybeProgressStream, GenInfo, ProcFiles,
            ProcTable0, ProcTable, !Specs, !IO),
        pred_info_set_proc_table(ProcTable, !PredInfo),
        io.file.make_temp_file(DataFileNameResult, !IO),
        (
            DataFileNameResult = ok(DataFileName),
            write_fact_table_arrays(MaybeProgressStream, OutputStream,
                FactTableArraySize, ModuleInfo, ProcFiles, DataFileName,
                FactTableProcMap, StructName, NumFacts, FactArgInfos,
                WriteHashTables, WriteDataAfterSorting,
                HeaderCode1, PrimaryProcId, !Specs, !IO),
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
            PrimaryProcId = invalid_proc_id,
            MaybeDataFileName = no
        )
    ;
        OpenCompileSpecs = [_ | _],
        !:Specs = OpenCompileSpecs ++ !.Specs,
        pred_info_get_markers(!.PredInfo, PredMarkers0),
        add_marker(marker_fact_table_semantic_errors,
            PredMarkers0, PredMarkers),
        pred_info_set_markers(PredMarkers, !PredInfo),
        HeaderCode = HeaderCode0,
        PrimaryProcId = invalid_proc_id,
        MaybeDataFileName = no
    ).

%---------------------------------------------------------------------------%

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
:- pred infer_determinism_pass_1(fact_table_gen_info::in,
    maybe_write_hash_tables::out, maybe_write_data_table::out,
    pred_info::in, pred_info::out) is det.

infer_determinism_pass_1(GenInfo,
        WriteHashTables, WriteDataTable, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    ProcIds = pred_info_all_proc_ids(!.PredInfo),
    infer_procs_determinism_pass_1(GenInfo, ProcIds,
        ProcTable0, ProcTable,
        do_not_write_hash_tables, WriteHashTables,
        do_not_write_data_table, WriteDataTable),
    pred_info_set_proc_table(ProcTable, !PredInfo).

:- pred infer_procs_determinism_pass_1(fact_table_gen_info::in,
    list(proc_id)::in, proc_table::in, proc_table::out,
    maybe_write_hash_tables::in, maybe_write_hash_tables::out,
    maybe_write_data_table::in, maybe_write_data_table::out) is det.

infer_procs_determinism_pass_1(_, [],
        !ProcTable, !WriteHashTables, !WriteDataTable).
infer_procs_determinism_pass_1(GenInfo, [ProcId | ProcIds],
        !ProcTable, !WriteHashTables, !WriteDataTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    GenInfo = fact_table_gen_info(_, FactTableProcMap, _, _),
    map.lookup(FactTableProcMap, ProcId, FactTableProcInfo),
    FactTableProcInfo = fact_table_proc_info(_, ModeClass, _),
    (
        ModeClass = all_in,
        InferredDetism = inferred(detism_semi),
        !:WriteHashTables = write_hash_tables
    ;
        ModeClass = all_out,
        proc_info_get_declared_determinism(ProcInfo0, MaybeDetism),
        ( if
            MaybeDetism = yes(Detism),
            ( Detism = detism_cc_multi
            ; Detism = detism_cc_non
            )
        then
            InferredDetism = inferred(detism_cc_multi)
        else
            InferredDetism = inferred(detism_multi)
        ),
        !:WriteDataTable = write_data_table
    ;
        ModeClass = in_out,
        % Don't have enough info to infer determinism yet.
        % Put it off till the second pass.
        InferredDetism = not_yet,
        % Add to list and check in pass 2.
        !:WriteHashTables = write_hash_tables,
        !:WriteDataTable = write_data_table
    ),
    ( if InferredDetism = inferred(Determinism) then
        proc_info_set_inferred_determinism(Determinism,
            ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    else
        true
    ),
    infer_procs_determinism_pass_1(GenInfo, ProcIds,
        !ProcTable, !WriteHashTables, !WriteDataTable).

%---------------------------------------------------------------------------%

    % Read in facts one by one and check and compile them.
    %
:- pred read_in_and_compile_facts(io.text_input_stream::in, string::in,
    maybe(io.text_output_stream)::in, int::in, pred_info::in,
    int::in, list(fact_arg_info)::in, list(proc_stream)::in,
    maybe(pair(io.text_output_stream, string))::in, int::in, int::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
        FactTableArraySize, PredInfo, NumFactArgInfos, FactArgInfos,
        ProcStreams, MaybeOutput, !FactNum, !Specs, !IO) :-
    mercury_term_parser.read_term(FileStream, Result0, !IO),
    (
        Result0 = eof
    ;
        Result0 = error(Message, LineNum),
        Context = context(FileName, LineNum),
        add_error_context_and_pieces(Context, [words(Message)], !Specs)
    ;
        Result0 = term(VarSet, Term),
        ( if 0 = !.FactNum mod FactTableArraySize then
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
        check_and_compile_fact_term(FileStream, FileName, MaybeProgressStream,
            FactTableArraySize, PredInfo, NumFactArgInfos, FactArgInfos,
            !.FactNum, VarSet, Term, ProcStreams, MaybeOutput,
            CheckSpecs, !IO),
        (
            CheckSpecs = [],
            !:FactNum = !.FactNum + 1
        ;
            CheckSpecs = [_ | _],
            !:Specs = CheckSpecs ++ !.Specs
        ),
        read_in_and_compile_facts(FileStream, FileName, MaybeProgressStream,
            FactTableArraySize, PredInfo, NumFactArgInfos, FactArgInfos,
            ProcStreams, MaybeOutput, !FactNum, !Specs, !IO)
    ).

    % Do syntactic and semantic checks on a fact term, and in the
    % absence of any problems, then compile it.
    %
    % XXX Except, we also compile them in the *presence* of some problems.
    %
:- pred check_and_compile_fact_term(io.text_input_stream::in, string::in,
    maybe(io.text_output_stream)::in, int::in, pred_info::in,
    int::in, list(fact_arg_info)::in, int::in, prog_varset::in, prog_term::in,
    list(proc_stream)::in, maybe(pair(io.text_output_stream, string))::in,
    list(err_spec)::out, io::di, io::uo) is det.

check_and_compile_fact_term(FileStream, FileName, MaybeProgressStream,
        FactTableArraySize, PredInfo, NumFactArgInfos, FactArgInfos, FactNum,
        VarSet, Term, ProcStreams, MaybeOutput, Specs, !IO) :-
    (
        Term = term.variable(_, _),
        io.get_line_number(FileStream, LineNum, !IO),
        Context = context(FileName, LineNum),
        Pieces = [words("Error: this term is")] ++
            color_as_incorrect([words("not a fact.")]) ++
            [nl],
        add_error_context_and_pieces(Context, Pieces, [], Specs)
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
                check_and_compile_fact_term_args(MaybeProgressStream,
                    FactTableArraySize, PredInfo, NumFactArgInfos,
                    FactArgInfos, FactNum, VarSet, ArgTerms, Context,
                    ProcStreams, MaybeOutput, Specs, !IO)
            else
                PredDotPieces = describe_one_pred_info_name(yes(color_subject),
                    should_not_module_qualify, [suffix(".")], PredInfo),
                Pieces = [words("Error: this clause is")] ++
                    color_as_incorrect([words("not")]) ++
                    [words("for")] ++ PredDotPieces ++ [nl],
                add_error_context_and_pieces(Context, Pieces, [], Specs)
            )
        else
            Pieces = [words("Error: this term is")] ++
                color_as_incorrect([words("not a fact.")]) ++
                [nl],
            add_error_context_and_pieces(Context, Pieces, [], Specs)
        )
    ).

:- pred check_and_compile_fact_term_args(maybe(io.text_output_stream)::in,
    int::in, pred_info::in, int::in, list(fact_arg_info)::in,
    int::in, prog_varset::in, list(prog_term)::in, prog_context::in,
    list(proc_stream)::in, maybe(pair(io.text_output_stream, string))::in,
    list(err_spec)::out, io::di, io::uo) is det.

check_and_compile_fact_term_args(MaybeProgressStream, FactTableArraySize,
        PredInfo, NumFactArgInfos, FactArgInfos, FactNum, VarSet, ArgTerms,
        Context, ProcStreams, MaybeOutput, Specs, !IO) :-
    % Check that arity of the fact is correct.
    list.length(ArgTerms, NumArgTerms),
    ( if NumFactArgInfos = NumArgTerms then
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        check_fact_type_and_mode(PredOrFunc, VarSet,
            FactArgInfos, ArgTerms, 1, FactArgs, [], Specs),
        % XXX We should probably avoid executing some of the code below
        % if Specs is not empty.
        string.int_to_string(FactNum, FactNumStr),
        write_sort_file_lines(FactNumStr, FactArgs,
            is_primary_proc(FactArgInfos), ProcStreams, !IO),
        % If there are no in_out modes to the predicate, we need to write out
        % the facts at this point. If there are input modes, the facts are
        % written out later on after being sorted on the first input mode.
        ( if
            MaybeOutput = yes(OutputStream - StructName),
            Specs = []
        then
            write_fact_data(OutputStream, MaybeProgressStream,
                FactTableArraySize, StructName, FactArgs, FactNum, !IO)
        else
            % If list.map above fails, don't do anything here. The error will
            % have already been reported in check_fact_type_and_mode.
            % XXX What list.map?
            true
        )
    else
        Pieces = [words("Error: fact has wrong number of arguments."),
            words("Expected")] ++
            color_as_correct([int_name(NumFactArgInfos)]) ++
            [words("arguments,"), words("got")] ++
            color_as_incorrect([int_name(NumArgTerms), suffix(".")]) ++
            [nl],
        add_error_context_and_pieces(Context, Pieces, [], Specs)
    ).

    % Check that the mode of the fact is correct. All terms must be ground
    % and be a constant of the expected type.
    %
:- pred check_fact_type_and_mode(pred_or_func::in, prog_varset::in,
    list(fact_arg_info)::in, list(prog_term)::in, int::in, list(fact_arg)::out,
    list(err_spec)::in, list(err_spec)::out) is det.

check_fact_type_and_mode(_, _, [], [], _, [], !Specs).
check_fact_type_and_mode(_, _, [_ | _], [], _, _, !Specs) :-
    unexpected($pred, "list length mismatch").
check_fact_type_and_mode(_, _, [], [_ | _], _, _, !Specs) :-
    unexpected($pred, "list length mismatch").
check_fact_type_and_mode(PredOrFunc, VarSet,
        [ArgInfo | ArgInfos], [ArgTerm | ArgTerms], ArgNum,
        [FactArg | FactArgs], !Specs) :-
    (
        ArgTerm = term.variable(_, _),
        report_arg_error(PredOrFunc, VarSet, ArgNum, ArgTerm, ArgTerms,
            "Mode", "a", "ground term", FactArg, !Specs)
    ;
        ArgTerm = term.functor(Functor, _SubTerms, _Context),
        % The term parser should never generate an int, a float or a string
        % with a nonempty list of subterms.
        ArgInfo = fact_arg_info(ArgType, _, _),
        (
            ArgType = fact_arg_type_int,
            ( if Functor = term.integer(Base, Integer, signed, size_word) then
                ( if source_integer_to_int(Base, Integer, Int) then
                    FactArg = fact_arg_int(Int)
                else
                    report_arg_error(PredOrFunc, VarSet, ArgNum,
                        ArgTerm, ArgTerms,
                        "Type", "an", "int that fits in a word",
                        FactArg, !Specs)
                )
            else
                report_arg_error(PredOrFunc, VarSet, ArgNum, ArgTerm, ArgTerms,
                    "Type", "an", "int", FactArg, !Specs)
            )
        ;
            ArgType = fact_arg_type_float,
            ( if Functor = term.float(Float) then
                FactArg = fact_arg_float(Float)
            else
                report_arg_error(PredOrFunc, VarSet, ArgNum, ArgTerm, ArgTerms,
                    "Type", "a", "float", FactArg, !Specs)
            )
        ;
            ArgType = fact_arg_type_string,
            ( if Functor = term.string(Str) then
                FactArg = fact_arg_string(Str)
            else
                report_arg_error(PredOrFunc, VarSet, ArgNum, ArgTerm, ArgTerms,
                    "Type", "a", "string", FactArg, !Specs)
            )
        )
    ),
    check_fact_type_and_mode(PredOrFunc, VarSet,
        ArgInfos, ArgTerms, ArgNum + 1, FactArgs, !Specs).

:- pred report_arg_error(pred_or_func::in, prog_varset::in, int::in,
    prog_term::in, list(prog_term)::in, string::in, string::in, string::in,
    fact_arg::out, list(err_spec)::in, list(err_spec)::out) is det.

report_arg_error(PredOrFunc, VarSet, ArgNum, ArgTerm, RemainingArgTerms,
        TypeOrMode, AAn, Expected, DummyFactArg, !Specs) :-
    ArgStr = describe_error_term(VarSet, ArgTerm),
    ExpectedGotPieces = [words("expected"), words(AAn)] ++
        color_as_correct([words(Expected), suffix(",")]) ++
        [words("got")] ++
        color_as_incorrect([quote(ArgStr), suffix(".")]) ++
        [nl],
    ( if
        PredOrFunc = pf_function,
        RemainingArgTerms = []
    then
        Pieces = [words(TypeOrMode), words("error in"),
            words("return value of function:") | ExpectedGotPieces]
    else
        Pieces = [words(TypeOrMode), words("error in"),
            words("argument"), int_fixed(ArgNum), suffix(":")
            | ExpectedGotPieces]
    ),
    Context = get_term_context(ArgTerm),
    add_error_context_and_pieces(Context, Pieces, !Specs),
    DummyFactArg = fact_arg_string("dummy").

%---------------------------------------------------------------------------%

:- func fact_table_file_header(string) = string.

fact_table_file_header(FileName) = FileHeader :-
    library.version(Version, Fullarch),
    string.append_list(
        ["// Automatically generated from `", FileName, "'\n",
        "// by the Mercury compiler, version ", Version, ",\n",
        "// configured for ", Fullarch, ".\n",
        "// Do not edit.\n",
        "\n",
        "#include ""mercury_imp.h""\n\n"],
        FileHeader).

:- pred create_fact_table_header(sym_name::in, list(fact_arg_info)::in,
    string::out, string::out) is det.

create_fact_table_header(PredSymName, FactArgInfos, HeaderCode, StructName) :-
    % FactTableArraySize is the maximum size of each array in the fact data
    % table.
    PredSymNameStr = sym_name_mangle(PredSymName),
    StructName = "mercury__" ++ PredSymNameStr ++ "_fact_table",

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
    % that are output in some mode.
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
        IsOutput = is_in_or_output_for_some_mode,
        string.format("\t%s V_%d;\n", [s(TypeStr), i(ArgNum)], StructField),
        string.append(StructField, StructContentsTail, StructContents)
    ;
        IsOutput = is_not_in_or_output_for_any_mode,
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

    % open_sort_files(ProcIds, ProcStreams):
    %
    % Open a temporary sort file for each proc_id in ProcIds.
    % Return a list of proc_streams for all the files opened.
    %
:- pred open_sort_files(fact_table_proc_map::in,
    list(proc_id)::in, list(proc_stream)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

open_sort_files(_, [], [], !Specs, !IO).
open_sort_files(ProcMap, [HeadProcId | TailProcIds], ProcStreams,
        !Specs, !IO) :-
    open_temp_output(SortFileNameResult, !IO),
    (
        SortFileNameResult = ok({SortFileName, Stream}),
        map.lookup(ProcMap, HeadProcId, ProcEntry),
        ProcEntry = fact_table_proc_info(FactTableVars, _, _),
        Modes = list.map((func(fact_table_var(_, M, _, _)) = M),
            FactTableVars),
        HeadProcStream = proc_stream(HeadProcId, Modes, SortFileName, Stream),
        open_sort_files(ProcMap, TailProcIds, TailProcStreams, !Specs, !IO),
        ProcStreams = [HeadProcStream | TailProcStreams]
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
close_sort_files([ProcStream | ProcStreams], [ProcId - FileName | ProcFiles],
        !IO) :-
    ProcStream = proc_stream(ProcId, _Modes, FileName, Stream),
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
    % read_sort_file_line, so if any changes are made here, corresponding
    % changes should be made there too.
    %
:- pred write_sort_file_lines(string::in, list(fact_arg)::in,
    maybe_primary_proc::in, list(proc_stream)::in, io::di, io::uo) is det.

write_sort_file_lines(_, _, _, [], !IO).
write_sort_file_lines(FactNumStr, FactArgs, IsPrimary,
        [ProcStream | ProcStreams], !IO) :-
    ProcStream = proc_stream(_ProcId, Modes, _SortFileName, Stream),
    make_sort_file_key(Modes, FactArgs, Key),
    (
        IsPrimary = is_primary_proc(FactArgInfos),
        make_fact_data_string(FactArgInfos, FactArgs, DataString)
    ;
        IsPrimary = is_not_primary_proc,
        DataString = ""
    ),
    io.format(Stream, "%s~%s~%s\n",
        [s(Key), s(FactNumStr), s(DataString)], !IO),
    write_sort_file_lines(FactNumStr, FactArgs, is_not_primary_proc,
        ProcStreams, !IO).

    % Create a key for the fact table entry.
    % Arguments are separated by ":".
    % Colons in string literals are replaced by "\c", tildes are replaced
    % by "\t", newlines are replaced by "\n" and backslashes by "\\".
    % This ensures that each possible set of arguments maps to a unique key
    % and guarantees that duplicate keys will be adjacent after sorting
    % with the sort program. The tilde ('~') character is used in the
    % sort file to separate the sort key from the data.
    %
:- pred make_sort_file_key(list(fact_table_mode)::in, list(fact_arg)::in,
    string::out) is det.

make_sort_file_key([], [], "").
make_sort_file_key([], [_ | _], _) :-
    unexpected($pred, "list length mismatch").
make_sort_file_key([_ | _], [], _) :-
    unexpected($pred, "list length mismatch").
make_sort_file_key([HeadMode | TailModes], [HeadArg | TailArgs], Key) :-
    make_sort_file_key(TailModes, TailArgs, TailKey),
    (
        HeadMode = fully_in,
        HeadKey = make_key_part(HeadArg),
        Key = HeadKey ++ ":" ++ TailKey
    ;
        HeadMode = fully_out,
        Key = TailKey
    ).

    % Like make_sort_file_key but for the output arguments of the fact.
    %
:- pred make_fact_data_string(list(fact_arg_info)::in, list(fact_arg)::in,
    string::out) is det.

make_fact_data_string([], [], "").
make_fact_data_string([], [_ | _], _) :-
    unexpected($pred, "list length mismatch").
make_fact_data_string([_ | _], [], _) :-
    unexpected($pred, "list length mismatch").
make_fact_data_string([HeadInfo | TailInfos], [HeadArg | TailArgs], String) :-
    make_fact_data_string(TailInfos, TailArgs, TailString),
    HeadInfo = fact_arg_info(_, _, HeadIsOutput),
    (
        HeadIsOutput = is_in_or_output_for_some_mode,
        HeadString = make_key_part(HeadArg),
        String = HeadString ++ ":" ++ TailString
    ;
        HeadIsOutput = is_not_in_or_output_for_any_mode,
        String = TailString
    ).

:- func make_key_part(fact_arg) = string.

make_key_part(Arg) = Key :-
    (
        Arg = fact_arg_int(Int),
        % Print the integer in base 36 to reduce the amount of I/O that we do.
        Key = string.int_to_base_string(Int, 36)
    ;
        Arg = fact_arg_float(F),
        Key = string.float_to_string(F)
    ;
        Arg = fact_arg_string(Str),
        string.to_char_list(Str, Chars),
        key_from_chars(Chars, EscapedChars),
        string.from_char_list(EscapedChars, Key)
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
        cord.snoc_list(['\\', '\\'], !EscapedCharsCord)
    else if Char = ('\n') then
        cord.snoc_list(['\\', 'n'], !EscapedCharsCord)
    else if Char = (':') then
        cord.snoc_list(['\\', 'c'], !EscapedCharsCord)
    else if Char = ('~') then
        cord.snoc_list(['\\', 't'], !EscapedCharsCord)
    else
        cord.snoc(Char, !EscapedCharsCord)
    ),
    key_from_chars_loop(Chars, !EscapedCharsCord).

%---------------------------------------------------------------------------%

    % infer_determinism_pass_2(ModuleInfo, GenInfo, ProcFiles,
    %   !ProcTable, !Specs, !IO):
    %
    % Run `sort' on each sort file to see if the keys are unique.
    % If they are, the procedure is semidet, otherwise it is nondet.
    % Return the updated proc_table.
    %
:- pred infer_determinism_pass_2(maybe(io.text_output_stream)::in,
    fact_table_gen_info::in, assoc_list(proc_id, string)::in,
    proc_table::in, proc_table::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

infer_determinism_pass_2(_, _, [], !ProcTable, !Specs, !IO).
infer_determinism_pass_2(MaybeProgressStream, GenInfo,
        [ProcId - FileName | ProcFiles], !ProcTable, !Specs, !IO) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
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
    (
        MaybeProgressStream = no,
        io.call_system.call_system(Command, Result, !IO)
    ;
        MaybeProgressStream = yes(ProgressStream),
        io.format(ProgressStream, "%% Invoking system command `%s' ...",
            [s(Command)], !IO),
        io.call_system.call_system(Command, Result, !IO),
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
                GenInfo = fact_table_gen_info(_, _, MaybeAllInProcId, _),
                % This is an all_in mode so it is semidet.
                % XXX This means that *some* procedure is all_in,
                % but what allows that comment to say that *this* procedure
                % is all_in?
                MaybeAllInProcId = yes(_),
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
            Spec = no_ctxt_spec($pred, severity_error, phase_fact_table_check,
                Pieces),
            !:Specs = [Spec | !.Specs],
            Determinism = detism_erroneous
        )
    ;
        Result = error(ErrorCode),
        add_call_system_error("sort", ErrorCode, !Specs, !IO),
        Determinism = detism_erroneous
    ),
    proc_info_set_inferred_determinism(Determinism, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    infer_determinism_pass_2(MaybeProgressStream, GenInfo, ProcFiles,
        !ProcTable, !Specs, !IO).

%---------------------------------------------------------------------------%

    % Write out the fact table data arrays and hash tables.
    %
:- pred write_fact_table_arrays(maybe(io.text_output_stream)::in,
    io.text_output_stream::in, int::in,
    module_info::in, assoc_list(proc_id, string)::in, string::in,
    fact_table_proc_map::in, string::in, int::in, list(fact_arg_info)::in,
    maybe_write_hash_tables::in, maybe_write_data_table::in,
    string::out, proc_id::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

write_fact_table_arrays(MaybeProgressStream, OutputStream, FactTableArraySize,
        ModuleInfo, ProcFiles, DataFileName, FactTableProcMap,
        StructName, NumFacts, FactArgInfos, WriteHashTables, WriteDataTable,
        HeaderCode, PrimaryProcId, !Specs, !IO) :-
    (
        % No sort files => there was only and all_out mode
        %   => nothing left to be done here.
        ProcFiles = [],
        HeaderCode = "",
        % This won't get used anyway.
        PrimaryProcId = initial_proc_id
    ;
        ProcFiles = [PrimaryProcId - FileName | TailProcFiles],
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
            write_primary_hash_table(MaybeProgressStream, OutputStream,
                FactTableArraySize, ModuleInfo, FactTableProcMap,
                PrimaryProcId, FileName, DataFileName, StructName,
                FactArgInfos, WriteDataTable, NumFacts, CreateFactMap,
                PrimaryResult, !Specs, !IO),
            (
                PrimaryResult = ok(FactMap, PrimaryHeaderCode),
                write_secondary_hash_tables(MaybeProgressStream, OutputStream,
                    FactTableArraySize, ModuleInfo, FactTableProcMap,
                    StructName, FactArgInfos, FactMap, TailProcFiles,
                    "", SecondaryHeadCode, !Specs, !IO),
                HeaderCode = PrimaryHeaderCode ++ SecondaryHeadCode
            ;
                PrimaryResult = error,
                HeaderCode = ""
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
write_fact_table_data(OutputStream, MaybeProgressStream,
        FactTableArraySize, StructName, [Fact | Facts], FactNum, !IO) :-
    write_fact_data(OutputStream, MaybeProgressStream,
        FactTableArraySize, StructName, Fact, FactNum, !IO),
    write_fact_table_data(OutputStream, MaybeProgressStream,
        FactTableArraySize, StructName, Facts, FactNum + 1, !IO).

    % Write out the data for a single fact, starting a new array if necessary.
    % Note: this predicate will not write the declaration or opening brace
    % for the first array or the closing brace of the last array.
    %
:- pred write_fact_data(io.text_output_stream::in,
    maybe(io.text_output_stream)::in, int::in, string::in,
    list(fact_arg)::in, int::in, io::di, io::uo) is det.

write_fact_data(OutputStream, MaybeProgressStream, FactTableArraySize,
        StructName, Args, FactNum, !IO) :-
    ( if 0 = FactNum mod FactTableArraySize then
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
write_fact_args(OutputStream, [FactArg | FactArgs], !IO) :-
    (
        FactArg = fact_arg_string(String),
        output_quoted_string_c(OutputStream, String, !IO),
        io.write_string(OutputStream, ", ", !IO)
    ;
        FactArg = fact_arg_int(Int),
        io.write_int(OutputStream, Int, !IO),
        io.write_string(OutputStream, ", ", !IO)
    ;
        FactArg = fact_arg_float(Float),
        io.write_float(OutputStream, Float, !IO),
        io.write_string(OutputStream, ", ", !IO)
    ),
    write_fact_args(OutputStream, FactArgs, !IO).

    % If a data table has been created in a separate file, append it to the
    % end of the main output file and then delete it.
    %
:- pred append_data_table(maybe(io.text_output_stream)::in,
    string::in, string::in, list(err_spec)::in, list(err_spec)::out,
    io::di, io::uo) is det.

append_data_table(MaybeProgressStream, OutputFileName, DataFileName,
        !Specs, !IO) :-
    make_command_string(string.format("cat %s >>%s",
        [s(DataFileName), s(OutputFileName)]), forward, Command),
    (
        MaybeProgressStream = no,
        io.call_system.call_system(Command, Result, !IO)
    ;
        MaybeProgressStream = yes(ProgressStream),
        io.format(ProgressStream, "%% Invoking system command `%s' ...",
            [s(Command)], !IO),
        io.call_system.call_system(Command, Result, !IO),
        io.write_string(ProgressStream, "done.\n", !IO)
    ),
    (
        Result = ok(ExitStatus),
        ( if ExitStatus = 0 then
            true
        else
            Pieces = [words("An error occurred while concatenating"),
                words("fact table output files."), nl],
            Spec = no_ctxt_spec($pred, severity_error, phase_fact_table_check,
                Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Result = error(ErrorCode),
        add_call_system_error("cat", ErrorCode, !Specs, !IO)
    ),
    delete_temporary_file(DataFileName, !Specs, !IO).

:- type fact_result
    --->    ok(map(int, int), string)
    ;       error.

    % Write hash tables for the primary key. Create a map from indices in the
    % original input table to the table sorted on the primary key.
    % Write out the data table if required.
    %
:- pred write_primary_hash_table(maybe(io.text_output_stream)::in,
    io.text_output_stream::in, int::in,
    module_info::in, fact_table_proc_map::in, proc_id::in,
    string::in, string::in, string::in, list(fact_arg_info)::in,
    maybe_write_data_table::in, int::in, maybe_create_fact_map::in,
    fact_result::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

write_primary_hash_table(MaybeProgressStream, OutputStream, FactTableArraySize,
        ModuleInfo, FactTableProcMap, ProcId, FileName, DataFileName,
        StructName, FactArgInfos, WriteDataTable, NumFacts,
        CreateFactMap, Result, !Specs, !IO) :-
    io.open_input(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        (
            WriteDataTable = write_data_table,
            io.open_output(DataFileName, OpenResult, !IO),
            (
                OpenResult = ok(DataStream),
                proc_id_to_int(ProcId, ProcIdInt),
                string.format("%s_hash_table_%d_",
                    [s(StructName), i(ProcIdInt)], HashTableName),
                % Note: the type declared here is not necessarily correct.
                % We declare it just to stop the C compiler emitting warnings.
                string.format(
                    "extern struct MR_fact_table_hash_table_i %s0;\n",
                    [s(HashTableName)], HeaderCode0),
                map.lookup(FactTableProcMap, ProcId, FactTableProcInfo),
                FactTableProcInfo = fact_table_proc_info(FactTableVars, _, _),
                FactTableModes =
                    list.map((func(fact_table_var(_, M, _, _)) = M),
                        FactTableVars),
                read_sort_file_line(FileStream, FileName,
                    FactArgInfos, FactTableModes, MaybeFirstFact, !Specs, !IO),
                % Output opening brace for first fact array,
                % XXX This design is inelegant.
                write_new_data_array_opening_brace(DataStream, StructName,
                    0, !IO),
                (
                    MaybeFirstFact = yes(FirstFact),
                    build_hash_table(MaybeProgressStream, FileStream, FileName,
                        OutputStream, yes(DataStream), FactTableArraySize,
                        ModuleInfo, primary_table, StructName, FactArgInfos,
                        FactTableModes, 0, HashTableName, 0, FirstFact, 0,
                        CreateFactMap, map.init, FactMap1, !Specs, !IO),
                    MaybeFactMap = yes(FactMap1)
                ;
                    MaybeFirstFact = no,
                    MaybeFactMap = no
                ),
                % Closing brace for last fact data array.
                write_closing_brace(DataStream, !IO),
                write_fact_table_pointer_array(DataStream, FactTableArraySize,
                    StructName, NumFacts, HeaderCode1, !IO),
                io.close_output(DataStream, !IO),
                (
                    MaybeFactMap = no,
                    Result = error
                ;
                    MaybeFactMap = yes(FactMap),
                    HeaderCode = HeaderCode0 ++ HeaderCode1,
                    Result = ok(FactMap, HeaderCode)
                )
            ;
                OpenResult = error(Error),
                add_file_open_error(no, DataFileName, "output", Error,
                    !Specs, !IO),
                Result = error
            )
        ;
            WriteDataTable = do_not_write_data_table,
            Result = error
        ),
        io.close_input(FileStream, !IO),
        delete_temporary_file(FileName, !Specs, !IO)
    ;
        FileResult = error(Error),
        add_file_open_error(no, FileName, "input", Error, !Specs, !IO),
        Result = error
    ).

    % Build hash tables for non-primary input procs.
    %
:- pred write_secondary_hash_tables(maybe(io.text_output_stream)::in,
    io.text_output_stream::in, int::in,
    module_info::in, fact_table_proc_map::in, string::in,
    list(fact_arg_info)::in, map(int, int)::in,
    assoc_list(proc_id, string)::in,
    string::in, string::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

write_secondary_hash_tables(_, _, _, _, _, _, _, _, [],
        !HeaderCode, !Specs, !IO).
write_secondary_hash_tables(MaybeProgressStream, OutputStream,
        FactTableArraySize, ModuleInfo, FactTableProcMap, StructName,
        FactArgInfos, FactMap, [ProcId - FileName | ProcFiles],
        !HeaderCode, !Specs, !IO) :-
    io.open_input(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        proc_id_to_int(ProcId, ProcIdInt),
        string.format("%s_hash_table_%d_",
            [s(StructName), i(ProcIdInt)], HashTableName),
        % Note: the type declared here is not necessarily correct.
        % The type is declared just to stop the C compiler emitting warnings.
        string.format(
            "extern struct MR_fact_table_hash_table_i %s0;\n",
            [s(HashTableName)], StructDeclCode),
        !:HeaderCode = !.HeaderCode ++ StructDeclCode,
        map.lookup(FactTableProcMap, ProcId, FactTableProcInfo),
        FactTableProcInfo = fact_table_proc_info(FactTableVars, _, _),
        FactTableModes = list.map((func(fact_table_var(_, M, _, _)) = M),
            FactTableVars),
        read_sort_file_line(FileStream, FileName,
            FactArgInfos, FactTableModes, MaybeFirstFact, !Specs, !IO),
        (
            MaybeFirstFact = yes(FirstFact),
            build_hash_table(MaybeProgressStream, FileStream, FileName,
                OutputStream, no, FactTableArraySize, ModuleInfo,
                not_primary_table, StructName, FactArgInfos, FactTableModes,
                0, HashTableName, 0, FirstFact, 0,
                do_not_create_fact_map, FactMap, _, !Specs, !IO),
            io.close_input(FileStream, !IO),
            delete_temporary_file(FileName, !Specs, !IO),
            write_secondary_hash_tables(MaybeProgressStream, OutputStream,
                FactTableArraySize, ModuleInfo, FactTableProcMap, StructName,
                FactArgInfos, FactMap, ProcFiles, !HeaderCode, !Specs, !IO)
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
    list(fact_arg_info)::in, list(fact_table_mode)::in,
    maybe(sort_file_line)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

read_sort_file_line(InputStream, InputFileName,
        FactArgInfos, Modes, MaybeSortFileLine, !Specs, !IO) :-
    io.read_line(InputStream, Result, !IO),
    (
        Result = ok(LineChars),
        string.from_char_list(LineChars, LineString),
        split_sort_file_line(FactArgInfos, Modes, LineString, SortFileLine),
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
        Spec = no_ctxt_spec($pred, severity_error, phase_fact_table_check,
            Pieces),
        !:Specs = [Spec | !.Specs],
        MaybeSortFileLine = no
    ).

    % Break up a string into the components of a sort file line.
    %
:- pred split_sort_file_line(list(fact_arg_info)::in,
    list(fact_table_mode)::in, string::in, sort_file_line::out) is det.

split_sort_file_line(FactArgInfos, Modes, Line0, SortFileLine) :-
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
        get_input_args_list(FactArgInfos, Modes,
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

:- pred get_input_args_list(list(fact_arg_info)::in, list(fact_table_mode)::in,
    list(string)::in, list(fact_arg)::out) is det.

get_input_args_list([], [], _, []).
get_input_args_list([_ | _], [], _, _) :-
    unexpected($pred, "too many fact_arg_infos").
get_input_args_list([], [_ | _], _, _) :-
    unexpected($pred, "too many argmodes").
get_input_args_list([Info | Infos], [Mode | Modes], ArgStrings0, Args) :-
    (
        Mode = fully_in,
        (
            ArgStrings0 = [ArgString | ArgStrings],
            Info = fact_arg_info(Type, _, _),
            convert_key_string_to_arg(ArgString, Type, Arg),
            get_input_args_list(Infos, Modes, ArgStrings, ArgsTail),
            Args = [Arg | ArgsTail]
        ;
            ArgStrings0 = [],
            unexpected($pred, "not enough ArgStrings")
        )
    ;
        Mode = fully_out,
        % This argument is not input, so skip it and try the next one.
        get_input_args_list(Infos, Modes, ArgStrings0, Args)
    ).

:- pred get_output_args_list(list(fact_arg_info)::in, list(string)::in,
    list(fact_arg)::out) is det.

get_output_args_list([], _, []).
get_output_args_list([Info | Infos], ArgStrings0, Args) :-
    Info = fact_arg_info(Type, _, IsOutput),
    (
        IsOutput = is_in_or_output_for_some_mode,
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
        IsOutput = is_not_in_or_output_for_any_mode,
        get_output_args_list(Infos, ArgStrings0, Args)
    ).

:- pred convert_key_string_to_arg(string::in, fact_arg_type::in, fact_arg::out)
    is det.

convert_key_string_to_arg(ArgString, Type, Arg) :-
    (
        Type = fact_arg_type_int,
        ( if string.base_string_to_int(36, ArgString, Int) then
            Arg = fact_arg_int(Int)
        else
            unexpected($pred, "could not convert string to int")
        )
    ;
        Type = fact_arg_type_float,
        ( if string.to_float(ArgString, F) then
            Arg = fact_arg_float(F)
        else
            unexpected($pred, "could not convert string to float")
        )
    ;
        Type = fact_arg_type_string,
        string.to_char_list(ArgString, Chars0),
        remove_sort_file_escapes(Chars0, [], RevChars),
        list.reverse(RevChars, Chars),
        string.from_char_list(Chars, S),
        Arg = fact_arg_string(S)
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
:- pred build_hash_table(maybe(io.text_output_stream)::in,
    io.text_input_stream::in, string::in,
    io.text_output_stream::in, maybe(io.text_output_stream)::in, int::in,
    module_info::in, maybe_primary_table::in, string::in,
    list(fact_arg_info)::in, list(fact_table_mode)::in,
    int::in, string::in, int::in, sort_file_line::in, int::in,
    maybe_create_fact_map::in, map(int, int)::in, map(int, int)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

build_hash_table(MaybeProgressStream, InputStream, InputFileName, OutputStream,
        MaybeDataStream, FactTableArraySize, ModuleInfo, IsPrimaryTable,
        StructName, Infos, Modes, InputArgNum, HashTableName, TableNum,
        FirstFact, FactNum, CreateFactMap, !FactMap, !Specs, !IO) :-
    build_hash_table_loop(MaybeProgressStream, InputStream, InputFileName,
        OutputStream, MaybeDataStream, FactTableArraySize, ModuleInfo,
        IsPrimaryTable, StructName, Infos, Modes, InputArgNum,
        HashTableName, TableNum, FirstFact, FactNum, CreateFactMap,
        !FactMap, [], HashList, !Specs, !IO),
    list.length(HashList, Len),
    module_info_get_globals(ModuleInfo, Globals),
    calculate_hash_table_size(Globals, Len, HashSize),
    hash_table_init(HashSize, HashTable0),
    hash_table_from_list(HashList, HashSize, HashTable0, HashTable),
    write_hash_table(OutputStream, HashTableName, TableNum, HashTable, !IO).

:- pred build_hash_table_loop(maybe(io.text_output_stream)::in,
    io.text_input_stream::in, string::in,
    io.text_output_stream::in, maybe(io.text_output_stream)::in, int::in,
    module_info::in, maybe_primary_table::in, string::in,
    list(fact_arg_info)::in, list(fact_table_mode)::in,
    int::in, string::in, int::in, sort_file_line::in, int::in,
    maybe_create_fact_map::in, map(int, int)::in, map(int, int)::out,
    list(hash_entry)::in, list(hash_entry)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

build_hash_table_loop(MaybeProgressStream, InputStream, InputFileName,
        OutputStream, MaybeDataStream, FactTableArraySize, ModuleInfo,
        IsPrimaryTable, StructName, Infos, Modes, InputArgNum,
        HashTableName, !.TableNum, FirstFact, FactNum, CreateFactMap,
        !FactMap, !HashList, !Specs, !IO) :-
    top_level_collect_matching_facts(InputStream, InputFileName,
        Infos, Modes, FirstFact, MatchingFacts, MaybeNextFact, !Specs, !IO),
    (
        CreateFactMap = create_fact_map,
        update_fact_map(FactNum, MatchingFacts, !FactMap)
    ;
        CreateFactMap = do_not_create_fact_map
    ),
    (
        MaybeDataStream = yes(DataStream),
        OutputData = list.map(
            (func(sort_file_line(_, _, OutArgs)) = OutArgs),
            MatchingFacts),
        write_fact_table_data(DataStream, MaybeProgressStream,
            FactTableArraySize, StructName, OutputData, FactNum, !IO)
    ;
        MaybeDataStream = no
    ),
    module_info_get_globals(ModuleInfo, Globals),
    do_build_hash_table(OutputStream, Globals, IsPrimaryTable,
        !.FactMap, FactNum, InputArgNum, HashTableName,
        MatchingFacts, !TableNum, !HashList, !IO),
    (
        MaybeNextFact = no
    ;
        MaybeNextFact = yes(NextFact),
        list.length(MatchingFacts, Len),
        NextFactNum = FactNum + Len,
        build_hash_table_loop(MaybeProgressStream, InputStream, InputFileName,
            OutputStream, MaybeDataStream, FactTableArraySize, ModuleInfo,
            IsPrimaryTable, StructName, Infos, Modes, InputArgNum,
            HashTableName, !.TableNum, NextFact, NextFactNum, CreateFactMap,
            !FactMap, !HashList, !Specs, !IO)
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

    % Read lines from the sort file that have the same first input argument
    % as Fact. Places these lines into MatchingFacts. The first fact
    % in MatchingFacts is always Fact. If an extra fact is read in following
    % the matching facts, it is placed in MaybeNextFact.
    %
:- pred top_level_collect_matching_facts(io.text_input_stream::in, string::in,
    list(fact_arg_info)::in, list(fact_table_mode)::in,
    sort_file_line::in, list(sort_file_line)::out, maybe(sort_file_line)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

top_level_collect_matching_facts(InputStream, InputFileName,
        Infos, Modes, Fact, MatchingFacts, MaybeNextFact, !Specs, !IO) :-
    top_level_collect_matching_facts_loop(InputStream, InputFileName,
        Infos, Modes, Fact, [], RevMatchingFacts, MaybeNextFact, !Specs, !IO),
    list.reverse(RevMatchingFacts, MatchingFactsTail),
    MatchingFacts = [Fact | MatchingFactsTail].

:- pred top_level_collect_matching_facts_loop(io.text_input_stream::in,
    string::in, list(fact_arg_info)::in, list(fact_table_mode)::in,
    sort_file_line::in, list(sort_file_line)::in, list(sort_file_line)::out,
    maybe(sort_file_line)::out,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

top_level_collect_matching_facts_loop(InputStream, InputFileName,
        Infos, Modes, Fact, !RevMatchingFacts, MaybeNextFact, !Specs, !IO) :-
    read_sort_file_line(InputStream, InputFileName,
        Infos, Modes, MaybeSortFileLine, !Specs, !IO),
    (
        MaybeSortFileLine = yes(Fact1),
        ( if
            Fact1 = sort_file_line([Arg1 | _], _, _),
            Fact  = sort_file_line([Arg  | _], _, _)
        then
            ( if Arg = Arg1 then
                !:RevMatchingFacts = [Fact1 | !.RevMatchingFacts],
                top_level_collect_matching_facts_loop(InputStream,
                    InputFileName, Infos, Modes, Fact,
                    !RevMatchingFacts, MaybeNextFact, !Specs, !IO)
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
    % The prime number is selected from a list of primes where
    % each prime is close to a power of 2 between 2^1 and 2^31.
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
    (
        Key = fact_arg_string(String),
        % XXX This method of hashing strings may not work if cross-compiling
        % between systems that have different character representations.
        string.to_char_list(String, Cs),
        list.map((pred(C::in, I::out) is det :- char.to_int(C, I)), Cs, Ns)
    ;
        Key = fact_arg_int(Int),
        int.abs(Int, N),
        Ns = [N]
    ;
        Key = fact_arg_float(Float),
        % XXX This method of hashing floats may not work cross-compiling
        % between architectures that have different floating-point
        % representations.
        int.abs(float.hash(Float), N),
        Ns = [N]
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
                Key = fact_arg_string(String),
                output_quoted_string_c(Stream, String, !IO)
            ;
                Key = fact_arg_int(Int),
                io.write_int(Stream, Int, !IO)
            ;
                Key = fact_arg_float(Float),
                io.write_float(Stream, Float, !IO)
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

:- inst key_char for char/0
    --->    ('s')
    ;       ('i')
    ;       ('f').

    % Return 's' for string, 'i' for int, 'f' for float.
    % Don't call this with an empty hash table.
    %
:- pred get_hash_table_type(hash_table::in, char::out(key_char)) is det.

get_hash_table_type(HashTable, TableType) :-
    HashTable = hash_table(_Size, Map),
    ( if map.is_empty(Map) then
        unexpected($pred, "empty hash table")
    else
        get_hash_table_type_loop(Map, 0, TableType)
    ).

:- pred get_hash_table_type_loop(map(int, hash_entry)::in, int::in,
    char::out(key_char)) is det.

get_hash_table_type_loop(Map, Index, TableType) :-
    ( if map.search(Map, Index, Entry) then
        Entry = hash_entry(Key, _, _),
        (
            Key = fact_arg_string(_),
            TableType = 's'
        ;
            Key = fact_arg_int(_),
            TableType = 'i'
        ;
            Key = fact_arg_float(_),
            TableType = 'f'
        )
    else
        get_hash_table_type_loop(Map, Index + 1, TableType)
    ).

%---------------------%

    % Write out the array of pointers to the fact table arrays.
    %
:- pred write_fact_table_pointer_array(io.text_output_stream::in, int::in,
    string::in, int::in, string::out, io::di, io::uo) is det.

write_fact_table_pointer_array(OutputStream, FactTableArraySize,
        StructName, NumFacts, HeaderCode, !IO) :-
    string.format("const struct %s_struct *%s[]",
        [s(StructName), s(StructName)], PointerArrayName),
    HeaderCode = "extern " ++ PointerArrayName ++ ";\n",
    io.format(OutputStream, "%s = {\n", [s(PointerArrayName)], !IO),
    write_fact_table_pointer_array_loop(OutputStream, FactTableArraySize,
        StructName, 0, NumFacts, !IO),
    io.write_string(OutputStream, "};\n", !IO).

:- pred write_fact_table_pointer_array_loop(io.text_output_stream::in, int::in,
    string::in, int::in, int::in, io::di, io::uo) is det.

write_fact_table_pointer_array_loop(OutputStream, FactTableArraySize,
        StructName, CurFact, NumFacts, !IO) :-
    ( if CurFact >= NumFacts then
        true
    else
        io.format(OutputStream, "\t%s%d,\n", [s(StructName), i(CurFact)], !IO),
        NextFact = CurFact + FactTableArraySize,
        write_fact_table_pointer_array_loop(OutputStream, FactTableArraySize,
            StructName, NextFact, NumFacts, !IO)
    ).

%---------------------%

:- pred write_fact_table_numfacts(io.text_output_stream::in,
    sym_name::in, int::in, string::out, io::di, io::uo) is det.

write_fact_table_numfacts(OutputStream, PredSymName, NumFacts, HeaderCode,
        !IO) :-
    % Write out the size of the fact table.
    PredSymNameStr = sym_name_mangle(PredSymName),
    io.format(OutputStream,
        "const MR_Integer mercury__%s_fact_table_num_facts = %d;\n\n",
        [s(PredSymNameStr), i(NumFacts)], !IO),
    string.format(
        "extern const MR_Integer mercury__%s_fact_table_num_facts;\n",
        [s(PredSymNameStr)], HeaderCode).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Delete a file. Report an error message if something goes wrong.
    %
:- pred delete_temporary_file(string::in,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

delete_temporary_file(FileName, !Specs, !IO) :-
    io.file.remove_file(FileName, Result, !IO),
    (
        Result = ok
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMsg),
        io.progname_base("mercury_compile", ProgName, !IO),
        Pieces = [fixed(ProgName), suffix(":"), words("error deleting file"),
            quote(FileName), suffix(":"), nl,
            words(ErrorMsg), suffix("."), nl],
        Spec = gen_spec($pred, severity_error, phase_fact_table_check,
            [gen_msg(no, treat_based_on_posn, 0u, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%

:- pred add_call_system_error(string::in, io.error::in,
    list(err_spec)::in, list(err_spec)::out, io::di, io::uo) is det.

add_call_system_error(Cmd, ErrorCode, !Specs, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    io.error_message(ErrorCode, ErrorMsg),
    Pieces = [fixed(ProgName), suffix(":"),
        words("error executing system command"), quote(Cmd), suffix(":"), nl,
        words(ErrorMsg), suffix("."), nl],
    Spec = gen_spec($pred, severity_error, phase_fact_table_check,
        [gen_msg(no, treat_based_on_posn, 0u, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_file_open_error(maybe(prog_context)::in, string::in, string::in,
    io.error::in, list(err_spec)::in, list(err_spec)::out,
    io::di, io::uo) is det.

add_file_open_error(MaybeContext, FileName, InOrOut, Error, !Specs, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    io.error_message(Error, ErrorMsg),
    Pieces = [fixed(ProgName), suffix(":"),
        words("error opening file"), quote(FileName),
        words("for"), words(InOrOut), suffix(":"), nl,
        words(ErrorMsg), nl],
    Spec = gen_spec($pred, severity_error, phase_fact_table_check,
        [gen_msg(MaybeContext, treat_based_on_posn, 0u, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_error_context_and_pieces(prog_context::in,
    list(format_piece)::in,
    list(err_spec)::in, list(err_spec)::out) is det.

add_error_context_and_pieces(Context, Pieces, !Specs) :-
    Spec = spec($pred, severity_error, phase_fact_table_check,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred add_error_pieces(list(format_piece)::in,
    list(err_spec)::in, list(err_spec)::out) is det.

add_error_pieces(Pieces, !Specs) :-
    Spec = no_ctxt_spec($pred, severity_error, phase_fact_table_check, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred get_maybe_progress_output_stream(module_info::in,
    io.text_output_stream::in, maybe(io.text_output_stream)::out) is det.

get_maybe_progress_output_stream(ModuleInfo, ProgressStream,
        MaybeProgressStream) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no,
        MaybeProgressStream = no
    ;
        VeryVerbose = yes,
        MaybeProgressStream = yes(ProgressStream)
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.fact_table_compile.
%---------------------------------------------------------------------------%
