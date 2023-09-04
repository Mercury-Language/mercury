%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2020-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Mercury coverage test tool.
%
% Author: Zoltan Somogyi.
%
%---------------------------------------------------------------------------%

:- module mcov.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.sym_name.
:- import_module mdbcomp.trace_counts.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

main(!IO) :-
    unlimit_stack(!IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options_io(OptionOps, Args0, Args, GetoptResult, !IO),
    (
        GetoptResult = ok(OptionTable),
        ( if lookup_bool_option(OptionTable, help, yes) then
            io.stdout_stream(StdOutStream, !IO),
            long_usage(StdOutStream, !IO)
        else
            do_coverage_testing(OptionTable, Args, !IO)
        )
    ;
        GetoptResult = error(GetoptError),
        GetoptErrorMsg = option_error_to_string(GetoptError),
        write_error_message(GetoptErrorMsg, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_coverage_testing(option_table(option)::in, list(string)::in,
    io::di, io::uo) is det.

do_coverage_testing(OptionTable, Args, !IO) :-
    io.stdout_stream(StdOutStream, !IO),
    io.stderr_stream(StdErrStream, !IO),
    (
        Args = [_ | _],
        lookup_bool_option(OptionTable, verbose, Verbose),
        (
            Verbose = yes,
            ShowProgress = yes(StdOutStream)
        ;
            Verbose = no,
            ShowProgress = no
        ),
        read_and_union_trace_counts(ShowProgress, Args, _NumTests,
            FileTypes, TraceCounts, MaybeReadError, !IO),
        (
            MaybeReadError = yes(ReadErrorMsg),
            write_error_message(ReadErrorMsg, !IO)
        ;
            MaybeReadError = no,
            set.to_sorted_list(FileTypes, FileTypeList),
            ( if FileTypeList = [single_file(BaseType)] then
                BaseType = base_count_file_type(Kind, _Program),
                (
                    Kind = user_nonzero,
                    io.write_string(StdErrStream, kind_warning, !IO)
                ;
                    Kind = user_all
                )
            else
                io.write_string(StdErrStream, consistency_warning, !IO)
            ),
            lookup_accumulating_option(OptionTable, modules, Modules),
            (
                Modules = [],
                RestrictToModules = module_restriction_default
            ;
                Modules = [_ | _],
                ModuleSyms = list.map(string_to_sym_name, Modules),
                RestrictToModules =
                    module_restriction_user(set.list_to_set(ModuleSyms))
            ),
            lookup_string_option(OptionTable, output_filename, OutputFile),
            ( if OutputFile = "" then
                write_coverage_test(StdOutStream, OptionTable,
                    RestrictToModules, TraceCounts, !IO)
            else
                io.open_output(OutputFile, OpenRes, !IO),
                (
                    OpenRes = ok(FileStream),
                    write_coverage_test(FileStream, OptionTable,
                        RestrictToModules, TraceCounts, !IO),
                    io.close_output(FileStream, !IO)
                ;
                    OpenRes = error(OpenError),
                    io.error_message(OpenError, OpenErrorMsg),
                    io.format(StdErrStream, "Error opening file `%s': %s\n",
                        [s(OutputFile), s(OpenErrorMsg)], !IO),
                    io.set_exit_status(1, !IO)
                )
            )
        )
    ;
        Args = [],
        short_usage(StdOutStream, !IO)
    ).

:- func kind_warning = string.

kind_warning =
    "Warning: the original trace count files did not include all labels.\n".

:- func consistency_warning = string.

consistency_warning =
    "Warning: reporting on a mixture of trace file types and/or programs.\n".

%---------------------------------------------------------------------------%

:- type proc_info
    --->    proc_info(
                proc_source_file    :: string,
                proc_line_number    :: int,
                proc_proc           :: proc_label
            ).

:- type label_info
    --->    label_info(
                label_source_file   :: string,
                label_line_number   :: int,
                label_proc          :: proc_label,
                label_path_port     :: path_port
            ).

:- type trace_counts_list ==
    assoc_list(proc_label_in_context, proc_trace_counts).

:- type module_restriction
    --->    module_restriction_default
            % The user did not specify a module restriction.
            % Apply whatever restriction is implied by the command line
            % options.

    ;       module_restriction_user(set(module_name)).
            % Only print coverage data for the user-specified set of modules
            % given by the argument.

:- pred write_coverage_test(io.text_output_stream::in,
    option_table(option)::in, module_restriction::in,
    trace_counts::in, io::di, io::uo) is det.

write_coverage_test(OutStream, OptionTable, RestrictToModules, TraceCountMap,
        !IO) :-
    map.to_assoc_list(TraceCountMap, TraceCounts0),
    (
        RestrictToModules = module_restriction_default,
        lookup_bool_option(OptionTable, ignore_stdlib, IgnoreStdlib),
        lookup_bool_option(OptionTable, ignore_mdbcomp, IgnoreMdbcomp),
        (
            IgnoreStdlib = yes,
            IgnoreMdbcomp = yes,
            list.negated_filter(in_stdlib_or_mdbcomp_module,
                TraceCounts0, TraceCounts)
        ;
            IgnoreStdlib = yes,
            IgnoreMdbcomp = no,
            list.negated_filter(in_stdlib_module, TraceCounts0, TraceCounts)
        ;
            IgnoreStdlib = no,
            IgnoreMdbcomp = yes,
            list.negated_filter(in_mdbcomp_module, TraceCounts0, TraceCounts)
        ;
            IgnoreStdlib = no,
            IgnoreMdbcomp = no,
            TraceCounts = TraceCounts0
        )
    ;
        RestrictToModules = module_restriction_user(Modules),
        list.filter(in_module_set(Modules), TraceCounts0, TraceCounts)
    ),
    lookup_bool_option(OptionTable, detailed, Detailed),
    (
        Detailed = no,
        collect_zero_count_local_procs(TraceCounts, ZeroCountProcs),
        list.sort(ZeroCountProcs, SortedZeroCountProcs),
        io.write_string(OutStream, "Unexecuted procedures:\n\n", !IO),
        list.foldl(write_proc_info(OutStream), SortedZeroCountProcs, !IO)
    ;
        Detailed = yes,
        collect_zero_count_local_labels(TraceCounts, [], ZeroCountLabels),
        list.sort(ZeroCountLabels, SortedZeroCountLabels),
        io.write_string(OutStream, "Unexecuted labels:\n\n", !IO),
        list.foldl(write_label_info(OutStream), SortedZeroCountLabels, !IO)
    ).

:- pred in_module_set(set(module_name)::in, pair(proc_label_in_context, T)::in)
    is semidet.

in_module_set(Modules, ProcLabelInContext - _) :-
    ProcLabelInContext = proc_label_in_context(Module, _, _),
    set.member(Module, Modules).

:- pred in_stdlib_module(pair(proc_label_in_context, proc_trace_counts)::in)
    is semidet.

in_stdlib_module(ProcLabelInContext - _) :-
    ProcLabelInContext = proc_label_in_context(Module, _, _),
    is_std_lib_module_name(Module, _).

:- pred in_mdbcomp_module(pair(proc_label_in_context, proc_trace_counts)::in)
    is semidet.

in_mdbcomp_module(ProcLabelInContext - _) :-
    ProcLabelInContext = proc_label_in_context(Module, _, _),
    is_mdbcomp_module_name(Module).

:- pred in_stdlib_or_mdbcomp_module(
    pair(proc_label_in_context, proc_trace_counts)::in) is semidet.

in_stdlib_or_mdbcomp_module(ProcLabelInContext - _) :-
    ProcLabelInContext = proc_label_in_context(Module, _, _),
    ( is_std_lib_module_name(Module, _)
    ; is_mdbcomp_module_name(Module)
    ).

%---------------------------------------------------------------------------%

:- pred collect_zero_count_local_procs(trace_counts_list::in,
    list(proc_info)::out) is det.

collect_zero_count_local_procs(TraceCounts, ZeroCountProcInfos) :-
    collect_proc_infos_counts(TraceCounts, map.init, ProcInfoMap,
        map.init, CountMap),
    map.to_assoc_list(CountMap, CountList),
    list.filter_map(is_zero_count_local_proc(ProcInfoMap), CountList,
        ZeroCountProcInfos).

:- pred collect_proc_infos_counts(trace_counts_list::in,
    map(proc_label, proc_info)::in, map(proc_label, proc_info)::out,
    map(proc_label, int)::in, map(proc_label, int)::out) is det.

collect_proc_infos_counts([], !ProcInfoMap, !CountMap).
collect_proc_infos_counts([Assoc | Assocs], !ProcInfoMap, !CountMap) :-
    Assoc = LabelFilename - PathPortCountMap,
    LabelFilename = proc_label_in_context(_ModuleNameSym, FileName, ProcLabel),
    map.foldl2(proc_process_path_port_count, PathPortCountMap,
        no, MaybeCallInfo, 0, CurCount),
    ( if map.search(!.CountMap, ProcLabel, OldCount) then
        map.det_update(ProcLabel, OldCount + CurCount, !CountMap)
    else
        map.det_insert(ProcLabel, CurCount, !CountMap)
    ),
    (
        MaybeCallInfo = no
    ;
        MaybeCallInfo = yes(LineNumber),
        ProcInfo = proc_info(FileName, LineNumber, ProcLabel),
        map.det_insert(ProcLabel, ProcInfo, !ProcInfoMap)
    ),
    collect_proc_infos_counts(Assocs, !ProcInfoMap, !CountMap).

:- pred proc_process_path_port_count(path_port::in, line_no_and_count::in,
    maybe(int)::in, maybe(int)::out, int::in, int::out) is det.

proc_process_path_port_count(PathPort, LineNumberAndCount, !MaybeCallInfo,
        !Count) :-
    LineNumberAndCount = line_no_and_count(LineNumber, CurCount, _NumTests),
    !:Count = !.Count + CurCount,
    ( if PathPort = port_only(port_call) then
        require(unify(!.MaybeCallInfo, no),
            "proc_process_path_port_count: duplicate call port:"),
        !:MaybeCallInfo = yes(LineNumber)
    else
        true
    ).

:- pred is_zero_count_local_proc(map(proc_label, proc_info)::in,
    pair(proc_label, int)::in, proc_info::out) is semidet.

is_zero_count_local_proc(ProcInfoMap, ProcLabel - Count, ProcInfo) :-
    Count = 0,
    is_local_proc(ProcLabel),
    map.lookup(ProcInfoMap, ProcLabel, ProcInfo).

%---------------------------------------------------------------------------%

:- pred collect_zero_count_local_labels(trace_counts_list::in,
    list(label_info)::in, list(label_info)::out) is det.

collect_zero_count_local_labels([], !ZeroLabelInfos).
collect_zero_count_local_labels([Assoc | Assocs], !ZeroLabelInfos) :-
    Assoc = LabelFilename - PathPortCountMap,
    LabelFilename = proc_label_in_context(_ModuleNameSym, FileName, ProcLabel),
    map.foldl(label_process_path_port_count(ProcLabel, FileName),
        PathPortCountMap, !ZeroLabelInfos),
    collect_zero_count_local_labels(Assocs, !ZeroLabelInfos).

:- pred label_process_path_port_count(proc_label::in, string::in,
    path_port::in, line_no_and_count::in,
    list(label_info)::in, list(label_info)::out) is det.

label_process_path_port_count(ProcLabel, FileName,
        PathPort, LineNumberAndCount, !ZeroLabelInfos) :-
    LineNumberAndCount = line_no_and_count(LineNumber, Count, _NumTests),
    ( if
        Count = 0,
        is_local_proc(ProcLabel)
    then
        LabelInfo = label_info(FileName, LineNumber, ProcLabel, PathPort),
        !:ZeroLabelInfos = [LabelInfo | !.ZeroLabelInfos]
    else
        true
    ).

%---------------------------------------------------------------------------%

    % We don't want to warn about zero executions of a copy of a procedure
    % inlined in a module other than the module that defines the procedure.
    %
:- pred is_local_proc(proc_label::in) is semidet.

is_local_proc(ProcLabel) :-
    (
        ProcLabel = ordinary_proc_label(DefModuleSym, _, DeclModuleSym,
            _, _, _),
        DefModuleSym = DeclModuleSym
    ;
        ProcLabel = special_proc_label(DefModuleSym, _, TypeModuleSym,
            _, _, _),
        DefModuleSym = TypeModuleSym
    ).

%---------------------------------------------------------------------------%

:- pred write_proc_info(io.text_output_stream::in, proc_info::in,
    io::di, io::uo) is det.

write_proc_info(OutStream, ProcInfo, !IO) :-
    ProcInfo = proc_info(FileName, LineNumber, ProcLabel),
    write_context(OutStream, FileName, LineNumber, !IO),
    write_proc_label_for_user(OutStream, ProcLabel, !IO),
    io.nl(OutStream, !IO).

:- pred write_label_info(io.text_output_stream::in, label_info::in,
    io::di, io::uo) is det.

write_label_info(OutStream, LabelInfo, !IO) :-
    LabelInfo = label_info(FileName, LineNumber, ProcLabel, PathPort),
    write_context(OutStream, FileName, LineNumber, !IO),
    write_proc_label_for_user(OutStream, ProcLabel, !IO),
    io.write_char(OutStream, ' ', !IO),
    write_path_port_for_user(OutStream, PathPort, !IO),
    io.nl(OutStream, !IO).

:- pred write_context(io.text_output_stream::in, string::in, int::in,
    io::di, io::uo) is det.

write_context(OutStream, FileName, LineNumber, !IO) :-
    io.format(OutStream, "%s:%d: ", [s(FileName), i(LineNumber)], !IO).

:- pred write_proc_label_for_user(io.text_output_stream::in, proc_label::in,
    io::di, io::uo) is det.

write_proc_label_for_user(OutStream, ProcLabel, !IO) :-
    (
        ProcLabel = ordinary_proc_label(_DefModuleSym, PredOrFunc,
            _DeclModuleSym, Name, Arity, Mode),
        (
            PredOrFunc = pf_predicate,
            PredOrFuncStr = "pred"
        ;
            PredOrFunc = pf_function,
            PredOrFuncStr = "func"
        ),
        QuotedName = term_io.quoted_atom(Name),
        io.format(OutStream, "%s %s/%d-%d",
            [s(PredOrFuncStr), s(QuotedName), i(Arity), i(Mode)], !IO)
    ;
        % We don't record trace counts in special preds.
        ProcLabel = special_proc_label(_, _, _, _, _, _),
        unexpected($file, $pred, "special_pred")
    ).

:- pred write_path_port_for_user(io.text_output_stream::in, path_port::in,
    io::di, io::uo) is det.

write_path_port_for_user(OutStream, port_only(Port), !IO) :-
    string_to_trace_port(PortStr, Port),
    io.write_string(OutStream, PortStr, !IO).
write_path_port_for_user(OutStream, path_only(Path), !IO) :-
    io.write_strings(OutStream, ["<", rev_goal_path_to_string(Path), ">"], !IO).
write_path_port_for_user(OutStream, port_and_path(Port, Path), !IO) :-
    string_to_trace_port(PortStr, Port),
    io.write_strings(OutStream,
        [PortStr, " <", rev_goal_path_to_string(Path), ">"], !IO).

%---------------------------------------------------------------------------%

:- pred short_usage(io.text_output_stream::in, io::di, io::uo) is det.

short_usage(OutStream, !IO) :-
    io.progname_base("mcov", ProgName, !IO),
    library.version(Version, FullArch),
    io.write_strings(OutStream, [
        "Mercury Coverage Testing Tool, version ", Version,
            ", on ", FullArch, ".\n",
        "Copyright (C) 2006-2007, 2010-2012 The University of Melbourne\n",
        "Copyright (C) 2014-2016, 2020-2023 The Mercury team\n",
        "Usage: ", ProgName, " [<options>] [<files>]\n",
        "Use `", ProgName, " --help' for more information.\n"
    ], !IO).

:- pred long_usage(io.text_output_stream::in, io::di, io::uo) is det.

long_usage(OutStream, !IO) :-
    library.version(Version, FullArch),
    io.format(OutStream,
        "Name: mcov -- Mercury Coverage Testing Tool, version %s, on %s\n",
        [s(Version), s(FullArch)], !IO),
    io.write_string(OutStream,
        "Copyright (C) 2006-2007, 2010-2012 The University of Melbourne\n", !IO),
    io.write_string(OutStream,
        "Copyright (C) 2014-2016, 2020-2023 The Mercury team\n", !IO),
    io.write_string(OutStream, "Usage: mcov [<options>] <arguments>\n", !IO),
    io.write_string(OutStream, "Arguments:\n", !IO),
    io.write_string(OutStream,
        "\tArguments are assumed to Mercury trace count files.\n", !IO),
    io.write_string(OutStream, "Options:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "-?, -h, --help",
        "\tPrint help about using mcov (on the standard output) and exit",
        "\twithout doing any further processing.",
        "-v, --verbose",
        "\tPrint the name of each trace count file as it is added to the union",
        "-d, --detailed",
        "\tPrint a report for each label that has not been executed, even",
        "\tif some other code has been executed in the same procedure.",
        "-m <module>, --module <module>",
        "\tRestrict the output to the module named by the argument.",
        "\tMultiple module options accumulate.",
        "-o <file>, --output-file <file>",
        "\tPrint output to the file specified by the argument.",
        "\tBy default the output will be printed to the standard output.",
        "--flags <file>, --flags-file <file>",
        "\tTake options from the specified file, and handle them as if they",
        "\twere specified on the command line.",
        "--no-ignore-stdlib",
        "\tInclude information about labels in the Mercury standard library",
        "\tin the reports."
        % This option is only useful for Mercury developers.
        %"--no-ignore-mdbcomp",
        %"\tInclude information about labels in the mdbcomp library in the",
        %"\treports.".
    ], !IO).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       verbose
    ;       detailed
    ;       modules
    ;       output_filename
    ;       flags_file
    ;       ignore_stdlib
    ;       ignore_mdbcomp.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('?', help).
short_option('h', help).
short_option('v', verbose).
short_option('d', detailed).
short_option('m', modules).
short_option('o', output_filename).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",             help).
long_option("verbose",          verbose).
long_option("detailed",         detailed).
long_option("module",           modules).
long_option("output-file",      output_filename).
long_option("flags",            flags_file).
long_option("flags-file",       flags_file).
long_option("ignore-stdlib",    ignore_stdlib).
long_option("ignore-mdbcomp",   ignore_mdbcomp).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(detailed,        bool(no)).
option_default(modules,         accumulating([])).
option_default(output_filename, string("")).
option_default(flags_file,      file_special).
option_default(ignore_stdlib,   bool(yes)).
option_default(ignore_mdbcomp,  bool(yes)).

%---------------------------------------------------------------------------%

:- pred write_error_message(string::in, io::di, io::uo) is det.

write_error_message(Msg, !IO) :-
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "%s\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%
:- end_module mcov.
%---------------------------------------------------------------------------%
