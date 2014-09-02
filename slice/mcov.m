%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Mercury coverage test tool.
%
% Author: Zoltan Somogyi.
%
%-----------------------------------------------------------------------------%

:- module mcov.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.sym_name.
:- import_module mdbcomp.trace_counts.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_io.

main(!IO) :-
    unlimit_stack(!IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        (
            Args = [_ | _],
            lookup_bool_option(OptionTable, verbose, Verbose),
            read_and_union_trace_counts(Verbose, Args, _NumTests, FileTypes,
                TraceCounts, MaybeReadError, !IO),
            stderr_stream(StdErr, !IO),
            (
                MaybeReadError = yes(ReadErrorMsg),
                io.write_string(StdErr, ReadErrorMsg, !IO),
                io.nl(StdErr, !IO)
            ;
                MaybeReadError = no,
                set.to_sorted_list(FileTypes, FileTypeList),
                ( FileTypeList = [single_file(BaseType)] ->
                    BaseType = base_count_file_type(Kind, _Program),
                    (
                        Kind = user_nonzero,
                        io.write_string(StdErr, kind_warning, !IO)
                    ;
                        Kind = user_all
                    )
                ;
                    io.write_string(StdErr, consistency_warning, !IO)
                ),
                lookup_bool_option(OptionTable, detailed, Detailed),
                lookup_accumulating_option(OptionTable, modules, Modules),
                (
                    Modules = [],
                    RestrictToModules = no
                ;
                    Modules = [_ | _],
                    ModuleSyms = list.map(string_to_sym_name, Modules),
                    RestrictToModules = yes(set.list_to_set(ModuleSyms))
                ),
                lookup_string_option(OptionTable, output_filename, OutputFile),
                ( OutputFile = "" ->
                    write_coverage_test(Detailed, RestrictToModules,
                        TraceCounts, !IO)
                ;
                    io.tell(OutputFile, OpenRes, !IO),
                    (
                        OpenRes = ok,
                        write_coverage_test(Detailed, RestrictToModules,
                            TraceCounts, !IO)
                    ;
                        OpenRes = error(OpenErrorMsg),
                        io.write_string(StdErr, "Error opening " ++
                            "file `" ++ OutputFile ++ "'" ++ ": " ++
                            string(OpenErrorMsg), !IO),
                        io.nl(StdErr, !IO)
                    )
                )
            )
        ;
            Args = [],
            usage(!IO)
        )
    ;
        GetoptResult = error(GetoptErrorMsg),
        io.write_string(GetoptErrorMsg, !IO),
        io.nl(!IO)
    ).

:- func kind_warning = string.

kind_warning =
    "Warning: the original trace count files did not include all labels.\n".

:- func consistency_warning = string.

consistency_warning =
    "Warning: reporting on a mixture of trace file types and/or programs.\n".

%-----------------------------------------------------------------------------%

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

:- pred write_coverage_test(bool::in, maybe(set(module_name))::in,
    trace_counts::in, io::di, io::uo) is det.

write_coverage_test(Detailed, RestrictToModules, TraceCountMap, !IO) :-
    map.to_assoc_list(TraceCountMap, TraceCounts0),
    (
        RestrictToModules = no,
        TraceCounts = TraceCounts0
    ;
        RestrictToModules = yes(Modules),
        list.filter(in_module_set(Modules), TraceCounts0, TraceCounts)
    ),
    (
        Detailed = no,
        collect_zero_count_local_procs(TraceCounts, ZeroCountProcs),
        sort(ZeroCountProcs, SortedZeroCountProcs),
        io.write_string("Unexecuted procedures:\n\n", !IO),
        list.foldl(write_proc_info, SortedZeroCountProcs, !IO)
    ;
        Detailed = yes,
        collect_zero_count_local_labels(TraceCounts, [], ZeroCountLabels),
        sort(ZeroCountLabels, SortedZeroCountLabels),
        io.write_string("Unexecuted labels:\n\n", !IO),
        list.foldl(write_label_info, SortedZeroCountLabels, !IO)
    ).

:- pred in_module_set(set(module_name)::in, pair(proc_label_in_context, T)::in)
    is semidet.

in_module_set(Modules, ProcLabelInContext - _) :-
    ProcLabelInContext = proc_label_in_context(Module, _, _),
    set.member(Module, Modules).

%-----------------------------------------------------------------------------%

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
    ( map.search(!.CountMap, ProcLabel, OldCount) ->
        map.det_update(ProcLabel, OldCount + CurCount, !CountMap)
    ;
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
    ( PathPort = port_only(port_call) ->
        require(unify(!.MaybeCallInfo, no),
            "proc_process_path_port_count: duplicate call port:"),
        !:MaybeCallInfo = yes(LineNumber)
    ;
        true
    ).

:- pred is_zero_count_local_proc(map(proc_label, proc_info)::in,
    pair(proc_label, int)::in, proc_info::out) is semidet.

is_zero_count_local_proc(ProcInfoMap, ProcLabel - Count, ProcInfo) :-
    Count = 0,
    is_local_proc(ProcLabel),
    map.lookup(ProcInfoMap, ProcLabel, ProcInfo).

%-----------------------------------------------------------------------------%

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
    (
        Count = 0,
        is_local_proc(ProcLabel)
    ->
        LabelInfo = label_info(FileName, LineNumber, ProcLabel, PathPort),
        !:ZeroLabelInfos = [LabelInfo | !.ZeroLabelInfos]
    ;
        true
    ).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- pred write_proc_info(proc_info::in, io::di, io::uo) is det.

write_proc_info(ProcInfo, !IO) :-
    ProcInfo = proc_info(FileName, LineNumber, ProcLabel),
    io.write_string(FileName, !IO),
    io.write_char(':', !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(": ", !IO),
    write_proc_label_for_user(ProcLabel, !IO),
    io.nl(!IO).

:- pred write_label_info(label_info::in, io::di, io::uo) is det.

write_label_info(LabelInfo, !IO) :-
    LabelInfo = label_info(FileName, LineNumber, ProcLabel, PathPort),
    io.write_string(FileName, !IO),
    io.write_char(':', !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(": ", !IO),
    write_proc_label_for_user(ProcLabel, !IO),
    write_path_port_for_user(PathPort, !IO),
    io.nl(!IO).

:- pred write_proc_label_for_user(proc_label::in, io::di, io::uo) is det.

write_proc_label_for_user(ProcLabel, !IO) :-
    (
        ProcLabel = ordinary_proc_label(_DefModuleSym, PredOrFunc,
            _DeclModuleSym, Name, Arity, Mode),
        (
            PredOrFunc = pf_predicate,
            io.write_string("pred ", !IO)
        ;
            PredOrFunc = pf_function,
            io.write_string("func ", !IO)
        ),
        term_io.quote_atom(Name, !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO),
        io.write_string("-", !IO),
        io.write_int(Mode, !IO)
    ;
        % We don't record trace counts in special preds.
        ProcLabel = special_proc_label(_, _, _, _, _, _),
        error("write_proc_label_for_user: special_pred")
    ).

:- pred write_path_port_for_user(path_port::in, io::di, io::uo) is det.

write_path_port_for_user(port_only(Port), !IO) :-
    string_to_trace_port(PortStr, Port),
    io.write_string(PortStr, !IO).
write_path_port_for_user(path_only(Path), !IO) :-
    io.write_strings(["<", rev_goal_path_to_string(Path), ">"], !IO).
write_path_port_for_user(port_and_path(Port, Path), !IO) :-
    string_to_trace_port(PortStr, Port),
    io.write_strings([PortStr, " <", rev_goal_path_to_string(Path), ">"], !IO).

%-----------------------------------------------------------------------------%

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_strings([
        "Usage: mcov [-d] [-v] [-m module] [-o output_file] file1 file2 ...\n",
        "The -d or --detailed option causes the printing of a report for\n",
        "each label that has not been executed, even if some other code\n",
        "has been executed in the same procedure.\n",
        "The -v or --verbose option causes each trace count file name\n",
        "to be printed as it is added to the union.\n",
        "file1, file2, etc should be trace count files.\n",
        "If one or more -m or --module options are given, then the output\n",
        "will be restricted to the modules named by their arguments.\n",
        "The argument of the -o or --output-file option gives the name\n",
        "of the output file.\n"],
        !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    detailed
    ;       modules
    ;       output_filename
    ;       verbose.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.

option_default(detailed,        bool(no)).
option_default(modules,         accumulating([])).
option_default(output_filename, string("")).
option_default(verbose,         bool(no)).

short_option('d',               detailed).
short_option('m',               modules).
short_option('o',               output_filename).
short_option('v',               verbose).

long_option("detailed",         detailed).
long_option("module",           modules).
long_option("output-file",      output_filename).
long_option("verbose",          verbose).

%-----------------------------------------------------------------------------%
