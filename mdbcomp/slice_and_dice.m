%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: dice.m
% Authors: Ian MacLarty and Zoltan Somogyi
%
% This module contains code for generating and manipulating slices and dices.
% A dice is the difference between one or more passing test runs
% and one (or more, but that is not yet implemented) failing test runs.
%
%---------------------------------------------------------------------------%

:- module mdbcomp.slice_and_dice.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.trace_counts.

:- import_module io.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type slice
    --->    slice(
                num_tests       :: int,
                slice_proc_map  :: map(proc_label, proc_slice)
            ).

:- type slice_proc_map   == map(proc_label, proc_slice).

:- type proc_slice       == map(path_port, slice_exec_count).

:- type slice_exec_count
    --->    slice_exec_count(
                slice_filename      ::  string,
                slice_linenumber    ::  int,

                % The number of times the label was executed
                % in all the test runs.
                slice_count         :: int,

                % The number of test runs the label was executed in.
                slice_tests         :: int
            ).

    % read_slice(File, MaybeSlice, !IO):
    %
    % Read the slices from File.
    %
:- pred read_slice(string::in, maybe_error(slice)::out, io::di, io::uo) is det.

    % read_slice_to_string(File, SortStr, MaxRows, MaybeMaxPredColumns,
    %   MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns,
    %   Module, SliceStr, Problem, !IO):
    %
    % Read the slice from File, and convert it to a string suitable for
    % displaying on the screen, sorting it first using SortStr. SortStr
    % can be any combination of the letters "cCtT" and indicates how the slice
    % is to be sorted. See the documentation for the `mslice' tool in the
    % user guide for an explanation of the sort string. Take only the top
    % MaxRows lines of the sorted list. If MaybeMaxPredColumns is yes,
    % allow only the number of columns it specifies for predicate names.
    % If MaybeMaxPathColumns is yes, allow only the number of columns it
    % specifies for ports and paths. If MaybeMaxFileColumns is yes, allow
    % only the number of columns it specifies for file names and line numbers.
    %
    % If Module is not the empty string then only labels from the named module
    % will be included in the dice string, otherwise all modules will be
    % included.
    %
    % If there was a problem reading the trace counts then Problem will
    % contain a string describing the problem encountered and SliceStr
    % will be the empty string, otherwise Problem will be the empty string.
    %
:- pred read_slice_to_string(string::in, string::in, int::in,
    maybe(int)::in, maybe(int)::in, maybe(int)::in,
    string::in, string::out, string::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type dice
    --->    dice(
                num_pass_tests  :: int,
                num_fail_tests  :: int,
                dice_proc_map   :: map(proc_label, proc_dice)
            ).

:- type dice_proc_map   == map(proc_label, proc_dice).

:- type proc_dice       == map(path_port, dice_exec_count).

:- type dice_exec_count
    --->    dice_exec_count(
                dice_filename   ::  string,
                dice_linenumber ::  int,

                % The number of times the label was executed in all
                % the passing test runs.
                pass_count      :: int,

                % The number of passing test runs the label was executed in.
                pass_tests      :: int,

                % The number of times the label was executed in failing
                % test runs.
                fail_count      :: int,

                % The number of failing test runs the label was executed in.
                fail_tests      :: int
            ).

    % read_dice(PassFile, FailFile, MaybeDice, !IO):
    %
    % Read the slice from PassFile, interpreting it as (a union of) passing
    % slices; read the slices from FailFile, interpreting it as (a union of)
    % failing slices; then produce the dice you get from them.
    %
:- pred read_dice(string::in, string::in, maybe_error(dice)::out,
    io::di, io::uo) is det.

    % read_dice_to_string(PassFile, FailFile, SortStr, MaxRow,
    %   MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns,
    %   Module, DiceStr, Problem, !IO):
    %
    % Read the slice from PassFile, interpreting it as (a union of) passing
    % slices; read the slices from FailFile, interpreting it as (a union of)
    % failing slices; then produce the dice you get from them.
    %
    % Then convert the dice to a string suitable for displaying on the screen,
    % sorting it first using SortStr. SortStr can be any combination of the
    % letters "sSpPfPdD" and indicates how the dice is to be sorted.
    % See the documentation for the `dice' command in the user guide
    % for an explanation of the sort string. Take only the top
    % MaxRows lines of the sorted list. If MaybeMaxPredColumns is yes,
    % allow only the number of columns it specifies for predicate names.
    % If MaybeMaxPathColumns is yes, allow only the number of columns it
    % specifies for ports and paths. If MaybeMaxFileColumns is yes, allow
    % only the number of columns it specifies for file names and line numbers.
    %
    % If Module is not the empty string then only labels from the named module
    % will be included in the dice string, otherwise all modules will be
    % included.
    %
    % If there was a problem reading the trace counts then Problem will
    % contain a string describing the problem encountered and DiceStr
    % will be the empty string, otherwise Problem will be the empty string.
    %
:- pred read_dice_to_string(string::in, string::in, string::in, int::in,
    maybe(int)::in, maybe(int)::in, maybe(int)::in,
    string::in, string::out, string::out, io::di, io::uo) is det.

    % suspicion_ratio(PassCount, FailCount) = Suspicion.
    % suspicion_ratio gives an indication of how likely a label is to
    % be buggy based on how many times it was executed in passing and
    % failing test runs.
    %
:- func suspicion_ratio(int, int) = float.

    % suspicion_ratio_normalised(PassCount, PassTests, FailCount, FailTests)
    %   = Suspicion.
    % suspicion_ratio_normalised gives an indication of how likely a label is
    % to be buggy based on how many times it was executed in passing and
    % failing test runs and on how many passing and failing test runs there
    % were.
    %
:- func suspicion_ratio_normalised(int, int, int, int) = float.

    % suspicion_ratio_binary(PassCount, FailCount) = Suspicion.
    % suspicion_ration_binary returns 1 if PassCount is 0 and FailCount is
    % > 0 and 0 otherwise.
    %
:- func suspicion_ratio_binary(int, int) = float.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.

:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%
% The mechanism for reading in slices. The structure is similar to the
% mechanism for reading in dices below.
%

read_slice(File, Result, !IO) :-
    read_trace_counts_source(File, ReadResult, !IO),
    (
        ReadResult = list_ok(FileType, TraceCounts),
        slice_merge_trace_counts(TraceCounts, map.init, SliceProcMap),
        NumTests = num_tests_for_file_type(FileType),
        Slice = slice(NumTests, SliceProcMap),
        Result = ok(Slice)
    ;
        ReadResult = list_error_message(Problem),
        Result = error(Problem)
    ).

%---------------------------------------------------------------------------%
%
% A mechanism for sorting and formatting slices. The structure is similar
% to the mechanism for sorting and formatting dices below.
%

read_slice_to_string(File, SortStr0, MaxRows,
        MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns,
        Module, SliceStr, Problem, !IO) :-
    ( if slice_sort_string_is_valid(SortStr0) then
        read_slice(File, ReadSliceResult, !IO),
        (
            ReadSliceResult = ok(Slice),
            Slice = slice(TotalTests, SliceProcMap),
            LabelCounts = slice_to_label_counts(SliceProcMap),
            ( if Module = "" then
                ModuleFilteredLabelCounts = LabelCounts
            else
                list.filter(slice_label_count_is_for_module(Module),
                    LabelCounts, ModuleFilteredLabelCounts)
            ),
            ( if string.append("z", SortStrPrime, SortStr0) then
                SortStr = SortStrPrime,
                list.filter(slice_label_count_is_zero,
                    ModuleFilteredLabelCounts, FilteredLabelCounts)
            else
                SortStr = SortStr0,
                FilteredLabelCounts = ModuleFilteredLabelCounts
            ),
            list.sort(slice_label_count_compare(SortStr), FilteredLabelCounts,
                SortedLabelCounts),
            ( if list.take(MaxRows, SortedLabelCounts, Taken) then
                TopNLabelCounts = Taken
            else
                TopNLabelCounts = SortedLabelCounts
            ),
            Problem = "",
            SliceStr = format_slice_label_counts(TopNLabelCounts, TotalTests,
                MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns)
        ;
            ReadSliceResult = error(Problem),
            SliceStr = ""
        )
    else
        Problem = "Invalid sort string",
        SliceStr = ""
    ).

%---------------------------------------------------------------------------%

    % Add the trace_counts to the given slice.
    %
:- pred slice_merge_trace_counts(trace_counts::in,
    slice_proc_map::in, slice_proc_map::out) is det.

slice_merge_trace_counts(TraceCounts, !SliceProcMap) :-
    map.foldl(slice_merge_proc_trace_counts, TraceCounts, !SliceProcMap).

:- pred slice_merge_proc_trace_counts(proc_label_in_context::in,
    proc_trace_counts::in, slice_proc_map::in, slice_proc_map::out) is det.

slice_merge_proc_trace_counts(ProcLabelAndFile, ProcTraceCounts,
        !SliceProcMap) :-
    ProcLabelAndFile = proc_label_in_context(_ModuleNameSym, FileName,
        ProcLabel),
    ( if map.search(!.SliceProcMap, ProcLabel, FoundProcSlice) then
        map.foldl(slice_merge_path_port(FileName), ProcTraceCounts,
            FoundProcSlice, MergedProcSlice),
        map.det_update(ProcLabel, MergedProcSlice, !SliceProcMap)
    else
        map.foldl(slice_merge_path_port(FileName), ProcTraceCounts,
            map.init, MergedProcSlice),
        map.det_insert(ProcLabel, MergedProcSlice, !SliceProcMap)
    ).

:- pred slice_merge_path_port(string::in, path_port::in, line_no_and_count::in,
    proc_slice::in, proc_slice::out) is det.

slice_merge_path_port(FileName, PathPort, LineNoAndCount, !ProcSlice) :-
    ( if
        map.transform_value(slice_add_trace_count(LineNoAndCount),
            PathPort, !.ProcSlice, UpdatedProcSlice)
    then
        !:ProcSlice = UpdatedProcSlice
    else
        LineNoAndCount = line_no_and_count(LineNumber, ExecCount, NumTests),
        SliceExecCount = slice_exec_count(FileName, LineNumber, ExecCount,
            NumTests),
        map.det_insert(PathPort, SliceExecCount, !ProcSlice)
    ).

:- pred slice_add_trace_count(line_no_and_count::in,
    slice_exec_count::in, slice_exec_count::out) is det.

slice_add_trace_count(LineNoAndCount, ExecCounts0, ExecCounts) :-
    LineNoAndCount = line_no_and_count(_LineNumber, ExecCount, NumTests),
    ExecCounts0 = slice_exec_count(FileName, LineNumber, Exec, Tests),
    ExecCounts = slice_exec_count(FileName, LineNumber, Exec + ExecCount,
        Tests + NumTests).

%---------------------------------------------------------------------------%
%
% The mechanism for reading in dices. The structure is similar to the
% mechanism for reading in slices above.
%

:- pragma foreign_export("C", read_dice(in, in, out, di, uo),
    "MR_MDBCOMP_read_dice").

read_dice(PassFile, FailFile, Result, !IO) :-
    read_trace_counts_source(PassFile, ReadPassResult, !IO),
    (
        ReadPassResult = list_ok(PassFileType, PassTraceCounts),
        read_trace_counts_source(FailFile, ReadFailResult, !IO),
        (
            ReadFailResult = list_ok(FailFileType, FailTraceCounts),
            dice_merge_trace_counts(pass_slice, PassTraceCounts,
                map.init, PassDiceProcMap),
            dice_merge_trace_counts(fail_slice, FailTraceCounts,
                PassDiceProcMap, DiceProcMap),
            TotalPassTests = num_tests_for_file_type(PassFileType),
            TotalFailTests = num_tests_for_file_type(FailFileType),
            Dice = dice(TotalPassTests, TotalFailTests, DiceProcMap),
            Result = ok(Dice)
        ;
            ReadFailResult = list_error_message(Problem),
            Result = error(Problem)
        )
    ;
        ReadPassResult = list_error_message(Problem),
        Result = error(Problem)
    ).

:- pred maybe_dice_error_to_problem_string(maybe_error(dice)::in, string::out)
    is det.

:- pragma foreign_export("C", maybe_dice_error_to_problem_string(in, out),
    "MR_MDBCOMP_maybe_dice_error_to_problem_string").

maybe_dice_error_to_problem_string(ok(_), "").
maybe_dice_error_to_problem_string(error(ErrorStr), ErrorStr).

:- pred det_maybe_dice_error_to_dice(maybe_error(dice)::in, dice::out) is det.

:- pragma foreign_export("C", det_maybe_dice_error_to_dice(in, out),
    "MR_MDBCOMP_det_maybe_dice_error_to_dice").

det_maybe_dice_error_to_dice(ok(Dice), Dice).
det_maybe_dice_error_to_dice(error(_), _) :-
    error("det_maybe_dice_error_to_dice: result is error").

:- type trace_counts_kind
    --->    pass_slice
    ;       fail_slice.

    % Merge the passing or failing trace_counts into the given dice.
    %
:- pred dice_merge_trace_counts(trace_counts_kind::in, trace_counts::in,
    dice_proc_map::in, dice_proc_map::out) is det.

dice_merge_trace_counts(Kind, TraceCounts, !DiceProcMap) :-
    map.foldl(dice_merge_proc_trace_counts(Kind), TraceCounts, !DiceProcMap).

:- pred dice_merge_proc_trace_counts(trace_counts_kind::in,
    proc_label_in_context::in, proc_trace_counts::in, dice_proc_map::in,
    dice_proc_map::out) is det.

dice_merge_proc_trace_counts(Kind, ProcLabelAndFile, ProcTraceCounts,
        !DiceProcMap) :-
    ProcLabelAndFile = proc_label_in_context(_ModuleNameSym, FileName,
        ProcLabel),
    ( if map.search(!.DiceProcMap, ProcLabel, FoundProcDice) then
        map.foldl(dice_merge_path_port(FileName, Kind), ProcTraceCounts,
            FoundProcDice, MergedProcDice),
        map.det_update(ProcLabel, MergedProcDice, !DiceProcMap)
    else
        map.foldl(dice_merge_path_port(FileName, Kind), ProcTraceCounts,
            map.init, MergedProcDice),
        map.det_insert(ProcLabel, MergedProcDice, !DiceProcMap)
    ).

:- pred dice_merge_path_port(string::in, trace_counts_kind::in, path_port::in,
    line_no_and_count::in, proc_dice::in, proc_dice::out) is det.

dice_merge_path_port(FileName, Kind, PathPort, LineNoAndCount, !ProcDice) :-
    ( if
        map.transform_value(dice_add_trace_count(Kind, LineNoAndCount),
            PathPort, !.ProcDice, UpdatedProcDice)
    then
        !:ProcDice = UpdatedProcDice
    else
        LineNoAndCount = line_no_and_count(LineNumber, ExecCount, NumTests),
        (
            Kind = pass_slice,
            InitCount = dice_exec_count(FileName, LineNumber,
                ExecCount, NumTests, 0, 0)
        ;
            Kind = fail_slice,
            InitCount = dice_exec_count(FileName, LineNumber, 0, 0,
                ExecCount, NumTests)
        ),
        map.det_insert(PathPort, InitCount, !ProcDice)
    ).

:- pred dice_add_trace_count(trace_counts_kind::in, line_no_and_count::in,
    dice_exec_count::in, dice_exec_count::out) is det.

dice_add_trace_count(pass_slice, LineNoAndCount, ExecCounts0, ExecCounts) :-
    LineNoAndCount = line_no_and_count(_LineNumber, ExecCount, NumTests),
    ExecCounts0 = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec, FailTests),
    ExecCounts = dice_exec_count(FileName, LineNumber,
        PassExec + ExecCount, PassTests + NumTests, FailExec, FailTests).
dice_add_trace_count(fail_slice, LineNoAndCount, ExecCounts0, ExecCounts) :-
    LineNoAndCount = line_no_and_count(_LineNumber, ExecCount, NumTests),
    ExecCounts0 = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec, FailTests),
    ExecCounts  = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec + ExecCount, FailTests + NumTests).

%---------------------------------------------------------------------------%

    % Values of this type uniquely identify a label in the program
    % and contain some statistics about the execution of the label.
    %
:- type slice_label_count
    --->    slice_label_count(
                slc_proc_label  :: proc_label,
                slc_path_port   :: path_port,
                slc_counts      :: slice_exec_count
            ).

:- pred slice_label_count_is_for_module(string::in, slice_label_count::in)
    is semidet.

slice_label_count_is_for_module(Module, slice_label_count(Label, _, _)) :-
    proc_label_is_for_module(Module, Label).

:- pred slice_label_count_is_zero(slice_label_count::in) is semidet.

slice_label_count_is_zero(SliceLabelCount) :-
    SliceLabelCount ^ slc_counts ^ slice_count > 0.

:- pred slice_label_count_compare(string::in,
    slice_label_count::in, slice_label_count::in,
    builtin.comparison_result::out) is det.

slice_label_count_compare(SortStr, LabelCountA, LabelCountB, Result) :-
    ( if SortStr = "" then
        LabelCountA = slice_label_count(ProcLabelA, PathPortA, CountsA),
        LabelCountB = slice_label_count(ProcLabelB, PathPortB, CountsB),
        builtin.compare(ProcLabelResult, ProcLabelA, ProcLabelB),
        (
            ProcLabelResult = (<),
            Result = (<)
        ;
            ProcLabelResult = (=),
            compare_path_ports(PathPortA, PathPortB, PathPortResult),
            (
                PathPortResult = (<),
                Result = (<)
            ;
                PathPortResult = (=),
                builtin.compare(Result, CountsA, CountsB)
            ;
                PathPortResult = (>),
                Result = (>)
            )
        ;
            ProcLabelResult = (>),
            Result = (>)
        )
    else
        slice_exec_count_compare(SortStr,
            LabelCountA ^ slc_counts, LabelCountB ^ slc_counts, Result)
    ).

:- pred compare_path_ports(path_port::in, path_port::in,
    builtin.comparison_result::out) is det.

compare_path_ports(PathPortA, PathPortB, Result) :-
    ( if
        % Handle the case where PathPortA and PathPortB have the same functor.
        (
            PathPortA = port_only(PortA),
            PathPortB = port_only(PortB),
            require_det (
                builtin.compare(ResultPrime, PortA, PortB)
            )
        ;
            PathPortA = path_only(RevPathA),
            PathPortB = path_only(RevPathB),
            require_det (
                rgp_to_fgp(RevPathA, PathA),
                rgp_to_fgp(RevPathB, PathB),
                builtin.compare(ResultPrime, PathA, PathB)
            )
        ;
            PathPortA = port_and_path(PortA, RevPathA),
            PathPortB = port_and_path(PortB, RevPathB),
            require_det (
                builtin.compare(PortResult, PortA, PortB),
                (
                    PortResult = (<),
                    ResultPrime = (<)
                ;
                    PortResult = (=),
                    rgp_to_fgp(RevPathA, PathA),
                    rgp_to_fgp(RevPathB, PathB),
                    builtin.compare(ResultPrime, PathA, PathB)
                ;
                    PortResult = (>),
                    ResultPrime = (>)
                )
            )
        )
    then
        Result = ResultPrime
    else
        % Handle the case where PathPortA and PathPortB have different
        % functors.
        builtin.compare(Result, PathPortA, PathPortB)
    ).

:- pred slice_exec_count_compare(string::in,
    slice_exec_count::in, slice_exec_count::in,
    builtin.comparison_result::out) is det.

slice_exec_count_compare(SortStr, ExecCount1, ExecCount2, Result) :-
    ( if string.first_char(SortStr, C, SortStrTail) then
        ( if C = 'c' then
            builtin.compare(Result0,
                ExecCount1 ^ slice_count, ExecCount2 ^ slice_count)
        else if C = 'C' then
            builtin.compare(Result0,
                ExecCount2 ^ slice_count, ExecCount1 ^ slice_count)
        else if C = 't' then
            builtin.compare(Result0,
                ExecCount1 ^ slice_tests, ExecCount2 ^ slice_tests)
        else if C = 'T' then
            builtin.compare(Result0,
                ExecCount2 ^ slice_tests, ExecCount1 ^ slice_tests)
        else
            unexpected($pred, "invalid sort string")
        ),
        ( if
            Result0 = (=),
            string.length(SortStrTail) > 0
        then
            slice_exec_count_compare(SortStrTail,
                ExecCount1, ExecCount2, Result)
        else
            Result = Result0
        )
    else
        unexpected($pred, "empty sort string")
    ).

:- pred slice_sort_string_is_valid(string::in) is semidet.

slice_sort_string_is_valid(Str0) :-
    Chrs0 = string.to_char_list(Str0),
    ( if Chrs0 = ['z' | ChrsPrime] then
        Chrs = ChrsPrime
    else
        Chrs = Chrs0
    ),
    ChrSet = set.list_to_set(Chrs),
    set.subset(ChrSet, set.list_to_set(['c', 'C', 't', 'T'])).

:- func slice_to_label_counts(slice_proc_map) = list(slice_label_count).

slice_to_label_counts(SliceProcMap) = LabelCounts :-
    map.foldl(append_slice_label_counts, SliceProcMap,
        cord.init, LabelCountsCord),
    LabelCounts = cord.list(LabelCountsCord).

:- pred append_slice_label_counts(proc_label::in, proc_slice::in,
    cord(slice_label_count)::in, cord(slice_label_count)::out) is det.

append_slice_label_counts(ProcLabel, ProcSlice, !LabelCounts) :-
    map.to_assoc_list(ProcSlice, ProcExecCounts),
    list.map(make_slice_label_count(ProcLabel),
        ProcExecCounts, NewLabelCounts),
    !:LabelCounts = !.LabelCounts ++ cord.from_list(NewLabelCounts).

:- pred make_slice_label_count(proc_label::in,
    pair(path_port, slice_exec_count)::in, slice_label_count::out) is det.

make_slice_label_count(ProcLabel, PathPort - ExecCount, SliceLabelCount) :-
    SliceLabelCount = slice_label_count(ProcLabel, PathPort, ExecCount).

    % Produce a formatted table from a list of slice_label_counts.
    %
:- func format_slice_label_counts(list(slice_label_count), int,
    maybe(int), maybe(int), maybe(int)) = string.

format_slice_label_counts(LabelCounts, TotalTests,
        MaybeMaxPredColumns, MaybePathColumns, MaybeMaxFileColumns) = Str :-
    list.map5(deconstruct_slice_label_count, LabelCounts, ProcLabels,
        PathPorts, FormattedContexts, Counts, Tests),
    FormattedProcLabels = list.map(format_proc_label, ProcLabels),
    FormattedPathPorts = list.map(format_path_port, PathPorts),
    CountStrs = list.map(string.int_to_string_thousands, Counts),
    AlwaysColumns = [
        left( ["Procedure" | FormattedProcLabels]) - MaybeMaxPredColumns,
        left( ["Path/Port" | FormattedPathPorts])  - MaybePathColumns,
        left( ["File:Line" | FormattedContexts])   - MaybeMaxFileColumns,
        right(["Count"     | CountStrs])           - no],
    filter(unify(1), Tests, _OneTests, OtherTests),
    (
        % All events were executed in one test. Don't include the redundant
        % column containing "(1)" at the end of each line.
        OtherTests = [],
        Columns = AlwaysColumns
    ;
        OtherTests = [_ | _],
        TestsStrs = list.map(bracket_int, Tests),
        TotalTestsStr = "(" ++ int_to_string_thousands(TotalTests) ++ ")",
        Columns = AlwaysColumns ++
            [right([TotalTestsStr | TestsStrs]) - no]
    ),
    Str = string.format_table_max(Columns, " ") ++ "\n".

:- pred deconstruct_slice_label_count(slice_label_count::in, proc_label::out,
    path_port::out, string::out, int::out, int::out) is det.

deconstruct_slice_label_count(SliceLabelCount, PathPort, ProcLabel,
        FormattedContext, Count, Tests) :-
    SliceLabelCount = slice_label_count(PathPort, ProcLabel, ExecCounts),
    ExecCounts = slice_exec_count(FileName, LineNumber, Count, Tests),
    FormattedContext = format_context(FileName, LineNumber).

%---------------------------------------------------------------------------%
%
% A mechanism for sorting and formatting dices. The structure is similar
% to the mechanism for sorting and formatting slices above.
%

:- pragma foreign_export("C",
    read_dice_to_string_no_limit(in, in, in, in, in, out, out, di, uo),
    "MR_MDBCOMP_read_dice_to_string").

:- pred read_dice_to_string_no_limit(string::in, string::in, string::in,
    int::in, string::in, string::out, string::out, io::di, io::uo) is det.

read_dice_to_string_no_limit(PassFile, FailFile, SortStr, MaxRow,
        Module, DiceStr, Problem, !IO) :-
    read_dice_to_string(PassFile, FailFile, SortStr, MaxRow, no, no, no,
        Module, DiceStr, Problem, !IO).

read_dice_to_string(PassFile, FailFile, SortStr, MaxRow,
        MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns,
        Module, DiceStr, Problem, !IO) :-
    ( if dice_sort_string_is_valid(SortStr) then
        read_dice(PassFile, FailFile, ReadDiceResult, !IO),
        (
            ReadDiceResult = ok(Dice),
            Dice = dice(TotalPassTests, TotalFailTests, DiceProcMap),
            LabelCounts = dice_to_label_counts(DiceProcMap),
            ( if Module = "" then
                FilteredLabelCounts = LabelCounts
            else
                list.filter(dice_label_count_is_for_module(Module),
                    LabelCounts, FilteredLabelCounts)
            ),
            list.sort(dice_label_count_compare(SortStr), FilteredLabelCounts,
                SortedLabelCounts),
            ( if list.take(MaxRow, SortedLabelCounts, Taken) then
                TopNLabelCounts = Taken
            else
                TopNLabelCounts = SortedLabelCounts
            ),
            Problem = "",
            DiceStr = format_dice_label_counts(TopNLabelCounts,
                TotalPassTests, TotalFailTests,
                MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns)
        ;
            ReadDiceResult = error(Problem),
            DiceStr = ""
        )
    else
        Problem = "Invalid sort string",
        DiceStr = ""
    ).

    % Values of this type uniquely identify a label in the program
    % and contain some statistics about the execution of the label.
    %
:- type dice_label_count
    --->    dice_label_count(
                dlc_proc_label  :: proc_label,
                dlc_path_port   :: path_port,
                dlc_counts      :: dice_exec_count
            ).

:- pred dice_label_count_is_for_module(string::in, dice_label_count::in)
    is semidet.

dice_label_count_is_for_module(Module, dice_label_count(Label, _, _)) :-
    proc_label_is_for_module(Module, Label).

:- pred dice_label_count_compare(string::in,
    dice_label_count::in, dice_label_count::in,
    builtin.comparison_result::out) is det.

dice_label_count_compare(SortStr, LabelCountA, LabelCountB, Result) :-
    ( if SortStr = "" then
        LabelCountA = dice_label_count(ProcLabelA, PathPortA, CountsA),
        LabelCountB = dice_label_count(ProcLabelB, PathPortB, CountsB),
        builtin.compare(ProcLabelResult, ProcLabelA, ProcLabelB),
        (
            ProcLabelResult = (<),
            Result = (<)
        ;
            ProcLabelResult = (=),
            compare_path_ports(PathPortA, PathPortB, PathPortResult),
            (
                PathPortResult = (<),
                Result = (<)
            ;
                PathPortResult = (=),
                builtin.compare(Result, CountsA, CountsB)
            ;
                PathPortResult = (>),
                Result = (>)
            )
        ;
            ProcLabelResult = (>),
            Result = (>)
        )
    else
        dice_exec_count_compare(SortStr,
            LabelCountA ^ dlc_counts, LabelCountB ^ dlc_counts, Result)
    ).

:- pred dice_exec_count_compare(string::in,
    dice_exec_count::in, dice_exec_count::in,
    builtin.comparison_result::out) is det.

dice_exec_count_compare(SortStr, ExecCount1, ExecCount2, Result) :-
    ( if string.first_char(SortStr, C, SortStrTail) then
        ( if C = 'p' then
            builtin.compare(Result0,
                ExecCount1 ^ pass_count, ExecCount2 ^ pass_count)
        else if C = 'P' then
            builtin.compare(Result0,
                ExecCount2 ^ pass_count, ExecCount1 ^ pass_count)
        else if C = 'f' then
            builtin.compare(Result0,
                ExecCount1 ^ fail_count, ExecCount2 ^ fail_count)
        else if C = 'F' then
            builtin.compare(Result0,
                ExecCount2 ^ fail_count, ExecCount1 ^ fail_count)
        else if C = 's' then
            builtin.compare(Result0,
                suspicion_ratio(ExecCount1 ^ pass_count,
                    ExecCount1 ^ fail_count),
                suspicion_ratio(ExecCount2 ^ pass_count,
                    ExecCount2 ^ fail_count))
        else if C = 'S' then
            builtin.compare(Result0,
                suspicion_ratio(ExecCount2 ^ pass_count,
                    ExecCount2 ^ fail_count),
                suspicion_ratio(ExecCount1 ^ pass_count,
                    ExecCount1 ^ fail_count))
        else if C = 'd' then
            % using - instead of int.minus is ambiguous.
            Diff1 = int.minus(ExecCount1 ^ pass_count,
                ExecCount1 ^ fail_count),
            Diff2 = int.minus(ExecCount2 ^ pass_count,
                ExecCount2 ^ fail_count),
            builtin.compare(Result0, Diff1, Diff2)
        else if C = 'D' then
            % using - instead of int.minus is ambiguous.
            Diff1 = int.minus(ExecCount1 ^ pass_count,
                ExecCount1 ^ fail_count),
            Diff2 = int.minus(ExecCount2 ^ pass_count,
                ExecCount2 ^ fail_count),
            builtin.compare(Result0, Diff2, Diff1)
        else
            unexpected($pred, "invalid sort string")
        ),
        ( if
            Result0 = (=),
            string.length(SortStrTail) > 0
        then
            dice_exec_count_compare(SortStrTail,
                ExecCount1, ExecCount2, Result)
        else
            Result = Result0
        )
    else
        unexpected($pred, "empty sort string")
    ).

:- pred dice_sort_string_is_valid(string::in) is semidet.

dice_sort_string_is_valid(Str) :-
    Chrs = string.to_char_list(Str),
    ChrSet = set.list_to_set(Chrs),
    set.subset(ChrSet,
        set.list_to_set(['p', 'P', 'f', 'F', 's', 'S', 'd', 'D'])).

:- func dice_to_label_counts(dice_proc_map) = list(dice_label_count).

dice_to_label_counts(DiceProcMap) = LabelCounts :-
    map.foldl(append_dice_label_counts, DiceProcMap, [], LabelCounts).

:- pred append_dice_label_counts(proc_label::in, proc_dice::in,
    list(dice_label_count)::in, list(dice_label_count)::out) is det.

append_dice_label_counts(ProcLabel, ProcDice, !LabelCounts) :-
    map.to_assoc_list(ProcDice, ProcExecCounts),
    list.map(make_dice_label_count(ProcLabel), ProcExecCounts, NewLabelCounts),
    append(!.LabelCounts, NewLabelCounts, !:LabelCounts).

:- pred make_dice_label_count(proc_label::in,
    pair(path_port, dice_exec_count)::in, dice_label_count::out) is det.

make_dice_label_count(ProcLabel, PathPort - ExecCount, DiceLabelCount) :-
    DiceLabelCount = dice_label_count(ProcLabel, PathPort, ExecCount).

    % Produce a formatted table from a list of dice_label_counts.
    %
:- func format_dice_label_counts(list(dice_label_count), int, int,
    maybe(int), maybe(int), maybe(int)) = string.

format_dice_label_counts(LabelCounts, TotalPassTests, _TotalFailTests,
        MaybeMaxPredColumns, MaybeMaxPathColumns, MaybeMaxFileColumns) = Str :-
    list.map7(deconstruct_dice_label_count, LabelCounts, ProcLabels,
        PathPorts, FormattedContexts, PassCounts, PassTests, FailCounts,
        _FailTests),
    FormattedProcLabels = list.map(format_proc_label, ProcLabels),
    FormattedPathPorts = list.map(format_path_port, PathPorts),
    PassCountStrs = list.map(string.int_to_string_thousands, PassCounts),
    PassTestsStrs = list.map(bracket_int, PassTests),
    FailCountStrs = list.map(string.int_to_string_thousands, FailCounts),
    SuspicionIndices = list.map_corresponding(suspicion_ratio,
        PassCounts, FailCounts),
    FormattedSuspicionIndices = list.map(format_float(2), SuspicionIndices),
    TotalPassTestsStr = "(" ++ int_to_string_thousands(TotalPassTests) ++ ")",
    Columns = [
        left( ["Procedure"       | FormattedProcLabels]) - MaybeMaxPredColumns,
        left( ["Path/Port"       | FormattedPathPorts])  - MaybeMaxPathColumns,
        left( ["File:Line"       | FormattedContexts])   - MaybeMaxFileColumns,
        right(["Pass"            | PassCountStrs])       - no,
        right([TotalPassTestsStr | PassTestsStrs])       - no,
        right(["Fail"            | FailCountStrs])       - no,
        right(["Suspicion"       | FormattedSuspicionIndices]) - no],
    Str = string.format_table_max(Columns, " ") ++ "\n".

:- pred deconstruct_dice_label_count(dice_label_count::in, proc_label::out,
    path_port::out, string::out, int::out, int::out, int::out, int::out)
    is det.

deconstruct_dice_label_count(DiceLabelCount, ProcLabel, PathPort,
        FormattedContext, PassCount, PassTests, FailCount, FailTests) :-
    DiceLabelCount = dice_label_count(ProcLabel, PathPort, ExecCounts),
    ExecCounts = dice_exec_count(FileName, LineNumber, PassCount, PassTests,
        FailCount, FailTests),
    FormattedContext = format_context(FileName, LineNumber).

%---------------------------------------------------------------------------%

suspicion_ratio(PassCount, FailCount) = R1 :-
    Denominator = PassCount + FailCount,
    ( if Denominator = 0 then
        % The denominator could be zero if user_all trace counts were provided.
        R1 = 0.0
    else
        R = float(FailCount) / float(Denominator),
        % The original threshold here was 0.2. The new value is 3/16, which is
        % exactly representable in binary. This avoids differences in rounding
        % between 32 and 64 bit platforms, which can show up as differences
        % between the stage 2 and 3 versions of the code we generate
        % for this module during a bootcheck in the C# and Java grades.
        ( if R >= 0.1875 then
            R1 = R
        else
            R1 = 0.0
        )
    ).

suspicion_ratio_normalised(PassCount, PassTests, FailCount, FailTests) = R :-
    ( if FailCount = 0 then
        R = 0.0
    else
        ( if PassTests = 0 then
            PassNorm = 0.0
        else
            PassNorm = float(PassCount) / float(PassTests)
        ),
        FailNorm = float(FailCount) / float(FailTests),
        R = float.max(FailNorm - PassNorm, 0.0) / FailNorm
    ).

suspicion_ratio_binary(PassCount, FailCount) = R :-
    ( if FailCount > 0, PassCount = 0 then
        R = 1.0
    else
        R = 0.0
    ).

:- func get_suspicion_for_label_layout(dice, label_layout) = float.

:- pragma foreign_export("C",
    get_suspicion_for_label_layout(in, in) = out,
    "MR_MDBCOMP_get_suspicion_for_label_layout").

get_suspicion_for_label_layout(Dice, LabelLayout) = Suspicion :-
    ProcLayout = get_proc_layout_from_label_layout(LabelLayout),
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    PathPort = get_path_port_from_label_layout(LabelLayout),
    ( if map.search(Dice ^ dice_proc_map, ProcLabel, PathPortMap) then
        ( if map.search(PathPortMap, PathPort, ExecCount) then
            Suspicion = suspicion_ratio_binary(
                  ExecCount ^ pass_count, ExecCount ^ fail_count)
        else
            Suspicion = 0.0
        )
    else
        Suspicion = 0.0
    ).

%---------------------------------------------------------------------------%
%
% Generic predicates useful for both slices and dices.
%

:- func bracket_int(int) = string.

bracket_int(X) = "(" ++ string.int_to_string_thousands(X) ++ ")".

:- func format_float(int, float) = string.

format_float(DecimalPlaces, Flt) =
    string.format("%.*f", [i(DecimalPlaces), f(Flt)]).

:- pred proc_label_is_for_module(string::in, proc_label::in) is semidet.

proc_label_is_for_module(Module, ProcLabel) :-
    (
        ProcLabel = ordinary_proc_label(_, _, ProcSymModule, _, _, _)
    ;
        ProcLabel = special_proc_label(_, _, ProcSymModule, _, _, _)
    ),
    SymModule = string_to_sym_name(Module),
    is_submodule(ProcSymModule, SymModule).

:- func format_proc_label(proc_label) = string.

format_proc_label(ProcLabel) = Str :-
    (
        ProcLabel = ordinary_proc_label(_, PredOrFunc, SymModule, Name, Arity0,
            ModeNum),
        Module = sym_name_to_string(SymModule),
        (
            PredOrFunc = pf_function,
            PredOrFuncStr = "func",
            Arity = Arity0 - 1
        ;
            PredOrFunc = pf_predicate,
            PredOrFuncStr = "pred",
            Arity = Arity0
        ),
        Str = string.format("%s %s.%s/%d-%d",
            [s(PredOrFuncStr), s(Module), s(Name), i(Arity), i(ModeNum)])
    ;
        ProcLabel = special_proc_label(_, SpecialPredId, SymModule, TypeName,
            TypeArity, ModeNum),
        Module = sym_name_to_string(SymModule),
        special_pred_name_arity(SpecialPredId, OpName, _, _OpArity),
        % XXX We used to print the arity of the operation here, but that
        % is extremely non-informative, always being 2 for unify and index
        % operations, and 3 for compares.
        %
        % Str = string.format("%s for %s.%s/%d",
        %     [s(OpName), s(Module), s(TypeName), i(OpArity)])
        %
        % We now instead print two pieces of information the user
        % may actually need: the arity of the type constructor
        % (since knowing that e.g. the compare predicate for t/2
        % is called more often than the compare predicate for t/1
        % may be useful), and the mode number (again, because
        % differences in trace counts for different modes
        % may help indicate hotspots).
        Str = string.format("%s for %s.%s/%d-%d",
            [s(OpName), s(Module), s(TypeName), i(TypeArity), i(ModeNum)])
    ).

:- func format_path_port(path_port) = string.

format_path_port(port_only(Port)) = Str :-
    string_to_trace_port(Str, Port).
format_path_port(path_only(Path)) =
    "<" ++ rev_goal_path_to_string(Path) ++ ">".
format_path_port(port_and_path(Port, Path)) =
    format_path_port(port_only(Port)) ++ " " ++
        format_path_port(path_only(Path)).

:- func format_context(string, int) = string.

format_context(FileName, LineNumber) = Str :-
    Str = FileName ++ ":" ++ int_to_string(LineNumber).

%---------------------------------------------------------------------------%
