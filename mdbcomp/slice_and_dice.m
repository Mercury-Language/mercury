%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dice.m
% Authors: Ian MacLarty and Zoltan Somogyi
%
% This module contains code for generating and manipulating slices and dices.
% A dice is the difference between one or more passing test runs
% and one (or more, but that is not yet implemented) failing test runs.

:- module mdbcomp.slice_and_dice.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.trace_counts.

:- import_module io.
:- import_module map.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type slice --->
    slice(
        num_tests       :: int,
        slice_proc_map  :: map(proc_label, proc_slice)
    ).

:- type slice_proc_map   == map(proc_label, proc_slice).

:- type proc_slice       == map(path_port, slice_exec_count).

:- type slice_exec_count --->
    slice_exec_count(
            slice_filename      ::  string,
            slice_linenumber    ::  int,

            slice_count         :: int,
                                % The number of times the label was executed in
                                % all the test runs.

            slice_tests         :: int
                                % The number of test runs the label was
                                % executed in.
    ).

    % read_slice(Source, File, MaybeSlice, !IO):
    %
    % Read the slice(s) from Source and File.
    %
:- pred read_slice(slice_source::in, string::in,
    maybe_error(pair(set(trace_count_file_type), slice))::out,
    io::di, io::uo) is det.

    % read_slice_to_string(File, SortStr, N, Module, SliceStr, Problem, !IO):
    %
    % Read the slice(s) from try_single_first and File, and convert it
    % to a string suitable for displaying on the screen, sorting it first
    % using SortStr. SortStr can be any combination of the letters "cCtT"
    % and indicates how the dice is to be sorted. See the documentation
    % for the `slice' command in the user guide for an explanation of
    % the sort string. Take only the top N lines of the sorted list.
    %
    % If Module is not the empty string then only labels from the named
    % module will be included in the dice string, otherwise all modules
    % will be included.
    %
    % If there was a problem reading the trace counts then Problem will
    % contain a string describing the problem encountered and SliceStr
    % will be the empty string, otherwise Problem will be the empty string.
    %
:- pred read_slice_to_string(string::in, string::in, int::in,
    string::in, string::out, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type dice --->
    dice(
        num_pass_tests  :: int,
        num_fail_tests  :: int,
        dice_proc_map   :: map(proc_label, proc_dice)
    ).

:- type dice_proc_map   == map(proc_label, proc_dice).

:- type proc_dice       == map(path_port, dice_exec_count).

:- type dice_exec_count --->
    dice_exec_count(
            dice_filename   ::  string,
            dice_linenumber ::  int,

            pass_count      :: int,
                            % The number of times the label was executed in
                            % all the passing test runs.

            pass_tests      :: int,
                            % The number of passing test runs the label
                            % was executed in.

            fail_count      :: int,
                            % The number of times the label was executed in
                            % failing test runs.

            fail_tests      :: int
                            % The number of failing test runs the label
                            % was executed in.
    ).

    % read_dice(PassSource, PassFile, FailSource, FailFile, MaybeDice, !IO):
    %
    % Read the slice(s) from PassSource and PassFile, interpreting them as
    % passing slices; read the slice(s) from FailSource and FailFile,
    % interpreting them as failing slices; then produce the dice you get
    % from them.
    % 
:- pred read_dice(slice_source::in, string::in, slice_source::in, string::in,
    maybe_error(dice)::out, io::di, io::uo) is det.

    % read_dice_to_string(PassFile, FailFile, SortStr, N, Module, DiceStr,
    %   Problem, !IO):
    %
    % Read the slice(s) from try_single_first and PassFile, interpreting them
    % as passing slices; read the slice(s) from try_single_first and FailFile,
    % interpreting them as failing slices; then produce the dice you get
    % from them.
    %
    % Then convert the dice to a string suitable for displaying on the screen,
    % sorting it first using SortStr. SortStr can be any combination of the
    % letters "sSpPfPdD" and indicates how the dice is to be sorted.
    % See the documentation for the `dice' command in the user guide
    % for an explanation of the sort string. Take only the top N lines
    % of the sorted list.
    %
    % If Module is not the empty string then only labels from the named
    % module will be included in the dice string, otherwise all modules
    % will be included.
    %
    % If there was a problem reading the trace counts then Problem will
    % contain a string describing the problem encountered and DiceStr
    % will be the empty string, otherwise Problem will be the empty string.
    %
:- pred read_dice_to_string(string::in, string::in, string::in, int::in,
    string::in, string::out, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.program_representation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%
%
% The mechanism for reading in slices. The structure is similar to the
% mechanism for reading in dices below.

read_slice(Source, File, Result, !IO) :-
    read_trace_counts_source(Source, File, ReadResult, !IO),
    (
        ReadResult = list_ok(AssocList),
        assoc_list__values(AssocList, TraceCounts),
        assoc_list__keys(AssocList, FileTypeList),
        set__list_to_set(FileTypeList, FileTypes),
        list.foldl(slice_merge_trace_counts, TraceCounts,
            map.init, SliceProcMap),
        TotalTests = length(TraceCounts),
        Slice = slice(TotalTests, SliceProcMap),
        Result = ok(FileTypes - Slice)
    ;
        ReadResult = list_error_message(Problem),
        Result = error(Problem)
    ).

    % Add the trace_counts to the given slice.
    %
:- pred slice_merge_trace_counts(trace_counts::in,
    slice_proc_map::in, slice_proc_map::out) is det.

slice_merge_trace_counts(TraceCounts, !SliceProcMap) :-
    map.foldl(slice_merge_proc_trace_counts, TraceCounts, !SliceProcMap).

:- pred slice_merge_proc_trace_counts(proc_label::in, proc_trace_counts::in,
    slice_proc_map::in, slice_proc_map::out) is det.

slice_merge_proc_trace_counts(ProcLabel, ProcTraceCounts, !SliceProcMap) :-
    ( map.search(!.SliceProcMap, ProcLabel, FoundProcSlice) ->
        map.foldl(slice_merge_path_port, ProcTraceCounts,
            FoundProcSlice, MergedProcSlice),
        svmap.det_update(ProcLabel, MergedProcSlice, !SliceProcMap)
    ;
        map.foldl(slice_merge_path_port, ProcTraceCounts,
            map.init, MergedProcSlice),
        svmap.det_insert(ProcLabel, MergedProcSlice, !SliceProcMap)
    ).

:- pred slice_merge_path_port(path_port::in, context_and_count::in,
    proc_slice::in, proc_slice::out) is det.

slice_merge_path_port(PathPort, ContextCount, !ProcSlice) :-
    (
        map.transform_value(slice_add_trace_count(ContextCount), PathPort,
            !.ProcSlice, UpdatedProcSlice)
    ->
        !:ProcSlice = UpdatedProcSlice
    ;
        ContextCount = context_and_count(FileName, LineNumber, Count),
        ExecCount = slice_exec_count(FileName, LineNumber, Count, 1),
        svmap.det_insert(PathPort, ExecCount, !ProcSlice)
    ).

:- pred slice_add_trace_count(context_and_count::in,
    slice_exec_count::in, slice_exec_count::out) is det.

slice_add_trace_count(ContextCount, ExecCounts0, ExecCounts) :-
    ContextCount = context_and_count(_FileName, _LineNumber, Count),
    ExecCounts0 = slice_exec_count(FileName, LineNumber, Exec, Tests),
    ExecCounts = slice_exec_count(FileName, LineNumber, Exec + Count,
        Tests + 1).

%-----------------------------------------------------------------------------%
%
% The mechanism for reading in dices. The structure is similar to the
% mechanism for reading in slices above.

read_dice(PassSource, PassFile, FailSource, FailFile, Result, !IO) :-
    read_trace_counts_source(PassSource, PassFile, ReadPassResult, !IO),
    (
        ReadPassResult = list_ok(PassAssocList),
        assoc_list__values(PassAssocList, PassTraceCountsList),
        read_trace_counts_source(FailSource, FailFile, ReadFailResult, !IO),
        (
            ReadFailResult = list_ok(FailAssocList),
            assoc_list__values(FailAssocList, FailTraceCountsList),
            list.foldl(dice_merge_trace_counts(pass), PassTraceCountsList,
                map.init, PassDiceProcMap),
            list.foldl(dice_merge_trace_counts(fail), FailTraceCountsList,
                PassDiceProcMap, DiceProcMap),
            TotalPassTests = length(PassTraceCountsList),
            TotalFailTests = length(FailTraceCountsList),
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

:- type trace_counts_kind
    --->    pass
    ;       fail.

    % Merge the passing or failing trace_counts into the given dice.
    %
:- pred dice_merge_trace_counts(trace_counts_kind::in, trace_counts::in,
    dice_proc_map::in, dice_proc_map::out) is det.

dice_merge_trace_counts(Kind, TraceCounts, !DiceProcMap) :-
    map.foldl(dice_merge_proc_trace_counts(Kind), TraceCounts, !DiceProcMap).

:- pred dice_merge_proc_trace_counts(trace_counts_kind::in, proc_label::in,
    proc_trace_counts::in, dice_proc_map::in, dice_proc_map::out) is det.

dice_merge_proc_trace_counts(Kind, ProcLabel, ProcTraceCounts, !DiceProcMap) :-
    ( map.search(!.DiceProcMap, ProcLabel, FoundProcDice) ->
        map.foldl(dice_merge_path_port(Kind), ProcTraceCounts, FoundProcDice,
            MergedProcDice),
        svmap.det_update(ProcLabel, MergedProcDice, !DiceProcMap)
    ;
        map.foldl(dice_merge_path_port(Kind), ProcTraceCounts,
            map.init, MergedProcDice),
        svmap.det_insert(ProcLabel, MergedProcDice, !DiceProcMap)
    ).

:- pred dice_merge_path_port(trace_counts_kind::in, path_port::in,
    context_and_count::in, proc_dice::in, proc_dice::out) is det.

dice_merge_path_port(Kind, PathPort, ContextCount, !ProcDice) :-
    (
        map.transform_value(dice_add_trace_count(Kind, ContextCount), PathPort,
            !.ProcDice, UpdatedProcDice)
    ->
        !:ProcDice = UpdatedProcDice
    ;
        ContextCount = context_and_count(FileName, LineNumber, Count),
        (
            Kind = pass,
            InitCount = dice_exec_count(FileName, LineNumber, Count, 1, 0, 0)
        ;
            Kind = fail,
            InitCount = dice_exec_count(FileName, LineNumber, 0, 0, Count, 1)
        ),
        svmap.det_insert(PathPort, InitCount, !ProcDice)
    ).

:- pred dice_add_trace_count(trace_counts_kind::in, context_and_count::in,
    dice_exec_count::in, dice_exec_count::out) is det.

dice_add_trace_count(pass, ContextCount, ExecCounts0, ExecCounts) :-
    ContextCount = context_and_count(_FileName, _LineNumber, Count),
    ExecCounts0 = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec, FailTests),
    ExecCounts  = dice_exec_count(FileName, LineNumber,
        PassExec + Count, PassTests + 1, FailExec, FailTests).
dice_add_trace_count(fail, ContextCount, ExecCounts0, ExecCounts) :-
    ContextCount = context_and_count(_FileName, _LineNumber, Count),
    ExecCounts0 = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec, FailTests),
    ExecCounts  = dice_exec_count(FileName, LineNumber,
        PassExec, PassTests, FailExec + Count, FailTests + 1).

%-----------------------------------------------------------------------------%
%
% A mechanism for sorting and formatting slices. The structure is similar
% to the mechanism for sorting and formatting dices below.

:- pragma export(read_slice_to_string(in, in, in, in, out, out, di, uo),
    "MR_MDB_read_slice_to_string").

read_slice_to_string(File, SortStr0, N, Module, SliceStr, Problem, !IO) :-
    ( slice_sort_string_is_valid(SortStr0) ->
        read_slice(try_single_first, File, ReadSliceResult, !IO),
        (
            ReadSliceResult = ok(_ - Slice),
            Slice = slice(TotalTests, SliceProcMap),
            LabelCounts = slice_to_label_counts(SliceProcMap),
            ( Module \= "" ->
                list.filter(slice_label_count_is_for_module(Module),
                    LabelCounts, ModuleFilteredLabelCounts)
            ;
                ModuleFilteredLabelCounts = LabelCounts
            ),
            ( string__append("z", SortStrPrime, SortStr0) ->
                SortStr = SortStrPrime,
                list.filter(slice_label_count_is_zero,
                    ModuleFilteredLabelCounts, FilteredLabelCounts)
            ;
                SortStr = SortStr0,
                FilteredLabelCounts = ModuleFilteredLabelCounts
            ),
            list.sort(slice_label_count_compare(SortStr), FilteredLabelCounts,
                SortedLabelCounts),
            ( list.take(N, SortedLabelCounts, Taken) ->
                TopNLabelCounts = Taken
            ;
                TopNLabelCounts = SortedLabelCounts
            ),
            Problem = "",
            SliceStr = format_slice_label_counts(TopNLabelCounts, TotalTests)
        ;
            ReadSliceResult = error(Problem),
            SliceStr = ""
        )
    ;
        Problem = "Invalid sort string",
        SliceStr = ""
    ).

    % Values of this type uniquely identify a label in the program
    % and contain some statistics about the execution of the label.
    %
:- type slice_label_count --->
    slice_label_count(
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

slice_label_count_compare(SortStr, LabelCount1, LabelCount2, Result) :-
    ( SortStr = "" ->
        builtin.compare(Result, LabelCount1, LabelCount2)
    ;
        slice_exec_count_compare(SortStr,
            LabelCount1 ^ slc_counts, LabelCount2 ^ slc_counts, Result)
    ).

:- pred slice_exec_count_compare(string::in,
    slice_exec_count::in, slice_exec_count::in,
    builtin.comparison_result::out) is det.

slice_exec_count_compare(SortStr, ExecCount1, ExecCount2, Result) :-
    (
        string.first_char(SortStr, C, Rest)
    ->
        ( C = 'c' ->
            builtin.compare(Result0, ExecCount1 ^ slice_count,
                ExecCount2 ^ slice_count)
        ; C = 'C' ->
            builtin.compare(Result0, ExecCount2 ^ slice_count,
                ExecCount1 ^ slice_count)
        ; C = 't' ->
            builtin.compare(Result0, ExecCount1 ^ slice_tests,
                ExecCount2 ^ slice_tests)
        ; C = 'T' ->
            builtin.compare(Result0, ExecCount2 ^ slice_tests,
                ExecCount1 ^ slice_tests)
        ;
            error("slice_exec_count_compare: invalid sort string")
        ),
        (
            Result0 = (=),
            string.length(Rest) > 0
        ->
            slice_exec_count_compare(Rest, ExecCount1, ExecCount2, Result)
        ;
            Result = Result0
        )
    ;
        error("slice_exec_count_compare: empty sort string")
    ).

:- pred slice_sort_string_is_valid(string::in) is semidet.

slice_sort_string_is_valid(Str0) :-
    Chrs0 = string.to_char_list(Str0),
    ( Chrs0 = ['z' | ChrsPrime] ->
        Chrs = ChrsPrime
    ;
        Chrs = Chrs0
    ),
    ChrSet = set.list_to_set(Chrs),
    set.subset(ChrSet, set.list_to_set(['c', 'C', 't', 'T'])).

:- func slice_to_label_counts(slice_proc_map) = list(slice_label_count).

slice_to_label_counts(SliceProcMap) = LabelCounts :-
    map.foldl(append_slice_label_counts, SliceProcMap, [], LabelCounts).

:- pred append_slice_label_counts(proc_label::in, proc_slice::in,
    list(slice_label_count)::in, list(slice_label_count)::out) is det.

append_slice_label_counts(ProcLabel, ProcSlice, !LabelCounts) :-
    map.to_assoc_list(ProcSlice, ProcExecCounts),
    list.map(make_slice_label_count(ProcLabel), ProcExecCounts,
        NewLabelCounts),
    append(!.LabelCounts, NewLabelCounts, !:LabelCounts).

:- pred make_slice_label_count(proc_label::in,
    pair(path_port, slice_exec_count)::in, slice_label_count::out) is det.

make_slice_label_count(ProcLabel, PathPort - ExecCount, SliceLabelCount) :-
    SliceLabelCount = slice_label_count(ProcLabel, PathPort, ExecCount).

    % Produce a formatted table from a list of slice_label_counts.
    %
:- func format_slice_label_counts(list(slice_label_count), int) = string.

format_slice_label_counts(LabelCounts, TotalTests) = Str :-
    list.map5(deconstruct_slice_label_count, LabelCounts, ProcLabels,
        PathPorts, FormattedContexts, Counts, Tests),
    FormattedProcLabels = list.map(format_proc_label, ProcLabels),
    FormattedPathPorts = list.map(format_path_port, PathPorts),
    CountStrs = list.map(string.int_to_string_thousands, Counts),
    TestsStrs = list.map(bracket_int, Tests),
    TotalTestsStr = "(" ++ int_to_string_thousands(TotalTests) ++ ")",
    Str = string.format_table([
        left( ["Procedure"       | FormattedProcLabels]),
        left( ["Path/Port"       | FormattedPathPorts]),
        left( ["File:Line"       | FormattedContexts]),
        right(["Count"           | CountStrs]),
        right([TotalTestsStr     | TestsStrs])], " ") ++ "\n".

:- pred deconstruct_slice_label_count(slice_label_count::in, proc_label::out,
    path_port::out, string::out, int::out, int::out) is det.

deconstruct_slice_label_count(SliceLabelCount, PathPort, ProcLabel,
        FormattedContext, Count, Tests) :-
    SliceLabelCount = slice_label_count(PathPort, ProcLabel, ExecCounts),
    ExecCounts = slice_exec_count(FileName, LineNumber, Count, Tests),
    FormattedContext = format_context(FileName, LineNumber).

:- func format_slice_exec_count(slice_exec_count) = string.

format_slice_exec_count(slice_exec_count(_, _, Count, Tests)) =
    string.pad_left(int_to_string(Count), ' ', 12)
    ++ string.pad_left("(" ++ int_to_string(Tests) ++ ")", ' ', 8).

%-----------------------------------------------------------------------------%
%
% A mechanism for sorting and formatting dices. The structure is similar
% to the mechanism for sorting and formatting slices above.

:- pragma export(read_dice_to_string(in, in, in, in, in, out, out, di, uo),
    "MR_MDB_read_dice_to_string").

read_dice_to_string(PassFile, FailFile, SortStr, N, Module, DiceStr, Problem,
        !IO) :-
    ( dice_sort_string_is_valid(SortStr) ->
        read_dice(try_single_first, PassFile, try_single_first, FailFile,
            ReadDiceResult, !IO),
        (
            ReadDiceResult = ok(Dice),
            Dice = dice(TotalPassTests, TotalFailTests, DiceProcMap),
            LabelCounts = dice_to_label_counts(DiceProcMap),
            ( Module \= "" ->
                list.filter(dice_label_count_is_for_module(Module),
                    LabelCounts, FilteredLabelCounts)
            ;
                FilteredLabelCounts = LabelCounts
            ),
            list.sort(dice_label_count_compare(SortStr), FilteredLabelCounts,
                SortedLabelCounts),
            ( list.take(N, SortedLabelCounts, Taken) ->
                TopNLabelCounts = Taken
            ;
                TopNLabelCounts = SortedLabelCounts
            ),
            Problem = "",
            DiceStr = format_dice_label_counts(TopNLabelCounts,
                TotalPassTests, TotalFailTests)
        ;
            ReadDiceResult = error(Problem),
            DiceStr = ""
        )
    ;
        Problem = "Invalid sort string",
        DiceStr = ""
    ).

    % Values of this type uniquely identify a label in the program
    % and contain some statistics about the execution of the label.
    %
:- type dice_label_count --->
    dice_label_count(
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

dice_label_count_compare(SortStr, LabelCount1, LabelCount2, Result) :-
    ( SortStr = "" ->
        builtin.compare(Result, LabelCount1, LabelCount2)
    ;
        dice_exec_count_compare(SortStr,
            LabelCount1 ^ dlc_counts, LabelCount2 ^ dlc_counts, Result)
    ).

:- pred dice_exec_count_compare(string::in,
    dice_exec_count::in, dice_exec_count::in,
    builtin.comparison_result::out) is det.

dice_exec_count_compare(SortStr, ExecCount1, ExecCount2, Result) :-
    (
        string.first_char(SortStr, C, Rest)
    ->
        ( C = 'p' ->
            builtin.compare(Result0, ExecCount1 ^ pass_count,
                ExecCount2 ^ pass_count)
        ; C = 'P' ->
            builtin.compare(Result0, ExecCount2 ^ pass_count,
                ExecCount1 ^ pass_count)
        ; C = 'f' ->
            builtin.compare(Result0, ExecCount1 ^ fail_count,
                ExecCount2 ^ fail_count)
        ; C = 'F' ->
            builtin.compare(Result0, ExecCount2 ^ fail_count,
                ExecCount1 ^ fail_count)
        ; C = 's' ->
            builtin.compare(Result0,
                suspicion_ratio(ExecCount1 ^ pass_count,
                    ExecCount1 ^ fail_count),
                suspicion_ratio(ExecCount2 ^ pass_count,
                    ExecCount2 ^ fail_count))
        ; C = 'S' ->
            builtin.compare(Result0,
                suspicion_ratio(ExecCount2 ^ pass_count,
                    ExecCount2 ^ fail_count),
                suspicion_ratio(ExecCount1 ^ pass_count,
                    ExecCount1 ^ fail_count))
        ; C = 'd' ->
            % using - instead of int__minus is ambiguous
            Diff1 = int__minus(ExecCount1 ^ pass_count,
                ExecCount1 ^ fail_count),
            Diff2 = int__minus(ExecCount2 ^ pass_count,
                ExecCount2 ^ fail_count),
            builtin.compare(Result0, Diff1, Diff2)
        ; C = 'D' ->
            % using - instead of int__minus is ambiguous
            Diff1 = int__minus(ExecCount1 ^ pass_count,
                ExecCount1 ^ fail_count),
            Diff2 = int__minus(ExecCount2 ^ pass_count,
                ExecCount2 ^ fail_count),
            builtin.compare(Result0, Diff2, Diff1)
        ;
            error("dice_exec_count_compare: invalid sort string")
        ),
        (
            Result0 = (=),
            string.length(Rest) > 0
        ->
            dice_exec_count_compare(Rest, ExecCount1, ExecCount2, Result)
        ;
            Result = Result0
        )
    ;
        error("dice_exec_count_compare: empty sort string")
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
:- func format_dice_label_counts(list(dice_label_count), int, int) = string.

format_dice_label_counts(LabelCounts, TotalPassTests, _TotalFailTests) = Str :-
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
    Str = string.format_table([
        left( ["Procedure"       | FormattedProcLabels]),
        left( ["Path/Port"       | FormattedPathPorts]),
        left( ["File:Line"       | FormattedContexts]),
        right(["Pass"            | PassCountStrs]),
        right([TotalPassTestsStr | PassTestsStrs]),
        right(["Fail"            | FailCountStrs]),
        right(["Suspicion"       | FormattedSuspicionIndices])], " ") ++ "\n".

:- pred deconstruct_dice_label_count(dice_label_count::in, proc_label::out,
    path_port::out, string::out, int::out, int::out, int::out, int::out)
    is det.

deconstruct_dice_label_count(DiceLabelCount, ProcLabel, PathPort,
        FormattedContext, PassCount, PassTests, FailCount, FailTests) :-
    DiceLabelCount = dice_label_count(ProcLabel, PathPort, ExecCounts),
    ExecCounts = dice_exec_count(FileName, LineNumber, PassCount, PassTests,
        FailCount, FailTests),
    FormattedContext = format_context(FileName, LineNumber).

:- func format_dice_exec_count(dice_exec_count) = string.

format_dice_exec_count(dice_exec_count(_, _, PassCount, PassTests,
        FailCount, FailTests)) =
    string.pad_left(int_to_string(PassCount), ' ', 12)
    ++ string.pad_left("(" ++ int_to_string(PassTests) ++ ")", ' ', 8)
    ++ string.pad_left(int_to_string(FailCount), ' ', 12)
    ++ string.pad_left("(" ++ int_to_string(FailTests) ++ ")", ' ', 8).

    % suspicion_ratio gives an indication of how likely a label is to
    % be buggy based on how many times it was executed in passing and
    % failing test runs.
    %
:- func suspicion_ratio(int, int) = float.

suspicion_ratio(PassCount, FailCount) =
    % PassCount + FailCount should never be zero since if a label
    % isn't executed in any tests then it wouldn't be included in the dice.
    float(FailCount) / float(PassCount + FailCount).

%-----------------------------------------------------------------------------%
%
% Generic predicates useful for both slices and dices.

:- func bracket_int(int) = string.

bracket_int(X) = "(" ++ string.int_to_string_thousands(X) ++ ")".

:- func format_float(int, float) = string.

format_float(DecimalPlaces, Flt) =
    string.format("%.*f", [i(DecimalPlaces), f(Flt)]). 

:- pred proc_label_is_for_module(string::in, proc_label::in) is semidet.

proc_label_is_for_module(Module, ProcLabel) :-
    (
        ProcLabel = proc(_, _, ProcSymModule, _, _, _)
    ;
        ProcLabel = special_proc(_, _, ProcSymModule, _, _, _)
    ),
    string_to_sym_name(Module, ".", SymModule),
    is_submodule(ProcSymModule, SymModule).

:- func format_proc_label(proc_label) = string.

format_proc_label(ProcLabel) = Str :-
    (
        ProcLabel = proc(_, PredOrFunc, SymModule, Name, Arity, ModeNo),
        Module = sym_name_to_string(SymModule),
        (
            PredOrFunc = function,
            ArityStr = int_to_string(Arity - 1),
            PredOrFuncStr = "func"
        ;
            PredOrFunc = predicate,
            ArityStr = int_to_string(Arity),
            PredOrFuncStr = "pred"
        ),
        Str = PredOrFuncStr ++ " " ++ Module ++ "." ++ Name ++
            "/" ++ ArityStr ++ "-" ++ int_to_string(ModeNo)
    ;
        ProcLabel = special_proc(_, SpecialPredId, SymModule, TypeName,
            _, _),
        Module = sym_name_to_string(SymModule),
        special_pred_name_arity(SpecialPredId, Name, Arity),
        Str = Name ++ " for " ++ Module ++ "." ++ TypeName ++ "/" ++
            int_to_string(Arity)
    ).

:- func format_path_port(path_port) = string.

format_path_port(port_only(Port)) = Str :-
    mdbcomp.trace_counts.string_to_trace_port(Str, Port).
format_path_port(path_only(Path)) = Str :-
    mdbcomp.program_representation.string_from_path(Path, PathStr),
    Str = "<" ++ PathStr ++ ">".
format_path_port(port_and_path(Port, Path)) =
    format_path_port(port_only(Port)) ++ " " ++
        format_path_port(path_only(Path)).

:- func format_context(string, int) = string.

format_context(FileName, LineNumber) =
        FileName ++ ":" ++ int_to_string(LineNumber).
