%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: dice.m
% Author: Ian MacLarty
%
% This module contains code for generating and manipulating dices.  A dice
% is the difference between one or more passing test runs and one or more
% failing test runs.
%

:- module mdb.dice.

:- interface.

:- import_module io.
:- import_module map.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.trace_counts.

:- type dice == map(proc_label, proc_dice).

:- type proc_dice == map(path_port, exec_count).

:- type exec_count
	--->	exec_count(
				% The number of times the label was executed in
				% all the passing test runs.
			pass_count	:: int,
				% The number of passing test runs the label
				% was executed in.
			pass_tests	:: int,
				% The number of times the label was executed in
				% failing test runs.
			fail_count	:: int,
				% The number of failing test runs the label
				% was executed in.
			fail_tests	:: int
	).

	% Merge the passing or failing trace_counts into the given dice.
	%
:- pred merge_trace_counts(trace_counts_kind::in, trace_counts::in,
	dice::in, dice::out) is det.

	% Read the trace_counts in the files whose names appear in FileName.
	%
:- pred read_trace_counts_list(string::in, read_trace_counts_list_result::out,
	io::di, io::uo) is det.

	% read_dice_to_string(PassFiles, FailFile, SortStr, N, DiceStr, 
	% 	Problem, !IO).
	% Read the trace_counts in the list of files in the file named
	% PassFiles, interpreting them as passing slices; read the
	% trace_counts from the file FailFile, interpreting it as a failing
	% slice; then produce a dice and convert the dice to a string suitable
	% for displaying on the screen, sorting it first using SortStr.
	% SortStr can be any combination of the letters "sSpPfP" and 
	% indicates how the dice is to be sorted.  See the documentation
	% for the `dice' command in the user guide for an explaination of the
	% sort string.  If there was a problem reading the trace_counts then
	% Problem will contain a string describing the problem encountered and
	% DiceStr will be the empty string, otherwise Problem will be the
	% empty string.
	%
:- pred read_dice_to_string(string::in, string::in, string::in, int::in,
	string::out, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string. 
:- import_module svmap.

:- import_module mdbcomp.program_representation.

:- type trace_counts_kind ---> pass ; fail.

merge_trace_counts(Kind, TraceCounts, !Dice) :-
	map.foldl(merge_proc_trace_counts(Kind), TraceCounts, !Dice).

:- pred merge_proc_trace_counts(trace_counts_kind::in, proc_label::in,
	proc_trace_counts::in, dice::in, dice::out) is det.

merge_proc_trace_counts(Kind, ProcLabel, ProcTraceCounts, !Dice) :-
	(
		map.search(!.Dice, ProcLabel, FoundProcDice)
	->
		ProcDice = FoundProcDice
	;
		ProcDice = map.init
	),
	map.foldl(merge_path_port(Kind), ProcTraceCounts, ProcDice,
		MergedProcDice),
	svmap.set(ProcLabel, MergedProcDice, !Dice).

:- pred merge_path_port(trace_counts_kind::in, path_port::in, int::in, 
	proc_dice::in, proc_dice::out) is det.

merge_path_port(Kind, PathPort, Count, !ProcDice) :-
	(
		map.transform_value(add_trace_count(Kind, Count), PathPort,
			!.ProcDice, UpdatedProcDice)
	->
		!:ProcDice = UpdatedProcDice
	;
		(
			Kind = pass,
			svmap.set(PathPort, exec_count(Count, 1, 0, 0), 
				!ProcDice)
		;
			Kind = fail,
			svmap.set(PathPort, exec_count(0, 0, Count, 1), 
				!ProcDice)
		)
	).	

:- pred add_trace_count(trace_counts_kind::in, int::in, 
	exec_count::in, exec_count::out) is det.

add_trace_count(pass, Count, exec_count(PassExec, PassTests, FailExec, 
	FailTests), exec_count(PassExec + Count, PassTests + 1, FailExec, 
	FailTests)).
add_trace_count(fail, Count, exec_count(PassExec, PassTests, FailExec, 
	FailTests), exec_count(PassExec, PassTests, FailExec + Count, 
	FailTests + 1)).

:- type read_trace_counts_list_result
	--->	ok(list(trace_counts))
	;	error_message(string).

read_trace_counts_list(FileName, Result, !IO) :-
	io.open_input(FileName, OpenResult, !IO),
	(
		OpenResult = ok(FileStream),
		read_trace_counts_list_stream(FileName, FileStream, Result, !IO)
	;
		OpenResult = error(IOError),
		Result = error_message("Error opening file `" ++ FileName ++
			"': " ++ string.string(IOError))
	).

	% Same as read_trace_counts_list/4, but read the filenames containing
	% the trace_counts from the given stream.  MainFileName is the
	% name of the file being read and is only used for error messages.
	%
:- pred read_trace_counts_list_stream(string::in, io.input_stream::in, 
	read_trace_counts_list_result::out, io::di, io::uo) is det.

read_trace_counts_list_stream(MainFileName, Stream, Result, !IO) :-
	io.read_line_as_string(Stream, ReadResult, !IO),
	(
		ReadResult = ok(Line),
		% Remove the trailing newline:
		FileName = string.left(Line, string.length(Line) - 1),
		trace_counts.read_trace_counts(FileName, ReadTCResult, !IO),
		(
			ReadTCResult = ok(TraceCount),
			read_trace_counts_list_stream(MainFileName, Stream, 
				RestResult, !IO),
			(
				RestResult = ok(TraceCounts)
			->
				Result = ok([TraceCount | TraceCounts])
			;
				Result = RestResult
			)
		;
			ReadTCResult = io_error(IOError),
			Result = error_message("IO error reading file "
				++ "`" ++ FileName ++ "': " 
				++ string.string(IOError))
		;
			ReadTCResult = error_message(Message),
			Result = error_message("Error reading trace counts "++
				"from file `" ++ FileName ++ "': " ++ 
				Message)
		)
	;
		ReadResult = error(Error),
		Result = error_message("IO error reading file " ++ "`" ++ 
			MainFileName ++ "': " ++ string.string(Error))
	;
		ReadResult = eof,
		Result = ok([])
	).

:- pragma export(read_dice_to_string(in, in, in, in, out, out, di, uo), 
	"MR_MDB_read_dice_to_string").

read_dice_to_string(PassFiles, FailFile, SortStr, N, DiceStr, Problem, !IO) :-
	(
		sort_string_is_valid(SortStr)
	->
		read_trace_counts_list(PassFiles, ReadPassResult, !IO),
		(
			ReadPassResult = ok(PassTraceCountsList),
			TotalPassTests = length(PassTraceCountsList),
			TotalFailTests = 1,
			read_trace_counts(FailFile, ReadFailResult, !IO),
			(
				ReadFailResult = ok(FailTraceCounts),
				list.foldl(merge_trace_counts(pass), 
					PassTraceCountsList, map.init, 
					PassDice),
				merge_trace_counts(fail, FailTraceCounts, 
					PassDice, Dice),
				LabelCounts = dice_to_label_counts(Dice),
				list.sort(label_count_compare(SortStr), 
					LabelCounts, SortedLabelCounts),
				(
					list.take(N, SortedLabelCounts, Taken)
				->
					TopNLabelCounts = Taken
				;
					TopNLabelCounts = SortedLabelCounts
				),
				DiceStr = format_label_counts(TopNLabelCounts, 
					TotalPassTests, TotalFailTests),
				Problem = ""
			;
				ReadFailResult = error_message(Message),
				DiceStr = "",
				Problem = "Error reading trace counts from "
					++ "file `" ++
					FailFile ++ "': " ++ Message
			;
				ReadFailResult = io_error(IOError),
				DiceStr = "",
				Problem = "IO Error reading trace counts from "
					++"file `" ++ FailFile ++ "': " 
					++ string.string(IOError)
			)
		;
			ReadPassResult = error_message(Problem),
			DiceStr = ""
		)
	;
		Problem = "Invalid sort string",
		DiceStr = ""
	).

	% Values of this type uniquely identify a label in the program
	% and contain some statistics about the execution of the label.
	%
:- type label_count
	--->	label_count(
			proc_label	:: proc_label, 
			path_port	:: path_port, 
			counts		:: exec_count
		).

:- pred label_count_compare(string::in, label_count::in, label_count::in,
	builtin.comparison_result::out) is det.

label_count_compare(SortStr, LabelCount1, LabelCount2, Result) :-
	(
		SortStr = ""
	->
		builtin.compare(Result, LabelCount1, LabelCount2)
	;
		exec_count_compare(SortStr, LabelCount1 ^ counts, 
			LabelCount2 ^ counts, Result)
	).

:- pred exec_count_compare(string::in, exec_count::in, exec_count::in,
	builtin.comparison_result::out) is det.

exec_count_compare(SortStr, ExecCount1, ExecCount2, Result) :-
	(
		string.first_char(SortStr, C, Rest)
	->
		(
			C = 'p'
		->
			builtin.compare(Result0, ExecCount1 ^ pass_count, 
				ExecCount2 ^ pass_count)
		;
			C = 'P'
		->
			builtin.compare(Result0, ExecCount2 ^ pass_count, 
				ExecCount1 ^ pass_count)
		;
			C = 'f'
		->
			builtin.compare(Result0, ExecCount1 ^ fail_count, 
				ExecCount2 ^ fail_count)
		;
			C = 'F'
		->
			builtin.compare(Result0, ExecCount2 ^ fail_count, 
				ExecCount1 ^ fail_count)
		;
			C = 's'
		->
			builtin.compare(Result0, 
				suspicion_ratio(ExecCount1 ^ pass_count,
					ExecCount1 ^ fail_count),
				suspicion_ratio(ExecCount2 ^ pass_count,
					ExecCount2 ^ fail_count))
		;
			C = 'S'
		->
			builtin.compare(Result0, 
				suspicion_ratio(ExecCount2 ^ pass_count,
					ExecCount2 ^ fail_count),
				suspicion_ratio(ExecCount1 ^ pass_count,
					ExecCount1 ^ fail_count))
		;
			error("label_count_compare: invalid sort string")
		),
		(
			Result0 = (=), string.length(Rest) > 0
		->
			exec_count_compare(Rest, ExecCount1, ExecCount2,
				Result)
		;
			Result = Result0
		)
	;
		error("label_count_compare: empty sort string")
	).

:- pred sort_string_is_valid(string::in) is semidet.

sort_string_is_valid(Str) :-
	Chrs = string.to_char_list(Str),
	ChrSet = set.list_to_set(Chrs),
	set.subset(ChrSet, set.list_to_set(['p', 'P', 'f', 'F', 's', 'S'])).

:- func dice_to_label_counts(dice) = list(label_count).

dice_to_label_counts(Dice) = LabelCounts :-
	map.foldl(append_label_counts, Dice, [], LabelCounts).

:- pred append_label_counts(proc_label::in, proc_dice::in, 
	list(label_count)::in, list(label_count)::out) is det.

append_label_counts(ProcLabel, ProcDice, !LabelCounts) :-
	map.to_assoc_list(ProcDice, ProcExecCounts),
	list.map(make_label_count(ProcLabel), ProcExecCounts, NewLabelCounts),
	append(!.LabelCounts, NewLabelCounts, !:LabelCounts).

:- pred make_label_count(proc_label::in, pair(path_port, exec_count)::in, 
	label_count::out) is det.

make_label_count(ProcLabel, PathPort - ExecCount, 
	label_count(ProcLabel, PathPort, ExecCount)).

	% Produce a formatted table from a list of label_counts.
	%
:- func format_label_counts(list(label_count), int, int) = string.

format_label_counts(LabelCounts, TotalPassTests, _TotalFailTests) = Str :-
	list.map6(deconstruct_label_count, LabelCounts, ProcLabels,
		PathPorts, PassCounts, PassTests, FailCounts, _FailTests),
	FormattedProcLabels = list.map(format_proc_label, ProcLabels),
	FormattedPathPorts = list.map(format_path_port, PathPorts),
	FormattedContexts = list.map(format_context, LabelCounts),
	PassCountStrs = list.map(string.int_to_string_thousands, PassCounts),
	PassTestsStrs = list.map(bracket_int, PassTests),
	FailCountStrs = list.map(string.int_to_string_thousands, FailCounts),
	SuspicionIndices = list.map_corresponding(suspicion_ratio, PassCounts,
		FailCounts),
	FormattedSuspicionIndices = list.map(dice.format_float(2),
		SuspicionIndices),
	TotalPassTestsStr = "(" ++ int_to_string_thousands(TotalPassTests) 
		++ ")",
	Str = string.format_table([
		left( ["Procedure"       | FormattedProcLabels]), 
		left( ["Path/Port"       | FormattedPathPorts]), 
		left( ["File:Line"       | FormattedContexts]),
		right(["Pass"            | PassCountStrs]), 
		right([TotalPassTestsStr | PassTestsStrs]), 
		right(["Fail"            | FailCountStrs]), 
		right(["Suspicion"       | FormattedSuspicionIndices])], " ").

:- func bracket_int(int) = string.

bracket_int(X) = "(" ++ string.int_to_string_thousands(X) ++ ")".

:- func suspicion_ratio(int, int) = float.

	% suspicion_ratio gives an indication of how likely a label is to
	% be buggy based on how many times it was executed in passing and
	% failing test runs.
	%
suspicion_ratio(PassCount, FailCount) = 
	% PassCount + FailCount should never be zero since if a label
	% isn't executed in any tests then it wouldn't be included in the dice.
	float(FailCount) / float(PassCount + FailCount).

:- func format_float(int, float) = string.

format_float(DecimalPlaces, Flt) = string.format("%.*f", [i(DecimalPlaces), f(Flt)]). 

:- pred deconstruct_label_count(label_count::in, proc_label::out, 
	path_port::out, int::out, int::out, int::out, int::out) is det.

deconstruct_label_count(label_count(PathPort, ProcLabel, 
	exec_count(PassCount, PassTests, FailCount, FailTests)),
	PathPort, ProcLabel, PassCount, PassTests, FailCount, FailTests).

:- func format_exec_count(exec_count) = string.

format_exec_count(exec_count(PassCount, PassTests, FailCount, FailTests)) = 
	string.pad_left(int_to_string(PassCount), ' ', 12)
	++ string.pad_left("(" ++ int_to_string(PassTests) ++ ")", ' ', 8)
	++ string.pad_left(int_to_string(FailCount), ' ', 12)
	++ string.pad_left("(" ++ int_to_string(FailTests) ++ ")", ' ', 8).

:- func make_label_counts_heading(int, int, int) = string.

make_label_counts_heading(MaxProcLabelLength, MaxPathPortLength, 
		MaxContextLength) =
	string.pad_right("Procedure", ' ', MaxProcLabelLength + 2)
	++ string.pad_right("Path/Port", ' ', MaxPathPortLength + 2)
	++ string.pad_right("Context", ' ', MaxContextLength + 2)
	++ string.pad_left("Pass Count", ' ', 20)
	++ string.pad_left("Fail Count", ' ', 20).

:- func format_proc_label(proc_label) = string.

format_proc_label(ProcLabel) = Str :-
	(
		ProcLabel = proc(_, PredOrFunc, SymModule, Name, Arity, 
			ModeNo),
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

:- func format_context(label_count) = string.

format_context(label_count(ProcLabel, PathPort, _)) = Str :-
	(
		find_context(ProcLabel, PathPort, FileName, LineNo)
	->
		Str = FileName ++ ":" ++ int_to_string(LineNo)
	;
		Str = ""
	).

:- func format_path_port(path_port) = string.

format_path_port(port_only(Port)) = Str :-
	mdbcomp.trace_counts.string_to_trace_port(Str, Port).
format_path_port(path_only(Path)) = Str :-
	mdbcomp.program_representation.string_from_path(Path, PathStr),
	Str = "<" ++ PathStr ++ ">".
format_path_port(port_and_path(Port, Path)) =
	format_path_port(port_only(Port)) ++
		" " ++ format_path_port(path_only(Path)).

	% Given a proc_label and path_port, find the source filename and line 
	% number of the label.  Fail if this is not possible because of
	% insufficient tracing information.
	%
	% We only look up the contexts for user predicates.  This is okay since
	% compiler generated predicates are not counted anyway and would be
	% of little interest in a dice.
	%
:- pred find_context(proc_label::in, path_port::in, string::out, int::out) 
	is semidet.

find_context(proc(DeclSymModule, _, _, Name, Arity, ModeNo), PathPort, 
		FileName, LineNo) :-
	DeclModule = sym_name_to_string(DeclSymModule),
	find_context_for_proc(DeclModule, Name, Arity, ModeNo, PathPort, 
		FileName, LineNo). 

:- pred find_context_for_proc(string::in, string::in, int::in, int::in, 
	path_port::in, string::out, int::out) is semidet.

:- pragma foreign_proc(c, find_context_for_proc(Module::in, Name::in, 
	Arity::in, ModeNo::in, PathPort::in, FileName::out, LineNo::out), 
	[promise_pure, may_call_mercury, thread_safe, terminates],
"
    const MR_Module_Layout		*module;
    const MR_Module_File_Layout		*file;
    const MR_Label_Layout		*label;
    const MR_Proc_Layout		*proc;
    const MR_User_Proc_Id		*id;
    int					num_modules;
    int					module_num;
    int					file_num;
    int					num_files;
    int					num_labels;
    int					label_num;
    const char*				filename;
    MR_bool				are_same;
    
    num_modules = MR_module_info_next;

    for (module_num = 0; module_num < num_modules; module_num++) {
        module = MR_module_infos[module_num];
        /* 
        ** Check if the label occurs in this module.
        */
        if (MR_streq(Module, module->MR_ml_name)) {
            num_files = module->MR_ml_filename_count;
            /*
            ** Check each file in the module.
            */
            for (file_num = 0; file_num < num_files; file_num++) {
                file = module->MR_ml_module_file_layout[file_num];
                num_labels = file->MR_mfl_label_count;
                /* 
                ** Check each label in the file.
                */
                for (label_num = 0; label_num < num_labels; label_num++) {
                    label = file->MR_mfl_label_layout[label_num];
                    proc = label->MR_sll_entry;
                    id = &proc->MR_sle_user;
                    /*
                    ** Check if the proc name, arity and mode are the same as
                    ** the name, arity and mode for the label we are interested
                    ** in.
                    */
                    if (MR_streq(id->MR_user_name, Name) &&
                            id->MR_user_arity == Arity &&
                            id->MR_user_mode == ModeNo) {
                        /*
                        ** Check if the path/port is the same as the
                        ** label we are interested in.
                        */
                        MR_TRACE_CALL_MERCURY(
                            MR_MDB_same_path_port(
                                (MR_String) MR_label_goal_path(label),
                                label->MR_sll_port, PathPort, &are_same);
                        );
                        if (are_same) {
                            /*
                            ** We have found the label, so return the 
                            ** filename and line number.
                            */
                            filename = file->MR_mfl_filename;
                            LineNo = file->MR_mfl_label_lineno[label_num];
                            SUCCESS_INDICATOR = MR_TRUE;
                            MR_TRACE_USE_HP(
                                MR_make_aligned_string(FileName,
                                    (MR_String) filename);
                            );
                            goto end;
                        }
                    }
                }
            }
        }
    }

    SUCCESS_INDICATOR = MR_FALSE;
    end:
").

	% Return yes if the given goal path string and port correspond to the
	% given path_port.
	%
:- pred same_path_port(string::in, trace_port::in, path_port::in, bool::out)
	is det.

:- pragma export(same_path_port(in, in, in, out), "MR_MDB_same_path_port").

same_path_port(PathStr1, Port1, PathPort2, Same) :-
	(
		PathPort2 = port_only(Port2),
		(
			Port1 = Port2
		->
			Same = yes
		;
			Same = no
		)
	;
		PathPort2 = path_only(Path2),
		mdbcomp.program_representation.path_from_string_det(PathStr1, 
			Path1),
		(
			Path1 = Path2
		->
			Same = yes
		;
			Same = no
		)
	;
		PathPort2 = port_and_path(Port2, Path2),
		mdbcomp.program_representation.path_from_string_det(PathStr1, 
			Path1),
		(
			Path1 = Path2, Port1 = Port2
		->
			Same = yes
		;
			Same = no
		)
	).
