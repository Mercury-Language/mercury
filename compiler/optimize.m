%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% optimize.m - LLDS to LLDS optimizations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__optimize.

:- interface.

:- import_module ll_backend__global_data.
:- import_module ll_backend__llds.

:- import_module io, list.

:- pred optimize_main(global_data::in,
	list(c_procedure)::in, list(c_procedure)::out, io::di, io::uo) is det.

:- pred optimize__proc(global_data::in, c_procedure::in, c_procedure::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module hlds__hlds_pred.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__delay_slot.
:- import_module ll_backend__dupelim.
:- import_module ll_backend__frameopt.
:- import_module ll_backend__jumpopt.
:- import_module ll_backend__labelopt.
:- import_module ll_backend__llds_out.
:- import_module ll_backend__opt_debug.
:- import_module ll_backend__opt_util.
:- import_module ll_backend__peephole.
:- import_module ll_backend__reassign.
:- import_module ll_backend__use_local_vars.
:- import_module ll_backend__wrap_blocks.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_out.

:- import_module bool, int, string.
:- import_module map, set, std_util, require, counter.

optimize_main(GlobalData, !Procs, !IO) :-
	list__map_foldl(optimize__proc(GlobalData), !Procs, !IO).

:- func make_internal_label(proc_label, int) = label.

make_internal_label(ProcLabel, LabelNum) = internal(LabelNum, ProcLabel).

optimize__proc(GlobalData, CProc0, CProc, !IO) :-
	some [!OptDebugInfo, !C, !Instrs] (
		CProc0 = c_procedure(Name, Arity, PredProcId, !:Instrs,
			ProcLabel, !:C, MayAlterRtti),
		optimize__init_opt_debug_info(Name, Arity, PredProcId,
			!.Instrs, !.C, !:OptDebugInfo, !IO),
		globals__io_lookup_int_option(optimize_repeat, Repeat, !IO),
		(
			global_data_maybe_get_proc_layout(GlobalData,
				PredProcId, ProcLayout)
		->
			LabelMap = ProcLayout ^ internal_map,
			map__sorted_keys(LabelMap, LayoutLabelNums),
			LayoutLabels = list__map(make_internal_label(ProcLabel),
				LayoutLabelNums),
			set__sorted_list_to_set(LayoutLabels, LayoutLabelSet)
		;
			set__init(LayoutLabelSet)
		),
		optimize__initial(LayoutLabelSet, ProcLabel, MayAlterRtti,
			!C, !OptDebugInfo, !Instrs, !IO),
		optimize__repeat(Repeat, LayoutLabelSet, ProcLabel,
			MayAlterRtti, !C, !OptDebugInfo, !Instrs, !IO),
		optimize__middle(yes, LayoutLabelSet, ProcLabel, MayAlterRtti,
			!C, !OptDebugInfo, !Instrs, !IO),
		optimize__last(LayoutLabelSet, !.C, !.OptDebugInfo, !Instrs,
			!IO),
		CProc = c_procedure(Name, Arity, PredProcId, !.Instrs,
			ProcLabel, !.C, MayAlterRtti)
	).

%-----------------------------------------------------------------------------%

:- type opt_debug_info
	--->	opt_debug_info(
			string,			% base file name
			int			% last file number written
		)
	;	no_opt_debug_info.

:- pred optimize__init_opt_debug_info(string::in, int::in, pred_proc_id::in,
	list(instruction)::in, counter::in, opt_debug_info::out,
	io::di, io::uo) is det.

optimize__init_opt_debug_info(Name, Arity, PredProcId, Instrs0, Counter,
		OptDebugInfo, !IO) :-
	globals__io_lookup_bool_option(debug_opt, DebugOpt, !IO),
	globals__io_lookup_int_option(debug_opt_pred_id, DebugOptPredId, !IO),
	PredProcId = proc(PredId, ProcId),
	pred_id_to_int(PredId, PredIdInt),
	proc_id_to_int(ProcId, ProcIdInt),
	(
		DebugOpt = yes,
		( DebugOptPredId >= 0 => DebugOptPredId = PredIdInt )
	->
		MangledName = name_mangle(Name),
		string__int_to_string(Arity, ArityStr),
		string__int_to_string(PredIdInt, PredIdStr),
		string__int_to_string(ProcIdInt, ProcIdStr),
		string__append_list([MangledName, "_", ArityStr,
			".pred", PredIdStr, ".proc", ProcIdStr], BaseName),
		OptDebugInfo = opt_debug_info(BaseName, 0),

		string__append_list([BaseName, ".opt0"], FileName),
		io__open_output(FileName, Res, !IO),
		( Res = ok(FileStream) ->
			io__set_output_stream(FileStream, OutputStream, !IO),
			counter__allocate(NextLabel, Counter, _),
			opt_debug__msg(yes, NextLabel, "before optimization",
				!IO),
			opt_debug__dump_instrs(yes, Instrs0, !IO),
			io__set_output_stream(OutputStream, _, !IO),
			io__close_output(FileStream, !IO)
		;
			string__append("cannot open ", FileName, ErrorMsg),
			error(ErrorMsg)
		)
	;
		OptDebugInfo = no_opt_debug_info
	).

:- pred optimize__maybe_opt_debug(list(instruction)::in, counter::in,
	string::in, opt_debug_info::in, opt_debug_info::out,
	io::di, io::uo) is det.

optimize__maybe_opt_debug(Instrs, Counter, Msg, OptDebugInfo0, OptDebugInfo,
		!IO) :-
	(
		OptDebugInfo0 = opt_debug_info(BaseName, OptNum0),
		OptNum = OptNum0 + 1,
		string__int_to_string(OptNum0, OptNum0Str),
		string__int_to_string(OptNum, OptNumStr),
		string__append_list([BaseName, ".opt", OptNum0Str],
			OptFileName0),
		string__append_list([BaseName, ".opt", OptNumStr],
			OptFileName),
		string__append_list([BaseName, ".diff", OptNumStr],
			DiffFileName),
		io__open_output(OptFileName, Res, !IO),
		( Res = ok(FileStream) ->
			io__set_output_stream(FileStream, OutputStream, !IO),
			counter__allocate(NextLabel, Counter, _),
			opt_debug__msg(yes, NextLabel, Msg, !IO),
			opt_debug__dump_instrs(yes, Instrs, !IO),
			io__set_output_stream(OutputStream, _, !IO),
			io__close_output(FileStream, !IO)
		;
			string__append("cannot open ", OptFileName,
				ErrorMsg),
			error(ErrorMsg)
		),
		% Although the -u is not fully portable, it is available
		% on all the systems we intend to use it on, and the main user
		% of --debug-opt (zs) strongly prefers -u to -c.
		string__append_list(["diff -u ",
			OptFileName0, " ", OptFileName,
			" > ", DiffFileName], DiffCommand),
		io__call_system(DiffCommand, _, !IO),
		OptDebugInfo = opt_debug_info(BaseName, OptNum)
	;
		OptDebugInfo0 = no_opt_debug_info,
		OptDebugInfo = no_opt_debug_info
	).

%-----------------------------------------------------------------------------%

:- pred optimize__initial(set(label)::in, proc_label::in, may_alter_rtti::in,
	counter::in, counter::out, opt_debug_info::in, opt_debug_info::out,
	list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize__initial(LayoutLabelSet, ProcLabel, MayAlterRtti, !C, !OptDebugInfo,
		!Instrs, !IO) :-
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	opt_util__find_first_label(!.Instrs, Label),
	opt_util__format_label(Label, LabelStr),

	globals__io_lookup_bool_option(optimize_frames, FrameOpt, !IO),
	(
		FrameOpt = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing nondet frames for ",
				!IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		frameopt_nondet(ProcLabel, LayoutLabelSet, MayAlterRtti, !C,
			!Instrs, _Mod),
		optimize__maybe_opt_debug(!.Instrs, !.C,
			"after nondet frame opt", !OptDebugInfo, !IO)
	;
		FrameOpt = no
	).

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int::in, set(label)::in, proc_label::in,
	may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out,
	list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize__repeat(CurIter, LayoutLabelSet, ProcLabel, MayAlterRtti,
		!C, !OptDebugInfo, !Instrs, !IO) :-
	( CurIter > 0 ->
		NextIter = CurIter - 1,
		( NextIter = 0 ->
			Final = yes
		;
			Final = no
		),
		optimize__repeated(Final, LayoutLabelSet, ProcLabel,
			MayAlterRtti, !C, !OptDebugInfo,
			!Instrs, Mod, !IO),
		(
			Mod = yes,
			optimize__repeat(NextIter, LayoutLabelSet, ProcLabel,
				MayAlterRtti, !C, !OptDebugInfo, !Instrs, !IO)
		;
			Mod = no
		)
	;
		true
	).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__repeated(bool::in, set(label)::in, proc_label::in,
	may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out,
	list(instruction)::in, list(instruction)::out, bool::out,
	io::di, io::uo) is det.

optimize__repeated(Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
		!C, !OptDebugInfo, !Instrs, Mod, !IO) :-
	InstrsAtStart = !.Instrs,
	opt_util__find_first_label(!.Instrs, Label),
	proc_label_to_c_string(get_proc_label(Label), no) = LabelStr,
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	globals__io_lookup_bool_option(optimize_jumps, Jumpopt, !IO),
	globals__io_lookup_bool_option(optimize_fulljumps, FullJumpopt, !IO),
	globals__io_lookup_bool_option(pessimize_tailcalls,
		PessimizeTailCalls, !IO),
	globals__io_lookup_bool_option(checked_nondet_tailcalls,
		CheckedNondetTailCalls, !IO),
	(
		Jumpopt = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing jumps for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		jumpopt_main(LayoutLabelSet, MayAlterRtti, ProcLabel,
			FullJumpopt, Final, PessimizeTailCalls,
			CheckedNondetTailCalls, !C, !Instrs, Mod1),
		optimize__maybe_opt_debug(!.Instrs, !.C, "after jump opt",
			!OptDebugInfo, !IO)
	;
		Jumpopt = no,
		Mod1 = no
	),
	globals__io_lookup_bool_option(optimize_peep, Peephole, !IO),
	(
		Peephole = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing locally for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		globals__io_get_gc_method(GC_Method, !IO),
		peephole__optimize(GC_Method, !Instrs, Mod2),
		optimize__maybe_opt_debug(!.Instrs, !.C, "after peephole",
			!OptDebugInfo, !IO)
	;
		Peephole = no,
		Mod2 = no
	),
	globals__io_lookup_bool_option(optimize_labels, LabelElim, !IO),
	(
		LabelElim = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing labels for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		labelopt_main(Final, LayoutLabelSet, !Instrs, Mod3),
		optimize__maybe_opt_debug(!.Instrs, !.C, "after label opt",
			!OptDebugInfo, !IO)
	;
		LabelElim = no,
		Mod3 = no
	),
	globals__io_lookup_bool_option(optimize_dups, DupElim, !IO),
	(
		DupElim = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing duplicates for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		dupelim_main(ProcLabel, !C, !Instrs),
		optimize__maybe_opt_debug(!.Instrs, !.C, "after duplicates",
			!OptDebugInfo, !IO)
	;
		DupElim = no
	),
	( Mod1 = no, Mod2 = no, Mod3 = no, !.Instrs = InstrsAtStart ->
		Mod = no
	;
		Mod = yes
	),
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	maybe_report_stats(Statistics, !IO).

:- pred optimize__middle(bool::in, set(label)::in, proc_label::in,
	may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out,
	list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize__middle(Final, LayoutLabelSet, ProcLabel, MayAlterRtti, !C,
		!OptDebugInfo, !Instrs, !IO) :-
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	opt_util__find_first_label(!.Instrs, Label),
	opt_util__format_label(Label, LabelStr),

	globals__io_lookup_bool_option(optimize_frames, FrameOpt, !IO),
	(
		FrameOpt = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing frames for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		frameopt_main(ProcLabel, !C, !Instrs, Mod1, Jumps),
		optimize__maybe_opt_debug(!.Instrs, !.C, "after frame opt",
			!OptDebugInfo, !IO),
		globals__io_lookup_bool_option(optimize_fulljumps,
			FullJumpopt, !IO),
		globals__io_lookup_bool_option(pessimize_tailcalls,
			PessimizeTailCalls, !IO),
		globals__io_lookup_bool_option(checked_nondet_tailcalls,
			CheckedNondetTailCalls, !IO),
		(
			Jumps = yes,
			FullJumpopt = yes
		->
			(
				VeryVerbose = yes,
				io__write_string("% Optimizing jumps for ",
					!IO),
				io__write_string(LabelStr, !IO),
				io__write_string("\n", !IO)
			;
				VeryVerbose = no
			),
			jumpopt_main(LayoutLabelSet, MayAlterRtti, ProcLabel,
				FullJumpopt, Final, PessimizeTailCalls,
				CheckedNondetTailCalls, !C, !Instrs, _Mod2),
			optimize__maybe_opt_debug(!.Instrs, !.C, "after jumps",
				!OptDebugInfo, !IO)
		;
			true
		),
		(
			Mod1 = yes,
			(
				VeryVerbose = yes,
				io__write_string("% Optimizing labels for ",
					!IO),
				io__write_string(LabelStr, !IO),
				io__write_string("\n", !IO)
			;
				VeryVerbose = no
			),
			labelopt_main(Final, LayoutLabelSet, !Instrs, _Mod3),
			optimize__maybe_opt_debug(!.Instrs, !.C, "after labels",
				!OptDebugInfo, !IO)
		;
			Mod1 = no
		)
	;
		FrameOpt = no
	),
	globals__io_lookup_bool_option(use_local_vars, UseLocalVars, !IO),
	(
		UseLocalVars = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing local vars for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		globals__io_lookup_int_option(num_real_r_regs, NumRealRRegs,
			!IO),
		globals__io_lookup_int_option(local_var_access_threshold,
			AccessThreshold, !IO),
		use_local_vars__main(!Instrs, ProcLabel, NumRealRRegs,
			AccessThreshold, !C),
		optimize__maybe_opt_debug(!.Instrs, !.C,
			"after use_local_vars", !OptDebugInfo, !IO)
	;
		UseLocalVars = no
	).

:- pred optimize__last(set(label)::in, counter::in, opt_debug_info::in,
	list(instruction)::in, list(instruction)::out, io::di, io::uo) is det.

optimize__last(LayoutLabelSet, C, !.OptDebugInfo, !Instrs, !IO) :-
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	opt_util__find_first_label(!.Instrs, Label),
	opt_util__format_label(Label, LabelStr),

	globals__io_lookup_bool_option(optimize_reassign, Reassign, !IO),
	globals__io_lookup_bool_option(optimize_delay_slot, DelaySlot, !IO),
	globals__io_lookup_bool_option(use_local_vars, UseLocalVars, !IO),
	(
		( Reassign = yes
		; DelaySlot = yes
		; UseLocalVars = yes
		)
	->
		% We must get rid of any extra labels added by other passes,
		% since they can confuse reassign, wrap_blocks and delay_slot.
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing labels for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		labelopt_main(no, LayoutLabelSet, !Instrs, _Mod1),
		optimize__maybe_opt_debug(!.Instrs, C, "after label opt",
			!OptDebugInfo, !IO)
	;
		true
	),
	(
		Reassign = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing reassign for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		remove_reassign(!Instrs),
		optimize__maybe_opt_debug(!.Instrs, C, "after reassign",
			!OptDebugInfo, !IO)
	;
		Reassign = no
	),
	(
		DelaySlot = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Optimizing delay slot for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		fill_branch_delay_slot(!Instrs),
		optimize__maybe_opt_debug(!.Instrs, C, "after delay slots",
			!OptDebugInfo, !IO)
	;
		DelaySlot = no
	),
	(
		UseLocalVars = yes,
		(
			VeryVerbose = yes,
			io__write_string("% Wrapping blocks for ", !IO),
			io__write_string(LabelStr, !IO),
			io__write_string("\n", !IO)
		;
			VeryVerbose = no
		),
		wrap_blocks(!Instrs),
		optimize__maybe_opt_debug(!.Instrs, C, "after wrap blocks",
			!.OptDebugInfo, _OptDebugInfo, !IO)
	;
		UseLocalVars = no
	).
