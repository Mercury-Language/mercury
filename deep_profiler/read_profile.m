%-----------------------------------------------------------------------------% % Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains code for reading in a deep profiling data file.
% Such files, named Deep.data, are created by deep profiled executables.

:- module read_profile.

:- interface.

:- import_module profile.
:- import_module io, std_util.

:- pred read_call_graph(string::in, maybe_error(initial_deep)::out,
	io__state::di, io__state::uo) is det.

:- implementation.

:- import_module measurements, array_util.
:- import_module array, char, string, int, float, std_util, list, require.

:- type deep_result(T)
	--->	ok(T)
	;	error(string).

:- type deep_result2(T1, T2)
	--->	ok2(T1, T2)
	;	error2(string).

:- type ptr_info --->
		ptr_info(
			ps	:: int,
			css	:: int,
			pd	:: int,
			csd	:: int
		).

:- type ptr_kind
	--->	ps
	;	pd
	;	css
	;	csd.

read_call_graph(FileName, Res) -->
	io__see_binary(FileName, Res0),
	(
		{ Res0 = ok },
		read_id_string(Res1),
		(
			{ Res1 = ok(_) },
			read_sequence7(
				read_fixed_size_int,
				read_fixed_size_int,
				read_fixed_size_int,
				read_fixed_size_int,
				read_num,
				read_num,
				read_num,
				(pred(NumCSDs::in, NumCSSs::in,
						NumPDs::in, NumPSs::in,
						TicksPerSec::in,
						InstrumentQuanta::in,
						UserQuanta::in,
						ResInitDeep::out) is det :-
					init_deep(NumCSDs, NumCSSs,
						NumPDs, NumPSs,
						TicksPerSec,
						InstrumentQuanta, UserQuanta,
						InitDeep0),
					ResInitDeep = ok(InitDeep0)
				),
				Res2),
			(
				{ Res2 = ok(InitDeep) },
				{ PtrInfo0 = ptr_info(0, 0, 0, 0) },
				read_nodes(InitDeep, PtrInfo0, Res3),
				io__seen_binary,
				{ resize_arrays(Res3, Res) }
			;
				{ Res2 = error(Err) },
				{ Res = error(Err) }
			)
		;
			{ Res1 = error(Msg) },
			{ Res = error(Msg) }
		)
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error(Msg) }
	).

:- pred read_id_string(deep_result(string)::out,
	io__state::di, io__state::uo) is det.

read_id_string(Res) -->
	read_n_byte_string(string__length(id_string), Res0),
	(
		{ Res0 = ok(String) },
		( { String = id_string } ->
			{ Res = ok(id_string) }
		;
			{ Res = error("not a deep profiling data file") }
		)
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- func id_string = string.

id_string = "Mercury deep profiler data".

:- pred init_deep(int::in, int::in, int::in, int::in, int::in, int::in,
	int::in, initial_deep::out) is det.

init_deep(NumCSDs, NumCSSs, NumPDs, NumPSs, InstrumentQuanta, UserQuanta,
		TicksPerSec, InitDeep) :-
	InitStats = profile_stats(
		InstrumentQuanta,
		UserQuanta,
		-1, -1, -1, -1,
		TicksPerSec),
	InitDeep = initial_deep(
		InitStats,
		proc_dynamic_ptr(-1),
		array__init(NumCSDs + 1,
			call_site_dynamic(
				proc_dynamic_ptr(-1),
				proc_dynamic_ptr(-1),
				zero_own_prof_info,
				no
			)),
		array__init(NumPDs + 1,
			proc_dynamic(proc_static_ptr(-1), array([]), no)),
		array__init(NumCSSs + 1,
			call_site_static(
				proc_static_ptr(-1), -1,
				normal_call(proc_static_ptr(-1), ""), -1, ""
			)),
		array__init(NumPSs + 1,
			proc_static(dummy_proc_id, "", "", "", array([])))
	).

:- pred read_nodes(initial_deep::in, ptr_info::in,
	deep_result2(initial_deep, ptr_info)::out,
	io__state::di, io__state::uo) is det.

read_nodes(InitDeep0, PtrInfo0, Res) -->
	read_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		( { Byte = token_call_site_static } ->
			read_call_site_static(Res1),
			(
				{ Res1 = ok2(CallSiteStatic, CSSI) },
				{ deep_insert(
					InitDeep0 ^ init_call_site_statics,
					CSSI, CallSiteStatic, CSSs) },
				{ InitDeep1 = InitDeep0
					^ init_call_site_statics := CSSs },
				{ PtrInfo1 = PtrInfo0 ^ css
					:= max(PtrInfo0 ^ css, CSSI) },
				read_nodes(InitDeep1, PtrInfo1, Res)
			;
				{ Res1 = error2(Err) },
				{ Res = error2(Err) }
			)
		; { Byte = token_proc_static } ->
			read_proc_static(Res1),
			(
				{ Res1 = ok2(ProcStatic, PSI) },
				{ deep_insert(
					InitDeep0 ^ init_proc_statics,
					PSI, ProcStatic, PSs) },
				{ InitDeep1 = InitDeep0
					^ init_proc_statics := PSs },
				{ PtrInfo1 = PtrInfo0 ^ ps
					:= max(PtrInfo0 ^ ps, PSI) },
				read_nodes(InitDeep1, PtrInfo1, Res)
			;
				{ Res1 = error2(Err) },
				{ Res = error2(Err) }
			)
		; { Byte = token_call_site_dynamic } ->
			read_call_site_dynamic(Res1),
			(
				{ Res1 = ok2(CallSiteDynamic, CSDI) },
				{ deep_insert(
					InitDeep0 ^ init_call_site_dynamics,
					CSDI, CallSiteDynamic, CSDs) },
				{ InitDeep1 = InitDeep0
					^ init_call_site_dynamics := CSDs },
				{ PtrInfo1 = PtrInfo0 ^ csd
					:= max(PtrInfo0 ^ csd, CSDI) },
				read_nodes(InitDeep1, PtrInfo1, Res)
			;
				{ Res1 = error2(Err) },
				{ Res = error2(Err) }
			)
		; { Byte = token_proc_dynamic } ->
			read_proc_dynamic(Res1),
			(
				{ Res1 = ok2(ProcDynamic, PDI) },
				{ deep_insert(
					InitDeep0 ^ init_proc_dynamics,
					PDI, ProcDynamic, PDs) },
				{ InitDeep1 = InitDeep0
					^ init_proc_dynamics := PDs },
				{ PtrInfo1 = PtrInfo0 ^ pd
					:= max(PtrInfo0 ^ pd, PDI) },
				read_nodes(InitDeep1, PtrInfo1, Res)
			;
				{ Res1 = error2(Err) },
				{ Res = error2(Err) }
			)
		; { Byte = token_root } ->
			read_root(Res1),
			(
				{ Res1 = ok(PDPtr) },
				{ InitDeep1 = InitDeep0 ^ init_root := PDPtr },
				read_nodes(InitDeep1, PtrInfo0, Res)
			;
				{ Res1 = error(Err) },
				{ Res = error2(Err) }
			)
		;
			{ format("unexpected token %d", [i(Byte)], Msg) },
			{ Res = error2(Msg) }
		)
	;
		{ Res0 = eof },
		{ Res = ok2(InitDeep0, PtrInfo0) }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error2(Msg) }
	).

:- pred read_root(deep_result(proc_dynamic_ptr)::out,
	io__state::di, io__state::uo) is det.

read_root(Res) -->
	% format("reading root.\n", []),
	read_ptr(pd, Res0),
	(
		{ Res0 = ok(PDI) },
		{ PDPtr = proc_dynamic_ptr(PDI) },
		{ Res = ok(PDPtr) }
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_call_site_static(deep_result2(call_site_static, int)::out,
	io__state::di, io__state::uo) is det.

read_call_site_static(Res) -->
	% format("reading call_site_static.\n", []),
	read_sequence4(
		read_ptr(css),
		read_call_site_kind_and_callee,
		read_num,
		read_string,
		(pred(CSSI0::in, Kind::in, LineNumber::in, Str::in, Res0::out)
				is det :-
			DummyPSPtr = proc_static_ptr(-1),
			DummySlotNum = -1,
			CallSiteStatic0 = call_site_static(DummyPSPtr,
				DummySlotNum, Kind, LineNumber, Str),
			Res0 = ok({CallSiteStatic0, CSSI0})
		),
		Res1),
	(
		{ Res1 = ok({CallSiteStatic, CSSI}) },
		{ Res = ok2(CallSiteStatic, CSSI) }
	;
		{ Res1 = error(Err) },
		{ Res = error2(Err) }
	).


:- pred read_proc_static(deep_result2(proc_static, int)::out,
	io__state::di, io__state::uo) is det.

read_proc_static(Res) -->
	% format("reading proc_static.\n", []),
	read_sequence4(
		read_ptr(ps),
		read_proc_id,
		read_string,
		read_num,
		(pred(PSI0::in, Id0::in, F0::in, N0::in, Stuff0::out) is det :-
			Stuff0 = ok({PSI0, Id0, F0, N0})
		),
		Res1),
	(
		{ Res1 = ok({PSI, Id, FileName, N}) },
		read_n_things(N, read_ptr(css), Res2),
		(
			{ Res2 = ok(Ptrs0) },
			{ map((pred(Ptr1::in, Ptr2::out) is det :-
				Ptr2 = call_site_static_ptr(Ptr1)
			), Ptrs0, Ptrs) },
			{ RefinedStr = refined_proc_id_to_string(Id) },
			{ RawStr = raw_proc_id_to_string(Id) },
			{ ProcStatic =
				proc_static(Id, RefinedStr, RawStr,
					FileName, array(Ptrs)) },
			{ Res = ok2(ProcStatic, PSI) }
		;
			{ Res2 = error(Err) },
			{ Res = error2(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error2(Err) }
	).

:- pred read_proc_id(deep_result(proc_id)::out,
	io__state::di, io__state::uo) is det.

read_proc_id(Res) -->
	read_deep_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		( { Byte = token_isa_compiler_generated } ->
			read_proc_id_compiler_generated(Res)
		; { Byte = token_isa_predicate } ->
			read_proc_id_user_defined(predicate, Res)
		; { Byte = token_isa_function } ->
			read_proc_id_user_defined(function, Res)
		;
			{ format("unexpected proc_id_kind %d",
				[i(Byte)], Msg) },
			{ Res = error(Msg) }
		)
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_proc_id_compiler_generated(deep_result(proc_id)::out,
	io__state::di, io__state::uo) is det.

read_proc_id_compiler_generated(Res) -->
	read_sequence6(
		read_string,
		read_string,
		read_string,
		read_string,
		read_num,
		read_num,
		(pred(TypeName::in, TypeModule::in, DefModule::in,
			PredName::in, Arity::in, Mode::in, ProcId::out)
			is det :-
			ProcId = ok(compiler_generated(TypeName, TypeModule,
				DefModule, PredName, Arity, Mode))
		),
		Res).

:- pred read_proc_id_user_defined(pred_or_func::in, deep_result(proc_id)::out,
	io__state::di, io__state::uo) is det.

read_proc_id_user_defined(PredOrFunc, Res) -->
	read_sequence5(
		read_string,
		read_string,
		read_string,
		read_num,
		read_num,
		(pred(DeclModule::in, DefModule::in, Name::in,
			Arity::in, Mode::in, ProcId::out)
			is det :-
			ProcId = ok(user_defined(PredOrFunc, DeclModule,
				DefModule, Name, Arity, Mode))
		),
		Res).

:- func raw_proc_id_to_string(proc_id) = string.

raw_proc_id_to_string(compiler_generated(TypeName, TypeModule, _DefModule,
		PredName, Arity, Mode)) =
	string__append_list(
		[PredName, " for ", TypeModule, ":", TypeName,
		"/", string__int_to_string(Arity),
		" mode ", string__int_to_string(Mode)]).
raw_proc_id_to_string(user_defined(PredOrFunc, DeclModule, _DefModule,
		Name, Arity, Mode)) =
	string__append_list([DeclModule, ":", Name,
		"/", string__int_to_string(Arity),
		( PredOrFunc = function -> "+1" ; "" ),
		"-", string__int_to_string(Mode)]).

:- func refined_proc_id_to_string(proc_id) = string.

refined_proc_id_to_string(compiler_generated(TypeName, TypeModule, _DefModule,
		RawPredName, Arity, Mode)) = Name :-
	( RawPredName = "__Unify__" ->
		PredName = "Unify"
	; RawPredName = "__Compare__" ->
		PredName = "Compare"
	; RawPredName = "__Index__" ->
		PredName = "Index"
	;
		string__append("unknown special predicate name ", RawPredName,
			Msg),
		error(Msg)
	),
	Name0 = string__append_list(
		[PredName, " for ", TypeModule, ":", TypeName,
			"/", string__int_to_string(Arity)]),
	( Mode = 0 ->
		Name = Name0
	;
		Name = string__append_list([Name0, " mode ", 
			string__int_to_string(Mode)])
	).
refined_proc_id_to_string(user_defined(PredOrFunc, DeclModule, _DefModule,
		ProcName, Arity, Mode)) = Name :-
	(
		string__append("TypeSpecOf__", ProcName1, ProcName),
		( string__append("pred__", ProcName2A, ProcName1) ->
			ProcName2 = ProcName2A
		; string__append("func__", ProcName2B, ProcName1) ->
			ProcName2 = ProcName2B
		; string__append("pred_or_func__", ProcName2C, ProcName1) ->
			ProcName2 = ProcName2C
		;
			error("typespec: neither pred nor func")
		),
		string__to_char_list(ProcName2, ProcName2Chars),
		fix_type_spec_suffix(ProcName2Chars, ProcNameChars, SpecInfo)
	->
		RefinedProcName = string__from_char_list(ProcNameChars),
		Name = string__append_list([DeclModule, ":", RefinedProcName,
			"/", string__int_to_string(Arity),
			( PredOrFunc = function -> "+1" ; "" ),
			"-", string__int_to_string(Mode),
			" [", SpecInfo, "]"])
	;
		string__append("IntroducedFrom__", ProcName1, ProcName),
		( string__append("pred__", ProcName2A, ProcName1) ->
			ProcName2 = ProcName2A
		; string__append("func__", ProcName2B, ProcName1) ->
			ProcName2 = ProcName2B
		;
			error("lambda: neither pred nor func")
		),
		string__to_char_list(ProcName2, ProcName2Chars),
		split_lambda_name(ProcName2Chars, Segments),
		glue_lambda_name(Segments, ContainingNameChars,
			LineNumberChars)
	->
		string__from_char_list(ContainingNameChars, ContainingName),
		string__from_char_list(LineNumberChars, LineNumber),
		Name = string__append_list([DeclModule, ":", ContainingName,
			" lambda line ", LineNumber,
			"/", string__int_to_string(Arity),
			( PredOrFunc = function -> "+1" ; "" )])
	;
		Name = string__append_list([DeclModule, ":", ProcName,
			"/", string__int_to_string(Arity),
			( PredOrFunc = function -> "+1" ; "" ),
			"-", string__int_to_string(Mode)])
	).

:- pred fix_type_spec_suffix(list(char)::in, list(char)::out, string::out)
	is semidet.

fix_type_spec_suffix(Chars0, Chars, SpecInfoStr) :-
	( Chars0 = ['_', '_', '[' | SpecInfo0 ] ->
		Chars = [],
		list__takewhile(non_right_bracket, SpecInfo0, SpecInfo, _),
		string__from_char_list(SpecInfo, SpecInfoStr)
	; Chars0 = [Char | TailChars0] ->
		fix_type_spec_suffix(TailChars0, TailChars, SpecInfoStr),
		Chars = [Char | TailChars]
	;
		fail
	).

:- pred non_right_bracket(char::in) is semidet.

non_right_bracket(C) :-
	C \= ']'.

:- pred split_lambda_name(list(char)::in, list(list(char))::out) is det.

split_lambda_name([], []).
split_lambda_name([Char0 | Chars0], StringList) :-
	( Chars0 = ['_', '_' | Chars1 ] ->
		split_lambda_name(Chars1, StringList0),
		StringList = [[Char0] | StringList0]
	;
		split_lambda_name(Chars0, StringList0),
		(
			StringList0 = [],
			StringList = [[Char0]]
		;
			StringList0 = [String0 | StringList1],
			StringList = [[Char0 | String0] | StringList1]
		)
	).

:- pred glue_lambda_name(list(list(char))::in, list(char)::out,
	list(char)::out) is semidet.

glue_lambda_name(Segments, PredName, LineNumber) :-
	( Segments = [LineNumberPrime, _] ->
		PredName = [],
		LineNumber = LineNumberPrime
	; Segments = [Segment | TailSegments] ->
		glue_lambda_name(TailSegments, PredName1, LineNumber),
		( PredName1 = [] ->
			PredName = Segment
		;
			list__append(Segment, ['_', '_' | PredName1], PredName)
		)
	;
		fail
	).

:- pred read_proc_dynamic(deep_result2(proc_dynamic, int)::out,
	io__state::di, io__state::uo) is det.

read_proc_dynamic(Res) -->
	% format("reading proc_dynamic.\n", []),
	read_sequence3(
		read_ptr(pd),
		read_ptr(ps),
		read_num,
		(pred(PDI0::in, PSI0::in, N0::in, Stuff0::out) is det :-
			Stuff0 = ok({PDI0, PSI0, N0})
		),
		Res1),
	(
		{ Res1 = ok({PDI, PSI, N}) },
		read_n_things(N, read_call_site_ref, Res2),
		(
			{ Res2 = ok(Refs) },
			{ PSPtr = proc_static_ptr(PSI) },
			{ ProcDynamic = proc_dynamic(PSPtr, array(Refs), no) },
			{ Res = ok2(ProcDynamic, PDI) }
		;
			{ Res2 = error(Err) },
			{ Res = error2(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error2(Err) }
	).

:- pred read_call_site_dynamic(deep_result2(call_site_dynamic, int)::out,
	io__state::di, io__state::uo) is det.

read_call_site_dynamic(Res) -->
	% format("reading call_site_dynamic.\n", []),
	read_ptr(csd, Res1),
	(
		{ Res1 = ok(CSDI) },
		read_ptr(pd, Res2),
		(
			{ Res2 = ok(PDI) },
			read_profile(Res3),
			(
				{ Res3 = ok(Profile) },
				{ PDPtr = proc_dynamic_ptr(PDI) },
				{ DummyPDPtr = proc_dynamic_ptr(-1) },
				{ CallSiteDynamic = call_site_dynamic(
					DummyPDPtr, PDPtr, Profile, no) },
				{ Res = ok2(CallSiteDynamic, CSDI) }
			;
				{ Res3 = error(Err) },
				{ Res = error2(Err) }
			)
		;
			{ Res2 = error(Err) },
			{ Res = error2(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error2(Err) }
	).

:- pred read_profile(deep_result(own_prof_info)::out,
	io__state::di, io__state::uo) is det.

read_profile(Res) -->
	read_num(Res0),
	(
		{ Res0 = ok(Mask) },
		{ MaybeError1 = no },
		% { MaybeError0 = no },
		% Calls are computed from the other counts below
		% ( { Mask /\ 0x0001 \= 0 } ->
		% 	maybe_read_num_handle_error(Calls,
		% 		MaybeError0, MaybeError1)
		% ;
		% 	{ Calls = 0 },
		% 	{ MaybeError1 = MaybeError0 }
		% ),
		( { Mask /\ 0x0002 \= 0 } ->
			maybe_read_num_handle_error(Exits,
				MaybeError1, MaybeError2)
		;
			{ Exits = 0 },
			{ MaybeError2 = MaybeError1 }
		),
		( { Mask /\ 0x0004 \= 0 } ->
			maybe_read_num_handle_error(Fails,
				MaybeError2, MaybeError3)
		;
			{ Fails = 0 },
			{ MaybeError3 = MaybeError2 }
		),
		( { Mask /\ 0x0008 \= 0 } ->
			maybe_read_num_handle_error(Redos,
				MaybeError3, MaybeError4)
		;
			{ Redos = 0 },
			{ MaybeError4 = MaybeError3 }
		),
		( { Mask /\ 0x0010 \= 0 } ->
			maybe_read_num_handle_error(Quanta,
				MaybeError4, MaybeError5)
		;
			{ Quanta = 0 },
			{ MaybeError5 = MaybeError4 }
		),
		( { Mask /\ 0x0020 \= 0 } ->
			maybe_read_num_handle_error(Mallocs,
				MaybeError5, MaybeError6)
		;
			{ Mallocs = 0 },
			{ MaybeError6 = MaybeError5 }
		),
		( { Mask /\ 0x0040 \= 0 } ->
			maybe_read_num_handle_error(Words,
				MaybeError6, MaybeError7)
		;
			{ Words = 0 },
			{ MaybeError7 = MaybeError6 }
		),
		(
			{ MaybeError7 = yes(Error) },
			{ Res = error(Error) }
		;
			{ MaybeError7 = no },
			{ Calls = Exits + Fails - Redos },
			{ Res = ok(compress_profile(Calls, Exits, Fails, Redos, 
				Quanta, Mallocs, Words)) }
		)
	;
		{ Res0 = error(Error) },
		{ Res = error(Error) }
	).

:- pred maybe_read_num_handle_error(int::out,
	maybe(string)::in, maybe(string)::out,
	io__state::di, io__state::uo) is det.

maybe_read_num_handle_error(Value, MaybeError0, MaybeError) -->
	read_num(Res),
	(
		{ Res = ok(Value) },
		{ MaybeError = MaybeError0 }
	;
		{ Res = error(Error) },
		{ Value = 0 },
		{ MaybeError = yes(Error) }
	).

:- pred read_call_site_ref(deep_result(call_site_array_slot)::out,
	io__state::di, io__state::uo) is det.

read_call_site_ref(Res) -->
	% format("reading call_site_ref.\n", []),
	read_call_site_kind(Res1),
	(
		{ Res1 = ok(Kind) },
		( { Kind = normal_call } ->
			read_ptr(csd, Res2),
			(
				{ Res2 = ok(Ptr) },
				{ CDPtr = call_site_dynamic_ptr(Ptr) },
				{ Res = ok(normal(CDPtr)) }
			;
				{ Res2 = error(Err) },
				{ Res = error(Err) }
			)
		;
			read_things(read_ptr(csd), Res2),
			(
				{ Res2 = ok(Ptrs0) },
				{ map((pred(PtrX::in, PtrY::out) is det :-
					PtrY = call_site_dynamic_ptr(PtrX)
				), Ptrs0, Ptrs) },
				{ Res = ok(multi(array(Ptrs))) }
			;
				{ Res2 = error(Err) },
				{ Res = error(Err) }
			)
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_call_site_kind(deep_result(call_site_kind)::out,
	io__state::di, io__state::uo) is det.

read_call_site_kind(Res) -->
	read_deep_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		( { Byte = token_normal_call } ->
			{ Res = ok(normal_call) }
		; { Byte = token_special_call } ->
			{ Res = ok(special_call) }
		; { Byte = token_higher_order_call } ->
			{ Res = ok(higher_order_call) }
		; { Byte = token_method_call } ->
			{ Res = ok(method_call) }
		; { Byte = token_callback } ->
			{ Res = ok(callback) }
		;
			{ format("unexpected call_site_kind %d",
				[i(Byte)], Msg) },
			{ Res = error(Msg) }
		)
		% io__write_string("call_site_kind "),
		% io__write(Res),
		% io__write_string("\n")
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_call_site_kind_and_callee(
	deep_result(call_site_kind_and_callee)::out,
	io__state::di, io__state::uo) is det.

read_call_site_kind_and_callee(Res) -->
	read_deep_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		( { Byte = token_normal_call } ->
			read_num(Res1),
			(
				{ Res1 = ok(CalleeProcStatic) },
				read_string(Res2),
				(
					{ Res2 = ok(TypeSubst) },
					{ Res = ok(normal_call(
						proc_static_ptr(
							CalleeProcStatic),
						TypeSubst)) }
				;
					{ Res2 = error(Err) },
					{ Res = error(Err) }
				)
			;
				{ Res1 = error(Err) },
				{ Res = error(Err) }
			)
		; { Byte = token_special_call } ->
			{ Res = ok(special_call) }
		; { Byte = token_higher_order_call } ->
			{ Res = ok(higher_order_call) }
		; { Byte = token_method_call } ->
			{ Res = ok(method_call) }
		; { Byte = token_callback } ->
			{ Res = ok(callback) }
		;
			{ format("unexpected call_site_kind %d",
				[i(Byte)], Msg) },
			{ Res = error(Msg) }
		)
		% io__write_string("call_site_kind_and_callee "),
		% io__write(Res),
		% io__write_string("\n")
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

%-----------------------------------------------------------------------------%

:- pred read_n_things(int, pred(deep_result(T), io__state, io__state),
		deep_result(list(T)), io__state, io__state).
:- mode read_n_things(in, pred(out, di, uo) is det, out, di, uo) is det.

read_n_things(N, ThingReader, Res) -->
	read_n_things(N, ThingReader, [], Res0),
	(
		{ Res0 = ok(Things0) },
		{ reverse(Things0, Things) },
		{ Res = ok(Things) }
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_n_things(int, pred(deep_result(T), io__state, io__state),
		list(T), deep_result(list(T)), io__state, io__state).
:- mode read_n_things(in, pred(out, di, uo) is det, in, out, di, uo) is det.

read_n_things(N, ThingReader, Things0, Res) -->
	( { N =< 0 } ->
		{ Res = ok(Things0) }
	;
		call(ThingReader, Res1),
		(
			{ Res1 = ok(Thing) },
			read_n_things(N - 1, ThingReader, [Thing|Things0], Res)
		;
			{ Res1 = error(Err) },
			{ Res = error(Err) }
		)
	).

:- pred read_things(pred(deep_result(T), io__state, io__state),
		deep_result(list(T)), io__state, io__state).
:- mode read_things(pred(out, di, uo) is det, out, di, uo) is det.

read_things(ThingReader, Res) -->
	read_things(ThingReader, [], Res).

:- pred read_things(pred(deep_result(T), io__state, io__state),
		list(T), deep_result(list(T)), io__state, io__state).
:- mode read_things(pred(out, di, uo) is det, in, out, di, uo) is det.

read_things(ThingReader, Things0, Res) -->
	read_deep_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		( { Byte = 0 } ->
			{ Res = ok(Things0) }
		;
			putback_byte(Byte),
			call(ThingReader, Res1),
			(
				{ Res1 = ok(Thing) },
				read_things(ThingReader, [Thing|Things0], Res)
			;
				{ Res1 = error(Err) },
				{ Res = error(Err) }
			)
		)
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

%-----------------------------------------------------------------------------%

:- pred read_sequence2(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(T1, T2, deep_result(T3)),
	deep_result(T3), io__state, io__state).
:- mode read_sequence2(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, out) is det,
	out, di, uo) is det.

read_sequence2(P1, P2, Combine, Res) -->
	call(P1, Res1),
	(
		{ Res1 = ok(T1) },
		call(P2, Res2),
		(
			{ Res2 = ok(T2) },
			{ call(Combine, T1, T2, Res) }
		;
			{ Res2 = error(Err) },
			{ Res = error(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_sequence3(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(deep_result(T3), io__state, io__state),
	pred(T1, T2, T3, deep_result(T4)),
	deep_result(T4), io__state, io__state).
:- mode read_sequence3(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, in, out) is det,
	out, di, uo) is det.

read_sequence3(P1, P2, P3, Combine, Res) -->
	call(P1, Res1),
	(
		{ Res1 = ok(T1) },
		call(P2, Res2),
		(
			{ Res2 = ok(T2) },
			call(P3, Res3),
			(
				{ Res3 = ok(T3) },
				{ call(Combine, T1, T2, T3, Res) }
			;
				{ Res3 = error(Err) },
				{ Res = error(Err) }
			)
		;
			{ Res2 = error(Err) },
			{ Res = error(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_sequence4(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(deep_result(T3), io__state, io__state),
	pred(deep_result(T4), io__state, io__state),
	pred(T1, T2, T3, T4, deep_result(T5)),
	deep_result(T5), io__state, io__state).
:- mode read_sequence4(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, in, in, out) is det,
	out, di, uo) is det.

read_sequence4(P1, P2, P3, P4, Combine, Res) -->
	call(P1, Res1),
	(
		{ Res1 = ok(T1) },
		call(P2, Res2),
		(
			{ Res2 = ok(T2) },
			call(P3, Res3),
			(
				{ Res3 = ok(T3) },
				call(P4, Res4),
				(
					{ Res4 = ok(T4) },
					{ call(Combine, T1, T2, T3, T4, Res) }
				;
					{ Res4 = error(Err) },
					{ Res = error(Err) }
				)
			;
				{ Res3 = error(Err) },
				{ Res = error(Err) }
			)
		;
			{ Res2 = error(Err) },
			{ Res = error(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_sequence5(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(deep_result(T3), io__state, io__state),
	pred(deep_result(T4), io__state, io__state),
	pred(deep_result(T5), io__state, io__state),
	pred(T1, T2, T3, T4, T5, deep_result(T6)),
	deep_result(T6), io__state, io__state).
:- mode read_sequence5(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, in, in, in, out) is det,
	out, di, uo) is det.

read_sequence5(P1, P2, P3, P4, P5, Combine, Res) -->
	call(P1, Res1),
	(
		{ Res1 = ok(T1) },
		call(P2, Res2),
		(
			{ Res2 = ok(T2) },
			call(P3, Res3),
			(
				{ Res3 = ok(T3) },
				call(P4, Res4),
				(
					{ Res4 = ok(T4) },
					call(P5, Res5),
					(
						{ Res5 = ok(T5) },
						{ call(Combine, T1, T2, T3, T4,
							T5, Res) }
					;
						{ Res5 = error(Err) },
						{ Res = error(Err) }
					)
				;
					{ Res4 = error(Err) },
					{ Res = error(Err) }
				)
			;
				{ Res3 = error(Err) },
				{ Res = error(Err) }
			)
		;
			{ Res2 = error(Err) },
			{ Res = error(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_sequence6(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(deep_result(T3), io__state, io__state),
	pred(deep_result(T4), io__state, io__state),
	pred(deep_result(T5), io__state, io__state),
	pred(deep_result(T6), io__state, io__state),
	pred(T1, T2, T3, T4, T5, T6, deep_result(T7)),
	deep_result(T7), io__state, io__state).
:- mode read_sequence6(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, in, in, in, in, out) is det,
	out, di, uo) is det.

read_sequence6(P1, P2, P3, P4, P5, P6, Combine, Res) -->
	call(P1, Res1),
	(
		{ Res1 = ok(T1) },
		call(P2, Res2),
		(
			{ Res2 = ok(T2) },
			call(P3, Res3),
			(
				{ Res3 = ok(T3) },
				call(P4, Res4),
				(
					{ Res4 = ok(T4) },
					call(P5, Res5),
					(
						{ Res5 = ok(T5) },
						call(P6, Res6),
						(
							{ Res6 = ok(T6) },
							{ call(Combine, T1, T2,
								T3, T4, T5,
								T6, Res) }
						;
							{ Res6 = error(Err) },
							{ Res = error(Err) }
						)
					;
						{ Res5 = error(Err) },
						{ Res = error(Err) }
					)
				;
					{ Res4 = error(Err) },
					{ Res = error(Err) }
				)
			;
				{ Res3 = error(Err) },
				{ Res = error(Err) }
			)
		;
			{ Res2 = error(Err) },
			{ Res = error(Err) }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_sequence7(
	pred(deep_result(T1), io__state, io__state),
	pred(deep_result(T2), io__state, io__state),
	pred(deep_result(T3), io__state, io__state),
	pred(deep_result(T4), io__state, io__state),
	pred(deep_result(T5), io__state, io__state),
	pred(deep_result(T6), io__state, io__state),
	pred(deep_result(T7), io__state, io__state),
	pred(T1, T2, T3, T4, T5, T6, T7, deep_result(T8)),
	deep_result(T8), io__state, io__state).
:- mode read_sequence7(
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(out, di, uo) is det,
	pred(in, in, in, in, in, in, in, out) is det,
	out, di, uo) is det.

read_sequence7(P1, P2, P3, P4, P5, P6, P7, Combine, Res) -->
	call(P1, Res1),
	(
	    { Res1 = ok(T1) },
	    call(P2, Res2),
	    (
	        { Res2 = ok(T2) },
	        call(P3, Res3),
	        (
	            { Res3 = ok(T3) },
	            call(P4, Res4),
	            (
	                { Res4 = ok(T4) },
	                call(P5, Res5),
	                (
	                    { Res5 = ok(T5) },
	                    call(P6, Res6),
	                    (
	                        { Res6 = ok(T6) },
	                        call(P7, Res7),
	                        (
	                            { Res7 = ok(T7) },
	                            { call(Combine, T1, T2, T3, T4, T5, T6, T7,
	                                Res) }
	                        ;
	                            { Res7 = error(Err) },
	                            { Res = error(Err) }
	                        )
	                    ;
	                        { Res6 = error(Err) },
	                        { Res = error(Err) }
	                    )
	                ;
	                    { Res5 = error(Err) },
	                    { Res = error(Err) }
	                )
	            ;
	                { Res4 = error(Err) },
	                { Res = error(Err) }
	            )
	        ;
	            { Res3 = error(Err) },
	            { Res = error(Err) }
	        )
	    ;
	        { Res2 = error(Err) },
	        { Res = error(Err) }
	    )
	;
	    { Res1 = error(Err) },
	    { Res = error(Err) }
	).

%-----------------------------------------------------------------------------%

:- pred read_string(deep_result(string)::out,
	io__state::di, io__state::uo) is det.

read_string(Res) -->
	read_num(Res0),
	(
		{ Res0 = ok(Length) },
		( { Length = 0 } ->
			{ Res = ok("") }
		;
			read_n_byte_string(Length, Res)
		)
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_n_byte_string(int::in, deep_result(string)::out,
	io__state::di, io__state::uo) is det.

read_n_byte_string(Length, Res) -->
	read_n_bytes(Length, Res1),
	(
		{ Res1 = ok(Bytes) },
		(
			{ map((pred(I::in, C::out) is semidet :-
				char__to_int(C, I)
			), Bytes, Chars) }
		->
			{ string__from_char_list(Chars, Str) },
			{ Res = ok(Str) }
		;
			{ Res = error("string contained bad char") }
		)
	;
		{ Res1 = error(Err) },
		{ Res = error(Err) }
	).
	% io__write_string("string "),
	% io__write(Res),
	% io__write_string("\n")

:- pred read_ptr(ptr_kind::in, deep_result(int)::out,
	io__state::di, io__state::uo) is det.

read_ptr(_Kind, Res) -->
	read_num1(0, Res).
	% io__write_string("ptr "),
	% io__write(Res),
	% io__write_string("\n").

:- pred read_num(deep_result(int)::out, io__state::di, io__state::uo) is det.

read_num(Res) -->
	read_num1(0, Res).
	% io__write_string("num "),
	% io__write(Res),
	% io__write_string("\n").

:- pred read_num1(int::in, deep_result(int)::out,
	io__state::di, io__state::uo) is det.

read_num1(Num0, Res) -->
	read_byte(Res0),
	(
		{ Res0 = ok(Byte) },
		{ Num1 = (Num0 << 7) \/ (Byte /\ 0x7F) },
		( { Byte /\ 0x80 \= 0 } ->
			read_num1(Num1, Res)
		;
			{ Res = ok(Num1) }
		)
	;
		{ Res0 = eof },
		{ Res = error("unexpected end of file") }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error(Msg) }
	).

:- func fixed_size_int_bytes = int.

% Must correspond to MR_FIXED_SIZE_INT_BYTES
% in runtime/mercury_deep_profiling.c.

fixed_size_int_bytes = 4.

:- pred read_fixed_size_int(deep_result(int)::out,
	io__state::di, io__state::uo) is det.

read_fixed_size_int(Res) -->
	read_fixed_size_int1(fixed_size_int_bytes, 0, 0, Res).

:- pred read_fixed_size_int1(int::in, int::in, int::in, deep_result(int)::out,
	io__state::di, io__state::uo) is det.

read_fixed_size_int1(BytesLeft, Num0, ShiftBy, Res) -->
	( { BytesLeft =< 0 } ->
		{ Res = ok(Num0) }
	;
		read_deep_byte(Res0),
		(
			{ Res0 = ok(Byte) },
			{ Num1 = Num0 \/ ( Byte << ShiftBy) },
			read_fixed_size_int1(BytesLeft - 1, Num1, ShiftBy + 8,
				Res)
		;
			{ Res0 = error(Err) },
			{ Res = error(Err) }
		)
	).

:- pred read_n_bytes(int::in, deep_result(list(int))::out,
	io__state::di, io__state::uo) is det.

read_n_bytes(N, Res) -->
	read_n_bytes(N, [], Res0),
	(
		{ Res0 = ok(Bytes0) },
		{ reverse(Bytes0, Bytes) },
		{ Res = ok(Bytes) }
	;
		{ Res0 = error(Err) },
		{ Res = error(Err) }
	).

:- pred read_n_bytes(int::in, list(int)::in, deep_result(list(int))::out,
	io__state::di, io__state::uo) is det.

read_n_bytes(N, Bytes0, Res) -->
	( { N =< 0 } ->
		{ Res = ok(Bytes0) }
	;
		read_deep_byte(Res0),
		(
			{ Res0 = ok(Byte) },
			read_n_bytes(N - 1, [Byte | Bytes0], Res)
		;
			{ Res0 = error(Err) },
			{ Res = error(Err) }
		)
	).

:- pred read_deep_byte(deep_result(int)::out,
	io__state::di, io__state::uo) is det.

read_deep_byte(Res) -->
	read_byte(Res0),
	% io__write_string("byte "),
	% io__write(Res),
	% io__write_string("\n"),
	(
		{ Res0 = ok(Byte) },
		{ Res = ok(Byte) }
	;
		{ Res0 = eof },
		{ Res = error("unexpected end of file") }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error(Msg) }
	).

%------------------------------------------------------------------------------%

:- pred deep_insert(array(T)::in, int::in, T::in, array(T)::out) is det.

deep_insert(A0, Ind, Thing, A) :-
	array__max(A0, Max),
	( Ind > Max ->
		array__lookup(A0, 0, X),
		array__resize(u(A0), 2 * (Max + 1), X, A1),
		deep_insert(A1, Ind, Thing, A)
	;
		set(u(A0), Ind, Thing, A)
	).

%------------------------------------------------------------------------------%

:- pragma c_header_code("
#include ""mercury_deep_profiling.h""
").

:- func token_root = int.
:- pragma c_code(token_root = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_root;").

:- func token_call_site_static = int.
:- pragma c_code(token_call_site_static = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_call_site_static;").

:- func token_call_site_dynamic = int.
:- pragma c_code(token_call_site_dynamic = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_call_site_dynamic;").

:- func token_proc_static = int.
:- pragma c_code(token_proc_static = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_proc_static;").

:- func token_proc_dynamic = int.
:- pragma c_code(token_proc_dynamic = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_proc_dynamic;").

:- func token_normal_call = int.
:- pragma c_code(token_normal_call = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_normal_call;").

:- func token_special_call = int.
:- pragma c_code(token_special_call = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_special_call;").

:- func token_higher_order_call = int.
:- pragma c_code(token_higher_order_call = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_higher_order_call;").

:- func token_method_call = int.
:- pragma c_code(token_method_call = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_method_call;").

:- func token_callback = int.
:- pragma c_code(token_callback = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_callback;").

:- func token_isa_predicate = int.
:- pragma c_code(token_isa_predicate = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_isa_predicate;").

:- func token_isa_function = int.
:- pragma c_code(token_isa_function = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_isa_function;").

:- func token_isa_compiler_generated = int.
:- pragma c_code(token_isa_compiler_generated = (X::out),
	[will_not_call_mercury, thread_safe],
	"X = MR_deep_token_isa_compiler_generated;").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pred resize_arrays(deep_result2(initial_deep, ptr_info)::in,
	maybe_error(initial_deep)::out) is det.

resize_arrays(error2(Err), error(Err)).
resize_arrays(ok2(InitDeep0, PI), ok(InitDeep)) :-
	PI ^ csd = CSDMax,
	CSDs0 = InitDeep0 ^ init_call_site_dynamics,
	array__lookup(CSDs0, 0, CSDx),
	array__resize(u(CSDs0), CSDMax + 1, CSDx, CSDs),
	InitDeep1 = InitDeep0 ^ init_call_site_dynamics := CSDs,

	PI ^ pd = PDMax,
	PDs0 = InitDeep1 ^ init_proc_dynamics,
	array__lookup(PDs0, 0, PDx),
	array__resize(u(PDs0), PDMax + 1, PDx, PDs),
	InitDeep2 = InitDeep1 ^ init_proc_dynamics := PDs,

	PI ^ css = CSSMax,
	CSSs0 = InitDeep2 ^ init_call_site_statics,
	array__lookup(CSSs0, 0, CSSx),
	array__resize(u(CSSs0), CSSMax + 1, CSSx, CSSs),
	InitDeep3 = InitDeep2 ^ init_call_site_statics := CSSs,

	PI ^ ps = PSMax,
	PSs0 = InitDeep3 ^ init_proc_statics,
	array__lookup(PSs0, 0, PSx),
	array__resize(u(PSs0), PSMax + 1, PSx, PSs),
	InitDeep4 = InitDeep3 ^ init_proc_statics := PSs,

	ProfileStats0 = InitDeep4 ^ init_profile_stats,
	ProfileStats0 = profile_stats(InstrumentQuanta, UserQuanta,
		_, _, _, _, TicksPerSec),
	ProfileStats = profile_stats(InstrumentQuanta, UserQuanta,
		CSDMax, PDMax, CSSMax, PSMax, TicksPerSec),
	InitDeep = InitDeep4 ^ init_profile_stats := ProfileStats.

%-----------------------------------------------------------------------------%
