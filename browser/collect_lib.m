%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: collect_lib.m
% Author: jahier
% Purpose:
%	This module defines functions that are needed to implement the
%	`collect' primitive.
%
% 	`collect' collects runtime information from Mercury program executions.
% 	It is intended to let users easily implement their own monitors with
% 	acceptable performances.
%
%	To use it, users just need to define 4 things in a file, using the
%	Mercury syntax:
%		1) `accumulator_type' which is the type of the accumulator.
%		2) The predicate initialize/1 which initializes the
%		   collecting variable. initialize/1 should respect the
%		   following declarations:
%			:- pred initialize(accumulator_type).
%			:- mode initialize(out) is det.
%		3) The predicate filter/4 which updates the collecting
%		   variable at each execution event. filter/4 also outputs
%		   a variable that indicates whether to stop collecting.
%		   If this variable is set to `stop', the collect process
%		   stops; if it is set to `continue', it continues. If this
%		   variable is always set to `continue', the collecting will
%		   process until the last event is reached. filter/4 should
%		   respect the following declarations:
%			:- pred filter(event, accumulator_type, accumulator_type,
%				stop_or_continue).
%			:- mode filter(in, di, uo, out) is det.
%               4) Optionally, a `post_process/2' function that lets
%   	           one post-process the final value of the
%   	           accumulator. `post_process/2' should respect the
%   	           following declarations:
%  	                :- pred post_process(accumulator_type, collected_type).
%	                :- mode post_process(in, out) is det.
% 	           If `collected_type' is different from `accumulator_type',
% 	           the `accumulator_type' should also be defined; otherwise
% 	           by default, `accumulator_type' is automatically defined as
% 	           being the same type as `collected_type'.
%		5) And optionally the mode definition of the second and the
%		   third arguments of filter/4: `acc_in' and `acc_out'. Those
%		   mode have `di' and `uo' respectively as default values.
%
% 	Then, this file is used to generate the Mercury module `collect.m',
% 	which is compiled and dynamically linked with the current execution.
% 	When a `collect' request is made from the external debugger, a variable
% 	of type accumulator_type is first initialized (with initialize/1) and
% 	then updated (with filter/4) for all the events of the remaining
% 	execution. When the end of the execution is reached, the last value of
% 	the collecting variable is send to the debugger.

:- module mdb__collect_lib.
:- interface.

:- pred dummy_pred_to_avoid_warning_about_nothing_exported is det.

%------------------------------------------------------------------------------%
:- implementation.
:- import_module int, list, std_util, io, char.
:- import_module mdb__dl.

dummy_pred_to_avoid_warning_about_nothing_exported.

:- pragma export(link_collect(in, out, out, out, out, out, out, out, di, uo),
	"ML_CL_link_collect").

:- pragma export(unlink_collect(in, di, uo), "ML_CL_unlink_collect").

% We need Handle to be able to close the shared object (dl__close) later on.
% When the link failed, we output NULL pointers instead of maybe pointers
% for performance reasons; indeed, filter will be called at every event
% so we don't want to pay the price of the maybe variable de-construction
% at each event.

% dynamically link the collect module;
:- pred link_collect(string, c_pointer, c_pointer, c_pointer, c_pointer,
	c_pointer, dl__result(handle), char, io__state, io__state).
:- mode link_collect(in, out, out, out, out, out, out, out, di, uo) is det.

link_collect(ObjectFile, Filter, Initialize, PostProcess, SendResult,
		GetCollectType, MaybeHandle, Result) -->
	%
	% Link in the object code for the module `collect' from ObjectFile.
	%
	dl__open(ObjectFile, lazy, local, MaybeHandle),
	(	
		{ MaybeHandle = error(Msg) },
		print("dlopen failed: "), print(Msg), nl,
		{ set_to_null_pointer(Initialize) },
		{ set_to_null_pointer(Filter) },
		{ set_to_null_pointer(PostProcess) },
		{ set_to_null_pointer(SendResult) },
		{ set_to_null_pointer(GetCollectType) },
		{ Result = 'n' }
	;
		{ MaybeHandle = ok(Handle) },
 		%
 		% Look up the address of the C functions corresponding to the
		% initialize/1 and filter/15 predicates in the collect module.
 		%
		dl__sym(Handle, "ML_COLLECT_initialize", MaybeInitialize),
		dl__sym(Handle, "ML_COLLECT_filter", MaybeFilter),
		dl__sym(Handle, "ML_COLLECT_post_process", MaybePostProcess),
		dl__sym(Handle, "ML_COLLECT_send_collect_result", MaybeSendResult),
		dl__sym(Handle, "ML_COLLECT_collected_variable_type", MaybeType),
		(
			{ MaybeInitialize = ok(Initialize0) },
			{ MaybeFilter = ok(Filter0) },
			{ MaybePostProcess = ok(PostProcess0) },
			{ MaybeSendResult = ok(SendResult0) },
			{ MaybeType = ok(Type0) }
		->
			{ Result = 'y' },
			{ Initialize = Initialize0 },
			{ Filter = Filter0 },
			{ PostProcess = PostProcess0 },
			{ GetCollectType = Type0 },
			{ SendResult = SendResult0 }
		;
			{ set_to_null_pointer(Initialize) },
			{ set_to_null_pointer(Filter) },
			{ set_to_null_pointer(PostProcess) },
			{ set_to_null_pointer(SendResult) },
			{ set_to_null_pointer(GetCollectType) },
			{ Result = 'n' }
		)
	).

:- pred set_to_null_pointer(c_pointer::out) is det.
:- pragma foreign_proc("C", set_to_null_pointer(Pointer::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"(Pointer = (MR_Word) NULL)").

%------------------------------------------------------------------------------%

:- pred unlink_collect(dl__result(handle), io__state, io__state).
:- mode unlink_collect(in, di, uo) is det.
 	% Dynamically unlink a module that was dynamically linked in
 	% using `link_collect'.
unlink_collect(MaybeHandle) -->
	(
		{ MaybeHandle = error(_) }
		% There is nothing to close since an error(_) here means that
		% the dlopen failed.
	;
		{ MaybeHandle = ok(Handle) },
		dl__close(Handle, Result),
		display_close_result(Result)
	).

:- pred display_close_result(dl__result, io__state, io__state).
:- mode display_close_result(in, di, uo) is det.

display_close_result(ok) --> [].
display_close_result(error(String)) -->
	print(String),
	nl.
