%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pragma_c_gen.m
%
% Main authors: dgj, conway, zs.
%
% The code in this module generates code for pragma_c_code goals.
%
% The schemes we use to generate code for model_det and model_semi
% pragma_c_codes are quite similar, so we handle them together.
% The code that does this is reasonably simple.
%
% The scheme for model_non pragma_c_codes is substantially different,
% so we handle them seperately.

:- module pragma_c_gen.

:- interface.

:- import_module hlds_goal, hlds_pred, prog_data.
:- import_module llds, code_info.
:- import_module list, std_util.

:- pred pragma_c_gen__generate_pragma_c_code(code_model::in, string::in,
	may_call_mercury::in, pred_id::in, proc_id::in, list(var)::in,
	list(maybe(string))::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred pragma_c_gen__generate_backtrack_pragma_c_code(code_model::in,
	string::in, may_call_mercury::in, pred_id::in, proc_id::in,
	list(var)::in, list(maybe(string))::in, list(pair(var, string))::in,
	list(string)::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is erroneous.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_pred, call_gen, tree.
:- import_module string, assoc_list, set, map, require.

% The code we generate for a model_det or model_semi pragma_c_code
% must be able to fit into the middle of a procedure, since such
% pragma_c_codes can be inlined. It is of the following form:
%
% <save live variables onto the stack> /* see note (1) below */
% {
%	<declaration of one local variable for each arg>
%	<assignment of input values from registers to local variables>
%	save_registers(); /* see notes (1) and (2) below */
%	{ <the c code itself> }
%	#ifndef CONSERVATIVE_GC
%	  restore_registers(); /* see notes (1) and (3) below */
%	#endif
%	<assignment of the output values from local variables to registers>
% }
%
% In the case of a semidet pragma c_code, this is followed by
%
%	if (r1) goto label;
%	<code to fail>
%	label:
%
% Notes:
%
% (1)	These parts are only emitted if the C code may call Mercury.
%	If a pragma c_code(will_not_call_mercury, ...) declaration was used,
%	they will not be emitted.
%
% (2)	The call to save_registers() is needed so that if the
%	C code calls Mercury code, we can call restore_registers()
%	on entry to the Mercury code (see export.m) to get the
%	right values of `sp', `hp', `curfr' and `maxfr' for the
%	recursive invocation of Mercury.
%
% (3)	The call to restore_registers() is needed in case the
%	C code calls Mercury code which allocates some data
%	on the heap, and this data is returned from Mercury
%	through C back to Mercury.  In that case, we need to
%	keep the value of `hp' that was set by the recursive
%	invocation of Mercury.  The Mercury calling convention
%	guarantees that the values of `sp', `curfr', and `maxfr'
%	will be preserved, so if we're using conservative gc,
%	there is nothing that needs restoring.

pragma_c_gen__generate_pragma_c_code(CodeModel, C_Code, MayCallMercury,
		PredId, ProcId, Args, Names, _GoalInfo, Code) -->
	% First we need to get a list of input and output arguments
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo),
	{ make_c_arg_list(Args, Names, ArgNames) },
	{ assoc_list__from_corresponding_lists(ArgNames, ArgInfo, ArgModes) },
	{ pragma_select_in_args(ArgModes, InArgs) },
	{ pragma_select_out_args(ArgModes, OutArgs) },

	( { MayCallMercury = will_not_call_mercury } ->
		{ SaveVarsCode = empty }
	;
		% the C code might call back Mercury code
		% which clobbers the succip
		code_info__succip_is_used,

		% the C code might call back Mercury code which clobbers the
		% other registers, so we need to save any live variables
		% (other than the output args) onto the stack
		{ get_c_arg_list_vars(OutArgs, OutArgs1) },
		{ set__list_to_set(OutArgs1, OutArgsSet) },
		call_gen__save_variables(OutArgsSet, SaveVarsCode)
	),

	make_pragma_decls(ArgNames, Decls),
	get_pragma_input_vars(InArgs, Inputs, InputVarsCode),
	( { CodeModel = model_semi } ->
		% We have to clear r1 for C code that gets inlined
		% so that it is safe to assign to SUCCESS_INDICATOR.
		code_info__clear_r1(ShuffleR1_Code),

		( { MayCallMercury = will_not_call_mercury } ->
			[]
		;
			% the C code may call Mercury code which clobbers
			% the regs
			code_info__clear_all_registers
		),

		% C code goes here

		code_info__get_next_label(SkipLab),
		code_info__generate_failure(FailCode),
		{ CheckFailureCode = tree(node([
			if_val(lval(reg(r, 1)), label(SkipLab)) -
				"Test for success of pragma_c_code"
			]), tree(FailCode, node([ label(SkipLab) - "" ])))
		},

		code_info__lock_reg(reg(r, 1)),
		pragma_acquire_regs(OutArgs, Regs),
		code_info__unlock_reg(reg(r, 1))
	;
		{ ShuffleR1_Code = empty },

		% c code goes here

		( { MayCallMercury = will_not_call_mercury } ->
			[]
		;
			% the C code may call Mercury code which clobbers
			% the regs
			code_info__clear_all_registers
		),

		{ CheckFailureCode = empty },

		pragma_acquire_regs(OutArgs, Regs)
	),
	place_pragma_output_args_in_regs(OutArgs, Regs, Outputs),

	( { MayCallMercury = will_not_call_mercury } ->
		{ Wrapped_C_Code = C_Code }
	;
		{ string__append_list([
				"\tsave_registers();\n{\n",
				C_Code, "\n}\n",
				"#ifndef CONSERVATIVE_GC\n",
				"\trestore_registers();\n",
				"#endif\n"
			], Wrapped_C_Code) }
	),

	% The context in the goal_info we are given is the context of the
	% call to the predicate whose definition is a pragma_c_code.
	% The context we want to put into the LLDS code we generate
	% is the context of the pragma_c_code line in the definition
	% of that predicate.
	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_goal(ProcInfo, OrigGoal) },
	{ OrigGoal = _ - OrigGoalInfo },
	{ goal_info_get_context(OrigGoalInfo, Context) },

	{ PragmaCode = node([
		pragma_c(Decls, Inputs, Wrapped_C_Code, Outputs, Context) - 
			"Pragma C inclusion"
	]) },
	{ Code =
		tree(SaveVarsCode,
		tree(InputVarsCode,
		tree(ShuffleR1_Code, 
		tree(PragmaCode,
		     CheckFailureCode))))
	}.

%---------------------------------------------------------------------------%

:- type c_arg	--->	c_arg(var, maybe(string)).

:- pred make_c_arg_list(list(var)::in, list(maybe(string))::in,
	list(c_arg)::out) is det.

make_c_arg_list(Vars, Names, ArgNames) :-
	make_c_arg_list_2(Vars, Names, [], ArgNames0),
	list__reverse(ArgNames0, ArgNames).

:- pred make_c_arg_list_2(list(var)::in, list(maybe(string))::in,
	list(c_arg)::in, list(c_arg)::out) is det.

make_c_arg_list_2([], [], ArgNames, ArgNames).
make_c_arg_list_2([Var | Vars], [Name | Names], ArgNames0, ArgNames) :-
	make_c_arg_list_2(Vars, Names, [c_arg(Var, Name) | ArgNames0],
		ArgNames).

make_c_arg_list_2([], [_ | _], _, _) :-
	error("pragma_c_gen:make_c_arg_list_2 - length mismatch").
make_c_arg_list_2([_ | _], [], _, _) :-
	error("pragma_c_gen:make_c_arg_list_2 - length mismatch").

:- pred get_c_arg_list_vars(list(c_arg)::in, list(var)::out) is det.

get_c_arg_list_vars([], []).
get_c_arg_list_vars([c_arg(Var, _) | Args], [Var | Vars1]) :-
	get_c_arg_list_vars(Args, Vars1).

%---------------------------------------------------------------------------%

% pragma_select_out_args returns the list of variables which are outputs for
% a procedure

:- pred pragma_select_out_args(assoc_list(c_arg, arg_info)::in,
	list(c_arg)::out) is det.

pragma_select_out_args([], []).
pragma_select_out_args([V - arg_info(_Loc, Mode) | Rest], Out) :-
        pragma_select_out_args(Rest, Out0),
        (
                Mode = top_out
        ->
                Out = [V | Out0]
        ;
                Out = Out0
        ).

% pragma_select_in_args returns the list of variables which are inputs for
% a procedure

:- pred pragma_select_in_args(assoc_list(c_arg, arg_info)::in,
	list(c_arg)::out) is det.

pragma_select_in_args([], []).
pragma_select_in_args([V - arg_info(_Loc, Mode) | Rest], In) :-
        pragma_select_in_args(Rest, In0),
        (
                Mode = top_in
        ->
		In = [V | In0]
        ;
                In = In0
        ).

%---------------------------------------------------------------------------%

% make_pragma_decls returns the list of pragma_decls for the pragma_c
% data structure in the llds. It is essentially a list of pairs of type and
% variable name, so that declarations of the form "Type Name;" can be made.

:- pred make_pragma_decls(list(c_arg)::in, list(pragma_c_decl)::out,
	code_info::in, code_info::out) is det.

make_pragma_decls([], []) --> [].
make_pragma_decls([c_arg(Arg, ArgName) | ArgNames], Decls) -->
	( { ArgName = yes(Name) } ->
		code_info__variable_type(Arg, Type),
		{ Decl = pragma_c_decl(Type, Name) },
		{ Decls = [Decl | Decls1] },
		make_pragma_decls(ArgNames, Decls1)
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		make_pragma_decls(ArgNames, Decls)
	).

%---------------------------------------------------------------------------%

% get_pragma_input_vars returns a list of pragma_c_inputs for the pragma_c
% data structure in the llds. It is essentially a list of the input variables,
% and the corresponding rvals assigned to those (C) variables.

:- pred get_pragma_input_vars(list(c_arg)::in, list(pragma_c_input)::out,
	code_tree::out, code_info::in, code_info::out) is det.

get_pragma_input_vars([], [], empty) --> [].
get_pragma_input_vars([c_arg(Arg, MaybeName) | Args], Inputs, Code) -->
	( { MaybeName = yes(Name) } ->
		code_info__variable_type(Arg, Type),
		code_info__produce_variable(Arg, Code0, Rval),
		{ Input = pragma_c_input(Name, Type, Rval) },
		{ Inputs = [Input | Inputs1] },
		{ Code = tree(Code0, Code1) },
		get_pragma_input_vars(Args, Inputs1, Code1)
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		get_pragma_input_vars(Args, Inputs, Code)
	).

%---------------------------------------------------------------------------%

% pragma_acquire_regs acquires a list of registers in which to place each
% of the given variables.

:- pred pragma_acquire_regs(list(c_arg)::in, list(lval)::out,
	code_info::in, code_info::out) is det.

pragma_acquire_regs([], []) --> [].
pragma_acquire_regs([c_arg(V, _) | Vars], [Reg | Regs]) -->
	code_info__acquire_reg_for_var(V, Reg),
	pragma_acquire_regs(Vars, Regs).

%---------------------------------------------------------------------------%

% place_pragma_output_args_in_regs returns a list of pragma_c_outputs, which
% are pairs of names of output registers and (C) variables which hold the
% output value.

:- pred place_pragma_output_args_in_regs(list(c_arg)::in, list(lval)::in,
	list(pragma_c_output)::out, code_info::in, code_info::out) is det.

place_pragma_output_args_in_regs([], [], []) --> [].
place_pragma_output_args_in_regs([_X | _Xs], [], []) --> 
	{ error("place_pragma_output_args_in_regs: list length mismatch") }.
place_pragma_output_args_in_regs([], [_X | _Xs], []) -->
	{ error("place_pragma_output_args_in_regs: list length mismatch") }.
place_pragma_output_args_in_regs([Arg | Args], [Reg | Regs], [O | Outputs]) -->
	( { Arg = c_arg(A, yes(Name)) } ->
		code_info__variable_type(A, Type),
		code_info__release_reg(Reg),
		code_info__set_var_location(A, Reg),
		{ O = pragma_c_output(Reg, Type, Name) },
		place_pragma_output_args_in_regs(Args, Regs, Outputs)
	;
		{ error("place_pragma_output_args_in_regs") }
	).

%---------------------------------------------------------------------------%

pragma_c_gen__generate_backtrack_pragma_c_code(_, _, _, _, _, _, _, _, _,
		_, _) -->
	{ error("nondet pragma_c_codes not yet implemented") }.

%---------------------------------------------------------------------------%
