%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_code_util.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It defines the ml_gen_info type and its access routines.

%-----------------------------------------------------------------------------%

:- module ml_code_util.
:- interface.

:- import_module prog_data.
:- import_module hlds_module, hlds_pred.
:- import_module builtin_ops, rtti, code_model.
:- import_module mlds.
:- import_module globals.

:- import_module bool, int, list, map, std_util.

%-----------------------------------------------------------------------------%
%
% Various utility routines used for MLDS code generation.
%

	% Generate an MLDS assignment statement.
:- func ml_gen_assign(mlds__lval, mlds__rval, prog_context) = mlds__statement.

	%
	% Append an appropriate `return' statement for the given code_model
	% and returning the given lvals, if needed.
	%
:- pred ml_append_return_statement(code_model, list(mlds__lval), prog_context,
		mlds__statements, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_append_return_statement(in, in, in, in, out, in, out) is det.

	% Generate a block statement, i.e. `{ <Decls>; <Statements>; }'.
	% But if the block consists only of a single statement with no
	% declarations, then just return that statement.
	%
:- func ml_gen_block(mlds__defns, mlds__statements, prog_context) =
		mlds__statement.

	% ml_join_decls:
	% 	Join two statement lists and their corresponding
	% 	declaration lists in sequence.
	% 
	% 	If the statements have no declarations in common,
	% 	then their corresponding declaration lists will be
	% 	concatenated together into a single list of declarations.
	% 	But if they have any declarations in common, then we
	% 	put each statement list and its declarations into
	% 	a block, so that the declarations remain local to
	% 	each statement list.
	% 
:- pred ml_join_decls(mlds__defns, mlds__statements,
		mlds__defns, mlds__statements, prog_context,
		mlds__defns, mlds__statements).
:- mode ml_join_decls(in, in, in, in, in, out, out) is det.

	% ml_combine_conj:
	%	Given closures to generate code for two conjuncts,
	%	generate code for their conjunction.

:- type gen_pred == pred(mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- inst gen_pred = (pred(out, out, in, out) is det).

:- pred ml_combine_conj(code_model, prog_context, gen_pred, gen_pred,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_combine_conj(in, in, in(gen_pred), in(gen_pred),
		out, out, in, out) is det.

	% Given a function label and the statement which will comprise
	% the function body for that function, generate an mlds__defn
	% which defines that function.
	%
:- pred ml_gen_nondet_label_func(ml_label_func, prog_context,
		mlds__statement, mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_nondet_label_func(in, in, in, out, in, out) is det.

	% Given a function label, the function parameters, and the statement
	% which will comprise the function body for that function,
	% generate an mlds__defn which defines that function.
	%
:- pred ml_gen_label_func(ml_label_func, mlds__func_params, prog_context,
		mlds__statement, mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_label_func(in, in, in, in, out, in, out) is det.

	%
	% Test to see if the procedure is 
	% a model_det function whose function result has an output mode
	% (whose type is not a dummy argument type like io__state),
	% and if so, bind RetVar to the procedure's return value.
	% These procedures need to handled specially: for such functions,
	% we map the Mercury function result to an MLDS return value.
	%
:- pred ml_is_output_det_function(module_info, pred_id, proc_id, prog_var).
:- mode ml_is_output_det_function(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%
%
% Routines for generating types.
%

	% A convenient abbreviation.
	%
:- type prog_type == prog_data__type.

	% Convert a Mercury type to an MLDS type.
	%
:- pred ml_gen_type(prog_type, mlds__type, ml_gen_info, ml_gen_info).
:- mode ml_gen_type(in, out, in, out) is det.

	% Convert the element type for an array_index operator
	% to an MLDS type.
	%
:- func ml_gen_array_elem_type(builtin_ops__array_elem_type) = mlds__type.

	% Return the MLDS type corresponding to a Mercury string type.
	%
:- func ml_string_type = mlds__type.

%-----------------------------------------------------------------------------%
%
% Routines for generating function declarations (i.e. mlds__func_params).
%

	% Generate the function prototype for a given procedure.
	%
:- func ml_gen_proc_params(module_info, pred_id, proc_id) = mlds__func_params.

:- func ml_gen_proc_params_from_rtti(module_info, rtti_proc_label) =
	mlds__func_params.

	% Generate the function prototype for a procedure with the
	% given argument types, modes, and code model.
	%
:- func ml_gen_params(module_info, list(mlds__var_name), list(prog_type),
		list(mode), pred_or_func, code_model) = mlds__func_params.

	% Given a list of variables and their corresponding modes,
	% return a list containing only those variables which have
	% an output mode.
	%
:- func select_output_vars(module_info, list(Var), list(mode),
		map(Var, prog_type)) = list(Var).

%-----------------------------------------------------------------------------%
%
% Routines for generating labels and entity names.
%

	% Generate the mlds__entity_name and module name for the entry point
	% function corresponding to a given procedure.
	%
:- pred ml_gen_proc_label(module_info, pred_id, proc_id,
		mlds__entity_name, mlds_module_name).
:- mode ml_gen_proc_label(in, in, in, out, out) is det.

	% Generate an mlds__entity_name for a continuation function
	% with the given sequence number.  The pred_id and proc_id
	% specify the procedure that this continuation function
	% is part of.
	%
:- func ml_gen_nondet_label(module_info, pred_id, proc_id, ml_label_func)
		= mlds__entity_name.

	% Allocate a new function label and return an rval containing
	% the function's address.  If parameters are not given, we
	% assume it's a continuation function, and give it the
	% appropriate arguments (depending on whether we are doing
	% nested functions or not).
	%
:- pred ml_gen_new_func_label(maybe(mlds__func_params), ml_label_func,
	mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_new_func_label(in, out, out, in, out) is det.

	% Generate the mlds__pred_label and module name
	% for a given procedure.
	%
:- pred ml_gen_pred_label(module_info, pred_id, proc_id,
		mlds__pred_label, mlds_module_name).
:- mode ml_gen_pred_label(in, in, in, out, out) is det.

:- pred ml_gen_pred_label_from_rtti(module_info, rtti_proc_label,
		mlds__pred_label, mlds_module_name).
:- mode ml_gen_pred_label_from_rtti(in, in, out, out) is det.

	% Allocate a new label name, for use in label statements.
	%
:- pred ml_gen_new_label(mlds__label, ml_gen_info, ml_gen_info).
:- mode ml_gen_new_label(out, in, out) is det.

%-----------------------------------------------------------------------------%
%
% Routines for dealing with variables
%

	% Generate a list of the mlds__lvals corresponding to a
	% given list of prog_vars.
	%
:- pred ml_gen_var_list(list(prog_var), list(mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_var_list(in, out, in, out) is det.

	% Generate the mlds__lval corresponding to a given prog_var.
	%
:- pred ml_gen_var(prog_var, mlds__lval, ml_gen_info, ml_gen_info).
:- mode ml_gen_var(in, out, in, out) is det.

	% Generate the mlds__lval corresponding to a given prog_var,
	% with a given type.
	%
:- pred ml_gen_var_with_type(prog_var, prog_type, mlds__lval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_var_with_type(in, in, out, in, out) is det.

	% Lookup the types of a list of variables.
	%
:- pred ml_variable_types(list(prog_var), list(prog_type),
		ml_gen_info, ml_gen_info).
:- mode ml_variable_types(in, out, in, out) is det.

	% Lookup the type of a variable.
	%
:- pred ml_variable_type(prog_var, prog_type, ml_gen_info, ml_gen_info).
:- mode ml_variable_type(in, out, in, out) is det.

	% Generate the MLDS variable names for a list of variables.
	%
:- func ml_gen_var_names(prog_varset, list(prog_var)) = list(mlds__var_name).

	% Generate the MLDS variable name for a variable.
	%
:- func ml_gen_var_name(prog_varset, prog_var) = mlds__var_name.

	% Generate an lval from the variable name and type. The variable
	% name will be qualified with the current module name.
	%
:- pred ml_gen_var_lval(mlds__var_name, mlds__type, mlds__lval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_var_lval(in, in, out, in, out) is det.

	% Generate a declaration for an MLDS variable, given its HLDS type.
	%
:- func ml_gen_var_decl(var_name, prog_type, mlds__context, module_info) =
	mlds__defn.

	% Generate a declaration for an MLDS variable, given its MLDS type.
	%
:- func ml_gen_mlds_var_decl(mlds__data_name, mlds__type, mlds__context) =
	mlds__defn.

	% Generate a declaration for an MLDS variable, given its MLDS type
	% and initializer.
	%
:- func ml_gen_mlds_var_decl(mlds__data_name, mlds__type, mlds__initializer,
	mlds__context) = mlds__defn.

	% Generate declaration flags for a local variable
	%
:- func ml_gen_local_var_decl_flags = mlds__decl_flags.
	
	% Generate declaration flags for a public field
	% of a class.
	%
:- func ml_gen_public_field_decl_flags = mlds__decl_flags.

	% Apply the usual %s_%d formatting to a MLDS variable name.
:- func ml_var_name_to_string(mlds__var_name) = string.

%-----------------------------------------------------------------------------%
%
% Routines for dealing with static constants
%

	% Generate a name for a local static constant.
	%
:- pred ml_format_static_const_name(string, const_seq, mlds__var_name,
		ml_gen_info, ml_gen_info).
:- mode ml_format_static_const_name(in, in, out, in, out) is det.

	% Generate a definition of a local static constant,
	% given the constant's name, type, and initializer.
	%
:- func ml_gen_static_const_defn(mlds__var_name, mlds__type, mlds__initializer,
		prog_context) = mlds__defn.

	% Return the declaration flags appropriate for an
	% initialized local static constant.
	%
:- func ml_static_const_decl_flags = mlds__decl_flags.

	% Succeed iff the specified mlds__defn defines
	% a static constant.
	%
:- pred ml_decl_is_static_const(mlds__defn::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Routines for dealing with fields
%

	% Given the user-specified field name, if any,
	% and the argument number (starting from one),
	% generate an MLDS field name.
:- func ml_gen_field_name(maybe(ctor_field_name), int) = mlds__field_name.

	% Succeed iff the specified type must be boxed when used as a field.
	% We need to box types that are not word-sized, because the code
	% for `arg' etc. in std_util.m rely on all arguments being word-sized.
:- pred ml_must_box_field_type(prog_type, module_info).
:- mode ml_must_box_field_type(in, in) is semidet.

%-----------------------------------------------------------------------------%
%
% Routines for handling success and failure
%

	% Generate code to succeed in the given code_model.
	%
:- pred ml_gen_success(code_model, prog_context, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_success(in, in, out, in, out) is det.

	% Generate code to fail in the given code_model.
	%
:- pred ml_gen_failure(code_model, prog_context, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_failure(in, in, out, in, out) is det.

	% Generate the declaration for the built-in `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
:- func ml_gen_succeeded_var_decl(mlds__context) = mlds__defn.

	% Return the lval for the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
:- pred ml_success_lval(mlds__lval, ml_gen_info, ml_gen_info).
:- mode ml_success_lval(out, in, out) is det.

	% Return an rval which will test the value of the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
:- pred ml_gen_test_success(mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_test_success(out, in, out) is det.
	
	% Generate code to set the `succeeded' flag to the
	% specified truth value.
	%
:- pred ml_gen_set_success(mlds__rval, prog_context, mlds__statement,
				ml_gen_info, ml_gen_info).
:- mode ml_gen_set_success(in, in, out, in, out) is det.

	% Generate the declaration for the specified `cond'
	% variable.
	% (`cond' variables are boolean variables used to record
	% the success or failure of model_non conditions of
	% if-then-elses.)
	%
:- func ml_gen_cond_var_decl(cond_seq, mlds__context) = mlds__defn.

	% Return the lval for the specified `cond' flag.
	% (`cond' variables are boolean variables used to record
	% the success or failure of model_non conditions of
	% if-then-elses.)
	%
:- pred ml_cond_var_lval(cond_seq, mlds__lval, ml_gen_info, ml_gen_info).
:- mode ml_cond_var_lval(in, out, in, out) is det.

	% Return an rval which will test the value of the specified
	% `cond' variable.
	% (`cond' variables are boolean variables used to record
	% the success or failure of model_non conditions of
	% if-then-elses.)
	%
:- pred ml_gen_test_cond_var(cond_seq, mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_test_cond_var(in, out, in, out) is det.
	
	% Generate code to set the specified `cond' variable to the
	% specified truth value.
	%
:- pred ml_gen_set_cond_var(cond_seq, mlds__rval, prog_context,
		mlds__statement, ml_gen_info, ml_gen_info).
:- mode ml_gen_set_cond_var(in, in, in, out, in, out) is det.

	% Return the success continuation that was
	% passed as the current function's argument(s).
	% The success continuation consists of two parts, the
	% `cont' argument, and the `cont_env' argument.
	% The `cont' argument is a continuation function that
	% will be called when a model_non goal succeeds.
	% The `cont_env' argument is a pointer to the environment (set
	% of local variables in the containing procedure) for the continuation
	% function.  (If we're using gcc nested function, the `cont_env'
	% is not used.)
	% The output variable lvals and types need to be supplied when
	% generating a continuation using --nondet-copy-out, otherwise
	% they should be empty.
	%
:- pred ml_initial_cont(list(mlds__lval), list(prog_type), success_cont,
			ml_gen_info, ml_gen_info).
:- mode ml_initial_cont(in, in, out, in, out) is det.

	% Generate code to call the current success continuation.
	% This is used for generating success when in a model_non context.
	%
:- pred ml_gen_call_current_success_cont(prog_context, mlds__statement,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_call_current_success_cont(in, out, in, out) is det.

	% Generate code to call the current success continuation, using
	% a local function as a proxy.
	% This is used for generating success when in a model_non context
	% from within pragma C code (currently only in IL).
	%
:- pred ml_gen_call_current_success_cont_indirectly(prog_context, 
		mlds__statement, ml_gen_info, ml_gen_info).
:- mode ml_gen_call_current_success_cont_indirectly(in, out, in, out) is det.

%-----------------------------------------------------------------------------%
%
% Routines for dealing with the environment pointer
% used for nested functions.
%

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
	% Note that we generate this as a dangling reference.
	% The ml_elim_nested pass will insert the declaration
	% of the env_ptr variable.
:- pred ml_get_env_ptr(mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_get_env_ptr(out, in, out) is det.

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
:- pred ml_declare_env_ptr_arg(pair(mlds__entity_name, mlds__type),
		ml_gen_info, ml_gen_info).
:- mode ml_declare_env_ptr_arg(out, in, out) is det.

%-----------------------------------------------------------------------------%
%
% Magic numbers relating to the representation of
% typeclass_infos, base_typeclass_infos, and closures.
%
	% This function returns the offset to add to the argument
	% number of a closure arg to get its field number.
:- func ml_closure_arg_offset = int.

	% This function returns the offset to add to the argument
	% number of a typeclass_info arg to get its field number.
:- func ml_typeclass_info_arg_offset = int.

	% This function returns the offset to add to the method number
	% for a type class method to get its field number within the
	% base_typeclass_info.
:- func ml_base_typeclass_info_method_offset = int.

%-----------------------------------------------------------------------------%
%
% Miscellaneous routines
%

	% Get the value of the appropriate --det-copy-out or --nondet-copy-out
	% option, depending on the code model.
:- func get_copy_out_option(globals, code_model) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The `ml_gen_info' ADT.
%

	%
	% The `ml_gen_info' type holds information used during
	% MLDS code generation for a given procedure.
	%
:- type ml_gen_info.

	% initialize the ml_gen_info, so that it is
	% ready for generating code for the given procedure
:- func ml_gen_info_init(module_info, pred_id, proc_id) = ml_gen_info.

	% accessor predicates; these just get the specified
	% information from the ml_gen_info

:- pred ml_gen_info_get_module_info(ml_gen_info, module_info).
:- mode ml_gen_info_get_module_info(in, out) is det.

:- pred ml_gen_info_get_module_name(ml_gen_info, mercury_module_name).
:- mode ml_gen_info_get_module_name(in, out) is det.

:- pred ml_gen_info_get_pred_id(ml_gen_info, pred_id).
:- mode ml_gen_info_get_pred_id(in, out) is det.

:- pred ml_gen_info_get_proc_id(ml_gen_info, proc_id).
:- mode ml_gen_info_get_proc_id(in, out) is det.

:- pred ml_gen_info_get_varset(ml_gen_info, prog_varset).
:- mode ml_gen_info_get_varset(in, out) is det.

:- pred ml_gen_info_get_var_types(ml_gen_info, map(prog_var, prog_type)).
:- mode ml_gen_info_get_var_types(in, out) is det.

:- pred ml_gen_info_get_byref_output_vars(ml_gen_info, list(prog_var)).
:- mode ml_gen_info_get_byref_output_vars(in, out) is det.

:- pred ml_gen_info_set_byref_output_vars(list(prog_var),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_byref_output_vars(in, in, out) is det.

:- pred ml_gen_info_get_value_output_vars(ml_gen_info, list(prog_var)).
:- mode ml_gen_info_get_value_output_vars(in, out) is det.

:- pred ml_gen_info_set_value_output_vars(list(prog_var),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_value_output_vars(in, in, out) is det.

:- pred ml_gen_info_get_globals(globals, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_get_globals(out, in, out) is det.

	% lookup the --gcc-nested-functions option
:- pred ml_gen_info_use_gcc_nested_functions(bool, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_use_gcc_nested_functions(out, in, out) is det.

	% lookup the --put-commit-in-nested-func option
:- pred ml_gen_info_put_commit_in_own_func(bool, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_put_commit_in_own_func(out, in, out) is det.

	% Generate a new label number for use in label statements.
	% This is used to give unique names to the case labels generated
	% for dense switch statements.
:- type label_num == int.
:- pred ml_gen_info_new_label(label_num, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_label(out, in, out) is det.

	% A number corresponding to an MLDS nested function which serves as a
	% label (i.e. a continuation function).
:- type ml_label_func == mlds__func_sequence_num.

	% Generate a new function label number.
	% This is used to give unique names to the nested functions
	% used when generating code for nondet procedures.
:- pred ml_gen_info_new_func_label(ml_label_func, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_func_label(out, in, out) is det.

	% Increase the function label counter by some
	% amount which is presumed to be sufficient
	% to ensure that if we start again with a fresh
	% ml_gen_info and then call this function,
	% we won't encounter any already-used function labels.
	% (This is used when generating wrapper functions
	% for type class methods.)
:- pred ml_gen_info_bump_func_label(ml_gen_info, ml_gen_info).
:- mode ml_gen_info_bump_func_label(in, out) is det.

	% Generate a new commit label number.
	% This is used to give unique names to the labels
	% used when generating code for commits.
:- type commit_sequence_num == int.
:- pred ml_gen_info_new_commit_label(commit_sequence_num,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_commit_label(out, in, out) is det.

	% Generate a new `cond' variable number.
	% This is used to give unique names to the local
	% variables used to hold the results of 
	% nondet conditions of if-then-elses.
:- type cond_seq == int.
:- pred ml_gen_info_new_cond_var(cond_seq,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_cond_var(out, in, out) is det.

	% Generate a new `conv' variable number.
	% This is used to give unique names to the local
	% variables generated by ml_gen_box_or_unbox_lval,
	% which are used to handle boxing/unboxing
	% argument conversions.
:- type conv_seq == int.
:- pred ml_gen_info_new_conv_var(conv_seq,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_conv_var(out, in, out) is det.

	% Generate a new `const' sequence number.
	% This is used to give unique names to the local constants
	% generated for --static-ground-terms.
:- type const_seq == int.
:- pred ml_gen_info_new_const(const_seq,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_const(out, in, out) is det.

	% Set the `const' sequence number
	% corresponding to a given HLDS variable.
	%
:- pred ml_gen_info_set_const_num(prog_var, const_seq,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_const_num(in, in, in, out) is det.

	% Lookup the `const' sequence number
	% corresponding to a given HLDS variable.
	%
:- pred ml_gen_info_lookup_const_num(prog_var, const_seq,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_lookup_const_num(in, out, in, out) is det.

	%
	% A success continuation specifies the (rval for the variable
	% holding the address of the) function that a nondet procedure
	% should call if it succeeds, and possibly also the
	% (rval for the variable holding) the environment pointer
	% for that function, and possibly also the (list of rvals
	% for the) arguments to the continuation.
	%
:- type success_cont 
	--->	success_cont(
			mlds__rval,	% function pointer
			mlds__rval,	% environment pointer
				% note that if we're using nested
				% functions then the environment
				% pointer will not be used
			list(mlds__type), % argument types, if any
			list(mlds__lval)  % arguments, if any
				% The arguments will only be non-empty if the
				% --nondet-copy-out option is enabled.
				% They do not include the environment pointer.
		).

	%
	% The ml_gen_info contains a stack of success continuations.
	% The following routines provide access to that stack.
	%

:- pred ml_gen_info_push_success_cont(success_cont, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_push_success_cont(in, in, out) is det.

:- pred ml_gen_info_pop_success_cont(ml_gen_info, ml_gen_info).
:- mode ml_gen_info_pop_success_cont(in, out) is det.

:- pred ml_gen_info_current_success_cont(success_cont,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_current_success_cont(out, in, out) is det.

	%
	% We keep a partial mapping from vars to lvals.
	% This is used in special cases to override the normal
	% lval for a variable.  ml_gen_var will check this
	% map first, and if the variable is not in this map,
	% then it will go ahead and generate an lval for it
	% as usual.
	%

	% Set the lval for a variable.
:- pred ml_gen_info_set_var_lval(prog_var, mlds__lval,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_var_lval(in, in, in, out) is det.

	% Get the partial mapping from variables to lvals.
:- pred ml_gen_info_get_var_lvals(ml_gen_info, map(prog_var, mlds__lval)).
:- mode ml_gen_info_get_var_lvals(in, out) is det.

	% Set the partial mapping from variables to lvals.
:- pred ml_gen_info_set_var_lvals(map(prog_var, mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_var_lvals(in, in, out) is det.

	%
	% The ml_gen_info contains a list of extra definitions
	% of functions or global constants which should be inserted
	% before the definition of the function for the current procedure.
	% This is used for the definitions of the wrapper functions needed
	% for closures.  When generating code for a procedure that creates
	% a closure, we insert the definition of the wrapper function used
	% for that closure into this list.
	%

	% Insert an extra definition at the start of the list of extra
	% definitions.
:- pred ml_gen_info_add_extra_defn(mlds__defn,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_add_extra_defn(in, in, out) is det.

	% Get the list of extra definitions.
:- pred ml_gen_info_get_extra_defns(ml_gen_info, mlds__defns).
:- mode ml_gen_info_get_extra_defns(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_call_gen.
:- import_module prog_util, type_util, mode_util, special_pred, error_util.
:- import_module code_util. % XXX for `code_util__compiler_generated'.
:- import_module globals, options.

:- import_module stack, string, require, term, varset.

%-----------------------------------------------------------------------------%
%
% Code for various utility routines
%

	% Generate an MLDS assignment statement.
ml_gen_assign(Lval, Rval, Context) = MLDS_Statement :-
	Assign = assign(Lval, Rval),
	MLDS_Stmt = atomic(Assign),
	MLDS_Statement = mlds__statement(MLDS_Stmt,
		mlds__make_context(Context)).

	%
	% Append an appropriate `return' statement for the given code_model
	% and returning the given OutputVarLvals, if needed.
	%
ml_append_return_statement(CodeModel, CopiedOutputVarLvals, Context,
		MLDS_Statements0, MLDS_Statements) -->
	( { CodeModel = model_semi } ->
		ml_gen_test_success(Succeeded),
		{ ReturnStmt = return([Succeeded]) },
		{ ReturnStatement = mlds__statement(ReturnStmt,
			mlds__make_context(Context)) },
		{ MLDS_Statements = list__append(MLDS_Statements0,
			[ReturnStatement]) }
	; { CodeModel \= model_non, CopiedOutputVarLvals \= [] } ->
		{ CopiedOutputVarRvals = list__map(func(Lval) = lval(Lval),
			CopiedOutputVarLvals) },
		{ ReturnStmt = return(CopiedOutputVarRvals) },
		{ ReturnStatement = mlds__statement(ReturnStmt,
			mlds__make_context(Context)) },
		{ MLDS_Statements = list__append(MLDS_Statements0,
			[ReturnStatement]) }
	;
		{ MLDS_Statements = MLDS_Statements0 }
	).

	% Generate a block statement, i.e. `{ <Decls>; <Statements>; }'.
	% But if the block consists only of a single statement with no
	% declarations, then just return that statement.
	%
ml_gen_block(VarDecls, Statements, Context) =
	(if VarDecls = [], Statements = [SingleStatement] then
		SingleStatement
	else
		mlds__statement(block(VarDecls, Statements),
			mlds__make_context(Context))
	).

	% ml_join_decls:
	% 	Join two statement lists and their corresponding
	% 	declaration lists in sequence.
	% 
	% 	If the statements have no declarations in common,
	% 	then their corresponding declaration lists will be
	% 	concatenated together into a single list of declarations.
	% 	But if they have any declarations in common, then we
	% 	put each statement list and its declarations into
	% 	a block, so that the declarations remain local to
	% 	each statement list.
	% 
ml_join_decls(FirstDecls, FirstStatements, RestDecls, RestStatements, Context,
		MLDS_Decls, MLDS_Statements) :-
	(
		list__member(mlds__defn(Name, _, _, _), FirstDecls),
		list__member(mlds__defn(Name, _, _, _), RestDecls)
	->
		First = ml_gen_block(FirstDecls, FirstStatements, Context),
		Rest = ml_gen_block(RestDecls, RestStatements, Context),
		MLDS_Decls = [],
		MLDS_Statements = [First, Rest]
	;
		MLDS_Decls = list__append(FirstDecls, RestDecls),
		MLDS_Statements = list__append(FirstStatements, RestStatements)
	).

	% ml_combine_conj:
	%	Given closures to generate code for two conjuncts,
	%	generate code for their conjunction.
	%
ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
		MLDS_Decls, MLDS_Statements) -->
	(
		%	model_det goal:
		%		<First, Rest>
		% 	===>
		%		<do First>
		%		<Rest>
		%	
		{ FirstCodeModel = model_det },
		DoGenFirst(FirstDecls, FirstStatements),
		DoGenRest(RestDecls, RestStatements),
		{ ml_join_decls(FirstDecls, FirstStatements,
			RestDecls, RestStatements, Context,
			MLDS_Decls, MLDS_Statements) }
	;
		%	model_semi goal:
		%		<Goal, Goals>
		% 	===>
		%		bool succeeded;
		%
		%		<succeeded = Goal>;
		%		if (succeeded) {
		%			<Goals>;
		%		}
		%	except that we hoist any declarations generated
		%	for <Goals> to the outer scope, rather than
		%	inside the `if', so that they remain in
		%	scope for any later goals which follow this
		%	(this is needed for declarations of static consts)
		{ FirstCodeModel = model_semi },
		DoGenFirst(FirstDecls, FirstStatements),
		ml_gen_test_success(Succeeded),
		DoGenRest(RestDecls, RestStatements),
		{ IfBody = ml_gen_block([], RestStatements, Context) },
		{ IfStmt = if_then_else(Succeeded, IfBody, no) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },
		{ MLDS_Decls = list__append(FirstDecls, RestDecls) },
		{ MLDS_Statements = list__append(FirstStatements,
			[IfStatement]) }
	;
		%	model_non goal:
		%		<First, Rest>
		% 	===>
		%		succ_func() {
		%			<Rest && SUCCEED()>;
		%		}
		%
		%		<First && succ_func()>;
		%	except that we hoist any declarations generated
		%	for <First> and any _static_ declarations generated
		%	for <Rest> to the top of the scope, rather
		%	than inside or after the succ_func(), so that they
		%	remain in scope for any code following them
		%	(this is needed for declarations of static consts).
		%
		%	We take care to only hoist _static_ declarations
		%	outside nested functions, since access to non-local
		%	variables is less efficient.
		%
		% XXX The pattern above leads to deep nesting for long
		%     conjunctions; we should avoid that.
		%

		{ FirstCodeModel = model_non },

		% allocate a name for the `succ_func'
		ml_gen_new_func_label(no, RestFuncLabel, RestFuncLabelRval),

		% generate <First && succ_func()>
		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(RestFuncLabelRval,
			EnvPtrRval, [], []) },
		ml_gen_info_push_success_cont(SuccessCont),
		DoGenFirst(FirstDecls, FirstStatements),
		ml_gen_info_pop_success_cont,

		% generate the `succ_func'
		/* push nesting level */
		DoGenRest(RestDecls, RestStatements),
		{ list__filter(ml_decl_is_static_const, RestDecls,
			RestStaticDecls, RestOtherDecls) },
		{ RestStatement = ml_gen_block(RestOtherDecls, RestStatements,
			Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(RestFuncLabel, Context, RestStatement,
			RestFunc),

		{ MLDS_Decls = list__condense(
			[FirstDecls, RestStaticDecls, [RestFunc]]) },
		{ MLDS_Statements = FirstStatements }
	).

	% Succeed iff the specified mlds__defn defines
	% a static constant.
	%
ml_decl_is_static_const(Defn) :-
	Defn = mlds__defn(Name, _Context, Flags, _DefnBody),
	Name = data(var(_)),
	Flags = ml_static_const_decl_flags.

	% Given a function label and the statement which will comprise
	% the function body for that function, generate an mlds__defn
	% which defines that function.
	%
ml_gen_nondet_label_func(FuncLabel, Context, Statement, Func) -->
	ml_gen_info_use_gcc_nested_functions(UseNested),
	( { UseNested = yes } ->
		{ FuncParams = mlds__func_params([], []) }
	;
		ml_declare_env_ptr_arg(EnvPtrArg),
		{ FuncParams = mlds__func_params([EnvPtrArg], []) }
	),
	ml_gen_label_func(FuncLabel, FuncParams, Context, Statement, Func).

	% Given a function label, the function parameters, and the statement
	% which will comprise the function body for that function,
	% generate an mlds__defn which defines that function.
	%
ml_gen_label_func(FuncLabel, FuncParams, Context, Statement, Func) -->
	%
	% compute the function name
	%
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_pred_id(Info, PredId) },
	{ ml_gen_info_get_proc_id(Info, ProcId) },
	{ FuncName = ml_gen_nondet_label(ModuleInfo, PredId, ProcId,
		FuncLabel) },

	%
	% compute the function definition
	%
	{ DeclFlags = ml_gen_label_func_decl_flags },
	{ MaybePredProcId = no },
	{ Attributes = [] },
	{ FuncDefn = function(MaybePredProcId, FuncParams, 
		defined_here(Statement), Attributes) },
	{ Func = mlds__defn(FuncName, mlds__make_context(Context), DeclFlags,
			FuncDefn) }.

	% Return the declaration flags appropriate for a label func
	% (a label func is a function used as a continuation
	% when generating nondet code).
	%
:- func ml_gen_label_func_decl_flags = mlds__decl_flags.
ml_gen_label_func_decl_flags = MLDS_DeclFlags :-
	Access = local,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%
%
% Code for generating types.
%

ml_gen_type(Type, MLDS_Type) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type) }.

ml_gen_array_elem_type(elem_type_string) = ml_string_type.
ml_gen_array_elem_type(elem_type_int) = mlds__native_int_type.
ml_gen_array_elem_type(elem_type_generic) = mlds__generic_type.

ml_string_type = mercury_type(string_type, str_type).

%-----------------------------------------------------------------------------%
%
% Code for generating function declarations (i.e. mlds__func_params).
%

	% Generate the function prototype for a given procedure.
	%
ml_gen_proc_params(ModuleInfo, PredId, ProcId) = FuncParams :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_headvars(ProcInfo, HeadVars),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_arg_types(PredInfo, HeadTypes),
	proc_info_argmodes(ProcInfo, HeadModes),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	HeadVarNames = ml_gen_var_names(VarSet, HeadVars),
	FuncParams = ml_gen_params(ModuleInfo, HeadVarNames, HeadTypes,
		HeadModes, PredOrFunc, CodeModel).

	% As above, but from the rtti_proc_id rather than
	% from the module_info, pred_id, and proc_id.
	%
ml_gen_proc_params_from_rtti(ModuleInfo, RttiProcId) = FuncParams :-
	VarSet = RttiProcId^proc_varset,
	HeadVars = RttiProcId^proc_headvars,
	ArgTypes = RttiProcId^arg_types,
	ArgModes = RttiProcId^proc_arg_modes,
	PredOrFunc = RttiProcId^pred_or_func,
	CodeModel = RttiProcId^proc_interface_code_model,
	HeadVarNames = ml_gen_var_names(VarSet, HeadVars),
	FuncParams = ml_gen_params_base(ModuleInfo, HeadVarNames,
		ArgTypes, ArgModes, PredOrFunc, CodeModel).
	
	% Generate the function prototype for a procedure with the
	% given argument types, modes, and code model.
	%
ml_gen_params(ModuleInfo, HeadVarNames, HeadTypes, HeadModes, PredOrFunc,
		CodeModel) = FuncParams :-
	modes_to_arg_modes(ModuleInfo, HeadModes, HeadTypes, ArgModes),
	FuncParams = ml_gen_params_base(ModuleInfo, HeadVarNames,
		HeadTypes, ArgModes, PredOrFunc, CodeModel).

:- func ml_gen_params_base(module_info, list(mlds__var_name), list(prog_type),
		list(arg_mode), pred_or_func, code_model) = mlds__func_params.

ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes, HeadModes,
		PredOrFunc, CodeModel) = FuncParams :-
	module_info_globals(ModuleInfo, Globals),
	CopyOut = get_copy_out_option(Globals, CodeModel),
	ml_gen_arg_decls(ModuleInfo, HeadVarNames, HeadTypes, HeadModes,
		CopyOut, FuncArgs0, RetTypes0),
	(
		CodeModel = model_det,
		%
		% for model_det Mercury functions whose result argument has an
		% output mode, make the result into the MLDS return type
		%
		(
			RetTypes0 = [],
			PredOrFunc = function,
			pred_args_to_func_args(HeadModes, _, ResultMode),
			ResultMode = top_out,
			pred_args_to_func_args(HeadTypes, _, ResultType),
			\+ type_util__is_dummy_argument_type(ResultType)
		->
			pred_args_to_func_args(FuncArgs0, FuncArgs,
				_RetArgName - RetTypePtr),
			( RetTypePtr = mlds__ptr_type(RetType) ->
				RetTypes = [RetType]
			;
				error("output mode function result doesn't have pointer type")
			)
		;
			FuncArgs = FuncArgs0,
			RetTypes = RetTypes0
		)
	;
		CodeModel = model_semi,
		%
		% for model_semi procedures, return a bool
		%
		FuncArgs = FuncArgs0,
		RetTypes = [mlds__native_bool_type | RetTypes0]
	;
		CodeModel = model_non,
		%
		% for model_non procedures, we return values
		% by passing them to the continuation
		%
		( CopyOut = yes ->
			ContType = mlds__cont_type(RetTypes0),
			RetTypes = []
		;
			ContType = mlds__cont_type([]),
			RetTypes = RetTypes0
		),
		ContName = data(var(var_name("cont", no))),
		ContArg = ContName - ContType,
		ContEnvType = mlds__generic_env_ptr_type,
		ContEnvName = data(var(var_name("cont_env_ptr", no))),
		ContEnvArg = ContEnvName - ContEnvType,
		globals__lookup_bool_option(Globals, gcc_nested_functions,
			NestedFunctions),
		(
			NestedFunctions = yes
		->
			FuncArgs = list__append(FuncArgs0, [ContArg])
		;
			FuncArgs = list__append(FuncArgs0,
				[ContArg, ContEnvArg])
		)
	),
	FuncParams = mlds__func_params(FuncArgs, RetTypes).

	% Given the argument variable names, and corresponding lists of their
	% types and modes, generate the MLDS argument declarations
	% and return types.
	%
:- pred ml_gen_arg_decls(module_info, list(mlds__var_name), list(prog_type),
		list(arg_mode), bool, mlds__arguments, mlds__return_types).
:- mode ml_gen_arg_decls(in, in, in, in, in, out, out) is det.

ml_gen_arg_decls(ModuleInfo, HeadVars, HeadTypes, HeadModes, CopyOut,
		FuncArgs, RetTypes) :-
	(
		HeadVars = [], HeadTypes = [], HeadModes = []
	->
		FuncArgs = [], RetTypes = []
	;	
		HeadVars = [Var | Vars],
		HeadTypes = [Type | Types],
		HeadModes = [Mode | Modes]
	->
		ml_gen_arg_decls(ModuleInfo, Vars, Types, Modes, CopyOut,
			FuncArgs0, RetTypes0),
		(
			%
			% exclude types such as io__state, etc.
			%
			type_util__is_dummy_argument_type(Type)
		->
			FuncArgs = FuncArgs0,
			RetTypes = RetTypes0
		;
			%
			% for by-value outputs, generate a return type
			%
			Mode = top_out,
			CopyOut = yes
		->
			RetType = mercury_type_to_mlds_type(ModuleInfo, Type),
			RetTypes = [RetType | RetTypes0],
			FuncArgs = FuncArgs0
		;
			%
			% for inputs and by-reference outputs,
			% generate argument
			%
			ml_gen_arg_decl(ModuleInfo, Var, Type, Mode, FuncArg),
			FuncArgs = [FuncArg | FuncArgs0],
			RetTypes = RetTypes0
		)
	;
		error("ml_gen_arg_decls: length mismatch")
	).

	% Given an argument variable, and its type and mode,
	% generate an MLDS argument declaration for it.
	%
:- pred ml_gen_arg_decl(module_info, var_name, prog_type, arg_mode,
			pair(mlds__entity_name, mlds__type)).
:- mode ml_gen_arg_decl(in, in, in, in, out) is det.

ml_gen_arg_decl(ModuleInfo, Var, Type, ArgMode, FuncArg) :-
	MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
	( ArgMode \= top_in ->
		MLDS_ArgType = mlds__ptr_type(MLDS_Type)
	;
		MLDS_ArgType = MLDS_Type
	),
	Name = data(var(Var)),
	FuncArg = Name - MLDS_ArgType.


ml_is_output_det_function(ModuleInfo, PredId, ProcId, RetArgVar) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo,
			ProcInfo),
	
	pred_info_get_is_pred_or_func(PredInfo, function),
	proc_info_interface_code_model(ProcInfo, model_det),

	proc_info_argmodes(ProcInfo, Modes),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_headvars(ProcInfo, ArgVars),
	modes_to_arg_modes(ModuleInfo, Modes, ArgTypes, ArgModes),
	pred_args_to_func_args(ArgModes, _InputArgModes, RetArgMode),
	pred_args_to_func_args(ArgTypes, _InputArgTypes, RetArgType),
	pred_args_to_func_args(ArgVars, _InputArgVars, RetArgVar),

	RetArgMode = top_out,
	\+ type_util__is_dummy_argument_type(RetArgType).


%-----------------------------------------------------------------------------%
%
% Code for generating mlds__entity_names.
%

	% Generate the mlds__entity_name and module name for the entry point
	% function corresponding to a given procedure.
	%
ml_gen_proc_label(ModuleInfo, PredId, ProcId, MLDS_Name, MLDS_ModuleName) :-
	ml_gen_func_label(ModuleInfo, PredId, ProcId, no,
		MLDS_Name, MLDS_ModuleName).

	% Generate an mlds__entity_name for a continuation function
	% with the given sequence number.  The pred_id and proc_id
	% specify the procedure that this continuation function
	% is part of.
	%
ml_gen_nondet_label(ModuleInfo, PredId, ProcId, SeqNum) = MLDS_Name :-
	ml_gen_func_label(ModuleInfo, PredId, ProcId, yes(SeqNum),
		MLDS_Name, _MLDS_ModuleName).

:- pred ml_gen_func_label(module_info, pred_id, proc_id,
		maybe(ml_label_func), mlds__entity_name, mlds_module_name).
:- mode ml_gen_func_label(in, in, in, in, out, out) is det.

ml_gen_func_label(ModuleInfo, PredId, ProcId, MaybeSeqNum,
		MLDS_Name, MLDS_ModuleName) :-
	ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		MLDS_PredLabel, MLDS_ModuleName),
	MLDS_Name = function(MLDS_PredLabel, ProcId, MaybeSeqNum, PredId).

	% Allocate a new function label and return an rval containing
	% the function's address.
	%
ml_gen_new_func_label(MaybeParams, FuncLabel, FuncLabelRval) -->
	ml_gen_info_new_func_label(FuncLabel),
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_pred_id(Info, PredId) },
	{ ml_gen_info_get_proc_id(Info, ProcId) },
	{ ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		PredLabel, PredModule) },
	{ ml_gen_info_use_gcc_nested_functions(UseNestedFuncs, Info, _) },
	{ MaybeParams = yes(Params) ->
		Signature = mlds__get_func_signature(Params)
	;
		( UseNestedFuncs = yes ->
			ArgTypes = []
		;
			ArgTypes = [mlds__generic_env_ptr_type]
		),
		Signature = mlds__func_signature(ArgTypes, [])
	},
	{ ProcLabel = qual(PredModule, PredLabel - ProcId) },
	{ FuncLabelRval = const(code_addr_const(internal(ProcLabel,
		FuncLabel, Signature))) }.

	% Generate the mlds__pred_label and module name
	% for a given procedure.
	%
ml_gen_pred_label(ModuleInfo, PredId, ProcId, MLDS_PredLabel, MLDS_Module) :-
	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel,
		MLDS_PredLabel, MLDS_Module).

ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel, MLDS_PredLabel,
		MLDS_Module) :-
	RttiProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,	
		PredName, PredArity, ArgTypes, PredId, ProcId,
		_VarSet, _HeadVars, _ArgModes, CodeModel,
		IsImported, _IsPseudoImported, _IsExported,
		IsSpecialPredInstance),
	(
		IsSpecialPredInstance = yes
	->
		(
			special_pred_get_type(PredName, ArgTypes, Type),
			type_to_type_id(Type, TypeId, _),
			% All type_ids other than tuples here should be
			% module qualified, since builtin types are handled
			% separately in polymorphism.m.
			(
				TypeId = unqualified(TypeName) - TypeArity,
				type_id_is_tuple(TypeId),
				mercury_public_builtin_module(TypeModule)
			;
				TypeId = qualified(TypeModule, TypeName)
						- TypeArity
			)
		->
			(
				ThisModule \= TypeModule,
				PredName = "__Unify__",
				\+ hlds_pred__in_in_unification_proc_id(ProcId)
			->
				% This is a locally-defined instance
				% of a unification procedure for a type
				% defined in some other module
				DefiningModule = ThisModule,
				MaybeDeclaringModule = yes(TypeModule)
			;
				% the module declaring the type is the same as
				% the module defining this special pred
				DefiningModule = TypeModule,
				MaybeDeclaringModule = no
			),
			MLDS_PredLabel = special_pred(PredName,
				MaybeDeclaringModule, TypeName, TypeArity)
		;
			string__append_list(["ml_gen_pred_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		)
	;
		(
			% Work out which module supplies the code for
			% the predicate.
			ThisModule \= PredModule,
			IsImported = no
		->
			% This predicate is a specialized version of 
			% a pred from a `.opt' file.
			DefiningModule = ThisModule,
			MaybeDeclaringModule = yes(PredModule)
		;	
			% The predicate was declared in the same module
			% that it is defined in
			DefiningModule = PredModule,
			MaybeDeclaringModule = no
		),
		(
			PredOrFunc = function,
			\+ ml_is_output_det_function(ModuleInfo, PredId,
				ProcId, _)
		->
			NonOutputFunc = yes
		;
			NonOutputFunc = no
		),
		MLDS_PredLabel = pred(PredOrFunc, MaybeDeclaringModule,
				PredName, PredArity, CodeModel,
				NonOutputFunc)
	),
	MLDS_Module = mercury_module_name_to_mlds(DefiningModule).

ml_gen_new_label(Label) -->
	ml_gen_info_new_label(LabelNum),
	{ string__format("label_%d", [i(LabelNum)], Label) }.

%-----------------------------------------------------------------------------%
%
% Code for dealing with variables
%

	% Generate a list of the mlds__lvals corresponding to a
	% given list of prog_vars.
	%
ml_gen_var_list([], []) --> [].
ml_gen_var_list([Var | Vars], [Lval | Lvals]) -->
	ml_gen_var(Var, Lval),
	ml_gen_var_list(Vars, Lvals).

	% Generate the mlds__lval corresponding to a given prog_var.
	%
ml_gen_var(Var, Lval) -->
	%
	% First check the var_lvals override mapping;
	% if an lval has been set for this variable, use it
	%
	=(Info),
	{ ml_gen_info_get_var_lvals(Info, VarLvals) },
	( { map__search(VarLvals, Var, VarLval) } ->
		{ Lval = VarLval }
	;
		%
		% Otherwise just look up the variable's type
		% and generate an lval for it using the ordinary
		% algorithm.
		%
		ml_variable_type(Var, Type),
		ml_gen_var_with_type(Var, Type, Lval)
	).

	% Generate the mlds__lval corresponding to a given prog_var,
	% with a given type.
	%
ml_gen_var_with_type(Var, Type, Lval) -->
	( { type_util__is_dummy_argument_type(Type) } ->
		%
		% The variable won't have been declared, so
		% we need to generate a dummy lval for this variable.
		%
		{ mercury_private_builtin_module(PrivateBuiltin) },
		{ MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin) },
		ml_gen_type(Type, MLDS_Type),
		{ Lval = var(qual(MLDS_Module, var_name("dummy_var", no)),
			MLDS_Type) }
	;
		=(MLDSGenInfo),
		{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
		{ VarName = ml_gen_var_name(VarSet, Var) },
		ml_gen_type(Type, MLDS_Type),
		ml_gen_var_lval(VarName, MLDS_Type, VarLval),
		%
		% output variables may be passed by reference...
		%
		{ ml_gen_info_get_byref_output_vars(MLDSGenInfo, OutputVars) },
		( { list__member(Var, OutputVars) } ->
			{ Lval = mem_ref(lval(VarLval), MLDS_Type) }
		;
			{ Lval = VarLval }
		)
	).

	% Lookup the types of a list of variables.
	%
ml_variable_types([], []) --> [].
ml_variable_types([Var | Vars], [Type | Types]) -->
	ml_variable_type(Var, Type),
	ml_variable_types(Vars, Types).

	% Lookup the type of a variable.
	%
ml_variable_type(Var, Type) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_var_types(MLDSGenInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

	% Generate the MLDS variable names for a list of HLDS variables.
	%
ml_gen_var_names(VarSet, Vars) = list__map(ml_gen_var_name(VarSet), Vars).

	% Generate the MLDS variable name for an HLDS variable.
	%
ml_gen_var_name(VarSet, Var) = UniqueVarName :-
	varset__lookup_name(VarSet, Var, VarName),
	term__var_to_int(Var, VarNumber),
	UniqueVarName = mlds__var_name(VarName, yes(VarNumber)).

	% Generate a name for a local static constant.
	%
	% To ensure that the names are unique, we qualify them with the
	% pred_id and proc_id numbers, as well as a sequence number.
	% This is needed to allow ml_elim_nested.m to hoist
	% such constants out to top level.
ml_format_static_const_name(BaseName, SequenceNum, ConstName) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_pred_id(MLDSGenInfo, PredId) },
	{ ml_gen_info_get_proc_id(MLDSGenInfo, ProcId) },
	{ pred_id_to_int(PredId, PredIdNum) },
	{ proc_id_to_int(ProcId, ProcIdNum) },
	{ ConstName = mlds__var_name(
		string__format("const_%d_%d_%d_%s", [i(PredIdNum),
			i(ProcIdNum), i(SequenceNum), s(BaseName)]), no) }.

	% Qualify the name of the specified variable
	% with the current module name.
	%
ml_gen_var_lval(VarName, VarType, QualifiedVarLval) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ QualifiedVarLval = var(qual(MLDS_Module, VarName), VarType) }.

	% Generate a declaration for an MLDS variable, given its HLDS type.
	%
ml_gen_var_decl(VarName, Type, Context, ModuleInfo) =
	ml_gen_mlds_var_decl(var(VarName),
		mercury_type_to_mlds_type(ModuleInfo, Type), Context).

	% Generate a declaration for an MLDS variable, given its MLDS type.
	%
ml_gen_mlds_var_decl(DataName, MLDS_Type, Context) = 
	ml_gen_mlds_var_decl(DataName, MLDS_Type, no_initializer, Context).
	

	% Generate a declaration for an MLDS variable, given its MLDS type
	% and initializer.
	%
ml_gen_mlds_var_decl(DataName, MLDS_Type, Initializer, Context) = MLDS_Defn :-
	Name = data(DataName),
	Defn = data(MLDS_Type, Initializer),
	DeclFlags = ml_gen_local_var_decl_flags,
	MLDS_Defn = mlds__defn(Name, Context, DeclFlags, Defn).

	% Generate a definition of a local static constant,
	% given the constant's name, type, and initializer.
	%
ml_gen_static_const_defn(ConstName, ConstType, Initializer, Context) =
		MLDS_Defn :-
	Name = data(var(ConstName)),
	Defn = data(ConstType, Initializer),
	DeclFlags = ml_static_const_decl_flags,
	MLDS_Context = mlds__make_context(Context),
	MLDS_Defn = mlds__defn(Name, MLDS_Context, DeclFlags, Defn).

	% Return the declaration flags appropriate for a public field
	% in the derived constructor class of a discriminated union.
	%
ml_gen_public_field_decl_flags = MLDS_DeclFlags :-
	Access = public,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

	% Return the declaration flags appropriate for a local variable.
ml_gen_local_var_decl_flags = MLDS_DeclFlags :-
	Access = local,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

	% Return the declaration flags appropriate for an
	% initialized local static constant.
	%
ml_static_const_decl_flags = MLDS_DeclFlags :-
	Access = local,
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = final,
	Constness = const,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

ml_var_name_to_string(var_name(Var, yes(Num))) =
	string__format("%s_%d", [s(Var), i(Num)]).
ml_var_name_to_string(var_name(Var, no)) = Var.
	

%-----------------------------------------------------------------------------%
%
% Code for dealing with fields
%

	% Given the user-specified field name, if any,
	% and the argument number (starting from one),
	% generate an MLDS field name.
	%
ml_gen_field_name(MaybeFieldName, ArgNum) = FieldName :-
	%
	% If the programmer specified a field name, we use that,
	% otherwise we just use `F' followed by the field number.
	%
	(
		MaybeFieldName = yes(QualifiedFieldName),
		unqualify_name(QualifiedFieldName, FieldName)
	;
		MaybeFieldName = no,
		FieldName = string__format("F%d", [i(ArgNum)])
	).

	% Succeed iff the specified type must be boxed when used as a field.
	% We need to box types that are not word-sized, because the code
	% for `arg' etc. in std_util.m rely on all arguments being word-sized.
ml_must_box_field_type(Type, ModuleInfo) :-
	classify_type(Type, ModuleInfo, Category),
	( Category = float_type
	; Category = char_type
	).

%-----------------------------------------------------------------------------%
%
% Code for handling success and failure
%

	% Generate code to succeed in the given code_model.
	%
ml_gen_success(model_det, _, MLDS_Statements) -->
	%
	% det succeed:
	%	<do true>
	% ===>
	%	/* just fall through */
	%
	{ MLDS_Statements = [] }.
ml_gen_success(model_semi, Context, [SetSuccessTrue]) -->
	%
	% semidet succeed:
	%	<do true>
	% ===>
	%	succeeded = TRUE;
	%
	ml_gen_set_success(const(true), Context, SetSuccessTrue).
ml_gen_success(model_non, Context, [CallCont]) -->
	%
	% nondet succeed:
	%	<true && SUCCEED()>
	% ===>
	%	SUCCEED()
	%
	ml_gen_call_current_success_cont(Context, CallCont).

	% Generate code to fail in the given code_model.
	%
ml_gen_failure(model_det, _, _) -->
	% this should never happen
	{ error("ml_code_gen: `fail' has determinism `det'") }.
ml_gen_failure(model_semi, Context, [SetSuccessFalse]) -->
	%
	% semidet fail:
	%	<do fail>
	% ===>
	%	succeeded = FALSE;
	%
	ml_gen_set_success(const(false), Context, SetSuccessFalse).
ml_gen_failure(model_non, _, MLDS_Statements) -->
	%
	% nondet fail:
	%	<fail && SUCCEED()>
	% ===>
	%	/* just fall through */
	%
	{ MLDS_Statements = [] }.

%-----------------------------------------------------------------------------%

	% Generate the declaration for the built-in `succeeded' variable.
	%
ml_gen_succeeded_var_decl(Context) =
	ml_gen_mlds_var_decl(var(var_name("succeeded", no)),
		mlds__native_bool_type, Context).

	% Return the lval for the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
ml_success_lval(SucceededLval) -->
	ml_gen_var_lval(var_name("succeeded", no),
		mlds__native_bool_type, SucceededLval).

	% Return an rval which will test the value of the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
ml_gen_test_success(SucceededRval) -->
	ml_success_lval(SucceededLval),
	{ SucceededRval = lval(SucceededLval) }.
	
	% Generate code to set the `succeeded' flag to the
	% specified truth value.
	%
ml_gen_set_success(Value, Context, MLDS_Statement) -->
	ml_success_lval(Succeeded),
	{ MLDS_Statement = ml_gen_assign(Succeeded, Value, Context) }.

%-----------------------------------------------------------------------------%

	% Generate the name for the specified `cond_<N>' variable.
	%
:- func ml_gen_cond_var_name(cond_seq) = mlds__var_name.
ml_gen_cond_var_name(CondVar) =
	mlds__var_name(string__append("cond_", string__int_to_string(CondVar)),
		no).

ml_gen_cond_var_decl(CondVar, Context) =
	ml_gen_mlds_var_decl(var(ml_gen_cond_var_name(CondVar)),
		mlds__native_bool_type, Context).

ml_cond_var_lval(CondVar, CondVarLval) -->
	ml_gen_var_lval(ml_gen_cond_var_name(CondVar), mlds__native_bool_type,
		CondVarLval).

ml_gen_test_cond_var(CondVar, CondVarRval) -->
	ml_cond_var_lval(CondVar, CondVarLval),
	{ CondVarRval = lval(CondVarLval) }.
	
ml_gen_set_cond_var(CondVar, Value, Context, MLDS_Statement) -->
	ml_cond_var_lval(CondVar, CondVarLval),
	{ MLDS_Statement = ml_gen_assign(CondVarLval, Value, Context) }.

%-----------------------------------------------------------------------------%

ml_initial_cont(OutputVarLvals0, OutputVarTypes0, Cont) -->
	{ ml_skip_dummy_argument_types(OutputVarTypes0, OutputVarLvals0,
		OutputVarTypes, OutputVarLvals) },
	list__map_foldl(ml_gen_type, OutputVarTypes, MLDS_OutputVarTypes),
	%
	% We expect OutputVarlvals0 and OutputVarTypes0 to be empty if
	% `--nondet-copy-out' is not enabled.
	%
	ml_gen_var_lval(mlds__var_name("cont", no), 
		mlds__cont_type(MLDS_OutputVarTypes), ContLval),
	ml_gen_var_lval(mlds__var_name("cont_env_ptr", no),
		mlds__generic_env_ptr_type, ContEnvLval),
	{ Cont = success_cont(lval(ContLval), lval(ContEnvLval),
		MLDS_OutputVarTypes, OutputVarLvals) }.

:- pred ml_skip_dummy_argument_types(list(prog_type), list(T),
		list(prog_type), list(T)).
:- mode ml_skip_dummy_argument_types(in, in, out, out) is det.

ml_skip_dummy_argument_types([], [], [], []).
ml_skip_dummy_argument_types([Type | Types0], [Var | Vars0],
		Types, Vars) :-
	ml_skip_dummy_argument_types(Types0, Vars0, Types1, Vars1),
	( type_util__is_dummy_argument_type(Type) ->
		Types = Types1,
		Vars = Vars1
	;
		Types = [Type | Types1],
		Vars = [Var | Vars1]
	).
ml_skip_dummy_argument_types([_|_], [], _, _) :-
	error("ml_skip_dummy_argument_types: length mismatch").
ml_skip_dummy_argument_types([], [_|_], _, _) :-
	error("ml_skip_dummy_argument_types: length mismatch").

	% Generate code to call the current success continuation.
	% This is used for generating success when in a model_non context.
	%
ml_gen_call_current_success_cont(Context, MLDS_Statement) -->
	ml_gen_info_current_success_cont(SuccCont),
	{ SuccCont = success_cont(FuncRval, EnvPtrRval,
		ArgTypes0, ArgLvals0) },
	{ ArgRvals0 = list__map(func(Lval) = lval(Lval), ArgLvals0) },
	ml_gen_info_use_gcc_nested_functions(UseNestedFuncs),
	( { UseNestedFuncs = yes } ->
		{ ArgTypes = ArgTypes0 },
		{ ArgRvals = ArgRvals0 }
	;
		{ ArgTypes = list__append(ArgTypes0,
			[mlds__generic_env_ptr_type]) },
		{ ArgRvals = list__append(ArgRvals0, [EnvPtrRval]) }
	),
	{ RetTypes = [] },
	{ Signature = mlds__func_signature(ArgTypes, RetTypes) },
	{ ObjectRval = no },
	{ RetLvals = [] },
	{ CallOrTailcall = call },
	{ MLDS_Stmt = call(Signature, FuncRval, ObjectRval, ArgRvals, RetLvals,
			CallOrTailcall) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
			mlds__make_context(Context)) }.

	% XXX this code is quite similar to some of the existing code
	% for calling continuations when doing copy-in/copy-out.
	% Sharing code should be investigated.

ml_gen_call_current_success_cont_indirectly(Context, MLDS_Statement) -->

		% We generate a call to the success continuation, just
		% as usual.
	ml_gen_info_current_success_cont(SuccCont),
	{ SuccCont = success_cont(ContinuationFuncRval, EnvPtrRval,
		ArgTypes0, ArgLvals0) },
	{ ArgRvals0 = list__map(func(Lval) = lval(Lval), ArgLvals0) },
	ml_gen_info_use_gcc_nested_functions(UseNestedFuncs),
	( { UseNestedFuncs = yes } ->
		{ ArgTypes = ArgTypes0 },
		{ ArgRvals = ArgRvals0 }
	;
		{ ArgTypes = list__append(ArgTypes0,
			[mlds__generic_env_ptr_type]) },
		{ ArgRvals = list__append(ArgRvals0, [EnvPtrRval]) }
	),
	{ RetTypes = [] },
	{ Signature = mlds__func_signature(ArgTypes, RetTypes) },
	{ ObjectRval = no },
	{ RetLvals = [] },
	{ CallOrTailcall = call },

	{ MLDS_Context = mlds__make_context(Context) },
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, PredModule) },
	{ MLDS_Module = mercury_module_name_to_mlds(PredModule) },

		% We generate a nested function that does the real call
		% to the continuation.
		%
		% All we do is change the call rvals to be the input
		% variables, and the func rval to be the input variable
		% for the continuation.
	ml_gen_cont_params(ArgTypes0, InnerFuncParams0),
	{ InnerFuncParams0 = func_params(InnerArgs0, Rets) },
	{ InnerArgRvals = list__map(
		(func(Data - Type) 
		= lval(var(qual(MLDS_Module, VarName), Type)) :-
			( Data = data(var(VarName0)) ->
				VarName = VarName0		
			;
				error("expected variable name in continuation parameters")
			)
		), 
			InnerArgs0) },
	{ InnerFuncArgType = mlds__cont_type(ArgTypes0) },
	{ PassedContVarName = mlds__var_name("passed_cont", no) },
	{ InnerFuncRval = lval(var(qual(MLDS_Module, PassedContVarName),
		InnerFuncArgType)) },
	{ InnerFuncParams = func_params(
		[data(var(PassedContVarName)) - InnerFuncArgType | InnerArgs0],
			Rets) },

	{ InnerMLDS_Stmt = call(Signature, InnerFuncRval, ObjectRval, 
			InnerArgRvals, RetLvals, CallOrTailcall) },
	{ InnerMLDS_Statement = statement(InnerMLDS_Stmt, MLDS_Context) },

	ml_gen_label_func(1, InnerFuncParams, Context, 
		InnerMLDS_Statement, Defn),

	{ ProxySignature = mlds__func_signature([InnerFuncArgType | ArgTypes],
		RetTypes) },
	{ ProxyArgRvals = [ContinuationFuncRval | ArgRvals] },

	{ 
		Defn = mlds__defn(function(PredLabel, ProcId, 
			yes(SeqNum), _), _, _, function(_, _,
			defined_here(_), _))
	->
		% We call the proxy function.
		QualProcLabel = qual(MLDS_Module, PredLabel - ProcId),
		ProxyFuncRval = const(code_addr_const(
			internal(QualProcLabel, SeqNum, ProxySignature))),

		% Put it inside a block where we call it.
		MLDS_Stmt = call(ProxySignature, ProxyFuncRval, ObjectRval,
			ProxyArgRvals, RetLvals, CallOrTailcall),
		MLDS_Statement = mlds__statement(
			block([Defn], [statement(MLDS_Stmt, MLDS_Context)]), 
			MLDS_Context)
	;
		error("success continuation generated was not a function")
	}.

%-----------------------------------------------------------------------------%
%
% Routines for dealing with the environment pointer
% used for nested functions.
%

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
	% Note that we generate this as a dangling reference.
	% The ml_elim_nested pass will insert the declaration
	% of the env_ptr variable.  At this point the type of these rvals 
	% is `mlds__unknown_type'.  
	%
ml_get_env_ptr(lval(EnvPtrLval)) -->
	ml_gen_var_lval(mlds__var_name("env_ptr", no),
		mlds__unknown_type, EnvPtrLval).

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
ml_declare_env_ptr_arg(Name - mlds__generic_env_ptr_type) -->
	{ Name = data(var(mlds__var_name("env_ptr_arg", no))) }.

%-----------------------------------------------------------------------------%
%
% The definition of the `ml_gen_info' ADT.
%

%
% The `ml_gen_info' type holds information used during MLDS code generation
% for a given procedure.
%
% Only the `func_label', `commit_label', `cond_var', `conv_var', `const_num',
% `var_lvals', `success_cont_stack', and `extra_defns' fields are mutable;
% the others are set when the `ml_gen_info' is created and then never
% modified.
% 

:- type ml_gen_info
	--->	ml_gen_info(
			%
			% these fields remain constant for each procedure
			%

			module_info :: module_info,
			pred_id :: pred_id,
			proc_id :: proc_id,
			varset :: prog_varset,
			var_types :: map(prog_var, prog_type),
			byref_output_vars :: list(prog_var),
				% output arguments that are passed by
				% reference
			value_output_vars :: list(prog_var),
				% output arguments that are returned as values

			%
			% these fields get updated as we traverse
			% each procedure
			%

				% XXX we should use the standard library
				% `counter' type here
			func_label :: mlds__func_sequence_num,
			commit_label :: commit_sequence_num,
			label :: label_num,
			cond_var :: cond_seq,
			conv_var :: conv_seq,
			const_num :: const_seq,
			const_num_map :: map(prog_var, const_seq),
			success_cont_stack :: stack(success_cont),
				% a partial mapping from vars to lvals,
				% used to override the normal lval
				% that we use for a variable
			var_lvals :: map(prog_var, mlds__lval),
				% definitions of functions or global
				% constants which should be inserted
				% before the definition of the function
				% for the current procedure
			extra_defns :: mlds__defns
		).

ml_gen_info_init(ModuleInfo, PredId, ProcId) = MLDSGenInfo :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_argmodes(ProcInfo, HeadModes),
	ByRefOutputVars = select_output_vars(ModuleInfo, HeadVars, HeadModes,
		VarTypes),
	ValueOutputVars = [],

	LabelCounter = 0,
	FuncLabelCounter = 0,
	CommitLabelCounter = 0,
	CondVarCounter = 0,
	ConvVarCounter = 0,
	ConstCounter = 0,
	map__init(ConstNumMap),
	stack__init(SuccContStack),
	map__init(VarLvals),
	ExtraDefns = [],

	MLDSGenInfo = ml_gen_info(
			ModuleInfo,
			PredId,
			ProcId,
			VarSet,
			VarTypes,
			ByRefOutputVars,
			ValueOutputVars,
			LabelCounter,
			FuncLabelCounter,
			CommitLabelCounter,
			CondVarCounter,
			ConvVarCounter,
			ConstCounter,
			ConstNumMap,
			SuccContStack,
			VarLvals,
			ExtraDefns
		).

ml_gen_info_get_module_info(Info, Info^module_info).

ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) :-
	ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo),
	module_info_name(ModuleInfo, ModuleName).

ml_gen_info_get_pred_id(Info, Info^pred_id).
ml_gen_info_get_proc_id(Info, Info^proc_id).
ml_gen_info_get_varset(Info, Info^varset).
ml_gen_info_get_var_types(Info, Info^var_types).
ml_gen_info_get_byref_output_vars(Info, Info^byref_output_vars).
ml_gen_info_get_value_output_vars(Info, Info^value_output_vars).
ml_gen_info_set_byref_output_vars(OutputVars, Info,
		Info^byref_output_vars := OutputVars).
ml_gen_info_set_value_output_vars(OutputVars, Info,
		Info^value_output_vars := OutputVars).

ml_gen_info_use_gcc_nested_functions(UseNestedFuncs) -->
	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, gcc_nested_functions,
		UseNestedFuncs) }.

ml_gen_info_put_commit_in_own_func(PutCommitInNestedFunc) -->
	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, put_commit_in_own_func,
		PutCommitInNestedFunc) }.

ml_gen_info_get_globals(Globals) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ module_info_globals(ModuleInfo, Globals) }.

ml_gen_info_new_label(Label, Info, Info^label := Label) :-
	Label = Info^label + 1.

ml_gen_info_new_func_label(Label, Info, Info^func_label := Label) :-
	Label = Info^func_label + 1.

ml_gen_info_bump_func_label(Info,
	Info^func_label := Info^func_label + 10000).

ml_gen_info_new_commit_label(CommitLabel, Info,
		Info^commit_label := CommitLabel) :-
	CommitLabel = Info^commit_label + 1.

ml_gen_info_new_cond_var(CondVar, Info, Info^cond_var := CondVar) :-
	CondVar = Info^cond_var + 1.

ml_gen_info_new_conv_var(ConvVar, Info, Info^conv_var := ConvVar) :-
	ConvVar = Info^conv_var + 1.

ml_gen_info_new_const(ConstVar, Info, Info^const_num := ConstVar) :-
	ConstVar = Info^const_num + 1.

ml_gen_info_set_const_num(Var, ConstVar, Info,
	Info^const_num_map := map__set(Info^const_num_map, Var, ConstVar)).

ml_gen_info_lookup_const_num(Var, ConstVar, Info, Info) :-
	ConstVar = map__lookup(Info^const_num_map, Var).

ml_gen_info_push_success_cont(SuccCont, Info,
	Info^success_cont_stack :=
		stack__push(Info^success_cont_stack, SuccCont)).

ml_gen_info_pop_success_cont(Info0, Info) :-
	Stack0 = Info0^success_cont_stack,
	stack__pop_det(Stack0, _SuccCont, Stack),
	Info = (Info0^success_cont_stack := Stack).

ml_gen_info_current_success_cont(SuccCont, Info, Info) :-
	stack__top_det(Info^success_cont_stack, SuccCont).

ml_gen_info_set_var_lval(Var, Lval, Info,
		Info^var_lvals := map__set(Info^var_lvals, Var, Lval)).

ml_gen_info_get_var_lvals(Info, Info^var_lvals).
ml_gen_info_set_var_lvals(VarLvals, Info, Info^var_lvals := VarLvals).

ml_gen_info_add_extra_defn(ExtraDefn, Info,
	Info^extra_defns := [ExtraDefn | Info^extra_defns]).

ml_gen_info_get_extra_defns(Info, Info^extra_defns).

%-----------------------------------------------------------------------------%

	% Given a list of variables and their corresponding modes,
	% return a list containing only those variables which have
	% an output mode.
	%
select_output_vars(ModuleInfo, HeadVars, HeadModes, VarTypes) = OutputVars :-
	( HeadVars = [], HeadModes = [] ->
		OutputVars = []
	; HeadVars = [Var|Vars], HeadModes = [Mode|Modes] ->
		map__lookup(VarTypes, Var, VarType),
		(
			\+ mode_to_arg_mode(ModuleInfo, Mode, VarType, top_in)
		->
			OutputVars1 = select_output_vars(ModuleInfo,
					Vars, Modes, VarTypes),
			OutputVars = [Var | OutputVars1]
		;
			OutputVars = select_output_vars(ModuleInfo,
					Vars, Modes, VarTypes)
		)
	;
		error("select_output_vars: length mismatch")
	).

%-----------------------------------------------------------------------------%

	% This function returns the offset to add to the argument
	% number of a closure arg to get its field number.
	%	field 0 is the closure layout
	%	field 1 is the closure address
	%	field 2 is the number of arguments
	%	field 3 is the 1st argument field
	%	field 4 is the 2nd argument field,
	%	etc.
	% Hence the offset to add to the argument number
	% to get the field number is 2.
ml_closure_arg_offset = 2.

	% This function returns the offset to add to the argument
	% number of a typeclass_info arg to get its field number.
	% The Nth extra argument to pass to the method is
	% in field N of the typeclass_info, so the offset is zero.
ml_typeclass_info_arg_offset = 0.

	% This function returns the offset to add to the method number
	% for a type class method to get its field number within the
	% base_typeclass_info. 
	%	field 0 is num_extra
	%	field 1 is num_constraints
	%	field 2 is num_superclasses
	%	field 3 is class_arity
	%	field 4 is num_methods
	%	field 5 is the 1st method
	%	field 6 is the 2nd method
	%	etc.
	%	(See the base_typeclass_info type in rtti.m or the
	%	description in notes/type_class_transformation.html for
	%	more information about the layout of base_typeclass_infos.)
	% Hence the offset is 4.
ml_base_typeclass_info_method_offset = 4.

%-----------------------------------------------------------------------------%
%
% Miscellaneous routines
%

	% Get the value of the appropriate --det-copy-out or --nondet-copy-out
	% option, depending on the code model.
get_copy_out_option(Globals, CodeModel) = CopyOut :-
	( CodeModel = model_non ->
		globals__lookup_bool_option(Globals,
			nondet_copy_out, CopyOut)
	;	
		globals__lookup_bool_option(Globals,
			det_copy_out, CopyOut)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
