%---------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: var_locn.m
% Author: zs.
%
% This module defines a set of predicates that operate on the abstract
% 'var_locn_info' structure which maintains information about where variables
% are stored, what their values are if they are not stored anywhere,
% and which registers are reserved for purposes such as holding the arguments
% of calls and tags that are to be switched upon.

%----------------------------------------------------------------------------%

:- module ll_backend__var_locn.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_llds.
:- import_module ll_backend__llds.
:- import_module libs__options.

:- import_module bool, list, assoc_list, map, set, std_util.

:- type var_locn_info.

%	var_locn__init_state(Arguments, Liveness, Varset,
%			StackSlots, FollowVars, Opts, VarLocnInfo)
%		Produces an initial state of the VarLocnInfo given
%		an association list of variables and lvalues. The initial
%		state places the given variables at their corresponding
%		locations, with the exception of variables which are not in
%		Liveness (this corresponds to input arguments that are not
%		used in the body). The Varset parameter contains a mapping from
%		variables to names, which is used when code is generated
%		to provide meaningful comments. StackSlots maps each variable
%		to its stack slot, if it has one. FollowVars is the initial
%		follow_vars set; such sets give guidance as to what lvals
%		(if any) each variable will be needed in next. Opts gives
%		the table of options; this is used to decide what expressions
%		are considered constants.

:- pred var_locn__init_state(assoc_list(prog_var, lval)::in, set(prog_var)::in,
	prog_varset::in, stack_slots::in, follow_vars::in, option_table::in,
	var_locn_info::out) is det.

%	var_locn__reinit_state(VarLocs, VarLocnInfo0, VarLocnInfo)
%		Produces a new state of the VarLocnInfo in which the static
%		and mostly static information (stack slot map, follow vars map,
%		varset, option settings) comes from VarLocnInfo0 but the
%		dynamic state regarding variable locations is thrown away
%		and then rebuilt from the information in VarLocs, an
%		association list of variables and lvals. The new state
%		places the given variables at their corresponding locations.

:- pred var_locn__reinit_state(assoc_list(prog_var, lval)::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__clobber_all_regs(OkToDeleteAny, VarLocnInfo0, VarLocnInfo)
%		Modifies the state VarLocnInfo0 to produce VarLocnInfo
%		in which all variables stored in registers are clobbered.
%		Aborts if this deletes the last record of the state of a
%		variable unless OkToDeleteAny is `yes'.

:- pred var_locn__clobber_all_regs(bool::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__clobber_regs(Regs, VarLocnInfo0, VarLocnInfo)
%		Modifies the state VarLocnInfo0 to produce VarLocnInfo
%		in which all variables stored in Regs (a list of lvals
%		which should contain only registers) are clobbered.

:- pred var_locn__clobber_regs(list(lval)::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__set_magic_var_location(Var, Lval, VarLocnInfo0, VarLocnInfo)
%		Modifies VarLocnInfo0 to produce VarLocnInfo in which
%		Var is *magically* stored in Lval. Does not care if Lval
%		is already in use; it overwrites it with the new information.
%		Used to implement the ends of erroneous branches.

:- pred var_locn__set_magic_var_location(prog_var::in, lval::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__check_and_set_magic_var_location(Var, Lval,
%			VarLocnInfo0, VarLocnInfo)
%		Modifies VarLocnInfo0 to produce VarLocnInfo in which
%		Var is *magically* stored in Lval. (The caller usually
%		generates code to perform this magic.) Aborts if Lval
%		is already in use.

:- pred var_locn__check_and_set_magic_var_location(prog_var::in, lval::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__lval_in_use(Lval, VarLocnInfo0, VarLocnInfo)
%		Succeeds iff Lval, which should be a register or stack slot,
%		holds (a path to) a variable or is otherwise reserved.

:- pred var_locn__lval_in_use(lval::in, var_locn_info::in, var_locn_info::out)
	is semidet.

%	var_locn__var_becomes_dead(Var, FirstTime, VarLocnInfo0, VarLocnInfo)
%		Frees any code generator resources used by Var in VarLocnInfo0
%		to produce VarLocnInfo. FirstTime should be no if this same
%		operation may already have been executed on Var; otherwise,
%		var_locn__var_becomes_dead will throw an exception if it does
%		not know about Var.
:- pred var_locn__var_becomes_dead(prog_var::in, bool::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__assign_var_to_var(Var, AssignedVar,
%			VarLocnInfo0, VarLocnInfo)
%		Reflects the effect of the assignment Var := AssignedVar in the
%		state of VarLocnInfo0 to yield VarLocnInfo.

:- pred var_locn__assign_var_to_var(prog_var::in, prog_var::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__assign_lval_to_var(Var, Lval, Code, VarLocnInfo0, VarLocnInfo)
%		Reflects the effect of the assignment Var := lval(Lval) in the
%		state of VarLocnInfo0 to yield VarLocnInfo; any code required
%		to effect the assignment will be returned in Code.

:- pred var_locn__assign_lval_to_var(prog_var::in, lval::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__assign_const_to_var(Var, ConstRval,
%			VarLocnInfo0, VarLocnInfo)
%		Reflects the effect of the assignment Var := const(ConstRval)
%		in the state of VarLocnInfo0 to yield VarLocnInfo.

:- pred var_locn__assign_const_to_var(prog_var::in, rval::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__assign_expr_to_var(Var, Rval, Code,
%			VarLocnInfo0, VarLocnInfo)
%		Generates code to execute the assignment Var := Expr, and
%		updates the state of VarLocnInfo0 accordingly.
%
%		Expr must contain no lvals, although it may (and typically
%		will) refer to the values of other variables through rvals
%		of the form var(_).

:- pred var_locn__assign_expr_to_var(prog_var::in, rval::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__assign_cell_to_var(Var, Ptag, Vector, CellNum, TypeMsg, Code,
%			VarLocnInfo0, VarLocnInfo)
%		Generates code to assign to Var a pointer, tagged by Ptag, to
%		the cell whose contents are given by the other arguments,
%		and updates the state of VarLocnInfo0 accordingly.

:- pred var_locn__assign_cell_to_var(prog_var::in, tag::in,
	list(maybe(rval))::in, int::in, string::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__place_var(Var, Lval, Code, VarLocnInfo0, VarLocnInfo)
%		Produces Code and a modified version of VarLocnInfo0,
%		VarLocnInfo which places the value of Var in Lval.

:- pred var_locn__place_var(prog_var::in, lval::in, code_tree::out,
		var_locn_info::in, var_locn_info::out) is det.

%	var_locn__place_vars(VarLocns, Code, VarLocnInfo0, VarLocnInfo)
%		Produces Code and a modified version of VarLocnInfo0,
%		VarLocnInfo which places the value of each variable
%		mentioned in VarLocns into the corresponding location.

:- pred var_locn__place_vars(assoc_list(prog_var, lval)::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__produce_var(Var, Rval, Code, VarLocnInfo0, VarLocnInfo)
%		Return the preferred way to refer to the value of Var
%		(which may be a const rval, or the value in an lval).
%
% 		If Var is currently a cached expression, then produce_var
%		will generate Code to evaluate the expression and put it
%		into an lval. (Since the code generator can ask for a variable
%		to be produced more than once, this is necessary to prevent
%		the expression, which may involve a possibly large number
%		of operations, from being evaluated several times.) Otherwise,
%		Code will be empty.

:- pred var_locn__produce_var(prog_var::in, rval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__produce_var_in_reg(Var, Lval, Code,
%			VarLocnInfo0, VarLocnInfo)
%		Produces a code fragment Code to evaluate Var if necessary
%		and provide it as an Lval of the form reg(_).

:- pred var_locn__produce_var_in_reg(prog_var::in, lval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__produce_var_in_reg_or_stack(Var, FollowVars, Lval, Code,
%			VarLocnInfo0, VarLocnInfo)
%		Produces a code fragment Code to evaluate Var if necessary
%		and provide it as an Lval of the form reg(_), stackvar(_),
%		or framevar(_).

:- pred var_locn__produce_var_in_reg_or_stack(prog_var::in, lval::out,
	code_tree::out, var_locn_info::in, var_locn_info::out) is det.

%	var_locn__acquire_reg(Lval, VarLocnInfo0, VarLocnInfo)
%		Finds an unused register and marks it as 'in use'.

:- pred var_locn__acquire_reg(lval::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__acquire_reg_require_given(Reg, Lval,
%			VarLocInfo0, VarLocInfo)
%		Marks Reg, which must be an unused register, as 'in use'.

:- pred var_locn__acquire_reg_require_given(lval::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__acquire_reg_prefer_given(Pref, Lval,
%			VarLocInfo0, VarLocInfo)
%		Finds an unused register, and marks it as 'in use'.
%		If Pref itself is free, assigns that.

:- pred var_locn__acquire_reg_prefer_given(int::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__acquire_reg_start_at_given(Start, Lval,
%			VarLocInfo0, VarLocInfo)
%		Finds an unused register, and marks it as 'in use'.
%		It starts the search at the one numbered Start,
%		continuing towards higher register numbers.

:- pred var_locn__acquire_reg_start_at_given(int::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__release_reg(Lval, VarLocnInfo, VarLocnInfo)
%		Marks a previously acquired reg as no longer 'in use'.

:- pred var_locn__release_reg(lval::in, var_locn_info::in, var_locn_info::out)
	is det.

%	var_locn__lock_regs(N, Exceptions, VarLocnInfo, VarLocnInfo)
%		Prevents registers r1 through rN from being reused, even if
%		there are no variables referring to them, with the exceptions
%		of the registers named in Exceptions, which however can only be
%		used to store their corresponding variables. Should be followed
%		by a call to var_locn__unlock_regs.

:- pred var_locn__lock_regs(int::in, assoc_list(prog_var, lval)::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__unlock_regs(VarLocnInfo0, VarLocnInfo)
%		Undoes a lock operation.

:- pred var_locn__unlock_regs(var_locn_info::in, var_locn_info::out) is det.

%	var_locn__clear_r1(Code)
%		Produces a code fragment Code to move whatever is in r1
%		to some other register, if r1 is live.  This is used
%		prior to semidet pragma c_codes.

:- pred var_locn__clear_r1(code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__materialize_vars_in_lval(Lval, FinalLval, Code,
%			VarLocnInfo0, VarLocnInfo)
%		For every variable in Lval, substitutes the value of the
%		variable and returns it as FinalLval. If we need to save the
%		values of some of the substituted variables somewhere so as to
%		prevent them from being evaluated again (and again ...), the
%		required code will be returned in Code.

:- pred var_locn__materialize_vars_in_lval(lval::in, lval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__get_var_locations(VarLocnInfo, Locations)
%		Returns a map from each live variable that occurs in
%		VarLocnInfo to the set of locations in which it may be found
%		(which may be empty, if the variable's value is either a known
%		constant, or an as-yet unevaluated expression).

:- pred var_locn__get_var_locations(var_locn_info::in,
	map(prog_var, set(lval))::out) is det.

%	var_locn__get_stack_slots(StackSlots)
%		Returns the table mapping each variable to its stack slot
%		(if any).

:- pred var_locn__get_stack_slots(stack_slots::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__get_follow_vars(FollowVars)
%		Returns the table mapping each variable to the lval (if any)
%		where it is desired next.

:- pred var_locn__get_follow_var_map(follow_vars_map::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__get_next_non_reserved(NonRes)
%		Returns the number of the first register which is free for
%		general use.

:- pred var_locn__get_next_non_reserved(int::out,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__set_follow_vars(FollowVars)
%		Sets the table mapping each variable to the lval (if any)
%		where it is desired next, and the number of the first
%		non-reserved register.

:- pred var_locn__set_follow_vars(follow_vars::in,
	var_locn_info::in, var_locn_info::out) is det.

%	var_locn__max_reg_in_use(MaxReg)
%		Returns the number of the highest numbered rN register in use.

:- pred var_locn__max_reg_in_use(var_locn_info::in, int::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend__code_util, ll_backend__exprn_aux.
:- import_module libs__options, libs__tree.
:- import_module int, string, bag, require, getopt, varset, term.

:- type dead_or_alive	--->	dead ; alive.

	% The state of a variable can be one of three kinds: const, cached
	% and general.
	%
	% 1.	The value of the variable is a known constant. In this case,
	%	the const_rval field will be yes, and the expr_rval field
	%	will be no. Both the empty set and nonempty sets are valid
	%	for the locs field. It will start out empty, will become
	%	nonempty if the variable is placed in some lval, and may
	%	become empty again if that lval is later overwritten.
	%
	% 2.	The value of the variable is not stored anywhere, but its
	%	definition (an expression involving other variables) is cached.
	%	In this case, the const_rval field will be no, and the locs
	%	field will contain the empty set, but the expr_rval field
	%	will be yes. The variables referred to in the expr_rval field
	%	will include this variable in their using_vars sets, which
	%	protects them from deletion from the code generator state until
	%	the using variable is produced or placed in an lval. When that
	%	happens, the using variable's state will be transformed to the
	%	general, third kind, releasing this variable's hold on the
	%	variables contained in its expr_rval field.
	%
	% 3.	The value of the variable is not a constant, nor is the
	%	variable cached. The locs field will be nonempty, and both
	%	const_rval and expr_rval will be no.

:- type var_state	--->
	state(
		locs		:: set(lval),	% must not contain var(_)
		const_rval	:: maybe(rval),	% must not contain var(_),
						% must be constant
		expr_rval	:: maybe(rval), % will contain var(_),
						% must not contain lvals
		using_vars	:: set(prog_var),
						% the set of vars whose
						% expr_rval field refers
						% to this var
		dead_or_alive	:: dead_or_alive
						% a dead variable should be
						% removed from var_state_map
						% when its using_vars field
						% becomes empty
	).

:- type var_state_map	==	map(prog_var, var_state).


	% The loc_var_map maps each root lval (register or stack slot)
	% to the set of variables that depend on that location,
	% either because they are stored there or because the location
	% contains a part of the pointer chain that leads to their address.
	% In concrete terms, this means the set of variables whose var_state's
	% locs field includes an lval that contains that root lval.
	%
	% If a root lval stack slot is unused, then it will either not appear
	% in the var_loc_map or it will be mapped to an empty set. Allowing
	% unused root lvals to be mapped to the empty set, and not requiring
	% their deletion from the map, makes it simpler to manipulate
	% loc_var_maps using higher-order code.

:- type loc_var_map	==	map(lval, set(prog_var)).

:- type var_locn_info	--->
	var_locn_info(
		varset		:: prog_varset,	% The varset from the
						% proc_info.
		stack_slots 	:: stack_slots,	% Maps each var to its stack
						% slot, if it has one.
		exprn_opts	:: exprn_opts,	% The values of the options
						% that are relevant to
						% decisions about which rvals
						% are constants.
		follow_vars_map	:: follow_vars_map,
						% Where vars are needed next.
		next_non_res	:: int,		% Next register that is not
						% reserved in follow_vars_map.
		var_state_map	:: var_state_map,
						% Documented above.
		loc_var_map	:: loc_var_map, % Documented above.
		acquired	:: set(lval),	% Locations that are
						% temporarily reserved for
						% purposes such as holding the
						% tags of variables during
						% switches.
		locked		:: int,		% If this slot contains N, then
						% registers r1 through rN
						% can only be modified by
						% a place_var operation,
						% or by a free_up_lval
						% operation that moves a
						% variable to the (free or
						% freeable) lval associated
						% with it in the exceptions.
						% field. Used to implement
						% calls, pragma_c_codes and the
						% store_maps at the ends of
						% branched control structures.
		exceptions	:: assoc_list(prog_var, lval)
						% See the documentation of the
						% locked field above.
	).

%----------------------------------------------------------------------------%

var_locn__init_state(VarLocs, Liveness, Varset, StackSlots, FollowVars,
		Options, VarLocnInfo) :-
	map__init(VarStateMap0),
	map__init(LocVarMap0),
	var_locn__init_state_2(VarLocs, yes(Liveness),
		VarStateMap0, VarStateMap, LocVarMap0, LocVarMap),
	exprn_aux__init_exprn_opts(Options, ExprnOpts),
	FollowVars = follow_vars(FollowVarMap, NextNonReserved),
	set__init(AcquiredRegs),
	VarLocnInfo = var_locn_info(Varset, StackSlots, ExprnOpts,
		FollowVarMap, NextNonReserved, VarStateMap, LocVarMap,
		AcquiredRegs, 0, []).

var_locn__reinit_state(VarLocs, VarLocnInfo0, VarLocnInfo) :-
	map__init(VarStateMap0),
	map__init(LocVarMap0),
	var_locn__init_state_2(VarLocs, no, VarStateMap0, VarStateMap,
		LocVarMap0, LocVarMap),
	set__init(AcquiredRegs),
	VarLocnInfo0 = var_locn_info(Varset, StackSlots, ExprnOpts,
		FollowVarMap, NextNonReserved, _, _, _, _, _),
	VarLocnInfo = var_locn_info(Varset, StackSlots, ExprnOpts,
		FollowVarMap, NextNonReserved, VarStateMap, LocVarMap,
		AcquiredRegs, 0, []).

:- pred var_locn__init_state_2(assoc_list(prog_var, lval)::in,
	maybe(set(prog_var))::in, var_state_map::in, var_state_map::out,
	loc_var_map::in, loc_var_map::out) is det.

var_locn__init_state_2([], _, VarStateMap, VarStateMap, LocVarMap, LocVarMap).
var_locn__init_state_2([Var - Lval |  Rest], MaybeLiveness,
		VarStateMap0, VarStateMap, LocVarMap0, LocVarMap) :-
	require(var_locn__is_root_lval(Lval),
		"var_locn__init_state_2: unexpected lval"),
	(
		MaybeLiveness = yes(Liveness),
		\+ set__member(Var, Liveness)
	->
			% If a variable is not live, then we do not record its
			% state. If we did, then the variable will never die
			% (since it is already dead), and the next call to
			% clobber_regs would throw an exception, since it would
			% believe that it is throwing away the last location
			% storing the value of a "live" variable.
		VarStateMap1 = VarStateMap0,
		LocVarMap1 = LocVarMap0
	;
		( map__search(VarStateMap0, Var, _) ->
			error("var_locn__init_state_2: repeated variable")
		;
			set__singleton_set(NewLocs, Lval),
			set__init(Using),
			State = state(NewLocs, no, no, Using, alive),
			map__det_insert(VarStateMap0, Var, State, VarStateMap1)
		),
		var_locn__make_var_depend_on_lval_roots(Var, Lval,
			LocVarMap0, LocVarMap1)
	),
	var_locn__init_state_2(Rest, MaybeLiveness, VarStateMap1, VarStateMap,
		LocVarMap1, LocVarMap).

%----------------------------------------------------------------------------%

var_locn__get_var_locations(VarLocnInfo, VarLocations) :-
	var_locn__get_var_state_map(VarStateMap, VarLocnInfo, _),
	map__to_assoc_list(VarStateMap, VarLocList),
	list__filter_map(var_locn__convert_live_to_lval_set, VarLocList,
		LiveVarLocList),
	map__from_assoc_list(LiveVarLocList, VarLocations).

:- pred var_locn__convert_live_to_lval_set(pair(prog_var, var_state)::in,
	pair(prog_var, set(lval))::out) is semidet.

var_locn__convert_live_to_lval_set(Var - State, Var - Lvals) :-
	State = state(Lvals, _, _, _, alive).

%----------------------------------------------------------------------------%

var_locn__clobber_all_regs(OkToDeleteAny) -->
	{ set__init(Acquired) },
	var_locn__set_acquired(Acquired),
	var_locn__set_locked(0),
	var_locn__set_exceptions([]),
	var_locn__get_loc_var_map(LocVarMap0),
	var_locn__get_var_state_map(VarStateMap0),
	{ map__keys(LocVarMap0, Locs) },
	{ var_locn__clobber_regs_in_maps(Locs, OkToDeleteAny,
		LocVarMap0, LocVarMap, VarStateMap0, VarStateMap) },
	var_locn__set_loc_var_map(LocVarMap),
	var_locn__set_var_state_map(VarStateMap).

var_locn__clobber_regs(Regs) -->
	var_locn__get_acquired(Acquired0),
	{ Acquired = set__delete_list(Acquired0, Regs) },
	var_locn__set_acquired(Acquired),
	var_locn__get_loc_var_map(LocVarMap0),
	var_locn__get_var_state_map(VarStateMap0),
	{ var_locn__clobber_regs_in_maps(Regs, no,
		LocVarMap0, LocVarMap, VarStateMap0, VarStateMap) },
	var_locn__set_loc_var_map(LocVarMap),
	var_locn__set_var_state_map(VarStateMap).

:- pred var_locn__clobber_regs_in_maps(list(lval)::in, bool::in,
	loc_var_map::in, loc_var_map::out,
	var_state_map::in, var_state_map::out) is det.

var_locn__clobber_regs_in_maps([], _, LocVarMap, LocVarMap,
		VarStateMap, VarStateMap).
var_locn__clobber_regs_in_maps([Lval | Lvals], OkToDeleteAny,
		LocVarMap0, LocVarMap, VarStateMap0, VarStateMap) :-
	(
		Lval = reg(_, _),
		map__search(LocVarMap0, Lval, DependentVarsSet)
	->
		map__delete(LocVarMap0, Lval, LocVarMap1),
		set__to_sorted_list(DependentVarsSet, DependentVars),
		list__foldl(var_locn__clobber_lval_in_var_state_map(Lval, [],
			OkToDeleteAny),
			DependentVars, VarStateMap0, VarStateMap1)
	;
		LocVarMap1 = LocVarMap0,
		VarStateMap1 = VarStateMap0
	),
	var_locn__clobber_regs_in_maps(Lvals, OkToDeleteAny,
		LocVarMap1, LocVarMap, VarStateMap1, VarStateMap).

:- pred var_locn__clobber_lval_in_var_state_map(lval::in, list(prog_var)::in,
	bool::in, prog_var::in, var_state_map::in, var_state_map::out) is det.

var_locn__clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny,
		Var, VarStateMap0, VarStateMap) :-
	(
		var_locn__try_clobber_lval_in_var_state_map(Lval,
			OkToDeleteVars, OkToDeleteAny, Var,
			VarStateMap0, VarStateMap1)
	->
		VarStateMap = VarStateMap1
	;
		error("var_locn__clobber_lval_in_var_state_map: empty state")
	).

% Try to record in VarStateMap that Var is no longer reachable through (paths
% including) Lval. If this deletes the last possible place where the value of
% Var can be found, and Var is not in OkToDeleteVars, then fail.

:- pred var_locn__try_clobber_lval_in_var_state_map(lval::in,
	list(prog_var)::in, bool::in, prog_var::in,
	var_state_map::in, var_state_map::out) is semidet.

var_locn__try_clobber_lval_in_var_state_map(Lval, OkToDeleteVars,
		OkToDeleteAny, Var, VarStateMap0, VarStateMap) :-
	map__lookup(VarStateMap0, Var, State0),
	State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval, Using,
		DeadOrAlive),
	LvalSet = set__filter(var_locn__lval_does_not_support_lval(Lval),
		LvalSet0),
	State = state(LvalSet, MaybeConstRval, MaybeExprRval, Using,
		DeadOrAlive),
	(
		var_locn__nonempty_state(State)
	;
		list__member(Var, OkToDeleteVars)
	;
		OkToDeleteAny = yes
	;
		DeadOrAlive = dead,
		set__to_sorted_list(Using, UsingVars),
		var_locn__recursive_using_vars_dead_and_ok_to_delete(UsingVars,
			VarStateMap0, OkToDeleteVars)
	),
	map__det_update(VarStateMap0, Var, State, VarStateMap).

:- pred var_locn__recursive_using_vars_dead_and_ok_to_delete(
	list(prog_var)::in, var_state_map::in, list(prog_var)::in) is semidet.

var_locn__recursive_using_vars_dead_and_ok_to_delete([], _, _).
var_locn__recursive_using_vars_dead_and_ok_to_delete([Var | Vars],
		VarStateMap, OkToDeleteVars) :-
	(
		list__member(Var, OkToDeleteVars)
	;
		map__lookup(VarStateMap, Var, State),
		State = state(_, _, _, Using, DeadOrAlive),
		DeadOrAlive = dead,
		set__to_sorted_list(Using, UsingVars),
		var_locn__recursive_using_vars_dead_and_ok_to_delete(UsingVars,
			VarStateMap, OkToDeleteVars)
	),
	var_locn__recursive_using_vars_dead_and_ok_to_delete(Vars,
		VarStateMap, OkToDeleteVars).

%----------------------------------------------------------------------------%

var_locn__assign_var_to_var(Var, OldVar) -->
	var_locn__check_var_is_unknown(Var),

	var_locn__get_var_state_map(VarStateMap0),
	{ map__lookup(VarStateMap0, OldVar, OldState0) },
	{ OldState0 = state(Lvals, MaybeConstRval, MaybeExprRval,
		Using0, DeadOrAlive) },
	{
		MaybeExprRval = yes(_),
		State = state(Lvals, MaybeConstRval, yes(var(OldVar)),
			set__init, alive),
		set__insert(Using0, Var, Using),
		OldState = state(Lvals, MaybeConstRval, MaybeExprRval,
			Using, DeadOrAlive),
		map__det_update(VarStateMap0, OldVar, OldState, VarStateMap1)
	;
		MaybeExprRval = no,
		set__init(Empty),
		State = state(Lvals, MaybeConstRval, no, Empty, alive),
		VarStateMap1 = VarStateMap0
	},
	{ map__det_insert(VarStateMap1, Var, State, VarStateMap) },
	var_locn__set_var_state_map(VarStateMap),

	var_locn__get_loc_var_map(LocVarMap0),
	{ var_locn__make_var_depend_on_lvals_roots(Var, Lvals,
		LocVarMap0, LocVarMap) },
	var_locn__set_loc_var_map(LocVarMap).

%----------------------------------------------------------------------------%

var_locn__assign_lval_to_var(Var, Lval0, Code) -->
	var_locn__check_var_is_unknown(Var),

	(
		{ Lval0 = field(yes(Ptag), var(BaseVar),
			const(int_const(Offset))) }
	->
		var_locn__get_var_state_map(VarStateMap0),
		{ map__lookup(VarStateMap0, BaseVar, BaseState) },
		{ BaseState = state(BaseVarLvals, MaybeConstBaseVarRval,
			_MaybeExprRval, _UsingVars, _DeadOrAlive) },
		(
			{ MaybeConstBaseVarRval = yes(BaseVarRval) },
			{ BaseVarRval = create(Ptag, BaseVarArgs, _,_,_,_,_) }
		->
			{ list__index0_det(BaseVarArgs, Offset, SelectedArg) },
			{ MaybeConstRval = SelectedArg },
			{ Lvals = set__map(var_locn__add_field_offset(
				yes(Ptag), const(int_const(Offset))),
				BaseVarLvals) },
			{ set__init(Using) },
			{ State = state(Lvals, MaybeConstRval, no,
				Using, alive) },
			{ map__det_insert(VarStateMap0, Var, State,
				VarStateMap) },
			var_locn__set_var_state_map(VarStateMap),

			var_locn__get_loc_var_map(LocVarMap0),
			{ var_locn__make_var_depend_on_lvals_roots(Var, Lvals,
				LocVarMap0, LocVarMap) },
			var_locn__set_loc_var_map(LocVarMap)
		;
			{ set__init(Lvals) },
			{ Expr = lval(Lval0) },
			{ set__init(Using) },
			{ State = state(Lvals, no, yes(Expr), Using, alive) },
			{ map__det_insert(VarStateMap0, Var, State,
				VarStateMap1) },
			{ var_locn__add_use_ref(BaseVar, Var,
				VarStateMap1, VarStateMap) },
			var_locn__set_var_state_map(VarStateMap)
		),
		{ Code = empty }
	;
		var_locn__materialize_vars_in_lval(Lval0, Lval, Code),

		var_locn__get_var_state_map(VarStateMap0),
		{ set__singleton_set(LvalSet, Lval) },
		{ State = state(LvalSet, no, no, set__init, alive) },
		{ map__det_insert(VarStateMap0, Var, State, VarStateMap) },
		var_locn__set_var_state_map(VarStateMap),

		var_locn__get_loc_var_map(LocVarMap0),
		{ var_locn__make_var_depend_on_lval_roots(Var, Lval,
			LocVarMap0, LocVarMap) },
		var_locn__set_loc_var_map(LocVarMap)
	).

:- func var_locn__add_field_offset(maybe(tag), rval, lval) = lval.

var_locn__add_field_offset(Ptag, Offset, Base) =
	field(Ptag, lval(Base), Offset).

%----------------------------------------------------------------------------%

var_locn__assign_const_to_var(Var, ConstRval0) -->
	var_locn__check_var_is_unknown(Var),

	var_locn__get_var_state_map(VarStateMap0),
	var_locn__get_exprn_opts(ExprnOpts),
	(
		{ var_locn__expr_is_constant(ConstRval0, VarStateMap0,
			ExprnOpts, ConstRval) }
	->
		{ State = state(set__init, yes(ConstRval), no,
			set__init, alive) },
		{ map__det_insert(VarStateMap0, Var, State, VarStateMap) },
		var_locn__set_var_state_map(VarStateMap)
	;
		{ error("var_locn__set_var_state_map: supposed constant isn't") }
	).

%----------------------------------------------------------------------------%

var_locn__assign_expr_to_var(Var, Rval, empty) -->
	var_locn__check_var_is_unknown(Var),

	var_locn__get_var_state_map(VarStateMap0),
	{ State = state(set__init, no, yes(Rval), set__init, alive) },
	{ map__det_insert(VarStateMap0, Var, State, VarStateMap1) },

	{ exprn_aux__vars_in_rval(Rval, ContainedVars0) },
	{ list__remove_dups(ContainedVars0, ContainedVars) },
	{ var_locn__add_use_refs(ContainedVars, Var,
		VarStateMap1, VarStateMap) },
	var_locn__set_var_state_map(VarStateMap).

:- pred var_locn__add_use_refs(list(prog_var)::in, prog_var::in,
	var_state_map::in, var_state_map::out) is det.

var_locn__add_use_refs([], _, VarStateMap, VarStateMap).
var_locn__add_use_refs([ContainedVar | ContainedVars], UsingVar,
		VarStateMap0, VarStateMap) :-
	var_locn__add_use_ref(ContainedVar, UsingVar,
		VarStateMap0, VarStateMap1),
	var_locn__add_use_refs(ContainedVars, UsingVar,
		VarStateMap1, VarStateMap).

:- pred var_locn__add_use_ref(prog_var::in, prog_var::in,
	var_state_map::in, var_state_map::out) is det.

var_locn__add_use_ref(ContainedVar, UsingVar, VarStateMap0, VarStateMap) :-
	map__lookup(VarStateMap0, ContainedVar, State0),
	State0 = state(Lvals, MaybeConstRval, MaybeExprRval,
		Using0, DeadOrAlive),
	set__insert(Using0, UsingVar, Using),
	State = state(Lvals, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive),
	map__det_update(VarStateMap0, ContainedVar, State, VarStateMap).

%----------------------------------------------------------------------------%

var_locn__assign_cell_to_var(Var, Ptag, Vector, CellNum, TypeMsg, Code) -->
	{ Reuse = no },
	{ CellRval0 = create(Ptag, Vector, uniform(no), can_be_either,
		CellNum, TypeMsg, Reuse) },
	(
		var_locn__get_var_state_map(VarStateMap),
		var_locn__get_exprn_opts(ExprnOpts),
		{ var_locn__expr_is_constant(CellRval0, VarStateMap, ExprnOpts,
			CellRval) }
	->
		var_locn__assign_const_to_var(Var, CellRval),
		{ Code = empty }
	;
		var_locn__assign_dynamic_cell_to_var(Var, Ptag, Vector,
			TypeMsg, Code)
	).

:- pred var_locn__assign_dynamic_cell_to_var(prog_var::in, tag::in,
	list(maybe(rval))::in, string::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__assign_dynamic_cell_to_var(Var, Ptag, Vector, TypeMsg, Code) -->
	var_locn__check_var_is_unknown(Var),

	var_locn__select_preferred_reg_or_stack(Var, Lval),
	var_locn__get_var_name(Var, VarName),
	{ list__length(Vector, Size) },
	{ CellCode = node([
		incr_hp(Lval, yes(Ptag), const(int_const(Size)), TypeMsg)
			- string__append("Allocating heap for ", VarName)
	]) },
	var_locn__set_magic_var_location(Var, Lval),
	var_locn__assign_cell_args(Vector, yes(Ptag), lval(Lval), 0, ArgsCode),
	{ Code = tree(CellCode, ArgsCode) }.

:- pred var_locn__assign_cell_args(list(maybe(rval))::in,
	maybe(tag)::in, rval::in, int::in, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__assign_cell_args([], _, _, _, empty) --> [].
var_locn__assign_cell_args([MaybeRval0 | MaybeRvals0], Ptag, Base, Offset,
		Code) -->
	( { MaybeRval0 = yes(Rval0) } ->
		{ Target = field(Ptag, Base, const(int_const(Offset))) },
		( { Rval0 = var(Var) } ->
			var_locn__find_var_availability(Var, no, Avail),
			(
				{ Avail = available(Rval) },
				{ EvalCode = empty }
			;
				{ Avail = needs_materialization },
				var_locn__materialize_var(Var, no, no, [],
					Rval, EvalCode)
			),
			var_locn__add_additional_lval_for_var(Var, Target),
			var_locn__get_var_name(Var, VarName),
			{ Comment = string__append("assigning from ",
				VarName) }
		; { Rval0 = const(_) } ->
			{ Rval = Rval0 },
			{ EvalCode = empty },
			{ Comment = "assigning field from const" }
		; { Rval0 = create(_, _, _, _, _, _, _) } ->
			{ Rval = Rval0 },
			{ EvalCode = empty },
			{ Comment = "assigning field from const struct" }
		;
			{ error("var_locn__assign_cell_args: unknown rval") }
		),
		{ AssignCode = node([
			assign(Target, Rval) - Comment
		]) },
		{ ThisCode = tree(EvalCode, AssignCode) }
	;
		{ ThisCode = empty }
	),
	var_locn__assign_cell_args(MaybeRvals0, Ptag, Base, Offset + 1,
		RestCode),
	{ Code = tree(ThisCode, RestCode) }.

%----------------------------------------------------------------------------%

% Record that Var is now available in Lval, as well as in the locations
% where it was available before.

:- pred var_locn__add_additional_lval_for_var(prog_var::in, lval::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__add_additional_lval_for_var(Var, Lval) -->
	var_locn__get_loc_var_map(LocVarMap0),
	{ var_locn__make_var_depend_on_lval_roots(Var, Lval,
		LocVarMap0, LocVarMap) },
	var_locn__set_loc_var_map(LocVarMap),

	var_locn__get_var_state_map(VarStateMap0),
	{ map__lookup(VarStateMap0, Var, State0) },
	{ State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval0,
		Using, DeadOrAlive) },
	{ set__insert(LvalSet0, Lval, LvalSet) },
	{ State = state(LvalSet, MaybeConstRval, no, Using, DeadOrAlive) },
	{ map__det_update(VarStateMap0, Var, State, VarStateMap) },
	var_locn__set_var_state_map(VarStateMap),

	var_locn__remove_use_refs(MaybeExprRval0, Var).

:- pred var_locn__remove_use_refs(maybe(rval)::in, prog_var::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__remove_use_refs(MaybeExprRval, UsingVar) -->
	(
		{ MaybeExprRval = yes(ExprRval) },
		{ exprn_aux__vars_in_rval(ExprRval, ContainedVars0) },
		{ list__remove_dups(ContainedVars0, ContainedVars) },
		var_locn__remove_use_refs_2(ContainedVars, UsingVar)
	;
		{ MaybeExprRval = no }
	).

:- pred var_locn__remove_use_refs_2(list(prog_var)::in, prog_var::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__remove_use_refs_2([], _) --> [].
var_locn__remove_use_refs_2([ContainedVar | ContainedVars], UsingVar) -->
	var_locn__get_var_state_map(VarStateMap0),
	{ map__lookup(VarStateMap0, ContainedVar, State0) },
	{ State0 = state(Lvals, MaybeConstRval, MaybeExprRval,
		Using0, DeadOrAlive) },
	{ set__remove(Using0, UsingVar, Using1) ->
		Using = Using1
	;
		error("var_locn__remove_use_refs_2: using ref not present")
	},
	{ State = state(Lvals, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive) },
	{ map__det_update(VarStateMap0, ContainedVar, State,
		VarStateMap) },
	var_locn__set_var_state_map(VarStateMap),
	(
		{ set__empty(Using) },
		{ DeadOrAlive = dead }
	->
		var_locn__var_becomes_dead(ContainedVar, no)
	;
		[]
	),
	var_locn__remove_use_refs_2(ContainedVars, UsingVar).

%----------------------------------------------------------------------------%

% Check that Lval was previously not in use, and record that Var has
% "magically" appeared there (i.e. our caller has arranged for it to be put
% there).

var_locn__check_and_set_magic_var_location(Var, Lval) -->
	( var_locn__lval_in_use(Lval) ->
		{ error("var_locn__check_and_set_magic_var_location: in use") }
	;
		var_locn__set_magic_var_location(Var, Lval)
	).

% Record that Var has "magically" appeared in Lval (i.e. our caller has
% arranged for it to be put there). Var must not have been previously known.

var_locn__set_magic_var_location(Var, Lval) -->
	var_locn__get_loc_var_map(LocVarMap0),
	{ var_locn__make_var_depend_on_lval_roots(Var, Lval,
		LocVarMap0, LocVarMap) },
	var_locn__set_loc_var_map(LocVarMap),

	var_locn__get_var_state_map(VarStateMap0),
	{ set__singleton_set(LvalSet, Lval) },
	{ State = state(LvalSet, no, no, set__init, alive) },
	{ map__det_insert(VarStateMap0, Var, State, VarStateMap) },
	var_locn__set_var_state_map(VarStateMap).

%----------------------------------------------------------------------------%

:- pred var_locn__check_var_is_unknown(prog_var::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__check_var_is_unknown(Var) -->
	var_locn__get_var_state_map(VarStateMap0),
	( { map__search(VarStateMap0, Var, _) } ->
		var_locn__get_var_name(Var, Name),
		{ string__append("var_locn__assign_to_var: existing definition of variable ",
			Name, Msg) },
		{ error(Msg) }
	;
		[]
	).

%----------------------------------------------------------------------------%

var_locn__produce_var(Var, Rval, Code) -->
	var_locn__get_var_state_map(VarStateMap),
	{ map__lookup(VarStateMap, Var, State) },
	{ State = state(Lvals, MaybeConstRval, _, _, _) },
	{ set__to_sorted_list(Lvals, LvalsList) },
	(
		{ var_locn__maybe_select_lval_or_rval(LvalsList,
			MaybeConstRval, Rval1) }
	->
		{ Rval = Rval1 },
		{ Code = empty }
	;
		var_locn__select_preferred_reg(Var, Lval),
		var_locn__place_var(Var, Lval, Code),
		{ Rval = lval(Lval) }
	).

var_locn__produce_var_in_reg(Var, Lval, Code) -->
	var_locn__get_var_state_map(VarStateMap),
	{ map__lookup(VarStateMap, Var, State) },
	{ State = state(Lvals, _, _, _, _) },
	{ set__to_sorted_list(Lvals, LvalList) },
	( { var_locn__select_reg_lval(LvalList, SelectLval) } ->
		{ Lval = SelectLval },
		{ Code = empty }
	;
		var_locn__select_preferred_reg(Var, Lval),
		var_locn__place_var(Var, Lval, Code)
	).

var_locn__produce_var_in_reg_or_stack(Var, Lval, Code) -->
	var_locn__get_var_state_map(VarStateMap),
	{ map__lookup(VarStateMap, Var, State) },
	{ State = state(Lvals, _, _, _, _) },
	{ set__to_sorted_list(Lvals, LvalList) },
	( { var_locn__select_reg_or_stack_lval(LvalList, SelectLval) } ->
		{ Lval = SelectLval },
		{ Code = empty }
	;
		var_locn__select_preferred_reg_or_stack(Var, Lval),
		var_locn__place_var(Var, Lval, Code)
	).

%----------------------------------------------------------------------------%

var_locn__clear_r1(Code) -->
	var_locn__free_up_lval(reg(r, 1), [], [], Code),

	var_locn__get_loc_var_map(LocVarMap0),
	var_locn__get_var_state_map(VarStateMap0),
	{ var_locn__clobber_regs_in_maps([reg(r, 1)], no,
		LocVarMap0, LocVarMap, VarStateMap0, VarStateMap) },
	var_locn__set_loc_var_map(LocVarMap),
	var_locn__set_var_state_map(VarStateMap).

% If we are asked to place several variables, then we must make sure that in
% the process of freeing up an lval for one variable, we do not save its
% previous contents to a location that VarLocns assigns to another variable.
% This is why we lock the registers used by VarLocns. (We don't need to lock
% stack slots, since stack slot allocation is required to ensure that the sets
% of variables that need to be save across calls or at entries to goals with
% resume points all have distinct stack slots.) However, we do make one
% exception: if the variable being moved by a freeing up operation is in
% VarLocns, then it is OK to move it to the location assigned to it by
% VarLocns.

var_locn__place_vars(VarLocns, Code) -->
	{ assoc_list__values(VarLocns, Lvals) },
	{ code_util__max_mentioned_reg(Lvals, MaxReg) },
	var_locn__lock_regs(MaxReg, VarLocns),
	var_locn__actually_place_vars(VarLocns, Code),
	var_locn__unlock_regs.

:- pred var_locn__actually_place_vars(assoc_list(prog_var, lval)::in,
	code_tree::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__actually_place_vars([], empty) --> [].
var_locn__actually_place_vars([Var - Lval | Rest], Code) -->
	var_locn__place_var(Var, Lval, FirstCode),
	var_locn__actually_place_vars(Rest, RestCode),
	{ Code = tree(FirstCode, RestCode) }.

var_locn__place_var(Var, Target, Code) -->
	var_locn__actually_place_var(Var, Target, [], Code).

:- pred var_locn__actually_place_var(prog_var::in, lval::in, list(lval)::in,
	code_tree::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__actually_place_var(Var, Target, ForbiddenLvals, Code) -->
	var_locn__get_acquired(Acquired),
	{ set__member(Target, Acquired) ->
		error("var_locn__actually_place_var: target is acquired reg")
	;
		true
	},
	var_locn__get_var_state_map(VarStateMap0),
	{ map__lookup(VarStateMap0, Var, State0) },
	{ State0 = state(Lvals0, _, _, _, _) },
	( { set__member(Target, Lvals0) } ->
		{ Code = empty }
	;
		var_locn__free_up_lval(Target, [Var], ForbiddenLvals,
			FreeCode),

			% If Var's value is cached, Lvals0 must be empty.
			% However, the cached value may simply be var(Other),
			% and Other may already be in Target. However, it may
			% also be in another lval, so we say we prefer the
			% copy in Target.
		var_locn__find_var_availability(Var, yes(Target), Avail),
		(
			{ Avail = available(Rval) },
			{ EvalCode = empty },
			( { Rval = lval(SourceLval) } ->
				var_locn__record_copy(SourceLval, Target)
			;
				var_locn__record_clobbering(Target, [Var])
			)
		;
			{ Avail = needs_materialization },
			var_locn__materialize_var(Var, yes(Target), no,
				[Target], Rval, EvalCode),
			var_locn__record_clobbering(Target, [Var])
		),

			% Record that Var is now in Target.
		var_locn__add_additional_lval_for_var(Var, Target),

		( { Rval = lval(Target) } ->
			{ AssignCode = empty }
		;
			var_locn__get_var_name(Var, VarName),
			{ ForbiddenLvals = [] ->
				string__append("Placing ", VarName, Msg)
			;
				string__int_to_string(
					list__length(ForbiddenLvals),
					LengthStr),
				string__append_list(["Placing ", VarName,
					" (depth ", LengthStr, ")"], Msg)
			},
			{ AssignCode = node([
				assign(Target, Rval)
					- Msg
			]) }
		),
		{ Code = tree(FreeCode, tree(EvalCode, AssignCode)) }
	).

:- pred var_locn__record_clobbering(lval::in, list(prog_var)::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__record_clobbering(Target, Assigns) -->
	var_locn__get_loc_var_map(LocVarMap1),
	( { map__search(LocVarMap1, Target, DependentVarsSet) } ->
		{ set__to_sorted_list(DependentVarsSet, DependentVars) },
		{ map__delete(LocVarMap1, Target, LocVarMap) },
		var_locn__set_loc_var_map(LocVarMap),

		var_locn__get_var_state_map(VarStateMap2),
		{ list__foldl(
			var_locn__clobber_lval_in_var_state_map(Target,
				Assigns, no),
			DependentVars, VarStateMap2, VarStateMap) },
		var_locn__set_var_state_map(VarStateMap)
	;
		[]
	).

% Make Lval available, i.e. make sure that the values of all variables
% that are stored in an lval involving Lval are also available in places
% not dependent on Lval. However, this requirement does not apply to the
% variables (if any) in ToBeAssignedVars, since this lists the variable
% that is to be assigned to Lval after it is freed up. (If ToBeAssignedVars
% contains more than one variable, then those variables must be guaranteed
% to be equal.) Nor does it apply to dead variables whose only use is as
% components of AssignedVars.
%
% The point of this exception is to eliminate unnecessary shuffles. If
% var_locn__place_var wants to put Var in Lval and Var is currently in (e.g)
% field(Ptag, Lval, Offset), it will ask var_locn__free_up_lval to free up
% Lval. However, if all the other variables affected variables are also
% available independently of Lval, there should be no need to move the value
% now in Lval somewhere else, since our caller can simply generate an
% assignment such as Lval := field(Ptag, Lval, Offset).

:- pred var_locn__free_up_lval(lval::in, list(prog_var)::in, list(lval)::in,
	code_tree::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__free_up_lval(Lval, ToBeAssignedVars, ForbiddenLvals, Code) -->
	(
		var_locn__get_loc_var_map(LocVarMap0),
		{ map__search(LocVarMap0, Lval, AffectedVarSet) },
		{ set__to_sorted_list(AffectedVarSet, AffectedVars) },
		var_locn__get_var_state_map(VarStateMap0),
		\+ { list__foldl(
			var_locn__try_clobber_lval_in_var_state_map(
				Lval, ToBeAssignedVars, no),
			AffectedVars, VarStateMap0, _) }
	->
		var_locn__free_up_lval_with_copy(Lval, ToBeAssignedVars,
			ForbiddenLvals, Code)
	;
		{ Code = empty }
	).

% If we must copy the value in Lval somewhere else to prevent it from being
% lost when Lval overwritten, then we try to put it into a location where it
% will be needed next. First we find a variable that is stored in Lval
% directly, and not just in some location whose path includes Lval (the set
% of all variables affected by the update of Lval is AffectedVarSet). Then we
% look up where that variable (OccupyingVar) ought to be. If its desired
% location is Lval itself, then the copy would be a null operation and would
% not free up Lval, so in that case we get a spare register. If the desired
% location is on the forbidden list, then we again get a spare register to
% avoid infinite recursion (see the documentation of var_locn__free_up_lval
% above). If the desired location (Pref) is neither Lval nor or on the
% forbidden list, then we can possibly copy Lval there. If Pref is neither
% in use nor locked, then moving Lval there requires just an assignment.
% If Pref is locked, then it is possible that it is locked for use by
% OccupyingVar. If this is so, we first recursively free up Pref, and then
% move OccupyingVar there.

:- pred var_locn__free_up_lval_with_copy(lval::in, list(prog_var)::in,
	list(lval)::in, code_tree::out, var_locn_info::in, var_locn_info::out)
	is det.

var_locn__free_up_lval_with_copy(Lval, ToBeAssignedVars, ForbiddenLvals, Code)
		-->
	(
		var_locn__get_loc_var_map(LocVarMap0),
		{ map__search(LocVarMap0, Lval, AffectedVarSet) },
		{ set__delete_list(AffectedVarSet, ToBeAssignedVars,
			EffAffectedVarSet) },
		{ set__to_sorted_list(EffAffectedVarSet, EffAffectedVars) },

		var_locn__get_var_state_map(VarStateMap0),
		(
			{ var_locn__find_one_occupying_var(EffAffectedVars,
				Lval, VarStateMap0, OccupyingVar,
				OtherSources) }
		->
			{ MovedVar = OccupyingVar },
			{ list__delete_all(EffAffectedVars, MovedVar,
				OtherVars) },
			list__foldl(var_locn__ensure_copies_are_present(
				Lval, OtherSources), OtherVars)
		;
			{ EffAffectedVars = [MovedVar] }
		),

		{ CheckInUse = no },
		var_locn__select_preferred_reg_or_stack(MovedVar, Pref,
			CheckInUse),
		{ \+ Pref = Lval },
		{ \+ list__member(Pref, ForbiddenLvals) },
		( \+ var_locn__lval_in_use(Pref) ->
			[]
		;
				% The code generator assumes that values in
				% stack slots don't get clobbered without an
				% explicit assignment (via a place_var
				% operation with a stack var as a target).
			{ Pref = reg(r, RegNum) },
			var_locn__reg_is_not_locked_for_var(RegNum, MovedVar)
		)
	->
		var_locn__actually_place_var(MovedVar, Pref,
			[Lval | ForbiddenLvals], Code)
	;
		var_locn__get_spare_reg(Target),
		var_locn__record_copy(Lval, Target),
		{ Code = node([
			assign(Target, lval(Lval))
				- "Freeing up the source lval"
		]) }
	).

% Find a variable in the given list that is currently stored directly in Lval
% (not just in some location who address includes Lval).

:- pred var_locn__find_one_occupying_var(list(prog_var)::in, lval::in,
	var_state_map::in, prog_var::out, list(lval)::out) is semidet.

var_locn__find_one_occupying_var([Var | Vars], Lval, VarStateMap, OccupyingVar,
		OtherSources) :-
	map__lookup(VarStateMap, Var, State),
	State = state(LvalSet, _, _, _, _),
	( set__member(Lval, LvalSet) ->
		OccupyingVar = Var,
		set__delete(LvalSet, Lval, OtherSourceSet),
		set__to_sorted_list(OtherSourceSet, OtherSources)
	;
		var_locn__find_one_occupying_var(Vars, Lval, VarStateMap,
			OccupyingVar, OtherSources)
	).

:- pred var_locn__ensure_copies_are_present(lval::in, list(lval)::in,
	prog_var::in, var_locn_info::in, var_locn_info::out) is det.

var_locn__ensure_copies_are_present(OneSource, OtherSources, Var) -->
	var_locn__get_var_state_map(VarStateMap0),
	{ map__lookup(VarStateMap0, Var, State0) },
	{ State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive) },
	{ set__to_sorted_list(LvalSet0, Lvals0) },
	{ list__foldl(var_locn__ensure_copies_are_present_lval(
		OtherSources, OneSource), Lvals0, LvalSet0, LvalSet) },
	{ State = state(LvalSet, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive) },
	{ map__det_update(VarStateMap0, Var, State, VarStateMap) },
	var_locn__set_var_state_map(VarStateMap),

	var_locn__get_loc_var_map(LocVarMap0),
	{ var_locn__record_change_in_root_dependencies(LvalSet0, LvalSet, Var,
		LocVarMap0, LocVarMap) },
	var_locn__set_loc_var_map(LocVarMap).

:- pred var_locn__ensure_copies_are_present_lval(list(lval)::in, lval::in,
	lval::in, set(lval)::in, set(lval)::out) is det.

var_locn__ensure_copies_are_present_lval([], _, _, LvalSet, LvalSet).
var_locn__ensure_copies_are_present_lval([OtherSource | OtherSources],
		OneSource, Lval, LvalSet0, LvalSet) :-
	SubstLval = var_locn__substitute_lval_in_lval(OneSource, OtherSource,
		Lval),
	set__insert(LvalSet0, SubstLval, LvalSet1),
	var_locn__ensure_copies_are_present_lval(OtherSources,
		OneSource, Lval, LvalSet1, LvalSet).

%----------------------------------------------------------------------------%

% Record the effect of the assignment New := Old on the state of all the
% affected variables.
%
% We find the set of affected variables by finding all the root lvals in
% New and Old, and finding all the variables that depend on them. This
% requires significant numbers of term traversals and lookup operations.
% We could eliminate this cost by considering *all* variables to be affected.
% Even though it would obviously call record_copy_for_var on more variables,
% this may be faster overall. The reason why we don't do that is that
% its worst case behavior can be pretty bad.

:- pred var_locn__record_copy(lval::in, lval::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__record_copy(Old, New) -->
	{ require(var_locn__is_root_lval(New),
		"var_locn__record_copy: non-root New lval") },
	var_locn__get_var_state_map(VarStateMap0),
	var_locn__get_loc_var_map(LocVarMap0),
	{ set__list_to_set([Old, New], AssignSet) },
	{ var_locn__get_var_set_roots(AssignSet, NoDupRootLvals) },
		% Convert the list of root lvals to the list of sets of
		% affected vars; if a root lval is not in LocVarMap0,
		% then it does not affect any variables.
	{ list__filter_map(map__search(LocVarMap0), NoDupRootLvals,
		AffectedVarSets) },
		% Take the union of the list of sets of affected vars.
	{ list__foldl(set__union, AffectedVarSets,
		set__init, AffectedVarSet) },
		% Convert the union set to a list of affected vars.
	{ set__to_sorted_list(AffectedVarSet, AffectedVars) },
	{ list__foldl2(var_locn__record_copy_for_var(Old, New),
		AffectedVars, VarStateMap0, VarStateMap,
		LocVarMap0, LocVarMap) },
	var_locn__set_loc_var_map(LocVarMap),
	var_locn__set_var_state_map(VarStateMap).

% Record the effect of the assignment New := Old on the state of the given
% variable.
%
% The main complication is that New and Old are not necessarily independent:
% it is possible e.g. for New to be r1 and Old to be field(0, r1, 2).
% This is why we perform the update in three steps.
%
% 1	For each lval in original LvalSet that contains Old, we add an
%	additional lval in which Old is replaced by a unique Token lval
%	(which cannot be a component of any legitimate lval). The Token
%	represents the value being assigned, and prevents us from forgetting
%	the path to the original value of Var from Old during step 2.
%
% 2	We delete from the set generated by step 1 all lvals that depend
%	on New, to reflect the fact that New no longer contains what it
%	used to contain.
%
% 3	We substitute New for all occurrences of Token, to reflect the fact
%	that the assigned value is now in New.
%
% For example, with New and Old being as above and LvalSet0 being the set
% { r5, field(3, field(0, r1, 2), 4) }:
%
% -	Step 1 will set LvalSet1 to { r5, field(3, Token, 4) }, and LvalSet2
% 	to { r5, field(3, field(0, r1, 2), 4), field(3, Token, 4) }.
%
% -	Step 2 will set LvalSet3 { r5, field(3, Token, 4) }.
%
% -	Step 3 will set LvalSet { r5, field(3, r1, 4) }.
%
% The reason why we don't need to modify the MaybeExprRval field in the
% variable state is that the only lvals these fields can refer to are
% of the form var(_).

:- pred var_locn__record_copy_for_var(lval::in, lval::in, prog_var::in,
	var_state_map::in, var_state_map::out,
	loc_var_map::in, loc_var_map::out) is det.

var_locn__record_copy_for_var(Old, New, Var,
		VarStateMap0, VarStateMap, LocVarMap0, LocVarMap) :-
	map__lookup(VarStateMap0, Var, State0),
	State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive),
	Token = reg(r, -42),
	LvalSet1 = set__map(var_locn__substitute_lval_in_lval(Old, Token),
		LvalSet0),
	set__union(LvalSet0, LvalSet1, LvalSet2),
	LvalSet3 = set__filter(var_locn__lval_does_not_support_lval(New),
		LvalSet2),
	LvalSet = set__map(var_locn__substitute_lval_in_lval(Token, New),
		LvalSet3),
	State = state(LvalSet, MaybeConstRval, MaybeExprRval,
		Using, DeadOrAlive),
	require(var_locn__nonempty_state(State),
		"var_locn__record_copy_for_var: empty state"),
	map__det_update(VarStateMap0, Var, State, VarStateMap),

	var_locn__record_change_in_root_dependencies(LvalSet0, LvalSet, Var,
		LocVarMap0, LocVarMap).

:- pred var_locn__record_change_in_root_dependencies(set(lval)::in,
	set(lval)::in, prog_var::in, loc_var_map::in, loc_var_map::out) is det.

var_locn__record_change_in_root_dependencies(OldLvalSet, NewLvalSet, Var,
		LocVarMap0, LocVarMap) :-
	var_locn__get_var_set_roots(OldLvalSet, OldRootLvals),
	var_locn__get_var_set_roots(NewLvalSet, NewRootLvals),
	set__list_to_set(OldRootLvals, OldRootLvalSet),
	set__list_to_set(NewRootLvals, NewRootLvalSet),
	set__difference(NewRootLvalSet, OldRootLvalSet, InsertSet),
	set__difference(OldRootLvalSet, NewRootLvalSet, DeleteSet),
	set__to_sorted_list(InsertSet, Inserts),
	set__to_sorted_list(DeleteSet, Deletes),
	list__foldl(var_locn__make_var_depend_on_root_lval(Var),
		Inserts, LocVarMap0, LocVarMap1),
	list__foldl(var_locn__make_var_not_depend_on_root_lval(Var),
		Deletes, LocVarMap1, LocVarMap).

:- func var_locn__substitute_lval_in_lval(lval, lval, lval) = lval.

var_locn__substitute_lval_in_lval(Old, New, Lval0) = Lval :-
	exprn_aux__substitute_lval_in_lval(Old, New, Lval0, Lval).

%----------------------------------------------------------------------------%

% Var has become dead. If there are no expression that depend on its value,
% delete the record of its state, thus freeing up the resources it has
% tied down: the locations it occupies, or the variables whose values its own
% expression refers to. If there *are* expressions that depend on its value,
% merely update the state of the variable to say that it is dead, which means
% that its resources will be freed when the last reference to its value is
% deleted.
%
% If FirstTime = no, then it is possible that this predicate has already been
% called for Var, if FirstTime = yes, then as a consistency check we would like
% to insist on Var being alive (but don't (yet) due to bugs in liveness).

var_locn__var_becomes_dead(Var, FirstTime) -->
	var_locn__get_var_state_map(VarStateMap0),
	( { map__search(VarStateMap0, Var, State0) } ->
		{ State0 = state(Lvals, MaybeConstRval, MaybeExprRval,
			Using, DeadOrAlive0) },
		( { DeadOrAlive0 = dead } ->
			{ require(unify(FirstTime, no),
				"var_locn__var_becomes_dead: already dead") }
		;
			[]
		),
		( { set__empty(Using) } ->
			{ map__det_remove(VarStateMap0, Var, _, VarStateMap) },
			var_locn__set_var_state_map(VarStateMap),

			var_locn__get_loc_var_map(LocVarMap0),
			{ var_locn__get_var_set_roots(Lvals, NoDupRootLvals) },
			{ list__foldl(
				var_locn__make_var_not_depend_on_root_lval(
					Var),
				NoDupRootLvals, LocVarMap0, LocVarMap) },
			var_locn__set_loc_var_map(LocVarMap),

			var_locn__remove_use_refs(MaybeExprRval, Var)
		;
			{ State = state(Lvals, MaybeConstRval, MaybeExprRval,
				Using, dead) },
			{ map__det_update(VarStateMap0, Var, State,
				VarStateMap) },
			var_locn__set_var_state_map(VarStateMap)
		)
	;
		{ require(unify(FirstTime, no),
			"var_locn__var_becomes_dead: premature deletion") }
	).

% Given a set of lvals, return the set of root lvals among them and inside
% them.

:- pred var_locn__get_var_set_roots(set(lval)::in, list(lval)::out) is det.

var_locn__get_var_set_roots(Lvals, NoDupRootLvals) :-
	set__to_sorted_list(Lvals, LvalList),
	code_util__lvals_in_lvals(LvalList, ContainedLvals),
	list__append(LvalList, ContainedLvals, AllLvals),
	list__filter(var_locn__is_root_lval, AllLvals, RootLvals),
	list__sort_and_remove_dups(RootLvals, NoDupRootLvals).

%----------------------------------------------------------------------------%

% Select the cheapest way to refer to the value of the variable.

% From the given list of lvals, select the cheapest one to use.

:- pred var_locn__select_lval(list(lval)::in, lval::out) is det.

var_locn__select_lval(Lvals, Lval) :-
	( var_locn__select_reg_lval(Lvals, Lval1) ->
		Lval = Lval1
	; var_locn__select_stack_lval(Lvals, Lval2) ->
		Lval = Lval2
	; var_locn__select_cheapest_lval(Lvals, Lval3) ->
		Lval = Lval3
	;
		error("var_locn__select_lval: nothing to select")
	).

% From the given list of lvals and maybe a constant rval, select the cheapest
% one to use.

:- pred var_locn__select_lval_or_rval(list(lval)::in, maybe(rval)::in,
	rval::out) is det.

var_locn__select_lval_or_rval(Lvals, MaybeConstRval, Rval) :-
	( var_locn__maybe_select_lval_or_rval(Lvals, MaybeConstRval, Rval1) ->
		Rval = Rval1
	;
		error("var_locn__select_lval_or_rval: nothing to select")
	).

:- pred var_locn__maybe_select_lval_or_rval(list(lval)::in, maybe(rval)::in,
	rval::out) is semidet.

var_locn__maybe_select_lval_or_rval(Lvals, MaybeConstRval, Rval) :-
	( var_locn__select_reg_lval(Lvals, Lval1) ->
		Rval = lval(Lval1)
	; var_locn__select_stack_lval(Lvals, Lval2) ->
		Rval = lval(Lval2)
	; MaybeConstRval = yes(ConstRval) ->
		Rval = ConstRval
	; var_locn__select_cheapest_lval(Lvals, Lval3) ->
		Rval = lval(Lval3)
	;
		fail
	).

:- pred var_locn__select_reg_lval(list(lval)::in, lval::out) is semidet.

var_locn__select_reg_lval([Lval0 | Lvals0], Lval) :-
	( Lval0 = reg(_, _) ->
		Lval = Lval0
	;
		var_locn__select_reg_lval(Lvals0, Lval)
	).

:- pred var_locn__select_stack_lval(list(lval)::in, lval::out) is semidet.

var_locn__select_stack_lval([Lval0 | Lvals0], Lval) :-
	( ( Lval0 = stackvar(_) ; Lval0 = framevar(_)) ->
		Lval = Lval0
	;
		var_locn__select_stack_lval(Lvals0, Lval)
	).

:- pred var_locn__select_reg_or_stack_lval(list(lval)::in, lval::out)
	is semidet.

var_locn__select_reg_or_stack_lval([Lval0 | Lvals0], Lval) :-
	(
		( Lval0 = reg(_, _)
		; Lval0 = stackvar(_)
		; Lval0 = framevar(_)
		)
	->
		Lval = Lval0
	;
		var_locn__select_reg_or_stack_lval(Lvals0, Lval)
	).

:- pred var_locn__select_cheapest_lval(list(lval)::in, lval::out) is semidet.

	% From the given list of lvals, select the cheapest one to use.
	% Since none of the lvals will be a register or stack variable,
	% in almost all cases, the given list will be a singleton.
var_locn__select_cheapest_lval([Lval | _], Lval).

%----------------------------------------------------------------------------%

:- pred var_locn__select_preferred_reg_avoid(prog_var::in, list(lval)::in,
	lval::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__select_preferred_reg_avoid(Var, Avoid, Lval) -->
	var_locn__select_preferred_reg(Var, yes, Avoid, Lval).

:- pred var_locn__select_preferred_reg(prog_var::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__select_preferred_reg(Var, Lval) -->
	var_locn__select_preferred_reg(Var, yes, [], Lval).

% Select the register into which Var should be put. If the follow_vars map
% maps Var to a register, then select that register, unless it is already in
% use, and CheckInUse = yes.

:- pred var_locn__select_preferred_reg(prog_var::in, bool::in, list(lval)::in,
	lval::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__select_preferred_reg(Var, CheckInUse, Avoid, Lval) -->
	var_locn__get_follow_var_map(FollowVarMap),
	(
		{ map__search(FollowVarMap, Var, PrefLval) },
		{ PrefLval = reg(_, _) }
	->
		(
			{ real_lval(PrefLval) },
			( { CheckInUse = yes } ->
				\+ var_locn__lval_in_use(PrefLval)
			;
				[]
			),
			{ \+ list__member(PrefLval, Avoid) }
		->
			{ Lval = PrefLval }
		;
			var_locn__get_spare_reg_avoid(Avoid, Lval)
		)
	;
		var_locn__get_spare_reg_avoid(Avoid, Lval)
	).

% Select the register or stack slot into which Var should be put. If the
% follow_vars map maps Var to a register, then select that register,
% unless it is already in use and CheckInUse = yes. If the follow_vars map
% does not contain Var, then Var is not needed in a register in the near
% future, and this we select Var's stack slot, unless it is in use and
% CheckInUse = yes. If all else fails, we get spare, unused register.
% (Note that if the follow_vars pass has not been run, then all follow vars
% maps will be empty, which would cause this predicate to try to put far too
% many things in stack slots. This is why the --no-lazy-code option implies
% --follow-vars.)

:- pred var_locn__select_preferred_reg_or_stack(prog_var::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__select_preferred_reg_or_stack(Var, Lval) -->
	var_locn__select_preferred_reg_or_stack(Var, Lval, yes).

:- pred var_locn__select_preferred_reg_or_stack(prog_var::in, lval::out,
	bool::in, var_locn_info::in, var_locn_info::out) is det.

var_locn__select_preferred_reg_or_stack(Var, Lval, CheckInUse) -->
	var_locn__get_follow_var_map(FollowVarMap),
	(
		{ map__search(FollowVarMap, Var, PrefLval) },
		{ PrefLval = reg(_, _) }
	->
		(
			{ real_lval(PrefLval) },
			( { CheckInUse = yes } ->
				\+ var_locn__lval_in_use(PrefLval)
			;
				[]
			)
		->
			{ Lval = PrefLval }
		;
			var_locn__get_spare_reg(Lval)
		)
	;
		(
			var_locn__get_stack_slots(StackSlots),
			{ map__search(StackSlots, Var, StackSlot) },
			( { CheckInUse = yes } ->
				\+ var_locn__lval_in_use(StackSlot)
			;
				[]
			)
		->
			{ Lval = StackSlot }
		;
			var_locn__get_spare_reg(Lval)
		)
	).

:- pred real_lval(lval::in) is semidet.

real_lval(Lval) :-
	\+ (
		Lval = reg(_, N),
		N < 1
	).

%----------------------------------------------------------------------------%

% Get a register that is not in use. We start the search at the next register
% that is needed for the next call.

:- pred var_locn__get_spare_reg_avoid(list(lval)::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__get_spare_reg_avoid(Avoid, Lval) -->
	var_locn__get_next_non_reserved(NextNonReserved),
	var_locn__get_spare_reg_2(Avoid, NextNonReserved, Lval).

:- pred var_locn__get_spare_reg(lval::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__get_spare_reg(Lval) -->
	var_locn__get_next_non_reserved(NextNonReserved),
	var_locn__get_spare_reg_2([], NextNonReserved, Lval).

:- pred var_locn__get_spare_reg_2(list(lval)::in, int::in, lval::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__get_spare_reg_2(Avoid, N0, Lval) -->
	{ TryLval = reg(r, N0) },
	( var_locn__lval_in_use(TryLval) ->
		var_locn__get_spare_reg_2(Avoid, N0 + 1, Lval)
	; { list__member(TryLval, Avoid) } ->
		var_locn__get_spare_reg_2(Avoid, N0 + 1, Lval)
	;
		{ Lval = TryLval }
	).

% Succeeds if Lval is currently in use or locked.

var_locn__lval_in_use(Lval) -->
	var_locn__get_loc_var_map(LocVarMap),
	var_locn__get_acquired(Acquired),
	var_locn__get_locked(Locked),
	{
		map__search(LocVarMap, Lval, UsingVars),
		\+ set__empty(UsingVars)
	;
		set__member(Lval, Acquired)
	;
		Lval = reg(r, N),
		N =< Locked
	}.

% Succeeds if Var may be stored in Reg, possibly after copying its contents
% somewhere else. This requires Reg to be either not locked, or if it is
% locked, to be locked for Var.

:- pred var_locn__reg_is_not_locked_for_var(int::in, prog_var::in,
	var_locn_info::in, var_locn_info::out) is semidet.

var_locn__reg_is_not_locked_for_var(RegNum, Var) -->
	var_locn__get_acquired(Acquired),
	var_locn__get_locked(Locked),
	var_locn__get_exceptions(Exceptions),
	{
		Reg = reg(r, RegNum),
		\+ set__member(Reg, Acquired),
		RegNum =< Locked => list__member(Var - Reg, Exceptions)
	}.

%----------------------------------------------------------------------------%

var_locn__acquire_reg(Lval) -->
	var_locn__get_spare_reg(Lval),
	var_locn__get_acquired(Acquired0),
	{ set__insert(Acquired0, Lval, Acquired) },
	var_locn__set_acquired(Acquired).

var_locn__acquire_reg_require_given(Lval) -->
	( var_locn__lval_in_use(Lval) ->
		{ error("var_locn__acquire_reg_require_given: lval in use") }
	;
		[]
	),
	var_locn__get_acquired(Acquired0),
	{ set__insert(Acquired0, Lval, Acquired) },
	var_locn__set_acquired(Acquired).

var_locn__acquire_reg_prefer_given(Pref, Lval) -->
	{ PrefLval = reg(r, Pref) },
	( var_locn__lval_in_use(PrefLval) ->
		var_locn__get_spare_reg(Lval)
	;
		{ Lval = PrefLval }
	),
	var_locn__get_acquired(Acquired0),
	{ set__insert(Acquired0, Lval, Acquired) },
	var_locn__set_acquired(Acquired).

var_locn__acquire_reg_start_at_given(Start, Lval) -->
	{ StartLval = reg(r, Start) },
	( var_locn__lval_in_use(StartLval) ->
		var_locn__acquire_reg_start_at_given(Start + 1, Lval)
	;
		{ Lval = StartLval },
		var_locn__get_acquired(Acquired0),
		{ set__insert(Acquired0, Lval, Acquired) },
		var_locn__set_acquired(Acquired)
	).

var_locn__release_reg(Lval) -->
	var_locn__get_acquired(Acquired0),
	( { set__member(Lval, Acquired0) } ->
		{ set__delete(Acquired0, Lval, Acquired) },
		var_locn__set_acquired(Acquired)
	;
		{ error("var_locn__release_reg: unacquired reg") }
	).

%----------------------------------------------------------------------------%

var_locn__lock_regs(N, Exceptions) -->
	var_locn__set_locked(N),
	var_locn__set_exceptions(Exceptions).

var_locn__unlock_regs -->
	var_locn__set_locked(0),
	var_locn__set_exceptions([]).

%----------------------------------------------------------------------------%

var_locn__max_reg_in_use(VarLocnInfo, Max) :-
	var_locn__get_loc_var_map(LocVarMap, VarLocnInfo, _),
	map__keys(LocVarMap, VarLocs),
	code_util__max_mentioned_reg(VarLocs, Max1),
	var_locn__get_acquired(Acquired, VarLocnInfo, _),
	set__to_sorted_list(Acquired, AcquiredList),
	code_util__max_mentioned_reg(AcquiredList, Max2),
	int__max(Max1, Max2, Max).

%----------------------------------------------------------------------------%

% var_locn__expr_is_constant(Rval0, VarStateMap, ExprnOpts, Rval)
% Check if Rval0 is a constant rval, after substituting the values of the
% variables inside it. Returns the substituted, ground rval in Rval.
% Note that this predicate is similar to code_exprn__expr_is_constant,
% but of courses its own version of the variable state data structure.

:- pred var_locn__expr_is_constant(rval::in, var_state_map::in, exprn_opts::in,
	rval::out) is semidet.

var_locn__expr_is_constant(const(Const), _, ExprnOpts, const(Const)) :-
	exprn_aux__const_is_constant(Const, ExprnOpts, yes).

var_locn__expr_is_constant(unop(Op, Expr0), VarStateMap, ExprnOpts,
		unop(Op, Expr)) :-
	var_locn__expr_is_constant(Expr0, VarStateMap, ExprnOpts, Expr).

var_locn__expr_is_constant(binop(Op, Expr1, Expr2), VarStateMap, ExprnOpts,
		binop(Op, Expr3, Expr4)) :-
	var_locn__expr_is_constant(Expr1, VarStateMap, ExprnOpts, Expr3),
	var_locn__expr_is_constant(Expr2, VarStateMap, ExprnOpts, Expr4).

var_locn__expr_is_constant(mkword(Tag, Expr0), VarStateMap, ExprnOpts,
		mkword(Tag, Expr)) :-
	var_locn__expr_is_constant(Expr0, VarStateMap, ExprnOpts, Expr).

var_locn__expr_is_constant(create(Tag, Args0, ArgTypes, StatDyn,
		Label, Msg, Reuse),
		VarStateMap, ExprnOpts, NewRval) :-
	Reuse = no,
	( StatDyn = must_be_static ->
		NewRval = create(Tag, Args0, ArgTypes, StatDyn,
			Label, Msg, Reuse)
	;
		ExprnOpts = nlg_asm_sgt_ubf(_, _, StaticGroundTerms, _),
		StaticGroundTerms = yes,
		var_locn__args_are_constant(Args0, VarStateMap, ExprnOpts,
			Args),
		NewRval = create(Tag, Args, ArgTypes, StatDyn,
			Label, Msg, Reuse)
	).

var_locn__expr_is_constant(var(Var), VarStateMap, ExprnOpts, Rval) :-
	map__search(VarStateMap, Var, State),
	State = state(_, yes(Rval), _, _, _),
	require(var_locn__expr_is_constant(Rval, VarStateMap, ExprnOpts, _),
		"non-constant rval in variable state").

:- pred var_locn__args_are_constant(list(maybe(rval))::in, var_state_map::in,
	exprn_opts::in, list(maybe(rval))::out) is semidet.

var_locn__args_are_constant([], _VarStateMap, _ExprnOpts, []).
var_locn__args_are_constant([Arg0 | Args0], VarStateMap, ExprnOpts,
		[Arg | Args]) :-
	% if any of the fields are 'no' then we cannot treat the
	% term as a constant.
	Arg0 = yes(Rval0),
	var_locn__expr_is_constant(Rval0, VarStateMap, ExprnOpts, Rval),
	Arg = yes(Rval),
	var_locn__args_are_constant(Args0, VarStateMap, ExprnOpts, Args).

%----------------------------------------------------------------------------%

% Lval is Lval0 with all variables in Lval0 replaced by their values.

var_locn__materialize_vars_in_lval(Lval0, Lval, Code) -->
	var_locn__materialize_vars_in_lval(Lval0, [], Lval, Code).

:- pred var_locn__materialize_vars_in_lval(lval::in, list(lval)::in,
	lval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__materialize_vars_in_lval(Lval0, Avoid, Lval, Code) -->
	(
		{ Lval0 = reg(_, _) },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = stackvar(_) },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = framevar(_) },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = succip },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = maxfr },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = curfr },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = hp },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = sp },
		{ Lval = Lval0 },
		{ Code = empty }
	;
		{ Lval0 = succip(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = succip(Rval) }
	;
		{ Lval0 = redoip(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = redoip(Rval) }
	;
		{ Lval0 = succfr(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = succfr(Rval) }
	;
		{ Lval0 = redofr(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = redofr(Rval) }
	;
		{ Lval0 = prevfr(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = prevfr(Rval) }
	;
		{ Lval0 = mem_ref(Rval0) },
		var_locn__materialize_vars_in_rval(Rval0, no, Avoid,
			Rval, Code),
		{ Lval = mem_ref(Rval) }
	;
		{ Lval0 = field(Tag, RvalA0, RvalB0) },
		var_locn__materialize_vars_in_rval(RvalA0, no, Avoid,
			RvalA, CodeA),
		var_locn__materialize_vars_in_rval(RvalB0, no, Avoid,
			RvalB, CodeB),
		{ Lval = field(Tag, RvalA, RvalB) },
		{ Code = tree(CodeA, CodeB) }
	;
		{ Lval0 = temp(_, _) },
		{ error("var_locn__materialize_vars_in_lval: temp") }
	;
		{ Lval0 = lvar(_) },
		{ error("var_locn__materialize_vars_in_lval: lvar") }
	).

% Rval is Rval0 with all variables in Rval0 replaced by their values.

:- pred var_locn__materialize_vars_in_rval(rval::in, maybe(lval)::in,
	list(lval)::in, rval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__materialize_vars_in_rval(Rval0, MaybePrefer, Avoid, Rval, Code) -->
	(
		{ Rval0 = lval(Lval0) },
		var_locn__materialize_vars_in_lval(Lval0, Avoid, Lval, Code),
		{ Rval = lval(Lval) }
	;
		{ Rval0 = mkword(Tag, SubRval0) },
		var_locn__materialize_vars_in_rval(SubRval0, no, Avoid,
			SubRval, Code),
		{ Rval = mkword(Tag, SubRval) }
	;
		{ Rval0 = unop(Unop, SubRval0) },
		var_locn__materialize_vars_in_rval(SubRval0, no, Avoid,
			SubRval, Code),
		{ Rval = unop(Unop, SubRval) }
	;
		{ Rval0 = binop(Binop, SubRvalA0, SubRvalB0) },
		var_locn__materialize_vars_in_rval(SubRvalA0, no, Avoid,
			SubRvalA, CodeA),
		var_locn__materialize_vars_in_rval(SubRvalB0, no, Avoid,
			SubRvalB, CodeB),
		{ Rval = binop(Binop, SubRvalA, SubRvalB) },
		{ Code = tree(CodeA, CodeB) }
	;
		{ Rval0 = const(_) },
		{ Rval = Rval0 },
		{ Code = empty }
	;
		{ Rval0 = mem_addr(_) },
		{ Rval = Rval0 },
		{ Code = empty }
	;
			% If we get here, the cell must be a constant.
		{ Rval0 = create(_, _, _, _, _, _, _) },
		{ Rval = Rval0 },
		{ Code = empty }
	;
		{ Rval0 = var(Var) },
		var_locn__find_var_availability(Var, MaybePrefer, Avail),
		(
			{ Avail = available(Rval) },
			{ Code = empty }
		;
			{ Avail = needs_materialization },
			var_locn__materialize_var(Var, MaybePrefer, yes,
				Avoid, Rval, Code)
		)
	).

:- type var_avail
	--->	available(rval)
	;	needs_materialization.

:- pred var_locn__find_var_availability(prog_var::in, maybe(lval)::in,
	var_avail::out, var_locn_info::in, var_locn_info::out) is det.

var_locn__find_var_availability(Var, MaybePrefer, Avail) -->
	var_locn__get_var_state_map(VarStateMap),
	{ map__lookup(VarStateMap, Var, State) },
	{ State = state(Lvals, MaybeConstRval, _, _, _) },
	{ set__to_sorted_list(Lvals, LvalsList) },
	(
		{ MaybePrefer = yes(Prefer) },
		{ list__member(Prefer, LvalsList) }
	->
		{ Rval = lval(Prefer) },
		{ Avail = available(Rval) }
	;
		{ var_locn__maybe_select_lval_or_rval(LvalsList,
			MaybeConstRval, Rval) }
	->
		{ Avail = available(Rval) }
	;
		{ Avail = needs_materialization }
	).

:- pred var_locn__materialize_var(prog_var::in, maybe(lval)::in, bool::in,
	list(lval)::in, rval::out, code_tree::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__materialize_var(Var, MaybePrefer, StoreIfReq, Avoid, Rval, Code) -->
	var_locn__get_var_state_map(VarStateMap),
	{ map__lookup(VarStateMap, Var, State) },
	{ State = state(_Lvals, _MaybeConstRval, MaybeExprRval,
		UsingVars, _DeadOrAlive) },
	{
		MaybeExprRval = yes(ExprRval)
	;
		MaybeExprRval = no,
		error("var_locn__materialize_var: no expr")
	},
	var_locn__materialize_vars_in_rval(ExprRval, MaybePrefer, Avoid,
		Rval0, ExprCode),
	(
		{ StoreIfReq = yes },
		{ set__count(UsingVars, NumUsingVars) },
		{ NumUsingVars > 1 }
	->
		var_locn__select_preferred_reg_avoid(Var, Avoid, Lval),
		var_locn__place_var(Var, Lval, PlaceCode),
		{ Rval = lval(Lval) },
		{ Code = tree(ExprCode, PlaceCode) }
	;
		{ Rval = Rval0 },
		{ Code = ExprCode }
	).

%----------------------------------------------------------------------------%

% Update LocVarMap0 to reflect the dependence of Var on all the root lvals
% among Lvals or contained inside Lvals.

:- pred var_locn__make_var_depend_on_lvals_roots(prog_var::in,
	set(lval)::in, loc_var_map::in, loc_var_map::out) is det.

var_locn__make_var_depend_on_lvals_roots(Var, Lvals,
		LocVarMap0, LocVarMap) :-
	var_locn__get_var_set_roots(Lvals, NoDupRootLvals),
	list__foldl(var_locn__make_var_depend_on_root_lval(Var),
		NoDupRootLvals, LocVarMap0, LocVarMap).

:- pred var_locn__make_var_depend_on_lval_roots(prog_var::in,
	lval::in, loc_var_map::in, loc_var_map::out) is det.

var_locn__make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap) :-
	set__singleton_set(Lvals, Lval),
	var_locn__make_var_depend_on_lvals_roots(Var, Lvals,
		LocVarMap0, LocVarMap).

:- pred var_locn__make_var_depend_on_root_lval(prog_var::in, lval::in,
	loc_var_map::in, loc_var_map::out) is det.

var_locn__make_var_depend_on_root_lval(Var, Lval, LocVarMap0, LocVarMap) :-
	require(var_locn__is_root_lval(Lval),
		"var_locn__make_var_depend_on_root_lval: non-root lval"),
	( map__search(LocVarMap0, Lval, Vars0) ->
		set__insert(Vars0, Var, Vars),
		map__det_update(LocVarMap0, Lval, Vars, LocVarMap)
	;
		set__singleton_set(Vars, Var),
		map__det_insert(LocVarMap0, Lval, Vars, LocVarMap)
	).

% Update LocVarMap0 to reflect that Var is no longer dependent on the root lval
% Lval.

:- pred var_locn__make_var_not_depend_on_root_lval(prog_var::in, lval::in,
	loc_var_map::in, loc_var_map::out) is det.

var_locn__make_var_not_depend_on_root_lval(Var, Lval, LocVarMap0, LocVarMap) :-
	require(var_locn__is_root_lval(Lval),
		"var_locn__make_var_depend_on_root_lval: non-root lval"),
	( map__search(LocVarMap0, Lval, Vars0) ->
		set__delete(Vars0, Var, Vars),
		( set__empty(Vars) ->
			map__det_remove(LocVarMap0, Lval, _, LocVarMap)
		;
			map__det_update(LocVarMap0, Lval, Vars, LocVarMap)
		)
	;
		error("var_locn__make_var_not_depend_on_root_lval: no record")
	).

:- pred var_locn__is_root_lval(lval::in) is semidet.

var_locn__is_root_lval(reg(r, _)).
var_locn__is_root_lval(stackvar(_)).
var_locn__is_root_lval(framevar(_)).

%----------------------------------------------------------------------------%

:- type dep_search_lval
	--->	all_regs
	;	specific_reg_or_stack(lval).

:- pred var_locn__lval_does_not_support_lval(lval::in, lval::in) is semidet.

var_locn__lval_does_not_support_lval(Lval1, Lval2) :-
	\+ var_locn__lval_depends_on_search_lval(Lval2,
		specific_reg_or_stack(Lval1)).

:- pred var_locn__rval_depends_on_search_lval(rval::in, dep_search_lval::in)
	is semidet.

var_locn__rval_depends_on_search_lval(lval(Lval), SearchLval) :-
	var_locn__lval_depends_on_search_lval(Lval, SearchLval).
var_locn__rval_depends_on_search_lval(var(_Var), _SearchLval) :-
	error("var_locn__rval_depends_on_search_lval: var").
var_locn__rval_depends_on_search_lval(create(_, Rvals, _, _, _, _, Reuse),
		SearchLval) :-
	var_locn__args_depend_on_search_lval([Reuse | Rvals], SearchLval).
var_locn__rval_depends_on_search_lval(mkword(_Tag, Rval), SearchLval) :-
	var_locn__rval_depends_on_search_lval(Rval, SearchLval).
var_locn__rval_depends_on_search_lval(const(_Const), _SearchLval) :-
	fail.
var_locn__rval_depends_on_search_lval(unop(_Op, Rval), SearchLval) :-
	var_locn__rval_depends_on_search_lval(Rval, SearchLval).
var_locn__rval_depends_on_search_lval(binop(_Op, Rval0, Rval1), SearchLval) :-
	(
		var_locn__rval_depends_on_search_lval(Rval0, SearchLval)
	;
		var_locn__rval_depends_on_search_lval(Rval1, SearchLval)
	).

:- pred var_locn__lval_depends_on_search_lval(lval::in, dep_search_lval::in)
	is semidet.

var_locn__lval_depends_on_search_lval(reg(Type, Num), SearchLval) :-
	(
		SearchLval = all_regs
	;
		SearchLval = specific_reg_or_stack(Lval),
		Lval = reg(Type, Num)
	).
var_locn__lval_depends_on_search_lval(stackvar(Num), SearchLval) :-
	SearchLval = specific_reg_or_stack(Lval),
	Lval = stackvar(Num).
var_locn__lval_depends_on_search_lval(framevar(Num), SearchLval) :-
	SearchLval = specific_reg_or_stack(Lval),
	Lval = framevar(Num).
var_locn__lval_depends_on_search_lval(lvar(_Var), _SearchLval) :-
	error("var_locn__lval_depends_on_search_lval: lvar").
var_locn__lval_depends_on_search_lval(field(_Tag, Rval0, Rval1), SearchLval) :-
	(
		var_locn__rval_depends_on_search_lval(Rval0, SearchLval)
	;
		var_locn__rval_depends_on_search_lval(Rval1, SearchLval)
	).

:- pred var_locn__args_depend_on_search_lval(list(maybe(rval))::in,
	dep_search_lval::in) is semidet.

var_locn__args_depend_on_search_lval([], _SearchLval) :-
	fail.
var_locn__args_depend_on_search_lval([Arg | Args], SearchLval) :-
	(
		Arg = yes(Rval),
		var_locn__rval_depends_on_search_lval(Rval, SearchLval)
	;
		var_locn__args_depend_on_search_lval(Args, SearchLval)
	).

%----------------------------------------------------------------------------%

var_locn__set_follow_vars(follow_vars(FollowVarMap, NextNonReserved)) -->
	var_locn__set_follow_var_map(FollowVarMap),
	var_locn__set_next_non_reserved(NextNonReserved).

%----------------------------------------------------------------------------%

:- pred var_locn__get_var_name(prog_var::in, string::out,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__get_var_name(Var, Name) -->
	var_locn__get_varset(Varset),
	{ varset__lookup_name(Varset, Var, Name) }.

%----------------------------------------------------------------------------%

:- pred var_locn__nonempty_state(var_state::in) is semidet.

var_locn__nonempty_state(State) :-
	State = state(LvalSet, MaybeConstRval, MaybeExprRval, _, _),
	( set__non_empty(LvalSet)
	; MaybeConstRval = yes(_)
	; MaybeExprRval = yes(_)
	).

%----------------------------------------------------------------------------%

:- pred var_locn__get_varset(prog_varset::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_varset(prog_varset::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_exprn_opts(exprn_opts::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_follow_var_map(follow_vars_map::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_next_non_reserved(int::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_var_state_map(var_state_map::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_var_state_map(var_state_map::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_loc_var_map(loc_var_map::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_loc_var_map(loc_var_map::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_acquired(set(lval)::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_acquired(set(lval)::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_locked(int::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_locked(int::in,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__get_exceptions(assoc_list(prog_var, lval)::out,
	var_locn_info::in, var_locn_info::out) is det.

:- pred var_locn__set_exceptions(assoc_list(prog_var, lval)::in,
	var_locn_info::in, var_locn_info::out) is det.

var_locn__get_varset(VI ^ varset, VI, VI).
var_locn__get_stack_slots(VI ^ stack_slots, VI, VI).
var_locn__get_exprn_opts(VI ^ exprn_opts, VI, VI).
var_locn__get_follow_var_map(VI ^ follow_vars_map, VI, VI).
var_locn__get_next_non_reserved(VI ^ next_non_res, VI, VI).
var_locn__get_var_state_map(VI ^ var_state_map, VI, VI).
var_locn__get_loc_var_map(VI ^ loc_var_map, VI, VI).
var_locn__get_acquired(VI ^ acquired, VI, VI).
var_locn__get_locked(VI ^ locked, VI, VI).
var_locn__get_exceptions(VI ^ exceptions, VI, VI).

var_locn__set_varset(VS, VI, VI ^ varset := VS).
var_locn__set_follow_var_map(FVM, VI, VI ^ follow_vars_map := FVM).
var_locn__set_next_non_reserved(NNR, VI, VI ^ next_non_res := NNR).
var_locn__set_var_state_map(VSM, VI, VI ^ var_state_map := VSM).
var_locn__set_loc_var_map(LVM, VI, VI ^ loc_var_map := LVM).
var_locn__set_acquired(A, VI, VI ^ acquired := A).
var_locn__set_locked(L, VI, VI ^ locked := L).
var_locn__set_exceptions(E, VI, VI ^ exceptions := E).

%----------------------------------------------------------------------------%
