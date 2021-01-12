%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: var_locn.m.
% Author: zs.
%
% This module defines a set of predicates that operate on the abstract
% 'var_locn_info' structure which maintains information about where variables
% are stored, what their values are if they are not stored anywhere,
% and which registers are reserved for purposes such as holding the arguments
% of calls and tags that are to be switched upon.
%
%---------------------------------------------------------------------------%

:- module ll_backend.var_locn.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type var_locn_info.

    % init_var_locn_state(ModuleInfo, Arguments, Liveness, VarSet, VarTypes,
    %   FloatRegType, StackSlots, FollowVars, VarLocnInfo):
    %
    % Produces an initial state of the VarLocnInfo given
    % an association list of variables and lvalues. The initial
    % state places the given variables at their corresponding
    % locations, with the exception of variables which are not in
    % Liveness (this corresponds to input arguments that are not
    % used in the body). The VarSet parameter contains a mapping from
    % variables to names, which is used when code is generated
    % to provide meaningful comments. VarTypes gives the types of
    % of all the procedure's variables. FloatRegType gives the preferred
    % register type for floats. StackSlots maps each variable
    % to its stack slot, if it has one. FollowVars is the initial
    % follow_vars set; such sets give guidance as to what lvals
    % (if any) each variable will be needed in next.
    %
:- pred init_var_locn_state(module_info::in, assoc_list(prog_var, lval)::in,
    set_of_progvar::in, prog_varset::in, vartypes::in, reg_type::in,
    stack_slots::in, abs_follow_vars::in, var_locn_info::out) is det.

    % reinit_var_locn_state(VarLocs, !VarLocnInfo):
    %
    % Produces a new state of the VarLocnInfo in which the static
    % and mostly static information (stack slot map, follow vars map,
    % varset, option settings) comes from VarLocnInfo0 but the
    % dynamic state regarding variable locations is thrown away
    % and then rebuilt from the information in VarLocs, an
    % association list of variables and lvals. The new state
    % places the given variables at their corresponding locations.
    %
:- pred reinit_var_locn_state(assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_clobber_all_regs(OkToDeleteAny, !VarLocnInfo):
    %
    % Modifies VarLocnInfo to show that all variables stored in registers
    % have been clobbered. Aborts if this deletes the last record of the
    % state of a variable unless OkToDeleteAny is `yes'.
    %
:- pred var_locn_clobber_all_regs(bool::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_clobber_reg(Reg, !VarLocnInfo):
    %
    % Modifies VarLocnInfo to show that all variables stored in Reg
    % (an lval which should be a register) are clobbered.
    %
:- pred var_locn_clobber_reg(lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_clobber_regs(Regs, !VarLocnInfo):
    %
    % Modifies VarLocnInfo to show that all variables stored in Regs
    % (a list of lvals which should contain only registers) are clobbered.
    %
:- pred var_locn_clobber_regs(list(lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_set_magic_var_location(Var, Lval, !VarLocnInfo):
    %
    % Updates !VarLocnInfo to show that Var is *magically* stored in Lval.
    % Does not care if Lval is already in use; it overwrites it with the
    % new information. Var must not have been previously known. Used to
    % implement the ends of erroneous branches.
    %
:- pred var_locn_set_magic_var_location(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_check_and_set_magic_var_location(Var, Lval, !VarLocnInfo):
    %
    % Updates VarLocnInfo to show that Var has been *magically* stored in Lval.
    % (The caller usually generates code to perform this magic.) Aborts if Lval
    % is already in use, or if Var was previously known.
    %
:- pred var_locn_check_and_set_magic_var_location(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_lval_in_use(VarLocnInfo, Lval):
    %
    % Succeeds iff Lval, which should be a register or stack slot,
    % holds (a path to) a variable or is otherwise reserved.
    %
:- pred var_locn_lval_in_use(var_locn_info::in, lval::in) is semidet.

    % var_locn_var_becomes_dead(Var, FirstTime, !VarLocnInfo):
    %
    % Frees any code generator resources used by Var in !VarLocnInfo.
    % FirstTime should be no if this same operation may already have been
    % executed on Var; otherwise, var_becomes_dead will throw an exception
    % if it does not know about Var.
    %
:- pred var_locn_var_becomes_dead(prog_var::in, bool::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_var_to_var(Var, AssignedVar, !VarLocnInfo):
    %
    % Reflects the effect of the assignment Var := AssignedVar in the
    % state of !VarLocnInfo.
    %
:- pred var_locn_assign_var_to_var(prog_var::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_lval_to_var(Var, Lval, StaticCellInfo, Code,
    %   !VarLocnInfo);
    %
    % Reflects the effect of the assignment Var := lval(Lval) in the
    % state of !VarLocnInfo; any code required to effect the assignment
    % will be returned in Code.
    %
:- pred var_locn_assign_lval_to_var(prog_var::in, lval::in,
    static_cell_info::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_field_lval_expr_to_var(ModuleInfo, Var, BaseVar, Expr,
    %   StaticCellInfo, Code, !VarLocnInfo);
    %
    % Reflects the effect of the assignment Var := Expr,
    % where Expr contains only field lvals with the base BaseVar.
    % Any code required to effect the assignment will be returned in Code.
    %
:- pred var_locn_assign_field_lval_expr_to_var(prog_var::in,
    prog_var::in, rval::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_const_to_var(ExprnOpts, Var, ConstRval,
    %   !VarLocnInfo):
    %
    % Reflects the effect of the assignment Var := const(ConstRval)
    % in the state of !VarLocnInfo.
    %
:- pred var_locn_assign_const_to_var(exprn_opts::in, prog_var::in, rval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_expr_to_var(Var, Rval, Code, !VarLocnInfo):
    %
    % Generates code to execute the assignment Var := Expr, and
    % updates the state of !VarLocnInfo accordingly.
    %
    % Expr must contain no lvals, although it may (and typically will) refer
    % to the values of other variables through rvals of the form var(_).
    %
:- pred var_locn_assign_expr_to_var(prog_var::in, rval::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_reassign_mkword_hole_var(Var, Ptag, Rval, Code, !VarLocnInfo):
    %
    % Generates code to execute the assignment Var := mkword(Ptag, Rval), and
    % updates the state of !VarLocnInfo accordingly. Var must previously have
    % been assigned the constant expression mkword_hole(Ptag).
    %
:- pred var_locn_reassign_mkword_hole_var(prog_var::in, ptag::in, rval::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

    % var_locn_reassign_tagword_var(Var, ToOrMask, ToOrRval, Code,
    %   !VarLocnInfo):
    %
    % Generates code to assign (Var & \ToOrMask) | ToOrRval to Var.
    % Obviously, Var must have previously been assigned a value.
    % ToOrRval may (and typically will) contain references to other variables
    % in var(_) form; it should *not* contain references directly to lvals.
    %
:- pred var_locn_reassign_tagword_var(prog_var::in, uint::in, rval::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

    % var_locn_assign_cell_to_var(ExprnOpts, Var, ReserveWordAtStart, Ptag,
    %   MaybeRvals, MaybeSize, FieldAddrs, TypeMsg,
    %   MayUseAtomic, Label, Code, !StaticCellInfo, !VarLocnInfo):
    %
    % Generates code to assign to Var a pointer, tagged by Ptag, to the cell
    % whose contents are given by the other arguments, and updates the state
    % of !VarLocnInfo accordingly. If ReserveWordAtStart is yes, and the cell
    % is allocated on the heap (rather than statically), then reserve an extra
    % word immediately before the allocated object, for the garbage collector
    % to use to hold a forwarding pointer. If MaybeSize is yes(SizeVal), then
    % reserve an extra word immediately before the allocated object (regardless
    % of whether it is allocated statically or dynamically), and initialize
    % this word with the value determined by SizeVal. (NOTE: ReserveWordAtStart
    % and MaybeSize should not be yes / yes(_), because that will cause an
    % obvious conflict.) Label can be used in the generated code if necessary.
    %
:- pred var_locn_assign_cell_to_var(exprn_opts::in, prog_var::in, bool::in,
    ptag::in, list(cell_arg)::in, how_to_construct::in,
    maybe(term_size_value)::in, maybe(alloc_site_id)::in,
    may_use_atomic_alloc::in, label::in,
    llds_code::out, static_cell_info::in, static_cell_info::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_save_cell_fields(Var, VarLval, Code, TempRegs, !VarLocnInfo)
    %
    % Save any variables which depend on the ReuseLval into temporary
    % registers so that they are available after ReuseLval is clobbered.
    %
:- pred var_locn_save_cell_fields(prog_var::in, lval::in,
    llds_code::out, list(lval)::out, var_locn_info::in, var_locn_info::out)
    is det.

    % var_locn_place_var(Var, Lval, Code, !VarLocnInfo):
    %
    % Produces Code to place the value of Var in Lval, and update !VarLocnInfo
    % to reflect this.
    %
:- pred var_locn_place_var(prog_var::in, lval::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_place_vars(VarLocns, Code, !VarLocnInfo):
    %
    % Produces Code to place the value of each variable mentioned in VarLocns
    % into the corresponding location, and update !VarLocnInfo to reflect this.
    %
:- pred var_locn_place_vars(assoc_list(prog_var, lval)::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_produce_var(Var, Rval, Code, !VarLocnInfo):
    %
    % Return the preferred way to refer to the value of Var
    % (which may be a const rval, or the value in an lval).
    %
    % If Var is currently a cached expression, then produce_var will generate
    % Code to evaluate the expression and put it into an lval. (Since the code
    % generator can ask for a variable to be produced more than once, this is
    % necessary to prevent the expression, which may involve a possibly large
    % number of operations, from being evaluated several times.) Otherwise,
    % Code will be empty.
    %
:- pred var_locn_produce_var(prog_var::in, rval::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_produce_var_in_reg(Var, Lval, Code, !VarLocnInfo):
    %
    % Produces a code fragment Code to evaluate Var if necessary
    % and provide it as an Lval of the form reg(_).
    %
:- pred var_locn_produce_var_in_reg(prog_var::in, lval::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_produce_var_in_reg_or_stack(Var, FollowVars, Lval, Code,
    %   !VarLocnInfo):
    %
    % Produces a code fragment Code to evaluate Var if necessary and provide it
    % as an Lval of the form reg(_), stackvar(_), or framevar(_).
    %
:- pred var_locn_produce_var_in_reg_or_stack(prog_var::in,
    lval::out, llds_code::out, var_locn_info::in, var_locn_info::out) is det.

    % var_locn_acquire_reg(Lval, !VarLocnInfo):
    %
    % Finds an unused register and marks it as 'in use'.
    %
:- pred var_locn_acquire_reg(reg_type::in, lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_acquire_reg_require_given(Reg, Lval, !VarLocInfo):
    %
    % Marks Reg, which must be an unused register, as 'in use'.
    %
:- pred var_locn_acquire_reg_require_given(lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_acquire_reg_prefer_given(Type, Pref, Lval, !VarLocInfo):
    %
    % Finds an unused register, and marks it as 'in use'.
    % If Pref itself is free, assigns that.
    %
:- pred var_locn_acquire_reg_prefer_given(reg_type::in, int::in, lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_acquire_reg_start_at_given(Start, Lval, !VarLocInfo):
    %
    % Finds an unused register, and marks it as 'in use'.
    % It starts the search at the one numbered Start,
    % continuing towards higher register numbers.
    %
:- pred var_locn_acquire_reg_start_at_given(reg_type::in, int::in, lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_release_reg(Lval, !VarLocnInfo):
    %
    % Marks a previously acquired reg as no longer 'in use'.
    %
:- pred var_locn_release_reg(lval::in, var_locn_info::in, var_locn_info::out)
    is det.

    % var_locn_lock_regs(R, F, Exceptions, !VarLocnInfo):
    %
    % Prevents registers r1 through rR and registers f1 through fF from being
    % reused, even if there are no variables referring to them, with the
    % exceptions of the registers named in Exceptions, which however can only
    % be used to store their corresponding variables. Should be followed by a
    % call to unlock_regs.
    %
:- pred var_locn_lock_regs(int::in, int::in, assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_unlock_regs(!VarLocnInfo):
    %
    % Undoes a lock operation.
    %
:- pred var_locn_unlock_regs(var_locn_info::in, var_locn_info::out) is det.

    % var_locn_clear_r1(ModuleInfo, Code, !VarLocnInfo):
    %
    % Produces a code fragment Code to move whatever is in r1 to some other
    % register, if r1 is live. This is used prior to semidet pragma
    % foreign_procs.
    %
:- pred var_locn_clear_r1(llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_materialize_vars_in_lval(Lval, FinalLval, Code, !VarLocnInfo):
    %
    % For every variable in Lval, substitutes the value of the variable and
    % returns it as FinalLval. If we need to save the values of some of the
    % substituted variables somewhere so as to prevent them from being
    % evaluated again (and again ...), the required code will be returned
    % in Code.
    %
:- pred var_locn_materialize_vars_in_lval(lval::in, lval::out,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

    % The equivalent of var_locn_materialize_vars_in_lval for rvals.
    %
:- pred var_locn_materialize_vars_in_rval(rval::in, rval::out,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

    % var_locn_get_var_locations(VarLocnInfo, Locations):
    %
    % Returns a map from each live variable that occurs in VarLocnInfo
    % to the set of locations in which it may be found (which may be empty,
    % if the variable's value is either a known constant, or an as-yet
    % unevaluated expression).
    %
:- pred var_locn_get_var_locations(var_locn_info::in,
    map(prog_var, set(lval))::out) is det.

    % var_locn_get_stack_slots(VarLocnInfo, StackSlots):
    %
    % Returns the table mapping each variable to its stack slot (if any).
    %
:- pred var_locn_get_stack_slots(var_locn_info::in, stack_slots::out) is det.

    % var_locn_get_follow_var_map(VarLocnInfo, FollowVars):
    %
    % Returns the table mapping each variable to the lval (if any)
    % where it is desired next.
    %
:- pred var_locn_get_follow_var_map(var_locn_info::in,
    abs_follow_vars_map::out) is det.

    % var_locn_get_next_non_reserved(VarLocnInfo, NonRes):
    %
    % Returns the number of the first register which is free for general use.
    % It does not reserve the register.
    %
:- pred var_locn_get_next_non_reserved(var_locn_info::in, reg_type::in,
    int::out) is det.

    % var_locn_set_follow_vars(FollowVars):
    %
    % Sets the table mapping each variable to the lval (if any) where it is
    % desired next, and the number of the first non-reserved register.
    %
:- pred var_locn_set_follow_vars(abs_follow_vars::in,
    var_locn_info::in, var_locn_info::out) is det.

    % var_locn_max_reg_in_use(MaxRegR, MaxRegF):
    %
    % Returns the number of the highest numbered rN and fN registers in use.
    %
:- pred var_locn_max_reg_in_use(var_locn_info::in, int::out, int::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.
:- import_module parse_tree.builtin_lib_types.

:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

var_locn_get_var_locations(VLI, VarLocations) :-
    var_locn_get_var_state_map(VLI, VarStateMap),
    map.to_assoc_list(VarStateMap, VarLocList),
    list.filter_map(convert_live_to_lval_set, VarLocList, LiveVarLocList),
    map.from_sorted_assoc_list(LiveVarLocList, VarLocations).

:- pred convert_live_to_lval_set(pair(prog_var, var_state)::in,
    pair(prog_var, set(lval))::out) is semidet.

convert_live_to_lval_set(Var - State, Var - Lvals) :-
    State = var_state(Lvals, _, _, _, doa_alive).

%---------------------------------------------------------------------------%

var_locn_clobber_all_regs(OkToDeleteAny, !VLI) :-
    var_locn_set_acquired(set.init, !VLI),
    var_locn_set_locked(0, 0, !VLI),
    var_locn_set_exceptions([], !VLI),
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.keys(LocVarMap0, Locs),
    clobber_regs_in_maps(Locs, OkToDeleteAny,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),
    var_locn_set_var_state_map(VarStateMap, !VLI).

var_locn_clobber_reg(Reg, !VLI) :-
    var_locn_get_acquired(!.VLI, Acquired0),
    Acquired = set.delete(Acquired0, Reg),
    var_locn_set_acquired(Acquired, !VLI),
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    clobber_reg_in_maps(Reg, no,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),
    var_locn_set_var_state_map(VarStateMap, !VLI).

var_locn_clobber_regs(Regs, !VLI) :-
    var_locn_get_acquired(!.VLI, Acquired0),
    Acquired = set.delete_list(Acquired0, Regs),
    var_locn_set_acquired(Acquired, !VLI),
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    clobber_regs_in_maps(Regs, no,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),
    var_locn_set_var_state_map(VarStateMap, !VLI).

:- pred clobber_regs_in_maps(list(lval)::in, bool::in,
    loc_var_map::in, loc_var_map::out,
    var_state_map::in, var_state_map::out) is det.

clobber_regs_in_maps([], _, !LocVarMap, !VarStateMap).
clobber_regs_in_maps([Lval | Lvals], OkToDeleteAny,
        !LocVarMap, !VarStateMap) :-
    clobber_reg_in_maps(Lval, OkToDeleteAny, !LocVarMap, !VarStateMap),
    clobber_regs_in_maps(Lvals, OkToDeleteAny, !LocVarMap, !VarStateMap).

:- pred clobber_reg_in_maps(lval::in, bool::in,
    loc_var_map::in, loc_var_map::out,
    var_state_map::in, var_state_map::out) is det.

clobber_reg_in_maps(Lval, OkToDeleteAny, !LocVarMap, !VarStateMap) :-
    ( if
        Lval = reg(_, _),
        map.search(!.LocVarMap, Lval, DependentVarsSet)
    then
        map.delete(Lval, !LocVarMap),
        set_of_var.fold(
            clobber_lval_in_var_state_map(Lval, [], OkToDeleteAny),
            DependentVarsSet, !VarStateMap)
    else
        true
    ).

:- pred clobber_lval_in_var_state_map(lval::in, list(prog_var)::in,
    bool::in, prog_var::in, var_state_map::in, var_state_map::out) is det.

clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny, Var,
        !VarStateMap) :-
    ( if
        try_clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny,
            Var, !VarStateMap)
    then
        true
    else
        unexpected($pred, "empty state")
    ).

    % Try to record in VarStateMap that Var is no longer reachable through
    % (paths including) Lval. If this deletes the last possible place
    % where the value of Var can be found, and Var is not in OkToDeleteVars,
    % then fail.
    %
:- pred try_clobber_lval_in_var_state_map(lval::in, list(prog_var)::in,
    bool::in, prog_var::in, var_state_map::in, var_state_map::out) is semidet.

try_clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny, Var,
        !VarStateMap) :-
    map.lookup(!.VarStateMap, Var, State0),
    State0 = var_state(LvalSet0, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    LvalSet = set.filter(lval_does_not_support_lval(Lval), LvalSet0),
    State = var_state(LvalSet, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    (
        nonempty_state(State)
    ;
        list.member(Var, OkToDeleteVars)
    ;
        OkToDeleteAny = yes
    ;
        DeadOrAlive = doa_dead,
        set_of_var.to_sorted_list(Using, UsingVars),
        recursive_using_vars_dead_and_ok_to_delete(UsingVars,
            !.VarStateMap, OkToDeleteVars)
    ),
    map.det_update(Var, State, !VarStateMap).

:- pred recursive_using_vars_dead_and_ok_to_delete(
    list(prog_var)::in, var_state_map::in, list(prog_var)::in) is semidet.

recursive_using_vars_dead_and_ok_to_delete([], _, _).
recursive_using_vars_dead_and_ok_to_delete([Var | Vars], VarStateMap,
        OkToDeleteVars) :-
    (
        list.member(Var, OkToDeleteVars)
    ;
        map.lookup(VarStateMap, Var, State),
        State = var_state(_, _, _, Using, DeadOrAlive),
        DeadOrAlive = doa_dead,
        set_of_var.to_sorted_list(Using, UsingVars),
        recursive_using_vars_dead_and_ok_to_delete(UsingVars,
            VarStateMap, OkToDeleteVars)
    ),
    recursive_using_vars_dead_and_ok_to_delete(Vars,
        VarStateMap, OkToDeleteVars).

%---------------------------------------------------------------------------%

var_locn_assign_var_to_var(Var, OldVar, !VLI) :-
    check_var_is_unknown(!.VLI, Var),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    ( if map.search(VarStateMap0, OldVar, OldState0) then
        OldState0 = var_state(Lvals, MaybeConstRval, MaybeExprRval,
            Using0, DeadOrAlive),
        (
            MaybeExprRval = yes(_),
            State = var_state(Lvals, MaybeConstRval, yes(var(OldVar)),
                set_of_var.init, doa_alive),
            set_of_var.insert(Var, Using0, Using),
            OldState = var_state(Lvals, MaybeConstRval, MaybeExprRval,
                Using, DeadOrAlive),
            map.det_update(OldVar, OldState, VarStateMap0, VarStateMap1)
        ;
            MaybeExprRval = no,
            State = var_state(Lvals, MaybeConstRval, no, set_of_var.init,
                doa_alive),
            VarStateMap1 = VarStateMap0
        ),
        map.det_insert(Var, State, VarStateMap1, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI),

        var_locn_get_loc_var_map(!.VLI, LocVarMap0),
        make_var_depend_on_lvals_roots(Var, Lvals, LocVarMap0, LocVarMap),
        var_locn_set_loc_var_map(LocVarMap, !VLI)
    else
        var_locn_get_dummy_map(!.VLI, DummyMap),
        map.lookup(DummyMap, OldVar, OldVarIsDummy),
        expect(unify(OldVarIsDummy, is_dummy_type), $pred,
            "assigning value of nondummy variable without a state")
    ).

%---------------------------------------------------------------------------%

var_locn_assign_lval_to_var(Var, Lval0, StaticCellInfo, Code, !VLI) :-
    check_var_is_unknown(!.VLI, Var),
    ( if
        Lval0 = field(yes(Ptag), var(BaseVar), const(llconst_int(Offset)))
    then
        var_locn_get_var_state_map(!.VLI, VarStateMap0),
        map.lookup(VarStateMap0, BaseVar, BaseState),
        BaseState = var_state(BaseVarLvals, MaybeConstBaseVarRval,
            _MaybeExprRval, _UsingVars, _DeadOrAlive),
        ( if
            MaybeConstBaseVarRval = yes(BaseVarRval),
            BaseVarRval = mkword(Ptag, BaseConst),
            BaseConst = const(llconst_data_addr(DataAddr, MaybeBaseOffset)),
            % XXX We could drop the MaybeBaseOffset = no condition,
            % but this would require more complex code below.
            MaybeBaseOffset = no,
            search_scalar_static_cell_offset(StaticCellInfo, DataAddr, Offset,
                SelectedArgRval)
        then
            MaybeConstRval = yes(SelectedArgRval),
            Lvals = set.map(add_field_offset(yes(Ptag),
                const(llconst_int(Offset))), BaseVarLvals),
            State = var_state(Lvals, MaybeConstRval, no, set_of_var.init,
                doa_alive),
            map.det_insert(Var, State, VarStateMap0, VarStateMap),
            var_locn_set_var_state_map(VarStateMap, !VLI),

            var_locn_get_loc_var_map(!.VLI, LocVarMap0),
            make_var_depend_on_lvals_roots(Var, Lvals, LocVarMap0, LocVarMap),
            var_locn_set_loc_var_map(LocVarMap, !VLI)
        else
            set.init(Lvals),
            Expr = lval(Lval0),
            State = var_state(Lvals, no, yes(Expr), set_of_var.init,
                doa_alive),
            map.det_insert(Var, State, VarStateMap0, VarStateMap1),
            add_use_ref(BaseVar, Var, VarStateMap1, VarStateMap),
            var_locn_set_var_state_map(VarStateMap, !VLI)
        ),
        Code = empty
    else
        var_locn_materialize_vars_in_lval(Lval0, Lval, Code, !VLI),

        var_locn_get_var_state_map(!.VLI, VarStateMap0),
        LvalSet = set.make_singleton_set(Lval),
        State = var_state(LvalSet, no, no, set_of_var.init, doa_alive),
        map.det_insert(Var, State, VarStateMap0, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI),

        var_locn_get_loc_var_map(!.VLI, LocVarMap0),
        make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
        var_locn_set_loc_var_map(LocVarMap, !VLI)
    ).

:- func add_field_offset(maybe(ptag), rval, lval) = lval.

add_field_offset(Ptag, Offset, Base) =
    field(Ptag, lval(Base), Offset).

var_locn_assign_field_lval_expr_to_var(Var, BaseVar, Expr, Code, !VLI) :-
    check_var_is_unknown(!.VLI, Var),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    State = var_state(set.init, no, yes(Expr), set_of_var.init, doa_alive),
    map.det_insert(Var, State, VarStateMap0, VarStateMap1),
    add_use_ref(BaseVar, Var, VarStateMap1, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI),
    Code = empty.

%---------------------------------------------------------------------------%

var_locn_assign_const_to_var(ExprnOpts, Var, ConstRval0, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    ( if expr_is_constant(VarStateMap0, ExprnOpts, ConstRval0, ConstRval) then
        State = var_state(set.init, yes(ConstRval), no, set_of_var.init,
            doa_alive),
        map.det_insert(Var, State, VarStateMap0, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI)
    else
        unexpected($pred, "supposed constant isn't")
    ).

%---------------------------------------------------------------------------%

var_locn_assign_expr_to_var(Var, Rval, empty, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    State = var_state(set.init, no, yes(Rval), set_of_var.init, doa_alive),
    map.det_insert(Var, State, VarStateMap0, VarStateMap1),

    exprn_aux.vars_in_rval(Rval, ContainedVars0),
    list.remove_dups(ContainedVars0, ContainedVars),
    add_use_refs(ContainedVars, Var, VarStateMap1, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI).

:- pred add_use_refs(list(prog_var)::in, prog_var::in,
    var_state_map::in, var_state_map::out) is det.

add_use_refs([], _, !VarStateMap).
add_use_refs([ContainedVar | ContainedVars], UsingVar, !VarStateMap) :-
    add_use_ref(ContainedVar, UsingVar, !VarStateMap),
    add_use_refs(ContainedVars, UsingVar, !VarStateMap).

:- pred add_use_ref(prog_var::in, prog_var::in,
    var_state_map::in, var_state_map::out) is det.

add_use_ref(ContainedVar, UsingVar, !VarStateMap) :-
    map.lookup(!.VarStateMap, ContainedVar, State0),
    State0 = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using0,
        DeadOrAlive),
    set_of_var.insert(UsingVar, Using0, Using),
    State = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    map.det_update(ContainedVar, State, !VarStateMap).

%---------------------------------------------------------------------------%

var_locn_reassign_mkword_hole_var(Var, Ptag, Rval, Code, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.lookup(VarStateMap0, Var, State0),
    ( if
        State0 = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using0,
            DeadOrAlive0),
        (
            MaybeConstRval = yes(mkword_hole(Ptag))
        ;
            MaybeConstRval = no
            % Already stored value.
        ),
        MaybeExprRval = no,
        set_of_var.is_empty(Using0),
        DeadOrAlive0 = doa_alive
    then
        set.fold(clobber_old_lval(Var), Lvals, !VLI),

        var_locn_get_var_state_map(!.VLI, VarStateMap1),
        map.det_remove(Var, _State1, VarStateMap1, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI),

        % Note that if Var was initially bound to a cons_id with a
        % direct_arg_tag cons_tag, then
        %
        % - the argument we are putting into the mkword hole already
        %   has a primary tag on it, and
        %
        % - now we are putting another primary tag (Ptag) on top of it.
        %
        % This works because we use direct_arg_tags only when the
        % first ptag is guaranteed to be zero.
        var_locn_assign_expr_to_var(Var, mkword(Ptag, Rval), Code, !VLI)
    else
        unexpected($pred, "unexpected var_state")
    ).

%---------------------------------------------------------------------------%

var_locn_reassign_tagword_var(Var, ToOrMask, ToOrRval0, Code, !VLI) :-
    var_locn_produce_var_in_reg_or_stack(Var, VarLval, VarCode, !VLI),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.lookup(VarStateMap0, Var, State0),
    ( if
        State0 = var_state(Lvals, _MaybeConstRval, _MaybeExprRval, Using0,
            DeadOrAlive0),
        set_of_var.is_empty(Using0),
        DeadOrAlive0 = doa_alive
    then
        % After the assignment we are generating, the new value of Var
        % will be available *only* in VarLval.
        set.delete(VarLval, Lvals, OtherLvals),
        set.fold(clobber_old_lval(Var), OtherLvals, !VLI),

        ComplementMask = const(llconst_uint(\ ToOrMask)),
        MaskedOldVarRval = binop(bitwise_and(int_type_uint),
            lval(VarLval), ComplementMask),
        var_locn_materialize_vars_in_rval(ToOrRval0, ToOrRval,
            MaterializeCode, !VLI),
        NewVarRval = binop(bitwise_or(int_type_uint),
            MaskedOldVarRval, ToOrRval),
        Comment = "updating tagword",
        AssignCode = singleton(
            llds_instr(assign(VarLval, NewVarRval), Comment)),
        Code = VarCode ++ MaterializeCode ++ AssignCode,

        var_locn_get_var_state_map(!.VLI, VarStateMap1),
        State = var_state(set.make_singleton_set(VarLval), no, no,
            Using0, DeadOrAlive0),
        map.det_update(Var, State, VarStateMap1, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI)
    else
        unexpected($pred, "unexpected var_state")
    ).

%---------------------------------------------------------------------------%

:- pred clobber_old_lval(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

clobber_old_lval(Var, Lval, !VLI) :-
    record_clobbering(Lval, [Var], !VLI).

%---------------------------------------------------------------------------%

var_locn_assign_cell_to_var(ExprnOpts, Var, ReserveWordAtStart,
        Ptag, CellArgs0, HowToConstruct, MaybeSize, MaybeAllocId, MayUseAtomic,
        Label, Code, !StaticCellInfo, !VLI) :-
    (
        MaybeSize = yes(SizeSource),
        (
            SizeSource = known_size(Size),
            SizeRval = const(llconst_int(Size))
        ;
            SizeSource = dynamic_size(SizeVar),
            SizeRval = var(SizeVar)
        ),
        CellArgs = [cell_arg_full_word(SizeRval, complete) | CellArgs0],
        MaybeOffset = yes(1)
    ;
        MaybeSize = no,
        CellArgs = CellArgs0,
        MaybeOffset = no
    ),
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    StaticGroundCells = get_static_ground_cells(ExprnOpts),
    % We can make the cell a constant only if all its fields are filled in,
    % and they are all constants.
    ( if
        StaticGroundCells = have_static_ground_cells,
        cell_is_constant(VarStateMap, ExprnOpts, CellArgs, TypedRvals)
    then
        add_scalar_static_cell(TypedRvals, DataAddr, !StaticCellInfo),
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        CellPtrRval = mkword(Ptag, CellPtrConst),
        var_locn_assign_const_to_var(ExprnOpts, Var, CellPtrRval, !VLI),
        Code = empty
    else
        var_locn_assign_dynamic_cell_to_var(Var, ReserveWordAtStart, Ptag,
            CellArgs, HowToConstruct, MaybeOffset, MaybeAllocId, MayUseAtomic,
            Label, Code, !VLI)
    ).

:- pred var_locn_assign_dynamic_cell_to_var(prog_var::in, bool::in, ptag::in,
    list(cell_arg)::in, how_to_construct::in, maybe(int)::in,
    maybe(alloc_site_id)::in, may_use_atomic_alloc::in, label::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

var_locn_assign_dynamic_cell_to_var(Var, ReserveWordAtStart, Ptag, CellArgs,
        HowToConstruct, MaybeOffset, MaybeAllocId, MayUseAtomic,
        Label, Code, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    select_preferred_reg_or_stack(!.VLI, Var, reg_r, Lval),
    get_var_name(!.VLI, Var, VarName),
    Size = size_of_cell_args(CellArgs),
    (
        ReserveWordAtStart = yes,
        (
            MaybeOffset = yes(_),
            % Accurate GC and term profiling both want to own the word
            % before this object.
            sorry($pred, "accurate GC combined with term size profiling")
        ;
            MaybeOffset = no,
            TotalOffset = yes(1)
        ),
        TotalSize = Size + 1
    ;
        ReserveWordAtStart = no,
        TotalOffset = MaybeOffset,
        TotalSize = Size
    ),
    (
        MaybeOffset = yes(Offset),
        StartOffset = -Offset
    ;
        MaybeOffset = no,
        StartOffset = 0
    ),
    % This must appear before the call to `var_locn_save_cell_fields' in the
    % reused_cell case, otherwise `var_locn_save_cell_fields' won't know not to
    % use Lval as a temporary register (if Lval is a register).
    var_locn_set_magic_var_location(Var, Lval, !VLI),
    (
        (
            HowToConstruct = construct_in_region(RegionVar),
            LldsComment = "Allocating region for ",
            var_locn_produce_var(RegionVar, RegionRval, RegionVarCode, !VLI),
            MaybeRegionRval = yes(RegionRval)
        ;
            HowToConstruct = construct_dynamically,
            LldsComment = "Allocating heap for ",
            RegionVarCode = empty,
            MaybeRegionRval = no
        ;
            HowToConstruct = construct_statically,
            % construct_statically is not normally used in LLDS grades, but
            % it may be set by mark_static_terms.m to support loop invariant
            % hoisting. If we get here then have we missed an opportunity to
            % construct a cell statically?
            % Currently this is reachable when cell_is_constant fails on a
            % dummy type argument (cell_arg_skip_one_word).
            LldsComment = "Allocating heap (unnecessarily?) for ",
            RegionVarCode = empty,
            MaybeRegionRval = no
        ),
        assign_all_cell_args(CellArgs, yes(Ptag), lval(Lval), StartOffset,
            ArgsCode, !VLI),
        SetupReuseCode = empty,
        MaybeReuse = no_llds_reuse
    ;
        HowToConstruct = reuse_cell(CellToReuse),
        CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsId, _NeedsUpdates0),
        var_locn_produce_var(ReuseVar, ReuseRval, ReuseVarCode, !VLI),
        ( if ReuseRval = lval(ReuseLval) then
            LldsComment = "Reusing cell on heap for ",
            assign_reused_cell_to_var(Lval, Ptag, CellArgs, CellToReuse,
                ReuseLval, ReuseVarCode, StartOffset, Label, MaybeReuse,
                SetupReuseCode, ArgsCode, !VLI),
            MaybeRegionRval = no,
            RegionVarCode = empty
        else
            % This can happen if ReuseVar actually points to static data, which
            % the structure reuse analysis wouldn't have known about.
            RegionVarCode = empty,
            MaybeRegionRval = no,
            LldsComment = "Allocating heap for ",
            assign_all_cell_args(CellArgs, yes(Ptag), lval(Lval), StartOffset,
                ArgsCode, !VLI),
            SetupReuseCode = empty,
            MaybeReuse = no_llds_reuse
        )
    ),
    CellCode = singleton(
        llds_instr(
            incr_hp(Lval, yes(Ptag), TotalOffset,
                const(llconst_int(TotalSize)), MaybeAllocId, MayUseAtomic,
                MaybeRegionRval, MaybeReuse),
            LldsComment ++ VarName)
    ),
    Code = SetupReuseCode ++ CellCode ++ RegionVarCode ++ ArgsCode.

:- pred assign_reused_cell_to_var(lval::in, ptag::in, list(cell_arg)::in,
    cell_to_reuse::in, lval::in, llds_code::in, int::in, label::in,
    llds_reuse::out, llds_code::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

assign_reused_cell_to_var(Lval, Ptag, CellArgs, CellToReuse,
        ReuseLval, ReuseVarCode, StartOffset, Label, MaybeReuse,
        SetupReuseCode, ArgsCode, !VLI) :-
    CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsId, NeedsUpdates0),

    % Save any variables which are available only in the reused cell into
    % temporary registers.
    var_locn_save_cell_fields(ReuseVar, ReuseLval, SaveArgsCode,
        TempRegs0, !VLI),
    SetupReuseCode = ReuseVarCode ++ SaveArgsCode,

    % If it's possible to avoid some field assignments, we'll need an extra
    % temporary register to record whether we actually are reusing a structure
    % or if a new object was allocated.
    ( if list.member(does_not_need_update, NeedsUpdates0) then
        var_locn_acquire_reg(reg_r, FlagReg, !VLI),
        MaybeFlag = yes(FlagReg),
        TempRegs = [FlagReg | TempRegs0]
    else
        MaybeFlag = no,
        TempRegs = TempRegs0
    ),

    % XXX Optimise the stripping of the ptag when the ptags are the same
    % or the old ptag is known, as we do in the high level backend.
    MaybeReuse = llds_reuse(unop(strip_tag, lval(ReuseLval)), MaybeFlag),

    % NeedsUpdates0 can be shorter than CellArgs due to extra fields.
    Padding = list.length(CellArgs) - list.length(NeedsUpdates0),
    ( if Padding >= 0 then
        NeedsUpdates = list.duplicate(Padding, needs_update) ++ NeedsUpdates0
    else
        unexpected($pred, "Padding < 0")
    ),

    (
        MaybeFlag = yes(FlagLval),
        assign_some_cell_args(CellArgs, NeedsUpdates, yes(Ptag), lval(Lval),
            StartOffset, CannotSkipArgsCode, CanSkipArgsCode, !VLI),
        ArgsCode =
            singleton(
                llds_instr(if_val(lval(FlagLval), code_label(Label)),
                    "skip some field assignments")
            ) ++
            CanSkipArgsCode ++
            singleton(
                llds_instr(label(Label),
                    "past skipped field assignments")
            ) ++
            CannotSkipArgsCode
    ;
        MaybeFlag = no,
        assign_all_cell_args(CellArgs, yes(Ptag), lval(Lval), StartOffset,
            ArgsCode, !VLI)
    ),

    list.foldl(var_locn_release_reg, TempRegs, !VLI).

:- pred assign_all_cell_args(list(cell_arg)::in, maybe(ptag)::in,
    rval::in, int::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

assign_all_cell_args([], _, _, _, empty, !VLI).
assign_all_cell_args([CellArg | CellArgs], Ptag, Base, Offset, Code, !VLI) :-
    (
        ( CellArg = cell_arg_full_word(Rval, _Completeness)
        ; CellArg = cell_arg_take_addr_one_word(_, yes(Rval))
        ),
        assign_cell_arg(Rval, Ptag, Base, Offset, ThisCode, !VLI),
        NextOffset = Offset + 1
    ;
        (
            CellArg = cell_arg_double_word(Rval0),
            materialize_if_var(Rval0, EvalCode, Rval, !VLI),
            RvalA = unop(dword_float_get_word0, Rval),
            RvalB = unop(dword_float_get_word1, Rval)
        ;
            CellArg = cell_arg_take_addr_two_words(_, yes({RvalA, RvalB})),
            EvalCode = cord.init
        ),
        assign_cell_arg(RvalA, Ptag, Base, Offset, ThisCodeA, !VLI),
        assign_cell_arg(RvalB, Ptag, Base, Offset + 1, ThisCodeB, !VLI),
        ThisCode = EvalCode ++ ThisCodeA ++ ThisCodeB,
        NextOffset = Offset + 2
    ;
        ( CellArg = cell_arg_skip_one_word
        ; CellArg = cell_arg_take_addr_one_word(_, no)
        ),
        ThisCode = empty,
        NextOffset = Offset + 1
    ;
        ( CellArg = cell_arg_skip_two_words
        ; CellArg = cell_arg_take_addr_two_words(_, no)
        ),
        ThisCode = empty,
        NextOffset = Offset + 2
    ),
    assign_all_cell_args(CellArgs, Ptag, Base, NextOffset, RestCode, !VLI),
    Code = ThisCode ++ RestCode.

:- pred assign_some_cell_args(list(cell_arg)::in, list(needs_update)::in,
    maybe(ptag)::in, rval::in, int::in, llds_code::out,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

assign_some_cell_args([], [], _, _, _, empty, empty, !VLI).
assign_some_cell_args([], [_ | _], _, _, _, _, _, !VLI) :-
    unexpected($pred, "mismatch lists").
assign_some_cell_args([_ | _], [], _, _, _, _, _, !VLI) :-
    unexpected($pred, "mismatch lists").
assign_some_cell_args([CellArg | CellArgs], [NeedsUpdate | NeedsUpdates],
        Ptag, Base, Offset, CannotSkipArgsCode, CanSkipArgsCode, !VLI) :-
    (
        ( CellArg = cell_arg_full_word(Rval, _Completeness)
        ; CellArg = cell_arg_take_addr_one_word(_, yes(Rval))
        ),
        assign_cell_arg(Rval, Ptag, Base, Offset, ThisCode, !VLI),
        NextOffset = Offset + 1
    ;
        (
            CellArg = cell_arg_double_word(Rval0),
            materialize_if_var(Rval0, EvalCode, Rval, !VLI),
            RvalA = unop(dword_float_get_word0, Rval),
            RvalB = unop(dword_float_get_word1, Rval)
        ;
            CellArg = cell_arg_take_addr_two_words(_, yes({RvalA, RvalB})),
            EvalCode = cord.init
        ),
        assign_cell_arg(RvalA, Ptag, Base, Offset, ThisCodeA, !VLI),
        assign_cell_arg(RvalB, Ptag, Base, Offset + 1, ThisCodeB, !VLI),
        ThisCode = EvalCode ++ ThisCodeA ++ ThisCodeB,
        NextOffset = Offset + 2
    ;
        ( CellArg = cell_arg_skip_one_word
        ; CellArg = cell_arg_take_addr_one_word(_, no)
        ),
        ThisCode = empty,
        NextOffset = Offset + 1
    ;
        ( CellArg = cell_arg_skip_two_words
        ; CellArg = cell_arg_take_addr_two_words(_, no)
        ),
        ThisCode = empty,
        NextOffset = Offset + 2
    ),
    assign_some_cell_args(CellArgs, NeedsUpdates, Ptag, Base,
        NextOffset, RestCannotSkipArgsCode, RestCanSkipArgsCode, !VLI),
    (
        NeedsUpdate = needs_update,
        CannotSkipArgsCode = ThisCode ++ RestCannotSkipArgsCode,
        CanSkipArgsCode = RestCanSkipArgsCode
    ;
        NeedsUpdate = does_not_need_update,
        CannotSkipArgsCode = RestCannotSkipArgsCode,
        CanSkipArgsCode = ThisCode ++ RestCanSkipArgsCode
    ).

:- pred assign_cell_arg(rval::in, maybe(ptag)::in, rval::in,
    int::in, llds_code::out, var_locn_info::in, var_locn_info::out) is det.

assign_cell_arg(Rval0, Ptag, Base, Offset, Code, !VLI) :-
    Target = field(Ptag, Base, const(llconst_int(Offset))),
    (
        Rval0 = var(Var),
        materialize_if_var(Rval0, EvalCode, Rval, !VLI),
        var_locn_get_dummy_map(!.VLI, DummyMap),
        map.lookup(DummyMap, Var, IsDummy),
        (
            IsDummy = is_dummy_type,
            AssignCode = empty
        ;
            IsDummy = is_not_dummy_type,
            add_additional_lval_for_var(Var, Target, !VLI),
            get_var_name(!.VLI, Var, VarName),
            Comment = "assigning from " ++ VarName,
            AssignCode = singleton(llds_instr(assign(Target, Rval), Comment))
        )
    ;
        (
            Rval0 = const(_),
            Comment = "assigning field from const"
        ;
            Rval0 = mkword(_, _),
            Comment = "assigning field from tagged pointer"
        ;
            Rval0 = cast(_, _),
            Comment = "assigning field from cast"
        ;
            Rval0 = unop(_, _),
            Comment = "assigning field from unary op"
        ;
            Rval0 = binop(_, _, _),
            Comment = "assigning field from binary op"
        ;
            Rval0 = lval(_),
            Comment = "assigning field"
        ),
        EvalCode = empty,
        AssignCode = singleton(llds_instr(assign(Target, Rval0), Comment))
    ;
        Rval0 = mkword_hole(_),
        unexpected($pred, "mkword_hole")
    ;
        Rval0 = mem_addr(_),
        unexpected($pred, "unknown rval")
    ),
    Code = EvalCode ++ AssignCode.

var_locn_save_cell_fields(ReuseVar, ReuseLval, Code, Regs, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    map.lookup(VarStateMap, ReuseVar, ReuseVarState0),
    DepVarsSet = ReuseVarState0 ^ using_vars,
    DepVars = set_of_var.to_sorted_list(DepVarsSet),
    list.map_foldl2(var_locn_save_cell_fields_2(ReuseLval),
        DepVars, SaveArgsCode, [], Regs, !VLI),
    Code = cord_list_to_cord(SaveArgsCode).

:- pred var_locn_save_cell_fields_2(lval::in, prog_var::in, llds_code::out,
    list(lval)::in, list(lval)::out,
    var_locn_info::in, var_locn_info::out) is det.

var_locn_save_cell_fields_2(ReuseLval, DepVar, SaveDepVarCode, !Regs, !VLI) :-
    find_var_availability(!.VLI, DepVar, no, Avail),
    (
        Avail = available(DepVarRval),
        EvalCode = empty
    ;
        Avail = needs_materialization,
        materialize_var_general(DepVar, no, do_not_store_var, [],
            DepVarRval, EvalCode, !VLI)
    ),
    var_locn_get_dummy_map(!.VLI, DummyMap),
    map.lookup(DummyMap, DepVar, IsDummy),
    (
        IsDummy = is_dummy_type,
        AssignCode = empty
    ;
        IsDummy = is_not_dummy_type,
        ( if
            rval_depends_on_search_lval(DepVarRval,
                specific_reg_or_stack(ReuseLval))
        then
            var_locn_get_vartypes(!.VLI, VarTypes),
            lookup_var_type(VarTypes, DepVar, DepVarType),
            reg_type_for_type(!.VLI, DepVarType, RegType),
            var_locn_acquire_reg(RegType, Target, !VLI),
            add_additional_lval_for_var(DepVar, Target, !VLI),
            get_var_name(!.VLI, DepVar, DepVarName),
            AssignCode = singleton(
                llds_instr(assign(Target, DepVarRval),
                    "saving " ++ DepVarName)
            ),
            !:Regs = [Target | !.Regs]
        else
            AssignCode = empty
        )
    ),
    SaveDepVarCode = EvalCode ++ AssignCode.

:- pred reg_type_for_type(var_locn_info::in, mer_type::in, reg_type::out)
    is det.

reg_type_for_type(VLI, Type, RegType) :-
    ( if Type = float_type then
        var_locn_get_float_reg_type(VLI, RegType)
    else
        RegType = reg_r
    ).

%---------------------------------------------------------------------------%

% Record that Var is now available in Lval, as well as in the locations
% where it was available before.

:- pred add_additional_lval_for_var(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

add_additional_lval_for_var(Var, Lval, !VLI) :-
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),

    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.lookup(VarStateMap0, Var, State0),
    State0 = var_state(LvalSet0, MaybeConstRval, MaybeExprRval0,
        Using, DeadOrAlive),
    set.insert(Lval, LvalSet0, LvalSet),
    State = var_state(LvalSet, MaybeConstRval, no, Using, DeadOrAlive),
    map.det_update(Var, State, VarStateMap0, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI),

    remove_use_refs(MaybeExprRval0, Var, !VLI).

:- pred remove_use_refs(maybe(rval)::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

remove_use_refs(MaybeExprRval, UsingVar, !VLI) :-
    (
        MaybeExprRval = yes(ExprRval),
        exprn_aux.vars_in_rval(ExprRval, ContainedVars0),
        list.remove_dups(ContainedVars0, ContainedVars),
        remove_use_refs_2(ContainedVars, UsingVar, !VLI)
    ;
        MaybeExprRval = no
    ).

:- pred remove_use_refs_2(list(prog_var)::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

remove_use_refs_2([], _, !VLI).
remove_use_refs_2([ContainedVar | ContainedVars], UsingVar, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.lookup(VarStateMap0, ContainedVar, State0),
    State0 = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using0,
        DeadOrAlive),
    ( if set_of_var.remove(UsingVar, Using0, Using1) then
        Using = Using1
    else
        unexpected($pred, "using ref not present")
    ),
    State = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    map.det_update(ContainedVar, State, VarStateMap0, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI),
    ( if
        set_of_var.is_empty(Using),
        DeadOrAlive = doa_dead
    then
        var_locn_var_becomes_dead(ContainedVar, no, !VLI)
    else
        true
    ),
    remove_use_refs_2(ContainedVars, UsingVar, !VLI).

%---------------------------------------------------------------------------%

var_locn_check_and_set_magic_var_location(Var, Lval, !VLI) :-
    ( if var_locn_lval_in_use(!.VLI, Lval) then
        unexpected($pred, "in use")
    else
        var_locn_set_magic_var_location(Var, Lval, !VLI)
    ).

var_locn_set_magic_var_location(Var, Lval, !VLI) :-
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),

    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    LvalSet = set.make_singleton_set(Lval),
    State = var_state(LvalSet, no, no, set_of_var.init, doa_alive),
    map.det_insert(Var, State, VarStateMap0, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI).

%---------------------------------------------------------------------------%

:- pred check_var_is_unknown(var_locn_info::in, prog_var::in) is det.

check_var_is_unknown(VLI, Var) :-
    var_locn_get_var_state_map(VLI, VarStateMap0),
    ( if map.search(VarStateMap0, Var, _) then
        get_var_name(VLI, Var, Name),
        unexpected($pred, "existing definition of variable " ++ Name)
    else
        true
    ).

%---------------------------------------------------------------------------%

var_locn_produce_var(Var, Rval, Code, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    map.lookup(VarStateMap, Var, State),
    State = var_state(Lvals, MaybeConstRval, MaybeExprRval, _, _),
    set.to_sorted_list(Lvals, LvalsList),
    ( if
        maybe_select_lval_or_rval(LvalsList, MaybeConstRval, Rval1)
    then
        Rval = Rval1,
        Code = empty
    else if
        MaybeExprRval = yes(var(ExprVar)),
        map.lookup(VarStateMap, ExprVar, ExprState),
        ExprState = var_state(ExprLvals, ExprMaybeConstRval, _, _, _),
        set.to_sorted_list(ExprLvals, ExprLvalsList),
        maybe_select_lval_or_rval(ExprLvalsList, ExprMaybeConstRval, Rval2)
    then
        % This path is designed to generate efficient code
        % mainly for variables produced by unsafe_cast goals.
        Rval = Rval2,
        Code = empty
    else
        reg_type_for_var(!.VLI, Var, RegType),
        select_preferred_reg(!.VLI, Var, RegType, Lval),
        var_locn_place_var(Var, Lval, Code, !VLI),
        Rval = lval(Lval)
    ).

var_locn_produce_var_in_reg(Var, Lval, Code, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    map.lookup(VarStateMap, Var, State),
    State = var_state(Lvals, _, _, _, _),
    set.to_sorted_list(Lvals, LvalList),
    ( if select_reg_lval(LvalList, SelectLval) then
        Lval = SelectLval,
        Code = empty
    else
        reg_type_for_var(!.VLI, Var, RegType),
        select_preferred_reg(!.VLI, Var, RegType, Lval),
        var_locn_place_var(Var, Lval, Code, !VLI)
    ).

var_locn_produce_var_in_reg_or_stack(Var, Lval, Code, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    map.lookup(VarStateMap, Var, State),
    State = var_state(Lvals, _, _, _, _),
    set.to_sorted_list(Lvals, LvalList),
    ( if select_reg_or_stack_lval(LvalList, SelectLval) then
        Lval = SelectLval,
        Code = empty
    else
        reg_type_for_var(!.VLI, Var, RegType),
        select_preferred_reg_or_stack(!.VLI, Var, RegType, Lval),
        var_locn_place_var(Var, Lval, Code, !VLI)
    ).

:- pred reg_type_for_var(var_locn_info::in, prog_var::in, reg_type::out)
    is det.

reg_type_for_var(VLI, Var, RegType) :-
    var_locn_get_float_reg_type(VLI, FloatRegType),
    (
        FloatRegType = reg_r,
        RegType = reg_r
    ;
        FloatRegType = reg_f,
        var_locn_get_vartypes(VLI, VarTypes),
        lookup_var_type(VarTypes, Var, VarType),
        ( if VarType = float_type then
            RegType = reg_f
        else
            RegType = reg_r
        )
    ).

%---------------------------------------------------------------------------%

var_locn_clear_r1(Code, !VLI) :-
    free_up_lval(reg(reg_r, 1), [], [], Code, !VLI),
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    clobber_regs_in_maps([reg(reg_r, 1)], no,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),
    var_locn_set_var_state_map(VarStateMap, !VLI).

var_locn_place_vars(VarLocns, Code, !VLI) :-
    % If we are asked to place several variables, then we must make sure that
    % in the process of freeing up an lval for one variable, we do not save its
    % previous contents to a location that VarLocns assigns to another
    % variable. This is why we lock the registers used by VarLocns.
    % (We don't need to lock stack slots, since stack slot allocation is
    % required to ensure that the sets of variables that need to be saved
    % across calls or at entries to goals with resume points all have distinct
    % stack slots.) However, we do make one exception: if the variable being
    % moved by a freeing up operation is in VarLocns, then it is OK to move it
    % to the location assigned to it by VarLocns.
    assoc_list.values(VarLocns, Lvals),
    code_util.max_mentioned_regs(Lvals, MaxRegR, MaxRegF),
    var_locn_lock_regs(MaxRegR, MaxRegF, VarLocns, !VLI),
    actually_place_vars(VarLocns, Code, !VLI),
    var_locn_unlock_regs(!VLI).

:- pred actually_place_vars(assoc_list(prog_var, lval)::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

actually_place_vars([], empty, !VLI).
actually_place_vars([Var - Lval | Rest], Code, !VLI) :-
    var_locn_place_var(Var, Lval, FirstCode, !VLI),
    actually_place_vars(Rest, RestCode, !VLI),
    Code = FirstCode ++ RestCode.

var_locn_place_var(Var, Target, Code, !VLI) :-
    actually_place_var(Var, Target, [], Code, !VLI).

:- pred actually_place_var(prog_var::in, lval::in, list(lval)::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

actually_place_var(Var, Target, ForbiddenLvals, Code, !VLI) :-
    var_locn_get_acquired(!.VLI, Acquired),
    ( if set.member(Target, Acquired) then
        unexpected($pred, "target is acquired reg")
    else
        true
    ),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    ( if map.search(VarStateMap0, Var, State0) then
        State0 = var_state(Lvals0, _, _, _, _),
        ( if set.member(Target, Lvals0) then
            Code = empty
        else
            free_up_lval(Target, [Var], ForbiddenLvals, FreeCode, !VLI),

            % If Var's value is cached, Lvals0 must be empty. However,
            % the cached value may simply be var(Other), and Other may
            % already be in Target. However, it may also be in another lval,
            % so we say we prefer the copy in Target.
            find_var_availability(!.VLI, Var, yes(Target), Avail),
            (
                Avail = available(Rval),
                EvalCode = empty,
                ( if Rval = lval(SourceLval) then
                    record_copy(SourceLval, Target, !VLI)
                else
                    record_clobbering(Target, [Var], !VLI)
                )
            ;
                Avail = needs_materialization,
                materialize_var_general(Var, yes(Target), do_not_store_var,
                    [Target], Rval, EvalCode, !VLI),
                record_clobbering(Target, [Var], !VLI)
            ),

            % Record that Var is now in Target.
            add_additional_lval_for_var(Var, Target, !VLI),

            ( if Rval = lval(Target) then
                AssignCode = empty
            else
                get_var_name(!.VLI, Var, VarName),
                (
                    ForbiddenLvals = [],
                    string.append("Placing ", VarName, Msg)
                ;
                    ForbiddenLvals = [_ | _],
                    string.int_to_string(list.length(ForbiddenLvals),
                        LengthStr),
                    string.append_list(["Placing ", VarName,
                        " (depth ", LengthStr, ")"], Msg)
                ),
                var_locn_get_dummy_map(!.VLI, DummyMap),
                map.lookup(DummyMap, Var, IsDummy),
                (
                    IsDummy = is_dummy_type,
                    AssignCode = empty
                ;
                    IsDummy = is_not_dummy_type,
                    AssignCode = singleton(
                        llds_instr(assign(Target, Rval), Msg))
                )
            ),
            Code = FreeCode ++ EvalCode ++ AssignCode
        )
    else
        var_locn_get_dummy_map(!.VLI, DummyMap),
        map.lookup(DummyMap, Var, IsDummy),
        expect(unify(IsDummy, is_dummy_type), $pred,
            "placing nondummy var which has no state"),
        Code = empty
    ).

:- pred record_clobbering(lval::in, list(prog_var)::in,
    var_locn_info::in, var_locn_info::out) is det.

record_clobbering(Target, Assigns, !VLI) :-
    var_locn_get_loc_var_map(!.VLI, LocVarMap1),
    ( if map.search(LocVarMap1, Target, DependentVarsSet) then
        set_of_var.to_sorted_list(DependentVarsSet, DependentVars),
        map.delete(Target, LocVarMap1, LocVarMap),
        var_locn_set_loc_var_map(LocVarMap, !VLI),

        var_locn_get_var_state_map(!.VLI, VarStateMap2),
        list.foldl(clobber_lval_in_var_state_map(Target, Assigns, no),
            DependentVars, VarStateMap2, VarStateMap),
        var_locn_set_var_state_map(VarStateMap, !VLI)
    else
        true
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
    % The point of this exception is to eliminate unnecessary shuffles.
    % If place_var wants to put Var in Lval and Var is currently in (e.g)
    % field(Ptag, Lval, Offset), it will ask free_up_lval to free up
    % Lval. However, if all the other variables affected variables are also
    % available independently of Lval, there should be no need to move
    % the value now in Lval somewhere else, since our caller can simply
    % generate an assignment such as Lval := field(Ptag, Lval, Offset).
    %
:- pred free_up_lval(lval::in, list(prog_var)::in, list(lval)::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

free_up_lval(Lval, ToBeAssignedVars, ForbiddenLvals, Code, !VLI) :-
    ( if
        var_locn_get_loc_var_map(!.VLI, LocVarMap0),
        map.search(LocVarMap0, Lval, AffectedVarSet),
        set_of_var.to_sorted_list(AffectedVarSet, AffectedVars),
        var_locn_get_var_state_map(!.VLI, VarStateMap0),
        not list.foldl(
            try_clobber_lval_in_var_state_map(Lval, ToBeAssignedVars, no),
            AffectedVars, VarStateMap0, _)
    then
        free_up_lval_with_copy(Lval, ToBeAssignedVars, ForbiddenLvals,
            Code, !VLI)
    else
        Code = empty
    ).

    % If we must copy the value in Lval somewhere else to prevent it from being
    % lost when Lval is overwritten, then we try to put it into a location
    % where it will be needed next. First we find a variable that is stored
    % in Lval directly, and not just in some location whose path includes Lval
    % (the set of all variables affected by the update of Lval is
    % AffectedVarSet). Then we look up where that variable (OccupyingVar)
    % ought to be. If its desired location is Lval itself, then the copy
    % would be a null operation and would not free up Lval, so in that case
    % we get a spare register. If the desired location is on the forbidden
    % list, then we again get a spare register to avoid infinite recursion
    % (see the documentation of free_up_lval above). If the desired location
    % (Pref) is neither Lval nor or on the forbidden list, then we can possibly
    % copy Lval there. If Pref is neither in use nor locked, then moving Lval
    % there requires just an assignment. If Pref is locked, then it is possible
    % that it is locked for use by OccupyingVar. If this is so, we first
    % recursively free up Pref, and then move OccupyingVar there.
    %
:- pred free_up_lval_with_copy(lval::in, list(prog_var)::in, list(lval)::in,
    llds_code::out, var_locn_info::in, var_locn_info::out) is det.

free_up_lval_with_copy(Lval, ToBeAssignedVars, ForbiddenLvals, Code, !VLI) :-
    ( if
        var_locn_get_loc_var_map(!.VLI, LocVarMap0),
        map.search(LocVarMap0, Lval, AffectedVarSet),
        set_of_var.delete_list(ToBeAssignedVars, AffectedVarSet,
            EffAffectedVarSet),
        set_of_var.to_sorted_list(EffAffectedVarSet, EffAffectedVars),

        var_locn_get_var_state_map(!.VLI, VarStateMap0),
        ( if
            find_one_occupying_var(EffAffectedVars, Lval, VarStateMap0,
                OccupyingVar, OtherSources)
        then
            MovedVar = OccupyingVar,
            list.delete_all(EffAffectedVars, MovedVar, OtherVars),
            list.foldl(ensure_copies_are_present(Lval, OtherSources),
                OtherVars, !VLI)
        else
            EffAffectedVars = [MovedVar]
        ),

        reg_type_for_var(!.VLI, MovedVar, RegType),
        select_preferred_reg_or_stack(!.VLI, MovedVar, RegType, Pref),
        not Pref = Lval,
        not list.member(Pref, ForbiddenLvals),
        ( if var_locn_lval_in_use(!.VLI, Pref) then
            % The code generator assumes that values in stack slots don't get
            % clobbered without an explicit assignment (via a place_var
            % operation with a stack var as a target).
            Pref = reg(PrefRegType, RegNum),
            reg_is_not_locked_for_var(!.VLI, PrefRegType, RegNum, MovedVar)
        else
            true
        )
    then
        actually_place_var(MovedVar, Pref, [Lval | ForbiddenLvals], Code, !VLI)
    else
        RegType = lval_spare_reg_type(Lval),
        get_spare_reg(!.VLI, RegType, Target),
        record_copy(Lval, Target, !VLI),
        ( if
            ( Lval = stackvar(N)
            ; Lval = framevar(N)
            ),
            N < 0
        then
            % We must not copy from invalid lvals. The value we would copy
            % is a dummy in any case, so Target won't be any more valid
            % if we assigned Lval to it.
            Code = empty
        else
            Code = singleton(
                llds_instr(assign(Target, lval(Lval)),
                    "Freeing up the source lval")
            )
        )
    ).

    % Find a variable in the given list that is currently stored directly
    % in Lval (not just in some location who address includes Lval).
    %
:- pred find_one_occupying_var(list(prog_var)::in, lval::in,
    var_state_map::in, prog_var::out, list(lval)::out) is semidet.

find_one_occupying_var([Var | Vars], Lval, VarStateMap, OccupyingVar,
        OtherSources) :-
    map.lookup(VarStateMap, Var, State),
    State = var_state(LvalSet, _, _, _, _),
    ( if set.member(Lval, LvalSet) then
        OccupyingVar = Var,
        set.delete(Lval, LvalSet, OtherSourceSet),
        set.to_sorted_list(OtherSourceSet, OtherSources)
    else
        find_one_occupying_var(Vars, Lval, VarStateMap, OccupyingVar,
            OtherSources)
    ).

:- pred ensure_copies_are_present(lval::in, list(lval)::in,
    prog_var::in, var_locn_info::in, var_locn_info::out) is det.

ensure_copies_are_present(OneSource, OtherSources, Var, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    map.lookup(VarStateMap0, Var, State0),
    State0 = var_state(LvalSet0, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    set.to_sorted_list(LvalSet0, Lvals0),
    list.foldl(ensure_copies_are_present_lval(OtherSources, OneSource),
        Lvals0, LvalSet0, LvalSet),
    State = var_state(LvalSet, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    map.det_update(Var, State, VarStateMap0, VarStateMap),
    var_locn_set_var_state_map(VarStateMap, !VLI),

    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    record_change_in_root_dependencies(LvalSet0, LvalSet, Var,
        LocVarMap0, LocVarMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI).

:- pred ensure_copies_are_present_lval(list(lval)::in, lval::in,
    lval::in, set(lval)::in, set(lval)::out) is det.

ensure_copies_are_present_lval([], _, _, !LvalSet).
ensure_copies_are_present_lval([OtherSource | OtherSources], OneSource, Lval,
        !LvalSet) :-
    SubstLval = substitute_lval_in_lval(OneSource, OtherSource, Lval),
    set.insert(SubstLval, !LvalSet),
    ensure_copies_are_present_lval(OtherSources, OneSource, Lval, !LvalSet).

:- func lval_spare_reg_type(lval) = reg_type.

lval_spare_reg_type(Lval) = RegType :-
    ( if Lval = reg(reg_f, _) then
        RegType = reg_f
    else if Lval = double_stackvar(_, _) then
        RegType = reg_f
    else
        RegType = reg_r
    ).

%---------------------------------------------------------------------------%

    % Record the effect of the assignment New := Old on the state of all the
    % affected variables.
    %
    % We find the set of affected variables by finding all the root lvals in
    % New and Old, and finding all the variables that depend on them. This
    % requires significant numbers of term traversals and lookup operations.
    % We could eliminate this cost by considering *all* variables to be
    % affected. Even though it would obviously call record_copy_for_var
    % on more variables, this may be faster overall. The reason why we
    % don't do that is that its worst case behavior can be pretty bad.
    %
:- pred record_copy(lval::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

record_copy(Old, New, !VLI) :-
    expect(is_root_lval(New), $pred, "non-root New lval"),
    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    var_locn_get_loc_var_map(!.VLI, LocVarMap0),
    set.list_to_set([Old, New], AssignSet),
    get_var_set_roots(AssignSet, NoDupRootLvals),

    % Convert the list of root lvals to the list of sets of affected vars;
    % if a root lval is not in LocVarMap0, then it does not affect any
    % variables.
    list.filter_map(map.search(LocVarMap0), NoDupRootLvals, AffectedVarSets),

    % Take the union of the list of sets of affected vars.
    list.foldl(set_of_var.union, AffectedVarSets,
        set_of_var.init, AffectedVarSet),

    % Convert the union set to a list of affected vars.
    set_of_var.to_sorted_list(AffectedVarSet, AffectedVars),
    list.foldl2(record_copy_for_var(Old, New), AffectedVars,
        VarStateMap0, VarStateMap, LocVarMap0, LocVarMap),
    var_locn_set_loc_var_map(LocVarMap, !VLI),
    var_locn_set_var_state_map(VarStateMap, !VLI).

    % Record the effect of the assignment New := Old on the state of the given
    % variable.
    %
    % The main complication is that New and Old are not necessarily
    % independent: it is possible e.g. for New to be r1 and Old to be
    % field(0, r1, 2). This is why we perform the update in three steps.
    %
    % 1 For each lval in original LvalSet that contains Old, we add an
    %   additional lval in which Old is replaced by a unique Token lval
    %   (which cannot be a component of any legitimate lval). The Token
    %   represents the value being assigned, and prevents us from forgetting
    %   the path to the original value of Var from Old during step 2.
    %
    % 2 We delete from the set generated by step 1 all lvals that depend
    %   on New, to reflect the fact that New no longer contains what it
    %   used to contain.
    %
    % 3 We substitute New for all occurrences of Token, to reflect the fact
    %   that the assigned value is now in New.
    %
    % For example, with New and Old being as above and LvalSet0 being the set
    % r5, field(3, field(0, r1, 2), 4):
    %
    % - Step 1 will set LvalSet1 to r5, field(3, Token, 4), and LvalSet2
    %   to r5, field(3, field(0, r1, 2), 4), field(3, Token, 4).
    %
    % - Step 2 will set LvalSet3 r5, field(3, Token, 4).
    %
    % - Step 3 will set LvalSet r5, field(3, r1, 4).
    %
    % The reason why we don't need to modify the MaybeExprRval field in the
    % variable state is that the only lvals these fields can refer to are
    % of the form var(_).
    %
:- pred record_copy_for_var(lval::in, lval::in, prog_var::in,
    var_state_map::in, var_state_map::out,
    loc_var_map::in, loc_var_map::out) is det.

record_copy_for_var(Old, New, Var, !VarStateMap, !LocVarMap) :-
    map.lookup(!.VarStateMap, Var, State0),
    State0 = var_state(LvalSet0, MaybeConstRval, MaybeExprRval,
        Using, DeadOrAlive),
    Token = reg(reg_r, -42),
    LvalSet1 = set.map(substitute_lval_in_lval(Old, Token), LvalSet0),
    set.union(LvalSet0, LvalSet1, LvalSet2),
    LvalSet3 = set.filter(lval_does_not_support_lval(New), LvalSet2),
    LvalSet = set.map(substitute_lval_in_lval(Token, New), LvalSet3),
    State = var_state(LvalSet, MaybeConstRval, MaybeExprRval,
        Using, DeadOrAlive),
    expect(nonempty_state(State), $pred, "empty state"),
    map.det_update(Var, State, !VarStateMap),
    record_change_in_root_dependencies(LvalSet0, LvalSet, Var, !LocVarMap).

:- pred record_change_in_root_dependencies(set(lval)::in,
    set(lval)::in, prog_var::in, loc_var_map::in, loc_var_map::out) is det.

record_change_in_root_dependencies(OldLvalSet, NewLvalSet, Var, !LocVarMap) :-
    get_var_set_roots(OldLvalSet, OldRootLvals),
    get_var_set_roots(NewLvalSet, NewRootLvals),
    set.list_to_set(OldRootLvals, OldRootLvalSet),
    set.list_to_set(NewRootLvals, NewRootLvalSet),
    set.difference(NewRootLvalSet, OldRootLvalSet, InsertSet),
    set.difference(OldRootLvalSet, NewRootLvalSet, DeleteSet),
    set.to_sorted_list(InsertSet, Inserts),
    set.to_sorted_list(DeleteSet, Deletes),
    list.foldl(make_var_depend_on_root_lval(Var), Inserts, !LocVarMap),
    list.foldl(make_var_not_depend_on_root_lval(Var), Deletes, !LocVarMap).

:- func substitute_lval_in_lval(lval, lval, lval) = lval.

substitute_lval_in_lval(Old, New, Lval0) = Lval :-
    exprn_aux.substitute_lval_in_lval(Old, New, Lval0, Lval).

%---------------------------------------------------------------------------%

var_locn_var_becomes_dead(Var, FirstTime, !VLI) :-
    % Var has become dead. If there are no expressions that depend on its
    % value, delete the record of its state, thus freeing up the resources
    % it has tied down: the locations it occupies, or the variables whose
    % values its own expression refers to. If there *are* expressions that
    % depend on its value, merely update the state of the variable to say
    % that it is dead, which means that its resources will be freed when
    % the last reference to its value is deleted.
    %
    % If FirstTime = no, then it is possible that this predicate has already
    % been called for Var, if FirstTime = yes, then as a consistency check
    % we want to insist on Var being alive.

    var_locn_get_var_state_map(!.VLI, VarStateMap0),
    ( if map.search(VarStateMap0, Var, State0) then
        State0 = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using,
            DeadOrAlive0),
        (
            DeadOrAlive0 = doa_dead,
            expect(unify(FirstTime, no), $pred, "already dead")
        ;
            DeadOrAlive0 = doa_alive
        ),
        ( if set_of_var.is_empty(Using) then
            map.det_remove(Var, _, VarStateMap0, VarStateMap),
            var_locn_set_var_state_map(VarStateMap, !VLI),

            var_locn_get_loc_var_map(!.VLI, LocVarMap0),
            get_var_set_roots(Lvals, NoDupRootLvals),
            list.foldl(make_var_not_depend_on_root_lval(Var),
                NoDupRootLvals, LocVarMap0, LocVarMap),
            var_locn_set_loc_var_map(LocVarMap, !VLI),

            remove_use_refs(MaybeExprRval, Var, !VLI)
        else
            State = var_state(Lvals, MaybeConstRval, MaybeExprRval, Using,
                doa_dead),
            map.det_update(Var, State, VarStateMap0, VarStateMap),
            var_locn_set_var_state_map(VarStateMap, !VLI)
        )
    else
        var_locn_get_dummy_map(!.VLI, DummyMap),
        map.lookup(DummyMap, Var, IsDummy),
        ( if
            ( IsDummy = is_dummy_type
            ; FirstTime = no
            )
        then
            true
        else
            unexpected($pred, "premature deletion")
        )
    ).

    % Given a set of lvals, return the set of root lvals among them and inside
    % them.
    %
:- pred get_var_set_roots(set(lval)::in, list(lval)::out) is det.

get_var_set_roots(Lvals, NoDupRootLvals) :-
    set.to_sorted_list(Lvals, LvalList),
    AllLvals = LvalList ++ lvals_in_lvals(LvalList),
    list.filter(is_root_lval, AllLvals, RootLvals),
    list.sort_and_remove_dups(RootLvals, NoDupRootLvals).

%---------------------------------------------------------------------------%

:- pred maybe_select_lval_or_rval(list(lval)::in, maybe(rval)::in,
    rval::out) is semidet.

maybe_select_lval_or_rval(Lvals, MaybeConstRval, Rval) :-
    ( if select_reg_lval(Lvals, Lval1) then
        Rval = lval(Lval1)
    else if select_stack_lval(Lvals, Lval2) then
        Rval = lval(Lval2)
    else if MaybeConstRval = yes(ConstRval) then
        Rval = ConstRval
    else if select_cheapest_lval(Lvals, Lval3) then
        Rval = lval(Lval3)
    else
        fail
    ).

:- pred select_reg_lval(list(lval)::in, lval::out) is semidet.

select_reg_lval([Lval0 | Lvals0], Lval) :-
    ( if Lval0 = reg(_, _) then
        Lval = Lval0
    else
        select_reg_lval(Lvals0, Lval)
    ).

:- pred select_stack_lval(list(lval)::in, lval::out) is semidet.

select_stack_lval([Lval0 | Lvals0], Lval) :-
    ( if
        ( Lval0 = stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = double_stackvar(_, _)
        )
    then
        Lval = Lval0
    else
        select_stack_lval(Lvals0, Lval)
    ).

:- pred select_reg_or_stack_lval(list(lval)::in, lval::out)
    is semidet.

select_reg_or_stack_lval([Lval0 | Lvals0], Lval) :-
    ( if
        ( Lval0 = reg(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = double_stackvar(_, _)
        )
    then
        Lval = Lval0
    else
        select_reg_or_stack_lval(Lvals0, Lval)
    ).

    % From the given list of lvals, select the cheapest one to use.
    % Since none of the lvals will be a register or stack variable,
    % in almost all cases, the given list will be a singleton.
    %
:- pred select_cheapest_lval(list(lval)::in, lval::out) is semidet.

select_cheapest_lval([Lval | _], Lval).

%---------------------------------------------------------------------------%

:- pred select_preferred_reg(var_locn_info::in, prog_var::in, reg_type::in,
    lval::out) is det.

select_preferred_reg(VLI, Var, RegType, Lval) :-
    select_preferred_reg_avoid(VLI, Var, RegType, [], Lval).

    % Select the register into which Var should be put. If the follow_vars map
    % maps Var to a register, then select that register, unless it is already
    % in use.
    %
:- pred select_preferred_reg_avoid(var_locn_info::in, prog_var::in,
    reg_type::in, list(lval)::in, lval::out) is det.

select_preferred_reg_avoid(VLI, Var, RegType, Avoid, Lval) :-
    var_locn_get_follow_var_map(VLI, FollowVarMap),
    ( if
        map.search(FollowVarMap, Var, PrefLocn),
        ( PrefLocn = abs_reg(_, _)
        ; PrefLocn = any_reg
        )
    then
        ( if
            PrefLocn = abs_reg(PrefRegType, N),
            PrefLval = reg(PrefRegType, N),
            not var_locn_lval_in_use(VLI, PrefLval),
            not list.member(PrefLval, Avoid)
        then
            Lval = PrefLval
        else
            get_spare_reg_avoid(VLI, RegType, Avoid, Lval)
        )
    else
        get_spare_reg_avoid(VLI, RegType, Avoid, Lval)
    ).

    % Select the register or stack slot into which Var should be put. If the
    % follow_vars map maps Var to a register, then select that register,
    % unless it is already in use. If the follow_vars map does not contain Var,
    % then Var is not needed in a register in the near future, and this we
    % select Var's stack slot, unless it is in use. If all else fails, we get
    % a spare, unused register. (Note that if the follow_vars pass has not
    % been run, then all follow vars maps will be empty, which would cause
    % this predicate to try to put far too many things in stack slots.)
    %
:- pred select_preferred_reg_or_stack(var_locn_info::in, prog_var::in,
    reg_type::in, lval::out) is det.

select_preferred_reg_or_stack(VLI, Var, RegType, Lval) :-
    var_locn_get_follow_var_map(VLI, FollowVarMap),
    ( if
        map.search(FollowVarMap, Var, PrefLocn),
        ( PrefLocn = abs_reg(_, _)
        ; PrefLocn = any_reg
        )
    then
        ( if
            PrefLocn = abs_reg(RegType, N),
            PrefLval = reg(RegType, N),
            not var_locn_lval_in_use(VLI, PrefLval)
        then
            Lval = PrefLval
        else
            get_spare_reg(VLI, RegType, Lval)
        )
    else
        ( if
            var_locn_get_stack_slots(VLI, StackSlots),
            map.search(StackSlots, Var, StackSlotLocn),
            StackSlot = stack_slot_to_lval(StackSlotLocn),
            not var_locn_lval_in_use(VLI, StackSlot)
        then
            Lval = StackSlot
        else
            get_spare_reg(VLI, RegType, Lval)
        )
    ).

%---------------------------------------------------------------------------%

    % Get a register that is not in use. We start the search at the next
    % register that is needed for the next call.
    %
:- pred get_spare_reg_avoid(var_locn_info::in, reg_type::in, list(lval)::in,
    lval::out) is det.

get_spare_reg_avoid(VLI, RegType, Avoid, Lval) :-
    var_locn_get_next_non_reserved(VLI, RegType, NextNonReserved),
    get_spare_reg_2(VLI, RegType, Avoid, NextNonReserved, Lval).

:- pred get_spare_reg(var_locn_info::in, reg_type::in, lval::out) is det.

get_spare_reg(VLI, RegType, Lval) :-
    var_locn_get_next_non_reserved(VLI, RegType, NextNonReserved),
    get_spare_reg_2(VLI, RegType, [], NextNonReserved, Lval).

:- pred get_spare_reg_2(var_locn_info::in, reg_type::in, list(lval)::in,
    int::in, lval::out) is det.

get_spare_reg_2(VLI, RegType, Avoid, N0, Lval) :-
    TryLval = reg(RegType, N0),
    ( if var_locn_lval_in_use(VLI, TryLval) then
        get_spare_reg_2(VLI, RegType, Avoid, N0 + 1, Lval)
    else if list.member(TryLval, Avoid) then
        get_spare_reg_2(VLI, RegType, Avoid, N0 + 1, Lval)
    else
        Lval = TryLval
    ).

var_locn_lval_in_use(VLI, Lval) :-
    var_locn_get_loc_var_map(VLI, LocVarMap),
    var_locn_get_acquired(VLI, Acquired),
    var_locn_get_locked(VLI, LockedR, LockedF),
    (
        map.search(LocVarMap, Lval, UsingVars),
        set_of_var.is_non_empty(UsingVars)
    ;
        set.member(Lval, Acquired)
    ;
        Lval = reg(reg_r, N),
        N =< LockedR
    ;
        Lval = reg(reg_f, N),
        N =< LockedF
    ).

    % Succeeds if Var may be stored in Reg, possibly after copying its contents
    % somewhere else. This requires Reg to be either not locked, or if it is
    % locked, to be locked for Var.
    %
:- pred reg_is_not_locked_for_var(var_locn_info::in, reg_type::in, int::in,
    prog_var::in) is semidet.

reg_is_not_locked_for_var(VLI, RegType, RegNum, Var) :-
    var_locn_get_acquired(VLI, Acquired),
    var_locn_get_exceptions(VLI, Exceptions),
    (
        RegType = reg_r,
        var_locn_get_locked(VLI, Locked, _LockedF)
    ;
        RegType = reg_f,
        var_locn_get_locked(VLI, _LockedR, Locked)
    ),
    Reg = reg(RegType, RegNum),
    not set.member(Reg, Acquired),
    RegNum =< Locked => list.member(Var - Reg, Exceptions).

%---------------------------------------------------------------------------%

var_locn_acquire_reg(Type, Lval, !VLI) :-
    get_spare_reg(!.VLI, Type, Lval),
    var_locn_get_acquired(!.VLI, Acquired0),
    set.insert(Lval, Acquired0, Acquired),
    var_locn_set_acquired(Acquired, !VLI).

var_locn_acquire_reg_require_given(Lval, !VLI) :-
    ( if var_locn_lval_in_use(!.VLI, Lval) then
        unexpected($pred, "lval in use")
    else
        true
    ),
    var_locn_get_acquired(!.VLI, Acquired0),
    set.insert(Lval, Acquired0, Acquired),
    var_locn_set_acquired(Acquired, !VLI).

var_locn_acquire_reg_prefer_given(Type, Pref, Lval, !VLI) :-
    PrefLval = reg(Type, Pref),
    ( if var_locn_lval_in_use(!.VLI, PrefLval) then
        get_spare_reg(!.VLI, Type, Lval)
    else
        Lval = PrefLval
    ),
    var_locn_get_acquired(!.VLI, Acquired0),
    set.insert(Lval, Acquired0, Acquired),
    var_locn_set_acquired(Acquired, !VLI).

var_locn_acquire_reg_start_at_given(Type, Start, Lval, !VLI) :-
    StartLval = reg(Type, Start),
    ( if var_locn_lval_in_use(!.VLI, StartLval) then
        var_locn_acquire_reg_start_at_given(Type, Start + 1, Lval, !VLI)
    else
        Lval = StartLval,
        var_locn_get_acquired(!.VLI, Acquired0),
        set.insert(Lval, Acquired0, Acquired),
        var_locn_set_acquired(Acquired, !VLI)
    ).

var_locn_release_reg(Lval, !VLI) :-
    var_locn_get_acquired(!.VLI, Acquired0),
    ( if set.member(Lval, Acquired0) then
        set.delete(Lval, Acquired0, Acquired),
        var_locn_set_acquired(Acquired, !VLI)
    else
        unexpected($pred, "unacquired reg")
    ).

%---------------------------------------------------------------------------%

var_locn_lock_regs(R, F, Exceptions, !VLI) :-
    var_locn_set_locked(R, F, !VLI),
    var_locn_set_exceptions(Exceptions, !VLI).

var_locn_unlock_regs(!VLI) :-
    var_locn_set_locked(0, 0, !VLI),
    var_locn_set_exceptions([], !VLI).

%---------------------------------------------------------------------------%

var_locn_max_reg_in_use(VLI, MaxR, MaxF) :-
    var_locn_get_loc_var_map(VLI, LocVarMap),
    map.keys(LocVarMap, VarLocs),
    code_util.max_mentioned_regs(VarLocs, MaxR1, MaxF1),
    var_locn_get_acquired(VLI, Acquired),
    set.to_sorted_list(Acquired, AcquiredList),
    code_util.max_mentioned_regs(AcquiredList, MaxR2, MaxF2),
    int.max(MaxR1, MaxR2, MaxR),
    int.max(MaxF1, MaxF2, MaxF).

%---------------------------------------------------------------------------%

:- pred cell_is_constant(var_state_map::in, exprn_opts::in,
    list(cell_arg)::in, list(typed_rval)::out) is semidet.

cell_is_constant(_VarStateMap, _ExprnOpts, [], []).
cell_is_constant(VarStateMap, ExprnOpts, [CellArg | CellArgs],
        [typed_rval(Rval, LldsType) | TypedRvals]) :-
    require_complete_switch [CellArg]
    (
        CellArg = cell_arg_full_word(Rval0, complete),
        NumWords = one_word
    ;
        CellArg = cell_arg_double_word(Rval0),
        NumWords = two_words
    ;
        ( CellArg = cell_arg_take_addr_one_word(_, _)
        ; CellArg = cell_arg_take_addr_two_words(_, _)
        ; CellArg = cell_arg_skip_two_words
        ; CellArg = cell_arg_skip_one_word
        ),
        fail
    ),
    expr_is_constant(VarStateMap, ExprnOpts, Rval0, Rval),
    UnboxedFloats = get_unboxed_floats(ExprnOpts),
    UnboxedInt64s = get_unboxed_int64s(ExprnOpts),
    LldsType = rval_type_as_arg(UnboxedFloats, UnboxedInt64s, NumWords, Rval),
    cell_is_constant(VarStateMap, ExprnOpts, CellArgs, TypedRvals).

    % expr_is_constant(VarStateMap, ExprnOpts, Rval0, Rval):
    %
    % Check if Rval0 is a constant rval, after substituting the values of the
    % variables inside it. Returns the substituted, ground rval in Rval.
    %
:- pred expr_is_constant(var_state_map::in, exprn_opts::in,
    rval::in, rval::out) is semidet.

expr_is_constant(VarStateMap, ExprnOpts, Rval0, Rval) :-
    require_complete_switch [Rval0]
    (
        Rval0 = const(Const),
        exprn_aux.const_is_constant(Const, ExprnOpts, yes),
        Rval = Rval0
    ;
        Rval0 = cast(Type, SubRval0),
        expr_is_constant(VarStateMap, ExprnOpts, SubRval0, SubRval),
        Rval = cast(Type, SubRval)
    ;
        Rval0 = unop(UnOp, SubRval0),
        expr_is_constant(VarStateMap, ExprnOpts, SubRval0, SubRval),
        Rval = unop(UnOp, SubRval)
    ;
        Rval0 = binop(BinOp, SubRvalA0, SubRvalB0),
        expr_is_constant(VarStateMap, ExprnOpts, SubRvalA0, SubRvalA),
        expr_is_constant(VarStateMap, ExprnOpts, SubRvalB0, SubRvalB),
        Rval = binop(BinOp, SubRvalA, SubRvalB)
    ;
        Rval0 = mkword(Tag, SubRval0),
        expr_is_constant(VarStateMap, ExprnOpts, SubRval0, SubRval),
        Rval = mkword(Tag, SubRval)
    ;
        Rval0 = mkword_hole(_Tag),
        Rval = Rval0
    ;
        Rval0 = var(Var),
        map.search(VarStateMap, Var, State),
        State = var_state(_, yes(Rval), _, _, _),
        expect(expr_is_constant(VarStateMap, ExprnOpts, Rval, _),
            $pred, "non-constant rval in variable state")
    ;
        ( Rval0 = lval(_)
        ; Rval0 = mem_addr(_)
        ),
        fail
    ).

%---------------------------------------------------------------------------%

var_locn_materialize_vars_in_lval(Lval0, Lval, Code, !VLI) :-
    materialize_vars_in_lval_avoid(Lval0, [], Lval, Code, !VLI).

var_locn_materialize_vars_in_rval(Rval0, Rval, Code, !VLI) :-
    materialize_vars_in_rval_avoid(Rval0, no, [], Rval, Code, !VLI).

:- pred materialize_vars_in_lval_avoid(lval::in, list(lval)::in,
    lval::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_lval_avoid(Lval0, Avoid, Lval, Code, !VLI) :-
    (
        ( Lval0 = reg(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = parent_stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = double_stackvar(_, _)
        ; Lval0 = global_var_ref(_)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ),
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = succip_slot(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = mem_ref(Rval0),
        materialize_vars_in_rval_avoid(Rval0, no, Avoid, Rval, Code, !VLI),
        Lval = mem_ref(Rval)
    ;
        Lval0 = field(Tag, RvalA0, RvalB0),
        materialize_vars_in_rval_avoid(RvalA0, no, Avoid, RvalA, CodeA, !VLI),
        materialize_vars_in_rval_avoid(RvalB0, no, Avoid, RvalB, CodeB, !VLI),
        Lval = field(Tag, RvalA, RvalB),
        Code = CodeA ++ CodeB
    ;
        Lval0 = temp(_, _),
        unexpected($pred, "temp")
    ;
        Lval0 = lvar(_),
        unexpected($pred, "lvar")
    ).

    % Rval is Rval0 with all variables in Rval0 replaced by their values.
    %
:- pred materialize_vars_in_rval_avoid(rval::in, maybe(lval)::in,
    list(lval)::in, rval::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_rval_avoid(Rval0, MaybePrefer, Avoid, Rval, Code, !VLI) :-
    (
        Rval0 = lval(Lval0),
        materialize_vars_in_lval_avoid(Lval0, Avoid, Lval, Code, !VLI),
        Rval = lval(Lval)
    ;
        Rval0 = mkword(Tag, SubRval0),
        materialize_vars_in_rval_avoid(SubRval0, no,
            Avoid, SubRval, Code, !VLI),
        Rval = mkword(Tag, SubRval)
    ;
        Rval0 = mkword_hole(_Tag),
        Rval = Rval0,
        Code = empty
    ;
        Rval0 = cast(Type, SubRval0),
        materialize_vars_in_rval_avoid(SubRval0, no,
            Avoid, SubRval, Code, !VLI),
        Rval = cast(Type, SubRval)
    ;
        Rval0 = unop(Unop, SubRval0),
        materialize_vars_in_rval_avoid(SubRval0, no,
            Avoid, SubRval, Code, !VLI),
        Rval = unop(Unop, SubRval)
    ;
        Rval0 = binop(Binop, SubRvalA0, SubRvalB0),
        materialize_vars_in_rval_avoid(SubRvalA0, no,
            Avoid, SubRvalA, CodeA, !VLI),
        materialize_vars_in_rval_avoid(SubRvalB0, no,
            Avoid, SubRvalB, CodeB, !VLI),
        Rval = binop(Binop, SubRvalA, SubRvalB),
        Code = CodeA ++ CodeB
    ;
        Rval0 = const(_),
        Rval = Rval0,
        Code = empty
    ;
        Rval0 = mem_addr(MemRef0),
        materialize_vars_in_mem_ref_avoid(MemRef0, MemRef,
            Avoid, Code, !VLI),
        Rval = mem_addr(MemRef)
    ;
        Rval0 = var(Var),
        find_var_availability(!.VLI, Var, MaybePrefer, Avail),
        (
            Avail = available(Rval),
            Code = empty
        ;
            Avail = needs_materialization,
            materialize_var_general(Var, MaybePrefer, store_var,
                Avoid, Rval, Code, !VLI)
        )
    ).

    % MemRef is MemRef0 with all variables in MemRef replaced by their values.
    %
:- pred materialize_vars_in_mem_ref_avoid(mem_ref::in, mem_ref::out,
    list(lval)::in, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_mem_ref_avoid(MemRef0, MemRef, Avoid, Code, !VLI) :-
    (
        ( MemRef0 = stackvar_ref(_)
        ; MemRef0 = framevar_ref(_)
        ),
        MemRef = MemRef0,
        Code = empty
    ;
        MemRef0 = heap_ref(PtrRval0, MaybeTag, FieldNumRval0),
        materialize_vars_in_rval_avoid(PtrRval0, no,
            Avoid, PtrRval, PtrCode, !VLI),
        materialize_vars_in_rval_avoid(FieldNumRval0, no,
            Avoid, FieldNumRval, FieldNumCode, !VLI),
        Code = PtrCode ++ FieldNumCode,
        MemRef = heap_ref(PtrRval, MaybeTag, FieldNumRval)
    ).

:- type var_avail
    --->    available(rval)
    ;       needs_materialization.

:- pred find_var_availability(var_locn_info::in, prog_var::in,
    maybe(lval)::in, var_avail::out) is det.

find_var_availability(VLI, Var, MaybePrefer, Avail) :-
    var_locn_get_var_state_map(VLI, VarStateMap),
    map.lookup(VarStateMap, Var, State),
    State = var_state(Lvals, MaybeConstRval, _, _, _),
    set.to_sorted_list(Lvals, LvalsList),
    ( if
        MaybePrefer = yes(Prefer),
        list.member(Prefer, LvalsList)
    then
        Rval = lval(Prefer),
        Avail = available(Rval)
    else if
        maybe_select_lval_or_rval(LvalsList, MaybeConstRval, Rval)
    then
        Avail = available(Rval)
    else
        Avail = needs_materialization
    ).

:- pred materialize_if_var(rval::in, llds_code::out, rval::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_if_var(Rval0, EvalCode, Rval, !VLI) :-
    (
        Rval0 = var(Var),
        materialize_var(Var, EvalCode, Rval, !VLI)
    ;
        ( Rval0 = const(_)
        ; Rval0 = mkword(_, _)
        ; Rval0 = mkword_hole(_)
        ; Rval0 = cast(_, _)
        ; Rval0 = unop(_, _)
        ; Rval0 = binop(_, _, _)
        ; Rval0 = lval(_)
        ; Rval0 = mem_addr(_)
        ),
        EvalCode = empty,
        Rval = Rval0
    ).

:- pred materialize_var(prog_var::in, llds_code::out, rval::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_var(Var, EvalCode, Rval, !VLI) :-
    find_var_availability(!.VLI, Var, no, Avail),
    (
        Avail = available(Rval),
        EvalCode = empty
    ;
        Avail = needs_materialization,
        materialize_var_general(Var, no, do_not_store_var, [],
            Rval, EvalCode, !VLI)
    ).

:- type store_var_if_required
    --->    do_not_store_var
    ;       store_var.

:- pred materialize_var_general(prog_var::in, maybe(lval)::in,
    store_var_if_required::in, list(lval)::in, rval::out, llds_code::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_var_general(Var, MaybePrefer, StoreIfReq, Avoid,
        Rval, Code, !VLI) :-
    var_locn_get_var_state_map(!.VLI, VarStateMap),
    map.lookup(VarStateMap, Var, State),
    State = var_state(_Lvals, _MaybeConstRval, MaybeExprRval,
        UsingVars, _DeadOrAlive),
    (
        MaybeExprRval = yes(ExprRval)
    ;
        MaybeExprRval = no,
        unexpected($pred, "no expr")
    ),
    materialize_vars_in_rval_avoid(ExprRval, MaybePrefer,
        Avoid, Rval0, ExprCode, !VLI),
    ( if
        StoreIfReq = store_var,
        NumUsingVars = set_of_var.count(UsingVars),
        NumUsingVars > 1
    then
        reg_type_for_var(!.VLI, Var, RegType),
        select_preferred_reg_avoid(!.VLI, Var, RegType, Avoid, Lval),
        var_locn_place_var(Var, Lval, PlaceCode, !VLI),
        Rval = lval(Lval),
        Code = ExprCode ++ PlaceCode
    else
        Rval = Rval0,
        Code = ExprCode
    ).

%---------------------------------------------------------------------------%

    % Update LocVarMap0 to reflect the dependence of Var on all the root lvals
    % among Lvals or contained inside Lvals.
    %
:- pred make_var_depend_on_lvals_roots(prog_var::in,
    set(lval)::in, loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_lvals_roots(Var, Lvals, !LocVarMap) :-
    get_var_set_roots(Lvals, NoDupRootLvals),
    list.foldl(make_var_depend_on_root_lval(Var),
        NoDupRootLvals, !LocVarMap).

:- pred make_var_depend_on_lval_roots(prog_var::in,
    lval::in, loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_lval_roots(Var, Lval, !LocVarMap) :-
    Lvals = set.make_singleton_set(Lval),
    make_var_depend_on_lvals_roots(Var, Lvals, !LocVarMap).

:- pred make_var_depend_on_root_lval(prog_var::in, lval::in,
    loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_root_lval(Var, Lval, !LocVarMap) :-
    expect(is_root_lval(Lval), $pred, "non-root lval"),
    ( if map.search(!.LocVarMap, Lval, Vars0) then
        set_of_var.insert(Var, Vars0, Vars),
        map.det_update(Lval, Vars, !LocVarMap)
    else
        Vars = set_of_var.make_singleton(Var),
        map.det_insert(Lval, Vars, !LocVarMap)
    ).

    % Update LocVarMap0 to reflect that Var is no longer dependent
    % on the root lval Lval.
    %
:- pred make_var_not_depend_on_root_lval(prog_var::in, lval::in,
    loc_var_map::in, loc_var_map::out) is det.

make_var_not_depend_on_root_lval(Var, Lval, !LocVarMap) :-
    expect(is_root_lval(Lval), $pred, "non-root lval"),
    ( if map.search(!.LocVarMap, Lval, Vars0) then
        set_of_var.delete(Var, Vars0, Vars),
        ( if set_of_var.is_empty(Vars) then
            map.det_remove(Lval, _, !LocVarMap)
        else
            map.det_update(Lval, Vars, !LocVarMap)
        )
    else
        unexpected($pred, "no record")
    ).

:- pred is_root_lval(lval::in) is semidet.

is_root_lval(reg(_, _)).
is_root_lval(stackvar(_)).
is_root_lval(parent_stackvar(_)).
is_root_lval(framevar(_)).
is_root_lval(double_stackvar(_, _)).

%---------------------------------------------------------------------------%

:- type dep_search_lval
    --->    all_regs
    ;       specific_reg_or_stack(lval).

:- pred lval_does_not_support_lval(lval::in, lval::in) is semidet.

lval_does_not_support_lval(Lval1, Lval2) :-
    not lval_depends_on_search_lval(Lval2, specific_reg_or_stack(Lval1)).

:- pred rval_depends_on_search_lval(rval::in, dep_search_lval::in) is semidet.

rval_depends_on_search_lval(Rval, SearchLval) :-
    require_complete_switch [Rval]
    (
        Rval = lval(Lval),
        lval_depends_on_search_lval(Lval, SearchLval)
    ;
        ( Rval = mkword(_Tag, SubRval)
        ; Rval = cast(_Type, SubRval)
        ; Rval = unop(_UnOp, SubRval)
        ),
        rval_depends_on_search_lval(SubRval, SearchLval)
    ;
        Rval = binop(_BinOp, SubRvalA, SubRvalB),
        (
            rval_depends_on_search_lval(SubRvalA, SearchLval)
        ;
            rval_depends_on_search_lval(SubRvalB, SearchLval)
        )
    ;
        Rval = mem_addr(MemRef),
        require_complete_switch [MemRef]
        (
            ( MemRef = stackvar_ref(SubRval)
            ; MemRef = framevar_ref(SubRval)
            ),
            rval_depends_on_search_lval(SubRval, SearchLval)
        ;
            MemRef = heap_ref(CellRval, _MaybeTag, FieldNumRval),
            rval_depends_on_search_lval(CellRval, SearchLval),
            rval_depends_on_search_lval(FieldNumRval, SearchLval)
        )
    ;
        ( Rval = const(_Const)
        ; Rval = mkword_hole(_Tag)
        ),
        fail
    ;
        Rval = var(_Var),
        unexpected($pred, "var")
    ).

:- pred lval_depends_on_search_lval(lval::in, dep_search_lval::in) is semidet.

lval_depends_on_search_lval(Lval, SearchLval) :-
    require_complete_switch [Lval]
    (
        Lval = reg(_Type, _Num),
        (
            SearchLval = all_regs
        ;
            SearchLval = specific_reg_or_stack(Lval)
        )
    ;
        Lval = stackvar(_Num),
        SearchLval = specific_reg_or_stack(Lval)
    ;
        Lval = framevar(_Num),
        SearchLval = specific_reg_or_stack(Lval)
    ;
        Lval = field(_Tag, Rval0, Rval1),
        (
            rval_depends_on_search_lval(Rval0, SearchLval)
        ;
            rval_depends_on_search_lval(Rval1, SearchLval)
        )
    ;
        Lval = double_stackvar(_, _),
        SearchLval = specific_reg_or_stack(Lval)
    ;
        ( Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        ; Lval = parent_sp
        ; Lval = temp(_, _)
        ; Lval = parent_stackvar(_)
        ; Lval = succip_slot(_)
        ; Lval = succfr_slot(_)
        ; Lval = redoip_slot(_)
        ; Lval = redofr_slot(_)
        ; Lval = redofr_slot(_)
        ; Lval = prevfr_slot(_)
        ; Lval = mem_ref(_)
        ; Lval = global_var_ref(_)
        ),
        fail
    ;
        Lval = lvar(_Var),
        unexpected($pred, "lvar")
    ).

%---------------------------------------------------------------------------%

var_locn_set_follow_vars(abs_follow_vars(FollowVarMap, NextNonReservedR,
        NextNonReservedF), !VLI) :-
    var_locn_set_follow_var_map(FollowVarMap, !VLI),
    var_locn_set_next_non_reserved(NextNonReservedR, NextNonReservedF, !VLI).

%---------------------------------------------------------------------------%

:- pred get_var_name(var_locn_info::in, prog_var::in, string::out) is det.

get_var_name(VLI, Var, Name) :-
    var_locn_get_varset(VLI, VarSet),
    varset.lookup_name(VarSet, Var, Name).

%---------------------------------------------------------------------------%

:- pred nonempty_state(var_state::in) is semidet.

nonempty_state(State) :-
    State = var_state(LvalSet, MaybeConstRval, MaybeExprRval, _, _),
    ( set.is_non_empty(LvalSet)
    ; MaybeConstRval = yes(_)
    ; MaybeExprRval = yes(_)
    ).

%---------------------------------------------------------------------------%

:- type dead_or_alive
    --->    doa_dead
    ;       doa_alive.

    % The state of a variable can be one of three kinds: const, cached
    % and general.
    %
    % 1 The value of the variable is a known constant. In this case,
    %   the const_rval field will be yes, and the expr_rval field
    %   will be no. Both the empty set and nonempty sets are valid
    %   for the locs field. It will start out empty, will become
    %   nonempty if the variable is placed in some lval, and may
    %   become empty again if that lval is later overwritten.
    %
    % 2 The value of the variable is not stored anywhere, but its
    %   definition (an expression involving other variables) is cached.
    %   In this case, the const_rval field will be no, and the locs
    %   field will contain the empty set, but the expr_rval field
    %   will be yes. The variables referred to in the expr_rval field
    %   will include this variable in their using_vars sets, which
    %   protects them from deletion from the code generator state until
    %   the using variable is produced or placed in an lval. When that
    %   happens, the using variable's state will be transformed to the
    %   general, third kind, releasing this variable's hold on the
    %   variables contained in its expr_rval field.
    %
    % 3 The value of the variable is not a constant, nor is the
    %   variable cached. The locs field will be nonempty, and both
    %   const_rval and expr_rval will be no.

:- type var_state
    --->    var_state(
                % Must not contain any rval of the form var(_).
                locs            :: set(lval),

                % Must not contain any rval of the form var(_);
                % must be constant.
                const_rval      :: maybe(rval),

                % Will contain var(_), must not contain lvals.
                expr_rval       :: maybe(rval),

                % The set of vars whose expr_rval field refers to this var.
                using_vars      :: set_of_progvar,

                % A dead variable should be removed from var_state_map
                % when its using_vars field becomes empty.
                dead_or_alive   :: dead_or_alive
            ).

:- type var_state_map   ==  map(prog_var, var_state).

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

:- type loc_var_map ==  map(lval, set_of_progvar).

:- type dummy_map == map(prog_var, is_dummy_type).

:- type var_locn_info
    --->    var_locn_info(
                % The varset and vartypes from the proc_info.
                % XXX These fields are redundant; they are also stored
                % in the code_info.
                vli_varset          :: prog_varset,
                vli_vartypes        :: vartypes,

                vli_dummy_map       :: dummy_map,

                % The register type to use for float vars.
                vli_float_reg_type  :: reg_type,

                % Maps each var to its stack slot, if it has one.
                vli_stack_slots     :: stack_slots,

                % Where vars are needed next.
                vli_follow_vars_map :: abs_follow_vars_map,

                % Next rN, fN register that isn't reserved in follow_vars_map.
                vli_next_non_res_r  :: int,
                vli_next_non_res_f  :: int,

                % Documented above.
                vli_var_state_map   :: var_state_map,
                vli_loc_var_map     :: loc_var_map,

                % Locations that are temporarily reserved for purposes such as
                % holding the tags of variables during switches.
                vli_acquired        :: set(lval),

                % If these slots contain R and F then registers r1 through rR
                % and f1 through fF can only be modified by a place_var
                % operation, or by a free_up_lval operation that moves a
                % variable to the (free or freeable) lval associated with it in
                % the exceptions field. Used to implement calls, foreign_procs
                % and the store_maps at the ends of branched control
                % structures.
                vli_locked_r        :: int,
                vli_locked_f        :: int,

                % See the documentation of the locked field above.
                vli_exceptions      :: assoc_list(prog_var, lval)
            ).

%---------------------------------------------------------------------------%

init_var_locn_state(ModuleInfo, VarLocs, Liveness, VarSet, VarTypes,
        FloatRegType, StackSlots, FollowVars, VarLocnInfo) :-
    map.init(VarStateMap0),
    map.init(LocVarMap0),
    init_var_locn_state_loop(VarLocs, Liveness,
        VarStateMap0, VarStateMap, LocVarMap0, LocVarMap),
    FollowVars = abs_follow_vars(FollowVarMap, NextNonReservedR,
        NextNonReservedF),
    set.init(AcquiredRegs),
    vartypes_to_sorted_assoc_list(VarTypes, VarsTypes),
    build_dummy_list(ModuleInfo, VarsTypes, [], RevDummyAssocList),
    map.from_rev_sorted_assoc_list(RevDummyAssocList, DummyMap),
    VarLocnInfo = var_locn_info(VarSet, VarTypes, DummyMap, FloatRegType,
        StackSlots, FollowVarMap, NextNonReservedR, NextNonReservedF,
        VarStateMap, LocVarMap, AcquiredRegs, 0, 0, []).

:- pred build_dummy_list(module_info::in, assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, is_dummy_type)::in,
    assoc_list(prog_var, is_dummy_type)::out) is det.

build_dummy_list(_, [], !RevDummyAssocList).
build_dummy_list(ModuleInfo, [Var - Type | VarTypes], !RevDummyAssocList) :-
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    !:RevDummyAssocList = [Var - IsDummy | !.RevDummyAssocList],
    build_dummy_list(ModuleInfo, VarTypes, !RevDummyAssocList).

:- pred init_var_locn_state_loop(assoc_list(prog_var, lval)::in,
    set_of_progvar::in, var_state_map::in, var_state_map::out,
    loc_var_map::in, loc_var_map::out) is det.

init_var_locn_state_loop([], _, !VarStateMap, !LocVarMap).
init_var_locn_state_loop([Var - Lval | VarLocns], Liveness,
        !VarStateMap, !LocVarMap) :-
    ( if set_of_var.member(Liveness, Var) then
        add_var_locn_state(Var, Lval, !VarStateMap, !LocVarMap)
    else
        % If a variable is not live, then we do not record its state.
        % If we did, then the variable will never die (since it is already
        % dead), and the next call to clobber_regs would throw an exception,
        % since it would believe that it is throwing away the last location
        % storing the value of a "live" variable.
        true
    ),
    init_var_locn_state_loop(VarLocns, Liveness, !VarStateMap, !LocVarMap).

%---------------------%

reinit_var_locn_state(VarLocs, !VarLocnInfo) :-
    map.init(VarStateMap0),
    map.init(LocVarMap0),
    reinit_var_locn_state_loop(VarLocs,
        VarStateMap0, VarStateMap, LocVarMap0, LocVarMap),
    set.init(AcquiredRegs),
    !.VarLocnInfo = var_locn_info(VarSet, VarTypes, DummyMap, FloatRegType,
        StackSlots, FollowVarMap, NextNonReservedR, NextNonReservedF,
        _, _, _, _, _, _),
    !:VarLocnInfo = var_locn_info(VarSet, VarTypes, DummyMap, FloatRegType,
        StackSlots, FollowVarMap, NextNonReservedR, NextNonReservedF,
        VarStateMap, LocVarMap, AcquiredRegs, 0, 0, []).

:- pred reinit_var_locn_state_loop(assoc_list(prog_var, lval)::in,
    var_state_map::in, var_state_map::out,
    loc_var_map::in, loc_var_map::out) is det.

reinit_var_locn_state_loop([], !VarStateMap, !LocVarMap).
reinit_var_locn_state_loop([Var - Lval | VarLocns],
        !VarStateMap, !LocVarMap) :-
    add_var_locn_state(Var, Lval, !VarStateMap, !LocVarMap),
    reinit_var_locn_state_loop(VarLocns, !VarStateMap, !LocVarMap).

%---------------------%

:- pred add_var_locn_state(prog_var::in, lval::in,
    var_state_map::in, var_state_map::out,
    loc_var_map::in, loc_var_map::out) is det.

add_var_locn_state(Var, Lval, !VarStateMap, !LocVarMap) :-
    expect(is_root_lval(Lval), $pred, "unexpected lval"),
    NewLocs = set.make_singleton_set(Lval),
    set_of_var.init(Using),
    State = var_state(NewLocs, no, no, Using, doa_alive),
    map.det_insert(Var, State, !VarStateMap),
    make_var_depend_on_lval_roots(Var, Lval, !LocVarMap).

%---------------------------------------------------------------------------%

:- pred var_locn_get_varset(var_locn_info::in, prog_varset::out) is det.
:- pred var_locn_get_vartypes(var_locn_info::in, vartypes::out) is det.
:- pred var_locn_get_dummy_map(var_locn_info::in, dummy_map::out) is det.
:- pred var_locn_get_float_reg_type(var_locn_info::in, reg_type::out) is det.
:- pred var_locn_get_var_state_map(var_locn_info::in, var_state_map::out)
    is det.
:- pred var_locn_get_loc_var_map(var_locn_info::in, loc_var_map::out) is det.
:- pred var_locn_get_acquired(var_locn_info::in, set(lval)::out) is det.
:- pred var_locn_get_locked(var_locn_info::in, int::out, int::out) is det.
:- pred var_locn_get_exceptions(var_locn_info::in,
    assoc_list(prog_var, lval)::out) is det.

:- pred var_locn_set_follow_var_map(abs_follow_vars_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_next_non_reserved(int::in, int::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_var_state_map(var_state_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_loc_var_map(loc_var_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_acquired(set(lval)::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_locked(int::in, int::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred var_locn_set_exceptions(assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

var_locn_get_varset(VI, VI ^ vli_varset).
var_locn_get_vartypes(VI, VI ^ vli_vartypes).
var_locn_get_dummy_map(VI, VI ^ vli_dummy_map).
var_locn_get_float_reg_type(VI, VI ^ vli_float_reg_type).
var_locn_get_stack_slots(VI, VI ^ vli_stack_slots).
var_locn_get_follow_var_map(VI, VI ^ vli_follow_vars_map).
var_locn_get_next_non_reserved(VI, reg_r, VI ^ vli_next_non_res_r).
var_locn_get_next_non_reserved(VI, reg_f, VI ^ vli_next_non_res_f).
var_locn_get_var_state_map(VI, VI ^ vli_var_state_map).
var_locn_get_loc_var_map(VI, VI ^ vli_loc_var_map).
var_locn_get_acquired(VI, VI ^ vli_acquired).
var_locn_get_locked(VI, VI ^ vli_locked_r, VI ^ vli_locked_f).
var_locn_get_exceptions(VI, VI ^ vli_exceptions).

var_locn_set_follow_var_map(FVM, VI, VI ^ vli_follow_vars_map := FVM).
var_locn_set_next_non_reserved(NNR, NNF, !VI) :-
    !VI ^ vli_next_non_res_r := NNR,
    !VI ^ vli_next_non_res_f := NNF.
var_locn_set_var_state_map(VSM, VI, VI ^ vli_var_state_map := VSM).
var_locn_set_loc_var_map(LVM, VI, VI ^ vli_loc_var_map := LVM).
var_locn_set_acquired(A, VI, VI ^ vli_acquired := A).
var_locn_set_locked(R, F, !VI) :-
    !VI ^ vli_locked_r := R,
    !VI ^ vli_locked_f := F.
var_locn_set_exceptions(E, VI, VI ^ vli_exceptions := E).

%---------------------------------------------------------------------------%
:- end_module ll_backend.var_locn.
%---------------------------------------------------------------------------%
