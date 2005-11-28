%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: var_locn.m
% Author: zs.

% This module defines a set of predicates that operate on the abstract
% 'var_locn_info' structure which maintains information about where variables
% are stored, what their values are if they are not stored anywhere,
% and which registers are reserved for purposes such as holding the arguments
% of calls and tags that are to be switched upon.

%----------------------------------------------------------------------------%

:- module ll_backend__var_locn.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module libs.options.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

%----------------------------------------------------------------------------%

:- type var_locn_info.

    % init_state(Arguments, Liveness, VarSet, VarTypes, StackSlots,
    %   FollowVars, Opts, VarLocnInfo):
    %
    % Produces an initial state of the VarLocnInfo given
    % an association list of variables and lvalues. The initial
    % state places the given variables at their corresponding
    % locations, with the exception of variables which are not in
    % Liveness (this corresponds to input arguments that are not
    % used in the body). The VarSet parameter contains a mapping from
    % variables to names, which is used when code is generated
    % to provide meaningful comments. VarTypes gives the types of
    % of all the procedure's variables. StackSlots maps each variable
    % to its stack slot, if it has one. FollowVars is the initial
    % follow_vars set; such sets give guidance as to what lvals
    % (if any) each variable will be needed in next. Opts gives
    % the table of options; this is used to decide what expressions
    % are considered constants.
    %
:- pred init_state(assoc_list(prog_var, lval)::in, set(prog_var)::in,
    prog_varset::in, vartypes::in, stack_slots::in, abs_follow_vars::in,
    option_table::in, var_locn_info::out) is det.

    % reinit_state(VarLocs, !VarLocnInfo):
    %
    % Produces a new state of the VarLocnInfo in which the static
    % and mostly static information (stack slot map, follow vars map,
    % varset, option settings) comes from VarLocnInfo0 but the
    % dynamic state regarding variable locations is thrown away
    % and then rebuilt from the information in VarLocs, an
    % association list of variables and lvals. The new state
    % places the given variables at their corresponding locations.
    %
:- pred reinit_state(assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % clobber_all_regs(OkToDeleteAny, !VarLocnInfo):
    %
    % Modifies VarLocnInfo to show that all variables stored in registers
    % have been clobbered. Aborts if this deletes the last record of the
    % state of a variable unless OkToDeleteAny is `yes'.
    %
:- pred clobber_all_regs(bool::in,
    var_locn_info::in, var_locn_info::out) is det.

    % clobber_regs(Regs, !VarLocnInfo):
    %
    % Modifies VarLocnInfo to show that all variables stored in Regs
    % (a list of lvals which should contain only registers) are clobbered.
    %
:- pred clobber_regs(list(lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % set_magic_var_location(Var, Lval, !VarLocnInfo):
    %
    % Updates !VarLocnInfo to show that Var is *magically* stored in Lval.
    % Does not care if Lval is already in use; it overwrites it with the
    % new information. Var must not have been previously known. Used to
    % implement the ends of erroneous branches.
    %
:- pred set_magic_var_location(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % check_and_set_magic_var_location(Var, Lval, !VarLocnInfo):
    %
    % Updates VarLocnInfo to show that Var has been *magically* stored in Lval.
    % (The caller usually generates code to perform this magic.) Aborts if Lval
    % is already in use, or if Var was previously known.
    %
:- pred check_and_set_magic_var_location(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % lval_in_use(VarLocnInfo, Lval)
    %
    % Succeeds iff Lval, which should be a register or stack slot,
    % holds (a path to) a variable or is otherwise reserved.
    %
:- pred lval_in_use(var_locn_info::in, lval::in) is semidet.

    % var_becomes_dead(Var, FirstTime, !VarLocnInfo):
    %
    % Frees any code generator resources used by Var in !VarLocnInfo.
    % FirstTime should be no if this same operation may already have been
    % executed on Var; otherwise, var_becomes_dead will throw an exception
    % if it does not know about Var.
    %
:- pred var_becomes_dead(prog_var::in, bool::in,
    var_locn_info::in, var_locn_info::out) is det.

    % assign_var_to_var(Var, AssignedVar, !VarLocnInfo):
    %
    % Reflects the effect of the assignment Var := AssignedVar in the
    % state of !VarLocnInfo.
    %
:- pred assign_var_to_var(prog_var::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

    % assign_lval_to_var(ModuleInfo, Var, Lval, StaticCellInfo, Code,
    %   !VarLocnInfo);
    %
    % Reflects the effect of the assignment Var := lval(Lval) in the
    % state of !VarLocnInfo; any code required to effect the assignment
    % will be returned in Code.
    %
:- pred assign_lval_to_var(module_info::in, prog_var::in, lval::in,
    static_cell_info::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

    % assign_const_to_var(Var, ConstRval, !VarLocnInfo):
    %
    % Reflects the effect of the assignment Var := const(ConstRval)
    % in the state of !VarLocnInfo.
    %
:- pred assign_const_to_var(prog_var::in, rval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % assign_expr_to_var(Var, Rval, Code, !VarLocnInfo):
    %
    % Generates code to execute the assignment Var := Expr, and
    % updates the state of !VarLocnInfo accordingly.
    %
    % Expr must contain no lvals, although it may (and typically will) refer
    % to the values of other variables through rvals of the form var(_).
    %
:- pred assign_expr_to_var(prog_var::in, rval::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

    % assign_cell_to_var(ModuleInfo, Var, ReserveWordAtStart, Ptag, Vector,
    %   SizeInfo, TypeMsg, Where, Code, !StaticCellInfo, !VarLocnInfo):
    %
    % Generates code to assign to Var a pointer, tagged by Ptag, to the cell
    % whose contents are given by the other arguments, and updates the state
    % of !VarLocnInfo accordingly. If ReserveWordAtStart is yes, and the cell
    % is allocated on the heap (rather than statically), then reserve an extra
    % word immediately before the allocated object, for the garbage collector
    % to use to hold a forwarding pointer. If SizeInfo is yes(SizeVal), then
    % reserve an extra word immediately before the allocated object (regardless
    % of whether it is allocated statically or dynamically), and initialize
    % this word with the value determined by SizeVal. (NOTE: ReserveWordAtStart
    % and SizeInfo should not be yes / yes(_), because that will cause an
    % obvious conflict.) Where will say where the created cell is.
    %
:- pred assign_cell_to_var(module_info::in, prog_var::in, bool::in, tag::in,
    list(maybe(rval))::in, maybe(term_size_value)::in, string::in,
    code_tree::out, static_cell_info::in, static_cell_info::out,
    var_locn_info::in, var_locn_info::out) is det.

    % place_var(ModuleInfo, Var, Lval, Code, !VarLocnInfo):
    %
    % Produces Code to place the value of Var in Lval, and update !VarLocnInfo
    % to reflect this.
    %
:- pred place_var(module_info::in, prog_var::in, lval::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

    % place_vars(ModuleInfo, VarLocns, Code, !VarLocnInfo):
    %
    % Produces Code to place the value of each variable mentioned in VarLocns
    % into the corresponding location, and update !VarLocnInfo to reflect this.
    %
:- pred place_vars(module_info::in, assoc_list(prog_var, lval)::in,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

    % produce_var(ModuleInfo, Var, Rval, Code, !VarLocnInfo):
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
:- pred produce_var(module_info::in, prog_var::in, rval::out, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

    % produce_var_in_reg(ModuleInfo, Var, Lval, Code, !VarLocnInfo):
    %
    % Produces a code fragment Code to evaluate Var if necessary
    % and provide it as an Lval of the form reg(_).
    %
:- pred produce_var_in_reg(module_info::in, prog_var::in, lval::out,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

    % produce_var_in_reg_or_stack(ModuleInfo, Var, FollowVars, Lval, Code,
    %   !VarLocnInfo):
    %
    % Produces a code fragment Code to evaluate Var if necessary and provide it
    % as an Lval of the form reg(_), stackvar(_), or framevar(_).
    %
:- pred produce_var_in_reg_or_stack(module_info::in, prog_var::in, lval::out,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

    % acquire_reg(Lval, !VarLocnInfo):
    %
    % Finds an unused register and marks it as 'in use'.
    %
:- pred acquire_reg(lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % acquire_reg_require_given(Reg, Lval, !VarLocInfo):
    %
    % Marks Reg, which must be an unused register, as 'in use'.
    %
:- pred acquire_reg_require_given(lval::in,
    var_locn_info::in, var_locn_info::out) is det.

    % acquire_reg_prefer_given(Pref, Lval, !VarLocInfo):
    %
    % Finds an unused register, and marks it as 'in use'.
    % If Pref itself is free, assigns that.
    %
:- pred acquire_reg_prefer_given(int::in, lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % acquire_reg_start_at_given(Start, Lval, !VarLocInfo):
    %
    % Finds an unused register, and marks it as 'in use'.
    % It starts the search at the one numbered Start,
    % continuing towards higher register numbers.
    %
:- pred acquire_reg_start_at_given(int::in, lval::out,
    var_locn_info::in, var_locn_info::out) is det.

    % release_reg(Lval, !VarLocnInfo):
    %
    % Marks a previously acquired reg as no longer 'in use'.
    %
:- pred release_reg(lval::in, var_locn_info::in, var_locn_info::out) is det.

    % lock_regs(N, Exceptions, !VarLocnInfo):
    %
    % Prevents registers r1 through rN from being reused, even if there are
    % no variables referring to them, with the exceptions of the registers
    % named in Exceptions, which however can only be used to store their
    % corresponding variables. Should be followed by a call to unlock_regs.
    %
:- pred lock_regs(int::in, assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

    % unlock_regs(!VarLocnInfo):
    %
    % Undoes a lock operation.
    %
:- pred unlock_regs(var_locn_info::in, var_locn_info::out) is det.

    % clear_r1(ModuleInfo, Code, !VarLocnInfo):
    %
    % Produces a code fragment Code to move whatever is in r1 to some other
    % register, if r1 is live. This is used prior to semidet pragma c_codes.
    %
:- pred clear_r1(module_info::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

    % materialize_vars_in_lval(ModuleInfo, Lval, FinalLval, Code,
    %   !VarLocnInfo):
    %
    % For every variable in Lval, substitutes the value of the variable and
    % returns it as FinalLval. If we need to save the values of some of the
    % substituted variables somewhere so as to prevent them from being
    % evaluated again (and again ...), the required code will be returned
    % in Code.
    %
:- pred materialize_vars_in_lval(module_info::in, lval::in, lval::out,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

    % get_var_locations(VarLocnInfo, Locations):
    %
    % Returns a map from each live variable that occurs in VarLocnInfo
    % to the set of locations in which it may be found (which may be empty,
    % if the variable's value is either a known constant, or an as-yet
    % unevaluated expression).
    %
:- pred get_var_locations(var_locn_info::in, map(prog_var, set(lval))::out)
    is det.

    % get_stack_slots(VarLocnInfo, StackSlots):
    %
    % Returns the table mapping each variable to its stack slot (if any).
    %
:- pred get_stack_slots(var_locn_info::in, stack_slots::out) is det.

    % get_follow_vars(VarLocnInfo, FollowVars):
    %
    % Returns the table mapping each variable to the lval (if any)
    % where it is desired next.
    %
:- pred get_follow_var_map(var_locn_info::in, abs_follow_vars_map::out) is det.

    % get_next_non_reserved(VarLocnInfo, NonRes):
    %
    % Returns the number of the first register which is free for general use.
    % It does not reserve the register.
    %
:- pred get_next_non_reserved(var_locn_info::in, int::out) is det.

    % set_follow_vars(FollowVars):
    %
    % Sets the table mapping each variable to the lval (if any) where it is
    % desired next, and the number of the first non-reserved register.
    %
:- pred set_follow_vars(abs_follow_vars::in,
    var_locn_info::in, var_locn_info::out) is det.

    % max_reg_in_use(MaxReg):
    %
    % Returns the number of the highest numbered rN register in use.
    %
:- pred max_reg_in_use(var_locn_info::in, int::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.

:- import_module bag.
:- import_module getopt_io.
:- import_module int.
:- import_module string.
:- import_module term.
:- import_module varset.

%----------------------------------------------------------------------------%

:- type dead_or_alive   --->    dead ; alive.

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

:- type var_state   --->
    state(
        locs            :: set(lval),
                        % must not contain var(_)

        const_rval      :: maybe(rval),
                        % must not contain var(_), must be constant

        expr_rval       :: maybe(rval),
                        % will contain var(_), must not contain lvals

        using_vars      :: set(prog_var),
                        % The set of vars whose expr_rval field refers
                        % to this var.

        dead_or_alive   :: dead_or_alive
                        % A dead variable should be removed from var_state_map
                        % when its using_vars field becomes empty.
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

:- type loc_var_map ==  map(lval, set(prog_var)).

:- type var_locn_info   --->
    var_locn_info(
        varset          :: prog_varset,
                        % The varset from the proc_info.

        vartypes        :: vartypes,
                        % The vartypes from the proc_info.

        stack_slots     :: stack_slots,
                        % Maps each var to its stack slot, if it has one.

        exprn_opts      :: exprn_opts,
                        % The values of the options that are relevant to
                        % decisions about which rvals are constants.

        follow_vars_map :: abs_follow_vars_map,
                        % Where vars are needed next.

        next_non_res    :: int,
                        % Next register that isn't reserved in follow_vars_map.

        var_state_map   :: var_state_map,
                        % Documented above.

        loc_var_map     :: loc_var_map,
                        % Documented above.

        acquired        :: set(lval),
                        % Locations that are temporarily reserved for purposes
                        % such as holding the tags of variables during
                        % switches.

        locked          :: int,
                        % If this slot contains N, then registers r1 through rN
                        % can only be modified by a place_var operation, or
                        % by a free_up_lval operation that moves a variable
                        % to the (free or freeable) lval associated with it
                        % in the exceptions field. Used to implement calls,
                        % foreign_procs and the store_maps at the ends of
                        % branched control structures.

        exceptions      :: assoc_list(prog_var, lval)
                        % See the documentation of the locked field above.
    ).

%----------------------------------------------------------------------------%

init_state(VarLocs, Liveness, VarSet, VarTypes, StackSlots, FollowVars,
        Options, VarLocnInfo) :-
    map__init(VarStateMap0),
    map__init(LocVarMap0),
    init_state_2(VarLocs, yes(Liveness), VarStateMap0, VarStateMap,
        LocVarMap0, LocVarMap),
    exprn_aux__init_exprn_opts(Options, ExprnOpts),
    FollowVars = abs_follow_vars(FollowVarMap, NextNonReserved),
    set__init(AcquiredRegs),
    VarLocnInfo = var_locn_info(VarSet, VarTypes, StackSlots, ExprnOpts,
        FollowVarMap, NextNonReserved, VarStateMap, LocVarMap,
        AcquiredRegs, 0, []).

reinit_state(VarLocs, !VarLocnInfo) :-
    map__init(VarStateMap0),
    map__init(LocVarMap0),
    init_state_2(VarLocs, no, VarStateMap0, VarStateMap,
        LocVarMap0, LocVarMap),
    set__init(AcquiredRegs),
    !.VarLocnInfo = var_locn_info(VarSet, VarTypes, StackSlots, ExprnOpts,
        FollowVarMap, NextNonReserved, _, _, _, _, _),
    !:VarLocnInfo = var_locn_info(VarSet, VarTypes, StackSlots, ExprnOpts,
        FollowVarMap, NextNonReserved, VarStateMap, LocVarMap,
        AcquiredRegs, 0, []).

:- pred init_state_2(assoc_list(prog_var, lval)::in,
    maybe(set(prog_var))::in, var_state_map::in, var_state_map::out,
    loc_var_map::in, loc_var_map::out) is det.

init_state_2([], _, !VarStateMap, !LocVarMap).
init_state_2([Var - Lval |  Rest], MaybeLiveness, !VarStateMap, !LocVarMap) :-
    expect(is_root_lval(Lval), this_file, "init_state_2: unexpected lval"),
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
        true
    ;
        ( map__search(!.VarStateMap, Var, _) ->
            unexpected(this_file, "init_state_2: repeated variable")
        ;
            set__singleton_set(NewLocs, Lval),
            set__init(Using),
            State = state(NewLocs, no, no, Using, alive),
            map__det_insert(!.VarStateMap, Var, State, !:VarStateMap)
        ),
        make_var_depend_on_lval_roots(Var, Lval, !LocVarMap)
    ),
    init_state_2(Rest, MaybeLiveness, !VarStateMap, !LocVarMap).

%----------------------------------------------------------------------------%

get_var_locations(VLI, VarLocations) :-
    get_var_state_map(VLI, VarStateMap),
    map__to_assoc_list(VarStateMap, VarLocList),
    list__filter_map(convert_live_to_lval_set, VarLocList, LiveVarLocList),
    map__from_assoc_list(LiveVarLocList, VarLocations).

:- pred convert_live_to_lval_set(pair(prog_var, var_state)::in,
    pair(prog_var, set(lval))::out) is semidet.

convert_live_to_lval_set(Var - State, Var - Lvals) :-
    State = state(Lvals, _, _, _, alive).

%----------------------------------------------------------------------------%

clobber_all_regs(OkToDeleteAny, !VLI) :-
    set_acquired(set__init, !VLI),
    set_locked(0, !VLI),
    set_exceptions([], !VLI),
    get_loc_var_map(!.VLI, LocVarMap0),
    get_var_state_map(!.VLI, VarStateMap0),
    map__keys(LocVarMap0, Locs),
    clobber_regs_in_maps(Locs, OkToDeleteAny,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    set_loc_var_map(LocVarMap, !VLI),
    set_var_state_map(VarStateMap, !VLI).

clobber_regs(Regs, !VLI) :-
    get_acquired(!.VLI, Acquired0),
    Acquired = set__delete_list(Acquired0, Regs),
    set_acquired(Acquired, !VLI),
    get_loc_var_map(!.VLI, LocVarMap0),
    get_var_state_map(!.VLI, VarStateMap0),
    clobber_regs_in_maps(Regs, no,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    set_loc_var_map(LocVarMap, !VLI),
    set_var_state_map(VarStateMap, !VLI).

:- pred clobber_regs_in_maps(list(lval)::in, bool::in,
    loc_var_map::in, loc_var_map::out,
    var_state_map::in, var_state_map::out) is det.

clobber_regs_in_maps([], _, !LocVarMap, !VarStateMap).
clobber_regs_in_maps([Lval | Lvals], OkToDeleteAny,
        !LocVarMap, !VarStateMap) :-
    (
        Lval = reg(_, _),
        map__search(!.LocVarMap, Lval, DependentVarsSet)
    ->
        map__delete(!.LocVarMap, Lval, !:LocVarMap),
        set__to_sorted_list(DependentVarsSet, DependentVars),
        list__foldl(clobber_lval_in_var_state_map(Lval, [], OkToDeleteAny),
            DependentVars, !VarStateMap)
    ;
        true
    ),
    clobber_regs_in_maps(Lvals, OkToDeleteAny, !LocVarMap, !VarStateMap).

:- pred clobber_lval_in_var_state_map(lval::in, list(prog_var)::in,
    bool::in, prog_var::in, var_state_map::in, var_state_map::out) is det.

clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny, Var,
        !VarStateMap) :-
    (
        try_clobber_lval_in_var_state_map(Lval, OkToDeleteVars, OkToDeleteAny,
            Var, !VarStateMap)
    ->
        true
    ;
        unexpected(this_file, "clobber_lval_in_var_state_map: empty state")
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
    map__lookup(!.VarStateMap, Var, State0),
    State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    LvalSet = set__filter(lval_does_not_support_lval(Lval), LvalSet0),
    State = state(LvalSet, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    (
        nonempty_state(State)
    ;
        list__member(Var, OkToDeleteVars)
    ;
        OkToDeleteAny = yes
    ;
        DeadOrAlive = dead,
        set__to_sorted_list(Using, UsingVars),
        recursive_using_vars_dead_and_ok_to_delete(UsingVars,
            !.VarStateMap, OkToDeleteVars)
    ),
    map__det_update(!.VarStateMap, Var, State, !:VarStateMap).

:- pred recursive_using_vars_dead_and_ok_to_delete(
    list(prog_var)::in, var_state_map::in, list(prog_var)::in) is semidet.

recursive_using_vars_dead_and_ok_to_delete([], _, _).
recursive_using_vars_dead_and_ok_to_delete([Var | Vars], VarStateMap,
        OkToDeleteVars) :-
    (
        list__member(Var, OkToDeleteVars)
    ;
        map__lookup(VarStateMap, Var, State),
        State = state(_, _, _, Using, DeadOrAlive),
        DeadOrAlive = dead,
        set__to_sorted_list(Using, UsingVars),
        recursive_using_vars_dead_and_ok_to_delete(UsingVars,
            VarStateMap, OkToDeleteVars)
    ),
    recursive_using_vars_dead_and_ok_to_delete(Vars,
        VarStateMap, OkToDeleteVars).

%----------------------------------------------------------------------------%

assign_var_to_var(Var, OldVar, !VLI) :-
    check_var_is_unknown(!.VLI, Var),
    get_var_state_map(!.VLI, VarStateMap0),
    map__lookup(VarStateMap0, OldVar, OldState0),
    OldState0 = state(Lvals, MaybeConstRval, MaybeExprRval,
        Using0, DeadOrAlive),
    (
        MaybeExprRval = yes(_),
        State = state(Lvals, MaybeConstRval, yes(var(OldVar)), set__init,
            alive),
        set__insert(Using0, Var, Using),
        OldState = state(Lvals, MaybeConstRval, MaybeExprRval, Using,
            DeadOrAlive),
        map__det_update(VarStateMap0, OldVar, OldState, VarStateMap1)
    ;
        MaybeExprRval = no,
        set__init(Empty),
        State = state(Lvals, MaybeConstRval, no, Empty, alive),
        VarStateMap1 = VarStateMap0
    ),
    map__det_insert(VarStateMap1, Var, State, VarStateMap),
    set_var_state_map(VarStateMap, !VLI),

    get_loc_var_map(!.VLI, LocVarMap0),
    make_var_depend_on_lvals_roots(Var, Lvals, LocVarMap0, LocVarMap),
    set_loc_var_map(LocVarMap, !VLI).

%----------------------------------------------------------------------------%

assign_lval_to_var(ModuleInfo, Var, Lval0, StaticCellInfo, Code, !VLI) :-
    check_var_is_unknown(!.VLI, Var),
    ( Lval0 = field(yes(Ptag), var(BaseVar), const(int_const(Offset))) ->
        get_var_state_map(!.VLI, VarStateMap0),
        map__lookup(VarStateMap0, BaseVar, BaseState),
        BaseState = state(BaseVarLvals, MaybeConstBaseVarRval,
            _MaybeExprRval, _UsingVars, _DeadOrAlive),
        (
            MaybeConstBaseVarRval = yes(BaseVarRval),
            BaseVarRval = mkword(Ptag, BaseConst),
            BaseConst = const(data_addr_const(DataAddr, MaybeBaseOffset)),
            % XXX We could drop the MaybeBaseOffset = no condition,
            % but this would require more complex code below.
            MaybeBaseOffset = no,
            search_static_cell_offset(StaticCellInfo, DataAddr, Offset,
                SelectedArgRval)
        ->
            MaybeConstRval = yes(SelectedArgRval),
            Lvals = set__map(add_field_offset(yes(Ptag),
                const(int_const(Offset))), BaseVarLvals),
            set__init(Using),
            State = state(Lvals, MaybeConstRval, no, Using, alive),
            map__det_insert(VarStateMap0, Var, State, VarStateMap),
            set_var_state_map(VarStateMap, !VLI),

            get_loc_var_map(!.VLI, LocVarMap0),
            make_var_depend_on_lvals_roots(Var, Lvals, LocVarMap0, LocVarMap),
            set_loc_var_map(LocVarMap, !VLI)
        ;
            set__init(Lvals),
            Expr = lval(Lval0),
            set__init(Using),
            State = state(Lvals, no, yes(Expr), Using, alive),
            map__det_insert(VarStateMap0, Var, State, VarStateMap1),
            add_use_ref(BaseVar, Var, VarStateMap1, VarStateMap),
            set_var_state_map(VarStateMap, !VLI)
        ),
        Code = empty
    ;
        materialize_vars_in_lval(ModuleInfo, Lval0, Lval, Code, !VLI),

        get_var_state_map(!.VLI, VarStateMap0),
        set__singleton_set(LvalSet, Lval),
        State = state(LvalSet, no, no, set__init, alive),
        map__det_insert(VarStateMap0, Var, State, VarStateMap),
        set_var_state_map(VarStateMap, !VLI),

        get_loc_var_map(!.VLI, LocVarMap0),
        make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
        set_loc_var_map(LocVarMap, !VLI)
    ).

:- func add_field_offset(maybe(tag), rval, lval) = lval.

add_field_offset(Ptag, Offset, Base) =
    field(Ptag, lval(Base), Offset).

%----------------------------------------------------------------------------%

assign_const_to_var(Var, ConstRval0, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    get_var_state_map(!.VLI, VarStateMap0),
    get_exprn_opts(!.VLI, ExprnOpts),
    ( expr_is_constant(VarStateMap0, ExprnOpts, ConstRval0, ConstRval) ->
        State = state(set__init, yes(ConstRval), no, set__init, alive),
        map__det_insert(VarStateMap0, Var, State, VarStateMap),
        set_var_state_map(VarStateMap, !VLI)
    ;
        unexpected(this_file, "set_var_state_map: supposed constant isn't")
    ).

%----------------------------------------------------------------------------%

assign_expr_to_var(Var, Rval, empty, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    get_var_state_map(!.VLI, VarStateMap0),
    State = state(set__init, no, yes(Rval), set__init, alive),
    map__det_insert(VarStateMap0, Var, State, VarStateMap1),

    exprn_aux__vars_in_rval(Rval, ContainedVars0),
    list__remove_dups(ContainedVars0, ContainedVars),
    add_use_refs(ContainedVars, Var, VarStateMap1, VarStateMap),
    set_var_state_map(VarStateMap, !VLI).

:- pred add_use_refs(list(prog_var)::in, prog_var::in,
    var_state_map::in, var_state_map::out) is det.

add_use_refs([], _, !VarStateMap).
add_use_refs([ContainedVar | ContainedVars], UsingVar, !VarStateMap) :-
    add_use_ref(ContainedVar, UsingVar, !VarStateMap),
    add_use_refs(ContainedVars, UsingVar, !VarStateMap).

:- pred add_use_ref(prog_var::in, prog_var::in,
    var_state_map::in, var_state_map::out) is det.

add_use_ref(ContainedVar, UsingVar, !VarStateMap) :-
    map__lookup(!.VarStateMap, ContainedVar, State0),
    State0 = state(Lvals, MaybeConstRval, MaybeExprRval, Using0, DeadOrAlive),
    set__insert(Using0, UsingVar, Using),
    State = state(Lvals, MaybeConstRval, MaybeExprRval, Using, DeadOrAlive),
    map__det_update(!.VarStateMap, ContainedVar, State, !:VarStateMap).

%----------------------------------------------------------------------------%

assign_cell_to_var(ModuleInfo, Var, ReserveWordAtStart, Ptag, MaybeRvals0,
        SizeInfo, TypeMsg, Code, !StaticCellInfo, !VLI) :-
    (
        SizeInfo = yes(SizeSource),
        (
            SizeSource = known_size(Size),
            SizeRval = const(int_const(Size))
        ;
            SizeSource = dynamic_size(SizeVar),
            SizeRval = var(SizeVar)
        ),
        MaybeRvals = [yes(SizeRval) | MaybeRvals0],
        MaybeOffset = yes(1)
    ;
        SizeInfo = no,
        MaybeRvals = MaybeRvals0,
        MaybeOffset = no
    ),
    get_var_state_map(!.VLI, VarStateMap),
    get_exprn_opts(!.VLI, ExprnOpts),
    ( cell_is_constant(VarStateMap, ExprnOpts, MaybeRvals, RvalsTypes) ->
        add_static_cell(RvalsTypes, DataAddr, !StaticCellInfo),
        CellPtrConst = const(data_addr_const(DataAddr, MaybeOffset)),
        CellPtrRval = mkword(Ptag, CellPtrConst),
        assign_const_to_var(Var, CellPtrRval, !VLI),
        Code = empty
    ;
        assign_dynamic_cell_to_var(ModuleInfo, Var, ReserveWordAtStart, Ptag,
            MaybeRvals, MaybeOffset, TypeMsg, Code, !VLI)
    ).

:- pred assign_dynamic_cell_to_var(module_info::in, prog_var::in, bool::in,
    tag::in, list(maybe(rval))::in, maybe(int)::in, string::in,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

assign_dynamic_cell_to_var(ModuleInfo, Var, ReserveWordAtStart, Ptag, Vector,
        MaybeOffset, TypeMsg, Code, !VLI) :-
    check_var_is_unknown(!.VLI, Var),

    select_preferred_reg_or_stack_check(!.VLI, Var, Lval),
    get_var_name(!.VLI, Var, VarName),
    list__length(Vector, Size),
    (
        ReserveWordAtStart = yes,
        (
            MaybeOffset = yes(_),
            % Accurate GC and term profiling both want to own the word
            % before this object.
            sorry(this_file, "accurate GC combined with term size profiling")
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
    CellCode = node([
        incr_hp(Lval, yes(Ptag), TotalOffset,
            const(int_const(TotalSize)), TypeMsg)
            - string__append("Allocating heap for ", VarName)
    ]),
    set_magic_var_location(Var, Lval, !VLI),
    (
        MaybeOffset = yes(Offset),
        StartOffset = -Offset
    ;
        MaybeOffset = no,
        StartOffset = 0
    ),
    assign_cell_args(ModuleInfo, Vector, yes(Ptag), lval(Lval), StartOffset,
        ArgsCode, !VLI),
    Code = tree(CellCode, ArgsCode).

:- pred assign_cell_args(module_info::in, list(maybe(rval))::in,
    maybe(tag)::in, rval::in, int::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

assign_cell_args(_, [], _, _, _, empty, !VLI).
assign_cell_args(ModuleInfo, [MaybeRval0 | MaybeRvals0], Ptag, Base, Offset,
        Code, !VLI) :-
    (
        MaybeRval0 = yes(Rval0),
        Target = field(Ptag, Base, const(int_const(Offset))),
        ( Rval0 = var(Var) ->
            find_var_availability(!.VLI, Var, no, Avail),
            (
                Avail = available(Rval),
                EvalCode = empty
            ;
                Avail = needs_materialization,
                materialize_var(ModuleInfo, Var, no, no, [], Rval, EvalCode,
                    !VLI)
            ),
            get_vartypes(!.VLI, VarTypes),
            map__lookup(VarTypes, Var, Type),
            ( is_dummy_argument_type(ModuleInfo, Type) ->
                AssignCode = empty
            ;
                add_additional_lval_for_var(Var, Target, !VLI),
                get_var_name(!.VLI, Var, VarName),
                Comment = "assigning from " ++ VarName,
                AssignCode = node([assign(Target, Rval) - Comment])
            )
        ; Rval0 = const(_) ->
            EvalCode = empty,
            Comment = "assigning field from const",
            AssignCode = node([assign(Target, Rval0) - Comment])
        ;
            unexpected(this_file, "assign_cell_args: unknown rval")
        ),
        ThisCode = tree(EvalCode, AssignCode)
    ;
        MaybeRval0 = no,
        ThisCode = empty
    ),
    assign_cell_args(ModuleInfo, MaybeRvals0, Ptag, Base, Offset + 1, RestCode,
        !VLI),
    Code = tree(ThisCode, RestCode).

%----------------------------------------------------------------------------%

% Record that Var is now available in Lval, as well as in the locations
% where it was available before.

:- pred add_additional_lval_for_var(prog_var::in, lval::in,
    var_locn_info::in, var_locn_info::out) is det.

add_additional_lval_for_var(Var, Lval, !VLI) :-
    get_loc_var_map(!.VLI, LocVarMap0),
    make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
    set_loc_var_map(LocVarMap, !VLI),

    get_var_state_map(!.VLI, VarStateMap0),
    map__lookup(VarStateMap0, Var, State0),
    State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval0,
        Using, DeadOrAlive),
    set__insert(LvalSet0, Lval, LvalSet),
    State = state(LvalSet, MaybeConstRval, no, Using, DeadOrAlive),
    map__det_update(VarStateMap0, Var, State, VarStateMap),
    set_var_state_map(VarStateMap, !VLI),

    remove_use_refs(MaybeExprRval0, Var, !VLI).

:- pred remove_use_refs(maybe(rval)::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

remove_use_refs(MaybeExprRval, UsingVar, !VLI) :-
    (
        MaybeExprRval = yes(ExprRval),
        exprn_aux__vars_in_rval(ExprRval, ContainedVars0),
        list__remove_dups(ContainedVars0, ContainedVars),
        remove_use_refs_2(ContainedVars, UsingVar, !VLI)
    ;
        MaybeExprRval = no
    ).

:- pred remove_use_refs_2(list(prog_var)::in, prog_var::in,
    var_locn_info::in, var_locn_info::out) is det.

remove_use_refs_2([], _, !VLI).
remove_use_refs_2([ContainedVar | ContainedVars], UsingVar, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap0),
    map__lookup(VarStateMap0, ContainedVar, State0),
    State0 = state(Lvals, MaybeConstRval, MaybeExprRval, Using0, DeadOrAlive),
    ( set__remove(Using0, UsingVar, Using1) ->
        Using = Using1
    ;
        unexpected(this_file, "remove_use_refs_2: using ref not present")
    ),
    State = state(Lvals, MaybeConstRval, MaybeExprRval, Using, DeadOrAlive),
    map__det_update(VarStateMap0, ContainedVar, State, VarStateMap),
    set_var_state_map(VarStateMap, !VLI),
    (
        set__empty(Using),
        DeadOrAlive = dead
    ->
        var_becomes_dead(ContainedVar, no, !VLI)
    ;
        true
    ),
    remove_use_refs_2(ContainedVars, UsingVar, !VLI).

%----------------------------------------------------------------------------%

check_and_set_magic_var_location(Var, Lval, !VLI) :-
    ( lval_in_use(!.VLI, Lval) ->
        unexpected(this_file, "check_and_set_magic_var_location: in use")
    ;
        set_magic_var_location(Var, Lval, !VLI)
    ).

set_magic_var_location(Var, Lval, !VLI) :-
    get_loc_var_map(!.VLI, LocVarMap0),
    make_var_depend_on_lval_roots(Var, Lval, LocVarMap0, LocVarMap),
    set_loc_var_map(LocVarMap, !VLI),

    get_var_state_map(!.VLI, VarStateMap0),
    set__singleton_set(LvalSet, Lval),
    State = state(LvalSet, no, no, set__init, alive),
    map__det_insert(VarStateMap0, Var, State, VarStateMap),
    set_var_state_map(VarStateMap, !VLI).

%----------------------------------------------------------------------------%

:- pred check_var_is_unknown(var_locn_info::in, prog_var::in) is det.

check_var_is_unknown(VLI, Var) :-
    get_var_state_map(VLI, VarStateMap0),
    ( map__search(VarStateMap0, Var, _) ->
        get_var_name(VLI, Var, Name),
        Msg = "assign_to_var: existing definition of variable " ++ Name,
        unexpected(this_file, Msg)
    ;
        true
    ).

%----------------------------------------------------------------------------%

produce_var(ModuleInfo, Var, Rval, Code, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap),
    map__lookup(VarStateMap, Var, State),
    State = state(Lvals, MaybeConstRval, MaybeExprRval, _, _),
    set__to_sorted_list(Lvals, LvalsList),
    (
        maybe_select_lval_or_rval(LvalsList, MaybeConstRval, Rval1)
    ->
        Rval = Rval1,
        Code = empty
    ;
        MaybeExprRval = yes(var(ExprVar)),
        map__lookup(VarStateMap, ExprVar, ExprState),
        ExprState = state(ExprLvals, ExprMaybeConstRval, _, _, _),
        set__to_sorted_list(ExprLvals, ExprLvalsList),
        maybe_select_lval_or_rval(ExprLvalsList, ExprMaybeConstRval, Rval2)
    ->
        % This path is designed to generate efficient code
        % mainly for variables produced by unsafe_cast goals.
        Rval = Rval2,
        Code = empty
    ;
        select_preferred_reg(!.VLI, Var, Lval),
        place_var(ModuleInfo, Var, Lval, Code, !VLI),
        Rval = lval(Lval)
    ).

produce_var_in_reg(ModuleInfo, Var, Lval, Code, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap),
    map__lookup(VarStateMap, Var, State),
    State = state(Lvals, _, _, _, _),
    set__to_sorted_list(Lvals, LvalList),
    ( select_reg_lval(LvalList, SelectLval) ->
        Lval = SelectLval,
        Code = empty
    ;
        select_preferred_reg(!.VLI, Var, Lval),
        place_var(ModuleInfo, Var, Lval, Code, !VLI)
    ).

produce_var_in_reg_or_stack(ModuleInfo, Var, Lval, Code, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap),
    map__lookup(VarStateMap, Var, State),
    State = state(Lvals, _, _, _, _),
    set__to_sorted_list(Lvals, LvalList),
    ( select_reg_or_stack_lval(LvalList, SelectLval) ->
        Lval = SelectLval,
        Code = empty
    ;
        select_preferred_reg_or_stack_check(!.VLI, Var, Lval),
        place_var(ModuleInfo, Var, Lval, Code, !VLI)
    ).

%----------------------------------------------------------------------------%

clear_r1(ModuleInfo, Code, !VLI) :-
    free_up_lval(ModuleInfo, reg(r, 1), [], [], Code, !VLI),
    get_loc_var_map(!.VLI, LocVarMap0),
    get_var_state_map(!.VLI, VarStateMap0),
    clobber_regs_in_maps([reg(r, 1)], no,
        LocVarMap0, LocVarMap, VarStateMap0, VarStateMap),
    set_loc_var_map(LocVarMap, !VLI),
    set_var_state_map(VarStateMap, !VLI).

place_vars(ModuleInfo, VarLocns, Code, !VLI) :-
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
    assoc_list__values(VarLocns, Lvals),
    code_util__max_mentioned_reg(Lvals, MaxReg),
    lock_regs(MaxReg, VarLocns, !VLI),
    actually_place_vars(ModuleInfo, VarLocns, Code, !VLI),
    unlock_regs(!VLI).

:- pred actually_place_vars(module_info::in, assoc_list(prog_var, lval)::in,
    code_tree::out, var_locn_info::in, var_locn_info::out) is det.

actually_place_vars(_, [], empty, !VLI).
actually_place_vars(ModuleInfo, [Var - Lval | Rest], Code, !VLI) :-
    place_var(ModuleInfo, Var, Lval, FirstCode, !VLI),
    actually_place_vars(ModuleInfo, Rest, RestCode, !VLI),
    Code = tree(FirstCode, RestCode).

place_var(ModuleInfo, Var, Target, Code, !VLI) :-
    actually_place_var(ModuleInfo, Var, Target, [], Code, !VLI).

:- pred actually_place_var(module_info::in, prog_var::in, lval::in,
    list(lval)::in, code_tree::out, var_locn_info::in, var_locn_info::out)
    is det.

actually_place_var(ModuleInfo, Var, Target, ForbiddenLvals, Code, !VLI) :-
    get_acquired(!.VLI, Acquired),
    ( set__member(Target, Acquired) ->
        unexpected(this_file, "actually_place_var: target is acquired reg")
    ;
        true
    ),
    get_var_state_map(!.VLI, VarStateMap0),
    map__lookup(VarStateMap0, Var, State0),
    State0 = state(Lvals0, _, _, _, _),
    ( set__member(Target, Lvals0) ->
        Code = empty
    ;
        free_up_lval(ModuleInfo, Target, [Var], ForbiddenLvals, FreeCode,
            !VLI),

        % If Var's value is cached, Lvals0 must be empty. However, the cached
        % value may simply be var(Other), and Other may already be in Target.
        % However, it may also be in another lval, so we say we prefer the
        % copy in Target.
        find_var_availability(!.VLI, Var, yes(Target), Avail),
        (
            Avail = available(Rval),
            EvalCode = empty,
            ( Rval = lval(SourceLval) ->
                record_copy(SourceLval, Target, !VLI)
            ;
                record_clobbering(Target, [Var], !VLI)
            )
        ;
            Avail = needs_materialization,
            materialize_var(ModuleInfo, Var, yes(Target), no, [Target], Rval,
                EvalCode, !VLI),
            record_clobbering(Target, [Var], !VLI)
        ),

        % Record that Var is now in Target.
        add_additional_lval_for_var(Var, Target, !VLI),

        ( Rval = lval(Target) ->
            AssignCode = empty
        ;
            get_var_name(!.VLI, Var, VarName),
            (
                ForbiddenLvals = [],
                string__append("Placing ", VarName, Msg)
            ;
                ForbiddenLvals = [_ | _],
                string__int_to_string(list__length(ForbiddenLvals),
                    LengthStr),
                string__append_list(["Placing ", VarName,
                    " (depth ", LengthStr, ")"], Msg)
            ),
            get_vartypes(!.VLI, VarTypes),
            map__lookup(VarTypes, Var, Type),
            ( is_dummy_argument_type(ModuleInfo, Type) ->
                AssignCode = empty
            ;
                AssignCode = node([assign(Target, Rval) - Msg])
            )
        ),
        Code = tree(FreeCode, tree(EvalCode, AssignCode))
    ).

:- pred record_clobbering(lval::in, list(prog_var)::in,
    var_locn_info::in, var_locn_info::out) is det.

record_clobbering(Target, Assigns, !VLI) :-
    get_loc_var_map(!.VLI, LocVarMap1),
    ( map__search(LocVarMap1, Target, DependentVarsSet) ->
        set__to_sorted_list(DependentVarsSet, DependentVars),
        map__delete(LocVarMap1, Target, LocVarMap),
        set_loc_var_map(LocVarMap, !VLI),

        get_var_state_map(!.VLI, VarStateMap2),
        list__foldl(clobber_lval_in_var_state_map(Target, Assigns, no),
            DependentVars, VarStateMap2, VarStateMap),
        set_var_state_map(VarStateMap, !VLI)
    ;
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
:- pred free_up_lval(module_info::in, lval::in, list(prog_var)::in,
    list(lval)::in, code_tree::out, var_locn_info::in, var_locn_info::out)
    is det.

free_up_lval(ModuleInfo, Lval, ToBeAssignedVars, ForbiddenLvals, Code, !VLI) :-
    (
        get_loc_var_map(!.VLI, LocVarMap0),
        map__search(LocVarMap0, Lval, AffectedVarSet),
        set__to_sorted_list(AffectedVarSet, AffectedVars),
        get_var_state_map(!.VLI, VarStateMap0),
        \+ list__foldl(
            try_clobber_lval_in_var_state_map(Lval, ToBeAssignedVars, no),
            AffectedVars, VarStateMap0, _)
    ->
        free_up_lval_with_copy(ModuleInfo, Lval, ToBeAssignedVars,
            ForbiddenLvals, Code, !VLI)
    ;
        Code = empty
    ).

    % If we must copy the value in Lval somewhere else to prevent it from
    % being lost when Lval overwritten, then we try to put it into a location
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
:- pred free_up_lval_with_copy(module_info::in, lval::in, list(prog_var)::in,
    list(lval)::in, code_tree::out, var_locn_info::in, var_locn_info::out)
    is det.

free_up_lval_with_copy(ModuleInfo, Lval, ToBeAssignedVars, ForbiddenLvals,
        Code, !VLI) :-
    (
        get_loc_var_map(!.VLI, LocVarMap0),
        map__search(LocVarMap0, Lval, AffectedVarSet),
        set__delete_list(AffectedVarSet, ToBeAssignedVars, EffAffectedVarSet),
        set__to_sorted_list(EffAffectedVarSet, EffAffectedVars),

        get_var_state_map(!.VLI, VarStateMap0),
        (
            find_one_occupying_var(EffAffectedVars, Lval, VarStateMap0,
                OccupyingVar, OtherSources)
        ->
            MovedVar = OccupyingVar,
            list__delete_all(EffAffectedVars, MovedVar, OtherVars),
            list__foldl(ensure_copies_are_present(Lval, OtherSources),
                OtherVars, !VLI)
        ;
            EffAffectedVars = [MovedVar]
        ),

        CheckInUse = no,
        select_preferred_reg_or_stack(!.VLI, MovedVar, Pref, CheckInUse),
        \+ Pref = Lval,
        \+ list__member(Pref, ForbiddenLvals),
        ( \+ lval_in_use(!.VLI, Pref) ->
            true
        ;
            % The code generator assumes that values in stack slots don't get
            % clobbered without an explicit assignment (via a place_var
            % operation with a stack var as a target).
            Pref = reg(r, RegNum),
            reg_is_not_locked_for_var(!.VLI, RegNum, MovedVar)
        )
    ->
        actually_place_var(ModuleInfo, MovedVar, Pref, [Lval | ForbiddenLvals],
            Code, !VLI)
    ;
        get_spare_reg(!.VLI, Target),
        record_copy(Lval, Target, !VLI),
        (
            ( Lval = stackvar(N)
            ; Lval = framevar(N)
            ),
            N < 0
        ->
            % We must not copy from invalid lvals. The value we would copy
            % is a dummy in any case, so Target won't be any more valid
            % if we assigned Lval to it.
            Code = empty
        ;
            Code = node([
                assign(Target, lval(Lval)) - "Freeing up the source lval"
            ])
        )
    ).

    % Find a variable in the given list that is currently stored directly
    % in Lval (not just in some location who address includes Lval).
    %
:- pred find_one_occupying_var(list(prog_var)::in, lval::in,
    var_state_map::in, prog_var::out, list(lval)::out) is semidet.

find_one_occupying_var([Var | Vars], Lval, VarStateMap, OccupyingVar,
        OtherSources) :-
    map__lookup(VarStateMap, Var, State),
    State = state(LvalSet, _, _, _, _),
    ( set__member(Lval, LvalSet) ->
        OccupyingVar = Var,
        set__delete(LvalSet, Lval, OtherSourceSet),
        set__to_sorted_list(OtherSourceSet, OtherSources)
    ;
        find_one_occupying_var(Vars, Lval, VarStateMap, OccupyingVar,
            OtherSources)
    ).

:- pred ensure_copies_are_present(lval::in, list(lval)::in,
    prog_var::in, var_locn_info::in, var_locn_info::out) is det.

ensure_copies_are_present(OneSource, OtherSources, Var, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap0),
    map__lookup(VarStateMap0, Var, State0),
    State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval, Using,
        DeadOrAlive),
    set__to_sorted_list(LvalSet0, Lvals0),
    list__foldl(ensure_copies_are_present_lval(OtherSources, OneSource),
        Lvals0, LvalSet0, LvalSet),
    State = state(LvalSet, MaybeConstRval, MaybeExprRval, Using, DeadOrAlive),
    map__det_update(VarStateMap0, Var, State, VarStateMap),
    set_var_state_map(VarStateMap, !VLI),

    get_loc_var_map(!.VLI, LocVarMap0),
    record_change_in_root_dependencies(LvalSet0, LvalSet, Var,
        LocVarMap0, LocVarMap),
    set_loc_var_map(LocVarMap, !VLI).

:- pred ensure_copies_are_present_lval(list(lval)::in, lval::in,
    lval::in, set(lval)::in, set(lval)::out) is det.

ensure_copies_are_present_lval([], _, _, !LvalSet).
ensure_copies_are_present_lval([OtherSource | OtherSources], OneSource, Lval,
        !LvalSet) :-
    SubstLval = substitute_lval_in_lval(OneSource, OtherSource, Lval),
    set__insert(!.LvalSet, SubstLval, !:LvalSet),
    ensure_copies_are_present_lval(OtherSources, OneSource, Lval, !LvalSet).

%----------------------------------------------------------------------------%

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
    expect(is_root_lval(New), this_file, "record_copy: non-root New lval"),
    get_var_state_map(!.VLI, VarStateMap0),
    get_loc_var_map(!.VLI, LocVarMap0),
    set__list_to_set([Old, New], AssignSet),
    get_var_set_roots(AssignSet, NoDupRootLvals),
        % Convert the list of root lvals to the list of sets of
        % affected vars; if a root lval is not in LocVarMap0,
        % then it does not affect any variables.
    list__filter_map(map__search(LocVarMap0), NoDupRootLvals,
        AffectedVarSets),
        % Take the union of the list of sets of affected vars.
    list__foldl(set__union, AffectedVarSets,
        set__init, AffectedVarSet),
        % Convert the union set to a list of affected vars.
    set__to_sorted_list(AffectedVarSet, AffectedVars),
    list__foldl2(record_copy_for_var(Old, New), AffectedVars,
        VarStateMap0, VarStateMap, LocVarMap0, LocVarMap),
    set_loc_var_map(LocVarMap, !VLI),
    set_var_state_map(VarStateMap, !VLI).

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
    map__lookup(!.VarStateMap, Var, State0),
    State0 = state(LvalSet0, MaybeConstRval, MaybeExprRval,
        Using, DeadOrAlive),
    Token = reg(r, -42),
    LvalSet1 = set__map(substitute_lval_in_lval(Old, Token), LvalSet0),
    set__union(LvalSet0, LvalSet1, LvalSet2),
    LvalSet3 = set__filter(lval_does_not_support_lval(New), LvalSet2),
    LvalSet = set__map(substitute_lval_in_lval(Token, New), LvalSet3),
    State = state(LvalSet, MaybeConstRval, MaybeExprRval,
        Using, DeadOrAlive),
    expect(nonempty_state(State), this_file,
        "record_copy_for_var: empty state"),
    map__det_update(!.VarStateMap, Var, State, !:VarStateMap),
    record_change_in_root_dependencies(LvalSet0, LvalSet, Var, !LocVarMap).

:- pred record_change_in_root_dependencies(set(lval)::in,
    set(lval)::in, prog_var::in, loc_var_map::in, loc_var_map::out) is det.

record_change_in_root_dependencies(OldLvalSet, NewLvalSet, Var, !LocVarMap) :-
    get_var_set_roots(OldLvalSet, OldRootLvals),
    get_var_set_roots(NewLvalSet, NewRootLvals),
    set__list_to_set(OldRootLvals, OldRootLvalSet),
    set__list_to_set(NewRootLvals, NewRootLvalSet),
    set__difference(NewRootLvalSet, OldRootLvalSet, InsertSet),
    set__difference(OldRootLvalSet, NewRootLvalSet, DeleteSet),
    set__to_sorted_list(InsertSet, Inserts),
    set__to_sorted_list(DeleteSet, Deletes),
    list__foldl(make_var_depend_on_root_lval(Var), Inserts, !LocVarMap),
    list__foldl(make_var_not_depend_on_root_lval(Var), Deletes, !LocVarMap).

:- func substitute_lval_in_lval(lval, lval, lval) = lval.

substitute_lval_in_lval(Old, New, Lval0) = Lval :-
    exprn_aux__substitute_lval_in_lval(Old, New, Lval0, Lval).

%----------------------------------------------------------------------------%

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
    % we would like to insist on Var being alive (but don't (yet) due to bugs
    % in liveness).
    %
var_becomes_dead(Var, FirstTime, !VLI) :-
    get_var_state_map(!.VLI, VarStateMap0),
    ( map__search(VarStateMap0, Var, State0) ->
        State0 = state(Lvals, MaybeConstRval, MaybeExprRval, Using,
            DeadOrAlive0),
        (
            DeadOrAlive0 = dead,
            expect(unify(FirstTime, no), this_file,
                "var_becomes_dead: already dead")
        ;
            DeadOrAlive0 = alive
        ),
        ( set__empty(Using) ->
            map__det_remove(VarStateMap0, Var, _, VarStateMap),
            set_var_state_map(VarStateMap, !VLI),

            get_loc_var_map(!.VLI, LocVarMap0),
            get_var_set_roots(Lvals, NoDupRootLvals),
            list__foldl(make_var_not_depend_on_root_lval(Var),
                NoDupRootLvals, LocVarMap0, LocVarMap),
            set_loc_var_map(LocVarMap, !VLI),

            remove_use_refs(MaybeExprRval, Var, !VLI)
        ;
            State = state(Lvals, MaybeConstRval, MaybeExprRval, Using, dead),
            map__det_update(VarStateMap0, Var, State, VarStateMap),
            set_var_state_map(VarStateMap, !VLI)
        )
    ;
        expect(unify(FirstTime, no), this_file,
            "var_becomes_dead: premature deletion")
    ).

    % Given a set of lvals, return the set of root lvals among them and inside
    % them.
    %
:- pred get_var_set_roots(set(lval)::in, list(lval)::out) is det.

get_var_set_roots(Lvals, NoDupRootLvals) :-
    set__to_sorted_list(Lvals, LvalList),
    code_util__lvals_in_lvals(LvalList, ContainedLvals),
    list__append(LvalList, ContainedLvals, AllLvals),
    list__filter(is_root_lval, AllLvals, RootLvals),
    list__sort_and_remove_dups(RootLvals, NoDupRootLvals).

%----------------------------------------------------------------------------%

    % Select the cheapest way to refer to the value of the variable.
    % From the given list of lvals, select the cheapest one to use.
    %
:- pred select_lval(list(lval)::in, lval::out) is det.

select_lval(Lvals, Lval) :-
    ( select_reg_lval(Lvals, Lval1) ->
        Lval = Lval1
    ; select_stack_lval(Lvals, Lval2) ->
        Lval = Lval2
    ; select_cheapest_lval(Lvals, Lval3) ->
        Lval = Lval3
    ;
        unexpected(this_file, "select_lval: nothing to select")
    ).

    % From the given list of lvals and maybe a constant rval, select the
    % cheapest one to use.
    %
:- pred select_lval_or_rval(list(lval)::in, maybe(rval)::in, rval::out) is det.

select_lval_or_rval(Lvals, MaybeConstRval, Rval) :-
    ( maybe_select_lval_or_rval(Lvals, MaybeConstRval, Rval1) ->
        Rval = Rval1
    ;
        unexpected(this_file, "select_lval_or_rval: nothing to select")
    ).

:- pred maybe_select_lval_or_rval(list(lval)::in, maybe(rval)::in,
    rval::out) is semidet.

maybe_select_lval_or_rval(Lvals, MaybeConstRval, Rval) :-
    ( select_reg_lval(Lvals, Lval1) ->
        Rval = lval(Lval1)
    ; select_stack_lval(Lvals, Lval2) ->
        Rval = lval(Lval2)
    ; MaybeConstRval = yes(ConstRval) ->
        Rval = ConstRval
    ; select_cheapest_lval(Lvals, Lval3) ->
        Rval = lval(Lval3)
    ;
        fail
    ).

:- pred select_reg_lval(list(lval)::in, lval::out) is semidet.

select_reg_lval([Lval0 | Lvals0], Lval) :-
    ( Lval0 = reg(_, _) ->
        Lval = Lval0
    ;
        select_reg_lval(Lvals0, Lval)
    ).

:- pred select_stack_lval(list(lval)::in, lval::out) is semidet.

select_stack_lval([Lval0 | Lvals0], Lval) :-
    ( ( Lval0 = stackvar(_) ; Lval0 = framevar(_)) ->
        Lval = Lval0
    ;
        select_stack_lval(Lvals0, Lval)
    ).

:- pred select_reg_or_stack_lval(list(lval)::in, lval::out)
    is semidet.

select_reg_or_stack_lval([Lval0 | Lvals0], Lval) :-
    (
        ( Lval0 = reg(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = framevar(_)
        )
    ->
        Lval = Lval0
    ;
        select_reg_or_stack_lval(Lvals0, Lval)
    ).

    % From the given list of lvals, select the cheapest one to use.
    % Since none of the lvals will be a register or stack variable,
    % in almost all cases, the given list will be a singleton.
    %
:- pred select_cheapest_lval(list(lval)::in, lval::out) is semidet.

select_cheapest_lval([Lval | _], Lval).

%----------------------------------------------------------------------------%

:- pred select_preferred_reg_avoid(var_locn_info::in, prog_var::in,
    list(lval)::in, lval::out) is det.

select_preferred_reg_avoid(VLI, Var, Avoid, Lval) :-
    select_preferred_reg(VLI, Var, yes, Avoid, Lval).

:- pred select_preferred_reg(var_locn_info::in, prog_var::in,
    lval::out) is det.

select_preferred_reg(VLI, Var, Lval) :-
    select_preferred_reg(VLI, Var, yes, [], Lval).

    % Select the register into which Var should be put. If the follow_vars map
    % maps Var to a register, then select that register, unless it is already
    % in use, and CheckInUse = yes.
    %
:- pred select_preferred_reg(var_locn_info::in, prog_var::in,
    bool::in, list(lval)::in, lval::out) is det.

select_preferred_reg(VLI, Var, CheckInUse, Avoid, Lval) :-
    get_follow_var_map(VLI, FollowVarMap),
    (
        map__search(FollowVarMap, Var, PrefLocn),
        ( PrefLocn = abs_reg(_)
        ; PrefLocn = any_reg
        )
    ->
        (
            PrefLocn = abs_reg(N),
            PrefLval = reg(r, N),
            (
                CheckInUse = yes,
                \+ lval_in_use(VLI, PrefLval)
            ;
                CheckInUse = no
            ),
            \+ list__member(PrefLval, Avoid)
        ->
            Lval = PrefLval
        ;
            get_spare_reg_avoid(VLI, Avoid, Lval)
        )
    ;
        get_spare_reg_avoid(VLI, Avoid, Lval)
    ).

    % Select the register or stack slot into which Var should be put. If the
    % follow_vars map maps Var to a register, then select that register,
    % unless it is already in use and CheckInUse = yes. If the follow_vars map
    % does not contain Var, then Var is not needed in a register in the near
    % future, and this we select Var's stack slot, unless it is in use and
    % CheckInUse = yes. If all else fails, we get spare, unused register.
    % (Note that if the follow_vars pass has not been run, then all follow vars
    % maps will be empty, which would cause this predicate to try to put far
    % too many things in stack slots.)
    %
:- pred select_preferred_reg_or_stack_check(var_locn_info::in,
    prog_var::in, lval::out) is det.

select_preferred_reg_or_stack_check(VLI, Var, Lval) :-
    select_preferred_reg_or_stack(VLI, Var, Lval, yes).

:- pred select_preferred_reg_or_stack(var_locn_info::in,
    prog_var::in, lval::out, bool::in) is det.

select_preferred_reg_or_stack(VLI, Var, Lval, CheckInUse) :-
    get_follow_var_map(VLI, FollowVarMap),
    (
        map__search(FollowVarMap, Var, PrefLocn),
        ( PrefLocn = abs_reg(_)
        ; PrefLocn = any_reg
        )
    ->
        (
            PrefLocn = abs_reg(N),
            PrefLval = reg(r, N),
            (
                CheckInUse = yes,
                \+ lval_in_use(VLI, PrefLval)
            ;
                CheckInUse = no
            )
        ->
            Lval = PrefLval
        ;
            get_spare_reg(VLI, Lval)
        )
    ;
        (
            get_stack_slots(VLI, StackSlots),
            map__search(StackSlots, Var, StackSlotLocn),
            StackSlot = stack_slot_to_lval(StackSlotLocn),
            (
                CheckInUse = yes,
                \+ lval_in_use(VLI, StackSlot)
            ;
                CheckInUse = no
            )
        ->
            Lval = StackSlot
        ;
            get_spare_reg(VLI, Lval)
        )
    ).

:- pred real_lval(lval::in) is semidet.

real_lval(Lval) :-
    \+ (
        Lval = reg(_, N),
        N < 1
    ).

%----------------------------------------------------------------------------%

    % Get a register that is not in use. We start the search at the next
    % register that is needed for the next call.
    %
:- pred get_spare_reg_avoid(var_locn_info::in, list(lval)::in,
    lval::out) is det.

get_spare_reg_avoid(VLI, Avoid, Lval) :-
    get_next_non_reserved(VLI, NextNonReserved),
    get_spare_reg_2(VLI, Avoid, NextNonReserved, Lval).

:- pred get_spare_reg(var_locn_info::in, lval::out) is det.

get_spare_reg(VLI, Lval) :-
    get_next_non_reserved(VLI, NextNonReserved),
    get_spare_reg_2(VLI, [], NextNonReserved, Lval).

:- pred get_spare_reg_2(var_locn_info::in, list(lval)::in, int::in,
    lval::out) is det.

get_spare_reg_2(VLI, Avoid, N0, Lval) :-
    TryLval = reg(r, N0),
    ( lval_in_use(VLI, TryLval) ->
        get_spare_reg_2(VLI, Avoid, N0 + 1, Lval)
    ; list__member(TryLval, Avoid) ->
        get_spare_reg_2(VLI, Avoid, N0 + 1, Lval)
    ;
        Lval = TryLval
    ).

lval_in_use(VLI, Lval) :-
    get_loc_var_map(VLI, LocVarMap),
    get_acquired(VLI, Acquired),
    get_locked(VLI, Locked),
    (
        map__search(LocVarMap, Lval, UsingVars),
        \+ set__empty(UsingVars)
    ;
        set__member(Lval, Acquired)
    ;
        Lval = reg(r, N),
        N =< Locked
    ).

    % Succeeds if Var may be stored in Reg, possibly after copying its contents
    % somewhere else. This requires Reg to be either not locked, or if it is
    % locked, to be locked for Var.
    %
:- pred reg_is_not_locked_for_var(var_locn_info::in, int::in, prog_var::in)
    is semidet.

reg_is_not_locked_for_var(VLI, RegNum, Var) :-
    get_acquired(VLI, Acquired),
    get_locked(VLI, Locked),
    get_exceptions(VLI, Exceptions),
    Reg = reg(r, RegNum),
    \+ set__member(Reg, Acquired),
    RegNum =< Locked => list__member(Var - Reg, Exceptions).

%----------------------------------------------------------------------------%

acquire_reg(Lval, !VLI) :-
    get_spare_reg(!.VLI, Lval),
    get_acquired(!.VLI, Acquired0),
    set__insert(Acquired0, Lval, Acquired),
    set_acquired(Acquired, !VLI).

acquire_reg_require_given(Lval, !VLI) :-
    ( lval_in_use(!.VLI, Lval) ->
        unexpected(this_file, "acquire_reg_require_given: lval in use")
    ;
        true
    ),
    get_acquired(!.VLI, Acquired0),
    set__insert(Acquired0, Lval, Acquired),
    set_acquired(Acquired, !VLI).

acquire_reg_prefer_given(Pref, Lval, !VLI) :-
    PrefLval = reg(r, Pref),
    ( lval_in_use(!.VLI, PrefLval) ->
        get_spare_reg(!.VLI, Lval)
    ;
        Lval = PrefLval
    ),
    get_acquired(!.VLI, Acquired0),
    set__insert(Acquired0, Lval, Acquired),
    set_acquired(Acquired, !VLI).

acquire_reg_start_at_given(Start, Lval, !VLI) :-
    StartLval = reg(r, Start),
    ( lval_in_use(!.VLI, StartLval) ->
        acquire_reg_start_at_given(Start + 1, Lval, !VLI)
    ;
        Lval = StartLval,
        get_acquired(!.VLI, Acquired0),
        set__insert(Acquired0, Lval, Acquired),
        set_acquired(Acquired, !VLI)
    ).

release_reg(Lval, !VLI) :-
    get_acquired(!.VLI, Acquired0),
    ( set__member(Lval, Acquired0) ->
        set__delete(Acquired0, Lval, Acquired),
        set_acquired(Acquired, !VLI)
    ;
        unexpected(this_file, "release_reg: unacquired reg")
    ).

%----------------------------------------------------------------------------%

lock_regs(N, Exceptions, !VLI) :-
    set_locked(N, !VLI),
    set_exceptions(Exceptions, !VLI).

unlock_regs(!VLI) :-
    set_locked(0, !VLI),
    set_exceptions([], !VLI).

%----------------------------------------------------------------------------%

max_reg_in_use(VLI, Max) :-
    get_loc_var_map(VLI, LocVarMap),
    map__keys(LocVarMap, VarLocs),
    code_util__max_mentioned_reg(VarLocs, Max1),
    get_acquired(VLI, Acquired),
    set__to_sorted_list(Acquired, AcquiredList),
    code_util__max_mentioned_reg(AcquiredList, Max2),
    int__max(Max1, Max2, Max).

%----------------------------------------------------------------------------%

:- pred cell_is_constant(var_state_map::in, exprn_opts::in,
    list(maybe(rval))::in, assoc_list(rval, llds_type)::out) is semidet.

cell_is_constant(_VarStateMap, _ExprnOpts, [], []).
cell_is_constant(VarStateMap, ExprnOpts, [yes(Rval0) | MaybeRvals],
        [Rval - LldsType | RvalsTypes]) :-
    expr_is_constant(VarStateMap, ExprnOpts, Rval0, Rval),
    rval_type_as_arg(Rval, ExprnOpts, LldsType),
    cell_is_constant(VarStateMap, ExprnOpts, MaybeRvals, RvalsTypes).

    % expr_is_constant(VarStateMap, ExprnOpts, Rval0, Rval):
    % Check if Rval0 is a constant rval, after substituting the values of the
    % variables inside it. Returns the substituted, ground rval in Rval.
    % Note that this predicate is similar to code_exprn__expr_is_constant,
    % but it uses its own version of the variable state data structure.
    %
:- pred expr_is_constant(var_state_map::in, exprn_opts::in,
    rval::in, rval::out) is semidet.

expr_is_constant(_, ExprnOpts, const(Const), const(Const)) :-
    exprn_aux__const_is_constant(Const, ExprnOpts, yes).
expr_is_constant(VarStateMap, ExprnOpts,
        unop(Op, Expr0), unop(Op, Expr)) :-
    expr_is_constant(VarStateMap, ExprnOpts, Expr0, Expr).
expr_is_constant(VarStateMap, ExprnOpts,
        binop(Op, Expr1, Expr2), binop(Op, Expr3, Expr4)) :-
    expr_is_constant(VarStateMap, ExprnOpts, Expr1, Expr3),
    expr_is_constant(VarStateMap, ExprnOpts, Expr2, Expr4).
expr_is_constant(VarStateMap, ExprnOpts,
        mkword(Tag, Expr0), mkword(Tag, Expr)) :-
    expr_is_constant(VarStateMap, ExprnOpts, Expr0, Expr).
expr_is_constant(VarStateMap, ExprnOpts, var(Var), Rval) :-
    map__search(VarStateMap, Var, State),
    State = state(_, yes(Rval), _, _, _),
    expect(expr_is_constant(VarStateMap, ExprnOpts, Rval, _),
        this_file, "non-constant rval in variable state").

%----------------------------------------------------------------------------%

materialize_vars_in_lval(ModuleInfo, Lval0, Lval, Code, !VLI) :-
    materialize_vars_in_lval(ModuleInfo, Lval0, [], Lval, Code, !VLI).

:- pred materialize_vars_in_lval(module_info::in, lval::in, list(lval)::in,
    lval::out, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_lval(ModuleInfo, Lval0, Avoid, Lval, Code, !VLI) :-
    (
        Lval0 = reg(_, _),
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = stackvar(_),
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = framevar(_),
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = succip,
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = maxfr,
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = curfr,
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = hp,
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = sp,
        Lval = Lval0,
        Code = empty
    ;
        Lval0 = succip(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = succip(Rval)
    ;
        Lval0 = redoip(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = redoip(Rval)
    ;
        Lval0 = succfr(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = succfr(Rval)
    ;
        Lval0 = redofr(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = redofr(Rval)
    ;
        Lval0 = prevfr(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = prevfr(Rval)
    ;
        Lval0 = mem_ref(Rval0),
        materialize_vars_in_rval(ModuleInfo, Rval0, no, Avoid, Rval, Code,
            !VLI),
        Lval = mem_ref(Rval)
    ;
        Lval0 = field(Tag, RvalA0, RvalB0),
        materialize_vars_in_rval(ModuleInfo, RvalA0, no, Avoid, RvalA, CodeA,
            !VLI),
        materialize_vars_in_rval(ModuleInfo, RvalB0, no, Avoid, RvalB, CodeB,
            !VLI),
        Lval = field(Tag, RvalA, RvalB),
        Code = tree(CodeA, CodeB)
    ;
        Lval0 = temp(_, _),
        unexpected(this_file, "materialize_vars_in_lval: temp")
    ;
        Lval0 = lvar(_),
        unexpected(this_file, "materialize_vars_in_lval: lvar")
    ).

    % Rval is Rval0 with all variables in Rval0 replaced by their values.
    %
:- pred materialize_vars_in_rval(module_info::in, rval::in, maybe(lval)::in,
    list(lval)::in, rval::out, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_rval(ModuleInfo, Rval0, MaybePrefer, Avoid, Rval, Code,
        !VLI) :-
    (
        Rval0 = lval(Lval0),
        materialize_vars_in_lval(ModuleInfo, Lval0, Avoid, Lval, Code, !VLI),
        Rval = lval(Lval)
    ;
        Rval0 = mkword(Tag, SubRval0),
        materialize_vars_in_rval(ModuleInfo, SubRval0, no, Avoid, SubRval,
            Code, !VLI),
        Rval = mkword(Tag, SubRval)
    ;
        Rval0 = unop(Unop, SubRval0),
        materialize_vars_in_rval(ModuleInfo, SubRval0, no, Avoid,
            SubRval, Code, !VLI),
        Rval = unop(Unop, SubRval)
    ;
        Rval0 = binop(Binop, SubRvalA0, SubRvalB0),
        materialize_vars_in_rval(ModuleInfo, SubRvalA0, no, Avoid, SubRvalA,
            CodeA, !VLI),
        materialize_vars_in_rval(ModuleInfo, SubRvalB0, no, Avoid, SubRvalB,
            CodeB, !VLI),
        Rval = binop(Binop, SubRvalA, SubRvalB),
        Code = tree(CodeA, CodeB)
    ;
        Rval0 = const(_),
        Rval = Rval0,
        Code = empty
    ;
        Rval0 = mem_addr(MemRef0),
        materialize_vars_in_mem_ref(ModuleInfo, MemRef0, MemRef, Avoid, Code,
            !VLI),
        Rval = mem_addr(MemRef)
    ;
        Rval0 = var(Var),
        find_var_availability(!.VLI, Var, MaybePrefer, Avail),
        (
            Avail = available(Rval),
            Code = empty
        ;
            Avail = needs_materialization,
            materialize_var(ModuleInfo, Var, MaybePrefer, yes, Avoid, Rval,
                Code, !VLI)
        )
    ).

    % MemRef is MemRef0 with all variables in MemRef replaced by their values.
    %
:- pred materialize_vars_in_mem_ref(module_info::in, mem_ref::in, mem_ref::out,
    list(lval)::in, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_vars_in_mem_ref(ModuleInfo, MemRef0, MemRef, Avoid, Code, !VLI) :-
    (
        MemRef0 = stackvar_ref(_),
        MemRef = MemRef0,
        Code = empty
    ;
        MemRef0 = framevar_ref(_),
        MemRef = MemRef0,
        Code = empty
    ;
        MemRef0 = heap_ref(PtrRval0, Ptag, FieldNum),
        materialize_vars_in_rval(ModuleInfo, PtrRval0, no, Avoid, PtrRval,
            Code, !VLI),
        MemRef = heap_ref(PtrRval, Ptag, FieldNum)
    ).

:- type var_avail
    --->    available(rval)
    ;       needs_materialization.

:- pred find_var_availability(var_locn_info::in, prog_var::in,
    maybe(lval)::in, var_avail::out) is det.

find_var_availability(VLI, Var, MaybePrefer, Avail) :-
    get_var_state_map(VLI, VarStateMap),
    map__lookup(VarStateMap, Var, State),
    State = state(Lvals, MaybeConstRval, _, _, _),
    set__to_sorted_list(Lvals, LvalsList),
    (
        MaybePrefer = yes(Prefer),
        list__member(Prefer, LvalsList)
    ->
        Rval = lval(Prefer),
        Avail = available(Rval)
    ;
        maybe_select_lval_or_rval(LvalsList, MaybeConstRval, Rval)
    ->
        Avail = available(Rval)
    ;
        Avail = needs_materialization
    ).

:- pred materialize_var(module_info::in, prog_var::in, maybe(lval)::in,
    bool::in, list(lval)::in, rval::out, code_tree::out,
    var_locn_info::in, var_locn_info::out) is det.

materialize_var(ModuleInfo, Var, MaybePrefer, StoreIfReq, Avoid, Rval, Code,
        !VLI) :-
    get_var_state_map(!.VLI, VarStateMap),
    map__lookup(VarStateMap, Var, State),
    State = state(_Lvals, _MaybeConstRval, MaybeExprRval, UsingVars,
        _DeadOrAlive),
    (
        MaybeExprRval = yes(ExprRval)
    ;
        MaybeExprRval = no,
        unexpected(this_file, "materialize_var: no expr")
    ),
    materialize_vars_in_rval(ModuleInfo, ExprRval, MaybePrefer, Avoid, Rval0,
        ExprCode, !VLI),
    (
        StoreIfReq = yes,
        set__count(UsingVars, NumUsingVars),
        NumUsingVars > 1
    ->
        select_preferred_reg_avoid(!.VLI, Var, Avoid, Lval),
        place_var(ModuleInfo, Var, Lval, PlaceCode, !VLI),
        Rval = lval(Lval),
        Code = tree(ExprCode, PlaceCode)
    ;
        Rval = Rval0,
        Code = ExprCode
    ).

%----------------------------------------------------------------------------%

    % Update LocVarMap0 to reflect the dependence of Var on all the root lvals
    % among Lvals or contained inside Lvals.
    %
:- pred make_var_depend_on_lvals_roots(prog_var::in,
    set(lval)::in, loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_lvals_roots(Var, Lvals, !LocVarMap) :-
    get_var_set_roots(Lvals, NoDupRootLvals),
    list__foldl(make_var_depend_on_root_lval(Var),
        NoDupRootLvals, !LocVarMap).

:- pred make_var_depend_on_lval_roots(prog_var::in,
    lval::in, loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_lval_roots(Var, Lval, !LocVarMap) :-
    set__singleton_set(Lvals, Lval),
    make_var_depend_on_lvals_roots(Var, Lvals, !LocVarMap).

:- pred make_var_depend_on_root_lval(prog_var::in, lval::in,
    loc_var_map::in, loc_var_map::out) is det.

make_var_depend_on_root_lval(Var, Lval, !LocVarMap) :-
    expect(is_root_lval(Lval),
        this_file, "make_var_depend_on_root_lval: non-root lval"),
    ( map__search(!.LocVarMap, Lval, Vars0) ->
        set__insert(Vars0, Var, Vars),
        map__det_update(!.LocVarMap, Lval, Vars, !:LocVarMap)
    ;
        set__singleton_set(Vars, Var),
        map__det_insert(!.LocVarMap, Lval, Vars, !:LocVarMap)
    ).

    % Update LocVarMap0 to reflect that Var is no longer dependent
    % on the root lval Lval.
    %
:- pred make_var_not_depend_on_root_lval(prog_var::in, lval::in,
    loc_var_map::in, loc_var_map::out) is det.

make_var_not_depend_on_root_lval(Var, Lval, !LocVarMap) :-
    expect(is_root_lval(Lval), this_file,
        "make_var_depend_on_root_lval: non-root lval"),
    ( map__search(!.LocVarMap, Lval, Vars0) ->
        set__delete(Vars0, Var, Vars),
        ( set__empty(Vars) ->
            map__det_remove(!.LocVarMap, Lval, _, !:LocVarMap)
        ;
            map__det_update(!.LocVarMap, Lval, Vars, !:LocVarMap)
        )
    ;
        unexpected(this_file, "make_var_not_depend_on_root_lval: no record")
    ).

:- pred is_root_lval(lval::in) is semidet.

is_root_lval(reg(r, _)).
is_root_lval(stackvar(_)).
is_root_lval(framevar(_)).

%----------------------------------------------------------------------------%

:- type dep_search_lval
    --->    all_regs
    ;       specific_reg_or_stack(lval).

:- pred lval_does_not_support_lval(lval::in, lval::in) is semidet.

lval_does_not_support_lval(Lval1, Lval2) :-
    \+ lval_depends_on_search_lval(Lval2, specific_reg_or_stack(Lval1)).

:- pred rval_depends_on_search_lval(rval::in, dep_search_lval::in) is semidet.

rval_depends_on_search_lval(lval(Lval), SearchLval) :-
    lval_depends_on_search_lval(Lval, SearchLval).
rval_depends_on_search_lval(var(_Var), _SearchLval) :-
    unexpected(this_file, "rval_depends_on_search_lval: var").
rval_depends_on_search_lval(mkword(_Tag, Rval), SearchLval) :-
    rval_depends_on_search_lval(Rval, SearchLval).
rval_depends_on_search_lval(const(_Const), _SearchLval) :-
    fail.
rval_depends_on_search_lval(unop(_Op, Rval), SearchLval) :-
    rval_depends_on_search_lval(Rval, SearchLval).
rval_depends_on_search_lval(binop(_Op, Rval0, Rval1), SearchLval) :-
    (
        rval_depends_on_search_lval(Rval0, SearchLval)
    ;
        rval_depends_on_search_lval(Rval1, SearchLval)
    ).

:- pred lval_depends_on_search_lval(lval::in, dep_search_lval::in) is semidet.

lval_depends_on_search_lval(reg(Type, Num), SearchLval) :-
    (
        SearchLval = all_regs
    ;
        SearchLval = specific_reg_or_stack(Lval),
        Lval = reg(Type, Num)
    ).
lval_depends_on_search_lval(stackvar(Num), SearchLval) :-
    SearchLval = specific_reg_or_stack(Lval),
    Lval = stackvar(Num).
lval_depends_on_search_lval(framevar(Num), SearchLval) :-
    SearchLval = specific_reg_or_stack(Lval),
    Lval = framevar(Num).
lval_depends_on_search_lval(lvar(_Var), _SearchLval) :-
    unexpected(this_file, "lval_depends_on_search_lval: lvar").
lval_depends_on_search_lval(field(_Tag, Rval0, Rval1), SearchLval) :-
    (
        rval_depends_on_search_lval(Rval0, SearchLval)
    ;
        rval_depends_on_search_lval(Rval1, SearchLval)
    ).

:- pred args_depend_on_search_lval(list(maybe(rval))::in, dep_search_lval::in)
    is semidet.

args_depend_on_search_lval([], _SearchLval) :-
    fail.
args_depend_on_search_lval([Arg | Args], SearchLval) :-
    (
        Arg = yes(Rval),
        rval_depends_on_search_lval(Rval, SearchLval)
    ;
        args_depend_on_search_lval(Args, SearchLval)
    ).

%----------------------------------------------------------------------------%

set_follow_vars(abs_follow_vars(FollowVarMap, NextNonReserved), !VLI) :-
    set_follow_var_map(FollowVarMap, !VLI),
    set_next_non_reserved(NextNonReserved, !VLI).

%----------------------------------------------------------------------------%

:- pred get_var_name(var_locn_info::in, prog_var::in, string::out) is det.

get_var_name(VLI, Var, Name) :-
    get_varset(VLI, VarSet),
    varset__lookup_name(VarSet, Var, Name).

%----------------------------------------------------------------------------%

:- pred nonempty_state(var_state::in) is semidet.

nonempty_state(State) :-
    State = state(LvalSet, MaybeConstRval, MaybeExprRval, _, _),
    ( set__non_empty(LvalSet)
    ; MaybeConstRval = yes(_)
    ; MaybeExprRval = yes(_)
    ).

%----------------------------------------------------------------------------%

:- pred get_varset(var_locn_info::in, prog_varset::out) is det.
:- pred get_vartypes(var_locn_info::in, vartypes::out) is det.
:- pred get_exprn_opts(var_locn_info::in, exprn_opts::out) is det.
:- pred get_var_state_map(var_locn_info::in, var_state_map::out) is det.
:- pred get_loc_var_map(var_locn_info::in, loc_var_map::out) is det.
:- pred get_acquired(var_locn_info::in, set(lval)::out) is det.
:- pred get_locked(var_locn_info::in, int::out) is det.
:- pred get_exceptions(var_locn_info::in, assoc_list(prog_var, lval)::out)
    is det.

:- pred set_follow_var_map(abs_follow_vars_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_next_non_reserved(int::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_var_state_map(var_state_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_loc_var_map(loc_var_map::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_acquired(set(lval)::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_locked(int::in,
    var_locn_info::in, var_locn_info::out) is det.
:- pred set_exceptions(assoc_list(prog_var, lval)::in,
    var_locn_info::in, var_locn_info::out) is det.

get_varset(VI, VI ^ varset).
get_vartypes(VI, VI ^ vartypes).
get_stack_slots(VI, VI ^ stack_slots).
get_exprn_opts(VI, VI ^ exprn_opts).
get_follow_var_map(VI, VI ^ follow_vars_map).
get_next_non_reserved(VI, VI ^ next_non_res).
get_var_state_map(VI, VI ^ var_state_map).
get_loc_var_map(VI, VI ^ loc_var_map).
get_acquired(VI, VI ^ acquired).
get_locked(VI, VI ^ locked).
get_exceptions(VI, VI ^ exceptions).

set_follow_var_map(FVM, VI, VI ^ follow_vars_map := FVM).
set_next_non_reserved(NNR, VI, VI ^ next_non_res := NNR).
set_var_state_map(VSM, VI, VI ^ var_state_map := VSM).
set_loc_var_map(LVM, VI, VI ^ loc_var_map := LVM).
set_acquired(A, VI, VI ^ acquired := A).
set_locked(L, VI, VI ^ locked := L).
set_exceptions(E, VI, VI ^ exceptions := E).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "var_locn.m".

%----------------------------------------------------------------------------%
:- end_module var_locn.
%----------------------------------------------------------------------------%
