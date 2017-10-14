%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with predicates
% and procedures.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_pred.
:- interface.

:- import_module analysis.
:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_graph.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module libs.options.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- interface.

    % A proc_id is the name of a mode within a particular predicate -
    % not to be confused with a mode_id, which is the name of a
    % user-defined mode.

:- type pred_id.
:- type proc_id.
:- type pred_proc_id
    --->    proc(pred_id, proc_id).

:- func pred_proc_id_project_pred_id(pred_proc_id) = pred_id.
:- func pred_proc_id_project_proc_id(pred_proc_id) = proc_id.

    % Several passes operate on the module one SCC at a time. An SCC is
    % a strongly connected component of the call graph, i.e. a group of
    % procedures that all recursively call each other, directly or indirectly,
    % which aren't mutually recursive with any procedure outside the SCC.
:- type scc == set(pred_proc_id).

    % Predicate and procedure ids are abstract data types. One important
    % advantage of this arrangement is to make it harder to accidentally
    % confuse them for each other, or to use an integer in their place.
    % However, you can convert between integers and pred_ids/proc_ids
    % with the following predicates and functions.
    %
:- func shroud_pred_id(pred_id) = shrouded_pred_id.
:- func shroud_proc_id(proc_id) = shrouded_proc_id.
:- func shroud_pred_proc_id(pred_proc_id) = shrouded_pred_proc_id.

:- func unshroud_pred_id(shrouded_pred_id) = pred_id.
:- func unshroud_proc_id(shrouded_proc_id) = proc_id.
:- func unshroud_pred_proc_id(shrouded_pred_proc_id) = pred_proc_id.

:- pred pred_id_to_int(pred_id, int).
:- mode pred_id_to_int(in, out) is det.
:- mode pred_id_to_int(out, in) is det.
:- func pred_id_to_int(pred_id) = int.

:- pred proc_id_to_int(proc_id, int).
:- mode proc_id_to_int(in, out) is det.
:- mode proc_id_to_int(out, in) is det.
:- func proc_id_to_int(proc_id) = int.

    % Return the id of the first predicate in a module, and of the first
    % procedure in a predicate.
    %
:- func initial_pred_id = pred_id.
:- func initial_proc_id = proc_id.

    % Return an invalid predicate or procedure id. These are intended to be
    % used to initialize the relevant fields in call(...) goals before
    % we do type- and mode-checks, or when those checks find that there was
    % no predicate matching the call.
    %
:- func invalid_pred_id = pred_id.
:- func invalid_proc_id = proc_id.

:- pred next_pred_id(pred_id::in, pred_id::out) is det.

    % For semidet complicated unifications with mode (in, in), these are
    % defined to have the same proc_id (0). This returns that proc_id.
    %
:- pred in_in_unification_proc_id(proc_id::out) is det.

:- type pred_info.
:- type proc_info.

:- type proc_table == map(proc_id, proc_info).

:- pred next_mode_id(proc_table::in, proc_id::out) is det.

:- type call_id
    --->    plain_call_id(simple_call_id)
    ;       generic_call_id(generic_call_id).

:- type generic_call_id
    --->    gcid_higher_order(purity, pred_or_func, arity)
    ;       gcid_class_method(class_id, simple_call_id)
    ;       gcid_event_call(string)
    ;       gcid_cast(cast_kind).

:- type pred_proc_list == list(pred_proc_id).

%-----------------------------------------------------------------------------%

:- type implementation_language
    --->    impl_lang_mercury
    ;       impl_lang_foreign(foreign_language).

    % The type of goals that have been given for a pred.

:- type goal_type
    --->    goal_type_clause
    ;       goal_type_foreign               % pragma foreign_proc(...)
    ;       goal_type_clause_and_foreign    % both clauses and foreign_procs
    ;       goal_type_promise(promise_type)
    ;       goal_type_none.

    % NOTE: `liveness_info' records liveness in the sense used by code
    % generation. This is *not* the same thing as the notion of liveness
    % used by mode analysis!  See compiler/notes/glossary.html.
    %
:- type liveness_info == set_of_progvar.    % The live variables.

:- type arg_info
    --->    arg_info(
                arg_loc,                    % Stored location.
                top_functor_mode            % Mode of top functor.
            ).

    % The top_functor_mode specifies the mode of the top-level functor
    % of a term (excluding `no_tag' functors, since those have no
    % representation). It is used by the code generators when determining
    % how to pass the argument.
    %
    % For the LLDS back-end, top_in arguments are passed in registers,
    % and top_out values are returned in registers; top_unused values
    % are not passed at all, but they are treated as if they were top_out
    % for the purpose of assigning arguments to registers. (So e.g. if
    % a det procedure has three arguments with top_functor_modes top_out,
    % top_unused, and top_out respectively, the last argument will be
    % returned in register r3, not r2.)
    %
    % For the MLDS back-end, top_in values are passed as arguments.
    % Top_out values are normally passed by reference, except that
    %   - if the procedure is model_nondet, and the --nondet-copy-out option
    %     is set, top_out values are passed by value to the continuation
    %     function;
    %   - if the procedure is model_det or model_semi, and the
    %     --det-copy-out option is set, top_out arguments in the HLDS
    %     are mapped to (multiple) return values in the MLDS; and
    %   - if the HLDS function return value for a det function has mode
    %     `top_out', it is mapped to an MLDS return value.
    % top_unused arguments are not passed at all.
    %
:- type top_functor_mode
    --->    top_in
    ;       top_out
    ;       top_unused.

:- type arg_loc
    --->    reg(reg_type, int).

    % Are calls from a predicate with the given import_status always fully
    % qualified. For calls occurring in `.opt' files this will return
    % `is_fully_qualified', otherwise `may_be_partially_qualified'.
    % XXX Obsolete documentation.
    %
:- func calls_are_fully_qualified(pred_markers) = is_fully_qualified.

    % Predicates can be marked with various boolean flags, called "markers".

    % A set of pred_markers.
:- type pred_markers == set(pred_marker).

:- type pred_marker
    --->    marker_stub
            % The predicate has no clauses. typecheck.m will generate a body
            % for the predicate which just throws an exception. This marker
            % is used to tell purity analysis and determinism analysis
            % not to issue warnings for these predicates.

    ;       marker_builtin_stub
            % This predicate is a builtin but has no clauses for whatever
            % reason. typecheck.m should generate a stub clause for it but no
            % warn about it.

    ;       marker_infer_type
            % Requests type inference for the predicate. These markers are
            % inserted by make_hlds for undeclared predicates.

    ;       marker_infer_modes
            % Requests mode inference for the predicate. These markers are
            % inserted by make_hlds for undeclared predicates.

    ;       marker_no_pred_decl
            % This predicate had no (valid) `:- pred' or `:- func' declaration.
            % Since we have generated an error message about this, suppress
            % the generation of any similar messages about missing mode
            % declarations, since the missing (or invalid) declaration
            % could have been a combined predmode declaration.

    ;       marker_obsolete
            % Requests warnings if this predicate is used.
            % Used for pragma(obsolete).

    ;       marker_no_detism_warning
            % Requests no warnings about the determinism of this predicate
            % being too loose.
            % Used for pragma(no_determinism_warning).

    ;       marker_user_marked_inline
            % The user requests that this be predicate should be inlined,
            % even if it exceeds the usual size limits. Used for
            % pragma(inline). Mutually exclusive with
            % marker_user_marked_no_inline.

    ;       marker_user_marked_no_inline
            % The user requests that this be predicate should not be inlined.
            % Used for pragma(no_inline). Mutually exclusive with
            % marker_user_marked_inline.

    ;       marker_heuristic_inline
            % The compiler (meaning probably inlining.m) requests that this
            % predicate be inlined. Does not override
            % marker_user_marked_no_inline.

    ;       marker_consider_used
            % The user has requested that this predicate be considered used
            % when we consider which procedures are dead, so we can generate
            % dead procedure warnings for them. If this marker is present
            % on a predicate, then neither the procedures of this predicate
            % nor the other procedures they call, directly or indirectly,
            % should get dead procedure warnings.

    ;       marker_class_method
            % Requests that this predicate be transformed into the appropriate
            % call to a class method.

    ;       marker_class_instance_method
            % This predicate was automatically generated for the implementation
            % of a class method for an instance.

    ;       marker_named_class_instance_method
            % This predicate was automatically generated for the implementation
            % of a class method for an instance, and the instance was defined
            % using the named syntax (e.g. "pred(...) is ...") rather than
            % the clause syntax. (For such predicates, we output slightly
            % different error messages.)

    ;       marker_is_impure
            % Requests that no transformation that would be inappropriate for
            % impure code be performed on calls to this predicate. This
            % includes reordering calls to it relative to other goals
            % (in both conjunctions and disjunctions), and removing
            % redundant calls to it.

    ;       marker_is_semipure
            % Requests that no transformation that would be inappropriate
            % for semipure code be performed on calls to this predicate.
            % This includes removing redundant calls to it on different sides
            % of an impure goal.

    ;       marker_promised_pure
            % Requests that calls to this predicate be transformed as usual,
            % despite any impure or semipure markers present.

    ;       marker_promised_semipure
            % Requests that calls to this predicate be treated as semipure,
            % despite any impure calls in the body.

    ;       marker_promised_equivalent_clauses
            % Promises that all modes of the predicate have equivalent
            % semantics, event if they are defined by different sets of
            % mode-specific clauses.

    % The terminates and does_not_terminate pragmas are kept as markers
    % to ensure that conflicting declarations are not made by the user.
    % Otherwise, the information could be added to the ProcInfos directly.

    ;       marker_terminates
            % The user guarantees that this predicate will terminate
            % for all (finite?) input.

    ;       marker_does_not_terminate
            % States that this predicate does not terminate. This is useful
            % for pragma foreign_code, which the compiler assumes to be
            % terminating.

    ;       marker_check_termination
            % The user requires the compiler to guarantee the termination
            % of this predicate. If the compiler cannot guarantee termination
            % then it must give an error message.

    ;       marker_calls_are_fully_qualified
            % All calls in this predicate are fully qualified. This occurs for
            % predicates read from `.opt' files and compiler-generated
            % predicates.

    ;       marker_mode_check_clauses
            % Each clause of the predicate should be modechecked separately.
            % Used for predicates defined by lots of clauses (usually facts)
            % for which the compiler's quadratic behavior during mode checking
            % (in inst_match.bound_inst_list_contains_instname and
            % instmap.merge) would be unacceptable.

    ;       marker_mutable_access_pred
            % This predicate is part of the machinery used to access mutables.
            % This marker is used to inform inlining that we should _always_
            % attempt to inline this predicate across module boundaries.

    ;       marker_has_require_scope
            % The body of this predicate contains a require_complete_switch
            % or require_detism scope. This marker is set if applicable during
            % determinism inference. It is used during determinism reporting:
            % procedures in predicates that have this marker are checked
            % for violations of the requirements of these scopes even if
            % the overall determinism of the procedure body is correct.

    ;       marker_has_incomplete_switch
            % The body of this predicate contains an incomplete switch
            % (one for which the switched-on variable may have a value
            % that does not match any of the cases). This marker is set
            % if applicable during determinism inference. It is used during
            % determinism reporting: if the inform_incomplete_switch option
            % is set, then procedures in predicates that have this marker
            % are traversed again to generate informational messages about
            % these incomplete switches, even if the overall determinism
            % of the procedure body is correct.

    ;       marker_has_format_call.
            % The body of this predicate contains calls to predicates
            % recognized by format_call.is_format_call. This marker is set
            % if applicable during typechecking, when the predicate body
            % has to be traversed anyway. It is used by later passes
            % that optimize correct format calls and/or warn about incorrect
            % (or at least not verifiably correct) format calls, which
            % would be no-ops on predicates that do not have this marker.

:- type pred_transformation
    --->    transform_higher_order_specialization(
                % Sequence number among the higher order specializations
                % of the original predicate.
                int
            )
    ;       transform_higher_order_type_specialization(
                % The procedure number of the original procedure.
                int
            )
    ;       transform_type_specialization(
                % The substitution from type variables (represented by
                % the integers) to types (represented by the terms).
                assoc_list(int, mer_type)
            )
    ;       transform_unused_argument_elimination(
                % The list of eliminated argument numbers.
                list(int)
            )
    ;       transform_accumulator(
                % The list of the numbers of the variables in the original
                % predicate interface that have been converted to accumulators.
                list(int)
            )
    ;       transform_loop_invariant(
                % The procedure number of the original procedure.
                int
            )
    ;       transform_tuple(
                % The procedure number of the original procedure.
                int
            )
    ;       transform_untuple(
                % The procedure number of the original procedure.
                int
            )
    ;       transform_dependent_parallel_conjunction
    ;       transform_parallel_loop_control
    ;       transform_return_via_ptr(
                % The id of the procedure this predicate is derived from.
                proc_id,
                % The arguments in these positions are returned via pointer.
                list(int)
            )
    ;       transform_table_generator
    ;       transform_stm_expansion
    ;       transform_dnf(
                % This predicate was originally part of a predicate
                % transformed into disjunctive normal form; this integer
                % gives the part number.
                int
            )
    ;       transform_structure_reuse
    ;       transform_source_to_source_debug.

:- type pred_creation
    --->    created_by_deforestation
    ;       created_by_io_tabling.
            % I/O tabling will create a new predicate if the predicate
            % to be I/O tabled must not be inlined.

:- type pred_origin
    --->    origin_special_pred(special_pred_id, type_ctor)
            % If the predicate is a unify, compare, index or initialisation
            % predicate, specify which one, and for which type constructor.

    ;       origin_instance_method(sym_name, instance_method_constraints)
            % The predicate is a class method implementation. Record
            % the method name and extra information about the class
            % context to allow polymorphism.m to correctly set up the
            % extra type_info and typeclass_info arguments.

    ;       origin_transformed(pred_transformation, pred_origin, pred_id)
            % The predicate is a transformed version of another predicate,
            % whose origin and identity are given by the second and third
            % arguments.

    ;       origin_created(pred_creation)
            % The predicate was created by the compiler, and there is no
            % information available on where it came from.

    ;       origin_assertion(string, int)
            % The predicate represents an assertion.

    ;       origin_lambda(string, int, int)
            % The predicate is a higher-order manifest constant.
            % The arguments specify its location in the source, as a
            % filename/line number pair, and a sequence number used to
            % distinguish multiple lambdas on the same line.

    ;       origin_solver_type(sym_name, arity, solver_type_pred_kind)
            % The predicate is a representation change predicate
            % either to or from either ground or any, as indicated by
            % the solver_type_pred_kind, for the type constructor given by
            % the sym_name and arity.

    ;       origin_tabling(simple_call_id, tabling_aux_pred_kind)
            % The predicate is an auxiliary predicate of the indicated kind
            % for the tabled predicate identified by the simple_call_id.

    ;       origin_mutable(module_name, string, mutable_pred_kind)
            % The predicate is a predicate that operates on the mutable
            % with the given name in the given module. The last argument
            % says whether the predicate is an init, pre_init, get, set,
            % lock, or unlock predicate on that mutable.

    ;       origin_user(sym_name).
            % The predicate is a normal user-written predicate;
            % the string is its name.

:- type tabling_aux_pred_kind
    --->    tabling_aux_pred_stats
    ;       tabling_aux_pred_reset.

:- type solver_type_pred_kind
    --->    solver_type_to_ground_pred
    ;       solver_type_to_any_pred
    ;       solver_type_from_ground_pred
    ;       solver_type_from_any_pred.

:- type mutable_pred_kind
    --->    mutable_pred_std_get
    ;       mutable_pred_std_set
    ;       mutable_pred_io_get
    ;       mutable_pred_io_set
    ;       mutable_pred_unsafe_get
    ;       mutable_pred_unsafe_set
    ;       mutable_pred_constant_get
    ;       mutable_pred_constant_secret_set
    ;       mutable_pred_lock
    ;       mutable_pred_unlock
    ;       mutable_pred_pre_init
    ;       mutable_pred_init.

:- type need_to_requantify
    --->    need_to_requantify
    ;       do_not_need_to_requantify.

    % This type is isomorphic to the module_section type, but defining it here
    % allows us not to depend on parse_tree.prog_item.m.
:- type decl_section
    --->    decl_interface
    ;       decl_implementation.

:- type maybe_predmode_decl
    --->    no_predmode_decl
    ;       predmode_decl.

:- type cur_user_decl_info
    --->    cur_user_decl_info(
                decl_section,
                maybe_predmode_decl,
                int                         % The item number.
            ).

    % pred_info_init(ModuleName, SymName, Arity, PredOrFunc, Context,
    %   Origin, Status, CurUserDecl, GoalType, Markers, ArgTypes, TypeVarSet,
    %   ExistQVars, ClassContext, ClassProofs, ClassConstraintMap,
    %   User, ClausesInfo, VarNameRemap, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults.
    %
:- pred pred_info_init(module_name::in, sym_name::in, arity::in,
    pred_or_func::in, prog_context::in, pred_origin::in, pred_status::in,
    maybe(cur_user_decl_info)::in, goal_type::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in,
    prog_constraints::in, constraint_proof_map::in, constraint_map::in,
    clauses_info::in, map(prog_var, string)::in, pred_info::out) is det.

    % pred_info_create(ModuleName, SymName, PredOrFunc, Context, Origin,
    %   Status, Markers, ArgTypes, TypeVarSet, ExistQVars,
    %   ClassContext, Assertions, VarNameRemap, ProcInfo, ProcId,
    %   PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults. The given
    % proc_info becomes the only procedure of the predicate (currently)
    % and its proc_id is returned as the second last argument.
    %
:- pred pred_info_create(module_name::in, sym_name::in, pred_or_func::in,
    prog_context::in, pred_origin::in, pred_status::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in, prog_constraints::in,
    set(assert_id)::in, map(prog_var, string)::in, proc_info::in,
    proc_id::out, pred_info::out) is det.

    % define_new_pred(Origin, Goal, CallGoal, Args, ExtraArgs,
    %   InstMap, PredName, TVarSet, VarTypes, ClassContext,
    %   TVarMap, TCVarMap, VarSet, Markers, IsAddressTaken, VarNameRemap,
    %   !ModuleInfo, PredProcId)
    %
    % Create a new predicate for the given goal, returning a goal to
    % call the created predicate. ExtraArgs is the list of extra
    % type_infos and typeclass_infos required by typeinfo liveness
    % which were added to the front of the argument list.
    %
:- pred define_new_pred(pred_origin::in,
    hlds_goal::in, hlds_goal::out, list(prog_var)::in, list(prog_var)::out,
    instmap::in, sym_name::in, tvarset::in, vartypes::in,
    prog_constraints::in, rtti_varmaps::in, prog_varset::in,
    inst_varset::in, pred_markers::in, is_address_taken::in,
    has_parallel_conj::in, map(prog_var, string)::in,
    module_info::in, module_info::out, pred_proc_id::out) is det.

    % Various predicates for accessing the information stored in the
    % pred_id and pred_info data structures.
    %
:- type external_type_params == list(tvar).

:- func pred_info_module(pred_info) =  module_name.
:- func pred_info_name(pred_info) = string.

    % Pred_info_orig_arity returns the arity of the predicate
    % *not* counting inserted type_info arguments for polymorphic preds.
    %
:- func pred_info_orig_arity(pred_info) = arity.

    % N-ary functions are converted into N+1-ary predicates.
    % (Clauses are converted in make_hlds, but calls to functions
    % cannot be converted until after type-checking, once we have
    % resolved overloading. So we do that during mode analysis.)
    % The `is_pred_or_func' field of the pred_info records whether
    % a pred_info is really for a predicate or whether it is for
    % what was originally a function.
    %
:- func pred_info_is_pred_or_func(pred_info) = pred_or_func.

:- pred pred_info_get_module_name(pred_info::in,
    module_name::out) is det.
:- pred pred_info_get_name(pred_info::in,
    string::out) is det.
:- pred pred_info_get_orig_arity(pred_info::in,
    arity::out) is det.
:- pred pred_info_get_is_pred_or_func(pred_info::in,
    pred_or_func::out) is det.
:- pred pred_info_get_context(pred_info::in,
    prog_context::out) is det.
:- pred pred_info_get_cur_user_decl_info(pred_info::in,
    maybe(cur_user_decl_info)::out) is det.
:- pred pred_info_get_origin(pred_info::in,
    pred_origin::out) is det.
:- pred pred_info_get_status(pred_info::in,
    pred_status::out) is det.
:- pred pred_info_get_goal_type(pred_info::in,
    goal_type::out) is det.
:- pred pred_info_get_markers(pred_info::in,
    pred_markers::out) is det.
:- pred pred_info_get_arg_types(pred_info::in,
    list(mer_type)::out) is det.
:- pred pred_info_get_typevarset(pred_info::in,
    tvarset::out) is det.
:- pred pred_info_get_tvar_kind_map(pred_info::in,
    tvar_kind_map::out) is det.
:- pred pred_info_get_exist_quant_tvars(pred_info::in,
    existq_tvars::out) is det.
:- pred pred_info_get_existq_tvar_binding(pred_info::in,
    tsubst::out) is det.
:- pred pred_info_get_external_type_params(pred_info::in,
    external_type_params::out) is det.
:- pred pred_info_get_class_context(pred_info::in,
    prog_constraints::out) is det.
:- pred pred_info_get_constraint_proof_map(pred_info::in,
    constraint_proof_map::out) is det.
:- pred pred_info_get_constraint_map(pred_info::in,
    constraint_map::out) is det.
:- pred pred_info_get_unproven_body_constraints(pred_info::in,
    list(prog_constraint)::out) is det.
:- pred pred_info_get_inst_graph_info(pred_info::in,
    inst_graph_info::out) is det.
:- pred pred_info_get_arg_modes_maps(pred_info::in,
    list(arg_modes_map)::out) is det.
:- pred pred_info_get_var_name_remap(pred_info::in,
    map(prog_var, string)::out) is det.
:- pred pred_info_get_assertions(pred_info::in,
    set(assert_id)::out) is det.
:- pred pred_info_get_instance_method_arg_types(pred_info::in,
    list(mer_type)::out) is det.
:- pred pred_info_get_clauses_info(pred_info::in,
    clauses_info::out) is det.
:- pred pred_info_get_proc_table(pred_info::in,
    proc_table::out) is det.

    % Setting the name of a pred_info after its creation won't remove its name
    % from the indexes under its old name or insert it into the indexes
    % under its new name. If is therefore safe to do this only after *all*
    % the passes that look up predicates by name.
    %
:- pred pred_info_set_name(string::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_set_orig_arity(arity::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_is_pred_or_func(pred_or_func::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_origin(pred_origin::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_status(pred_status::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_markers(pred_markers::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_typevarset(tvarset::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_tvar_kind_map(tvar_kind_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_existq_tvar_binding(tsubst::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_external_type_params(external_type_params::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_class_context(prog_constraints::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_proof_map(constraint_proof_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_map(constraint_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_unproven_body_constraints(list(prog_constraint)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_inst_graph_info(inst_graph_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_arg_modes_maps(list(arg_modes_map)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_var_name_remap(map(prog_var, string)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_assertions(set(assert_id)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_instance_method_arg_types(list(mer_type)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_clauses_info(clauses_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_proc_table(proc_table::in,
    pred_info::in, pred_info::out) is det.

    % Mode information for the arguments of a procedure.
    % The first map gives the instantiation state on entry of the node
    % corresponding to the prog_var. The second map gives the instantiation
    % state on exit.
    %
:- type arg_modes_map == pair(map(prog_var, bool)).

    % Return a list of all the proc_ids for the valid modes of this predicate.
    % This does not include candidate modes that were generated during mode
    % inference but which mode inference found were not valid modes.
    %
:- func pred_info_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate,
    % including invalid modes.
    %
:- func pred_info_all_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes of this predicate
    % that are not imported.
    %
:- func pred_info_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate
    % that are not imported (including invalid modes).
    %
    % XXX The implementation of this function is currently identical
    % to the implementation of pred_info_non_imported_procids.
    %
:- func pred_info_all_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes of this predicate
    % that are exported.
    %
:- func pred_info_exported_procids(pred_info) = list(proc_id).

    % Remove a procedure from the pred_info.
    %
:- pred pred_info_remove_procid(proc_id::in, pred_info::in, pred_info::out)
    is det.

:- pred pred_info_get_arg_types(pred_info::in, tvarset::out, existq_tvars::out,
    list(mer_type)::out) is det.

:- pred pred_info_set_arg_types(tvarset::in, existq_tvars::in,
    list(mer_type)::in, pred_info::in, pred_info::out) is det.

:- pred pred_info_get_univ_quant_tvars(pred_info::in, list(tvar)::out)
    is det.

:- pred pred_info_proc_info(pred_info::in, proc_id::in, proc_info::out) is det.

:- pred pred_info_set_proc_info(proc_id::in, proc_info::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_imported_not_external(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

    % pred_info_is_exported does *not* include predicates which are
    % exported_to_submodules or pseudo_exported.
    %
:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_opt_exported(pred_info::in) is semidet.

:- pred pred_info_is_exported_to_submodules(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

    % procedure_is_exported includes all modes of exported or
    % exported_to_submodules predicates, plus the in-in mode
    % for pseudo_exported unification predicates.
    %
:- pred procedure_is_exported(module_info::in, pred_info::in, proc_id::in)
    is semidet.

    % Set the pred_status of the predicate to `imported'.
    % This is used for `:- pragma external_{pred/func}(foo/2).'.
    %
:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

    % Do we have a clause goal type?
    % (this means either "clauses" or "clauses_and_pragmas")
    %
:- pred pred_info_clause_goal_type(pred_info::in) is semidet.

    % Do we have a pragma goal type?
    % (this means either "pragmas" or "clauses_and_pragmas")
    %
:- pred pred_info_pragma_goal_type(pred_info::in) is semidet.

:- pred pred_info_update_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.

    % Succeeds if there was a `:- pragma inline(...)' declaration
    % for this predicate. Note that the compiler may decide
    % to inline a predicate even if there was no pragma inline(...)
    % declaration for that predicate.
    %
:- pred pred_info_requested_inlining(pred_info::in) is semidet.

    % Succeeds if there was a `:- pragma no_inline(...)' declaration
    % for this predicate.
    %
:- pred pred_info_requested_no_inlining(pred_info::in) is semidet.

:- pred pred_info_get_purity(pred_info::in, purity::out) is det.

    % If the predicate has a purity promise, return it wrapped inside a `yes'.
    % Otherwise, return `no'.
    %
:- pred pred_info_get_promised_purity(pred_info::in, maybe(purity)::out)
    is det.

:- pred pred_info_infer_modes(pred_info::in) is semidet.

:- pred purity_to_markers(purity::in, list(pred_marker)::out) is det.

:- pred pred_info_get_call_id(pred_info::in, simple_call_id::out) is det.

:- pred pred_info_get_sym_name(pred_info::in, sym_name::out) is det.

    % Create an empty set of markers.
    %
:- pred init_markers(pred_markers::out) is det.

    % Check if a particular is in the set.
    %
:- pred check_marker(pred_markers::in, pred_marker::in) is semidet.

    % Add some markers to the set.
    %
:- pred add_marker(pred_marker::in,
    pred_markers::in, pred_markers::out) is det.
:- pred add_markers(list(pred_marker)::in,
    pred_markers::in, pred_markers::out) is det.

    % Remove a marker from the set.
    %
:- pred remove_marker(pred_marker::in,
    pred_markers::in, pred_markers::out) is det.

    % Convert the set to and from a list.
    %
:- pred markers_to_marker_list(pred_markers::in, list(pred_marker)::out)
    is det.
:- pred marker_list_to_markers(list(pred_marker)::in, pred_markers::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type pred_id
    --->    pred_id(int).

:- type proc_id == int.

pred_proc_id_project_pred_id(proc(PredId, _ProcId)) = PredId.
pred_proc_id_project_proc_id(proc(_PredId, ProcId)) = ProcId.

shroud_pred_id(pred_id(PredId)) = shrouded_pred_id(PredId).
shroud_proc_id(ProcId) = shrouded_proc_id(ProcId).
shroud_pred_proc_id(proc(pred_id(PredId), ProcId)) =
    shrouded_pred_proc_id(PredId, ProcId).

unshroud_pred_id(shrouded_pred_id(PredId)) = pred_id(PredId).
unshroud_proc_id(shrouded_proc_id(ProcId)) = ProcId.
unshroud_pred_proc_id(shrouded_pred_proc_id(PredId, ProcId)) =
    proc(pred_id(PredId), ProcId).

pred_id_to_int(pred_id(PredId), PredId).
pred_id_to_int(pred_id(PredId)) = PredId.

proc_id_to_int(ProcId, ProcId).
proc_id_to_int(ProcId) = ProcId.

initial_pred_id = pred_id(0).
initial_proc_id = 0.

invalid_pred_id = pred_id(-1).
invalid_proc_id = -1.

next_pred_id(pred_id(PredId), pred_id(NextPredId)) :-
    NextPredId = PredId + 1.

in_in_unification_proc_id(0).

    % We could store the next available ModeId rather than recomputing
    % it on demand, but it is probably more efficient this way.
    %
next_mode_id(Procs, ModeId) :-
    map.to_assoc_list(Procs, List),
    list.length(List, ModeInt),
    proc_id_to_int(ModeId, ModeInt).

calls_are_fully_qualified(Markers) =
    ( if check_marker(Markers, marker_calls_are_fully_qualified) then
        is_fully_qualified
    else
        may_be_partially_qualified
    ).

%-----------------------------------------------------------------------------%

    % The information specific to a predicate, as opposed to a procedure.
    % (Functions count as predicates.)
    %
    % The pred_info and pred_sub_info types constitute a single logical
    % data structure split into two parts for efficiency purposes.
    %
    % The pred_info type contains the most frequently accessed and/or updated
    % pieces of information about the predicate. Everything else is in the
    % pred_sub_info type. This arrangement minimizes the amount of memory that
    % needs to be allocated, and filled in, when a field is updated.

:- type pred_info
    --->    pred_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples of 2. Ideally, we would want the number of fields
                % of pred_info to be a multiple of 2 as well, but as of
                % 2017 march 15, this seems to be the optimal arrangement (zs).

                % Module in which pred occurs.
/*  1 */        pi_module_name          :: module_name,

                % Predicate name.
/*  2 */        pi_name                 :: string,

                % The original arity of the pred, i.e. its arity *not* counting
                % any type_info and/or typeclass_info arguments inserted
                % automatically by the compiler.
                %
                % For functions, the original arity *includes* the return
                % value, so that e.g. the original arity of int.+ would be 3.
/*  3 */        pi_orig_arity           :: arity,

                % Is this "predicate" really a predicate or a function?
/*  4 */        pi_is_pred_or_func      :: pred_or_func,

                % Where did the predicate come from?
/*  5 */        pi_pred_origin          :: pred_origin,

/*  6 */        pi_status               :: pred_status,

                % Various boolean flags.
/*  7 */        pi_markers              :: pred_markers,

                % Argument types.
                % Note that it is an invariant that any type_info- and/or
                % typeclass_info-related variables in the arguments of a
                % predicate must precede any polymorphically-typed arguments
                % whose type depends on the values of those type_info- and/or
                % typeclass_info-related variables; accurate GC for the MLDS
                % back-end relies on this.
/*  8 */        pi_arg_types            :: list(mer_type),

                % Names of type vars in the predicate's type declaration.
/*  9 */        pi_decl_typevarset      :: tvarset,

                % Names of type vars in the predicate's type declaration
                % or in the variable type assignments.
/* 10 */        pi_typevarset           :: tvarset,

                % The set of existentially quantified type variables in the
                % predicate's type declaration.
/* 11 */        pi_exist_quant_tvars    :: existq_tvars,

                % The class constraints on the type variables in the
                % predicate's type declaration.
/* 12 */        pi_class_context        :: prog_constraints,

/* 13 */        pi_clauses_info         :: clauses_info,

/* 14 */        pi_proc_table           :: proc_table,

/* 15 */        pi_pred_sub_info        :: pred_sub_info
            ).

:- type pred_sub_info
    --->    pred_sub_info(
                % The location (line #) of the :- pred decl.
                psi_context                     :: prog_context,

                % If the predicate is defined (a) in the current module, and
                % (b) explicitly by the user, as opposed to by the compiler,
                % then this records what section the predicate declaration
                % is in, and whether it is a predmode declaration.
                psi_cur_user_decl                :: maybe(cur_user_decl_info),

                % Whether the goals seen so far, if any, for this predicate
                % are clauses or foreign_code(...) pragmas.
                psi_goal_type                   :: goal_type,

                % Kinds of the type vars.
                psi_tvar_kind_map               :: tvar_kind_map,

                % The statically known bindings of existentially quantified
                % type variables inside this predicate. This field is set
                % at the end of the polymorphism stage.
                psi_existq_tvar_binding         :: tsubst,

                % The set of type variables which the body of the predicate
                % can't bind, and whose type_infos are produced elsewhere.
                % This includes universally quantified head types (the
                % type_infos are passed in) plus existentially quantified types
                % in preds called from the body (the type_infos are returned
                % from the called predicates). Computed during type checking.
                psi_external_type_params        :: external_type_params,

                % Explanations of how redundant constraints were eliminated.
                % These are needed by polymorphism.m to work out where to get
                % the typeclass_infos from. Computed during type checking.
                psi_constraint_proof_map        :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                % Computed during type checking.
                psi_constraint_map              :: constraint_map,

                % Unproven class constraints on type variables in the
                % predicate's body, if any (if this remains non-empty after
                % type checking has finished, post_typecheck.m will report a
                % type error).
                psi_unproven_body_constraints   :: list(prog_constraint),

                % The predicate's inst graph, for constraint based
                % mode analysis.
                psi_inst_graph_info             :: inst_graph_info,

                % Mode information extracted from constraint based
                % mode analysis.
                psi_arg_modes_maps              :: list(arg_modes_map),

                % Renames of some head variables computed by headvar_names.m,
                % for use by the debugger.
                psi_var_name_remap              :: map(prog_var, string),

                % List of assertions which mention this predicate.
                psi_assertions                  :: set(assert_id),

                % If this predicate is a class method implementation, this
                % list records the argument types before substituting the type
                % variables for the instance.
                % XXX does that make sense?
                psi_instance_method_arg_types   :: list(mer_type)
            ).

pred_info_init(ModuleName, PredSymName, Arity, PredOrFunc, Context,
        Origin, Status, CurUserDecl, GoalType, Markers,
        ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofs,
        ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo) :-
    % argument Context
    % argument GoalType
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    type_vars_list(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    % argument ClassProofs
    % argument ClassConstraintMap
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    set.init(Assertions),
    InstanceMethodArgTypes = [],
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, InstanceMethodArgTypes),

    sym_name_get_module_name_default(PredSymName, ModuleName, PredModuleName),
    PredName = unqualify_name(PredSymName),
    % argument Arity
    % argument PredOrFunc
    % argument Origin
    % argument Status
    % argument Markers
    % argument ArgTypes
    % argument TypeVarSet
    % argument ExistQVars
    % argument ClassContext
    % argument ClausesInfo
    map.init(ProcTable),
    PredInfo = pred_info(PredModuleName, PredName, Arity, PredOrFunc,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

pred_info_create(ModuleName, PredSymName, PredOrFunc, Context, Origin, Status,
        Markers, ArgTypes, TypeVarSet, ExistQVars, ClassContext,
        Assertions, VarNameRemap, ProcInfo, ProcId, PredInfo) :-
    % argument Context
    CurUserDecl = maybe.no,
    GoalType = goal_type_clause,
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    type_vars_list(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    map.init(ClassProofs),
    map.init(ClassConstraintMap),
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    % argument Assertions
    InstanceMethodArgTypes = [],

    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, InstanceMethodArgTypes),

    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.init(TVarNameMap),
    proc_info_get_headvars(ProcInfo, HeadVars),
    HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
    % The empty list of clauses is a little white lie.
    ClausesRep = init_clauses_rep,
    ItemNumbers = init_clause_item_numbers_user,
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    HasForeignClauses = no,
    HadSyntaxErrors = no,
    ClausesInfo = clauses_info(VarSet, TVarNameMap, VarTypes, VarTypes,
        HeadVarVec, ClausesRep, ItemNumbers, RttiVarMaps,
        HasForeignClauses, HadSyntaxErrors),

    % argument ModuleName
    PredName = unqualify_name(PredSymName),
    list.length(ArgTypes, Arity),
    % argument PredOrFunc
    % argument Origin
    % argument Status
    % argument Markers
    % argument ArgTypes
    % argument TypeVarSet
    % argument ExistQVars
    % argument ClassContext
    map.init(ProcTable0),
    next_mode_id(ProcTable0, ProcId),
    map.det_insert(ProcId, ProcInfo, ProcTable0, ProcTable),
    PredInfo = pred_info(ModuleName, PredName, Arity, PredOrFunc,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

define_new_pred(Origin, Goal0, Goal, ArgVars0, ExtraTypeInfos, InstMap0,
        SymName, TVarSet, VarTypes0, ClassContext, RttiVarMaps,
        VarSet0, InstVarSet, Markers, IsAddressTaken, HasParallelConj,
        VarNameRemap, ModuleInfo0, ModuleInfo, PredProcId) :-
    Goal0 = hlds_goal(_GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

    % XXX The set of existentially quantified type variables
    % here might not be correct.
    ExistQVars = [],

    % If interface typeinfo liveness is set, all type_infos for the
    % arguments need to be passed in, not just the ones that are used.
    % Similarly if the address of a procedure of this predicate is taken,
    % so that we can copy the closure.
    module_info_get_globals(ModuleInfo0, Globals),
    PredStatus = pred_status(status_local),
    non_special_interface_should_use_typeinfo_liveness(PredStatus,
        IsAddressTaken, Globals, TypeInfoLiveness),
    (
        TypeInfoLiveness = yes,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        goal_util.extra_nonlocal_typeinfos(RttiVarMaps, VarTypes0,
            ExistQVars, NonLocals, ExtraTypeInfos0),
        set_of_var.delete_list(ArgVars0, ExtraTypeInfos0, ExtraTypeInfos1),
        set_of_var.to_sorted_list(ExtraTypeInfos1, ExtraTypeInfos),
        ArgVars = ExtraTypeInfos ++ ArgVars0
    ;
        TypeInfoLiveness = no,
        ArgVars = ArgVars0,
        ExtraTypeInfos = []
    ),

    Context = goal_info_get_context(GoalInfo),
    ItemNumber = -1,
    Detism = goal_info_get_determinism(GoalInfo),
    compute_arg_types_modes(ArgVars, VarTypes0, InstMap0, InstMap,
        ArgTypes, ArgModes),

    % XXX why does pred_info_create take a sym_name only to unqualify it?
    module_info_get_name(ModuleInfo0, ModuleName),
    sym_name_get_module_name_default(SymName, ModuleName, SymNameModule),

    % Remove unneeded variables from the vartypes and varset.
    goal_util.goal_vars(Goal0, GoalVars0),
    set_of_var.insert_list(ArgVars, GoalVars0, GoalVars),
    GoalVarsSet = set_of_var.bitset_to_set(GoalVars),
    vartypes_select(GoalVarsSet, VarTypes0, VarTypes),
    varset.select(GoalVarsSet, VarSet0, VarSet),

    % Approximate the termination information for the new procedure.
    ( if goal_cannot_loop(ModuleInfo0, Goal0) then
        TermInfo = yes(cannot_loop(unit))
    else
        TermInfo = no
    ),

    MaybeDeclaredDetism = no,
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarSet, VarTypes, ArgVars, InstVarSet, ArgModes,
        detism_decl_none, MaybeDeclaredDetism, Detism, Goal0,
        RttiVarMaps, IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo0),
    proc_info_set_maybe_termination_info(TermInfo, ProcInfo0, ProcInfo),

    set.init(Assertions),

    pred_info_create(SymNameModule, SymName, pf_predicate, Context, Origin,
        PredStatus, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Assertions, VarNameRemap, ProcInfo, ProcId, PredInfo),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(PredInfo, PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo),

    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(list(prog_var)::in, vartypes::in,
    instmap::in, instmap::in, list(mer_type)::out, list(mer_mode)::out) is det.

compute_arg_types_modes([], _, _, _, [], []).
compute_arg_types_modes([Var | Vars], VarTypes, InstMapInit, InstMapFinal,
        [Type | Types], [Mode | Modes]) :-
    lookup_var_type(VarTypes, Var, Type),
    instmap_lookup_var(InstMapInit, Var, InstInit),
    instmap_lookup_var(InstMapFinal, Var, InstFinal),
    Mode = from_to_mode(InstInit, InstFinal),
    compute_arg_types_modes(Vars, VarTypes, InstMapInit, InstMapFinal,
        Types, Modes).

%-----------------------------------------------------------------------------%

% The trivial access predicates.

pred_info_module(PI) = X :-
    pred_info_get_module_name(PI, X).
pred_info_name(PI) = X :-
    pred_info_get_name(PI, X).
pred_info_orig_arity(PI) = X :-
    pred_info_get_orig_arity(PI, X).
pred_info_is_pred_or_func(PI) = X :-
    pred_info_get_is_pred_or_func(PI, X).

pred_info_get_module_name(!.PI, X) :-
    X = !.PI ^ pi_module_name.
pred_info_get_name(!.PI, X) :-
    X = !.PI ^ pi_name.
pred_info_get_orig_arity(!.PI, X) :-
    X = !.PI ^ pi_orig_arity.
pred_info_get_is_pred_or_func(!.PI, X) :-
    X = !.PI ^ pi_is_pred_or_func.

pred_info_get_context(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_context.
pred_info_get_cur_user_decl_info(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_cur_user_decl.
pred_info_get_origin(!.PI, X) :-
    X = !.PI ^ pi_pred_origin.
pred_info_get_status(!.PI, X) :-
    X = !.PI ^ pi_status.
pred_info_get_goal_type(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_goal_type.
pred_info_get_markers(!.PI, X) :-
    X = !.PI ^ pi_markers.
pred_info_get_arg_types(!.PI, X) :-
    X = !.PI ^ pi_arg_types.
pred_info_get_typevarset(!.PI, X) :-
    X = !.PI ^ pi_typevarset.
pred_info_get_tvar_kind_map(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_tvar_kind_map.
pred_info_get_exist_quant_tvars(!.PI, X) :-
    X = !.PI ^ pi_exist_quant_tvars.
pred_info_get_existq_tvar_binding(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_existq_tvar_binding.
pred_info_get_external_type_params(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_external_type_params.
pred_info_get_class_context(!.PI, X) :-
    X = !.PI ^ pi_class_context.
pred_info_get_constraint_proof_map(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_constraint_proof_map.
pred_info_get_constraint_map(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_constraint_map.
pred_info_get_unproven_body_constraints(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints.
pred_info_get_inst_graph_info(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_inst_graph_info.
pred_info_get_arg_modes_maps(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_arg_modes_maps.
pred_info_get_var_name_remap(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_var_name_remap.
pred_info_get_assertions(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_assertions.
pred_info_get_instance_method_arg_types(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types.
pred_info_get_clauses_info(!.PI, X) :-
    X = !.PI ^ pi_clauses_info.
pred_info_get_proc_table(!.PI, X) :-
    X = !.PI ^ pi_proc_table.

pred_info_set_name(X, !PI) :-
    !PI ^ pi_name := X.
pred_info_set_orig_arity(X, !PI) :-
    !PI ^ pi_orig_arity := X.
pred_info_set_is_pred_or_func(X, !PI) :-
    ( if X = !.PI ^ pi_is_pred_or_func then
        true
    else
        !PI ^ pi_is_pred_or_func := X
    ).
pred_info_set_origin(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ pi_pred_origin) then
        true
    else
        !PI ^ pi_pred_origin := X
    ).
pred_info_set_status(X, !PI) :-
    !PI ^ pi_status := X.
pred_info_set_goal_type(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_goal_type := X.
pred_info_set_markers(X, !PI) :-
    !PI ^ pi_markers := X.
pred_info_set_typevarset(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ pi_typevarset) then
        true
    else
        !PI ^ pi_typevarset := X
    ).
pred_info_set_tvar_kind_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_tvar_kind_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_tvar_kind_map:= X
    ).
pred_info_set_existq_tvar_binding(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_existq_tvar_binding := X.
pred_info_set_external_type_params(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_external_type_params)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_external_type_params := X
    ).
pred_info_set_class_context(X, !PI) :-
    !PI ^ pi_class_context := X.
pred_info_set_constraint_proof_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_constraint_proof_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_constraint_proof_map := X
    ).
pred_info_set_constraint_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_constraint_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_constraint_map := X
    ).
pred_info_set_unproven_body_constraints(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints := X
    ).
pred_info_set_inst_graph_info(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_inst_graph_info := X.
pred_info_set_arg_modes_maps(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_arg_modes_maps := X.
pred_info_set_var_name_remap(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_var_name_remap)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_var_name_remap := X
    ).
pred_info_set_assertions(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_assertions := X.
pred_info_set_instance_method_arg_types(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types := X.
pred_info_set_clauses_info(X, !PI) :-
    !PI ^ pi_clauses_info := X.
pred_info_set_proc_table(X, !PI) :-
    !PI ^ pi_proc_table := X.

% Access stats for the pred_info structure, derived on 2014 dec 13:
%
%  i      read      same      diff   same%
%  0  43651895         0         0            module_name
%  1  32003757         0       795   0.00%    name
%  2  25261836         0         0            orig_arity
%  3  15356498       727        68  91.45%    is_pred_or_func
%  4   1667832         0         0            context
%  5  12075408    382680     89618  81.03%    origin
%  6  72037616     96336   1082541   8.17%    status
%  7   6629313    100630   1054197   8.71%    goal_type
%  8  24876447    905599   1098352  45.19%    markers
%  9        10         0         0            attributes
% 10  20415273         0         0            arg_types
% 11  11783136   9736724    752983  92.82%    typevarset
% 12   3820195     85054         0 100.00%    tvar_kind_map
% 13   1374911         0         0            exist_quant_vars
% 14     22563         0        80   0.00%    existq_tvar_binding
% 15    476703    276426    152903  64.39%    external_type_params
% 16   7871038         0   2700797   0.00%    class_context
% 17   2591016    425209      3483  99.19%    constraint_proof_map
% 18   2752537    404771     23921  94.42%    constraint_map
% 19    285538    428650         4 100.00%    unproven_body_constraints
% 20         0         0         0            inst_graph_info
% 21         0         0         0            arg_modes_maps
% 22   5892199      6544      6726  49.31%    var_name_remap
% 23      3834         0      3797   0.00%    assertions
% 24         0         0     19439   0.00%    instance_method_arg_types
% 25  22294552     19444  12496762   0.16%    clauses_info
% 26 124287827    348040  31892426   1.08%    procedures
% 27  11128685   4600914   1642568  73.69%    three fields: decl_typevarset,
%                                             exist_quant_vars and arg_types

%-----------------------------------------------------------------------------%

% The non-trivial access predicates.

pred_info_all_procids(PredInfo) = ProcIds :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.keys(ProcTable, ProcIds).

pred_info_procids(PredInfo) = ValidProcIds :-
    AllProcIds = pred_info_all_procids(PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    IsValid =
        ( pred(ProcId::in) is semidet :-
            ProcInfo = map.lookup(ProcTable, ProcId),
            proc_info_is_valid_mode(ProcInfo)
        ),
    list.filter(IsValid, AllProcIds, ValidProcIds).

pred_info_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ;
        OldImportStatus = status_pseudo_imported,
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseudo_imported preds, procid 0 is imported
        list.delete_all(ProcIds0, 0, ProcIds)
    ;
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ; OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_abstract_exported
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_exported_to_submodules
        ; OldImportStatus = status_local
        ),
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_all_non_imported_procids(PredInfo) = ProcIds :-
    % XXX The documentation of this predicate says that the job it does
    % is different from the job of pred_info_non_imported_procids, but
    % the code is identical.
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ;
        OldImportStatus = status_pseudo_imported,
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseudo_imported preds, procid 0 is imported
        list.delete_all(ProcIds0, 0, ProcIds)
    ;
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ; OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_abstract_exported
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_exported_to_submodules
        ; OldImportStatus = status_local
        ),
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_exported_procids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        ProcIds = pred_info_procids(PredInfo)
    ;
        OldImportStatus = status_pseudo_exported,
        ProcIds = [0]
    ;
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ; OldImportStatus = status_pseudo_imported
        ; OldImportStatus = status_abstract_exported
        ; OldImportStatus = status_local
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ).

pred_info_remove_procid(ProcId, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, Procs0),
    map.delete(ProcId, Procs0, Procs),
    pred_info_set_proc_table(Procs, !PredInfo).

pred_info_get_arg_types(!.PredInfo, X, Y, Z) :-
    X = !.PredInfo ^ pi_decl_typevarset,
    Y = !.PredInfo ^ pi_exist_quant_tvars,
    Z = !.PredInfo ^ pi_arg_types.

pred_info_set_arg_types(X, Y, Z, !PredInfo) :-
    !PredInfo ^ pi_decl_typevarset := X,
    !PredInfo ^ pi_exist_quant_tvars := Y,
    !PredInfo ^ pi_arg_types := Z.

pred_info_proc_info(PredInfo, ProcId, ProcInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo).

pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, Procedures0),
    map.set(ProcId, ProcInfo, Procedures0, Procedures),
    pred_info_set_proc_table(Procedures, !PredInfo).

pred_info_is_imported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    ( PredStatus = pred_status(status_imported(_))
    ; PredStatus = pred_status(status_external(_))
    ).

pred_info_is_imported_not_external(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_imported(_)).

pred_info_is_pseudo_imported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_pseudo_imported).

pred_info_is_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_exported).

pred_info_is_opt_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_opt_exported).

pred_info_is_exported_to_submodules(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_exported_to_submodules).

pred_info_is_pseudo_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_pseudo_exported).

procedure_is_exported(ModuleInfo, PredInfo, ProcId) :-
    % XXX STATUS
    (
        pred_info_is_exported(PredInfo)
    ;
        pred_info_is_opt_exported(PredInfo)
    ;
        pred_info_is_exported_to_submodules(PredInfo)
    ;
        pred_info_is_pseudo_exported(PredInfo),
        in_in_unification_proc_id(ProcId)
    ;
        pred_info_get_status(PredInfo, PredStatus),
        PredStatus = pred_status(status_external(ExternalImportStatus)),
        pred_status_is_exported(pred_status(ExternalImportStatus)) = yes
    ;
        pred_info_get_origin(PredInfo, Origin),
        Origin = origin_special_pred(SpecialPredId, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        % If the search fails, then TypeCtor must be a builtin type
        % constructor, such as the tuple constructor.
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_in_exported_eqv(TypeDefn, yes),
        require_complete_switch [SpecialPredId]
        (
            SpecialPredId = spec_pred_unify,
            % The other proc_ids are module-specific.
            in_in_unification_proc_id(ProcId)
        ;
            SpecialPredId = spec_pred_compare
            % The declared modes are all global, and we don't
            % generate any modes for compare preds dynamically.
        ;
            SpecialPredId = spec_pred_index,
            % The index predicate is never called from anywhere
            % except the compare predicate.
            fail
        )
    ).

pred_info_mark_as_external(!PredInfo) :-
    pred_info_get_status(!.PredInfo, PredStatus0),
    PredStatus0 = pred_status(OldImportStatus0),
    PredStatus = pred_status(status_external(OldImportStatus0)),
    pred_info_set_status(PredStatus, !PredInfo).

pred_info_clause_goal_type(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    clause_goal_type(GoalType).

pred_info_pragma_goal_type(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    pragma_goal_type(GoalType).

:- pred clause_goal_type(goal_type::in) is semidet.

clause_goal_type(goal_type_clause).
clause_goal_type(goal_type_clause_and_foreign).

:- pred pragma_goal_type(goal_type::in) is semidet.

pragma_goal_type(goal_type_foreign).
pragma_goal_type(goal_type_clause_and_foreign).

pred_info_update_goal_type(GoalType1, !PredInfo) :-
    pred_info_get_goal_type(!.PredInfo, GoalType0),
    (
        GoalType0 = goal_type_none,
        GoalType = GoalType1
    ;
        GoalType0 = goal_type_foreign,
        ( if clause_goal_type(GoalType1) then
            GoalType = goal_type_clause_and_foreign
        else
            GoalType = goal_type_foreign
        )
    ;
        GoalType0 = goal_type_clause,
        ( if pragma_goal_type(GoalType1) then
            GoalType = goal_type_clause_and_foreign
        else
            GoalType = goal_type_clause
        )
    ;
        GoalType0 = goal_type_clause_and_foreign,
        GoalType = GoalType0
    ;
        GoalType0 = goal_type_promise(_),
        unexpected($module, $pred, "promise")
    ),
    % ( if GoalType = GoalType0 then
    %     % Avoid unnecessary memory allocation.
    %     true
    % else
        pred_info_set_goal_type(GoalType, !PredInfo).
    % ).

pred_info_requested_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    (
        check_marker(Markers, marker_user_marked_inline)
    ;
        check_marker(Markers, marker_heuristic_inline)
    ).

pred_info_requested_no_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    check_marker(Markers, marker_user_marked_no_inline).

pred_info_get_purity(PredInfo0, Purity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( if check_marker(Markers, marker_is_impure) then
        Purity = purity_impure
    else if check_marker(Markers, marker_is_semipure) then
        Purity = purity_semipure
    else
        Purity = purity_pure
    ).

pred_info_get_promised_purity(PredInfo0, MaybePromisedPurity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( if check_marker(Markers, marker_promised_pure) then
        MaybePromisedPurity = yes(purity_pure)
    else if check_marker(Markers, marker_promised_semipure) then
        MaybePromisedPurity = yes(purity_semipure)
    else
        MaybePromisedPurity = no
    ).

pred_info_infer_modes(PredInfo) :-
    pred_info_get_markers(PredInfo, Markers),
    check_marker(Markers, marker_infer_modes).

purity_to_markers(purity_pure, []).
purity_to_markers(purity_semipure, [marker_is_semipure]).
purity_to_markers(purity_impure, [marker_is_impure]).

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    type_vars_list(ArgTypes, ArgTypeVars0),
    list.sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    list.delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

%-----------------------------------------------------------------------------%

pred_info_get_call_id(PredInfo, SimpleCallId) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_sym_name(PredInfo, SymName),
    Arity = pred_info_orig_arity(PredInfo),
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity).

pred_info_get_sym_name(PredInfo, SymName) :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    SymName = qualified(Module, Name).

%-----------------------------------------------------------------------------%

init_markers(set.init).

check_marker(MarkerSet, Marker) :-
    set.member(Marker, MarkerSet).

add_marker(Marker, !MarkerSet) :-
    set.insert(Marker, !MarkerSet).

add_markers(Markers, !MarkerSet) :-
    set.insert_list(Markers, !MarkerSet).

remove_marker(Marker, !MarkerSet) :-
    set.delete(Marker, !MarkerSet).

markers_to_marker_list(MarkerSet, Markers) :-
    set.to_sorted_list(MarkerSet, Markers).

marker_list_to_markers(Markers, MarkerSet) :-
    set.list_to_set(Markers, MarkerSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Various predicates for accessing the proc_info data structure,
    % and the types they work with.

:- interface.

:- type is_address_taken
    --->    address_is_taken
    ;       address_is_not_taken.

:- type deep_profile_role
    --->    deep_prof_inner_proc(
                dpip_outer_proc         :: pred_proc_id
            )
    ;       deep_prof_outer_proc(
                dpop_inner_proc         :: pred_proc_id
            ).

:- type deep_recursion_info
    --->    deep_recursion_info(
                dri_role                :: deep_profile_role,

                % If the procedure is not tail recursive, this list is empty.
                % Otherwise, it contains outer-inner pairs of procedures
                % in the visible SCC, including this procedure and its copy.
                dri_visible_scc         :: list(visible_scc_data)
            ).

:- type visible_scc_data
    --->    visible_scc_data(
                vis_outer_proc          :: pred_proc_id,
                vis_inner_proc          :: pred_proc_id,

                % A list of all the call site numbers that correspond to
                % tail calls. (Call sites are numbered depth-first,
                % left-to-right, starting from zero.)
                rec_call_sites          :: list(int)
            ).

:- type call_site_static_data           % defines MR_CallSiteStatic
    --->    normal_call(
                normal_callee           :: rtti_proc_label,
                normal_type_subst       :: string,
                normal_file_name        :: string,
                normal_line_number      :: int,
                normal_goal_path        :: forward_goal_path
            )
    ;       special_call(
                special_file_name       :: string,
                special_line_number     :: int,
                special_goal_path       :: forward_goal_path
            )
    ;       higher_order_call(
                higher_order_file_name  :: string,
                ho_line_number          :: int,
                ho_goal_path            :: forward_goal_path
            )
    ;       method_call(
                method_file_name        :: string,
                method_line_number      :: int,
                method_goal_path        :: forward_goal_path
            )
    ;       callback(
                callback_file_name      :: string,
                callback_line_number    :: int,
                callback_goal_path      :: forward_goal_path
            ).

:- type hlds_proc_static
    --->    hlds_proc_static(           % defines part of MR_ProcStatic
                proc_static_file_name   :: string,
                proc_static_line_number :: int,
                proc_is_in_interface    :: bool,
                call_site_statics       :: list(call_site_static_data),
                coverage_points         :: list(coverage_point_info)
            ).

    % The hlds_deep_excp_vars gives the variables that hold the values returned
    % by the call port code, which are needed to let exception.throw perform
    % the work we need to do at the excp port.
:- type hlds_deep_excp_vars
    --->    hlds_deep_excp_vars(
                top_csd                 :: prog_var,
                middle_csd              :: prog_var,
                old_outermost           :: maybe(prog_var)
                                        % Needed only with the save/restore
                                        % approach, not the activation counting
                                        % approach.
            ).

:- type hlds_deep_layout
    --->    hlds_deep_layout(
                deep_layout_static      :: hlds_proc_static,
                deep_layout_excp        :: hlds_deep_excp_vars
            ).

:- type deep_profile_proc_info
    --->    deep_profile_proc_info(
                % This field is set during the first part of the deep profiling
                % transformation; tail recursion, if that is enabled.
                deep_rec                :: maybe(deep_recursion_info),

                % This field is set during the second part; it will be bound
                % to `no' before and during the first part, and to `yes'
                % after the second. The contents of this field govern
                % what will go into MR_ProcStatic structures.
                deep_layout             :: maybe(hlds_deep_layout),

                % This field stores the original body of a procedure,
                % before either part of the deep profiling transformation
                % was executed. For inner procedures created by the tail
                % recursion part of the deep profiling transformation,
                % it holds the original body of the outer procedure.
                deep_orig_body          :: deep_original_body
            ).

:- type deep_original_body
    --->    deep_original_body(
                dob_body                :: hlds_goal,
                dob_head_vars           :: list(prog_var),
                dob_instmap             :: instmap,
                dob_vartypes            :: vartypes,
                dob_detism              :: determinism,
                dob_varset              :: prog_varset
            ).

:- type table_arg_infos
    --->    table_arg_infos(
                list(table_arg_info),
                map(tvar, table_locn)
            ).

:- type table_arg_info
    --->    table_arg_info(
                orig_var_num            :: int,
                orig_var_name           :: string,
                slot_num                :: int,
                arg_type                :: mer_type
            ).

    % This type is analogous to llds:layout_locn, but it refers to slots in
    % the extended answer blocks used by I/O action tabling for declarative
    % debugging, not to lvals.
:- type table_locn
    --->    table_locn_direct(int)
    ;       table_locn_indirect(int, int).

    % This type differs from the type table_step_kind in table_statistics.m
    % in the library in that
    % (a) in gives more information about the type of the corresponding
    % argument (if this info is needed and available),
    % (b) it doesn't have to be an enum, and
    % (c) it doesn't have to handle dummy steps.
    %
:- type table_trie_step
    --->    table_trie_step_dummy
    ;       table_trie_step_int(int_type)
    ;       table_trie_step_char
    ;       table_trie_step_string
    ;       table_trie_step_float
    ;       table_trie_step_enum(
                % The int gives the number of alternatives in the enum type,
                % and thus the size of the corresponding trie node.
                int
            )
    ;       table_trie_step_foreign_enum
    ;       table_trie_step_general(
                mer_type,
                table_is_poly,
                table_value_or_addr
            )
    ;       table_trie_step_typeinfo
    ;       table_trie_step_typeclassinfo
    ;       table_trie_step_promise_implied.

:- type table_is_poly
    --->    table_is_mono       % The table type is monomorphic.
    ;       table_is_poly.      % The table type is polymorphic.

:- type table_value_or_addr
    --->    table_value         % We are tabling the value itself.
    ;       table_addr.         % We are tabling only the address.

    % Return a description of what kind of statistics we collect for a trie
    % step of a given kind. The description is the name of a value in the C
    % enum type MR_TableStepStatsKind. (We will need to generalize this
    % when we implement tabling for non-C backends.)
    %
:- func table_step_stats_kind(table_trie_step) = string.

:- type proc_table_io_info
    --->    proc_table_io_info(
                % The information we need to display an I/O action to the user.
                %
                % The table_arg_infos correspond one to one to the elements
                % of the block saved for an I/O action. The first element
                % will be the pointer to the proc_layout of the action's
                % procedure.
                %
                % The right tvarset for interpreting the types in the
                % table_arg_infos is the one in the proc_info in which
                % the proc_table_io_info is stored.

                maybe(table_arg_infos)
            ).

:- type table_step_desc
    --->    table_step_desc(
                tsd_var_name                :: string,
                tsd_step                    :: table_trie_step
            ).

:- type proc_table_struct_info
    --->    proc_table_struct_info(
                % The information we need to create the data structures
                % created by tabling for a procedure, and to interpret them
                % for the debugger (except the information -such as
                % determinism- that is already available from proc_layout
                % structures.
                %
                % The table_arg_infos list first all the input arguments,
                % then all the output arguments.
                %
                % The right tvarset for interpreting the types in the
                % table_arg_infos is the one stored below. It is taken from
                % the proc_info of the procedure whose table this structure
                % describes. Since we care only about the shapes of the types,
                % we don't care about neither the actual numerical values
                % nor the names of the type variables, so we don't care if
                % the tvarset in that proc_info changes after table_gen.m
                % takes the snapshot stored here.
                %
                % We record the rtti_proc_label of the procedure whose table
                % this is. We can't record its identity in the form of a
                % pred_proc_id, since that won't work if the procedure is
                % deleted before the code generation phase.

                ptsi_proc_label             :: rtti_proc_label,
                ptsi_tvarset                :: tvarset,
                ptsi_context                :: prog_context,
                ptsi_num_inputs             :: int,
                ptsi_num_outputs            :: int,
                ptsi_input_steps            :: list(table_step_desc),
                ptsi_maybe_output_steps     :: maybe(list(table_step_desc)),
                ptsi_gen_arg_infos          :: table_arg_infos,
                ptsi_eval_method            :: eval_method
            ).

:- type special_proc_return
    --->    generator_return(
                % The generator is stored in this location. We can't use an
                % rval to represent the location, since we don't want this
                % module to depend on the ll_backend package.
                generator_rval          :: string,

                % What should we pass as the value of the debug parameter
                % in the call to MR_tbl_mmos_return_answer?
                return_debug            :: string
            ).

:- type structure_sharing_domain_and_status
    --->    structure_sharing_domain_and_status(
                structure_sharing_domain,
                analysis_status
            ).

:- type structure_reuse_domain_and_status
    --->    structure_reuse_domain_and_status(
                structure_reuse_domain,
                analysis_status
            ).

:- type untuple_proc_info
    --->    untuple_proc_info(
                map(prog_var, prog_vars)
            ).

:- type detism_decl
    --->    detism_decl_explicit
    ;       detism_decl_implicit
    ;       detism_decl_none.
            % The determinism of the procedure is not declared.

:- pred proc_info_init(prog_context::in, int::in, arity::in,
    list(mer_type)::in, maybe(list(mer_mode))::in, list(mer_mode)::in,
    maybe(list(is_live))::in, detism_decl::in, maybe(determinism)::in,
    is_address_taken::in, has_parallel_conj::in, map(prog_var, string)::in,
    proc_info::out) is det.

:- pred proc_info_create(prog_context::in, int::in,
    prog_varset::in, vartypes::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

:- pred proc_info_set_body(prog_varset::in, vartypes::in,
    list(prog_var)::in, hlds_goal::in, rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.

:- type can_process
    --->    cannot_process_yet
    ;       can_process_now.

:- type needs_maxfr_slot
    --->    needs_maxfr_slot
    ;       does_not_need_maxfr_slot.

:- type has_parallel_conj
    --->    has_parallel_conj
    ;       has_no_parallel_conj.

:- type has_user_event
    --->    has_user_event
    ;       has_no_user_event.

:- type has_self_tail_rec_call
    --->    has_self_tail_rec_call
    ;       has_no_self_tail_rec_call.

:- type has_mutual_tail_rec_call
    --->    has_mutual_tail_rec_call
    ;       has_no_mutual_tail_rec_call.

:- type has_tail_rec_call
    --->    has_tail_rec_call(
                has_self_tail_rec_call,
                has_mutual_tail_rec_call
            ).

:- type oisu_pred_kind_for
    --->    oisu_creator_for(type_ctor)
    ;       oisu_mutator_for(type_ctor)
    ;       oisu_destructor_for(type_ctor).

    % Is a procedure the subject of any foreign_export pragmas?
    %
:- type proc_foreign_exports
    --->    no_foreign_exports
    ;       has_foreign_exports.

    % Gives an indication of whether or not the procedure
    % might throw an exception.
    %
:- type proc_exception_info
    --->    proc_exception_info(
                proc_exception_status               :: exception_status,
                proc_maybe_excep_analysis_status    :: maybe(analysis_status)
            ).

    % Gives an indication of whether or not the procedure modifies the trail.
    %
:- type proc_trailing_info
    --->    proc_trailing_info(
                proc_trailing_status                :: trailing_status,
                proc_maybe_trail_analysis_status    :: maybe(analysis_status)
            ).

    % Gives an indication of whether or not the procedure, or one of its
    % subgoals, calls a procedure that is tabled using minimal model tabling.
:- type proc_mm_tabling_info
    --->    proc_mm_tabling_info(
                % The tabling status for this procedures as determined
                % by tabling analysis.
                proc_mm_status                      :: mm_tabling_status,

                % The status of the tabling analysis results for this
                % procedure. This is used by the intermodule analysis
                % framework to determine if there is any benefit in
                % re-analysing this procedure.
                proc_mm_analysis_status             :: maybe(analysis_status)
            ).

    % Predicates to get fields of proc_infos.

:- pred proc_info_get_headvars(proc_info::in, list(prog_var)::out) is det.
:- pred proc_info_get_goal(proc_info::in, hlds_goal::out) is det.
:- pred proc_info_get_varset(proc_info::in, prog_varset::out) is det.
:- pred proc_info_get_vartypes(proc_info::in, vartypes::out) is det.
:- pred proc_info_get_rtti_varmaps(proc_info::in, rtti_varmaps::out) is det.
:- pred proc_info_get_inst_varset(proc_info::in, inst_varset::out) is det.
:- pred proc_info_get_maybe_declared_argmodes(proc_info::in,
    maybe(list(mer_mode))::out) is det.
:- pred proc_info_get_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_get_maybe_arglives(proc_info::in,
    maybe(list(is_live))::out) is det.
:- pred proc_info_get_declared_determinism(proc_info::in,
    maybe(determinism)::out) is det.
:- pred proc_info_get_inferred_determinism(proc_info::in,
    determinism::out) is det.
:- pred proc_info_get_eval_method(proc_info::in, eval_method::out) is det.
:- pred proc_info_get_mode_errors(proc_info::in,
    list(mode_error_info)::out) is det.

:- pred proc_info_get_context(proc_info::in, prog_context::out) is det.
:- pred proc_info_get_item_number(proc_info::in, int::out) is det.
:- pred proc_info_get_can_process(proc_info::in, can_process::out) is det.
:- pred proc_info_get_detism_decl(proc_info::in, detism_decl::out) is det.
:- pred proc_info_get_maybe_untuple_info(proc_info::in,
    maybe(untuple_proc_info)::out) is det.
:- pred proc_info_get_var_name_remap(proc_info::in,
    map(prog_var, string)::out) is det.
:- pred proc_info_get_statevar_warnings(proc_info::in,
    list(error_spec)::out) is det.
:- pred proc_info_get_deleted_call_callees(proc_info::in,
    set(pred_proc_id)::out) is det.
:- pred proc_info_get_is_address_taken(proc_info::in,
    is_address_taken::out) is det.
:- pred proc_info_get_has_any_foreign_exports(proc_info::in,
    proc_foreign_exports::out) is det.
:- pred proc_info_get_has_parallel_conj(proc_info::in,
    has_parallel_conj::out) is det.
:- pred proc_info_get_has_user_event(proc_info::in,
    has_user_event::out) is det.
:- pred proc_info_get_has_tail_rec_call(proc_info::in,
    has_tail_rec_call::out) is det.
:- pred proc_info_get_oisu_kind_fors(proc_info::in,
    list(oisu_pred_kind_for)::out) is det.
:- pred proc_info_get_maybe_require_tailrec_info(proc_info::in,
    maybe(require_tail_recursion)::out) is det.
:- pred proc_info_get_reg_r_headvars(proc_info::in,
    set_of_progvar::out) is det.
:- pred proc_info_get_maybe_arg_info(proc_info::in,
    maybe(list(arg_info))::out) is det.
:- pred proc_info_get_maybe_special_return(proc_info::in,
    maybe(special_proc_return)::out) is det.
:- pred proc_info_get_liveness_info(proc_info::in, liveness_info::out) is det.
:- pred proc_info_get_stack_slots(proc_info::in, stack_slots::out) is det.
:- pred proc_info_get_needs_maxfr_slot(proc_info::in,
    needs_maxfr_slot::out) is det.
:- pred proc_info_get_call_table_tip(proc_info::in,
    maybe(prog_var)::out) is det.
:- pred proc_info_get_maybe_proc_table_io_info(proc_info::in,
    maybe(proc_table_io_info)::out) is det.
:- pred proc_info_get_table_attributes(proc_info::in,
    maybe(table_attributes)::out) is det.
:- pred proc_info_get_maybe_deep_profile_info(proc_info::in,
    maybe(deep_profile_proc_info)::out) is det.
:- pred proc_info_get_maybe_arg_size_info(proc_info::in,
    maybe(arg_size_info)::out) is det.
:- pred proc_info_get_maybe_termination_info(proc_info::in,
    maybe(termination_info)::out) is det.
:- pred proc_info_get_termination2_info(proc_info::in,
    termination2_info::out) is det.
:- pred proc_info_get_exception_info(proc_info::in,
    maybe(proc_exception_info)::out) is det.
:- pred proc_info_get_trailing_info(proc_info::in,
    maybe(proc_trailing_info)::out) is det.
:- pred proc_info_get_mm_tabling_info(proc_info::in,
    maybe(proc_mm_tabling_info)::out) is det.

    % Predicates to set fields of proc_infos.

:- pred proc_info_set_headvars(list(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_goal(hlds_goal::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_varset(prog_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_vartypes(vartypes::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_rtti_varmaps(rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inst_varset(inst_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_declared_argmodes(maybe(list(mer_mode))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_argmodes(list(mer_mode)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_head_modes_constraint(mode_constraint::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arglives(maybe(list(is_live))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inferred_determinism(determinism::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_eval_method(eval_method::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_mode_errors(list(mode_error_info)::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_set_can_process(can_process::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_detism_decl(detism_decl::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_untuple_info(maybe(untuple_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_var_name_remap(map(prog_var, string)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_statevar_warnings(list(error_spec)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_deleted_call_callees(set(pred_proc_id)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_address_taken(is_address_taken::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_any_foreign_exports(proc_foreign_exports::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_parallel_conj(has_parallel_conj::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_user_event(has_user_event::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_tail_rec_call(has_tail_rec_call::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_oisu_kind_fors(list(oisu_pred_kind_for)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_require_tailrec_info(require_tail_recursion::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_reg_r_headvars(set_of_progvar::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_arg_info(list(arg_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_special_return(maybe(special_proc_return)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_liveness_info(liveness_info::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_stack_slots(stack_slots::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_needs_maxfr_slot(needs_maxfr_slot::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_call_table_tip(maybe(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_proc_table_io_info(maybe(proc_table_io_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_table_attributes(maybe(table_attributes)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_deep_profile_info(
    maybe(deep_profile_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arg_size_info(maybe(arg_size_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_termination_info(maybe(termination_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_termination2_info(termination2_info::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_exception_info(maybe(proc_exception_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_trailing_info(maybe(proc_trailing_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_mm_tabling_info(maybe(proc_mm_tabling_info)::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_structure_sharing(proc_info::in,
    maybe(structure_sharing_domain_and_status)::out) is det.

:- pred proc_info_set_structure_sharing(
    structure_sharing_domain_and_status::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_imported_structure_sharing(proc_info::in,
    prog_vars::out, list(mer_type)::out, structure_sharing_domain::out)
    is semidet.

:- pred proc_info_set_imported_structure_sharing(prog_vars::in,
    list(mer_type)::in, structure_sharing_domain::in, proc_info::in,
    proc_info::out) is det.

:- pred proc_info_reset_imported_structure_sharing(proc_info::in,
    proc_info::out) is det.

:- pred proc_info_get_structure_reuse(proc_info::in,
    maybe(structure_reuse_domain_and_status)::out) is det.

:- pred proc_info_set_structure_reuse(structure_reuse_domain_and_status::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_imported_structure_reuse(proc_info::in,
    prog_vars::out, list(mer_type)::out, structure_reuse_domain::out)
    is semidet.

:- pred proc_info_set_imported_structure_reuse(prog_vars::in,
    list(mer_type)::in, structure_reuse_domain::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_reset_imported_structure_reuse(proc_info::in,
    proc_info::out) is det.

:- pred proc_info_head_modes_constraint(proc_info::in, mode_constraint::out)
    is det.

    % See also proc_info_interface_code_model in code_model.m.
    %
:- pred proc_info_interface_determinism(proc_info::in, determinism::out)
    is det.

    % proc_info_never_succeeds(ProcInfo, Result):
    %
    % Return Result = yes if the procedure is known to never succeed
    % according to the declared determinism.
    %
:- pred proc_info_never_succeeds(proc_info::in, bool::out) is det.

:- pred proc_info_declared_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_arglives(proc_info::in, module_info::in,
    list(is_live)::out) is det.
:- pred proc_info_arg_info(proc_info::in, list(arg_info)::out) is det.
:- pred proc_info_get_initial_instmap(proc_info::in, module_info::in,
    instmap::out) is det.

:- pred proc_info_ensure_unique_names(proc_info::in, proc_info::out) is det.

    % Create a new variable of the given type to the procedure.
    %
:- pred proc_info_create_var_from_type(mer_type::in, maybe(string)::in,
    prog_var::out, proc_info::in, proc_info::out) is det.

    % Create a new variable for each element of the list of types.
    %
:- pred proc_info_create_vars_from_types(list(mer_type)::in,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % (further) instantiated by the procedure.
    %
:- pred proc_info_instantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % not (further) instantiated by the procedure.
    %
:- pred proc_info_uninstantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Return true if the interface of the given procedure must include
    % typeinfos for all the type variables in the types of the arguments.
    %
:- pred proc_interface_should_use_typeinfo_liveness(pred_info::in, proc_id::in,
    globals::in, bool::out) is det.

    % Return true if the body of a procedure from the given predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    % Note that body typeinfo liveness implies interface typeinfo liveness,
    % but not vice versa.
    %
:- pred body_should_use_typeinfo_liveness(pred_info::in, globals::in,
    bool::out) is det.

    % If the procedure has a input/output pair of io.state arguments,
    % return the positions of those arguments in the argument list.
    % The positions are given as argument numbers, with the first argument
    % in proc_info_get_headvars being position 1, and so on. The first output
    % argument gives the position of the input state, the second the
    % position of the output state.
    %
    % Note that the automatically constructed unify, index and compare
    % procedures for the io:state type are not counted as having io:state
    % args, since they do not fall into the scheme of one input and one
    % output arg. Since they should never be called, this should not matter.
    %
:- pred proc_info_has_io_state_pair(module_info::in, proc_info::in,
    int::out, int::out) is semidet.

:- pred proc_info_has_io_state_pair_from_details(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, vartypes::in,
    int::out, int::out) is semidet.

:- pred proc_info_has_higher_order_arg_from_details(module_info::in,
    vartypes::in, list(prog_var)::in) is semidet.

    % Given a procedure table and the id of a procedure in that table,
    % return a procedure id to be attached to a clone of that procedure.
    % (The task of creating the clone proc_info and inserting into the
    % procedure table is the task of the caller.)
    %
:- pred clone_proc_id(proc_table::in, proc_id::in, proc_id::out) is det.

    % When mode inference is enabled, we record for each inferred mode
    % whether it is valid or not by keeping a list of error messages
    % in the proc_info. The mode is valid iff this list is empty.
    %
:- pred proc_info_is_valid_mode(proc_info::in) is semidet.

    % Make sure that all headvars are named. This can be useful e.g.
    % because the debugger ignores unnamed variables.
    %
:- pred ensure_all_headvars_are_named(proc_info::in, proc_info::out) is det.

    % Test whether the variable is of a dummy type, based on the vartypes.
    %
:- pred var_is_of_dummy_type(module_info::in, vartypes::in, prog_var::in)
    is semidet.

:- pred var_is_of_non_dummy_type(module_info::in, vartypes::in, prog_var::in)
    is semidet.

:- implementation.

    % The information specific to a procedure, as opposed to a predicate.
    %
    % The proc_info and proc_sub_info types constitute a single logical
    % data structure split into two parts for efficiency purposes.
    %
    % The proc_info type contains the most frequently accessed and/or updated
    % pieces of information about the procedure. Everything else is in the
    % proc_sub_info type. This arrangement minimizes the amount of memory that
    % needs to be allocated, and filled in, when a field is updated.

:- type proc_info
    --->    proc_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples of 2. Ideally, we would want the number of fields
                % of pred_info to be a multiple of 2 as well, but as of
                % 2017 march 15, this seems to be the optimal arrangement (zs).

/*  1 */        proc_head_vars                  :: list(prog_var),
/*  2 */        proc_body                       :: hlds_goal,

/*  3 */        proc_prog_varset                :: prog_varset,
/*  4 */        proc_var_types                  :: vartypes,

                % Information about type_infos and typeclass_infos.
/*  5 */        proc_rtti_varmaps               :: rtti_varmaps,

/*  6 */        proc_inst_varset                :: inst_varset,

                % The declared modes of arguments.
/*  7 */        proc_maybe_decl_head_modes      :: maybe(list(mer_mode)),

/*  8 */        proc_actual_head_modes          :: list(mer_mode),
/*  9 */        proc_maybe_head_modes_constr    :: maybe(mode_constraint),

                % Liveness (in the mode analysis sense) of the arguments
                % in the caller; says whether each argument may be used
                % after the call.
/* 10 */        proc_headvar_caller_liveness    :: maybe(list(is_live)),

                % The _declared_ determinism of the procedure, or `no'
                % if there was no detism declaration.
/* 11 */        proc_declared_detism            :: maybe(determinism),
/* 12 */        proc_inferred_detism            :: determinism,

                % How should the proc be evaluated.
/* 13 */        proc_eval_method                :: eval_method,

/* 14 */        proc_mode_errors                :: list(mode_error_info),

/* 15 */        proc_sub_info                   :: proc_sub_info
            ).

:- type proc_sub_info
    --->    proc_sub_info(
                % The context of the `:- mode' decl, or the context of the
                % first clause if there was no mode declaration.
                psi_proc_context                :: prog_context,

                % The item number of the mode declaration, if there was one,
                % and a negative number otherwise.
                psi_item_number                 :: int,

                % Set to cannot_process if we must not process this procedure
                % just yet. This is used to delay mode checking etc. for
                % complicated modes of unification predicates until the end
                % of the unique_modes pass.
                psi_can_process                 :: can_process,

                % Was the determinism declaration explicit, or was it implicit,
                % as for functions?
                psi_detism_decl                 :: detism_decl,

                % If set, it means this procedure was created from another
                % procedure by the untupling transformation. This slot records
                % which of the procedure's arguments were derived from which
                % arguments in the original procedure.
                %
                % This is effectively a record of the *procedure*'s origin.
                % (The pred_origin field records the *predicate*'s origin.)
                psi_maybe_untuple_info          :: maybe(untuple_proc_info),

                % Remaps the compiler-created variables named HeadVar__N
                % to the user-given variable names that actually occupied the
                % corresponding argument slots in the procedure's clauses.
                % Has an entry for a head variable only if *all* the clauses
                % consistently give that argument that name, if they give it
                % any name at all.
                % This renaming is applied only after semantic analysis,
                % although it is recorded earlier. The reason for this is
                % to make any error messages about the goals that unify the
                % original headvar (e.g. "X") with the introduced headvar
                % (e.g. "HeadVar__1") give the goal as HeadVar__1 = X,
                % not as X = X, since the latter would be very confusing.
                psi_proc_var_name_remap         :: map(prog_var, string),

                % Any warnings generated by the state variable transformation
                % that we should print only if we find a mode error that could
                % be caused by the problem being warned about.
                psi_statevar_warnings           :: list(error_spec),

                % The set of procedures that the body of this procedure
                % *used* to call, but doesn't anymore. This can happen
                % For several reason. These reasons include the call being
                % - inside a trace goal scope whose compile-time condition
                %   turned out to be false,
                % - in the then part of an if-then-else whose condition
                %   never succeeds,
                % - in the else part of an if-then-else whose condition
                %   never fails.
                % We record the callees of the deleted calls so that
                % dead procedure analysis does not generate warnings
                % for these procedures, or the other procedures reachable
                % from them.
                psi_deleted_call_callees        :: set(pred_proc_id),

                %-----------------------------------------------------------%
                % Flags that record simple properties of the procedure.
                %-----------------------------------------------------------%

                % Is the address of this procedure taken? If yes, we will
                % need to use typeinfo liveness for them, so that deep_copy
                % and accurate gc have the RTTI they need for copying closures.
                %
                % Note that any non-local procedure must be considered
                % as having its address taken, since it is possible that
                % some other module may do so.
                psi_is_address_taken            :: is_address_taken,

                % Is the procedure mentioned in any foreign_export pragma,
                % regardless of what the current supported foreign languages
                % are?
                psi_has_any_foreign_exports     :: proc_foreign_exports,

                % Does this procedure contain parallel conjunction?
                % If yes, it should be run through the dependent parallel
                % conjunction transformation.
                %
                % This slot is set by the simplification pass.
                % Note that after some optimization passes, this flag
                % may be a conservative approximation.
                psi_proc_has_parallel_conj      :: has_parallel_conj,

                % Does this procedure contain a user event?
                %
                % This slot is set by the simplification pass.
                psi_proc_has_user_event         :: has_user_event,

                psi_proc_has_tail_rec_call      :: has_tail_rec_call,

                % Is the procedure mentioned in any order-independent-state-
                % update pragmas? If yes, list the role of this procedure
                % for the each of the types in those pragmas.
                psi_oisu_kind_fors              :: list(oisu_pred_kind_for),

                % Has the user requested (via a require_tail_recursion
                % pragma) that we suppress or enable warnings about tail
                % recursion for this procedure?
                psi_maybe_require_tailrec   :: maybe(require_tail_recursion),

                %-----------------------------------------------------------%
                % Information needed by the LLDS code generator.
                %-----------------------------------------------------------%

                % The head variables which must be forced to use regular
                % registers by the calling convention, despite having type
                % float. This is only meaningful with float registers.
                psi_reg_r_headvars              :: set_of_progvar,

                % The calling convention of each argument: information computed
                % by arg_info.m (based on the modes etc.) and used by code
                % generation to determine how each argument should be passed.
                psi_maybe_arg_info              :: maybe(list(arg_info)),

                psi_maybe_special_return        :: maybe(special_proc_return),

                % The initial liveness, for code generation.
                psi_initial_liveness            :: liveness_info,

                % Allocation of variables to stack slots.
                psi_stack_slots                 :: stack_slots,

                % True iff tracing is enabled, this is a procedure that lives
                % on the det stack, and the code of this procedure may create
                % a frame on the det stack. (Only in these circumstances do we
                % need to reserve a stack slot to hold the value of maxfr
                % at the call, for use in implementing retry.) This slot
                % is used only with the LLDS backend XXX. Its value is set
                % during the live_vars pass; it is invalid before then.
                psi_needs_maxfr_slot            :: needs_maxfr_slot,

                %-----------------------------------------------------------%
                % Information needed for tabling.
                %-----------------------------------------------------------%

                % If the procedure's evaluation method is memo, loopcheck or
                % minimal, this slot identifies the variable that holds the tip
                % of the call table. Otherwise, this field will be set to `no'.
                %
                % Tabled procedures record, in the data structure identified
                % by this variable, that the call is active. When performing
                % a retry across such a procedure, we must reset the state
                % of the call; if we don't, the retried call will find the
                % active call and report an infinite loop error.
                %
                % Such resetting of course requires the debugger to know
                % whether the procedure has reached the call table tip yet.
                % Therefore when binding this variable, the code generator
                % of the relevant backend must record this fact in a place
                % accessible to the debugger, if debugging is enabled.
                psi_call_table_tip              :: maybe(prog_var),

                % If set, it means that procedure has been subject to the I/O
                % tabling transformation. The argument will contain all the
                % information we need to display I/O actions involving
                % this procedure.
                %
                % (If the procedure has been subject to other kinds of tabling
                % transformations, the corresponding information will be
                % recorded in a map in the module_info.)
                % XXX For now, the compiler fully supports only procedures
                % whose arguments are all either ints, floats or strings.
                % However, this is still sufficient for debugging most problems
                % in the tabling system.
                psi_maybe_table_io_info         :: maybe(proc_table_io_info),

                psi_table_attributes            :: maybe(table_attributes),

                %-----------------------------------------------------------%
                % Information needed for deep profiling.
                %-----------------------------------------------------------%

                psi_maybe_deep_prof_info      :: maybe(deep_profile_proc_info),

                %-----------------------------------------------------------%
                % The results of program analyses.
                %-----------------------------------------------------------%

                % Information about the relative sizes of the input and output
                % args of the procedure. Set by termination analysis.
                psi_maybe_arg_sizes             :: maybe(arg_size_info),

                % The termination properties of the procedure.
                % Set by termination analysis.
                psi_maybe_termination           :: maybe(termination_info),

                % Termination properties and argument size constraints for
                % the procedure. Set by termination2 analysis.
                psi_termination2                :: termination2_info,

                % The results of the analyses in exception_analysis.m,
                % trailing_analysis.m and tabling_analysis, if available.
                psi_exception_info              :: maybe(proc_exception_info),
                psi_trailing_info               :: maybe(proc_trailing_info),
                psi_mm_tabling_info             :: maybe(proc_mm_tabling_info),

                % Structure sharing information as obtained by the structure
                % sharing analysis.
                psi_structure_sharing           :: structure_sharing_info,

                % Structure reuse conditions obtained by the structure reuse
                % analysis (CTGC).
                psi_structure_reuse             :: structure_reuse_info
        ).

:- type structure_sharing_info
    --->    structure_sharing_info(
                maybe_sharing   :: maybe(structure_sharing_domain_and_status),
                maybe_imported_sharing      :: maybe(imported_sharing)
                % Records the sharing information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed at
                % the beginning of structure sharing analysis. After that,
                % this field is of no use.
            ).

    % Sharing information is expressed in terms of head variables and the
    % type variables occurring in their types. In order to correctly process
    % (mainly renaming) this information, we need both the list of head
    % variables as well as their types. As this list of head variables may
    % contain any compiler-added head variables, the processing of imported
    % structure sharing information needs to be postponed until the actual
    % structure sharing analysis, which explains the need for the type
    % imported_sharing to temporarily store the imported sharing information.
    %
:- type imported_sharing
    --->    imported_sharing(
                % The list of head variables in which terms the imported
                % sharing is expressed.
                s_headvars        :: prog_vars,

                % The types of the head variables.
                s_types           :: list(mer_type),

                s_sharing         :: structure_sharing_domain
            ).

:- func structure_sharing_info_init = structure_sharing_info.

structure_sharing_info_init = structure_sharing_info(no, no).

:- type structure_reuse_info
    --->    structure_reuse_info(
                maybe_reuse     :: maybe(structure_reuse_domain_and_status),

                maybe_imported_reuse  :: maybe(imported_reuse)
                % Records the reuse information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed
                % at the beginning of structure reuse analysis. After that
                % this field is of no use.
            ).

    % Same rationale as for imported_sharing.
    %
:- type imported_reuse
    --->    imported_reuse(
                % The list of headvars in which terms the imported reuse
                % information is expressed.
                r_headvars        :: prog_vars,

                % The types of the headvars.
                r_types           :: list(mer_type),

                r_reuse           :: structure_reuse_domain
            ).

:- func structure_reuse_info_init = structure_reuse_info.

structure_reuse_info_init = structure_reuse_info(no, no).

table_step_stats_kind(Step) = KindStr :-
    (
        ( Step = table_trie_step_int(_)
        ; Step = table_trie_step_char
        ; Step = table_trie_step_string
        ; Step = table_trie_step_float
        ; Step = table_trie_step_typeinfo
        ; Step = table_trie_step_typeclassinfo
        ; Step = table_trie_step_foreign_enum
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_HASH"
    ;
        Step = table_trie_step_enum(_),
        KindStr = "MR_TABLE_STATS_DETAIL_ENUM"
    ;
        Step = table_trie_step_general(_Type, IsPoly, ValueOrAddr),
        (
            ValueOrAddr = table_addr,
            KindStr = "MR_TABLE_STATS_DETAIL_HASH"
        ;
            ValueOrAddr = table_value,
            (
                IsPoly = table_is_mono,
                KindStr = "MR_TABLE_STATS_DETAIL_DU"
            ;
                IsPoly = table_is_poly,
                KindStr = "MR_TABLE_STATS_DETAIL_POLY"
            )
        )
    ;
        ( Step = table_trie_step_promise_implied
        ; Step = table_trie_step_dummy
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_NONE"
    ).

proc_info_init(MainContext, ItemNumber, Arity, Types, DeclaredModes, Modes,
        MaybeArgLives, DetismDecl, MaybeDeclaredDetism, IsAddressTaken,
        HasParallelConj, VarNameRemap, ProcInfo) :-
    % When this predicate is invoked during the construction of the HLDS,
    % some parts of the procedure aren't known yet. In that case, we can
    % simply initialize them to any old garbage which we will later throw away.
    %
    % However, when this predicate is invoked by HLDS transformation passes
    % after the front-end has finished, this strategy won't work. We need
    % to fill in every field with meaningful, correct information, unless
    % we know for sure that before the next pass that needs the correct value
    % in a field, we will invoke another pass that fills in the correct value
    % in that field.
    %
    % XXX I (zs) am far from sure that all the field initializations below,
    % in this predicate and in proc_info_create_with_declared_detism,
    % fulfill this condition.

    % Please use a variable for every field of the proc_info and proc_sub_info,
    % and please keep the definitions of those variables in the same order
    % as the fields themselves.

    % argument MainContext
    % argument ItemNumber
    CanProcess = can_process_now,
    % argument DetismDecl
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(TraceGoalProcs),
    % argument IsAddressTaken
    HasForeignProcExports = no_foreign_exports,
    % argument HasParallelConj
    HasUserEvent = has_no_user_event,
    HasTailCallEvent = has_tail_rec_call(has_no_self_tail_rec_call,
        has_no_mutual_tail_rec_call),
    OisuKinds = [],
    MaybeRequireTailRecursion = no,
    set_of_var.init(RegR_HeadVars),
    MaybeArgPassInfo = no `with_type` maybe(list(arg_info)),
    MaybeSpecialReturn = no `with_type` maybe(special_proc_return),
    set_of_var.init(InitialLiveness),
    map.init(StackSlots),
    NeedsMaxfrSlot = does_not_need_maxfr_slot,
    MaybeCallTableTip = no `with_type` maybe(prog_var),
    MaybeTableIOInfo = no `with_type` maybe(proc_table_io_info),
    MaybeTableAttrs = no `with_type` maybe(table_attributes),
    MaybeDeepProfProcInfo = no `with_type` maybe(deep_profile_proc_info),
    MaybeArgSizes = no `with_type` maybe(arg_size_info),
    MaybeTermInfo = no `with_type` maybe(termination_info),
    Term2Info = term_constr_main_types.term2_info_init,
    MaybeExceptionInfo = no `with_type` maybe(proc_exception_info),
    MaybeTrailingInfo = no `with_type` maybe(proc_trailing_info),
    MaybeMMTablingInfo = no `with_type` maybe(proc_mm_tabling_info),
    SharingInfo = structure_sharing_info_init,
    ReuseInfo = structure_reuse_info_init,

    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        CanProcess,
        DetismDecl,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        TraceGoalProcs,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        NeedsMaxfrSlot,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingInfo,
        ReuseInfo),

    make_n_fresh_vars("HeadVar__", Arity, HeadVars, varset.init, BodyVarSet),
    goal_info_init(GoalInfo),
    BodyGoal = hlds_goal(conj(plain_conj, []), GoalInfo),
    vartypes_from_corresponding_lists(HeadVars, Types, BodyTypes),
    rtti_varmaps_init(RttiVarMaps),
    varset.init(InstVarSet),
    % argument DeclaredModes
    % argument Modes
    MaybeHeadModesConstr = no `with_type` maybe(mode_constraint),
    % argument MaybeArgLives
    % argument MaybeDeclaredDetism
    % Inferred determinism gets initialized to `erroneous'.
    % This is what `det_analysis.m' wants. det_analysis.m
    % will later provide the correct inferred determinism for it.
    InferredDetism = detism_erroneous,
    EvalMethod = eval_normal,
    ModeErrors = [],

    ProcInfo = proc_info(
        HeadVars,
        BodyGoal,
        BodyVarSet,
        BodyTypes,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeHeadModesConstr,
        MaybeArgLives,
        MaybeDeclaredDetism,
        InferredDetism,
        EvalMethod,
        ModeErrors,
        ProcSubInfo).

proc_info_create(Context, ItemNumber, VarSet, VarTypes, HeadVars,
        InstVarSet, HeadModes, DetismDecl, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarSet, VarTypes, HeadVars, InstVarSet, HeadModes,
        DetismDecl, yes(Detism), Detism, Goal, RttiVarMaps, IsAddressTaken,
        HasParallelConj, VarNameRemap, ProcInfo).

:- pred proc_info_create_with_declared_detism(prog_context::in, int::in,
    prog_varset::in, vartypes::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, maybe(determinism)::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

proc_info_create_with_declared_detism(MainContext, ItemNumber,
        VarSet, VarTypes, HeadVars, InstVarSet, Modes,
        DetismDecl, MaybeDeclaredDetism, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    % See the comment at the top of  proc_info_init; it applies here as well.

    % Please use a variable for every field of the proc_info and proc_sub_info,
    % and please keep the definitions of those variables in the same order
    % as the fields themselves.

    % argument MainContext
    % argument ItemNumber
    CanProcess = can_process_now,
    % argument DetismDecl
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(TraceGoalProcs),
    % argument IsAddressTaken
    HasForeignProcExports = no_foreign_exports,
    % argument HasParallelConj
    HasUserEvent = has_no_user_event,
    HasTailCallEvent = has_tail_rec_call(has_no_self_tail_rec_call,
        has_no_mutual_tail_rec_call),
    OisuKinds = [],
    MaybeRequireTailRecursion = no,
    set_of_var.init(RegR_HeadVars),
    MaybeArgPassInfo = no `with_type` maybe(list(arg_info)),
    MaybeSpecialReturn = no `with_type` maybe(special_proc_return),
    set_of_var.init(InitialLiveness),
    map.init(StackSlots),
    NeedsMaxfrSlot = does_not_need_maxfr_slot,
    MaybeCallTableTip = no `with_type` maybe(prog_var),
    MaybeTableIOInfo = no `with_type` maybe(proc_table_io_info),
    MaybeTableAttrs = no `with_type` maybe(table_attributes),
    MaybeDeepProfProcInfo = no `with_type` maybe(deep_profile_proc_info),
    MaybeArgSizes = no `with_type` maybe(arg_size_info),
    MaybeTermInfo = no `with_type` maybe(termination_info),
    Term2Info = term_constr_main_types.term2_info_init,
    MaybeExceptionInfo = no `with_type` maybe(proc_exception_info),
    MaybeTrailingInfo = no `with_type` maybe(proc_trailing_info),
    MaybeMMTablingInfo = no `with_type` maybe(proc_mm_tabling_info),
    SharingInfo = structure_sharing_info_init,
    ReuseInfo = structure_reuse_info_init,

    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        CanProcess,
        DetismDecl,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        TraceGoalProcs,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        NeedsMaxfrSlot,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingInfo,
        ReuseInfo),

    % argument HeadVars
    % argument Goal
    % argument VarSet
    % argument VarTypes
    % argument RttiVarMaps
    % argument InstVarSet
    DeclaredModes = no,
    % argument Modes
    MaybeHeadModesConstr = no `with_type` maybe(mode_constraint),
    MaybeArgLives = no,
    % argument MaybeDeclaredDetism
    % argument Detism
    EvalMethod = eval_normal,
    ModeErrors = [],

    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarSet,
        VarTypes,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeHeadModesConstr,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ModeErrors,
        ProcSubInfo).

proc_info_set_body(VarSet, VarTypes, HeadVars, Goal, RttiVarMaps, !ProcInfo) :-
    !ProcInfo ^ proc_prog_varset := VarSet,
    !ProcInfo ^ proc_var_types := VarTypes,
    !ProcInfo ^ proc_head_vars := HeadVars,
    !ProcInfo ^ proc_body := Goal,
    !ProcInfo ^ proc_rtti_varmaps := RttiVarMaps.

proc_info_get_headvars(PI, X) :-
    X = PI ^ proc_head_vars.
proc_info_get_goal(PI, X) :-
    X = PI ^ proc_body.
proc_info_get_varset(PI, X) :-
    X = PI ^ proc_prog_varset.
proc_info_get_vartypes(PI, X) :-
    X = PI ^ proc_var_types.
proc_info_get_rtti_varmaps(PI, X) :-
    X = PI ^ proc_rtti_varmaps.
proc_info_get_inst_varset(PI, X) :-
    X = PI ^ proc_inst_varset.
proc_info_get_maybe_declared_argmodes(PI, X) :-
    X = PI ^ proc_maybe_decl_head_modes.
proc_info_get_argmodes(PI, X) :-
    X = PI ^ proc_actual_head_modes.
proc_info_get_maybe_arglives(PI, X) :-
    X = PI ^ proc_headvar_caller_liveness.
proc_info_get_declared_determinism(PI, X) :-
    X = PI ^ proc_declared_detism.
proc_info_get_inferred_determinism(PI, X) :-
    X = PI ^ proc_inferred_detism.
proc_info_get_eval_method(PI, X) :-
    X = PI ^ proc_eval_method.
proc_info_get_mode_errors(PI, X) :-
    X = PI ^ proc_mode_errors.

proc_info_get_context(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_context.
proc_info_get_item_number(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_item_number.
proc_info_get_can_process(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_can_process.
proc_info_get_detism_decl(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_detism_decl.
proc_info_get_maybe_untuple_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_untuple_info.
proc_info_get_var_name_remap(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_var_name_remap.
proc_info_get_statevar_warnings(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_statevar_warnings.
proc_info_get_deleted_call_callees(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_deleted_call_callees.
proc_info_get_is_address_taken(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_is_address_taken.
proc_info_get_has_any_foreign_exports(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_has_any_foreign_exports.
proc_info_get_has_parallel_conj(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_parallel_conj.
proc_info_get_has_user_event(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_user_event.
proc_info_get_has_tail_rec_call(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_tail_rec_call.
proc_info_get_maybe_require_tailrec_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_require_tailrec.
proc_info_get_oisu_kind_fors(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_oisu_kind_fors.
proc_info_get_reg_r_headvars(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_reg_r_headvars.
proc_info_get_maybe_arg_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_arg_info.
proc_info_get_maybe_special_return(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_special_return.
proc_info_get_liveness_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_initial_liveness.
proc_info_get_stack_slots(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_stack_slots.
proc_info_get_needs_maxfr_slot(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_needs_maxfr_slot.
proc_info_get_call_table_tip(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_call_table_tip.
proc_info_get_maybe_proc_table_io_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_table_io_info.
proc_info_get_table_attributes(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_table_attributes.
proc_info_get_maybe_deep_profile_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_deep_prof_info.
proc_info_get_maybe_arg_size_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_arg_sizes.
proc_info_get_maybe_termination_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_termination.
proc_info_get_termination2_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_termination2.
proc_info_get_exception_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_exception_info.
proc_info_get_trailing_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_trailing_info.
proc_info_get_mm_tabling_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_mm_tabling_info.

proc_info_set_headvars(X, !PI) :-
    !PI ^ proc_head_vars := X.
proc_info_set_goal(X, !PI) :-
    !PI ^ proc_body := X.
proc_info_set_varset(X, !PI) :-
    !PI ^ proc_prog_varset := X.
proc_info_set_vartypes(X, !PI) :-
    !PI ^ proc_var_types := X.
proc_info_set_rtti_varmaps(X, !PI) :-
    !PI ^ proc_rtti_varmaps := X.
proc_info_set_inst_varset(X, !PI) :-
    !PI ^ proc_inst_varset := X.
proc_info_set_maybe_declared_argmodes(X, !PI) :-
    !PI ^ proc_maybe_decl_head_modes := X.
proc_info_set_argmodes(X, !PI) :-
    !PI ^ proc_actual_head_modes := X.
proc_info_set_head_modes_constraint(X, !PI) :-
    !PI ^ proc_maybe_head_modes_constr := yes(X).
proc_info_set_maybe_arglives(X, !PI) :-
    !PI ^ proc_headvar_caller_liveness := X.
proc_info_set_inferred_determinism(X, !PI) :-
    !PI ^ proc_inferred_detism := X.
proc_info_set_eval_method(X, !PI) :-
    !PI ^ proc_eval_method := X.
proc_info_set_mode_errors(X, !PI) :-
    !PI ^ proc_mode_errors := X.

proc_info_set_can_process(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_can_process := X.
proc_info_set_detism_decl(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_detism_decl := X.
proc_info_set_maybe_untuple_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_untuple_info := X.
proc_info_set_var_name_remap(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_var_name_remap := X.
proc_info_set_statevar_warnings(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_statevar_warnings := X.
proc_info_set_deleted_call_callees(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_deleted_call_callees := X.
proc_info_set_address_taken(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_is_address_taken := X.
proc_info_set_has_any_foreign_exports(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_has_any_foreign_exports := X.
proc_info_set_has_parallel_conj(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_parallel_conj := X.
proc_info_set_has_user_event(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_user_event := X.
proc_info_set_has_tail_rec_call(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_tail_rec_call := X.
proc_info_set_oisu_kind_fors(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_oisu_kind_fors := X.
proc_info_set_require_tailrec_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_require_tailrec := yes(X).
proc_info_set_reg_r_headvars(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_reg_r_headvars := X.
proc_info_set_arg_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_arg_info := yes(X).
proc_info_set_maybe_special_return(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_special_return := X.
proc_info_set_liveness_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_initial_liveness := X.
proc_info_set_stack_slots(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_stack_slots := X.
proc_info_set_needs_maxfr_slot(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_needs_maxfr_slot := X.
proc_info_set_call_table_tip(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_call_table_tip := X.
proc_info_set_maybe_proc_table_io_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_table_io_info := X.
proc_info_set_table_attributes(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_table_attributes := X.
proc_info_set_maybe_deep_profile_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_deep_prof_info := X.
proc_info_set_maybe_arg_size_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_arg_sizes := X.
proc_info_set_maybe_termination_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_termination := X.
proc_info_set_termination2_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_termination2 := X.
proc_info_set_exception_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_exception_info := X.
proc_info_set_trailing_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_trailing_info := X.
proc_info_set_mm_tabling_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_mm_tabling_info := X.

proc_info_head_modes_constraint(ProcInfo, HeadModesConstraint) :-
    MaybeHeadModesConstraint = ProcInfo ^ proc_maybe_head_modes_constr,
    (
        MaybeHeadModesConstraint = yes(HeadModesConstraint)
    ;
        MaybeHeadModesConstraint = no,
        unexpected($module, $pred, "no constraint")
    ).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    mode_list_get_initial_insts(ModuleInfo, ArgModes, InitialInsts),
    assoc_list.from_corresponding_lists(HeadVars, InitialInsts, InstAL),
    InstMap = instmap_from_assoc_list(InstAL).

proc_info_declared_argmodes(ProcInfo, ArgModes) :-
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes1),
        ArgModes = ArgModes1
    ;
        MaybeArgModes = no,
        proc_info_get_argmodes(ProcInfo, ArgModes)
    ).

proc_info_interface_determinism(ProcInfo, Determinism) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDeterminism),
    (
        MaybeDeterminism = no,
        proc_info_get_inferred_determinism(ProcInfo, Determinism)
    ;
        MaybeDeterminism = yes(Determinism)
    ).

    % Return Result = yes if the called predicate is known to never succeed.
    %
proc_info_never_succeeds(ProcInfo, NeverSucceeds) :-
    proc_info_get_declared_determinism(ProcInfo, DeclaredDeterminism),
    (
        DeclaredDeterminism = no,
        NeverSucceeds = no
    ;
        DeclaredDeterminism = yes(Determinism),
        determinism_components(Determinism, _, MaxSoln),
        (
            MaxSoln = at_most_zero,
            NeverSucceeds = yes
        ;
            ( MaxSoln = at_most_one
            ; MaxSoln = at_most_many
            ; MaxSoln = at_most_many_cc
            ),
            NeverSucceeds = no
        )
    ).

proc_info_arglives(ProcInfo, ModuleInfo, ArgLives) :-
    proc_info_get_maybe_arglives(ProcInfo, MaybeArgLives),
    (
        MaybeArgLives = yes(ArgLives0),
        ArgLives = ArgLives0
    ;
        MaybeArgLives = no,
        proc_info_get_argmodes(ProcInfo, Modes),
        get_arg_lives(ModuleInfo, Modes, ArgLives)
    ).

proc_info_is_valid_mode(ProcInfo) :-
    proc_info_get_mode_errors(ProcInfo, ModeErrors),
    ModeErrors = [].

proc_info_arg_info(ProcInfo, ArgInfo) :-
    proc_info_get_maybe_arg_info(ProcInfo, MaybeArgInfo0),
    (
        MaybeArgInfo0 = yes(ArgInfo)
    ;
        MaybeArgInfo0 = no,
        unexpected($module, $pred, "arg_info not set")
    ).

proc_info_get_structure_sharing(ProcInfo, MaybeSharing) :-
    MaybeSharing = ProcInfo ^ proc_sub_info ^ psi_structure_sharing
        ^ maybe_sharing.

proc_info_set_structure_sharing(Sharing, !ProcInfo) :-
    !ProcInfo ^ proc_sub_info ^ psi_structure_sharing ^ maybe_sharing :=
        yes(Sharing).

proc_info_get_imported_structure_sharing(ProcInfo, HeadVars, Types, Sharing) :-
    MaybeImportedSharing = ProcInfo ^ proc_sub_info ^ psi_structure_sharing
        ^ maybe_imported_sharing,
    MaybeImportedSharing = yes(ImportedSharing),
    ImportedSharing = imported_sharing(HeadVars, Types, Sharing).

proc_info_set_imported_structure_sharing(HeadVars, Types, Sharing,
        !ProcInfo) :-
    ImportedSharing = imported_sharing(HeadVars, Types, Sharing),
    MaybeImportedSharing = yes(ImportedSharing),
    !ProcInfo ^ proc_sub_info ^ psi_structure_sharing
        ^ maybe_imported_sharing := MaybeImportedSharing.

proc_info_reset_imported_structure_sharing(!ProcInfo) :-
    !ProcInfo ^ proc_sub_info ^ psi_structure_sharing
        ^ maybe_imported_sharing := no.

proc_info_get_structure_reuse(ProcInfo, MaybeReuse) :-
    MaybeReuse = ProcInfo ^ proc_sub_info ^ psi_structure_reuse ^ maybe_reuse.

proc_info_set_structure_reuse(Reuse, !ProcInfo) :-
    !ProcInfo ^ proc_sub_info ^ psi_structure_reuse ^ maybe_reuse
        := yes(Reuse).

proc_info_get_imported_structure_reuse(ProcInfo, HeadVars, Types, Reuse) :-
    MaybeImportedReuse = ProcInfo ^ proc_sub_info ^ psi_structure_reuse
        ^ maybe_imported_reuse,
    MaybeImportedReuse = yes(ImportedReuse),
    ImportedReuse = imported_reuse(HeadVars, Types, Reuse).

proc_info_set_imported_structure_reuse(HeadVars, Types, Reuse, !ProcInfo) :-
    ImportedReuse = imported_reuse(HeadVars, Types, Reuse),
    MaybeImportedReuse = yes(ImportedReuse),
    !ProcInfo ^ proc_sub_info ^ psi_structure_reuse ^ maybe_imported_reuse :=
        MaybeImportedReuse.

proc_info_reset_imported_structure_reuse(!ProcInfo) :-
    !ProcInfo ^ proc_sub_info ^ psi_structure_reuse
        ^ maybe_imported_reuse := no.

proc_info_ensure_unique_names(!ProcInfo) :-
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    vartypes_vars(VarTypes, AllVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    varset.ensure_unique_names(AllVars, "p", VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

proc_info_create_var_from_type(Type, MaybeName, NewVar, !ProcInfo) :-
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    varset.new_maybe_named_var(MaybeName, NewVar, VarSet0, VarSet),
    add_var_type(NewVar, Type, VarTypes0, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_create_vars_from_types(Types, NewVars, !ProcInfo) :-
    list.length(Types, NumVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    varset.new_vars(NumVars, NewVars, VarSet0, VarSet),
    vartypes_add_corresponding_lists(NewVars, Types, VarTypes0, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_instantiated_head_vars(ModuleInfo, ProcInfo, ChangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstChanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        lookup_var_type(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        not inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list.filter_map(IsInstChanged, HeadVarModes, ChangedInstHeadVars).

proc_info_uninstantiated_head_vars(ModuleInfo, ProcInfo,
        UnchangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstUnchanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        lookup_var_type(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list.filter_map(IsInstUnchanged, HeadVarModes, UnchangedInstHeadVars).

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
        InterfaceTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( if no_type_info_builtin(PredModule, PredName, PredArity) then
        InterfaceTypeInfoLiveness = no
    else
        pred_info_get_status(PredInfo, Status),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_is_address_taken(ProcInfo, IsAddressTaken),
        non_special_interface_should_use_typeinfo_liveness(Status,
            IsAddressTaken, Globals, InterfaceTypeInfoLiveness)
    ).

    % Return true if the interface of a procedure in a non-special predicate
    % with the given characteristics (import/export/local status,
    % address taken status) must include typeinfos for all the type variables
    % in the types of the arguments.
    %
    % Note that only a few predicates in the builtin modules are special
    % in this sense, and that compiler-generated predicates are never special.
    %
:- pred non_special_interface_should_use_typeinfo_liveness(pred_status::in,
    is_address_taken::in, globals::in, bool::out) is det.

non_special_interface_should_use_typeinfo_liveness(PredStatus, IsAddressTaken,
        Globals, InterfaceTypeInfoLiveness) :-
    ( if
        (
            IsAddressTaken = address_is_taken
        ;
            % If the predicate is exported, its address may have
            % been taken elsewhere. If it is imported, then it
            % follows that it must be exported somewhere.
            PredStatus \= pred_status(status_local)
        ;
            % If term size profiling (of either form) is enabled,
            % then we may need to access the typeinfo of any
            % variable bound to a heap cell argument. The only way
            % to ensure that this is possible is to preserve the
            % ability to access the typeinfo of any variable.
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        ;
            non_special_body_should_use_typeinfo_liveness(Globals, yes)
        )
    then
        InterfaceTypeInfoLiveness = yes
    else
        InterfaceTypeInfoLiveness = no
    ).

body_should_use_typeinfo_liveness(PredInfo, Globals, BodyTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( if no_type_info_builtin(PredModule, PredName, PredArity) then
        BodyTypeInfoLiveness = no
    else
        non_special_body_should_use_typeinfo_liveness(Globals,
            BodyTypeInfoLiveness)
    ).

    % Return true if the body of a procedure in a non-special predicate
    % must keep a typeinfo variable alive during the lifetime of all variables
    % whose type includes the corresponding type variable.
    %
:- pred non_special_body_should_use_typeinfo_liveness(globals::in,
    bool::out) is det.

non_special_body_should_use_typeinfo_liveness(Globals, BodyTypeInfoLiveness) :-
    globals.lookup_bool_option(Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness).

proc_info_has_io_state_pair(ModuleInfo, ProcInfo, InArgNum, OutArgNum) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars,
        ArgModes, VarTypes, InArgNum, OutArgNum).

proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars, ArgModes,
        VarTypes, InArgNum, OutArgNum) :-
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarsModes),
    proc_info_has_io_state_pair_2(HeadVarsModes, ModuleInfo, VarTypes,
        1, no, MaybeIn, no, MaybeOut),
    ( if
        MaybeIn = yes(In),
        MaybeOut = yes(Out)
    then
        InArgNum = In,
        OutArgNum = Out
    else
        fail
    ).

:- pred proc_info_has_io_state_pair_2(assoc_list(prog_var, mer_mode)::in,
    module_info::in, vartypes::in, int::in,
    maybe(int)::in, maybe(int)::out, maybe(int)::in, maybe(int)::out)
    is semidet.

proc_info_has_io_state_pair_2([], _, _, _, !MaybeIn, !MaybeOut).
proc_info_has_io_state_pair_2([Var - Mode | VarModes], ModuleInfo, VarTypes,
        ArgNum, !MaybeIn, !MaybeOut) :-
    ( if
        lookup_var_type(VarTypes, Var, VarType),
        type_is_io_state(VarType)
    then
        ( if mode_is_fully_input(ModuleInfo, Mode) then
            (
                !.MaybeIn = no,
                !:MaybeIn = yes(ArgNum)
            ;
                !.MaybeIn = yes(_),
                % Procedures with two input arguments of type io.state
                % (e.g. the automatically generated unification or comparison
                % procedure for the io.state type) do not fall into the
                % one input/one output pattern we are looking for.
                fail
            )
        else if mode_is_fully_output(ModuleInfo, Mode) then
            (
                !.MaybeOut = no,
                !:MaybeOut = yes(ArgNum)
            ;
                !.MaybeOut = yes(_),
                % Procedures with two output arguments of type io.state
                % do not fall into the one input/one output pattern we are
                % looking for.
                fail
            )
        else
            fail
        )
    else
        true
    ),
    proc_info_has_io_state_pair_2(VarModes, ModuleInfo, VarTypes,
        ArgNum + 1, !MaybeIn, !MaybeOut).

proc_info_has_higher_order_arg_from_details(ModuleInfo, VarTypes,
        [HeadVar | HeadVars]) :-
    (
        lookup_var_type(VarTypes, HeadVar, VarType),
        type_is_higher_order(VarType)
    ;
        proc_info_has_higher_order_arg_from_details(ModuleInfo, VarTypes,
            HeadVars)
    ).

clone_proc_id(ProcTable, _ProcId, CloneProcId) :-
    find_lowest_unused_proc_id(ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id(proc_table::in, proc_id::out) is det.

find_lowest_unused_proc_id(ProcTable, CloneProcId) :-
    find_lowest_unused_proc_id_2(0, ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id_2(proc_id::in, proc_table::in, proc_id::out)
    is det.

find_lowest_unused_proc_id_2(TrialProcId, ProcTable, CloneProcId) :-
    ( if map.search(ProcTable, TrialProcId, _) then
        find_lowest_unused_proc_id_2(TrialProcId + 1, ProcTable, CloneProcId)
    else
        CloneProcId = TrialProcId
    ).

ensure_all_headvars_are_named(!ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    ensure_all_headvars_are_named_2(HeadVars, 1, VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

:- pred ensure_all_headvars_are_named_2(list(prog_var)::in, int::in,
    prog_varset::in, prog_varset::out) is det.

ensure_all_headvars_are_named_2([], _, !VarSet).
ensure_all_headvars_are_named_2([Var | Vars], SeqNum, !VarSet) :-
    ( if varset.search_name(!.VarSet, Var, _Name) then
        true
    else
        Name = "HeadVar__" ++ int_to_string(SeqNum),
        varset.name_var(Var, Name, !VarSet)
    ),
    ensure_all_headvars_are_named_2(Vars, SeqNum + 1, !VarSet).

var_is_of_dummy_type(ModuleInfo, VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, Type),
    check_dummy_type(ModuleInfo, Type) = is_dummy_type.

var_is_of_non_dummy_type(ModuleInfo, VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, Type),
    check_dummy_type(ModuleInfo, Type) = is_not_dummy_type.

%-----------------------------------------------------------------------------%

    % Predicates to deal with record syntax.

:- interface.

    % field_extraction_function_args(Args, InputTermArg).
    % Work out which arguments of a field access correspond to the
    % field being extracted/set, and which are the container arguments.
    %
:- pred field_extraction_function_args(list(prog_var)::in, prog_var::out)
    is det.

    % field_update_function_args(Args, InputTermArg, FieldArg).
    %
:- pred field_update_function_args(list(prog_var)::in, prog_var::out,
    prog_var::out) is det.

    % field_access_function_name(AccessType, FieldName, FuncName).
    %
    % From the access type and the name of the field,
    % construct a function name.
    %
:- pred field_access_function_name(field_access_type::in, sym_name::in,
    sym_name::out) is det.

    % is_field_access_function_name(ModuleInfo, FuncName, Arity,
    %   AccessType, FieldName).
    %
    % Inverse of the above.
    %
:- pred is_field_access_function_name(module_info::in, sym_name::in,
    arity::out, field_access_type::out, sym_name::out) is semidet.

:- pred pred_info_is_field_access_function(module_info::in, pred_info::in)
    is semidet.

:- implementation.

field_extraction_function_args(Args, TermInputArg) :-
    ( if Args = [TermInputArg0] then
        TermInputArg = TermInputArg0
    else
        unexpected($module, $pred, "num_args != 1")
    ).

field_update_function_args(Args, TermInputArg, FieldArg) :-
    ( if Args = [TermInputArg0, FieldArg0] then
        FieldArg = FieldArg0,
        TermInputArg = TermInputArg0
    else
        unexpected($module, $pred, "num_args != 2")
    ).

field_access_function_name(get, FieldName, FieldName).
field_access_function_name(set, FieldName, FuncName) :-
    add_sym_name_suffix(FieldName, " :=", FuncName).

is_field_access_function_name(ModuleInfo, FuncName, Arity,
        AccessType, FieldName) :-
    ( if remove_sym_name_suffix(FuncName, " :=", FieldName0) then
        Arity = 2,
        AccessType = set,
        FieldName = FieldName0
    else
        Arity = 1,
        AccessType = get,
        FieldName = FuncName
    ),
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.contains(CtorFieldTable, FieldName).

pred_info_is_field_access_function(ModuleInfo, PredInfo) :-
    pred_info_is_pred_or_func(PredInfo) = pf_function,
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    adjust_func_arity(pf_function, FuncArity, PredArity),
    is_field_access_function_name(ModuleInfo, qualified(Module, Name),
        FuncArity, _, _).

%-----------------------------------------------------------------------------%

    % Predicates to deal with builtins.

:- interface.

    % is_unify_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler-generated instance of a type-specific unify predicate.
    %
:- pred is_unify_pred(pred_info::in) is semidet.

    % is_unify_or_compare_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler generated instance of a type-specific special_pred (i.e. one
    % of the unify, compare, or index predicates generated as a type-specific
    % instance of unify/2, index/2, or compare/3).
    %
:- pred is_unify_or_compare_pred(pred_info::in) is semidet.

    % Is the argument the pred_info for a builtin that can be generated inline?
    %
:- pred pred_info_is_builtin(pred_info::in) is semidet.

    % builtin_state(ModuleInfo, CallerPredId, PredId, ProcId, BuiltinState)
    %
    % Is the given procedure a builtin that should be generated inline
    % in the given caller?
    %
:- func builtin_state(module_info, pred_id, pred_id, proc_id) = builtin_state.

    % Succeeds iff PredInfo represents a promise of the given type.
    %
:- pred pred_info_is_promise(pred_info::in, promise_type::out) is semidet.

:- implementation.

is_unify_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_special_pred(spec_pred_unify, _TypeCtor).

is_unify_or_compare_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, origin_special_pred(_, _)). % XXX bug

pred_info_is_builtin(PredInfo) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    ProcId = initial_proc_id,
    is_inline_builtin(ModuleName, PredName, ProcId, Arity).

builtin_state(ModuleInfo, CallerPredId, PredId, ProcId) = BuiltinState :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    ( if
        % XXX This should ask: is this an inline builtin FOR THIS BACKEND?
        is_inline_builtin(ModuleName, PredName, ProcId, Arity),
        (
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, allow_inlining,
                AllowInlining),
            AllowInlining = yes,
            globals.lookup_bool_option(Globals, inline_builtins,
                InlineBuiltins),
            InlineBuiltins = yes
        ;
            % The "recursive" call in the automatically generated body
            % of each builtin predicate MUST be generated inline.
            % If it isn't generated inline, then any call to the predicate
            % form of the builtin would fall into an infinite loop.
            CallerPredId = PredId
        )
    then
        BuiltinState = inline_builtin
    else
        BuiltinState = not_builtin
    ).

:- pred is_inline_builtin(module_name::in, string::in, proc_id::in, arity::in)
    is semidet.

is_inline_builtin(ModuleName, PredName, ProcId, Arity) :-
    Arity =< 3,
    list.duplicate(Arity, 0, Args),
    builtin_ops.test_if_builtin(ModuleName, PredName, ProcId, Args).

pred_info_is_promise(PredInfo, PromiseType) :-
    pred_info_get_goal_type(PredInfo, goal_type_promise(PromiseType)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

    % Check if the given evaluation method is allowed with
    % the given determinism.
    %
:- func valid_determinism_for_eval_method(eval_method, determinism) = bool.

    % Return true if the given evaluation method requires a
    % stratification check.
    %
:- func eval_method_needs_stratification(eval_method) = bool.

    % Return true if the given evaluation method uses a per-procedure
    % tabling pointer. If so, the back-end must generate a declaration
    % for the variable to hold the table.
    %
:- func eval_method_has_per_proc_tabling_pointer(eval_method) = bool.

    % Return true if the given evaluation method requires the body
    % of the procedure using it to be transformed by table_gen.m.
    %
:- func eval_method_requires_tabling_transform(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be ground.
    %
:- func eval_method_requires_ground_args(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be non-unique.
    %
:- func eval_method_destroys_uniqueness(eval_method) = bool.

    % Return the change a given evaluation method can do to a given
    % determinism.
    %
:- func eval_method_change_determinism(eval_method, determinism) = determinism.

:- implementation.

valid_determinism_for_eval_method(eval_normal, _) = yes.
valid_determinism_for_eval_method(eval_loop_check, Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    (
        MaxSoln = at_most_zero,
        Valid = no
    ;
        ( MaxSoln = at_most_one
        ; MaxSoln = at_most_many
        ; MaxSoln = at_most_many_cc
        ),
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_memo, Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    (
        MaxSoln = at_most_zero,
        Valid = no
    ;
        ( MaxSoln = at_most_one
        ; MaxSoln = at_most_many
        ; MaxSoln = at_most_many_cc
        ),
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_table_io(_, _), _) = _ :-
    unexpected($module, $pred, "called after tabling phase").
valid_determinism_for_eval_method(eval_minimal(_), Detism) = Valid :-
    % The following reasons specify why a particular determinism is
    % incompatible with minimal model tabling.
    %
    % Reason 1:
    % Determinism analysis isn't yet smart enough to know whether
    % a cannot_fail execution path is guaranteed not to go through a call
    % to a predicate that is mutually recursive with this one, which (if this
    % predicate is minimal model) is the only way that the predicate can be
    % properly cannot_fail. The problem is that in general, the mutually
    % recursive predicate may be in another module.
    %
    % Reason 2:
    % The transformation, as currently implemented, assumes that it is possible
    % to reach the call table tip, and generates HLDS that refers to the
    % introduced variable representing this tip. This variable however, will
    % be optimized away if the code cannot succeed, causing a code generator
    % abort.
    %
    % Reason 3:
    % Minimal model semantics requires computing to a fixpoint, and this is
    % incompatible with the notion of committed choice.
    %
    % Reason 4:
    % Doing the analysis required to ensure that a predicate can't have more
    % than one solution is much harder if the predicate concerned is
    % minimal_model. In theory, this analysis could be done, but it would
    % take a lot of programming, and since there is a simple workaround
    % (make the predicate nondet, and check the number of solutions at the
    % caller), this would not be cost-effective.
    (
        Detism = detism_det,
        Valid = no                  % Reason 1
    ;
        Detism = detism_semi,
        Valid = no                  % Reason 4
    ;
        Detism = detism_multi,      % Reason 1
        Valid = yes
    ;
        Detism = detism_non,
        Valid = yes
    ;
        Detism = detism_cc_multi,   % Reason 3
        Valid = no
    ;
        Detism = detism_cc_non,     % Reason 3
        Valid = no
    ;
        Detism = detism_erroneous,  % Reason 2
        Valid = no
    ;
        Detism = detism_failure,    % Reason 2
        Valid = no
    ).

eval_method_needs_stratification(eval_normal) = no.
eval_method_needs_stratification(eval_loop_check) = no.
eval_method_needs_stratification(eval_table_io(_, _)) = no.
eval_method_needs_stratification(eval_memo) = no.
eval_method_needs_stratification(eval_minimal(_)) = yes.

eval_method_has_per_proc_tabling_pointer(eval_normal) = no.
eval_method_has_per_proc_tabling_pointer(eval_loop_check) = yes.
eval_method_has_per_proc_tabling_pointer(eval_table_io(_, _)) = no.
eval_method_has_per_proc_tabling_pointer(eval_memo) = yes.
eval_method_has_per_proc_tabling_pointer(eval_minimal(stack_copy)) = yes.
eval_method_has_per_proc_tabling_pointer(eval_minimal(own_stacks_consumer))
    = no.
eval_method_has_per_proc_tabling_pointer(eval_minimal(own_stacks_generator))
    = yes.

eval_method_requires_tabling_transform(eval_normal) = no.
eval_method_requires_tabling_transform(eval_loop_check) = yes.
eval_method_requires_tabling_transform(eval_table_io(_, _)) = yes.
eval_method_requires_tabling_transform(eval_memo) = yes.
eval_method_requires_tabling_transform(eval_minimal(_)) = yes.

eval_method_requires_ground_args(eval_normal) = no.
eval_method_requires_ground_args(eval_loop_check) = yes.
eval_method_requires_ground_args(eval_table_io(_, _)) = yes.
eval_method_requires_ground_args(eval_memo) = yes.
eval_method_requires_ground_args(eval_minimal(_)) = yes.

eval_method_destroys_uniqueness(eval_normal) = no.
eval_method_destroys_uniqueness(eval_loop_check) = yes.
eval_method_destroys_uniqueness(eval_table_io(_, _)) = no.
eval_method_destroys_uniqueness(eval_memo) = yes.
eval_method_destroys_uniqueness(eval_minimal(_)) = yes.

eval_method_change_determinism(eval_normal, Detism) = Detism.
eval_method_change_determinism(eval_loop_check, Detism) = Detism.
eval_method_change_determinism(eval_table_io(_, _), Detism) = Detism.
eval_method_change_determinism(eval_memo, Detism) = Detism.
eval_method_change_determinism(eval_minimal(_), Detism0) = Detism :-
    det_conjunction_detism(detism_semi, Detism0, Detism).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_pred.
%-----------------------------------------------------------------------------%
