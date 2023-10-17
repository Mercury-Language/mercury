%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with predicates
% and procedures.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_pred.
:- interface.

:- import_module analysis.
:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_promise.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_graph.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

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

    % These types are abstract exported to permit the proc_info fields
    % of these types to be part of the argument lists of proc_prepare_to_clone
    % and proc_create.
    %
:- type structure_sharing_info.
:- type structure_reuse_info.

:- type proc_table == map(proc_id, proc_info).

:- pred next_proc_id(proc_table::in, proc_id::out) is det.

:- type call_id
    --->    plain_call_id(pf_sym_name_arity)
    ;       generic_call_id(generic_call_id).

:- type generic_call_id
    --->    gcid_higher_order(purity, pred_or_func, pred_form_arity)
    ;       gcid_class_method(class_id, pf_sym_name_arity)
    ;       gcid_event_call(string)
    ;       gcid_cast(cast_kind).

:- type pred_proc_list == list(pred_proc_id).

%---------------------------------------------------------------------------%

:- type implementation_language
    --->    impl_lang_mercury
    ;       impl_lang_foreign(foreign_language).

    % A predicate, and the goal inside it, may implement a promise declaration,
    % or it may be an ordinary predicate.
:- type goal_type
    --->    goal_not_for_promise(np_goal_type)
    ;       goal_for_promise(promise_type).

    % An ordinary non-promise predicate may be defined by Mercury clauses,
    % foreign procs, both, or neither. (The last is the recorded situation
    % when we have added the predicate's declaration to the HLDS but have not
    % processed any clauses or foreign procs just yet.)
    %
    % We use this information in two ways.
    %
    % First, intermod.m needs to know whether a predicate's definition
    % contains any foreign_procs, because if it does, then it cannot append
    % variable numbers after variable names for disambiguation, in e.g. clause
    % heads, since that would screw up references to those variables in the
    % foreign code.
    %
    % Second, purity.m has special handling for predicates that are defined
    % *only* by foreign procs.
    %
    % Therefore the compiler does make a distinction between how it handles
    % np_goal_type_foreign and np_goal_type_clause_and_foreign.
    %
    % As it happens, the compiler makes no distinction between how it handles
    % np_goal_type_none and np_goal_type_clause, with the obvious exception
    % that adding a foreign proc to the two results in no_goal_types that
    % *are* distinguishable.
:- type np_goal_type
    --->    np_goal_type_none
    ;       np_goal_type_clause
    ;       np_goal_type_foreign
    ;       np_goal_type_clause_and_foreign.

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

    % Are calls from a predicate with the pred_markers always fully
    % qualified? Basically, this function tests for the presence or absence
    % of marker_calls_are_fully_qualified.
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

    ;       marker_no_detism_warning
            % Requests no warnings about the determinism of this predicate
            % being too loose.
            % Used for pragma(no_determinism_warning).

    ;       marker_user_marked_inline
            % The user requests that this be predicate should be inlined,
            % even if it exceeds the usual size limits. Used for
            % pragma(inline). Mutually exclusive with
            % marker_user_marked_no_inline.

    ;       marker_heuristic_inline
            % The compiler (meaning probably inlining.m) requests that this
            % predicate be inlined. Does not override
            % marker_user_marked_no_inline.

    ;       marker_user_marked_no_inline
            % The user requests that this be predicate should not be inlined.
            % Used for pragma(no_inline). Mutually exclusive with
            % marker_user_marked_inline.

    ;       marker_mmc_marked_no_inline
            % The compiler requests that this be predicate should not be
            % inlined. Used for pragma(mode_check_clauses). Mutually exclusive
            % with marker_user_marked_inline.

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

    ;       marker_has_format_call
            % The body of this predicate contains calls to predicates
            % recognized by format_call.is_format_call. This marker is set
            % (if applicable) during determinism analysis, when the predicate
            % body has to be traversed anyway. It is used by the simplification
            % pass at the end of semantic analysis, both to warn about
            % incorrect (or at least not verifiably correct) format calls,
            % and to optimize correct format calls. Neither the warnings
            % nor the optimizations can be applicable to predicates that
            % do not contain format calls, as shown by not having this marker.

    ;       marker_has_rhs_lambda
            % The body of this predicate contains a unification whose
            % right hand side is a lambda goal. This marker is set by
            % the typecheck pass, and it is used (as of this writing)
            % only by the post-typecheck pass.

    ;       marker_fact_table_semantic_errors.
            % This predicate has a fact_table pragma for it, so it is
            % *expected* not to have any clauses in the program itself,
            % but the compiler found some problems with its declaration,
            % and so the compiler did not generate clauses (actually,
            % foreign_procs) for it either. Therefore its procedures
            % have no implementations, but there should be no separate
            % error message about this: since they would probably generate
            % more confusion than enlightenment. The error messages generated
            % by fact_table.m should be entirely sufficient.

:- pred marker_name(pred_marker::in, string::out) is det.

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
                item_seq_num
            ).

:- type format_call
    --->    format_call(
                % The context of the format_call pragma whose into
                % this field of the pred_info records. We use this
                % to generate more informative error messages in cases of
                % duplicate format_call pragmas.
                prog_context,

                % The <format string arg #, values list arg #> pairs
                % listed in that pragma.
                one_or_more(format_string_values)
            ).

    % pred_info_init(PredOrFunc, PredModuleName, PredName, Arity, Context,
    %   Origin, Status, CurUserDecl, GoalType, Markers,
    %   ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofs,
    %   ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo):
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults.
    %
:- pred pred_info_init(pred_or_func::in, module_name::in, string::in,
    pred_form_arity::in, prog_context::in, pred_origin::in,
    pred_status::in, maybe(cur_user_decl_info)::in, goal_type::in,
    pred_markers::in, list(mer_type)::in, tvarset::in, existq_tvars::in,
    prog_constraints::in, constraint_proof_map::in, constraint_map::in,
    clauses_info::in, map(prog_var, string)::in, pred_info::out) is det.

    % pred_info_create(ModuleInfo, PredOrFunc, ModuleName, PredName,
    %   Context, Origin, Status, Markers, ArgTypes, TypeVarSet, ExistQVars,
    %   ClassContext, Assertions, VarNameRemap, ProcInfo, ProcId, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults. The given
    % proc_info becomes the only procedure of the predicate (currently)
    % and its proc_id is returned as the second last argument.
    %
:- pred pred_info_create(pred_or_func::in, module_name::in, string::in,
    prog_context::in, pred_origin::in, pred_status::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in, prog_constraints::in,
    set(assert_id)::in, map(prog_var, string)::in, goal_type::in,
    proc_info::in, proc_id::out, pred_info::out) is det.

%---------------------%

% pred_prepare_to_clone returns all the fields of an existing pred_info,
% while pred_create constructs a new pred_info putting the supplied values
% to each field.
%
% These predicates exist because we want keep the definition of the pred_info
% type private (to make future changes easier), but we also want to make it
% possible to create slightly modified copies of existing predicates
% with the least amount of programming work. We also want to require
% (a) programmers writing such cloning code to consider what effect
% the modification may have on *all* fields of the pred_info, and
% (b) programmers who add new fields to the pred_info to update
% all the places in the compiler that do such cloning.

:- pred pred_prepare_to_clone(pred_info::in,
    module_name::out, pred_or_func::out, string::out, pred_form_arity::out,
    pred_origin::out, pred_status::out, pred_markers::out, list(mer_type)::out,
    tvarset::out, tvarset::out, existq_tvars::out, int::out,
    prog_constraints::out, clauses_info::out,
    proc_table::out, prog_context::out,
    maybe(cur_user_decl_info)::out, goal_type::out, tvar_kind_map::out,
    tsubst::out, external_type_params::out, constraint_proof_map::out,
    constraint_map::out, list(prog_constraint)::out, inst_graph_info::out,
    list(arg_modes_map)::out, map(prog_var, string)::out, set(assert_id)::out,
    maybe(list(sym_name_arity))::out, maybe(format_call)::out,
    list(mer_type)::out) is det.

:- pred pred_create(module_name::in,
    pred_or_func::in, string::in, pred_form_arity::in,
    pred_origin::in, pred_status::in, pred_markers::in, list(mer_type)::in,
    tvarset::in, tvarset::in, existq_tvars::in, int::in, prog_constraints::in,
    clauses_info::in, proc_table::in, prog_context::in,
    maybe(cur_user_decl_info)::in, goal_type::in, tvar_kind_map::in,
    tsubst::in, external_type_params::in, constraint_proof_map::in,
    constraint_map::in, list(prog_constraint)::in, inst_graph_info::in,
    list(arg_modes_map)::in, map(prog_var, string)::in, set(assert_id)::in,
    maybe(list(sym_name_arity))::in, maybe(format_call)::in,
    list(mer_type)::in, pred_info::out) is det.

%---------------------%

    % define_new_pred(SymName, Origin, TVarSet, InstVarSet, VarTable,
    %   RttiVarMaps, ClassContext, InstMap0, VarNameRemap,
    %   Markers, IsAddressTaken, HasParallelConj, PredProcId,
    %   ArgVars, ExtraTiTcis, Goal0, CallGoal, !ModuleInfo):
    %
    % Create a new predicate for the given goal, returning a goal to
    % call the created predicate. ExtraArgs is the list of extra
    % type_infos and typeclass_infos required by typeinfo liveness
    % which were added to the front of the argument list.
    %
:- pred define_new_pred(sym_name::in, pred_origin::in,
    tvarset::in, inst_varset::in, var_table::in,
    rtti_varmaps::in, prog_constraints::in, instmap::in,
    map(prog_var, string)::in, pred_markers::in,
    is_address_taken::in, has_parallel_conj::in, pred_proc_id::out,
    list(prog_var)::in, list(prog_var)::out, hlds_goal::in, hlds_goal::out,
    module_info::in, module_info::out) is det.

    % Various predicates for accessing the information stored in the
    % pred_id and pred_info data structures.
    %
:- type external_type_params == list(tvar).

:- func pred_info_module(pred_info) = module_name.
:- func pred_info_name(pred_info) = string.

    % N-ary functions are converted into N+1-ary predicates.
    % (Clauses are converted in make_hlds, but calls to functions
    % cannot be converted until after type-checking, once we have
    % resolved overloading. So we do that during mode analysis.)
    % The `is_pred_or_func' field of the pred_info records whether
    % a pred_info is really for a predicate or whether it is for
    % what was originally a function.
    %
:- func pred_info_is_pred_or_func(pred_info) = pred_or_func.

    % Pred_info_orig_arity returns the arity of the predicate
    % *not* counting inserted type_info arguments for polymorphic preds.
    %
:- func pred_info_pred_form_arity(pred_info) = pred_form_arity.
:- func pred_info_user_arity(pred_info) = user_arity.

:- pred pred_info_get_module_name(pred_info::in,
    module_name::out) is det.
:- pred pred_info_get_is_pred_or_func(pred_info::in,
    pred_or_func::out) is det.
:- pred pred_info_get_name(pred_info::in,
    string::out) is det.
:- pred pred_info_get_orig_arity(pred_info::in,
    pred_form_arity::out) is det.
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
:- pred pred_info_get_polymorphism_added_args(pred_info::in,
    int::out) is det.
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
:- pred pred_info_get_obsolete_in_favour_of(pred_info::in,
    maybe(list(sym_name_arity))::out) is det.
:- pred pred_info_get_format_call(pred_info::in,
    maybe(format_call)::out) is det.
:- pred pred_info_get_instance_method_arg_types(pred_info::in,
    list(mer_type)::out) is det.
:- pred pred_info_get_clauses_info(pred_info::in,
    clauses_info::out) is det.
:- pred pred_info_get_proc_table(pred_info::in,
    proc_table::out) is det.

    % Setting any part of the sym_name of a pred_info after its creation
    % won't remove its name from the indexes under its old name or insert it
    % into the indexes under its new name. If is therefore safe to do this
    % only after *all* the passes that look up predicates by name.
    %
:- pred pred_info_set_module_name(module_name::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_is_pred_or_func(pred_or_func::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_name(string::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_orig_arity(pred_form_arity::in,
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
:- pred pred_info_set_polymorphism_added_args(int::in,
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
:- pred pred_info_set_obsolete_in_favour_of(
    maybe(list(sym_name_arity))::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_format_call(maybe(format_call)::in,
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

    % Return a list of the proc_ids for all the modes of this predicate,
    %
:- func pred_info_all_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate
    % that are not imported.
    %
:- func pred_info_all_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate
    % that are exported.
    %
:- func pred_info_all_exported_procids(pred_info) = list(proc_id).

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
:- pred pred_info_defn_has_clause(pred_info::in) is semidet.

    % Do we have a pragma goal type?
    % (this means either "pragmas" or "clauses_and_pragmas")
    %
:- pred pred_info_defn_has_foreign_proc(pred_info::in) is semidet.

:- pred pred_info_update_goal_type(np_goal_type::in,
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

:- pred pred_info_get_pf_sym_name_arity(pred_info::in, pf_sym_name_arity::out)
    is det.

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.optimization_options.

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

next_proc_id(ProcTable, ProcId) :-
    % We could store the next available ModeId rather than recomputing
    % it on demand, but it is probably more efficient this way.
    map.to_assoc_list(ProcTable, ProcIdsInfos),
    list.length(ProcIdsInfos, Num),
    proc_id_to_int(ProcId, Num).

calls_are_fully_qualified(Markers) =
    ( if check_marker(Markers, marker_calls_are_fully_qualified) then
        is_fully_qualified
    else
        may_be_partially_qualified
    ).

%---------------------------------------------------------------------------%

% For markers that we add to a predicate because of a pragma on that predicate,
% the marker name MUST correspond to the name of the pragma.
marker_name(marker_stub, "stub").
marker_name(marker_builtin_stub, "builtin_stub").
marker_name(marker_infer_type, "infer_type").
marker_name(marker_infer_modes, "infer_modes").
marker_name(marker_user_marked_inline, "inline").
marker_name(marker_heuristic_inline, "heuristic_inline").
marker_name(marker_no_pred_decl, "no_pred_decl").
marker_name(marker_user_marked_no_inline, "no_inline").
marker_name(marker_mmc_marked_no_inline, "mmc_no_inline").
marker_name(marker_consider_used, "consider_used").
marker_name(marker_no_detism_warning, "no_determinism_warning").
marker_name(marker_class_method, "class_method").
marker_name(marker_class_instance_method, "class_instance_method").
marker_name(marker_named_class_instance_method, "named_class_instance_method").
marker_name(marker_is_impure, "impure").
marker_name(marker_is_semipure, "semipure").
marker_name(marker_promised_pure, "promise_pure").
marker_name(marker_promised_semipure, "promise_semipure").
marker_name(marker_promised_equivalent_clauses, "promise_equivalent_clauses").
marker_name(marker_terminates, "terminates").
marker_name(marker_check_termination, "check_termination").
marker_name(marker_does_not_terminate, "does_not_terminate").
marker_name(marker_calls_are_fully_qualified, "calls_are_fully_qualified").
marker_name(marker_mode_check_clauses, "mode_check_clauses").
marker_name(marker_mutable_access_pred, "mutable_access_pred").
marker_name(marker_has_require_scope, "has_require_scope").
marker_name(marker_has_incomplete_switch, "has_incomplete_switch").
marker_name(marker_has_format_call, "has_format_call").
marker_name(marker_has_rhs_lambda, "has_rhs_lambda").
marker_name(marker_fact_table_semantic_errors, "fact_table_semantic_errors").

%---------------------------------------------------------------------------%

% Access stats for the pred_info structure, derived on 2014 dec 13:
%
%  i        read        same        diff   same%
%  1 124,287,827     348,040  31,892,426   1.08%    procedures
%  2  72,037,616      96,336   1,082,541   8.17%    status
%  3  43,651,895           0           0            module_name
%  4  32,003,757           0         795   0.00%    name
%  5  25,261,836           0           0            orig_arity
%  6  24,876,447     905,599   1,098,352  45.19%    markers
%  7  22,294,552      19,444  12,496,762   0.16%    clauses_info
%  8  20,415,273           0           0            arg_types
%  9  15,356,498         727          68  91.45%    is_pred_or_func
% 10  12,075,408     382,680      89,618  81.03%    origin
% 11  11,783,136   9,736,724     752,983  92.82%    typevarset
% 12  11,128,685   4,600,914   1,642,568  73.69%    three fields:
%                                                       decl_typevarset,
%                                                       exist_quant_vars and
%                                                       arg_types
% 13   7,871,038           0   2,700,797   0.00%    class_context
% 14   6,629,313     100,630   1,054,197   8.71%    goal_type
% 15   5,892,199       6,544       6,726  49.31%    var_name_remap
% 16   3,820,195      85,054           0 100.00%    tvar_kind_map
% 17   2,752,537     404,771      23,921  94.42%    constraint_map
% 18   2,591,016     425,209       3,483  99.19%    constraint_proof_map
% 19   1,667,832           0           0            context
% 20   1,374,911           0           0            exist_quant_vars
% 21     476,703     276,426     152,903  64.39%    external_type_params
% 22     285,538     428,650           4 100.00%    unproven_body_constraints
% 23      22,563           0          80   0.00%    existq_tvar_binding
% 24       3,834           0       3,797   0.00%    assertions
% 25          10           0           0            attributes
% 26           0           0      19,439   0.00%    instance_method_arg_types
% 27           0           0           0            arg_modes_maps
% 28           0           0           0            inst_graph_info

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

                % Is this "predicate" really a predicate or a function?
/*  2 */        pi_is_pred_or_func      :: pred_or_func,

                % Predicate name.
/*  3 */        pi_name                 :: string,

                % The original arity of the pred, i.e. its arity *not* counting
                % any type_info and/or typeclass_info arguments inserted
                % automatically by the compiler.
                %
                % For functions, the original arity *includes* the return
                % value, so that e.g. the original arity of int.+ would be 3.
/*  4 */        pi_orig_arity           :: pred_form_arity,

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
                %
                % For predicates that represent a method of a typeclass,
                % the first universal constraint will be the constraint
                % for that typeclass. This is ensured by code in
                % module_add_class_method, which is executed when
                % the class method's pred declaration is added to the HLDS.
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
                %
                % Note that "defined explicitly by the user" does not guarantee
                % that the cur_user_decl_info will contain a valid
                % item_seq_num, because for class methods, it won't.
                % (This is because predicate declarations in typeclass items
                % do not have their own separate item_seq_num.)
                psi_cur_user_decl               :: maybe(cur_user_decl_info),

                % Whether the goals seen so far, if any, for this predicate
                % are clauses or foreign_code(...) pragmas.
                psi_goal_type                   :: goal_type,

                % Kinds of the type vars.
                psi_tvar_kind_map               :: tvar_kind_map,

                % The statically known bindings of existentially quantified
                % type variables inside this predicate. This field is set
                % at the end of the polymorphism stage.
                psi_existq_tvar_binding         :: tsubst,

                % The number of type_info and/or typeclass_info arguments
                % added by the polymorphism pass. This field is set
                % at the end of that pass.
                %
                % XXX ARGVEC: When we use argvecs to record the predicate's
                % argument vector, we should be able to delete this field.
                psi_polymorphism_added_args     :: int,

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

                % If this predicate is marked as obsolete, this will be a
                % "yes(_)" wrapped around a list of the predicate names that
                % the compiler should suggest as possible replacements.
                % (Note that the list of possible replacements may be empty.)
                % In the usual case where this predicate is NOT marked
                % as obsolete, this will be "no".
                psi_obsolete_in_favour_of       :: maybe(list(sym_name_arity)),

                % If this field contains yes(FormatCall), then this predicate
                % has a format_call pragma, and FormatCall contains both the
                % <format string, values list> argument number pairs
                % specified in that pragma, and the context of that pragma.
                % If this field contains no, then the predicate does not have
                % a format_call pragma.
                %
                % When the HLDS is first created, the argument numbers
                % in the format_string_values structures in the list
                % refer to the position of the arguments in the visible
                % argument list. (The numbering starts at 1.) When polymorphism
                % adds compiler-generated arguments to the start of the
                % argument list, it increments all the argument numbers
                % in this field to compensate.
                %
                % Some optimizations may also add or delete arguments,
                % but they don't have to update this field, because
                %
                % - this field is used only by format_call.m, during the
                %   simplification pass done at the end of the front end,
                %
                % - optimizations that can change argument lists are
                %   all run *after* the front end, and therefore after
                %   all code that cares about the value of this field.
                psi_format_call                 :: maybe(format_call),

                % If this predicate is a class method implementation, this
                % list records the argument types before substituting the type
                % variables for the instance.
                % XXX does that make sense?
                psi_instance_method_arg_types   :: list(mer_type)
            ).

pred_info_init(PredOrFunc, PredModuleName, PredName, PredFormArity, Context,
        Origin, Status, CurUserDecl, GoalType, Markers,
        ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofs,
        ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo) :-
    % argument Context
    % argument GoalType
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    PolymorphismAddedArgs = 0,
    type_vars_in_types(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    % argument ClassProofs
    % argument ClassConstraintMap
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    set.init(Assertions),
    ObsoleteInFavourOf = maybe.no,
    FormatCall = maybe.no,
    InstanceMethodArgTypes = [],
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),

    % argument PredModuleName
    % argument PredName
    % NOTE We cannot assert anything about the relationship
    % between PredFormArity and the number of arguments in ArgTypes, because
    %
    % - ArgTypes may be have more arguments than PredFormArity,
    %   due to the type_info/typeclass_info arguments added by the
    %   polymorphism pass, and
    %
    % - ArgTypes have have fewer arguments than PredFormArity,
    %   because some arguments may have been removed by the unused_args pass.
    %
    % XXX ARGVEC Eventually, when we start using arg vectors, the arguments
    % added by the polymorphism pass would be counted separately.
    %
    % XXX ARITY The unused_args pass *should* decrement PredFormArity
    % by the number of arguments it eliminates, but at the moment, it does not.
    %
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
    PredInfo = pred_info(PredModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

pred_info_create(PredOrFunc, PredModuleName, PredName,
        Context, Origin, Status, Markers, ArgTypes, TypeVarSet,
        ExistQVars, ClassContext, Assertions, VarNameRemap, GoalType,
        ProcInfo, ProcId, PredInfo) :-
    % argument Context
    CurUserDecl = maybe.no,
    % argument GoalType
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    PolymorphismAddedArgs = 0,
    type_vars_in_types(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    map.init(ClassProofs),
    map.init(ClassConstraintMap),
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    % argument Assertions
    ObsoleteInFavourOf = maybe.no,
    FormatCall = maybe.no,
    InstanceMethodArgTypes = [],

    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),

    % The VarSet and ExplicitVarTypes fields are not needed after typechecking.
    varset.init(VarSet),
    init_vartypes(ExplicitVarTypes),
    proc_info_get_var_table(ProcInfo, VarTable),
    map.init(TVarNameMap),
    proc_info_get_headvars(ProcInfo, HeadVars),
    HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
    % The empty list of clauses is a little white lie.
    ClausesRep = init_clauses_rep,
    ItemNumbers = init_clause_item_numbers_user,
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, VarTable, RttiVarMaps,
        TVarNameMap, HeadVarVec, ClausesRep, ItemNumbers,
        no_foreign_lang_clauses, no_clause_syntax_errors),

    % argument PredModuleName
    % argument PredName
    list.length(ArgTypes, NumArgs),
    PredFormArity = pred_form_arity(NumArgs),
    % argument PredOrFunc
    % argument Origin
    % argument Status
    % argument Markers
    % argument ArgTypes
    % argument TypeVarSet
    % argument ExistQVars
    % argument ClassContext
    map.init(ProcTable0),
    next_proc_id(ProcTable0, ProcId),
    map.det_insert(ProcId, ProcInfo, ProcTable0, ProcTable),
    PredInfo = pred_info(PredModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

pred_prepare_to_clone(PredInfo, ModuleName, PredOrFunc, PredName,
        PredFormArity, Origin, Status, Markers, ArgTypes,
        DeclTypeVarSet, TypeVarSet, ExistQVars, PolymorphismAddedArgs,
        ClassContext, ClausesInfo, ProcTable, Context,
        CurUserDecl, GoalType, Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, VarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes) :-
    PredInfo = pred_info(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo),
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes).

pred_create(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, PolymorphismAddedArgs,
        ClassContext, ClausesInfo, ProcTable, Context,
        CurUserDecl, GoalType, Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, VarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes, PredInfo) :-
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),
    PredInfo = pred_info(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

define_new_pred(PredSymName, Origin, TVarSet, InstVarSet,
        VarTable0, RttiVarMaps, ClassContext, InstMap0, VarNameRemap,
        Markers, IsAddressTaken, HasParallelConj, PredProcId,
        ArgVars0, ExtraTiTcis, Goal0, CallGoal, !ModuleInfo) :-
    Goal0 = hlds_goal(_GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),

    % XXX The set of existentially quantified type variables
    % here might not be correct.
    ExistQVars = [],

    % If interface typeinfo liveness is set, all type_infos for the
    % arguments need to be passed in, not just the ones that are used.
    % Similarly if the address of a procedure of this predicate is taken,
    % so that we can copy the closure.
    module_info_get_globals(!.ModuleInfo, Globals),
    PredStatus = pred_status(status_local),
    non_special_interface_should_use_typeinfo_liveness(PredStatus,
        IsAddressTaken, Globals, TypeInfoLiveness),
    (
        TypeInfoLiveness = yes,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        goal_util.extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps,
            VarTable0, ExistQVars, NonLocals, ExtraTiTcis0),
        set_of_var.delete_list(ArgVars0, ExtraTiTcis0, ExtraTiTcis1),
        set_of_var.to_sorted_list(ExtraTiTcis1, ExtraTiTcis),
        ArgVars = ExtraTiTcis ++ ArgVars0
    ;
        TypeInfoLiveness = no,
        ArgVars = ArgVars0,
        ExtraTiTcis = []
    ),

    Context = goal_info_get_context(GoalInfo),
    ItemNumber = item_no_seq_num,
    Detism = goal_info_get_determinism(GoalInfo),
    compute_arg_types_modes(VarTable0, InstMap0, InstMap,
        ArgVars, ArgTypes, ArgModes),

    (
        PredSymName = qualified(PredModuleName, PredName)
    ;
        PredSymName = unqualified(PredName),
        module_info_get_name(!.ModuleInfo, ModuleName),
        PredModuleName = ModuleName
    ),

    % Remove unneeded variables from the var_table.
    goal_util.goal_vars(Goal0, GoalVars0),
    set_of_var.insert_list(ArgVars, GoalVars0, GoalVars),
    GoalVarsSet = set_of_var.bitset_to_set(GoalVars),
    var_table_select(GoalVarsSet, VarTable0, VarTable),

    % Approximate the termination information for the new procedure.
    ( if goal_cannot_loop_term_info(!.ModuleInfo, Goal0) then
        TermInfo = yes(cannot_loop(unit))
    else
        TermInfo = no
    ),

    MaybeDeclaredDetism = no,
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarTable, ArgVars, InstVarSet, ArgModes,
        detism_decl_none, MaybeDeclaredDetism, Detism, Goal0,
        RttiVarMaps, IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo0),
    proc_info_set_maybe_termination_info(TermInfo, ProcInfo0, ProcInfo),

    set.init(Assertions),
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_create(pf_predicate, PredModuleName, PredName,
        Context, Origin, PredStatus, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Assertions, VarNameRemap, GoalType, ProcInfo,
        ProcId, PredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(PredInfo, PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),

    CallGoalExpr =
        plain_call(PredId, ProcId, ArgVars, not_builtin, no, PredSymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(var_table::in, instmap::in, instmap::in,
    list(prog_var)::in, list(mer_type)::out, list(mer_mode)::out) is det.

compute_arg_types_modes(_, _, _, [], [], []).
compute_arg_types_modes(VarTable, InstMapInit, InstMapFinal,
        [Var | Vars], [Type | Types], [Mode | Modes]) :-
    lookup_var_type(VarTable, Var, Type),
    instmap_lookup_var(InstMapInit, Var, InstInit),
    instmap_lookup_var(InstMapFinal, Var, InstFinal),
    Mode = from_to_mode(InstInit, InstFinal),
    compute_arg_types_modes(VarTable, InstMapInit, InstMapFinal,
        Vars, Types, Modes).

%---------------------------------------------------------------------------%

% The trivial access predicates.

pred_info_module(PI) = X :-
    pred_info_get_module_name(PI, X).
pred_info_name(PI) = X :-
    pred_info_get_name(PI, X).
pred_info_is_pred_or_func(PI) = X :-
    pred_info_get_is_pred_or_func(PI, X).
pred_info_pred_form_arity(PI) = PredFormArity :-
    pred_info_get_orig_arity(PI, PredFormArity).
pred_info_user_arity(PI) = UserArity :-
    pred_info_get_is_pred_or_func(PI, PredOrFunc),
    pred_info_get_orig_arity(PI, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity).

pred_info_get_module_name(!.PI, X) :-
    X = !.PI ^ pi_module_name.
pred_info_get_is_pred_or_func(!.PI, X) :-
    X = !.PI ^ pi_is_pred_or_func.
pred_info_get_name(!.PI, X) :-
    X = !.PI ^ pi_name.
pred_info_get_orig_arity(!.PI, X) :-
    X = !.PI ^ pi_orig_arity.

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
pred_info_get_polymorphism_added_args(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_polymorphism_added_args.
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
pred_info_get_obsolete_in_favour_of(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_obsolete_in_favour_of.
pred_info_get_format_call(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_format_call.
pred_info_get_instance_method_arg_types(!.PI, X) :-
    X = !.PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types.
pred_info_get_clauses_info(!.PI, X) :-
    X = !.PI ^ pi_clauses_info.
pred_info_get_proc_table(!.PI, X) :-
    X = !.PI ^ pi_proc_table.

pred_info_set_module_name(X, !PI) :-
    !PI ^ pi_module_name := X.
pred_info_set_is_pred_or_func(X, !PI) :-
    ( if X = !.PI ^ pi_is_pred_or_func then
        true
    else
        !PI ^ pi_is_pred_or_func := X
    ).
pred_info_set_name(X, !PI) :-
    !PI ^ pi_name := X.
pred_info_set_orig_arity(X, !PI) :-
    !PI ^ pi_orig_arity := X.
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
pred_info_set_polymorphism_added_args(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_polymorphism_added_args := X.
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
pred_info_set_obsolete_in_favour_of(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_obsolete_in_favour_of := X.
pred_info_set_format_call(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_format_call := X.
pred_info_set_instance_method_arg_types(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types := X.
pred_info_set_clauses_info(X, !PI) :-
    !PI ^ pi_clauses_info := X.
pred_info_set_proc_table(X, !PI) :-
    !PI ^ pi_proc_table := X.

%---------------------------------------------------------------------------%

% The non-trivial access predicates.

pred_info_all_procids(PredInfo) = ProcIds :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.keys(ProcTable, ProcIds).

pred_info_all_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ;
        OldImportStatus = status_pseudo_imported,
        ProcIds0 = pred_info_all_procids(PredInfo),
        % For pseudo_imported preds, procid 0 is imported
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
        ProcIds = pred_info_all_procids(PredInfo)
    ).

pred_info_all_exported_procids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        ProcIds = pred_info_all_procids(PredInfo)
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

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    type_vars_in_types(ArgTypes, ArgTypeVars0),
    list.sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    list.delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

pred_info_proc_info(PredInfo, ProcId, ProcInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo).

pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo).

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
        Origin = origin_compiler(made_for_uci(SpecialPredId, TypeCtor)),
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

pred_info_defn_has_clause(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    GoalType = goal_not_for_promise(NPGoalType),
    goal_type_has_clause(NPGoalType).

pred_info_defn_has_foreign_proc(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    GoalType = goal_not_for_promise(NPGoalType),
    goal_type_has_foreign_proc(NPGoalType).

:- pred goal_type_has_clause(np_goal_type::in) is semidet.

goal_type_has_clause(np_goal_type_clause).
goal_type_has_clause(np_goal_type_clause_and_foreign).

:- pred goal_type_has_foreign_proc(np_goal_type::in) is semidet.

goal_type_has_foreign_proc(np_goal_type_foreign).
goal_type_has_foreign_proc(np_goal_type_clause_and_foreign).

pred_info_update_goal_type(NPGoalType1, !PredInfo) :-
    pred_info_get_goal_type(!.PredInfo, GoalType0),
    (
        GoalType0 = goal_not_for_promise(NPGoalType0),
        (
            NPGoalType0 = np_goal_type_none,
            NPGoalType = NPGoalType1
        ;
            NPGoalType0 = np_goal_type_clause,
            ( if goal_type_has_foreign_proc(NPGoalType1) then
                NPGoalType = np_goal_type_clause_and_foreign
            else
                NPGoalType = np_goal_type_clause
            )
        ;
            NPGoalType0 = np_goal_type_foreign,
            ( if goal_type_has_clause(NPGoalType1) then
                NPGoalType = np_goal_type_clause_and_foreign
            else
                NPGoalType = np_goal_type_foreign
            )
        ;
            NPGoalType0 = np_goal_type_clause_and_foreign,
            NPGoalType = NPGoalType0
        ),
        GoalType = goal_not_for_promise(NPGoalType),
        ( if GoalType = GoalType0 then
            % Avoid unnecessary memory allocation.
            true
        else
            pred_info_set_goal_type(GoalType, !PredInfo)
        )
    ;
        GoalType0 = goal_for_promise(_),
        unexpected($pred, "promise")
    ).

pred_info_requested_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, marker_user_marked_inline)
    ; check_marker(Markers, marker_heuristic_inline)
    ).

pred_info_requested_no_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, marker_user_marked_no_inline)
    ; check_marker(Markers, marker_mmc_marked_no_inline)
    ).

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

%---------------------------------------------------------------------------%

pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_sym_name(PredInfo, SymName),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity).

pred_info_get_sym_name(PredInfo, SymName) :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    SymName = qualified(Module, Name).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
                dob_var_table           :: var_table,
                dob_detism              :: determinism
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
                % The int gives the maximum enum value in the enum type + 1,
                % and thus the size of the corresponding trie node.
                % If the enum type is not a subtype, then the value is equal to
                % the number of alternatives in the type.
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
                ptsi_eval_method            :: tabled_eval_method
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

:- pred proc_info_init(module_info::in, prog_context::in, item_seq_num::in,
    list(mer_type)::in, maybe(list(mer_mode))::in, list(mer_mode)::in,
    maybe(list(is_live))::in, detism_decl::in, maybe(determinism)::in,
    is_address_taken::in, has_parallel_conj::in, map(prog_var, string)::in,
    proc_info::out) is det.

:- pred proc_info_create(prog_context::in, item_seq_num::in,
    var_table::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

%---------------------%

% proc_prepare_to_clone returns all the fields of an existing proc_info,
% while proc_create constructs a new proc_info putting the supplied values
% to each field.
%
% These predicates exist because we want keep the definition of the proc_info
% type private (to make future changes easier), but we also want to make it
% possible to create slightly modified copies of existing procedures
% with the least amount of programming work. We also want to require
% (a) programmers writing such cloning code to consider what effect
% the modification may have on *all* fields of the proc_info, and
% (b) programmers who add new fields to the proc_info to update
% all the places in the compiler that do such cloning.

:- pred proc_prepare_to_clone(proc_info::in, list(prog_var)::out,
    hlds_goal::out, var_table::out, rtti_varmaps::out,
    inst_varset::out, maybe(list(mer_mode))::out, list(mer_mode)::out,
    maybe(list(is_live))::out, maybe(determinism)::out, determinism::out,
    eval_method::out, prog_context::out, item_seq_num::out,
    can_process::out, maybe(mode_constraint)::out, detism_decl::out,
    list(prog_context)::out, maybe(untuple_proc_info)::out,
    map(prog_var, string)::out, list(error_spec)::out, set(pred_proc_id)::out,
    is_address_taken::out, proc_foreign_exports::out, has_parallel_conj::out,
    has_user_event::out, has_tail_rec_call::out, list(oisu_pred_kind_for)::out,
    maybe(require_tail_recursion)::out, set_of_progvar::out,
    maybe(list(arg_info))::out, maybe(special_proc_return)::out,
    liveness_info::out, stack_slots::out, needs_maxfr_slot::out,
    maybe(prog_var)::out, maybe(proc_table_io_info)::out,
    maybe(table_attributes)::out, maybe(list(sym_name_arity))::out,
    maybe(deep_profile_proc_info)::out, maybe(arg_size_info)::out,
    maybe(termination_info)::out, termination2_info::out,
    maybe(proc_exception_info)::out, maybe(proc_trailing_info)::out,
    maybe(proc_mm_tabling_info)::out, structure_sharing_info::out,
    structure_reuse_info::out) is det.

:- pred proc_create(list(prog_var)::in,
    hlds_goal::in, var_table::in, rtti_varmaps::in,
    inst_varset::in, maybe(list(mer_mode))::in, list(mer_mode)::in,
    maybe(list(is_live))::in, maybe(determinism)::in, determinism::in,
    eval_method::in, prog_context::in, item_seq_num::in, can_process::in,
    maybe(mode_constraint)::in, detism_decl::in,
    list(prog_context)::in, maybe(untuple_proc_info)::in,
    map(prog_var, string)::in, list(error_spec)::in, set(pred_proc_id)::in,
    is_address_taken::in, proc_foreign_exports::in, has_parallel_conj::in,
    has_user_event::in, has_tail_rec_call::in, list(oisu_pred_kind_for)::in,
    maybe(require_tail_recursion)::in, set_of_progvar::in,
    maybe(list(arg_info))::in, maybe(special_proc_return)::in,
    liveness_info::in, stack_slots::in, needs_maxfr_slot::in,
    maybe(prog_var)::in, maybe(proc_table_io_info)::in,
    maybe(table_attributes)::in, maybe(list(sym_name_arity))::in,
    maybe(deep_profile_proc_info)::in, maybe(arg_size_info)::in,
    maybe(termination_info)::in, termination2_info::in,
    maybe(proc_exception_info)::in, maybe(proc_trailing_info)::in,
    maybe(proc_mm_tabling_info)::in, structure_sharing_info::in,
    structure_reuse_info::in, proc_info::out) is det.

%---------------------%

:- pred proc_info_set_body(var_table::in,
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
:- pred proc_info_get_var_table(proc_info::in, var_table::out) is det.
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

:- pred proc_info_get_context(proc_info::in, prog_context::out) is det.
:- pred proc_info_get_item_number(proc_info::in, item_seq_num::out) is det.
:- pred proc_info_get_can_process(proc_info::in, can_process::out) is det.
:- pred proc_info_get_maybe_head_modes_constr(proc_info::in,
    maybe(mode_constraint)::out) is det.
:- pred proc_info_get_detism_decl(proc_info::in, detism_decl::out) is det.
:- pred proc_info_get_cse_nopull_contexts(proc_info::in,
    list(prog_context)::out) is det.
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
:- pred proc_info_get_obsolete_in_favour_of(proc_info::in,
    maybe(list(sym_name_arity))::out) is det.
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
:- pred proc_info_set_var_table(var_table::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_rtti_varmaps(rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inst_varset(inst_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_declared_argmodes(maybe(list(mer_mode))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_argmodes(list(mer_mode)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arglives(maybe(list(is_live))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inferred_determinism(determinism::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_eval_method(eval_method::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_set_can_process(can_process::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_head_modes_constraint(mode_constraint::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_detism_decl(detism_decl::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_cse_nopull_contexts(list(prog_context)::in,
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
:- pred proc_info_set_obsolete_in_favour_of(
    maybe(list(sym_name_arity))::in,
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
                % multiples (and usually powers) of 2. Ideally, we would want
                % the number of fields of proc_info to match one of the Boehm
                % block sizes, but as of 2017 march 15, this seemed to be the
                % optimal arrangement (zs).

/*  1 */        proc_head_vars                  :: list(prog_var),
/*  2 */        proc_body                       :: hlds_goal,

/*  3 */        proc_var_table                  :: var_table,

                % Information about type_infos and typeclass_infos.
/*  4 */        proc_rtti_varmaps               :: rtti_varmaps,

/*  5 */        proc_inst_varset                :: inst_varset,

                % The declared modes of arguments.
/*  6 */        proc_maybe_decl_head_modes      :: maybe(list(mer_mode)),

/*  7 */        proc_actual_head_modes          :: list(mer_mode),

                % Liveness (in the mode analysis sense) of the arguments
                % in the caller; says whether each argument may be used
                % after the call.
/*  8 */        proc_headvar_caller_liveness    :: maybe(list(is_live)),

                % The _declared_ determinism of the procedure, or `no'
                % if there was no detism declaration.
/*  9 */        proc_declared_detism            :: maybe(determinism),
/* 10 */        proc_inferred_detism            :: determinism,

                % How should the proc be evaluated.
/* 11 */        proc_eval_method                :: eval_method,

/* 12 */        proc_sub_info                   :: proc_sub_info
            ).

:- type proc_sub_info
    --->    proc_sub_info(
                % The context of the `:- mode' decl, or the context of the
                % first clause if there was no mode declaration.
                psi_proc_context                :: prog_context,

                % The item number of the mode declaration, if there was one.
                psi_item_number                 :: item_seq_num,

                % Set to cannot_process if we must not process this procedure
                % just yet. This is used to delay mode checking etc. for
                % complicated modes of unification predicates until the end
                % of the unique_modes pass.
                psi_can_process                 :: can_process,

                % XXX The mode of the procedure in the ROBDD based
                % constraint system. Whether it represents the declared
                % or the actual mode is unclear, but since that constraint
                % system is obsolete, this does not much matter :-(
                psi_maybe_head_modes_constr     :: maybe(mode_constraint),

                % Was the determinism declaration explicit, or was it implicit,
                % as for functions?
                psi_detism_decl                 :: detism_decl,

                % A list of all the contexts at which cse_detection.m
                % declined to pull out a common deconstruction out of
                % a branched control structure due to concerns about
                % uniqueness in the inst of the affected variable.
                % Determinism analysis wants this information so that
                % it knows whether to mention this fact to the user
                % as a possible cause of a determinism error.
                % See Mantis bug #496.
                psi_cse_nopull_contexts         :: list(prog_context),

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

                % If this procedure is marked as obsolete, this will be a
                % "yes(_)" wrapped around a list of the predicate names that
                % the compiler should suggest as possible replacements.
                % (Note that the list of possible replacements may be empty.)
                % In the usual case where this predicate is NOT marked
                % as obsolete, this will be "no".
                psi_proc_obsolete_in_favour_of  :: maybe(list(
                                                    sym_name_arity)),

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

%---------------------------------------------------------------------------%

proc_info_init(ModuleInfo, MainContext, ItemNumber, Types,
        DeclaredModes, Modes, MaybeArgLives, DetismDecl, MaybeDeclaredDetism,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
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
    CseNopullContexts = [],
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(DeletedCallees),
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
    MaybeObsoleteInFavourOf = no `with_type` maybe(list(sym_name_arity)),
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
        MaybeHeadModesConstr,
        DetismDecl,
        CseNopullContexts,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
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
        MaybeObsoleteInFavourOf,
        MaybeDeepProfProcInfo,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingInfo,
        ReuseInfo),

    init_var_table(VarTable0),
    make_fresh_prefix_named_vars_from_types(ModuleInfo, "HeadVar__", 1,
        Types, HeadVars, VarTable0, VarTable),
    goal_info_init(GoalInfo),
    BodyGoal = hlds_goal(conj(plain_conj, []), GoalInfo),
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

    ProcInfo = proc_info(
        HeadVars,
        BodyGoal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        InferredDetism,
        EvalMethod,
        ProcSubInfo).

:- pred make_fresh_prefix_named_vars_from_types(module_info::in,
    string::in, int::in, list(mer_type)::in, list(prog_var)::out,
    var_table::in, var_table::out) is det.

make_fresh_prefix_named_vars_from_types(_, _, _, [], [], !Info).
make_fresh_prefix_named_vars_from_types(ModuleInfo, BaseName, Num,
        [Type | Types], [Var | Vars], !VarTable) :-
    make_fresh_prefix_named_var_from_type(ModuleInfo, BaseName, Num,
        Type, Var, !VarTable),
    make_fresh_prefix_named_vars_from_types(ModuleInfo, BaseName, Num + 1,
        Types, Vars, !VarTable).

:- pred make_fresh_prefix_named_var_from_type(module_info::in,
    string::in, int::in, mer_type::in, prog_var::out,
    var_table::in, var_table::out) is det.

make_fresh_prefix_named_var_from_type(ModuleInfo, BaseName, Num, Type, Var,
        !VarTable) :-
    string.format("%s%d", [s(BaseName), i(Num)], Name),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    add_var_entry(Entry, Var, !VarTable).

%---------------------------------------------------------------------------%

proc_info_create(Context, ItemNumber, VarTable, HeadVars,
        InstVarSet, HeadModes, DetismDecl, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarTable, HeadVars, InstVarSet, HeadModes,
        DetismDecl, yes(Detism), Detism, Goal, RttiVarMaps, IsAddressTaken,
        HasParallelConj, VarNameRemap, ProcInfo).

:- pred proc_info_create_with_declared_detism(prog_context::in,
    item_seq_num::in, var_table::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, maybe(determinism)::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

proc_info_create_with_declared_detism(MainContext, ItemNumber,
        VarTable, HeadVars, InstVarSet, Modes,
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
    CseNopullContexts = [],
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(DeletedCallees),
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
    MaybeObsoleteInFavourOf = no `with_type` maybe(list(sym_name_arity)),
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
        MaybeHeadModesConstr,
        DetismDecl,
        CseNopullContexts,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
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
        MaybeObsoleteInFavourOf,
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

    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo).

proc_prepare_to_clone(ProcInfo, HeadVars, Goal, VarTable, RttiVarMaps,
        InstVarSet, DeclaredModes, Modes, MaybeArgLives,
        MaybeDeclaredDetism, Detism, EvalMethod,
        MainContext, ItemNumber, CanProcess, MaybeHeadModesConstr, DetismDecl,
        CseNopullContexts, MaybeUntupleInfo, VarNameRemap, StateVarWarnings,
        DeletedCallees, IsAddressTaken, HasForeignProcExports, HasParallelConj,
        HasUserEvent, HasTailCallEvent, OisuKinds, MaybeRequireTailRecursion,
        RegR_HeadVars, MaybeArgPassInfo, MaybeSpecialReturn, InitialLiveness,
        StackSlots, NeedsMaxfrSlot, MaybeCallTableTip, MaybeTableIOInfo,
        MaybeTableAttrs, MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo,
        MaybeArgSizes, MaybeTermInfo, Term2Info, MaybeExceptionInfo,
        MaybeTrailingInfo, MaybeMMTablingInfo, SharingInfo, ReuseInfo) :-
    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo),
    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        CanProcess,
        MaybeHeadModesConstr,
        DetismDecl,
        CseNopullContexts,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
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
        MaybeObsoleteInFavourOf,
        MaybeDeepProfProcInfo,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingInfo,
        ReuseInfo).

proc_create(HeadVars, Goal, VarTable, RttiVarMaps,
        InstVarSet, DeclaredModes, Modes, MaybeArgLives,
        MaybeDeclaredDetism, Detism, EvalMethod,
        MainContext, ItemNumber, CanProcess, MaybeHeadModesConstr, DetismDecl,
        CseNopullContexts, MaybeUntupleInfo, VarNameRemap, StateVarWarnings,
        DeletedCallees, IsAddressTaken, HasForeignProcExports, HasParallelConj,
        HasUserEvent, HasTailCallEvent, OisuKinds, MaybeRequireTailRecursion,
        RegR_HeadVars, MaybeArgPassInfo, MaybeSpecialReturn, InitialLiveness,
        StackSlots, NeedsMaxfrSlot, MaybeCallTableTip, MaybeTableIOInfo,
        MaybeTableAttrs, MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo,
        MaybeArgSizes, MaybeTermInfo, Term2Info, MaybeExceptionInfo,
        MaybeTrailingInfo, MaybeMMTablingInfo, SharingInfo, ReuseInfo,
        ProcInfo) :-
    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        CanProcess,
        MaybeHeadModesConstr,
        DetismDecl,
        CseNopullContexts,
        MaybeUntupleInfo,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
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
        MaybeObsoleteInFavourOf,
        MaybeDeepProfProcInfo,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingInfo,
        ReuseInfo),
    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo).

proc_info_set_body(VarTable, HeadVars, Goal, RttiVarMaps, !ProcInfo) :-
    !ProcInfo ^ proc_var_table := VarTable,
    !ProcInfo ^ proc_head_vars := HeadVars,
    !ProcInfo ^ proc_body := Goal,
    !ProcInfo ^ proc_rtti_varmaps := RttiVarMaps.

proc_info_get_headvars(PI, X) :-
    X = PI ^ proc_head_vars.
proc_info_get_goal(PI, X) :-
    X = PI ^ proc_body.
proc_info_get_var_table(PI, X) :-
    X = PI ^ proc_var_table.
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

proc_info_get_context(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_context.
proc_info_get_item_number(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_item_number.
proc_info_get_can_process(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_can_process.
proc_info_get_maybe_head_modes_constr(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_head_modes_constr.
proc_info_get_detism_decl(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_detism_decl.
proc_info_get_cse_nopull_contexts(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_cse_nopull_contexts.
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
proc_info_get_oisu_kind_fors(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_oisu_kind_fors.
proc_info_get_maybe_require_tailrec_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_require_tailrec.
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
proc_info_get_obsolete_in_favour_of(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_obsolete_in_favour_of.
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
proc_info_set_var_table(X, !PI) :-
    !PI ^ proc_var_table := X.
proc_info_set_rtti_varmaps(X, !PI) :-
    !PI ^ proc_rtti_varmaps := X.
proc_info_set_inst_varset(X, !PI) :-
    !PI ^ proc_inst_varset := X.
proc_info_set_maybe_declared_argmodes(X, !PI) :-
    !PI ^ proc_maybe_decl_head_modes := X.
proc_info_set_argmodes(X, !PI) :-
    !PI ^ proc_actual_head_modes := X.
proc_info_set_maybe_arglives(X, !PI) :-
    !PI ^ proc_headvar_caller_liveness := X.
proc_info_set_inferred_determinism(X, !PI) :-
    !PI ^ proc_inferred_detism := X.
proc_info_set_eval_method(X, !PI) :-
    !PI ^ proc_eval_method := X.

proc_info_set_can_process(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_can_process := X.
proc_info_set_head_modes_constraint(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_head_modes_constr := yes(X).
proc_info_set_detism_decl(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_detism_decl := X.
proc_info_set_cse_nopull_contexts(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_cse_nopull_contexts := X.
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
proc_info_set_obsolete_in_favour_of(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_obsolete_in_favour_of := X.
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

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
        InterfaceTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
    ( if no_type_info_builtin(PredModule, PredName, PredFormArityInt) then
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
    pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
    ( if no_type_info_builtin(PredModule, PredName, PredFormArityInt) then
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

%---------------------------------------------------------------------------%

    % Predicates to deal with record syntax.

:- interface.

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
    % XXX ARITY The third argument should be either pred_form_arity or
    % user_arity.
    %
:- pred is_field_access_function_name(module_info::in, sym_name::in,
    arity::out, field_access_type::out, sym_name::out) is semidet.

:- pred pred_info_is_field_access_function(module_info::in, pred_info::in)
    is semidet.

:- implementation.

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
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    user_arity_pred_form_arity(pf_function, user_arity(FuncArityInt),
        PredFormArity),
    is_field_access_function_name(ModuleInfo, qualified(Module, Name),
        FuncArityInt, _, _).

%---------------------------------------------------------------------------%

    % Predicates to deal with builtins.

:- interface.

    % is_unify_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler-generated instance of a type-specific unify predicate.
    %
:- pred is_unify_pred(pred_info::in) is semidet.

    % is_unify_index_or_compare_pred(PredInfo) succeeds iff the PredInfo
    % is for a compiler generated instance of a type-specific special_pred
    % (i.e. one of the unify, compare, or index predicates generated as
    % a type-specific instance of unify/2, index/2, or compare/3).
    %
:- pred is_unify_index_or_compare_pred(pred_info::in) is semidet.

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
    Origin = origin_compiler(made_for_uci(spec_pred_unify, _TypeCtor)).

is_unify_index_or_compare_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_compiler(made_for_uci(_SpecialPredId, _TypeCtor)).

pred_info_is_builtin(PredInfo) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    is_inline_builtin(ModuleName, PredName, PredFormArity).

builtin_state(ModuleInfo, CallerPredId, PredId, _ProcId) = BuiltinState :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    ( if
        % XXX This should ask: is this an inline builtin FOR THIS BACKEND?
        is_inline_builtin(ModuleName, PredName, PredFormArity),
        (
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_opt_tuple(Globals, OptTuple),
            OptTuple ^ ot_allow_inlining = allow_inlining,
            (
                OptTuple ^ ot_inline_builtins = inline_builtins
            ;
                PredName = "store_at_ref_impure",
                ModuleName = mercury_private_builtin_module
            )
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

:- pred is_inline_builtin(module_name::in, string::in, pred_form_arity::in)
    is semidet.

is_inline_builtin(ModuleName, PredName, PredFormArity) :-
    PredFormArity = pred_form_arity(Arity),
    % None of our inline builtins has an arity greater than three.
    % Fail for predicates with arities of four or more *without*
    % doing a switch on ModuleName or PredName.
    Arity =< 3,
    builtin_ops.test_if_builtin(ModuleName, PredName, Arity).

pred_info_is_promise(PredInfo, PromiseType) :-
    pred_info_get_goal_type(PredInfo, goal_for_promise(PromiseType)).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_pred.
%---------------------------------------------------------------------------%
