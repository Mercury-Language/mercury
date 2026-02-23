%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_args_warn_pragma.m.
%
% If the appropriate options are set,
%
% - generate warnings about unused arguments in procedures, and/or
%
% - generate unused_args pragmas to be put into the module's .opt file.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.unused_args_warn_pragma.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type maybe_gather_pragma_unused_args
    --->    do_not_gather_pragma_unused_args
    ;       do_gather_pragma_unused_args.

:- type maybe_record_analysis_unused_args
    --->    do_not_record_analysis_unused_args
    ;       do_record_analysis_unused_args.

:- type maybe_warn_unused_args
    --->    do_not_warn_unused_args
    ;       do_warn_unused_args.

    % Except for type_infos, all args that are unused in one mode of a
    % predicate should be unused in all of the modes of a predicate, so we
    % only need to put out one warning for each predicate.
    %
:- pred gather_warnings_and_pragmas(module_info::in, unused_arg_info::in,
    maybe_warn_unused_args::in, maybe_gather_pragma_unused_args::in,
    list(pred_proc_id)::in, set(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out,
    set(gen_pragma_unused_args_info)::in,
    set(gen_pragma_unused_args_info)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.

%---------------------------------------------------------------------------%

gather_warnings_and_pragmas(_, _, _, _, [], _,
        !Specs, !PragmaUnusedArgInfos).
gather_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, DoWarn, DoPragma,
        [PredProcId | PredProcIds], !.WarnedPredIds,
        !Specs, !PragmaUnusedArgInfos) :-
    ( if map.search(UnusedArgInfo, PredProcId, UnusedArgs) then
        PredProcId = proc(PredId, ProcId) ,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ( if
            may_gather_warning_pragma_for_pred(ModuleInfo, PredId, PredInfo)
        then
            (
                DoWarn = do_not_warn_unused_args
            ;
                DoWarn = do_warn_unused_args,
                maybe_gather_warning(ModuleInfo, PredInfo, PredId, ProcId,
                    UnusedArgs, !WarnedPredIds, !Specs)
            ),
            (
                DoPragma = do_not_gather_pragma_unused_args
            ;
                DoPragma = do_gather_pragma_unused_args,
                maybe_gather_unused_args_pragma(PredInfo, ProcId, UnusedArgs,
                    !PragmaUnusedArgInfos)
            )
        else
            true
        )
    else
        true
    ),
    gather_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, DoWarn, DoPragma,
        PredProcIds, !.WarnedPredIds, !Specs, !PragmaUnusedArgInfos).

:- pred may_gather_warning_pragma_for_pred(module_info::in,
    pred_id::in, pred_info::in) is semidet.

may_gather_warning_pragma_for_pred(ModuleInfo, PredId, PredInfo) :-
    ( if
        may_gather_warning_pragma_for_pred_old(ModuleInfo, PredId, PredInfo)
    then
        ( if may_gather_warning_pragma_for_pred_new(PredInfo) then
            true
        else
            unexpected($pred, "old succeeds, new fails")
        )
    else
        ( if may_gather_warning_pragma_for_pred_new(PredInfo) then
            unexpected($pred, "old fails, new succeeds")
        else
            fail
        )
    ).

:- pred may_gather_warning_pragma_for_pred_old(module_info::in,
    pred_id::in, pred_info::in) is semidet.

may_gather_warning_pragma_for_pred_old(ModuleInfo, PredId, PredInfo) :-
    not pred_info_is_imported(PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus \= pred_status(status_opt_imported),

    % Don't warn about builtins that have unused arguments.
    not pred_info_is_builtin(PredInfo),
    not is_unify_index_or_compare_pred(PredInfo),

    % Don't warn about stubs for procedures with no clauses --
    % in that case, we *expect* none of the arguments to be used.
    pred_info_get_markers(PredInfo, Markers),
    not marker_is_present(Markers, marker_stub),

    % Don't warn about lambda expressions not using arguments.
    % (The warning message for these doesn't contain context,
    % so it's useless).
    Name = pred_info_name(PredInfo),
    not string.sub_string_search(Name, "__LambdaGoal__", _),

    % Don't warn for a specialized version.
    not (
        string.sub_string_search(Name, "__ho", Position),
        string.length(Name, Length),
        IdLen = Length - Position - 4,
        string.right(Name, IdLen, Id),
        string.to_int(Id, _)
    ),
    module_info_get_type_spec_tables(ModuleInfo, TypeSpecTables),
    TypeSpecTables = type_spec_tables(_, TypeSpecForcePreds, _, _),
    not set.member(PredId, TypeSpecForcePreds),

    % Don't warn for a loop-invariant hoisting-generated procedure.
    pred_info_get_origin(PredInfo, Origin),
    not (
        Origin = origin_proc_transform(proc_transform_loop_inv(_, _), _, _, _)
    ),

    % XXX We don't currently generate pragmas for the automatically
    % generated class instance methods because the compiler aborts
    % when trying to read them back in from the `.opt' files.
    not marker_is_present(Markers, marker_class_instance_method),
    not marker_is_present(Markers, marker_named_class_instance_method).

:- pred may_gather_warning_pragma_for_pred_new(pred_info::in) is semidet.

may_gather_warning_pragma_for_pred_new(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    % Previously, this test was effectively:
    % PredStatus \= pred_status(status_imported(_)),
    % PredStatus \= pred_status(status_opt_imported),
    % PredStatus \= pred_status(status_external(_)),
    % However, PredStatus cannot be status_abstract_imported,
    % and the status_pseudo_imported case is caught by
    % the test for made_for_uci below.
    pred_status_defined_in_this_module(PredStatus) = yes,

    pred_info_get_origin(PredInfo, Origin),
    require_complete_switch [Origin]
    (
        Origin = origin_user(UserMade),
        require_complete_switch [UserMade]
        (
            UserMade = user_made_pred(_, _, _),
            % Don't warn about builtins that have unused arguments.
            not pred_info_is_builtin(PredInfo)
        ;
            UserMade = user_made_lambda(_, _, _),
            % Don't warn about lambda expressions not using arguments.
            % (The warning message for these doesn't contain context,
            % so it is useless).
            % NOTE We *could* add any required context. However,
            % in some cases, people use lambdas as a shim between
            % their own code, and library predicates over whose argument
            % lists they have no control. In such cases, ignoring
            % an argument that the user code does not need but the
            % library predicate insists on supplying may be
            % the *whole point* of the lambda expression.
            %
            % XXX The above is nice reasoning, but line in the old version
            % of this test that is relevant here, namely
            %
            %   not string.sub_string_search(Name, "__LambdaGoal__", _),
            %
            % has NOT caused the test to fail since *1997*. Specifically,
            % since Tom's commit 2980b5947418ca8b0ed5312aa87d34b4c6ff5514,
            % which replaced __LambdaGoal__ in the names of the predicates
            % we construct for lambda expressions with IntroducedFrom__.
            true
        ;
            UserMade = user_made_class_method(_, _)
        ;
            UserMade = user_made_instance_method(_, _),
            % XXX We don't currently generate pragmas for the automatically
            % generated class instance methods because the compiler aborts
            % when trying to read them back in from the `.opt' files.
            %
            % I am also not sure whether we would *want* to generated warnings
            % about unused arguments in instance methods. If a class method
            % has an input that is needed by some instances but not others,
            % warning about the instances in the second category would not be
            % helpful, since the class methods needs the argument, and
            % the instance must conform to it.
            ( if
                ( marker_is_present(Markers, marker_class_instance_method)
                ; marker_is_present(Markers,
                    marker_named_class_instance_method)
                )
            then
                fail
            else
                unexpected($pred, "user_made_instance_method with marker")
            )
        ;
            UserMade = user_made_assertion(_, _, _)
            % XXX By construction, assertions should never have any
            % unused arguments, so trying to find them is a waste of time.
        ),

        % Don't warn about stubs for procedures with no clauses --
        % in that case, we *expect* none of the arguments to be used.
        %
        % XXX I (zs) am not sure whether typecheck.m can ever mark as stub
        % a predicate whose origin is not user_made_pred.
        % (There is a test filtering out predicates with marker_class_method,
        % but nothing similar for the other values of UserMade.)
        pred_info_get_markers(PredInfo, Markers),
        not marker_is_present(Markers, marker_stub)
    ;
        Origin = origin_compiler(CompilerMade),
        require_complete_switch [CompilerMade]
        (
            CompilerMade = made_for_uci(_, _),
            fail
        ;
            ( CompilerMade = made_for_deforestation(_, _)
            ; CompilerMade = made_for_solver_repn(_, _)
            ; CompilerMade = made_for_tabling(_, _)
            ; CompilerMade = made_for_mutable(_, _, _)
            ; CompilerMade = made_for_initialise(_, _)
            ; CompilerMade = made_for_finalise(_, _)
            )
            % XXX It is likely that some of these kinds of predicates
            % can never contain unused args, which means that
            % processing them is pointless.
        )
    ;
        Origin = origin_pred_transform(PredTransform, _, _),
        require_complete_switch [PredTransform]
        (
            PredTransform = pred_transform_pragma_type_spec(_),
            fail
        ;
            ( PredTransform = pred_transform_distance_granularity(_)
            ; PredTransform = pred_transform_table_generator
            ; PredTransform = pred_transform_ssdebug(_)
            ; PredTransform = pred_transform_structure_reuse
            )
        )
    ;
        Origin = origin_proc_transform(ProcTransform, _, _, _),
        require_complete_switch [ProcTransform]
        (
            ( ProcTransform = proc_transform_loop_inv(_, _)
            ; ProcTransform = proc_transform_higher_order_spec(_)
            ),
            fail
        ;
            ( ProcTransform = proc_transform_user_type_spec(_, _)
            ; ProcTransform = proc_transform_accumulator(_, _)
            ; ProcTransform = proc_transform_tuple(_, _)
            ; ProcTransform = proc_transform_untuple(_, _)
            ; ProcTransform = proc_transform_dep_par_conj(_)
            ; ProcTransform = proc_transform_par_loop_ctrl
            ; ProcTransform = proc_transform_lcmc(_, _)
            ; ProcTransform = proc_transform_stm_expansion
            ; ProcTransform = proc_transform_io_tabling
            ; ProcTransform = proc_transform_direct_arg_in_out
            )
            % XXX It is likely that some of these kinds of predicates
            % can never contain unused args, which means that
            % processing them is pointless.
        ;
            ProcTransform = proc_transform_unused_args(_),
            % These shouldn't have been created yet,
            % since we do not ever repeat the unused_args pass.
            unexpected($pred, "proc_transform_unused_args")
        )
    ).

:- pred maybe_gather_warning(module_info::in, pred_info::in,
    pred_id::in, proc_id::in, list(int)::in,
    set(pred_id)::in, set(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_gather_warning(ModuleInfo, PredInfo, PredId, ProcId, UnusedArgs0,
        !WarnedPredIds, !Specs) :-
    ( if set.member(PredId, !.WarnedPredIds) then
        true
    else
        set.insert(PredId, !WarnedPredIds),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, Proc),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        proc_info_get_headvars(Proc, HeadVars),
        NumExtraArgs = num_extra_args(PredFormArity, HeadVars),
        % Strip off the extra type_info/typeclass_info arguments
        % inserted at the front by polymorphism.m.
        drop_poly_inserted_args(NumExtraArgs, UnusedArgs0, UnusedArgs),
        (
            UnusedArgs = [_ | _],
            Spec = report_unused_args(ModuleInfo, PredInfo, UnusedArgs),
            !:Specs = [Spec | !.Specs]
        ;
            UnusedArgs = []
        )
    ).

    % Adjust the argument numbers from how they look in an argument list
    % *with* the extra arguments inserted by polymorphism, to how they would
    % look without them. This means dropping the inserted argument
    % if they appear, and subtracting the number of inserted arguments
    % from the argument numbers of all the other arguments.
    %
:- pred drop_poly_inserted_args(int::in, list(int)::in, list(int)::out) is det.

drop_poly_inserted_args(_, [], []).
drop_poly_inserted_args(NumInserted, [HeadArgWith | TailArgsWith],
        ArgsWithout) :-
    drop_poly_inserted_args(NumInserted, TailArgsWith, TailArgsWithout),
    HeadArgWithout = HeadArgWith - NumInserted,
    ( if HeadArgWithout < 1 then
        ArgsWithout = TailArgsWithout
    else
        ArgsWithout = [HeadArgWithout | TailArgsWithout]
    ).

    % Warn about unused arguments in a predicate. We consider an argument
    % unused *only* if it is unused in *every* mode of the predicate.
    % We also never warn about arguments inserted by the polymorphism pass.
    %
    % The latter test is done by maybe_gather_warning with help from
    % drop_poly_inserted_args.
    %
    % XXX I (zs) would like to know where the first test is done,
    % since it is *not* done here. My suspicion is that it is not done at all.
    %
:- func report_unused_args(module_info, pred_info, list(int)) = error_spec.

report_unused_args(_ModuleInfo, PredInfo, UnusedArgs) = Spec :-
    list.length(UnusedArgs, NumArgs),
    pred_info_get_context(PredInfo, Context),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc,
        user_arity(UserArityInt), PredFormArity),
    SNA = sym_name_arity(qualified(ModuleName, PredName), UserArityInt),
    Pieces1 = [words("In"), fixed(pred_or_func_to_full_str(PredOrFunc)),
        qual_sym_name_arity(SNA), suffix(":"), nl, words("warning:")],
    UnusedArgNs = list.map(func(N) = int_fixed(N), UnusedArgs),
    UnusedArgPieces = piece_list_to_color_pieces(color_subject, "and", [],
        UnusedArgNs),
    ( if NumArgs = 1 then
        Pieces2 = [words("argument")] ++ UnusedArgPieces ++
            [words("is")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
    else
        Pieces2 = [words("arguments")] ++ UnusedArgPieces ++
            [words("are")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
    ),
    Spec = spec($pred, severity_warning(warn_requested_by_option),
        phase_code_gen, Context, Pieces1 ++ Pieces2).

%---------------------------------------------------------------------------%

:- pred maybe_gather_unused_args_pragma(pred_info::in, proc_id::in,
    list(int)::in,
    set(gen_pragma_unused_args_info)::in,
    set(gen_pragma_unused_args_info)::out) is det.

maybe_gather_unused_args_pragma(PredInfo, ProcId, UnusedArgs,
        !UnusedArgInfos) :-
    ( if
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_opt_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        ),
        UnusedArgs = [_ | _]
    then
        ModuleName = pred_info_module(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, PredSymName,
            UserArity, ModeNum),
        % We can either collect a set of gen_pragma_unused_args
        % with dummy contexts and item sequence numbers now,
        % or we can collect PredNameArityPFMn/UnusedArgs pairs,
        % and add the dummy contexts and item sequence numbers to them
        % later. Both should work; this is marginally simpler to program.
        UnusedArgInfo = gen_pragma_unused_args_info(PredNameArityPFMn,
            UnusedArgs, dummy_context, item_no_seq_num),
        set.insert(UnusedArgInfo, !UnusedArgInfos)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.unused_args_warn_pragma.
%---------------------------------------------------------------------------%
