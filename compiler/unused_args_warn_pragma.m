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
:- pred gather_warnings_and_pragmas(module_info::in,
    proc_to_unused_args_map::in,
    maybe_warn_unused_args::in, maybe_gather_pragma_unused_args::in,
    list(error_spec)::out, set(gen_pragma_unused_args_info)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.mode_test.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

gather_warnings_and_pragmas(ModuleInfo, ProcToUnusedArgsMap,
        DoWarn, DoPragma, Specs, PragmaUnusedArgInfos) :-
    map.foldl2(gather_warnings_and_pragmas_ppid(ModuleInfo, DoWarn, DoPragma),
        ProcToUnusedArgsMap,
        map.init, WarnUnusedPredArgsMap, set.init, PragmaUnusedArgInfos),
    map.foldl(warn_unused_args_in_pred, WarnUnusedPredArgsMap, [], Specs).

:- pred gather_warnings_and_pragmas_ppid(module_info::in,
    maybe_warn_unused_args::in, maybe_gather_pragma_unused_args::in,
    pred_proc_id::in, list(int)::in,
    warn_unused_pred_args_map::in, warn_unused_pred_args_map::out,
    set(gen_pragma_unused_args_info)::in,
    set(gen_pragma_unused_args_info)::out) is det.

gather_warnings_and_pragmas_ppid(ModuleInfo, DoWarn, DoPragma,
        PredProcId, UnusedArgs,
        !WarnUnusedPredArgsMap, !PragmaUnusedArgInfos) :-
    PredProcId = proc(PredId, ProcId) ,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if may_gather_warning_pragma_for_pred(PredInfo) then
        (
            DoWarn = do_not_warn_unused_args
        ;
            DoWarn = do_warn_unused_args,
            maybe_add_proc_to_unused_args_map(ModuleInfo, PredInfo, PredId,
                ProcId, UnusedArgs, !WarnUnusedPredArgsMap)
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
    ).

:- pred may_gather_warning_pragma_for_pred(pred_info::in) is semidet.

may_gather_warning_pragma_for_pred(PredInfo) :-
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

%---------------------------------------------------------------------------%

:- type warn_unused_pred_args_map == map(pred_id, warn_unused_pred_args).

:- type warn_unused_pred_args
    --->    warn_unused_pred_args(
                pred_info,
                one_or_more(pair(proc_id, unused_proc_args))
            ).

:- type unused_proc_args == one_or_more(unused_proc_arg).
:- type unused_proc_arg
    --->    unused_proc_arg(
                % The argument number.
                int,

                maybe_marked_unused
            ).

:- type maybe_marked_unused
    --->    not_marked_unused
    ;       marked_unused.

:- pred maybe_add_proc_to_unused_args_map(module_info::in, pred_info::in,
    pred_id::in, proc_id::in, list(int)::in,
    warn_unused_pred_args_map::in, warn_unused_pred_args_map::out) is det.

maybe_add_proc_to_unused_args_map(ModuleInfo, PredInfo, PredId, ProcId,
        UnusedArgs0, !WarnUnusedPredArgsMap) :-
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    NumExtraArgs = num_extra_args(PredFormArity, ArgTypes),
    % Strip off the extra type_info/typeclass_info arguments
    % inserted at the front by polymorphism.m.
    drop_poly_inserted_args(NumExtraArgs, UnusedArgs0, UnusedArgs),
    (
        UnusedArgs = [_ | _],
        pred_info_proc_info(PredInfo, ProcId, ProcInfo),
        proc_info_get_argmodes(ProcInfo, ArgModes0),
        list.det_drop(NumExtraArgs, ArgModes0, ArgModes),
        record_which_unused_args_are_marked(ModuleInfo, ArgModes,
            UnusedArgs, UnusedProcArgs),
        % If UnusedArgs is not empty, then UnusedProcArgs cannot be empty.
        det_list_to_one_or_more(UnusedProcArgs, OoMUnusedProcArgs),
        ( if
            map.search(!.WarnUnusedPredArgsMap, PredId, WarnUnusedPredArgs0)
        then
            WarnUnusedPredArgs0 = warn_unused_pred_args(_PredInfo, ProcAL0),
            one_or_more.cons(ProcId - OoMUnusedProcArgs, ProcAL0, ProcAL),
            WarnUnusedPredArgs = warn_unused_pred_args(PredInfo, ProcAL),
            map.det_update(PredId, WarnUnusedPredArgs, !WarnUnusedPredArgsMap)
        else
            ProcAL = one_or_more(ProcId - OoMUnusedProcArgs, []),
            WarnUnusedPredArgs = warn_unused_pred_args(PredInfo, ProcAL),
            map.det_insert(PredId, WarnUnusedPredArgs, !WarnUnusedPredArgsMap)
        )
    ;
        UnusedArgs = []
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

:- pred record_which_unused_args_are_marked(module_info::in,
    list(mer_mode)::in, list(int)::in, list(unused_proc_arg)::out) is det.

record_which_unused_args_are_marked(_, _, [], []).
record_which_unused_args_are_marked(ModuleInfo, ArgModes,
        [ArgNum | ArgNums], [ProcArg | ProcArgs]) :-
    list.det_index1(ArgModes, ArgNum, ArgMode),
    ( if mode_is_unused(ModuleInfo, ArgMode) then
        MaybeMarked = marked_unused
    else
        MaybeMarked = not_marked_unused
    ),
    ProcArg = unused_proc_arg(ArgNum, MaybeMarked),
    record_which_unused_args_are_marked(ModuleInfo, ArgModes,
        ArgNums, ProcArgs).

%---------------------------------------------------------------------------%

:- pred warn_unused_args_in_pred(pred_id::in, warn_unused_pred_args::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_unused_args_in_pred(_PredId, WarnUnusedPredArgs, !Specs) :-
    WarnUnusedPredArgs = warn_unused_pred_args(PredInfo, ProcUnusedArgsAL0),
    one_or_more.sort(ProcUnusedArgsAL0, ProcUnusedArgsAL),
    pred_info_get_proc_table(PredInfo, ProcTable),
    one_or_more.foldl2(do_all_procs_have_same_unused_args, ProcUnusedArgsAL,
        map.init, UnusedArgsToProcMap, ProcTable, UnmentionedProcTable),
    map.to_assoc_list(UnusedArgsToProcMap, UnusedArgsToProcAL),
    % We can generate a single warning that applies to the whole predicate
    % only if
    % - all procedures have some unused arguments, and
    % - they all have the *same set* of unused arguments, *and*
    %   they agree on which unused args are marked as such.
    ( if
        map.is_empty(UnmentionedProcTable),
        UnusedArgsToProcAL = [UnusedProcArgs - _OoMProcIds]
    then
        report_pred_general_unused_args(PredInfo, UnusedProcArgs, !Specs)
    else
        % Otherwise, we generate procedure-specific warnings.
        %
        % We *could* generate a single warning for whole sets of procedures
        % that share the same pattern of unused arguments, but
        %
        % - having different procedures having different sets of unused args
        %   is extremely rare, and
        %
        % - if that *does* happen, then getting the mode-specific reports
        %   in mode number order probably does more to improve readability
        %   than shortening the output would do, especially if the procedures
        %   in a set sharing the same set of unused args are not adjacent.
        one_or_more.foldl(report_proc_specific_unused_args(PredInfo),
            ProcUnusedArgsAL, !Specs)
    ).

:- pred do_all_procs_have_same_unused_args(
    pair(proc_id, unused_proc_args)::in,
    map(unused_proc_args, one_or_more(proc_id))::in,
    map(unused_proc_args, one_or_more(proc_id))::out,
    map(proc_id, proc_info)::in, map(proc_id, proc_info)::out) is det.

do_all_procs_have_same_unused_args(ProcId - ProcUnusedArgs,
        !UnusedArgsToProcmap, !UnmentionedProcTable) :-
    ( if map.search(!.UnusedArgsToProcmap, ProcUnusedArgs, OoMProcIds0) then
        one_or_more.cons(ProcId, OoMProcIds0, OoMProcIds),
        map.det_update(ProcUnusedArgs, OoMProcIds, !UnusedArgsToProcmap)
    else
        OoMProcIds = one_or_more(ProcId, []),
        map.det_insert(ProcUnusedArgs, OoMProcIds, !UnusedArgsToProcmap)
    ),
    % By construction, each ProcId may appear in the input list
    % (or actually one_or_more) only once.
    map.det_remove(ProcId, _, !UnmentionedProcTable).

    % Warn about unused arguments in a predicate.
    %
    % We never warn about arguments inserted by the polymorphism pass.
    % This filtering out is done by the call to drop_poly_inserted_args
    % in maybe_add_proc_to_unused_args_map above.
    %
:- pred report_pred_general_unused_args(pred_info::in, unused_proc_args::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_pred_general_unused_args(PredInfo, ProcUnusedArgs, !Specs) :-
    NameColonNlPieces = describe_one_pred_info_name(yes(color_subject),
        should_not_module_qualify, [suffix(":"), nl], PredInfo),
    pred_info_get_context(PredInfo, Context),
    report_unused_args(NameColonNlPieces, Context, ProcUnusedArgs, !Specs).

:- pred report_proc_specific_unused_args(pred_info::in,
    pair(proc_id, unused_proc_args)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_proc_specific_unused_args(PredInfo, ProcId - ProcUnusedArgs, !Specs) :-
    NameColonNlPieces = describe_one_proc_name_pred_info_maybe_argmodes(
        PredInfo, output_mercury, yes(color_subject),
        should_not_module_qualify, [suffix(":"), nl], ProcId),
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    report_unused_args(NameColonNlPieces, Context, ProcUnusedArgs, !Specs).

:- pred report_unused_args(list(format_piece)::in, prog_context::in,
    unused_proc_args::in, list(error_spec)::in, list(error_spec)::out) is det.

report_unused_args(NameColonNlPieces, Context, ProcUnusedArgs, !Specs) :-
    one_or_more.foldl2(classify_unused_proc_arg, ProcUnusedArgs,
        [], UnmarkedArgs0, [], MarkedArgs0),
    list.sort(UnmarkedArgs0, UnmarkedArgs),
    list.sort(MarkedArgs0, MarkedArgs),
    (
        UnmarkedArgs = []
        % Since ProcUnusedArgs cannot be empty, we get here only if
        % all the unused arguments are explicitly marked as such.
        % In such cases, the warning would be a distraction, not a help.
    ;
        UnmarkedArgs = [_ | TailUnmarkedArgs],
        Pieces1 = [words("In")] ++ NameColonNlPieces ++ [words("warning:")],
        UnmarkedArgPieces = piece_list_to_color_pieces(color_subject,
            "and", [], UnmarkedArgs),
        (
            TailUnmarkedArgs = [],
            Pieces2 = [words("argument")] ++ UnmarkedArgPieces ++
                [words("is")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
        ;
            TailUnmarkedArgs = [_ | _],
            Pieces2 = [words("arguments")] ++ UnmarkedArgPieces ++
                [words("are")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
        ),
        Addendum = marked_unused_args_addendum(MarkedArgs),
        Spec = spec($pred, severity_warning(warn_requested_by_option),
            phase_code_gen, Context, Pieces1 ++ Pieces2 ++ Addendum),
        !:Specs = [Spec | !.Specs]
    ).

:- pred classify_unused_proc_arg(unused_proc_arg::in,
    list(format_piece)::in, list(format_piece)::out,
    list(format_piece)::in, list(format_piece)::out) is det.

classify_unused_proc_arg(UnusedProcArg, !UnmarkedArgNums, !MarkedArgNums) :-
    UnusedProcArg = unused_proc_arg(ArgNum, MaybeMarked),
    (
        MaybeMarked = not_marked_unused,
        !:UnmarkedArgNums = [int_fixed(ArgNum) | !.UnmarkedArgNums]
    ;
        MaybeMarked = marked_unused,
        !:MarkedArgNums = [int_fixed(ArgNum) | !.MarkedArgNums]
    ).

:- func marked_unused_args_addendum(list(format_piece)) = list(format_piece).

marked_unused_args_addendum(MarkedArgs) = Pieces :-
    MarkedArgPieces = piece_list_to_color_pieces(color_subject,
        "and", [], MarkedArgs),
    (
        MarkedArgs = [],
        Pieces = []
    ;
        MarkedArgs = [_],
        Pieces = [words("(Argument")] ++ MarkedArgPieces ++
            [words("is also unused, but its mode")] ++
            color_as_correct([words("marks it")]) ++
            [words("as unused.)"), nl]
    ;
        MarkedArgs = [_, _ | _],
        Pieces = [words("(Arguments")] ++ MarkedArgPieces ++
            [words("are also unused, but their modes")] ++
            color_as_correct([words("mark them")]) ++
            [words("as unused.)"), nl]
    ).

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
