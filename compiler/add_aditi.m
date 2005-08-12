%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__add_aditi.
:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__make_hlds__qual_info.
:- import_module hlds__make_hlds__state_var.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module io.
:- import_module list.

:- inst aditi_update_str
    --->    "aditi_insert"
    ;       "aditi_delete"
    ;       "aditi_bulk_insert"
    ;       "aditi_bulk_delete"
    ;       "aditi_bulk_modify".

    % See the "Aditi update syntax" section of the
    % Mercury Language Reference Manual.
    %
:- pred transform_aditi_builtin(string::in(aditi_update_str),
    list(prog_term)::in, prog_context::in, hlds_goal::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % Produce an invalid goal when parsing of an Aditi update fails.
    %
:- pred invalid_goal(string::in, list(prog_term)::in, hlds_goal_info::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred set_pred_owner(sym_name::in, arity::in, string::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred add_base_relation_index(sym_name::in, arity::in, index_spec::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

:- implementation.

:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_out.
:- import_module hlds__make_hlds__add_clause.
:- import_module hlds__make_hlds__make_hlds_passes.
:- import_module hlds__make_hlds__qual_info.
:- import_module hlds__make_hlds__superhomogeneous.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_io_goal.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

transform_aditi_builtin(UpdateStr, Args0, Context, Goal, !VarSet,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        ( UpdateStr = "aditi_insert", Update = insert
        ; UpdateStr = "aditi_delete", Update = delete
        )
    ->
        transform_aditi_tuple_update(UpdateStr, Update, Args0,
            Context, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        ( UpdateStr = "aditi_bulk_insert", Update = bulk_insert
        ; UpdateStr = "aditi_bulk_delete", Update = bulk_delete
        ; UpdateStr = "aditi_bulk_modify", Update = bulk_modify
        )
    ->
        transform_aditi_bulk_update(UpdateStr, Update, Args0,
            Context, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        error("transform_aditi_builtin")
    ).

:- pred transform_aditi_tuple_update(string::in, aditi_tuple_update::in,
    list(prog_term)::in, prog_context::in, hlds_goal::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

transform_aditi_tuple_update(UpdateStr, Update, Args0, Context,
        Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    % Build an empty goal_info.
    goal_info_init(Context, GoalInfo),

    %
    % Syntax -
    % aditi_insert(p(_DB, X, Y), DB0, DB).
    %
    % `p(_DB, X, Y)' is the tuple to insert, not a higher-order term.
    %
    ( Args0 = [InsertTupleTerm, AditiState0Term, AditiStateTerm] ->
        (
            % Parse the tuple to insert.
            parse_pred_or_func_and_args(InsertTupleTerm,
                PredOrFunc, SymName, TupleArgTerms)
        ->
            %
            % Make new variables for the arguments.
            % The argument list of the `aditi_insert'
            % goal contains the arguments of the tuple
            % to insert and the `aditi__state' arguments.
            %
            make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
                !VarSet, !SInfo, !IO),
            make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
                !VarSet, !SInfo, !IO),
            make_fresh_arg_vars(TupleArgTerms, TupleArgVars,
                !VarSet, !SInfo, !IO),
            list__append(TupleArgVars,
                [AditiState0Var, AditiStateVar], AllArgs),
            list__length(TupleArgVars, InsertArity),

            PredId = invalid_pred_id,
            Builtin = aditi_tuple_update(Update, PredId),
            InsertCallId = PredOrFunc - SymName/InsertArity,
            Call = generic_call(
                aditi_builtin(Builtin, InsertCallId),
                AllArgs, [], det),
            Goal0 = Call - GoalInfo,
            CallId = generic_call(aditi_builtin(Builtin,
                InsertCallId)),
            list__append(TupleArgTerms,
                [AditiState0Term, AditiStateTerm],
                AllArgTerms),

            record_called_pred_or_func(PredOrFunc, SymName, InsertArity,
                !QualInfo),
            insert_arg_unifications(AllArgs, AllArgTerms, Context,
                call(CallId), Goal0, Goal, !VarSet, !ModuleInfo, !QualInfo,
                !SInfo, !IO)
        ;
            invalid_goal(UpdateStr, Args0, GoalInfo,
                Goal, !VarSet, !SInfo, !IO),
            qual_info_set_found_syntax_error(yes, !QualInfo),
            io__set_exit_status(1, !IO),
            prog_out__write_context(Context, !IO),
            io__write_string("Error: expected tuple to ", !IO),
            io__write(Update, !IO),
            io__write_string(" in `", !IO),
            io__write_string(UpdateStr, !IO),
            io__write_string("'.\n", !IO)
        )
    ;
        invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet,
            !SInfo, !IO),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        list__length(Args0, Arity),
        aditi_update_arity_error(Context, UpdateStr, Arity, [3], !IO)
    ).

    % Parse an `aditi_delete' or `aditi_modify' goal.
    %
:- pred transform_aditi_bulk_update(string::in, aditi_bulk_update::in,
    list(prog_term)::in, prog_context::in, hlds_goal::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

transform_aditi_bulk_update(Descr, Update, Args0, Context, UpdateGoal,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    goal_info_init(Context, GoalInfo),
    (
        list__length(Args0, Arity),
        Arity \= 3,
        Arity \= 4
    ->
        invalid_goal(Descr, Args0, GoalInfo, UpdateGoal, !VarSet, !SInfo, !IO),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        aditi_update_arity_error(Context, Descr, Arity, [3, 4], !IO)
    ;
        %
        % First syntax -
        %   aditi_insert((p(X, Y, _DB0) :- X = 2, Y = 1), DB0, DB).
        % or
        %   aditi_delete((p(X, Y, _DB0) :- X = 2), DB0, DB).
        % or
        %   aditi_modify((p(X0, Y0, _DB0) ==> p(X0, Y, _DB) :-
        %       X0 < 100, Y = Y0 + 1), DB0, DB).
        %
        Args0 = [HOTerm, AditiState0Term, AditiStateTerm],
        parse_rule_term(Context, HOTerm, HeadTerm, GoalTerm1),
        (
            Update = bulk_insert,
            parse_pred_or_func_and_args(HeadTerm, PredOrFunc, SymName,
	    	HeadArgs1),
            list__length(HeadArgs1, PredArity)
        ;
            Update = bulk_delete,
            parse_pred_or_func_and_args(HeadTerm,
                PredOrFunc, SymName, HeadArgs1),
            list__length(HeadArgs1, PredArity)
        ;
            Update = bulk_modify,
            HeadTerm = term__functor(term__atom("==>"),
                [LeftHeadTerm, RightHeadTerm], _),
            parse_pred_or_func_and_args(LeftHeadTerm,
                PredOrFunc, SymName, LeftHeadArgs),
            parse_pred_or_func_and_args(RightHeadTerm,
                PredOrFunc, SymName, RightHeadArgs),
            list__append(LeftHeadArgs, RightHeadArgs, HeadArgs1),
            list__length(LeftHeadArgs, PredArity),
            list__length(RightHeadArgs, PredArity)
        )
    ->
        %
        % This syntax is transformed into a construction of
        % a lambda expression for the modification condition
        % and a call to an update goal with that closure.
        % The transformed code is equivalent to the
        % `sym_name_and_closure' syntax which is parsed below.
        %
        Syntax = pred_term,

        %
        % Parse the modification goal as for a lambda expression.
        %
        make_fresh_arg_vars(HeadArgs1, HeadArgs, !VarSet, !SInfo, !IO),
        term__coerce(GoalTerm1, GoalTerm),
        parse_goal(GoalTerm, ParsedGoal, !VarSet),

        prepare_for_lambda(!SInfo),

        hlds_goal__true_goal(PredHead0),
        ArgContext = head(PredOrFunc, PredArity),
        insert_arg_unifications(HeadArgs, HeadArgs1, Context, ArgContext,
            PredHead0, PredHead, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        prepare_for_body(FinalSVarMap, !VarSet, !SInfo),

        map__init(Substitution),
        transform_goal(ParsedGoal, Substitution, PredBody,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        finish_goals(Context, FinalSVarMap, [PredHead, PredBody],
            PredGoal0, !.SInfo),

        % Quantification will reduce this down to
        % the proper set of nonlocal arguments.
        goal_util__goal_vars(PredGoal, LambdaGoalVars0),
        set__delete_list(LambdaGoalVars0, HeadArgs, LambdaGoalVars1),
        set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals),
        aditi_bulk_update_goal_info(Update,
            PredOrFunc, SymName, PredArity, HeadArgs,
            LambdaPredOrFunc, EvalMethod, LambdaModes,
            Detism, PredGoal0, PredGoal),
        ModifiedCallId = PredOrFunc - SymName/PredArity,

        PredId = invalid_pred_id,
        Builtin = aditi_bulk_update(Update, PredId, Syntax),
        MainContext =
            call(generic_call(
                aditi_builtin(Builtin, ModifiedCallId)),
            1),
        varset__new_var(!.VarSet, LambdaVar, !:VarSet),

        % Tell purity.m to change the mode of the `aditi__state'
        % arguments of the closure to `unused', to make sure
        % that the closure does not call any Aditi relations.
        % We don't know which argument is the `aditi__state' until
        % after typechecking.
        % The `aditi__state's are passed even though they are not
        % used to make the arguments of the closure match the
        % arguments of the relation being updated.
        FixModes = modes_need_fixing,

        % Build the lambda expression for the modification condition.
        make_atomic_unification(LambdaVar,
            lambda_goal((pure), LambdaPredOrFunc, EvalMethod,
                FixModes, LambdaNonLocals,
                HeadArgs, LambdaModes, Detism, PredGoal),
            Context, MainContext, [], LambdaConstruct, !QualInfo),

        make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
            !VarSet, !SInfo, !IO),
        make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
            !VarSet, !SInfo, !IO),
        AllArgs = [LambdaVar, AditiState0Var, AditiStateVar],

        % post_typecheck.m will fill this in.
        GenericCallModes = [],

        Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
            AllArgs, GenericCallModes, det) - GoalInfo,

        %
        % Wrap an explicit quantification around the goal to make
        % sure that the closure construction and the
        % `aditi_delete' or `aditi_modify' call are not separated.
        % Separating the goals would make optimization of the update
        % using indexes more difficult.
        %
        UpdateConj = scope(barrier(not_removable),
            conj([LambdaConstruct, Call]) - GoalInfo) - GoalInfo,

        CallId = call(generic_call(
            aditi_builtin(Builtin, ModifiedCallId))),

        record_called_pred_or_func(PredOrFunc, SymName, PredArity, !QualInfo),
        insert_arg_unifications(AllArgs,
            [term__variable(LambdaVar), AditiState0Term, AditiStateTerm],
            Context, CallId, UpdateConj, UpdateGoal, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO)
    ;
        %
        % Second syntax -
        % aditi_bulk_delete(pred p/3,
        %   (aditi_bottom_up pred(..) :- ..), DB0, DB).
        %
        % The `pred_term' syntax parsed above is transformed
        % into the equivalent of this syntax.
        %
        Args0 = [PredCallIdTerm | OtherArgs0],
        OtherArgs0 = [_, _, _],

        parse_pred_or_func_name_and_arity(PredCallIdTerm, PredOrFunc, SymName,
            Arity0),
        adjust_func_arity(PredOrFunc, Arity0, Arity)
    ->
        Syntax = sym_name_and_closure,

        make_fresh_arg_vars(OtherArgs0, OtherArgs, !VarSet, !SInfo, !IO),
        PredId = invalid_pred_id,

        Builtin = aditi_bulk_update(Update, PredId, Syntax),

        ModifiedCallId = PredOrFunc - SymName/Arity,

        % post_typecheck.m will fill this in.
        GenericCallModes = [],

        Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
            OtherArgs, GenericCallModes, det) - GoalInfo,
        CallId = call(generic_call(aditi_builtin(Builtin, ModifiedCallId))),
        record_called_pred_or_func(PredOrFunc, SymName, Arity, !QualInfo),
        insert_arg_unifications(OtherArgs, OtherArgs0, Context, CallId,
            Call, UpdateGoal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        invalid_goal(Descr, Args0, GoalInfo, UpdateGoal, !VarSet, !SInfo, !IO),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        io__set_exit_status(1, !IO),
        output_expected_aditi_update_syntax(Context, Update, !IO)
    ).

:- pred aditi_bulk_update_goal_info(aditi_bulk_update::in, pred_or_func::in,
    sym_name::in, arity::in, list(prog_var)::in, pred_or_func::out,
    lambda_eval_method::out, list(mode)::out, determinism::out,
    hlds_goal::in, hlds_goal::out) is det.

aditi_bulk_update_goal_info(bulk_insert, PredOrFunc, _SymName,
        PredArity, _Args, LambdaPredOrFunc, EvalMethod,
        LambdaModes, Detism, Goal, Goal) :-
    LambdaPredOrFunc = PredOrFunc,
    EvalMethod = (aditi_bottom_up),
    out_mode(OutMode),
    Detism = nondet,
    % Modes for the arguments of the input tuple.
    list__duplicate(PredArity, OutMode, LambdaModes).

aditi_bulk_update_goal_info(bulk_delete, PredOrFunc,
        SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
        LambdaModes, Detism, Goal0, Goal) :-
    LambdaPredOrFunc = PredOrFunc,
    EvalMethod = (aditi_bottom_up),
    Detism = nondet,
    out_mode(OutMode),
    list__duplicate(PredArity, OutMode, LambdaModes),

    % Join the result of the deletion goal with the relation to be updated.
    conjoin_aditi_update_goal_with_call(PredOrFunc, SymName,
        Args, Goal0, Goal).

aditi_bulk_update_goal_info(bulk_modify, PredOrFunc,
        SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
        LambdaModes, Detism, Goal0, Goal) :-

    % The closure passed to `aditi_modify' and `aditi_bulk_modify'
    % is always a predicate closure.
    LambdaPredOrFunc = predicate,

    out_mode(OutMode),
    EvalMethod = (aditi_bottom_up),
    Detism = nondet,

    % Modes for the arguments corresponding to the input tuple.
    list__duplicate(PredArity, OutMode, DeleteModes),

    % `Args' must have length `PredArity * 2', so this will always succeed.
    ( list__take(PredArity, Args, CallArgs0) ->
        CallArgs = CallArgs0
    ;
        error("aditi_delete_insert_delete_modify_goal_info")
    ),

    % Join the result of the modify goal with the relation to be updated.
    conjoin_aditi_update_goal_with_call(PredOrFunc, SymName,
        CallArgs, Goal0, Goal),

    % Modes for the arguments corresponding to the output tuple.
    list__duplicate(PredArity, OutMode, InsertModes),
    list__append(DeleteModes, InsertModes, LambdaModes).

:- pred conjoin_aditi_update_goal_with_call(pred_or_func::in, sym_name::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out) is det.

conjoin_aditi_update_goal_with_call(PredOrFunc, SymName, Args, Goal0, Goal) :-
    PredId = invalid_pred_id,
    Goal0 = _ - GoalInfo,

    % The predicate is recorded as used in
    % transform_aditi_tuple_update and
    % transform_aditi_insert_delete_modify
    do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
        GoalInfo, CallGoal),

    Goal = conj([CallGoal, Goal0]) - GoalInfo.

:- pred output_expected_aditi_update_syntax(prog_context::in,
    aditi_bulk_update::in, io::di, io::uo) is det.

output_expected_aditi_update_syntax(Context, bulk_insert, !IO) :-
    output_insert_or_delete_expected_syntax(Context, "aditi_bulk_insert", !IO).
output_expected_aditi_update_syntax(Context, bulk_delete, !IO) :-
    output_insert_or_delete_expected_syntax(Context, "aditi_bulk_delete", !IO).
output_expected_aditi_update_syntax(Context, bulk_modify, !IO) :-
    Name = "aditi_bulk_modify",
    prog_out__write_context(Context, !IO),
    io__write_string("Error: expected\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  `", !IO),
    io__write_string(Name, !IO),
    io__write_string("(\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("    (p(<Args0>) ==> p(<Args>) :- <Goal>),\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string( "    DB0, DB)'\n", !IO),
    output_aditi_closure_syntax(Context, Name, !IO).

:- pred output_insert_or_delete_expected_syntax(prog_context::in, string::in,
    io::di, io::uo) is det.

output_insert_or_delete_expected_syntax(Context, Name, !IO) :-
    prog_out__write_context(Context, !IO),
    io__write_string("Error: expected `", !IO),
    io__write_string(Name, !IO),
    io__write_string("((p(<Args>) :- <Goal>), DB0, DB)'\n", !IO),
    output_aditi_closure_syntax(Context, Name, !IO).

:- pred output_aditi_closure_syntax(prog_context::in, string::in,
    io::di, io::uo) is det.

output_aditi_closure_syntax(Context, Name, !IO) :-
    prog_out__write_context(Context, !IO),
    io__write_string("  or `", !IO),
    io__write_string(Name, !IO),
    io__write_string("(PredOrFunc p/N, Closure, DB0, DB)'.\n", !IO).

    % Report an error for an Aditi update with the wrong number
    % of arguments.
    %
:- pred aditi_update_arity_error(prog_context::in, string::in, int::in,
    list(int)::in, io::di, io::uo) is det.

aditi_update_arity_error(Context, UpdateStr, Arity, ExpectedArities, !IO) :-
    io__set_exit_status(1, !IO),
    MaybePredOrFunc = no,
    prog_out__write_context(Context, !IO),
    io__write_string("Error: ", !IO),
    MaybePredOrFunc = no,
    report_error_num_args(MaybePredOrFunc, Arity, ExpectedArities, !IO),
    io__nl(!IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  in `", !IO),
    io__write_string(UpdateStr, !IO),
    io__write_string("'.\n", !IO).

invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet, !SInfo, !IO) :-
    make_fresh_arg_vars(Args0, HeadVars, !VarSet, !SInfo, !IO),
    MaybeUnifyContext = no,
    Goal = call(invalid_pred_id, invalid_proc_id, HeadVars, not_builtin,
        MaybeUnifyContext, unqualified(UpdateStr)) - GoalInfo.

set_pred_owner(Name, Arity, Owner, Status, Context, !ModuleInfo, !IO) :-
    SetOwner = (pred(PredInfo0::in, PredInfo::out) is det :-
        pred_info_set_aditi_owner(Owner, PredInfo0, PredInfo)
    ),
    MarkerMustBeExported = yes,
    do_add_pred_marker("owner", Name, Arity, Status, MarkerMustBeExported,
        Context, SetOwner, !ModuleInfo, _, !IO).

add_base_relation_index(Name, Arity, Index, Status, Context, !ModuleInfo,
        !IO) :-
    AddIndex = (pred(PredInfo0::in, PredInfo::out) is det :-
        pred_info_get_indexes(PredInfo0, Indexes0),
        Indexes = [Index | Indexes0],
        pred_info_set_indexes(Indexes, PredInfo0, PredInfo)
    ),
    MarkerMustBeExported = yes,
    do_add_pred_marker("aditi_index", Name, Arity, Status,
        MarkerMustBeExported, Context, AddIndex, !ModuleInfo, PredIds, !IO),
    Index = index_spec(_, Attrs),
    list__foldl(check_index_attribute(Name, Arity, Context), Attrs, !IO),
    list__foldl(
        check_index_attribute_pred(!.ModuleInfo, Name, Arity, Context, Attrs),
        PredIds, !IO).

    % Check that the index attributes are legal for the predicate's arity.
    %
:- pred check_index_attribute(sym_name::in, arity::in, term__context::in,
    int::in, io::di, io::uo) is det.

check_index_attribute(Name, Arity, Context, Attr, !IO) :-
    (
        Attr > 0,
        Attr =< Arity
    ->
        true
    ;
        prog_out__write_context(Context, !IO),
        io__write_string("In `:- pragma aditi_index' declaration for `", !IO),
        prog_out__write_sym_name_and_arity(Name/Arity, !IO),
        io__write_string("':\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  attribute ", !IO),
        io__write_int(Attr, !IO),
        io__write_string(" is out of range.\n", !IO),
        io__set_exit_status(1, !IO)
    ).

    % Check that a relation with an index specified is a base relation
    % and that the indexed attributes do not include aditi__states.
    %
:- pred check_index_attribute_pred(module_info::in, sym_name::in, arity::in,
    term__context::in, list(int)::in, pred_id::in, io::di, io::uo) is det.

check_index_attribute_pred(ModuleInfo, Name, Arity, Context, Attrs, PredId,
        !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ( check_marker(Markers, base_relation) ->
        true
    ;
        prog_out__write_context(Context, !IO),
        io__write_string("Error: `:- pragma aditi_index' declaration", !IO),
        io__nl(!IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  for ", !IO),
        write_simple_call_id(PredOrFunc, Name/Arity, !IO),
        io__write_string(" without preceding\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  `:- pragma base_relation' declaration.\n", !IO),
        io__set_exit_status(1, !IO)
    ),

    pred_info_arg_types(PredInfo, ArgTypes),
    AttrIsAditiState = (pred(Attr::in) is semidet :-
        list__index0(ArgTypes, Attr, ArgType),
        type_is_aditi_state(ArgType)
    ),
    list__filter(AttrIsAditiState, Attrs, AditiStateAttrs),
    ( AditiStateAttrs = [AditiStateAttr | _] ->
        % Indexing on aditi__state attributes is pretty silly,
        % since they're removed by magic.m.
        prog_out__write_context(Context, !IO),
        io__write_string("In `:- pragma aditi_index' declaration for ", !IO),
        write_simple_call_id(PredOrFunc, Name/Arity, !IO),
        io__write_string(":\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  attribute ", !IO),
        io__write_int(AditiStateAttr, !IO),
        io__write_string(" is an aditi__state.\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        true
    ).
