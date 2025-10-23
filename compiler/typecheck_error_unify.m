%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_unify.m.
%
% This file contains predicates to report type errors in unifications.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_unify.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- func report_error_unify_var_var(typecheck_info, unify_context, prog_context,
    prog_var, prog_var, type_assign_set) = error_spec.

:- func report_error_unify_var_lambda(typecheck_info, unify_context,
    prog_context, pred_or_func, prog_var, list(prog_var), type_assign_set)
    = error_spec.

:- func report_error_unify_var_functor_result(typecheck_info,
    unify_context, prog_context, prog_var, list(cons_type_info), cons_id,
    int, type_assign_set) = error_spec.

:- func report_error_unify_var_functor_args(typecheck_info,
    unify_context, prog_context, prog_var, list(cons_type_info), cons_id,
    list(prog_var), args_type_assign_set) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_builtin.
:- import_module check_hlds.typecheck_error_type_assign.
:- import_module check_hlds.typecheck_error_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.maybe_util.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

report_error_unify_var_var(Info, UnifyContext, Context, X, Y, TypeAssignSet)
        = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, _LastContextWord,
        InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    MaybeColor = yes(color_inconsistent),
    MainPieces = [words("type error in unification of variable"),
        quote(mercury_var_to_name_only_vs(VarSet, X)), words("and variable"),
        quote(mercury_var_to_name_only_vs(VarSet, Y)), suffix("."), nl] ++
        color_as_subject([quote(mercury_var_to_name_only_vs(VarSet, X))]) ++
        type_of_var_to_pieces(InstVarSet, MaybeColor,
            TypeAssignSet, [suffix(",")], X) ++ [nl] ++
        color_as_subject([quote(mercury_var_to_name_only_vs(VarSet, Y))]) ++
        type_of_var_to_pieces(InstVarSet, MaybeColor,
            TypeAssignSet, [suffix(".")], Y) ++ [nl],
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_lambda(Info, UnifyContext, Context, PredOrFunc,
        Var, ArgVars, TypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, LastContextWord,
        InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    VarNamePieces = argument_name_to_pieces_uc(VarSet, LastContextWord, Var),
    get_inst_varset(ClauseContext, InstVarSet),
    Pieces1 = [words("type error in unification of")] ++ VarNamePieces ++ [nl],
    (
        PredOrFunc = pf_predicate,
        Pieces2 = [words("and"), prefix("pred("),
            words(mercury_vars_to_name_only_vs(VarSet, ArgVars)),
            suffix(")"), words(":- ...':"), nl]
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        Pieces2 = [words("and"), prefix("func("),
            words(mercury_vars_to_name_only_vs(VarSet, FuncArgs)),
            suffix(")"), fixed("="),
            words(mercury_var_to_name_only_vs(VarSet, RetVar)),
            words(":- ...':"), nl]
    ),

    % XXX There is no test case that tests the output of this function,
    % so it is hard to decide what should be colored.
    MaybeColor = maybe.no,
    Pieces3 = VarNamePieces ++
        type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
            [suffix(",")], Var) ++
        [nl],

    (
        PredOrFunc = pf_predicate,
        (
            ArgVars = [],
            LambdaTypePieces = [words("pred")]
        ;
            ArgVars = [_ | _],
            list.length(ArgVars, NumArgVars),
            list.duplicate(NumArgVars, "_", Unders),
            CommaUnders = string.join_list(", ", Unders),
            LambdaTypePieces = [words("pred(_" ++ CommaUnders ++ ")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgVars, _),
        (
            FuncArgVars = [],
            LambdaTypePieces = [words("func = _")]
        ;
            FuncArgVars = [_ | _],
            list.length(FuncArgVars, NumArgVars),
            list.duplicate(NumArgVars, "_", Unders),
            CommaUnders = string.join_list(", ", Unders),
            LambdaTypePieces = [words("func(_" ++ CommaUnders ++ ") = _")]
        )
    ),
    Pieces4 = [words("lambda expression has type") | LambdaTypePieces] ++
        [suffix("."), nl],
    MainPieces = Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4,

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_functor_result(Info, UnifyContext, Context,
        Var, ConsDefnList, Functor, Arity, TypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    ( if varset.search_name(VarSet, Var, _) then
        unify_context_to_pieces(UnifyContext, _LastContextWord,
            InClauseForPieces, ContextPieces),
        VarName = mercury_var_to_name_only_vs(VarSet, Var),
        VarDescPieces = [words("variable"), quote(VarName)]
    else
        UnifyContext = unify_context(MainContext, BottomUpSubContexts),
        ( if
            BottomUpSubContexts = [BottomSubContext | NonBottomSubContexts],
            BottomSubContext = unify_sub_context(ConsId, _ArgNum),
            not cons_id_may_be_list_cons(ConsId),
            UnifyContextToUse =
                unify_context(MainContext, NonBottomSubContexts),
            unify_context_to_pieces(UnifyContextToUse, LastContextWord,
                InClauseForPieces, ContextPiecesPrime),
            ( LastContextWord = lcw_none
            ; LastContextWord = lcw_call
            ; LastContextWord = lcw_argument
            )
        then
            % Instead of printing something like:
            %
            %   <initial context>
            %   in argument N of functor F:
            %   type error in unification of argument
            %   and <other term's top functor>:
            %   Argument has type ...
            %
            % print
            %
            %   <initial context>
            %   type error in unification of argument N of functor F
            %   and <other term's top functor>:
            %   Argument N of functor F has type ...
            ContextPieces = ContextPiecesPrime,
            VarDescPieces = argument_to_pieces(BottomSubContext)
        else
            unify_context_to_pieces(UnifyContext, LastContextWord,
                InClauseForPieces, ContextPieces),
            VarDescPieces = last_context_word_to_string_lc(LastContextWord)
        )
    ),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),

    VarTypePieces = type_of_var_to_pieces(InstVarSet,
        yes(color_incorrect), TypeAssignSet, [suffix(",")], Var),
    FunctorTypePieces = type_of_functor_to_pieces(InstVarSet,
        yes(color_correct), Functor, Arity, ConsDefnList, [suffix(".")]),
    % XXX TYPECHECK_ERRORS If all elements of TypeAssignSet agree on the type
    % of Var, and if ConsDefnList contains only a single element, then
    % we should be able to print the diff between the two types,
    % including pointers such as the arity we expected vs what we got.

    MainPieces = [words("type error in unification of")] ++ VarDescPieces ++
        [nl, words("and")] ++
        functor_name_to_pieces(Functor, Arity) ++ [suffix("."), nl] ++

        color_as_subject([upper_case_next | VarDescPieces]) ++
        VarTypePieces ++ [nl] ++

        color_as_subject(functor_name_to_pieces(Functor, Arity)) ++
        FunctorTypePieces ++ [nl],

    ( if
        Functor = some_int_const(int_const(_)),
        get_all_transformed_type_stuffs(typestuff_to_type, TypeAssignSet,
            Var, TypesOfVar),
        list.any_true(type_needs_int_constant_suffix, TypesOfVar)
    then
        NoSuffixIntegerPieces = nosuffix_integer_pieces
    else
        NoSuffixIntegerPieces = []
    ),

    ( if is_int_func_op(Functor) then
        acc_builtin_types_of_var(TypeAssignSet, Var, set.init, BuiltinTypes0),
        acc_builtin_types_in_cons_type_infos(ConsDefnList,
            BuiltinTypes0, BuiltinTypes),
        InvisIntPieces =
            report_any_invisible_int_types(ClauseContext, BuiltinTypes)
    else
        InvisIntPieces = []
    ),

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    AlwaysPieces = ContextPieces ++ MainPieces ++
        NoSuffixIntegerPieces ++ InvisIntPieces,
    Msg = simple_msg(Context, [always(AlwaysPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_functor_args(Info, UnifyContext, Context,
        Var, ConsDefnList, Functor, ArgVars, ArgsTypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, LastContextWord,
        InClauseForPieces, ContextPieces),

    ModuleInfo = ClauseContext ^ tecc_module_info,
    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    StrippedFunctorStr = functor_cons_id_to_string(ModuleInfo,
        vns_varset(VarSet), print_name_only, StrippedFunctor, ArgVars),
    list.length(ArgVars, Arity),

    TypeAssignSet = args_type_assign_set_to_type_assign_set(ArgsTypeAssignSet),

    % If we have consistent information about the argument types,
    % we prefer to print an error message that mentions only the arguments
    % that may be in error.
    ConsArgTypesSet = list.map(get_expected_arg_types, ArgsTypeAssignSet),

    ( if
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _]
    then
        assoc_list.from_corresponding_lists(ArgVars, ConsArgTypes,
            ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(do_not_add_quotes, InstVarSet, TypeAssigns,
            1, ArgExpTypes,
            [], RevSubsumesMismatches, [], RevNoSubsumeMismatches),
        % RevSubsumesMismatches will contain errors where the actual type
        % is e.g. list(T), while the expected type is list(some_actual_type).
        % Since the argument may be just list(T) because it is [],
        % we don't mention these arguments (which are likely to be red
        % herrings, i.e. not the actual cause of the problem) unless
        % there are no arguments whose possible actual types do not include
        % one that subsumes the expected type.
        (
            RevNoSubsumeMismatches = [_ | _],
            list.reverse(RevNoSubsumeMismatches, Mismatches)
        ;
            RevNoSubsumeMismatches = [],
            list.reverse(RevSubsumesMismatches, Mismatches)
        ),
        categorize_mismatch_infos(Mismatches, CatMismatches0,
            map.init, ExpTVarCounts),
        list.negated_filter(hide_categorized_mismatch_info(ExpTVarCounts),
            CatMismatches0, CatMismatches),
        list.length(CatMismatches, NumCatMismatches),
        MaybeNumMismatches = yes(NumCatMismatches),
        ArgErrorPieces = mismatched_args_to_pieces(VarSet, Functor, is_first,
            CatMismatches),
        VerboseComponents = []
    else
        % XXX It should be possible to compute which arguments are
        % definitely OK, and which are suspect.
        MaybeNumMismatches = no,

        % For polymorphic data structures, the type of `Var' (the functor's
        % result type) can affect the valid types for the arguments.
        ( if
            % Could the type of the functor be polymorphic?
            some [ConsDefn] (
                list.member(ConsDefn, ConsDefnList),
                ConsDefn ^ cti_arg_types = [_ | _]
            )
        then
            % If so, print out the type of `Var'.
            % XXX Should this be in color, and if so, *which* color?
            ResultColor = maybe.no,
            ResultTypePieces =
                argument_name_to_pieces_uc(VarSet, LastContextWord, Var) ++
                type_of_var_to_pieces(InstVarSet, ResultColor, TypeAssignSet,
                    [suffix(",")], Var) ++
                [nl]
        else
            ResultTypePieces = []
        ),
        FunctorColor = yes(color_correct),
        (
            ArgVars = [],
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, FunctorColor,
                    Functor, Arity, ConsDefnList, [suffix(".")]) ++
                [nl]
        ;
            ArgVars = [HeadArgVar | TailArgVars],
            VarColor = yes(color_incorrect),
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, FunctorColor,
                    Functor, Arity, ConsDefnList, [suffix(",")]) ++
                types_of_vars_to_pieces(VarSet, InstVarSet, VarColor,
                    TypeAssignSet, [suffix("."), nl], HeadArgVar, TailArgVars)
        ),
        ArgErrorPieces = ResultTypePieces ++ AllTypesPieces,
        type_assign_set_msg_to_verbose_component(Info, VarSet,
            TypeAssignSet, VerboseComponent),
        VerboseComponents = [VerboseComponent]
    ),
    (
        MaybeNumMismatches = no,
        Arguments = "argument(s)"
    ;
        MaybeNumMismatches = yes(NumMismatches),
        ( if NumMismatches = 1 then
            Arguments = "argument"
        else
            Arguments = "arguments"
        )
    ),
    VarAndTermPieces = [words("in unification of")] ++
        color_as_subject(
            argument_name_to_pieces_lc(VarSet, LastContextWord, Var)) ++
        [nl,
        words("and term")] ++
        color_as_subject([words_quote(StrippedFunctorStr), suffix(":")]) ++
            [nl,
        words("type error in"), words(Arguments), words("of")] ++
        functor_name_to_pieces(StrippedFunctor, Arity) ++ [suffix("."), nl],

    ( if is_int_func_op(Functor) then
        list.foldl(acc_builtin_types_of_var(TypeAssignSet), [Var | ArgVars],
            set.init, BuiltinTypes0),
        acc_builtin_types_in_cons_type_infos(ConsDefnList,
            BuiltinTypes0, BuiltinTypes),
        InvisIntTypePieces =
            report_any_invisible_int_types(ClauseContext, BuiltinTypes)
    else
        InvisIntTypePieces = []
    ),

    AlwaysPieces = ContextPieces ++ VarAndTermPieces ++ ArgErrorPieces ++
        InvisIntTypePieces,
    Msg = simple_msg(Context, [always(AlwaysPieces) | VerboseComponents]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

:- type mismatch_info
    --->    mismatch_info(
                % XXX We should report the context of the argument,
                % but unfortunately that information is not stored
                % in the HLDS.
                int,                % Argument number, starting from 1.
                prog_var,           % Variable in that position
                type_mismatch,      % The first mismatch for this arg.
                list(type_mismatch) % Later type mismatches for this arg.
            ).

:- type categorized_mismatch_info
    --->    categorized_mismatch_info(
                int,                % Argument number, starting from 1.
                prog_var,           % Variable in that position
                mismatch_category,
                set(type_mismatch_special)
            ).

:- type mismatch_category
    --->    one_expected(
                one_expected_tvar   :: maybe(tvar),
                one_expected_pieces :: list(format_piece),
                one_actual_pieces   :: one_or_more(list(format_piece))
            )
    ;       several_expected(
                several_head        :: type_mismatch,
                several_tail        :: list(type_mismatch)
            ).

:- type actual_and_maybe_special
    --->    actual_and_maybe_special(
                list(format_piece),
                maybe(type_mismatch_special)
            ).

:- type does_actual_subsume_expected
    --->    actual_does_not_subsume_expected
    ;       actual_subsumes_expected.

:- type type_mismatch_special
    --->    type_mismatch_special_getopt_error(string).

:- type type_mismatch
    --->    type_mismatch_exp_act(
                expected_is_tvar    :: maybe(tvar),
                expected_type_desc  :: list(format_piece),
                actual_type_desc    :: list(format_piece),
                mismatch_subsumes   :: does_actual_subsume_expected,
                maybe_special       :: maybe(type_mismatch_special)
            ).

:- pred find_mismatched_args(maybe_add_quotes::in, inst_varset::in,
    type_assign_set::in, int::in, assoc_list(prog_var, mer_type)::in,
    list(mismatch_info)::in, list(mismatch_info)::out,
    list(mismatch_info)::in, list(mismatch_info)::out) is det.

find_mismatched_args(_, _, _, _, [],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).
find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum, [Arg - ExpType | ArgExpTypes],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches) :-
    % XXX When we get a test case in which the quadratic behavior of
    % get_all_type_stuffs_remove_dups is a performance issue, we should
    % try switching to get_all_type_stuffs without the remove_dups,
    % since the call to list.sort_and_remove_dups below should make it
    % semantically unnecessary.
    get_all_type_stuffs_remove_dups(TypeAssignSet, Arg, TypeStuffList),
    strip_module_names_from_type(strip_builtin_module_name, set_default_func,
        ExpType, StrippedExpType),
    list.foldl2(
        substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType),
        TypeStuffList,
        [], TypeMismatches0, no_type_stuff_matches, DoesSomeTypeStuffMatch),
    (
        DoesSomeTypeStuffMatch = some_type_stuff_matches
        % It is possible some TypeStuff in TypeStuffList matches,
        % and some doesn't, so TypeMismatches0 may not be empty.
        % We could gather it and return it in a new accumulator,
        % to be printed if the final contents of both RevSubsumesMismatches
        % and RevNoSubsumeMismatches is empty. However, this should never
        % happen, since report_error_functor_arg_types should not be invoked
        % in the absence of a known mismatch in argument types.
    ;
        DoesSomeTypeStuffMatch = no_type_stuff_matches,
        list.sort_and_remove_dups(TypeMismatches0, TypeMismatches),
        (
            TypeMismatches = [],
            unexpected($pred, "no_type_stuff_matches but TypeMismatches = []")
        ;
            TypeMismatches = [HeadTypeMismatch | TailTypeMismatches],
            Mismatch = mismatch_info(CurArgNum, Arg, HeadTypeMismatch,
                TailTypeMismatches),
            ( if all_no_subsume_mismatches(TypeMismatches) then
                !:RevNoSubsumeMismatches =
                    [Mismatch | !.RevNoSubsumeMismatches]
            else
                !:RevSubsumesMismatches =
                    [Mismatch | !.RevSubsumesMismatches]
            )
        )
    ),
    find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum + 1, ArgExpTypes,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).

:- type does_some_type_stuff_match
    --->    no_type_stuff_matches
    ;       some_type_stuff_matches.

:- pred substitute_types_check_match(maybe_add_quotes::in, inst_varset::in,
    mer_type::in, type_stuff::in,
    list(type_mismatch)::in, list(type_mismatch)::out,
    does_some_type_stuff_match::in, does_some_type_stuff_match::out) is det.

substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType, TypeStuff,
        !TypeMismatches, !DoesSomeTypeStuffMatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, ExistQTVars),
    strip_module_names_from_type(strip_builtin_module_name, set_default_func,
        ArgType, StrippedArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedExpType, FullExpType),
    ( if
        (
            % There is no mismatch if the actual type of the argument
            % is the same as the expected type.
            identical_types(FullArgType, FullExpType)
        ;
            % There is no mismatch if the actual type of the argument
            % has no constraints on it.
            FullArgType = defined_type(unqualified("<any>"), [], _)
        )
    then
        !:DoesSomeTypeStuffMatch = some_type_stuff_matches
    else
        ( if type_subsumes(FullArgType, FullExpType, _Subst) then
            ActualSubsumesExpected = actual_subsumes_expected
        else
            ActualSubsumesExpected = actual_does_not_subsume_expected
        ),
        ExpectedPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullExpType),
        ActualPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullArgType),
        ( if ExpectedPieces0 = ActualPieces0 then
            ExpectedPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullExpType),
            ActualPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullArgType)
        else
            ExpectedPieces = ExpectedPieces0,
            ActualPieces = ActualPieces0
        ),
        ( if
            FullExpType = builtin_type(builtin_type_string),
            FullArgType = defined_type(ArgTypeCtorSymName, [_], kind_star),
            ArgTypeCtorSymName = qualified(ArgTypeModuleName, ArgTypeName),
            ArgTypeName = "option_error",
            is_std_lib_module_name(ArgTypeModuleName, StdLibModuleName),
            ( StdLibModuleName = "getopt"
            ; StdLibModuleName = "getopt_io"
            )
        then
            Special = type_mismatch_special_getopt_error(StdLibModuleName),
            MaybeSpecial = yes(Special)
        else
            MaybeSpecial = no
        ),
        ( if
            FullExpType = type_variable(ExpTVar, kind_star),
            not list.member(ExpTVar, ExistQTVars)
        then
            MaybeExpTVar = yes(ExpTVar)
        else
            MaybeExpTVar = no
        ),
        TypeMismatch = type_mismatch_exp_act(MaybeExpTVar, ExpectedPieces,
            ActualPieces, ActualSubsumesExpected, MaybeSpecial),
        !:TypeMismatches = [TypeMismatch | !.TypeMismatches]
    ).

:- pred all_no_subsume_mismatches(list(type_mismatch)::in) is semidet.

all_no_subsume_mismatches([]).
all_no_subsume_mismatches([Mismatch | Mismatches]) :-
    Mismatch ^ mismatch_subsumes = actual_does_not_subsume_expected,
    all_no_subsume_mismatches(Mismatches).

:- pred categorize_mismatch_infos(list(mismatch_info)::in,
    list(categorized_mismatch_info)::out,
    map(tvar, int)::in, map(tvar, int)::out) is det.

categorize_mismatch_infos([], [], !ExpTVarCounts).
categorize_mismatch_infos([Mismatch | Mismatches], [CatMI | CatMIs],
        !ExpTVarCounts) :-
    Mismatch = mismatch_info(ArgNum, Var,
        HeadTypeMismatch, TailTypeMismatches),
    HeadTypeMismatch =
        type_mismatch_exp_act(MaybeExpTVar,
            HeadExpectedPieces, HeadActualPieces,
            _ActualSubsumesExpected, _MaybeSpecial),
    ( if
        expected_types_all_same_return_actuals(HeadExpectedPieces,
            TailTypeMismatches, TailActualPieces)
    then
        MismatchCategory = one_expected(MaybeExpTVar, HeadExpectedPieces,
            one_or_more(HeadActualPieces, TailActualPieces)),
        (
            MaybeExpTVar = yes(ExpTVar),
            ( if map.search(!.ExpTVarCounts, ExpTVar, Count0) then
                map.det_update(ExpTVar, Count0 + 1, !ExpTVarCounts)
            else
                map.det_insert(ExpTVar, 1, !ExpTVarCounts)
            )
        ;
            MaybeExpTVar = no
        )
    else
        % NOTE Instead of returning all the mismatches unchanged,
        % we could group them by expected pieces. For example, if
        % [HeadTypeMismatch | TailTypeMismatches] had five elements, and
        % two had one value for the expected pieces and three another value,
        % then we could return a list containing two one_expected structures
        % (which should then be renamed).
        %
        % This would probably generate more useful output. However,
        % we almost never get errors for which this change would make
        % any difference, so it is not a priority, and we would need
        % a motivating example where it *would* make a difference to
        % help us decide on what the right formatting would be for such cases.
        MismatchCategory =
            several_expected(HeadTypeMismatch, TailTypeMismatches)
    ),
    gather_special_type_mismatches([HeadTypeMismatch | TailTypeMismatches],
        SpecialMismatches),
    CatMI = categorized_mismatch_info(ArgNum, Var, MismatchCategory,
        SpecialMismatches),
    categorize_mismatch_infos(Mismatches, CatMIs, !ExpTVarCounts).

    % Do not display for the user any message that says
    % "Argument has type <actual type>, expected type was <distinct typevar>",
    % since such mismatches are NOT type errors.
    %
:- pred hide_categorized_mismatch_info(map(tvar, int)::in,
    categorized_mismatch_info::in) is semidet.

hide_categorized_mismatch_info(ExpTVarCounts, CatMI) :-
    CatMI = categorized_mismatch_info(_ArgNum, _Var, MismatchCategory,
        _SpecialMismatches),
    MismatchCategory = one_expected(MaybeExpTVar, _ExpPieces, _ActPieces),
    MaybeExpTVar = yes(ExpTVar),
    map.lookup(ExpTVarCounts, ExpTVar, Count),
    Count < 2.

:- func mismatched_args_to_pieces(prog_varset, cons_id, is_first,
    list(categorized_mismatch_info)) = list(format_piece).

mismatched_args_to_pieces(_, _, _, []) = [].
mismatched_args_to_pieces(VarSet, Functor, First, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = categorized_mismatch_info(ArgNum, Var,
        MismatchCategory, SpecialMismatches),
    ( if
        % Handle higher-order syntax such as ''(F, A) specially:
        % output
        %   Functor (F) has type ...;
        %   argument 1 (A) has type ...
        % instead of
        %   Argument 1 (F) has type ...;
        %   argument 2 (A) has type ...
        Functor = du_data_ctor(du_ctor(unqualified(""), Arity, _)),
        Arity > 0
    then
        (
            First = is_first,
            ArgNumPieces = [fixed("Functor")]
        ;
            First = is_not_first,
            ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum - 1)]
        )
    else
        ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum)]
    ),
    ( if varset.search_name(VarSet, Var, _) then
        VarName = mercury_var_to_name_only_vs(VarSet, Var),
        VarNamePieces = [prefix("("), words(VarName), suffix(")")]
    else
        VarNamePieces = []
    ),
    (
        MismatchCategory =
            one_expected(_ExpType, ExpectedPieces, OoMActualPieces),
        OoMActualPieces = one_or_more(HeadActualPieces, TailActualPieces),
        ExpectedDotPieces = ExpectedPieces ++ [suffix(".")],
        (
            TailActualPieces = [],
            ActualCommaPieces = HeadActualPieces ++ [suffix(",")],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                color_as_incorrect(ActualCommaPieces) ++
                    [nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                color_as_correct(ExpectedDotPieces) ++
                    [nl_indent_delta(-1)]
        ;
            TailActualPieces =
                [SecondActualPieces | ThirdPlusActualPieces],
            ActualCommaPieces =
                report_actual_types(HeadActualPieces,
                    SecondActualPieces, ThirdPlusActualPieces) ++
                [suffix(",")],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                color_as_incorrect(ActualCommaPieces) ++
                    [nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                color_as_correct(ExpectedDotPieces) ++
                    [nl_indent_delta(-1)]
        )
    ;
        MismatchCategory =
            several_expected(HeadTypeMismatch, TailTypeMismatches),
        AllMismatches = [HeadTypeMismatch | TailTypeMismatches],
        ErrorDescPieces =
            [words("has one of the following type mismatches."), nl] ++
            report_possible_expected_actual_types(1, AllMismatches)
    ),
    SpecialReasonPieces = report_special_type_mismatches(SpecialMismatches),

    ThisMismatchPieces = color_as_subject(ArgNumPieces ++ VarNamePieces) ++
        ErrorDescPieces ++ SpecialReasonPieces,

    (
        Mismatches = [],
        FollowingMismatchPieces = []
    ;
        Mismatches = [_ | _],
        FollowingMismatchPieces = mismatched_args_to_pieces(VarSet,
            Functor, is_not_first, Mismatches)
    ),
    Pieces = ThisMismatchPieces ++ FollowingMismatchPieces.

:- pred expected_types_all_same_return_actuals(list(format_piece)::in,
    list(type_mismatch)::in, list(list(format_piece))::out) is semidet.

expected_types_all_same_return_actuals(_ExpTypePieces, [], []).
expected_types_all_same_return_actuals(ExpTypePieces,
        [HeadMismatch | TailMismatches],
        [HeadActualTypePieces | TailActualTypePieces]) :-
    HeadMismatch = type_mismatch_exp_act(_ExpType,
        HeadExpTypePieces, HeadActualTypePieces,
        _ActualSubsumesExpected, _MaybeSpecial),
    ExpTypePieces = HeadExpTypePieces,
    expected_types_all_same_return_actuals(ExpTypePieces,
        TailMismatches, TailActualTypePieces).

:- func report_actual_types(list(format_piece),
    list(format_piece), list(list(format_piece))) =
    list(format_piece).

report_actual_types(FirstActualTypePieces, SecondActualTypePieces,
        ThirdPlusActualTypePieces) = Pieces :-
    (
        ThirdPlusActualTypePieces = [],
        Pieces =
            FirstActualTypePieces ++ [words("or")] ++ SecondActualTypePieces
    ;
        ThirdPlusActualTypePieces =
            [ThirdActualTypePieces | FourthPlusActualTypePieces],
        Pieces =
            FirstActualTypePieces ++ [suffix(",")] ++
            report_actual_types(SecondActualTypePieces, ThirdActualTypePieces,
                FourthPlusActualTypePieces)
    ).

:- func report_possible_expected_actual_types(int, list(type_mismatch)) =
    list(format_piece).

report_possible_expected_actual_types(_CurPossNum, []) = [].
report_possible_expected_actual_types(CurPossNum, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = type_mismatch_exp_act(_ExpType,
        ExpectedTypePieces, ActualTypePieces,
        _ActualSubsumesExpected, _MaybeSpecial),
    HeadPieces =
        [words("Possibility"), int_fixed(CurPossNum), suffix(":")] ++
        [words("actual type")] ++
            color_as_incorrect(ActualTypePieces ++ [suffix(",")]) ++
        [words("expected type")] ++
            color_as_correct(ExpectedTypePieces ++ [suffix("."), nl]),
    TailPieces = report_possible_expected_actual_types(CurPossNum + 1,
        Mismatches),
    Pieces = HeadPieces ++ TailPieces.

%---------------------%

:- pred gather_special_type_mismatches(list(type_mismatch)::in,
    set(type_mismatch_special)::out) is det.

gather_special_type_mismatches([], set.init).
gather_special_type_mismatches([Mismatch | Mismatches], !:Specials) :-
    gather_special_type_mismatches(Mismatches, !:Specials),
    Mismatch = type_mismatch_exp_act(_, _, _, _, MaybeSpecial),
    (
        MaybeSpecial = no
    ;
        MaybeSpecial = yes(Special),
        set.insert(Special, !Specials)
    ).

:- func report_special_type_mismatches(set(type_mismatch_special))
    = list(format_piece).

report_special_type_mismatches(Specials) = Pieces :-
    report_special_type_mismatches_loop(is_first,
        set.to_sorted_list(Specials), Pieces).

:- pred report_special_type_mismatches_loop(is_first::in,
    list(type_mismatch_special)::in, list(format_piece)::out) is det.

report_special_type_mismatches_loop(_IsFirst, [], []).
report_special_type_mismatches_loop(IsFirst, [HeadSpecial | TailSpecials],
        Pieces) :-
    report_special_type_mismatches_loop(is_not_first, TailSpecials,
        TailPieces),
    HeadPieces = report_special_type_mismatch(IsFirst, HeadSpecial),
    Pieces = HeadPieces ++ TailPieces.

:- func report_special_type_mismatch(is_first, type_mismatch_special)
    = list(format_piece).

report_special_type_mismatch(IsFirst, MismatchSpecial) = Pieces :-
    (
        IsFirst = is_first,
        ReasonIsPieces =
            [words("One possible reason for the error is that")]
    ;
        IsFirst = is_not_first,
        ReasonIsPieces =
            [words("Another possible reason for the error is that")]
    ),
    (
        MismatchSpecial = type_mismatch_special_getopt_error(GetoptModule),
        Pieces = ReasonIsPieces ++
            [words("the signatures of the option processing predicates"),
            words("in the"), quote(GetoptModule), words("module")] ++
            color_as_hint([words("have changed recently.")]) ++
            [words("Errors are now returned in a structured form,"),
            words("which can be converted to a string by calling the"),
            quote("option_error_to_string"), words("function."), nl]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The rest of this module contains utility predicates and functions
% for use by the code above.
%

:- func types_of_vars_to_pieces(prog_varset, inst_varset, maybe(color_name),
    type_assign_set, list(format_piece), prog_var, list(prog_var))
    = list(format_piece).

types_of_vars_to_pieces(VarSet, InstVarSet, MaybeColor, TypeAssignSet,
        FinalPieces, HeadVar, TailVars) = Pieces :-
    (
        TailVars = [],
        Pieces =
            argument_name_to_pieces_lc(VarSet, lcw_none, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
                FinalPieces, HeadVar)
    ;
        TailVars = [HeadTailVar | TailTailVars],
        Pieces =
            argument_name_to_pieces_lc(VarSet, lcw_none, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
                [suffix(","), nl], HeadVar) ++
            types_of_vars_to_pieces(VarSet, InstVarSet, MaybeColor,
                TypeAssignSet, FinalPieces, HeadTailVar, TailTailVars)
    ).

%---------------------------------------------------------------------------%

:- func functor_name_to_pieces(cons_id, arity) = list(format_piece).

functor_name_to_pieces(ConsId, Arity) = Pieces :-
    strip_builtin_qualifier_from_cons_id(ConsId, StrippedConsId),
    ( if Arity = 0 then
        Piece1 = words("constant"),
        ( if ConsId = du_data_ctor(du_ctor(SymName, _, _)) then
            Piece2 = qual_sym_name(SymName)
        else
            Piece2 = quote(cons_id_and_arity_to_string(StrippedConsId))
        ),
        Pieces = [Piece1, Piece2]
    else if ConsId = du_data_ctor(du_ctor(unqualified(""), _, _)) then
        Pieces = [words("higher-order term (with arity"),
            int_fixed(Arity - 1), suffix(")")]
    else
        Pieces = [words("functor"), qual_cons_id_and_maybe_arity(ConsId)]
    ).

:- func type_of_var_to_pieces(inst_varset, maybe(color_name), type_assign_set,
    list(format_piece), prog_var) = list(format_piece).

type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet, SuffixPieces, Var)
        = Pieces :-
    get_all_transformed_type_stuffs(
        typestuff_to_pieces(do_not_add_quotes, InstVarSet),
        TypeAssignSet, Var, TypePiecesLists0),
    list.sort_and_remove_dups(TypePiecesLists0, TypePiecesLists),
    ( if TypePiecesLists = [_TypePieces] then
        Pieces = [words("has type"), nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor,
                pieces_list_to_line_pieces(TypePiecesLists) ++
                SuffixPieces) ++
            [nl_indent_delta(-1)]
    else
        Pieces = [words("has overloaded type {"), nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor,
                pieces_list_to_line_pieces(TypePiecesLists)) ++
                [nl_indent_delta(-1)] ++
            [words("}")] ++ SuffixPieces
    ).

:- func type_of_functor_to_pieces(inst_varset, maybe(color_name),
    cons_id, arity, list(cons_type_info), list(format_piece))
    = list(format_piece).

type_of_functor_to_pieces(InstVarSet, MaybeColor, Functor, Arity, ConsDefnList,
        SuffixPieces) = Pieces :-
    ( if ConsDefnList = [SingleDefn] then
        ConsTypePieces = cons_type_to_pieces(InstVarSet, SingleDefn, Functor),
        Pieces =
            [words("has type"),
                nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor, ConsTypePieces ++ SuffixPieces) ++
                [nl_indent_delta(-1)]
    else
        ConsTypeListPieces =
            cons_type_list_to_pieces(InstVarSet, ConsDefnList, Functor, Arity),
        Pieces =
            [words("has overloaded type {"),
                nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor, ConsTypeListPieces) ++
                [nl_indent_delta(-1)] ++
            [fixed("}")] ++ SuffixPieces ++ [nl]
    ).

    % Return a description of the given data constructor's argument types.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_to_pieces(inst_varset, cons_type_info, cons_id)
    = list(format_piece).

cons_type_to_pieces(InstVarSet, ConsInfo, Functor) = Pieces :-
    ConsInfo = cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _, _),
    (
        ArgTypes = [_ | _],
        ( if Functor = du_data_ctor(du_ctor(SymName, _Arity, _)) then
            % What we construct in Type is not really a type: it is a
            % function symbol applied to a list of argument types. However
            % *syntactically*, it looks like a type, and we already have
            % code to print types, so we take a shortcut.
            Type = defined_type(SymName, ArgTypes, kind_star),
            ArgPieces =
                type_to_pieces(TVarSet, InstVarSet,
                    print_name_only, do_not_add_quotes, ExistQVars, Type) ++
                [suffix(":")]
        else
            unexpected($pred, "invalid cons_id")
        )
    ;
        ArgTypes = [],
        ArgPieces = []
    ),
    Pieces = ArgPieces ++
        type_to_pieces(TVarSet, InstVarSet, print_name_only, do_not_add_quotes,
            ExistQVars, ConsType).

    % Return a description of the argument types of the given list of
    % data constructors.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_list_to_pieces(inst_varset, list(cons_type_info), cons_id,
    int) = list(format_piece).

cons_type_list_to_pieces(_, [], _, _) = [].
cons_type_list_to_pieces(InstVarSet, [ConsDefn | ConsDefns], Functor, Arity)
        = Pieces :-
    ThisPieces = cons_type_to_pieces(InstVarSet, ConsDefn, Functor),
    (
        ConsDefns = [],
        Pieces = ThisPieces
    ;
        ConsDefns = [_ | _],
        ( if Arity = 0 then
            ConnectPieces = [suffix(",")]
        else
            ConnectPieces = [suffix(","), nl]
        ),
        TailPieces = cons_type_list_to_pieces(InstVarSet, ConsDefns, Functor,
            Arity),
        Pieces = ThisPieces ++ ConnectPieces ++ TailPieces
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_unify.
%---------------------------------------------------------------------------%
