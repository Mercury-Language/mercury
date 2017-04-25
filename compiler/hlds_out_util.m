%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_out_util.m.
% Author: zs.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_data.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type hlds_out_info
    --->    hlds_out_info(
                hoi_dump_hlds_options           :: string,
                hoi_dump_hlds_options_backup    :: string,
                hoi_dump_hlds_pred_ids          :: list(string),
                hoi_dump_hlds_pred_names        :: list(string),
                hoi_mercury_to_mercury          :: merc_out_info
            ).

:- func init_hlds_out_info(globals, output_lang) = hlds_out_info.

%-----------------------------------------------------------------------------%

:- type is_first
    --->    is_first
    ;       is_not_first.

:- type is_last
    --->    is_last
    ;       is_not_last.

%-----------------------------------------------------------------------------%

    % write_pred_id/4 writes out a message such as
    %       predicate `foo.bar/3'
    % or    function `foo.myfoo/5'
    % except in some special cases where the predicate name is mangled
    % and we can print a more meaningful identification of the predicate
    % in question.
    %
:- pred write_pred_id(module_info::in, pred_id::in, io::di, io::uo) is det.
:- func pred_id_to_string(module_info, pred_id) = string.

    % Do the job of pred_id_to_string, but don't look up the pred_info
    % in the module_info; get it directly from the caller.
    %
:- func pred_info_id_to_string(pred_info) = string.

:- pred write_pred_proc_id(module_info::in, pred_proc_id::in, io::di, io::uo)
    is det.
:- func pred_proc_id_to_string(module_info, pred_proc_id) = string.

:- pred write_pred_proc_id_pair(module_info::in, pred_id::in, proc_id::in,
    io::di, io::uo) is det.
:- func pred_proc_id_pair_to_string(module_info, pred_id, proc_id) = string.

%-----------------------------------------------------------------------------%

    % unify_context_to_pieces generates a message such as
    %   foo.m:123:   in argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % based on the unify_context and prog_context.
    %
:- pred unify_context_to_pieces(unify_context::in,
    list(format_component)::in, list(format_component)::out) is det.

    % unify_context_first_to_pieces is the same as above, except that
    % it also takes and returns a flag which specifies whether this is the
    % start of a sentence. If the first argument is `is_first', then it means
    % this is the first line of an error message, so the message starts with
    % a capital letter, e.g.
    %   foo.m:123:   In argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % The flag returned as the second argument will be `is_not_first'
    % unless nothing was generated, in which case it will be the same
    % as the first argument.
    %
:- pred unify_context_first_to_pieces(is_first::in, is_first::out,
    unify_context::in,
    list(format_component)::in, list(format_component)::out) is det.

%-----------------------------------------------------------------------------%

:- func call_id_to_string(call_id) = string.

:- func generic_call_id_to_string(generic_call_id) = string.

:- func cast_type_to_string(cast_kind) = string.

    % Generate a message of the form "argument %i of call to pred_or_func
    % `foo/n'". The pred_markers argument is used to tell if the calling
    % predicate is a type class method implementation; if so, we omit the
    % "call to" part, since the user didn't write any explicit call.
    %
:- func call_arg_id_to_string(call_id, int, pred_markers) = string.

%-----------------------------------------------------------------------------%

    % Print out a functor and its arguments. The prog_varset gives
    % the context.
    %
:- pred write_functor(prog_varset::in, var_name_print::in,
    const::in, list(prog_var)::in, io::di, io::uo) is det.
:- func functor_to_string(prog_varset, var_name_print, const, list(prog_var))
    = string.

:- pred write_functor_maybe_needs_quotes(prog_varset::in, var_name_print::in,
    needs_quotes::in, const::in, list(prog_var)::in, io::di, io::uo) is det.
:- func functor_to_string_maybe_needs_quotes(prog_varset, var_name_print,
    needs_quotes, const, list(prog_var)) = string.

:- pred write_qualified_functor(prog_varset::in, var_name_print::in,
    module_name::in, const::in, list(prog_var)::in, io::di, io::uo) is det.
:- func qualified_functor_to_string(prog_varset, var_name_print,
    module_name, const, list(prog_var)) = string.

:- pred write_qualified_functor_with_term_args(prog_varset::in,
    var_name_print::in, module_name::in, const::in, list(prog_term)::in,
    io::di, io::uo) is det.
:- func qualified_functor_with_term_args_to_string(prog_varset, var_name_print,
    module_name, const, list(prog_term)) = string.

    % Print out a cons_id and arguments. The module_info and prog_varset
    % give the context.
    %
:- pred write_functor_cons_id(module_info::in, prog_varset::in,
    var_name_print::in, cons_id::in, list(prog_var)::in,
    io::di, io::uo) is det.
:- func functor_cons_id_to_string(module_info, prog_varset, var_name_print,
    cons_id, list(prog_var)) = string.

:- type maybe_qualify_cons_id
    --->    qualify_cons_id
    ;       do_not_qualify_cons_id.

:- pred write_cons_id_and_vars_or_arity(prog_varset::in,
    maybe_qualify_cons_id::in, cons_id::in, maybe(list(prog_var))::in,
    io::di, io::uo) is det.
:- func cons_id_and_vars_or_arity_to_string(prog_varset,
    maybe_qualify_cons_id, cons_id, maybe(list(prog_var))) = string.

%-----------------------------------------------------------------------------%

:- pred write_constraint_proof_map(tvarset::in, var_name_print::in, int::in,
    constraint_proof_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Print out a list of variables and their corresponding modes
    % (e.g. for a lambda expressions). The varsets gives the context.
    %
:- pred write_var_modes(prog_varset::in, inst_varset::in, var_name_print::in,
    list(prog_var)::in, list(mer_mode)::in, io::di, io::uo) is det.
:- func var_modes_to_string(prog_varset, inst_varset, var_name_print,
    list(prog_var), list(mer_mode)) = string.

:- pred write_var_mode(prog_varset::in, inst_varset::in, var_name_print::in,
    pair(prog_var, mer_mode)::in, io::di, io::uo) is det.
:- func var_mode_to_string(prog_varset, inst_varset, var_name_print,
    pair(prog_var, mer_mode)) = string.

%-----------------------------------------------------------------------------%

:- func type_import_status_to_string(type_status) = string.
:- func inst_import_status_to_string(inst_status) = string.
:- func mode_import_status_to_string(mode_status) = string.
:- func typeclass_import_status_to_string(typeclass_status) = string.
:- func instance_import_status_to_string(instance_status) = string.
:- func pred_import_status_to_string(pred_status) = string.

%-----------------------------------------------------------------------------%

    % Write out a list of integers as a Mercury list.
    %
:- pred write_intlist(list(int)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write out the given indent level (two spaces per indent).
    %
:- pred write_indent(int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_rtti.
:- import_module hlds.special_pred.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_item.  % undesirable dependency

:- import_module assoc_list.
:- import_module int.
:- import_module integer.
:- import_module map.
:- import_module string.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

init_hlds_out_info(Globals, Lang) = Info :-
    globals.lookup_string_option(Globals, dump_hlds_options, DumpOptions),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_id, Ids),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_name, Names),
    MercInfo = init_merc_out_info(Globals, unqualified_item_names, Lang),
    Info = hlds_out_info(DumpOptions, DumpOptions, Ids, Names, MercInfo).

%-----------------------------------------------------------------------------%
%
% Write out the ids of predicates and procedures.
%

write_pred_id(ModuleInfo, PredId, !IO) :-
    % The code of this predicate duplicates the functionality of
    % hlds_error_util.describe_one_pred_name. Changes here should be made
    % there as well.
    io.write_string(pred_id_to_string(ModuleInfo, PredId), !IO).

pred_id_to_string(ModuleInfo, PredId) = Str :-
    module_info_get_preds(ModuleInfo, PredTable),
    ( if map.search(PredTable, PredId, PredInfo) then
        Str = pred_info_id_to_string(PredInfo)
    else
        % The predicate has been deleted, so we print what we can.
        pred_id_to_int(PredId, PredIdInt),
        Str = "deleted predicate " ++ int_to_string(PredIdInt)
    ).

pred_info_id_to_string(PredInfo) = Str :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    (
        Origin = origin_special_pred(SpecialId, TypeCtor),
        special_pred_description(SpecialId, Descr),
        TypeCtor = type_ctor(_TypeSymName, TypeArity),
        ( if TypeArity = 0 then
            ForStr = " for type "
        else
            ForStr = " for type constructor "
        ),
        Str = Descr ++ ForStr ++ type_name_to_string(TypeCtor)
    ;
        Origin = origin_instance_method(MethodName, MethodConstraints),
        MethodConstraints = instance_method_constraints(ClassId,
            InstanceTypes, _, _),
        MethodStr = simple_call_id_to_string(PredOrFunc, MethodName,
            Arity),
        ClassId = class_id(ClassName, _),
        ClassStr = sym_name_to_string(ClassName),
        TypeStrs = mercury_type_list_to_string(varset.init, InstanceTypes),
        Str = string.append_list([
            "instance method ", MethodStr,
            " for `", ClassStr, "(", TypeStrs, ")'"
        ])
    ;
        Origin = origin_assertion(FileName, LineNumber),
        ( if pred_info_is_promise(PredInfo, PromiseType) then
            Str = string.format("`%s' declaration (%s:%d)",
                [s(prog_out.promise_to_string(PromiseType)),
                s(FileName), i(LineNumber)])
        else
            SymName = qualified(Module, Name),
            Str = simple_call_id_to_string(PredOrFunc, SymName, Arity)
        )
    ;
        Origin = origin_tabling(BasePredId, TablingAuxPredKind),
        BasePredIdStr = simple_call_id_to_string(BasePredId),
        (
            TablingAuxPredKind = tabling_aux_pred_stats,
            Str = "table statistics predicate for " ++ BasePredIdStr
        ;
            TablingAuxPredKind = tabling_aux_pred_reset,
            Str = "table reset predicate for " ++ BasePredIdStr
        )
    ;
        Origin = origin_solver_type(TypeCtorSymName, TypeCtorArity,
            SolverAuxPredKind),
        TypeStr = sym_name_and_arity_to_string(
            sym_name_arity(TypeCtorSymName, TypeCtorArity)),
        (
            SolverAuxPredKind = solver_type_to_ground_pred,
            Str = "to ground representation predicate for " ++ TypeStr
        ;
            SolverAuxPredKind = solver_type_to_any_pred,
            Str = "to any representation predicate for " ++ TypeStr
        ;
            SolverAuxPredKind = solver_type_from_ground_pred,
            Str = "from ground representation predicate for " ++ TypeStr
        ;
            SolverAuxPredKind = solver_type_from_any_pred,
            Str = "from any representation predicate for " ++ TypeStr
        )
    ;
        ( Origin = origin_transformed(_, _, _)
        ; Origin = origin_created(_)
        ; Origin = origin_mutable(_, _, _)
        ; Origin = origin_lambda(_, _, _)
        ; Origin = origin_user(_)
        ),
        SymName = qualified(Module, Name),
        Str = simple_call_id_to_string(PredOrFunc, SymName, Arity)
    ).

write_pred_proc_id(ModuleInfo, proc(PredId, ProcId), !IO) :-
    write_pred_proc_id_pair(ModuleInfo, PredId, ProcId, !IO).

pred_proc_id_to_string(ModuleInfo, proc(PredId, ProcId)) =
    pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId).

write_pred_proc_id_pair(ModuleInfo, PredId, ProcId, !IO) :-
    io.write_string(pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId),
        !IO).

pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId) = Str :-
    proc_id_to_int(ProcId, ModeNum),
    Str = pred_id_to_string(ModuleInfo, PredId)
        ++ " mode " ++ int_to_string(ModeNum).

%-----------------------------------------------------------------------------%
%
% Write out the contexts of unifications.
%

unify_context_to_pieces(UnifyContext, !Pieces) :-
    unify_context_first_to_pieces(is_not_first, _, UnifyContext, !Pieces).

unify_context_first_to_pieces(!First, UnifyContext, !Pieces) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list.reverse(RevSubContexts, SubContexts),
    unify_main_context_to_pieces(!First, MainContext, !Pieces),
    unify_sub_contexts_to_pieces(!First, SubContexts, !Pieces).

:- pred unify_main_context_to_pieces(is_first::in, is_first::out,
    unify_main_context::in,
    list(format_component)::in, list(format_component)::out) is det.

unify_main_context_to_pieces(!First, MainContext, !Pieces) :-
    (
        MainContext = umc_explicit
    ;
        MainContext = umc_head(ArgNum),
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        ArgNumStr = int_to_string(ArgNum),
        !:Pieces = !.Pieces ++
            [words("argument"), fixed(ArgNumStr), words("of clause head:"), nl]
    ;
        MainContext = umc_head_result,
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        !:Pieces = !.Pieces ++
            [words("function result term of clause head:"), nl]
    ;
        MainContext = umc_call(CallId, ArgNum),
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        % The markers argument below is used only for type class method
        % implementations defined using the named syntax rather than
        % the clause syntax, and the bodies of such procedures should
        % only contain a single call, so we shouldn't get unifications
        % nested inside calls. Hence we can safely initialize the
        % markers to empty here. (Anyway the worst possible consequence
        % is slightly sub-optimal text for an error message.)
        init_markers(Markers),
        ArgIdStr = call_arg_id_to_string(CallId, ArgNum, Markers),
        !:Pieces = !.Pieces ++ [words(ArgIdStr), suffix(":"), nl]
    ;
        MainContext = umc_implicit(Source),
        start_in_message_to_pieces(!.First, !Pieces),
        string.format("implicit %s unification:\n", [s(Source)], Msg),
        !:Pieces = !.Pieces ++ [words(Msg), nl]
    ).

:- pred unify_sub_contexts_to_pieces(is_first::in, is_first::out,
    unify_sub_contexts::in,
    list(format_component)::in, list(format_component)::out) is det.

unify_sub_contexts_to_pieces(!First, [], !Pieces).
unify_sub_contexts_to_pieces(!First, [SubContext | SubContexts], !Pieces) :-
    ( if
        contexts_describe_list_element([SubContext | SubContexts],
            0, ElementNum, AfterContexts)
    then
        in_element_to_pieces(!.First, ElementNum, !Pieces),
        !:First = is_not_first,
        unify_sub_contexts_to_pieces(!First, AfterContexts, !Pieces)
    else
        in_argument_to_pieces(!.First, SubContext, !Pieces),
        !:First = is_not_first,
        unify_sub_contexts_to_pieces(!First, SubContexts, !Pieces)
    ).

:- pred contexts_describe_list_element(unify_sub_contexts::in,
    int::in, int::out, unify_sub_contexts::out) is semidet.

contexts_describe_list_element([SubContext | SubContexts],
        NumElementsBefore, ElementNum, AfterContexts) :-
    SubContext = unify_sub_context(ConsId, ArgNum),
    ConsId = cons(Functor, 2, _TypeCtor),
    % We ignore _TypeCtor since it may not have been set yet.
    (
        Functor = unqualified("[|]")
    ;
        Functor = qualified(ModuleSymName, "[|]"),
        is_std_lib_module_name(ModuleSymName, "list")
    ),
    (
        ArgNum = 1,
        ElementNum = NumElementsBefore,
        AfterContexts = SubContexts
    ;
        ArgNum = 2,
        contexts_describe_list_element(SubContexts,
            NumElementsBefore + 1, ElementNum, AfterContexts)
    ).

:- pred in_argument_to_pieces(is_first::in, unify_sub_context::in,
    list(format_component)::in, list(format_component)::out) is det.

in_argument_to_pieces(First, SubContext, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    SubContext = unify_sub_context(ConsId, ArgNum),
    ArgNumStr = int_to_string(ArgNum),
    % XXX Using cons_id_and_arity_to_string here results in the
    % quotes being in the wrong place.
    ConsIdStr = cons_id_and_arity_to_string(ConsId),
    !:Pieces = !.Pieces ++ [words("argument"), fixed(ArgNumStr),
        words("of functor"), quote(ConsIdStr), suffix(":"), nl].

:- pred in_element_to_pieces(is_first::in, int::in,
    list(format_component)::in, list(format_component)::out) is det.

in_element_to_pieces(First, ElementNum, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    ElementNumStr = int_to_string(ElementNum),
    !:Pieces = !.Pieces ++ [words("list element"),
        prefix("#"), fixed(ElementNumStr), suffix(":"), nl].

:- pred start_in_message_to_pieces(is_first::in,
    list(format_component)::in, list(format_component)::out) is det.

start_in_message_to_pieces(First, !Pieces) :-
    (
        First = is_first,
        % It is possible for First to be yes and !.Pieces to be nonempty,
        % since !.Pieces may contain stuff from before the unify context.
        !:Pieces = !.Pieces ++ [words("In")]
    ;
        First = is_not_first,
        !:Pieces = !.Pieces ++ [words("in")]
    ).

%-----------------------------------------------------------------------------%
%
% Write out ids of calls.
%

call_id_to_string(plain_call_id(PredCallId)) =
    simple_call_id_to_string(PredCallId).
call_id_to_string(generic_call_id(GenericCallId)) =
    generic_call_id_to_string(GenericCallId).

generic_call_id_to_string(gcid_higher_order(Purity, PredOrFunc, _)) =
    purity_prefix_to_string(Purity) ++ "higher-order "
    ++ prog_out.pred_or_func_to_full_str(PredOrFunc) ++ " call".
generic_call_id_to_string(gcid_class_method(_ClassId, MethodId)) =
    simple_call_id_to_string(MethodId).
generic_call_id_to_string(gcid_event_call(EventName)) =
    "event " ++ EventName.
generic_call_id_to_string(gcid_cast(CastType)) =
    cast_type_to_string(CastType).

cast_type_to_string(unsafe_type_cast) = "unsafe_type_cast".
cast_type_to_string(unsafe_type_inst_cast) = "unsafe_type_inst_cast".
cast_type_to_string(equiv_type_cast) = "equiv_type_cast".
cast_type_to_string(exists_cast) = "exists_cast".

call_arg_id_to_string(CallId, ArgNum, PredMarkers) = Str :-
    ( if ArgNum =< 0 then
        % Argument numbers that are less than or equal to zero
        % are used for the type_info and typeclass_info arguments
        % that are introduced by polymorphism.m.
        % I think argument number equal to zero might also be used
        % in some other cases when we just don't have any information
        % about which argument it is.
        % For both of these, we just say "in call to"
        % rather than "in argument N of call to".
        Str1 = ""
    else
        Str1 = arg_number_to_string(CallId, ArgNum) ++ " of "
    ),
    ( if
        (
            % The text printed for generic calls other than
            % `class_method' does not need the "call to"
            % prefix ("in call to higher-order call" is redundant,
            % it's much better to just say "in higher-order call").
            CallId = generic_call_id(GenericCallId),
            not GenericCallId = gcid_class_method(_, _)
        ;
            % For calls from type class instance implementations
            % that were defined using the named syntax rather
            % than the clause syntax, we also omit the "call to",
            % since in that case there was no explicit call in
            % the user's source code.
            check_marker(PredMarkers, marker_named_class_instance_method)
        )
    then
        Str2 = Str1
    else
        Str2 = Str1 ++ "call to "
    ),
    Str = Str2 ++ call_id_to_string(CallId).

:- func arg_number_to_string(call_id, int) = string.

arg_number_to_string(CallId, ArgNum) = Str :-
    (
        CallId = plain_call_id(simple_call_id(PredOrFunc, _, Arity)),
        ( if
            PredOrFunc = pf_function,
            Arity = ArgNum
        then
            Str = "the return value"
        else
            Str = "argument " ++ int_to_string(ArgNum)
        )
    ;
        CallId = generic_call_id(GenericCallId),
        (
            GenericCallId = gcid_higher_order(_Purity, PredOrFunc, Arity),
            ( if
                PredOrFunc = pf_function,
                ArgNum = Arity
            then
                Str = "the return value"
            else
                % Make error messages for higher-order calls
                % such as `P(A, B)' clearer.
                Main = "argument " ++ int_to_string(ArgNum),
                PredOrFuncStr = prog_out.pred_or_func_to_full_str(PredOrFunc),
                ( if ArgNum = 1 then
                    Expl = "the " ++ PredOrFuncStr ++ " term"
                else
                    Expl = "argument " ++ int_to_string(ArgNum - 1)
                        ++ " of the called " ++ PredOrFuncStr
                ),
                Str = Main ++ " (i.e. " ++ Expl ++ ")"
            )
        ;
            ( GenericCallId = gcid_class_method(_, _)
            ; GenericCallId = gcid_event_call(_)
            ; GenericCallId = gcid_cast(_)
            ),
            Str = "argument " ++ int_to_string(ArgNum)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Write out functors.
%

write_functor(VarSet, VarNamePrint, Functor, ArgVars, !IO) :-
    write_functor_maybe_needs_quotes(VarSet, VarNamePrint,
        not_next_to_graphic_token, Functor, ArgVars, !IO).

functor_to_string(VarSet, VarNamePrint, Functor, ArgVars)  =
    functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
        not_next_to_graphic_token, Functor, ArgVars).

write_functor_maybe_needs_quotes(VarSet, VarNamePrint, NextToGraphicToken,
        Functor, ArgVars, !IO) :-
    io.write_string(functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
        NextToGraphicToken, Functor, ArgVars), !IO).

functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint, NextToGraphicToken,
        Functor, ArgVars) = Str :-
    term.context_init(Context),
    term.var_list_to_term_list(ArgVars, ArgTerms),
    Term = term.functor(Functor, ArgTerms, Context),
    Str = mercury_term_nq_to_string(VarSet, VarNamePrint, NextToGraphicToken,
        Term).

write_qualified_functor(VarSet, VarNamePrint, ModuleName, Functor, ArgVars,
        !IO) :-
    io.write_string(qualified_functor_to_string(VarSet, VarNamePrint,
        ModuleName, Functor, ArgVars), !IO).

qualified_functor_to_string(VarSet, VarNamePrint, ModuleName, Functor,
        ArgVars) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    FunctorStr = functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
        next_to_graphic_token, Functor, ArgVars),
    Str = ModuleNameStr ++ "." ++ FunctorStr.

write_qualified_functor_with_term_args(VarSet, VarNamePrint,
        ModuleName, Functor, ArgTerms, !IO) :-
    io.write_string(qualified_functor_with_term_args_to_string(VarSet,
        VarNamePrint, ModuleName, Functor, ArgTerms), !IO).

qualified_functor_with_term_args_to_string(VarSet, VarNamePrint,
        ModuleName, Functor, ArgTerms) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    term.context_init(Context),
    Term = term.functor(Functor, ArgTerms, Context),
    TermStr = mercury_term_nq_to_string(VarSet, VarNamePrint,
        next_to_graphic_token, Term),
    Str = ModuleNameStr ++ "." ++ TermStr.

write_functor_cons_id(ModuleInfo, VarSet, VarNamePrint, ConsId, ArgVars,
        !IO) :-
    io.write_string(functor_cons_id_to_string(ModuleInfo, VarSet, VarNamePrint,
        ConsId, ArgVars), !IO).

functor_cons_id_to_string(ModuleInfo, VarSet, VarNamePrint, ConsId, ArgVars)
        = Str :-
    (
        ConsId = cons(SymName, _, _),
        (
            SymName = qualified(Module, Name),
            Str = qualified_functor_to_string(VarSet, VarNamePrint,
                Module, term.atom(Name), ArgVars)
        ;
            SymName = unqualified(Name),
            Str = functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
                next_to_graphic_token, term.atom(Name), ArgVars)
        )
    ;
        ConsId = tuple_cons(_),
        Str = functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
            next_to_graphic_token, term.atom("{}"), ArgVars)
    ;
        ConsId = int_const(Int),
        Str = functor_to_string(VarSet, VarNamePrint,
            term.integer(base_10, integer(Int), signed, size_word), ArgVars)
    ;
        ConsId = uint_const(UInt),
        Str = functor_to_string(VarSet, VarNamePrint,
            term.integer(base_10, integer.from_uint(UInt), unsigned, size_word),
            ArgVars)
    ;
        ConsId = float_const(Float),
        Str = functor_to_string(VarSet, VarNamePrint,
            term.float(Float), ArgVars)
    ;
        ConsId = char_const(Char),
        % XXX The strings ('z') and ('\n') should always denote
        % the last letter of the alphabet and the newline character
        % respectively. We need to decide whether forms such as (z)
        % and 'z' should acceptable too. I (zs) think that 'z' should
        % be acceptable to the scanner and parser (which currently it isn't),
        % but (z) should not be.
        Str = "(" ++ term_io.quoted_char(Char) ++ ")"
    ;
        ConsId = string_const(String),
        Str = functor_to_string(VarSet, VarNamePrint,
            term.string(String), ArgVars)
    ;
        ConsId = impl_defined_const(Name),
        Str = "$" ++ Name
    ;
        ConsId = closure_cons(ShroudedPredProcId, _),
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredSymName = qualified(PredModule, PredName),
        PredConsId = cons(PredSymName, list.length(ArgVars),
            cons_id_dummy_type_ctor),
        Str = functor_cons_id_to_string(ModuleInfo, VarSet, VarNamePrint,
            PredConsId, ArgVars)
    ;
        ConsId = type_ctor_info_const(Module, Name, Arity),
        Str = "type_ctor_info("""
            ++ prog_out.sym_name_to_escaped_string(Module)
            ++ """, """ ++ Name ++ """, " ++ int_to_string(Arity) ++ ")"
    ;
        ConsId = base_typeclass_info_const(Module, ClassId, _, Instance),
        ClassId = class_id(Name, Arity),
        Str = "base_typeclass_info("""
            ++ prog_out.sym_name_to_escaped_string(Module) ++ """, "
            ++ "class_id(" ++ prog_out.sym_name_to_escaped_string(Name)
            ++ ", " ++ int_to_string(Arity) ++ "), " ++ Instance ++ ")"
    ;
        ConsId = type_info_cell_constructor(_),
        Str = functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
            next_to_graphic_token,
            term.atom("type_info_cell_constructor"), ArgVars)
    ;
        ConsId = typeclass_info_cell_constructor,
        Str = functor_to_string_maybe_needs_quotes(VarSet, VarNamePrint,
            next_to_graphic_token,
            term.atom("typeclass_info_cell_constructor"), ArgVars)
    ;
        ConsId = type_info_const(TIConstNum),
        Str = "type_info_const(" ++ int_to_string(TIConstNum) ++ ")"
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        Str = "typeclass_info_const(" ++ int_to_string(TCIConstNum) ++ ")"
    ;
        ConsId = ground_term_const(ConstNum, SubConsId),
        SubStr = functor_cons_id_to_string(ModuleInfo, VarSet, VarNamePrint,
            SubConsId, []),
        Str = "ground_term_const(" ++ int_to_string(ConstNum) ++ ", " ++
            SubStr ++ ")"
    ;
        ConsId = tabling_info_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "tabling_info_const("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ ", " ++ int_to_string(ProcIdInt) ++ ")"
    ;
        ConsId = table_io_entry_desc(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "table_io_entry_desc("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ " (mode " ++ int_to_string(ProcIdInt) ++ "))"
    ;
        ConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "deep_profiling_proc_layout("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ " (mode " ++ int_to_string(ProcIdInt) ++ "))"
    ).

write_cons_id_and_vars_or_arity(Qual, VarSet, ConsId, MaybeArgVars, !IO) :-
    io.write_string(
        cons_id_and_vars_or_arity_to_string(Qual, VarSet,
            ConsId, MaybeArgVars),
        !IO).

cons_id_and_vars_or_arity_to_string(VarSet, Qual, ConsId, MaybeArgVars)
        = String :-
    (
        ConsId = cons(SymName0, Arity, _TypeCtor),
        (
            Qual = qualify_cons_id,
            SymName = SymName0
        ;
            Qual = do_not_qualify_cons_id,
            SymName = unqualified(unqualify_name(SymName0))
        ),
        SymNameString0 = sym_name_to_string(SymName),
        ( if string.contains_char(SymNameString0, '*') then
            % We need to protect against the * appearing next to a /
            Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
                ( if Char = ('*') then
                    string.append(Str0, "star", Str)
                else
                    string.char_to_string(Char, CharStr),
                    string.append(Str0, CharStr, Str)
                )
            ),
            string.foldl(Stuff, SymNameString0, "", SymNameString1)
        else
            SymNameString1 = SymNameString0
        ),
        SymNameString = term_io.escaped_string(SymNameString1),
        (
            MaybeArgVars = no,
            String = SymNameString ++ "/" ++ string.int_to_string(Arity)
        ;
            MaybeArgVars = yes(ArgVars),
            (
                ArgVars = [],
                String = SymNameString ++ "/" ++ string.int_to_string(Arity)
            ;
                ArgVars = [_ | _],
                ArgStr = mercury_vars_to_name_only(VarSet, ArgVars),
                String = SymNameString ++ "(" ++ ArgStr ++ ")"
            )
        )
    ;
        ConsId = tuple_cons(Arity),
        (
            MaybeArgVars = no,
            String = "{}/" ++ string.int_to_string(Arity)
        ;
            MaybeArgVars = yes(ArgVars),
            (
                ArgVars = [],
                String = "{}/" ++ string.int_to_string(Arity)
            ;
                ArgVars = [_ | _],
                ArgStr = mercury_vars_to_name_only(VarSet, ArgVars),
                String = "{" ++ ArgStr ++ "}"
            )
        )
    ;
        ConsId = int_const(Int),
        string.int_to_string(Int, String)
    ;
        ConsId = uint_const(UInt),
        String = uint_to_string(UInt) ++ "u"
    ;
        ConsId = float_const(Float),
        String = float_to_string(Float)
    ;
        ConsId = char_const(CharConst),
        String = term_io.quoted_char(CharConst)
    ;
        ConsId = string_const(StringConst),
        String = term_io.quoted_string(StringConst)
    ;
        ConsId = impl_defined_const(Name),
        String = "$" ++ Name
    ;
        ConsId = closure_cons(PredProcId, _),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<pred " ++ int_to_string(PredId) ++
            " proc " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = type_ctor_info_const(Module, Ctor, Arity),
        String =
            "<type_ctor_info " ++ sym_name_to_string(Module) ++ "." ++
            Ctor ++ "/" ++ int_to_string(Arity) ++ ">"
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        String = "<base_typeclass_info>"
    ;
        ConsId = type_info_cell_constructor(_),
        String = "<type_info_cell_constructor>"
    ;
        ConsId = typeclass_info_cell_constructor,
        String = "<typeclass_info_cell_constructor>"
    ;
        ConsId = type_info_const(_),
        String = "<type_info_const>"
    ;
        ConsId = typeclass_info_const(_),
        String = "<typeclass_info_const>"
    ;
        ConsId = ground_term_const(_, _),
        String = "<ground_term_const>"
    ;
        ConsId = tabling_info_const(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<tabling_info " ++ int_to_string(PredId) ++
            ", " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = table_io_entry_desc(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<table_io_entry_desc " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ;
        ConsId = deep_profiling_proc_layout(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ).

%-----------------------------------------------------------------------------%
%
% Write out constraint proofs.
%

write_constraint_proof_map(TVarSet, VarNamePrint, Indent, ProofMap, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% Proofs: \n", !IO),
    map.to_assoc_list(ProofMap, ProofsList),
    io.write_list(ProofsList, "\n",
        write_constraint_proof(TVarSet, VarNamePrint, Indent), !IO).

:- pred write_constraint_proof(tvarset::in, var_name_print::in, int::in,
    pair(prog_constraint, constraint_proof)::in, io::di, io::uo) is det.

write_constraint_proof(TVarSet, VarNamePrint, Indent, Constraint - Proof,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_constraint(TVarSet, VarNamePrint, Constraint, !IO),
    io.write_string(": ", !IO),
    (
        Proof = apply_instance(Num),
        io.write_string("apply instance decl #", !IO),
        io.write_int(Num, !IO)
    ;
        Proof = superclass(Super),
        io.write_string("super class of ", !IO),
        mercury_output_constraint(TVarSet, VarNamePrint, Super, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out modes.
%

write_var_modes(VarSet, InstVarSet, VarNamePrint, Vars, Modes, !IO) :-
    io.write_string(
        var_modes_to_string(VarSet, InstVarSet, VarNamePrint, Vars, Modes),
        !IO).

var_modes_to_string(VarSet, InstVarSet, VarNamePrint, Vars, Modes) = Str :-
    assoc_list.from_corresponding_lists(Vars, Modes, VarModes),
    Strs = list.map(var_mode_to_string(VarSet, InstVarSet, VarNamePrint),
        VarModes),
    Str = string.join_list(", ", Strs).

write_var_mode(VarSet, InstVarSet, VarNamePrint, Var - Mode, !IO) :-
    io.write_string(var_mode_to_string(VarSet, InstVarSet, VarNamePrint,
        Var - Mode), !IO).

var_mode_to_string(VarSet, InstVarSet, VarNamePrint, Var - Mode) =
    mercury_var_to_string(VarSet, VarNamePrint, Var)
        ++ "::" ++ mercury_mode_to_string(output_debug, InstVarSet, Mode).

%-----------------------------------------------------------------------------%
%
% Write out statuses.
%

type_import_status_to_string(type_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
inst_import_status_to_string(inst_status(InstModeStatus)) =
    instmode_status_to_string(InstModeStatus).
mode_import_status_to_string(mode_status(InstModeStatus)) =
    instmode_status_to_string(InstModeStatus).
typeclass_import_status_to_string(typeclass_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
instance_import_status_to_string(instance_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
pred_import_status_to_string(pred_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).

:- func instmode_status_to_string(new_instmode_status) = string.

instmode_status_to_string(InstModeStatus) = Str :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_nowhere,
            Str = "this_module(export_nowhere)"
        ;
            InstModeExport = instmode_export_only_submodules,
            Str = "this_module(export_only_submodules)"
        ;
            InstModeExport = instmode_export_anywhere,
            Str = "this_module(export_anywhere)"
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(InstModeImport),
        (
            InstModeImport = instmode_import_plain(InstModeImportLocn),
            (
                InstModeImportLocn = instmode_import_plain_imp,
                Str = "other_module(import_plain(imp))"
            ;
                InstModeImportLocn = instmode_import_plain_int,
                Str = "other_module(import_plain(int))"
            ;
                InstModeImportLocn =
                    instmode_import_plain_ancestors_priv_int_file,
                Str = "other_module(import_plain(ancestors_priv_int_file))"
            )
        ;
            InstModeImport = instmode_import_abstract,
            Str = "other_module(import_abstract)"
        ;
            InstModeImport = instmode_import_opt,
            Str = "other_module(import_opt)"
        )
    ).

:- func old_import_status_to_string(old_import_status) = string.

old_import_status_to_string(status_local) =
    "local".
old_import_status_to_string(status_exported) =
    "exported".
old_import_status_to_string(status_opt_exported) =
    "opt_exported".
old_import_status_to_string(status_abstract_exported) =
    "abstract_exported".
old_import_status_to_string(status_pseudo_exported) =
    "pseudo_exported".
old_import_status_to_string(status_imported(import_locn_interface)) =
    "imported in the interface".
old_import_status_to_string(status_imported(import_locn_implementation)) =
    "imported in the implementation".
old_import_status_to_string(status_imported(
        import_locn_ancestor_private_interface_proper)) =
    "imported from an ancestor's private interface".
old_import_status_to_string(status_imported(import_locn_import_by_ancestor)) =
    "imported by an ancestor".
old_import_status_to_string(status_external(Status)) =
    "external (and " ++ old_import_status_to_string(Status) ++ ")".
old_import_status_to_string(status_abstract_imported) =
    "abstract_imported".
old_import_status_to_string(status_opt_imported) =
    "opt_imported".
old_import_status_to_string(status_pseudo_imported) =
    "pseudo_imported".
old_import_status_to_string(status_exported_to_submodules) =
    "exported_to_submodules".

%-----------------------------------------------------------------------------%
%
% Write out lists of integers as Mercury terms.
%

write_intlist(IntList, !IO) :-
    (
        IntList = [],
        io.write_string("[]", !IO)
    ;
        IntList = [H | T],
        io.write_string("[", !IO),
        write_intlist_2(H, T, !IO),
        io.write_string("]", !IO)
    ).

:- pred write_intlist_2(int::in, list(int)::in, io::di, io::uo)
    is det.

write_intlist_2(H, T, !IO) :-
    io.write_int(H, !IO),
    (
        T = [TH | TT],
        io.write_string(", ", !IO),
        write_intlist_2(TH, TT, !IO)
    ;
        T = []
    ).

%-----------------------------------------------------------------------------%
%
% Write out indentation.
%

write_indent(Indent, !IO) :-
    ( if Indent = 0 then
        true
    else
        io.write_string("  ", !IO),
        write_indent(Indent - 1, !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_util.
%-----------------------------------------------------------------------------%
