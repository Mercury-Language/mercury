%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: hlds_out.m.
% Main authors: conway, fjh.
% 
% There is quite a bit of overlap between the following modules:
%
%   hlds_out.m
%   mercury_to_mercury.m
%   term_io.m
%
% mercury_to_mercury.m prints the parse tree data structure defined
% in prog_data.m.  hlds_out.m does a similar task, but for the data
% structure defined in hlds.m.  term_io.m prints terms.
% 
% There are two different ways of printing variables.
% One way uses the names Var', Var'', etc. which are generated
% by the compiler.  The other way converts all names back into
% a format allowed as source code.  Currently this module calls
% mercury_to_mercury.m, which uses the second method, rather
% than term_io.m, which uses the first method.  We should
% think about using an option to specify which method is used.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_out.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred write_type_ctor(type_ctor::in, io::di, io::uo) is det.
:- func type_ctor_to_string(type_ctor) = string.

:- pred write_class_id(class_id::in, io::di, io::uo) is det.

:- pred write_cons_id(cons_id::in, io::di, io::uo) is det.
:- func cons_id_to_string(cons_id) = string.

    % write_pred_id/4 writes out a message such as
    %       predicate `foo.bar/3'
    % or    function `foo.myfoo/5'
    % except in some special cases where the predicate name is mangled
    % and we can print a more meaningful identification of the predicate
    % in question.
    %
:- pred write_pred_id(module_info::in, pred_id::in, io::di, io::uo) is det.
:- func pred_id_to_string(module_info, pred_id) = string.

:- pred write_pred_proc_id(module_info::in, pred_proc_id::in, io::di, io::uo)
    is det.
:- func pred_proc_id_to_string(module_info, pred_proc_id) = string.

:- pred write_pred_proc_id_pair(module_info::in, pred_id::in, proc_id::in,
    io::di, io::uo) is det.
:- func pred_proc_id_pair_to_string(module_info, pred_id, proc_id) = string.

:- pred write_call_id(call_id::in, io::di, io::uo) is det.
:- func call_id_to_string(call_id) = string.

    % Write "argument %i of call to pred_or_func `foo/n'". The pred_markers
    % argument is used to tell if the calling predicate is a type class method
    % implementation; if so, we omit the "call to" part, since the user didn't
    % write any explicit call.
    %
:- pred write_call_arg_id(call_id::in, int::in, pred_markers::in,
    io::di, io::uo) is det.
:- func call_arg_id_to_string(call_id, int, pred_markers) = string.

    % write_unify_context/5 writes out a message such as
    %   foo.m:123:   in argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % based on the unify_context and prog_context.
    %
:- pred write_unify_context(unify_context::in, prog_context::in,
    io::di, io::uo) is det.
:- pred unify_context_to_pieces(unify_context::in,
    list(format_component)::in, list(format_component)::out) is det.

    % write_unify_context_first/6 is the same as above, except that
    % it also takes and returns a bool which specifies whether this is the
    % start of a sentence. If the first argument is `yes', then it means
    % this is the first line of an error message, so the message starts with
    % a capital letter, e.g.
    %   foo.m:123:   In argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % The bool returned as the second argument will be `no' unless nothing
    % was printed out, in which case it will be the same as the first arg.
    %
:- pred write_unify_context_first(bool::in, bool::out, unify_context::in,
    prog_context::in, io::di, io::uo) is det.
:- pred unify_context_first_to_pieces(bool::in, bool::out, unify_context::in,
    list(format_component)::in, list(format_component)::out) is det.

:- pred write_determinism(determinism::in, io::di, io::uo) is det.
:- func determinism_to_string(determinism) = string.

:- pred write_can_fail(can_fail::in, io::di, io::uo) is det.
:- func can_fail_to_string(can_fail) = string.

:- pred write_eval_method(eval_method::in, io::di, io::uo) is det.

:- pred write_import_status(import_status::in, io::di, io::uo) is det.
:- func import_status_to_string(import_status) = string.

%-----------------------------------------------------------------------------%

    % Print out an entire HLDS structure.
    %
:- pred write_hlds(int::in, module_info::in, io::di, io::uo) is det.

    % write_clauses(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
    %   HeadVars, PredOrFunc, Clauses, MaybeVarTypes).
    %
:- pred write_clauses(int::in, module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_var)::in, pred_or_func::in,
    list(clause)::in, maybe_vartypes::in, io::di, io::uo) is det.

    % write_clause(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
    %   HeadTerms, PredOrFunc, Clause, UseDeclaredModes, MaybeVarTypes).
    %
:- pred write_clause(int::in, module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_term)::in, pred_or_func::in,
    clause::in, bool::in, maybe_vartypes::in, io::di, io::uo) is det.

:- pred write_promise(promise_type::in, int::in, module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_var)::in, pred_or_func::in,
    clause::in, maybe_vartypes::in, io::di, io::uo) is det.

    % Print out an HLDS goal. The module_info and prog_varset give
    % the context of the goal. The boolean says whether variables should
    % have their numbers appended to them. The integer gives the level
    % of indentation to be used within the goal. The string says what
    % should end the line containing the goal; it should include a newline
    % character, but may also contain other characters before that.
    %
:- pred write_goal(hlds_goal::in, module_info::in, prog_varset::in, bool::in,
    int::in, string::in, io::di, io::uo) is det.

    % write_goal_list is used to write both disjunctions
    % and parallel conjunctions. The module_info, prog_varset and
    % maybe_vartypes give the context of the goal. The boolean
    % says whether variables should have their numbers appended to
    % them. The integer gives the level of indentation to be used
    % within the goal. The string says what should be on the line
    % between each goal; it should include a newline character,
    % but may also contain other characters before that.
    %
:- pred write_goal_list(list(hlds_goal)::in, module_info::in, prog_varset::in,
    bool::in, int::in, string::in, maybe_vartypes::in, io::di, io::uo) is det.

    % Print out a functor and its arguments. The prog_varset gives
    % the context. The boolean says whether variables should have their
    % numbers appended to them.
    %
:- pred write_functor(const::in, list(prog_var)::in, prog_varset::in, bool::in,
    io::di, io::uo) is det.
:- func functor_to_string(const, list(prog_var), prog_varset, bool) = string.

    % Print out a cons_id and arguments. The module_info and prog_varset
    % give the context. The boolean says whether variables should have
    % their numbers appended to them.
    %
:- pred write_functor_cons_id(cons_id::in, list(prog_var)::in, prog_varset::in,
    module_info::in, bool::in, io::di, io::uo) is det.
:- func functor_cons_id_to_string(cons_id, list(prog_var), prog_varset,
    module_info, bool) = string.

    % Print out the right-hand-side of a unification. The module_info and
    % the varsets give the context of the rhs. The boolean says whether
    % variables should have their numbers appended to them. The integer
    % gives the level of indentation to be used within the rhs.
    %
:- pred write_unify_rhs(unify_rhs::in, module_info::in, prog_varset::in,
    inst_varset::in, bool::in, int::in, io::di, io::uo) is det.

    % Converts the right-hand-side of a unification to a string, similarly to
    % write_unify_rhs, but doesn't print any details for lambda goals.
    % The module_info and the varset give the context of the rhs. The boolean
    % says whether variables should have their numbers appended to them.
    %
:- func unify_rhs_to_string(unify_rhs, module_info, prog_varset, bool)
    = string.

    % Print out a list of variables and their corresponding modes
    % (e.g. for a lambda expressions). The varsets gives the context.
    % The boolean says whether variables should have their numbers
    % appended to them.
    %
:- pred write_var_modes(list(prog_var)::in, list(mer_mode)::in,
    prog_varset::in, inst_varset::in, bool::in, io::di, io::uo) is det.
:- func var_modes_to_string(list(prog_var), list(mer_mode), prog_varset,
    inst_varset, bool) = string.

:- pred write_instmap(instmap::in, prog_varset::in, bool::in, int::in,
    io::di, io::uo) is det.

    % Find the name of a marker.
    %
:- pred marker_name(marker::in, string::out) is det.

    % Print out the name of a marker.
    %
:- pred write_marker(marker::in, io::di, io::uo) is det.

:- type maybe_vartypes
    --->    varset_vartypes(tvarset, vartypes)
    ;       no_varset_vartypes.

    % Convert a mode or inst to a term representation.
    %
:- func mode_to_term(mer_mode) = prog_term.
:- func inst_to_term(mer_inst) = prog_term.

%-----------------------------------------------------------------------------%

:- pred mercury_output_uni_mode(uni_mode::in, inst_varset::in,
    io::di, io::uo) is det.

:- func mercury_uni_mode_to_string(uni_mode, inst_varset) = string.

:- pred mercury_output_uni_mode_list(list(uni_mode)::in, inst_varset::in,
    io::di, io::uo) is det.

:- func mercury_uni_mode_list_to_string(list(uni_mode), inst_varset) = string.

    % Output an inst in a format where all compiler-defined insts
    % have been expanded out; recursive insts have their self-referential
    % parts printed out as elipses ("...").
    % (These routines are used for outputting insts in mode errors.)
    %
:- pred mercury_output_expanded_inst(mer_inst::in, inst_varset::in,
    module_info::in, io::di, io::uo) is det.
:- func mercury_expanded_inst_to_string(mer_inst, inst_varset, module_info)
    = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_table.
:- import_module hlds.instmap.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

write_type_ctor(type_ctor(Name, Arity), !IO) :-
    prog_out.write_sym_name_and_arity(Name / Arity, !IO).

type_ctor_to_string(type_ctor(Name, Arity)) =
    prog_out.sym_name_and_arity_to_string(Name / Arity).

write_class_id(class_id(Name, Arity), !IO) :-
    prog_out.write_sym_name_and_arity(Name / Arity, !IO).

write_cons_id(ConsId, !IO) :-
    io.write_string(cons_id_to_string(ConsId), !IO).

cons_id_to_string(cons(SymName, Arity)) = String :-
    SymNameString0 = sym_name_to_string(SymName),
    ( string.contains_char(SymNameString0, '*') ->
        % We need to protect against the * appearing next to a /
        Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
            ( Char = ('*') ->
                string.append(Str0, "star", Str)
            ;
                string.char_to_string(Char, CharStr),
                string.append(Str0, CharStr, Str)
            )
        ),
        string.foldl(Stuff, SymNameString0, "", SymNameString1)
    ;
        SymNameString1 = SymNameString0
    ),
    SymNameString = term_io.escaped_string(SymNameString1),
    string.int_to_string(Arity, ArityString),
    string.append_list([SymNameString, "/", ArityString], String).
cons_id_to_string(int_const(Int)) = String :-
    string.int_to_string(Int, String).
cons_id_to_string(string_const(String)) =
    term_io.quoted_string(String).
cons_id_to_string(float_const(Float)) =
    float_to_string(Float).
cons_id_to_string(pred_const(shrouded_pred_proc_id(PredId, ProcId), _)) =
    "<pred " ++ int_to_string(PredId) ++
    " proc " ++ int_to_string(ProcId) ++ ">".
cons_id_to_string(type_ctor_info_const(Module, Ctor, Arity)) =
    "<type_ctor_info " ++ sym_name_to_string(Module) ++ "." ++
    Ctor ++ "/" ++ int_to_string(Arity) ++ ">".
cons_id_to_string(base_typeclass_info_const(_, _, _, _)) =
    "<base_typeclass_info>".
cons_id_to_string(type_info_cell_constructor(_)) =
    "<type_info_cell_constructor>".
cons_id_to_string(typeclass_info_cell_constructor) =
    "<typeclass_info_cell_constructor>".
cons_id_to_string(tabling_info_const(shrouded_pred_proc_id(PredId, ProcId))) =
    "<tabling_info " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".
cons_id_to_string(deep_profiling_proc_layout(
        shrouded_pred_proc_id(PredId, ProcId))) =
    "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".
cons_id_to_string(table_io_decl(shrouded_pred_proc_id(PredId, ProcId))) =
    "<table_io_decl " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".

write_pred_id(ModuleInfo, PredId, !IO) :-
    % The code of this predicate duplicates the functionality of
    % hlds_error_util.describe_one_pred_name. Changes here should be made
    % there as well.
    io.write_string(pred_id_to_string(ModuleInfo, PredId), !IO).

pred_id_to_string(ModuleInfo, PredId) = Str :-
    module_info_preds(ModuleInfo, PredTable),
    ( map.search(PredTable, PredId, PredInfo) ->
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = origin_special_pred(SpecialId - TypeCtor)
        ->
            special_pred_description(SpecialId, Descr),
            TypeCtor = type_ctor(_TypeSymName, TypeArity),
            ( TypeArity = 0 ->
                ForStr = " for type "
            ;
                ForStr = " for type constructor "
            ),
            Str = Descr ++ ForStr ++ type_name_to_string(TypeCtor)
        ;
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, marker_class_instance_method)
        ->
            Str = "type class method implementation"
        ;
            pred_info_get_goal_type(PredInfo, goal_type_promise(PromiseType))
        ->
            Str = "`" ++ prog_out.promise_to_string(PromiseType)
                ++ "' declaration"
        ;
            Str = simple_call_id_to_string(PredOrFunc, qualified(Module, Name),
                Arity)
        )
    ;
        % The predicate has been deleted, so we print what we can.
        pred_id_to_int(PredId, PredIdInt),
        Str = "deleted predicate " ++ int_to_string(PredIdInt)
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

write_call_id(CallId, !IO) :-
    Str = call_id_to_string(CallId),
    io.write_string(Str, !IO).

call_id_to_string(plain_call_id(PredCallId)) =
    simple_call_id_to_string(PredCallId).
call_id_to_string(generic_call_id(GenericCallId)) =
    generic_call_id_to_string(GenericCallId).

:- pred write_generic_call_id(generic_call_id::in, io::di, io::uo) is det.

write_generic_call_id(GenericCallId, !IO) :-
    Str = generic_call_id_to_string(GenericCallId),
    io.write_string(Str, !IO).

:- func generic_call_id_to_string(generic_call_id) = string.

generic_call_id_to_string(gcid_higher_order(Purity, PredOrFunc, _)) =
    purity_prefix_to_string(Purity) ++ "higher-order "
    ++ prog_out.pred_or_func_to_full_str(PredOrFunc) ++ " call".
generic_call_id_to_string(gcid_class_method(_ClassId, MethodId)) =
    simple_call_id_to_string(MethodId).
generic_call_id_to_string(gcid_event_call(EventName)) =
    "event " ++ EventName.
generic_call_id_to_string(gcid_cast(CastType)) =
    cast_type_to_string(CastType).

:- func cast_type_to_string(cast_kind) = string.

cast_type_to_string(unsafe_type_cast) = "unsafe_type_cast".
cast_type_to_string(unsafe_type_inst_cast) = "unsafe_type_inst_cast".
cast_type_to_string(equiv_type_cast) = "equiv_type_cast".
cast_type_to_string(exists_cast) = "exists_cast".

write_call_arg_id(CallId, ArgNum, PredMarkers, !IO) :-
    Str = call_arg_id_to_string(CallId, ArgNum, PredMarkers),
    io.write_string(Str, !IO).

call_arg_id_to_string(CallId, ArgNum, PredMarkers) = Str :-
    ( ArgNum =< 0 ->
        % Argument numbers that are less than or equal to zero
        % are used for the type_info and typeclass_info arguments
        % that are introduced by polymorphism.m.
        % I think argument number equal to zero might also be used
        % in some other cases when we just don't have any information
        % about which argument it is.
        % For both of these, we just say "in call to"
        % rather than "in argument N of call to".
        Str1 = ""
    ;
        Str1 = arg_number_to_string(CallId, ArgNum) ++ " of "
    ),
    (
        (
            % The text printed for generic calls other than
            % `class_method' does not need the "call to"
            % prefix ("in call to higher-order call" is redundant,
            % it's much better to just say "in higher-order call").
            CallId = generic_call_id(GenericCallId),
            \+ GenericCallId = gcid_class_method(_, _)
        ;
            % For calls from type class instance implementations
            % that were defined using the named syntax rather
            % than the clause syntax, we also omit the "call to",
            % since in that case there was no explicit call in
            % the user's source code.
            check_marker(PredMarkers, marker_named_class_instance_method)
        )
    ->
        Str2 = Str1
    ;
        Str2 = Str1 ++ "call to "
    ),
    Str = Str2 ++ call_id_to_string(CallId).

:- pred write_arg_number(call_id::in, int::in, io::di, io::uo) is det.

write_arg_number(CallId, ArgNum, !IO) :-
    Str = arg_number_to_string(CallId, ArgNum),
    io.write_string(Str, !IO).

:- func arg_number_to_string(call_id, int) = string.

arg_number_to_string(plain_call_id(simple_call_id(PredOrFunc, _, Arity)),
        ArgNum) =
    (
        PredOrFunc = function,
        Arity = ArgNum
    ->
        "the return value"
    ;
        "argument " ++ int_to_string(ArgNum)
    ).
arg_number_to_string(generic_call_id(
        gcid_higher_order(_Purity, PredOrFunc, Arity)), ArgNum) = Str :-
    (
        PredOrFunc = function,
        ArgNum = Arity
    ->
        Str = "the return value"
    ;
        % Make error messages for higher-order calls
        % such as `P(A, B)' clearer.
        Main = "argument " ++ int_to_string(ArgNum),
        PredOrFuncStr = prog_out.pred_or_func_to_full_str(PredOrFunc),
        ( ArgNum = 1 ->
            Expl = "the " ++ PredOrFuncStr ++ " term"
        ;
            Expl = "argument " ++ int_to_string(ArgNum - 1)
                ++ " of the called " ++ PredOrFuncStr
        ),
        Str = Main ++ " (i.e. " ++ Expl ++ ")"
    ).
arg_number_to_string(generic_call_id(gcid_class_method(_, _)), ArgNum) =
    "argument " ++ int_to_string(ArgNum).
arg_number_to_string(generic_call_id(gcid_event_call(_)), ArgNum) =
    "argument " ++ int_to_string(ArgNum).
arg_number_to_string(generic_call_id(gcid_cast(_)), ArgNum) =
    "argument " ++ int_to_string(ArgNum).

%-----------------------------------------------------------------------------%

write_unify_context(UnifyContext, Context, !IO) :-
    write_unify_context_first(no, _, UnifyContext, Context, !IO).

unify_context_to_pieces(UnifyContext, !Pieces) :-
    unify_context_first_to_pieces(no, _, UnifyContext, !Pieces).

write_unify_context_first(!First, UnifyContext, Context, !IO) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list.reverse(RevSubContexts, SubContexts),
    write_unify_main_context(!First, MainContext, Context, !IO),
    write_unify_sub_contexts(!First, SubContexts, Context, !IO).

unify_context_first_to_pieces(!First, UnifyContext, !Pieces) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list.reverse(RevSubContexts, SubContexts),
    unify_main_context_to_pieces(!First, MainContext, !Pieces),
    unify_sub_contexts_to_pieces(!First, SubContexts, !Pieces).

:- pred write_unify_main_context(bool::in, bool::out,
    unify_main_context::in, prog_context::in, io::di, io::uo) is det.

write_unify_main_context(!First, umc_explicit, _, !IO).
write_unify_main_context(!First, umc_head(ArgNum), Context, !IO) :-
    write_in_argument(!.First, ArgNum, Context, !IO),
    !:First = no,
    io.write_string(" of clause head:\n", !IO).
write_unify_main_context(!First, umc_head_result, Context, !IO) :-
    start_in_message(!.First, Context, !IO),
    !:First = no,
    io.write_string("function result term of clause head:\n", !IO).
write_unify_main_context(!First, umc_call(CallId, ArgNum), Context, !IO) :-
    start_in_message(!.First, Context, !IO),
    !:First = no,
    % The markers argument below is used only for type class method
    % implementations defined using the named syntax rather than
    % the clause syntax, and the bodies of such procedures should
    % only contain a single call, so we shouldn't get unifications
    % nested inside calls.  Hence we can safely initialize the
    % markers to empty here.  (Anyway the worst possible consequence
    % is slightly sub-optimal text for an error message.)
    init_markers(Markers),
    write_call_arg_id(CallId, ArgNum, Markers, !IO),
    io.write_string(":\n", !IO).
write_unify_main_context(!First, umc_implicit(Source), Context, !IO) :-
    start_in_message(!.First, Context, !IO),
    io.format("implicit %s unification:\n", [s(Source)], !IO).

:- pred unify_main_context_to_pieces(bool::in, bool::out,
    unify_main_context::in,
    list(format_component)::in, list(format_component)::out) is det.

unify_main_context_to_pieces(!First, umc_explicit, !Pieces).
unify_main_context_to_pieces(!First, umc_head(ArgNum), !Pieces) :-
    in_argument_to_pieces(!.First, ArgNum, !Pieces),
    !:First = no,
    !:Pieces = !.Pieces ++ [words("of clause head:"), nl].
unify_main_context_to_pieces(!First, umc_head_result, !Pieces) :-
    start_in_message_to_pieces(!.First, !Pieces),
    !:First = no,
    !:Pieces = !.Pieces ++ [words("function result term of clause head:"), nl].
unify_main_context_to_pieces(!First, umc_call(CallId, ArgNum),
        !Pieces) :-
    start_in_message_to_pieces(!.First, !Pieces),
    !:First = no,
    % The markers argument below is used only for type class method
    % implementations defined using the named syntax rather than
    % the clause syntax, and the bodies of such procedures should
    % only contain a single call, so we shouldn't get unifications
    % nested inside calls.  Hence we can safely initialize the
    % markers to empty here.  (Anyway the worst possible consequence
    % is slightly sub-optimal text for an error message.)
    init_markers(Markers),
    ArgIdStr = call_arg_id_to_string(CallId, ArgNum, Markers),
    !:Pieces = !.Pieces ++ [words(ArgIdStr ++ ":"), nl].
unify_main_context_to_pieces(!First, umc_implicit(Source), !Pieces) :-
    start_in_message_to_pieces(!.First, !Pieces),
    string.format("implicit %s unification:\n", [s(Source)], Msg),
    !:Pieces = !.Pieces ++ [words(Msg), nl].

:- pred write_unify_sub_contexts(bool::in, bool::out,
    unify_sub_contexts::in, prog_context::in, io::di, io::uo) is det.

write_unify_sub_contexts(!First, [], _, !IO).
write_unify_sub_contexts(!First, [ConsId - ArgNum | SubContexts], Context,
        !IO) :-
    write_in_argument(!.First, ArgNum, Context, !IO),
    !:First = no,
    io.write_string(" of functor `", !IO),
    write_cons_id(ConsId, !IO),
    io.write_string("':\n", !IO),
    write_unify_sub_contexts(!First, SubContexts, Context, !IO).

:- pred unify_sub_contexts_to_pieces(bool::in, bool::out,
    unify_sub_contexts::in,
    list(format_component)::in, list(format_component)::out) is det.

unify_sub_contexts_to_pieces(!First, [], !Pieces).
unify_sub_contexts_to_pieces(!First, [ConsId - ArgNum | SubContexts],
        !Pieces) :-
    in_argument_to_pieces(!.First, ArgNum, !Pieces),
    !:First = no,
    NewPieces = [words("of functor"),
        fixed("`" ++ cons_id_to_string(ConsId) ++ "':"), nl],
    !:Pieces = !.Pieces ++ NewPieces,
    unify_sub_contexts_to_pieces(!First, SubContexts, !Pieces).

:- pred write_in_argument(bool::in, int::in, prog_context::in,
    io::di, io::uo) is det.

write_in_argument(First, ArgNum, Context, !IO) :-
    start_in_message(First, Context, !IO),
    io.write_string("argument ", !IO),
    io.write_int(ArgNum, !IO).

:- pred in_argument_to_pieces(bool::in, int::in,
    list(format_component)::in, list(format_component)::out) is det.

in_argument_to_pieces(First, ArgNum, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    ArgNumStr = int_to_string(ArgNum),
    !:Pieces = !.Pieces ++ [words("argument"), words(ArgNumStr)].

:- pred start_in_message(bool::in, prog_context::in, io::di, io::uo) is det.

start_in_message(First, Context, !IO) :-
    prog_out.write_context(Context, !IO),
    (
        First = yes,
        io.write_string("  In ", !IO)
    ;
        First = no,
        io.write_string("  in ", !IO)
    ).

:- pred start_in_message_to_pieces(bool::in,
    list(format_component)::in, list(format_component)::out) is det.

start_in_message_to_pieces(First, !Pieces) :-
    (
        First = yes,
        % It is possible for First to be yes and !.Pieces to be nonempty,
        % since !.Pieces may contain stuff from before the unify context.
        !:Pieces = !.Pieces ++ [words("In")]
    ;
        First = no,
        !:Pieces = !.Pieces ++ [words("in")]
    ).

%-----------------------------------------------------------------------------%

write_hlds(Indent, Module, !IO) :-
    module_info_get_imported_module_specifiers(Module, Imports),
    module_info_preds(Module, PredTable),
    module_info_get_type_table(Module, TypeTable),
    module_info_get_inst_table(Module, InstTable),
    module_info_get_mode_table(Module, ModeTable),
    module_info_get_class_table(Module, ClassTable),
    module_info_get_instance_table(Module, InstanceTable),
    write_header(Indent, Module, !IO),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'I') ->
        write_imports(Indent, Imports, !IO)
    ;
        true
    ),
    ( string.contains_char(Verbose, 'T') ->
        write_types(Indent, TypeTable, !IO),
        io.write_string("\n", !IO),
        write_classes(Indent, ClassTable, !IO),
        io.write_string("\n", !IO),
        write_instances(Indent, InstanceTable, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    ( string.contains_char(Verbose, 'M') ->
        write_insts(Indent, InstTable, !IO),
        io.write_string("\n", !IO),
        write_modes(Indent, ModeTable, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    write_preds(Indent, Module, PredTable, !IO),
    write_footer(Indent, Module, !IO).

:- pred write_header(int::in, module_info::in, io::di, io::uo) is det.

write_header(Indent, Module, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% vim: ts=2 sw=2\n\n", !IO),
    module_info_get_name(Module, Name),
    write_indent(Indent, !IO),
    io.write_string(":- module ", !IO),
    prog_out.write_sym_name(Name, !IO),
    io.write_string(".\n\n", !IO).

:- pred write_imports(int::in, set(module_specifier)::in, io::di, io::uo)
    is det.

write_imports(Indent, ImportSet, !IO) :-
    write_indent(Indent, !IO),
    io.write_string(":- import_module ", !IO),
    io.write_list(set.to_sorted_list(ImportSet), ", ", write_sym_name, !IO),
    io.write_string(".\n\n", !IO).

:- pred write_footer(int::in, module_info::in, io::di, io::uo) is det.

write_footer(Indent, Module, !IO) :-
    module_info_get_name(Module, Name),
    write_indent(Indent, !IO),
    io.write_string(":- end_module ", !IO),
    prog_out.write_sym_name(Name, !IO),
    io.write_string(".\n", !IO).

:- pred write_preds(int::in, module_info::in, pred_table::in,
    io::di, io::uo) is det.

write_preds(Indent, ModuleInfo, PredTable, !IO) :-
    io.write_string("%-------- Predicates --------\n\n", !IO),
    write_indent(Indent, !IO),
    map.keys(PredTable, PredIds),
    list.foldl(maybe_write_pred(Indent, ModuleInfo, PredTable), PredIds, !IO).

:- pred maybe_write_pred(int::in, module_info::in, pred_table::in,
    pred_id::in, io::di, io::uo) is det.

maybe_write_pred(Indent, ModuleInfo, PredTable, PredId, !IO) :-
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    globals.io_lookup_int_option(dump_hlds_pred_id, DumpPredId, !IO),
    pred_id_to_int(PredId, PredIdInt),
    map.lookup(PredTable, PredId, PredInfo),
    (
        % If the user requested one predicate/function to be dumped,
        % we dump it even if the condition of the nested if-then-else
        % says it shouldn't be dumped, and we don't dump anything else.
        DumpPredId >= 0
    ->
        ( PredIdInt = DumpPredId ->
            write_pred(Indent, ModuleInfo, PredId, PredInfo, !IO)
        ;
            true
        )
    ;
        (
            \+ string.contains_char(Verbose, 'I'),
            pred_info_is_imported(PredInfo)
        ;
            % For pseudo-imported predicates (i.e. unification preds),
            % only print them if we are using a local mode for them.
            \+ string.contains_char(Verbose, 'I'),
            pred_info_is_pseudo_imported(PredInfo),
            ProcIds = pred_info_procids(PredInfo),
            hlds_pred.in_in_unification_proc_id(ProcId),
            ProcIds = [ProcId]
        ;
            % We dump unification and other compiler-generated special
            % predicates if suboption 'U' is on. We don't need that information
            % to understand how the program has been transformed.
            \+ string.contains_char(Verbose, 'U'),
            is_unify_or_compare_pred(PredInfo)
        )
    ->
        true
    ;
        write_pred(Indent, ModuleInfo, PredId, PredInfo, !IO)
    ).

:- pred write_pred(int::in, module_info::in, pred_id::in, pred_info::in,
    io::di, io::uo) is det.

write_pred(Indent, ModuleInfo, PredId, PredInfo, !IO) :-
    Module = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    pred_info_context(PredInfo, Context),
    pred_info_get_import_status(PredInfo, ImportStatus),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_constraint_proofs(PredInfo, Proofs),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_head_type_params(PredInfo, HeadTypeParams),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),
    ( string.contains_char(Verbose, 'C') ->
        % Information about predicates is dumped if 'C' suboption is on.
        (
            PredOrFunc = predicate,
            mercury_output_pred_type(TVarSet, ExistQVars,
                qualified(Module, PredName), ArgTypes, no, Purity,
                ClassContext, Context, AppendVarNums, !IO)
        ;
            PredOrFunc = function,
            pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
            mercury_output_func_type(TVarSet, ExistQVars,
                qualified(Module, PredName), FuncArgTypes, FuncRetType, no,
                Purity, ClassContext, Context, AppendVarNums, !IO)
        )
    ;
        true
    ),
    ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars, ClausesRep,
        RttiVarMaps, _),
    ( string.contains_char(Verbose, 'C') ->
        write_indent(Indent, !IO),
        io.write_string("% pred id: ", !IO),
        pred_id_to_int(PredId, PredInt),
        io.write_int(PredInt, !IO),
        io.write_string(", category: ", !IO),
        write_pred_or_func(PredOrFunc, !IO),
        io.write_string(", status: ", !IO),
        write_import_status(ImportStatus, !IO),
        io.write_string("\n", !IO),
        io.write_string("% goal_type: ", !IO),
        pred_info_get_goal_type(PredInfo, GoalType),
        io.write(GoalType, !IO),
        io.write_string("\n", !IO),
        markers_to_marker_list(Markers, MarkerList),
        (
            MarkerList = []
        ;
            MarkerList = [_ | _],
            io.write_string("% markers: ", !IO),
            write_marker_list(MarkerList, !IO),
            io.write_string("\n", !IO)
        ),
        write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
            !IO),
        ( map.is_empty(Proofs) ->
            true
        ;
            write_constraint_proofs(Indent, TVarSet, Proofs, AppendVarNums,
                !IO),
            io.write_string("\n", !IO)
        ),
        ( map.is_empty(ConstraintMap) ->
            true
        ;
            write_constraint_map(Indent, TVarSet, ConstraintMap, AppendVarNums,
                !IO)
        ),

        (
            HeadTypeParams = [_ | _],
            io.write_string("% head_type_params:\n", !IO),
            io.write_string("% ", !IO),
            mercury_output_vars(TVarSet, AppendVarNums, HeadTypeParams, !IO),
            io.write_string("\n", !IO)
        ;
            HeadTypeParams = []
        ),
        write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),

        get_clause_list(ClausesRep, Clauses),
        (
            Clauses = [_ | _],
            set_dump_opts_for_clauses(SavedDumpString, !IO),
            write_clauses(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
                HeadVars, PredOrFunc, Clauses, no_varset_vartypes, !IO),
            globals.io_set_option(dump_hlds_options, string(SavedDumpString),
                !IO)
        ;
            Clauses = []
        ),

        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = origin_instance_method(MethodConstraints),
            MethodConstraints = instance_method_constraints(ClassId,
                InstanceTypes, InstanceConstraints, ClassMethodConstraints),
            io.write_string("% instance method constraints:\n", !IO),
            ClassId = class_id(ClassName, _),
            mercury_output_constraint(TVarSet, AppendVarNums,
                constraint(ClassName, InstanceTypes), !IO),
            io.nl(!IO),
            io.write_string("instance constraints: ", !IO),
            io.write_list(InstanceConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO),

            ClassMethodConstraints = constraints(MethodUnivConstraints,
                MethodExistConstraints),
            io.write_string("method univ constraints: ", !IO),
            io.write_list(MethodUnivConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO),
            io.write_string("method exist constraints: ", !IO),
            io.write_list(MethodExistConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO)
        ;
            Origin = origin_special_pred(_),
            io.write_string("% special pred\n", !IO)
        ;
            Origin = origin_transformed(Transformation, _, OrigPredId),
            io.write_string("% transformed from ", !IO),
            write_pred_id(ModuleInfo, OrigPredId, !IO),
            io.write_string(": ", !IO),
            io.write(Transformation, !IO),
            io.nl(!IO)
        ;
            Origin = origin_created(Creation),
            io.write_string("% created: ", !IO),
            io.write(Creation, !IO),
            io.nl(!IO)
        ;
            Origin = origin_assertion(_, _),
            io.write_string("% assertion\n", !IO)
        ;
            Origin = origin_lambda(_, _, _)
        ;
            Origin = origin_user(_)
        )
    ;
        true
    ),
    write_procs(Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus,
        PredInfo, !IO),
    io.write_string("\n", !IO).

:- pred set_dump_opts_for_clauses(string::out, io::di, io::uo) is det.

set_dump_opts_for_clauses(SavedDumpStr, !IO) :-
    globals.io_lookup_string_option(dump_hlds_options, SavedDumpStr, !IO),
    some [!DumpStr] (
        !:DumpStr = "",
        ( string.contains_char(SavedDumpStr, 'c') ->
            !:DumpStr = !.DumpStr ++ "c"
        ;
            true
        ),
        ( string.contains_char(SavedDumpStr, 'n') ->
            !:DumpStr = !.DumpStr ++ "n"
        ;
            true
        ),
        ( string.contains_char(SavedDumpStr, 'v') ->
            !:DumpStr = !.DumpStr ++ "v"
        ;
            true
        ),
        ( string.contains_char(SavedDumpStr, 'g') ->
            !:DumpStr = !.DumpStr ++ "g"
        ;
            true
        ),
        DumpStr = !.DumpStr
    ),
    globals.io_set_option(dump_hlds_options, string(DumpStr), !IO).

:- pred write_marker_list(list(marker)::in, io::di, io::uo) is det.

write_marker_list(Markers, !IO) :-
    io.write_list(Markers, ", ", write_marker, !IO).

marker_name(marker_stub, "stub").
marker_name(marker_infer_type, "infer_type").
marker_name(marker_infer_modes, "infer_modes").
marker_name(marker_user_marked_inline, "inline").
marker_name(marker_user_marked_no_inline, "no_inline").
marker_name(marker_heuristic_inline, "heuristic_inline").
marker_name(marker_obsolete, "obsolete").
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

write_marker(Marker, !IO) :-
    marker_name(Marker, Name),
    io.write_string(Name, !IO).

write_promise(PromiseType, Indent, ModuleInfo, _PredId, VarSet,
        AppendVarNums, HeadVars, _PredOrFunc, Clause, TypeQual, !IO) :-
    % Curry the varset for term_io.write_variable/4.
    PrintVar = (pred(VarName::in, IOState0::di, IOState::uo) is det :-
        term_io.write_variable(VarName, VarSet, IOState0, IOState)
    ),

    write_indent(Indent, !IO),

    % Print initial formatting differently for assertions.
    ( PromiseType = promise_type_true ->
        io.write_string(":- promise all [", !IO),
        io.write_list(HeadVars, ", ", PrintVar, !IO),
        io.write_string("] (\n", !IO)
    ;
        io.write_string(":- all [", !IO),
        io.write_list(HeadVars, ", ", PrintVar, !IO),
        io.write_string("]", !IO),
        mercury_output_newline(Indent, !IO),
        prog_out.write_promise_type(PromiseType, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("(\n", !IO)
    ),

    Clause = clause(_Modes, Goal, _Lang, _Context),
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent+1, ").\n",
        TypeQual, !IO).

write_clauses(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
        HeadVars, PredOrFunc, Clauses0, TypeQual, !IO) :-
    write_clauses_2(Indent, ModuleInfo, PredId, VarSet,
        AppendVarNums, HeadVars, PredOrFunc, Clauses0, TypeQual, 1, !IO).

:- pred write_clauses_2(int::in, module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_var)::in, pred_or_func::in,
    list(clause)::in, maybe_vartypes::in, int::in, io::di, io::uo) is det.

write_clauses_2(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
        HeadVars, PredOrFunc, Clauses0, TypeQual, ClauseNum, !IO) :-
    (
        Clauses0 = [Clause | Clauses]
    ->
        term.var_list_to_term_list(HeadVars, HeadTerms),
        UseDeclaredModes = no,
        io.write_string("% clause ", !IO),
        io.write_int(ClauseNum, !IO),
        io.write_string("\n", !IO),
        write_clause(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadTerms, PredOrFunc, Clause, UseDeclaredModes, TypeQual, !IO),
        write_clauses_2(Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadVars, PredOrFunc, Clauses, TypeQual, ClauseNum + 1, !IO)
    ;
        true
    ).

write_clause(Indent, ModuleInfo, PredId, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, Clause, UseDeclaredModes, TypeQual, !IO) :-
    Clause = clause(Modes, Goal, Lang, Context),
    Indent1 = Indent + 1,
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'm') ->
        write_indent(Indent, !IO),
        io.write_string("% Modes for which this clause applies: ", !IO),
        ModeInts = list.map(proc_id_to_int, Modes),
        write_intlist(ModeInts, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    (
        Lang = impl_lang_mercury
    ;
        Lang = impl_lang_foreign(ForeignLang),
        io.write_string("% Language of implementation: ", !IO),
        io.write(ForeignLang, !IO),
        io.nl(!IO)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    (
        ( Modes = []
        ; Modes = ProcIds
        )
    ->
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadTerms, PredOrFunc, !IO)
    ;
        % If Modes contains more than one mode, the output will have
        % multiple clause heads. This won't be pretty and it won't be
        % syntactically valid, but it is more useful for debugging
        % than a compiler abort during the dumping process.
        write_annotated_clause_heads(ModuleInfo, Context, PredId,
            Modes, VarSet, AppendVarNums, HeadTerms, PredOrFunc,
            UseDeclaredModes, !IO)
    ),
    ( Goal = conj(plain_conj, []) - _GoalInfo ->
        io.write_string(".\n", !IO)
    ;
        io.write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, ".\n",
            TypeQual, !IO)
    ).

:- pred write_annotated_clause_heads(module_info::in, term.context::in,
    pred_id::in, list(proc_id)::in, prog_varset::in, bool::in,
    list(prog_term)::in, pred_or_func::in, bool::in, io::di, io::uo) is det.

write_annotated_clause_heads(_, _, _, [], _, _, _, _, _, !IO).
write_annotated_clause_heads(ModuleInfo, Context, PredId, [ProcId | ProcIds],
        VarSet, AppendVarNums, HeadTerms, PredOrFunc, UseDeclaredModes, !IO) :-
    write_annotated_clause_head(ModuleInfo, Context, PredId,
        ProcId, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, UseDeclaredModes, !IO),
    write_annotated_clause_heads(ModuleInfo, Context, PredId,
        ProcIds, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, UseDeclaredModes, !IO).

:- pred write_annotated_clause_head(module_info::in, term.context::in,
    pred_id::in, proc_id::in, prog_varset::in, bool::in, list(prog_term)::in,
    pred_or_func::in, bool::in, io::di, io::uo) is det.

write_annotated_clause_head(ModuleInfo, Context, PredId, ProcId, VarSet,
        AppendVarNums, HeadTerms, PredOrFunc, UseDeclaredModes, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, Procedures),
    ( map.search(Procedures, ProcId, ProcInfo) ->
        % When writing `.opt' files, use the declared argument modes so that
        % the modes are guaranteed to be syntactically identical to those
        % in the original program. The test in add_clause.m to check whether
        % a clause matches a procedure tests for syntactic identity (roughly).
        % The modes returned by proc_info_get_argmodes may have been slightly
        % expanded by propagate_types_into_modes.
        %
        % We can't use the declared argument modes when writing HLDS dumps
        % because the modes of the type-infos will not have been added,
        % so the call to assoc_list.from_corresponding_lists below
        % will abort. `.opt' files are written before the polymorphism pass.
        (
            UseDeclaredModes = yes,
            proc_info_declared_argmodes(ProcInfo, ArgModes)
        ;
            UseDeclaredModes = no,
            proc_info_get_argmodes(ProcInfo, ArgModes)
        ),
        assoc_list.from_corresponding_lists(HeadTerms, ArgModes,
            AnnotatedPairs),
        AnnotatedHeadTerms = list.map(add_mode_qualifier(Context),
            AnnotatedPairs),
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            AnnotatedHeadTerms, PredOrFunc, !IO)
    ;
        % This procedure, even though it existed in the past, has been
        % eliminated.
        true
    ).

:- pred write_clause_head(module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_term)::in, pred_or_func::in,
    io::di, io::uo) is det.

write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, !IO) :-
    PredName = predicate_name(ModuleInfo, PredId),
    ModuleName = predicate_module(ModuleInfo, PredId),
    (
        PredOrFunc = function,
        pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
        write_qualified_functor_with_term_args(ModuleName,
            term.atom(PredName), FuncArgs, VarSet, AppendVarNums, !IO),
        io.write_string(" = ", !IO),
        mercury_output_term_nq(VarSet, AppendVarNums, next_to_graphic_token,
            RetVal, !IO)
    ;
        PredOrFunc = predicate,
        write_qualified_functor_with_term_args(ModuleName,
            term.atom(PredName), HeadTerms, VarSet, AppendVarNums, !IO)
    ).

write_goal(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow, !IO) :-
    % Don't type qualify everything.
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        no_varset_vartypes, !IO).

    % TypeQual is yes(TVarset, VarTypes) if all constructors should be
    % module qualified.
    %
:- pred write_goal_a(hlds_goal::in, module_info::in, prog_varset::in,
    bool::in, int::in, string::in, maybe_vartypes::in, io::di, io::uo) is det.

write_goal_a(Goal - GoalInfo, ModuleInfo, VarSet, AppendVarNums, Indent,
        Follow, TypeQual, !IO) :-
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'c') ->
        goal_info_get_context(GoalInfo, Context),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        ( FileName \= "" ->
            write_indent(Indent, !IO),
            io.write_string("% context: file `", !IO),
            io.write_string(FileName, !IO),
            io.write_string("', line ", !IO),
            io.write_int(LineNumber, !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'P') ->
        goal_info_get_goal_path(GoalInfo, Path),
        (
            Path = [_ | _],
            goal_path_to_string(Path, PathStr),
            write_indent(Indent, !IO),
            io.write_string("% goal path: ", !IO),
            io.write_string(PathStr, !IO),
            io.write_string("\n", !IO)
        ;
            Path = []
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'n') ->
        goal_info_get_nonlocals(GoalInfo, NonLocalsSet),
        set.to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            write_indent(Indent, !IO),
            io.write_string("% nonlocals: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, NonLocalsList, !IO),
            io.write_string("\n", !IO)
        ;
            NonLocalsList = []
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'p') ->
        (
            goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
            set.to_sorted_list(PreDeaths, PreDeathList),
            PreDeathList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io.write_string("% pre-deaths: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, PreDeathList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        ),
        (
            goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
            set.to_sorted_list(PreBirths, PreBirthList),
            PreBirthList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io.write_string("% pre-births: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, PreBirthList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'B') ->
        ProducingVars = GoalInfo ^ producing_vars,
        ( set.non_empty(ProducingVars) ->
            set.to_sorted_list(ProducingVars, ProducingVarsList),
            write_indent(Indent, !IO),
            io.write_string("% producing vars: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, ProducingVarsList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        ),

        ConsumingVars = GoalInfo ^ consuming_vars,
        ( set.non_empty(ConsumingVars) ->
            set.to_sorted_list(ConsumingVars, ConsumingVarsList),
            write_indent(Indent, !IO),
            io.write_string("% consuming vars: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, ConsumingVarsList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        ),

        MakeVisibleVars = GoalInfo ^ make_visible_vars,
        ( set.non_empty(MakeVisibleVars) ->
            set.to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            write_indent(Indent, !IO),
            io.write_string("% make_visible vars: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, MakeVisibleVarsList,
                !IO),
            io.write_string("\n", !IO)
        ;
            true
        ),

        NeedVisibleVars = GoalInfo ^ need_visible_vars,
        ( set.non_empty(NeedVisibleVars) ->
            set.to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            write_indent(Indent, !IO),
            io.write_string("% need_visible vars: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, NeedVisibleVarsList,
                !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'd') ->
        write_indent(Indent, !IO),
        io.write_string("% determinism: ", !IO),
        goal_info_get_determinism(GoalInfo, Determinism),
        write_determinism(Determinism, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    write_goal_2(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO),
    ( string.contains_char(Verbose, 'i') ->
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        (
            instmap_delta_is_reachable(InstMapDelta),
            instmap_delta_changed_vars(InstMapDelta, Vars),
            set.empty(Vars)
        ->
            true
        ;
            write_indent(Indent, !IO),
            ( string.contains_char(Verbose, 'D') ->
                io.write_string("% new insts: ", !IO),
                write_instmap_delta(InstMapDelta, VarSet, AppendVarNums,
                    Indent, !IO),
                io.write_string("\n", !IO)
            ;
                io.write_string("% vars with new insts: ", !IO),
                write_instmap_delta_vars(InstMapDelta, VarSet, AppendVarNums,
                    !IO),
                io.write_string("\n", !IO)
            )
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'p') ->
        (
            goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
            set.to_sorted_list(PostDeaths, PostDeathList),
            PostDeathList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io.write_string("% post-deaths: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, PostDeathList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        ),
        (
            goal_info_maybe_get_post_births(GoalInfo, PostBirths),
            set.to_sorted_list(PostBirths, PostBirthList),
            PostBirthList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io.write_string("% post-births: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, PostBirthList, !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'R') ->
        (
            goal_info_maybe_get_lfu(GoalInfo, LFU),
            goal_info_maybe_get_lbu(GoalInfo, LBU), 
            goal_info_maybe_get_reuse(GoalInfo, ReuseDescription), 
            set.to_sorted_list(LFU, ListLFU),
            set.to_sorted_list(LBU, ListLBU)
        ->
            write_indent(Indent, !IO),
            io.write_string("% LFU: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, ListLFU, !IO),
            io.write_string("\n", !IO),
            write_indent(Indent, !IO),
            io.write_string("% LBU: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, ListLBU, !IO),
            io.write_string("\n", !IO),

            write_indent(Indent, !IO),
            write_string("% Reuse: ", !IO),
            (
                ReuseDescription = no_reuse_info,
                io.write_string("no", !IO)
            ;
                ReuseDescription = missed_reuse(Messages),
                io.write_string("missed (", !IO), 
                io.write_list(Messages, ", ", io.write_string, !IO),
                io.write_string(")", !IO)
            ;
                ReuseDescription = potential_reuse(ShortReuseDescr),
                io.write_string("potential reuse (", !IO), 
                write_short_reuse_description(ShortReuseDescr, VarSet, 
                    AppendVarNums, !IO), 
                io.write_string(")", !IO)
            ;
                ReuseDescription = reuse(ShortReuseDescr),
                io.write_string("reuse (", !IO), 
                write_short_reuse_description(ShortReuseDescr, VarSet,
                    AppendVarNums, !IO), 
                io.write_string(")", !IO)
            ), 
            io.write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    (
        CodeGenInfo = no_code_gen_info
    ;
        CodeGenInfo = llds_code_gen_info(_CodeGenDetails),
        write_llds_code_gen_info(GoalInfo, VarSet, AppendVarNums, Indent,
            Verbose, !IO)
    ),
    ( string.contains_char(Verbose, 'g') ->
        goal_info_get_features(GoalInfo, Features),
        set.to_sorted_list(Features, FeatureList),
        (
            FeatureList = []
        ;
            FeatureList = [_ | _],
            write_indent(Indent, !IO),
            io.write_string("% Goal features:  ", !IO),
            io.write(FeatureList, !IO),
            io.write_string("\n", !IO)
        )
    ;
        true
    ).

:- pred write_goal_2(hlds_goal_expr::in, module_info::in,
    prog_varset::in, bool::in, int::in, string::in, maybe_vartypes::in,
    io::di, io::uo) is det.

write_goal_2(switch(Var, CanFail, CasesList), ModuleInfo, VarSet,
        AppendVarNums, Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("( % ", !IO),
    write_can_fail(CanFail, !IO),
    io.write_string(" switch on `", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string("'\n", !IO),
    Indent1 = Indent + 1,
    (
        CasesList = [Case | Cases],
        write_case(Case, Var, ModuleInfo, VarSet, AppendVarNums, Indent1,
            TypeQual, !IO),
        write_cases(Cases, Var, ModuleInfo, VarSet, AppendVarNums, Indent,
            TypeQual, !IO)
    ;
        CasesList = [],
        write_indent(Indent1, !IO),
        io.write_string("fail\n", !IO)
    ),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

write_goal_2(scope(Reason, Goal), ModuleInfo, VarSet, AppendVarNums, Indent,
        Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    (
        Reason = exist_quant(Vars),
        io.write_string("some [", !IO),
        mercury_output_vars(VarSet, AppendVarNums, Vars, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = promise_purity(Implicit, Purity),
        (
            Purity = purity_pure,
            io.write_string("promise_pure (", !IO)
        ;
            Purity = purity_semipure,
            io.write_string("promise_semipure (", !IO)
        ;
            Purity = purity_impure,
            io.write_string("promise_impure (", !IO)
        ),
        (
            Implicit = make_implicit_promises,
            io.write_string("implicit\n", !IO)
        ;
            Implicit = dont_make_implicit_promises,
            io.write_string("\n", !IO)
        )
    ;
        Reason = promise_solutions(Vars, Kind),
        (
            Kind = equivalent_solutions,
            io.write_string("promise_equivalent_solutions", !IO)
        ;
            Kind = equivalent_solution_sets,
            io.write_string("promise_equivalent_solution_sets", !IO)
        ;
            Kind = equivalent_solution_sets_arbitrary,
            io.write_string("arbitrary", !IO)
        ),
        io.write_string(" [", !IO),
        mercury_output_vars(VarSet, AppendVarNums, Vars, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = barrier(removable),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% barrier(removable)\n", !IO)
    ;
        Reason = barrier(not_removable),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% barrier(not_removable)\n", !IO)
    ;
        Reason = commit(force_pruning),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% commit(force_pruning)\n", !IO)
    ;
        Reason = commit(dont_force_pruning),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% commit(dont_force_pruning)\n", !IO)
    ;
        Reason = from_ground_term(Var),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% from_ground_term [", !IO),
        mercury_output_var(VarSet, AppendVarNums, Var, !IO),
        io.write_string("]\n", !IO)
    ;
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIO,
            MutableVars, QuantVars),
        io.write_string("(\n", !IO),
        write_indent(Indent + 1, !IO),
        io.write_string("% trace\n", !IO),
        (
            MaybeCompileTime = yes(CompileTime),
            write_indent(Indent + 1, !IO),
            io.write_string("% compiletime(", !IO),
            mercury_output_trace_expr(mercury_output_trace_compiletime,
                CompileTime, !IO),
            io.write_string(")\n", !IO)
        ;
            MaybeCompileTime = no
        ),
        (
            MaybeRunTime = yes(RunTime),
            write_indent(Indent + 1, !IO),
            io.write_string("% runtime(", !IO),
            mercury_output_trace_expr(mercury_output_trace_runtime,
                RunTime, !IO),
            io.write_string(")\n", !IO)
        ;
            MaybeRunTime = no
        ),
        (
            MaybeIO = yes(IOStateVarName),
            write_indent(Indent + 1, !IO),
            io.write_string("% io(!" ++ IOStateVarName ++ ")\n", !IO)
        ;
            MaybeIO = no
        ),
        list.foldl(write_trace_mutable_var_hlds(Indent + 1), MutableVars, !IO),
        write_indent(Indent + 1, !IO),
        io.write_string("% quantified vars ", !IO),
        list.foldl(mercury_output_var(VarSet, AppendVarNums), QuantVars, !IO),
        io.nl(!IO)
    ),
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

write_goal_2(if_then_else(Vars, Cond, Then, Else), ModuleInfo, VarSet,
        AppendVarNums, Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("(if", !IO),
    write_some(Vars, VarSet, !IO),
    io.write_string("\n", !IO),
    Indent1 = Indent + 1,
    write_goal_a(Cond, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string("then\n", !IO),
    write_goal_a(Then, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string("else\n", !IO),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    (
        Verbose \= "",
        Else = if_then_else(_, _, _, _) - _
    ->
        write_goal_a(Else, ModuleInfo, VarSet, AppendVarNums, Indent, "\n",
            TypeQual, !IO)
    ;
        write_goal_a(Else, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
            TypeQual, !IO)
    ),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

write_goal_2(negation(Goal), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("\\+ (\n", !IO),
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

write_goal_2(conj(ConjType, List), ModuleInfo, VarSet, AppendVarNums, Indent,
        Follow, TypeQual, !IO) :-
    (
        List = [Goal | Goals],
        (
            ConjType = plain_conj,
            globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
            ( Verbose \= "" ->
                write_indent(Indent, !IO),
                io.write_string("( % conjunction\n", !IO),
                write_conj(Goal, Goals, ModuleInfo, VarSet, AppendVarNums,
                    Indent + 1, "\n", Verbose, ",\n", TypeQual, !IO),
                write_indent(Indent, !IO),
                io.write_string(")", !IO),
                io.write_string(Follow, !IO)
            ;
                write_conj(Goal, Goals, ModuleInfo, VarSet, AppendVarNums,
                    Indent, Follow, Verbose, ",\n", TypeQual, !IO)
            )
        ;
            ConjType = parallel_conj,
            write_indent(Indent, !IO),
            io.write_string("( % parallel conjunction\n", !IO),
            write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1,
                "\n", TypeQual, !IO),
            % See comments at write_goal_list.
            write_goal_list(Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
                "&\n", TypeQual, !IO),
            write_indent(Indent, !IO),
            io.write_string(")", !IO),
            io.write_string(Follow, !IO)
        )
    ;
        List = [],
        write_indent(Indent, !IO),
        (
            ConjType = plain_conj,
            io.write_string("true", !IO)
        ;
            ConjType = parallel_conj,
            io.write_string("/* parallel */ true", !IO)
        ),
        io.write_string(Follow, !IO)
    ).

write_goal_2(disj(List), ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    ( List = [Goal | Goals] ->
        io.write_string("( % disjunction\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
            TypeQual, !IO),
        write_goal_list(Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
            ";\n", TypeQual, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO),
        io.write_string(Follow, !IO)
    ;
        io.write_string("fail", !IO),
        io.write_string(Follow, !IO)
    ).

write_goal_2(generic_call(GenericCall, ArgVars, Modes, _),
        _ModuleInfo, VarSet, AppendVarNums, Indent, Follow, _, !IO) :-
    % XXX we should print more info here
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
        globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        (
            PredOrFunc = predicate,
            ( string.contains_char(Verbose, 'l') ->
                write_indent(Indent, !IO),
                io.write_string("% higher-order predicate call\n", !IO)
            ;
                true
            ),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            write_functor(term.atom("call"), [PredVar | ArgVars], VarSet,
                AppendVarNums, !IO)
        ;
            PredOrFunc = function,
            ( string.contains_char(Verbose, 'l') ->
                write_indent(Indent, !IO),
                io.write_string("% higher-order function application\n", !IO)
            ;
                true
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            mercury_output_var(VarSet, AppendVarNums, FuncRetVar, !IO),
            io.write_string(" = ", !IO),
            write_functor(term.atom("apply"), FuncArgVars, VarSet,
                AppendVarNums, !IO)
        ),
        io.write_string(Follow, !IO)
    ;
        GenericCall = class_method(TCInfoVar, MethodNum, _ClassId, _MethodId),
        globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string.contains_char(Verbose, 'l') ->
            write_indent(Indent, !IO),
            io.write_string("% class method call\n", !IO)
        ;
            true
        ),
        term.context_init(Context),
        Functor = term.atom("class_method_call"),
        TCInfoTerm = term.variable(TCInfoVar, Context),
        MethodNumTerm = term.functor(term.integer(MethodNum), [], Context),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
            Context),
        write_indent(Indent, !IO),
        mercury_output_term(VarSet, AppendVarNums, Term, !IO),
        io.write_string(Follow, !IO)
    ;
        GenericCall = event_call(EventName),
        globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string.contains_char(Verbose, 'l') ->
            write_indent(Indent, !IO),
            io.write_string("% event call\n", !IO)
        ;
            true
        ),
        term.context_init(Context),
        Functor = term.atom(EventName),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, ArgTerms, Context),
        write_indent(Indent, !IO),
        mercury_output_term(VarSet, AppendVarNums, Term, !IO),
        io.write_string(Follow, !IO)
    ;
        GenericCall = cast(CastType),
        CastTypeString = cast_type_to_string(CastType),
        globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string.contains_char(Verbose, 'l') ->
            write_indent(Indent, !IO),
            io.write_strings(["% ", CastTypeString, "\n"], !IO)
        ;
            true
        ),
        ( string.contains_char(Verbose, 'D') ->
            write_indent(Indent, !IO),
            io.write_string("% modes: ", !IO),
            varset.init(InstVarSet),
            mercury_output_mode_list(Modes, InstVarSet, !IO),
            io.nl(!IO)
        ;
            true
        ),
        Functor = term.atom(CastTypeString),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        term.context_init(Context),
        Term = term.functor(Functor, ArgTerms, Context),
        write_indent(Indent, !IO),
        mercury_output_term(VarSet, AppendVarNums, Term, !IO),
        io.write_string(Follow, !IO)
    ).

write_goal_2(plain_call(PredId, ProcId, ArgVars, Builtin, MaybeUnifyContext,
        PredName), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'b') ->
        (
            Builtin = inline_builtin,
            write_indent(Indent, !IO),
            io.write_string("% inline builtin\n", !IO)
        ;
            Builtin = out_of_line_builtin,
            write_indent(Indent, !IO),
            io.write_string("% out of line builtin\n", !IO)
        ;
            Builtin = not_builtin
        )
    ;
        true
    ),
    write_indent(Indent, !IO),
    ( PredId = invalid_pred_id ->
        % If we don't know then the call must be treated as a predicate.
        PredOrFunc = predicate
    ;
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_purity(PredInfo, Purity),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        write_purity_prefix(Purity, !IO)
    ),
    (
        PredOrFunc = predicate,
        NewArgVars = ArgVars
    ;
        PredOrFunc = function,
        pred_args_to_func_args(ArgVars, NewArgVars, LHSVar),
        mercury_output_var(VarSet, AppendVarNums, LHSVar, !IO),
        io.write_string(" = ", !IO)
    ),
    write_sym_name_and_args(PredName, NewArgVars, VarSet, AppendVarNums, !IO),
    io.write_string(Follow, !IO),
    ( string.contains_char(Verbose, 'l') ->
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        write_indent(Indent, !IO),
        io.write_string("% pred id: ", !IO),
        io.write_int(PredNum, !IO),
        io.write_string(", proc id: ", !IO),
        io.write_int(ProcNum, !IO),
        io.write_string(Follow, !IO),
        (
            MaybeUnifyContext = yes(CallUnifyContext),
            (
                TypeQual = varset_vartypes(_, VarTypes),
                map.lookup(VarTypes, Var, UniType),
                VarType = yes(UniType)
            ;
                TypeQual = no_varset_vartypes,
                VarType = no
            ),
            CallUnifyContext = call_unify_context(Var, RHS, _UnifyContext),
            write_indent(Indent, !IO),
            io.write_string("% unify context: ", !IO),
            mercury_output_var(VarSet, AppendVarNums, Var, !IO),
            io.write_string(" = ", !IO),
            % XXX Fake the inst varset.
            varset.init(InstVarSet),
            write_unify_rhs_2(RHS, ModuleInfo, VarSet, InstVarSet,
                AppendVarNums, Indent, Follow, VarType, TypeQual, !IO)
        ;
            MaybeUnifyContext = no
        )
    ;
        true
    ).

write_goal_2(unify(A, B, _, Unification, _), ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    mercury_output_var(VarSet, AppendVarNums, A, !IO),
    io.write_string(" = ", !IO),
    (
        TypeQual = varset_vartypes(_, VarTypes),
        map.lookup(VarTypes, A, UniType),
        VarType = yes(UniType)
    ;
        TypeQual = no_varset_vartypes,
        VarType = no
    ),
    % XXX Fake the inst varset.
    varset.init(InstVarSet),
    write_unify_rhs_2(B, ModuleInfo, VarSet, InstVarSet,
        AppendVarNums, Indent, Follow, VarType, TypeQual, !IO),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    (
        (
            string.contains_char(Verbose, 'u')
        ;
            string.contains_char(Verbose, 'p')
        )
    ->
        (
            % Don't output bogus info if we haven't been through
            % mode analysis yet.
            Unification = complicated_unify(Mode, CanFail, TypeInfoVars),
            CanFail = can_fail,
            Mode = (free - free -> free - free),
            TypeInfoVars = []
        ->
            true
        ;
            write_unification(Unification, ModuleInfo, VarSet, InstVarSet,
                AppendVarNums, Indent, !IO)
        )
    ;
        true
    ).

write_goal_2(call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, PragmaCode), ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, _, !IO) :-
    ForeignLang = get_foreign_language(Attributes),
    write_indent(Indent, !IO),
    io.write_string("$pragma_foreign_proc(/* ", !IO),
    io.write_string(foreign_language_string(ForeignLang), !IO),
    io.write_string(" */, ", !IO),
    write_pred_id(ModuleInfo, PredId, !IO),
    io.write_string(" pred ", !IO),
    pred_id_to_int(PredId, PredIdInt),
    io.write_int(PredIdInt, !IO),
    io.write_string(" proc ", !IO),
    proc_id_to_int(ProcId, ProcIdInt),
    io.write_int(ProcIdInt, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeTraceRuntimeCond = no
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        write_indent(Indent, !IO),
        io.write_string("% trace_runtime_cond(", !IO),
        mercury_output_trace_expr(mercury_output_trace_runtime,
            TraceRuntimeCond, !IO),
        io.write_string(")\n", !IO)
    ),
    write_indent(Indent, !IO),
    % XXX We don't have the TypeVarSet available here, but it is only used
    % for printing out the names of the type variables, which isn't essential.
    varset.init(TypeVarSet),
    io.write_string("[", !IO),
    write_foreign_args(Args, VarSet, TypeVarSet, AppendVarNums, !IO),
    io.write_string("],\n", !IO),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        write_indent(Indent, !IO),
        io.write_string("{", !IO),
        write_foreign_args(ExtraArgs, VarSet, TypeVarSet, AppendVarNums, !IO),
        io.write_string("},\n", !IO)
    ),
    (
        PragmaCode = fc_impl_ordinary(C_Code, _),
        io.write_string("""", !IO),
        io.write_string(C_Code, !IO),
        io.write_string("""", !IO)
    ;
        PragmaCode = fc_impl_model_non(Fields, _FieldsContext,
            First, _FirstContext, Later, _LaterContext,
            Treat, Shared, _SharedContext),
        io.write_string("local_vars(""", !IO),
        io.write_string(Fields, !IO),
        io.write_string("""), ", !IO),
        io.write_string("first_code(""", !IO),
        io.write_string(First, !IO),
        io.write_string("""), ", !IO),
        io.write_string("retry_code(""", !IO),
        io.write_string(Later, !IO),
        io.write_string("""), ", !IO),
        (
            Treat = share,
            io.write_string("shared_code(""", !IO)
        ;
            Treat = duplicate,
            io.write_string("duplicated_code(""", !IO)
        ;
            Treat = automatic,
            io.write_string("common_code(""", !IO)
        ),
        io.write_string(Shared, !IO),
        io.write_string(""")", !IO)
    ;
        PragmaCode = fc_impl_import(Name, _, _, _Context),
        io.write_string("""", !IO),
        io.write_string(Name, !IO),
        io.write_string("""", !IO)
    ),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

write_goal_2(shorthand(ShortHandGoal), ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, TypeQual, !IO) :-
    write_goal_2_shorthand(ShortHandGoal, ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, TypeQual, !IO).

:- pred write_goal_2_shorthand(shorthand_goal_expr::in, module_info::in,
    prog_varset::in, bool::in, int::in, string::in, maybe_vartypes::in,
    io::di, io::uo) is det.

write_goal_2_shorthand(bi_implication(LHS, RHS), ModuleInfo, VarSet,
        AppendVarNums, Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("( % bi-implication\n", !IO),
    Indent1 = Indent + 1,
    write_goal_a(LHS, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string("<=>\n", !IO),
    write_goal_a(RHS, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

:- pred write_trace_mutable_var_hlds(int::in, trace_mutable_var_hlds::in,
    io::di, io::uo) is det.

write_trace_mutable_var_hlds(Indent, MutableVar, !IO) :-
    MutableVar = trace_mutable_var_hlds(MutableName, StateVarName),
    write_indent(Indent, !IO),
    io.write_string("% mutable " ++ MutableName ++ ": ", !IO),
    io.write_string("!" ++ StateVarName ++ "\n", !IO).

:- pred write_foreign_args(list(foreign_arg)::in, prog_varset::in,
    tvarset::in, bool::in, io::di, io::uo) is det.

write_foreign_args([], _, _, _, !IO).
write_foreign_args([Arg | Args], VarSet, TVarSet, AppendVarNums, !IO) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    (
        MaybeNameMode = yes(Name - Mode),
        io.write_string("/" ++ Name ++ "(", !IO),
        ( Mode = in_mode ->
            io.write_string("in", !IO)
        ; Mode = out_mode ->
            io.write_string("out", !IO)
        ;
            io.write(Mode, !IO)
        ),
        io.write_string(")", !IO)
    ;
        MaybeNameMode = no
    ),
    (
        BoxPolicy = native_if_possible
    ;
        BoxPolicy = always_boxed,
        io.write_string("$alwaysboxed", !IO)
    ),
    io.write_string("@", !IO),
    mercury_output_type(TVarSet, AppendVarNums, Type, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string(", ", !IO),
        write_foreign_args(Args, VarSet, TVarSet, AppendVarNums, !IO)
    ).

:- pred write_llds_code_gen_info(hlds_goal_info::in, prog_varset::in,
    bool::in, int::in, string::in, io::di, io::uo) is det.

write_llds_code_gen_info(GoalInfo, VarSet, AppendVarNums,
        Indent, Verbose, !IO) :-
    ( string.contains_char(Verbose, 'f') ->
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextReg),
            map.to_assoc_list(FollowVarsMap, FVlist),
            write_indent(Indent, !IO),
            io.write_string("% follow vars: ", !IO),
            io.write_int(NextReg, !IO),
            io.write_string("\n", !IO),
            write_var_to_abs_locns(FVlist, VarSet, AppendVarNums, Indent, !IO)
        ;
            MaybeFollowVars = no
        )
    ;
        true
    ),
    ( string.contains_char(Verbose, 'r') ->
        goal_info_get_resume_point(GoalInfo, Resume),
        (
            Resume = no_resume_point
        ;
            Resume = resume_point(ResumeVars, Locs),
            set.to_sorted_list(ResumeVars, ResumeVarList),
            write_indent(Indent, !IO),
            io.write_string("% resume point ", !IO),
            (
                Locs = resume_locs_orig_only,
                io.write_string("orig only ", !IO)
            ;
                Locs = resume_locs_stack_only,
                io.write_string("stack only ", !IO)
            ;
                Locs = resume_locs_orig_and_stack,
                io.write_string("orig and stack ", !IO)
            ;
                Locs = resume_locs_stack_and_orig,
                io.write_string("stack and orig ", !IO)
            ),
            mercury_output_vars(VarSet, AppendVarNums, ResumeVarList, !IO),
            io.write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string.contains_char(Verbose, 's'),
        goal_info_get_store_map(GoalInfo, StoreMap),
        map.to_assoc_list(StoreMap, StoreMapList),
        StoreMapList = [_ | _]
    ->
        write_indent(Indent, !IO),
        io.write_string("% store map:\n", !IO),
        write_var_to_abs_locns(StoreMapList, VarSet, AppendVarNums, Indent,
            !IO)
    ;
        true
    ),
    (
        string.contains_char(Verbose, 's'),
        goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
        MaybeNeedAcrossCall = yes(NeedAcrossCall)
    ->
        NeedAcrossCall = need_across_call(CallForwardSet, CallResumeSet,
            CallNondetSet),
        set.to_sorted_list(CallForwardSet, CallForwardList),
        set.to_sorted_list(CallResumeSet, CallResumeList),
        set.to_sorted_list(CallNondetSet, CallNondetList),
        write_indent(Indent, !IO),
        io.write_string("% need across call forward vars: ", !IO),
        (
            CallForwardList = [],
            io.write_string("none\n", !IO)
        ;
            CallForwardList = [_ | _],
            write_vars(VarSet, AppendVarNums, CallForwardList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need across call resume vars: ", !IO),
        (
            CallResumeList = [],
            io.write_string("none\n", !IO)
        ;
            CallResumeList = [_ | _],
            write_vars(VarSet, AppendVarNums, CallResumeList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need across call nondet vars: ", !IO),
        (
            CallNondetList = [],
            io.write_string("none\n", !IO)
        ;
            CallNondetList = [_ | _],
            write_vars(VarSet, AppendVarNums, CallNondetList, !IO),
            io.write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string.contains_char(Verbose, 's'),
        goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume),
        MaybeNeedInResume = yes(NeedInResume)
    ->
        NeedInResume = need_in_resume(ResumeOnStack, ResumeResumeSet,
            ResumeNondetSet),
        set.to_sorted_list(ResumeResumeSet, ResumeResumeList),
        set.to_sorted_list(ResumeNondetSet, ResumeNondetList),

        write_indent(Indent, !IO),
        (
            ResumeOnStack = yes,
            io.write_string("% resume point has stack label\n", !IO)
        ;
            ResumeOnStack = no,
            io.write_string("% resume point has no stack label\n", !IO)
        ),
        write_indent(Indent, !IO),
        io.write_string("% need in resume resume vars: ", !IO),
        (
            ResumeResumeList = [],
            io.write_string("none\n", !IO)
        ;
            ResumeResumeList = [_ | _],
            write_vars(VarSet, AppendVarNums, ResumeResumeList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need in resume nondet vars: ", !IO),
        (
            ResumeNondetList = [],
            io.write_string("none\n", !IO)
        ;
            ResumeNondetList = [_ | _],
            write_vars(VarSet, AppendVarNums, ResumeNondetList, !IO),
            io.write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string.contains_char(Verbose, 's'),
        goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj),
        MaybeNeedInParConj = yes(NeedInParConj)
    ->
        NeedInParConj = need_in_par_conj(ParConjSet),
        set.to_sorted_list(ParConjSet, ParConjList),
        write_indent(Indent, !IO),
        io.write_string("% need in par_conj vars: ", !IO),
        write_vars(VarSet, AppendVarNums, ParConjList, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ).

:- pred write_vars(prog_varset::in, bool::in, list(prog_var)::in,
    io::di, io::uo) is det.

write_vars(_, _, [], !IO).
write_vars(VarSet, AppendVarNums, [Var], !IO) :-
    mercury_output_var(VarSet, AppendVarNums, Var, !IO).
write_vars(VarSet, AppendVarNums, [Var1, Var2 | Vars], !IO) :-
    mercury_output_var(VarSet, AppendVarNums, Var1, !IO),
    io.write_string(", ", !IO),
    write_vars(VarSet, AppendVarNums, [Var2 | Vars], !IO).

:- pred write_varnum_list(list(prog_var)::in, io::di, io::uo) is det.

write_varnum_list([], !IO).
write_varnum_list([Var], !IO) :-
    write_varnum(Var, !IO).
write_varnum_list([Var1, Var2 | Vars], !IO) :-
    write_varnum(Var1, !IO),
    io.write_string(", ", !IO),
    write_varnum_list([Var2 | Vars], !IO).

:- pred write_varnum(var(T)::in, io::di, io::uo) is det.

write_varnum(Var, !IO) :-
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO).

:- pred write_var_name_list(list(pair(var(T), string))::in,
    io::di, io::uo) is det.

write_var_name_list([], !IO).
write_var_name_list([Var - Name], !IO) :-
    write_varnum(Var, !IO),
    io.write_string(" - ", !IO),
    io.write_string(Name, !IO).

write_var_name_list([Var1 - Name1, VarName2 | Vars], !IO) :-
    write_varnum(Var1, !IO),
    io.write_string(" - ", !IO),
    io.write_string(Name1, !IO),
    io.write_string(", ", !IO),
    write_var_name_list([VarName2 | Vars], !IO).

:- pred write_unification(unification::in, module_info::in, prog_varset::in,
    inst_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_unification(assign(X, Y), _, ProgVarSet, _InstVarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, X, !IO),
    io.write_string(" := ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, Y, !IO),
    io.write_string("\n", !IO).

write_unification(simple_test(X, Y), _, ProgVarSet, _, AppendVarNums, Indent,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, X, !IO),
    io.write_string(" == ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, Y, !IO),
    io.write_string("\n", !IO).

write_unification(construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
        Uniqueness, SubInfo), ModuleInfo, ProgVarSet, InstVarSet,
        AppendVarNums, Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, Var, !IO),
    io.write_string(" := ", !IO),
    write_functor_and_submodes(ConsId, ArgVars, ArgModes, ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO),
    (
        Uniqueness = cell_is_unique,
        write_indent(Indent, !IO),
        io.write_string("% cell_is_unique\n", !IO)
    ;
        Uniqueness = cell_is_shared
    ),
    (
        SubInfo = no_construct_sub_info
    ;
        SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
        (
            MaybeTakeAddr = yes(TakeAddressFields),
            write_indent(Indent, !IO),
            io.write_string("% take address fields: ", !IO),
            write_intlist(TakeAddressFields, !IO),
            io.write_string("\n", !IO)
        ;
            MaybeTakeAddr = no
        ),
        (
            MaybeSize = yes(SizeSource),
            write_indent(Indent, !IO),
            io.write_string("% term size ", !IO),
            (
                SizeSource = known_size(KnownSize),
                io.write_string("const ", !IO),
                io.write_int(KnownSize, !IO),
                io.write_string("\n", !IO)
            ;
                SizeSource = dynamic_size(SizeVar),
                io.write_string("var ", !IO),
                mercury_output_var(ProgVarSet, AppendVarNums, SizeVar, !IO),
                io.write_string("\n", !IO)
            )
        ;
            MaybeSize = no
        )
    ),
    (
        ConstructHow = construct_dynamically
    ;
        ConstructHow = construct_statically(StaticConsList),
        write_indent(Indent, !IO),
        io.write_string("% construct statically\n", !IO),
        list.foldl(write_static_cons(Indent, 1, ProgVarSet, AppendVarNums),
            StaticConsList, !IO)
    ;
        ConstructHow = reuse_cell(CellToReuse),
        CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds, _FieldAssigns),
        write_indent(Indent, !IO),
        io.write_string("% reuse cell: ", !IO),
        mercury_output_var(ProgVarSet, AppendVarNums, ReuseVar, !IO),
        io.write_string("\n", !IO)
    ).

write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail, CanCGC),
        ModuleInfo, ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO) :-
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'G') ->
        write_indent(Indent, !IO),
        io.write_string("% Compile time garbage collect: ", !IO),
        io.write(CanCGC, !IO),
        io.nl(!IO)
    ;
        true
    ),
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(ProgVarSet, AppendVarNums, Var, !IO),
    (
        CanFail = can_fail,
        io.write_string(" ?= ", !IO)
    ;
        CanFail = cannot_fail,
        io.write_string(" => ", !IO)
    ),
    write_functor_and_submodes(ConsId, ArgVars, ArgModes, ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO).

write_unification(complicated_unify(Mode, CanFail, TypeInfoVars), _ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    (
        CanFail = can_fail,
        io.write_string("can_fail, ", !IO)
    ;
        CanFail = cannot_fail,
        io.write_string("cannot_fail, ", !IO)
    ),
    io.write_string("mode: ", !IO),
    mercury_output_uni_mode(Mode, InstVarSet, !IO),
    io.write_string("\n", !IO),
    write_indent(Indent, !IO),
    io.write_string("% type-info vars: ", !IO),
    mercury_output_vars(ProgVarSet, AppendVarNums, TypeInfoVars, !IO),
    io.write_string("\n", !IO).

:- pred write_static_cons(int::in, int::in, prog_varset::in, bool::in,
    static_cons::in, io::di, io::uo) is det.

write_static_cons(Indent, Depth, VarSet, AppendVarNums, StaticCons, !IO) :-
    StaticCons = static_cons(ConsId, ArgVars, ArgStaticConstList),
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    write_indent(Depth, !IO),
    mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
    io.write_string("\n", !IO),
    (
        ArgVars = []
    ;
        ArgVars = [_ | _],
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        write_indent(Depth, !IO),
        mercury_output_vars(VarSet, AppendVarNums, ArgVars, !IO),
        io.write_string("\n", !IO)
    ),
    list.foldl(write_static_cons(Indent, Depth + 1, VarSet, AppendVarNums),
        ArgStaticConstList, !IO).

:- pred write_functor_and_submodes(cons_id::in, list(prog_var)::in,
    list(uni_mode)::in, module_info::in, prog_varset::in, inst_varset::in,
    bool::in, int::in, io::di, io::uo) is det.

write_functor_and_submodes(ConsId, ArgVars, ArgModes, _ModuleInfo, ProgVarSet,
        InstVarSet, AppendVarNums, Indent, !IO) :-
    write_cons_id(ConsId, !IO),
    (
        ArgVars = [],
        io.write_string("\n", !IO)
    ;
        ArgVars = [_ | _],
        io.write_string(" (", !IO),
        mercury_output_vars(ProgVarSet, AppendVarNums, ArgVars, !IO),
        io.write_string(")\n", !IO),
        globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string.contains_char(Verbose, 'a') ->
            write_indent(Indent, !IO),
            io.write_string("% arg-modes ", !IO),
            mercury_output_uni_mode_list(ArgModes, InstVarSet, !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ).

write_unify_rhs(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums, Indent,
        !IO) :-
    write_unify_rhs_3(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums,
        Indent, no, no_varset_vartypes, !IO).

:- pred write_unify_rhs_2(unify_rhs::in, module_info::in,
    prog_varset::in, inst_varset::in, bool::in, int::in, string::in,
    maybe(mer_type)::in, maybe_vartypes::in, io::di, io::uo) is det.

write_unify_rhs_2(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums, Indent,
        Follow, MaybeType, TypeQual, !IO) :-
    write_unify_rhs_3(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums,
        Indent, MaybeType, TypeQual, !IO),
    io.write_string(Follow, !IO).

:- pred write_unify_rhs_3(unify_rhs::in, module_info::in,
    prog_varset::in, inst_varset::in, bool::in, int::in, maybe(mer_type)::in,
    maybe_vartypes::in, io::di, io::uo) is det.

write_unify_rhs_3(rhs_var(Var), _, VarSet, _, AppendVarNums, _, _, _, !IO) :-
    mercury_output_var(VarSet, AppendVarNums, Var, !IO).
write_unify_rhs_3(rhs_functor(ConsId0, IsExistConstruct, ArgVars), ModuleInfo,
        VarSet, _, AppendVarNums, _Indent, MaybeType, TypeQual, !IO) :-
    (
        IsExistConstruct = yes,
        ConsId0 = cons(SymName0, Arity)
    ->
        remove_new_prefix(SymName, SymName0),
        ConsId = cons(SymName, Arity)
    ;
        ConsId = ConsId0
    ),
    write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo, AppendVarNums,
        !IO),
    (
        MaybeType = yes(Type),
        TypeQual = varset_vartypes(TVarSet, _)
    ->
        io.write_string(" : ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        true
    ).
write_unify_rhs_3(rhs_lambda_goal(Purity, PredOrFunc, _EvalMethod, NonLocals,
        Vars, Modes, Det, Goal), ModuleInfo, VarSet, InstVarSet,
        AppendVarNums, Indent, MaybeType, TypeQual, !IO) :-
    Indent1 = Indent + 1,
    write_purity_prefix(Purity, !IO),
    (
        PredOrFunc = predicate,
        io.write_string("(", !IO),
        (
            Vars = [],
            io.write_string("(pred)", !IO)
        ;
            Vars = [_ | _],
            io.write_string("pred(", !IO),
            write_var_modes(Vars, Modes, VarSet, InstVarSet, AppendVarNums,
                !IO),
            io.write_string(")", !IO)
        ),
        io.write_string(" is ", !IO),
        mercury_output_det(Det, !IO),
        io.write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
            TypeQual, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        pred_args_to_func_args(Vars, ArgVars, RetVar),
        io.write_string("(", !IO),
        (
            ArgVars = [],
            io.write_string("(func)", !IO)
        ;
            ArgVars = [_ | _],
            io.write_string("func(", !IO),
            write_var_modes(ArgVars, ArgModes, VarSet, InstVarSet,
                AppendVarNums, !IO),
            io.write_string(")", !IO)
        ),
        io.write_string(" = (", !IO),
        write_var_mode(VarSet, InstVarSet, AppendVarNums,
            RetVar - RetMode, !IO),
        io.write_string(") is ", !IO),
        mercury_output_det(Det, !IO),
        io.write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
            TypeQual, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO)
    ),
    (
        MaybeType = yes(Type),
        TypeQual = varset_vartypes(TVarSet, _)
    ->
        io.write_string(" : ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        true
    ),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'n') ->
        (
            NonLocals = [_ | _],
            write_indent(Indent1, !IO),
            io.write_string("% lambda nonlocals: ", !IO),
            mercury_output_vars(VarSet, AppendVarNums, NonLocals, !IO)
        ;
            NonLocals = []
        )
    ;
        true
    ).

unify_rhs_to_string(rhs_var(Var), _ModuleInfo, VarSet, AppendVarNums)
    = mercury_var_to_string(VarSet, AppendVarNums, Var).
unify_rhs_to_string(rhs_functor(ConsId0, IsExistConstruct, ArgVars),
        ModuleInfo, VarSet, AppendVarNums) = Str :-
    (
        IsExistConstruct = yes,
        ConsId0 = cons(SymName0, Arity)
    ->
        remove_new_prefix(SymName, SymName0),
        ConsId = cons(SymName, Arity)
    ;
        ConsId = ConsId0
    ),
    Str = functor_cons_id_to_string(ConsId, ArgVars, VarSet, ModuleInfo,
        AppendVarNums).
unify_rhs_to_string(rhs_lambda_goal(_, _, _, _, _, _, _, _), _, _, _)
    = "lambda goal".

:- pred write_sym_name_and_args(sym_name::in, list(prog_var)::in,
    prog_varset::in, bool::in, io::di, io::uo) is det.

write_sym_name_and_args(PredName, ArgVars, VarSet, AppendVarNums, !IO) :-
    (
        PredName = qualified(ModuleName, Name),
        write_qualified_functor(ModuleName, term.atom(Name), ArgVars, VarSet,
            AppendVarNums, !IO)
    ;
        PredName = unqualified(Name),
        write_functor_maybe_needs_quotes(term.atom(Name), ArgVars, VarSet,
            AppendVarNums, next_to_graphic_token, !IO)
    ).

write_functor(Functor, ArgVars, VarSet, AppendVarNums, !IO) :-
    write_functor_maybe_needs_quotes(Functor, ArgVars, VarSet, AppendVarNums,
        not_next_to_graphic_token, !IO).

functor_to_string(Functor, ArgVars, VarSet, AppendVarNums) =
    functor_to_string_maybe_needs_quotes(Functor, ArgVars, VarSet,
        AppendVarNums, not_next_to_graphic_token).

:- pred write_functor_maybe_needs_quotes(const::in, list(prog_var)::in,
    prog_varset::in, bool::in, needs_quotes::in, io::di, io::uo) is det.

write_functor_maybe_needs_quotes(Functor, ArgVars, VarSet, AppendVarNums,
        NextToGraphicToken, !IO) :-
    io.write_string(functor_to_string_maybe_needs_quotes(Functor, ArgVars,
        VarSet, AppendVarNums, NextToGraphicToken), !IO).

:- func functor_to_string_maybe_needs_quotes(const, list(prog_var),
    prog_varset, bool, needs_quotes) = string.

functor_to_string_maybe_needs_quotes(Functor, ArgVars, VarSet, AppendVarNums,
        NextToGraphicToken) = Str :-
    term.context_init(Context),
    term.var_list_to_term_list(ArgVars, ArgTerms),
    Term = term.functor(Functor, ArgTerms, Context),
    Str = mercury_term_nq_to_string(VarSet, AppendVarNums, NextToGraphicToken,
        Term).

:- pred write_qualified_functor(module_name::in, const::in,
    list(prog_var)::in, prog_varset::in, bool::in, io::di, io::uo) is det.

write_qualified_functor(ModuleName, Functor, ArgVars, VarSet, AppendVarNums,
        !IO) :-
    io.write_string(qualified_functor_to_string(ModuleName, Functor,
        ArgVars, VarSet, AppendVarNums), !IO).

:- func qualified_functor_to_string(module_name, const,
    list(prog_var), prog_varset, bool) = string.

qualified_functor_to_string(ModuleName, Functor, ArgVars, VarSet,
        AppendVarNums) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    FunctorStr = functor_to_string_maybe_needs_quotes(Functor, ArgVars, VarSet,
        AppendVarNums, next_to_graphic_token),
    Str = ModuleNameStr ++ "." ++ FunctorStr.

:- pred write_qualified_functor_with_term_args(module_name::in, const::in,
    list(prog_term)::in, prog_varset::in, bool::in, io::di, io::uo) is det.

write_qualified_functor_with_term_args(ModuleName, Functor, ArgTerms, VarSet,
        AppendVarNums, !IO) :-
    io.write_string(qualified_functor_with_term_args_to_string(ModuleName,
        Functor, ArgTerms, VarSet, AppendVarNums), !IO).

:- func qualified_functor_with_term_args_to_string(module_name,
    const, list(prog_term), prog_varset, bool) = string.

qualified_functor_with_term_args_to_string(ModuleName, Functor, ArgTerms,
        VarSet, AppendVarNums) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    term.context_init(Context),
    Term = term.functor(Functor, ArgTerms, Context),
    TermStr = mercury_term_nq_to_string(VarSet, AppendVarNums,
        next_to_graphic_token, Term),
    Str = ModuleNameStr ++ "." ++ TermStr.

write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo, AppendVarNums,
        !IO) :-
    io.write_string(functor_cons_id_to_string(ConsId, ArgVars,
        VarSet, ModuleInfo, AppendVarNums), !IO).

functor_cons_id_to_string(ConsId, ArgVars, VarSet, ModuleInfo, AppendVarNums)
        = Str :-
    (
        ConsId = cons(SymName, _),
        (
            SymName = qualified(Module, Name),
            Str = qualified_functor_to_string(Module, term.atom(Name),
                ArgVars, VarSet, AppendVarNums)
        ;
            SymName = unqualified(Name),
            Str = functor_to_string_maybe_needs_quotes(term.atom(Name),
                ArgVars, VarSet, AppendVarNums, next_to_graphic_token)
        )
    ;
        ConsId = int_const(Int),
        Str = functor_to_string(term.integer(Int), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = float_const(Float),
        Str = functor_to_string(term.float(Float), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = string_const(String),
        Str = functor_to_string(term.string(String), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = pred_const(ShroudedPredProcId, _),
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        Str = functor_cons_id_to_string(cons(qualified(PredModule, PredName),
            list.length(ArgVars)), ArgVars, VarSet, ModuleInfo, AppendVarNums)
    ;
        ConsId = type_ctor_info_const(Module, Name, Arity),
        Str = "type_ctor_info("""
            ++ prog_out.sym_name_to_escaped_string(Module)
            ++ """, """ ++ Name ++ """, " ++ int_to_string(Arity) ++ ")"
    ;
        ConsId = base_typeclass_info_const(Module, class_id(Name, Arity), _,
            Instance),
        Str = "base_typeclass_info("""
            ++ prog_out.sym_name_to_escaped_string(Module) ++ """, "
            ++ "class_id(" ++ prog_out.sym_name_to_escaped_string(Name)
            ++ ", " ++ int_to_string(Arity) ++ "), " ++ Instance ++ ")"
    ;
        ConsId = type_info_cell_constructor(_),
        Str = functor_to_string_maybe_needs_quotes(
            term.atom("type_info_cell_constructor"),
            ArgVars, VarSet, AppendVarNums, next_to_graphic_token)
    ;
        ConsId = typeclass_info_cell_constructor,
        Str = functor_to_string_maybe_needs_quotes(
            term.atom("typeclass_info_cell_constructor"),
            ArgVars, VarSet, AppendVarNums, next_to_graphic_token)
    ;
        ConsId = tabling_info_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "tabling_info_const("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ ", " ++ int_to_string(ProcIdInt) ++ ")"
    ;
        ConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "deep_profiling_proc_layout("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ " (mode " ++ int_to_string(ProcIdInt) ++ "))"
    ;
        ConsId = table_io_decl(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "table_io_decl("
            ++ pred_id_to_string(ModuleInfo, PredId)
            ++ " (mode " ++ int_to_string(ProcIdInt) ++ "))"
    ).

write_var_modes(Vars, Modes, VarSet, InstVarSet, AppendVarNums, !IO) :-
    io.write_string(var_modes_to_string(Vars, Modes, VarSet,
        InstVarSet, AppendVarNums), !IO).

var_modes_to_string(Vars, Modes, VarSet, InstVarSet, AppendVarNums) = Str :-
    assoc_list.from_corresponding_lists(Vars, Modes, VarModes),
    Strs = list.map(var_mode_to_string(VarSet, InstVarSet, AppendVarNums),
        VarModes),
    Str = string.join_list(", ", Strs).

:- pred write_var_mode(prog_varset::in, inst_varset::in, bool::in,
    pair(prog_var, mer_mode)::in, io::di, io::uo) is det.

write_var_mode(VarSet, InstVarSet, AppendVarNums, Var - Mode, !IO) :-
    io.write_string(var_mode_to_string(VarSet, InstVarSet, AppendVarNums,
        Var - Mode), !IO).

:- func var_mode_to_string(prog_varset, inst_varset, bool,
    pair(prog_var, mer_mode)) = string.

var_mode_to_string(VarSet, InstVarSet, AppendVarNums, Var - Mode) =
    mercury_var_to_string(VarSet, AppendVarNums, Var)
        ++ "::" ++ mercury_mode_to_string(Mode, InstVarSet).

:- pred write_conj(hlds_goal::in, list(hlds_goal)::in,
    module_info::in, prog_varset::in, bool::in, int::in, string::in,
    string::in, string::in, maybe_vartypes::in, io::di, io::uo) is det.

write_conj(Goal1, Goals1, ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        Verbose, Separator, TypeQual, !IO) :-
    (
        Goals1 = [Goal2 | Goals2],
        ( Verbose \= "" ->
            % When generating verbose dumps, we want the comma on its own line,
            % since that way it visually separates the lines after one goal
            % and the lines before the next.
            write_goal_a(Goal1, ModuleInfo, VarSet, AppendVarNums, Indent,
                "\n", TypeQual, !IO),
            write_indent(Indent, !IO),
            io.write_string(Separator, !IO)
        ;
            write_goal_a(Goal1, ModuleInfo, VarSet, AppendVarNums, Indent,
                Separator, TypeQual, !IO)
        ),
        write_conj(Goal2, Goals2, ModuleInfo, VarSet, AppendVarNums,
            Indent, Follow, Verbose, Separator, TypeQual, !IO)
    ;
        Goals1 = [],
        write_goal_a(Goal1, ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
            TypeQual, !IO)
    ).

write_goal_list(GoalList, ModuleInfo, VarSet, AppendVarNums, Indent, Separator,
        TypeQual, !IO) :-
    (
        GoalList = [Goal | Goals],
        write_indent(Indent, !IO),
        io.write_string(Separator, !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
            TypeQual, !IO),
        write_goal_list(Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
            Separator, TypeQual, !IO)
    ;
        GoalList = []
    ).

:- pred write_case(case::in, prog_var::in, module_info::in, prog_varset::in,
    bool::in, int::in, maybe_vartypes::in, io::di, io::uo) is det.

write_case(case(ConsId, Goal), Var, ModuleInfo, VarSet, AppendVarNums, Indent,
        VarTypes, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" has functor ", !IO),
    write_cons_id(ConsId, !IO),
    io.write_string("\n", !IO),
    % XXX if the output of this is to be used, e.g. in
    % inter-module optimization, output a unification to bind the
    % Var to the functor, since simplify.m and unused_args.m remove
    % the unification. At the moment this is not a problem, since
    % intermod.m works on the unoptimized clauses.
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, "\n",
        VarTypes, !IO).

:- pred write_cases(list(case)::in, prog_var::in, module_info::in,
    prog_varset::in, bool::in, int::in, maybe_vartypes::in, io::di, io::uo)
    is det.

write_cases(CasesList, Var, ModuleInfo, VarSet, AppendVarNums, Indent,
        VarTypes, !IO) :-
    (
        CasesList = [Case | Cases],
        write_indent(Indent, !IO),
        io.write_string(";\n", !IO),
        write_case(Case, Var, ModuleInfo, VarSet, AppendVarNums, Indent + 1,
            VarTypes, !IO),
        write_cases(Cases, Var, ModuleInfo, VarSet, AppendVarNums, Indent,
            VarTypes, !IO)
    ;
        CasesList = []
    ).

:- pred write_some(list(prog_var)::in, prog_varset::in, io::di, io::uo) is det.

    % Quantification is all implicit by the time we get to the HLDS.
write_some(_Vars, _VarSet, !IO).

write_instmap(InstMap, VarSet, AppendVarNums, Indent, !IO) :-
    ( instmap.is_unreachable(InstMap) ->
        io.write_string("unreachable", !IO)
    ;
        instmap.to_assoc_list(InstMap, AssocList),
        write_instmap_2(AssocList, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_2(assoc_list(prog_var, mer_inst)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_instmap_2([], _, _, _, !IO).
write_instmap_2([Var - Inst | Rest], VarSet, AppendVarNums, Indent, !IO) :-
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" -> ", !IO),
    varset.init(InstVarSet),
    mercury_output_inst(Inst, InstVarSet, !IO),
    (
        Rest = []
    ;
        Rest = [_ | _],
        mercury_output_newline(Indent, !IO),
        io.write_string("%            ", !IO),
        write_instmap_2(Rest, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_delta(instmap_delta::in, prog_varset::in, bool::in,
    int::in, io::di, io::uo) is det.

write_instmap_delta(InstMapDelta, VarSet, AppendVarNums, Indent, !IO) :-
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        io.write_string("unreachable", !IO)
    ;
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        write_instmap_2(AssocList, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_delta_vars(instmap_delta::in, prog_varset::in,
    bool::in, io::di, io::uo) is det.

write_instmap_delta_vars(InstMapDelta, VarSet, AppendVarNums, !IO) :-
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        io.write_string("unreachable", !IO)
    ;
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        assoc_list.keys(AssocList, Vars),
        write_vars(VarSet, AppendVarNums, Vars, !IO)
    ).

write_import_status(Status, !IO) :-
    io.write_string(import_status_to_string(Status), !IO).

import_status_to_string(status_local) =
    "local".
import_status_to_string(status_exported) =
    "exported".
import_status_to_string(status_opt_exported) =
    "opt_exported".
import_status_to_string(status_abstract_exported) =
    "abstract_exported".
import_status_to_string(status_pseudo_exported) =
    "pseudo_exported".
import_status_to_string(status_imported(import_locn_interface)) =
    "imported in the interface".
import_status_to_string(status_imported(import_locn_implementation)) =
    "imported in the implementation".
import_status_to_string(status_imported(
        import_locn_ancestor_private_interface)) =
    "imported from an ancestor's private interface".
import_status_to_string(status_imported(import_locn_ancestor)) =
    "imported by an ancestor".
import_status_to_string(status_external(Status)) =
    "external (and " ++ import_status_to_string(Status) ++ ")".
import_status_to_string(status_abstract_imported) =
    "abstract_imported".
import_status_to_string(status_opt_imported) =
    "opt_imported".
import_status_to_string(status_pseudo_imported) =
    "pseudo_imported".
import_status_to_string(status_exported_to_submodules) =
    "exported_to_submodules".

:- pred write_type_list(list(mer_type)::in, tvarset::in, bool::in,
    io::di, io::uo) is det.

write_type_list(Types, TypeVarSet, AppendVarNums, !IO) :-
    list.foldl(output_type_and_comma(TypeVarSet, AppendVarNums), Types, !IO).

:- pred output_type_and_comma(tvarset::in, bool::in, mer_type::in,
    io::di, io::uo) is det.

output_type_and_comma(TypeVarSet, AppendVarNums, Type, !IO) :-
    mercury_output_type(TypeVarSet, AppendVarNums, Type, !IO),
    io.write_string(", ", !IO).

:- pred write_var_types(int::in, prog_varset::in, bool::in,
    vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% variable types map:\n", !IO),
    map.keys(VarTypes, Vars),
    write_var_types_2(Vars, Indent, VarSet, AppendVarNums, VarTypes, TVarSet,
        !IO).

:- pred write_var_types_2(list(prog_var)::in, int::in, prog_varset::in,
    bool::in, vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types_2([], _, _, _, _, _, !IO).
write_var_types_2([Var | Vars], Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO) :-
    map.lookup(VarTypes, Var, Type),
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(")", !IO),
    io.write_string(": ", !IO),
    mercury_output_type(TypeVarSet, AppendVarNums, Type, !IO),
    io.write_string("\n", !IO),
    write_var_types_2(Vars, Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO).

:- pred write_rtti_varmaps(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, io::di, io::uo) is det.

write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% type_info varmap:\n", !IO),
    rtti_varmaps_tvars(RttiVarMaps, TypeVars),
    list.foldl(write_type_info_locn(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), TypeVars, !IO),
    write_indent(Indent, !IO),
    io.write_string("% typeclass_info varmap:\n", !IO),
    rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
    list.foldl(write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), Constraints, !IO),
    write_indent(Indent, !IO),
    io.write_string("% rtti_var_info:\n", !IO),
    rtti_varmaps_rtti_prog_vars(RttiVarMaps, ProgVars),
    list.foldl(write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), ProgVars, !IO).

:- pred write_type_info_locn(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, tvar::in, io::di, io::uo) is det.

write_type_info_locn(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, TVar,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),

    mercury_output_var(TVarSet, AppendVarNums, TVar, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(TVar, TVarNum),
    io.write_int(TVarNum, !IO),
    io.write_string(")", !IO),

    io.write_string(" -> ", !IO),
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    (
        Locn = type_info(Var),
        io.write_string("type_info(", !IO),
        mercury_output_var(VarSet, AppendVarNums, Var, !IO),
        io.write_string(") ", !IO)
    ;
        Locn = typeclass_info(Var, Index),
        io.write_string("typeclass_info(", !IO),
        mercury_output_var(VarSet, AppendVarNums, Var, !IO),
        io.write_string(", ", !IO),
        io.write_int(Index, !IO),
        io.write_string(") ", !IO)
    ),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(")", !IO),
    io.write_string("\n", !IO).

:- pred write_typeclass_info_var(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_constraint::in, io::di, io::uo) is det.

write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
        Constraint, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO),
    io.write_string(" -> ", !IO),
    rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.nl(!IO).

:- pred write_rtti_var_info(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_var::in, io::di, io::uo) is det.

write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, Var,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(") ", !IO),
    io.write_string(" -> ", !IO),
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        io.write_string("type_info for ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        VarInfo = typeclass_info_var(Constraint),
        io.write_string("typeclass_info for ", !IO),
        mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO)
    ;
        VarInfo = non_rtti_var,
        unexpected(this_file, "write_rtti_var_info: non rtti var")
    ),
    io.nl(!IO).

:- pred write_stack_slots(int::in, stack_slots::in, prog_varset::in,
    bool::in, io::di, io::uo) is det.

write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO) :-
    map.to_assoc_list(StackSlots, VarSlotList0),
    VarSlotList = assoc_list.map_values(key_stack_slot_to_abs_locn,
        VarSlotList0),
    write_var_to_abs_locns(VarSlotList, VarSet, AppendVarNums, Indent, !IO).

:- pred write_var_to_abs_locns(assoc_list(prog_var, abs_locn)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_var_to_abs_locns([], _, _, _, !IO).
write_var_to_abs_locns([Var - Loc | VarLocs], VarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%\t", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string("\t-> ", !IO),
    io.write_string(abs_locn_to_string(Loc), !IO),
    io.write_string("\n", !IO),
    write_var_to_abs_locns(VarLocs, VarSet, AppendVarNums, Indent, !IO).

:- pred write_untuple_info(untuple_proc_info::in, prog_varset::in,
    bool::in, int::in, io::di, io::uo) is det.

write_untuple_info(untuple_proc_info(UntupleMap), VarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% untuple:\n", !IO),
    map.foldl(write_untuple_info_2(VarSet, AppendVarNums, Indent), UntupleMap,
        !IO).

:- pred write_untuple_info_2(prog_varset::in, bool::in, int::in,
    prog_var::in, prog_vars::in, io::di, io::uo) is det.

write_untuple_info_2(VarSet, AppendVarNums, Indent, OldVar, NewVars, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%\t", !IO),
    mercury_output_var(VarSet, AppendVarNums, OldVar, !IO),
    io.write_string("\t-> ", !IO),
    mercury_output_vars(VarSet, AppendVarNums, NewVars, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_types(int::in, type_table::in, io::di, io::uo) is det.

write_types(Indent, TypeTable, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%-------- Types --------\n", !IO),
    map.to_assoc_list(TypeTable, TypeAssocList),
    write_types_2(Indent, TypeAssocList, !IO).

:- pred write_types_2(int::in, assoc_list(type_ctor, hlds_type_defn)::in,
    io::di, io::uo) is det.

write_types_2(_Indent, [], !IO).
write_types_2(Indent, [TypeCtor - TypeDefn | Types], !IO) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, Status),
    hlds_data.get_type_defn_context(TypeDefn, Context),

    % Write the context.
    io.write_char('\n', !IO),
    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'c') ->
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        ( FileName \= "" ->
            write_indent(Indent, !IO),
            io.write_string("% context: file `", !IO),
            io.write_string(FileName, !IO),
            io.write_string("', line ", !IO),
            io.write_int(LineNumber, !IO),
            io.write_string(", status ", !IO),
            write_import_status(Status, !IO),
            io.write_char('\n', !IO)
        ;
            true
        )
    ;
        true
    ),

    write_indent(Indent, !IO),
    (
        ( TypeBody = hlds_solver_type(_, _)
        ; TypeBody = hlds_abstract_type(solver_type)
        )
    ->
        io.write_string(":- solver type ", !IO)
    ;
        io.write_string(":- type ", !IO)
    ),
    write_type_name(TypeCtor, !IO),
    write_type_params(TVarSet, TypeParams, !IO),
    write_type_body(Indent + 1, TVarSet, TypeBody, !IO),
    write_types_2(Indent, Types, !IO).

:- pred write_type_name(type_ctor::in, io::di, io::uo) is det.

write_type_name(type_ctor(Name, _Arity), !IO) :-
    prog_out.write_sym_name(Name, !IO).

:- func type_name_to_string(type_ctor) = string.

type_name_to_string(type_ctor(Name, _Arity)) =
    prog_out.sym_name_to_escaped_string(Name).

:- pred write_type_params(tvarset::in, list(type_param)::in,
    io::di, io::uo) is det.

write_type_params(_TVarSet, [], !IO).
write_type_params(TVarSet, [P], !IO) :-
    io.write_string("(", !IO),
    mercury_output_var(TVarSet, no, P, !IO),
    io.write_string(")", !IO).
write_type_params(TVarSet, [P | Ps], !IO) :-
    Ps = [_ | _],
    io.write_string("(", !IO),
    mercury_output_var(TVarSet, no, P, !IO),
    write_type_params_2(TVarSet, Ps, !IO).

:- pred write_type_params_2(tvarset::in, list(type_param)::in,
    io::di, io::uo) is det.

write_type_params_2(_TVarSet, [], !IO) :-
    io.write_string(")", !IO).
write_type_params_2(TVarSet, [P | Ps], !IO) :-
    io.write_string(", ", !IO),
    mercury_output_var(TVarSet, no, P, !IO),
    write_type_params_2(TVarSet, Ps, !IO).

:- pred write_type_body(int::in, tvarset::in, hlds_type_body::in,
    io::di, io::uo) is det.

write_type_body(Indent, TVarSet, hlds_du_type(Ctors, Tags, EnumDummy,
        MaybeUserEqComp, ReservedTag, Foreign), !IO) :-
    io.write_string(" --->\n", !IO),
    (
        EnumDummy = is_enum,
        write_indent(Indent, !IO),
        io.write_string("/* enumeration */\n", !IO)
    ;
        EnumDummy = is_dummy,
        write_indent(Indent, !IO),
        io.write_string("/* dummy */\n", !IO)
    ;
        EnumDummy = not_enum_or_dummy
    ),
    (
        ReservedTag = yes,
        write_indent(Indent, !IO),
        io.write_string("/* reserved_tag */\n", !IO)
    ;
        ReservedTag = no
    ),
    write_constructors(Indent, TVarSet, Ctors, Tags, !IO),
    mercury_output_where_attributes(TVarSet, no, MaybeUserEqComp, !IO),
    (
        Foreign = yes(_),
        write_indent(Indent, !IO),
        io.write_string("/* has foreign_type */\n", !IO)
    ;
        Foreign = no
    ),
    io.write_string(".\n", !IO).

write_type_body(_Indent, TVarSet, hlds_eqv_type(Type), !IO) :-
    io.write_string(" == ", !IO),
    mercury_output_type(TVarSet, no, Type, !IO),
    io.write_string(".\n", !IO).

write_type_body(_Indent, _TVarSet, hlds_abstract_type(_IsSolverType), !IO) :-
    io.write_string(".\n", !IO).

write_type_body(_Indent, _TVarSet, hlds_foreign_type(_), !IO) :-
    % XXX
    io.write_string(" == $foreign_type.\n", !IO).

write_type_body(_Indent, TVarSet,
        hlds_solver_type(SolverTypeDetails, MaybeUserEqComp), !IO) :-
    mercury_output_where_attributes(TVarSet, yes(SolverTypeDetails),
        MaybeUserEqComp, !IO),
    io.write_string(".\n", !IO).

:- pred write_constructors(int::in, tvarset::in,
    list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

write_constructors(_Indent, _TVarSet, [], _, !IO) :-
    unexpected(this_file, "write_constructors: empty constructor list?").
write_constructors(Indent, TVarSet, [C], TagValues, !IO) :-
    write_indent(Indent, !IO),
    io.write_char('\t', !IO),
    write_ctor(C, TVarSet, TagValues, !IO).
write_constructors(Indent, TVarSet, [C | Cs], TagValues, !IO) :-
    Cs = [_ | _],
    write_indent(Indent, !IO),
    io.write_char('\t', !IO),
    write_ctor(C, TVarSet, TagValues, !IO),
    io.write_string("\n", !IO),
    write_constructors_2(Indent, TVarSet, Cs, TagValues, !IO).

:- pred write_constructors_2(int::in, tvarset::in,
    list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

write_constructors_2(_Indent, _TVarSet, [], _, !IO).
write_constructors_2(Indent, TVarSet, [C | Cs], TagValues, !IO) :-
    write_indent(Indent, !IO),
    io.write_string(";\t", !IO),
    write_ctor(C, TVarSet, TagValues, !IO),
    (
        Cs = []
    ;
        Cs = [_ | _],
        io.write_string("\n", !IO),
        write_constructors_2(Indent, TVarSet, Cs, TagValues, !IO)
    ).

:- pred write_ctor(constructor::in, tvarset::in, cons_tag_values::in,
    io::di, io::uo) is det.

write_ctor(C, TVarSet, TagValues, !IO) :-
    mercury_output_ctor(C, TVarSet, !IO),
    C = ctor(_, _, Name, Args, _),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    ( map.search(TagValues, ConsId, TagValue) ->
        io.write_string("\t% tag: ", !IO),
        io.print(TagValue, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred write_classes(int::in, class_table::in, io::di, io::uo) is det.

write_classes(Indent, ClassTable, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%-------- Classes --------\n", !IO),
    map.to_assoc_list(ClassTable, ClassTableList),
    io.write_list(ClassTableList, "\n", write_class_defn(Indent), !IO),
    io.nl(!IO).

:- pred write_class_defn(int::in,
    pair(class_id, hlds_class_defn)::in, io::di, io::uo) is det.

write_class_defn(Indent, ClassId - ClassDefn, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),

    write_class_id(ClassId, !IO),
    io.write_string(":\n", !IO),

    ClassDefn = hlds_class_defn(_, Constraints, FunDeps, _, Vars, _, _,
        Interface, VarSet, Context),

    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    ( FileName \= "" ->
        write_indent(Indent, !IO),
        io.write_string("% context: file `", !IO),
        io.write_string(FileName, !IO),
        io.write_string("', line ", !IO),
        io.write_int(LineNumber, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),

    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),

    write_indent(Indent, !IO),
    io.write_string("% Vars: ", !IO),
    mercury_output_vars(VarSet, AppendVarNums, Vars, !IO),
    io.nl(!IO),

    write_indent(Indent, !IO),
    io.write_string("% Functional dependencies: ", !IO),
    io.write_list(FunDeps, ", ", hlds_output_fundep, !IO),
    io.nl(!IO),

    write_indent(Indent, !IO),
    io.write_string("% Constraints: ", !IO),
    io.write_list(Constraints, ", ",
        mercury_output_constraint(VarSet, AppendVarNums), !IO),
    io.nl(!IO),

    write_indent(Indent, !IO),
    io.write_string("% Class Methods: ", !IO),
    io.write_list(Interface, ", ", write_class_proc, !IO),
    io.nl(!IO).

:- pred hlds_output_fundep(hlds_class_fundep::in, io::di, io::uo) is det.

hlds_output_fundep(fundep(Domain, Range), !IO) :-
    io.write_char('(', !IO),
    DomainList = set.to_sorted_list(Domain),
    io.write_list(DomainList, ", ", io.write_int, !IO),
    io.write_string(" -> ", !IO),
    RangeList = set.to_sorted_list(Range),
    io.write_list(RangeList, ", ", io.write_int, !IO),
    io.write_char(')', !IO).

    % Just output the class methods as pred_ids and proc_ids because it is
    % probably not that useful to have the names. If that information is
    % needed, it shouldn't be a very difficult fix.
    %
:- pred write_class_proc(hlds_class_proc::in, io::di, io::uo) is det.

write_class_proc(hlds_class_proc(PredId, ProcId), !IO) :-
    io.write_string("hlds_class_proc(pred_id:", !IO),
    pred_id_to_int(PredId, PredInt),
    io.write_int(PredInt, !IO),
    io.write_string(", proc_id:", !IO),
    proc_id_to_int(ProcId, ProcInt),
    io.write_int(ProcInt, !IO),
    io.write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- pred write_instances(int::in, instance_table::in, io::di, io::uo) is det.

write_instances(Indent, InstanceTable, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%-------- Instances --------\n", !IO),
    map.to_assoc_list(InstanceTable, InstanceTableList),
    io.write_list(InstanceTableList, "\n\n",
        write_instance_defns(Indent), !IO),
    io.nl(!IO).

:- pred write_instance_defns(int::in,
    pair(class_id, list(hlds_instance_defn))::in, io::di, io::uo) is det.

write_instance_defns(Indent, ClassId - InstanceDefns, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    write_class_id(ClassId, !IO),
    io.write_string(":\n", !IO),
    io.write_list(InstanceDefns, "\n", write_instance_defn(Indent), !IO).

:- pred write_instance_defn(int::in, hlds_instance_defn::in,
    io::di, io::uo) is det.

write_instance_defn(Indent, InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, _Status,
        Context, Constraints, Types, Body, MaybePredProcIds, VarSet, Proofs),

    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    ( FileName \= "" ->
        write_indent(Indent, !IO),
        io.write_string("% context: file `", !IO),
        io.write_string(FileName, !IO),
        io.write_string("', line ", !IO),
        io.write_int(LineNumber, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),

    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),

    % Curry the varset for term_io.write_variable/4.
    PrintTerm = (pred(TypeName::in, IO0::di, IO::uo) is det :-
        mercury_output_type(VarSet, AppendVarNums, TypeName, IO0, IO)
    ),
    write_indent(Indent, !IO),
    io.write_string("% Types: ", !IO),
    io.write_list(Types, ", ", PrintTerm, !IO),
    io.nl(!IO),

    write_indent(Indent, !IO),
    io.write_string("% Constraints: ", !IO),
    io.write_list(Constraints, ", ",
        mercury_output_constraint(VarSet, AppendVarNums), !IO),
    io.nl(!IO),

    write_indent(Indent, !IO),
    (
        Body = instance_body_abstract,
        io.write_string("% abstract", !IO)
    ;
        Body = instance_body_concrete(Methods),
        io.write_string("% Instance Methods: ", !IO),
        mercury_output_instance_methods(Methods, !IO)
    ),
    io.nl(!IO),

    (
        MaybePredProcIds = yes(PredProcIds),
        write_indent(Indent, !IO),
        io.write_string("% procedures: ", !IO),
        io.write(PredProcIds, !IO),
        io.nl(!IO)
    ;
        MaybePredProcIds = no
    ),
    write_constraint_proofs(Indent, VarSet, Proofs, AppendVarNums,
        !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_insts(int::in, inst_table::in, io::di, io::uo) is det.

write_insts(Indent, _InstTable, !IO) :-
    % XXX fix this up.
    write_indent(Indent, !IO),
    io.write_string("%-------- Insts --------\n", !IO),
    write_indent(Indent, !IO),
    io.write_string("%%% Not yet implemented, sorry.\n", !IO).
    % io.write_string("% ", !IO).
    % io.print(InstTable, !IO),
    % io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_modes(int::in, mode_table::in, io::di, io::uo) is det.

write_modes(Indent, _ModeTable, !IO) :-
    % XXX fix this up.
    write_indent(Indent, !IO),
    io.write_string("%-------- Modes --------\n", !IO),
    write_indent(Indent, !IO),
    io.write_string("%%% Not yet implemented, sorry.\n", !IO).
    % io.write_string("% ", !IO),
    % io.print(ModeTable, !IO),
    % io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_procs(int::in, bool::in, module_info::in, pred_id::in,
    import_status::in, pred_info::in, io::di, io::uo) is det.

write_procs(Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus, PredInfo,
        !IO) :-
    pred_info_get_procedures(PredInfo, ProcTable),
    ProcIds = pred_info_procids(PredInfo),
    write_procs_2(ProcIds, AppendVarNums, ModuleInfo, Indent, PredId,
        ImportStatus, ProcTable, !IO).

:- pred write_procs_2(list(proc_id)::in, bool::in, module_info::in, int::in,
    pred_id::in, import_status::in, proc_table::in, io::di, io::uo) is det.

write_procs_2([], _, _, _, _, _, _, !IO).
write_procs_2([ProcId | ProcIds], AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    write_proc(Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, ProcInfo, !IO),
    write_procs_2(ProcIds, AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO).

:- pred write_proc(int::in, bool::in, module_info::in, pred_id::in,
    proc_id::in, import_status::in, proc_info::in, io::di, io::uo) is det.

write_proc(Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, Proc, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_vartypes(Proc, VarTypes),
    proc_info_get_declared_determinism(Proc, DeclaredDeterminism),
    proc_info_get_inferred_determinism(Proc, InferredDeterminism),
    proc_info_get_varset(Proc, VarSet),
    proc_info_get_headvars(Proc, HeadVars),
    proc_info_get_argmodes(Proc, HeadModes),
    proc_info_get_maybe_arglives(Proc, MaybeArgLives),
    proc_info_maybe_arg_info(Proc, MaybeArgInfos),
    proc_info_get_goal(Proc, Goal),
    proc_info_get_context(Proc, ModeContext),
    proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize),
    proc_info_get_maybe_termination_info(Proc, MaybeTermination),
    proc_info_get_structure_sharing(Proc, MaybeStructureSharing), 
    proc_info_get_rtti_varmaps(Proc, RttiVarMaps),
    proc_info_get_eval_method(Proc, EvalMethod),
    proc_info_get_is_address_taken(Proc, IsAddressTaken),
    proc_info_get_has_parallel_conj(Proc, HasParallelConj),
    proc_info_get_has_user_event(Proc, HasUserEvent),
    proc_info_get_call_table_tip(Proc, MaybeCallTableTip),
    proc_info_get_maybe_deep_profile_info(Proc, MaybeDeepProfileInfo),
    proc_info_get_maybe_untuple_info(Proc, MaybeUntupleInfo),
    Indent1 = Indent + 1,

    write_indent(Indent1, !IO),
    io.write_string("% pred id ", !IO),
    pred_id_to_int(PredId, PredInt),
    io.write_int(PredInt, !IO),
    io.nl(!IO),
    write_indent(Indent1, !IO),
    io.write_string("% mode number ", !IO),
    proc_id_to_int(ProcId, ProcInt),
    io.write_int(ProcInt, !IO),
    io.write_string(" of ", !IO),
    write_pred_id(ModuleInfo, PredId, !IO),
    io.write_string(" (", !IO),
    write_determinism(InferredDeterminism, !IO),
    io.write_string("):\n", !IO),

    globals.io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string.contains_char(Verbose, 't') ->
        write_indent(Indent, !IO),
        io.write_string("% Arg size properties: ", !IO),
        write_maybe_arg_size_info(MaybeArgSize, yes, !IO),
        io.nl(!IO),
        write_indent(Indent, !IO),
        io.write_string("% Termination properties: ", !IO),
        write_maybe_termination_info(MaybeTermination, yes, !IO),
        io.nl(!IO)
    ;
        true
    ),

        % Dump structure sharing information.
    ( string.contains_char(Verbose, 'S') ->
        write_indent(Indent, !IO),
        io.write_string("% Structure sharing: \n", !IO),
        dump_maybe_structure_sharing_domain(VarSet, TVarSet, 
            MaybeStructureSharing, !IO)
    ;
        true
    ),

    write_indent(Indent, !IO),
    write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),
    write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
        !IO),

    (
        IsAddressTaken = address_is_taken,
        io.write_string("% address is taken\n", !IO)
    ;
        IsAddressTaken = address_is_not_taken,
        io.write_string("% address is not taken\n", !IO)
    ),

    (
        HasParallelConj = yes,
        io.write_string("% contains parallel conjunction\n", !IO)
    ;
        HasParallelConj = no,
        io.write_string("% does not contain parallel conjunction\n", !IO)
    ),

    (
        HasUserEvent = yes,
        io.write_string("% contains user event\n", !IO)
    ;
        HasUserEvent = no,
        io.write_string("% does not contain user event\n", !IO)
    ),

    ( EvalMethod = eval_normal ->
        true
    ;
        io.write_string("% eval method: ", !IO),
        write_eval_method(EvalMethod, !IO),
        io.write_string("\n", !IO)
    ),

    (
        MaybeCallTableTip = yes(CallTableTip),
        io.write_string("% call table tip: ", !IO),
        mercury_output_var(VarSet, AppendVarNums, CallTableTip, !IO),
        io.write_string("\n", !IO)
    ;
        MaybeCallTableTip = no
    ),

    (
        MaybeDeepProfileInfo = yes(DeepProfileInfo),
        DeepProfileInfo = deep_profile_proc_info(MaybeRecInfo,
            MaybeDeepLayout),
        (
            MaybeRecInfo = yes(DeepRecInfo),
            DeepRecInfo = deep_recursion_info(Role, _),
            io.write_string("% deep recursion info: ", !IO),
            (
                Role = inner_proc(DeepPredProcId),
                io.write_string("inner, outer is ", !IO)
            ;
                Role = outer_proc(DeepPredProcId),
                io.write_string("outer, inner is ", !IO)
            ),
            DeepPredProcId = proc(DeepPredId, DeepProcId),
            pred_id_to_int(DeepPredId, DeepPredInt),
            proc_id_to_int(DeepProcId, DeepProcInt),
            io.write_int(DeepPredInt, !IO),
            io.write_string("/", !IO),
            io.write_int(DeepProcInt, !IO),
            io.write_string("\n", !IO)
        ;
            MaybeRecInfo = no
        ),
        (
            MaybeDeepLayout = yes(DeepLayout),
            DeepLayout = hlds_deep_layout(_, ExcpVars),
            ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
                MaybeOldOutermost),
            io.write_string("% deep layout info: ", !IO),
            io.write_string("TopCSD is ", !IO),
            mercury_output_var(VarSet, AppendVarNums, TopCSD, !IO),
            io.write_string(", MiddleCSD is ", !IO),
            mercury_output_var(VarSet, AppendVarNums, MiddleCSD, !IO),
            (
                MaybeOldOutermost = yes(OldOutermost),
                io.write_string(", OldOutermost is ", !IO),
                mercury_output_var(VarSet, AppendVarNums, OldOutermost, !IO)
            ;
                MaybeOldOutermost = no
            ),
            io.write_string("\n", !IO)
        ;
            MaybeDeepLayout = no
        )
    ;
        MaybeDeepProfileInfo = no
    ),

    (
        MaybeUntupleInfo = yes(UntupleInfo),
        write_untuple_info(UntupleInfo, VarSet, AppendVarNums,
            Indent, !IO)
    ;
        MaybeUntupleInfo = no
    ),

    write_indent(Indent, !IO),
    PredName = predicate_name(ModuleInfo, PredId),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    varset.init(ModeVarSet),
    (
        PredOrFunc = predicate,
        mercury_output_pred_mode_decl(ModeVarSet, unqualified(PredName),
            HeadModes, DeclaredDeterminism, ModeContext, !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(HeadModes, FuncHeadModes, RetHeadMode),
        mercury_output_func_mode_decl(ModeVarSet, unqualified(PredName),
            FuncHeadModes, RetHeadMode, DeclaredDeterminism, ModeContext, !IO)
    ),

    (
        MaybeArgLives = yes(ArgLives),
        write_indent(Indent, !IO),
        io.write_string("% arg lives: ", !IO),
        io.print(ArgLives, !IO),
        io.nl(!IO)
    ;
        MaybeArgLives = no
    ),

    (
        string.contains_char(Verbose, 'A'),
        MaybeArgInfos = yes(ArgInfos)
    ->
        write_indent(Indent, !IO),
        io.write_string("% arg_infos: ", !IO),
        io.print(ArgInfos, !IO),
        io.nl(!IO)
    ;
        true
    ),

    (
        ImportStatus = status_pseudo_imported,
        hlds_pred.in_in_unification_proc_id(ProcId)
    ->
        true
    ;
        proc_info_get_stack_slots(Proc, StackSlots),
        write_indent(Indent, !IO),
        write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO),
        write_indent(Indent, !IO),
        term.var_list_to_term_list(HeadVars, HeadTerms),
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadTerms, PredOrFunc, !IO),
        io.write_string(" :-\n", !IO),
        write_goal(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, ".\n",
            !IO)
    ).

write_determinism(Detism, !IO) :-
    io.write_string(determinism_to_string(Detism), !IO).

determinism_to_string(detism_det) = "det".
determinism_to_string(detism_semi) = "semidet".
determinism_to_string(detism_non) = "nondet".
determinism_to_string(detism_multi) = "multi".
determinism_to_string(detism_cc_non) = "cc_nondet".
determinism_to_string(detism_cc_multi) = "cc_multi".
determinism_to_string(detism_erroneous) = "erroneous".
determinism_to_string(detism_failure) = "failure".

write_can_fail(CanFail, !IO) :-
    io.write_string(can_fail_to_string(CanFail), !IO).

can_fail_to_string(can_fail) = "can_fail".
can_fail_to_string(cannot_fail) = "cannot_fail".

write_eval_method(EvalMethod, !IO) :-
    io.write_string(eval_method_to_string(EvalMethod), !IO).

%-----------------------------------------------------------------------------%

:- pred write_indent(int::in, io::di, io::uo) is det.

write_indent(Indent, !IO) :-
    ( Indent = 0 ->
        true
    ;
        io.write_char('\t', !IO),
        write_indent(Indent - 1, !IO)
    ).

:- pred write_intlist(list(int)::in, io::di, io::uo) is det.

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

:- pred write_constraint_proofs(int::in, tvarset::in,
    constraint_proof_map::in, bool::in, io::di, io::uo) is det.

write_constraint_proofs(Indent, VarSet, Proofs, AppendVarNums, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% Proofs: \n", !IO),
    map.to_assoc_list(Proofs, ProofsList),
    io.write_list(ProofsList, "\n",
        write_constraint_proof(Indent, VarSet, AppendVarNums), !IO).

:- pred write_constraint_proof(int::in, tvarset::in, bool::in,
    pair(prog_constraint, constraint_proof)::in, io::di, io::uo) is det.

write_constraint_proof(Indent, VarSet, AppendVarNums, Constraint - Proof,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_constraint(VarSet, AppendVarNums, Constraint, !IO),
    io.write_string(": ", !IO),
    (
        Proof = apply_instance(Num),
        io.write_string("apply instance decl #", !IO),
        io.write_int(Num, !IO)
    ;
        Proof = superclass(Super),
        io.write_string("super class of ", !IO),
        mercury_output_constraint(VarSet, AppendVarNums, Super, !IO)
    ).

:- pred write_constraint_map(int::in, tvarset::in,
    constraint_map::in, bool::in, io::di, io::uo) is det.

write_constraint_map(Indent, VarSet, ConstraintMap, AppendVarNums, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% Constraint Map:\n", !IO),
    map.foldl(write_constraint_map_2(Indent, VarSet, AppendVarNums),
        ConstraintMap, !IO).

:- pred write_constraint_map_2(int::in, tvarset::in, bool::in,
    constraint_id::in, prog_constraint::in, io::di, io::uo) is det.

write_constraint_map_2(Indent, VarSet, AppendVarNums, ConstraintId,
        ProgConstraint, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    write_constraint_id(ConstraintId, !IO),
    io.write_string(": ", !IO),
    mercury_output_constraint(VarSet, AppendVarNums, ProgConstraint, !IO),
    io.nl(!IO).

:- pred write_constraint_id(constraint_id::in, io::di, io::uo) is det.

write_constraint_id(ConstraintId, !IO) :-
    ConstraintId = constraint_id(ConstraintType, GoalPath, N),
    (
        ConstraintType = assumed,
        io.write_string("(E, ", !IO)
    ;
        ConstraintType = unproven,
        io.write_string("(A, ", !IO)
    ),
    goal_path_to_string(GoalPath, GoalPathStr),
    io.write_strings(["""", GoalPathStr, """, "], !IO),
    io.write_int(N, !IO),
    io.write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- func add_mode_qualifier(prog_context, pair(prog_term, mer_mode))
    = prog_term.

add_mode_qualifier(Context, HeadTerm - Mode) = AnnotatedTerm :-
    construct_qualified_term(unqualified("::"),
        [HeadTerm, mode_to_term_with_context(Context, Mode)],
        Context, AnnotatedTerm).

mode_to_term(Mode) = mode_to_term_with_context(term.context_init, Mode).

:- func mode_to_term_with_context(term.context, mer_mode) = prog_term.

mode_to_term_with_context(Context, (InstA -> InstB)) = Term :-
    (
        % Check for higher-order pred or func modes, and output them
        % in a nice format.
        InstA = ground(_Uniq, higher_order(_)),
        InstB = InstA
    ->
        Term = inst_to_term_with_context(InstA, Context)
    ;
        construct_qualified_term(unqualified(">>"),
            [inst_to_term_with_context(InstA, Context),
            inst_to_term_with_context(InstB, Context)],
            Context, Term)
    ).
mode_to_term_with_context(Context, user_defined_mode(Name, Args)) = Term :-
    construct_qualified_term(Name, list.map(map_inst_to_term(Context), Args),
        Context, Term).

:- func make_atom(string, prog_context) = prog_term.

make_atom(Name, Context) =
    term.functor(term.atom(Name), [], Context).

:- func map_inst_to_term(prog_context, mer_inst) = prog_term.

map_inst_to_term(Context, Inst) = inst_to_term_with_context(Inst, Context).

inst_to_term(Inst) = inst_to_term_with_context(Inst, term.context_init).

:- func inst_to_term_with_context(mer_inst, prog_context) = prog_term.

inst_to_term_with_context(any(Uniq), Context) =
    make_atom(any_inst_uniqueness(Uniq), Context).
inst_to_term_with_context(free, Context) =
    make_atom("free", Context).
inst_to_term_with_context(free(Type), Context) = Term :-
    unparse_type(Type, Term0),
    Term1 = term.coerce(Term0),
    Term = term.functor(term.atom("free"), [Term1], Context).
inst_to_term_with_context(bound(Uniq, BoundInsts), Context) = Term :-
    construct_qualified_term(unqualified(inst_uniqueness(Uniq, "bound")),
        [bound_insts_to_term(BoundInsts, Context)], Context, Term).
inst_to_term_with_context(ground(Uniq, GroundInstInfo), Context) = Term :-
    (
        GroundInstInfo = higher_order(pred_inst_info(PredOrFunc, Modes, Det)),
        % XXX we ignore Uniq
        (
            PredOrFunc = predicate,
            construct_qualified_term(unqualified("pred"),
                list.map(mode_to_term_with_context(Context), Modes),
                Context, ModesTerm)
        ;
            PredOrFunc = function,
            pred_args_to_func_args(Modes, ArgModes, RetMode),
            construct_qualified_term(unqualified("func"),
                list.map(mode_to_term_with_context(Context), ArgModes),
                Context, ArgModesTerm),
            construct_qualified_term(unqualified("="),
                [ArgModesTerm, mode_to_term_with_context(Context, RetMode)],
                Context, ModesTerm)
        ),
        construct_qualified_term(unqualified("is"), [
            ModesTerm, det_to_term(Det, Context)], Context, Term)
    ;
        GroundInstInfo = none,
        Term = make_atom(inst_uniqueness(Uniq, "ground"), Context)
    ).
inst_to_term_with_context(inst_var(Var), _) =
    term.coerce(term.variable(Var, context_init)).
inst_to_term_with_context(constrained_inst_vars(Vars, Inst), Context) =
    set.fold(func(Var, Term) =
            term.functor(term.atom("=<"),
                [term.coerce(term.variable(Var, context_init)), Term], Context),
        Vars, inst_to_term_with_context(Inst, Context)).
inst_to_term_with_context(abstract_inst(Name, Args), Context) =
    inst_name_to_term(user_inst(Name, Args), Context).
inst_to_term_with_context(defined_inst(InstName), Context) =
    inst_name_to_term(InstName, Context).
inst_to_term_with_context(not_reached, Context) =
    make_atom("not_reached", Context).

:- func inst_name_to_term(inst_name, prog_context) = prog_term.

inst_name_to_term(user_inst(Name, Args), Context) = Term :-
    construct_qualified_term(Name,
        list.map(map_inst_to_term(Context), Args),
        Context, Term).
inst_name_to_term(merge_inst(InstA, InstB), Context) = Term :-
    construct_qualified_term(unqualified("$merge_inst"),
        list.map(map_inst_to_term(Context), [InstA, InstB]),
        Context, Term).
inst_name_to_term(shared_inst(InstName), Context) = Term :-
    construct_qualified_term(unqualified("$shared_inst"),
        [inst_name_to_term(InstName, Context)],
        Context, Term).
inst_name_to_term(mostly_uniq_inst(InstName), Context) = Term :-
    construct_qualified_term(unqualified("$mostly_uniq_inst"),
        [inst_name_to_term(InstName, Context)],
        Context, Term).
inst_name_to_term(unify_inst(Liveness, InstA, InstB, Real), Context) = Term :-
    construct_qualified_term(unqualified("$unify"),
        [make_atom((Liveness = live -> "live" ; "dead"), Context)] ++
        list.map(map_inst_to_term(Context), [InstA, InstB]) ++
        [make_atom((Real = real_unify -> "real" ; "fake"), Context)],
        Context, Term).
inst_name_to_term(ground_inst(InstName, IsLive, Uniq, Real), Context) = Term :-
    construct_qualified_term(unqualified("$ground"),
        [inst_name_to_term(InstName, Context),
        make_atom((IsLive = live -> "live" ; "dead"), Context),
        make_atom(inst_uniqueness(Uniq, "shared"), Context),
        make_atom((Real = real_unify -> "real" ; "fake"), Context)],
        Context, Term).
inst_name_to_term(any_inst(InstName, IsLive, Uniq, Real), Context) = Term :-
    construct_qualified_term(unqualified("$any"),
        [inst_name_to_term(InstName, Context),
        make_atom((IsLive = live -> "live" ; "dead"), Context),
        make_atom(inst_uniqueness(Uniq, "shared"), Context),
        make_atom((Real = real_unify -> "real" ; "fake"), Context)],
        Context, Term).
inst_name_to_term(typed_ground(Uniq, Type), Context) = Term :-
    unparse_type(Type, Term0),
    construct_qualified_term(unqualified("$typed_ground"),
        [make_atom(inst_uniqueness(Uniq, "shared"), Context),
        term.coerce(Term0)],
        Context, Term).
inst_name_to_term(typed_inst(Type, InstName), Context) = Term :-
    unparse_type(Type, Term0),
    construct_qualified_term(unqualified("$typed_inst"),
        [term.coerce(Term0),
        inst_name_to_term(InstName, Context)],
        Context, Term).

:- func any_inst_uniqueness(uniqueness) = string.

any_inst_uniqueness(shared) = "any".
any_inst_uniqueness(unique) = "unique_any".
any_inst_uniqueness(mostly_unique) = "mostly_unique_any".
any_inst_uniqueness(clobbered) = "clobbered_any".
any_inst_uniqueness(mostly_clobbered) = "mostly_clobbered_any".

:- func inst_uniqueness(uniqueness, string) = string.

inst_uniqueness(shared, SharedName) = SharedName.
inst_uniqueness(unique, _) = "unique".
inst_uniqueness(mostly_unique, _) = "mostly_unique".
inst_uniqueness(clobbered, _) = "clobbered".
inst_uniqueness(mostly_clobbered, _) = "mostly_clobbered".

:- func bound_insts_to_term(list(bound_inst), prog_context) = prog_term.

bound_insts_to_term([], _) = _ :-
    unexpected(this_file, "bound_insts_to_term([])").
bound_insts_to_term([bound_functor(ConsId, Args) | BoundInsts], Context)
        = Term :-
    (
        cons_id_and_args_to_term(ConsId,
            list.map(map_inst_to_term(Context), Args), FirstTerm)
    ->
        (
            BoundInsts = [],
            Term = FirstTerm
        ;
            BoundInsts = [_ | _],
            construct_qualified_term(unqualified(";"),
                [FirstTerm, bound_insts_to_term(BoundInsts, Context)],
                Context, Term)
        )
    ;
        unexpected(this_file,
            "bound_insts_to_term: cons_id_and_args_to_term failed")
    ).

:- func det_to_term(determinism, prog_context) = prog_term.

det_to_term(Det, Context) = make_atom(det_to_string(Det), Context).

:- func det_to_string(determinism) = string.

det_to_string(detism_erroneous) = "erroneous".
det_to_string(detism_failure) = "failure".
det_to_string(detism_det) = "det".
det_to_string(detism_semi) = "semidet".
det_to_string(detism_cc_multi) = "cc_multi".
det_to_string(detism_cc_non) = "cc_nondet".
det_to_string(detism_multi) = "multi".
det_to_string(detism_non) = "nondet".

%-----------------------------------------------------------------------------%

mercury_output_uni_mode_list(UniModes, VarSet, !IO) :-
    mercury_format_uni_mode_list(UniModes, VarSet, !IO).

mercury_uni_mode_list_to_string(UniModes, VarSet) = String :-
    mercury_format_uni_mode_list(UniModes, VarSet, "", String).

:- pred mercury_format_uni_mode_list(list(uni_mode)::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode_list([], _VarSet, !IO).
mercury_format_uni_mode_list([Mode | Modes], VarSet, !IO) :-
    mercury_format_uni_mode(Mode, VarSet, !IO),
    (
        Modes = [],
        true
    ;
        Modes = [_ | _],
        add_string(", ", !IO),
        mercury_format_uni_mode_list(Modes, VarSet, !IO)
    ).

mercury_output_uni_mode(UniMode, VarSet, !IO) :-
    mercury_format_uni_mode(UniMode, VarSet, !IO).

mercury_uni_mode_to_string(UniMode, VarSet) = String :-
    mercury_format_uni_mode(UniMode, VarSet, "", String).

:- pred mercury_format_uni_mode(uni_mode::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode((InstA1 - InstB1 -> InstA2 - InstB2), VarSet, !IO) :-
    mercury_format_mode((InstA1 -> InstA2), simple_inst_info(VarSet), !IO),
    add_string(" = ", !IO),
    mercury_format_mode((InstB1 -> InstB2), simple_inst_info(VarSet), !IO).

:- instance inst_info(expanded_inst_info) where [
    func(instvarset/1) is eii_varset,
    pred(format_defined_inst/4) is mercury_format_expanded_defined_inst
].

:- type expanded_inst_info
    --->    expanded_inst_info(
                eii_varset      :: inst_varset,
                eii_module_info :: module_info,
                eii_expansions  :: set(inst_name)
                                % the set of already-expanded insts;
                                % further occurrences of these will
                                % be output as "..."
            ).

:- pred mercury_format_expanded_defined_inst(inst_name::in,
    expanded_inst_info::in, U::di, U::uo) is det <= output(U).

mercury_format_expanded_defined_inst(InstName, ExpandedInstInfo, !S) :-
    ( set.member(InstName, ExpandedInstInfo ^ eii_expansions) ->
        add_string("...", !S)
    ; InstName = user_inst(_, _) ->
        % Don't expand user-defined insts, just output them as is
        % (we do expand any compiler-defined insts that occur
        % in the arguments of the user-defined inst, however).
        mercury_format_inst_name(InstName, ExpandedInstInfo, !S)
    ;
        inst_lookup(ExpandedInstInfo ^ eii_module_info, InstName, Inst),
        set.insert(ExpandedInstInfo ^ eii_expansions, InstName, Expansions),
        mercury_format_inst(Inst,
            ExpandedInstInfo ^ eii_expansions := Expansions, !S)
    ).

mercury_output_expanded_inst(Inst, VarSet, ModuleInfo, !IO) :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), !IO).

mercury_expanded_inst_to_string(Inst, VarSet, ModuleInfo) = String :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), "", String).

:- pred write_short_reuse_description(short_reuse_description::in, 
    prog_varset::in, bool::in, 
    io::di, io::uo) is det.
write_short_reuse_description(ShortDescription, VarSet, AppendVarnums, !IO):- 
    (
        ShortDescription = cell_died, 
        io.write_string("cell died", !IO)
    ;
        ShortDescription = cell_reused(Var, IsConditional, _, _),
        io.write_string("cell reuse - ", !IO),
        mercury_output_var(VarSet, AppendVarnums, Var, !IO),
        io.write_string(" - ", !IO), 
        write_is_conditional(IsConditional, !IO)
    ;
        ShortDescription = reuse_call(IsConditional),
        io.write_string("reuse call - ", !IO), 
        write_is_conditional(IsConditional, !IO)
    ).

:- pred write_is_conditional(is_conditional::in, io::di, io::uo) is det.
write_is_conditional(IsConditional, !IO) :- 
    (
        IsConditional = conditional_reuse,
        io.write_string("with condition", !IO)
    ;
        IsConditional = unconditional_reuse,
        io.write_string("always safe", !IO)
    ).
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_out.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.
%-----------------------------------------------------------------------------%
