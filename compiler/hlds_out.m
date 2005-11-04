%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% hlds_out.m

% Main authors: conway, fjh.

% There is quite a bit of overlap between the following modules:
%
%   hlds_out.m
%   mercury_to_mercury.m
%   term_io.m
%
% mercury_to_mercury.m prints the parse tree data structure defined
% in prog_data.m.  hlds_out.m does a similar task, but for the data
% structure defined in hlds.m.  term_io.m prints terms.

% There are two different ways of printing variables.
% One way uses the names Var', Var'', etc. which are generated
% by the compiler.  The other way converts all names back into
% a format allowed as source code.  Currently this module calls
% mercury_to_mercury.m, which uses the second method, rather
% than term_io.m, which uses the first method.  We should
% think about using an option to specify which method is used.

%-----------------------------------------------------------------------------%

:- module hlds__hlds_out.

:- interface.

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

:- pred aditi_builtin_name(aditi_builtin::in, string::out) is det.

    % write_pred_id/4 writes out a message such as
    %       predicate `foo:bar/3'
    % or    function `foo:myfoo/5'
    % except in some special cases where the predicate name is mangled
    % and we can print a more meaningful identification of the predicate
    % in question.
    %
:- pred write_pred_id(module_info::in, pred_id::in, io::di, io::uo) is det.
:- func pred_id_to_string(module_info, pred_id) = string.

:- pred write_pred_proc_id(module_info::in, pred_proc_id::in, io::di, io::uo)
    is det.
:- func pred_proc_id_to_string(module_info, pred_proc_id) = string.

:- pred write_pred_proc_id(module_info::in, pred_id::in, proc_id::in,
    io::di, io::uo) is det.
:- func pred_proc_id_to_string(module_info, pred_id, proc_id) = string.

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
:- pred write_unify_context(bool::in, bool::out, unify_context::in,
    prog_context::in, io::di, io::uo) is det.
:- pred unify_context_to_pieces(bool::in, bool::out, unify_context::in,
    list(format_component)::in, list(format_component)::out) is det.

:- pred write_determinism(determinism::in, io::di, io::uo) is det.
:- func determinism_to_string(determinism) = string.

:- pred write_can_fail(can_fail::in, io::di, io::uo) is det.
:- func can_fail_to_string(can_fail) = string.

:- pred write_eval_method(eval_method::in, io::di, io::uo) is det.

:- pred write_import_status(import_status::in, io::di, io::uo) is det.
:- func import_status_to_string(import_status) = string.

%-----------------------------------------------------------------------------%

    % Print out an entire hlds structure.
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
    --->    yes(tvarset, vartypes)
    ;       no.

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
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.instmap.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term_io.
:- import_module varset.

write_type_ctor(Name - Arity, !IO) :-
    prog_out__write_sym_name_and_arity(Name / Arity, !IO).

type_ctor_to_string(Name - Arity) = Str :-
    prog_out__sym_name_and_arity_to_string(Name / Arity, Str).

write_class_id(class_id(Name, Arity), !IO) :-
    prog_out__write_sym_name_and_arity(Name / Arity, !IO).

write_cons_id(ConsId, !IO) :-
    io__write_string(cons_id_to_string(ConsId), !IO).

cons_id_to_string(cons(SymName, Arity)) = String :-
    mdbcomp__prim_data__sym_name_to_string(SymName, SymNameString0),
    ( string__contains_char(SymNameString0, '*') ->
        % We need to protect against the * appearing next to a /
        Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
            ( Char = ('*') ->
                string__append(Str0, "star", Str)
            ;
                string__char_to_string(Char, CharStr),
                string__append(Str0, CharStr, Str)
            )
        ),
        string__foldl(Stuff, SymNameString0, "", SymNameString1)
    ;
        SymNameString1 = SymNameString0
    ),
    SymNameString = term_io__escaped_string(SymNameString1),
    string__int_to_string(Arity, ArityString),
    string__append_list([SymNameString, "/", ArityString], String).
cons_id_to_string(int_const(Int)) = String :-
    string__int_to_string(Int, String).
cons_id_to_string(string_const(String)) =
    term_io__quoted_string(String).
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
cons_id_to_string(
        tabling_pointer_const(shrouded_pred_proc_id(PredId, ProcId))) =
    "<tabling_pointer " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".
cons_id_to_string(deep_profiling_proc_layout(
        shrouded_pred_proc_id(PredId, ProcId))) =
    "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".
cons_id_to_string(table_io_decl(shrouded_pred_proc_id(PredId, ProcId))) =
    "<table_io_decl " ++ int_to_string(PredId) ++
    ", " ++ int_to_string(ProcId) ++ ">".

    % The code of this predicate duplicates the functionality of
    % hlds_error_util__describe_one_pred_name. Changes here should be made
    % there as well.
write_pred_id(ModuleInfo, PredId, !IO) :-
    io__write_string(pred_id_to_string(ModuleInfo, PredId), !IO).

pred_id_to_string(ModuleInfo, PredId) = Str :-
    module_info_preds(ModuleInfo, PredTable),
    ( map__search(PredTable, PredId, PredInfo) ->
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = special_pred(SpecialId - TypeCtor)
        ->
            special_pred_description(SpecialId, Descr),
            TypeCtor = _TypeSymName - TypeArity,
            ( TypeArity = 0 ->
                ForStr = " for type "
            ;
                ForStr = " for type constructor "
            ),
            Str = Descr ++ ForStr ++ type_name_to_string(TypeCtor)
        ;
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, class_instance_method)
        ->
            Str = "type class method implementation"
        ;
            pred_info_get_goal_type(PredInfo, promise(PromiseType))
        ->
            Str = "`" ++ prog_out__promise_to_string(PromiseType)
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
    write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO).

pred_proc_id_to_string(ModuleInfo, proc(PredId, ProcId)) =
    pred_proc_id_to_string(ModuleInfo, PredId, ProcId).

write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO) :-
    io__write_string(
        pred_proc_id_to_string(ModuleInfo, PredId, ProcId), !IO).

pred_proc_id_to_string(ModuleInfo, PredId, ProcId) = Str :-
    proc_id_to_int(ProcId, ModeNum),
    Str = pred_id_to_string(ModuleInfo, PredId)
        ++ " mode " ++ int_to_string(ModeNum).

write_call_id(CallId, !IO) :-
    Str = call_id_to_string(CallId),
    io__write_string(Str, !IO).

call_id_to_string(call(PredCallId)) =
    simple_call_id_to_string(PredCallId).
call_id_to_string(generic_call(GenericCallId)) =
    generic_call_id_to_string(GenericCallId).

:- pred write_generic_call_id(generic_call_id::in,
    io::di, io::uo) is det.

write_generic_call_id(GenericCallId, !IO) :-
    Str = generic_call_id_to_string(GenericCallId),
    io__write_string(Str, !IO).

:- func generic_call_id_to_string(generic_call_id) = string.

generic_call_id_to_string(higher_order(Purity, PredOrFunc, _)) =
    purity_prefix_to_string(Purity) ++ "higher-order "
    ++ prog_out__pred_or_func_to_full_str(PredOrFunc) ++ " call".
generic_call_id_to_string(class_method(_ClassId, MethodId)) =
    simple_call_id_to_string(MethodId).
generic_call_id_to_string(cast(CastType)) =
    cast_type_to_string(CastType).
generic_call_id_to_string(aditi_builtin(AditiBuiltin, CallId)) = Str :-
    aditi_builtin_name(AditiBuiltin, Name),
    Str = "`" ++ Name ++ "' of " ++ simple_call_id_to_string(CallId).

:- func cast_type_to_string(cast_type) = string.

cast_type_to_string(unsafe_type_cast) = "unsafe_type_cast".
cast_type_to_string(unsafe_type_inst_cast) = "unsafe_type_inst_cast".
cast_type_to_string(equiv_type_cast) = "equiv_type_cast".
cast_type_to_string(exists_cast) = "exists_cast".

write_call_arg_id(CallId, ArgNum, PredMarkers, !IO) :-
    Str = call_arg_id_to_string(CallId, ArgNum, PredMarkers),
    io__write_string(Str, !IO).

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
            % `class__method' does not need the "call to"
            % prefix ("in call to higher-order call" is redundant,
            % it's much better to just say "in higher-order call").
            CallId = generic_call(GenericCall),
            \+ GenericCall = class_method(_, _)
        ;
            % For calls from type class instance implementations
            % that were defined using the named syntax rather
            % than the clause syntax, we also omit the "call to",
            % since in that case there was no explicit call in
            % the user's source code.
            check_marker(PredMarkers, named_class_instance_method)
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
    io__write_string(Str, !IO).

:- func arg_number_to_string(call_id, int) = string.

arg_number_to_string(call(PredOrFunc - _/Arity), ArgNum) =
    (
        PredOrFunc = function,
        Arity = ArgNum
    ->
        "the return value"
    ;
        "argument " ++ int_to_string(ArgNum)
    ).
arg_number_to_string(generic_call(
        higher_order(_Purity, PredOrFunc, Arity)), ArgNum) = Str :-
    (
        PredOrFunc = function,
        ArgNum = Arity
    ->
        Str = "the return value"
    ;
        % Make error messages for higher-order calls
        % such as `P(A, B)' clearer.
        Main = "argument " ++ int_to_string(ArgNum),
        PredOrFuncStr = prog_out__pred_or_func_to_full_str(PredOrFunc),
        ( ArgNum = 1 ->
            Expl = "the " ++ PredOrFuncStr ++ " term"
        ;
            Expl = "argument " ++ int_to_string(ArgNum - 1)
                ++ " of the called " ++ PredOrFuncStr
        ),
        Str = Main ++ " (i.e. " ++ Expl ++ ")"
    ).
arg_number_to_string(generic_call(class_method(_, _)), ArgNum) =
    "argument " ++ int_to_string(ArgNum).
arg_number_to_string(generic_call(cast(_)), ArgNum) =
    "argument " ++ int_to_string(ArgNum).
arg_number_to_string(generic_call(aditi_builtin(Builtin, CallId)), ArgNum) =
    aditi_builtin_arg_number_to_string(Builtin, CallId, ArgNum).

:- pred write_aditi_builtin_arg_number(aditi_builtin::in,
    simple_call_id::in, int::in, io::di, io::uo) is det.

write_aditi_builtin_arg_number(Builtin, SimpleCallId, ArgNum, !IO) :-
    Str = aditi_builtin_arg_number_to_string(Builtin, SimpleCallId, ArgNum),
    io__write_string(Str, !IO).

:- func aditi_builtin_arg_number_to_string(aditi_builtin, simple_call_id, int)
    = string.

aditi_builtin_arg_number_to_string(aditi_tuple_update(InsertDelete, _),
        _ - _/Arity, ArgNum) = Str :-
    ( ArgNum =< Arity ->
        ( InsertDelete = insert, OpStr = "inserted"
        ; InsertDelete = delete, OpStr = "deleted"
        ),
        Str = "argument " ++ int_to_string(ArgNum) ++
            " of the " ++ OpStr ++ " tuple"
    ;
        Str = "argument " ++ int_to_string(ArgNum - Arity + 1)
    ).
aditi_builtin_arg_number_to_string(aditi_bulk_update(_, _, pred_term),
        _, ArgNum) = Str :-
    Str = "argument " ++ int_to_string(ArgNum).
aditi_builtin_arg_number_to_string(
        aditi_bulk_update(_, _, sym_name_and_closure), _, ArgNum) = Str :-
    % The original goal had a sym_name/arity
    % at the front of the argument list.
    Str = "argument " ++ int_to_string(ArgNum + 1).

%-----------------------------------------------------------------------------%

write_unify_context(UnifyContext, Context, !IO) :-
    write_unify_context(no, _, UnifyContext, Context, !IO).

unify_context_to_pieces(UnifyContext, !Pieces) :-
    unify_context_to_pieces(no, _, UnifyContext, !Pieces).

write_unify_context(!First, UnifyContext, Context, !IO) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list__reverse(RevSubContexts, SubContexts),
    write_unify_main_context(!First, MainContext, Context, !IO),
    write_unify_sub_contexts(!First, SubContexts, Context, !IO).

unify_context_to_pieces(!First, UnifyContext, !Pieces) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list__reverse(RevSubContexts, SubContexts),
    unify_main_context_to_pieces(!First, MainContext, !Pieces),
    unify_sub_contexts_to_pieces(!First, SubContexts, !Pieces).

:- pred write_unify_main_context(bool::in, bool::out,
    unify_main_context::in, prog_context::in, io::di, io::uo) is det.

write_unify_main_context(!First, explicit, _, !IO).
write_unify_main_context(!First, head(ArgNum), Context, !IO) :-
    write_in_argument(!.First, ArgNum, Context, !IO),
    !:First = no,
    io__write_string(" of clause head:\n", !IO).
write_unify_main_context(!First, head_result, Context, !IO) :-
    start_in_message(!.First, Context, !IO),
    !:First = no,
    io__write_string("function result term of clause head:\n", !IO).
write_unify_main_context(!First, call(CallId, ArgNum), Context, !IO) :-
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
    io__write_string(":\n", !IO).
write_unify_main_context(!First, implicit(Source), Context, !IO) :-
    start_in_message(!.First, Context, !IO),
    io__format("implicit %s unification:\n", [s(Source)], !IO).

:- pred unify_main_context_to_pieces(bool::in, bool::out,
    unify_main_context::in,
    list(format_component)::in, list(format_component)::out) is det.

unify_main_context_to_pieces(!First, explicit, !Pieces).
unify_main_context_to_pieces(!First, head(ArgNum), !Pieces) :-
    in_argument_to_pieces(!.First, ArgNum, !Pieces),
    !:First = no,
    !:Pieces = !.Pieces ++ [words("of clause head:"), nl].
unify_main_context_to_pieces(!First, head_result, !Pieces) :-
    start_in_message_to_pieces(!.First, !Pieces),
    !:First = no,
    !:Pieces = !.Pieces ++ [words("function result term of clause head:"), nl].
unify_main_context_to_pieces(!First, call(CallId, ArgNum),
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
unify_main_context_to_pieces(!First, implicit(Source), !Pieces) :-
    start_in_message_to_pieces(!.First, !Pieces),
    string__format("implicit %s unification:\n", [s(Source)], Msg),
    !:Pieces = !.Pieces ++ [words(Msg), nl].

:- pred write_unify_sub_contexts(bool::in, bool::out,
    unify_sub_contexts::in, prog_context::in, io::di, io::uo) is det.

write_unify_sub_contexts(!First, [], _, !IO).
write_unify_sub_contexts(!First, [ConsId - ArgNum | SubContexts], Context,
        !IO) :-
    write_in_argument(!.First, ArgNum, Context, !IO),
    !:First = no,
    io__write_string(" of functor `", !IO),
    write_cons_id(ConsId, !IO),
    io__write_string("':\n", !IO),
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
    io__write_string("argument ", !IO),
    io__write_int(ArgNum, !IO).

:- pred in_argument_to_pieces(bool::in, int::in,
    list(format_component)::in, list(format_component)::out) is det.

in_argument_to_pieces(First, ArgNum, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    ArgNumStr = int_to_string(ArgNum),
    !:Pieces = !.Pieces ++ [words("argument"), words(ArgNumStr)].

:- pred start_in_message(bool::in, prog_context::in, io::di, io::uo) is det.

start_in_message(First, Context, !IO) :-
    prog_out__write_context(Context, !IO),
    (
        First = yes,
        io__write_string("  In ", !IO)
    ;
        First = no,
        io__write_string("  in ", !IO)
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
    module_info_get_superclass_table(Module, SuperClassTable),
    module_info_get_instance_table(Module, InstanceTable),
    write_header(Indent, Module, !IO),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'I') ->
        write_imports(Indent, Imports, !IO)
    ;
        true
    ),
    ( string__contains_char(Verbose, 'T') ->
        write_types(Indent, TypeTable, !IO),
        io__write_string("\n", !IO),
        write_classes(Indent, ClassTable, !IO),
        io__write_string("\n", !IO),
        write_superclasses(Indent, SuperClassTable, !IO),
        io__write_string("\n", !IO),
        write_instances(Indent, InstanceTable, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),
    ( string__contains_char(Verbose, 'M') ->
        write_insts(Indent, InstTable, !IO),
        io__write_string("\n", !IO),
        write_modes(Indent, ModeTable, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),
    write_preds(Indent, Module, PredTable, !IO),
    write_footer(Indent, Module, !IO).

:- pred write_header(int::in, module_info::in, io::di, io::uo) is det.

write_header(Indent, Module, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% vim: ts=2 sw=2\n\n", !IO),
    module_info_get_name(Module, Name),
    write_indent(Indent, !IO),
    io__write_string(":- module ", !IO),
    prog_out__write_sym_name(Name, !IO),
    io__write_string(".\n\n", !IO).

:- pred write_imports(int::in, set(module_specifier)::in, io::di, io::uo)
    is det.

write_imports(Indent, ImportSet, !IO) :-
    write_indent(Indent, !IO),
    io__write_string(":- import_module ", !IO),
    io__write_list(set__to_sorted_list(ImportSet), ", ", write_sym_name, !IO),
    io__write_string(".\n\n", !IO).

:- pred write_footer(int::in, module_info::in, io::di, io::uo) is det.

write_footer(Indent, Module, !IO) :-
    module_info_get_name(Module, Name),
    write_indent(Indent, !IO),
    io__write_string(":- end_module ", !IO),
    prog_out__write_sym_name(Name, !IO),
    io__write_string(".\n", !IO).

:- pred write_preds(int::in, module_info::in, pred_table::in,
    io::di, io::uo) is det.

write_preds(Indent, ModuleInfo, PredTable, !IO) :-
    io__write_string("%-------- Predicates --------\n\n", !IO),
    write_indent(Indent, !IO),
    map__keys(PredTable, PredIds),
    list__foldl(maybe_write_pred(Indent, ModuleInfo, PredTable), PredIds, !IO).

:- pred maybe_write_pred(int::in, module_info::in, pred_table::in,
    pred_id::in, io::di, io::uo) is det.

maybe_write_pred(Indent, ModuleInfo, PredTable, PredId, !IO) :-
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    globals__io_lookup_int_option(dump_hlds_pred_id, DumpPredId, !IO),
    pred_id_to_int(PredId, PredIdInt),
    map__lookup(PredTable, PredId, PredInfo),
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
            \+ string__contains_char(Verbose, 'I'),
            pred_info_is_imported(PredInfo)
        ;
            % For pseudo-imported predicates (i.e. unification preds),
            % only print them if we are using a local mode for them.
            \+ string__contains_char(Verbose, 'I'),
            pred_info_is_pseudo_imported(PredInfo),
            ProcIds = pred_info_procids(PredInfo),
            hlds_pred__in_in_unification_proc_id(ProcId),
            ProcIds = [ProcId]
        ;
            % We dump unification and other compiler-generated special
            % predicates if suboption 'U' is on. We don't need that information
            % to understand how the program has been transformed.
            \+ string__contains_char(Verbose, 'U'),
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
    pred_info_arg_types(PredInfo, ArgTypes),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    pred_info_typevarset(PredInfo, TVarSet),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    pred_info_context(PredInfo, Context),
    pred_info_import_status(PredInfo, ImportStatus),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_constraint_proofs(PredInfo, Proofs),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_head_type_params(PredInfo, HeadTypeParams),
    pred_info_get_indexes(PredInfo, Indexes),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),
    ( string__contains_char(Verbose, 'C') ->
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
    ( string__contains_char(Verbose, 'C') ->
        write_indent(Indent, !IO),
        io__write_string("% pred id: ", !IO),
        pred_id_to_int(PredId, PredInt),
        io__write_int(PredInt, !IO),
        io__write_string(", category: ", !IO),
        write_pred_or_func(PredOrFunc, !IO),
        io__write_string(", status: ", !IO),
        write_import_status(ImportStatus, !IO),
        io__write_string("\n", !IO),
        io__write_string("% goal_type: ", !IO),
        pred_info_get_goal_type(PredInfo, GoalType),
        io__write(GoalType, !IO),
        io__write_string("\n", !IO),
        markers_to_marker_list(Markers, MarkerList),
        (
            MarkerList = []
        ;
            MarkerList = [_ | _],
            io__write_string("% markers: ", !IO),
            write_marker_list(MarkerList, !IO),
            io__write_string("\n", !IO)
        ),
        write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
            !IO),
        ( map__is_empty(Proofs) ->
            true
        ;
            write_constraint_proofs(Indent, TVarSet, Proofs, AppendVarNums,
                !IO),
            io__write_string("\n", !IO)
        ),
        ( map__is_empty(ConstraintMap) ->
            true
        ;
            write_constraint_map(Indent, TVarSet, ConstraintMap, AppendVarNums,
                !IO)
        ),

        % XXX The indexes are not part of the clauses_info,
        % so why is this code inside this if-then-else
        % with the condition `string__contains_char(Verbose, 'C')'?
        % Shouldn't it be dependent on a different letter?

        (
            Indexes = []
        ;
            Indexes = [_ | _],
            io__write_string("% indexes: ", !IO),
            io__write_list(Indexes, ", ", mercury_output_index_spec, !IO),
            io__nl(!IO)
        ),

        (
            HeadTypeParams = [_ | _],
            io__write_string("% head_type_params:\n", !IO),
            io__write_string("% ", !IO),
            mercury_output_vars(HeadTypeParams, TVarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            HeadTypeParams = []
        ),
        write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),

        get_clause_list(ClausesRep, Clauses),
        (
            Clauses = [_ | _],
            set_dump_opts_for_clauses(SavedDumpString, !IO),
            write_clauses(Indent, ModuleInfo, PredId, VarSet,
                AppendVarNums, HeadVars, PredOrFunc, Clauses, no, !IO),
            globals__io_set_option(dump_hlds_options, string(SavedDumpString),
                !IO)
        ;
            Clauses = []
        ),

        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = instance_method(MethodConstraints),
            MethodConstraints = instance_method_constraints(ClassId,
                InstanceTypes, InstanceConstraints, ClassMethodConstraints),
            io__write_string("% instance method constraints:\n", !IO),
            ClassId = class_id(ClassName, _),
            mercury_output_constraint(TVarSet, AppendVarNums,
                constraint(ClassName, InstanceTypes), !IO),
            io__nl(!IO),
            io__write_string("instance constraints: ", !IO),
            io__write_list(InstanceConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io__nl(!IO),

            ClassMethodConstraints = constraints(MethodUnivConstraints,
                MethodExistConstraints),
            io__write_string("method univ constraints: ", !IO),
            io__write_list(MethodUnivConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io__nl(!IO),
            io__write_string("method exist constraints: ", !IO),
            io__write_list(MethodExistConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io__nl(!IO)
        ;
            Origin = special_pred(_),
            io__write_string("% special pred\n", !IO)
        ;
            Origin = transformed(Transformation, _, OrigPredId),
            io__write_string("% transformed from ", !IO),
            write_pred_id(ModuleInfo, OrigPredId, !IO),
            io__write_string(": ", !IO),
            io__write(Transformation, !IO),
            io__nl(!IO)
        ;
            Origin = created(Creation),
            io__write_string("% created: ", !IO),
            io__write(Creation, !IO),
            io__nl(!IO)
        ;
            Origin = assertion(_, _),
            io__write_string("% assertion\n", !IO)
        ;
            Origin = lambda(_, _, _)
        ;
            Origin = user(_)
        )
    ;
        true
    ),
    write_procs(Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus,
        PredInfo, !IO),
    io__write_string("\n", !IO).

:- pred set_dump_opts_for_clauses(string::out, io::di, io::uo) is det.

set_dump_opts_for_clauses(SavedDumpStr, !IO) :-
    globals__io_lookup_string_option(dump_hlds_options, SavedDumpStr, !IO),
    some [!DumpStr] (
        !:DumpStr = "",
        ( string__contains_char(SavedDumpStr, 'c') ->
            !:DumpStr = !.DumpStr ++ "c"
        ;
            true
        ),
        ( string__contains_char(SavedDumpStr, 'n') ->
            !:DumpStr = !.DumpStr ++ "n"
        ;
            true
        ),
        ( string__contains_char(SavedDumpStr, 'v') ->
            !:DumpStr = !.DumpStr ++ "v"
        ;
            true
        ),
        ( string__contains_char(SavedDumpStr, 'g') ->
            !:DumpStr = !.DumpStr ++ "g"
        ;
            true
        ),
        DumpStr = !.DumpStr
    ),
    globals__io_set_option(dump_hlds_options, string(DumpStr), !IO).

:- pred write_marker_list(list(marker)::in, io::di, io::uo) is det.

write_marker_list(Markers, !IO) :-
    io__write_list(Markers, ", ", write_marker, !IO).

marker_name(stub, "stub").
marker_name(infer_type, "infer_type").
marker_name(infer_modes, "infer_modes").
marker_name(user_marked_inline, "inline").
marker_name(user_marked_no_inline, "no_inline").
marker_name(heuristic_inline, "heuristic_inline").
marker_name(dnf, "dnf").
marker_name(obsolete, "obsolete").
marker_name(class_method, "class_method").
marker_name(class_instance_method, "class_instance_method").
marker_name(named_class_instance_method, "named_class_instance_method").
marker_name(is_impure, "impure").
marker_name(is_semipure, "semipure").
marker_name(promised_pure, "promise_pure").
marker_name(promised_semipure, "promise_semipure").
marker_name(terminates, "terminates").
marker_name(check_termination, "check_termination").
marker_name(does_not_terminate, "does_not_terminate").
marker_name(aditi, "aditi").
marker_name(base_relation, "base_relation").
marker_name(generate_inline, "generate_inline").
marker_name(aditi_memo, "aditi_memo").
marker_name(aditi_no_memo, "aditi_no_memo").
marker_name(naive, "naive").
marker_name(psn, "psn").
marker_name(supp_magic, "supp_magic").
marker_name(context, "context").
marker_name(calls_are_fully_qualified, "calls_are_fully_qualified").
marker_name(mode_check_clauses, "mode_check_clauses").

write_marker(Marker, !IO) :-
    marker_name(Marker, Name),
    io__write_string(Name, !IO).

write_promise(PromiseType, Indent, ModuleInfo, _PredId, VarSet,
        AppendVarNums, HeadVars, _PredOrFunc, Clause, TypeQual, !IO) :-
    % Curry the varset for term_io__write_variable/4.
    PrintVar = (pred(VarName::in, IOState0::di, IOState::uo) is det :-
        term_io__write_variable(VarName, VarSet, IOState0, IOState)
    ),

    write_indent(Indent, !IO),

    % Print initial formatting differently for assertions.
    ( PromiseType = true ->
        io__write_string(":- promise all [", !IO),
        io__write_list(HeadVars, ", ", PrintVar, !IO),
        io__write_string("] (\n", !IO)
    ;
        io__write_string(":- all [", !IO),
        io__write_list(HeadVars, ", ", PrintVar, !IO),
        io__write_string("]", !IO),
        mercury_output_newline(Indent, !IO),
        prog_out__write_promise_type(PromiseType, !IO),
        mercury_output_newline(Indent, !IO),
        io__write_string("(\n", !IO)
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
        term__var_list_to_term_list(HeadVars, HeadTerms),
        UseDeclaredModes = no,
        io__write_string("% clause ", !IO),
        io__write_int(ClauseNum, !IO),
        io__write_string("\n", !IO),
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
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'm') ->
        write_indent(Indent, !IO),
        io__write_string("% Modes for which this clause applies: ", !IO),
        ModeInts = list__map(proc_id_to_int, Modes),
        write_intlist(ModeInts, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),
    (
        Lang = mercury
    ;
        Lang = foreign_language(ForeignLang),
        io__write_string("% Language of implementation: ", !IO),
        io__write(ForeignLang, !IO),
        io__nl(!IO)
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
    ( Goal = conj([]) - _GoalInfo ->
        io__write_string(".\n", !IO)
    ;
        io__write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, ".\n",
            TypeQual, !IO)
    ).

:- pred write_annotated_clause_heads(module_info::in, term__context::in,
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

:- pred write_annotated_clause_head(module_info::in, term__context::in,
    pred_id::in, proc_id::in, prog_varset::in, bool::in, list(prog_term)::in,
    pred_or_func::in, bool::in, io::di, io::uo) is det.

write_annotated_clause_head(ModuleInfo, Context, PredId, ProcId, VarSet,
        AppendVarNums, HeadTerms, PredOrFunc, UseDeclaredModes, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_procedures(PredInfo, Procedures),
    ( map__search(Procedures, ProcId, ProcInfo) ->
        % When writing `.opt' files, use the declared argument modes so that
        % the modes are guaranteed to be syntactically identical to those
        % in the original program. The test in add_clause.m to check whether
        % a clause matches a procedure tests for syntactic identity (roughly).
        % The modes returned by proc_info_argmodes may have been slightly
        % expanded by propagate_types_into_modes.
        %
        % We can't use the declared argument modes when writing HLDS dumps
        % because the modes of the type-infos will not have been added,
        % so the call to assoc_list__from_corresponding_lists below
        % will abort. `.opt' files are written before the polymorphism pass.
        (
            UseDeclaredModes = yes,
            proc_info_declared_argmodes(ProcInfo, ArgModes)
        ;
            UseDeclaredModes = no,
            proc_info_argmodes(ProcInfo, ArgModes)
        ),
        assoc_list__from_corresponding_lists(HeadTerms, ArgModes,
            AnnotatedPairs),
        AnnotatedHeadTerms = list__map(add_mode_qualifier(Context),
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
    predicate_name(ModuleInfo, PredId, PredName),
    predicate_module(ModuleInfo, PredId, ModuleName),
    (
        PredOrFunc = function,
        pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
        write_qualified_functor_with_term_args(ModuleName,
            term__atom(PredName), FuncArgs, VarSet, AppendVarNums, !IO),
        io__write_string(" = ", !IO),
        mercury_output_term(RetVal, VarSet, AppendVarNums,
            next_to_graphic_token, !IO)
    ;
        PredOrFunc = predicate,
        write_qualified_functor_with_term_args(ModuleName,
            term__atom(PredName), HeadTerms, VarSet, AppendVarNums, !IO)
    ).

write_goal(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow, !IO) :-
    % Don't type qualify everything.
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow, no,
        !IO).

    % TypeQual is yes(TVarset, VarTypes) if all constructors should be
    % module qualified.
    %
:- pred write_goal_a(hlds_goal::in, module_info::in, prog_varset::in,
    bool::in, int::in, string::in, maybe_vartypes::in, io::di, io::uo) is det.

write_goal_a(Goal - GoalInfo, ModuleInfo, VarSet, AppendVarNums, Indent,
        Follow, TypeQual, !IO) :-
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'c') ->
        goal_info_get_context(GoalInfo, Context),
        term__context_file(Context, FileName),
        term__context_line(Context, LineNumber),
        ( FileName \= "" ->
            write_indent(Indent, !IO),
            io__write_string("% context: file `", !IO),
            io__write_string(FileName, !IO),
            io__write_string("', line ", !IO),
            io__write_int(LineNumber, !IO),
            io__write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'P') ->
        goal_info_get_goal_path(GoalInfo, Path),
        (
            Path = [_ | _],
            goal_path_to_string(Path, PathStr),
            write_indent(Indent, !IO),
            io__write_string("% goal path: ", !IO),
            io__write_string(PathStr, !IO),
            io__write_string("\n", !IO)
        ;
            Path = []
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'n') ->
        goal_info_get_nonlocals(GoalInfo, NonLocalsSet),
        set__to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            write_indent(Indent, !IO),
            io__write_string("% nonlocals: ", !IO),
            mercury_output_vars(NonLocalsList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            NonLocalsList = []
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'p') ->
        (
            goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
            set__to_sorted_list(PreDeaths, PreDeathList),
            PreDeathList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io__write_string("% pre-deaths: ", !IO),
            mercury_output_vars(PreDeathList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            true
        ),
        (
            goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
            set__to_sorted_list(PreBirths, PreBirthList),
            PreBirthList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io__write_string("% pre-births: ", !IO),
            mercury_output_vars(PreBirthList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'B') ->
        ProducingVars = GoalInfo ^ producing_vars,
        ( set__non_empty(ProducingVars) ->
            set__to_sorted_list(ProducingVars, ProducingVarsList),
            write_indent(Indent, !IO),
            io__write_string("% producing vars: ", !IO),
            mercury_output_vars(ProducingVarsList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            true
        ),

        ConsumingVars = GoalInfo ^ consuming_vars,
        ( set__non_empty(ConsumingVars) ->
            set__to_sorted_list(ConsumingVars, ConsumingVarsList),
            write_indent(Indent, !IO),
            io__write_string("% consuming vars: ", !IO),
            mercury_output_vars(ConsumingVarsList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            true
        ),

        MakeVisibleVars = GoalInfo ^ make_visible_vars,
        ( set__non_empty(MakeVisibleVars) ->
            set__to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            write_indent(Indent, !IO),
            io__write_string("% make_visible vars: ", !IO),
            mercury_output_vars(MakeVisibleVarsList, VarSet, AppendVarNums,
                !IO),
            io__write_string("\n", !IO)
        ;
            true
        ),

        NeedVisibleVars = GoalInfo ^ need_visible_vars,
        ( set__non_empty(NeedVisibleVars) ->
            set__to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            write_indent(Indent, !IO),
            io__write_string("% need_visible vars: ", !IO),
            mercury_output_vars(NeedVisibleVarsList, VarSet, AppendVarNums,
                !IO),
            io__write_string("\n", !IO)
        ;
            true
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'd') ->
        write_indent(Indent, !IO),
        io__write_string("% determinism: ", !IO),
        goal_info_get_determinism(GoalInfo, Determinism),
        write_determinism(Determinism, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),
    write_goal_2(Goal, ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO),
    ( string__contains_char(Verbose, 'i') ->
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        (
            instmap_delta_is_reachable(InstMapDelta),
            instmap_delta_changed_vars(InstMapDelta, Vars),
            set__empty(Vars)
        ->
            true
        ;
            write_indent(Indent, !IO),
            ( string__contains_char(Verbose, 'D') ->
                io__write_string("% new insts: ", !IO),
                write_instmap_delta(InstMapDelta, VarSet, AppendVarNums,
                    Indent, !IO),
                io__write_string("\n", !IO)
            ;
                io__write_string("% vars with new insts: ", !IO),
                write_instmap_delta_vars(InstMapDelta, VarSet, AppendVarNums,
                    !IO),
                io__write_string("\n", !IO)
            )
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'p') ->
        (
            goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
            set__to_sorted_list(PostDeaths, PostDeathList),
            PostDeathList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io__write_string("% post-deaths: ", !IO),
            mercury_output_vars(PostDeathList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ;
            true
        ),
        (
            goal_info_maybe_get_post_births(GoalInfo, PostBirths),
            set__to_sorted_list(PostBirths, PostBirthList),
            PostBirthList = [_ | _]
        ->
            write_indent(Indent, !IO),
            io__write_string("% post-births: ", !IO),
            mercury_output_vars(PostBirthList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
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
    ( string__contains_char(Verbose, 'g') ->
        goal_info_get_features(GoalInfo, Features),
        set__to_sorted_list(Features, FeatureList),
        (
            FeatureList = []
        ;
            FeatureList = [_ | _],
            write_indent(Indent, !IO),
            io__write_string("% Goal features:  ", !IO),
            io__write(FeatureList, !IO),
            io__write_string("\n", !IO)
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
    io__write_string("( % ", !IO),
    write_can_fail(CanFail, !IO),
    io__write_string(" switch on `", !IO),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string("'\n", !IO),
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
        io__write_string("fail\n", !IO)
    ),
    write_indent(Indent, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

write_goal_2(scope(Reason, Goal), ModuleInfo, VarSet, AppendVarNums, Indent,
        Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    (
        Reason = exist_quant(Vars),
        io__write_string("some [", !IO),
        mercury_output_vars(Vars, VarSet, AppendVarNums, !IO),
        io__write_string("] (\n", !IO)
    ;
        Reason = promise_purity(Implicit, Purity),
        (
            Purity = purity_pure,
            io__write_string("promise_pure (", !IO)
        ;
            Purity = purity_semipure,
            io__write_string("promise_semipure (", !IO)
        ;
            Purity = purity_impure,
            io__write_string("promise_impure (", !IO)
        ),
        (
            Implicit = make_implicit_promises,
            io__write_string("implicit\n", !IO)
        ;
            Implicit = dont_make_implicit_promises,
            io__write_string("\n", !IO)
        )
    ;
        Reason = promise_equivalent_solutions(Vars),
        io__write_string("promise_equivalent_solutions [", !IO),
        mercury_output_vars(Vars, VarSet, AppendVarNums, !IO),
        io__write_string("] (\n", !IO)
    ;
        Reason = barrier(removable),
        io__write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io__write_string("% barrier(removable)\n", !IO)
    ;
        Reason = barrier(not_removable),
        io__write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io__write_string("% barrier(not_removable)\n", !IO)
    ;
        Reason = commit(force_pruning),
        io__write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io__write_string("% commit(force_pruning)\n", !IO)
    ;
        Reason = commit(dont_force_pruning),
        io__write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io__write_string("% commit(dont_force_pruning)\n", !IO)
    ;
        Reason = from_ground_term(Var),
        io__write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io__write_string("% from_ground_term [", !IO),
        mercury_output_var(Var, VarSet, AppendVarNums, !IO),
        io__write_string("]\n", !IO)
    ),
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

write_goal_2(if_then_else(Vars, Cond, Then, Else), ModuleInfo, VarSet,
        AppendVarNums, Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("(if", !IO),
    write_some(Vars, VarSet, !IO),
    io__write_string("\n", !IO),
    Indent1 = Indent + 1,
    write_goal_a(Cond, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string("then\n", !IO),
    write_goal_a(Then, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string("else\n", !IO),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
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
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

write_goal_2(not(Goal), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("\\+ (\n", !IO),
    write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

write_goal_2(conj(List), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    (
        List = [Goal | Goals],
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( Verbose \= "" ->
            write_indent(Indent, !IO),
            io__write_string("( % conjunction\n", !IO),
            write_conj(Goal, Goals, ModuleInfo, VarSet, AppendVarNums,
                Indent + 1, "\n", Verbose, ",\n", TypeQual, !IO),
            write_indent(Indent, !IO),
            io__write_string(")", !IO),
            io__write_string(Follow, !IO)
        ;
            write_conj(Goal, Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
                Follow, Verbose, ",\n", TypeQual, !IO)
        )
    ;
        List = [],
        write_indent(Indent, !IO),
        io__write_string("true", !IO),
        io__write_string(Follow, !IO)
    ).

write_goal_2(par_conj(List), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    write_indent(Indent, !IO),
    ( List = [Goal | Goals] ->
        io__write_string("( % parallel conjunction\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
            TypeQual, !IO),
        % See comments at write_goal_list.
        write_goal_list(Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
            "&\n", TypeQual, !IO),
        write_indent(Indent, !IO),
        io__write_string(")", !IO),
        io__write_string(Follow, !IO)
    ;
        io__write_string("/* parallel */ true", !IO),
        io__write_string(Follow, !IO)
    ).

write_goal_2(disj(List), ModuleInfo, VarSet, AppendVarNums,
        Indent, Follow, TypeQual, !IO) :-
    write_indent(Indent, !IO),
    ( List = [Goal | Goals] ->
        io__write_string("( % disjunction\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent + 1, "\n",
            TypeQual, !IO),
        write_goal_list(Goals, ModuleInfo, VarSet, AppendVarNums, Indent,
            ";\n", TypeQual, !IO),
        write_indent(Indent, !IO),
        io__write_string(")", !IO),
        io__write_string(Follow, !IO)
    ;
        io__write_string("fail", !IO),
        io__write_string(Follow, !IO)
    ).

write_goal_2(generic_call(GenericCall, ArgVars, Modes, _),
        ModuleInfo, VarSet, AppendVarNums, Indent, Follow, _, !IO) :-
    % XXX we should print more info here
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        (
            PredOrFunc = predicate,
            ( string__contains_char(Verbose, 'l') ->
                write_indent(Indent, !IO),
                io__write_string("% higher-order predicate call\n", !IO)
            ;
                true
            ),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            write_functor(term__atom("call"),
                [PredVar | ArgVars], VarSet, AppendVarNums, !IO)
        ;
            PredOrFunc = function,
            ( string__contains_char(Verbose, 'l') ->
                write_indent(Indent, !IO),
                io__write_string("% higher-order function application\n", !IO)
            ;
                true
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            mercury_output_var(FuncRetVar, VarSet, AppendVarNums, !IO),
            io__write_string(" = ", !IO),
            write_functor(term__atom("apply"), FuncArgVars, VarSet,
                AppendVarNums, !IO)
        ),
        io__write_string(Follow, !IO)
    ;
        GenericCall = class_method(TCInfoVar, MethodNum, _ClassId, _MethodId),
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string__contains_char(Verbose, 'l') ->
            write_indent(Indent, !IO),
            io__write_string("% class method call\n", !IO)
        ;
            true
        ),
        term__context_init(Context),
        Functor = term__atom("class_method_call"),
        TCInfoTerm = term__variable(TCInfoVar),
        MethodNumTerm = term__functor(term__integer(MethodNum), [], Context),
        term__var_list_to_term_list(ArgVars, ArgTerms),
        Term = term__functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
            Context),
        write_indent(Indent, !IO),
        mercury_output_term(Term, VarSet, AppendVarNums, !IO),
        io__write_string(Follow, !IO)
    ;
        GenericCall = cast(CastType),
        CastTypeString = cast_type_to_string(CastType),
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string__contains_char(Verbose, 'l') ->
            write_indent(Indent, !IO),
            io__write_strings(["% ", CastTypeString, "\n"], !IO)
        ;
            true
        ),
        ( string__contains_char(Verbose, 'D') ->
            write_indent(Indent, !IO),
            io__write_string("% modes: ", !IO),
            varset__init(InstVarSet),
            mercury_output_mode_list(Modes, InstVarSet, !IO),
            io__nl(!IO)
        ;
            true
        ),
        Functor = term__atom(CastTypeString),
        term__var_list_to_term_list(ArgVars, ArgTerms),
        term__context_init(Context),
        Term = term__functor(Functor, ArgTerms, Context),
        write_indent(Indent, !IO),
        mercury_output_term(Term, VarSet, AppendVarNums, !IO),
        io__write_string(Follow, !IO)
    ;
        GenericCall = aditi_builtin(AditiBuiltin, CallId),
        write_indent(Indent, !IO),
        write_aditi_builtin(ModuleInfo, AditiBuiltin, CallId, ArgVars, VarSet,
            AppendVarNums, Indent, Follow, !IO)
    ).

write_goal_2(call(PredId, ProcId, ArgVars, Builtin, MaybeUnifyContext,
        PredName), ModuleInfo, VarSet, AppendVarNums, Indent, Follow,
        TypeQual, !IO) :-
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'b') ->
        (
            Builtin = inline_builtin,
            write_indent(Indent, !IO),
            io__write_string("% inline builtin\n", !IO)
        ;
            Builtin = out_of_line_builtin,
            write_indent(Indent, !IO),
            io__write_string("% out of line builtin\n", !IO)
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
        mercury_output_var(LHSVar, VarSet, AppendVarNums, !IO),
        io__write_string(" = ", !IO)
    ),
    write_sym_name_and_args(PredName, NewArgVars, VarSet, AppendVarNums, !IO),
    io__write_string(Follow, !IO),
    ( string__contains_char(Verbose, 'l') ->
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        write_indent(Indent, !IO),
        io__write_string("% pred id: ", !IO),
        io__write_int(PredNum, !IO),
        io__write_string(", proc id: ", !IO),
        io__write_int(ProcNum, !IO),
        io__write_string(Follow, !IO),
        (
            MaybeUnifyContext = yes(CallUnifyContext),
            ( TypeQual = yes(_, VarTypes) ->
                map__lookup(VarTypes, Var, UniType),
                VarType = yes(UniType)
            ;
                VarType = no
            ),
            CallUnifyContext = call_unify_context(Var, RHS, _UnifyContext),
            write_indent(Indent, !IO),
            io__write_string("% unify context: ", !IO),
            mercury_output_var(Var, VarSet, AppendVarNums, !IO),
            io__write_string(" = ", !IO),
            % XXX Fake the inst varset.
            varset__init(InstVarSet),
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
    mercury_output_var(A, VarSet, AppendVarNums, !IO),
    io__write_string(" = ", !IO),
    ( TypeQual = yes(_, VarTypes) ->
        map__lookup(VarTypes, A, UniType),
        VarType = yes(UniType)
    ;
        VarType = no
    ),
    % XXX Fake the inst varset.
    varset__init(InstVarSet),
    write_unify_rhs_2(B, ModuleInfo, VarSet, InstVarSet,
        AppendVarNums, Indent, Follow, VarType, TypeQual, !IO),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    (
        (
            string__contains_char(Verbose, 'u')
        ;
            string__contains_char(Verbose, 'p')
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

write_goal_2(foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        PragmaCode), ModuleInfo, VarSet, AppendVarNums, Indent, Follow, _,
        !IO) :-
    ForeignLang = foreign_language(Attributes),
    write_indent(Indent, !IO),
    io__write_string("$pragma_foreign_proc(/* ", !IO),
    io__write_string(foreign_language_string(ForeignLang), !IO),
    io__write_string(" */, ", !IO),
    write_pred_id(ModuleInfo, PredId, !IO),
    io__write_string(" pred ", !IO),
    pred_id_to_int(PredId, PredIdInt),
    io__write_int(PredIdInt, !IO),
    io__write_string(" proc ", !IO),
    proc_id_to_int(ProcId, ProcIdInt),
    io__write_int(ProcIdInt, !IO),
    io__write_string(",\n", !IO),
    write_indent(Indent, !IO),
    % XXX We don't have the TypeVarSet available here, but it is only used
    % for printing out the names of the type variables, which isn't essential.
    varset__init(TypeVarSet),
    io__write_string("[", !IO),
    write_foreign_args(Args, VarSet, TypeVarSet, AppendVarNums, !IO),
    io__write_string("],\n", !IO),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        write_indent(Indent, !IO),
        io__write_string("{", !IO),
        write_foreign_args(ExtraArgs, VarSet, TypeVarSet, AppendVarNums, !IO),
        io__write_string("},\n", !IO)
    ),
    (
        PragmaCode = ordinary(C_Code, _),
        io__write_string("""", !IO),
        io__write_string(C_Code, !IO),
        io__write_string("""", !IO)
    ;
        PragmaCode = nondet(Fields, _FieldsContext, First, _FirstContext,
            Later, _LaterContext, Treat, Shared, _SharedContext),
        io__write_string("local_vars(""", !IO),
        io__write_string(Fields, !IO),
        io__write_string("""), ", !IO),
        io__write_string("first_code(""", !IO),
        io__write_string(First, !IO),
        io__write_string("""), ", !IO),
        io__write_string("retry_code(""", !IO),
        io__write_string(Later, !IO),
        io__write_string("""), ", !IO),
        (
            Treat = share,
            io__write_string("shared_code(""", !IO)
        ;
            Treat = duplicate,
            io__write_string("duplicated_code(""", !IO)
        ;
            Treat = automatic,
            io__write_string("common_code(""", !IO)
        ),
        io__write_string(Shared, !IO),
        io__write_string(""")", !IO)
    ;
        PragmaCode = import(Name, _, _, _Context),
        io__write_string("""", !IO),
        io__write_string(Name, !IO),
        io__write_string("""", !IO)
    ),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

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
    io__write_string("( % bi-implication\n", !IO),
    Indent1 = Indent + 1,
    write_goal_a(LHS, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string("<=>\n", !IO),
    write_goal_a(RHS, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
        TypeQual, !IO),
    write_indent(Indent, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO).

:- pred write_foreign_args(list(foreign_arg)::in, prog_varset::in,
    tvarset::in, bool::in, io::di, io::uo) is det.

write_foreign_args([], _, _, _, !IO).
write_foreign_args([Arg | Args], VarSet, TVarSet, AppendVarNums, !IO) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    (
        MaybeNameMode = yes(Name - Mode),
        io__write_string("/" ++ Name ++ "(", !IO),
        ( Mode = in_mode ->
            io__write_string("in", !IO)
        ; Mode = out_mode ->
            io__write_string("out", !IO)
        ;
            io__write(Mode, !IO)
        ),
        io__write_string(")", !IO)
    ;
        MaybeNameMode = no
    ),
    io__write_string("@", !IO),
    mercury_output_type(TVarSet, AppendVarNums, Type, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io__write_string(", ", !IO),
        write_foreign_args(Args, VarSet, TVarSet,
            AppendVarNums, !IO)
    ).

:- pred write_llds_code_gen_info(hlds_goal_info::in, prog_varset::in,
    bool::in, int::in, string::in, io::di, io::uo) is det.

write_llds_code_gen_info(GoalInfo, VarSet, AppendVarNums,
        Indent, Verbose, !IO) :-
    ( string__contains_char(Verbose, 'f') ->
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextReg),
            map__to_assoc_list(FollowVarsMap, FVlist),
            write_indent(Indent, !IO),
            io__write_string("% follow vars: ", !IO),
            io__write_int(NextReg, !IO),
            io__write_string("\n", !IO),
            write_var_to_abs_locns(FVlist, VarSet, AppendVarNums, Indent, !IO)
        ;
            MaybeFollowVars = no
        )
    ;
        true
    ),
    ( string__contains_char(Verbose, 'r') ->
        goal_info_get_resume_point(GoalInfo, Resume),
        (
            Resume = no_resume_point
        ;
            Resume = resume_point(ResumeVars, Locs),
            set__to_sorted_list(ResumeVars, ResumeVarList),
            write_indent(Indent, !IO),
            io__write_string("% resume point ", !IO),
            (
                Locs = orig_only,
                io__write_string("orig only ", !IO)
            ;
                Locs = stack_only,
                io__write_string("stack only ", !IO)
            ;
                Locs = orig_and_stack,
                io__write_string("orig and stack ", !IO)
            ;
                Locs = stack_and_orig,
                io__write_string("stack and orig ", !IO)
            ),
            mercury_output_vars(ResumeVarList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string__contains_char(Verbose, 's'),
        goal_info_get_store_map(GoalInfo, StoreMap),
        map__to_assoc_list(StoreMap, StoreMapList),
        StoreMapList \= []
    ->
        write_indent(Indent, !IO),
        io__write_string("% store map:\n", !IO),
        write_var_to_abs_locns(StoreMapList, VarSet, AppendVarNums, Indent,
            !IO)
    ;
        true
    ),
    (
        string__contains_char(Verbose, 's'),
        goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
        MaybeNeedAcrossCall = yes(NeedAcrossCall)
    ->
        NeedAcrossCall = need_across_call(CallForwardSet, CallResumeSet,
            CallNondetSet),
        set__to_sorted_list(CallForwardSet, CallForwardList),
        set__to_sorted_list(CallResumeSet, CallResumeList),
        set__to_sorted_list(CallNondetSet, CallNondetList),
        write_indent(Indent, !IO),
        io__write_string("% need across call forward vars: ", !IO),
        (
            CallForwardList = [],
            io__write_string("none\n", !IO)
        ;
            CallForwardList = [_ | _],
            write_vars(CallForwardList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io__write_string("% need across call resume vars: ", !IO),
        (
            CallResumeList = [],
            io__write_string("none\n", !IO)
        ;
            CallResumeList = [_ | _],
            write_vars(CallResumeList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io__write_string("% need across call nondet vars: ", !IO),
        (
            CallNondetList = [],
            io__write_string("none\n", !IO)
        ;
            CallNondetList = [_ | _],
            write_vars(CallNondetList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string__contains_char(Verbose, 's'),
        goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume),
        MaybeNeedInResume = yes(NeedInResume)
    ->
        NeedInResume = need_in_resume(ResumeOnStack, ResumeResumeSet,
            ResumeNondetSet),
        set__to_sorted_list(ResumeResumeSet, ResumeResumeList),
        set__to_sorted_list(ResumeNondetSet, ResumeNondetList),

        write_indent(Indent, !IO),
        (
            ResumeOnStack = yes,
            io__write_string("% resume point has stack label\n", !IO)
        ;
            ResumeOnStack = no,
            io__write_string("% resume point has no stack label\n", !IO)
        ),
        write_indent(Indent, !IO),
        io__write_string("% need in resume resume vars: ", !IO),
        (
            ResumeResumeList = [],
            io__write_string("none\n", !IO)
        ;
            ResumeResumeList = [_ | _],
            write_vars(ResumeResumeList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io__write_string("% need in resume nondet vars: ", !IO),
        (
            ResumeNondetList = [],
            io__write_string("none\n", !IO)
        ;
            ResumeNondetList = [_ | _],
            write_vars(ResumeNondetList, VarSet, AppendVarNums, !IO),
            io__write_string("\n", !IO)
        )
    ;
        true
    ),
    (
        string__contains_char(Verbose, 's'),
        goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj),
        MaybeNeedInParConj = yes(NeedInParConj)
    ->
        NeedInParConj = need_in_par_conj(ParConjSet),
        set__to_sorted_list(ParConjSet, ParConjList),
        write_indent(Indent, !IO),
        io__write_string("% need in par_conj vars: ", !IO),
        write_vars(ParConjList, VarSet, AppendVarNums, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ).

:- pred write_vars(list(prog_var)::in, prog_varset::in, bool::in,
    io::di, io::uo) is det.

write_vars([], _, _, !IO).
write_vars([Var], VarSet, AppendVarNums, !IO) :-
    mercury_output_var(Var, VarSet, AppendVarNums, !IO).
write_vars([Var1, Var2 | Vars], VarSet, AppendVarNums, !IO) :-
    mercury_output_var(Var1, VarSet, AppendVarNums, !IO),
    io__write_string(", ", !IO),
    write_vars([Var2 | Vars], VarSet, AppendVarNums, !IO).

:- pred write_varnum_list(list(prog_var)::in, io::di, io::uo) is det.

write_varnum_list([], !IO).
write_varnum_list([Var], !IO) :-
    write_varnum(Var, !IO).
write_varnum_list([Var1, Var2 | Vars], !IO) :-
    write_varnum(Var1, !IO),
    io__write_string(", ", !IO),
    write_varnum_list([Var2 | Vars], !IO).

:- pred write_varnum(var(T)::in, io::di, io::uo) is det.

write_varnum(Var, !IO) :-
    term__var_to_int(Var, VarNum),
    io__write_int(VarNum, !IO).

:- pred write_var_name_list(list(pair(var(T), string))::in,
    io::di, io::uo) is det.

write_var_name_list([], !IO).
write_var_name_list([Var - Name], !IO) :-
    write_varnum(Var, !IO),
    io__write_string(" - ", !IO),
    io__write_string(Name, !IO).

write_var_name_list([Var1 - Name1, VarName2 | Vars], !IO) :-
    write_varnum(Var1, !IO),
    io__write_string(" - ", !IO),
    io__write_string(Name1, !IO),
    io__write_string(", ", !IO),
    write_var_name_list([VarName2 | Vars], !IO).

:- pred write_aditi_builtin(module_info::in, aditi_builtin::in,
    simple_call_id::in, list(prog_var)::in, prog_varset::in, bool::in,
    int::in, string::in, io::di, io::uo) is det.

write_aditi_builtin(_ModuleInfo, aditi_tuple_update(InsertDelete, PredId),
        CallId, ArgVars, VarSet, AppendVarNums, Indent, Follow, !IO) :-
    % make_hlds.m checks the arity so this cannot fail.
    get_state_args_det(ArgVars, Args, State0Var, StateVar),
    write_indent(Indent, !IO),
    (
        InsertDelete = insert,
        io__write_string("aditi_insert(", !IO)
    ;
        InsertDelete = delete,
        io__write_string("aditi_delete(", !IO)
    ),
    CallId = PredOrFunc - SymName / _,
    (
        PredOrFunc = predicate,
        write_sym_name_and_args(SymName, Args, VarSet, AppendVarNums, !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Args, FuncArgs, RetArg),
        io__write_string("(", !IO),
        write_sym_name_and_args(SymName, FuncArgs, VarSet,
            AppendVarNums, !IO),
        io__write_string(" = ", !IO),
        mercury_output_var(RetArg, VarSet, AppendVarNums, !IO),
        io__write_string(")", !IO)
    ),
    io__write_string(", ", !IO),
    mercury_output_var(State0Var, VarSet, AppendVarNums, !IO),
    io__write_string(", ", !IO),
    mercury_output_var(StateVar, VarSet, AppendVarNums, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO),
    io__nl(!IO),
    write_aditi_builtin_pred_id(Indent, PredId, !IO).

write_aditi_builtin(_ModuleInfo, Builtin, CallId, ArgVars, VarSet,
        AppendVarNums, Indent, Follow, !IO) :-
    Builtin = aditi_bulk_update(_, PredId, _Syntax),
    write_indent(Indent, !IO),
    aditi_builtin_name(Builtin, UpdateName),
    io__write_string(UpdateName, !IO),
    io__write_string("(", !IO),
    CallId = PredOrFunc - _,
    PredOrFuncStr = prog_out__pred_or_func_to_str(PredOrFunc),
    io__write_string(PredOrFuncStr, !IO),
    io__write_string(" ", !IO),
    simple_call_id_to_sym_name_and_arity(CallId, SymArity),
    prog_out__write_sym_name_and_arity(SymArity, !IO),
    io__write_string(", ", !IO),
    mercury_output_vars(ArgVars, VarSet, AppendVarNums, !IO),
    io__write_string(")", !IO),
    io__write_string(Follow, !IO),
    io__nl(!IO),
    write_aditi_builtin_pred_id(Indent, PredId, !IO).

:- pred write_aditi_builtin_pred_id(int::in, pred_id::in,
    io::di, io::uo) is det.

write_aditi_builtin_pred_id(Indent, PredId, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% Update of pred_id: ", !IO),
    pred_id_to_int(PredId, PredInt),
    io__write_int(PredInt, !IO),
    io__write_string(".\n", !IO).

aditi_builtin_name(aditi_tuple_update(_, _), "aditi_insert").
aditi_builtin_name(aditi_bulk_update(Update, _, _), Name) :-
    aditi_bulk_update_name(Update, Name).

:- pred aditi_bulk_update_name(aditi_bulk_update::in, string::out) is det.

aditi_bulk_update_name(bulk_insert, "aditi_bulk_insert").
aditi_bulk_update_name(bulk_delete, "aditi_bulk_delete").
aditi_bulk_update_name(bulk_modify, "aditi_bulk_modify").

:- pred write_unification(unification::in, module_info::in, prog_varset::in,
    inst_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_unification(assign(X, Y), _, ProgVarSet, _InstVarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(X, ProgVarSet, AppendVarNums, !IO),
    io__write_string(" := ", !IO),
    mercury_output_var(Y, ProgVarSet, AppendVarNums, !IO),
    io__write_string("\n", !IO).

write_unification(simple_test(X, Y), _, ProgVarSet, _, AppendVarNums, Indent,
        !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(X, ProgVarSet, AppendVarNums, !IO),
    io__write_string(" == ", !IO),
    mercury_output_var(Y, ProgVarSet, AppendVarNums, !IO),
    io__write_string("\n", !IO).

write_unification(construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
        Uniqueness, SubInfo), ModuleInfo, ProgVarSet, InstVarSet,
        AppendVarNums, Indent, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(Var, ProgVarSet, AppendVarNums, !IO),
    io__write_string(" := ", !IO),
    write_functor_and_submodes(ConsId, ArgVars, ArgModes, ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO),
    (
        Uniqueness = cell_is_unique,
        write_indent(Indent, !IO),
        io__write_string("% cell_is_unique\n", !IO)
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
            io__write_string("% take address fields: ", !IO),
            write_intlist(TakeAddressFields, !IO),
            io__write_string("\n", !IO)
        ;
            MaybeTakeAddr = no
        ),
        (
            MaybeSize = yes(SizeSource),
            write_indent(Indent, !IO),
            io__write_string("% term size ", !IO),
            (
                SizeSource = known_size(KnownSize),
                io__write_string("const ", !IO),
                io__write_int(KnownSize, !IO),
                io__write_string("\n", !IO)
            ;
                SizeSource = dynamic_size(SizeVar),
                io__write_string("var ", !IO),
                mercury_output_var(SizeVar, ProgVarSet, AppendVarNums, !IO),
                io__write_string("\n", !IO)
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
        io__write_string("% construct statically\n", !IO),
        list__foldl(write_static_cons(Indent, 1, ProgVarSet, AppendVarNums),
            StaticConsList, !IO)
    ;
        ConstructHow = reuse_cell(CellToReuse),
        CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds, _FieldAssigns),
        write_indent(Indent, !IO),
        io__write_string("% reuse cell: ", !IO),
        mercury_output_var(ReuseVar, ProgVarSet, AppendVarNums, !IO),
        io__write_string("\n", !IO)
    ).

write_unification(deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail, CanCGC),
        ModuleInfo, ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO) :-
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'G') ->
        write_indent(Indent, !IO),
        io__write_string("% Compile time garbage collect: ", !IO),
        io__write(CanCGC, !IO),
        io__nl(!IO)
    ;
        true
    ),
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(Var, ProgVarSet, AppendVarNums, !IO),
    (
        CanFail = can_fail,
        io__write_string(" ?= ", !IO)
    ;
        CanFail = cannot_fail,
        io__write_string(" => ", !IO)
    ),
    write_functor_and_submodes(ConsId, ArgVars, ArgModes, ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO).

write_unification(complicated_unify(Mode, CanFail, TypeInfoVars), _ModuleInfo,
        ProgVarSet, InstVarSet, AppendVarNums, Indent, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    (
        CanFail = can_fail,
        io__write_string("can_fail, ", !IO)
    ;
        CanFail = cannot_fail,
        io__write_string("cannot_fail, ", !IO)
    ),
    io__write_string("mode: ", !IO),
    mercury_output_uni_mode(Mode, InstVarSet, !IO),
    io__write_string("\n", !IO),
    write_indent(Indent, !IO),
    io__write_string("% type-info vars: ", !IO),
    mercury_output_vars(TypeInfoVars, ProgVarSet, AppendVarNums, !IO),
    io__write_string("\n", !IO).

:- pred write_static_cons(int::in, int::in, prog_varset::in, bool::in,
    static_cons::in, io::di, io::uo) is det.

write_static_cons(Indent, Depth, VarSet, AppendVarNums, StaticCons, !IO) :-
    StaticCons = static_cons(ConsId, ArgVars, ArgStaticConstList),
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    write_indent(Depth, !IO),
    mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
    io__write_string("\n", !IO),
    (
        ArgVars = []
    ;
        ArgVars = [_ | _],
        write_indent(Indent, !IO),
        io__write_string("% ", !IO),
        write_indent(Depth, !IO),
        mercury_output_vars(ArgVars, VarSet, AppendVarNums, !IO),
        io__write_string("\n", !IO)
    ),
    list__foldl(write_static_cons(Indent, Depth + 1, VarSet, AppendVarNums),
        ArgStaticConstList, !IO).

:- pred write_functor_and_submodes(cons_id::in, list(prog_var)::in,
    list(uni_mode)::in, module_info::in, prog_varset::in, inst_varset::in,
    bool::in, int::in, io::di, io::uo) is det.

write_functor_and_submodes(ConsId, ArgVars, ArgModes, _ModuleInfo, ProgVarSet,
        InstVarSet, AppendVarNums, Indent, !IO) :-
    write_cons_id(ConsId, !IO),
    (
        ArgVars = [],
        io__write_string("\n", !IO)
    ;
        ArgVars = [_ | _],
        io__write_string(" (", !IO),
        mercury_output_vars(ArgVars, ProgVarSet, AppendVarNums, !IO),
        io__write_string(")\n", !IO),
        globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
        ( string__contains_char(Verbose, 'a') ->
            write_indent(Indent, !IO),
            io__write_string("% arg-modes ", !IO),
            mercury_output_uni_mode_list(ArgModes, InstVarSet, !IO),
            io__write_string("\n", !IO)
        ;
            true
        )
    ).

write_unify_rhs(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums, Indent,
        !IO) :-
    write_unify_rhs_3(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums,
        Indent, no, no, !IO).

:- pred write_unify_rhs_2(unify_rhs::in, module_info::in,
    prog_varset::in, inst_varset::in, bool::in, int::in, string::in,
    maybe(mer_type)::in, maybe_vartypes::in, io::di, io::uo) is det.

write_unify_rhs_2(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums, Indent,
        Follow, MaybeType, TypeQual, !IO) :-
    write_unify_rhs_3(RHS, ModuleInfo, VarSet, InstVarSet, AppendVarNums,
        Indent, MaybeType, TypeQual, !IO),
    io__write_string(Follow, !IO).

:- pred write_unify_rhs_3(unify_rhs::in, module_info::in,
    prog_varset::in, inst_varset::in, bool::in, int::in, maybe(mer_type)::in,
    maybe_vartypes::in, io::di, io::uo) is det.

write_unify_rhs_3(var(Var), _, VarSet, _, AppendVarNums, _, _, _, !IO) :-
    mercury_output_var(Var, VarSet, AppendVarNums, !IO).
write_unify_rhs_3(functor(ConsId0, IsExistConstruct, ArgVars), ModuleInfo,
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
        TypeQual = yes(TVarSet, _)
    ->
        io__write_string(" : ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        true
    ).
write_unify_rhs_3(lambda_goal(Purity, PredOrFunc, EvalMethod, _, NonLocals,
        Vars, Modes, Det, Goal), ModuleInfo, VarSet, InstVarSet, AppendVarNums,
        Indent, MaybeType, TypeQual, !IO) :-
    Indent1 = Indent + 1,
    write_purity_prefix(Purity, !IO),
    (
        EvalMethod = lambda_normal,
        EvalStr = ""
    ;
        EvalMethod = lambda_aditi_bottom_up,
        EvalStr = "aditi_bottom_up "
    ),
    (
        PredOrFunc = predicate,
        io__write_string("(", !IO),
        io__write_string(EvalStr, !IO),
        (
            Vars = [],
            io__write_string("(pred)", !IO)
        ;
            Vars = [_ | _],
            io__write_string("pred(", !IO),
            write_var_modes(Vars, Modes, VarSet, InstVarSet, AppendVarNums,
                !IO),
            io__write_string(")", !IO)
        ),
        io__write_string(" is ", !IO),
        mercury_output_det(Det, !IO),
        io__write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
            TypeQual, !IO),
        write_indent(Indent, !IO),
        io__write_string(")", !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        pred_args_to_func_args(Vars, ArgVars, RetVar),
        io__write_string("(", !IO),
        io__write_string(EvalStr, !IO),
        (
            ArgVars = [],
            io__write_string("(func)", !IO)
        ;
            ArgVars = [_ | _],
            io__write_string("func(", !IO),
            write_var_modes(ArgVars, ArgModes, VarSet, InstVarSet,
                AppendVarNums, !IO),
            io__write_string(")", !IO)
        ),
        io__write_string(" = (", !IO),
        write_var_mode(VarSet, InstVarSet, AppendVarNums,
            RetVar - RetMode, !IO),
        io__write_string(") is ", !IO),
        mercury_output_det(Det, !IO),
        io__write_string(" :-\n", !IO),
        write_goal_a(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, "\n",
            TypeQual, !IO),
        write_indent(Indent, !IO),
        io__write_string(")", !IO)
    ),
    (
        MaybeType = yes(Type),
        TypeQual = yes(TVarSet, _)
    ->
        io__write_string(" : ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        true
    ),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'n') ->
        (
            NonLocals = [_ | _],
            write_indent(Indent1, !IO),
            io__write_string("% lambda nonlocals: ", !IO),
            mercury_output_vars(NonLocals, VarSet, AppendVarNums, !IO)
        ;
            NonLocals = []
        )
    ;
        true
    ).

unify_rhs_to_string(var(Var), _ModuleInfo, VarSet, AppendVarNums)
    = mercury_var_to_string(Var, VarSet, AppendVarNums).
unify_rhs_to_string(functor(ConsId0, IsExistConstruct, ArgVars), ModuleInfo,
        VarSet, AppendVarNums) = Str :-
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
unify_rhs_to_string(lambda_goal(_, _, _, _, _, _, _, _, _), _, _, _)
    = "lambda goal".

:- pred write_sym_name_and_args(sym_name::in, list(prog_var)::in,
    prog_varset::in, bool::in, io::di, io::uo) is det.

write_sym_name_and_args(PredName, ArgVars, VarSet, AppendVarNums, !IO) :-
    (
        PredName = qualified(ModuleName, Name),
        write_qualified_functor(ModuleName, term__atom(Name), ArgVars, VarSet,
            AppendVarNums, !IO)
    ;
        PredName = unqualified(Name),
        write_functor(term__atom(Name), ArgVars, VarSet, AppendVarNums,
            next_to_graphic_token, !IO)
    ).

write_functor(Functor, ArgVars, VarSet, AppendVarNums, !IO) :-
    write_functor(Functor, ArgVars, VarSet, AppendVarNums,
        not_next_to_graphic_token, !IO).

functor_to_string(Functor, ArgVars, VarSet, AppendVarNums) =
    functor_to_string(Functor, ArgVars, VarSet, AppendVarNums,
        not_next_to_graphic_token).

:- pred write_functor(const::in, list(prog_var)::in, prog_varset::in,
    bool::in, needs_quotes::in, io::di, io::uo) is det.

write_functor(Functor, ArgVars, VarSet, AppendVarNums, NextToGraphicToken,
        !IO) :-
    io__write_string(functor_to_string(Functor, ArgVars, VarSet,
        AppendVarNums, NextToGraphicToken), !IO).

:- func functor_to_string(const, list(prog_var), prog_varset,
    bool, needs_quotes) = string.

functor_to_string(Functor, ArgVars, VarSet, AppendVarNums, NextToGraphicToken)
        = Str :-
    term__context_init(Context),
    term__var_list_to_term_list(ArgVars, ArgTerms),
    Term = term__functor(Functor, ArgTerms, Context),
    Str = mercury_term_to_string(Term, VarSet, AppendVarNums,
        NextToGraphicToken).

:- pred write_qualified_functor(module_name::in, const::in,
    list(prog_var)::in, prog_varset::in, bool::in, io::di, io::uo) is det.

write_qualified_functor(ModuleName, Functor, ArgVars, VarSet, AppendVarNums,
        !IO) :-
    io__write_string(qualified_functor_to_string(ModuleName, Functor,
        ArgVars, VarSet, AppendVarNums), !IO).

:- func qualified_functor_to_string(module_name, const,
    list(prog_var), prog_varset, bool) = string.

qualified_functor_to_string(ModuleName, Functor, ArgVars, VarSet,
        AppendVarNums) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    FunctorStr = functor_to_string(Functor, ArgVars, VarSet,
        AppendVarNums, next_to_graphic_token),
    Str = ModuleNameStr ++ "." ++ FunctorStr.

:- pred write_qualified_functor_with_term_args(module_name::in, const::in,
    list(prog_term)::in, prog_varset::in, bool::in, io::di, io::uo) is det.

write_qualified_functor_with_term_args(ModuleName, Functor, ArgTerms, VarSet,
        AppendVarNums, !IO) :-
    io__write_string(qualified_functor_with_term_args_to_string(ModuleName,
        Functor, ArgTerms, VarSet, AppendVarNums), !IO).

:- func qualified_functor_with_term_args_to_string(module_name,
    const, list(prog_term), prog_varset, bool) = string.

qualified_functor_with_term_args_to_string(ModuleName, Functor, ArgTerms,
        VarSet, AppendVarNums) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    term__context_init(Context),
    TermStr = mercury_term_to_string(term__functor(Functor, ArgTerms, Context),
        VarSet, AppendVarNums, next_to_graphic_token),
    Str = ModuleNameStr ++ "." ++ TermStr.

write_functor_cons_id(ConsId, ArgVars, VarSet, ModuleInfo, AppendVarNums,
        !IO) :-
    io__write_string(functor_cons_id_to_string(ConsId, ArgVars,
        VarSet, ModuleInfo, AppendVarNums), !IO).

functor_cons_id_to_string(ConsId, ArgVars, VarSet, ModuleInfo, AppendVarNums)
        = Str :-
    (
        ConsId = cons(SymName, _),
        (
            SymName = qualified(Module, Name),
            Str = qualified_functor_to_string(Module, term__atom(Name),
                ArgVars, VarSet, AppendVarNums)
        ;
            SymName = unqualified(Name),
            Str = functor_to_string(term__atom(Name), ArgVars, VarSet,
                AppendVarNums, next_to_graphic_token)
        )
    ;
        ConsId = int_const(Int),
        Str = functor_to_string(term__integer(Int), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = float_const(Float),
        Str = functor_to_string(term__float(Float), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = string_const(String),
        Str = functor_to_string(term__string(String), ArgVars, VarSet,
            AppendVarNums)
    ;
        ConsId = pred_const(ShroudedPredProcId, _),
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        Str = functor_cons_id_to_string(cons(qualified(PredModule, PredName),
            list__length(ArgVars)), ArgVars, VarSet, ModuleInfo, AppendVarNums)
    ;
        ConsId = type_ctor_info_const(Module, Name, Arity),
        Str = "type_ctor_info("""
            ++ prog_out__sym_name_to_escaped_string(Module)
            ++ """, """ ++ Name ++ """, " ++ int_to_string(Arity) ++ ")"
    ;
        ConsId = base_typeclass_info_const(Module, class_id(Name, Arity), _,
            Instance),
        Str = "base_typeclass_info("""
            ++ prog_out__sym_name_to_escaped_string(Module) ++ """, "
            ++ "class_id(" ++ prog_out__sym_name_to_escaped_string(Name)
            ++ ", " ++ int_to_string(Arity) ++ "), " ++ Instance ++ ")"
    ;
        ConsId = type_info_cell_constructor(_),
        Str = functor_to_string(
            term__atom("type_info_cell_constructor"),
            ArgVars, VarSet, AppendVarNums, next_to_graphic_token)
    ;
        ConsId = typeclass_info_cell_constructor,
        Str = functor_to_string(
            term__atom("typeclass_info_cell_constructor"),
            ArgVars, VarSet, AppendVarNums, next_to_graphic_token)
    ;
        ConsId = tabling_pointer_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = "tabling_pointer_const("
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
    io__write_string(var_modes_to_string(Vars, Modes, VarSet,
        InstVarSet, AppendVarNums), !IO).

var_modes_to_string(Vars, Modes, VarSet, InstVarSet, AppendVarNums) = Str :-
    assoc_list__from_corresponding_lists(Vars, Modes, VarModes),
    Strs = list__map(var_mode_to_string(VarSet, InstVarSet, AppendVarNums),
        VarModes),
    Str = string__join_list(", ", Strs).

:- pred write_var_mode(prog_varset::in, inst_varset::in, bool::in,
    pair(prog_var, mer_mode)::in, io::di, io::uo) is det.

write_var_mode(VarSet, InstVarSet, AppendVarNums, Var - Mode, !IO) :-
    io__write_string(var_mode_to_string(VarSet, InstVarSet, AppendVarNums,
        Var - Mode), !IO).

:- func var_mode_to_string(prog_varset, inst_varset, bool,
    pair(prog_var, mer_mode)) = string.

var_mode_to_string(VarSet, InstVarSet, AppendVarNums, Var - Mode) =
    mercury_var_to_string(Var, VarSet, AppendVarNums)
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
            io__write_string(Separator, !IO)
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
        io__write_string(Separator, !IO),
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
    io__write_string("% ", !IO),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string(" has functor ", !IO),
    write_cons_id(ConsId, !IO),
    io__write_string("\n", !IO),
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
        io__write_string(";\n", !IO),
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
    ( instmap__is_unreachable(InstMap) ->
        io__write_string("unreachable", !IO)
    ;
        instmap__to_assoc_list(InstMap, AssocList),
        write_instmap_2(AssocList, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_2(assoc_list(prog_var, mer_inst)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_instmap_2([], _, _, _, !IO).
write_instmap_2([Var - Inst | Rest], VarSet, AppendVarNums, Indent, !IO) :-
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string(" -> ", !IO),
    varset__init(InstVarSet),
    mercury_output_inst(Inst, InstVarSet, !IO),
    (
        Rest = []
    ;
        Rest = [_ | _],
        mercury_output_newline(Indent, !IO),
        io__write_string("%            ", !IO),
        write_instmap_2(Rest, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_delta(instmap_delta::in, prog_varset::in, bool::in,
    int::in, io::di, io::uo) is det.

write_instmap_delta(InstMapDelta, VarSet, AppendVarNums, Indent, !IO) :-
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        io__write_string("unreachable", !IO)
    ;
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        write_instmap_2(AssocList, VarSet, AppendVarNums, Indent, !IO)
    ).

:- pred write_instmap_delta_vars(instmap_delta::in, prog_varset::in,
    bool::in, io::di, io::uo) is det.

write_instmap_delta_vars(InstMapDelta, VarSet, AppendVarNums, !IO) :-
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        io__write_string("unreachable", !IO)
    ;
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        assoc_list__keys(AssocList, Vars),
        write_vars(Vars, VarSet, AppendVarNums, !IO)
    ).

write_import_status(Status, !IO) :-
    io__write_string(import_status_to_string(Status), !IO).

import_status_to_string(local) =
    "local".
import_status_to_string(exported) =
    "exported".
import_status_to_string(opt_exported) =
    "opt_exported".
import_status_to_string(abstract_exported) =
    "abstract_exported".
import_status_to_string(pseudo_exported) =
    "pseudo_exported".
import_status_to_string(imported(interface)) =
    "imported in the interface".
import_status_to_string(imported(implementation)) =
    "imported in the implementation".
import_status_to_string(imported(ancestor_private_interface)) =
    "imported from an ancestor's private interface".
import_status_to_string(imported(ancestor)) =
    "imported by an ancestor".
import_status_to_string(external(Status)) =
    "external (and " ++ import_status_to_string(Status) ++ ")".
import_status_to_string(abstract_imported) =
    "abstract_imported".
import_status_to_string(opt_imported) =
    "opt_imported".
import_status_to_string(pseudo_imported) =
    "pseudo_imported".
import_status_to_string(exported_to_submodules) =
    "exported_to_submodules".

:- pred write_type_list(list(mer_type)::in, tvarset::in, bool::in,
    io::di, io::uo) is det.

write_type_list(Types, TypeVarSet, AppendVarNums, !IO) :-
    list__foldl(output_type_and_comma(TypeVarSet, AppendVarNums), Types, !IO).

:- pred output_type_and_comma(tvarset::in, bool::in, mer_type::in,
    io::di, io::uo) is det.

output_type_and_comma(TypeVarSet, AppendVarNums, Type, !IO) :-
    mercury_output_type(TypeVarSet, AppendVarNums, Type, !IO),
    io__write_string(", ", !IO).

:- pred write_var_types(int::in, prog_varset::in, bool::in,
    vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% variable types map:\n", !IO),
    map__keys(VarTypes, Vars),
    write_var_types_2(Vars, Indent, VarSet, AppendVarNums, VarTypes, TVarSet,
        !IO).

:- pred write_var_types_2(list(prog_var)::in, int::in, prog_varset::in,
    bool::in, vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types_2([], _, _, _, _, _, !IO).
write_var_types_2([Var | Vars], Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO) :-
    map__lookup(VarTypes, Var, Type),
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string(" (number ", !IO),
    term__var_to_int(Var, VarNum),
    io__write_int(VarNum, !IO),
    io__write_string(")", !IO),
    io__write_string(": ", !IO),
    mercury_output_type(TypeVarSet, AppendVarNums, Type, !IO),
    io__write_string("\n", !IO),
    write_var_types_2(Vars, Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO).

:- pred write_rtti_varmaps(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, io::di, io::uo) is det.

write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% type_info varmap:\n", !IO),
    rtti_varmaps_tvars(RttiVarMaps, TypeVars),
    list__foldl(write_type_info_locn(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), TypeVars, !IO),
    write_indent(Indent, !IO),
    io__write_string("% typeclass_info varmap:\n", !IO),
    rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
    list__foldl(write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), Constraints, !IO),
    write_indent(Indent, !IO),
    io__write_string("% rtti_var_info:\n", !IO),
    rtti_varmaps_rtti_prog_vars(RttiVarMaps, ProgVars),
    list__foldl(write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), ProgVars, !IO).

:- pred write_type_info_locn(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, tvar::in, io::di, io::uo) is det.

write_type_info_locn(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, TVar,
        !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),

    mercury_output_var(TVar, TVarSet, AppendVarNums, !IO),
    io__write_string(" (number ", !IO),
    term__var_to_int(TVar, TVarNum),
    io__write_int(TVarNum, !IO),
    io__write_string(")", !IO),

    io__write_string(" -> ", !IO),
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    (
        Locn = type_info(Var),
        io__write_string("type_info(", !IO),
        mercury_output_var(Var, VarSet, AppendVarNums, !IO),
        io__write_string(") ", !IO)
    ;
        Locn = typeclass_info(Var, Index),
        io__write_string("typeclass_info(", !IO),
        mercury_output_var(Var, VarSet, AppendVarNums, !IO),
        io__write_string(", ", !IO),
        io__write_int(Index, !IO),
        io__write_string(") ", !IO)
    ),
    io__write_string(" (number ", !IO),
    term__var_to_int(Var, VarNum),
    io__write_int(VarNum, !IO),
    io__write_string(")", !IO),
    io__write_string("\n", !IO).

:- pred write_typeclass_info_var(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_constraint::in, io::di, io::uo) is det.

write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
        Constraint, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO),
    io__write_string(" -> ", !IO),
    rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__nl(!IO).

:- pred write_rtti_var_info(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_var::in, io::di, io::uo) is det.

write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, Var,
        !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string(" (number ", !IO),
    term__var_to_int(Var, VarNum),
    io__write_int(VarNum, !IO),
    io__write_string(") ", !IO),
    io__write_string(" -> ", !IO),
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        io__write_string("type_info for ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        VarInfo = typeclass_info_var(Constraint),
        io__write_string("typeclass_info for", !IO),
        mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO)
    ;
        VarInfo = non_rtti_var,
        unexpected(this_file, "write_rtti_var_info: non rtti var")
    ),
    io__nl(!IO).

:- pred write_stack_slots(int::in, stack_slots::in, prog_varset::in,
    bool::in, io::di, io::uo) is det.

write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO) :-
    map__to_assoc_list(StackSlots, VarSlotList0),
    VarSlotList = assoc_list__map_values(key_stack_slot_to_abs_locn,
        VarSlotList0),
    write_var_to_abs_locns(VarSlotList, VarSet, AppendVarNums, Indent, !IO).

:- pred write_var_to_abs_locns(assoc_list(prog_var, abs_locn)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

write_var_to_abs_locns([], _, _, _, !IO).
write_var_to_abs_locns([Var - Loc | VarLocs], VarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%\t", !IO),
    mercury_output_var(Var, VarSet, AppendVarNums, !IO),
    io__write_string("\t-> ", !IO),
    io__write_string(abs_locn_to_string(Loc), !IO),
    io__write_string("\n", !IO),
    write_var_to_abs_locns(VarLocs, VarSet, AppendVarNums, Indent, !IO).

:- pred write_untuple_info(untuple_proc_info::in, prog_varset::in,
    bool::in, int::in, io::di, io::uo) is det.

write_untuple_info(untuple_proc_info(UntupleMap), VarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% untuple:\n", !IO),
    map__foldl(write_untuple_info_2(VarSet, AppendVarNums, Indent), UntupleMap,
        !IO).

:- pred write_untuple_info_2(prog_varset::in, bool::in, int::in,
    prog_var::in, prog_vars::in, io::di, io::uo) is det.

write_untuple_info_2(VarSet, AppendVarNums, Indent, OldVar, NewVars, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%\t", !IO),
    mercury_output_var(OldVar, VarSet, AppendVarNums, !IO),
    io__write_string("\t-> ", !IO),
    mercury_output_vars(NewVars, VarSet, AppendVarNums, !IO),
    io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_types(int::in, type_table::in, io::di, io::uo) is det.

write_types(Indent, TypeTable, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%-------- Types --------\n", !IO),
    map__to_assoc_list(TypeTable, TypeAssocList),
    write_types_2(Indent, TypeAssocList, !IO).

:- pred write_types_2(int::in, assoc_list(type_ctor, hlds_type_defn)::in,
    io::di, io::uo) is det.

write_types_2(_Indent, [], !IO).
write_types_2(Indent, [TypeCtor - TypeDefn | Types], !IO) :-
    hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data__get_type_defn_body(TypeDefn, TypeBody),
    hlds_data__get_type_defn_status(TypeDefn, Status),
    hlds_data__get_type_defn_context(TypeDefn, Context),

    % Write the context.
    io__write_char('\n', !IO),
    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'c') ->
        term__context_file(Context, FileName),
        term__context_line(Context, LineNumber),
        ( FileName \= "" ->
            write_indent(Indent, !IO),
            io__write_string("% context: file `", !IO),
            io__write_string(FileName, !IO),
            io__write_string("', line ", !IO),
            io__write_int(LineNumber, !IO),
            io__write_string(", status ", !IO),
            write_import_status(Status, !IO),
            io__write_char('\n', !IO)
        ;
            true
        )
    ;
        true
    ),

    write_indent(Indent, !IO),
    (
        ( TypeBody = solver_type(_, _)
        ; TypeBody = abstract_type(solver_type)
        )
    ->
        io__write_string(":- solver type ", !IO)
    ;
        io__write_string(":- type ", !IO)
    ),
    write_type_name(TypeCtor, !IO),
    write_type_params(TVarSet, TypeParams, !IO),
    write_type_body(Indent + 1, TVarSet, TypeBody, !IO),
    write_types_2(Indent, Types, !IO).

:- pred write_type_name(type_ctor::in, io::di, io::uo) is det.

write_type_name(Name - _Arity, !IO) :-
    prog_out__write_sym_name(Name, !IO).

:- func type_name_to_string(type_ctor) = string.

type_name_to_string(Name - _Arity) =
    prog_out__sym_name_to_escaped_string(Name).

:- pred write_type_params(tvarset::in, list(type_param)::in,
    io::di, io::uo) is det.

write_type_params(_TVarSet, [], !IO).
write_type_params(TVarSet, [P], !IO) :-
    io__write_string("(", !IO),
    mercury_output_var(P, TVarSet, no, !IO),
    io__write_string(")", !IO).
write_type_params(TVarSet, [P | Ps], !IO) :-
    Ps = [_ | _],
    io__write_string("(", !IO),
    mercury_output_var(P, TVarSet, no, !IO),
    write_type_params_2(TVarSet, Ps, !IO).

:- pred write_type_params_2(tvarset::in, list(type_param)::in,
    io::di, io::uo) is det.

write_type_params_2(_TVarSet, [], !IO) :-
    io__write_string(")", !IO).
write_type_params_2(TVarSet, [P | Ps], !IO) :-
    io__write_string(", ", !IO),
    mercury_output_var(P, TVarSet, no, !IO),
    write_type_params_2(TVarSet, Ps, !IO).

:- pred write_type_body(int::in, tvarset::in, hlds_type_body::in,
    io::di, io::uo) is det.

write_type_body(Indent, TVarSet, du_type(Ctors, Tags, EnumDummy,
        MaybeUserEqComp, ReservedTag, Foreign), !IO) :-
    io__write_string(" --->\n", !IO),
    (
        EnumDummy = is_enum,
        write_indent(Indent, !IO),
        io__write_string("/* enumeration */\n", !IO)
    ;
        EnumDummy = is_dummy,
        write_indent(Indent, !IO),
        io__write_string("/* dummy */\n", !IO)
    ;
        EnumDummy = not_enum_or_dummy
    ),
    (
        ReservedTag = yes,
        write_indent(Indent, !IO),
        io__write_string("/* reserved_tag */\n", !IO)
    ;
        ReservedTag = no
    ),
    write_constructors(Indent, TVarSet, Ctors, Tags, !IO),
    mercury_output_where_attributes(TVarSet, no, MaybeUserEqComp, !IO),
    (
        Foreign = yes(_),
        write_indent(Indent, !IO),
        io__write_string("/* has foreign_type */\n", !IO)
    ;
        Foreign = no
    ),
    io__write_string(".\n", !IO).

write_type_body(_Indent, TVarSet, eqv_type(Type), !IO) :-
    io__write_string(" == ", !IO),
    mercury_output_type(TVarSet, no, Type, !IO),
    io__write_string(".\n", !IO).

write_type_body(_Indent, _TVarSet, abstract_type(_IsSolverType), !IO) :-
    io__write_string(".\n", !IO).

write_type_body(_Indent, _TVarSet, foreign_type(_), !IO) :-
    % XXX
    io__write_string(" == $foreign_type.\n", !IO).

write_type_body(_Indent, TVarSet,
        solver_type(SolverTypeDetails, MaybeUserEqComp), !IO) :-
    mercury_output_where_attributes(TVarSet, yes(SolverTypeDetails),
        MaybeUserEqComp, !IO),
    io__write_string(".\n", !IO).

:- pred write_constructors(int::in, tvarset::in,
    list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

write_constructors(_Indent, _TVarSet, [], _, !IO) :-
    error("write_constructors: empty constructor list?").
write_constructors(Indent, TVarSet, [C], TagValues, !IO) :-
    write_indent(Indent, !IO),
    io__write_char('\t', !IO),
    write_ctor(C, TVarSet, TagValues, !IO).
write_constructors(Indent, TVarSet, [C | Cs], TagValues, !IO) :-
    Cs = [_ | _],
    write_indent(Indent, !IO),
    io__write_char('\t', !IO),
    write_ctor(C, TVarSet, TagValues, !IO),
    io__write_string("\n", !IO),
    write_constructors_2(Indent, TVarSet, Cs, TagValues, !IO).

:- pred write_constructors_2(int::in, tvarset::in,
    list(constructor)::in, cons_tag_values::in, io::di, io::uo) is det.

write_constructors_2(_Indent, _TVarSet, [], _, !IO).
write_constructors_2(Indent, TVarSet, [C | Cs], TagValues, !IO) :-
    write_indent(Indent, !IO),
    io__write_string(";\t", !IO),
    write_ctor(C, TVarSet, TagValues, !IO),
    (
        Cs = []
    ;
        Cs = [_ | _],
        io__write_string("\n", !IO),
        write_constructors_2(Indent, TVarSet, Cs, TagValues, !IO)
    ).

:- pred write_ctor(constructor::in, tvarset::in, cons_tag_values::in,
    io::di, io::uo) is det.

write_ctor(C, TVarSet, TagValues, !IO) :-
    mercury_output_ctor(C, TVarSet, !IO),
    C = ctor(_, _, Name, Args),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    ( map__search(TagValues, ConsId, TagValue) ->
        io__write_string("\t% tag: ", !IO),
        io__print(TagValue, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred write_classes(int::in, class_table::in, io::di, io::uo) is det.

write_classes(Indent, ClassTable, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%-------- Classes --------\n", !IO),
    map__to_assoc_list(ClassTable, ClassTableList),
    io__write_list(ClassTableList, "\n", write_class_defn(Indent), !IO),
    io__nl(!IO).

:- pred write_class_defn(int::in,
    pair(class_id, hlds_class_defn)::in, io::di, io::uo) is det.

write_class_defn(Indent, ClassId - ClassDefn, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),

    write_class_id(ClassId, !IO),
    io__write_string(":\n", !IO),

    ClassDefn = hlds_class_defn(_, Constraints, FunDeps, _, Vars, _, _,
        Interface, VarSet, Context),

    term__context_file(Context, FileName),
    term__context_line(Context, LineNumber),
    ( FileName \= "" ->
        write_indent(Indent, !IO),
        io__write_string("% context: file `", !IO),
        io__write_string(FileName, !IO),
        io__write_string("', line ", !IO),
        io__write_int(LineNumber, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),

    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),

    write_indent(Indent, !IO),
    io__write_string("% Vars: ", !IO),
    mercury_output_vars(Vars, VarSet, AppendVarNums, !IO),
    io__nl(!IO),

    write_indent(Indent, !IO),
    io__write_string("% Functional dependencies: ", !IO),
    io__write_list(FunDeps, ", ", hlds_output_fundep, !IO),
    io__nl(!IO),

    write_indent(Indent, !IO),
    io__write_string("% Constraints: ", !IO),
    io__write_list(Constraints, ", ",
        mercury_output_constraint(VarSet, AppendVarNums), !IO),
    io__nl(!IO),

    write_indent(Indent, !IO),
    io__write_string("% Class Methods: ", !IO),
    io__write_list(Interface, ", ", write_class_proc, !IO),
    io__nl(!IO).

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
    io__write_string("hlds_class_proc(pred_id:", !IO),
    pred_id_to_int(PredId, PredInt),
    io__write_int(PredInt, !IO),
    io__write_string(", proc_id:", !IO),
    proc_id_to_int(ProcId, ProcInt),
    io__write_int(ProcInt, !IO),
    io__write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- pred write_superclasses(int::in, superclass_table::in, io::di, io::uo)
    is det.

write_superclasses(Indent, SuperClassTable, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%-------- Super Classes --------\n", !IO),
    multi_map__to_assoc_list(SuperClassTable, SuperClassTableList),
    io__write_list(SuperClassTableList, "\n\n", write_superclass(Indent), !IO),
    io__nl(!IO).

:- pred write_superclass(int::in, pair(class_id, list(subclass_details))::in,
    io::di, io::uo) is det.

write_superclass(Indent, ClassId - SubClassDetailsList, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    write_class_id(ClassId, !IO),
    io__write_string(":\n", !IO),

    io__write_list(SubClassDetailsList, "\n",
        write_subclass_details(Indent, ClassId), !IO).

:- pred write_subclass_details(int::in, class_id::in, subclass_details::in,
    io::di, io::uo) is det.

write_subclass_details(Indent, SuperClassId, SubClassDetails, !IO) :-
    SubClassDetails = subclass_details(SuperClassVars, SubClassId,
        SubClassVars, VarSet),

    % Curry the varset for term_io__write_variable/4.
    PrintVar = (pred(VarName::in, IO0::di, IO::uo) is det :-
            term_io__write_variable(VarName, VarSet, IO0, IO)
        ),
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    SubClassId = class_id(SubSymName, _SubArity),
    prog_out__write_sym_name(SubSymName, !IO),
    io__write_char('(', !IO),
    io__write_list(SubClassVars, ", ", PrintVar, !IO),
    io__write_string(") <= ", !IO),

    SuperClassId = class_id(SuperSymName, _SuperArity),
    prog_out__write_sym_name(SuperSymName, !IO),
    io__write_char('(', !IO),
    io__write_list(SuperClassVars, ", ", mercury_output_type(VarSet, no), !IO),
    io__write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- pred write_instances(int::in, instance_table::in, io::di, io::uo) is det.

write_instances(Indent, InstanceTable, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("%-------- Instances --------\n", !IO),
    map__to_assoc_list(InstanceTable, InstanceTableList),
    io__write_list(InstanceTableList, "\n\n",
        write_instance_defns(Indent), !IO),
    io__nl(!IO).

:- pred write_instance_defns(int::in,
    pair(class_id, list(hlds_instance_defn))::in, io::di, io::uo) is det.

write_instance_defns(Indent, ClassId - InstanceDefns, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    write_class_id(ClassId, !IO),
    io__write_string(":\n", !IO),
    io__write_list(InstanceDefns, "\n", write_instance_defn(Indent), !IO).

:- pred write_instance_defn(int::in, hlds_instance_defn::in,
    io::di, io::uo) is det.

write_instance_defn(Indent, InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, _Status,
        Context, Constraints, Types, Body, MaybePredProcIds, VarSet, Proofs),

    term__context_file(Context, FileName),
    term__context_line(Context, LineNumber),
    ( FileName \= "" ->
        write_indent(Indent, !IO),
        io__write_string("% context: file `", !IO),
        io__write_string(FileName, !IO),
        io__write_string("', line ", !IO),
        io__write_int(LineNumber, !IO),
        io__write_string("\n", !IO)
    ;
        true
    ),

    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),

    % Curry the varset for term_io__write_variable/4.
    PrintTerm = (pred(TypeName::in, IO0::di, IO::uo) is det :-
        mercury_output_type(VarSet, AppendVarNums, TypeName, IO0, IO)
    ),
    write_indent(Indent, !IO),
    io__write_string("% Types: ", !IO),
    io__write_list(Types, ", ", PrintTerm, !IO),
    io__nl(!IO),

    write_indent(Indent, !IO),
    io__write_string("% Constraints: ", !IO),
    io__write_list(Constraints, ", ",
        mercury_output_constraint(VarSet, AppendVarNums), !IO),
    io__nl(!IO),

    write_indent(Indent, !IO),
    (
        Body = abstract,
        io__write_string("% abstract", !IO)
    ;
        Body = concrete(Methods),
        io__write_string("% Instance Methods: ", !IO),
        mercury_output_instance_methods(Methods, !IO)
    ),
    io__nl(!IO),

    (
        MaybePredProcIds = yes(PredProcIds),
        write_indent(Indent, !IO),
        io__write_string("% procedures: ", !IO),
        io__write(PredProcIds, !IO),
        io__nl(!IO)
    ;
        MaybePredProcIds = no
    ),
    write_constraint_proofs(Indent, VarSet, Proofs, AppendVarNums,
        !IO),
    io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_insts(int::in, inst_table::in, io::di, io::uo) is det.

write_insts(Indent, _InstTable, !IO) :-
    % XXX fix this up.
    write_indent(Indent, !IO),
    io__write_string("%-------- Insts --------\n", !IO),
    write_indent(Indent, !IO),
    io__write_string("%%% Not yet implemented, sorry.\n", !IO).
    % io__write_string("% ", !IO).
    % io__print(InstTable, !IO),
    % io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_modes(int::in, mode_table::in, io::di, io::uo) is det.

write_modes(Indent, _ModeTable, !IO) :-
    % XXX fix this up.
    write_indent(Indent, !IO),
    io__write_string("%-------- Modes --------\n", !IO),
    write_indent(Indent, !IO),
    io__write_string("%%% Not yet implemented, sorry.\n", !IO).
    % io__write_string("% ", !IO),
    % io__print(ModeTable, !IO),
    % io__nl(!IO).

%-----------------------------------------------------------------------------%

:- pred write_procs(int::in, bool::in, module_info::in, pred_id::in,
    import_status::in, pred_info::in, io::di, io::uo) is det.

write_procs(Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus, PredInfo,
        !IO) :-
    pred_info_procedures(PredInfo, ProcTable),
    ProcIds = pred_info_procids(PredInfo),
    write_procs_2(ProcIds, AppendVarNums, ModuleInfo, Indent, PredId,
        ImportStatus, ProcTable, !IO).

:- pred write_procs_2(list(proc_id)::in, bool::in, module_info::in, int::in,
    pred_id::in, import_status::in, proc_table::in, io::di, io::uo) is det.

write_procs_2([], _, _, _, _, _, _, !IO).
write_procs_2([ProcId | ProcIds], AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO) :-
    map__lookup(ProcTable, ProcId, ProcInfo),
    write_proc(Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, ProcInfo, !IO),
    write_procs_2(ProcIds, AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO).

:- pred write_proc(int::in, bool::in, module_info::in, pred_id::in,
    proc_id::in, import_status::in, proc_info::in, io::di, io::uo) is det.

write_proc(Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, Proc, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_typevarset(PredInfo, TVarSet),
    proc_info_vartypes(Proc, VarTypes),
    proc_info_declared_determinism(Proc, DeclaredDeterminism),
    proc_info_inferred_determinism(Proc, InferredDeterminism),
    proc_info_varset(Proc, VarSet),
    proc_info_headvars(Proc, HeadVars),
    proc_info_argmodes(Proc, HeadModes),
    proc_info_maybe_arglives(Proc, MaybeArgLives),
    proc_info_maybe_arg_info(Proc, MaybeArgInfos),
    proc_info_goal(Proc, Goal),
    proc_info_context(Proc, ModeContext),
    proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize),
    proc_info_get_maybe_termination_info(Proc, MaybeTermination),
    proc_info_rtti_varmaps(Proc, RttiVarMaps),
    proc_info_eval_method(Proc, EvalMethod),
    proc_info_is_address_taken(Proc, IsAddressTaken),
    proc_info_get_call_table_tip(Proc, MaybeCallTableTip),
    proc_info_get_maybe_deep_profile_info(Proc, MaybeDeepProfileInfo),
    proc_info_get_maybe_untuple_info(Proc, MaybeUntupleInfo),
    Indent1 = Indent + 1,

    write_indent(Indent1, !IO),
    io__write_string("% pred id ", !IO),
    pred_id_to_int(PredId, PredInt),
    io__write_int(PredInt, !IO),
    io__nl(!IO),
    write_indent(Indent1, !IO),
    io__write_string("% mode number ", !IO),
    proc_id_to_int(ProcId, ProcInt),
    io__write_int(ProcInt, !IO),
    io__write_string(" of ", !IO),
    write_pred_id(ModuleInfo, PredId, !IO),
    io__write_string(" (", !IO),
    write_determinism(InferredDeterminism, !IO),
    io__write_string("):\n", !IO),

    globals__io_lookup_string_option(dump_hlds_options, Verbose, !IO),
    ( string__contains_char(Verbose, 't') ->
        write_indent(Indent, !IO),
        io__write_string("% Arg size properties: ", !IO),
        write_maybe_arg_size_info(MaybeArgSize, yes, !IO),
        io__nl(!IO),
        write_indent(Indent, !IO),
        io__write_string("% Termination properties: ", !IO),
        write_maybe_termination_info(MaybeTermination, yes, !IO),
        io__nl(!IO)
    ;
        true
    ),

    write_indent(Indent, !IO),
    write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),
    write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
        !IO),

    (
        IsAddressTaken = address_is_taken,
        io__write_string("% address is taken\n", !IO)
    ;
        IsAddressTaken = address_is_not_taken,
        io__write_string("% address is not taken\n", !IO)
    ),

    ( EvalMethod = eval_normal ->
        true
    ;
        io__write_string("% eval method: ", !IO),
        write_eval_method(EvalMethod, !IO),
        io__write_string("\n", !IO)
    ),

    (
        MaybeCallTableTip = yes(CallTableTip),
        io__write_string("% call table tip: ", !IO),
        mercury_output_var(CallTableTip, VarSet, AppendVarNums, !IO),
        io__write_string("\n", !IO)
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
            io__write_string("% deep recursion info: ", !IO),
            (
                Role = inner_proc(DeepPredProcId),
                io__write_string("inner, outer is ", !IO)
            ;
                Role = outer_proc(DeepPredProcId),
                io__write_string("outer, inner is ", !IO)
            ),
            DeepPredProcId = proc(DeepPredId, DeepProcId),
            pred_id_to_int(DeepPredId, DeepPredInt),
            proc_id_to_int(DeepProcId, DeepProcInt),
            io__write_int(DeepPredInt, !IO),
            io__write_string("/", !IO),
            io__write_int(DeepProcInt, !IO),
            io__write_string("\n", !IO)
        ;
            MaybeRecInfo = no
        ),
        (
            MaybeDeepLayout = yes(DeepLayout),
            DeepLayout = hlds_deep_layout(_, ExcpVars),
            ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
                MaybeOldOutermost),
            io__write_string("% deep layout info: ", !IO),
            io__write_string("TopCSD is ", !IO),
            mercury_output_var(TopCSD, VarSet, AppendVarNums, !IO),
            io__write_string(", MiddleCSD is ", !IO),
            mercury_output_var(MiddleCSD, VarSet, AppendVarNums, !IO),
            (
                MaybeOldOutermost = yes(OldOutermost),
                io__write_string(", OldOutermost is ", !IO),
                mercury_output_var(OldOutermost, VarSet, AppendVarNums, !IO)
            ;
                MaybeOldOutermost = no
            ),
            io__write_string("\n", !IO)
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
    predicate_name(ModuleInfo, PredId, PredName),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    varset__init(ModeVarSet),
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
        io__write_string("% arg lives: ", !IO),
        io__print(ArgLives, !IO),
        io__nl(!IO)
    ;
        MaybeArgLives = no
    ),

    (
        string__contains_char(Verbose, 'A'),
        MaybeArgInfos = yes(ArgInfos)
    ->
        write_indent(Indent, !IO),
        io__write_string("% arg_infos: ", !IO),
        io__print(ArgInfos, !IO),
        io__nl(!IO)
    ;
        true
    ),

    (
        ImportStatus = pseudo_imported,
        hlds_pred__in_in_unification_proc_id(ProcId)
    ->
        true
    ;
        proc_info_stack_slots(Proc, StackSlots),
        write_indent(Indent, !IO),
        write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO),
        write_indent(Indent, !IO),
        term__var_list_to_term_list(HeadVars, HeadTerms),
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadTerms, PredOrFunc, !IO),
        io__write_string(" :-\n", !IO),
        write_goal(Goal, ModuleInfo, VarSet, AppendVarNums, Indent1, ".\n",
            !IO)
    ).

write_determinism(Detism, !IO) :-
    io__write_string(determinism_to_string(Detism), !IO).

determinism_to_string(det) = "det".
determinism_to_string(semidet) = "semidet".
determinism_to_string(nondet) = "nondet".
determinism_to_string(multidet) = "multi".
determinism_to_string(cc_nondet) = "cc_nondet".
determinism_to_string(cc_multidet) = "cc_multi".
determinism_to_string(erroneous) = "erroneous".
determinism_to_string(failure) = "failure".

write_can_fail(CanFail, !IO) :-
    io__write_string(can_fail_to_string(CanFail), !IO).

can_fail_to_string(can_fail) = "can_fail".
can_fail_to_string(cannot_fail) = "cannot_fail".

write_eval_method(EvalMethod, !IO) :-
    io__write_string(eval_method_to_one_string(EvalMethod), !IO).

%-----------------------------------------------------------------------------%

:- pred write_indent(int::in, io::di, io::uo) is det.

write_indent(Indent, !IO) :-
    ( Indent = 0 ->
        true
    ;
        io__write_char('\t', !IO),
        write_indent(Indent - 1, !IO)
    ).

:- pred write_intlist(list(int)::in, io::di, io::uo) is det.

write_intlist(IntList, !IO) :-
    (
        IntList = [],
        io__write_string("[]", !IO)
    ;
        IntList = [H | T],
        io__write_string("[", !IO),
        write_intlist_2(H, T, !IO),
        io__write_string("]", !IO)
    ).

:- pred write_intlist_2(int::in, list(int)::in, io::di, io::uo)
    is det.

write_intlist_2(H, T, !IO) :-
    io__write_int(H, !IO),
    (
        T = [TH | TT],
        io__write_string(", ", !IO),
        write_intlist_2(TH, TT, !IO)
    ;
        T = []
    ).

%-----------------------------------------------------------------------------%

:- pred write_constraint_proofs(int::in, tvarset::in,
    constraint_proof_map::in, bool::in, io::di, io::uo) is det.

write_constraint_proofs(Indent, VarSet, Proofs, AppendVarNums, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% Proofs: \n", !IO),
    map__to_assoc_list(Proofs, ProofsList),
    io__write_list(ProofsList, "\n",
        write_constraint_proof(Indent, VarSet, AppendVarNums), !IO).

:- pred write_constraint_proof(int::in, tvarset::in, bool::in,
    pair(prog_constraint, constraint_proof)::in, io::di, io::uo) is det.

write_constraint_proof(Indent, VarSet, AppendVarNums, Constraint - Proof,
        !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    mercury_output_constraint(VarSet, AppendVarNums, Constraint, !IO),
    io__write_string(": ", !IO),
    (
        Proof = apply_instance(Num),
        io__write_string("apply instance decl #", !IO),
        io__write_int(Num, !IO)
    ;
        Proof = superclass(Super),
        io__write_string("super class of ", !IO),
        mercury_output_constraint(VarSet, AppendVarNums, Super, !IO)
    ).

:- pred write_constraint_map(int::in, tvarset::in,
    constraint_map::in, bool::in, io::di, io::uo) is det.

write_constraint_map(Indent, VarSet, ConstraintMap, AppendVarNums, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% Constraint Map:\n", !IO),
    map__foldl(write_constraint_map_2(Indent, VarSet, AppendVarNums),
        ConstraintMap, !IO).

:- pred write_constraint_map_2(int::in, tvarset::in, bool::in,
    constraint_id::in, prog_constraint::in, io::di, io::uo) is det.

write_constraint_map_2(Indent, VarSet, AppendVarNums, ConstraintId,
        ProgConstraint, !IO) :-
    write_indent(Indent, !IO),
    io__write_string("% ", !IO),
    write_constraint_id(ConstraintId, !IO),
    io__write_string(": ", !IO),
    mercury_output_constraint(VarSet, AppendVarNums, ProgConstraint, !IO),
    io__nl(!IO).

:- pred write_constraint_id(constraint_id::in, io::di, io::uo) is det.

write_constraint_id(ConstraintId, !IO) :-
    ConstraintId = constraint_id(ConstraintType, GoalPath, N),
    (
        ConstraintType = assumed,
        io__write_string("(E, ", !IO)
    ;
        ConstraintType = unproven,
        io__write_string("(A, ", !IO)
    ),
    goal_path_to_string(GoalPath, GoalPathStr),
    io__write_strings(["""", GoalPathStr, """, "], !IO),
    io__write_int(N, !IO),
    io__write_char(')', !IO).

%-----------------------------------------------------------------------------%

:- func add_mode_qualifier(prog_context, pair(prog_term, mer_mode))
    = prog_term.

add_mode_qualifier(Context, HeadTerm - Mode) = AnnotatedTerm :-
    construct_qualified_term(unqualified("::"),
        [HeadTerm, mode_to_term(Context, Mode)], Context, AnnotatedTerm).

mode_to_term(Mode) = mode_to_term(term__context_init, Mode).

:- func mode_to_term(term__context, mer_mode) = prog_term.

mode_to_term(Context, (InstA -> InstB)) = Term :-
    (
        % Check for higher-order pred or func modes, and output them
        % in a nice format.
        InstA = ground(_Uniq, higher_order(_)),
        InstB = InstA
    ->
        Term = inst_to_term(InstA, Context)
    ;
        construct_qualified_term(unqualified(">>"),
            [inst_to_term(InstA, Context), inst_to_term(InstB, Context)],
            Context, Term)
    ).
mode_to_term(Context, user_defined_mode(Name, Args)) = Term :-
    construct_qualified_term(Name, list__map(map_inst_to_term(Context), Args),
        Context, Term).

:- func make_atom(string, prog_context) = prog_term.

make_atom(Name, Context) =
    term__functor(term__atom(Name), [], Context).

:- func map_inst_to_term(prog_context, mer_inst) = prog_term.

map_inst_to_term(Context, Inst) = inst_to_term(Inst, Context).

inst_to_term(Inst) = inst_to_term(Inst, term__context_init).

:- func inst_to_term(mer_inst, prog_context) = prog_term.

inst_to_term(any(Uniq), Context) =
    make_atom(any_inst_uniqueness(Uniq), Context).
inst_to_term(free, Context) =
    make_atom("free", Context).
inst_to_term(free(Type), Context) = Term :-
    unparse_type(Type, Term0),
    Term1 = term__coerce(Term0),
    Term = term__functor(term__atom("free"), [Term1], Context).
inst_to_term(bound(Uniq, BoundInsts), Context) = Term :-
    construct_qualified_term(unqualified(inst_uniqueness(Uniq, "bound")),
        [bound_insts_to_term(BoundInsts, Context)], Context, Term).
inst_to_term(ground(Uniq, GroundInstInfo), Context) = Term :-
    (
        GroundInstInfo = higher_order(pred_inst_info(PredOrFunc, Modes, Det)),
        % XXX we ignore Uniq
        (
            PredOrFunc = predicate,
            construct_qualified_term(unqualified("pred"),
                list__map(mode_to_term(Context), Modes),
                Context, ModesTerm)
        ;
            PredOrFunc = function,
            pred_args_to_func_args(Modes, ArgModes, RetMode),
            construct_qualified_term(unqualified("func"),
                list__map(mode_to_term(Context), ArgModes),
                Context, ArgModesTerm),
            construct_qualified_term(unqualified("="),
                [ArgModesTerm, mode_to_term(Context, RetMode)],
                Context, ModesTerm)
        ),
        construct_qualified_term(unqualified("is"), [
            ModesTerm, det_to_term(Det, Context)], Context, Term)
    ;
        GroundInstInfo = none,
        Term = make_atom(inst_uniqueness(Uniq, "ground"), Context)
    ).
inst_to_term(inst_var(Var), _) =
    term__coerce(term__variable(Var)).
inst_to_term(constrained_inst_vars(Vars, Inst), Context) =
    set__fold(func(Var, Term) =
            term__functor(term__atom("=<"),
                [term__coerce(term__variable(Var)), Term], Context),
        Vars, inst_to_term(Inst, Context)).
inst_to_term(abstract_inst(Name, Args), Context) =
    inst_name_to_term(user_inst(Name, Args), Context).
inst_to_term(defined_inst(InstName), Context) =
    inst_name_to_term(InstName, Context).
inst_to_term(not_reached, Context) =
    make_atom("not_reached", Context).

:- func inst_name_to_term(inst_name, prog_context) = prog_term.

inst_name_to_term(user_inst(Name, Args), Context) = Term :-
    construct_qualified_term(Name,
        list__map(map_inst_to_term(Context), Args),
        Context, Term).
inst_name_to_term(merge_inst(InstA, InstB), Context) = Term :-
    construct_qualified_term(unqualified("$merge_inst"),
        list__map(map_inst_to_term(Context), [InstA, InstB]),
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
        list__map(map_inst_to_term(Context), [InstA, InstB]) ++
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
        term__coerce(Term0)],
        Context, Term).
inst_name_to_term(typed_inst(Type, InstName), Context) = Term :-
    unparse_type(Type, Term0),
    construct_qualified_term(unqualified("$typed_inst"),
        [term__coerce(Term0),
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
    error("bound_insts_to_term([])").
bound_insts_to_term([functor(ConsId, Args) | BoundInsts], Context) = Term :-
    (
        cons_id_and_args_to_term(ConsId,
            list__map(map_inst_to_term(Context), Args), FirstTerm)
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
        error("bound_insts_to_term: cons_id_and_args_to_term failed")
    ).

:- func det_to_term(determinism, prog_context) = prog_term.

det_to_term(Det, Context) = make_atom(det_to_string(Det), Context).

:- func det_to_string(determinism) = string.

det_to_string(erroneous) = "erroneous".
det_to_string(failure) = "failure".
det_to_string(det) = "det".
det_to_string(semidet) = "semidet".
det_to_string(cc_multidet) = "cc_multi".
det_to_string(cc_nondet) = "cc_nondet".
det_to_string(multidet) = "multi".
det_to_string(nondet) = "nondet".

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
    ( set__member(InstName, ExpandedInstInfo ^ eii_expansions) ->
        add_string("...", !S)
    ; InstName = user_inst(_, _) ->
        % Don't expand user-defined insts, just output them as is
        % (we do expand any compiler-defined insts that occur
        % in the arguments of the user-defined inst, however).
        mercury_format_inst_name(InstName, ExpandedInstInfo, !S)
    ;
        inst_lookup(ExpandedInstInfo ^ eii_module_info, InstName, Inst),
        set__insert(ExpandedInstInfo ^ eii_expansions, InstName, Expansions),
        mercury_format_inst(Inst,
            ExpandedInstInfo ^ eii_expansions := Expansions, !S)
    ).

mercury_output_expanded_inst(Inst, VarSet, ModuleInfo, !IO) :-
    set__init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), !IO).

mercury_expanded_inst_to_string(Inst, VarSet, ModuleInfo) = String :-
    set__init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), "", String).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_out.m".

%-----------------------------------------------------------------------------%
:- end_module hlds__hlds_out.
%-----------------------------------------------------------------------------%
