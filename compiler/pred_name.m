%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2014-2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pred_name.m.
%
% When we create transformed versions of predicates and functions, we need
% names for them. This module creates those names out of the names of the
% original predicate or function, and the identity and parameters of the
% transformation.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.pred_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % XXX The name of this type should NOT give the misleading impression
    % that it has anything to do with pred_ids.
:- type new_pred_id
    --->    newpred_counter(int, int)                   % Line number, Counter
    ;       newpred_type_subst(tvarset, type_subst)
    ;       newpred_unused_args(list(int))
    ;       newpred_parallel_args(list(int))
    ;       newpred_parallel_loop_control
    ;       newpred_structure_reuse(int, list(int))     % Mode, no-clobber
                                                        % arguments.
    ;       newpred_distance_granularity(int).          % Distance

    % make_pred_name_with_context(ModuleName, Prefix, PredOrFunc,
    %   PredName, Line, Counter, SymName):
    %
    % Create a predicate name and return it as SymName. Create the name
    % based on the Prefix, the PredOrFunc, the base name PredName,
    % and the line number Line.
    %
    % For use in cases where we create more than one predicate for the
    % same line, we also include the per-line distinguishing Counter
    % in the name.
    %
:- pred make_pred_name_with_context(module_name::in, string::in,
    pred_or_func::in, string::in, int::in, int::in, sym_name::out) is det.

    % make_pred_name_with_context(ModuleName, Prefix, MaybePredOrFunc,
    %   PredName, NewPredId, SymName):
    %
    % Create a predicate name and return it as SymName. Create the name
    % based on the Prefix, the (maybe) PredOrFunc, the base name PredName,
    % and the pred-name-suffix generating scheme described by NewPredId.
    %
    % XXX The Prefix should be implicit in new_pred_id.
    %
:- pred make_pred_name(module_name::in, string::in, maybe(pred_or_func)::in,
    string::in, new_pred_id::in, sym_name::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.

:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
        Line, Counter, SymName) :-
    make_pred_name(ModuleName, Prefix, yes(PredOrFunc), PredName,
        newpred_counter(Line, Counter), SymName).

make_pred_name(ModuleName, Prefix, MaybePredOrFunc, PredName,
        NewPredId, SymName) :-
    (
        MaybePredOrFunc = yes(PredOrFunc),
        PFS = pred_or_func_to_str(PredOrFunc)
    ;
        MaybePredOrFunc = no,
        PFS = "pred_or_func"
    ),
    (
        NewPredId = newpred_counter(Line, Counter),
        string.format("%d__%d", [i(Line), i(Counter)], PredIdStr)
    ;
        NewPredId = newpred_type_subst(VarSet, TypeSubst),
        SubstToString =
            ( pred(SubstElem::in, SubstStr::out) is det :-
                SubstElem = Var - Type,
                varset.lookup_name(VarSet, Var, VarName),
                TypeStr =
                    mercury_type_to_string(VarSet, print_name_only, Type),
                % XXX The use of = here *requires* mangling the names
                % that we construct using SubstStr.
                string.format("%s = %s", [s(VarName), s(TypeStr)], SubstStr)
            ),
        list_to_string(SubstToString, TypeSubst, PredIdStr)
    ;
        ( NewPredId = newpred_unused_args(ArgNums)
        ; NewPredId = newpred_parallel_args(ArgNums)
        ),
        list_to_string(string.int_to_string, ArgNums, PredIdStr)
    ;
        NewPredId = newpred_structure_reuse(ModeNum, ArgNums),
        list_to_string(string.int_to_string, ArgNums, ArgNumsStr),
        string.format("%i__%s", [i(ModeNum), s(ArgNumsStr)], PredIdStr)
    ;
        NewPredId = newpred_distance_granularity(Distance),
        string.int_to_string(Distance, PredIdStr)
    ;
        NewPredId = newpred_parallel_loop_control,
        % XXX This leaves Name ending with two underscores.
        PredIdStr = ""
    ),

    % XXX The format of PredIdStr, including whether it may be empty,
    % depends on Prefix; therefore it should immediately follow Prefix.
    string.format("%s__%s__%s__%s",
        [s(Prefix), s(PFS), s(PredName), s(PredIdStr)], Name),
    SymName = qualified(ModuleName, Name).

:- pred list_to_string(pred(T, string)::in(pred(in, out) is det),
    list(T)::in, string::out) is det.

list_to_string(Pred, List, String) :-
    % XXX The use of [] here *requires* mangling the names that
    % our callers construct using String.
    list_to_string_2(Pred, List, ["]"], Strings),
    string.append_list(["[" | Strings], String).

:- pred list_to_string_2(pred(T, string)::in(pred(in, out) is det),
    list(T)::in, list(string)::in, list(string)::out) is det.

list_to_string_2(_, [], !Strings).
list_to_string_2(Pred, [T | Ts], !Strings) :-
    % XXX The use of , here *requires* mangling the names that
    % our callers construct using !Strings.
    (
        Ts = []
    ;
        Ts = [_ | _],
        list_to_string_2(Pred, Ts, !Strings),
        !:Strings = [", " | !.Strings]
    ),
    call(Pred, T, String),
    !:Strings = [String | !.Strings].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.pred_name.
%-----------------------------------------------------------------------------%
