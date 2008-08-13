%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mdprof_procrep.m.
% Author: zs.
%
% A program to test the reading of program representations.
%
%-----------------------------------------------------------------------------%

:- module mdprof_procrep.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        MaybeModules = no
    ;
        Args = [_ | _],
        MaybeModules = yes(Args)
    ),
    read_prog_rep_file("Deep.procrep", ProgRepRes, !IO),
    (
        ProgRepRes = ok(ProgRep),
        ProgRep = prog_rep(ModuleReps),
        print_selected_modules(ModuleReps, MaybeModules, !IO)
    ;
        ProgRepRes = error(Error),
        io.error_message(Error, Msg),
        io.format("mdprof_procrep: %s\n", [s(Msg)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred print_selected_modules(list(module_rep)::in, maybe(list(string))::in,
    io::di, io::uo) is det.

print_selected_modules([], _, !IO).
print_selected_modules([ModuleRep | ModuleReps], MaybeModules, !IO) :-
    ModuleRep = module_rep(ModuleName, _StringTable, _ProcReps),
    (
        MaybeModules = no,
        print_module(ModuleRep, !IO)
    ;
        MaybeModules = yes(Modules),
        ( list.member(ModuleName, Modules) ->
            print_module(ModuleRep, !IO)
        ;
            true
        )
    ),
    print_selected_modules(ModuleReps, MaybeModules, !IO).

:- pred print_module(module_rep::in, io::di, io::uo) is det.

print_module(ModuleRep, !IO) :-
    ModuleRep = module_rep(ModuleName, _StringTable, ProcReps),
    io.format("Module %s\n", [s(ModuleName)], !IO),
    list.foldl(print_proc, ProcReps, !IO).

:- pred print_proc(proc_rep::in, io::di, io::uo) is det.

print_proc(ProcRep, !IO) :-
    ProcRep = proc_rep(ProcLabel, ProcDefnRep),
    ProcDefnRep = proc_defn_rep(ArgVarReps, GoalRep, VarTable, Detism),
    print_proc_label(Detism, ProcLabel, !IO),
    print_args(VarTable, ArgVarReps, !IO),
    io.write_string(" :-\n", !IO),
    print_goal(VarTable, 1, GoalRep, !IO),
    io.nl(!IO).

:- pred print_proc_label(detism_rep::in, string_proc_label::in, io::di, io::uo)
    is det.

print_proc_label(Detism, ProcLabel, !IO) :-
    print_detism(Detism, !IO),
    (
        ProcLabel = str_ordinary_proc_label(PredFunc, DeclModule, _DefModule,
            Name, Arity, Mode),
        (
            PredFunc = pf_predicate,
            PF = "pred"
        ;
            PredFunc = pf_function,
            PF = "func"
        ),
        io.format(" %s %s.%s/%d-%d",
            [s(PF), s(DeclModule), s(Name), i(Arity), i(Mode)], !IO)
    ;
        ProcLabel = str_special_proc_label(TypeName, TypeModule, _DefModule,
            Name, Arity, Mode),
        io.format(" %s for %s.%s/%d-%d",
            [s(Name), s(TypeModule), s(TypeName), i(Arity), i(Mode)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred print_goal(var_table::in, int::in, goal_rep::in, io::di, io::uo) 
    is det.

print_goal(VarTable, Indent, GoalRep, !IO) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep),
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj(VarTable, Indent, ConjGoalReps, !IO)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        indent(Indent, !IO),
        print_detism(DetismRep, !IO),
        io.write_string(" (\n", !IO),
        print_disj(VarTable, Indent, DisjGoalReps, no, !IO),
        indent(Indent, !IO),
        io.write_string(")\n", !IO)
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CasesRep),
        indent(Indent, !IO),
        print_detism(DetismRep, !IO),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        io.format(" ( switch on %s\n", [s(SwitchVarName)], !IO),
        print_switch(VarTable, Indent, CasesRep, no, !IO),
        indent(Indent, !IO),
        io.write_string(")\n", !IO)
    ;
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep),
        indent(Indent, !IO),
        print_detism(DetismRep, !IO),
        io.write_string(" (\n", !IO),
        print_goal(VarTable, Indent + 1, CondRep, !IO),
        indent(Indent, !IO),
        io.write_string("->\n", !IO),
        print_goal(VarTable, Indent + 1, ThenRep, !IO),
        indent(Indent, !IO),
        io.write_string(";\n", !IO),
        print_goal(VarTable, Indent + 1, ElseRep, !IO),
        indent(Indent, !IO),
        io.write_string(")\n", !IO)
    ;
        GoalExprRep = negation_rep(SubGoalRep),
        indent(Indent, !IO),
        io.write_string("not (\n", !IO),
        print_goal(VarTable, Indent + 1, SubGoalRep, !IO),
        indent(Indent, !IO),
        io.write_string(")\n", !IO)
    ;
        GoalExprRep = scope_rep(SubGoalRep, MaybeCut),
        indent(Indent, !IO),
        print_detism(DetismRep, !IO),
        io.write_string(" scope", !IO),
        (
            MaybeCut = scope_is_cut
        ;
            MaybeCut = scope_is_no_cut,
            io.write_string(" cut", !IO)
        ),
        io.write_string(" (\n", !IO),
        print_goal(VarTable, Indent + 1, SubGoalRep, !IO),
        indent(Indent, !IO),
        io.write_string(")\n", !IO)
    ;
        GoalExprRep = atomic_goal_rep(_FileName, _LineNumber,
            _BoundVars, AtomicGoalRep),
        print_atomic_goal(VarTable, Indent, DetismRep, AtomicGoalRep, !IO)
    ).

:- pred print_conj(var_table::in, int::in, list(goal_rep)::in, io::di, io::uo)
    is det.

print_conj(VarTable, Indent, GoalReps, !IO) :-
    (
        GoalReps = [],
        indent(Indent, !IO),
        io.write_string("true\n", !IO)
    ;
        GoalReps = [_ | _],
        print_conj_2(VarTable, Indent, GoalReps, !IO)
    ).

:- pred print_conj_2(var_table::in, int::in, list(goal_rep)::in, 
    io::di, io::uo) is det.

print_conj_2(_, _Indent, [], !IO).
print_conj_2(VarTable, Indent, [GoalRep | GoalReps], !IO) :-
    % We use the absence of a separator to denote conjunction.
    %
    % We could try to append the comma at the end of each goal that is
    % not last in a conjunction, but that would be significant work,
    % and (at least for now) there is no real need for it.
    print_goal(VarTable, Indent, GoalRep, !IO),
    print_conj_2(VarTable, Indent, GoalReps, !IO).

:- pred print_disj(var_table::in, int::in, list(goal_rep)::in, bool::in, 
    io::di, io::uo) is det.

print_disj(_, _Indent, [], _PrintSemi, !IO).
print_disj(VarTable, Indent, [GoalRep | GoalReps], PrintSemi, !IO) :-
    (
        PrintSemi = no
    ;
        PrintSemi = yes,
        indent(Indent, !IO),
        io.write_string(";\n", !IO)
    ),
    print_goal(VarTable, Indent + 1, GoalRep, !IO),
    print_disj(VarTable, Indent, GoalReps, yes, !IO).

:- pred print_switch(var_table::in, int::in, list(case_rep)::in, bool::in, 
    io::di, io::uo) is det.

print_switch(_, _Indent, [], _PrintSemi, !IO).
print_switch(VarTable, Indent, [CaseRep | CaseReps], PrintSemi, !IO) :-
    (
        PrintSemi = no
    ;
        PrintSemi = yes,
        indent(Indent, !IO),
        io.write_string(";\n", !IO)
    ),
    CaseRep = case_rep(MainConsIdArityRep, OtherConsIdArityRep, GoalRep),
    print_cons_id_and_arity(Indent + 1, MainConsIdArityRep, !IO),
    list.foldl(print_cons_id_and_arity(Indent + 1), OtherConsIdArityRep, !IO),
    print_goal(VarTable, Indent + 1, GoalRep, !IO),
        print_switch(VarTable, Indent, CaseReps, yes, !IO).

:- pred print_cons_id_and_arity(int::in, cons_id_arity_rep::in,
    io::di, io::uo) is det.

print_cons_id_and_arity(Indent, ConsIdArityRep, !IO) :-
    ConsIdArityRep = cons_id_arity_rep(ConsIdRep, Arity),
    indent(Indent + 1, !IO),
    io.format("%% case %s/%d\n", [s(ConsIdRep), i(Arity)], !IO).

%-----------------------------------------------------------------------------%

:- pred print_atomic_goal(var_table::in, int::in, detism_rep::in, 
    atomic_goal_rep::in, io::di, io::uo) is det.

print_atomic_goal(VarTable, Indent, DetismRep, AtomicGoalRep, !IO) :-
    indent(Indent, !IO),
    print_detism(DetismRep, !IO),
    (
        (
            AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "<="
        ;
            AtomicGoalRep = unify_deconstruct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "=>"
        ),
        lookup_var_name(VarTable, VarRep, VarName),
        io.format(" %s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)], !IO),
        print_args(VarTable, ArgReps, !IO)
    ;
        (
            AtomicGoalRep = partial_construct_rep(VarRep, ConsIdRep,
                MaybeArgReps),
            UnifyOp = "<="
        ;
            AtomicGoalRep = partial_deconstruct_rep(VarRep, ConsIdRep,
                MaybeArgReps),
            UnifyOp = "=>"
        ),
        lookup_var_name(VarTable, VarRep, VarName),
        io.format(" %s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)], !IO),
        print_maybe_args(VarTable, MaybeArgReps, !IO)
    ;
        AtomicGoalRep = unify_assign_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        io.format(" %s := %s", [s(TargetName), s(SourceName)], !IO)
    ;
        AtomicGoalRep = cast_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        io.format(" cast %s to %s", [s(SourceName), s(TargetName)], !IO)
    ;
        AtomicGoalRep = unify_simple_test_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        io.format(" %s == %s", [s(SourceName), s(TargetName)], !IO)
    ;
        AtomicGoalRep = pragma_foreign_code_rep(Args),
        io.write_string(" foreign_proc(", !IO),
        print_args(VarTable, Args, !IO),
        io.write_string(")", !IO)
    ;
        AtomicGoalRep = higher_order_call_rep(HOVarRep, Args),
        lookup_var_name(VarTable, HOVarRep, HOVarName),
        io.format(" %s(", [s(HOVarName)], !IO),
        print_args(VarTable, Args, !IO),
        io.write_string(")", !IO)
    ;
        AtomicGoalRep = method_call_rep(TCIVarRep, MethodNumber, Args),
        lookup_var_name(VarTable, TCIVarRep, TCIVarName),
        io.format(" method %d of %s(", [i(MethodNumber), s(TCIVarName)], !IO),
        print_args(VarTable, Args, !IO),
        io.write_string(")", !IO)
    ;
        AtomicGoalRep = plain_call_rep(Module, Pred, Args),
        io.format(" %s.%s", [s(Module), s(Pred)], !IO),
        print_args(VarTable, Args, !IO)
    ;
        AtomicGoalRep = builtin_call_rep(Module, Pred, Args),
        io.format(" builtin %s.%s", [s(Module), s(Pred)], !IO),
        print_args(VarTable, Args, !IO)
    ;
        AtomicGoalRep = event_call_rep(Event, Args),
        io.format(" event %s", [s(Event)], !IO),
        print_args(VarTable, Args, !IO)
    ),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred print_args(var_table::in, list(var_rep)::in, io::di, io::uo) is det.

print_args(VarTable, Args, !IO) :-
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string("(", !IO),
        print_args_2(VarTable, Args, "", !IO),
        io.write_string(")", !IO)
    ).

:- pred print_args_2(var_table::in, list(var_rep)::in, string::in, 
    io::di, io::uo) is det.

print_args_2(_,        [],                 _,      !IO).
print_args_2(VarTable, [VarRep | VarReps], Prefix, !IO) :-
    lookup_var_name(VarTable, VarRep, VarName),
    io.write_string(Prefix ++ VarName, !IO),
    print_args_2(VarTable, VarReps, ", ", !IO).

:- pred print_maybe_args(var_table::in, list(maybe(var_rep))::in, 
    io::di, io::uo) is det.

print_maybe_args(VarTable, MaybeArgs, !IO) :-
    (
        MaybeArgs = []
    ;
        MaybeArgs = [_ | _],
        io.write_string("(", !IO),
        print_maybe_args_2(VarTable, MaybeArgs, "", !IO),
        io.write_string(")", !IO)
    ).

:- pred print_maybe_args_2(var_table::in, list(maybe(var_rep))::in, string::in,
    io::di, io::uo) is det.

print_maybe_args_2(_, [], _, !IO).
print_maybe_args_2(VarTable, [MaybeVarRep | MaybeVarReps], Prefix, !IO) :-
    io.write_string(Prefix, !IO),
    (
        MaybeVarRep = no,
        io.write_string("_", !IO)
    ;
        MaybeVarRep = yes(VarRep),
        lookup_var_name(VarTable, VarRep, VarName),
        io.write_string(VarName, !IO)
    ),
    print_maybe_args_2(VarTable, MaybeVarReps, ", ", !IO).

:- pred indent(int::in, io::di, io::uo) is det.

indent(N, !IO) :-
    ( N =< 0 ->
        true
    ;
        io.write_string("  ", !IO),
        indent(N - 1, !IO)
    ).

:- pred print_detism(detism_rep::in, io::di, io::uo) is det.

print_detism(Detism, !IO) :-
    (
        Detism = det_rep,
        DetismStr = "det"
    ;
        Detism = semidet_rep,
        DetismStr = "semidet"
    ;
        Detism = nondet_rep,
        DetismStr = "nondet"
    ;
        Detism = multidet_rep,
        DetismStr = "multi"
    ;
        Detism = cc_nondet_rep,
        DetismStr = "cc_nondet"
    ;
        Detism = cc_multidet_rep,
        DetismStr = "cc_multi"
    ;
        Detism = erroneous_rep,
        DetismStr = "erroneous"
    ;
        Detism = failure_rep,
        DetismStr = "failure"
    ),
    io.write_string(DetismStr, !IO).

%-----------------------------------------------------------------------------%
:- end_module mdprof_procrep.
%-----------------------------------------------------------------------------%
