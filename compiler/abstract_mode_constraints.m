%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2009-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: abstract_mode_constraints.m.
% Main author: richardf.
%
% This module contains data structures designed for use with constraints
% based mode analysis. It deals specifically with constraints for
% determining producing and consuming goals for program variables.
%
%---------------------------------------------------------------------------%

:- module check_hlds.abstract_mode_constraints.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % `unit'-like type used to distinguish mode constraint variables.
    %
:- type mc_type.
:- type mc_var == var(mc_type).         % Constraint variable.
:- type mc_varset == varset(mc_type).   % Source of constraint variables.

%---------------------------------------------------------------------------%
%
% Abstract Constraints
%

% Data structures for storing abstract constraints. Conjunctions and
% disjunctions can be nested. The atomic constraints between constraint
% variables are designed around the types of constraints required by
% producer/consumer determining in constraint based mode analysis. The
% paper "Constraint-based mode analysis of Mercury" by David Overton,
% Zoltan Somogyi and Peter Stuckey documents these mode constraints.

    % Represents conjunctions and disjunctions between atomic constraints
    % on constraint variables.
    %
:- type mc_constraint
    --->    mc_atomic(var_constraint)

    ;       mc_disj(list(mc_constraint))
            % Primarily included for the purposes of representing mode
            % declaration and call constraints, which are a disjunction
            % of conjunctions of atomic constraints.  The intended form
            % is: mc_disj([conj(...), ..., mc_conj(...)])
            %
            % Note: mc_disj([]) represents false.

    ;       mc_conj(list(mc_constraint)).
            % See amc_disj.
            % Note: amc_conj([]) is the empty constraint, or true.

    % var_constraint represents a boolean constraint between
    % producer/consumer constraint variables.
    %
:- type var_constraint == var_constraint(mc_type).
:- type var_constraint(T)
    --->    equiv_bool(var(T), bool)
            % equiv_bool(X, yes) gives the constraint (X)
            % equiv_bool(X, no) gives the constraint not(X)

    ;       equivalent(list(var(T)))
            % equivalent(Xs) gives (all Xi, Xj in Xs).(Xi<->Xj)

    ;       implies(var(T), var(T))
            % implies(X, Y) gives X->Y

    ;       equiv_disj(var(T), list(var(T)))
            % equiv_disj(X, [Y1,...,Yn]) gives X<->OR(Y1,...,Yn)

    ;       at_most_one(list(var(T)))
            % at_most_one(Xs) gives the constraint
            % (all Xi, Xj in Xs).(i = j or not(Xi and Xj))

    ;       exactly_one(list(var(T))).
            % exactly_one(Xs) gives XOR(Xs)

    % Maps from producer/consumer constraint variables to a boolean
    % binding for them.
    %
:- type mc_bindings == map(mc_var, bool).

%---------------------------------------------------------------------------%
%
% Constraint collection structures.
%

:- type mc_ann_constraint
    --->    mc_ann_constraint(mc_constraint, mc_annotation).

:- func project_mc_constraint(mc_ann_constraint) = mc_constraint.
:- func project_mc_annotation(mc_ann_constraint) = mc_annotation.

    % Various information about the creation of the constraint in
    % question.
    %
:- type mc_annotation
    --->    mc_annotation(
                % Context of the goal this constraint was formed for.
                context             ::      prog_context
            ).

    % producer/consumer constraints for a predicate.
    %
:- type pred_p_c_constraints
    --->    pred_p_c_constraints(
                % Stores procedure specific constraints such as mode
                % declaration constraints.
                ppcc_procspec_constraints   ::  multi_map(proc_id,
                                                    mc_ann_constraint),

                % Stores constraints that apply to all procedures of the
                % predicate - typically generated from its clauses.
                ppcc_allproc_constraints    ::  list(mc_ann_constraint),

                % Collection of predicates with no declared modes that are
                % called by this predicate.
                ppcc_mode_infer_callees     ::  set(pred_id)
            ).

%---------------------------------------------------------------------------%

    % Return a representation of the empty set of constraints.
    %
:- func init_pred_p_c_constraints = pred_p_c_constraints.

    % add_constraint(DebugStream, MCVarSet, Context, Constraint,
    %   !PredPCConstraints):
    %
    % Add the constraint given by Constraint (whose vars are described by
    % MCVarSet, and which comes from Context) to the constraint system
    % in PredPCConstraints.
    %
:- pred add_constraint(mc_varset::in, prog_context::in, mc_constraint::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % add_proc_specific_constraint(DebugStream, MCVarSet, Context,
    %   ProcId, Constraint, !PredPCConstraints):
    %
    % Add the constraint given by Constraint to the constraint system in
    % PredPCConstraints, and associate it specifically with the given
    % procedure.
    %
:- pred add_proc_specific_constraint(mc_varset::in, prog_context::in,
    proc_id::in, mc_constraint::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % add_mode_infer_callee(PredId, !PredPCConstraints):
    %
    % Record in PredPCConstraints that predicate PredId is called and
    % therefore needs to have modes inferred for it. Without those inferred
    % modes, mode analysis cannot properly analyze the predicate whose
    % mode constraints PredPCConstraints represents.
    %
:- pred add_mode_infer_callee(pred_id::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % Return the constraints that apply to all procedures of a predicate.
    %
:- func allproc_constraints(pred_p_c_constraints)
    = list(mc_constraint).

    % Return all constraints that apply to the given procedure.
    % This includes both constraints specific to that procedure,
    % and constraints that apply to all procedures.
    %
:- func all_constraints_for_proc(proc_id, pred_p_c_constraints)
    = list(mc_constraint).

    % Return the annotated constraints that apply to all procedures
    % of a predicate.
    %
:- func allproc_annotated_constraints(pred_p_c_constraints) =
    list(mc_ann_constraint).

    % Return the annotated constraints that apply to the given procedure.
    % This includes both constraints specific to that procedure,
    % and constraints that apply to all procedures.
    %
:- func all_annotated_constraints_for_proc(proc_id, pred_p_c_constraints)
    = list(mc_ann_constraint).

    % Return the annotation constraints that apply specifically to the given
    % procedure, but not constraints that apply to all procedures.
    %
:- func proc_specific_annotated_constraints(proc_id, pred_p_c_constraints)
    = list(mc_ann_constraint).

%---------------------------------------------------------------------------%

    % equiv_no(MCVarSet, Context, MCVar, !Constraints) constrains MCVar to `no'
    % in Constraints. Context should be the context of the goal or declaration
    % that imposed this constraint.
    %
:- pred equiv_no(mc_varset::in, prog_context::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % equivalent(Context, MCVars, !Constraints) constrains MCVars
    % in Constraints to all take the same value. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred equivalent(mc_varset::in, prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % equiv_disj(MCVarSet, Context, X, Ys, !Constraints) constrains X and Ys in
    % Constraints such that (X <-> disj(Ys)) - i.e. if X is true at least one
    % of the Ys must be true, and if X is false, all of Ys must be false.
    % Context should be the context of the goal or declaration that imposed
    % this constraint.
    %
:- pred equiv_disj(mc_varset::in, prog_context::in, mc_var::in,
    list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % at_most_one(MCVarSet, Context, MCVars, !Constraints) constrains MCVars in
    % Constraints such that at most one of them can be true. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred at_most_one(mc_varset::in, prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % not_both(MCVarSet, Context, A, B, !Constraints) constrains mode
    % constraint variables A and B in Constraints by the constraint
    % `not (A and B)'. Context should be the context of the goal or
    % declaration that imposed this constraint.
    %
:- pred not_both(mc_varset::in, prog_context::in, mc_var::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % exactly_one(MCVarSet, Context, MCVars, !Constraints) constrains MCVars in
    % Constraints such that exactly one of them is `yes'. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred exactly_one(mc_varset::in, prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % xor(MCVarSet, Context, A, B, !Constraints) constrains mode constraint
    % variables A and B in Constraints by the constraint (A xor B),
    % i.e. constrains exactly one of them to be true. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred xor(mc_varset::in, prog_context::in, mc_var::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

%---------------------------------------------------------------------------%

    % Print the mc_constraints it is passed in a human readable format.
    %
:- pred dump_constraints_and_annotations(io.text_output_stream::in,
    globals::in, mc_varset::in, list(mc_ann_constraint)::in,
    io::di, io::uo) is det.

    % Print the mc_constraint it is passed in a human readable format.
    %
:- pred pretty_print_constraint(io.text_output_stream::in, mc_varset::in,
    mc_constraint::in, io::di, io::uo) is det.

    % Print the mc_constraints it is passed in a human readable format.
    %
:- pred pretty_print_constraints(io.text_output_stream::in, mc_varset::in,
    list(mc_constraint)::in, io::di, io::uo) is det.

    % Print a list of models for the constraint system.
    %
:- pred pretty_print_solutions(io.text_output_stream::in, mc_varset::in,
    list(mc_bindings)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.write_error_spec.

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

    % Dummy type used to distinguish mode constraint variables.
    %
:- type mc_type
    --->    mc_type.

project_mc_constraint(mc_ann_constraint(Constraint, _)) = Constraint.
project_mc_annotation(mc_ann_constraint(_, Annotation)) = Annotation.

%---------------------------------------------------------------------------%

    % Initialises all the parts of a mode_constraints_info type.
    %
init_pred_p_c_constraints = pred_p_c_constraints(multi_map.init, [], set.init).

add_constraint(MCVarSet, Context, Constraint, !PredPCConstraints) :-
    trace [
        compile_time(flag("goal_mode_constraints")),
        run_time(env("GOAL_MODE_CONSTRAINTS")),
        io(!IO)
    ] (
        % XXX STREAM
        io.output_stream(OutputStream, !IO),
        io.write_string(OutputStream, "add constraint ", !IO),
        pretty_print_constraint(OutputStream, MCVarSet, Constraint, !IO)
    ),

    AllProcsConstraints = !.PredPCConstraints ^ ppcc_allproc_constraints,
    ConstraintAnnotation = mc_annotation(Context),
    AnnotatedConstraint = mc_ann_constraint(Constraint, ConstraintAnnotation),
    !PredPCConstraints ^ ppcc_allproc_constraints :=
        [AnnotatedConstraint | AllProcsConstraints].

add_proc_specific_constraint(MCVarSet, Context, ProcId, Constraint,
        !PredPCConstraints) :-
    trace [
        compile_time(flag("goal_mode_constraints")),
        run_time(env("GOAL_MODE_CONSTRAINTS")),
        io(!IO)
    ] (
        % XXX STREAM
        io.output_stream(OutputStream, !IO),
        io.format(OutputStream, "add proc-specific constraint for proc %d ",
            [i(proc_id_to_int(ProcId))], !IO),
        pretty_print_constraint(OutputStream, MCVarSet, Constraint, !IO)
    ),

    ProcConstraints = !.PredPCConstraints ^ ppcc_procspec_constraints,
    ConstraintAnnotation = mc_annotation(Context),
    AnnotatedConstraint = mc_ann_constraint(Constraint, ConstraintAnnotation),
    !PredPCConstraints ^ ppcc_procspec_constraints :=
        multi_map.add(ProcConstraints, ProcId, AnnotatedConstraint).

add_mode_infer_callee(PredId, !PredPCConstraints) :-
    ModeInferCallees = !.PredPCConstraints ^ ppcc_mode_infer_callees,
    !PredPCConstraints ^ ppcc_mode_infer_callees :=
        set.insert(ModeInferCallees, PredId).

%---------------------------------------------------------------------------%

allproc_constraints(PredPCConstraints) = Constraints :-
    AnnotatedConstraints = allproc_annotated_constraints(PredPCConstraints),
    Constraints = list.map(project_mc_constraint, AnnotatedConstraints).

all_constraints_for_proc(ProcId, PredPCConstraints) = Constraints :-
    AnnotatedConstraints = all_annotated_constraints_for_proc(ProcId,
        PredPCConstraints),
    Constraints = list.map(project_mc_constraint, AnnotatedConstraints).

allproc_annotated_constraints(PredPCConstraints) = AnnotatedConstraints :-
    AnnotatedConstraints = PredPCConstraints ^ ppcc_allproc_constraints.

all_annotated_constraints_for_proc(ProcId, PredPCConstraints)
        = AnnotatedConstraints :-
    multi_map.lookup(PredPCConstraints ^ ppcc_procspec_constraints, ProcId,
        ThisProcConstraints),
    AllProcConstraints = PredPCConstraints ^ ppcc_allproc_constraints,
    AnnotatedConstraints = ThisProcConstraints ++ AllProcConstraints.

proc_specific_annotated_constraints(ProcId, PredPCConstraints) =
    multi_map.lookup(PredPCConstraints ^ ppcc_procspec_constraints, ProcId).

%---------------------------------------------------------------------------%
%
% Predicates to allow easy adding of var_constraints.
%

equiv_no(MCVarSet, Context, MCVar, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(equiv_bool(MCVar, no)), !Constraints).

equivalent(MCVarSet, Context, MCVars, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(equivalent(MCVars)), !Constraints).

equiv_disj(MCVarSet, Context, X, Ys, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(equiv_disj(X, Ys)), !Constraints).

at_most_one(MCVarSet, Context, MCVars, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(at_most_one(MCVars)), !Constraints).

not_both(MCVarSet, Context, A, B, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(at_most_one([A, B])), !Constraints).

exactly_one(MCVarSet, Context, MCVars, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(exactly_one(MCVars)), !Constraints).

xor(MCVarSet, Context, A, B, !Constraints) :-
    add_constraint(MCVarSet, Context,
        mc_atomic(exactly_one([A, B])), !Constraints).

%---------------------------------------------------------------------------%
%
% Dumping constraints for --debug-mode-constraints.
%

dump_constraints_and_annotations(OutputStream, Globals, VarSet,
        AnnConstraints, !IO) :-
    Indent = 0,
    list.foldl(dump_ann_constraint(OutputStream, Globals, VarSet, Indent),
        AnnConstraints, !IO).

    % Dumps a list of constraints using the same constraint annotation
    % at indent level indicated by the int.
    %
:- pred dump_constraints(io.text_output_stream::in, globals::in, mc_varset::in,
    int::in, mc_annotation::in, list(mc_constraint)::in,
    io::di, io::uo) is det.

dump_constraints(OutputStream, Globals, VarSet, Indent, Annotation,
        Constraints, !IO) :-
    list.foldl(
        dump_constraint(OutputStream, Globals, VarSet, Indent, Annotation),
        Constraints, !IO).

:- pred dump_ann_constraint(io.text_output_stream::in, globals::in,
    mc_varset::in, int::in, mc_ann_constraint::in, io::di, io::uo) is det.

dump_ann_constraint(OutputStream, Globals, VarSet, Indent,
        AnnConstraint, !IO) :-
    AnnConstraint = mc_ann_constraint(Constraint, Annotation),
    dump_constraint(OutputStream, Globals, VarSet, Indent,
        Annotation, Constraint, !IO).

    % Prints one mc_constraint to the output. The int is an indent level.
    %
:- pred dump_constraint(io.text_output_stream::in, globals::in, mc_varset::in,
    int::in, mc_annotation::in, mc_constraint::in, io::di, io::uo) is det.

dump_constraint(OutputStream, Globals, VarSet, Indent, Annotation,
        Constraint, !IO) :-
    (
        Constraint = mc_disj(Constraints),
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words("disj(")], !IO),
        dump_constraints(OutputStream, Globals, VarSet, Indent + 1,
            Annotation, Constraints, !IO),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words(") end disj")], !IO)
    ;
        Constraint = mc_conj(Constraints),
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words("conj(")], !IO),
        dump_constraints(OutputStream, Globals, VarSet, Indent+1,
            Annotation, Constraints, !IO),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words(") end conj")], !IO)
    ;
        Constraint = mc_atomic(AtomicConstraint),
        dump_var_constraint(OutputStream, Globals, VarSet, Indent, Annotation,
            AtomicConstraint, !IO)
    ).

    % Prints a var_constraint to the output. The int is an indent level.
    %
:- pred dump_var_constraint(io.text_output_stream::in, globals::in,
    mc_varset::in, int::in, mc_annotation::in, var_constraint::in,
    io::di, io::uo) is det.

dump_var_constraint(OutputStream, Globals, VarSet, Indent, Annotation,
        Constraint, !IO) :-
    (
        Constraint = equiv_bool(X, Val),
        mc_var_list_to_string(VarSet, [X], VarName),
        mc_var_val_to_string(Val, ValString),
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words(VarName ++ " = " ++ ValString)], !IO)
    ;
        Constraint = equivalent(Xs),
        mc_var_list_to_string(VarSet, Xs, VarsString),
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words("equivalent(" ++ VarsString ++ ")")], !IO)
    ;
        Constraint = implies(X, Y),
        mc_var_list_to_string(VarSet, [X], XName),
        mc_var_list_to_string(VarSet, [Y], YName),
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent,
            [words(XName ++ " -> " ++ YName)], !IO)
    ;
        Constraint = equiv_disj(X, Xs),
        mc_var_list_to_string(VarSet, [X], XName),
        mc_var_list_to_string(VarSet, Xs, XsString),
        Context = context(Annotation),
        Pieces = [words(XName ++ " <-> disj(" ++ XsString ++ ")")],
        write_error_pieces(OutputStream, Globals, Context, Indent, Pieces, !IO)
    ;
        Constraint = at_most_one(Xs),
        mc_var_list_to_string(VarSet, Xs, XsString),
        Pieces = [words("at_most_one(" ++ XsString ++ ")")],
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent, Pieces, !IO)
    ;
        Constraint = exactly_one(Xs),
        mc_var_list_to_string(VarSet, Xs, XsString),
        Pieces = [words("exactly_one(" ++ XsString ++ ")")],
        Context = context(Annotation),
        write_error_pieces(OutputStream, Globals, Context, Indent, Pieces, !IO)
    ).

    % mc_var_list_to_string(VarSet, MCVars, MCVarsString):
    %
    % Makes a comma separated list of MCVars as a string.
    %
:- pred mc_var_list_to_string(mc_varset::in, list(mc_var)::in,
    string::out) is det.

mc_var_list_to_string(_VarSet, [], "").
mc_var_list_to_string(VarSet, [MCVar], VarName) :-
    varset.lookup_name(VarSet, MCVar, VarName).
mc_var_list_to_string(VarSet, [MCVar1, MCVar2 | MCVars],
    VarName ++ ", " ++ VarNames) :-
    varset.lookup_name(VarSet, MCVar1, VarName),
    mc_var_list_to_string(VarSet, [MCVar2 | MCVars], VarNames).

    % Makes a string representation of an mc_var binding.
    %
:- pred mc_var_val_to_string(bool::in, string::out) is det.

mc_var_val_to_string(yes, "yes").
mc_var_val_to_string(no, "no").

%---------------------------------------------------------------------------%
%
% Pretty printing predicates for the formulae type, and others
%

pretty_print_constraint(OutputStream, VarSet, Constraint, !IO) :-
    Indent = "",
    pretty_print_constraint_indent(OutputStream, VarSet, Indent,
        Constraint, !IO).

pretty_print_constraints(OutputStream, VarSet, Constraints, !IO) :-
    Indent = "",
    pretty_print_constraints_indent(OutputStream, VarSet, Indent,
        Constraints, !IO).

    % Prints one mc_constraint to the output stream. Always puts
    % a new line at the end.
    %
:- pred pretty_print_constraint_indent(io.text_output_stream::in,
    mc_varset::in, string::in, mc_constraint::in, io::di, io::uo) is det.

pretty_print_constraint_indent(OutputStream, VarSet, Indent,
        Constraint, !IO) :-
    (
        Constraint = mc_disj(Constraints),
        io.write_string(OutputStream, Indent, !IO),
        io.write_string(OutputStream, "disj(\n", !IO),
        pretty_print_constraints_indent(OutputStream, VarSet, "\t" ++ Indent,
            Constraints, !IO),
        io.write_string(OutputStream, Indent, !IO),
        io.write_string(OutputStream, ") end disj\n", !IO)
    ;
        Constraint = mc_conj(Constraints),
        io.write_string(OutputStream, Indent, !IO),
        io.write_string(OutputStream, "conj(\n", !IO),
        pretty_print_constraints_indent(OutputStream, VarSet, "\t" ++ Indent,
            Constraints, !IO),
        io.write_string(OutputStream, Indent, !IO),
        io.write_string(OutputStream, ") end conj\n", !IO)
    ;
        Constraint = mc_atomic(AtomicConstraint),
        io.write_string(OutputStream, Indent, !IO),
        pretty_print_var_constraint(OutputStream, VarSet,
            AtomicConstraint, !IO),
        io.nl(OutputStream, !IO)
    ).

    % Same as before, but with an indent argument used to indent
    % conjunctions and disjunctions of constraints.
    %
:- pred pretty_print_constraints_indent(io.text_output_stream::in,
    mc_varset::in, string::in, list(mc_constraint)::in, io::di, io::uo) is det.

pretty_print_constraints_indent(_OutputStream, _VarSet, _Indent, [], !IO).
pretty_print_constraints_indent(OutputStream, VarSet, Indent,
        [Constraint | Constraints], !IO) :-
    pretty_print_constraint_indent(OutputStream, VarSet, Indent,
        Constraint, !IO),
    pretty_print_constraints_indent(OutputStream, VarSet, Indent,
        Constraints, !IO).

    % Prints a var_constraint to the screen. No indents, no line return.
    %
:- pred pretty_print_var_constraint(io.text_output_stream::in, mc_varset::in,
    var_constraint::in, io::di, io::uo) is det.

pretty_print_var_constraint(OutputStream, VarSet, Constraint, !IO) :-
    (
        Constraint = equiv_bool(X, TF),
        pretty_print_mc_var(OutputStream, VarSet, X, !IO),
        io.write_string(OutputStream, " = ", !IO),
        io.print(OutputStream, TF, !IO)
    ;
        Constraint = equivalent(Xs),
        io.write_string(OutputStream, "equivalent(", !IO),
        pretty_print_mc_vars(OutputStream, VarSet, Xs, !IO),
        io.write_string(OutputStream, ")", !IO)
    ;
        Constraint = implies(X, Y),
        pretty_print_mc_var(OutputStream, VarSet, X, !IO),
        io.write_string(OutputStream, " -> ", !IO),
        pretty_print_mc_var(OutputStream, VarSet, Y, !IO)
    ;
        Constraint = equiv_disj(X, Xs),
        pretty_print_mc_var(OutputStream, VarSet, X, !IO),
        io.write_string(OutputStream, " <-> disj(", !IO),
        pretty_print_mc_vars(OutputStream, VarSet, Xs, !IO),
        io.write_string(OutputStream, ")", !IO)
    ;
        Constraint = at_most_one(Xs),
        io.write_string(OutputStream, "at_most_one(", !IO),
        pretty_print_mc_vars(OutputStream, VarSet, Xs, !IO),
        io.write_string(OutputStream, ")", !IO)
    ;
        Constraint = exactly_one(Xs),
        io.write_string(OutputStream, "exactly_one(", !IO),
        pretty_print_mc_vars(OutputStream, VarSet, Xs, !IO),
        io.write_string(OutputStream, ")", !IO)
    ).

    % Prints a constraint var to the screen. No indents, no line return.
    % Simply uses the variable's name from the varset.
    %
:- pred pretty_print_mc_var(io.text_output_stream::in, mc_varset::in,
    mc_var::in, io::di, io::uo) is det.

pretty_print_mc_var(OutputStream, VarSet, Var, !IO) :-
    varset.lookup_name(VarSet, Var, VarName),
    io.write_string(OutputStream, VarName, !IO).

    % Prints a comma separated list of constraint variables.
    %
:- pred pretty_print_mc_vars(io.text_output_stream::in, mc_varset::in,
    list(mc_var)::in, io::di, io::uo) is det.

pretty_print_mc_vars(_OutputStream, _VarSet, [], !IO).
pretty_print_mc_vars(OutputStream, VarSet, [Var | Vars], !IO) :-
    pretty_print_mc_var(OutputStream, VarSet, Var, !IO),
    (
        Vars = []
    ;
        Vars = [_ | _],
        io.write_string(OutputStream, ", ", !IO),
        pretty_print_mc_vars(OutputStream, VarSet, Vars, !IO)
    ).

%---------------------------------------------------------------------------%

    % Prints a list of models for the constraint system.
    %
pretty_print_solutions(OutputStream, VarSet, Solutions, !IO) :-
    list.foldl2(pretty_print_bindings(OutputStream, VarSet), Solutions,
        0, _,  !IO).

    % Prints the variable bindings of this solution, one per line.
    %
:- pred pretty_print_bindings(io.text_output_stream::in, mc_varset::in,
    mc_bindings::in, int::in, int::out, io::di, io::uo) is det.

pretty_print_bindings(OutputStream, VarSet, Bindings, N, N + 1, !IO) :-
    io.write_string(OutputStream,
        "Solution " ++ string.from_int(N) ++ ":\n{\n", !IO),
    Variables = map.keys(Bindings),
    list.foldl(pretty_print_binding(OutputStream, VarSet, Bindings),
        Variables, !IO),
    io.write_string(OutputStream, "}\n", !IO).

:- pred pretty_print_binding(io.text_output_stream::in, mc_varset::in,
    mc_bindings::in, mc_var::in, io::di, io::uo) is det.

pretty_print_binding(OutputStream, VarSet, Bindings, Var, !IO) :-
    VarName = varset.lookup_name(VarSet, Var),
    map.lookup(Bindings, Var, Value),
    ValueStr = string.string(Value),
    io.format(OutputStream, "    %s = %s\n", [s(VarName), s(ValueStr)], !IO).

%---------------------------------------------------------------------------%
:- end_module check_hlds.abstract_mode_constraints.
%---------------------------------------------------------------------------%
