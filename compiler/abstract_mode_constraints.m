%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: abstract_mode_constraints.m
% Main author: richardf

% This module contains data structures designed for use with constraints
% based mode analysis. It deals specifically with constraints for
% determining producing and consuming goals for program variables.

%-----------------------------------------------------------------------------%

:- module check_hlds.abstract_mode_constraints.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % `unit'-like type used to distinguish mode constraint variables.
    %
:- type mc_type.
:- type mc_var == var(mc_type).         % Constraint variable.
:- type mc_varset == varset(mc_type).   % Source of constraint variables.

%-----------------------------------------------------------------------------%
%
% Abstract Constraints
%

% Data structures for storing abstract constraints. Conjunctions and
% disjunctions can be nested. The atomic constraints between constraint
% variables are designed around the types of constraints required by
% producer/consumer determining in constraint based mode analysis. The
% paper "Constraint-based mode analysis of Mercury" by David Overton,
% Zoltan Somogyi and Peter Stuckey documents these mode constraints.

    % Represents conjunctions and disjunctions between atomic
    % constraints on constraint variables.
    %
:- type constraint_formulae == list(constraint_formula).
:- type constraint_formula
    --->    atomic_constraint(var_constraint)

    ;       disj(constraint_formulae)
            % Primarily included for the purposes of representing mode
            % declaration and call constraints, which are a disjunction
            % of conjunctions of atomic constraints.  The intended form
            % is: disj([conj(...), ..., conj(...)])
            %
            % Note: disj([]) represents false.

    ;       conj(constraint_formulae).
            % See disj.
            % Note: conj([]) is the empty constraint, or true.

    % var_constraint represents a boolean constraint between
    % producer/consumer constraint variables
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

%-----------------------------------------------------------------------------%
%
% Constraint collection structures.
%

:- type constraint_and_annotation ==
    pair(constraint_formula, constraint_annotation).

    % Various information about the creation of the constraint in
    % question.
    %
:- type constraint_annotation
    --->    constraint_annotation(
                context     ::      prog_context
                                    % Context of the goal this constraint
                                    % was formed for.
            ).

    % producer/consumer constraints for a predicate.
    %
:- type pred_p_c_constraints --->
    pred_constraints(
        proc_constraints    ::  multi_map(proc_id, constraint_and_annotation),
                                % Stores procedure specific constraints
                                % such as mode declaration constraints.
        pred_constraints    ::  assoc_list(constraint_formula,
                                    constraint_annotation)
                                % Stores constraints that apply to all
                                % procedures of the predicate -
                                % typically generated from its clauses.
    ).

%-----------------------------------------------------------------------------%

    % Prints the constraint_formulae it is passed in a human readable
    % format.
    %
:- pred pretty_print_constraints(mc_varset::in, constraint_formulae::in,
    io::di, io::uo) is det.

    % Prints the constraint_formulae it is passed in a human readable
    % format.
    %
:- pred dump_constraints_and_annotations(mc_varset::in,
    assoc_list(constraint_formula, constraint_annotation)::in,
    io::di, io::uo) is det.

    % Prints a list of models for the constraint system.
    %
:- pred pretty_print_solutions(mc_varset::in, list(mc_bindings)::in, io::di,
    io::uo) is det.

%-----------------------------------------------------------------------------%

    % Function version if init/1.
    %
:- func init = pred_p_c_constraints.

    % add_constraint(Formula, !PredConstraints) adds the constraint
    % given by Formula to the constraint system in PredConstraints.
    %
:- pred add_constraint(prog_context::in, constraint_formula::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % Adds a procedure specific constraint to the system.
    %
:- pred add_constraint(prog_context::in, proc_id::in, constraint_formula::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % pred_constraints_to_formulae rips the barebones
    % constraints (those that apply to all procedures of a
    % predicate) out of the pred_p_c_constraints structure
    % and returns them as a list of constraint formulae.
    %
:- func pred_constraints_to_formulae(pred_p_c_constraints)
    = constraint_formulae.

    % pred_constraints_to_formulae/2 returns all constraints that
    % apply to the given procedure from the pred_p_c_constraints,
    % including constraints that apply to all procedures.
    %
:- func pred_constraints_to_formulae(proc_id, pred_p_c_constraints)
    = constraint_formulae.

    % pred_constraints_to_formulae_and_annotations returns constraints
    % that apply to all procedures of a predicate as a list of pairs of
    % constraint_formula and constraint_annotation.
    %
:- func pred_constraints_to_formulae_and_annotations(pred_p_c_constraints) =
    assoc_list(constraint_formula, constraint_annotation).

    % pred_constraints_to_formulae_and_annotations(ProcId, PredConstraints)
    % returns all constraints that apply to the given procedure from the
    % pred_p_c_constraints, including constraints that apply to all
    % procedures, with their constraint annotations, in pairs.
    %
:- func pred_constraints_to_formulae_and_annotations(proc_id,
    pred_p_c_constraints)
    = assoc_list(constraint_formula, constraint_annotation).

    % proc_constraints_to_formulae_and_annotations(ProcId, PredConstraints)
    % returns constraints that apply specifically to the given
    % procedure (but not constraints that apply to all procedures).
    %
:- func proc_constraints_to_formulae_and_annotations(proc_id,
    pred_p_c_constraints)
    = assoc_list(constraint_formula, constraint_annotation).

%-----------------------------------------------------------------------------%

    % equiv_no(Context, MCVar, !Constraints) constrains MCVar to `no' in
    % Constraints. Context should be the context of the goal or
    % declaration that imposed this constraint.
    %
:- pred equiv_no(prog_context::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % equivalent(Context, MCVars, !Constraints) constrains MCVars in
    % Constraints to all take the same value. Context should be the context of
    % the goal or declaration that imposed this constraint.
    %
:- pred equivalent(prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % equiv_disj(Context, X, Ys, !Constraints) constrains X and Ys in
    % Constraints such that (X <-> disj(Ys)) - ie if X is true at least one of
    % Ys must be true, if X is false, all of Ys must be false. Context should
    % be the context of the goal or declaration that imposed this constraint.
    %
:- pred equiv_disj(prog_context::in, mc_var::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % at_most_one(Context, MCVars, !Constraints) constrains MCVars in
    % Constraints such that at most one of them can be true. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred at_most_one(prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % not_both(Context, A, B, !Constraints) constrains mode constraint
    % variables A and B in Constraints by the constraint (not A ^ B). Context
    % should be the context of the goal or declaration that imposed this
    % constraint.
    %
:- pred not_both(prog_context::in, mc_var::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % exactly_one(Context, MCVars, !Constraints) constrains MCVars in
    % Constraints such that exactly one of them is `yes'. Context should be
    % the context of the goal or declaration that imposed this constraint.
    %
:- pred exactly_one(prog_context::in, list(mc_var)::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

    % xor(Context, A, B, !Constraints) constrains mode constraint variables A
    % and B in Constraints by the constraint (A xor B), ie constrains exactly
    % one of them to be true. Context should be the context of the goal or
    % declaration that imposed this constraint.
    %
:- pred xor(prog_context::in, mc_var::in, mc_var::in,
    pred_p_c_constraints::in, pred_p_c_constraints::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Dummy type used to distinguish mode constraint variables.
    %
:- type mc_type ---> mc_type.

    % Initialises all the parts of a mode_constraints_info type.
    %
init = pred_constraints(multi_map.init, []).

    % add_constraint(Formula, !PredConstraints) adds the constraint
    % given by Formula to the constraint system in PredConstraints.
    %
add_constraint(Context, ConstraintFormula, !PredConstraints) :-
    AllProcsConstraints = !.PredConstraints ^ pred_constraints,
    ConstraintAnnotation = constraint_annotation(Context),
    FormulaAndAnnotation = pair(ConstraintFormula, ConstraintAnnotation),
    !:PredConstraints = !.PredConstraints ^ pred_constraints :=
        list.cons(FormulaAndAnnotation, AllProcsConstraints).

    % add_constraint(Context, ProcId, Formula, !PredConstraints) adds the
    % constraint given by Formula to the constraint system in
    % PredConstraints, and associates it specificaly with procedure
    % ProcId.
    %
add_constraint(Context, ProcId, ConstraintFormula, !PredConstraints) :-
    ProcConstraints = !.PredConstraints ^ proc_constraints,
    ConstraintAnnotation = constraint_annotation(Context),
    FormulaAndAnnotation = pair(ConstraintFormula, ConstraintAnnotation),
    !:PredConstraints = !.PredConstraints ^ proc_constraints :=
        multi_map.add(ProcConstraints, ProcId, FormulaAndAnnotation).

    % pred_constraints_to_formulae returns constraints that apply
    % to all procedures of a predicate as a list of constraint formulae.
    %
pred_constraints_to_formulae(PCs) = assoc_list.keys(PCs ^ pred_constraints).

    % pred_constraints_to_formulae/2 returns all constraints that
    % apply to the given procedure from the pred_p_c_constraints,
    % including constraints that apply to all procedures.
    %
pred_constraints_to_formulae(ProcId, PredConstraints) = ConstraintFormulae :-
    ThisProcConstraints = multi_map.lookup(PredConstraints ^ proc_constraints,
        ProcId),
    AllProcConstraints = PredConstraints ^ pred_constraints,
    ConstraintFormulae = assoc_list.keys(ThisProcConstraints) ++
        assoc_list.keys(AllProcConstraints).

    % pred_constraints_to_formulae_and_annotations returns constraints
    % that apply to all procedures of a predicate as a list of pairs of
    % constraint_formula and constraint_annotation.
    %
pred_constraints_to_formulae_and_annotations(PredConstraints) =
    PredConstraints ^ pred_constraints.

    % pred_constraints_to_formulae_and_annotations(PredConstraints)
    % returns all constraints that apply to the given procedure from the
    % pred_p_c_constraints, including constraints that apply to all
    % procedures, with their constraint annotations, in pairs.
    %
pred_constraints_to_formulae_and_annotations(ProcId, PredConstraints) =
        ConstraintFormulae :-
    ThisProcConstraints = multi_map.lookup(PredConstraints ^ proc_constraints,
        ProcId),
    AllProcConstraints = PredConstraints ^ pred_constraints,
    ConstraintFormulae = ThisProcConstraints ++ AllProcConstraints.

proc_constraints_to_formulae_and_annotations(ProcId, PredConstraints) =
    multi_map.lookup(PredConstraints ^ proc_constraints, ProcId).

%-----------------------------------------------------------------------------%
%
% Predicates to allow easy adding of var_constraints.
%

equiv_no(Context, MCVar, !Constraints) :-
    add_constraint(Context, atomic_constraint(equiv_bool(MCVar, no)),
        !Constraints).

equivalent(Context, MCVars, !Constraints) :-
    add_constraint(Context, atomic_constraint(equivalent(MCVars)),
        !Constraints).

equiv_disj(Context, X, Ys, !Constraints) :-
    add_constraint(Context, atomic_constraint(equiv_disj(X, Ys)),
        !Constraints).

at_most_one(Context, MCVars, !Constraints) :-
    add_constraint(Context, atomic_constraint(at_most_one(MCVars)),
        !Constraints).

not_both(Context, A, B, !Constraints) :-
    at_most_one(Context, [A, B], !Constraints).

exactly_one(Context, MCVars, !Constraints) :-
    add_constraint(Context, atomic_constraint(exactly_one(MCVars)),
        !Constraints).

xor(Context, A, B, !Constraints) :-
    exactly_one(Context, [A, B], !Constraints).

%-----------------------------------------------------------------------------%
%
% Dumping constraints for --debug-mode-constraints
%

dump_constraints_and_annotations(Varset, ConstraintsAndAnnotations, !IO) :-
    Indent = 0,
    keys_and_values(ConstraintsAndAnnotations, Constraints, Annotations),
    list.foldl_corresponding(dump_constraint(Varset, Indent), Annotations,
        Constraints, !IO).

    % Dumps a list of constraints using the same constraint annotation
    % at indent level indicated by the int.
    %
:- pred dump_constraints(mc_varset::in, int::in, constraint_annotation::in,
    constraint_formulae::in, io::di, io::uo) is det.

dump_constraints(Varset, Indent, Annotation, Constraints, !IO) :-
    list.foldl(dump_constraint(Varset, Indent, Annotation), Constraints, !IO).

    % Prints one constraint_formulae to the output. The int is an
    % indent level.
    %
:- pred dump_constraint(mc_varset::in, int::in, constraint_annotation::in,
    constraint_formula::in, io::di, io::uo) is det.

dump_constraint(Varset, Indent, Annotation, disj(Constraints), !IO) :-
    Context = context(Annotation),
    write_error_pieces(Context, Indent, [words("disj(")], !IO),
    dump_constraints(Varset, Indent+1, Annotation, Constraints, !IO),
    write_error_pieces(Context, Indent, [words(") end disj")], !IO).

dump_constraint(Varset, Indent, Annotation, conj(Constraints), !IO) :-
    Context = context(Annotation),
    write_error_pieces(Context, Indent, [words("conj(")], !IO),
    dump_constraints(Varset, Indent+1, Annotation, Constraints, !IO),
    write_error_pieces(Context, Indent, [words(") end conj")], !IO).

dump_constraint(Varset, Indent, Annotation, atomic_constraint(Constraint),
        !IO) :-
    dump_var_constraint(Varset, Indent, Annotation, Constraint, !IO).

    % Prints a var_constraint to the output. The int is an indent level.
    %
:- pred dump_var_constraint(mc_varset::in, int::in, constraint_annotation::in,
    var_constraint::in, io::di, io::uo) is det.

dump_var_constraint(Varset, Indent, Annotation, equiv_bool(X, Val), !IO) :-
    mc_var_list_to_string(Varset, [X], VarName),
    mc_var_val_to_string(Val, ValString),
    Context = context(Annotation),
    write_error_pieces(Context, Indent,
        [words(VarName ++ " = " ++ ValString)], !IO).

dump_var_constraint(Varset, Indent, Annotation, equivalent(Xs), !IO) :-
    mc_var_list_to_string(Varset, Xs, VarsString),
    Context = context(Annotation),
    write_error_pieces(Context, Indent,
        [words("equivalent(" ++ VarsString ++ ")")], !IO).

dump_var_constraint(Varset, Indent, Annotation, implies(X, Y), !IO) :-
    mc_var_list_to_string(Varset, [X], XName),
    mc_var_list_to_string(Varset, [Y], YName),
    Context = context(Annotation),
    write_error_pieces(Context, Indent,
        [words(XName ++ " -> " ++ YName)], !IO).

dump_var_constraint(Varset, Indent, Annotation, equiv_disj(X, Xs), !IO) :-
    mc_var_list_to_string(Varset, [X], XName),
    mc_var_list_to_string(Varset, Xs, XsString),
    Context = context(Annotation),
    Pieces = [words(XName ++ " <-> disj(" ++ XsString ++ ")")],
    write_error_pieces(Context, Indent, Pieces, !IO).

dump_var_constraint(Varset, Indent, Annotation, at_most_one(Xs), !IO) :-
    mc_var_list_to_string(Varset, Xs, XsString),
    Pieces = [words("at_most_one(" ++ XsString ++ ")")],
    Context = context(Annotation),
    write_error_pieces(Context, Indent, Pieces, !IO).

dump_var_constraint(Varset, Indent, Annotation, exactly_one(Xs), !IO) :-
    mc_var_list_to_string(Varset, Xs, XsString),
    Pieces = [words("exactly_one(" ++ XsString ++ ")")],
    Context = context(Annotation),
    write_error_pieces(Context, Indent, Pieces, !IO).

    % mc_var_list_to_string(Varset, MCVars, MCVarsString)
    % Makes a comma separated list of MCVars as a string.
    %
:- pred mc_var_list_to_string(mc_varset::in, list(mc_var)::in,
    string::out) is det.

mc_var_list_to_string(_Varset, [], "").
mc_var_list_to_string(Varset, [MCVar], VarName) :-
    varset.lookup_name(Varset, MCVar, VarName).
mc_var_list_to_string(Varset, [MCVar1, MCVar2 | MCVars],
    VarName ++ ", " ++ VarNames) :-
    varset.lookup_name(Varset, MCVar1, VarName),
    mc_var_list_to_string(Varset, [MCVar2 | MCVars], VarNames).

    % Makes a string representation of an mc_var binding.
    %
:- pred mc_var_val_to_string(bool::in, string::out) is det.

mc_var_val_to_string(yes, "yes").
mc_var_val_to_string(no, "no").

%-----------------------------------------------------------------------------%
%
% Pretty printing predicates for the formulae type, and others
%

pretty_print_constraints(VarSet, Constraints, !IO) :-
    Indent = "",
    pretty_print_constraints(VarSet, Constraints, Indent, !IO).

    % Same as before, but with an indent argument used to indent
    % conjunctions and disjunctions of constraints.
    %
:- pred pretty_print_constraints(mc_varset::in, constraint_formulae::in,
    string::in, io::di, io::uo) is det.

pretty_print_constraints(_VarSet, [], _Indent, !IO).
pretty_print_constraints(VarSet, [Constr | Constrs], Indent, !IO) :-
    pretty_print_constraint(VarSet, Constr, Indent, !IO),
    pretty_print_constraints(VarSet, Constrs, Indent, !IO).

    % Prints one constraint_formulae to the output stream. Always
    % puts a new line at the end.
    %
:- pred pretty_print_constraint(mc_varset::in, constraint_formula::in,
    string::in, io::di, io::uo) is det.

pretty_print_constraint(VarSet, disj(Constraints), Indent, !IO) :-
    io.write_string(Indent, !IO),
    io.write_string("disj(\n", !IO),
    pretty_print_constraints(VarSet, Constraints, "\t" ++ Indent, !IO),
    io.write_string(Indent, !IO),
    io.write_string(") end disj\n", !IO).

pretty_print_constraint(VarSet, conj(Constraints), Indent, !IO) :-
    io.write_string(Indent, !IO),
    io.write_string("conj(\n", !IO),
    pretty_print_constraints(VarSet, Constraints, "\t" ++ Indent, !IO),
    io.write_string(Indent, !IO),
    io.write_string(") end conj\n", !IO).

pretty_print_constraint(VarSet, atomic_constraint(Constraint), Indent, !IO) :-
    io.write_string(Indent, !IO),
    pretty_print_var_constraint(VarSet, Constraint, !IO),
    io.nl(!IO).

    % Prints a var_constraint to the screen. No indents, no line
    % return.
    %
:- pred pretty_print_var_constraint(mc_varset::in, var_constraint::in,
    io::di, io::uo) is det.

pretty_print_var_constraint(VarSet, equiv_bool(X, TF), !IO) :-
    pretty_print_mc_var(VarSet, X, !IO),
    io.write_string(" = ", !IO),
    io.print(TF, !IO).

pretty_print_var_constraint(VarSet, equivalent(Xs), !IO) :-
    io.write_string("equivalent(", !IO),
    pretty_print_mc_vars(VarSet, Xs, !IO),
    io.write_string(")", !IO).

pretty_print_var_constraint(VarSet, implies(X, Y), !IO) :-
    pretty_print_mc_var(VarSet, X, !IO),
    io.write_string(" -> ", !IO),
    pretty_print_mc_var(VarSet, Y, !IO).

pretty_print_var_constraint(VarSet, equiv_disj(X, Xs), !IO) :-
    pretty_print_mc_var(VarSet, X, !IO),
    io.write_string(" <-> disj(", !IO),
    pretty_print_mc_vars(VarSet, Xs, !IO),
    io.write_string(")", !IO).

pretty_print_var_constraint(VarSet, at_most_one(Xs), !IO) :-
    io.write_string("at_most_one(", !IO),
    pretty_print_mc_vars(VarSet, Xs, !IO),
    io.write_string(")", !IO).

pretty_print_var_constraint(VarSet, exactly_one(Xs), !IO) :-
    io.write_string("exactly_one(", !IO),
    pretty_print_mc_vars(VarSet, Xs, !IO),
    io.write_string(")", !IO).

    % Prints a constraint var to the screen. No indents, no line return.
    % Simply uses the variable's name from the varset.
    %
:- pred pretty_print_mc_var(mc_varset::in, mc_var::in,
    io::di, io::uo) is det.

pretty_print_mc_var(VarSet, Var, !IO) :-
    varset.lookup_name(VarSet, Var, VarName),
    io.write_string(VarName, !IO).

    % Prints a comma separated list of constraint variables.
    %
:- pred pretty_print_mc_vars(mc_varset::in, list(mc_var)::in,
    io::di, io::uo) is det.

pretty_print_mc_vars(_VarSet, [], !IO).
pretty_print_mc_vars(VarSet, [Var | Vars], !IO) :-
    pretty_print_mc_var(VarSet, Var, !IO),
    (
        Vars = []
    ;
        Vars = [_ | _],
        io.write_string(", ", !IO),
        pretty_print_mc_vars(VarSet, Vars, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Prints a list of models for the constraint system.
    %
pretty_print_solutions(VarSet, Solutions, !IO) :-
    list.foldl2(pretty_print_bindings(VarSet), Solutions, 0, _,  !IO).

    % Prints the variable bindings of this solution, one per line.
    %
:- pred pretty_print_bindings(mc_varset::in, mc_bindings::in, int::in, int::out,
    io::di, io::uo) is det.

pretty_print_bindings(VarSet, Bindings, N, N + 1, !IO) :-
    io.write_string("Solution " ++ string.from_int(N) ++ ":\n{\n", !IO),
    Variables = map.keys(Bindings),
    list.foldl(pretty_print_binding(VarSet, Bindings), Variables, !IO),
    io.write_string("}\n", !IO).

:- pred pretty_print_binding(mc_varset::in, mc_bindings::in, mc_var::in,
    io::di, io::uo) is det.

pretty_print_binding(VarSet, Bindings, Var, !IO) :-
    io.write_string("    ", !IO),
    io.write_string(varset.lookup_name(VarSet, Var), !IO),
    io.write_string(" = ", !IO),
    map.lookup(Bindings, Var, Value),
    io.print(Value, !IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%
:- end_module abstract_mode_constraints.
%----------------------------------------------------------------------------%
