%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: abstract_mode_constraints.m
% Main author: richardf
%
% This module contains data structures designed for use with constraints
% based mode analysis. They represent constraints between constraint
% variables, such as those one might use to describe where a program
% variable may be produced.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.abstract_mode_constraints.

:- interface.

:- import_module bool.
:- import_module counter.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type mc_type.

:- type mc_var == var(mc_type).		% Constraint variable.

:- type mc_varset == varset(mc_type).	% Source of constraint variables
					% And their names.
:- type vars(T) == list(var(T)).

%-----------------------------------------------------------------------------%
%
% Data structures for storing abstract constraints. Conjunctions and
% disjunctions can be formed. The atomic constraints between constraint
% variables are designed around the types of constraints required by
% constraint based mode analysis. The paper "Constraint-based mode
% analysis of Mercury" by David Overton, Zoltan Somogyi and Peter
% Stuckey documents these mode constraints.
%

	% Represents conjunctions and disjunctions between atomic
	% constraints on constraint variables.  The advantage of the
	% constraints for this implementation of mode checking is that
	% they can be expressed almost entirely as variable to variable
	% constraints, with little use for the disj and conj functors of
	% this structure.
	%
:- type constraint_formulae == list(constraint_formula).
:- type constraint_formula
	--->	atomic_constraint(var_constraint)
	;	disj(constraint_formulae)
			% Initially included for the purposes of
			% representing the mode declaration constraints,
			% which are a disjunction of conjunctions of
			% other constraints.  The intended form is:
			% disj([conj(...), ..., conj(...)]) Note
			% disj([]) represents false.
	;	conj(constraint_formulae)
			% See disj.
			% Note that conj([]) is the empty constraint, or true.
.


% var_constraint represents a constraint between variables
:- type var_constraint == var_constraint(mc_type).
:- type var_constraint(T)
	--->	equiv_bool(var(T), bool)
			% equiv_bool(X, yes) gives the constraint (X)
			% equiv_bool(X, no) gives the constraint not(X)
	;	equivalent(vars(T))
			% equivalent(Xs) gives (all Xi, Xj in Xs).(Xi<->Xj)
	;	implies(var(T), var(T))
			% implies(X, Y) gives X->Y
	;	equiv_disj(var(T), vars(T))
			% equiv_disj(X, [Y1,...,Yn]) gives X<->OR(Y1,...,Yn)
			% XXX Thinking of making a constraint that is
			% the conjunction of equiv_disj and at_most_one
			% because they occur together so often.
	;	at_most_one(vars(T))
			% at_most_one(Xs) gives
			% (all Xi, Xj in Xs).(i = j or not(Xi and Xj))
	;	at_least_one(vars(T))
			% at_least_one(Xs) gives OR(Xs)
	;	exactly_one(vars(T)).
			% exactly_one(Xs) gives
			% at_least_one(Xs) and at_most_one(Xs)

	% Attempts to print the constraint_formulae it is passed in a
	% human readable format. Always puts a new line after it is
	% finished.
	%
:- pred pretty_print_constraints(mc_varset::in, constraint_formulae::in,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% The remainder of the code in this module is not in use! It is intended
% to supply a basic structure for a propagation solver to deal with the
% constraints defined above.
%
% Propagation would work like this: Initially constraint variables are
% bound during constraint construction, propagation, or once all
% possible propagation has been done, a variable may have true or false
% arbitrarily chosen for it and the choicepoint recorded.  Once a
% variable has been bound, the propagator looks it up in the var_map
% field of mode_constraints_info. The var_state structure will then
% reveal which constraints this variable participates in. These
% constraints are then looked up in the constraint_map field of
% mode_constraints_info. Then can be refined due to the variable having
% been bound, and may now imply new bindings on other variables. These
% new variables can be put in a stack, queue or similar structure to be
% bound and propagated once the first variable's constraints have been
% fully processed.
%
% This code is by no means complete; some speculative fields exist or
% have been commented out. Only the basic structure and functionality
% described above is truly intended.
%

% XXX	What's lacking in this is the following:
% Done 	1)	An at least one constraint, for when equiv_disj simplifies
% 	2)	A good reason to keep the propagate if true/false lists
% 	3)	A separate way to store lists of variable equivalences,
% 		so that only one of a list of equivalent variables need
% 		be used in the constraints.
% 	4)	A way to store whether a variable being bound will have
% 		an effect on a particular constraint - but this can change
% 		without reference to a variable, so maybe not a practical idea.
% 	5)	T/F lists for variables known to be true/false for all
% 		possible models?
% 	6)	Access and manipulation predicates.

:- type mode_constraints_info --->
	mode_constraints_info(
		constraint_map	:: map(constraint_id, constraint),
			% Constraining the variables in the var_map.
		var_map		:: map(mc_var, var_state),
			% The variables this constraint system constrains
		id_counter	:: counter
			% Supplies unique IDs for the constraint map.
	).

:- type constraint_id == int.

:- type constraint --->
	constraint(
		id		:: constraint_id,
		current		:: constraint_formula_and_vars,
			% Formula modified as variables are bound.
		original	:: constraint_formula_and_vars
%		dead		:: bool
%			% for if the current constraint is empty... don't
%			% know if this will be used
	).

:- type constraint_formula_and_vars --->
	constraint_formula_and_vars(
		constraint_formula	:: constraint_formula,
		participating_vars	:: vars(mc_type)
	).

:- type var_state --->
	var_state(
		is_bound	:: maybe(var_binding),
			% If it is bound, some certain information
			% should be recorded. See var_binding type.
		is_constrained	:: list(constrainment_info)
			% A list of the constraints that this variable
			% participates in. May include propagation tree
			% information in the form of branches for bound
			% true/false, however this may have a
			% significant cost in space.
	).

:- type var_binding --->
	var_binding(
		bool			:: bool,
			% What the variable has been bound to
		binding_constraint	:: constraint_id,
			% The constraint that finally bound it
		history			:: list(mc_var)
			% The variables in the constraint that bound
			% this variable that had already been bound at
			% that point - their binding_constraint and
			% history can be looked at recursively to build
			% the full history. I strongly prefer this to
			% listing the full history each time a variable
			% is bound, for space considerations.
	).

:- type constrainment_info --->
	constrainment_info(
%		variable		:: mc_var,
%			% To make it clear what variable the propagation
%			% information is for.
%		propagate_if_true	:: list(var_binding),
%			% The history field should be empty. In the end it
%			% gets the participating variables of this constraint
%			% that have been bound.
%		propagate_if_false	:: list(var_binding),
		constraint		:: constraint_id
	).

	% Initiates all the parts of a mode_constraints_info type.
	%
:- pred abstract_mode_constraints.init(mode_constraints_info::out) is det.

	% Function version if init/1.
:- func abstract_mode_constraints.init = mode_constraints_info.

	% Incorporates a new constraint into the system.
	%
:- pred abstract_mode_constraints.add_constraint(constraint_formula::in,
	mode_constraints_info::in, mode_constraints_info::out) is det.

	% Function version of add_constraint/3.
	%
:- func abstract_mode_constraints.add_constraint(constraint_formula,
	mode_constraints_info) = mode_constraints_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

:- type mc_type ---> mc_type.

	% Initiates all the parts of a mode_constraints_info type.
	%
abstract_mode_constraints.init(ModeConstraintsInfo) :-
	ModeConstraintsInfo = mode_constraints_info(
		map.init,
		map.init,
		counter.init(0)		% Start allocating ids from 0
	).

	% See the predicate version.
	%
abstract_mode_constraints.init = ModeConstraintsInfo :-
	abstract_mode_constraints.init(ModeConstraintsInfo).

	% Incorporates a new constraint into the system.
	%
abstract_mode_constraints.add_constraint(ConstraintFormula,
		!ModeConstraintsInfo) :-
	formula_to_formula_and_vars(ConstraintFormula, 
		Vars, FormulaAndVars),
	counter.allocate(NewID, !.ModeConstraintsInfo ^ id_counter,
		NewCounter),
	!:ModeConstraintsInfo =
		!.ModeConstraintsInfo ^ id_counter := NewCounter,
	update_vars_map_with_constrainment_info(constrainment_info(NewID),
		Vars, !ModeConstraintsInfo),
	!:ModeConstraintsInfo = !.ModeConstraintsInfo ^ constraint_map :=
		map.det_insert(!.ModeConstraintsInfo ^ constraint_map,
			NewID, 
			constraint(NewID, FormulaAndVars, FormulaAndVars)).
	
	% Functional version of add_constraint/3.
	%
abstract_mode_constraints.add_constraint(CF, MCI0) = MCI :-
	abstract_mode_constraints.add_constraint(CF, MCI0, MCI).

% update_vars_map_with_constrainment_info adds the supplied
% constrainment_info to the list of constraints associated with each of
% the variables supplied in the mode_constraints_info structure.

:- pred update_vars_map_with_constrainment_info(
	constrainment_info::in, vars(mc_type)::in,
	mode_constraints_info::in, mode_constraints_info::out
	) is det.

update_vars_map_with_constrainment_info(_ConstrainmentInfo, [], !MCI).
update_vars_map_with_constrainment_info(ConstrainmentInfo, [Var|Vars], !MCI) :-
	(	map.search(!.MCI ^ var_map, Var, VarState1)
	->	CInfoList = [ConstrainmentInfo| VarState1 ^ is_constrained],
		VarState = VarState1 ^ is_constrained := CInfoList,
		!:MCI = !.MCI ^ var_map :=
			map.det_update(!.MCI ^ var_map, Var, VarState)
	;	VarState = var_state(no, [ConstrainmentInfo]),
		!:MCI = !.MCI ^ var_map :=
			map.det_insert(!.MCI ^ var_map, Var, VarState)
	),
	update_vars_map_with_constrainment_info(ConstrainmentInfo, Vars, !MCI).

% formula_to_formula_and_vars makes the list Vars of variables that
% appear in Formula and packages Formula and Vars together in
% FormulaAndVars.
:- pred formula_to_formula_and_vars(
	constraint_formula::in,
	vars(mc_type)::out,
	constraint_formula_and_vars::out
	) is det.

formula_to_formula_and_vars(Formula, Vars, FormulaAndVars) :-
	formula_to_vars(Formula, Vars),
	FormulaAndVars = constraint_formula_and_vars(Formula, Vars).

% Sub section of the formula_to_formula_and_vars predicate, Vars is the
% variables that appear in Formula
:- pred formula_to_vars(constraint_formula::in, vars(mc_type)::out) is det.

formula_to_vars(Formula, Vars) :-
	Formula = atomic_constraint(VarConstraint),
	var_constraint_to_vars(VarConstraint, Vars).
formula_to_vars(Formula, Vars) :-
	Formula = disj(Formulae),
	list.foldr(
		(pred(Formula1::in, Vars1::in, VarsNew::out) is det :-
			formula_to_vars(Formula1, Vars2),
			append(Vars2, Vars1, VarsNew)
		),
		Formulae,
		[],
		Vars
	).
formula_to_vars(Formula, Vars) :-
	Formula = conj(Formulae),
	list.foldr(
		(pred(Formula1::in, Vars1::in, VarsNew::out) is det :-
			formula_to_vars(Formula1, Vars2),
			append(Vars2, Vars1, VarsNew)
		),
		Formulae,
		[],
		Vars
	).

% var_constraint_to_vars takes a constraint between variables as input
% and gives a list of those variables as output.
:- pred var_constraint_to_vars(var_constraint::in, vars(mc_type)::out) is det.

var_constraint_to_vars(equiv_bool(Var, _B), [Var]).
var_constraint_to_vars(equivalent(Vars), Vars).
var_constraint_to_vars(implies(V1, V2), [V1, V2]).
var_constraint_to_vars(equiv_disj(Var, Vars), [Var|Vars]).
var_constraint_to_vars(at_most_one(Vars), Vars).
var_constraint_to_vars(at_least_one(Vars), Vars).
var_constraint_to_vars(exactly_one(Vars), Vars).

% Some thoughts:
% add_constraint or similar pred adds the specified constraint (making
% sure that the variables in it are added if need be, and updated with a
% link to that constraint. What to do if one or more of the variables
% are already bound? Still put in the original - so that you can always
% know that if you put something in it will be there, but also put in an
% updated version according to the bindings? This seems like a good
% idea, though it forces propagation...
% Any predicate that edits constraints leaves the 'original' as it is,
% and only modifies the 'current' branch - eg replacing one var with an
% equivalent etc.

%-----------------------------------------------------------------------------%
%
% Pretty printing predicates for the formulae type, and others
%

pretty_print_constraints(Varset, Constraints, !IO) :-
	pretty_print_constraints(
		Varset,
		Constraints,
		"",		% Extra argument for indent.
		!IO
	).

	% Same as before, but with an indent argument used to indent
	% conjunctions and disjunctions of constraints.
	%
:- pred pretty_print_constraints(mc_varset::in, constraint_formulae::in,
	string::in, io::di, io::uo) is det.

pretty_print_constraints(_Varset, [], _Indent, !IO).
pretty_print_constraints(Varset, [Constr|Constrs], Indent, !IO) :-
	pretty_print_constraint(Varset, Constr, Indent, !IO),
	pretty_print_constraints(Varset, Constrs, Indent, !IO).

	% Prints one constraint_formulae to the output stream. Always
	% puts a new line at the end.
	%
:- pred pretty_print_constraint(mc_varset::in, constraint_formula::in,
	string::in, io::di, io::uo) is det.

pretty_print_constraint(Varset, disj(Constraints), Indent, !IO) :-
	io.print(Indent, !IO),
	io.print("disj(\n", !IO),
	pretty_print_constraints(
		Varset, Constraints, "\t" ++ Indent, !IO),
	io.print(Indent, !IO),
	io.print(") end disj", !IO),
	io.nl(!IO).

pretty_print_constraint(Varset, conj(Constraints), Indent, !IO) :-
	io.print(Indent, !IO),
	io.print("conj(\n", !IO),
	pretty_print_constraints(
		Varset, Constraints, "\t" ++ Indent, !IO),
	io.print(Indent, !IO),
	io.print(") end conj", !IO),
	io.nl(!IO).

pretty_print_constraint(
	Varset, atomic_constraint(Constraint), Indent, !IO) :-
	io.print(Indent, !IO),
	pretty_print_var_constraint(Varset, Constraint, !IO),
	io.nl(!IO).

	% Prints a var_constraint to the screen. No indents, no line
	% return.
	%
:- pred pretty_print_var_constraint(mc_varset::in, var_constraint::in,
	io::di, io::uo) is det.

pretty_print_var_constraint(Varset, equiv_bool(X, TF), !IO) :-
	pretty_print_mc_var(Varset, X, !IO),
	io.print(" = ", !IO),
	io.print(TF, !IO).

pretty_print_var_constraint(Varset, equivalent(Xs), !IO) :-
	io.print("equivalent(", !IO),
	pretty_print_mc_vars(Varset, Xs, !IO),
	io.print(")", !IO).

pretty_print_var_constraint(Varset, implies(X, Y), !IO) :-
	pretty_print_mc_var(Varset, X, !IO),
	io.print(" -> ", !IO),
	pretty_print_mc_var(Varset, Y, !IO).

pretty_print_var_constraint(Varset, equiv_disj(X, Xs), !IO) :-
	pretty_print_mc_var(Varset, X, !IO),
	io.print(" <-> disj(", !IO),
	pretty_print_mc_vars(Varset, Xs, !IO),
	io.print(")", !IO).

pretty_print_var_constraint(Varset, at_most_one(Xs), !IO) :-
	io.print("at_most_one(", !IO),
	pretty_print_mc_vars(Varset, Xs, !IO),
	io.print(")", !IO).

pretty_print_var_constraint(Varset, at_least_one(Xs), !IO) :-
	io.print("at_least_one(", !IO),
	pretty_print_mc_vars(Varset, Xs, !IO),
	io.print(")", !IO).

pretty_print_var_constraint(Varset, exactly_one(Xs), !IO) :-
	io.print("exactly_one(", !IO),
	pretty_print_mc_vars(Varset, Xs, !IO),
	io.print(")", !IO).

	% Prints a constraint var to the screen. No indents, no line
	% return.  Simply uses the variable's name from the varset.
	%
:- pred pretty_print_mc_var(mc_varset::in, mc_var::in,
	io::di, io::uo) is det.

pretty_print_mc_var(Varset, Var, !IO) :-
	varset.lookup_name(Varset, Var, VarName),
	io.print(VarName, !IO).

	% Prints a comma separated list of constraint variables.
	%
:- pred pretty_print_mc_vars(mc_varset::in, list(mc_var)::in,
	io::di, io::uo) is det.

pretty_print_mc_vars(_Varset, [], !IO).
pretty_print_mc_vars(Varset, [Var| Tail], !IO) :-
	pretty_print_mc_var(Varset, Var, !IO),
	pretty_print_mc_vars_tail(Varset, Tail, !IO).

	% Prints a comma separated list of constraint variables under
	% the assumption that at least one has already been printed - ie
	% prints a divider then prints the rest of the list.  If there
	% is no more list it does nothing.
	%
:- pred pretty_print_mc_vars_tail(mc_varset::in, list(mc_var)::in,
	io::di, io::uo) is det.

pretty_print_mc_vars_tail(_Varset, [], !IO).
pretty_print_mc_vars_tail(Varset, [Var| Vars], !IO) :-
	io.print(", ", !IO),
	pretty_print_mc_vars(Varset, [Var| Vars], !IO).

%-----------------------------------------------------------------------------%
:- end_module abstract_mode_constraints.
%-----------------------------------------------------------------------------%
