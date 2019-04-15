%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1996, 2004-2006, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: graph_colour.m.
% Main author: conway.
%
% This file contains functionality to find a 'good' colouring of a graph.
% The predicate group_elements(set(set(T)), set(set(T))),
% takes a set of sets each containing elements that touch, and returns
% a set of sets each containing elements that can be assigned the same
% colour, ensuring that touching elements have different colours.
% ("Good" means using as few colours as possible.)
%
% XXX We do not use this module anymore. Instead, we use set_of_var.m,
% which uses a more efficient representation of sets of elements. Since that
% more efficient representation depends on knowing that the elements are
% variables and therefore in the enum type class, a generic module like this
% cannot use that representation.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module libs.graph_colour.
:- interface.

:- import_module set.

%-----------------------------------------------------------------------------%

:- pred group_elements(set(set(T))::in, set(set(T))::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

group_elements(!.Constraints, Colours) :-
    set.power_union(!.Constraints, AllVars),
    set.init(EmptySet),
    set.delete(EmptySet, !Constraints),
    set.to_sorted_list(!.Constraints, ConstraintList),
    find_all_colours(ConstraintList, AllVars, ColourList),
    set.list_to_set(ColourList, Colours),

    % Performance reducing sanity check.
    trace [compile_time(flag("graph_colour_assertions"))] (
        ( if
            set.power_union(Colours, AllColours),
            (
                set.member(Var, AllVars)
            =>
                set.member(Var, AllColours)
            )
        then
            unexpected($pred, "sanity check failed")
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Iterate the assignment of a new colour until all constraints
    % are satisfied.
    %
:- pred find_all_colours(list(set(T))::in, set(T)::in,
    list(set(T))::out) is det.

find_all_colours(ConstraintList, Vars, ColourList) :-
    (
        ConstraintList = [],
        ColourList = []
    ;
        ConstraintList = [_ | _],
        next_colour(Vars, ConstraintList, RemainingConstraints, Colour),
        set.difference(Vars, Colour, RestVars),
        find_all_colours(RemainingConstraints, RestVars, ColourList0),
        ColourList = [Colour | ColourList0]
    ).

%-----------------------------------------------------------------------------%

:- pred next_colour(set(T)::in, list(set(T))::in,
    list(set(T))::out, set(T)::out) is det.

next_colour(Vars0, ConstraintList, Remainder, SameColour) :-
    % Check if there are any constraints left to be satisfied.
    (
        ConstraintList = [_ | _],
        % Select a variable to assign a colour, ...
        choose_var(Vars0, Var, Vars1),

        % ... and divide the constraints into those that
        % may be the same colour as that var and those
        % that may not.
        divide_constraints(Var, ConstraintList, WereContaining, NotContaining,
            Vars1, RestVars),
        (
            % See if there are sets that can share a colour with the
            % selected var.
            NotContaining = [_ | _],
            ( if set.is_empty(RestVars) then
                % There were no variables left that could share a colour,
                % so create a singleton set containing this variable.
                SameColour = set.make_singleton_set(Var),
                ResidueSets = NotContaining
            else
                % If there is at least one variable that can share a colour
                % with the selected variable, then recursively use the
                % remaining constraints to assign a colour to one of the
                % remaining vars, and assemble the constraint residues.
                next_colour(RestVars, NotContaining, ResidueSets, SameColour0),

                % Add this variable to the variables of the current colour.
                set.insert(Var, SameColour0, SameColour)
            )
        ;
            NotContaining = [],
            % There were no more constraints which could be satisfied
            % by assigning any variable a colour the same as the current
            % variable, so create a signleton set with the current var,
            % and assign the residue to the empty set.
            SameColour = set.make_singleton_set(Var),
            ResidueSets = []
        ),
        % The remaining constraints are the residue sets that could not be
        % satisfied by assigning any variable to the current colour, and the
        % constraints that were already satisfied by the assignment of the
        % current variable to this colour.
        list.append(ResidueSets, WereContaining, Remainder)
    ;
        % If there were no constraints, then no colours were needed.
        ConstraintList = [],
        Remainder = [],
        set.init(SameColour)
    ).

%-----------------------------------------------------------------------------%

    % Divide_constraints takes a var and a list of sets of var, and divides
    % the list into two lists: a list of sets containing the given variable
    % and a list of sets not containing that variable. The sets in the list
    % containing the variable have that variable removed. Additionally, a set
    % of variables is threaded through the computation, and any variables that
    % were in sets that also contained the given variables are removed from
    % the threaded set.
    %
:- pred divide_constraints(T::in, list(set(T))::in,
    list(set(T))::out, list(set(T))::out, set(T)::in, set(T)::out) is det.

divide_constraints(_Var, [], [], [], !Vars).
divide_constraints(Var, [S | Ss], C, NC, !Vars) :-
    divide_constraints(Var, Ss, C0, NC0, !Vars),
    ( if set.member(Var, S) then
        set.delete(Var, S, T),
        ( if set.is_empty(T) then
            C = C0
        else
            C = [T | C0]
        ),
        NC = NC0,
        set.difference(!.Vars, T, !:Vars)
    else
        C = C0,
        NC = [S | NC0]
    ).

%-----------------------------------------------------------------------------%

    % Choose_var/3, given a set of variables, chooses one, returns it
    % and the set with that variable removed.
    %
:- pred choose_var(set(T)::in, T::out, set(T)::out) is det.

choose_var(Vars0, Var, Vars) :-
    ( if set.remove_least(VarPrime, Vars0, VarsPrime) then
        Var = VarPrime,
        Vars = VarsPrime
    else
        unexpected($pred, "no vars!")
    ).

%-----------------------------------------------------------------------------%
:- end_module libs.graph_colour.
%-----------------------------------------------------------------------------%
