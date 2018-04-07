%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: set_of_var.m.
%
% A module to define the abstract data structure we use to represent
% sets of variables.
%
%---------------------------------------------------------------------------%

:- module parse_tree.set_of_var.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module set.
:- import_module term.

:- type set_of_var(T).
:- type set_of_progvar == set_of_var(prog_var_type).
:- type set_of_tvar    == set_of_var(tvar_type).

:- func init = set_of_var(T).
:- pred init(set_of_var(T)::out) is det.

:- func make_singleton(var(T)) = set_of_var(T).
:- pred make_singleton(var(T)::in, set_of_var(T)::out) is det.

:- func count(set_of_var(T)) = int.

%---------------------%
% Tests.

:- pred is_empty(set_of_var(T)::in) is semidet.
:- pred is_non_empty(set_of_var(T)::in) is semidet.
:- pred is_singleton(set_of_var(T)::in, var(T)::out) is semidet.

:- pred member(set_of_var(T), var(T)).
:- mode member(in, in) is semidet.
:- mode member(in, out) is nondet.

:- pred is_member(set_of_var(T)::in, var(T)::in, bool::out) is det.

:- pred contains(set_of_var(T)::in, var(T)::in) is semidet.

:- pred equal(set_of_var(T)::in, set_of_var(T)::in) is semidet.

%---------------------%
% Conversions.

:- func list_to_set(list(var(T))) = set_of_var(T).
:- func sorted_list_to_set(list(var(T))) = set_of_var(T).
:- func to_sorted_list(set_of_var(T)) = list(var(T)).

:- pred list_to_set(list(var(T))::in, set_of_var(T)::out) is det.
:- pred sorted_list_to_set(list(var(T))::in, set_of_var(T)::out) is det.
:- pred to_sorted_list(set_of_var(T)::in, list(var(T))::out) is det.

:- func set_to_bitset(set(var(T))) = set_of_var(T).
:- func bitset_to_set(set_of_var(T)) = set(var(T)).

%---------------------%
% Updates.

:- pred insert(var(T)::in,
    set_of_var(T)::in, set_of_var(T)::out) is det.
:- pred insert_list(list(var(T))::in,
    set_of_var(T)::in, set_of_var(T)::out) is det.
:- pred delete(var(T)::in,
    set_of_var(T)::in, set_of_var(T)::out) is det.
:- pred delete_list(list(var(T))::in,
    set_of_var(T)::in, set_of_var(T)::out) is det.
:- pred remove(var(T)::in,
    set_of_var(T)::in, set_of_var(T)::out) is semidet.
:- pred remove_list(list(var(T))::in,
    set_of_var(T)::in, set_of_var(T)::out) is semidet.
:- pred remove_least(var(T)::out,
    set_of_var(T)::in, set_of_var(T)::out) is semidet.

%---------------------%
% Set operations.

:- func union(set_of_var(T), set_of_var(T)) = set_of_var(T).
:- pred union(set_of_var(T)::in, set_of_var(T)::in,
    set_of_var(T)::out) is det.

:- func union_list(list(set_of_var(T))) = set_of_var(T).
:- pred union_list(list(set_of_var(T))::in, set_of_var(T)::out) is det.

:- func intersect(set_of_var(T), set_of_var(T)) = set_of_var(T).
:- pred intersect(set_of_var(T)::in, set_of_var(T)::in,
    set_of_var(T)::out) is det.

:- func intersect_list(list(set_of_var(T))) = set_of_var(T).
:- pred intersect_list(list(set_of_var(T))::in, set_of_var(T)::out) is det.

:- func difference(set_of_var(T), set_of_var(T)) = set_of_var(T).
:- pred difference(set_of_var(T)::in, set_of_var(T)::in,
    set_of_var(T)::out) is det.

:- pred divide(pred(var(T))::in(pred(in) is semidet), set_of_var(T)::in,
    set_of_var(T)::out, set_of_var(T)::out) is det.

:- pred divide_by_set(set_of_var(T)::in, set_of_var(T)::in,
    set_of_var(T)::out, set_of_var(T)::out) is det.

:- pred cartesian_product(set_of_var(T)::in, set_of_var(T)::in,
    list(set_of_var(T))::out) is det.

:- pred cartesian_product_list(list(set_of_var(T))::in,
    list(set_of_var(T))::out) is det.

%---------------------%
% Traversals.

:- pred fold(pred(var(T), Acc, Acc), set_of_var(T), Acc, Acc).
:- mode fold(pred(in, in, out) is det, in, in, out) is det.
:- mode fold(pred(in, in, out) is semidet, in, in, out) is semidet.

:- pred fold_func((func(var(T), Acc) = Acc), set_of_var(T), Acc, Acc).
:- mode fold_func(in((func(in, in) = out) is det), in, in, out) is det.

    % `filter(Pred, Set) = TrueSet' returns the elements of Set for which
    % Pred succeeds.
    %
:- func filter(pred(var(T))::in(pred(in) is semidet), set_of_var(T)::in)
    = (set_of_var(T)::out) is det.
:- pred filter(pred(var(T))::in(pred(in) is semidet),
    set_of_var(T)::in, set_of_var(T)::out) is det.

    % `filter(Pred, Set, TrueSet, FalseSet)' returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(var(T))::in(pred(in) is semidet),
    set_of_var(T)::in, set_of_var(T)::out, set_of_var(T)::out) is det.

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(var(T))::in(pred(in) is semidet), set_of_var(T)::in)
    is semidet.

%---------------------%
% Graph colouring.

    % Find a 'good' colouring of a graph.
    % The predicate takes a set of sets each containing elements that touch,
    % and returns a set of sets each containing elements that can be assigned
    % the same colour, ensuring that touching elements have different colours.
    % ("Good" means using as few colours as possible.)
    %
:- pred graph_colour_group_elements(set(set_of_var(T))::in,
    set(set_of_var(T))::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

% We want to define set_of_var as sparse_bitset for performance.
% However, until we have user-specified pretty printing in the debugger,
% debugging will be much easier if set_of_var is just a plain set.
% The definition of the type is hidden here to make it relatively easy
% to change.
%
% If you want to debug a new set representation, then
%
% - make the test_bitset.m module use the new representation instead of
%   sparse_bitset.m (all the operations will be run both on the new
%   representation and on set_ordlist, aborting on any discrepancy),
%
% - change every occurrence of sparse_bitset in this file that is on a line
%   containing MODULE to test_bitset.
%
% Once the representation has been proven, you can change all those occurrences
% of test_bitset to the name of the module implementing the new representation.

:- import_module sparse_bitset.                                       % MODULE
:- import_module require.

:- type set_of_var(T) ==  sparse_bitset(var(T)).                      % MODULE

%---------------------------------------------------------------------------%

init = sparse_bitset.init.                                            % MODULE
init(Set) :-
    Set = set_of_var.init.

make_singleton(Elem) = sparse_bitset.make_singleton_set(Elem).        % MODULE
make_singleton(Elem, Set) :-
    Set = set_of_var.make_singleton(Elem).

count(Set) = Count :-
    Count = sparse_bitset.count(Set).                                 % MODULE

%---------------------%
% Tests.

is_empty(Set) :-
    sparse_bitset.is_empty(Set).                                      % MODULE

is_non_empty(Set) :-
    sparse_bitset.is_non_empty(Set).                                  % MODULE

is_singleton(Set, Elem) :-
    sparse_bitset.is_singleton(Set, Elem).                            % MODULE

member(Set, Elem) :-
    sparse_bitset.member(Elem, Set).                                  % MODULE

is_member(Set, Elem, IsMember) :-
    ( if set_of_var.contains(Set, Elem) then
        IsMember = yes
    else
        IsMember = no
    ).

contains(Set, Elem) :-
    sparse_bitset.contains(Set, Elem).                                % MODULE

equal(SetA, SetB) :-
    sparse_bitset.equal(SetA, SetB).                                  % MODULE

%---------------------%
% Conversions.

list_to_set(List) = sparse_bitset.list_to_set(List).                  % MODULE

sorted_list_to_set(List) = sparse_bitset.sorted_list_to_set(List).    % MODULE

to_sorted_list(Set) = sparse_bitset.to_sorted_list(Set).              % MODULE

list_to_set(List, Set) :-
    Set = set_of_var.list_to_set(List).

sorted_list_to_set(List, Set) :-
    Set = set_of_var.sorted_list_to_set(List).

to_sorted_list(Set, List) :-
    List = set_of_var.to_sorted_list(Set).

set_to_bitset(OrdSet) = BitSet :-
    % We don't use from_set, since set.m itself doesn't have that.
    set.to_sorted_list(OrdSet, List),
    sparse_bitset.sorted_list_to_set(List, BitSet).                   % MODULE

bitset_to_set(BitSet) = OrdSet :-
    % We don't use to_set, since set.m itself doesn't have that.
    sparse_bitset.to_sorted_list(BitSet, List),                       % MODULE
    set.sorted_list_to_set(List, OrdSet).

%---------------------%
% Updates.

insert(Elem, !Set) :-
    sparse_bitset.insert(Elem, !Set).                                 % MODULE

insert_list(Elems, !Set) :-
    sparse_bitset.insert_list(Elems, !Set).                           % MODULE

delete(Elem, !Set) :-
    sparse_bitset.delete(Elem, !Set).                                 % MODULE

delete_list(Elems, !Set) :-
    sparse_bitset.delete_list(Elems, !Set).                           % MODULE

remove(Elem, !Set) :-
    sparse_bitset.remove(Elem, !Set).                                 % MODULE

remove_list(Elems, !Set) :-
    sparse_bitset.remove_list(Elems, !Set).                           % MODULE

remove_least(LeastElem, !Set) :-
    sparse_bitset.remove_least(LeastElem, !Set).                      % MODULE

%---------------------%
% Set operations.

union(SetA, SetB) = sparse_bitset.union(SetA, SetB).                  % MODULE
union(SetA, SetB, Set) :-
    sparse_bitset.union(SetA, SetB, Set).                             % MODULE

union_list(Sets) = sparse_bitset.union_list(Sets).                    % MODULE
union_list(Sets, Set) :-
    Set = sparse_bitset.union_list(Sets).                             % MODULE

intersect(SetA, SetB) = sparse_bitset.intersect(SetA, SetB).          % MODULE
intersect(SetA, SetB, Set) :-
    sparse_bitset.intersect(SetA, SetB, Set).                         % MODULE

intersect_list(Sets) = sparse_bitset.intersect_list(Sets).            % MODULE
intersect_list(Sets, Set) :-
    Set = sparse_bitset.intersect_list(Sets).                         % MODULE

difference(SetA, SetB) = sparse_bitset.difference(SetA, SetB).        % MODULE
difference(SetA, SetB, Set) :-
    sparse_bitset.difference(SetA, SetB, Set).                        % MODULE

divide(Pred, Set, InPart, OutPart) :-
    sparse_bitset.divide(Pred, Set, InPart, OutPart).                 % MODULE

divide_by_set(DivideBySet, Set, InPart, OutPart) :-
    sparse_bitset.divide_by_set(DivideBySet, Set, InPart, OutPart).   % MODULE

cartesian_product(A, B, Product) :-
    sparse_bitset.foldl(cartesian_product2(A), B, [], Product).       % MODULE

:- pred cartesian_product2(set_of_var(T)::in, var(T)::in,
    list(set_of_var(T))::in, list(set_of_var(T))::out) is det.

cartesian_product2(SetA, VarB, !Sets) :-
    Pred =
        (pred(VarA::in, SetsI0::in, SetsI::out) is det :-
            Set = set_of_var.list_to_set([VarA, VarB]),
            SetsI = [Set | SetsI0]
        ),
    set_of_var.fold(Pred, SetA, !Sets).

cartesian_product_list([], []).
cartesian_product_list([FirstSet | OtherSets], Product) :-
    list.foldl(cartesian_product_list2(FirstSet), OtherSets, [], Product).

:- pred cartesian_product_list2(set_of_var(T)::in, set_of_var(T)::in,
    list(set_of_var(T))::in, list(set_of_var(T))::out) is det.

cartesian_product_list2(A, B, SetsAcc, Product ++ SetsAcc) :-
    cartesian_product(A, B, Product).

%---------------------%
% Traversals.

fold(P, Set, !Acc) :-
    sparse_bitset.foldl(P, Set, !Acc).                                % MODULE

fold_func(P, Set, !Acc) :-
    !:Acc = sparse_bitset.foldl(P, Set, !.Acc).                       % MODULE

filter(P, Set) = sparse_bitset.filter(P, Set).                        % MODULE

filter(P, Set, Trues) :-
    Trues = sparse_bitset.filter(P, Set).                             % MODULE

filter(P, Set, Trues, Falses) :-
     sparse_bitset.filter(P, Set, Trues, Falses).                     % MODULE

all_true(P, Set) :-
     sparse_bitset.all_true(P, Set).                                  % MODULE

%---------------------%
% Graph colouring.

% The code of graph_colour_group_elements and its auxiliary predicates
% is adapted from graph_colour.m.
%
% Note that this algorithm is NOT guaranteed to find the exact same colour
% assignment as graph_colour.m. That is because the sorted list of sets that
% find_all_colours iterates over is sorted by different criteria when the
% elements are set(prog_var), as in graph_colour.m, and when they are
% set_of_progvar, as they are here. The same is true for the set of colours
% that graph_colour_group_elements returns. However, you *do* get the exact
% same results if you re-sort both the input and output sets-of-sets using
% the set.m set representation of the elements.

graph_colour_group_elements(!.Constraints, Colours) :-
    set.delete(set_of_var.init, !Constraints),
    set.to_sorted_list(!.Constraints, ConstraintList),
    set_of_var.union_list(ConstraintList, AllVars),
    find_all_colours(ConstraintList, AllVars, ColourList),
    Colours = set.list_to_set(ColourList).

    % Iterate the assignment of a new colour until all constraints
    % are satisfied.
    %
:- pred find_all_colours(list(set_of_var(T))::in, set_of_var(T)::in,
    list(set_of_var(T))::out) is det.

find_all_colours(ConstraintList, Vars, ColourList) :-
    (
        ConstraintList = [],
        ColourList = []
    ;
        ConstraintList = [_ | _],
        next_colour(Vars, ConstraintList, RemainingConstraints, Colour),
        set_of_var.difference(Vars, Colour, RestVars),
        find_all_colours(RemainingConstraints, RestVars, ColourList0),
        ColourList = [Colour | ColourList0]
    ).

:- pred next_colour(set_of_var(T)::in, list(set_of_var(T))::in,
    list(set_of_var(T))::out, set_of_var(T)::out) is det.

next_colour(Vars0, ConstraintList, Remainder, SameColour) :-
    % Check if there are any constraints left to be satisfied.
    (
        ConstraintList = [_ | _],
        % Select a variable to assign a colour, ...
        choose_var(Vars0, Var, Vars1),

        % ... and divide the constraints into those that may be the same colour
        % as that var and those that may not.
        divide_constraints(Var, ConstraintList, WereContaining, NotContaining,
            Vars1, RestVars),
        (
            % See if there are sets that can share a colour with the
            % selected var.
            NotContaining = [_ | _],
            ( if set_of_var.is_empty(RestVars) then
                % There were no variables left that could share a colour,
                % so create a singleton set containing this variable.
                SameColour = set_of_var.make_singleton(Var),
                ResidueSets = NotContaining
            else
                % If there is at least one variable that can share a colour
                % with the selected variable, then recursively use the
                % remaining constraints to assign a colour to one of the
                % remaining vars, and assemble the constraint residues.
                next_colour(RestVars, NotContaining, ResidueSets, SameColour0),

                % Add this variable to the variables of the current colour.
                set_of_var.insert(Var, SameColour0, SameColour)
            )
        ;
            NotContaining = [],
            % There were no more constraints which could be satisfied
            % by assigning any variable a colour the same as the current
            % variable, so create a signleton set with the current var,
            % and assign the residue to the empty set.
            SameColour = set_of_var.make_singleton(Var),
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
        SameColour = set_of_var.init
    ).

    % Divide_constraints takes a var and a list of sets of var, and divides
    % the list into two lists: a list of sets containing the given variable
    % and a list of sets not containing that variable. The sets in the list
    % containing the variable have that variable removed. Additionally, a set
    % of variables is threaded through the computation, and any variables that
    % were in sets that also contained the given variables are removed from
    % the threaded set.
    %
:- pred divide_constraints(var(T)::in, list(set_of_var(T))::in,
    list(set_of_var(T))::out, list(set_of_var(T))::out,
    set_of_var(T)::in, set_of_var(T)::out) is det.

divide_constraints(_Var, [], [], [], !Vars).
divide_constraints(Var, [S | Ss], C, NC, !Vars) :-
    divide_constraints(Var, Ss, C0, NC0, !Vars),
    ( if set_of_var.member(S, Var) then
        set_of_var.delete(Var, S, T),
        ( if set_of_var.is_empty(T) then
            C = C0
        else
            C = [T | C0]
        ),
        NC = NC0,
        set_of_var.difference(!.Vars, T, !:Vars)
    else
        C = C0,
        NC = [S | NC0]
    ).

    % Choose_var/3, given a set of variables, chooses one, returns it
    % and the set with that variable removed.
    %
:- pred choose_var(set_of_var(T)::in, var(T)::out, set_of_var(T)::out) is det.

choose_var(Vars0, Var, Vars) :-
    ( if set_of_var.remove_least(VarPrime, Vars0, VarsPrime) then
        Var = VarPrime,
        Vars = VarsPrime
    else
        unexpected($pred, "no vars!")
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.set_of_var.
%---------------------------------------------------------------------------%
