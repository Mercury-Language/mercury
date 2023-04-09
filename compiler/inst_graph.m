%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: inst_graph.m.
% Author: dmo.
%
% This module defines operations on instantiation graphs. The purpose of the
% data structure and of the operations on it are defined in chapter 6 of
% David Overton's PhD thesis.
%
%-----------------------------------------------------------------------------%

:- module hlds.inst_graph.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- type inst_graph == map(prog_var, node).

:- type node
    --->    node(
                map(cons_id, list(prog_var)),
                % If the variable that maps to this node occurs on the
                % left hand side of any var-functor unifications,
                % this map gives, for each functor that occurs in such
                % unifications, the identities of the variables
                % chosen by the transformation to hyperhomogeneous form
                % to represent the arguments of that functor inside
                % the cell variable.

                maybe_parent
                % Specifies whether
            ).

:- type maybe_parent
    --->    top_level
            % The variable in whose node this maybe_parent value occurs
            % doesn't appear on the right hand side of any var-functor
            % unifications.

    ;       parent(prog_var).
            % The variable in whose node this maybe_parent value occurs
            % does appear on the right hand side of a var-functor unification:
            % the argument of parent identifies the variable on the left hand
            % side. The definition of hyperhomogeneous form guarantees that
            % this variable is unique.

    % Initialise an inst_graph. Adds a node for each variable, and
    % initializes each node to have no parents and no children.
    %
:- pred init(list(prog_var)::in, inst_graph::out) is det.

    % set_parent(Parent, Child, Graph0, Graph):
    %
    % Sets Parent to be the parent node of Child. Aborts if Child
    % already has a parent.
    %
:- pred set_parent(prog_var::in, prog_var::in, inst_graph::in, inst_graph::out)
    is det.

    % top_level_node(InstGraph, VarA, VarB):
    %
    % Succeeds iff VarB is the top_level node reachable from VarA in InstGraph.
    %
:- pred top_level_node(inst_graph::in, prog_var::in, prog_var::out) is det.

    % descendant(InstGraph, VarA, VarB):
    %
    % Succeeds iff VarB is a descendant of VarA in InstGraph.
    %
:- pred descendant(inst_graph::in, prog_var::in, prog_var::out) is nondet.

    % reachable(InstGraph, VarA, VarB):
    %
    % Succeeds iff VarB is a descendant of VarA in InstGraph,
    % or if VarB *is* VarA.
    %
:- pred reachable(inst_graph::in, prog_var::in, prog_var::out) is multi.

    % reachable(InstGraph, Vars, VarB):
    %
    % Succeeds iff VarB is a descendant in InstGraph of any VarA in Vars.
    %
:- pred reachable_from_list(inst_graph::in, list(prog_var)::in, prog_var::out)
    is nondet.

    % foldl_reachable(Pred, InstGraph, Var, !Acc):
    %
    % Performs a foldl operation over all variables V for which
    % reachable(InstGraph, Var, V) is true.
    %
:- pred foldl_reachable(pred(prog_var, T, T)::pred(in, in, out) is det,
    inst_graph::in, prog_var::in, T::in, T::out) is det.

    % foldl_reachable_from_list(Pred, InstGraph, Vars, !Acc):
    %
    % Performs a foldl operation over all variables V for which
    % reachable_from_list(InstGraph, Vars, V) is true.
    %
:- pred foldl_reachable_from_list(
    pred(prog_var, T, T)::pred(in, in, out) is det,
    inst_graph::in, list(prog_var)::in, T::in, T::out) is det.

    % A version of foldl_reachable with two accumulators.
    %
:- pred foldl_reachable2(
    pred(prog_var, T, T, U, U)::pred(in, in, out, in, out) is det,
    inst_graph::in, prog_var::in, T::in, T::out, U::in, U::out) is det.

    % A version of foldl_reachable_from_list with two accumulators.
    %
:- pred foldl_reachable_from_list2(
    pred(prog_var, T, T, U, U)::pred(in, in, out, in, out) is det,
    inst_graph::in, list(prog_var)::in, T::in, T::out, U::in, U::out)
    is det.

:- pred same_graph_corresponding_nodes(inst_graph::in,
    prog_var::in, prog_var::in, prog_var::out, prog_var::out) is multi.

:- pred two_graphs_corresponding_nodes(inst_graph::in, inst_graph::in,
    prog_var::in, prog_var::in, prog_var::out, prog_var::out) is multi.

:- pred corresponding_nodes_from_lists(inst_graph::in, inst_graph::in,
    list(prog_var)::in, list(prog_var)::in, prog_var::out, prog_var::out)
    is nondet.

    % Merge two inst_graphs by renaming the variables in the second inst_graph.
    % Also return the variable substitution map.
    %
:- pred merge(inst_graph::in, prog_varset::in, inst_graph::in, prog_varset::in,
    inst_graph::out, prog_varset::out, map(prog_var, prog_var)::out) is det.

%   % Join two inst_graphs together by taking the maximum unrolling
%   % of the type tree of each variable from the two graphs.
%   %
% :- pred join(inst_graph::in, prog_varset::in, inst_graph::in,
%   prog_varset::in, inst_graph::out, prog_varset::out) is det.

    % Print the given inst_graph over the given varset in a format
    % suitable for debugging output.
    %
:- pred dump(inst_graph::in, prog_varset::in, io::di, io::uo) is det.

    % XXX This should probably go in list.m.
    %
:- pred corresponding_members(list(T)::in, list(U)::in, T::out, U::out)
    is nondet.

    % Values of this type are intended to contain all the info related
    % to inst_graphs for a predicate that needs to be stored in the pred_info.
:- type inst_graph_info
    --->    inst_graph_info(
                % Inst graph derived from the mode declarations,
                % if there are any. If there are no mode declarations
                % for the pred, this is the same as the
                % implementation_inst_graph.
                interface_inst_graph        :: inst_graph,

                % Vars that appear in the head of the mode declaration
                % constraint.
                interface_vars              :: list(prog_var),

                % Table of the variables used for interface_inst_graph.
                interface_var_table         :: var_table,

                % Inst graph derived from the body of the predicate.
                implementation_inst_graph   :: inst_graph
            ).

    % Create an empty inst_graph_info.
    %
:- func inst_graph_info_init = inst_graph_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_cons_id.

:- import_module require.
:- import_module set.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

init(Vars, InstGraph) :-
    map.init(InstGraph0),
    list.foldl(init_var, Vars, InstGraph0, InstGraph).

:- pred init_var(prog_var::in, inst_graph::in, inst_graph::out) is det.

init_var(Var, !InstGraph) :-
    map.det_insert(Var, node(map.init, top_level), !InstGraph).

set_parent(Parent, Child, !InstGraph) :-
    map.lookup(!.InstGraph, Child, node(Functors, MaybeParent0)),
    (
        MaybeParent0 = top_level,
        map.det_update(Child, node(Functors, parent(Parent)), !InstGraph)
    ;
        MaybeParent0 = parent(_),
        unexpected($pred, "node already has parent")
    ).

top_level_node(InstGraph, Var, TopLevel) :-
    map.lookup(InstGraph, Var, node(_, MaybeParent)),
    (
        MaybeParent = parent(Parent),
        top_level_node(InstGraph, Parent, TopLevel)
    ;
        MaybeParent = top_level,
        TopLevel = Var
    ).

descendant(InstGraph, Var, Descendant) :-
    set.init(Seen),
    descendant_2(InstGraph, Seen, Var, Descendant).

:- pred descendant_2(inst_graph::in, set(prog_var)::in, prog_var::in,
    prog_var::out) is nondet.

descendant_2(InstGraph, Seen, Var, Descendant) :-
    map.lookup(InstGraph, Var, node(Functors, _)),
    map.member(Functors, _ConsId, Args),
    list.member(Arg, Args),
    (
        Descendant = Arg
    ;
        ( if Arg `set.member` Seen then
            fail
        else
            descendant_2(InstGraph, Seen `set.insert` Arg, Arg, Descendant)
        )
    ).

reachable(_InstGraph, Var, Var).
reachable(InstGraph, Var, Reachable) :-
    descendant(InstGraph, Var, Reachable).

reachable_from_list(InstGraph, Vars, Reachable) :-
    list.member(Var, Vars),
    reachable(InstGraph, Var, Reachable).

foldl_reachable(P, InstGraph, Var, !Acc) :-
    % A possible alternate implementation:
    % aggregate(reachable(InstGraph, Var), P, !Acc).
    foldl_reachable_aux(P, InstGraph, Var, set.init, !Acc).

:- pred foldl_reachable_aux(pred(prog_var, T, T)::pred(in, in, out) is det,
    inst_graph::in, prog_var::in, set(prog_var)::in, T::in, T::out) is det.

foldl_reachable_aux(P, InstGraph, Var, Seen, !Acc) :-
    P(Var, !Acc),
    map.lookup(InstGraph, Var, node(Functors, _)),
    map.foldl((pred(_ConsId::in, Args::in, MAcc0::in, MAcc::out) is det :-
        list.foldl((pred(Arg::in, LAcc0::in, LAcc::out) is det :-
            ( if Arg `set.member` Seen then
                LAcc = LAcc0
            else
                foldl_reachable_aux(P, InstGraph, Arg, Seen `set.insert` Arg,
                    LAcc0, LAcc)
            )
        ), Args, MAcc0, MAcc)
    ), Functors, !Acc).

foldl_reachable_from_list(P, InstGraph, Vars, !Acc) :-
    list.foldl(foldl_reachable(P, InstGraph), Vars, !Acc).

foldl_reachable2(P, InstGraph, Var, !Acc1, !Acc2) :-
    % A possible alternate implementation:
    % aggregate2(reachable(InstGraph, Var), P, !Acc1, !Acc2).
    foldl_reachable_aux2(P, InstGraph, Var, set.init, !Acc1, !Acc2).

:- pred foldl_reachable_aux2(
    pred(prog_var, T, T, U, U)::pred(in, in, out, in, out) is det,
    inst_graph::in, prog_var::in, set(prog_var)::in, T::in, T::out,
    U::in, U::out) is det.

foldl_reachable_aux2(P, InstGraph, Var, Seen, !Acc1, !Acc2) :-
    P(Var, !Acc1, !Acc2),
    map.lookup(InstGraph, Var, node(Functors, _)),
    map.foldl2((pred(_ConsId::in, Args::in, MAcc10::in, MAcc1::out,
            MAcc20::in, MAcc2::out) is det :-
        list.foldl2((pred(Arg::in, LAccA0::in, LAccA::out,
                LAccB0::in, LAccB::out) is det :-
            ( if Arg `set.member` Seen then
                LAccA = LAccA0,
                LAccB = LAccB0
            else
                foldl_reachable_aux2(P, InstGraph, Arg, Seen `set.insert` Arg,
                    LAccA0, LAccA, LAccB0, LAccB)
            )
        ), Args, MAcc10, MAcc1, MAcc20, MAcc2)
    ), Functors, !Acc1, !Acc2).

foldl_reachable_from_list2(P, InstGraph, Vars, !Acc1, !Acc2) :-
    list.foldl2(foldl_reachable2(P, InstGraph), Vars,
        !Acc1, !Acc2).

same_graph_corresponding_nodes(InstGraph, A, B, V, W) :-
    two_graphs_corresponding_nodes(InstGraph, InstGraph, A, B, V, W).

two_graphs_corresponding_nodes(InstGraphA, InstGraphB, A, B, V, W) :-
    corresponding_nodes_2(InstGraphA, InstGraphB,
        set.init, set.init, A, B, V, W).

:- pred corresponding_nodes_2(inst_graph::in, inst_graph::in,
    set(prog_var)::in, set(prog_var)::in, prog_var::in, prog_var::in,
    prog_var::out, prog_var::out) is multi.

corresponding_nodes_2(_, _, _, _, A, B, A, B).
corresponding_nodes_2(InstGraphA, InstGraphB, SeenA0, SeenB0, A, B, V, W) :-
    not (
        A `set.member` SeenA0,
        B `set.member` SeenB0
    ),

    map.lookup(InstGraphA, A, node(FunctorsA, _)),
    map.lookup(InstGraphB, B, node(FunctorsB, _)),

    SeenA = SeenA0 `set.insert` A,
    SeenB = SeenB0 `set.insert` B,

    ( if map.member(FunctorsA, ConsId, ArgsA) then
        ( if map.is_empty(FunctorsB) then
            list.member(V0, ArgsA),
            corresponding_nodes_2(InstGraphA, InstGraphB, SeenA, SeenB,
                V0, B, V, W)
        else
            map.search(FunctorsB, ConsId, ArgsB),
            corresponding_members(ArgsA, ArgsB, V0, W0),
            corresponding_nodes_2(InstGraphA, InstGraphB, SeenA, SeenB,
                V0, W0, V, W)
        )
    else
        map.member(FunctorsB, _ConsId, ArgsB),
        list.member(W0, ArgsB),
        corresponding_nodes_2(InstGraphA, InstGraphB, SeenA, SeenB,
            A, W0, V, W)
    ).

corresponding_nodes_from_lists(InstGraphA, InstGraphB, VarsA, VarsB, V, W) :-
    corresponding_members(VarsA, VarsB, A, B),
    two_graphs_corresponding_nodes(InstGraphA, InstGraphB, A, B, V, W).

corresponding_members([A | _], [B | _], A, B).
corresponding_members([_ | As], [_ | Bs], A, B) :-
    corresponding_members(As, Bs, A, B).

merge(InstGraph0, VarSet0, NewInstGraph, NewVarSet, InstGraph, VarSet,
        Renaming) :-
    varset.merge_renaming_without_names(VarSet0, NewVarSet, VarSet, Renaming),
    map.foldl((pred(Var0::in, Node0::in, IG0::in, IG::out) is det :-
        Node0 = node(Functors0, MaybeParent),
        map.map_values_only(
            (pred(Args0::in, Args::out) is det :-
                map.apply_to_list(Args0, Renaming, Args)),
            Functors0, Functors),
        Node = node(Functors, MaybeParent),
        map.lookup(Renaming, Var0, Var),
        map.det_insert(Var, Node, IG0, IG)
    ), NewInstGraph, InstGraph0, InstGraph).

%-----------------------------------------------------------------------------%

% join(InstGraphA, VarSetA, InstGraphB, VarSetB,
%       InstGraph, VarSet) :-
%   solutions((pred(V::out) is nondet :-
%           map.member(InstGraphB, V, node(_, top_level))
%       ), VarsB),
%   list.foldl2(join_nodes(InstGraphB, VarSetB), VarsB, InstGraphA,
%       InstGraph, VarSetA, VarSet).
%
% :- pred join_nodes(inst_graph, prog_varset, prog_var, inst_graph, inst_graph,
%       prog_varset, prog_varset).
% :- mode join_nodes(in, in, in, in, out, in, out) is det.
%
% join_nodes(_, _, _, _, _, _, _) :- error("join_nodes: NYI").

%-----------------------------------------------------------------------------%

dump(InstGraph, VarSet, !IO) :-
    map.foldl(dump_node(VarSet), InstGraph, !IO).

:- pred dump_node(prog_varset::in, prog_var::in, node::in,
    io::di, io::uo) is det.

dump_node(VarSet, Var, Node, !IO) :-
    Node = node(Functors, MaybeParent),
    io.write_string("%% ", !IO),
    term_io.write_variable(Var, VarSet, !IO),
    io.write_string(": ", !IO),
    (
        MaybeParent = parent(Parent),
        term_io.write_variable(Parent, VarSet, !IO)
    ;
        MaybeParent = top_level
    ),
    io.nl(!IO),
    map.foldl(dump_functor(VarSet), Functors, !IO).

:- pred dump_functor(prog_varset::in, cons_id::in, list(prog_var)::in,
    io::di, io::uo) is det.

dump_functor(VarSet, ConsId, Args, !IO) :-
    io.write_string("%%\t", !IO),
    io.write_string(cons_id_and_arity_to_string(ConsId), !IO),
    (
        Args = [_ | _],
        io.write_char('(', !IO),
        io.write_list(Args, ", ", dump_var(VarSet), !IO),
        io.write_char(')', !IO)
    ;
        Args = []
    ),
    io.nl(!IO).

:- pred dump_var(prog_varset::in, prog_var::in, io::di, io::uo) is det.

dump_var(VarSet, Var, !IO) :-
    term_io.write_variable(Var, VarSet, !IO).

%-----------------------------------------------------------------------------%

inst_graph_info_init = Info :-
    init_var_table(VarTable),
    map.init(InstGraph),
    Info = inst_graph_info(InstGraph, [], VarTable, InstGraph).

%-----------------------------------------------------------------------------%
:- end_module hlds.inst_graph.
%-----------------------------------------------------------------------------%
