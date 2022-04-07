%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.points_to_info.m.
% Main author: Quan Phan.
%
% This module defines the "rpta_info" and "rpta_info_table" types.
% rpta_info_table maps a procedure to its corresponding rpt information
% (i.e., the rpt graph and the alpha mappings (at the call sites in it)).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.points_to_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module map.

%-----------------------------------------------------------------------------%

:- type rpta_info_table == map(pred_proc_id, rpta_info).

:- func rpta_info_table_init = rpta_info_table.

:- func rpta_info_table_search_rpta_info(pred_proc_id, rpta_info_table)
    = rpta_info is semidet.

:- pred rpta_info_table_set_rpta_info(pred_proc_id::in, rpta_info::in,
    rpta_info_table::in, rpta_info_table::out) is det.

:- type rpta_info
        ---> rpta_info(rpt_graph, rpt_alpha_mapping).

    % The rpta_info will be for a specific procedure, at the beginning
    % the alpha mapping is empty and the rpt graph contains all the nodes
    % corresponding to all the variables appear in the procedure.
    %
:- func rpta_info_init(proc_info) = rpta_info.

:- pred rpta_info_equal(rpta_info::in, rpta_info::in) is semidet.

%-----------------------------------------------------------------------------%

    % This type represents the alpha mapping of all call sites in a procedure.
    % For documentation of alpha mappings, see Chapter 4 in Quan's thesis;
    % basically, they map the nodes in the region points-to graph of the callee
    % to the corresponding nodes in the caller.
    %
:- type rpt_alpha_mapping == map(program_point, rpt_call_alpha_mapping).

    % This type represents the alpha mapping at one call site.
    %
:- type rpt_call_alpha_mapping == map(rptg_node, rptg_node).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

rpta_info_table_init = map.init.

rpta_info_table_search_rpta_info(PredProcId, Table) = RptaInfo :-
    Table ^ elem(PredProcId) = RptaInfo.

rpta_info_table_set_rpta_info(PredProcId, RptaInfo, !Table) :-
    !Table ^ elem(PredProcId) := RptaInfo.

rpta_info_init(ProcInfo) = RptaInfo :-
    proc_info_get_varset_vartypes(ProcInfo, _VarSet, VarTypes),
    vartypes_vars(VarTypes, Vars),
    list.foldl2(add_node_from_var(VarTypes), Vars, 1, _Reg,
        rpt_graph_init, Graph),
    map.init(AlphaMapping),
    RptaInfo = rpta_info(Graph, AlphaMapping).

:- pred add_node_from_var(vartypes::in, prog_var::in, int::in,
    int::out, rpt_graph::in, rpt_graph::out) is det.

add_node_from_var(VarTypes, Var, Reg0, Reg, !Graph) :-
    lookup_var_type(VarTypes, Var, NodeType),
    set.init(Varset0),
    set.insert(Var, Varset0, Varset),
    Reg = Reg0 + 1,
    string.append("R", string.int_to_string(Reg0), RegName),
    NodeInfo = rptg_node_content(Varset, RegName, set.init, NodeType, bool.no),
    rptg_add_node(NodeInfo, _Node, !Graph).

rpta_info_equal(RptaInfoA, RptaInfoB):-
    RptaInfoA = rpta_info(GraphA, AlphaA),
    RptaInfoB = rpta_info(GraphB, AlphaB),
    rptg_equal(GraphA, GraphB),
    rpt_alpha_mapping_equal(AlphaA, AlphaB).

%-----------------------------------------------------------------------------%
%
% Alpha mapping at call sites
%

:- pred rpt_alpha_mapping_equal(rpt_alpha_mapping::in,
    rpt_alpha_mapping::in) is semidet.

rpt_alpha_mapping_equal(AlphaMappingA, AlphaMappingB) :-
    map.count(AlphaMappingA, CA),
    map.count(AlphaMappingB, CB),
    CA = CB,
    map.keys(AlphaMappingA, CallSitesA),
    rpt_alpha_mapping_equal_2(CallSitesA, AlphaMappingA, AlphaMappingB).

:- pred rpt_alpha_mapping_equal_2(list(program_point)::in,
    rpt_alpha_mapping::in, rpt_alpha_mapping::in) is semidet.

rpt_alpha_mapping_equal_2([], _, _).
rpt_alpha_mapping_equal_2([CallSiteA | CallSiteAs],
        AlphaMappingA, AlphaMappingB) :-
    map.search(AlphaMappingB, CallSiteA, AlphaMappingAtCallSiteB),
    map.lookup(AlphaMappingA, CallSiteA, AlphaMappingAtCallSiteA),
    rpt_alpha_mapping_at_call_site_equal(
        AlphaMappingAtCallSiteA,AlphaMappingAtCallSiteB),
    rpt_alpha_mapping_equal_2(CallSiteAs, AlphaMappingA, AlphaMappingB).

:- pred rpt_alpha_mapping_at_call_site_equal(rpt_call_alpha_mapping::in,
    rpt_call_alpha_mapping::in) is semidet.

rpt_alpha_mapping_at_call_site_equal(AMAtCallSiteA, AMAtCallSiteB) :-
    map.count(AMAtCallSiteA, CA),
    map.count(AMAtCallSiteB, CB),
    CA = CB,
    map.keys(AMAtCallSiteA, NodesA),
    rpt_alpha_mapping_at_call_site_equal_2(NodesA, AMAtCallSiteA,
        AMAtCallSiteB).

:- pred rpt_alpha_mapping_at_call_site_equal_2(list(rptg_node)::in,
    rpt_call_alpha_mapping::in, rpt_call_alpha_mapping::in) is semidet.

rpt_alpha_mapping_at_call_site_equal_2([], _, _).
rpt_alpha_mapping_at_call_site_equal_2([N | Ns], AMAtCallSiteA,
        AMAtCallSiteB) :-
    map.search(AMAtCallSiteB, N, NPrimeB),
    map.lookup(AMAtCallSiteA, N, NPrimeA),
    NPrimeA = NPrimeB,
    rpt_alpha_mapping_at_call_site_equal_2(Ns, AMAtCallSiteA, AMAtCallSiteB).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.points_to_info.
%-----------------------------------------------------------------------------%
