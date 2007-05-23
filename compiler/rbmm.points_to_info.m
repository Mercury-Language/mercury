%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
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

    % type rpta_info and operations
    %
:- type rpta_info 
        ---> rpta_info(rpt_graph, rpt_alpha_mapping).	

    % The rpta_info will be for a specific procedure, at the beginning 
    % the alpha mapping is empty and the rpt graph contains all the nodes 
    % corresponding to all the variables appear in the procedure.
    %
:- func rpta_info_init(proc_info) = rpta_info.

:- pred rpta_info_equal(rpta_info::in, rpta_info::in) is semidet.

%-----------------------------------------------------------------------------%

:- type rpt_alpha_mapping == map(program_point, map(rptg_node, rptg_node)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_module. 
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module set. 
:- import_module std_util.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

rpta_info_table_init = map.init. 
rpta_info_table_search_rpta_info(PredProcId, Table) = RptaInfo :- 
    Table^elem(PredProcId) = RptaInfo.
rpta_info_table_set_rpta_info(PredProcId, RptaInfo, Table0, Table) :- 
    Table = Table0^elem(PredProcId) := RptaInfo.

    % The rpta_info will be for a specific procedure, so at the beginning 
    % the alpha mapping is empty and the rpt graph contains all the nodes 
    % corresponding to all the variables appear in the procedure.
    %
rpta_info_init(ProcInfo) = RptaInfo :-
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.keys(VarTypes, Vars),
    list.foldl2(add_node_from_var(VarTypes), Vars, 1, _Reg,
        rpt_graph_init, Graph),
    map.init(AlphaMapping),
    RptaInfo = rpta_info(Graph, AlphaMapping).

:- pred add_node_from_var(map(prog_var, mer_type)::in, prog_var::in, int::in,
    int::out, rpt_graph::in, rpt_graph::out) is det.

add_node_from_var(VarTypes, Var, Reg0, Reg, !Graph) :-
    map.lookup(VarTypes, Var, NodeType), 
    set.init(Varset0),
    set.insert(Varset0, Var, Varset),
    Reg = Reg0 + 1,
    string.append("R", string.int_to_string(Reg0), RegName),
    NodeInfo = rptg_node_content(Varset, RegName, set.init, NodeType),
    rptg_set_node(NodeInfo, _Node, !Graph).

rpta_info_equal(RptaInfo1, RptaInfo2):-
    RptaInfo1 = rpta_info(Graph1, Alpha1),
    RptaInfo2 = rpta_info(Graph2, Alpha2), 
    rptg_equal(Graph1, Graph2),
    rpt_alpha_mapping_equal(Alpha1, Alpha2).

%-----------------------------------------------------------------------------%
%
% Alpha mapping at call sites
%

:- pred rpt_alpha_mapping_equal(rpt_alpha_mapping::in, 
    rpt_alpha_mapping::in) is semidet.
            
rpt_alpha_mapping_equal(AlphaMapping1, AlphaMapping2) :-
    map.count(AlphaMapping1, C1),
    map.count(AlphaMapping2, C2),
    C1 = C2,
    map.keys(AlphaMapping1, CallSites1),
    rpt_alpha_mapping_equal_2(CallSites1, AlphaMapping1, AlphaMapping2).

:- pred rpt_alpha_mapping_equal_2(list(program_point)::in,
    rpt_alpha_mapping::in, rpt_alpha_mapping::in) is semidet.

rpt_alpha_mapping_equal_2([], _, _).
rpt_alpha_mapping_equal_2([CallSite1 | CallSite1s],
        AlphaMapping1, AlphaMapping2) :-
    map.search(AlphaMapping2, CallSite1, AlphaMappingAtCallSite2),
    map.lookup(AlphaMapping1, CallSite1, AlphaMappingAtCallSite1),
    rpt_alpha_mapping_at_call_site_equal(
        AlphaMappingAtCallSite1,AlphaMappingAtCallSite2),
    rpt_alpha_mapping_equal_2(CallSite1s, AlphaMapping1, AlphaMapping2).

:- pred rpt_alpha_mapping_at_call_site_equal(map(rptg_node, rptg_node)::in,
    map(rptg_node, rptg_node)::in) is semidet.

rpt_alpha_mapping_at_call_site_equal(AMAtCallSite1, AMAtCallSite2) :-
    map.count(AMAtCallSite1, C1),
    map.count(AMAtCallSite2, C2),
    C1 = C2,
    map.keys(AMAtCallSite1, Nodes1),
    rpt_alpha_mapping_at_call_site_equal_2(Nodes1, AMAtCallSite1,
        AMAtCallSite2).

:- pred rpt_alpha_mapping_at_call_site_equal_2(list(rptg_node)::in,
    map(rptg_node, rptg_node)::in, map(rptg_node, rptg_node)::in) is semidet.
    
rpt_alpha_mapping_at_call_site_equal_2([], _, _).
rpt_alpha_mapping_at_call_site_equal_2([N | Ns], AMAtCallSite1,
        AMAtCallSite2) :-
    map.search(AMAtCallSite2, N, NPrime2),
    map.lookup(AMAtCallSite1, N, NPrime1),
    NPrime1 = NPrime2,
    rpt_alpha_mapping_at_call_site_equal_2(Ns, AMAtCallSite1, AMAtCallSite2).

%-----------------------------------------------------------------------------%
:- end_module rbmm.points_to_info.
%-----------------------------------------------------------------------------%
