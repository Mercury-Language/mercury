%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% prof_info.m

% Main author: petdr.
%
% Declare the main data structures for mercury__profile and their access
% predicates, the actual types are exported as well.  This is because some
% predicates need to access entire data structure.
% XXX Should maybe changed at a later date.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prof_info.

:- interface.

:- import_module float, int, list, map, string.

%-----------------------------------------------------------------------------%


	% prof: Data structure which contains ALL the relevant info for use
	%	in generating the output.
:- type prof --->
		prof(
			int,			% Hertz of the system clock
			int,			% No. of clock ticks between 
						% each prof signal.
			int,			% Total counts of the profile
						% run
			addrdecl,		% Map between label name and
						% label addr used to find key
						% to look up prof_node_map.
			prof_node_map		% Map between label addresses
						% and all the relevant data 
						% about that predicate
		).

	% addrdecl = map(label_name, label_address)
:- type addrdecl	==	map(string, int).

	% prof_node_map = map(label_address, prof_node)
:- type prof_node_map	==	map(int, prof_node).

	% prof_node: Contains all the info used for output, for a single pred.
:- type prof_node 	---> 
		prof_node(
			string, 		% current predicate (label)
			int,			% index number
			int, 			% self counts
			float, 			% propagated counts
			list(pred_info), 	% Parent pred and the number
						% of times it calls this
						% predicate
			list(pred_info),	% Child pred and the number of
						% times they are called from
						% this predicate.
			int,			% total count of times this 
						% predicate called.
			int,			% Number of self recursive 
						% calls of this routine
			list(string)		% Alternative names for this
						% predicate eg. Labels with
						% different names but the same
						% address.
		).

:- type pred_info --->
		pred_info(
			string,			% predicate (label)     
			int			% count     (to or from)
		).

:- type junk		==	int.



%-----------------------------------------------------------------------------%


:- pred get_prof_node(string, addrdecl, prof_node_map, prof_node).
:- mode get_prof_node(in, in, in, out) is det.


	% *** Access prof predicates *** %

:- pred prof_get_addrdeclmap(prof, addrdecl).
:- mode prof_get_addrdeclmap(in, out) is det.

:- pred prof_get_profnodemap(prof, prof_node_map).
:- mode prof_get_profnodemap(in, out) is det.


	% *** Update prof predicates *** %

:- pred prof_set_profnodemap(prof_node_map, prof, prof).
:- mode prof_set_profnodemap(in, in, out) is det.


	% *** Initialise predicates *** %

:- pred prof_node_init(string, prof_node).
:- mode prof_node_init(in, out) is det.


	% *** Access Predicate for prof_node *** %

:- pred prof_node_get_pred_name(prof_node, string).
:- mode prof_node_get_pred_name(in, out) is det.

:- pred prof_node_get_index_number(prof_node, int).
:- mode prof_node_get_index_number(in, out) is det.

:- pred prof_node_get_initial_counts(prof_node, int).
:- mode prof_node_get_initial_counts(in, out) is det.

:- pred prof_node_get_propagated_counts(prof_node, float).
:- mode prof_node_get_propagated_counts(in, out) is det.

:- pred prof_node_get_parent_list(prof_node, list(pred_info)).
:- mode prof_node_get_parent_list(in, out) is det.

:- pred prof_node_get_child_list(prof_node, list(pred_info)).
:- mode prof_node_get_child_list(in, out) is det.

:- pred prof_node_get_total_calls(prof_node, int).
:- mode prof_node_get_total_calls(in, out) is det.

:- pred prof_node_get_self_calls(prof_node, int).
:- mode prof_node_get_self_calls(in, out) is det.


	% *** Update prof_node predicates *** %

:- pred prof_node_set_index_num(int, prof_node, prof_node).
:- mode prof_node_set_index_num(in, in, out) is det.

:- pred prof_node_set_initial_counts(int, prof_node, prof_node).
:- mode prof_node_set_initial_counts(in, in, out) is det.

:- pred prof_node_set_propagated_counts(float, prof_node, prof_node).
:- mode prof_node_set_propagated_counts(in, in, out) is det.

:- pred prof_node_concat_to_parent(pred_info, prof_node, prof_node).
:- mode prof_node_concat_to_parent(in, in, out) is det.

:- pred prof_node_concat_to_child(pred_info, prof_node, prof_node).
:- mode prof_node_concat_to_child(in, in, out) is det.

:- pred prof_node_set_total_calls(int, prof_node, prof_node).
:- mode prof_node_set_total_calls(in, in, out) is det.

:- pred prof_node_set_self_calls(int, prof_node, prof_node).
:- mode prof_node_set_self_calls(in, in, out) is det.

:- pred prof_node_concat_to_name_list(string, prof_node, prof_node).
:- mode prof_node_concat_to_name_list(in, in, out) is det.


	% *** Access predicates for pred_info *** %

:- pred pred_info_get_pred_name(pred_info, string).
:- mode pred_info_get_pred_name(in, out) is det.

:- pred pred_info_get_counts(pred_info, int).
:- mode pred_info_get_counts(in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.


% get_prof_node:
%  	Gets the prof_node given a label name.
%
get_prof_node(Pred, AddrMap, ProfNodeMap, ProfNode) :-
	map__lookup(AddrMap, Pred, Key),
	map__lookup(ProfNodeMap, Key, ProfNode).


% *** Access prof predicates *** %

prof_get_addrdeclmap(prof(_, _, _, AddrDeclMap, _), AddrDeclMap).

prof_get_profnodemap(prof(_, _, _, _, ProfNodeMap), ProfNodeMap).


% *** Update prof predicates *** %

prof_set_profnodemap(ProfNodeMap, prof(A, B, C, D, _), 
						prof(A, B, C, D, ProfNodeMap)).


% *** Initialise predicates *** %

prof_node_init(PredName, prof_node(PredName, 0, 0, 0.0, [], [], 0, 0, [])).


% *** Access prof_node predicates *** %

prof_node_get_pred_name(prof_node(Name, _, _, _, _, _, _, _, _), Name).

prof_node_get_index_number(prof_node(_, Index, _, _, _, _, _, _, _), Index).

prof_node_get_initial_counts(prof_node(_, _, Count, _, _, _, _, _, _), Count).

prof_node_get_propagated_counts(prof_node(_, _, _, Count, _, _, _, _, _), Count).

prof_node_get_parent_list(prof_node(_, _, _, _, PList, _, _, _, _), PList).

prof_node_get_child_list(prof_node(_, _, _, _, _, Clist, _, _, _), Clist).

prof_node_get_total_calls(prof_node(_, _, _, _, _, _, Calls, _, _), Calls).

prof_node_get_self_calls(prof_node(_, _, _, _, _, _, _, Calls, _), Calls).

% *** Update prof_node predicates *** %

prof_node_set_index_num(Index, prof_node(A, _, C, D, E, F, G, H, I), 
				prof_node(A, Index, C, D, E, F, G, H, I)).

prof_node_set_initial_counts(Count, prof_node(A, B, _, D, E, F, G, H, I), 
				prof_node(A, B, Count, D, E, F, G, H, I)).

prof_node_set_propagated_counts(Count, prof_node(A, B, C, _, E, F, G, H, I),
				 prof_node(A, B, C, Count, E, F, G, H, I)).

prof_node_concat_to_parent(PredInfo, prof_node(A, B, C, D, PList, F, G, H, I), 
			prof_node(A, B, C, D, [PredInfo | PList], F, G, H, I)).

prof_node_concat_to_child(PredInfo, prof_node(A, B, C, D, E, CList, G, H, I), 
			prof_node(A, B, C, D, E, [PredInfo | CList], G, H, I)).

prof_node_set_total_calls(Calls, prof_node(A, B, C, D, E, F, _, H, I),
				prof_node(A, B, C, D, E, F, Calls, H, I)).

prof_node_set_self_calls(Calls, prof_node(A, B, C, D, E, F, G, _, I),
				prof_node(A, B, C, D, E, F, G, Calls, I)).

prof_node_concat_to_name_list(Name, prof_node(A, B, C, D, E, F, G, H, NL), 
			prof_node(A, B, C, D, E, F, G, H, [Name | NL])).


%-----------------------------------------------------------------------------%

% *** Access predicates for pred_info *** %

pred_info_get_pred_name(pred_info(Name, _), Name).

pred_info_get_counts(pred_info(_, Count), Count).

%-----------------------------------------------------------------------------%
