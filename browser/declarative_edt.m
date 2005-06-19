%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_edt.m
% Authors: Ian MacLarty, Mark Brown
%
% This module defines Evaluation Dependency Trees (EDTs) which represent the
% dependencies between calls made during the execution of a buggy program.
% Search spaces are also defined as a layer on top of EDTs.  
%
% The search space records extra information like which nodes have been
% eliminated from the bug search, which nodes have been skipped or are
% trusted, the weight of each node, and in future might also store information
% like the probability of each node being buggy, based on some heursitic(s).
%
% The search space provides a consistent view of the debug tree - combining
% seperately generated EDT subtrees into one tree.  Each node in the search
% space corresponds to a node either explicitly represented in a generated EDT
% subtree or the root of an implicitly represented EDT subtree.  We maintain
% the invarient that search space nodes always correspond with explicit nodes
% where an explicit version of the EDT subtree containing that node exists, and
% only correspond to implicit roots when an explicit version of the EDT subtree
% rooted at the node has not yet been generated.
%
% Every node in the EDT may not have a corresponding node in the search space:
% nodes are only added to the search space when they are needed by a
% particular search algorithm.
%
% By convention nodes in the search space are referred to as `suspects', while
% nodes in the EDT are referred to as `EDT nodes', or just `nodes'.  
% 
% Also we use the term "root" to refer to the root of the smallest subtree in
% the search space that must contain a bug based on the answers received so
% far and the term "topmost" for the suspect in the search space with the
% lowest depth.
%
% Each suspect in the search space can be assigned a weight to be used for
% divide and query search.  Any heuristic can be used, as long as the combined
% weight of all the children of a suspect does not exceed the suspect's own
% weight.  Sometimes the weight may depend on the weights of unmaterialized
% portions of the EDT resulting in the situation where the combined weight of
% the children of a suspect exceeds the parent's weight.  If this happens then
% an "excess weight" may be specified along with the normal weight which will
% be added to all the ancestor's of the overweight suspect.  For example if the
% number of events in descendent suspects is being used as a weight, then for a
% FAIL node some events may be repeated in siblings of the FAIL node.  In this
% case the duplicate events might not have been included in the ancestor's
% weights, so should be added.
%
% When debugging, a search space consistency check can be turned on by
% compiling with the C macro MR_DD_CHECK_SEARCH_SPACE defined (i.e. by
% putting "EXTRA_CFLAGS=-DMR_DD_CHECK_SEARCH_SPACE" in Mmake.browser.params).
%

:- module mdb.declarative_edt.

:- interface.

:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_oracle.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.

:- import_module bool, list, std_util.

	% This typeclass defines how EDTs may be accessed by this module.
	% An EDT is a tree of nodes, each of which contains a question
	% about the truth of an assertion.  The children of a node may
	% not be immediately accessible if the sub-tree beneath that
	% node is represented implicitly.  In this case, the analyser
	% must request that it be made explicit before continuing.
	%
	% The first argument is intuitively a "store", which maps
	% references to the things they reference.  The second argument
	% is the type of trees themselves.  By convention, we use the
	% names S and T for type variables which are constrained by
	% mercury_edt.
	%
	% By convention, we also use the names S and T in type declarations
	% where it is *intended* that the type variables be constrained by
	% mercury_edt.
	%
	% (Compare with the similar conventions for annotated_trace/2.)
	%
:- typeclass mercury_edt(S, T) where [
		
		% Returns the question corresponding to the given node.
		%
	pred edt_question(S::in, T::in, decl_question(T)::out) is det,
	
		% If this node is an E-bug, then return the bug.
		% An E-bug is an erroneous node whos children are all correct.
		%
	pred edt_get_e_bug(S::in, T::in, decl_e_bug::out) is det,

		% If this node is an I-bug then return the bug.
		% An I-bug is an erroneous node whos children are all correct
		% or inadmissible, with at least one child being inadmissible.
		%
	pred edt_get_i_bug(S::in, T::in, T::in, decl_i_bug::out) is det,

		% Gives the list of children of the given tree node.  If the
		% tree is represented implicitly, then the procedure fails.
		%
	pred edt_children(S::in, T::in, list(T)::out) is semidet,

		% Return a parent of an EDT node.  Using the annotated trace to
		% generate the EDT there may be more than one parent of a given
		% node (see the comment above trace_last_parent/3 in
		% declarative_tree.m).  This member is required to
		% deterministically pick one if this is the case.  Fails if the
		% node is the root of the initial explicit portion of the EDT,
		% or the root of a portion of the EDT generated as an explicit
		% supertree.
		%
	pred edt_parent(S::in, T::in, T::out) is semidet,

		% Given a subterm of a tree, find the mode of that subterm
		% and the origin of it amongst a parent, siblings or
		% children.
		%
	pred edt_dependency(S::in, T::in, arg_pos::in, term_path::in, 
		subterm_mode::out, subterm_origin(T)::out) is det,

		% Just find the mode of the subterm.
		%
	pred edt_subterm_mode(S::in, T::in, arg_pos::in, term_path::in,
		subterm_mode::out) is det,
	
		% Succeeds if the Node is the root of an implicit subtree.
		% Fails otherwise.
		%
	pred edt_is_implicit_root(S::in, T::in) is semidet,

		% True if the two nodes are the same even if one may
		% be represented implicitly and the other explicitly.
		%
	pred edt_same_nodes(S::in, T::in, T::in) is semidet, 

		% True if it is not possible to materialize any nodes 
		% above the given node.
		%
	pred edt_topmost_node(S::in, T::in) is semidet,
 
 		% edt_weight(Store, Node, Weight, ExcessWeight).
		% Find the weight and excess weight for a node.  See the 
		% comment at the top of this module for the meaning of 
		% the weight of a node.
		%
 	pred edt_weight(S::in, T::in, int::out, int::out) is det,

		% Return the filename and line number of the predicate
		% associated with the given node.  Also return the parent
		% context if available.  Sometimes the main context may
		% not be available (for example exception nodes).  In this case
		% fail.
		%
	pred edt_context(S::in, T::in, pair(string, int)::out, 
		maybe(pair(string, int))::out) is semidet,

		% Return the proc label for the given node.
		%
	func edt_proc_label(S, T) = proc_label,

		% Convert an arg_pos to a user arg number using the
		% atom of the given node.
		%
	func edt_arg_pos_to_user_arg_num(S, T, arg_pos) = int
].

:- type subterm_mode
	--->	subterm_in
	;	subterm_out.

:- type subterm_origin(T)

			% Subterm came from an output of a child or sibling
			% call. The first argument records the child or sibling
			% edt node. The second and third arguments state which
			% part of which argument is the origin.
	--->	output(T, arg_pos, term_path)

			% Subterm came from an input of a parent. The
			% arguments identify which part of which argument of
			% the clause head is the origin.
	;	input(arg_pos, term_path)

			% Subterm was constructed in the body.  We record
			% the filename, line number and type of the primitive
			% operation that constructed it.
	;	primitive_op(string, int, primitive_op_type)

			% The origin could not be found due to missing
			% information.
	;	not_found
			
			% An explicit subtree is required.
	;	require_explicit_subtree.

	% The type of primitive operation that bound a subterm that was being
	% tracked.
	%
:- type primitive_op_type
	--->	foreign_proc
	;	builtin_call
	;	untraced_call
	;	unification.

:- func primitive_op_type_to_string(primitive_op_type) = string.

	% This type defines a search space in which the declarative debugger
	% can look for bugs.  The search space keeps track of which nodes in
	% the EDT could contain a bug as well as skipped or ignored nodes.
	% Each suspect in the search space has an identifier (suspect_id)
	% that's independent of the EDT node id and independent of whether the
	% EDT node is represented implicitly or explicitly.
	%
	% Information about each node that is relevant to the bug search is
	% stored in the search space.  For example information about the status
	% of the node like whether it was skipped, ignored or marked erroneous,
	% correct or inadmissible and the depth of each node in the EDT is
	% stored here.  In future information like the probability that a node
	% contains a bug could also be stored here.
	%
:- type search_space(T).

	% suspect_id is used to lookup suspects in the search space.  Each
	% suspect in the search space will have a unique suspect_id.  When an
	% explicit subtree is generated, a new EDT node is generated for the
	% root of the explicit subtree, replacing the EDT node that represented
	% the subtree implicitly.  However the suspect_id will remain
	% unchanged.  Any search algorithms that needs to keep track of EDT
	% nodes can use suspect_ids for this purpose and not have to worry
	% about updating these when an explicit subtree is generated.
	% 
:- type suspect_id.

	% Returns a search space with no suspects.
	%
:- func empty_search_space = search_space(T).

	% Creates a new search space containing just the one EDT node with 
	% an initial status of unknown.
	%
:- pred initialise_search_space(S::in, T::in, search_space(T)::out) 
	is det <= mercury_edt(S, T).

	% The root of the search space is the root of the subtree of the EDT
	% that we think contains a bug, based on information received so far.
	% Normally the root will be marked erroneous, but it could also be
	% marked unknown, skipped or ignored (for example when the search has
	% just started and the oracle hasn't asserted any suspects are
	% erroneous or when a bug search is being revised.  This pred returns
	% the root, or fails if the search space is empty.
	%
:- pred root(search_space(T)::in, suspect_id::out) is semidet.

	% Return the topmost suspect in the search space and throw an 
	% exception if the search space is empty.
	%
:- pred topmost_det(search_space(T)::in, suspect_id::out) is det.

	% non_ignored_descendents(Store, Oracle, SuspectIds, 
	% 	!SearchSpace, Descendents).
	% Descendents is the non-ignored children of the suspects in 
	% SuspectIds appended together.  If a child is ignored then its
	% non-ignored children are added to the list.  This is done 
	% recursively.  Fails if an explicit subtree is required to find
	% the children of an ignored suspect.
	%
:- pred non_ignored_descendents(S::in, oracle_state::in,
	list(suspect_id)::in, search_space(T)::in, search_space(T)::out,
	list(suspect_id)::out) is semidet <= mercury_edt(S, T).

	% children(Store, Oracle, SuspectId, !SearchSpace, 
	% 	Children).
	% Children is the list of children of SuspectId in the SearchSpace.  If
	% the children were not in the search space then they are added.  Fails
	% if SuspectId is the root of an implicit subtree.  
	% 
:- pred children(S::in, oracle_state::in, suspect_id::in, 
	search_space(T)::in, search_space(T)::out, list(suspect_id)::out) 
	is semidet <= mercury_edt(S, T).

	% parent(SearchSpace, SuspectId, ParentId).
	% Succeeds if ParentId is the Id of the parent of SuspectId in 
	% SearchSpace and fails if SuspectId has no parent in SearchSpace.
	%
:- pred parent(search_space(T)::in, suspect_id::in, suspect_id::out) 
	is semidet.

	% Marks the suspect correct and alls its decendents as pruned.
	%
:- pred assert_suspect_is_correct(suspect_id::in, 
	search_space(T)::in, search_space(T)::out) is det.

	% Marks the supect erroneous and marks the complement of the subtree
	% rooted at the erroneous suspect as in_erroneous_subtree_complement.
	%
:- pred assert_suspect_is_erroneous(suspect_id::in, search_space(T)::in,
	search_space(T)::out) is det.

	% Marks the suspect inadmissible and alls its decendents as pruned.
	%
:- pred assert_suspect_is_inadmissible(suspect_id::in, 
	search_space(T)::in, search_space(T)::out) is det.

	% Marks the suspect ignored.
	%
:- pred ignore_suspect(S::in, suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det <= mercury_edt(S, T).

	% Marks the suspect as skipped.
	%
:- pred skip_suspect(suspect_id::in, search_space(T)::in, search_space(T)::out)
	is det.
	
	% find_subterm_origin(Store, Oracle, SuspectId, ArgPos, 
	%	TermPath, !SearchSpace, Response).  
	% Finds the origin of the subterm given by SuspectId, ArgPos and
	% TermPath in its immediate neighbours.  If the children of a suspect
	% are required then they'll be added to the search space, unless an
	% explicit subtree is required in which case the appropriate response
	% is returned (see definition of find_origin_response type below).
	%
:- pred find_subterm_origin(S::in, oracle_state::in, 
	suspect_id::in, arg_pos::in, term_path::in, search_space(T)::in,
	search_space(T)::out, find_origin_response::out) 
	is det <= mercury_edt(S, T).

:- type find_origin_response
			% The origin couldn't be found because of insufficient
			% tracing information.
	--->	not_found
	
			% The subterm originated from the suspect referenced by 
			% argument 1.  The second and third arguments give the
			% position of the subterm in the origin node, while the
			% fourth argument gives the mode of the subterm.
	;	origin(suspect_id, arg_pos, term_path, subterm_mode)
	
			% The subterm was bound by a primitive operation inside
			% the suspect.  The other arguments are the filename,
			% line number and type of the primitive operation 
			% that bound the subterm.
	;	primitive_op(
			suspect_id,		% The node in which the subterm 
						% was bound.
			string, 		% File name of primitive op.
			int, 			% Line number of primitive op.
			primitive_op_type,	% Type of primitive operation.
			bool			% Whether the subterm appears
						% as an output of the
						% binding node.
			)
			
			% The suspect is the root of an implicit subtree and
			% the origin lies in one of its children.
	;	require_explicit_subtree
	
			% The suspect is the root of the topmost explicit 
			% EDT and the origin lies in an ancestor.  A new
			% supertree needs to be generated.
	;	require_explicit_supertree.

	% Returns the depth of the suspect in the EDT.
	%
:- func suspect_depth(search_space(T), suspect_id) = int.

	% travel_up(SearchSpace, SuspectId, N, AncestorId).  
	% True iff AncestorId is the Nth ancestor of SuspectId in SearchSpace.
	%
:- pred travel_up(search_space(_)::in, suspect_id::in, int::in, 
		suspect_id::out) is det.

	% incorporate_explicit_subtree(SuspectId, Node, !SearchSpace).
	% Replaces the EDT node referenced by SuspectId with Node.
	%
:- pred incorporate_explicit_subtree(suspect_id::in, T::in,
	search_space(T)::in, search_space(T)::out) is det.

	% incorporate_explicit_supertree(Store, Oracle, Node, 
	%	!SearchSpace).
	% Node should be the implicit root in a newly generated supertree
	% that represents the topmost node of the current search space.
	% Node's parent will be inserted at the top of the search space.
	%
:- pred incorporate_explicit_supertree(S::in, 
	oracle_state::in, T::in, search_space(T)::in,
	search_space(T)::out) is det <= mercury_edt(S, T).

	% extend_search_space_upwards(Store, Oracle, 
	%	!SearchSpace).
	% Attempts to add a parent of the current topmost node to the
	% search space.  Fails if this is not possible because an explicit
	% supertree is required.
	%
:- pred extend_search_space_upwards(S::in, oracle_state::in,
	search_space(T)::in, search_space(T)::out) 
	is semidet <= mercury_edt(S, T).

	% Return the EDT node corresponding to the suspect_id.
	%
:- func get_edt_node(search_space(T), suspect_id) = T.

	% Return the weight of the suspect.
	%
:- func get_weight(search_space(T), suspect_id) = int.

	% Succeeds if the suspect has been marked correct or inadmissible
	% or is the descendent of a suspect that was marked correct or 
	% inadmissible.  Fails otherwise.
	%
:- pred suspect_in_excluded_subtree(search_space(T)::in, suspect_id::in) 
	is semidet.

	% Succeeds if the suspect has been marked erroneous or is in the
	% complement of a subtree with an erroneous root. Fails otherwise.
	%
:- pred suspect_in_excluded_complement(search_space(T)::in, suspect_id::in) 
	is semidet.

	% Succeeds if the suspect's status is unknown.
	%
:- pred suspect_unknown(search_space(T)::in, suspect_id::in) is semidet.

	% Succeeds if the suspect's status is erroneous.
	%
:- pred suspect_erroneous(search_space(T)::in, suspect_id::in) is semidet.

	% Succeeds if the suspect's status is skipped.
	%
:- pred suspect_skipped(search_space(T)::in, suspect_id::in) is semidet.

	% Succeeds if the suspect's status is ignored.
	%
:- pred suspect_ignored(search_space(T)::in, suspect_id::in) is semidet.

	% first_unknown_descendent(Store, Oracle, SuspectId, 
	%	!SearchSpace, MaybeDescendent).
	% Search the search space for a suspect with status = unknown in a
	% top down fashion, starting with SuspectId.  If no unknown
	% suspect is found then MaybeDescendent will be no.  If there are no
	% unknown suspects in the explicit part of the search space and a
	% skipped, ignored or erroneous suspect is the root of an implicit
	% subtree, then the call will fail.
	%
:- pred first_unknown_descendent(S::in, oracle_state::in,
	suspect_id::in, search_space(T)::in, search_space(T)::out,
	maybe_found_descendent::out) is det <= mercury_edt(S, T).

:- type maybe_found_descendent
	--->	found(suspect_id)
	;	not_found
	;	require_explicit_subtree(suspect_id).

	% choose_skipped_suspect(SearchSpace, Skipped) True iff Skipped is the
	% skipped suspect in SearchSpace with the lowest skip order (i.e. was
	% skipped the longest time ago).  Fails if there are no skipped
	% suspects in SearchSpace.
	%
:- pred choose_skipped_suspect(search_space(T)::in, suspect_id::out)
	is semidet.

	% get_path(SearchSpace, BottomId, TopId, Path).
	% Path is InitialPath appended to the list of suspect_id's between
	% FromId and ToId (inclusive).  ToId should be an ancestor of FromId.
	% If it isn't then the call will fail.
	%
:- pred get_path(search_space(T)::in, suspect_id::in, suspect_id::in, 
	list(suspect_id)::out) is semidet.

	% Succeeds if the suspect has been marked correct or 
	% inadmissible.
	%
:- pred suspect_correct_or_inadmissible(search_space(T)::in, suspect_id::in) 
	is semidet.

	% Succeeds if the suspect has been marked inadmissible.
	%
:- pred suspect_inadmissible(search_space(T)::in, suspect_id::in) is semidet.

	% When tracking a sub-term, should we give up if we reach the given 
	% suspect, because the binding node must lie in a portion of 
	% the tree we've already eliminated?
	%
:- pred give_up_subterm_tracking(search_space(T)::in, suspect_id::in,
	subterm_mode::in) is semidet.

	% Mark the root and its non-ignored children as unknown.
	% Throws an exception if the search space doesn't have a root.
	%
:- pred revise_root(S::in, search_space(T)::in, search_space(T)::out) 
	is det <= mercury_edt(S, T).

	% Check the consistency of the search space if the
	% MR_DD_CHECK_SEARCH_SPACE C macro is defined.  Throw an exception
	% if it's not consistent.  Used for assertion checking during
	% debugging.
	%
:- pred maybe_check_search_space_consistency(S::in, search_space(T)::in,
	string::in) is det <= mercury_edt(S, T).

	% Return the proc_label for the given suspect.
	%
:- func get_proc_label_for_suspect(S, search_space(T), suspect_id) = proc_label
	<= mercury_edt(S, T).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception, map, int, counter, std_util, string, bimap.
:- import_module float.

	% A suspect is an edt node with some additional information relevant
	% to the bug search.
	% 
:- type suspect(T)
	--->	suspect(
				% The suspect's parent id in the search space.
				% no if the suspect is at the root of the
				% search space.
			parent			:: maybe(suspect_id),
			
				% The EDT node.
			edt_node		:: T,

				% What is the status of this node with 
				% respect to the bug search.
			status			:: suspect_status,
				
				% The depth of the suspect in the EDT.
				% Initially the depth of the topmost node will
				% be zero, however if a new explicit supertree
				% is generated and added to the search space,
				% we allow the depth of the new topmost node 
				% to be negative.
			depth			:: int,
			
				% The children of the suspect.  If this is
				% no then the children have not yet been 
				% explored.  Children are only added to the
				% search space when they are required.
			children		:: maybe(list(suspect_id)),

				% A weighting used for divide and query
				% search.
			weight			:: int
	).

:- type suspect_status
	--->	ignored 
	;	skipped(int)	% We record the order nodes were skipped in.
	;	correct
	;	erroneous
	;	inadmissible   
	
			% The suspect is in a subtree with a correct or 
			% inadmissible root.
	;	pruned
			
			% The suspect was in the complement of a subtree with
			% an erroneous root.
	;	in_erroneous_subtree_complement
	;	unknown.

:- type suspect_id == int.

:- type search_space(T) 
	--->	search_space(
				% The root of the subtree in the search space
				% that contains a bug, based on the answers 
				% received so far.  The search space root
				% will be the last suspect marked erroneous, 
				% or no if no suspects have been marked 
				% erroneous yet.
			root			:: maybe(suspect_id),
			
				% The topmost node of all the nodes in the
				% search space.  Will be no if the search
				% space is empty.
			topmost			:: maybe(suspect_id),
			
				% Counter for generating suspect_ids.
			suspect_id_counter	:: counter,

				% So we can keep the skipped nodes in some
				% kind of order to avoid asking about
				% the same skipped node twice in a row.
			skip_counter		:: counter,
			
				% The collection of suspects in the search
				% space.
			store			:: map(suspect_id, suspect(T)),

				% A map of roots of implicit subtrees in the
				% EDT to explicit subtrees.
				% We use a bimap so we can also find the
				% implicit root given an explicit root.
				%
			implicit_roots_to_explicit_roots	:: bimap(T, T)
	).

empty_search_space = search_space(no, no, counter.init(0), counter.init(0), 
	map.init, bimap.init).

root(SearchSpace, RootId) :- SearchSpace ^ root = yes(RootId).

topmost_det(SearchSpace, TopMostId) :- 
	(
		SearchSpace ^ topmost = yes(Id),
		TopMostId = Id
	;
		SearchSpace ^ topmost = no,
		throw(internal_error("topmost_det", "search space empty"))
	).

suspect_correct_or_inadmissible(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Status = Suspect ^ status,
	( Status = correct ; Status = inadmissible ).

	% Succeeds if the suspect is in a part of the search space that could
	% contain a bug.
	%
:- pred suspect_in_buggy_subtree(search_space(T)::in, 
	suspect_id::in) is semidet.

suspect_in_buggy_subtree(SearchSpace, SuspectId) :-
	in_buggy_subtree(get_status(SearchSpace, SuspectId), yes).

suspect_inadmissible(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ status = inadmissible.

suspect_unknown(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ status = unknown.

suspect_erroneous(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ status = erroneous.

suspect_skipped(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ status = skipped(_).

suspect_ignored(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ status = ignored.

suspect_in_excluded_subtree(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	excluded_subtree(Suspect ^ status, yes).

	% Succeeds if we haven't got an answer from the oracle about this
	% suspect, and haven't been able to infer anything about this suspect
	% from other oracle answers.
	%
:- pred suspect_is_questionable(search_space(T)::in, suspect_id::in) 
	is semidet.
	
suspect_is_questionable(SearchSpace, SuspectId) :-
	questionable(get_status(SearchSpace, SuspectId), yes).

	% Does the given status mean the suspect is in a subtree that was
	% excluded from the bug search (because it was marked correct or
	% inadmissible or is the descedent of such a suspect)?
	%
:- pred excluded_subtree(suspect_status::in, bool::out) is det.

excluded_subtree(ignored, no).
excluded_subtree(skipped(_), no).
excluded_subtree(correct, yes).
excluded_subtree(erroneous, no).
excluded_subtree(inadmissible, yes).
excluded_subtree(pruned, yes).
excluded_subtree(in_erroneous_subtree_complement, no).
excluded_subtree(unknown, no).

	% Does the status mean we haven't got an answer from the oracle, or
	% haven't been able to infer anything about this suspect from other
	% oracle answers?
	%
:- pred questionable(suspect_status::in, bool::out) is det.

questionable(ignored, no).
questionable(skipped(_), yes).
questionable(correct, no).
questionable(erroneous, no).
questionable(inadmissible, no).
questionable(pruned, no).
questionable(in_erroneous_subtree_complement, no).
questionable(unknown, yes).

suspect_in_excluded_complement(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	excluded_complement(Suspect ^ status, yes).

	% Does the given status mean the suspect is in the complement of 
	% a subtree whos root was marked erroneous or is erroneous itself.
	%
:- pred excluded_complement(suspect_status::in, bool::out) is det.

excluded_complement(ignored, no).
excluded_complement(skipped(_), no).
excluded_complement(correct, no).
excluded_complement(erroneous, yes).
excluded_complement(inadmissible, no).
excluded_complement(pruned, no).
excluded_complement(in_erroneous_subtree_complement, yes).
excluded_complement(unknown, no).

	% Does the given status mean the suspect is in a subtree that could
	% contain a bug. 
	%
:- pred in_buggy_subtree(suspect_status::in, bool::out) is det.

in_buggy_subtree(ignored, yes).
in_buggy_subtree(skipped(_), yes).
in_buggy_subtree(correct, no).
in_buggy_subtree(erroneous, yes).
in_buggy_subtree(inadmissible, no).
in_buggy_subtree(pruned, no).
in_buggy_subtree(in_erroneous_subtree_complement, no).
in_buggy_subtree(unknown, yes).

	% Return the status that should be assigned to children of a suspect 
	% with the given status, when the children are being added to the
	% search space.
	%
:- func new_child_status(suspect_status) = suspect_status.

new_child_status(ignored) = unknown.
new_child_status(skipped(_)) = unknown.
new_child_status(correct) = pruned.
new_child_status(erroneous) = unknown.
new_child_status(inadmissible) = pruned.
new_child_status(pruned) = pruned.
new_child_status(in_erroneous_subtree_complement) = 
	in_erroneous_subtree_complement.
new_child_status(unknown) = unknown.

	% Return the status that should be assigned to the parent of a suspect
	% with the given status, when the parent is being added to the search
	% space.
	%
:- func new_parent_status(suspect_status) = suspect_status.

new_parent_status(ignored) = unknown.
new_parent_status(skipped(_)) = unknown.
new_parent_status(correct) = unknown.
new_parent_status(erroneous) = in_erroneous_subtree_complement.
new_parent_status(inadmissible) = unknown.
new_parent_status(pruned) = pruned.
new_parent_status(in_erroneous_subtree_complement) = 
	in_erroneous_subtree_complement.
new_parent_status(unknown) = unknown.

give_up_subterm_tracking(SearchSpace, SuspectId, subterm_in) :-
	Status = get_status(SearchSpace, SuspectId),
	excluded_complement(Status, yes).

	% Mark the suspect as correct or inadmissible.
	%
:- pred assert_suspect_is_valid(suspect_status::in, suspect_id::in, 
	search_space(T)::in, search_space(T)::out) is det.

assert_suspect_is_valid(Status, SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, 
		(Suspect ^ status := Status) ^ weight := 0, SuspectStore),
	!:SearchSpace = !.SearchSpace ^ store := SuspectStore,
	(
		Suspect ^ children = yes(Children),
		list.foldl(propagate_status_downwards(pruned, 
			[correct, inadmissible]), Children, !SearchSpace)
	;
		Suspect ^ children = no
	),
	% Remove the suspect's weight from its ancestors, since its weight is
	% now zero.
	add_weight_to_ancestors(SuspectId, - Suspect ^ weight, !SearchSpace),
	%
	% If the suspect was erroneous or excluded because of another erronoeus
	% suspect, then we should update the complement of the subtree rooted
	% at the suspect to unknown.
	%
	(
		excluded_complement(Suspect ^ status, yes)
	->
		propagate_status_upwards(unknown, [erroneous], SuspectId, 
			Lowest, !SearchSpace),
		%
		% Update the root to the next lowest erroneous suspect.
		%
		(
			suspect_erroneous(!.SearchSpace, Lowest)
		->
			!:SearchSpace = !.SearchSpace ^ root := yes(Lowest)
		;
			!:SearchSpace = !.SearchSpace ^ root := no 
		)
	;
		true
	).

assert_suspect_is_inadmissible(SuspectId, !SearchSpace) :-
	assert_suspect_is_valid(inadmissible, SuspectId, !SearchSpace).

assert_suspect_is_correct(SuspectId, !SearchSpace) :-
	assert_suspect_is_valid(correct, SuspectId, !SearchSpace).

assert_suspect_is_erroneous(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		erroneous, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	propagate_status_upwards(in_erroneous_subtree_complement, 
		[erroneous, correct, inadmissible], SuspectId, _,
		!SearchSpace),
	!:SearchSpace = !.SearchSpace ^ root := yes(SuspectId).

ignore_suspect(Store, SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children, 
		ignored, !.SearchSpace, Weight, _),
	map.set(!.SearchSpace ^ store, SuspectId, 
		(Suspect^ status := ignored) ^ weight := Weight, SuspectStore),
	!:SearchSpace = !.SearchSpace ^ store := SuspectStore,
	add_weight_to_ancestors(SuspectId, Weight - Suspect ^ weight,
		!SearchSpace).

skip_suspect(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	counter.allocate(N, !.SearchSpace ^ skip_counter, SkipCounter),
	!:SearchSpace = !.SearchSpace ^ skip_counter := SkipCounter,
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		skipped(N), Store),
	!:SearchSpace = !.SearchSpace ^ store := Store.

revise_root(Store, !SearchSpace) :-
	(
		!.SearchSpace ^ root = yes(RootId),
		force_propagate_status_downwards(unknown, 
			[correct, inadmissible], RootId, StopSuspects, 
			!SearchSpace),
		list.foldl(force_propagate_status_downwards(unknown, [correct,
			inadmissible]), StopSuspects, !SearchSpace),
		propagate_status_upwards(unknown, [erroneous, correct,
			inadmissible], RootId, Lowest, !SearchSpace),
		( suspect_erroneous(!.SearchSpace, Lowest) ->
			!:SearchSpace = !.SearchSpace ^ root := yes(Lowest)
		;
			!:SearchSpace = !.SearchSpace ^ root := no
		),
		%
		% Recompute the suspect weights from the bottom up.
		%
		map.keys(!.SearchSpace ^ store, AllSuspects),
		list.filter(suspect_is_leaf(!.SearchSpace), AllSuspects, 
			Leaves),
		recalc_weights_upto_ancestor(Store, Lowest, Leaves, 
			!SearchSpace)
	;
		!.SearchSpace ^ root = no,
		throw(internal_error("revise_root", "no root"))
	).

	% True if the suspect is a leaf node in the search space (i.e. it has
	% either `no' or `yes([])' in its children field.
	%
:- pred suspect_is_leaf(search_space(T)::in, suspect_id::in)
	is semidet.

suspect_is_leaf(SearchSpace, SuspectId) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	(Suspect ^ children = no ; Suspect ^ children = yes([])).

suspect_depth(SearchSpace, SuspectId) = Suspect ^ depth :-
	lookup_suspect(SearchSpace, SuspectId, Suspect).

travel_up(SearchSpace, StartId, Distance, FinishId) :- 
	(
		Distance > 0,
		lookup_suspect(SearchSpace, StartId, Suspect),
		Suspect ^ parent = yes(ParentId)
	->
		travel_up(SearchSpace, ParentId, Distance - 1, FinishId)
	;
		FinishId = StartId
	).

find_subterm_origin(Store, Oracle, SuspectId, ArgPos, TermPath, 
		!SearchSpace, Response) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	ImplicitToExplicit = !.SearchSpace ^
		implicit_roots_to_explicit_roots,
	% The node in the search space will be the explicit version.
	ExplicitNode = Suspect ^ edt_node,
	edt_subterm_mode(Store, ExplicitNode, ArgPos, TermPath, Mode),
	%
	% If the mode is input then the origin will be in a parent or a 
	% sibling.  In either case we need access to a parent EDT node, so
	% if the node is at the top of a generated explicit subtree we must use
	% the implicit root instead, so the dependency tracking algorithm 
	% has access to the node's parent and siblings in the EDT.
	%
	(
		Mode = subterm_in,
		bimap.search(ImplicitToExplicit, ImplicitNode, ExplicitNode)
	->
		Node = ImplicitNode
	;
		Node = ExplicitNode
	),
	(
		Mode = subterm_in,
		(
			Suspect ^ parent = yes(ParentId),
			resolve_origin(Store, Oracle, Node,
				ArgPos, TermPath, ParentId, no, !SearchSpace,
				Response)
		;
			Suspect ^ parent = no,
			(
				extend_search_space_upwards(Store, 
					Oracle, !SearchSpace)
			->
				topmost_det(!.SearchSpace, NewRootId),
				resolve_origin(Store, Oracle, 
					Node, ArgPos, TermPath, NewRootId, no,
					!SearchSpace, Response)
			;
				Response = require_explicit_supertree
			)
		)
	;
		Mode = subterm_out,
		resolve_origin(Store, Oracle, Node, ArgPos,
			TermPath, SuspectId, yes, !SearchSpace,
			Response)
	).

	% resolve_origin(Store, Oracle, Node, ArgPos, TermPath, 
	%	SuspectId, Output, !SearchSpace, Response).
	% Find the origin of the subterm in Node and report the origin as
	% SuspectId if the origin is a primitive op or an input and as the
	% appropriate child of SuspectId if the origin is an output.  SuspectId
	% should point to a parent of Node if the mode of the sub-term is
	% input and should point to Node itself if the mode of the sub-term is
	% output.  Output should be yes if the sub-term is an output of
	% SuspectId and no if it isn't.
	%
:- pred resolve_origin(S::in, oracle_state::in, T::in, 
	arg_pos::in, term_path::in, suspect_id::in, bool::in,
	search_space(T)::in, search_space(T)::out, find_origin_response::out)
	is det <= mercury_edt(S, T).

resolve_origin(Store, Oracle, Node, ArgPos, TermPath, SuspectId, 
		Output, !SearchSpace, Response) :-
	edt_dependency(Store, Node, ArgPos, TermPath, _, Origin),
	(
		Origin = primitive_op(FileName, LineNo, PrimOpType),
		Response = primitive_op(SuspectId, FileName, LineNo, 
			PrimOpType, Output)
	;
		Origin = not_found,
		Response = not_found
	;
		Origin = input(InputArgPos, InputTermPath),
		Response = origin(SuspectId, InputArgPos, InputTermPath, 
			subterm_in)
	;
		Origin = output(OriginNode, OutputArgPos, OutputTermPath),
		(
			bimap.search(!.SearchSpace ^
				implicit_roots_to_explicit_roots, OriginNode,
				ExplicitNode)
		->
			ExplicitOrigin = ExplicitNode
		;
			ExplicitOrigin = OriginNode
		),
		(
			children(Store, Oracle, SuspectId, 
				!SearchSpace, Children)
		->
			(
				find_edt_node_in_suspect_list(Children,
					ExplicitOrigin, !.SearchSpace,
					OriginId)
			->
				Response = origin(OriginId, OutputArgPos,
					OutputTermPath, subterm_out)
			;
				throw(internal_error("find_subterm_origin",
					"output origin for input subterm "++
					"not in siblings"))
			)
		;
			Response = require_explicit_subtree
		)
	;
		Origin = require_explicit_subtree,
		Response = require_explicit_subtree
	).

	% Returns the suspect id in the given list that refers to the given edt
	% node or fails if it can't find such a suspect in the list.
	%
:- pred find_edt_node_in_suspect_list(list(suspect_id)::in, T::in,
	search_space(T)::in, suspect_id::out) is semidet.

find_edt_node_in_suspect_list([SuspectId | SuspectIds], Node, SearchSpace, 
		FoundId) :-
	(	
		map.search(SearchSpace ^ store, SuspectId, Suspect),
		Node = Suspect ^ edt_node
	->
		FoundId = SuspectId
	;
		find_edt_node_in_suspect_list(SuspectIds, Node, SearchSpace, 
			FoundId)
	).

	% Looks up the suspect in the search space and throws an exception if
	% it can't find the suspect.
	% 
:- pred lookup_suspect(search_space(T)::in, suspect_id::in, suspect(T)::out) 
	is det.

lookup_suspect(SearchSpace, SuspectId, Suspect) :-
	(
		map.search(SearchSpace ^ store, SuspectId, FoundSuspect)
	->
		Suspect = FoundSuspect
	;
		throw(internal_error("lookup_suspect", 
			"couldn't find suspect"))
	).

	% propagate_status_downwards(Status, StopStatusSet, SuspectId, 
	%	StopSuspects, !SearchSpace). 
	% Sets the status of SuspectId and all its descendents to Status.
	% If a descendent (including the suspect) already has a status in
	% StopStatusSet then propagate_status_downwards won't update any
	% further descendents.  The list of all the children of the lowest
	% updated suspects is returned in StopSuspects.
	% 
:- pred propagate_status_downwards(suspect_status::in, 
	list(suspect_status)::in, suspect_id::in, list(suspect_id)::out, 
	search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, StopSuspects, 
	!SearchSpace) :-
	propagate_status_downwards(Status, StopStatusSet, SuspectId, [], 
		StopSuspects, !SearchSpace).

	% A version of propagate_status_downwards which doesn't return leaves.
	%
:- pred propagate_status_downwards(suspect_status::in, 
	list(suspect_status)::in, suspect_id::in, 
	search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, !SearchSpace) :-
	propagate_status_downwards(Status, StopStatusSet, SuspectId, _, 
		!SearchSpace).

	% An accumulator version of propagate_status_downwards.
	%
:- pred propagate_status_downwards(suspect_status::in, 
	list(suspect_status)::in, suspect_id::in, 
	list(suspect_id)::in, list(suspect_id)::out, 
	search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, !StopSuspects, 
		!SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		\+ member(Suspect ^ status, StopStatusSet)
	->
		map.set(!.SearchSpace ^ store, SuspectId, 
			Suspect ^ status := Status, Store),
		!:SearchSpace = !.SearchSpace ^ store := Store,
		(
			Suspect ^ children = yes(Children),
			list.foldl2(propagate_status_downwards(Status, 
				StopStatusSet), Children, !StopSuspects,
				!SearchSpace)
		;
			Suspect ^ children = no
		)
	;
		list.cons(SuspectId, !StopSuspects)
	).

	% force_propagate_status_downwards is like propagate_status_downwards,
	% except that the given suspect's status will be changed no matter what
	% its current status.
	%
:- pred force_propagate_status_downwards(suspect_status::in, 
	list(suspect_status)::in, suspect_id::in, 
	search_space(T)::in, search_space(T)::out) is det.

force_propagate_status_downwards(Status, StopStatusSet, SuspectId, 
		!SearchSpace) :-
	force_propagate_status_downwards(Status, StopStatusSet, SuspectId, _, 
		!SearchSpace).

:- pred force_propagate_status_downwards(suspect_status::in, 
	list(suspect_status)::in, suspect_id::in, list(suspect_id)::out,
	search_space(T)::in, search_space(T)::out) is det.

force_propagate_status_downwards(Status, StopStatusSet, SuspectId, 
		StopSuspects, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, 
		Suspect ^ status := Status, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	(
		Suspect ^ children = yes(Children),
		list.foldl2(propagate_status_downwards(Status, StopStatusSet), 
			Children, [], StopSuspects, !SearchSpace)
	;
		Suspect ^ children = no,
		StopSuspects = []
	).

maybe_check_search_space_consistency(Store, SearchSpace, Context) :-
	(
		should_check_search_space_consistency
	->
		check_search_space_consistency(Store, SearchSpace, Context)
	;
		true
	).

:- pred check_search_space_consistency(S::in, search_space(T)::in,
	string::in) is det <= mercury_edt(S, T).

check_search_space_consistency(Store, SearchSpace, Context) :-
	(
		find_inconsistency_in_weights(Store, SearchSpace, Message)
	->
		throw(internal_error("check_search_space_consistency",
			Message ++ "\n Context = " ++ Context))
	;
		true
	).

:- pred should_check_search_space_consistency is semidet.

:- pragma foreign_proc("C", 
	should_check_search_space_consistency, 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	#ifdef MR_DD_CHECK_SEARCH_SPACE
		SUCCESS_INDICATOR = MR_TRUE;
	#else
		SUCCESS_INDICATOR = MR_FALSE;
	#endif
").

	% Calculate the weight of a suspect based on the weights of its
	% children.  If the node is correct or inadmissible then the weight is
	% zero.  If the node is ignored then the weight is the sum of the
	% weights of the children plus the sum of the excess weights of the
	% children.  Otherwise the weight is the original weight of the node
	% as reported by edt_weight/4 minus the original weights of the
	% children as reported by edt_weight/4 plus the current weight of
	% the children plus any excess weight in the children.
	%
:- pred calc_suspect_weight(S::in, T::in, maybe(list(suspect_id))::in,
	suspect_status::in, search_space(T)::in, int::out, int::out) 
	is det <= mercury_edt(S, T).

calc_suspect_weight(Store, Node, MaybeChildren, Status, SearchSpace, Weight,
		ExcessWeight) :-
	(
		( Status = correct
		; Status = inadmissible
		)
	->
		Weight = 0,
		ExcessWeight = 0
	;
		edt_weight(Store, Node, OriginalWeight, ExcessWeight),
		(
			MaybeChildren = no,
			Weight = OriginalWeight
		;
			MaybeChildren = yes(Children),
			list.map(lookup_suspect(SearchSpace), Children,
				ChildrenSuspects),
			ChildrenNodes = list.map(
				func(S) = N :- N = S ^ edt_node, 
				ChildrenSuspects),
			list.foldl2(add_original_weight(Store), 
				ChildrenNodes, 0, ChildrenOriginalWeight,
				0, ChildrenExcess),
			list.foldl(add_existing_weight, ChildrenSuspects, 0,
				ChildrenWeight),
			(
				Status = ignored
			->
				Weight = ChildrenWeight + ChildrenExcess
			;
				Weight = OriginalWeight - 
					ChildrenOriginalWeight + ChildrenWeight
					+ ChildrenExcess
			)
		)
	).

	% Add the given weight to the ancestors of the given suspect
	% (excluding the given suspect) until an erroneous node is encountered
	% (the erroneous node is also updated).
	%
:- pred add_weight_to_ancestors(suspect_id::in, int::in,
	search_space(T)::in, search_space(T)::out) is det.

add_weight_to_ancestors(SuspectId, Weight, !SearchSpace) :-
	(
		% 
		% Stop if the weight is 0, if the node is erroneous or
		% if there is no parent.
		%
		Weight \= 0,
		lookup_suspect(!.SearchSpace, SuspectId, Suspect),
		Suspect ^ parent = yes(ParentId)
	->
		lookup_suspect(!.SearchSpace, ParentId, Parent),
		map.set(!.SearchSpace ^ store, ParentId, 
			Parent ^ weight := Parent ^ weight + Weight,
			SuspectStore),
		!:SearchSpace = !.SearchSpace ^ store := SuspectStore,
		excluded_complement(Parent ^ status, ExcludedComplement),
		(
			ExcludedComplement = yes
		;
			ExcludedComplement = no,
			add_weight_to_ancestors(ParentId, Weight, !SearchSpace)
		)
	;
		true
	).

:- pred add_original_weight(S::in, T::in, int::in, int::out, int::in, int::out) 
	is det <= mercury_edt(S, T).

add_original_weight(Store, Node, Prev, Prev + Weight, PrevExcess, 
		PrevExcess + Excess) :-
	edt_weight(Store, Node, Weight, Excess).

:- pred add_existing_weight(suspect(T)::in, int::in, int::out) is det.

add_existing_weight(Suspect, Prev, Prev + Suspect ^ weight).

	% recalc_weights_upto_ancestor(Store, Ancestor, Suspects, !SearchSpace)
	% Recalculate the weights of the suspects in Suspects and all their
	% ancestors below Ancestor.  Ancestor must be a common ancestor
	% of all the suspects in Suspects.
	%
:- pred recalc_weights_upto_ancestor(S::in, suspect_id::in,
	list(suspect_id)::in, search_space(T)::in, search_space(T)::out) 
	is det <= mercury_edt(S, T).

recalc_weights_upto_ancestor(Store, Ancestor, SuspectIds, !SearchSpace) :-
	recalc_weights_and_get_parents(Store, SuspectIds, [], Parents,
		!SearchSpace),
	list.filter(unify(Ancestor), Parents, _, FilteredParents),
	(
		FilteredParents = [_ | _],
		list.sort_and_remove_dups(FilteredParents, SortedParents),
		recalc_weights_upto_ancestor(Store, Ancestor, SortedParents,
			!SearchSpace)
	;
		FilteredParents = [],
		recalc_weights_and_get_parents(Store, [Ancestor], [], _,
			!SearchSpace)
	).

:- pred recalc_weights_and_get_parents(S::in, list(suspect_id)::in,
	list(suspect_id)::in, list(suspect_id)::out, 
	search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

recalc_weights_and_get_parents(_, [], Parents, Parents, !SearchSpace).
recalc_weights_and_get_parents(Store, [SuspectId | SuspectIds], PrevParents,
		Parents, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children,
		Suspect ^ status, !.SearchSpace, Weight, _),
	map.set(!.SearchSpace ^ store, SuspectId, 
		Suspect ^ weight := Weight, SuspectStore),
	!:SearchSpace = !.SearchSpace ^ store := SuspectStore,
	(
		Suspect ^ parent = yes(ParentId),
		NewPrevParents = [ParentId | PrevParents]
	;
		Suspect ^ parent = no,
		NewPrevParents = PrevParents
	),
	recalc_weights_and_get_parents(Store, SuspectIds, NewPrevParents,
		Parents, !SearchSpace).

	% Work out the number of unknown suspects in the search space.
	% Used for assertion checking.
:- func calc_num_unknown(search_space(T)) = int.

calc_num_unknown(SearchSpace) = NumUnknown :-
	Suspects = map.values(SearchSpace ^ store),
	list.filter(
		( pred(suspect(_, _, Status, _, _, _)::in) is semidet :- 
			questionable(Status, yes)
		), Suspects, Questionable),
	NumUnknown = list.length(Questionable).

	% Work out the number of suspects with unexplored children.
	% Used for assertion checking.
:- func calc_num_unexplored(search_space(T)) = int.

calc_num_unexplored(SearchSpace) = NumUnexplored :-
	Suspects = map.values(SearchSpace ^ store),
	list.filter(
		( pred(suspect(_, _, Status, _, no, _)::in) is semidet :- 
			in_buggy_subtree(Status, yes)
		), Suspects, Unexplored),
	NumUnexplored = list.length(Unexplored).

	% Try to find an inconsistency in the weights of the suspects.  If one
	% is found output an error message, otherwise fail.
	%
:- pred find_inconsistency_in_weights(S::in, search_space(T)::in, 
	string::out) is semidet <= mercury_edt(S, T).

find_inconsistency_in_weights(Store, SearchSpace, Message) :-
	( root(SearchSpace, RootId) ->
		find_inconsistency_in_weights_2(Store, SearchSpace, 
			RootId, Message)
	;
		topmost_det(SearchSpace, TopMostId),
		find_inconsistency_in_weights_2(Store, SearchSpace, 
			TopMostId, Message)
	).

	% Check that the weights are correct from the given suspect down.
	%
:- pred find_inconsistency_in_weights_2(S::in, search_space(T)::in,
	suspect_id::in, string::out) is semidet <= mercury_edt(S, T).

find_inconsistency_in_weights_2(Store, SearchSpace, SuspectId, Message) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children,
		Suspect ^ status, SearchSpace, Weight, _),
	(
		Weight = Suspect ^ weight,
		Weight >= 0
	->
		Suspect ^ children = yes(Children),
		in_buggy_subtree(Suspect ^ status, yes),
		list.filter_map(find_inconsistency_in_weights_2(Store, 
			SearchSpace), Children, Messages),
		Messages = [Message | _]
	;
		Message = "Weights not consistent for suspect id " ++
			int_to_string(SuspectId) ++ ", Suspect = " ++ 
			string(Suspect) ++ " Calculated weight = " ++
			int_to_string(Weight)
	).

	% propagate_status_upwards(Status, StopStatusSet, SuspectId, Lowest, 
	% 	!SearchSpace)
	% Marks all suspects not in the subtree rooted at SuspectId
	% with Status.  If an ancestor of SuspectId has a status in 
	% StopStatusSet, then perculation will not progress passed this
	% ancestor.  The lowest ancestor of SuspectId with a status in
	% StopStatusSet is returned in Lowest.  If there are no ancestors
	% with a status in StopStatusSet then Lowest will be the topmost 
	% suspect.
	% 
:- pred propagate_status_upwards(suspect_status::in, list(suspect_status)::in, 
	suspect_id::in, suspect_id::out, 
	search_space(T)::in, search_space(T)::out) is det.

propagate_status_upwards(Status, StopStatusSet, SuspectId, Lowest, 
		!SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ parent = yes(ParentId)
	->
		get_siblings(!.SearchSpace, SuspectId, Siblings),
		list.foldl(propagate_status_downwards(Status, StopStatusSet), 
			Siblings, !SearchSpace),
		lookup_suspect(!.SearchSpace, ParentId, Parent),
		(
			\+ list.member(Parent ^ status, StopStatusSet)
		->
			propagate_status_upwards(Status, StopStatusSet,
				ParentId, Lowest, !SearchSpace),
			map.set(!.SearchSpace ^ store, ParentId, 
				Parent ^ status := Status, Store),
			!:SearchSpace = !.SearchSpace ^ store := Store
		;
			Lowest = ParentId
		)
	;
		Lowest = SuspectId
	).

	% Find the siblings of a suspect in the search space.  This does not
	% include the suspect itself.
	% 
:- pred get_siblings(search_space(T)::in, suspect_id::in, 
	list(suspect_id)::out) is det.

get_siblings(SearchSpace, SuspectId, Siblings) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ parent = yes(ParentId),
		lookup_suspect(SearchSpace, ParentId, Parent),
		(
			Parent ^ children = yes(Children),
			(
				Children = [_ | _],
				list.filter(unify(SuspectId), Children, _, 
					Siblings)
			;
				Children = [],
				throw(internal_error("get_siblings",
					"parent has no children"))
			)
		;
			Parent ^ children = no,
			throw(internal_error("get_siblings",
				"parent's children unexplored"))
		)
	;
		Suspect ^ parent = no,
		Siblings = []
	).

	% Add the list of EDT nodes to the search space as children to 
	% the given suspect.  The suspect_ids for the new suspects will
	% also be returned.
	%
:- pred add_children(S::in, oracle_state::in, list(T)::in, 
	suspect_id::in, suspect_status::in, search_space(T)::in,
	search_space(T)::out, list(suspect_id)::out) 
	is det <= mercury_edt(S, T).

add_children(Store, Oracle, EDTChildren, SuspectId, Status, 
		!SearchSpace, Children) :-
	Counter0 = !.SearchSpace ^ suspect_id_counter,
	lookup_suspect(!.SearchSpace, SuspectId, Suspect0),
	Depth = Suspect0 ^ depth + 1,
	add_children_2(Store, Oracle, EDTChildren, SuspectId, 
		Status, Depth, !SearchSpace, Counter0, Counter, Children),
	% Lookup the suspect again, since its weight and/or status may have 
	% changed.
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	!:SearchSpace = !.SearchSpace ^ suspect_id_counter := Counter,
	SuspectWithChildren = Suspect ^ children := yes(Children),
	map.set(!.SearchSpace ^ store, SuspectId, SuspectWithChildren, 
		SuspectStoreWithChildren),
	!:SearchSpace = !.SearchSpace ^ store := SuspectStoreWithChildren,
	list.foldl(adjust_suspect_status_from_oracle(Store, Oracle), Children,
		!SearchSpace),
	%
	% Recalc the weight if the suspect is ignored.  This wouldn't have
	% been done by ignore_suspect/4 since the children wouldn't have been
	% available.
	%
	(
		Suspect ^ status = ignored
	->
		calc_suspect_weight(Store, Suspect ^ edt_node, yes(Children),
			ignored, !.SearchSpace, Weight, _),
		map.set(!.SearchSpace ^ store, SuspectId, 
			SuspectWithChildren ^ weight := Weight, 	
			SuspectStoreWithWeight),
		!:SearchSpace = !.SearchSpace ^ store := 
			SuspectStoreWithWeight,
		add_weight_to_ancestors(SuspectId, Weight - Suspect ^ weight,
			!SearchSpace)
	;
		true
	).

:- pred add_children_2(S::in, oracle_state::in, list(T)::in,
	suspect_id::in, suspect_status::in, int::in,
	search_space(T)::in, search_space(T)::out, counter::in, counter::out, 
	list(suspect_id)::out) is det <= mercury_edt(S, T).

add_children_2(_, _, [], _, _, _, !SearchSpace, !Counter, []).
add_children_2(Store, Oracle, [EDTChild | EDTChildren], 
		ParentId, Status, Depth, !SearchSpace, !Counter, Children) 
		:-
	allocate(NextId, !Counter),
	calc_suspect_weight(Store, EDTChild, no, Status, !.SearchSpace, Weight,
		ExcessWeight),
	map.det_insert(!.SearchSpace ^ store, NextId, 
		suspect(yes(ParentId), EDTChild, Status, Depth, no, Weight), 
		SuspectStore),
	!:SearchSpace = !.SearchSpace ^ store := SuspectStore,
	add_weight_to_ancestors(NextId, ExcessWeight, !SearchSpace),
	add_children_2(Store, Oracle, EDTChildren, ParentId, 
		Status, Depth, !SearchSpace, !Counter, OtherChildren),
	Children = [NextId | OtherChildren].

:- pred add_child_to_parent(suspect_id::in, suspect(T)::in, suspect(T)::out) 
	is det.

add_child_to_parent(ChildId, !Parent) :-
	(
		!.Parent ^ children = no,
		NewChildren = [ChildId]
	;
		!.Parent ^ children = yes(Children),
		list.append(Children, [ChildId], NewChildren)
	),
	!:Parent = !.Parent ^ children := yes(NewChildren).

:- pred adjust_suspect_status_from_oracle(S::in, 
	oracle_state::in, suspect_id::in, search_space(T)::in,
	search_space(T)::out) is det <= mercury_edt(S, T).

adjust_suspect_status_from_oracle(Store, Oracle, SuspectId, 
		!SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ status = unknown
	->
		edt_question(Store, Suspect ^ edt_node, Question),
		(
			answer_known(Oracle, Question, Answer)
		->
			(
				Answer = ignore(_),
				ignore_suspect(Store, SuspectId, !SearchSpace)
			;
				Answer = truth_value(_, Truth),
				(
					Truth = erroneous,
					assert_suspect_is_erroneous(SuspectId,
						!SearchSpace)
				;
					Truth = correct,
					assert_suspect_is_correct(SuspectId,
						!SearchSpace)
				;
					Truth = inadmissible,
					assert_suspect_is_inadmissible(
						SuspectId, !SearchSpace)
				)
			)
		;
			true
		)
	;
		true
	).

initialise_search_space(Store, Node, SearchSpace) :-
	edt_weight(Store, Node, Weight, _),
	map.set(init, 0, suspect(no, Node, unknown, 0, no, Weight), 
		SuspectStore),
	SearchSpace = search_space(no, yes(0), counter.init(1), 
		counter.init(0), SuspectStore, bimap.init).

incorporate_explicit_subtree(SuspectId, Node, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ edt_node := Node,
		Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	bimap.set(!.SearchSpace ^ implicit_roots_to_explicit_roots, 
		Suspect ^ edt_node, Node, ImplicitToExplicit),
	!:SearchSpace = 
		!.SearchSpace ^ implicit_roots_to_explicit_roots :=
		ImplicitToExplicit.

incorporate_explicit_supertree(Store, Oracle, Node, 
		!SearchSpace) :-
	topmost_det(!.SearchSpace, OldTopMostId),
	(
		edt_parent(Store, Node, Parent)
	->
		insert_new_topmost_node(Store, Oracle, Parent, 
			!SearchSpace),
		%
		% Node implicitly represents the root of the old search space,
		% which we already have an explicit version of, so we link
		% the two by placing an entry in 
		% implicit_roots_to_explicit_roots.
		%
		bimap.set(!.SearchSpace ^ implicit_roots_to_explicit_roots, 
			Node, get_edt_node(!.SearchSpace, OldTopMostId), 
			ImplicitToExplicit),
		!:SearchSpace = 
			!.SearchSpace ^ implicit_roots_to_explicit_roots :=
			ImplicitToExplicit
	;
		throw(internal_error("incorporate_explicit_supertree",
			"no parent"))
	).

extend_search_space_upwards(Store, Oracle, !SearchSpace) :-
	topmost_det(!.SearchSpace, OldTopMostId),
	edt_parent(Store, get_edt_node(!.SearchSpace, OldTopMostId), 
		NewTopMost),
	insert_new_topmost_node(Store, Oracle, NewTopMost, 
		!SearchSpace).

	% Add the given EDT node to the top of the search space.  The given
	% node should be a parent of the current topmost node in the search 
	% space.
	%
:- pred insert_new_topmost_node(S::in, oracle_state::in, 
	T::in, search_space(T)::in, search_space(T)::out)
	is det <= mercury_edt(S, T).

insert_new_topmost_node(Store, Oracle, NewTopMostEDTNode, 
		!SearchSpace) :-
	(
		edt_children(Store, NewTopMostEDTNode, EDTChildren)
	->	
		topmost_det(!.SearchSpace, OldTopMostId),
		lookup_suspect(!.SearchSpace, OldTopMostId, OldTopMost),
		(
			%
			% One of the children of the new topmost node will be
			% the old topmost node so filter it out so it isn't
			% added twice.  
			%
			find_node_in_list(Store, EDTChildren, 
				OldTopMost ^ edt_node, Pos),
			list.split_list(Pos - 1, EDTChildren, LeftChildren, 
				[_ | RightChildren])
		->
			% 
			% Insert the new topmost node.
			%
			NewTopMostStatus = new_parent_status(
				OldTopMost ^ status),
			NewTopMostDepth = OldTopMost ^ depth - 1,
			% We will update the weight below, so for now we
			% just use 0.
			NewTopMost = suspect(no, NewTopMostEDTNode, 
				NewTopMostStatus, NewTopMostDepth, no, 0),
			some [!Counter, !SuspectStore] (
				!:Counter = !.SearchSpace ^ suspect_id_counter,
				counter.allocate(NewTopMostId, !Counter),
				!:SearchSpace = 
					!.SearchSpace ^ suspect_id_counter :=
					!.Counter,
				!:SuspectStore = !.SearchSpace ^ store,
				map.set(!.SuspectStore, NewTopMostId, 
					NewTopMost, !:SuspectStore),
				!:SearchSpace = !.SearchSpace ^ store := 
					!.SuspectStore
			),
			SiblingStatus = new_child_status(NewTopMostStatus),
			add_children(Store, Oracle,
				append(LeftChildren, RightChildren), 
				NewTopMostId, SiblingStatus,
				!SearchSpace, ChildrenIds),
			
			%
			% Now add the old topmost node as a child to the new
			% topmost node.
			%
			(
				list.split_list(Pos - 1, ChildrenIds,
					LeftChildrenIds, RightChildrenIds)
			->
				append(LeftChildrenIds, [OldTopMostId | 
					RightChildrenIds], 
					NewTopMostChildrenIds)
			;
				throw(internal_error("insert_new_topmost_node",
					"invalid position"))
			),

			calc_suspect_weight(Store, NewTopMostEDTNode, 
				yes(NewTopMostChildrenIds), NewTopMostStatus,
				!.SearchSpace, Weight, _),
			some [!SuspectStore] (
				!:SuspectStore = !.SearchSpace ^ store,
				NewTopMostWithCorrectChildren = 
					NewTopMost ^ children := 
					yes(NewTopMostChildrenIds),
				NewTopMostWithCorrectWeight =
					NewTopMostWithCorrectChildren 
						^ weight := Weight,
				map.set(!.SuspectStore, NewTopMostId, 
					NewTopMostWithCorrectWeight,
					!:SuspectStore),
				map.set(!.SuspectStore, OldTopMostId, 
					OldTopMost ^ parent := 
						yes(NewTopMostId), 
					!:SuspectStore),
				!:SearchSpace = !.SearchSpace ^ store := 
					!.SuspectStore
			),
			!:SearchSpace = !.SearchSpace ^ topmost :=
				yes(NewTopMostId),
			adjust_suspect_status_from_oracle(Store, 
				Oracle, NewTopMostId, !SearchSpace)
		;
			throw(internal_error("insert_new_topmost_node",
				"couldn't find event number"))
		)
	;
		throw(internal_error("insert_new_topmost_node", 
			"couldn't get new topmost node's children"))
	).

:- pred find_node_in_list(S::in, list(T)::in, T::in,
	int::out) is semidet <= mercury_edt(S, T).

find_node_in_list(Store, [Node | Nodes], NodeToMatch, Pos) :-
	(
		edt_same_nodes(Store, Node, NodeToMatch)
	->
		Pos = 1
	;
		find_node_in_list(Store, Nodes, NodeToMatch, TailPos),
		Pos = TailPos + 1
	).

get_edt_node(SearchSpace, SuspectId) = Node :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Node = Suspect ^ edt_node.

get_weight(SearchSpace, SuspectId) = Weight :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Weight = Suspect ^ weight.

	% Return the status of the suspect.
:- func get_status(search_space(T), suspect_id) = suspect_status.

get_status(SearchSpace, SuspectId) = Status :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Status = Suspect ^ status.

parent(SearchSpace, SuspectId, ParentId) :-
	lookup_suspect(SearchSpace, SuspectId, Parent),
	Parent ^ parent = yes(ParentId).

children(Store, Oracle, SuspectId, !SearchSpace, Children) :- 
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ children = yes(Children)
	;
		Suspect ^ children = no,
		edt_children(Store, Suspect ^ edt_node, EDTChildren),
		NewStatus = new_child_status(Suspect ^ status),
		add_children(Store, Oracle, EDTChildren, 
			SuspectId, NewStatus, !SearchSpace, Children)
	).

non_ignored_descendents(_, _, [], !SearchSpace, []).
non_ignored_descendents(Store, Oracle, [SuspectId | SuspectIds], 
		!SearchSpace, Descendents) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ status = ignored
	->
		children(Store, Oracle, SuspectId, !SearchSpace, 
			Children),
		non_ignored_descendents(Store, Oracle, Children, 
			!SearchSpace, Descendents1)
	;
		Descendents1 = [SuspectId]
	),
	non_ignored_descendents(Store, Oracle, SuspectIds, 
		!SearchSpace, Descendents2),
	append(Descendents1, Descendents2, Descendents).

choose_skipped_suspect(SearchSpace, Skipped) :-
	SearchSpace ^ topmost = yes(TopMostId),
	% XXX This can be done more efficiently, but I don't think this
	% predicate will be called too often.
	map.foldl(least_skipped(SearchSpace), SearchSpace ^ store, TopMostId,
		Skipped),
	(
		TopMostId = Skipped
	=>
		skipped(_) = get_status(SearchSpace, TopMostId)
	).

	% least_skipped(SearchSpace, SuspectId1, Suspect1, SuspectId2, 
	%	LeastSkipped) :-
	% LeastSkipped is whichever of SuspectId1 and SuspectId2 has the lowest
	% skip order?  If neither has been skipped then LeastSuspect =
	% SuspectId2.  Suspect1 is the suspect referenced by SuspectId1 and is
	% present so we can use this predicate with map.foldl.
	%
:- pred least_skipped(search_space(T)::in, suspect_id::in, suspect(T)::in, 
	suspect_id::in, suspect_id::out) is det.

least_skipped(SearchSpace, SuspectId1, Suspect1, SuspectId2, LeastSkipped) :-
	Status1 = Suspect1 ^ status,
	Status2 = get_status(SearchSpace, SuspectId2),
	(
		Status1 = skipped(N), Status2 = skipped(M)
	->
		(
			N > M
		->
			LeastSkipped = SuspectId2
		;
			LeastSkipped = SuspectId1
		)
	;
		Status1 = skipped(_)
	->
		LeastSkipped = SuspectId1
	;
		LeastSkipped = SuspectId2
	).

first_unknown_descendent(Store, Oracle, SuspectId, !SearchSpace, 
		MaybeFound) :-
	first_unknown_descendent_list(Store, Oracle, [SuspectId], 
		!SearchSpace, MaybeFound).
		
	% first_unknown_descendent_list(Store, Oracle, List, 
	%	!SearchSpace, MaybeDescendent).
	% Find the first unknown suspect in List.  If one is found then 
	% it is returned through MaybeDescendent.  Otherwise if there are
	% any skipped, ignored or erroneous suspects in List then look in the
	% list of all the children of the skipped, ignored or erroneous nodes
	% in List, recursively.  Fails if an explicit subtree is required to
	% get the children of an implicit subtree and there are no other
	% unknown suspects.  MaybeDescendent will be no if there are no
	% unknown descendents and no explicit subtrees are required.
	%
:- pred first_unknown_descendent_list(S::in, 
	oracle_state::in, list(suspect_id)::in, search_space(T)::in,
	search_space(T)::out, maybe_found_descendent::out) 
	is det <= mercury_edt(S, T).

first_unknown_descendent_list(Store, Oracle, SuspectList,
		!SearchSpace, MaybeFound) :-
	list.filter(suspect_unknown(!.SearchSpace), SuspectList, UnknownList,
		Others),
	(
		UnknownList = [Unknown | _],
		MaybeFound = found(Unknown)
	;
		UnknownList = [],
		list.filter(suspect_in_buggy_subtree(
			!.SearchSpace), Others, InBuggySubtree),
		get_children_list(Store, Oracle, InBuggySubtree, 
			!SearchSpace, ExplicitRequired, Children),
		(
			Children = [],
			(
				ExplicitRequired = no,
				MaybeFound = not_found
			;
				ExplicitRequired = yes(RequireExplicitId),
				MaybeFound = require_explicit_subtree(
					RequireExplicitId)
			)
		;
			Children = [_ | _],
			first_unknown_descendent_list(Store, 
				Oracle, Children, !SearchSpace,
				MaybeFound0),
			(
				MaybeFound0 = not_found,
				(
					ExplicitRequired = no,
					MaybeFound = not_found
				;
					ExplicitRequired = yes(
						RequireExplicitId),
					MaybeFound = require_explicit_subtree(
						RequireExplicitId)
				)
			;
				MaybeFound0 = found(_),
				MaybeFound = MaybeFound0
			;
				MaybeFound0 = require_explicit_subtree(_),
				MaybeFound = MaybeFound0
			)
		)
	).

	% get_children_list(Store, Oracle, SuspectIds, 
	% 	!SearchSpace, ExplicitRequired, Children).
	% Children is the children of all the suspects in SuspectIds appended
	% together.  If an explicit subtree is required to find the children
	% of at least one element of SuspectIds, then ExplicitRequired will be
	% yes, otherwise it'll be no.  If an explicit subtree is required for
	% a suspect then its children are not included in Children.
	%
:- pred get_children_list(S::in, oracle_state::in, 
	list(suspect_id)::in, search_space(T)::in, search_space(T)::out,
	maybe(suspect_id)::out, list(suspect_id)::out) 
	is det <= mercury_edt(S, T).

get_children_list(_, _, [], SearchSpace, SearchSpace, no, []).
get_children_list(Store, Oracle, [SuspectId | SuspectIds], 
		!SearchSpace, ExplicitRequired, ChildrenList) :-
	get_children_list(Store, Oracle, SuspectIds, !SearchSpace, 
		ExplicitRequired0, ChildrenList0),
	(
		children(Store, Oracle, SuspectId, !SearchSpace, 
			Children)
	->
		append(Children, ChildrenList0, ChildrenList),
		ExplicitRequired = ExplicitRequired0
	;	
		ChildrenList = ChildrenList0,
		ExplicitRequired = yes(SuspectId)
	).

	% Look for an implicit root in the descendents of each suspect in
	% the list in a depth first fashion.
	%
:- pred find_first_implicit_root(S::in, search_space(T)::in, 
	list(suspect_id)::in, suspect_id::out) is semidet <= mercury_edt(S, T).

find_first_implicit_root(Store, SearchSpace, [SuspectId | SuspectIds], 
		ImplicitRoot) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Status = Suspect ^ status,
	(
		%
		% Check that it might be worth our while building an explicit
		% subtree here.
		%
		in_buggy_subtree(Status, yes),
		edt_is_implicit_root(Store, Suspect ^ edt_node)
	->
		ImplicitRoot = SuspectId
	;
		(
			in_buggy_subtree(Status, yes),
			Suspect ^ children = yes(Children),
			find_first_implicit_root(Store, SearchSpace, 
				Children, ImplicitRootInChildren)
		->
			ImplicitRoot = ImplicitRootInChildren
		;
			find_first_implicit_root(Store, SearchSpace,
				SuspectIds, ImplicitRoot)
		)
	).

get_path(SearchSpace, BottomId, TopId, Path) :-
	get_path(SearchSpace, BottomId, TopId, [], Path).

	% get_path(SearchSpace, BottomId, TopId, PathSoFar, Path).
	% Path = append(RemainingPath, PathSoFar) where RemainingPath is the
	% path in the search space between TopId and BottomId, starting at
	% TopId and ending at BottomId (inclusive).  Fails if TopId is not an
	% ancestor of BottomId.
	%
:- pred get_path(search_space(T)::in, suspect_id::in, suspect_id::in, 
	list(suspect_id)::in, list(suspect_id)::out) is semidet.

get_path(SearchSpace, BottomId, TopId, PathSoFar, Path) :-
	(
		BottomId = TopId
	->
		Path = [TopId | PathSoFar]
	;
		lookup_suspect(SearchSpace, BottomId, Bottom),
		Bottom ^ parent = yes(ParentId),
		get_path(SearchSpace, ParentId, TopId, [BottomId | PathSoFar],
			Path)
	).

primitive_op_type_to_string(foreign_proc) = "foreign procedure call".
primitive_op_type_to_string(builtin_call) = "builtin operation".
primitive_op_type_to_string(untraced_call) = "untraced call".
primitive_op_type_to_string(unification) = "unification".

get_proc_label_for_suspect(Store, SearchSpace, SuspectId) = 
	edt_proc_label(Store, get_edt_node(SearchSpace, SuspectId)).
