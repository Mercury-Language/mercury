%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2004 The University of Melbourne.
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
% eliminated from the bug search and which nodes have been skipped or are
% trusted and in future might also store information like the probability of
% each node being buggy, based on some heursitic(s).
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
% Every node in the EDT may not have a corresponing node in the search space:
% nodes are only added to the search space when they are needed by a
% particular search algorithm.
%
% By convention nodes in the search space are referred to as `suspects', while
% nodes in the EDT are referred to as `EDT nodes', or just `nodes'.
%

:- module mdb.declarative_edt.

:- interface.

:- import_module mdbcomp.program_representation.
:- import_module mdb.io_action.
:- import_module mdb.declarative_debugger.

:- import_module list, std_util.

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
	pred edt_question(io_action_map::in, S::in, T::in, 
		decl_question(T)::out) is det,
	
		% If this node is an E-bug, then return the bug.
		% An E-bug is an erroneous node whos children are all correct.
		%
	pred edt_get_e_bug(io_action_map::in, S::in, T::in, decl_e_bug::out)
		is det,

		% If this node is an I-bug then return the bug.
		% An I-bug is an erroneous node whos children are all correct
		% or inadmissible, with at least one child being inadmissible.
		%
	pred edt_get_i_bug(S::in, T::in, T::in, decl_i_bug::out) is det,

		% Gives the list of children of the given tree node.  If the
		% tree is represented implicitly, then the procedure fails.
		%
	pred edt_children(S::in, T::in, list(T)::out) is semidet,

		% Given a subterm of a tree, find the mode of that subterm
		% and the origin of it amongst the parent, siblings or
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
	pred edt_is_implicit_root(S::in, T::in) is semidet
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

			% Subterm came from an input of the parent. The
			% arguments identify which part of which argument of
			% the clause head is the origin.
	;	input(arg_pos, term_path)

			% Subterm was constructed in the body.  We record
			% the filename and line number of the primitive
			% operation (unification or inlined foreign_proc)
			% that constructed it.
	;	primitive_op(string, int)

			% The origin could not be found due to missing
			% information.
	;	not_found.

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

	% Creates a new search space containing just the one EDT node.
	%
:- pred initialise_search_space(T::in, search_space(T)::out) is det.

	% The root of the search space is the root of the subtree of the EDT
	% that we think contains a bug, based on information received so far.
	% Normally the root will be marked erroneous, but it could also be
	% marked unknown, skipped or ignored (for example when the search has
	% just started and the oracle hasn't asserted any suspects are
	% erroneous or when a bug search is being revised.  This pred returns
	% the root, or fails if the search space is empty.
	%
:- pred root(search_space(T)::in, suspect_id::out) is semidet.

	% Returns the root but throws an exception if the search space is
	% empty.
	%
:- pred root_det(search_space(T)::in, suspect_id::out) is det.

	% no_more_questions(Store, !SearchSpace, CorrectDescendents, 
	%	InadmissibleChildren).
	% Succeeds if the root of the search space has only correct,
	% inadmissible, pruned or ignored descendents.  The direct children of
	% the root who are inadmissible are placed in InadmissibleChildren.
	% CorrectDescendents is all the correct and inadmissible 
	% descendents of the root.
	%
:- pred no_more_questions(S::in, search_space(T)::in, search_space(T)::out, 
	list(suspect_id)::out, list(suspect_id)::out) 
	is semidet <= mercury_edt(S, T).

	% children(Store, SuspectId, !SearchSpace, Children).
	% Children is the list of children of SuspectId in the SearchSpace.  If
	% the children were not in the search space then they are added.  Fails
	% if SuspectId is the root of an implicit subtree.  
	% 
:- pred children(S::in, suspect_id::in, search_space(T)::in, 
	search_space(T)::out, list(suspect_id)::out) 
	is semidet <= mercury_edt(S, T).

	% parent(SearchSpace, SuspectId, ParentId).
	% Succeeds if ParentId is the Id of the parent of SuspectId in 
	% SearchSpace and fails if SuspectId has no parent in SearchSpace.
	%
:- pred parent(search_space(T)::in, suspect_id::in, suspect_id::out) 
	is semidet.

	% Marks the suspect correct and alls its decendents as pruned.
	%
:- pred assert_suspect_is_correct(suspect_id::in, search_space(T)::in,
	search_space(T)::out) is det.

	% Marks the supect erroneous and marks the complement of the subtree
	% rooted at the erroneous suspect as in_erroneous_subtree_complement.
	%
:- pred assert_suspect_is_erroneous(suspect_id::in, search_space(T)::in,
	search_space(T)::out) is det.

	% Marks the suspect inadmissible and alls its decendents as pruned.
	%
:- pred assert_suspect_is_inadmissible(suspect_id::in, search_space(T)::in,
	search_space(T)::out) is det.

	% Marks the suspect ignored.
	%
:- pred ignore_suspect(suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det.

	% Marks the suspect as skipped.
	%
:- pred skip_suspect(suspect_id::in, search_space(T)::in, search_space(T)::out)
	is det.
	
	% find_subterm_origin(Store, SuspectId, ArgPos, TermPath, !SearchSpace,
	% 	Response).  
	% Finds the origin of the subterm given by SuspectId, ArgPos and
	% TermPath in its immediate neighbors.  If the children of a suspect
	% are required then they'll be added to the search space, unless an
	% explicit subtree is required in which case the appropriate response
	% is returned (see definition of find_origin_response type below).
	%
:- pred find_subterm_origin(S::in, suspect_id::in, arg_pos::in, term_path::in,
	search_space(T)::in, search_space(T)::out, find_origin_response::out)
	is det <= mercury_edt(S, T).

:- type find_origin_response
			% The origin couldn't be found because of insufficient
			% tracing information.
	--->	not_found
	
			% The subterm originated from the suspect referenced by 
			% argument 1.  The 2nd and 3rd arguments give the
			% position of the subterm in the origin node.
	;	origin(suspect_id, arg_pos, term_path)
	
			% The subterm was bound by a primitive operation inside
			% the suspect.  The arguments are the filename and line
			% number of primitive op that bound the subterm.
	;	primitive_op(string, int)
			
			% The suspect is the root of an implicit subtree and
			% the origin lies in one of it's children.
	;	require_explicit. 

	% Returns the depth of the suspect in the EDT.
	%
:- pred depth(suspect_id::in, search_space(T)::in, int::out) is det.

	% travel_up(SearchSpace, SuspectId, N, AncestorId).  
	% True iff AncestorId is the Nth ancestor of SuspectId in SearchSpace.
	%
:- pred travel_up(search_space(_)::in, suspect_id::in, int::in, 
		suspect_id::out) is det.

	% incorporate_explicit_subtree(SuspectId, Node, !SearchSpace).
	% Replaces the EDT node referenced by SuspectId with Node.
:- pred incorporate_explicit_subtree(suspect_id::in, T::in,
	search_space(T)::in, search_space(T)::out) is det.

	% Makes the given suspect the root of the search space and also changes
	% it and all it's descendent's status to unknown (except for skipped
	% and ignored nodes which are left as is).
	%
:- pred revise_suspect(suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det.

	% Return the EDT node corresponding to the suspect_id.
	%
:- func get_edt_node(search_space(T), suspect_id) = T.

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

	% first_unknown_descendent(Store, SuspectId, !SearchSpace, 
	%	MaybeDescendent).
	% Search the search space for a suspect with status = unknown in a
	% top down fashion, starting with SuspectId.  If no unknown
	% suspect is found then MaybeDescendent will be no.  If there are no
	% unknown suspects in the explicit part of the search space and a
	% skipped, ignored or erroneous suspect is the root of an implicit
	% subtree, then the call will fail.
	%
:- pred first_unknown_descendent(S::in, suspect_id::in,
	search_space(T)::in, search_space(T)::out, maybe(suspect_id)::out) 
	is semidet <= mercury_edt(S, T).

	% choose_skipped_suspect(SearchSpace, Skipped) True iff Skipped is the
	% skipped suspect in SearchSpace with the lowest skip order (i.e. was
	% skipped the longest time ago).  Fails if there are no skipped
	% suspects in SearchSpace.
	%
:- pred choose_skipped_suspect(search_space(T)::in, suspect_id::out)
	is semidet.

	% pick_implicit_root(Store, SearchSpace, ImplicitRoot)
	% Picks a suspect in SearchSpace who's EDT node is the root of an
	% implicit subtree. Only suspects with a status of unknown, skipped or
	% ignored will be considered.  If there are multiple such suspects then
	% one is committed to.  ImplicitRoot will always be a descendent of the
	% root of the search space.  XXX currently ImplicitRoot is chosen
	% naively, but in future better methods could be used to pick an
	% implicit root (such as the implicit root whos subtree is most lightly
	% to contain a bug according to some heuristic(s)).
	%
:- pred pick_implicit_root(S::in, search_space(T)::in, suspect_id::out) 
	is semidet <= mercury_edt(S, T).

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

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception, map, int, counter, std_util, string, bool.

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
			depth			:: int,
			
				% The children of the suspect.  If this is
				% no then the children have not yet been 
				% explored.  Children are only added to the
				% search space when they are required.
			children		:: maybe(list(suspect_id))
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
				% The root of the (potentially) buggy subtree
				% in the search space.  The search space root
				% will be the last suspect marked erroneous,
				% except for when the search first starts and
				% the oracle hasn't asserted any suspects are
				% erroneous, or when the root of the EDT is
				% revised (so its erroneous status is reset).  
			root			:: maybe(suspect_id),
			
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
			implicit_roots_to_explicit_roots	:: map(T, T)
	).

empty_search_space = search_space(no, counter.init(0), counter.init(0), 
	map.init, map.init).

root(SearchSpace, RootId) :- SearchSpace ^ root = yes(RootId).

root_det(SearchSpace, RootId) :- 
	(
		SearchSpace ^ root = yes(Id),
		RootId = Id
	;
		SearchSpace ^ root = no,
		throw(internal_error("root_det", "search space empty"))
	).

no_more_questions(Store, !SearchSpace, CorrectDescendents, 
		InadmissibleChildren) :-
	root_det(!.SearchSpace, RootId),
	!.SearchSpace ^ root = yes(RootId),
	\+ suspect_is_questionable(!.SearchSpace, RootId),
	(
		suspect_in_buggy_subtree(!.SearchSpace, RootId)
	->
		children(Store, RootId, !SearchSpace, Children),
		non_ignored_descendents(Store, Children, !SearchSpace,
			Descendents),
		filter(suspect_correct_or_inadmissible(!.SearchSpace),
			Descendents, CorrectDescendents, []),
		filter(suspect_inadmissible(!.SearchSpace), Children,
			InadmissibleChildren)
	;
		CorrectDescendents = [],
		InadmissibleChildren = []
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

:- pred suspect_inadmissible(search_space(T)::in, suspect_id::in) is semidet.

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
	% from other oracle answers?
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


	% Should the suspect's status be propogated to it's children when the
	% children are added to the search space?
	%
:- pred propogate_status_to_children(suspect_status::in, bool::out) is det.

propogate_status_to_children(ignored, no).
propogate_status_to_children(skipped(_), no).
propogate_status_to_children(correct, no).
propogate_status_to_children(erroneous, no).
propogate_status_to_children(inadmissible, no).
propogate_status_to_children(pruned, yes).
propogate_status_to_children(in_erroneous_subtree_complement, yes).
propogate_status_to_children(unknown, no).

assert_suspect_is_correct(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		correct, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	(
		Suspect ^ children = yes(Children),
		list.foldl(trickle_status(pruned), Children, 
			!SearchSpace)
	;
		Suspect ^ children = no
	).

assert_suspect_is_erroneous(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		erroneous, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	exclude_complement(SuspectId, !SearchSpace),
	!:SearchSpace = !.SearchSpace ^ root := yes(SuspectId).

assert_suspect_is_inadmissible(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		inadmissible, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	(
		Suspect ^ children = yes(Children),
		list.foldl(trickle_status(pruned), Children, 
			!SearchSpace)
	;
		Suspect ^ children = no
	).

ignore_suspect(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		ignored, Store),
	!:SearchSpace = !.SearchSpace ^ store := Store.
	
skip_suspect(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	counter.allocate(N, !.SearchSpace ^ skip_counter, SkipCounter),
	!:SearchSpace = !.SearchSpace ^ skip_counter := SkipCounter,
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ status :=
		skipped(N), Store),
	!:SearchSpace = !.SearchSpace ^ store := Store.

depth(SuspectId, SearchSpace, Depth) :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Suspect ^ depth = Depth.

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

find_subterm_origin(Store, SuspectId, ArgPos, TermPath, !SearchSpace,
		Response) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	Node = Suspect ^ edt_node,
	edt_dependency(Store, Node, ArgPos, TermPath, Mode, Origin),
	(
		Origin = primitive_op(FileName, LineNo),
		Response = primitive_op(FileName, LineNo)
	;
		Origin = not_found,
		Response = not_found
	;
		Origin = input(InputArgPos, InputTermPath),
		(
			Mode = subterm_in,
			(
				Suspect ^ parent = yes(ParentId),
				Response = origin(ParentId, InputArgPos,
					InputTermPath)
			;
				Suspect ^ parent = no,
				% Origin lies above the root of the search 
				% space, so return not_found.
				Response = not_found
			)
		;
			Mode = subterm_out,
			Response = origin(SuspectId, InputArgPos, 
				InputTermPath)
		)
	;
		Origin = output(OriginNode, OutputArgPos, OutputTermPath),
		(
			map.search(
				!.SearchSpace^implicit_roots_to_explicit_roots,
				OriginNode, ExplicitNode)
		->
			ExplicitOrigin = ExplicitNode
		;
			ExplicitOrigin = OriginNode
		),
		(
			Mode = subterm_in,
			get_siblings(!.SearchSpace, SuspectId, Siblings),
			(
				find_edt_node_in_suspect_list(Siblings, 
					ExplicitOrigin, !.SearchSpace, 
					OriginId)
			->
				Response = origin(OriginId, OutputArgPos,
					OutputTermPath)
			;
				throw(internal_error("find_subterm_origin",
					"output origin for input subterm "++
					"not in siblings"))
			)
		;
			Mode = subterm_out,
			(
				children(Store, SuspectId, !.SearchSpace,
					SearchSpace1, Children)
			->
				!:SearchSpace = SearchSpace1,
				(
					find_edt_node_in_suspect_list(Children,
						ExplicitOrigin, !.SearchSpace, 
						OriginId)
				->
					Response = origin(OriginId, 
						OutputArgPos, OutputTermPath)
				;
					throw(internal_error(
						"find_subterm_origin",
						"output origin for output "++
						"subterm not in children"))
				)
			;
				Response = require_explicit
			)
		)
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

	% Sets the status of a node and all it's descendents to the given 
	% status.  If a descendent already has the status then trickle_status
	% assumes all it's descendents already have the same status and won't
	% bother updating them.
	% 
:- pred trickle_status(suspect_status::in, suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det.

trickle_status(Status, SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ status \= Status
	->
		map.set(!.SearchSpace ^ store, SuspectId, 
			Suspect ^ status := Status, Store),
		!:SearchSpace = !.SearchSpace ^ store := Store,
		(
			Suspect ^ children = yes(Children),
			list.foldl(trickle_status(Status), Children, 
				!SearchSpace)
		;
			Suspect ^ children = no
		)
	;
		true
	).

	% Marks all suspects not in the subtree with the given suspect
	% as the root as in_erroneous_subtree_complement.
	% 
:- pred exclude_complement(suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det.

exclude_complement(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	Status = get_status(!.SearchSpace, SuspectId),  
	(
		Status \= in_erroneous_subtree_complement
	->
		(
			Suspect ^ parent = yes(ParentId)
		->
			get_siblings(!.SearchSpace, SuspectId, Siblings),
			list.foldl(trickle_status(
				in_erroneous_subtree_complement), 
				Siblings, !SearchSpace),
			exclude_complement(ParentId, !SearchSpace),
			lookup_suspect(!.SearchSpace, ParentId, Parent),
			map.set(!.SearchSpace ^ store, ParentId, 
				Parent ^ status := 
				in_erroneous_subtree_complement, Store),
			!:SearchSpace = !.SearchSpace ^ store := Store,
			!:SearchSpace = !.SearchSpace ^ root := yes(SuspectId)
		;
			true
		)
	;
		true
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
:- pred add_children(list(T)::in, suspect_id::in, suspect_status::in, 
	search_space(T)::in, search_space(T)::out, list(suspect_id)::out) 
	is det.

add_children(EDTChildren, SuspectId, Status, !SearchSpace, Children) :-
	Counter0 = !.SearchSpace ^ suspect_id_counter,
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	add_children_2(EDTChildren, SuspectId, Status, Suspect ^ depth + 1, 
		!SearchSpace, Counter0, Counter, Children),
	!:SearchSpace = !.SearchSpace ^ suspect_id_counter := Counter,
	map.set(!.SearchSpace ^ store, SuspectId, 
		Suspect ^ children := yes(Children), Store),
	!:SearchSpace = !.SearchSpace ^ store := Store.

:- pred add_children_2(list(T)::in, suspect_id::in, suspect_status::in, 
	int::in, search_space(T)::in, search_space(T)::out, counter::in,
	counter::out, list(suspect_id)::out) is det.

add_children_2([], _, _, _, SearchSpace, SearchSpace, Counter, Counter, []).

add_children_2([EDTChild | EDTChildren], SuspectId, Status, Depth, 
		!SearchSpace, !Counter, Children) :-
	(
		allocate(NextId, !Counter),
		map.det_insert(!.SearchSpace ^ store, NextId, 
			suspect(yes(SuspectId), EDTChild, Status, Depth, 
				no), Store),
		!:SearchSpace = !.SearchSpace ^ store := Store,
		add_children_2(EDTChildren, SuspectId, Status, Depth, 
			!SearchSpace, !Counter, OtherChildren),
		Children = [NextId | OtherChildren]
	).

initialise_search_space(Node, SearchSpace) :-
	map.set(init, 0, suspect(no, Node, unknown, 0, no), SuspectStore),
	SearchSpace = search_space(yes(0), counter.init(1), 
		counter.init(0), SuspectStore, map.init).

incorporate_explicit_subtree(SuspectId, Node, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	map.set(!.SearchSpace ^ store, SuspectId, Suspect ^ edt_node := Node,
		Store),
	!:SearchSpace = !.SearchSpace ^ store := Store,
	map.set(!.SearchSpace ^ implicit_roots_to_explicit_roots, 
		Suspect ^ edt_node, Node, ImplicitToExplicit),
	!:SearchSpace = 
		!.SearchSpace ^ implicit_roots_to_explicit_roots :=
		ImplicitToExplicit.
	
revise_suspect(SuspectId, !SearchSpace) :-
	!:SearchSpace = !.SearchSpace ^ root := yes(SuspectId),
	revise_suspects(SuspectId, !SearchSpace).

:- pred revise_suspects(suspect_id::in, search_space(T)::in, 
	search_space(T)::out) is det.

revise_suspects(SuspectId, !SearchSpace) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	Status = Suspect ^ status,
	(
		( Status = ignored ; Status = skipped(_) ; Status = unknown )
	->
		true
	;
		map.set(!.SearchSpace ^ store, SuspectId, 
			Suspect ^ status := unknown, Store),
		!:SearchSpace = !.SearchSpace ^ store := Store
	),
	(
		Suspect ^ children = yes(Children),
		foldl(revise_suspects, Children, !SearchSpace)
	;
		Suspect ^ children = no
	).

get_edt_node(SearchSpace, SuspectId) = Node :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Node = Suspect ^ edt_node.
	
	% Return the status of the suspect.
:- func get_status(search_space(T), suspect_id) = suspect_status.

get_status(SearchSpace, SuspectId) = Status :-
	lookup_suspect(SearchSpace, SuspectId, Suspect),
	Status = Suspect ^ status.

parent(SearchSpace, SuspectId, ParentId) :-
	lookup_suspect(SearchSpace, SuspectId, Parent),
	Parent ^ parent = yes(ParentId).

children(Store, SuspectId, !SearchSpace, Children) :- 
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ children = yes(Children)
	;
		Suspect ^ children = no,
		edt_children(Store, Suspect ^ edt_node, EDTChildren),
		(
			propogate_status_to_children(Suspect ^ status, yes)
		->
			add_children(EDTChildren, SuspectId, Suspect ^ status,
				!SearchSpace, Children)
		;
			add_children(EDTChildren, SuspectId, unknown, 
				!SearchSpace, Children)
		)
	).

	% non_ignored_descendents(Store, SuspectIds, !SearchSpace,
	%	Descendents).
	% Descendents is the non-ignored children of the suspects in 
	% SuspectIds appended together.  If a child is ignored then it's
	% non-ignored children are added to the list.  This is done 
	% recursively.  Fails if an explicit subtree is required to find
	% the children of an ignored suspect.
	%
:- pred non_ignored_descendents(S::in, list(suspect_id)::in, 
	search_space(T)::in, search_space(T)::out, list(suspect_id)::out) 
	is semidet <= mercury_edt(S, T).

non_ignored_descendents(_, [], !SearchSpace, []).
non_ignored_descendents(Store, [SuspectId | SuspectIds], !SearchSpace,
		Descendents) :-
	lookup_suspect(!.SearchSpace, SuspectId, Suspect),
	(
		Suspect ^ status = ignored
	->
		children(Store, SuspectId, !SearchSpace, Children),
		non_ignored_descendents(Store, Children, !SearchSpace,
			Descendents1)
	;
		Descendents1 = [SuspectId]
	),
	non_ignored_descendents(Store, SuspectIds, !SearchSpace, Descendents2),
	append(Descendents1, Descendents2, Descendents).

choose_skipped_suspect(SearchSpace, Skipped) :-
	SearchSpace ^ root = yes(RootId),
	% XXX This can be done more efficiently, but I don't think this
	% predicate will be called too often.
	map.foldl(least_skipped(SearchSpace), SearchSpace ^ store, RootId,
		Skipped),
	(
		RootId = Skipped
	=>
		skipped(_) = get_status(SearchSpace, RootId)
	).

	% least_skipped(SearchSpace, SuspectId1, Suspect1, SuspectId2, 
	%	LeastSkipped) :-
	% LeastSkipped is whichever of SuspectId1 and SuspectId2 has the lowest
	% skip order?  If neither has been skipped then LeastSuspect =
	% SuspectId1.  Suspect1 is the suspect referenced by SuspectId1 and is
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
		Status2 = skipped(_)
	->
		LeastSkipped = SuspectId2
	;
		LeastSkipped = SuspectId1
	).

first_unknown_descendent(Store, SuspectId, !SearchSpace, 
		MaybeDescendent) :-
	first_unknown_descendent_list(Store, [SuspectId], !SearchSpace,
		MaybeDescendent).
		
	% first_unknown_descendent_list(Store, List, !SearchSpace, 
	%	MaybeDescendent).
	% Find the first unknown suspect in List.  If one is found then 
	% it is returned through MaybeDescendent.  Otherwise if there are
	% any skipped, ignored or erroneous suspects in List then look in the
	% list of all the children of the skipped, ignored or erroneous nodes
	% in List, recursively.  Fails if an explicit subtree is required to
	% get the children of an explicit subtree and there are no other
	% unknown suspects.  MaybeDescendent will be no if there are no
	% unknown descendents and no explicit subtree's are required.
	%
:- pred first_unknown_descendent_list(S::in, list(suspect_id)::in, 
	search_space(T)::in, search_space(T)::out, maybe(suspect_id)::out)
	is semidet <= mercury_edt(S, T).
	
first_unknown_descendent_list(Store, SuspectList, !SearchSpace,
		MaybeDescendent) :-
	list.filter(suspect_unknown(!.SearchSpace), SuspectList, UnknownList,
		Others),
	(
		UnknownList = [Unknown | _],
		MaybeDescendent = yes(Unknown)
	;
		UnknownList = [],
		list.filter(suspect_in_buggy_subtree(
			!.SearchSpace), Others, InBuggySubtree),
		get_children_list(Store, InBuggySubtree, !SearchSpace,
			ExplicitRequired, Children),
		(
			Children = [],
			ExplicitRequired = no,
			MaybeDescendent = no
		;
			Children = [_ | _],
			first_unknown_descendent_list(Store, Children, 
				!SearchSpace, MaybeDescendentChildren),
			(
				MaybeDescendentChildren = no,
				ExplicitRequired = no,
				MaybeDescendent = no
			;
				MaybeDescendentChildren = yes(Unknown),
				MaybeDescendent = yes(Unknown)
			)
		)
	).

	% get_children_list(Store, SuspectIds, !SearchSpace, ExplicitRequired,
	%	Children).
	% Children is the children of all the suspects in SuspectIds appended
	% together.  If an explicit subtree is required to find the children
	% of at least one element of SuspectIds, then ExplicitRequired will be
	% yes, otherwise it'll be no.  If an explicit subtree is required for
	% a suspect then it's children are not included in Children.
	%
:- pred get_children_list(S::in, list(suspect_id)::in, search_space(T)::in,
	search_space(T)::out, bool::out, list(suspect_id)::out) is det
	<= mercury_edt(S, T).

get_children_list(_, [], SearchSpace, SearchSpace, no, []).
get_children_list(Store, [SuspectId | SuspectIds], !SearchSpace, 
		ExplicitRequired, ChildrenList) :-
	get_children_list(Store, SuspectIds, !SearchSpace, 
		ExplicitRequired0, ChildrenList0),
	(
		children(Store, SuspectId, !SearchSpace, Children)
	->
		append(Children, ChildrenList0, ChildrenList),
		ExplicitRequired = ExplicitRequired0
	;	
		ChildrenList = ChildrenList0,
		ExplicitRequired = yes
	).

pick_implicit_root(Store, SearchSpace, ImplicitRoot) :-
	root(SearchSpace, RootId),
	find_first_implicit_root(Store, SearchSpace, [RootId], ImplicitRoot).

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


