%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.fixpoint_table.m
% Main author: nancy.
%
% This module defines a generic table to be used for fixpoint computations. 
% The purpose of this table is mainly to map pred_proc_id's onto abstract
% substitutions representing either structure sharing or structure reuse.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.fixpoint_table.

:- interface.

:- import_module list.

:- type fixpoint_table(K, E). 

	% Initialise the table.
	% The first parameter is a function that produces the initial value
	% for each of the keys that are to be inserted into the table. 
:- func init(func(K) = E, list(K)) = fixpoint_table(K, E).

	% Inform the table that a new run has begun.
	%
:- pred new_run(fixpoint_table(K, E)::in, fixpoint_table(K, E)::out) is det.

	% Which run of the fix point are we up to?
	%
:- func which_run(fixpoint_table(K, E)) = int.

	% Check whether a fixpoint has been reached.
	%
:- pred fixpoint_reached(fixpoint_table(K, E)::in) is semidet.

	% Return a short description of the state of the fixpoint table.
	%
:- func description(fixpoint_table(K, E)) = string.

	% Check whether the entries are recursive.
	%
:- pred is_recursive(fixpoint_table(K,E)::in) is semidet.

	% Add a new element (E) associated with key (K) to the table.
	%   - if an element is already recorded with that key:
	%      * if the new value is subsumed by the existing value, then
	%      	 a fixpoint is obtained as far as this key is concerned.
	%      * if the values are different, fixpoint is not reached yet, 
	%	 and the new value is recorded instead of the old one. 
	%   - if the key has not yet any value associated to it, add it
	%     to the table (which does not change the stability of the
	%     table) 
	% add( EqualityTest, Key, Element, TableIn, TableOut).
	%
:- pred add(pred(E, E), K, E, fixpoint_table(K, E), fixpoint_table(K, E)).
:- mode add(pred(in, in) is semidet, in, in, in, out) is det.

	% Retrieve an element E associated with key K from the table.
	% This operation changes the state of the table if the
	% element _is_ present in the table. This means we are facing
	% a recursive calltree. If the key is not an element of the
	% allowed keys, then the procedure fails.
	%
:- pred get(K::in, E::out, fixpoint_table(K, E)::in, 
	fixpoint_table(K, E)::out) is semidet.

	% Retrieve an element E associated with key K from the table. 
	% The operation reports a software error when the element is not
	% present. 
	%
:- func get_final(K, fixpoint_table(K,E)) = E.

	% Same as get_final, but the predicate fails instead of aborting when
	% the element is not present. 
	%
:- func get_final_semidet(K, fixpoint_table(K,E)) = E is semidet.

%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module libs.compiler_util.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.

:- type fixpoint_table(K, E)
	--->	ft(  
		     keys	:: list(K),	% list of allowed keys
		     run	:: int,		% number of runs
		     recursive	:: bool,	% is recursive or not
		     mapping 	:: map(K, fp_entry(E))
		).

:- type fp_entry(E) 
	--->	entry(
			bool, 	% stability: yes = stable, no = unstable
			E). 
		   
:- func fp_entry_init(E) = fp_entry(E).
:- func fp_entry_stability(fp_entry(E)) = bool. 
:- func fp_entry_elem(fp_entry(E)) = E. 
:- func fp_entry_init(bool, E) = fp_entry(E). 
fp_entry_init(Elem) = entry(no, Elem).  
fp_entry_init(Bool, Elem) = entry(Bool, Elem). 
fp_entry_stability(entry(S, _)) = S. 
fp_entry_elem(entry(_, Elem)) = Elem. 

init(InitFunction, Ks) =  ft(Ks, Run, IsRecursive, Map) :- 
	Run = 0,
	IsRecursive = no,
	map__init(Map0),
	list__foldl(
		(pred(K::in, M0::in, M::out) is det :- 
			E = InitFunction(K),
			map__det_insert(M0, K, fp_entry_init(E), M)
		),
		Ks, 
		Map0, 
		Map
	).

new_run(T0, T0 ^ run := T0 ^ run + 1).
which_run(T0) = T0 ^ run.

is_recursive(T) :- T ^ recursive = yes.

fixpoint_reached(T) :- 
	(
		T ^ recursive = yes
	->
		map__foldl(
			pred(_K::in, Entry::in, S0::in, S::out) is det :- 
			(
				( S0 = no -> 
					S = no
				;
					S = fp_entry_stability(Entry)
				)
			),
			T ^ mapping,
			yes, 
			yes)
	;
		true
	). 

description(T) = Descr :- 
	(
		fixpoint_reached(T)
	->
		Descr = "stable"
	;
		Descr = "unstable"
	).
	
	
add(IsLessOrEqualTest, Index, Elem, Tin, Tout) :- 
	Map = Tin ^ mapping, 
	map__lookup(Map, Index, Entry),
	TabledElem = fp_entry_elem(Entry),
	(
		IsLessOrEqualTest(Elem, TabledElem)
	->
		S = yes
	;
		S = no 
	),

	% whether or not the tabled element is equal to the new element, the
	% final tabled element will always be set to the new one. This is handy
	% for performing the following trick: equality can be checked on some
	% partial piece of the elements (for deciding stability), but some
	% other part might have changed, a change that should become visible in
	% the table too.  (in fact this is necessary for the reuse-fixpoint
	% table where not only the reuses are kept (the abstract substitution),
	% but also the goal that might have changed. 
	FinalTabledElem = fp_entry_init(S, Elem),
	map__det_update(Map, Index, FinalTabledElem, MapOut),
	Tout = (Tin ^ mapping := MapOut).

get(Index, Elem, Tin, Tout) :-
	List = Tin ^ keys, 
	list__member(Index, List), % can fail
	Mapin = Tin ^ mapping,
	map__lookup(Mapin, Index, Entry), 
	Elem = fp_entry_elem(Entry),
	Mapout = Mapin,
	Tout = (Tin ^ mapping := Mapout) ^ recursive := yes.

get_final(Index, T) = Elem :- 
	(
		TabledElem = get_final_semidet(Index, T)
	->
		Elem = TabledElem
	; 
		unexpected(this_file, "get_final: key not in map.")
	).

get_final_semidet(Index, T) = Elem :- 
	map__search(T ^ mapping, Index, Entry),
	Elem = fp_entry_elem(Entry). 

:- func this_file = string.
this_file = "ctgc.fixpoint_table".
