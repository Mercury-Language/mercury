%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: smm_fixpoint_table.m
% Main author: nancy.
%
% This modules defines a generic table to be used for fixpoint computations. 
% For each key (usually pred_proc_id), it will map a given abstract
% substitution. Here the notion of abstract substitution is abstracted 
% away as a typeclass which required only two operations: equal and init.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.smm_fixpoint_table.
:- interface.

:- import_module list.

:- type fixpoint_table(K, E). 

	% Initialise the table.
	% The first parameter is a list of keys which will be allowed in 
	% the table. 
	% fp_init(Initializer, Keys, Table).
    %
:- pred fp_init(pred(K, E), list(K), fixpoint_table(K, E)).
:- mode fp_init(pred(in, out) is det, in, out) is det.

	% Inform the table that a new run has begun.
    %
:- pred fp_new_run(fixpoint_table(K, E)::in, fixpoint_table(K, E)::out) 
        is det.

	% Which run of the fix point are we up to?
    %
:- func fp_which_run(fixpoint_table(K, E)) = int.

	% Check whether a fixpoint has been reached.
    %
:- pred fp_stable(fixpoint_table(K, E)::in) is semidet.

	% Check whether the entries are recursive.
    %
:- pred fp_is_recursive(fixpoint_table(K,E)::in) is semidet.

	% Add a new element (E) associated with key (K) to the table.
	%   - if an element is already recorded with that key, 
	%      * and if both elements are equal, then a fixpoint is obtained
	%        as far as this key is concerned
	%      * if the elements are different, fixpoint is not reached yet, 
	%	 and the new information is simply kept
	%   - if the element was not yet present in the table, add it
	%     to the table (which does not change the stability of the
	%     table) 
	% fp_add( EqualityTest, Key, Element, TableIn, TableOut).
    %
:- pred fp_add(pred(E, E), K, E, fixpoint_table(K, E), fixpoint_table(K, E)).
:- mode fp_add(pred(in, in) is semidet, in, in, in, out) is det.

	% Retrieve an element (E) associated with key (K) from the table.
	% This operation will change the state of the table if the
	% element _is_ present in the table. This means we're facing
	% a recursive calltree. If the key is not an element of the
	% allowed keys, then the procedure will fail.
    %
:- pred fp_get(K::in, E::out, fixpoint_table(K, E)::in, 
        fixpoint_table(K, E)::out) is semidet.

	% Retrieve an element (E) associated with key (K) from the table. 
	% The operation reports an error when the element is not present. 
    %
:- pred fp_get_final(K::in, E::out, fixpoint_table(K,E)::in) is det.

	% Same as fp_get_final, but the predicate fails instead
	% of aborting when the element is not present. 
    %
:- pred fp_get_final_semidet(K::in, E::out, fixpoint_table(K,E)::in) 
        is semidet.

%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module libs.
:- import_module libs.compiler_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module string.

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
                E
            ). 
		   
:- func fp_entry_init(E) = fp_entry(E).

fp_entry_init(Elem) = entry(no, Elem).  

:- func fp_entry_stability(fp_entry(E)) = bool. 

fp_entry_stability(entry(S, _)) = S. 

:- func fp_entry_elem(fp_entry(E)) = E. 

fp_entry_elem(entry(_, Elem)) = Elem. 

:- func fp_entry_init(bool, E) = fp_entry(E). 

fp_entry_init(Bool, Elem) = entry(Bool, Elem). 

fp_init(Init, Ks, ft(Ks, Run, IsRecursive, Map)) :- 
	Run = 0,
	IsRecursive = no,
	map.init(Map0),
	list.foldl(
		(pred(K::in, M0::in, M::out) is det :- 
			Init(K, E),
			map.det_insert(M0, K, fp_entry_init(E), M)
		), Ks, Map0, Map).

fp_new_run(T0, T0 ^ run := T0 ^ run + 1).

fp_which_run(T0) = T0 ^ run.

fp_is_recursive(T) :- T ^ recursive = yes.

fp_stable(T) :- 
	(
		T ^ recursive = yes
	->
		map.foldl(
			pred(_K::in, Entry::in, S0::in, S::out) is det :- 
			(
				( S0 = no -> 
					S = no
				;
					S = fp_entry_stability(Entry)
				)
			), T ^ mapping, yes, yes)
	;
		true
	). 
	
fp_add(Equal, Index, Elem, Tin, Tout) :- 
	Map = Tin ^ mapping, 
	( 
		map.search(Map, Index, Entry),
		TabledElem = fp_entry_elem(Entry)
	->
		( Equal(TabledElem, Elem) ->
			S = yes
		;
			S = no 
		),
        % whether or not the tabled element is equal to
        % the new element, the final tabled element will 
        % always be set to the new one. This is handy
        % for performing the following trick: equality
        % can be checked on some partial piece of the 
        % elements (for deciding stability), but some other
        % part might have changed, a change that should 
        % become visible in the table too. 
        % (in fact this is necessary for the reuse-fixpoint
        % table where not only the reuses are kept (the
        % abstract substitution), but also the goal that
        % might have changed. 
		FinalTabledElem = fp_entry_init(S, Elem),
		map.det_update(Map, Index, FinalTabledElem, MapOut)
	;
		% should not occur!
		map.det_insert(Map, Index, fp_entry_init(Elem), MapOut)
	),
	Tout = (Tin ^ mapping := MapOut).

fp_get(Index, Elem, Tin, Tout) :-
	List = Tin ^ keys, 
	list.member(Index, List), % can fail
	Mapin = Tin ^ mapping,
	(	
		map.search(Mapin, Index, Entry), 
		TabledElem = fp_entry_elem(Entry)
	->
		Elem = TabledElem,
		Mapout = Mapin
	;
		unexpected(this_file, "fp_get: key not in map")
	),
	Tout = (Tin ^ mapping := Mapout) ^ recursive := yes.

fp_get_final(Index, Elem, T) :- 
	( fp_get_final_semidet(Index, TabledElem, T) ->
		Elem = TabledElem
	; 
		unexpected(this_file, "fp_get_final: final element not found")
	).

fp_get_final_semidet(Index, Elem, T):- 
	map.search(T^mapping, Index, Entry),
	Elem = fp_entry_elem(Entry). 

:- func this_file = string.
this_file = "smm_fixpoint_table.m".
