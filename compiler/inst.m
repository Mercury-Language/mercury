%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 1999-2000, 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% inst.m - Contains the (inst) data type.
% Main author: bromage
%
%-----------------------------------------------------------------------------%

:- module (parse_tree__inst).
:- interface.

% This module should NOT import hlds*.m.  Any types which are needed in
% both the insts and in the HLDS should be defined here, rather than
% in hlds*.m, because insts are part of the parse tree and the parse tree
% should not depend on the HLDS.
%
% XXX Currently we have to import hlds_data for the `cons_id' type.
%     I think the cons_ids in insts only use a subset of the functors
%     of the `cons_id' type, and so we could define a new type
%     `abstract_cons_id' and use that here instead of `cons_id'.

:- import_module parse_tree__prog_data, hlds__hlds_data.
:- import_module list, map, set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type (inst)
	--->		any(uniqueness)
	;		free
	;		free(type)
	;		bound(uniqueness, list(bound_inst))
				% The list(bound_inst) must be sorted
	;		ground(uniqueness, ground_inst_info)
				% The ground_inst_info holds extra information
				% about the ground inst.
	;		not_reached
	;		inst_var(inst_var)
				% constrained_inst_vars is a set of inst
				% variables that are constrained to have the
				% same uniqueness as and to match_final the
				% specified inst.
	;		constrained_inst_vars(set(inst_var), inst)
				% A defined_inst is possibly recursive
				% inst whose value is stored in the
				% inst_table.  This is used both for
				% user-defined insts and for
				% compiler-generated insts.
	;		defined_inst(inst_name)
				% An abstract inst is a defined inst which
				% has been declared but not actually been
				% defined (yet).
	;		abstract_inst(sym_name, list(inst)).

:- type uniqueness
	--->		shared		% there might be other references
	;		unique		% there is only one reference
	;		mostly_unique	% there is only one reference
					% but there might be more on
					% backtracking
	;		clobbered	% this was the only reference, but
					% the data has already been reused
	;		mostly_clobbered.
					% this was the only reference, but
					% the data has already been reused;
					% however, there may be more references
					% on backtracking, so we will need to
					% restore the old value on backtracking

	% The ground_inst_info type gives extra information about ground insts.
:- type ground_inst_info
	--->	higher_order(pred_inst_info)
			% The ground inst is higher-order.
	;	none.
			% No extra information is available.

	% higher-order predicate terms are given the inst
	%	`ground(shared, higher_order(PredInstInfo))'
	% where the PredInstInfo contains the extra modes and the determinism
	% for the predicate.  Note that the higher-order predicate term
	% itself must be ground.

:- type pred_inst_info
	---> pred_inst_info(
			pred_or_func,		% is this a higher-order func
						% mode or a higher-order pred
						% mode?
			list(mode),		% the modes of the additional
						% (i.e. not-yet-supplied)
						% arguments of the pred;
						% for a function, this includes
						% the mode of the return value
						% as the last element of the
						% list.
			determinism		% the determinism of the
						% predicate or function
	).

:- type bound_inst	--->	functor(cons_id, list(inst)).

:- type inst_var_sub == map(inst_var, inst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% Empty for now.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
