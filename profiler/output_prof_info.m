%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% output_prof_info.m
%
% Main author: petdr.
%
% Declare the main data structures for mercury__profile and their access
% predicates, the actual types are exported as well.  This is because some
% predicates need to access entire data structure.
% XXX Should maybe changed at a later date.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module output_prof_info.

:- interface.

:- import_module float, int, list,  map, string.

%-----------------------------------------------------------------------------%

	% XXX Needs to be explained more clearly.
:- type output
	---> output(
			map(string, output_prof),	% Map which contains all
							% the info which is 
							% required to generate
							% the output.
			list(string),			% list of label names
							% used to lookup map.
							% list sorted so that
							% it is in the correct
							% order for call.
			list(string)			% same except for flat
	).

:- type output_prof
	---> output_prof(
			string,			% predicate name
			int,			% cycle number
			float,			% %time in current predicate and
						% descendants
			float,			% %time in current predicate
			float,			% self: time spent in current
						% predicate
			float,			% descendants: time spent in 
						% current predicate and 
						% descendants
			int,			% called: number of times 
						% predicate is called excluding
						% self recursive calls
			int,			% number of times predicate 
						% calls itself.
			list(parent),		% parents of predicate
			list(child),		% children of predicate
			list(parent),		% parents and children who are
			list(child)		% members of the same cycle.
		)
	;
		output_cycle_prof(
			string,			% predicate name
			int,			% cycle number
			float,			% %time in current predicate and
						% descendants
			float,			% self: time spent in current
						% predicate
			float,			% descendants: time spent in 
						% current predicate and 
						% descendants
			int,			% called: number of times 
						% predicate is called excluding
						% self recursive calls
			int,			% number of times predicate 
						% calls itself.
			list(parent),		% parents of predicate
			list(child)		% children of predicate
		).

:- type parent
	---> parent(
			string,			% parent name
			int,			% cycle number
			float,			% the number of seconds of 
						% current predicate's self time
						% which is due to calls from
						% this parent.
			float,			% same as above for descendants
			int			% calls to current predicate
	).

						
:- type child
	---> child(
			string,			% child name
			int,			% cycle number
			float,			% the number of seconds of 
						% child's self time
						% which is due to calls from
						% the current predicate.
			float,			% same as above for descendants
			int,			% number of times child called
			int			% total calls of child
	).

%-----------------------------------------------------------------------------%
:- end_module output_prof_info.
%-----------------------------------------------------------------------------%
