%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% output_prof_info.m
%
% Main author: petdr.
%
% Declare the main data structure's for mercury__profile and their access
% predicates, the actual type's are exported as well.  This is because some
% predicates need to access entire data structure.
% XXX Should maybe changed at a later date.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module output_prof_info.

:- interface.

:- import_module float, int, list,  map, string.


%-----------------------------------------------------------------------------%

	% XXX Need's to be explained more clearly.
:- type output --->
		output(
			map(string, output_prof),	% Map which contains all
							% the info which is 
							% required to generate
							% the output.
			list(string),			% list of label name's
							% used to lookup map.
							% list sorted so that
							% it is in the correct
							% order for call.
			list(string)			% same except for flat
		).

:- type output_prof --->
		output_prof(
			string,			% predicate name
			float,			% %time in current predicate and
						% descendant's
			float,			% %time in current predicate
			float,			% self: time spent in current
						% predicate
			float,			% descendants: time spent in 
						% current predicate and 
						% descendant's
			int,			% called: number of times 
						% predicate is called excluding
						% self recursive calls
			int,			% number of times predicate 
						% calls itself.
			list(parent),		
			list(child)
		).

:- type parent --->
		parent(
			string,			% parent name
			int,			% index number
			float,			% the number of second's of 
						% current predicate's self time
						% which is due to call's from
						% this parent.
			float,			% same as above for decsendant's
			int			% calls to current predicate
		).

						
:- type child --->
		child(
			string,			% child name
			int,			% index number
			float,			% the number of second's of 
						% child's self time
						% which is due to call's from
						% the current predicate.
			float,			% same as above for decsendant's
			int,			% number of times child called
			int			% total call's of child
		).


%-----------------------------------------------------------------------------%
