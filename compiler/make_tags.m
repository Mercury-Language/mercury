%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1996 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: make_tags.m.
% main author: fjh.

	% This module is where we determine the representation for
	% discriminated union types.  Each d.u. type is represented as
	% a word.  In the case of functors with arguments, we allocate
	% the arguments on the heap, and the word contains a pointer to
	% those arguments.
	% 
	% For types which are just enumerations (all the constructors
	% are constants), we just assign a different value for each
	% constructor.
	%
	% For types which have only one functor of arity one, there is
	% no need to store the functor, and we just store the argument
	% value directly; construction and deconstruction unifications
	% on these type are no-ops.
	% 
	% For other types, we use a couple of bits of the word as a
	% tag.  We split the constructors into constants and functors,
	% and assign tag zero to the constants (if any).  If there is
	% more than one constant, we distinguish between the different
	% constants by the value of the rest of the word.  Then we
	% assign one tag bit each to the first few functors.  The
	% remaining functors all get the last remaining two-bit tag.
	% These functors are distinguished by a secondary tag which is
	% the first word of the argument vector for those functors.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module make_tags.
:- interface.

:- import_module prog_data, hlds_data, globals.
:- import_module bool.

% assign_constructor_tags(Constructors, Globals, TagValues, IsEnum):
%	Assign a constructor tag to each constructor for a discriminated
%	union type, and determine whether the type is an enumeration
%	type or not.  (`Globals' is passed because exact way in which
%	this is done is dependent on a compilation option.)

:- pred assign_constructor_tags(list(constructor), globals,
				cons_tag_values, bool).
:- mode assign_constructor_tags(in, in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_util, type_util, globals, options.
:- import_module int, map, list, std_util, require.

%-----------------------------------------------------------------------------%

assign_constructor_tags(Ctors, Globals, CtorTags, IsEnum) :-

		% work out how many tag bits there are
	globals__lookup_int_option(Globals, num_tag_bits, NumTagBits),

		% now assign them
	map__init(CtorTags0),
	(
		ctors_are_all_constants(Ctors)
	->
		IsEnum = yes,
		assign_enum_constants(Ctors, 0, CtorTags0, CtorTags)
	;
		IsEnum = no,
		(
			% assign single functor of arity one a `no_tag' tag
			% (unless it is type_info/1)
			type_is_no_tag_type(Ctors, SingleFunc, SingleArg)
		->
			create_cons_id(SingleFunc, [SingleArg], SingleConsId),
			map__set(CtorTags0, SingleConsId, no_tag, CtorTags)
		;
			NumTagBits = 0
		->
			( Ctors = [_SingleCtor] ->
				assign_simple_tags(Ctors, 0, 1,
					CtorTags0, CtorTags)
			;
				assign_complicated_tags(Ctors, 0, 0,
					CtorTags0, CtorTags)
			)
		;
			max_num_tags(NumTagBits, MaxNumTags),
			MaxTag is MaxNumTags - 1,
			split_constructors(Ctors, Constants, Functors),
			assign_constant_tags(Constants, CtorTags0,
						CtorTags1, NextTag),
			assign_simple_tags(Functors, NextTag, MaxTag,
						CtorTags1, CtorTags)
		)
	).

:- pred assign_enum_constants(list(constructor), int, cons_tag_values,
				cons_tag_values).
:- mode assign_enum_constants(in, in, in, out) is det.

assign_enum_constants([], _, CtorTags, CtorTags).
assign_enum_constants([Name - Args | Rest], Val, CtorTags0, CtorTags) :-
	create_cons_id(Name, Args, ConsId),
	Tag = int_constant(Val),
	map__set(CtorTags0, ConsId, Tag, CtorTags1),
	Val1 is Val + 1,
	assign_enum_constants(Rest, Val1, CtorTags1, CtorTags).

:- pred assign_constant_tags(list(constructor), cons_tag_values,
				cons_tag_values, int).
:- mode assign_constant_tags(in, in, out, out) is det.

	% If there's no constants, don't do anything.  Otherwise,
	% allocate the first tag for the constants, and give
	% them all complicated tags with that tag as the
	% primary tag, and different secondary tags starting from
	% zero.
	% Note that if there's a single constant, we still give it a
	% complicated_constant_tag rather than a simple_tag.  That's
	% because deconstruction of the complicated_constant_tag
	% is more efficient.

assign_constant_tags(Constants, CtorTags0, CtorTags1, NextTag) :-
	( Constants = [] ->
		NextTag = 0,
		CtorTags1 = CtorTags0
	;
		NextTag = 1,
		assign_complicated_constant_tags(Constants,
			0, 0, CtorTags0, CtorTags1)
	).

:- pred assign_simple_tags(list(constructor), int, int, cons_tag_values,
				cons_tag_values).
:- mode assign_simple_tags(in, in, in, in, out) is det.

assign_simple_tags([], _, _, CtorTags, CtorTags).
assign_simple_tags([Name - Args | Rest], Val, MaxTag, CtorTags0, CtorTags) :-
	create_cons_id(Name, Args, ConsId),
		% if we're about to run out of simple tags, start assigning
		% complicated tags instead
	( Val = MaxTag, Rest \= [] ->
		assign_complicated_tags([Name - Args | Rest], MaxTag, 0,
			CtorTags0, CtorTags)
	;
		Tag = simple_tag(Val),
		map__set(CtorTags0, ConsId, Tag, CtorTags1),
		Val1 is Val + 1,
		assign_simple_tags(Rest, Val1, MaxTag, CtorTags1, CtorTags)
	).

:- pred assign_complicated_tags(list(constructor), int, int, cons_tag_values,
				cons_tag_values).
:- mode assign_complicated_tags(in, in, in, in, out) is det.

assign_complicated_tags([], _, _, CtorTags, CtorTags).
assign_complicated_tags([Name - Args | Rest], PrimaryVal, SecondaryVal,
		CtorTags0, CtorTags) :-
	create_cons_id(Name, Args, ConsId),
	Tag = complicated_tag(PrimaryVal, SecondaryVal),
	map__set(CtorTags0, ConsId, Tag, CtorTags1),
	SecondaryVal1 is SecondaryVal + 1,
	assign_complicated_tags(Rest, PrimaryVal, SecondaryVal1,
		CtorTags1, CtorTags).

:- pred assign_complicated_constant_tags(list(constructor), int, int,
				cons_tag_values, cons_tag_values).
:- mode assign_complicated_constant_tags(in, in, in, in, out) is det.

assign_complicated_constant_tags([], _, _, CtorTags, CtorTags).
assign_complicated_constant_tags([Name - Args | Rest], PrimaryVal,
		SecondaryVal, CtorTags0, CtorTags) :-
	create_cons_id(Name, Args, ConsId),
	Tag = complicated_constant_tag(PrimaryVal, SecondaryVal),
	map__set(CtorTags0, ConsId, Tag, CtorTags1),
	SecondaryVal1 is SecondaryVal + 1,
	assign_complicated_constant_tags(Rest, PrimaryVal, SecondaryVal1,
		CtorTags1, CtorTags).

%-----------------------------------------------------------------------------%

:- pred max_num_tags(int, int).
:- mode max_num_tags(in, out) is det.

max_num_tags(NumTagBits, MaxTags) :-
	int__pow(2, NumTagBits, MaxTags).

%-----------------------------------------------------------------------------%

:- pred ctors_are_all_constants(list(constructor)).
:- mode ctors_are_all_constants(in) is semidet.

ctors_are_all_constants([]).
ctors_are_all_constants([_Name - Args | Rest]) :-
	Args = [],
	ctors_are_all_constants(Rest).

%-----------------------------------------------------------------------------%

:- pred split_constructors(list(constructor),
				list(constructor), list(constructor)).
:- mode split_constructors(in, out, out) is det.

split_constructors([], [], []).
split_constructors([Ctor | Ctors], Constants, Functors) :-
	Ctor = _Name - Args,
	( Args = [] ->
		Constants = [Ctor | Constants0],
		Functors = Functors0
	;
		Constants = Constants0,
		Functors = [Ctor | Functors0]
	),
	split_constructors(Ctors, Constants0, Functors0).

%-----------------------------------------------------------------------------%

:- pred create_cons_id(sym_name, list(_), cons_id).
:- mode create_cons_id(in, in, out) is det.

create_cons_id(SymName, Args, cons(SymName, Arity)) :-
	list__length(Args, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
