%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% file: make_tags.nl.
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
	% For other types, we use the bottom two bits of the word as a
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
:- import_module std_util.
:- import_module prog_io, prog_util, hlds, globals.

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
:- import_module map, list, int.
:- import_module globals, options, getopt.

%-----------------------------------------------------------------------------%

assign_constructor_tags(Ctors, Globals, CtorTags, IsEnum) :-

		% work out how many tag bits there are
	globals__get_options(Globals, OptionTable),
	map__lookup(OptionTable, num_tag_bits, int(NumTagBits0)),
	globals__get_tags_method(Globals, TagMethod),
	adjust_num_tag_bits(TagMethod, NumTagBits0, NumTagBits),

		% now assign them
	map__init(CtorTags0),
	(
		ctors_are_all_constants(Ctors)
	->
		IsEnum = yes,
		assign_enum_constants(Ctors, 0, CtorTags0, CtorTags)
	;
		IsEnum = no,
		max_num_tags(NumTagBits, MaxNumTags),
		MaxTag is MaxNumTags - 1,
		split_constructors(Ctors, Constants, Functors),
		assign_constant_tags(Constants, CtorTags0,
					CtorTags1, NextTag),
		assign_simple_tags(Functors, NextTag, MaxTag, CtorTags1,
					CtorTags)
	).

:- pred adjust_num_tag_bits(tags_method, int, int).
:- mode adjust_num_tag_bits(in, in, out) is det.

adjust_num_tag_bits(none, _, 0).
adjust_num_tag_bits(low, Num0, Num) :-
	int__min(Num0, 2, Num).  % assuming at least a 32-bit architecture
adjust_num_tag_bits(high, Num, Num).

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
	% them all complicated tags with the that tag as the
	% primary tag, and different secondary tags starting from
	% zero.

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

create_cons_id(SymName, Args, cons(Name, Arity)) :-
	unqualify_name(SymName, Name),
	list__length(Args, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
