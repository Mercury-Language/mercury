%-----------------------------------------------------------------------------%
%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%
%-----------------------------------------------------------------------------%
%
% File: demangle.m
% Author: fjh
%
% A mercury symbol demangler.
% This is used to convert symbol names back
% into a form that users can understand.
%
% BEWARE: the code here is duplicated in util/mdemangle.c,
% so any changes here will need to be duplicated there.
%
%-----------------------------------------------------------------------------%

:- module demangle.
:- interface.
:- import_module string.

:- pred demangle(string::in, string::out) is det.

/*---------------------------------------------------------------------------*/

:- implementation.
:- import_module int, list, char, std_util, bool, require.

:- type pred_category
	--->	index
	;	unify
	;	compare
	;	ordinary
	;	lambda(int, int, string). % line, sequence number, pred name

:- type data_category
	--->	common
	;	info
	;	layout
	;	functors.

demangle(MangledName, Name) :-
	( demangle_from_asm(MangledName, DemangledName) ->
		Name = DemangledName
	;
		Name = MangledName
	).

:- pred demangle_from_asm(string, string).
:- mode demangle_from_asm(in, out) is semidet.
demangle_from_asm -->
	% skip any leading underscore inserted by the C compiler,
	% and skip the `_entry_' prefix, if any.
	( remove_prefix("_entry_") ->
		[]
	;
		maybe_remove_prefix("_"),
		maybe_remove_prefix("_entry_")
	),

	demangle_from_c.

:- pred demangle_from_c(string, string).
:- mode demangle_from_c(in, out) is semidet.
demangle_from_c -->
	( demangle_proc ->
		{ true }
	; demangle_data ->
		{ true }
	;
		{ fail }
	).

/*---------------------------------------------------------------------------*/

:- pred demangle_proc(string, string).
:- mode demangle_proc(in, out) is semidet.
demangle_proc -->
	remove_prefix("mercury__"),

	%
	% strip off the `fn__' prefix, if any
	%
	( remove_prefix("fn__") ->
		{ PredOrFunc = "function" }
	;
		{ PredOrFunc = "predicate" }
	),

	%
	% Get integer from end of string (it might be the mode number,
	% it might be the internal label number).
	%
	remove_trailing_int(Int),

	( m_remove_suffix("i") ->
		%
		% if we got to an `i', that means it is an internal
		% label of the form `mercury__append_3_0_i1'
		% in that case, save the internal label number and then
		% get the mode number
		%
		{ MaybeInternalLabelNum = yes(Int) },
		m_remove_suffix("_"),
		remove_trailing_int(ModeNum0)
	;
		{ MaybeInternalLabelNum = no },
		{ ModeNum0 = Int }
	),

	%
	% scan back past the arity number and then parse it
	%
	m_remove_suffix("_"),
	remove_trailing_int(Arity),
	m_remove_suffix("_"),

	%
	% Now start processing from the start of the string again.
	% Check whether the start of the string matches the name of
	% one of the special compiler-generated predicates; if so,
	% set the `category' to the appropriate value and then
	% skip past the prefix.
	%
	( remove_prefix("__Unify__") ->
		{ Category0 = unify }
	; remove_prefix("__Compare__") ->
		{ Category0 = compare },
		% there should only be one mode for compare/3 preds
		{ ModeNum0 = 0 }
	; remove_prefix("__Index__") ->
		{ Category0 = index },
		% there should only be one mode for index/2 preds
		{ ModeNum0 = 0 }
	;	
		{ Category0 = ordinary }
	),

	%
	% Fix any ascii codes mangled in the predicate name
	%
	fix_mangled_ascii,

	%
	% Process the mangling introduced by unused_args.m.
	% This involves stripping off the `__ua<m>' or `__uab<m>' added to 
	% the end of the predicate/function name, where m is the mode number.
	% 
	(
		remove_trailing_int(UA_ModeNum),
		m_remove_suffix("__ua")
	->
		{ UnusedArgs = yes },
		{ ModeNum1 is UA_ModeNum mod 10000 }
	;
		remove_trailing_int(UA_ModeNum),
		m_remove_suffix("__uab")
	->
		{ UnusedArgs = yes },
		{ ModeNum1 is UA_ModeNum mod 10000 }
	;
		{ UnusedArgs = no },
		{ ModeNum1 = ModeNum0 }
	),
		
	%
	% Process the mangling introduced by higher_order.m.
	% This involves stripping off the `__ho<n>' where
	% n is a unique identifier for this specialized version
	%
	(
		remove_trailing_int(HO_ModeNum),
		m_remove_suffix("__ho")
	->
		{ HigherOrder = yes },
		{ ModeNum is HO_ModeNum mod 10000 }
	;
		{ HigherOrder = no },
		{ ModeNum = ModeNum1 }
	),

	%
	% Separate the module name from the type name for the compiler
	% generated predicates.
	%
	( { Category0 \= ordinary } ->
		remove_prefix("_"),
		remove_maybe_module_prefix(MaybeModule),
		{ MaybeModule \= yes("") }
	;
		remove_maybe_module_prefix(MaybeModule)
	),

	%
	% Make sure special predicates with unused_args 
	% are reported correctly.
	%

	( { UnusedArgs = yes, Category0 \= ordinary } ->
		remove_trailing_int(Arity)
	;
		{ true }
	),

	%
	% Now we need to look at the pred name and see if it is an
	% introduced lambda predicate.
	%

	=(PredName0),

	( remove_prefix("IntroducedFrom__") ->
		( remove_prefix("pred__") ->
			{ LambdaPredOrFunc = "pred" }
		; remove_prefix("func__") ->
			{ LambdaPredOrFunc = "func" }
		;
			{ fail }
		),
		remove_maybe_module_prefix(MPredName),
		{ MPredName = yes(PredName) },
		remove_int(Line),
		remove_prefix("__"),
		remove_int(Seq),
		{ Category = lambda(Line, Seq, LambdaPredOrFunc) }
	;
		{ Category = Category0 },
		{ PredName = PredName0 }
	),


	%
	% Now, finally, we can construct the demangled symbol name
	%
	{ format_proc(Category, MaybeModule, PredOrFunc, PredName,
		Arity, ModeNum, HigherOrder, UnusedArgs, MaybeInternalLabelNum,
		Parts, []) },
	{ string__append_list(Parts, DemangledName) },
	dcg_set(DemangledName).

:- pred format_proc(pred_category, maybe(string), string, string, int, int,
		bool, bool, maybe(int), list(string), list(string)).
:- mode format_proc(in, in, in, in, in, in, in, in, in, out, in) is det.
format_proc(Category, MaybeModule, PredOrFunc, PredName, Arity, ModeNum, 
		HigherOrder, UnusedArgs, MaybeInternalLabelNum) -->
	["<"],
	{ format_maybe_module(MaybeModule, PredName, QualifiedName) },
	{
		Category = unify,
		string__format("unification predicate for type %s/%d mode %d",
			[s(QualifiedName), i(Arity), i(ModeNum)],
			MainPart)
	;
		Category = compare,
		string__format("compare/3 predicate for type %s/%d",
			[s(QualifiedName), i(Arity)],
			MainPart)
	;
		Category = index,
		string__format("index/2 predicate for type %s/%d",
			[s(QualifiedName), i(Arity)],
			MainPart)
	;
		Category = ordinary,
		string__format("%s %s/%d mode %d",
			[s(PredOrFunc), s(QualifiedName), i(Arity), i(ModeNum)],
			MainPart)
	;
		Category = lambda(Line, Seq, LambdaPredOrFunc),
		string__format("%s goal (#%d) from %s line %d",
			[s(LambdaPredOrFunc), i(Seq), s(QualifiedName),
			i(Line)], MainPart)
	},
	[MainPart],
	( { HigherOrder = yes } ->
		[" (specialized)"]
	;
		[]
	),
	( { UnusedArgs = yes } ->
		[" (minus unused args)"]
	;
		[]
	),
	( { MaybeInternalLabelNum = yes(Internal) } ->
		{ string__format(" label %d", [i(Internal)], Label) },
		[Label]
	;
		[]
	),
	[">"].

/*---------------------------------------------------------------------------*/

% 
% Code to deal with mercury_data items.
%

:- pred demangle_data(string, string).
:- mode demangle_data(in, out) is semidet.
demangle_data -->
	remove_prefix("mercury_data_"),
	remove_maybe_module_prefix(MaybeModule0),
	{ MaybeModule0 = yes("") ->
		MaybeModule = no
	;
		MaybeModule = MaybeModule0
	},
	( remove_prefix("base_type_info_") ->
		{ DataCategory = info },
		remove_trailing_int(Arity),
		m_remove_suffix("_")
	; remove_prefix("base_type_layout_") ->
		{ DataCategory = layout },
		remove_trailing_int(Arity),
		m_remove_suffix("_")
	; remove_prefix("base_type_functors_") ->
		{ DataCategory = functors },
		remove_trailing_int(Arity),
		m_remove_suffix("_")
	; remove_prefix("common_") ->
		{ DataCategory = common },
		remove_trailing_int(Arity)
	;
		{ fail }
	),

	fix_mangled_ascii,

	=(Name),
	{ format_data(DataCategory, MaybeModule, Name, Arity, Result) },
	dcg_set(Result).

:- pred format_data(data_category, maybe(string), string, int, string).
:- mode format_data(in, in, in, in, out) is semidet.
format_data(info, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<base type_info for type '%s:%s'/%d>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<base type_info for type '%s'/%d>",
			[s(Name), i(Arity)], Result)
	).
format_data(layout, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<type layout for type '%s:%s'/%d>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<type layout for type '%s'/%d>",
			[s(Name), i(Arity)], Result)
	).
format_data(functors, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<type functors for type '%s:%s'/%d>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<type functors for type '%s'/%d>",
			[s(Name), i(Arity)], Result)
	).
format_data(common, MaybeModule, _Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<shared constant number %d for module %s>",
			[i(Arity), s(Module)], Result)
	;
		fail
	).

/*---------------------------------------------------------------------------*/

	%
	% The compiler changes all names starting with `f_' so that
	% they start with `f__' instead, and uses names starting with
	% `f_' for mangled names which are either descriptions (such
	% as `f_greater_than' for `>') or sequences of decimal
	% reprententations of ASCII codes separated by underscores.
	% If the name starts with `f__', we must change it back to
	% start with `f_'.  Otherwise, if it starts with `f_' we must
	% convert the mnemonic or list of ASCII codes back into an
	% identifier.
	%

:- pred fix_mangled_ascii(string, string).
:- mode fix_mangled_ascii(in, out) is semidet.
fix_mangled_ascii -->
	( remove_prefix("f__") ->
		insert_prefix("f_")
	; remove_prefix("f_not_equal") ->
		insert_prefix("\\=")
	; remove_prefix("f_greater_or_equal") ->
		insert_prefix(">=")
	; remove_prefix("f_less_or_equal") ->
		insert_prefix("=<")
	; remove_prefix("f_equal") ->
		insert_prefix("=")
	; remove_prefix("f_less_than") ->
		insert_prefix("<")
	; remove_prefix("f_greater_than") ->
		insert_prefix(">")
	; remove_prefix("f_minus") ->
		insert_prefix("-")
	; remove_prefix("f_plus") ->
		insert_prefix("+")
	; remove_prefix("f_times") ->
		insert_prefix("*")
	; remove_prefix("f_slash") ->
		insert_prefix("/")
	; remove_prefix("f_comma") ->
		insert_prefix(",")
	; remove_prefix("f_semicolon") ->
		insert_prefix(";")
	; remove_prefix("f_cut") ->
		insert_prefix("!")
	; remove_prefix("f_") ->
		fix_mangled_ascii_chars
	;
		[]
	).

:- pred fix_mangled_ascii_chars(string, string).
:- mode fix_mangled_ascii_chars(in, out) is semidet.
fix_mangled_ascii_chars -->
	remove_int(I),
	( remove_prefix("_") ->
		fix_mangled_ascii_chars
	;
		[]
	),
	{ char__to_int(C, I) },
	insert_prefix_char(C).

/*---------------------------------------------------------------------------*/

:- pred remove_int(int, string, string).
:- mode remove_int(out, in, out) is semidet.
remove_int(Int) -->
	remove_digit(Digit),
	remove_int_2(Digit, Int).

:- pred remove_int_2(int, int, string, string).
:- mode remove_int_2(in, out, in, out) is semidet.
remove_int_2(Int0, Int) -->
	( remove_digit(Next) ->
		{ Int1 is Int0 * 10 + Next },
		remove_int_2(Int1, Int)
	;
		{ Int = Int0 }
	).

:- pred remove_digit(int, string, string).
:- mode remove_digit(out, in, out) is semidet.
remove_digit(Digit, String0, String) :-
	string__first_char(String0, Char, String),
	digit(Char, Digit).

:- pred digit(character, int).
:- mode digit(in, uo) is semidet.
digit('0', 0).
digit('1', 1).
digit('2', 2).
digit('3', 3).
digit('4', 4).
digit('5', 5).
digit('6', 6).
digit('7', 7).
digit('8', 8).
digit('9', 9).

/*---------------------------------------------------------------------------*/

:- pred remove_maybe_module_prefix(maybe(string), string, string).
:- mode remove_maybe_module_prefix(out, in, out) is det.
remove_maybe_module_prefix(MaybeModule, String0, String) :-
	( string__sub_string_search(String0, "__", Index) ->
		string__left(String0, Index, Module),
		string__length(String0, Len),
		Index2 is Index + 2,
		string__substring(String0, Index2, Len, String),
		MaybeModule = yes(Module)
	;
		String = String0,
		MaybeModule = no
	).

:- pred maybe_remove_prefix(string, string, string).
:- mode maybe_remove_prefix(in, in, out) is det.
maybe_remove_prefix(Prefix) -->
	( remove_prefix(Prefix) -> [] ; [] ).

:- pred remove_prefix(string, string, string).
:- mode remove_prefix(in, in, out) is semidet.
remove_prefix(Prefix, Name0, Name) :-
	string__append(Prefix, Name, Name0).

:- pred m_remove_suffix(string, string, string).
:- mode m_remove_suffix(in, in, out) is semidet.
m_remove_suffix(Suffix, Name0, Name) :-
	string__remove_suffix(Name0, Suffix, Name).

:- pred insert_prefix(string, string, string).
:- mode insert_prefix(in, in, out) is det.
insert_prefix(Prefix, Name0, Name) :-
	string__append(Prefix, Name0, Name).

:- pred insert_prefix_char(char, string, string).
:- mode insert_prefix_char(in, in, out) is det.
insert_prefix_char(Prefix, Name0, Name) :-
	string__first_char(Name, Prefix, Name0).

:- pred dcg_set(T1, T2, T1).
:- mode dcg_set(in, in, out) is det.
dcg_set(X, _, X).

:- pred format_maybe_module(maybe(string), string, string).
:- mode format_maybe_module(in, in, out) is det.
format_maybe_module(no, Name, QualifiedName) :-
	string__format("'%s'", [s(Name)], QualifiedName).
format_maybe_module(yes(Module), Name, QualifiedName) :-
	string__format("%s:'%s'", [s(Module), s(Name)], QualifiedName).

:- pred remove_trailing_int(int, string, string).
:- mode remove_trailing_int(out, in, out) is semidet.
remove_trailing_int(Int) -->
	remove_trailing_digit(Digit),
	( remove_trailing_int(Rest) ->
		{ Int is Rest * 10 + Digit }
	;
		{ Int = Digit }
	).

:- pred remove_trailing_digit(int, string, string).
:- mode remove_trailing_digit(out, in, out) is semidet.
remove_trailing_digit(Digit, String0, String) :-
	string_last_char(String0, Char, String),
	digit(Char, Digit).

:- pred string_last_char(string, character, string).
:- mode string_last_char(in, out, out) is semidet.
string_last_char(String0, Char, String) :-
	string__length(String0, Len),
	Len1 is Len - 1,
	string__index(String0, Len1, Char),
	string__left(String0, Len1, String).

/*---------------------------------------------------------------------------*/
