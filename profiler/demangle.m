%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
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
	;	introduced(introduced_pred_type, int, int, string)
			% type, line, sequence number, pred name
	.

:- type introduced_pred_type
	--->	(lambda)
	;	deforestation
	;	accumulator
	;	type_spec(string).

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

:- pred demangle_from_asm(string::in, string::out) is semidet.

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

:- pred demangle_from_c(string::in, string::out) is semidet.

demangle_from_c -->
	( demangle_proc_hl ->
		{ true }
	; demangle_proc_ll ->
		{ true }
	; demangle_data ->
		{ true }
	; demangle_typeclass_info ->
		{ true }
	;
		{ fail }
	).

/*---------------------------------------------------------------------------*/

:- pred demangle_proc_ll(string::in, string::out) is semidet.

demangle_proc_ll -->
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
	handle_compiler_generated_pred(ModeNum0, Category0),

	%
	% Fix any ascii codes mangled in the predicate name
	%
	fix_mangled_ascii,

	%
	% Process the mangling introduced by unused_args.m
	% and higher_order.m.
	% This involves stripping off the `__ua<m>', `__uab<m>',
	% and/or `__ho<n>' added to the end of the
	% predicate/function name, where m is the mode number.
	%
	demangle_unused_args(UnusedArgs, ModeNum0, ModeNum1),
	demangle_higher_order(HigherOrder, ModeNum1, ModeNum),

	%
	% Make sure special predicates with unused_args
	% are reported correctly.
	%
	( { UnusedArgs = yes(_), Category0 \= ordinary } ->
		remove_trailing_int(Arity)
	;
		{ true }
	),

	%
	% Separate the module name from the type name for the compiler
	% generated predicates.
	%
	( { Category0 \= ordinary } ->
		remove_prefix("_"),
		remove_maybe_module_prefix(MaybeModule,
			["IntroducedFrom__", "DeforestationIn__",
			"AccFrom__", "TypeSpecOf__"]),
		{ MaybeModule \= yes("") }
	;
		remove_maybe_module_prefix(MaybeModule,
			["IntroducedFrom__", "DeforestationIn__",
			"AccFrom__", "TypeSpecOf__"])
	),

	% Remove any prefixes added for introduced predicates,
	% and get the predicate name.
	handle_category_etc(PredName, Category0, Category),

	%
	% Now, finally, we can construct the demangled symbol name
	%
	{ format_proc(Category, MaybeModule, PredOrFunc, PredName,
		Arity, ModeNum, HigherOrder, UnusedArgs, MaybeInternalLabelNum,
		Parts, []) },
	{ string__append_list(Parts, DemangledName) },
	dcg_set(DemangledName).

:- pred demangle_proc_hl(string::in, string::out) is semidet.

demangle_proc_hl -->
	% Symbols in the Mercury standard library get an additional
	% "mercury__" prefix in their mangled name.
	maybe_remove_prefix("mercury__"),

	%
	% Get integer from end of string (it might be the mode number,
	% it might be the internal label number).
	%
	remove_trailing_int(Int),
	(
		%
		% if we got to another int, that means it is an internal
		% label of the form `append_3_p_0_1'
		% in that case, save the internal label number and then
		% get the mode number
		%
		m_remove_suffix("_"),
		remove_trailing_int(ModeNum0)
	->
		{ ModeNum1 = ModeNum0 },
		{ MaybeInternalLabelNum0 = yes(Int) }
	;
		{ ModeNum1 = Int },
		{ MaybeInternalLabelNum0 = no }
	),

	%
	% Handle the "f_" or "p_" suffix which indicates whether
	% the procedure is a function or a predicate
	%
	( m_remove_suffix("f_") ->
		{ PredOrFunc = "function" },
		{ Normal = yes }
	; m_remove_suffix("p_") ->
		{ PredOrFunc = "predicate" },
		{ Normal = yes }
	;
		% it could be a compiler-generated unify or compare predicate
		{ PredOrFunc = "predicate" },
		{ Normal = no }
	),

	(
		%
		% Scan back past the arity number and then parse it.
		%
		m_remove_suffix("_"),
		remove_trailing_int(Arity0)
	->
		{ Arity = Arity0 },
		{ ModeNum2 = ModeNum1 },
		{ MaybeInternalLabelNum = MaybeInternalLabelNum0 }
	;
		% It must be a compiler-generated unify or compare.
		% What we thought were the mode number and label number
		% were actually the arity and mode number
		{ Normal = no },
		{ Arity = ModeNum1 },
		{ yes(ModeNum2) = MaybeInternalLabelNum0 },
		{ MaybeInternalLabelNum = no }
	),
	m_remove_suffix("_"),

	%
	% Process the mangling introduced by unused_args.m
	% and higher_order.m.
	% This involves stripping off the `__ua<m>', `__uab<m>',
	% and/or `__ho<n>' added to the end of the
	% predicate/function name, where m is the mode number.
	%
	demangle_unused_args(UnusedArgs, ModeNum2, ModeNum3),
	demangle_higher_order(HigherOrder, ModeNum3, ModeNum),

	%
	% Make sure special predicates with unused_args
	% are reported correctly.
	%

	( { UnusedArgs = yes(_), Normal = no } ->
		remove_trailing_int(Arity)
	;
		{ true }
	),

	%
	% Separate the module name from the predicate name
	%
	remove_maybe_module_prefix(MaybeModule0,
		["IntroducedFrom__", "DeforestationIn__",
		"AccFrom__", "TypeSpecOf__", "__"]),

	%
	% Check whether the start of the string matches the name of
	% one of the special compiler-generated predicates; if so,
	% set the `category' to the appropriate value and then
	% skip past the prefix.  Also check that the mode number
	% is not invalid for the specified category.
	%
	handle_compiler_generated_pred(ModeNum, Category0),
	( { Category0 \= ordinary } ->
		remove_prefix("__")
	;
		[]
	),

	%
	% Check that the setting of the category matches the setting
	% of `Normal' determined above.
	%
	{ Normal = yes, Category0 = ordinary
	; Normal = no, Category0 \= ordinary
	},

	%
	% Fix any mangled ascii codes in the predicate name.
	%
	% XXX This should be done *before* stripping off
	% the mangling added by HLDS->HLDS passes such as
	% unused_args.m and higher_order.m.
	% (Doing it here means that we won't properly demangle
	% names that involve both special characters and
	% unused_args/higher_order specializations.)
	% But for the MLDS back-end, it needs to be done *after*
	% removing the module prefix, and currently that can't be
	% done until after stripping off the `__ua*' and `__ho*' suffixes.
	%
	fix_mangled_ascii,

	%
	% Fix any mangled ascii codes in the module name, if any.
	%
	{
		MaybeModule0 = no,
		MaybeModule = no
	;
		MaybeModule0 = yes(ModuleName0),
		fix_mangled_ascii(ModuleName0, ModuleName),
		MaybeModule = yes(ModuleName)
	},

	% Remove any prefixes added for introduced predicates,
	% and get the predicate name.
	handle_category_etc(PredName, Category0, Category),

	%
	% Now, finally, we can construct the demangled symbol name
	%
	{ format_proc(Category, MaybeModule, PredOrFunc, PredName,
		Arity, ModeNum, HigherOrder, UnusedArgs, MaybeInternalLabelNum,
		Parts, []) },
	{ string__append_list(Parts, DemangledName) },
	dcg_set(DemangledName).

:- pred demangle_unused_args(maybe(pair(int, bool))::out, int::in, int::out,
	string::in, string::out) is det.

demangle_unused_args(UnusedArgs, ModeNum0, ModeNum) -->
	%
	% Process the mangling introduced by unused_args.m.
	% This involves stripping off the `__ua<m>' or `__uab<m>' added to
	% the end of the predicate/function name, where m is the mode number.
	% XXX This is out-of-date. The compiler now generates names
	% such as UnusedArgs__p__[1].
	%
	(
		remove_trailing_int(UA_ModeNum),
		m_remove_suffix("__ua")
	->
		{ UnusedArgs = yes(ModeNum0 - no) },
		{ ModeNum = UA_ModeNum mod 10000 }
	;
		remove_trailing_int(UA_ModeNum),
		m_remove_suffix("__uab")
	->
		{ UnusedArgs = yes(ModeNum0 - yes) },
		{ ModeNum = UA_ModeNum mod 10000 }
	;
		{ UnusedArgs = no },
		{ ModeNum = ModeNum0 }
	).

:- pred demangle_higher_order(maybe(int)::out, int::in, int::out,
	string::in, string::out) is det.

demangle_higher_order(HigherOrder, ModeNum0, ModeNum) -->
	%
	% Process the mangling introduced by higher_order.m.
	% This involves stripping off the `__ho<n>' where
	% n is a unique identifier for this specialized version
	%
	(
		remove_trailing_int(HO_Num),
		m_remove_suffix("__ho")
	->
		{ HigherOrder = yes(HO_Num) }
	;
		{ HigherOrder = no }
	),
	{ ModeNum = ModeNum0 }.

	%
	% Check whether the start of the string matches the name of
	% one of the special compiler-generated predicates; if so,
	% set the category to the appropriate value and then
	% skip past the prefix.  Fails if the mode number
	% is invalid for the specified category.
	%
:- pred handle_compiler_generated_pred(int::in, pred_category::out,
	string::in, string::out) is semidet.

handle_compiler_generated_pred(ModeNum0, Category0) -->
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
	).

	% Remove any prefixes added for introduced predicates,
	% and get the predicate name.
:- pred handle_category_etc(string::out, pred_category::in, pred_category::out,
	string::in, string::out) is semidet.

handle_category_etc(PredName, Category0, Category) -->
	%
	% we need to look at the pred name and see if it is an
	% introduced predicate (lambda, deforestation, accumulator, etc.).
	% XXX handle multiple prefixes
	%

	=(PredName0),

	(
		(
			remove_prefix("IntroducedFrom__")
		->
			{ IntroducedPredType0 = (lambda) }
		;
			remove_prefix("DeforestationIn__")
		->
			{ IntroducedPredType0 = deforestation }
		;
			remove_prefix("AccFrom__")
		->
			{ IntroducedPredType0 = accumulator }
		;
			remove_prefix("TypeSpecOf__"),
			{ IntroducedPredType0 = type_spec("") }
		)
	->
		(
			remove_prefix("pred__")
		->
			{ LambdaPredOrFunc = "pred" }
		;
			remove_prefix("func__")
		->
			{ LambdaPredOrFunc = "func" }
		;
			{ IntroducedPredType0 = type_spec(_) },
			remove_prefix("pred_or_func__")
		->
			{ LambdaPredOrFunc = "" }
		;
			{ fail }
		),
		(
			remove_maybe_pred_name(MPredName),
			{ MPredName = yes(PredName1) },
			( { IntroducedPredType0 = type_spec(_) } ->
				remove_type_spec(TypeSpec),
				{ IntroducedPredType = type_spec(TypeSpec) },
				{ Seq = 0 },
				{ Line = 0 }

				% The compiler adds a redundant mode
				% number to the predicate name to avoid
				% creating two predicates with the same
				% name (deep profiling doesn't like that).
				% It isn't used here so we just ignore it.
				% The compiler also adds a version number
				% for the argument order used for specialized
				% versions, which can also be ignored.
			;
				{ IntroducedPredType = IntroducedPredType0 },
				remove_int(Line),
				remove_prefix("__"),
				remove_int(Seq)
			)
		->
			{ PredName = PredName1 },
			{ Category = introduced(IntroducedPredType, Line,
				Seq, LambdaPredOrFunc) }
		;
			% If we get here it usually means that there
			% were multiple prefixes, which aren't dealt
			% with properly yet. Just treat it as an
			% ordinary name for now.
			{ Category = ordinary },
			{ PredName = PredName0 }
		)
	;
		{ Category = Category0 },
		{ PredName = PredName0 }
	).

:- pred format_proc(pred_category::in, maybe(string)::in, string::in,
	string::in, int::in, int::in, maybe(int)::in,
	maybe(pair(int, bool))::in, maybe(int)::in,
	list(string)::out, list(string)::in) is det.

format_proc(Category, MaybeModule, PredOrFunc, PredName, Arity, ModeNum,
		HigherOrder, UnusedArgs, MaybeInternalLabelNum) -->
	["<"],
	{ format_maybe_module(MaybeModule, PredName, QualifiedName) },
	{
		Category = unify,
		string__format(
			"unification predicate for type `%s/%d' mode %d",
			[s(QualifiedName), i(Arity), i(ModeNum)],
			MainPart)
	;
		Category = compare,
		string__format("compare/3 predicate for type `%s/%d'",
			[s(QualifiedName), i(Arity)],
			MainPart)
	;
		Category = index,
		string__format("index/2 predicate for type `%s/%d'",
			[s(QualifiedName), i(Arity)],
			MainPart)
	;
		Category = ordinary,
		string__format("%s `%s/%d' mode %d",
			[s(PredOrFunc), s(QualifiedName), i(Arity), i(ModeNum)],
			MainPart)
	;
		Category = introduced(Type, Line, Seq, IntroPredOrFunc),
		(
			Type = (lambda),
			string__format("%s goal (#%d) from `%s' line %d",
				[s(IntroPredOrFunc), i(Seq), s(QualifiedName),
				i(Line)], MainPart)
		;
			Type = deforestation,
			string__format(
			"deforestation procedure (#%d) from `%s' line %d",
				[i(Seq), s(QualifiedName), i(Line)], MainPart)
		;
			Type = accumulator,
			string__format(
				"accumulator procedure from `%s' line %d",
				[s(QualifiedName), i(Line)], MainPart)
		;
			Type = type_spec(TypeSpec),
			string__format(
				"%s `%s/%d' mode %d (type specialized %s)",
				[s(PredOrFunc), s(QualifiedName),
				i(Arity), i(ModeNum), s(TypeSpec)],
				MainPart)
		)
	},
	[MainPart],
	( { HigherOrder = yes(HO_Num) } ->
		[" (specialized [#", string__int_to_string(HO_Num), "])"]
	;
		[]
	),
	( { UnusedArgs = yes(UA_Num - Extra) } ->
		( { Extra = yes } ->
			[" (minus extra unused args [#",
			 string__int_to_string(UA_Num),
			 "])"]
		;
			[" (minus unused args [#",
			 string__int_to_string(UA_Num),
			 "])"]
		)
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

:- pred demangle_data(string::in, string::out) is semidet.

demangle_data -->
	( remove_prefix("mercury_data_") ->
		% LLDS mangled data
		{ HighLevel = no }
	;
		% MLDS mangled data
		{ HighLevel = yes },
		maybe_remove_prefix("mercury__")
	),
	remove_maybe_module_prefix(MaybeModule0,
		["type_ctor_info_", "type_ctor_layout_",
		"type_ctor_functors_", "common_"]),
	{ MaybeModule0 = yes("") ->
		MaybeModule = no
	;
		% for the MLDS back-end,
		% the module qualifiers get include twice (XXX why?)
		HighLevel = yes,
		MaybeModule0 = yes(Twice)
	->
		Once = string__left(Twice, string__length(Twice) // 2),
		Once = string__right(Twice, string__length(Twice) // 2),
		MaybeModule = yes(Once)
	;
		MaybeModule = MaybeModule0
	},
	( remove_prefix("type_ctor_info_") ->
		{ DataCategory = info },
		remove_trailing_int(Arity),
		m_remove_suffix("_")
	; remove_prefix("type_ctor_layout_") ->
		{ DataCategory = layout },
		remove_trailing_int(Arity),
		m_remove_suffix("_")
	; remove_prefix("type_ctor_functors_") ->
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

:- pred format_data(data_category::in, maybe(string)::in, string::in, int::in,
	string::out) is semidet.

format_data(info, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<type_ctor_info for type `%s.%s/%d'>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<type_ctor_info for type `%s/%d'>",
			[s(Name), i(Arity)], Result)
	).
format_data(layout, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<type_ctor_layout for type `%s.%s/%d'>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<type_ctor_layout for type `%s/%d'>",
			[s(Name), i(Arity)], Result)
	).
format_data(functors, MaybeModule, Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<type_ctor_functors for type `%s.%s/%d'>",
			[s(Module), s(Name), i(Arity)], Result)
	;
		string__format("<type_ctor_functors for type `%s/%d'>",
			[s(Name), i(Arity)], Result)
	).
format_data(common, MaybeModule, _Name, Arity, Result) :-
	( MaybeModule = yes(Module) ->
		string__format("<shared constant number %d for module %s>",
			[i(Arity), s(Module)], Result)
	;
		fail
	).

:- pred demangle_typeclass_info(string::in, string::out) is semidet.

demangle_typeclass_info -->
	maybe_remove_prefix("mercury_data___"),
	remove_prefix("base_typeclass_info_"),
	remove_maybe_module_prefix(yes(ClassName), ["arity"]),
	{ ClassName \= "" },
	remove_prefix("arity"),
	remove_int(ClassArity),
	remove_prefix("__"),
	fix_mangled_ascii,
	demangle_class_args(ClassArity, Args),
	{ string__format("<instance declaration for %s(%s)>",
		[s(ClassName), s(Args)], Result) },
	dcg_set(Result).

:- pred demangle_class_args(int::in, string::out, string::in, string::out)
	is semidet.

demangle_class_args(Num, FormattedArgs) -->
	remove_maybe_module_prefix(yes(TypeName), ["arity"]),
	{ TypeName \= "" },
	remove_prefix("arity"),
	remove_int(TypeArity),
	remove_prefix("__"),
	( { Num > 1 } ->
		{ Sep = ", " },
		{ Num1 = Num - 1 },
		demangle_class_args(Num1, Rest)
	;
		{ Sep = "" },
		{ Rest = "" }
	),
	{ string__format("%s/%d%s%s",
		[s(TypeName), i(TypeArity), s(Sep), s(Rest)],
		FormattedArgs) }.

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

:- pred fix_mangled_ascii(string::in, string::out) is semidet.

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
	; remove_prefix("f_tuple") ->
		insert_prefix("{}")
	; remove_prefix("f_cons") ->
		insert_prefix("[|]")
	; remove_prefix("f_nil") ->
		insert_prefix("[]")
	; remove_prefix("f_") ->
		fix_mangled_ascii_chars
	;
		[]
	).

:- pred fix_mangled_ascii_chars(string::in, string::out) is semidet.

fix_mangled_ascii_chars -->
	remove_int(I),
	( remove_prefix("_") ->
		fix_mangled_ascii_chars
	;
		[]
	),
	{ char__to_int(C, I) },
	insert_prefix_char(C).

%---------------------------------------------------------------------------%

:- pred remove_int(int::out, string::in, string::out) is semidet.

remove_int(Int) -->
	remove_digit(Digit),
	remove_int_2(Digit, Int).

:- pred remove_int_2(int::in, int::out, string::in, string::out) is semidet.

remove_int_2(Int0, Int) -->
	( remove_digit(Next) ->
		{ Int1 = Int0 * 10 + Next },
		remove_int_2(Int1, Int)
	;
		{ Int = Int0 }
	).

:- pred remove_digit(int::out, string::in, string::out) is semidet.

remove_digit(Digit, String0, String) :-
	string__first_char(String0, Char, String),
	digit(Char, Digit).

:- pred digit(character::in, int::out) is semidet.

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

:- pred remove_maybe_module_prefix(maybe(string)::out, list(string)::in,
	string::in, string::out) is det.

remove_maybe_module_prefix(MaybeModule, StringsToStopAt, String0, String) :-
	(
		list__member(StopString, StringsToStopAt),
		string__prefix(String0, StopString)
	->
		MaybeModule = no,
		String = String0
	;
		string__sub_string_search(String0, "__", Index)
	->
		string__left(String0, Index, Module),
		string__length(String0, Len),
		Index2 = Index + 2,
		string__substring(String0, Index2, Len, String1),
		(
			remove_maybe_module_prefix(yes(SubModule),
				StringsToStopAt, String1, String2)
		->
			string__append_list([Module, ".", SubModule],
				QualifiedModule),
			MaybeModule = yes(QualifiedModule),
			String = String2
		;
			MaybeModule = yes(Module),
			String = String1
		)
	;
		String = String0,
		MaybeModule = no
	).

:- pred remove_maybe_pred_name(maybe(string)::out, string::in, string::out)
	is det.

remove_maybe_pred_name(MaybePredName, String0, String) :-
	(
		string__sub_string_search(String0, "__", Index)
	->
		string__left(String0, Index, PredName),
		string__length(String0, Len),
		Index2 = Index + 2,
		string__substring(String0, Index2, Len, String),
		MaybePredName = yes(PredName)
	;
		String = String0,
		MaybePredName = no
	).

:- pred remove_type_spec(string::out, string::in, string::out) is semidet.

remove_type_spec(TypeSpec, String0, String) :-
	string__length(String0, Length),
	Length > 2,
	string__unsafe_index(String0, 0, '['),
	NumBrackets = 0,
	find_matching_close_bracket(NumBrackets, Length,
		String0, 1, Index),
	string__split(String0, Index + 1, TypeSpec, String).

:- pred find_matching_close_bracket(int::in, int::in, string::in, int::in,
	int::out) is semidet.

find_matching_close_bracket(NumBrackets0, Length, String, Index0, Index) :-
	Index0 < Length,
	string__unsafe_index(String, Index0, Char),
	( Char = ']', NumBrackets0 = 0 ->
		Index = Index0
	;
		% Handle matching brackets in type names.
		( Char = '[' ->
			NumBrackets = NumBrackets0 + 1
		; Char = ']' ->
			NumBrackets = NumBrackets0 - 1
		;
			NumBrackets = NumBrackets0
		),
		find_matching_close_bracket(NumBrackets, Length,
			String, Index0 + 1, Index)
	).

:- pred maybe_remove_prefix(string::in, string::in, string::out) is det.

maybe_remove_prefix(Prefix) -->
	( remove_prefix(Prefix) -> [] ; [] ).

:- pred remove_prefix(string::in, string::in, string::out) is semidet.

remove_prefix(Prefix, Name0, Name) :-
	string__append(Prefix, Name, Name0).

:- pred m_remove_suffix(string::in, string::in, string::out) is semidet.

m_remove_suffix(Suffix, Name0, Name) :-
	string__remove_suffix(Name0, Suffix, Name).

:- pred insert_prefix(string::in, string::in, string::out) is det.

insert_prefix(Prefix, Name0, Name) :-
	string__append(Prefix, Name0, Name).

:- pred insert_prefix_char(char::in, string::in, string::out) is det.

insert_prefix_char(Prefix, Name0, Name) :-
	string__first_char(Name, Prefix, Name0).

:- pred dcg_set(T1::in, T2::in, T1::out) is det.

dcg_set(X, _, X).

:- pred format_maybe_module(maybe(string)::in, string::in, string::out) is det.

format_maybe_module(no, Name, QualifiedName) :-
	string__format("%s", [s(Name)], QualifiedName).
format_maybe_module(yes(Module), Name, QualifiedName) :-
	string__format("%s.%s", [s(Module), s(Name)], QualifiedName).

:- pred remove_trailing_int(int::out, string::in, string::out) is semidet.

remove_trailing_int(Int) -->
	remove_trailing_digit(Digit),
	( remove_trailing_int(Rest) ->
		{ Int = Rest * 10 + Digit }
	;
		{ Int = Digit }
	).

:- pred remove_trailing_digit(int::out, string::in, string::out) is semidet.

remove_trailing_digit(Digit, String0, String) :-
	string_last_char(String0, Char, String),
	digit(Char, Digit).

:- pred string_last_char(string::in, character::out, string::out) is semidet.

string_last_char(String0, Char, String) :-
	string__length(String0, Len),
	Len1 = Len - 1,
	string__index(String0, Len1, Char),
	string__left(String0, Len1, String).

/*---------------------------------------------------------------------------*/
