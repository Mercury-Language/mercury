%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module globals.

% Main author: fjh.

% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io__state.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module options, trace_params, prog_data.
:- import_module bool, getopt, list, io.

:- type globals.

:- type compilation_target
	--->	c	% Generate C code (including GNU C)
	;	il	% Generate IL assembler code
			% IL is the Microsoft .NET Intermediate Language
	;	java	% Generate Java
			% (Work in progress)
	;	asm. 	% Compile directly to assembler via the GCC back-end.
			% Do not go via C, instead generate GCC's internal
			% `tree' data structure.
			% (Work in progress.)
:- type gc_method
	--->	none
	;	conservative
	;	accurate.

:- type tags_method
	--->	none
	;	low
	;	high.

:- type termination_norm
	--->	simple
	;	total
	;	num_data_elems
	;	size_data_elems.

:- pred convert_target(string::in, compilation_target::out) is semidet.
:- pred convert_foreign_language(string::in, foreign_language::out) is semidet.
:- pred convert_gc_method(string::in, gc_method::out) is semidet.
:- pred convert_tags_method(string::in, tags_method::out) is semidet.
:- pred convert_termination_norm(string::in, termination_norm::out) is semidet.

%-----------------------------------------------------------------------------%

	% Access predicates for the `globals' structure.

:- pred globals__init(option_table::di, compilation_target::di, gc_method::di,
	tags_method::di, termination_norm::di,
	trace_level::di, trace_suppress_items::di, globals::uo) is det.

:- pred globals__get_options(globals::in, option_table::out) is det.
:- pred globals__get_target(globals::in, compilation_target::out) is det.
:- pred globals__get_backend_foreign_languages(globals::in,
		list(foreign_language)::out) is det.
:- pred globals__get_gc_method(globals::in, gc_method::out) is det.
:- pred globals__get_tags_method(globals::in, tags_method::out) is det.
:- pred globals__get_termination_norm(globals::in, termination_norm::out) 
	is det.
:- pred globals__get_trace_level(globals::in, trace_level::out) is det.
:- pred globals__get_trace_suppress(globals::in, trace_suppress_items::out)
	is det.

:- pred globals__set_options(globals::in, option_table::in, globals::out)
	is det.

:- pred globals__set_trace_level(globals::in, trace_level::in, globals::out)
	is det.
:- pred globals__set_trace_level_none(globals::in, globals::out) is det.

:- pred globals__lookup_option(globals::in, option::in, option_data::out)
	is det.

:- pred globals__lookup_bool_option(globals, option, bool).
:- mode globals__lookup_bool_option(in, in, out) is det.
:- mode globals__lookup_bool_option(in, in, in) is semidet. % implied
:- pred globals__lookup_int_option(globals::in, option::in, int::out) is det.
:- pred globals__lookup_string_option(globals::in, option::in, string::out)
	is det.
:- pred globals__lookup_accumulating_option(globals::in, option::in,
	list(string)::out) is det.

%-----------------------------------------------------------------------------%

	% More complex options

	% Check if static code addresses are available in the
	% current grade of compilation.

:- pred globals__have_static_code_addresses(globals::in, bool::out) is det.

	% Check if we should include variable information in the layout
	% structures of call return sites.

:- pred globals__want_return_var_layouts(globals::in, bool::out) is det.

%-----------------------------------------------------------------------------%

	% Access predicates for storing a `globals' structure in the
	% io__state using io__set_globals and io__get_globals.

:- pred globals__io_init(option_table::di, compilation_target::in,
	gc_method::in, tags_method::in,
	termination_norm::in, trace_level::in, trace_suppress_items::in,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_target(compilation_target::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_backend_foreign_languages(list(foreign_language)::out,
	io__state::di, io__state::uo) is det.
	
:- pred globals__io_lookup_foreign_language_option(option::in,
	foreign_language::out, io__state::di, io__state::uo) is det.

:- pred globals__io_get_gc_method(gc_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_tags_method(tags_method::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_termination_norm(termination_norm::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_trace_level(trace_level::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_trace_suppress(trace_suppress_items::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_get_globals(globals::out, io__state::di, io__state::uo)
	is det.

:- pred globals__io_set_globals(globals::di, io__state::di, io__state::uo)
	is det.

:- pred globals__io_set_option(option::in, option_data::in,
	io__state::di, io__state::uo) is det.

:- pred globals__io_set_trace_level(trace_level::in,
	io__state::di, io__state::uo) is det.

:- pred globals__io_set_trace_level_none(io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_option(option::in, option_data::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_bool_option(option, bool, io__state, io__state).
:- mode globals__io_lookup_bool_option(in, out, di, uo) is det.
:- mode globals__io_lookup_bool_option(in, in, di, uo) is semidet. % implied

:- pred globals__io_lookup_int_option(option::in, int::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_string_option(option::in, string::out,
	io__state::di, io__state::uo) is det.

:- pred globals__io_lookup_accumulating_option(option::in, list(string)::out,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exprn_aux.
:- import_module map, std_util, require, string.

convert_target(String, Target) :-
	convert_target_2(string__to_lower(String), Target).

:- pred convert_target_2(string::in, compilation_target::out) is semidet.

convert_target_2("java", java).
convert_target_2("asm", asm).
convert_target_2("il", il).
convert_target_2("c", c).

convert_foreign_language(String, ForeignLanguage) :-
	convert_foreign_language_2(string__to_lower(String), ForeignLanguage).

:- pred convert_foreign_language_2(string::in, foreign_language::out)
	is semidet.

convert_foreign_language_2("c", c).
convert_foreign_language_2("mc++", managed_cplusplus).
convert_foreign_language_2("managedc++", managed_cplusplus).
convert_foreign_language_2("managed c++", managed_cplusplus).
convert_foreign_language_2("c#", csharp).
convert_foreign_language_2("csharp", csharp).
convert_foreign_language_2("c sharp", csharp).
convert_foreign_language_2("il", il).

convert_gc_method("none", none).
convert_gc_method("conservative", conservative).
convert_gc_method("accurate", accurate).

convert_tags_method("none", none).
convert_tags_method("low", low).
convert_tags_method("high", high).

convert_termination_norm("simple", simple).
convert_termination_norm("total", total).
convert_termination_norm("num-data-elems", num_data_elems).
convert_termination_norm("size-data-elems", size_data_elems).

%-----------------------------------------------------------------------------%

:- type globals
	--->	globals(
			options 		:: option_table,
			target 			:: compilation_target,
			gc_method 		:: gc_method,
			tags_method 		:: tags_method,
			termination_norm 	:: termination_norm,
			trace_level 		:: trace_level,
			trace_suppress_items	:: trace_suppress_items
		).

globals__init(Options, Target, GC_Method, TagsMethod,
		TerminationNorm, TraceLevel, TraceSuppress,
	globals(Options, Target, GC_Method, TagsMethod,
		TerminationNorm, TraceLevel, TraceSuppress)).

globals__get_options(Globals, Globals ^ options).
globals__get_target(Globals, Globals ^ target).
globals__get_gc_method(Globals, Globals ^ gc_method).
globals__get_tags_method(Globals, Globals ^ tags_method).
globals__get_termination_norm(Globals, Globals ^ termination_norm).
globals__get_trace_level(Globals, Globals ^ trace_level).
globals__get_trace_suppress(Globals, Globals ^ trace_suppress_items).

globals__get_backend_foreign_languages(Globals, ForeignLangs) :-
	globals__lookup_accumulating_option(Globals, backend_foreign_languages,
		LangStrs),
	ForeignLangs = list__map(func(String) = ForeignLang :-
		( convert_foreign_language(String, ForeignLang0) ->
			ForeignLang = ForeignLang0
		;
			error("globals__io_get_backend_foreign_languages: invalid foreign_language string")
		), LangStrs).

globals__set_options(Globals, Options, Globals ^ options := Options).

globals__set_trace_level(Globals, TraceLevel,
	Globals ^ trace_level := TraceLevel).
globals__set_trace_level_none(Globals,
	Globals ^ trace_level := trace_level_none).

globals__lookup_option(Globals, Option, OptionData) :-
	globals__get_options(Globals, OptionTable),
	map__lookup(OptionTable, Option, OptionData).

%-----------------------------------------------------------------------------%

globals__lookup_bool_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = bool(Bool) ->
		Value = Bool
	;
		error("globals__lookup_bool_option: invalid bool option")
	).

globals__lookup_string_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = string(String) ->
		Value = String
	;
		error("globals__lookup_string_option: invalid string option")
	).

globals__lookup_int_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = int(Int) ->
		Value = Int
	;
		error("globals__lookup_int_option: invalid int option")
	).

globals__lookup_accumulating_option(Globals, Option, Value) :-
	globals__lookup_option(Globals, Option, OptionData),
	( OptionData = accumulating(Accumulating) ->
		Value = Accumulating
	;
		error("globals__lookup_accumulating_option: invalid accumulating option")
	).

%-----------------------------------------------------------------------------%

globals__have_static_code_addresses(Globals, IsConst) :-
	globals__get_options(Globals, OptionTable),
	globals__have_static_code_addresses_2(OptionTable, IsConst).

:- pred globals__have_static_code_addresses_2(option_table::in, 
	bool::out) is det.

globals__have_static_code_addresses_2(OptionTable, IsConst) :-
	getopt__lookup_bool_option(OptionTable, gcc_non_local_gotos,
		NonLocalGotos),
	getopt__lookup_bool_option(OptionTable, asm_labels, AsmLabels),
	exprn_aux__imported_is_constant(NonLocalGotos, AsmLabels, IsConst).

globals__want_return_var_layouts(Globals, WantReturnLayouts) :-
	% We need to generate layout info for call return labels
	% if we are using accurate gc or if the user wants uplevel printing.
	(
		(
			globals__get_gc_method(Globals, GC_Method),
			GC_Method = accurate
		;
			globals__get_trace_level(Globals, TraceLevel),
			globals__get_trace_suppress(Globals, TraceSuppress),
			trace_needs_return_info(TraceLevel, TraceSuppress)
				= yes
		)
	->
		WantReturnLayouts = yes
	;
		WantReturnLayouts = no
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

globals__io_init(Options, Target, GC_Method, TagsMethod,
		TerminationNorm, TraceLevel, TraceSuppress) -->
	{ copy(Target, Target1) },
	{ copy(GC_Method, GC_Method1) },
	{ copy(TagsMethod, TagsMethod1) },
	{ copy(TerminationNorm, TerminationNorm1) },
	{ copy(TraceLevel, TraceLevel1) },
	{ copy(TraceSuppress, TraceSuppress1) },
	{ globals__init(Options, Target1, GC_Method1, TagsMethod1,
		TerminationNorm1, TraceLevel1, TraceSuppress1, Globals) },
	globals__io_set_globals(Globals).

globals__io_get_target(Target) -->
	globals__io_get_globals(Globals),
	{ globals__get_target(Globals, Target) }.

globals__io_get_gc_method(GC_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) }.

globals__io_get_tags_method(Tags_Method) -->
	globals__io_get_globals(Globals),
	{ globals__get_tags_method(Globals, Tags_Method) }.

globals__io_get_termination_norm(TerminationNorm) -->
	globals__io_get_globals(Globals),
	{ globals__get_termination_norm(Globals, TerminationNorm) }.

globals__io_get_trace_level(TraceLevel) -->
	globals__io_get_globals(Globals),
	{ globals__get_trace_level(Globals, TraceLevel) }.

globals__io_get_trace_suppress(TraceSuppress) -->
	globals__io_get_globals(Globals),
	{ globals__get_trace_suppress(Globals, TraceSuppress) }.

globals__io_get_globals(Globals) -->
	io__get_globals(UnivGlobals),
	{
		univ_to_type(UnivGlobals, Globals0)
	->
		Globals = Globals0
	;
		error("globals__io_get_globals: univ_to_type failed")
	}.

globals__io_set_globals(Globals) -->
	{ type_to_univ(Globals, UnivGlobals) },
	io__set_globals(UnivGlobals).

%-----------------------------------------------------------------------------%

globals__io_lookup_option(Option, OptionData) -->
	globals__io_get_globals(Globals),
	{ globals__get_options(Globals, OptionTable) },
	{ map__lookup(OptionTable, Option, OptionData) }.

globals__io_set_option(Option, OptionData) -->
	globals__io_get_globals(Globals0),
	{ globals__get_options(Globals0, OptionTable0) },
	{ map__set(OptionTable0, Option, OptionData, OptionTable) },
	{ globals__set_options(Globals0, OptionTable, Globals1) },
		% XXX there is a bit of a design flaw with regard to
		% uniqueness and io__set_globals
	{ unsafe_promise_unique(Globals1, Globals) },
	globals__io_set_globals(Globals).

globals__io_set_trace_level(TraceLevel) -->
	globals__io_get_globals(Globals0),
	{ globals__set_trace_level(Globals0, TraceLevel, Globals1) },
	{ unsafe_promise_unique(Globals1, Globals) },
		% XXX there is a bit of a design flaw with regard to
		% uniqueness and io__set_globals
	globals__io_set_globals(Globals).

	% This predicate is needed because mercury_compile.m doesn't know
	% anything about type trace_level.
globals__io_set_trace_level_none -->
	globals__io_set_trace_level(trace_level_none).

%-----------------------------------------------------------------------------%

globals__io_lookup_foreign_language_option(Option, ForeignLang) -->
	globals__io_lookup_string_option(Option, String),
	{ convert_foreign_language(String, ForeignLang0) ->
		ForeignLang = ForeignLang0
	;
		error("globals__io_lookup_foreign_language_option: invalid foreign_language option")
	}.

globals__io_get_backend_foreign_languages(ForeignLangs) -->
	globals__io_get_globals(Globals),
	{ globals__get_backend_foreign_languages(Globals, ForeignLangs) }.

globals__io_lookup_bool_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, Option, Value) }.

globals__io_lookup_int_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_int_option(Globals, Option, Value) }.

globals__io_lookup_string_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_string_option(Globals, Option, Value) }.

globals__io_lookup_accumulating_option(Option, Value) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_accumulating_option(Globals, Option, Value) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
