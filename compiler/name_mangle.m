%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: name_mangle.m

% This module defines routines for generating and/or outputing identifiers
% for modules, predicates/functions, and procedures in forms that are
% syntactically acceptable in all our target languages, meaning C, Java
% and MSIL.

% Warning: any changes to the name mangling algorithms implemented in this
% module will also require changes to extras/dynamic_linking/name_mangle.m,
% profiler/demangle.m and util/mdemangle.c.

%-----------------------------------------------------------------------------%

:- module backend_libs__name_mangle.
:- interface.

:- import_module backend_libs__proc_label.
:- import_module hlds__hlds_data.
:- import_module parse_tree__prog_data.

:- import_module io, bool, string.

	% Output a proc label.

:- pred output_proc_label(proc_label::in, io::di, io::uo) is det.

	% Output a proc label. The boolean controls whether label_prefixs
	% is added to it.

:- pred output_proc_label(proc_label::in, bool::in, io::di, io::uo) is det.

	% Get a proc label string (used by procs which are exported to C).
	% The boolean controls whether label_prefix is added to the string.

:- func proc_label_to_c_string(proc_label, bool) = string.

	% Mangle an arbitrary name into a C etc identifier

:- func name_mangle(string) = string.

	% Mangle a possibly module-qualified Mercury symbol name
	% into a C identifier.

:- func sym_name_mangle(sym_name) = string.

	% Produces a string of the form Module__Name.

:- func qualify_name(string, string) = string.

	% Create a name for base_typeclass_info.

:- func make_base_typeclass_info_name(class_id, string) = string.

	% Output the name for base_typeclass_info,
	% with the appropriate mercury_data_prefix.

:- pred output_base_typeclass_info_name(class_id::in, string::in,
	io__state::di, io__state::uo) is det.

	% Prints the name of the initialization function
	% for a given module.

:- pred output_init_name(module_name::in, io__state::di, io__state::uo) is det.

	% Returns the name of the initialization function
	% for a given module.

:- func make_init_name(module_name) = string.

	% Returns the name of the Aditi-RL code constant
	% for a given module.

:- func make_rl_data_name(module_name) = string.

	% Print out the name of the tabling variable for the specified
	% procedure.

:- pred output_tabling_pointer_var_name(proc_label::in,
	io__state::di, io__state::uo) is det.

	% To ensure that Mercury labels don't clash with C symbols, we
	% prefix them with `mercury__'.

:- func mercury_label_prefix = string.

	% All the C data structures we generate which are either fully static
	% or static after initialization should have one of these two prefixes,
	% to ensure that Mercury global variables don't clash with C symbols.

:- func mercury_data_prefix = string.
:- func mercury_common_prefix = string.

	% All the C types we generate should have this prefix to ensure
	% that they don't clash with C symbols.

:- func mercury_common_type_prefix = string.

:- implementation.

:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module parse_tree__prog_util.

:- import_module char, int, list, std_util.

%-----------------------------------------------------------------------------%

output_proc_label(ProcLabel, !IO) :-
	output_proc_label(ProcLabel, yes, !IO).

output_proc_label(ProcLabel, AddPrefix, !IO) :-
	ProcLabelString = proc_label_to_c_string(ProcLabel, AddPrefix),
	io__write_string(ProcLabelString, !IO).

proc_label_to_c_string(proc(DefiningModule, PredOrFunc, PredModule,
		PredName, Arity, ModeNum0), AddPrefix) = ProcLabelString :-
	LabelName = make_pred_or_func_name(DefiningModule, PredOrFunc,
		PredModule, PredName, Arity, AddPrefix),
	( PredOrFunc = function ->
		OrigArity = Arity - 1
	;
		OrigArity = Arity
	),
	string__int_to_string(OrigArity, ArityString),
	proc_id_to_int(ModeNum0, ModeInt),
	string__int_to_string(ModeInt, ModeNumString),
	string__append_list([LabelName, "_", ArityString, "_", ModeNumString],
		ProcLabelString).

	% For a special proc, output a label of the form:
	% mercury____<PredName>___<TypeModule>__<TypeName>_<TypeArity>_<Mode>
proc_label_to_c_string(special_proc(Module, SpecialPredId, TypeModule,
		TypeName, TypeArity, ModeNum0), AddPrefix) = ProcLabelString :-
	% figure out the LabelName
	DummyArity = -1,	% not used by make_pred_or_func_name.
	TypeCtor = qualified(TypeModule, TypeName) - TypeArity,
	PredName = special_pred_name(SpecialPredId, TypeCtor),
	LabelName = make_pred_or_func_name(unqualified(""), predicate,
		unqualified(""), PredName, DummyArity, AddPrefix),

	% figure out the ModeNumString
	string__int_to_string(TypeArity, TypeArityString),
	proc_id_to_int(ModeNum0, ModeInt),
	string__int_to_string(ModeInt, ModeNumString),

	% mangle all the relevent names
	MangledModule = sym_name_mangle(Module),
	MangledTypeModule = sym_name_mangle(TypeModule),
	MangledTypeName = name_mangle(TypeName),

	% Module-qualify the type name.
	% To handle locally produced unification preds for imported types,
	% we need to qualify it with both the module name of the
	% type, and also (if it is different) the module name of the
	% current module.
	QualifiedMangledTypeName = qualify_name(MangledTypeModule,
		MangledTypeName),
	FullyQualifiedMangledTypeName = maybe_qualify_name(MangledModule,
		QualifiedMangledTypeName),

	% join it all together
	string__append_list( [LabelName, "_", FullyQualifiedMangledTypeName,
		"_", TypeArityString, "_", ModeNumString],
		ProcLabelString).

	% Make a name identifying a predicate or a function, given the
	% defining module, predicate or function indicator, declaring module,
	% predicate name, arity, and whether or not to add the
	% mercury_label_prefix.

:- func make_pred_or_func_name(module_name, pred_or_func, module_name, string,
	arity, bool) = string.

%
% Warning: any changes to the name mangling algorithm here will also
% require changes to extras/dynamic_linking/name_mangle.m,
% profiler/demangle.m and util/mdemangle.c.
%
make_pred_or_func_name(DefiningModule, PredOrFunc, DeclaringModule,
		Name0, Arity, AddPrefix) = LabelName :-
	DeclaringModuleName = sym_name_mangle(DeclaringModule),
	DefiningModuleName = sym_name_mangle(DefiningModule),
	(
		(
			Name0 = "main",
			Arity = 2
		;
			string__prefix(Name0, "__")
		)
		% The conditions above define which labels are printed without
		% module qualification.
	->
		LabelName0 = Name0
	;
		LabelName0 = qualify_name(DeclaringModuleName, Name0)
	),
	(
		% if this is a specialized version of a predicate
		% defined in some other module, then it needs both
		% module prefixes
		DefiningModule \= DeclaringModule
	->
		string__append_list([DefiningModuleName, "__", LabelName0],
			LabelName1)
	;
		LabelName1 = LabelName0
	),
	LabelName2 = name_mangle(LabelName1),
	(
		PredOrFunc = function,
		string__append("fn__", LabelName2, LabelName3)
	;
		PredOrFunc = predicate,
		LabelName3 = LabelName2
	),
	(
		AddPrefix = yes
	->
		string__append(mercury_label_prefix, LabelName3, LabelName)
	;
		LabelName = LabelName3
	).

%
% Warning: any changes to the name mangling algorithm here will also
% require changes to extras/dynamic_linking/name_mangle.m,
% profiler/demangle.m and util/mdemangle.c.
%

sym_name_mangle(unqualified(Name)) =
	name_mangle(Name).
sym_name_mangle(qualified(ModuleName, PlainName)) = MangledName :-
	MangledModuleName = sym_name_mangle(ModuleName),
	MangledPlainName = name_mangle(PlainName),
	MangledName = qualify_name(MangledModuleName, MangledPlainName).

	% Convert a Mercury predicate name into something that can form
	% part of a C identifier.  This predicate is necessary because
	% quoted names such as 'name with embedded spaces' are valid
	% predicate names in Mercury.

name_mangle(Name) = MangledName :-
	( string__is_alnum_or_underscore(Name) ->
		% any names that start with `f_' are changed so that
		% they start with `f__', so that we can use names starting
		% with `f_' (followed by anything except an underscore)
		% without fear of name collisions
		( string__append("f_", Suffix, Name) ->
			string__append("f__", Suffix, MangledName)
		;
			MangledName = Name
		)
	;
		MangledName = convert_to_valid_c_identifier(Name)
	).

:- func convert_to_valid_c_identifier(string) = string.

convert_to_valid_c_identifier(String) = Name :-
	( name_conversion_table(String, Name0) ->
		Name = Name0
	;
		Name0 = convert_to_valid_c_identifier_2(String),
		string__append("f", Name0, Name)
	).

qualify_name(Module0, Name0) = Name :-
	string__append_list([Module0, "__", Name0], Name).

	% Produces a string of the form Module__Name, unless Module__
	% is already a prefix of Name.

:- func maybe_qualify_name(string, string) = string.

maybe_qualify_name(Module0, Name0) = Name :-
	string__append(Module0, "__", UnderscoresModule),
	( string__append(UnderscoresModule, _, Name0) ->
		Name = Name0
	;
		string__append(UnderscoresModule, Name0, Name)
	).

	% A table used to convert Mercury functors into
	% C identifiers.  Feel free to add any new translations you want.
	% The C identifiers should start with "f_",
	% to avoid introducing name clashes.
	% If the functor name is not found in the table, then
	% we use a fall-back method which produces ugly names.

:- pred name_conversion_table(string, string).
:- mode name_conversion_table(in, out) is semidet.

name_conversion_table("\\=", "f_not_equal").
name_conversion_table(">=", "f_greater_or_equal").
name_conversion_table("=<", "f_less_or_equal").
name_conversion_table("=", "f_equal").
name_conversion_table("<", "f_less_than").
name_conversion_table(">", "f_greater_than").
name_conversion_table("-", "f_minus").
name_conversion_table("+", "f_plus").
name_conversion_table("*", "f_times").
name_conversion_table("/", "f_slash").
name_conversion_table(",", "f_comma").
name_conversion_table(";", "f_semicolon").
name_conversion_table("!", "f_cut").
name_conversion_table("{}", "f_tuple").
name_conversion_table("[|]", "f_cons").
name_conversion_table("[]", "f_nil").

	% This is the fall-back method.
	% Given a string, produce a C identifier
	% for that string by concatenating the decimal
	% expansions of the character codes in the string,
	% separated by undellines.
	% The C identifier will start with "f_"; this predicate
	% constructs everything except the initial "f".
	%
	% For example, given the input "\n\t" we return "_10_8".

:- func convert_to_valid_c_identifier_2(string) = string.

convert_to_valid_c_identifier_2(String) = Name :-
	( string__first_char(String, Char, Rest) ->
		% XXX This will cause ABI incompatibilities between
		%     compilers which are built in grades that have
		%     different character representations.
		char__to_int(Char, Code),
		string__int_to_string(Code, CodeString),
		string__append("_", CodeString, ThisCharString),
		Name0 = convert_to_valid_c_identifier_2(Rest),
		string__append(ThisCharString, Name0, Name)
	;
		% String is the empty string
		Name = String
	).

%-----------------------------------------------------------------------------%

output_base_typeclass_info_name(ClassId, TypeNames, !IO) :-
	Str = make_base_typeclass_info_name(ClassId, TypeNames),
	io__write_string(mercury_data_prefix, !IO),
	io__write_string("__", !IO),
	io__write_string(Str, !IO).

make_base_typeclass_info_name(class_id(ClassSym, ClassArity), TypeNames)
		= Str :-
	MangledClassString = sym_name_mangle(ClassSym),
	string__int_to_string(ClassArity, ArityString),
	MangledTypeNames = name_mangle(TypeNames),
	string__append_list(["base_typeclass_info_", MangledClassString,
		"__arity", ArityString, "__", MangledTypeNames], Str).

%-----------------------------------------------------------------------------%

output_init_name(ModuleName, !IO) :-
	InitName = make_init_name(ModuleName),
	io__write_string(InitName, !IO).

make_init_name(ModuleName) = InitName :-
	MangledModuleName = sym_name_mangle(ModuleName),
	string__append_list(["mercury__", MangledModuleName, "__"], InitName).

make_rl_data_name(ModuleName) = RLDataConstName :-
	MangledModuleName = sym_name_mangle(ModuleName),
	string__append("mercury__aditi_rl_data__", MangledModuleName,
		RLDataConstName).

output_tabling_pointer_var_name(ProcLabel, !IO) :-
	io__write_string("mercury_var__table_root__", !IO),
	output_proc_label(ProcLabel, !IO).

%-----------------------------------------------------------------------------%

mercury_label_prefix = "mercury__".

mercury_data_prefix = "mercury_data_".

mercury_common_prefix = "mercury_common_".

mercury_common_type_prefix = "mercury_type_".

%-----------------------------------------------------------------------------%
