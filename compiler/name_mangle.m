%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: name_mangle.m

% This module defines routines for generating and/or outputing identifiers
% for modules, predicates/functions, and procedures in forms that are
% syntactically acceptable in all our target languages, meaning C, Java
% and MSIL.

% NOTE: some parts of the name mangling routines are defined in 
% prog_foreign.m because they are required by the frontend of the compiler,
% for generating makefile fragments etc.

% Warning: any changes to the name mangling algorithms implemented in this
% module may also require changes to extras/dynamic_linking/name_mangle.m,
% profiler/demangle.m, util/mdemangle.c and compiler/prog_foreign.m.

%-----------------------------------------------------------------------------%

:- module backend_libs__name_mangle.
:- interface.

:- import_module backend_libs__rtti.
:- import_module mdbcomp__prim_data.

:- import_module bool.
:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------------%

	% Output a proc label.
	%
:- pred output_proc_label(proc_label::in, io::di, io::uo) is det.

	% Output a proc label. The boolean controls whether
	% mercury_label_prefix is added to it.
	%
:- pred output_proc_label(proc_label::in, bool::in, io::di, io::uo) is det.

	% Get a proc label string (used by procs which are exported to C).
	% The boolean controls whether label_prefix is added to the string.
	%
:- func proc_label_to_c_string(proc_label, bool) = string.

	% Succeed iff the given name or sym_name doesn't need mangling.
	%
:- pred name_doesnt_need_mangling(string::in) is semidet.
:- pred sym_name_doesnt_need_mangling(sym_name::in) is semidet.

	% Create a name for base_typeclass_info.
	%
:- func make_base_typeclass_info_name(tc_name, string) = string.

	% Output the name for base_typeclass_info,
	% with the appropriate mercury_data_prefix.
	%
:- pred output_base_typeclass_info_name(tc_name::in, string::in,
	io::di, io::uo) is det.

	% Prints the name of the initialization function
	% for a given module.
	%
:- pred output_init_name(module_name::in, io::di, io::uo) is det.

	% Print out the name of the tabling variable for the specified
	% procedure.
	%
:- pred output_tabling_pointer_var_name(proc_label::in, io::di, io::uo) is det.

	% To ensure that Mercury labels don't clash with C symbols, we
	% prefix them with `mercury__'.
	%
:- func mercury_label_prefix = string.

	% All the C data structures we generate which are either fully static
	% or static after initialization should have one of these two prefixes,
	% to ensure that Mercury global variables don't clash with C symbols.
	%
:- func mercury_data_prefix = string.
:- func mercury_common_prefix = string.

	% All the C types we generate should have this prefix to ensure
	% that they don't clash with C symbols.
	%
:- func mercury_common_type_prefix = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_util.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

output_proc_label(ProcLabel, !IO) :-
	output_proc_label(ProcLabel, yes, !IO).

output_proc_label(ProcLabel, AddPrefix, !IO) :-
	ProcLabelString = proc_label_to_c_string(ProcLabel, AddPrefix),
	io__write_string(ProcLabelString, !IO).

proc_label_to_c_string(proc(DefiningModule, PredOrFunc, PredModule,
		PredName, Arity, ModeInt), AddPrefix) = ProcLabelString :-
	LabelName = make_pred_or_func_name(DefiningModule, PredOrFunc,
		PredModule, PredName, Arity, AddPrefix),
	( PredOrFunc = function ->
		OrigArity = Arity - 1
	;
		OrigArity = Arity
	),
	string__int_to_string(OrigArity, ArityString),
	string__int_to_string(ModeInt, ModeNumString),
	string__append_list([LabelName, "_", ArityString, "_", ModeNumString],
		ProcLabelString).

	% For a special proc, output a label of the form:
	% mercury____<PredName>___<TypeModule>__<TypeName>_<TypeArity>_<Mode>
	%
proc_label_to_c_string(special_proc(Module, SpecialPredId, TypeModule,
		TypeName, TypeArity, ModeInt), AddPrefix) = ProcLabelString :-
	% figure out the LabelName
	DummyArity = -1,	% not used by make_pred_or_func_name.
	TypeCtor = qualified(TypeModule, TypeName) - TypeArity,
	PredName = special_pred_name(SpecialPredId, TypeCtor),
	LabelName = make_pred_or_func_name(unqualified(""), predicate,
		unqualified(""), PredName, DummyArity, AddPrefix),

	% figure out the ModeNumString
	string__int_to_string(TypeArity, TypeArityString),
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
	string__append_list([LabelName, "_", FullyQualifiedMangledTypeName,
		"_", TypeArityString, "_", ModeNumString],
		ProcLabelString).

	% Make a name identifying a predicate or a function, given the
	% defining module, predicate or function indicator, declaring module,
	% predicate name, arity, and whether or not to add the
	% mercury_label_prefix.

:- func make_pred_or_func_name(module_name, pred_or_func, module_name, string,
	arity, bool) = string.

%
% Warning: any changes to the name mangling algorithm here may also
% require changes to extras/dynamic_linking/name_mangle.m, profiler/demangle.m,
% util/mdemangle.c and compiler/prog_foreign.m.
%
make_pred_or_func_name(DefiningModule, PredOrFunc, DeclaringModule,
		Name0, Arity, AddPrefix) = LabelName :-
	DeclaringModuleName = sym_name_mangle(DeclaringModule),
	DefiningModuleName = sym_name_mangle(DefiningModule),
	( dont_module_qualify_name(Name0, Arity) ->
		LabelName0 = Name0
	;
		LabelName0 = qualify_name(DeclaringModuleName, Name0)
	),
	% If this is a specialized version of a predicate defined
	% in some other module, then it needs both module prefixes.
	( DefiningModule \= DeclaringModule ->
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
	( AddPrefix = yes ->
		string__append(mercury_label_prefix, LabelName3, LabelName)
	;
		LabelName = LabelName3
	).

	% Define the conditions for which labels are printed
	% without module qualification.
:- pred dont_module_qualify_name(string::in, arity::in) is semidet.

dont_module_qualify_name(Name, Arity) :-
	(
		Name = "main",
		Arity = 2
	;
		string__prefix(Name, "__")
	).

	% Convert a Mercury predicate name into something that can form
	% part of a C identifier.  This predicate is necessary because
	% quoted names such as 'name with embedded spaces' are valid
	% predicate names in Mercury.

name_doesnt_need_mangling(Name) :-
	string__is_alnum_or_underscore(Name),
	\+ string__append("f_", _Suffix, Name).

sym_name_doesnt_need_mangling(unqualified(Name)) :-
	name_doesnt_need_mangling(Name).
sym_name_doesnt_need_mangling(qualified(ModuleName, PlainName)) :-
	sym_name_doesnt_need_mangling(ModuleName),
	name_doesnt_need_mangling(PlainName).

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

%-----------------------------------------------------------------------------%

output_base_typeclass_info_name(TCName, TypeNames, !IO) :-
	Str = make_base_typeclass_info_name(TCName, TypeNames),
	io__write_string(mercury_data_prefix, !IO),
	io__write_string("__", !IO),
	io__write_string(Str, !IO).

make_base_typeclass_info_name(TCName, TypeNames) = Str :-
	TCName = tc_name(ModuleName, ClassName, ClassArity),
	ClassSym = qualified(ModuleName, ClassName),
	MangledClassString = sym_name_mangle(ClassSym),
	string__int_to_string(ClassArity, ArityString),
	MangledTypeNames = name_mangle(TypeNames),
	string__append_list(["base_typeclass_info_", MangledClassString,
		"__arity", ArityString, "__", MangledTypeNames], Str).

%-----------------------------------------------------------------------------%

output_init_name(ModuleName, !IO) :-
	InitName = make_init_name(ModuleName),
	io__write_string(InitName, !IO).

output_tabling_pointer_var_name(ProcLabel, !IO) :-
	io__write_string("mercury_var__table_root__", !IO),
	output_proc_label(ProcLabel, !IO).

%-----------------------------------------------------------------------------%

mercury_label_prefix = "mercury__".

mercury_data_prefix = "mercury_data_".

mercury_common_prefix = "mercury_common_".

mercury_common_type_prefix = "mercury_type_".

%-----------------------------------------------------------------------------%
:- end_module name_mangle.
%-----------------------------------------------------------------------------%
