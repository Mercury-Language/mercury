%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines predicates for interfacing with foreign languages.
% In particular, this module supports interfacing with languages
% other than the target of compilation.

% Main authors: trd, dgj.
% Parts of this code were originally written by dgj, and have since been
% moved here.

%-----------------------------------------------------------------------------%

:- module backend_libs__foreign.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool, list, std_util, string, term.

:- type foreign_decl_info 		== list(foreign_decl_code).
					% in reverse order
:- type foreign_body_info		== list(foreign_body_code).
					% in reverse order

:- type foreign_decl_code
	--->	foreign_decl_code(
			foreign_language,
			foreign_decl_is_local,
			string,
			prog_context
		).

:- type foreign_body_code
	--->	foreign_body_code(
			foreign_language,
			string,
			prog_context
		).

:- type foreign_export_defns == list(foreign_export).
:- type foreign_export_decls
	--->	foreign_export_decls(
			foreign_decl_info,
			list(foreign_export_decl)
		).

:- type foreign_export_decl
	--->	foreign_export_decl(
			foreign_language,	% language of the export
			string,			% return type
			string,			% function name
			string			% argument declarations
		).

	% Some code from a `pragma foreign_code' declaration that is not
	% associated with a given procedure.
:- type user_foreign_code
	--->	user_foreign_code(
			foreign_language,	% language of this code
			string,			% code
			term__context		% source code location
		).

	% the code for `pragma export' is generated directly as strings
	% by export.m.
:- type foreign_export	==	string.

	% A type which is used to determine the string representation of a
	% mercury type for various foreign languages.
:- type exported_type.

	% Given a type which is not defined as a foreign type, get the
	% exported_type representation of that type.
:- func non_foreign_type((type)) = exported_type.

	% Does the foreign_type_body contain a definition usable
	% when compiling to the given target.
:- pred have_foreign_type_for_backend(compilation_target::in,
	foreign_type_body::in, bool::out) is det.

	% Given an arbitary mercury type, get the exported_type representation
	% of that type on the current backend.
:- func to_exported_type(module_info, (type)) = exported_type.

	% Does the implementation of the given foreign type body on
	% the current backend use a user-defined comparison predicate.
:- func foreign_type_body_has_user_defined_eq_comp_pred(module_info,
	foreign_type_body) = unify_compare is semidet.

	% Find the current target backend from the module_info, and given
	% a foreign_type_body, return the name of the foreign language type
	% the identity of any user-defined unify/compare predicates, and the
	% assertions applicable to that backend.
:- pred foreign_type_body_to_exported_type(module_info::in,
	foreign_type_body::in, sym_name::out, maybe(unify_compare)::out,
	list(foreign_type_assertion)::out) is det.

	% Given the exported_type representation for a type, determine
	% whether or not it is a foreign type, and if yes, return the foreign
	% type's assertions.
:- func is_foreign_type(exported_type) = maybe(list(foreign_type_assertion)).

	% Given a representation of a type, determine the string which
	% corresponds to that type in the specified foreign language,
	% for use with foreign language interfacing (`pragma export' or
	% `pragma foreign_proc').
:- func to_type_string(foreign_language, exported_type) = string.
:- func to_type_string(foreign_language, module_info, (type)) = string.

	% Filter the decls for the given foreign language.
	% The first return value is the list of matches, the second is
	% the list of mis-matches.
:- pred filter_decls(foreign_language::in, foreign_decl_info::in,
	foreign_decl_info::out, foreign_decl_info::out) is det.

	% Filter the module imports for the given foreign language.
	% The first return value is the list of matches, the second is
	% the list of mis-matches.
:- pred filter_imports(foreign_language::in,
	foreign_import_module_info::in, foreign_import_module_info::out,
	foreign_import_module_info::out) is det.

	% Filter the bodys for the given foreign language.
	% The first return value is the list of matches, the second is
	% the list of mis-matches.
:- pred filter_bodys(foreign_language::in, foreign_body_info::in,
	foreign_body_info::out, foreign_body_info::out) is det.

	% Given some foreign code, generate some suitable proxy code for
	% calling the code via one of the given languages.
	% This might mean, for example, generating a call to a
	% forwarding function in C.
	% The foreign language argument specifies which language is the
	% target language, the other inputs are the name, types, input
	% variables and so on for a piece of pragma foreign code.
	% The outputs are the new attributes and implementation for this
	% code.
	% XXX This implementation is currently incomplete, so in future
	% this interface may change.
:- pred extrude_pragma_implementation(list(foreign_language)::in,
	list(pragma_var)::in, sym_name::in, pred_or_func::in, prog_context::in,
	module_info::in, module_info::out,
	pragma_foreign_proc_attributes::in, pragma_foreign_proc_attributes::out,
	pragma_foreign_code_impl::in, pragma_foreign_code_impl::out) is det.

	% make_pragma_import turns pragma imports into pragma foreign_code.
	% Given the pred and proc info for this predicate, the name
	% of the function to import, the context of the import pragma
	% and the module_info, create a pragma_foreign_code_impl
	% which imports the foreign function, and return the varset,
	% pragma_vars, argument types and other information about the
	% generated predicate body.
:- pred make_pragma_import(pred_info::in, proc_info::in, string::in,
	prog_context::in, module_info::in, pragma_foreign_code_impl::out,
	prog_varset::out, list(pragma_var)::out, list(type)::out, arity::out,
	pred_or_func::out) is det.

	% The name of the #define which can be used to guard declarations with
	% to prevent entities being declared twice.
:- func decl_guard(sym_name) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__name_mangle.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module parse_tree__error_util.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module list, map, assoc_list, std_util, string, varset, int, term.
:- import_module require.

filter_decls(WantedLang, Decls0, LangDecls, NotLangDecls) :-
	list__filter((pred(foreign_decl_code(Lang, _, _, _)::in) is semidet :-
			WantedLang = Lang),
		Decls0, LangDecls, NotLangDecls).

filter_imports(WantedLang, Imports0, LangImports, NotLangImports) :-
	list__filter(
		(pred(foreign_import_module(Lang, _, _)::in) is semidet :-
			WantedLang = Lang),
		Imports0, LangImports, NotLangImports).

filter_bodys(WantedLang, Bodys0, LangBodys, NotLangBodys) :-
	list__filter((pred(foreign_body_code(Lang, _, _)::in) is semidet :-
			WantedLang = Lang),
		Bodys0, LangBodys, NotLangBodys).

extrude_pragma_implementation([], _PragmaVars,
		_PredName, _PredOrFunc, _Context,
		!ModuleInfo, !NewAttributes, !Impl) :-
	unexpected(this_file, "no suitable target languages available").

	% We just use the first target language for now, it might be nice
	% to try a few others if the backend supports multiple ones.
extrude_pragma_implementation([TargetLang | TargetLangs],
		_PragmaVars, _PredName, _PredOrFunc, _Context,
		!ModuleInfo, !Attributes, !Impl) :-
	ForeignLanguage = foreign_language(!.Attributes),

		% If the foreign language is available as a target language,
		% we don't need to do anything.
	( list__member(ForeignLanguage, [TargetLang | TargetLangs]) ->
		true
	;
		set_foreign_language(TargetLang, !Attributes),
		extrude_pragma_implementation_2(TargetLang, ForeignLanguage,
			!ModuleInfo, !Impl)
	).

:- pred extrude_pragma_implementation_2(
	foreign_language::in, foreign_language::in,
	module_info::in, module_info::out,
	pragma_foreign_code_impl::in, pragma_foreign_code_impl::out) is det.

	% This isn't finished yet, and we probably won't implement it for C
	% calling MC++.  For C calling normal C++ we would generate a proxy
	% function in C++ (implemented in a piece of C++ body code) with C
	% linkage, and import that function.  The backend would spit the C++
	% body code into a separate file.
	% The code would look a little like this:
	/*
	NewName = make_pred_name(ForeignLanguage, PredName),
	( PredOrFunc = predicate ->
		ReturnCode = ""
	;
		ReturnCode = "ReturnVal = "
	),
	C_ExtraCode = "Some Extra Code To Run",
	create_pragma_import_c_code(PragmaVars, ModuleInfo0, "", VarString),
	module_add_foreign_body_code(cplusplus,
		C_ExtraCode, Context, ModuleInfo0, ModuleInfo),
	Impl = import(NewName, ReturnCode, VarString, no)
	*/

extrude_pragma_implementation_2(c, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(c, managed_cplusplus).

extrude_pragma_implementation_2(c, csharp, _, _, _, _) :-
	unimplemented_combination(c, csharp).

extrude_pragma_implementation_2(c, il, _, _, _, _) :-
	unimplemented_combination(c, il).

extrude_pragma_implementation_2(c, java, _, _, _, _) :-
	unimplemented_combination(c, java).

extrude_pragma_implementation_2(c, c, !ModuleInfo, !Impl).

		% Don't do anything - C and MC++ are embedded inside MC++
		% without any changes.
extrude_pragma_implementation_2(managed_cplusplus, managed_cplusplus,
	!ModuleInfo, !Impl).

extrude_pragma_implementation_2(managed_cplusplus, c, !ModuleInfo, !Impl).

extrude_pragma_implementation_2(managed_cplusplus, csharp, _, _, _, _) :-
	unimplemented_combination(managed_cplusplus, csharp).

extrude_pragma_implementation_2(managed_cplusplus, il, _, _, _, _) :-
	unimplemented_combination(managed_cplusplus, il).

extrude_pragma_implementation_2(managed_cplusplus, java, _, _, _, _) :-
	unimplemented_combination(managed_cplusplus, java).

extrude_pragma_implementation_2(csharp, csharp, !ModuleInfo, !Impl).

extrude_pragma_implementation_2(csharp, c, _, _, _, _) :-
	unimplemented_combination(csharp, c).

extrude_pragma_implementation_2(csharp, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(csharp, managed_cplusplus).

extrude_pragma_implementation_2(csharp, il, _, _, _, _) :-
	unimplemented_combination(csharp, il).

extrude_pragma_implementation_2(csharp, java, _, _, _, _) :-
	unimplemented_combination(csharp, java).

extrude_pragma_implementation_2(il, il, !ModuleInfo, !Impl).

extrude_pragma_implementation_2(il, c, _, _, _, _) :-
	unimplemented_combination(il, c).

extrude_pragma_implementation_2(il, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(il, managed_cplusplus).

extrude_pragma_implementation_2(il, csharp, _, _, _, _) :-
	unimplemented_combination(il, csharp).

extrude_pragma_implementation_2(il, java, _, _, _, _) :-
	unimplemented_combination(il, java).

extrude_pragma_implementation_2(java, java,
	!ModuleInfo, !Impl).

extrude_pragma_implementation_2(java, c, _, _, _, _) :-
	unimplemented_combination(java, c).

extrude_pragma_implementation_2(java, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(java, managed_cplusplus).

extrude_pragma_implementation_2(java, csharp, _, _, _, _) :-
	unimplemented_combination(java, csharp).

extrude_pragma_implementation_2(java, il, _, _, _, _) :-
	unimplemented_combination(java, il).

:- pred unimplemented_combination(foreign_language::in, foreign_language::in)
	is erroneous.

unimplemented_combination(Lang1, Lang2) :-
	error("unimplemented: calling " ++ foreign_language_string(Lang2)
		++ " foreign code from " ++ foreign_language_string(Lang1)).

	% XXX we haven't implemented these functions yet.
	% What is here is only a guide
:- func make_pred_name(foreign_language, sym_name) = string.

make_pred_name(Lang, SymName) =
	"mercury_" ++ simple_foreign_language_string(Lang) ++ "__" ++
		make_pred_name_rest(Lang, SymName).

:- func make_pred_name_rest(foreign_language, sym_name) = string.

make_pred_name_rest(c, _SymName) = "some_c_name".
make_pred_name_rest(managed_cplusplus, qualified(ModuleSpec, Name)) =
	make_pred_name_rest(managed_cplusplus, ModuleSpec) ++ "__" ++ Name.
make_pred_name_rest(managed_cplusplus, unqualified(Name)) = Name.
make_pred_name_rest(csharp, _SymName) = "some_csharp_name".
make_pred_name_rest(il, _SymName) = "some_il_name".
make_pred_name_rest(java, _SymName) = "some_java_name".

make_pragma_import(PredInfo, ProcInfo, C_Function, Context,
		ModuleInfo, PragmaImpl, VarSet, PragmaVars, ArgTypes,
		Arity, PredOrFunc) :-
	%
	% lookup some information we need from the pred_info and proc_info
	%
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_interface_code_model(ProcInfo, CodeModel),

	%
	% Build a list of argument variables, together with their
	% names, modes, and types.
	%
	varset__init(VarSet0),
	list__length(Modes, Arity),
	varset__new_vars(VarSet0, Arity, Vars, VarSet),
	create_pragma_vars(Vars, Modes, 0, PragmaVars),
	assoc_list__from_corresponding_lists(PragmaVars, ArgTypes,
			PragmaVarsAndTypes),

	%
	% Construct parts of the C_code string for calling a C_function.
	% This C code fragment invokes the specified C function
	% with the appropriate arguments from the list constructed
	% above, passed in the appropriate manner (by value, or by
	% passing the address to simulate pass-by-reference), and
	% assigns the return value (if any) to the appropriate place.
	% As this phase occurs before polymorphism, we don't know about
	% the type-infos yet.  polymorphism.m is responsible for adding
	% the type-info arguments to the list of variables.
	%
	handle_return_value(CodeModel, PredOrFunc, PragmaVarsAndTypes,
			ModuleInfo, ArgPragmaVarsAndTypes, Return),
	assoc_list__keys(ArgPragmaVarsAndTypes, ArgPragmaVars),
	create_pragma_import_c_code(ArgPragmaVars, ModuleInfo,
			"", Variables),

	%
	% Make an import implementation
	%
	PragmaImpl = import(C_Function, Return, Variables, yes(Context)).

%
% handle_return_value(CodeModel, PredOrFunc, Args0, M, Args, C_Code0):
%	Figures out what to do with the C function's return value,
%	based on Mercury procedure's code model, whether it is a predicate
%	or a function, and (if it is a function) the type and mode of the
%	function result.  Constructs a C code fragment `C_Code0' which
%	is a string of the form "<Something> =" that assigns the return
%	value to the appropriate place, if there is a return value,
%	or is an empty string, if there is no return value.
%	Returns in Args all of Args0 that must be passed as arguments
%	(i.e. all of them, or all of them except the return value).
%
:- pred handle_return_value(code_model::in, pred_or_func::in,
	assoc_list(pragma_var, type)::in, module_info::in,
	assoc_list(pragma_var, type)::out, string::out) is det.

handle_return_value(CodeModel, PredOrFunc, Args0, ModuleInfo, Args, C_Code0) :-
	( CodeModel = model_det,
		(
			PredOrFunc = function,
			pred_args_to_func_args(Args0, Args1, RetArg),
			RetArg = pragma_var(_, RetArgName, RetMode) - RetType,
			mode_to_arg_mode(ModuleInfo, RetMode, RetType,
				RetArgMode),
			RetArgMode = top_out,
			\+ type_util__is_dummy_argument_type(RetType)
		->
			string__append(RetArgName, " = ", C_Code0),
			Args2 = Args1
		;
			C_Code0 = "",
			Args2 = Args0
		)
	; CodeModel = model_semi,
		% we treat semidet functions the same as semidet predicates,
		% which means that for Mercury functions the Mercury return
		% value becomes the last argument, and the C return value
		% is a bool that is used to indicate success or failure.
		C_Code0 = "SUCCESS_INDICATOR = ",
		Args2 = Args0
	; CodeModel = model_non,
		% XXX we should report an error here, rather than generating
		% C code with `#error'...
		C_Code0 = "\n#error ""cannot import nondet procedure""\n",
		Args2 = Args0
	),
	list__filter(include_import_arg(ModuleInfo), Args2, Args).

%
% include_import_arg(M, Arg):
%	Succeeds iff Arg should be included in the arguments of the C
%	function.  Fails if `Arg' has a type such as `io__state' that
%	is just a dummy argument that should not be passed to C.
%
:- pred include_import_arg(module_info::in, pair(pragma_var, type)::in)
	is semidet.

include_import_arg(ModuleInfo, pragma_var(_Var, _Name, Mode) - Type) :-
	mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
	ArgMode \= top_unused,
	\+ type_util__is_dummy_argument_type(Type).

%
% create_pragma_vars(Vars, Modes, ArgNum0, PragmaVars):
%	given list of vars and modes, and an initial argument number,
%	allocate names to all the variables, and
%	construct a single list containing the variables, names, and modes.
%
:- pred create_pragma_vars(list(prog_var)::in, list(mode)::in, int::in,
	list(pragma_var)::out) is det.

create_pragma_vars([], [], _Num, []).
create_pragma_vars([Var|Vars], [Mode|Modes], ArgNum0,
		[PragmaVar | PragmaVars]) :-
	%
	% Figure out a name for the C variable which will hold this argument
	%
	ArgNum = ArgNum0 + 1,
	string__int_to_string(ArgNum, ArgNumString),
	string__append("Arg", ArgNumString, ArgName),
	PragmaVar = pragma_var(Var, ArgName, Mode),
	create_pragma_vars(Vars, Modes, ArgNum, PragmaVars).
create_pragma_vars([_|_], [], _, _) :-
	error("create_pragma_vars: length mis-match").
create_pragma_vars([], [_|_], _, _) :-
	error("create_pragma_vars: length mis-match").

%
% create_pragma_import_c_code(PragmaVars, M, C_Code0, C_Code):
%	This predicate creates the C code fragments for each argument
%	in PragmaVars, and appends them to C_Code0, returning C_Code.
%
:- pred create_pragma_import_c_code(list(pragma_var)::in, module_info::in,
	string::in, string::out) is det.

create_pragma_import_c_code([], _ModuleInfo, C_Code, C_Code).
create_pragma_import_c_code([PragmaVar | PragmaVars], ModuleInfo,
		C_Code0, C_Code) :-
	PragmaVar = pragma_var(_Var, ArgName, Mode),

	%
	% Construct the C code fragment for passing this argument,
	% and append it to C_Code0.
	% Note that C handles output arguments by passing the variable'
	% address, so if the mode is output, we need to put an `&' before
	% the variable name.
	%
	( mode_is_output(ModuleInfo, Mode) ->
		string__append(C_Code0, "&", C_Code1)
	;
		C_Code1 = C_Code0
	),
	string__append(C_Code1, ArgName, C_Code2),
	( PragmaVars \= [] ->
		string__append(C_Code2, ", ", C_Code3)
	;
		C_Code3 = C_Code2
	),

	create_pragma_import_c_code(PragmaVars, ModuleInfo, C_Code3, C_Code).

%-----------------------------------------------------------------------------%

have_foreign_type_for_backend(c, ForeignTypeBody,
		( ForeignTypeBody ^ c = yes(_) -> yes ; no )).
have_foreign_type_for_backend(il, ForeignTypeBody,
		( ForeignTypeBody ^ il = yes(_) -> yes ; no )).
have_foreign_type_for_backend(java, ForeignTypeBody,
		( ForeignTypeBody ^ java = yes(_) -> yes ; no )).
have_foreign_type_for_backend(asm, ForeignTypeBody, Result) :-
	have_foreign_type_for_backend(c, ForeignTypeBody, Result).

:- type exported_type
	--->	foreign(sym_name, list(foreign_type_assertion))
					% A type defined by a pragma
					% foreign_type, and the assertions
					% on that foreign_type.
	;	mercury((type)).	% Any other mercury type.

non_foreign_type(Type) = mercury(Type).

to_exported_type(ModuleInfo, Type) = ExportType :-
	module_info_types(ModuleInfo, Types),
	(
		type_to_ctor_and_args(Type, TypeCtor, _),
		map__search(Types, TypeCtor, TypeDefn)
	->
		hlds_data__get_type_defn_body(TypeDefn, Body),
		( Body = foreign_type(ForeignTypeBody) ->
			foreign_type_body_to_exported_type(ModuleInfo,
				ForeignTypeBody, ForeignTypeName, _,
				Assertions),
			ExportType = foreign(ForeignTypeName, Assertions)
		;
			ExportType = mercury(Type)
		)
	;
		ExportType = mercury(Type)
	).

foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo, Body) =
		UserEqComp :-
	foreign_type_body_to_exported_type(ModuleInfo, Body, _,
		MaybeUserEqComp, _),
	MaybeUserEqComp = yes(UserEqComp).

foreign_type_body_to_exported_type(ModuleInfo, ForeignTypeBody, Name,
		MaybeUserEqComp, Assertions) :-
	ForeignTypeBody = foreign_type_body(MaybeIL, MaybeC, MaybeJava),
	module_info_globals(ModuleInfo, Globals),
	globals__get_target(Globals, Target),
	(
		Target = c,
		(
			MaybeC = yes(Data),
			Data = foreign_type_lang_data(c(NameStr),
				MaybeUserEqComp, Assertions),
			Name = unqualified(NameStr)
		;
			MaybeC = no,
			unexpected(this_file, "to_exported_type: no C type")
		)
	;
		Target = il,
		(
			MaybeIL = yes(Data),
			Data = foreign_type_lang_data(il(_, _, Name),
				MaybeUserEqComp, Assertions)
		;
			MaybeIL = no,
			unexpected(this_file, "to_exported_type: no IL type")
		)
	;
		Target = java,
		(
			MaybeJava = yes(Data),
			Data = foreign_type_lang_data(java(NameStr),
				MaybeUserEqComp, Assertions),
			Name = unqualified(NameStr)
		;
			MaybeJava = no,
			unexpected(this_file,
				"to_exported_type: no Java type")
		)
	;
		Target = asm,
		(
			MaybeC = yes(Data),
			Data = foreign_type_lang_data(c(NameStr),
				MaybeUserEqComp, Assertions),
			Name = unqualified(NameStr)
		;
			MaybeC = no,
			unexpected(this_file, "to_exported_type: no C type")
		)
	).

is_foreign_type(foreign(_, Assertions)) = yes(Assertions).
is_foreign_type(mercury(_)) = no.

to_type_string(Lang, ModuleInfo, Type) =
	to_type_string(Lang, to_exported_type(ModuleInfo, Type)).

to_type_string(c, foreign(ForeignType, _)) = Result :-
	( ForeignType = unqualified(Result0) ->
		Result = Result0
	;
		unexpected(this_file, "to_type_string: qualified C type")
	).
to_type_string(csharp, foreign(ForeignType, _)) = Result :-
	sym_name_to_string(ForeignType, ".", Result).
to_type_string(managed_cplusplus, foreign(ForeignType, _)) = Result ++ " *" :-
	sym_name_to_string(ForeignType, "::", Result).
to_type_string(il, foreign(ForeignType, _)) = Result :-
	sym_name_to_string(ForeignType, ".", Result).
to_type_string(java, foreign(ForeignType, _)) = Result :-
	sym_name_to_string(ForeignType, ".", Result).

	% XXX does this do the right thing for high level data?
to_type_string(c, mercury(Type)) = Result :-
	( Type = term__functor(term__atom("int"), [], _) ->
		Result = "MR_Integer"
	; Type = term__functor(term__atom("float"), [], _) ->
		Result = "MR_Float"
	; Type = term__functor(term__atom("string"), [], _) ->
		Result = "MR_String"
	; Type = term__functor(term__atom("character"), [], _) ->
		Result = "MR_Char"
	;
		Result = "MR_Word"
	).
to_type_string(csharp, mercury(_Type)) = _ :-
	sorry(this_file, "to_type_string for csharp").
to_type_string(managed_cplusplus, mercury(Type)) = TypeString :-
	(
		prog_type__var(Type, _)
	->
		TypeString = "MR_Box"
	;
		TypeString = to_type_string(c, mercury(Type))
	).
to_type_string(il, mercury(_Type)) = _ :-
	sorry(this_file, "to_type_string for il").
to_type_string(java, mercury(Type)) = Result :-
	( Type = term__functor(term__atom("int"), [], _) ->
		Result = "int"
	; Type = term__functor(term__atom("float"), [], _) ->
		Result = "double"
	; Type = term__functor(term__atom("string"), [], _) ->
		Result = "java.lang.String"
	; Type = term__functor(term__atom("character"), [], _) ->
		Result = "char"
	;
		Result = "java.lang.Object"
	).

%-----------------------------------------------------------------------------%

decl_guard(ModuleName) = UppercaseModuleName ++ "_DECL_GUARD" :-
	MangledModuleName = sym_name_mangle(ModuleName),
	string__to_upper(MangledModuleName, UppercaseModuleName).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "foreign.m".

%-----------------------------------------------------------------------------%
:- end_module foreign.
%-----------------------------------------------------------------------------%
