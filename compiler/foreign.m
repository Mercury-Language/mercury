%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines predicates for interfacing with foreign languages.
% In particular, this module supports interfacing with with languages
% other than the target of compilation.  

% Main authors: trd, dgj.
% Parts of this code were originally written by dgj, and have since been
% moved here.

%-----------------------------------------------------------------------------%

:- module foreign.

:- interface.

:- import_module prog_data, globals.
:- import_module hlds_module, hlds_pred.
:- import_module llds.

:- import_module bool, list, string.
	% A type which is used to determine the string representation of a
	% mercury type for various foreign languages.
:- type exported_type.

	% Given a type which is not defined as a foreign type, get the
	% exported_type representation of that type.
:- func foreign__non_foreign_type((type)) = exported_type.

	% Given an arbitary mercury type, get the exported_type representation
	% of that type.
:- func foreign__to_exported_type(module_info, (type)) = exported_type.

	% Given a representation of a type determine the string which
	% corresponds to that type in the specified foreign language.
:- func foreign__to_type_string(foreign_language, exported_type) = string.
:- func foreign__to_type_string(foreign_language, module_info, (type)) = string.

	% Filter the decls for the given foreign language. 
	% The first return value is the list of matches, the second is
	% the list of mis-matches.
:- pred foreign__filter_decls(foreign_language, foreign_decl_info,
		foreign_decl_info, foreign_decl_info).
:- mode foreign__filter_decls(in, in, out, out) is det.

	% Filter the bodys for the given foreign language. 
	% The first return value is the list of matches, the second is
	% the list of mis-matches.
:- pred foreign__filter_bodys(foreign_language, foreign_body_info,
		foreign_body_info, foreign_body_info).
:- mode foreign__filter_bodys(in, in, out, out) is det.

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
:- pred foreign__extrude_pragma_implementation(list(foreign_language),
		list(pragma_var), sym_name, pred_or_func, prog_context,
		module_info, pragma_foreign_proc_attributes,
		pragma_foreign_code_impl, 
		module_info, pragma_foreign_proc_attributes,
		pragma_foreign_code_impl).
:- mode foreign__extrude_pragma_implementation(in, in, in, in, in,
		in, in, in, out, out, out) is det.

	% make_pragma_import turns pragma imports into pragma foreign_code.
	% Given the pred and proc info for this predicate, the name
	% of the function to import, the context of the import pragma
	% and the module_info, create a pragma_foreign_code_impl
	% which imports the foreign function, and return the varset,
	% pragma_vars, argument types and other information about the
	% generated predicate body.
:- pred foreign__make_pragma_import(pred_info, proc_info, string, prog_context,
	module_info, pragma_foreign_code_impl, prog_varset, 
	list(pragma_var), list(type), arity, pred_or_func).
:- mode foreign__make_pragma_import(in, in, in, in, in,
	out, out, out, out, out, out) is det.


	% It is possible that more than one foreign language could be used to
	% implement a particular piece of code.
	% Therefore, foreign languages have an order of preference, from most
	% preferred to least perferred.
	% prefer_foreign_language(Globals, Target, Lang1, Lang2) returns the
	% yes if Lang2 is preferred over Lang1.
	%
	% Otherwise it will return no.
	
:- func foreign__prefer_foreign_language(globals, compilation_target, 
	foreign_language, foreign_language) = bool.

	% A string representation of the foreign language suitable 
	% for use in human-readable error messages
:- func foreign_language_string(foreign_language) = string.

	% A string representation of the foreign language suitable 
	% for use in machine-readable name mangling.
:- func simple_foreign_language_string(foreign_language) = string.

	% The file extension used for this foreign language (including
	% the dot).
	% Not all foreign languages generate external files,
	% so this function only succeeds for those that do.
:- func foreign_language_file_extension(foreign_language) = string
		is semidet.

	% The module name used for this foreign language.
	% Not all foreign languages generate external modules 
	% so this function only succeeds for those that do.
:- func foreign_language_module_name(module_name, foreign_language) =
		module_name is semidet.

:- implementation.

:- import_module list, map, assoc_list, std_util, string, varset, int, term.
:- import_module require.

:- import_module hlds_pred, hlds_module, type_util, mode_util, error_util.
:- import_module hlds_data, prog_out.
:- import_module code_model, globals.

	% Currently we don't use the globals to compare foreign language
	% interfaces, but if we added appropriate options we might want
	% to do this later.

	% When compiling to C, C is always preferred over any other language.
prefer_foreign_language(_Globals, c, Lang1, Lang2) = 
	( Lang2 = c, not Lang1 = c ->
		yes
	; 
		no
	).

	% When compiling to asm, C is always preferred over any other language.
prefer_foreign_language(_Globals, asm, Lang1, Lang2) = 
	( Lang2 = c, not Lang1 = c ->
		yes
	; 
		no
	).

	% Whe compiling to il, first we prefer il, then csharp, then
	% managed_cplusplus, after that we don't care.
prefer_foreign_language(_Globals, il, Lang1, Lang2) = Comp :-
	PreferredList = [il, csharp, managed_cplusplus],

	FindLangPriority = (func(L) = X :-
		( list__nth_member_search(PreferredList, L, X0) ->
			X = X0
		;
			X = list__length(PreferredList) + 1
		)),
	N1 = FindLangPriority(Lang1),
	N2 = FindLangPriority(Lang2),
	( N2 < N1 ->
		Comp = yes
	;
		Comp = no
	).

	% Nothing useful to do here, but when we add Java as a
	% foreign language, we should add it here.
prefer_foreign_language(_Globals, java, _Lang1, _Lang2) = no.


foreign__filter_decls(WantedLang, Decls0, LangDecls, NotLangDecls) :-
	list__filter((pred(foreign_decl_code(Lang, _, _)::in) is semidet :-
			WantedLang = Lang),
		Decls0, LangDecls, NotLangDecls).

foreign__filter_bodys(WantedLang, Bodys0, LangBodys, NotLangBodys) :-
	list__filter((pred(foreign_body_code(Lang, _, _)::in) is semidet :-
			WantedLang = Lang),
		Bodys0, LangBodys, NotLangBodys).
	
foreign__extrude_pragma_implementation([], _PragmaVars,
	_PredName, _PredOrFunc, _Context, _ModuleInfo0, _Attributes, _Impl0, 
	_ModuleInfo, _NewAttributes, _Impl) :-
	unexpected(this_file, "no suitable target languages available").

	% We just use the first target language for now, it might be nice
	% to try a few others if the backend supports multiple ones.
foreign__extrude_pragma_implementation([TargetLang | TargetLangs], 
		_PragmaVars, _PredName, _PredOrFunc, _Context,
		ModuleInfo0, Attributes, Impl0, 
		ModuleInfo, NewAttributes, Impl) :-
	foreign_language(Attributes, ForeignLanguage),

		% If the foreign language is available as a target language, 
		% we don't need to do anything.
	( list__member(ForeignLanguage, [TargetLang | TargetLangs]) ->
		Impl = Impl0,
		ModuleInfo = ModuleInfo0,
		NewAttributes = Attributes
	;
		set_foreign_language(Attributes, TargetLang, NewAttributes),
		extrude_pragma_implementation_2(TargetLang, ForeignLanguage,
			ModuleInfo0, Impl0, ModuleInfo, Impl)
	).


:- pred extrude_pragma_implementation_2(
	foreign_language::in, foreign_language::in,
	module_info::in, pragma_foreign_code_impl::in,
	module_info::out, pragma_foreign_code_impl::out) is det.

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

extrude_pragma_implementation_2(c, c, ModuleInfo, Impl, ModuleInfo, Impl).


		% Don't do anything - C and MC++ are embedded inside MC++
		% without any changes.
extrude_pragma_implementation_2(managed_cplusplus, managed_cplusplus,
	ModuleInfo, Impl, ModuleInfo, Impl).

extrude_pragma_implementation_2(managed_cplusplus, c,
	ModuleInfo, Impl, ModuleInfo, Impl).

extrude_pragma_implementation_2(managed_cplusplus, csharp, _, _, _, _) :-
	unimplemented_combination(managed_cplusplus, csharp).

extrude_pragma_implementation_2(managed_cplusplus, il, _, _, _, _) :-
	unimplemented_combination(managed_cplusplus, il).



extrude_pragma_implementation_2(csharp, csharp,
	ModuleInfo, Impl, ModuleInfo, Impl).

extrude_pragma_implementation_2(csharp, c, _, _, _, _) :-
	unimplemented_combination(csharp, c).

extrude_pragma_implementation_2(csharp, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(csharp, managed_cplusplus).

extrude_pragma_implementation_2(csharp, il, _, _, _, _) :-
	unimplemented_combination(csharp, il).


extrude_pragma_implementation_2(il, il,
	ModuleInfo, Impl, ModuleInfo, Impl).

extrude_pragma_implementation_2(il, c, _, _, _, _) :-
	unimplemented_combination(il, c).

extrude_pragma_implementation_2(il, managed_cplusplus, _, _, _, _) :-
	unimplemented_combination(il, managed_cplusplus).

extrude_pragma_implementation_2(il, csharp, _, _, _, _) :-
	unimplemented_combination(il, csharp).



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


make_pragma_import(PredInfo, ProcInfo, C_Function, Context,
		ModuleInfo, PragmaImpl, VarSet, PragmaVars, ArgTypes, 
		Arity, PredOrFunc) :-
	%
	% lookup some information we need from the pred_info and proc_info
	%
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
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
:- pred handle_return_value(code_model, pred_or_func,
		assoc_list(pragma_var, type), module_info,
		assoc_list(pragma_var, type), string).
:- mode handle_return_value(in, in, in, in, out, out) is det.

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
:- pred include_import_arg(module_info, pair(pragma_var, type)).
:- mode include_import_arg(in, in) is semidet.

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
:- pred create_pragma_vars(list(prog_var), list(mode), int, list(pragma_var)).
:- mode create_pragma_vars(in, in, in, out) is det.

create_pragma_vars([], [], _Num, []).

create_pragma_vars([Var|Vars], [Mode|Modes], ArgNum0,
		[PragmaVar | PragmaVars]) :-
	%
	% Figure out a name for the C variable which will hold this argument
	%
	ArgNum is ArgNum0 + 1,
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
:- pred create_pragma_import_c_code(list(pragma_var), module_info,
				string, string).
:- mode create_pragma_import_c_code(in, in, in, out) is det.

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

foreign_language_string(c) = "C".
foreign_language_string(managed_cplusplus) = "Managed C++".
foreign_language_string(csharp) = "C#".
foreign_language_string(il) = "IL".

simple_foreign_language_string(c) = "c".
simple_foreign_language_string(managed_cplusplus) = "cpp". % XXX mcpp is better
simple_foreign_language_string(csharp) = "csharp".
simple_foreign_language_string(il) = "il".

foreign_language_file_extension(c) = ".c".
foreign_language_file_extension(managed_cplusplus) = ".cpp".
foreign_language_file_extension(csharp) = ".cs".
foreign_language_file_extension(il) = _ :- fail.

foreign_language_module_name(M, L) = FM :-

		% Only succeed if this language generates external files.
	_ = foreign_language_file_extension(L),

	Ending = "__" ++ simple_foreign_language_string(L) ++ "_code",
	( M = unqualified(Name),
		FM = unqualified(Name ++ Ending)
	; M = qualified(Module, Name),
		FM = qualified(Module, Name ++ Ending)
	).

%-----------------------------------------------------------------------------%

:- type exported_type
	--->	foreign(sym_name)	% A type defined by a
					% pragma foreign_type.
	;	mercury((type)).	% Any other mercury type.

non_foreign_type(Type) = mercury(Type).

to_exported_type(ModuleInfo, Type) = ExportType :-
	module_info_types(ModuleInfo, Types),
	(
		type_to_type_id(Type, TypeId, _),
		map__search(Types, TypeId, TypeDefn)
	->
		hlds_data__get_type_defn_body(TypeDefn, Body),
		( Body = foreign_type(ForeignType, _) ->
			ExportType = foreign(ForeignType)
		;
			ExportType = mercury(Type)
		)
	;
		ExportType = mercury(Type)
	).

to_type_string(Lang, ModuleInfo, Type) =
	to_type_string(Lang, to_exported_type(ModuleInfo, Type)).

to_type_string(c, foreign(_ForeignType)) = _ :-
	sorry(this_file, "foreign types on a C backend").
to_type_string(csharp, foreign(ForeignType)) = Result :-
	sym_name_to_string(ForeignType, ".", Result).
to_type_string(managed_cplusplus, foreign(ForeignType)) = Result ++ " *":-
	sym_name_to_string(ForeignType, "::", Result).
to_type_string(il, foreign(ForeignType)) = Result :-
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
		type_util__var(Type, _)
	->
		TypeString = "MR_Box"
	;
		TypeString = to_type_string(c, mercury(Type))
	).
to_type_string(il, mercury(_Type)) = _ :-
	sorry(this_file, "to_type_string for il").
	
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "foreign.m".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
