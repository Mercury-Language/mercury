%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_pragma.m.
% Main authors: fjh, dgj.
%
% This module handles the parsing of pragma directives.

:- module parse_tree__prog_io_pragma.

:- interface.

:- import_module parse_tree__prog_data, parse_tree__prog_io_util.
:- import_module list, varset, term.

	% parse the pragma declaration. 
:- pred parse_pragma(module_name, varset, list(term), maybe1(item)).
:- mode parse_pragma(in, in, in, out) is semidet.

:- implementation.

:- import_module libs__globals, parse_tree__prog_io, parse_tree__prog_io_goal.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__term_util, transform_hlds__term_errors.
:- import_module int, map, string, std_util, bool, require, set.

parse_pragma(ModuleName, VarSet, PragmaTerms, Result) :-
	(
		% new syntax: `:- pragma foo(...).'
		PragmaTerms = [SinglePragmaTerm],
		SinglePragmaTerm = term__functor(term__atom(PragmaType), 
					PragmaArgs, _),
		parse_pragma_type(ModuleName, PragmaType, PragmaArgs,
				SinglePragmaTerm, VarSet, Result0)
	->
		Result = Result0
	;
		% old syntax: `:- pragma(foo, ...).'
		% XXX we should issue a warning; this syntax is deprecated.
		PragmaTerms = [PragmaTypeTerm | PragmaArgs2],
		PragmaTypeTerm = term__functor(term__atom(PragmaType), [], _),
		parse_pragma_type(ModuleName, PragmaType, PragmaArgs2,
				PragmaTypeTerm, VarSet, Result1)
	->
		Result = Result1
	;
		fail
	).

:- pred parse_pragma_type(module_name, string, list(term), term,
						varset, maybe1(item)).
:- mode parse_pragma_type(in, in, in, in, in, out) is semidet.

parse_pragma_type(_, "source_file", PragmaTerms, ErrorTerm, _VarSet, Result) :-
	( PragmaTerms = [SourceFileTerm] ->
	    (
		SourceFileTerm = term__functor(term__string(SourceFile), [], _)
	    ->
		Result = ok(pragma(source_file(SourceFile)))
	    ;
		Result = error(
		"string expected in `:- pragma source_file' declaration",
				SourceFileTerm)
	    )
	;
	    Result = error(
	"wrong number of arguments in `:- pragma source_file' declaration",
			ErrorTerm)
	).

parse_pragma_type(ModuleName, "foreign_type", PragmaTerms,
            ErrorTerm, VarSet, Result) :-
    ( PragmaTerms = [LangTerm, MercuryTypeTerm, ForeignTypeTerm] ->
	( parse_foreign_language(LangTerm, Language) ->
	    parse_foreign_language_type(ForeignTypeTerm, Language,
	    	MaybeForeignType),
	    (
		MaybeForeignType = ok(ForeignType),
		parse_type_defn_head(ModuleName, MercuryTypeTerm,
		    ErrorTerm, MaybeTypeDefnHead),
		(
		    MaybeTypeDefnHead = ok(MercuryTypeSymName, MercuryArgs0),
		    varset__coerce(VarSet, TVarSet),
		    MercuryArgs = list__map(term__coerce, MercuryArgs0),
		    Result = ok(pragma(foreign_type(ForeignType,
			    TVarSet, MercuryTypeSymName, MercuryArgs))) 
		;
		    MaybeTypeDefnHead = error(String, Term),
		    Result = error(String, Term)
		)
	    ;
		MaybeForeignType = error(String, Term),
		Result = error(String, Term)
	    )
	;   
	   Result = error(
	   "invalid foreign language in `:- pragma foreign_type' declaration",
			LangTerm)
	)
    ;
        Result = error(
    "wrong number of arguments in `:- pragma foreign_type' declaration",
            ErrorTerm)
    ).

parse_pragma_type(ModuleName, "foreign_decl", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	parse_pragma_foreign_decl_pragma(ModuleName, "foreign_decl",
		PragmaTerms, ErrorTerm, VarSet, Result).
		
parse_pragma_type(ModuleName, "c_header_code", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	(
	    PragmaTerms = [term__functor(_, _, Context) | _]
	->
	    LangC = term__functor(term__string("C"), [], Context),
	    parse_pragma_foreign_decl_pragma(ModuleName, "c_header_code",
		[LangC | PragmaTerms], ErrorTerm, VarSet, Result)
	;
	    Result = error("wrong number of arguments or unexpected variable in `:- pragma c_header_code' declaration", 
		    ErrorTerm)
	).

parse_pragma_type(ModuleName, "foreign_code", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	parse_pragma_foreign_code_pragma(ModuleName, "foreign_code",
		    PragmaTerms, ErrorTerm, VarSet, Result).

parse_pragma_type(ModuleName, "foreign_proc", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	parse_pragma_foreign_proc_pragma(ModuleName, "foreign_proc",
		    PragmaTerms, ErrorTerm, VarSet, Result).

	% pragma c_code is almost as if we have written foreign_code
	% or foreign_proc with the language set to "C".
	% There are a few differences (error messages, some deprecated
	% syntax is still supported for c_code) so we pass the original
	% pragma name to parse_pragma_foreign_code_pragma.
parse_pragma_type(ModuleName, "c_code", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	(
			% arity = 1 (same as foreign_code)
		PragmaTerms = [term__functor(_, _, Context)]
	->
		LangC = term__functor(term__string("C"), [], Context),
		parse_pragma_foreign_code_pragma(ModuleName, "c_code",
			[LangC | PragmaTerms], ErrorTerm, VarSet, Result)
	;
			% arity > 1 (same as foreign_proc)
		PragmaTerms = [term__functor(_, _, Context) | _]
	->
		LangC = term__functor(term__string("C"), [], Context),
		parse_pragma_foreign_proc_pragma(ModuleName, "c_code",
			[LangC | PragmaTerms], ErrorTerm, VarSet, Result)
	;
	    Result = error("wrong number of arguments or unexpected variable in `:- pragma c_code' declaration", 
		    ErrorTerm)
	).

parse_pragma_type(_ModuleName, "c_import_module", PragmaTerms,
		ErrorTerm, _VarSet, Result) :-
	(
		PragmaTerms = [ImportTerm],
		sym_name_and_args(ImportTerm, Import, [])
	->
		Result = ok(pragma(foreign_import_module(c, Import)))	
	;
		Result = error("wrong number of arguments or invalid module name in `:- pragma c_import_module' declaration", 
			ErrorTerm)
	).

parse_pragma_type(_ModuleName, "foreign_import_module", PragmaTerms,
		ErrorTerm, _VarSet, Result) :-
	(
	    PragmaTerms = [LangTerm, ImportTerm],
	    sym_name_and_args(ImportTerm, Import, [])
	->
	    ( parse_foreign_language(LangTerm, Language) ->
		( Language = c ->
		    Result = ok(pragma(
		    	foreign_import_module(Language, Import)))
		;
		    Result = error("`:- pragma foreign_import_module' not yet supported for languages other than C", LangTerm)
		)
	    ;
		Result = error("invalid foreign language in `:- pragma foreign_import_module' declaration",
			LangTerm)
	    )
	;
	    Result = error("wrong number of arguments or invalid module name in `:- pragma foreign_import_module' declaration", 
			ErrorTerm)
			
	).

:- pred parse_foreign_language(term, foreign_language).
:- mode parse_foreign_language(in, out) is semidet.

parse_foreign_language(term__functor(term__string(String), _, _), Lang) :-
	globals__convert_foreign_language(String, Lang).
parse_foreign_language(term__functor(term__atom(String), _, _), Lang) :-
	globals__convert_foreign_language(String, Lang).

:- pred parse_foreign_language_type(term, foreign_language,
		maybe1(foreign_language_type)).
:- mode parse_foreign_language_type(in, in, out) is det.

parse_foreign_language_type(InputTerm, Language, Result) :-
	( 
		Language = il
	->
		( 
			InputTerm = term__functor(term__string(ILTypeName),
				[], _)
		->
			parse_il_type_name(ILTypeName, InputTerm, Result)
		;
			Result = error("invalid backend specification term",
				InputTerm)
		)
	;
		Language = c
	->
		( 
			InputTerm = term__functor(term__string(CTypeName),
				[], _)
		->
			Result = ok(c(c(CTypeName)))
		;
			Result = error("invalid backend specification term",
				InputTerm)
		)
	;

		Result = error("unsupported language specified, unable to parse backend type", InputTerm)
	).

:- pred parse_il_type_name(string, term, maybe1(foreign_language_type)).
:- mode parse_il_type_name(in, in, out) is det.

parse_il_type_name(String0, ErrorTerm, ForeignType) :-
	(
		parse_special_il_type_name(String0, ForeignTypeResult)
	->
		ForeignType = ok(il(ForeignTypeResult))
	;
		string__append("class [", String1, String0),
		string__sub_string_search(String1, "]", Index)
	->
		string__left(String1, Index, AssemblyName),
		string__split(String1, Index + 1, _, TypeNameStr),
		string_to_sym_name(TypeNameStr, ".", TypeSymName),
		ForeignType = ok(il(il(reference, AssemblyName, TypeSymName)))
	;
		string__append("valuetype [", String1, String0),
		string__sub_string_search(String1, "]", Index)
	->
		string__left(String1, Index, AssemblyName),
		string__split(String1, Index + 1, _, TypeNameStr),
		string_to_sym_name(TypeNameStr, ".", TypeSymName),
		ForeignType = ok(il(il(value, AssemblyName, TypeSymName)))
	;
		ForeignType = error(
			"invalid foreign language type description", ErrorTerm)
	).

	% Parse all the special assembler names for all the builtin types.
	% See Parition I 'Built-In Types' (Section 8.2.2) for the list
	% of all builtin types.
:- pred parse_special_il_type_name(string::in, il_foreign_type::out) is semidet.

parse_special_il_type_name("bool", il(value, "mscorlib",
			qualified(unqualified("System"), "Boolean"))).
parse_special_il_type_name("char", il(value, "mscorlib",
			qualified(unqualified("System"), "Char"))).
parse_special_il_type_name("object", il(reference, "mscorlib",
			qualified(unqualified("System"), "Object"))).
parse_special_il_type_name("string", il(reference, "mscorlib",
			qualified(unqualified("System"), "String"))).
parse_special_il_type_name("float32", il(value, "mscorlib",
			qualified(unqualified("System"), "Single"))).
parse_special_il_type_name("float64", il(value, "mscorlib",
			qualified(unqualified("System"), "Double"))).
parse_special_il_type_name("int8", il(value, "mscorlib",
			qualified(unqualified("System"), "SByte"))).
parse_special_il_type_name("int16", il(value, "mscorlib",
			qualified(unqualified("System"), "Int16"))).
parse_special_il_type_name("int32", il(value, "mscorlib",
			qualified(unqualified("System"), "Int32"))).
parse_special_il_type_name("int64", il(value, "mscorlib",
			qualified(unqualified("System"), "Int64"))).
parse_special_il_type_name("natural int", il(value, "mscorlib",
			qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("native int", il(value, "mscorlib",
			qualified(unqualified("System"), "IntPtr"))).
parse_special_il_type_name("natural unsigned int", il(value, "mscorlib",
			qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("native unsigned int", il(value, "mscorlib",
			qualified(unqualified("System"), "UIntPtr"))).
parse_special_il_type_name("refany", il(value, "mscorlib",
			qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("typedref", il(value, "mscorlib",
			qualified(unqualified("System"), "TypedReference"))).
parse_special_il_type_name("unsigned int8", il(value, "mscorlib",
			qualified(unqualified("System"), "Byte"))).
parse_special_il_type_name("unsigned int16", il(value, "mscorlib",
			qualified(unqualified("System"), "UInt16"))).
parse_special_il_type_name("unsigned int32", il(value, "mscorlib",
			qualified(unqualified("System"), "UInt32"))).
parse_special_il_type_name("unsigned int64", il(value, "mscorlib",
			qualified(unqualified("System"), "UInt64"))).

	% This predicate parses both c_header_code and foreign_decl pragmas.
:- pred parse_pragma_foreign_decl_pragma(module_name, string,
		list(term), term, varset, maybe1(item)).
:- mode parse_pragma_foreign_decl_pragma(in, in, in, in, in, out) is det.
parse_pragma_foreign_decl_pragma(_ModuleName, Pragma, PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
	string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
		InvalidDeclStr),
	( 
		PragmaTerms = [Lang, HeaderTerm]
	->
		( 
			parse_foreign_language(Lang, ForeignLanguage)
		->
			(
				HeaderTerm = term__functor(term__string(
					HeaderCode), [], _)
			->
				Result = ok(pragma(foreign_decl(
					ForeignLanguage, HeaderCode)))
			;
				ErrMsg = "-- expected string for foreign declaration code",
				Result = error(string__append(InvalidDeclStr,
					ErrMsg), HeaderTerm)
			)
		;
			ErrMsg = "-- invalid language parameter",
			Result = error(string__append(InvalidDeclStr, ErrMsg), 
				Lang)
		)
	;
	    string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
		ErrorStr),
	    Result = error(ErrorStr, ErrorTerm)
        ).

	% This predicate parses both c_code and foreign_code pragmas.
	% Processing of foreign_proc (or c_code that defines a procedure)
	% is handled in parse_pragma_foreign_proc_pragma below.
:- pred parse_pragma_foreign_code_pragma(module_name, string,
		list(term), term, varset, maybe1(item)).
:- mode parse_pragma_foreign_code_pragma(in, in, in, in, in, out) is det.

parse_pragma_foreign_code_pragma(_ModuleName, Pragma, PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
	string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
		InvalidDeclStr),

	Check1 = (func(PTerms1, ForeignLanguage) = Res is semidet :- 
		PTerms1 = [Just_Code_Term],
		(
			Just_Code_Term = term__functor(term__string(
				Just_Code), [], _)
		->
			Res = ok(pragma(foreign_code(ForeignLanguage, 
				Just_Code)))
		;
			ErrMsg = "-- expected string for foreign code",
			Res = error(string__append(InvalidDeclStr, ErrMsg),
				ErrorTerm)
		)
	),

	CheckLength = (func(PTermsLen, ForeignLanguage) = Res :- 
		( 
			Res0 = Check1(PTermsLen, ForeignLanguage)
		->	
			Res = Res0
		;
			ErrMsg = "-- wrong number of arguments",
			Res = error(string__append(InvalidDeclStr, ErrMsg), 
				ErrorTerm)
		)	
	),

	CheckLanguage = (func(PTermsLang) = Res is semidet :- 
		PTermsLang = [Lang | Rest],
		( 
	    		parse_foreign_language(Lang, ForeignLanguage)
		->
			Res = CheckLength(Rest, ForeignLanguage)
		;
			ErrMsg = "-- invalid language parameter",
			Res = error(string__append(InvalidDeclStr, ErrMsg), 
				Lang)
		)
	),

	(
		Result0 = CheckLanguage(PragmaTerms)
	->
		Result = Result0
	;
		ErrMsg0 = "-- wrong number of arguments",
		Result = error(string__append(InvalidDeclStr, ErrMsg0),
			ErrorTerm)
	).


	% This predicate parses both c_code and foreign_proc pragmas.
:- pred parse_pragma_foreign_proc_pragma(module_name, string,
		list(term), term, varset, maybe1(item)).
:- mode parse_pragma_foreign_proc_pragma(in, in, in, in, in, out) is det.

parse_pragma_foreign_proc_pragma(ModuleName, Pragma, PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	string__format("invalid `:- pragma %s' declaration ", [s(Pragma)],
		InvalidDeclStr),

	Check6 = (func(PTerms6, ForeignLanguage) = Res is semidet :- 
            PTerms6 = [PredAndVarsTerm, FlagsTerm,
		    FieldsTerm, FirstTerm, LaterTerm, SharedTerm],
	    parse_pragma_foreign_proc_attributes_term(
	    		ForeignLanguage, Pragma, FlagsTerm, MaybeFlags),
	    ( MaybeFlags = ok(Flags) ->
	        ( parse_pragma_keyword("local_vars", FieldsTerm, Fields,
			FieldsContext) ->
	            ( parse_pragma_keyword("first_code", FirstTerm, First,
		    		FirstContext) ->
	                ( parse_pragma_keyword("retry_code", LaterTerm, Later,
				LaterContext) ->
	                    ( parse_pragma_keyword("shared_code", SharedTerm,
			    		Shared, SharedContext) ->
	        	        parse_pragma_foreign_code(ModuleName,
				    Flags, PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					share, Shared, yes(SharedContext)),
				    VarSet, Res)
		            ; parse_pragma_keyword("duplicated_code",
			    		SharedTerm, Shared, SharedContext) ->
	        	        parse_pragma_foreign_code(ModuleName,
				    Flags, PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					duplicate, Shared, yes(SharedContext)),
				    VarSet, Res)
		            ; parse_pragma_keyword("common_code", SharedTerm,
			    		Shared, SharedContext) ->
	        	        parse_pragma_foreign_code(ModuleName, 
				    Flags, PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					automatic, Shared, yes(SharedContext)),
				    VarSet, Res)
		            ;
		                ErrMsg = "-- invalid seventh argument, expecting `common_code(<code>)'",
				Res = error(string__append(InvalidDeclStr,
					ErrMsg), SharedTerm)
			    )
		        ;
		            ErrMsg = "-- invalid sixth argument, expecting `retry_code(<code>)'",
			    Res = error(string__append(InvalidDeclStr, ErrMsg),
			    	LaterTerm)
			)
		    ;
		        ErrMsg = "-- invalid fifth argument, expecting `first_code(<code>)'",
			Res = error(string__append(InvalidDeclStr, ErrMsg),
			    	FirstTerm)
		    )
		;
		    ErrMsg = "-- invalid fourth argument, expecting `local_vars(<fields>)'",
		    Res = error(string__append(InvalidDeclStr, ErrMsg),
			    	FieldsTerm)
		)
	    ;
		MaybeFlags = error(FlagsErrorStr, ErrorTerm),
		ErrMsg = "-- invalid third argument: " ++ FlagsErrorStr,
		Res = error(string__append(InvalidDeclStr, ErrMsg), ErrorTerm)
	    )
	),

	Check5 = (func(PTerms5, ForeignLanguage) = Res is semidet :- 
		PTerms5 = [PredAndVarsTerm, FlagsTerm,
		    FieldsTerm, FirstTerm, LaterTerm],
		term__context_init(DummyContext),
		SharedTerm = term__functor(term__atom("common_code"),
			[term__functor(term__string(""), [], DummyContext)],
			DummyContext),
		Res = Check6([PredAndVarsTerm, FlagsTerm, FieldsTerm, FirstTerm,
			LaterTerm, SharedTerm], ForeignLanguage)
	),

	Check3 = (func(PTerms3, ForeignLanguage) = Res is semidet :- 
    	    PTerms3 = [PredAndVarsTerm, FlagsTerm, CodeTerm],
	    (
		CodeTerm = term__functor(term__string(Code), [], Context)
	    ->
		parse_pragma_foreign_proc_attributes_term(ForeignLanguage, 
			Pragma, FlagsTerm, MaybeFlags),
		( 
			MaybeFlags = ok(Flags),
			parse_pragma_foreign_code(ModuleName, Flags,
				PredAndVarsTerm, ordinary(Code, yes(Context)),
				VarSet, Res)
	        ; 
			MaybeFlags = error(FlagsErr, FlagsErrTerm),
			parse_pragma_foreign_proc_attributes_term(
				ForeignLanguage, Pragma, PredAndVarsTerm,
				MaybeFlags2),
			(
				MaybeFlags2 = ok(Flags),
			    % XXX we should issue a warning; this syntax is
			    % deprecated We will continue to accept this if
			    % c_code is used, but not with foreign_code
				( Pragma = "c_code" ->
					parse_pragma_foreign_code(ModuleName,
						Flags, FlagsTerm,
						ordinary(Code, yes(Context)),
						VarSet, Res)
				;
					ErrMsg = "-- invalid second argument, expecting predicate or function mode",
					Res = error(string__append(
						InvalidDeclStr, ErrMsg), 
						PredAndVarsTerm)
				)	
			;
				MaybeFlags2 = error(_, _),
				ErrMsg = "-- invalid third argument: ",
				Res = error(InvalidDeclStr ++ ErrMsg ++
					FlagsErr, FlagsErrTerm)
			)
		)
	    ;
		ErrMsg = "-- invalid fourth argument, expecting string containing foreign code",
		Res = error(string__append(InvalidDeclStr, ErrMsg), 
		    	CodeTerm)
	    )
	),


	Check2 = (func(PTerms2, ForeignLanguage) = Res is semidet :- 
		PTerms2 = [PredAndVarsTerm, CodeTerm],
	    	% XXX we should issue a warning; this syntax is deprecated.
		% We will continue to accept this if c_code is used, but
		% not with foreign_code
		( 
			Pragma = "c_code"
		->
			% may_call_mercury is a conservative default.
			default_attributes(ForeignLanguage, Attributes0),
			set_legacy_purity_behaviour(Attributes0, yes,
				Attributes),
			(
			    CodeTerm = term__functor(term__string(Code), [],
				Context)
			->
			    parse_pragma_foreign_code(ModuleName, 
			        Attributes, PredAndVarsTerm, ordinary(Code,
				yes(Context)), VarSet, Res)
			;
			    ErrMsg = "-- expecting either `may_call_mercury' or `will_not_call_mercury', and a string for foreign code",
			    Res = error(string__append(InvalidDeclStr, ErrMsg), 
				CodeTerm)
	    		)
		;
			ErrMsg = "-- doesn't say whether it can call mercury",
			Res = error(string__append(InvalidDeclStr, ErrMsg),
				ErrorTerm)
		)
	),

	CheckLength = (func(PTermsLen, ForeignLanguage) = Res :- 
		( 
			Res0 = Check2(PTermsLen, ForeignLanguage)
		->	
			Res = Res0
		;
			Res0 = Check3(PTermsLen, ForeignLanguage)
		->	
			Res = Res0
		;
			Res0 = Check5(PTermsLen, ForeignLanguage)
		->	
			Res = Res0
		;
			Res0 = Check6(PTermsLen, ForeignLanguage)
		->	
			Res = Res0
		;
			ErrMsg = "-- wrong number of arguments",
			Res = error(string__append(InvalidDeclStr, ErrMsg), 
				ErrorTerm)
		)	
	),

	CheckLanguage = (func(PTermsLang) = Res is semidet :- 
		PTermsLang = [Lang | Rest],
		( 
	    		parse_foreign_language(Lang, ForeignLanguage)
		->
			Res = CheckLength(Rest, ForeignLanguage)
		;
			ErrMsg = "-- invalid language parameter",
			Res = error(string__append(InvalidDeclStr, ErrMsg), 
				Lang)
		)
	),

	(
		Result0 = CheckLanguage(PragmaTerms)
	->
		Result = Result0
	;
		ErrMsg0 = "-- wrong number of arguments",
		Result = error(string__append(InvalidDeclStr, ErrMsg0),
			ErrorTerm)
	).


parse_pragma_type(ModuleName, "import", PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
		% XXX we assume all imports are C
	ForeignLanguage = c,
	(
	    (
		PragmaTerms = [PredAndModesTerm, FlagsTerm, FunctionTerm],
		parse_pragma_foreign_proc_attributes_term(ForeignLanguage,
				"import", FlagsTerm, MaybeFlags),
		(
			MaybeFlags = error(FlagError, ErrorTerm),
			FlagsResult = error("invalid second argument in `:- pragma import/3' declaration : " ++ FlagError, ErrorTerm)
		;
			MaybeFlags = ok(Flags),
			FlagsResult = ok(Flags)
	        )
	    ;
		PragmaTerms = [PredAndModesTerm, FunctionTerm],
		default_attributes(ForeignLanguage, Flags0),
			% pragma import uses legacy purity behaviour
		set_legacy_purity_behaviour(Flags0, yes, Flags),
		FlagsResult = ok(Flags)
	    )	
 	-> 
	    (
		FunctionTerm = term__functor(term__string(Function), [], _)
	    ->
		parse_pred_or_func_and_arg_modes(yes(ModuleName),
			PredAndModesTerm, ErrorTerm,
			"`:- pragma import' declaration",
			PredAndArgModesResult),
		(
		    PredAndArgModesResult = ok(PredName - PredOrFunc,
				ArgModes),
		    (
			FlagsResult = ok(Attributes),
			Result = ok(pragma(import(PredName, PredOrFunc,
				ArgModes, Attributes, Function)))
		    ;
			FlagsResult = error(Msg, Term),
			Result = error(Msg, Term)
		    )
		;
			PredAndArgModesResult = error(Msg, Term),
			Result = error(Msg, Term)
		)
	    ;
	    	Result = error(
	"expected pragma import(PredName(ModeList), Function)",
		     PredAndModesTerm)
	    )
	;
	    Result = 
	    	error(
		"wrong number of arguments in `:- pragma import' declaration",
		ErrorTerm)
	).

parse_pragma_type(_ModuleName, "export", PragmaTerms,
		ErrorTerm, _VarSet, Result) :-
	% XXX we implicitly assume exports are only for C
       (
	    PragmaTerms = [PredAndModesTerm, FunctionTerm]
       ->
	    (
	        FunctionTerm = term__functor(term__string(Function), [], _)
	    ->
		parse_pred_or_func_and_arg_modes(no, PredAndModesTerm,
			ErrorTerm, "`:- pragma export' declaration",
			PredAndModesResult),
		(
			PredAndModesResult = ok(PredName - PredOrFunc, Modes),
		    	Result = ok(pragma(export(PredName, PredOrFunc,
					Modes, Function)))
		;    
			PredAndModesResult = error(Msg, Term),
			Result = error(Msg, Term)
		)
	    ;
	    	Result = error(
		     "expected pragma export(PredName(ModeList), Function)",
		     PredAndModesTerm)
	    )
	;
	    Result = 
	    	error(
		"wrong number of arguments in `:- pragma export' declaration",
		ErrorTerm)
       ).

parse_pragma_type(ModuleName, "inline", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_simple_pragma(ModuleName, "inline",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = inline(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "no_inline", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_simple_pragma(ModuleName, "no_inline",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = no_inline(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "memo", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_tabling_pragma(ModuleName, "memo", eval_memo, 
		PragmaTerms, ErrorTerm, Result).
parse_pragma_type(ModuleName, "loop_check", PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
	parse_tabling_pragma(ModuleName, "loop_check", eval_loop_check, 
		PragmaTerms, ErrorTerm, Result).
parse_pragma_type(ModuleName, "minimal_model", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_tabling_pragma(ModuleName, "minimal_model", eval_minimal, 
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "obsolete", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_simple_pragma(ModuleName, "obsolete",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = obsolete(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

	% pragma unused_args should never appear in user programs,
	% only in .opt files.
parse_pragma_type(ModuleName, "unused_args", PragmaTerms,
		ErrorTerm, _VarSet, Result) :-
	(
		PragmaTerms = [
			PredOrFuncTerm,
			PredNameTerm,
			term__functor(term__integer(Arity), [], _),
			term__functor(term__integer(ModeNum), [], _),
			UnusedArgsTerm
		],
		(
			PredOrFuncTerm = term__functor(
					term__atom("predicate"), [], _),
			PredOrFunc = predicate
		;
			PredOrFuncTerm = term__functor(
					term__atom("function"), [], _),
			PredOrFunc = function 
		),
		parse_implicitly_qualified_term(ModuleName, PredNameTerm,
			ErrorTerm, "`:- pragma unused_args' declaration",
			PredNameResult),
		PredNameResult = ok(PredName, []),
		convert_int_list(UnusedArgsTerm, UnusedArgsResult),
		UnusedArgsResult = ok(UnusedArgs)
	->	
		Result = ok(pragma(unused_args(PredOrFunc, PredName,
				Arity, ModeNum, UnusedArgs)))
	;
		Result = error("error in `:- pragma unused_args'", ErrorTerm)
	).

parse_pragma_type(ModuleName, "type_spec", PragmaTerms, ErrorTerm, 
		VarSet0, Result) :-
	(
	    (
	        PragmaTerms = [PredAndModesTerm, TypeSubnTerm],
		MaybeName = no
	    ;
		PragmaTerms = [PredAndModesTerm, TypeSubnTerm, SpecNameTerm],
		SpecNameTerm = term__functor(_, _, SpecContext),

		% This form of the pragma should not appear in source files.
		term__context_file(SpecContext, FileName),
		\+ string__remove_suffix(FileName, ".m", _),	

		parse_implicitly_qualified_term(ModuleName,
			SpecNameTerm, ErrorTerm, "", NameResult),
		NameResult = ok(SpecName, []),
		MaybeName = yes(SpecName)
	    )
	->
	    parse_arity_or_modes(ModuleName, PredAndModesTerm, ErrorTerm,
			"`:- pragma type_spec' declaration",
			ArityOrModesResult),
	    (
		ArityOrModesResult = ok(arity_or_modes(PredName,
			 Arity, MaybePredOrFunc, MaybeModes)),
		conjunction_to_list(TypeSubnTerm, TypeSubnList),

		% The varset is actually a tvarset.
		varset__coerce(VarSet0, TVarSet),
		( list__map(convert_type_spec_pair, TypeSubnList, TypeSubn) ->
			( MaybeName = yes(SpecializedName0) ->
				SpecializedName = SpecializedName0
		    	;
				unqualify_name(PredName, UnqualName),
				make_pred_name(ModuleName, "TypeSpecOf",
					MaybePredOrFunc, UnqualName,
					type_subst(TVarSet, TypeSubn),
					SpecializedName)
		    	),
		   	Result = ok(pragma(type_spec(PredName,
				SpecializedName, Arity, MaybePredOrFunc,
				MaybeModes, TypeSubn, TVarSet, set__init)))
		    ;
			Result = error(
	"expected type substitution in `:- pragma type_spec' declaration",
				TypeSubnTerm)
		)
	    ;
		    ArityOrModesResult = error(Msg, Term),
		    Result = error(Msg, Term)
	    )
	;
	    Result = error(
	"wrong number of arguments in `:- pragma type_spec' declaration", 
		ErrorTerm)
	).

parse_pragma_type(ModuleName, "fact_table", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	(
	    PragmaTerms = [PredAndArityTerm, FileNameTerm]
	->
	    parse_pred_name_and_arity(ModuleName, "fact_table",
	    	PredAndArityTerm, ErrorTerm, NameArityResult),
	    (
		NameArityResult = ok(PredName, Arity),
		(
		    FileNameTerm = term__functor(term__string(FileName), [], _)
		->
		    Result = ok(pragma(fact_table(PredName, Arity, FileName)))
		;
		    Result = error("expected string for fact table filename",
			    FileNameTerm)
		)
	    ;
		NameArityResult = error(ErrorMsg, _),
		Result = error(ErrorMsg, PredAndArityTerm)
	    )
	;
	    Result = 
		error(
	"wrong number of arguments in `:- pragma fact_table' declaration",
		ErrorTerm)
	).

parse_pragma_type(ModuleName, "aditi", PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "aditi",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = aditi(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "base_relation", PragmaTerms, 
		ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "base_relation",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = base_relation(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_index", PragmaTerms,
		ErrorTerm, _, Result) :-
	( PragmaTerms = [PredNameArityTerm, IndexTypeTerm, AttributesTerm] ->
	    parse_pred_name_and_arity(ModuleName, "aditi_index",
	    	PredNameArityTerm, ErrorTerm, NameArityResult),
	    (
	        NameArityResult = ok(PredName, PredArity),
		(
		    IndexTypeTerm = term__functor(term__atom(IndexTypeStr),
		    		[], _),
		    (
			IndexTypeStr = "unique_B_tree",
			IndexType = unique_B_tree
		    ;
			IndexTypeStr = "non_unique_B_tree",
		    	IndexType = non_unique_B_tree
		    )
		->
		    convert_int_list(AttributesTerm, AttributeResult),
		    (
			AttributeResult = ok(Attributes),
			Result = ok(pragma(aditi_index(PredName, PredArity,
			    		index_spec(IndexType, Attributes))))
		    ;
			AttributeResult = error(_, AttrErrorTerm),
			Result = error(
	"expected attribute list for `:- pragma aditi_index' declaration", 
				AttrErrorTerm)	
		    )
	    	;
		    Result = error(
	"expected index type for `:- pragma aditi_index' declaration",
	    			IndexTypeTerm)	
	        )
	    ;
		NameArityResult = error(NameErrorMsg, NameErrorTerm),
		Result = error(NameErrorMsg, NameErrorTerm)
	    )
	;
	    Result = error(
	"wrong number of arguments in `:- pragma aditi_index' declaration",
		ErrorTerm)
	).

parse_pragma_type(ModuleName, "naive", PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "naive",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = naive(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "psn", PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "psn",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = psn(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_memo",
		PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "aditi_memo",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = aditi_memo(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "aditi_no_memo",
		PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "aditi_no_memo",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = aditi_no_memo(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "supp_magic", 
		PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "supp_magic",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = supp_magic(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "context",
		PragmaTerms, ErrorTerm, _, Result) :-
	parse_simple_pragma(ModuleName, "context",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = context(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "owner",
		PragmaTerms, ErrorTerm, _, Result) :-
	( PragmaTerms = [SymNameAndArityTerm, OwnerTerm] ->
	    ( OwnerTerm = term__functor(term__atom(Owner), [], _) ->
		parse_simple_pragma(ModuleName, "owner",
			lambda([Name::in, Arity::in, Pragma::out] is det,
				Pragma = owner(Name, Arity, Owner)),
			[SymNameAndArityTerm], ErrorTerm, Result)
	    ;
	ErrorMsg = "expected owner name for `:- pragma owner' declaration",
	        Result = error(ErrorMsg, OwnerTerm)
	    )
	;
    ErrorMsg = "wrong number of arguments in `:- pragma owner' declaration",
	    Result = error(ErrorMsg, ErrorTerm)
	).

parse_pragma_type(ModuleName, "promise_pure", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_simple_pragma(ModuleName, "promise_pure",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = promise_pure(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "promise_semipure", PragmaTerms, ErrorTerm,
		_VarSet, Result) :-
	parse_simple_pragma(ModuleName, "promise_semipure",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = promise_semipure(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "termination_info", PragmaTerms, ErrorTerm,
	_VarSet, Result) :-
    (
	PragmaTerms = [
	    PredAndModesTerm0,
	    ArgSizeTerm,
	    TerminationTerm
	],
	parse_pred_or_func_and_arg_modes(yes(ModuleName), PredAndModesTerm0,
		ErrorTerm, "`:- pragma termination_info' declaration",
		NameAndModesResult),
	NameAndModesResult = ok(PredName - PredOrFunc, ModeList),
	(			
		ArgSizeTerm = term__functor(term__atom("not_set"), [], _),
		MaybeArgSizeInfo = no
	;
		ArgSizeTerm = term__functor(term__atom("infinite"), [], _),
		MaybeArgSizeInfo = yes(infinite)
	;
		ArgSizeTerm = term__functor(term__atom("finite"),
			[IntTerm, UsedArgsTerm], _),
		IntTerm = term__functor(term__integer(Int), [], _),
		convert_bool_list(UsedArgsTerm, UsedArgs),
		MaybeArgSizeInfo = yes(finite(Int, UsedArgs))
	),
	(
		TerminationTerm = term__functor(term__atom("not_set"), [], _),
		MaybeTerminationInfo = no
	;
		TerminationTerm = term__functor(term__atom("can_loop"), [], _),
		MaybeTerminationInfo = yes(can_loop)
	;
		TerminationTerm = term__functor(term__atom("cannot_loop"),
			[], _),
		MaybeTerminationInfo = yes(cannot_loop)
	),
	Result0 = ok(pragma(termination_info(PredOrFunc, PredName, 
			ModeList, MaybeArgSizeInfo, MaybeTerminationInfo)))
    ->
	Result = Result0
    ;
	Result = error(
		"syntax error in `:- pragma termination_info' declaration",
		ErrorTerm)
    ).
			
parse_pragma_type(ModuleName, "terminates", PragmaTerms,
				ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma(ModuleName, "terminates",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = terminates(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "does_not_terminate", PragmaTerms,
				ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma(ModuleName, "does_not_terminate",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = does_not_terminate(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

parse_pragma_type(ModuleName, "check_termination", PragmaTerms,
				ErrorTerm, _VarSet, Result) :-
	parse_simple_pragma(ModuleName, "check_termination",
		lambda([Name::in, Arity::in, Pragma::out] is det,
			Pragma = check_termination(Name, Arity)),
		PragmaTerms, ErrorTerm, Result).

:- pred parse_simple_pragma(module_name, string,
			pred(sym_name, int, pragma_type),
			list(term), term, maybe1(item)).
:- mode parse_simple_pragma(in, in, pred(in, in, out) is det,
			in, in, out) is det.

parse_simple_pragma(ModuleName, PragmaType, MakePragma,
				PragmaTerms, ErrorTerm, Result) :-
	( PragmaTerms = [PredAndArityTerm] ->
	    parse_pred_name_and_arity(ModuleName, PragmaType,
		PredAndArityTerm, ErrorTerm, NameArityResult),
	    (
	    	NameArityResult = ok(PredName, Arity),
		call(MakePragma, PredName, Arity, Pragma),
		Result = ok(pragma(Pragma))
	    ;
		NameArityResult = error(ErrorMsg, _),
	        Result = error(ErrorMsg, PredAndArityTerm)
	    )
	;
	    string__append_list(["wrong number of arguments in `:- pragma ",
		 PragmaType, "' declaration"], ErrorMsg),
	    Result = error(ErrorMsg, ErrorTerm)
       ).

:- pred parse_pred_name_and_arity(module_name, string, term, term,
		maybe2(sym_name, arity)).
:- mode parse_pred_name_and_arity(in, in, in, in, out) is det.

parse_pred_name_and_arity(ModuleName, PragmaType, PredAndArityTerm,
		ErrorTerm, Result) :-
	(
		parse_name_and_arity(ModuleName, PredAndArityTerm,
			PredName, Arity)
	->
		Result = ok(PredName, Arity)
	;
		string__append_list(["expected predname/arity for `pragma ",
			PragmaType, "' declaration"], ErrorMsg),
		Result = error(ErrorMsg, ErrorTerm)
	).

%-----------------------------------------------------------------------------%

:- pred parse_pragma_keyword(string, term, string, term__context).
:- mode parse_pragma_keyword(in, in, out, out) is semidet.

parse_pragma_keyword(ExpectedKeyword, Term, StringArg, StartContext) :-
	Term = term__functor(term__atom(ExpectedKeyword), [Arg], _),
	Arg = term__functor(term__string(StringArg), [], StartContext).

%-----------------------------------------------------------------------------%

:- type collected_pragma_foreign_proc_attribute
	--->	may_call_mercury(may_call_mercury)
	;	thread_safe(thread_safe)
	;	tabled_for_io(tabled_for_io)
	;	purity(purity)
	;	aliasing
	;	max_stack_size(int).

:- pred parse_pragma_foreign_proc_attributes_term(foreign_language, string,
		term, maybe1(pragma_foreign_proc_attributes)).
:- mode parse_pragma_foreign_proc_attributes_term(in, in, in, out) is det.

parse_pragma_foreign_proc_attributes_term(ForeignLanguage, Pragma, Term,
		MaybeAttributes) :-
	default_attributes(ForeignLanguage, Attributes0),
	( ( Pragma = "c_code" ; Pragma = "import" ) ->
		set_legacy_purity_behaviour(Attributes0, yes, Attributes1),
		set_purity(Attributes1, pure, Attributes2)
	;
		Attributes2 = Attributes0
	),
	ConflictingAttributes = [
		may_call_mercury(will_not_call_mercury) - 
			may_call_mercury(may_call_mercury),
		thread_safe(thread_safe) - 
			thread_safe(not_thread_safe),
		tabled_for_io(tabled_for_io) - 
			tabled_for_io(not_tabled_for_io),
		purity(pure) - purity(impure),
		purity(pure) - purity(semipure),
		purity(semipure) - purity(impure)
	],
	(
		parse_pragma_foreign_proc_attributes_term0(Term, AttrList)
	->
		( 
			list__member(Conflict1 - Conflict2,
				ConflictingAttributes),
			list__member(Conflict1, AttrList),
			list__member(Conflict2, AttrList)
		->
			MaybeAttributes = error("conflicting attributes in attribute list", Term)
		;
			list__foldl(
				process_attribute,
				AttrList, Attributes2, Attributes),
			MaybeAttributes = check_required_attributes(
				ForeignLanguage, Attributes, Term)
		)
	;
		ErrMsg = "expecting a foreign proc attribute or list of attributes",
		MaybeAttributes = error(ErrMsg, Term)
	).


	% Update the pragma_foreign_proc_attributes according to the given 
	% collected_pragma_foreign_proc_attribute.
:- pred process_attribute(collected_pragma_foreign_proc_attribute::in,
		pragma_foreign_proc_attributes::in,
		pragma_foreign_proc_attributes::out) is det.

process_attribute(may_call_mercury(MayCallMercury), Attrs0, Attrs) :-
	set_may_call_mercury(Attrs0, MayCallMercury, Attrs).
process_attribute(thread_safe(ThreadSafe), Attrs0, Attrs) :-
	set_thread_safe(Attrs0, ThreadSafe, Attrs).
process_attribute(tabled_for_io(TabledForIO), Attrs0, Attrs) :-
	set_tabled_for_io(Attrs0, TabledForIO, Attrs).
process_attribute(purity(Pure), Attrs0, Attrs) :-
	set_purity(Attrs0, Pure, Attrs).
process_attribute(max_stack_size(Size), Attrs0, Attrs) :-
	add_extra_attribute(Attrs0, max_stack_size(Size), Attrs).

	% Aliasing is currently ignored in the main branch compiler.
process_attribute(aliasing, Attrs, Attrs).


	% Check whether all the required attributes have been set for
	% a particular language
:- func check_required_attributes(foreign_language,
		pragma_foreign_proc_attributes, term)
	= maybe1(pragma_foreign_proc_attributes).

check_required_attributes(c, Attrs, _Term) = ok(Attrs).
check_required_attributes(managed_cplusplus, Attrs, _Term) = ok(Attrs).
check_required_attributes(csharp, Attrs, _Term) = ok(Attrs).
check_required_attributes(il, Attrs, Term) = Res :-
	( [] = list__filter_map(
		(func(X) = X is semidet :- X = max_stack_size(_)),
		Attrs ^ extra_attributes)
	->
		Res = error(
			"expecting max_stack_size attribute for IL code", Term)
	;
		Res = ok(Attrs)
	).

:- pred parse_pragma_foreign_proc_attributes_term0(term,
		list(collected_pragma_foreign_proc_attribute)).
:- mode parse_pragma_foreign_proc_attributes_term0(in, out) is semidet.

parse_pragma_foreign_proc_attributes_term0(Term, Flags) :-
	(
		parse_single_pragma_foreign_proc_attribute(Term, Flag)
	->
		Flags = [Flag]
	;
		(
			Term = term__functor(term__atom("[]"), [], _),
			Flags = []
		;
			Term = term__functor(term__atom("[|]"), [Hd, Tl], _),
			Flags = [Flag|Flags0],
			parse_single_pragma_foreign_proc_attribute(Hd, Flag),
			parse_pragma_foreign_proc_attributes_term0(Tl, Flags0)
		)
	).

:- pred parse_single_pragma_foreign_proc_attribute(term,
		collected_pragma_foreign_proc_attribute).
:- mode parse_single_pragma_foreign_proc_attribute(in, out) is semidet.

parse_single_pragma_foreign_proc_attribute(Term, Flag) :-
	( parse_may_call_mercury(Term, MayCallMercury) ->
		Flag = may_call_mercury(MayCallMercury)
	; parse_threadsafe(Term, ThreadSafe) ->
		Flag = thread_safe(ThreadSafe)
	; parse_tabled_for_io(Term, TabledForIo) ->
		Flag = tabled_for_io(TabledForIo)
	; parse_aliasing(Term) ->
		Flag = aliasing
	; parse_max_stack_size(Term, Size) ->
		Flag = max_stack_size(Size)
	; parse_purity_promise(Term, Purity) ->
		Flag = purity(Purity)
	;
		fail
	).

:- pred parse_may_call_mercury(term, may_call_mercury).
:- mode parse_may_call_mercury(in, out) is semidet.

parse_may_call_mercury(term__functor(term__atom("recursive"), [], _),
	may_call_mercury).
parse_may_call_mercury(term__functor(term__atom("non_recursive"), [], _),
	will_not_call_mercury).
parse_may_call_mercury(term__functor(term__atom("may_call_mercury"), [], _),
	may_call_mercury).
parse_may_call_mercury(term__functor(term__atom("will_not_call_mercury"), [],
	_), will_not_call_mercury).

:- pred parse_threadsafe(term, thread_safe).
:- mode parse_threadsafe(in, out) is semidet.

parse_threadsafe(term__functor(term__atom("thread_safe"), [], _),
	thread_safe).
parse_threadsafe(term__functor(term__atom("not_thread_safe"), [], _),
	not_thread_safe).

:- pred parse_tabled_for_io(term, tabled_for_io).
:- mode parse_tabled_for_io(in, out) is semidet.

parse_tabled_for_io(term__functor(term__atom("tabled_for_io"), [], _),
	tabled_for_io).
parse_tabled_for_io(term__functor(term__atom("not_tabled_for_io"), [], _),
	not_tabled_for_io).

	% XXX For the moment we just ignore the following attributes.
	% These attributes are used for aliasing on the reuse branch,
	% and ignoring them allows the main branch compiler to compile
	% the reuse branch.
:- pred parse_aliasing(term).
:- mode parse_aliasing(in) is semidet.

parse_aliasing(term__functor(term__atom("no_aliasing"), [], _)).
parse_aliasing(term__functor(term__atom("unknown_aliasing"), [], _)).
parse_aliasing(term__functor(term__atom("alias"), [_Types, _Alias], _)).


:- pred parse_max_stack_size(term::in, int::out) is semidet.

parse_max_stack_size(term__functor(
		term__atom("max_stack_size"), [SizeTerm], _), Size) :-
	SizeTerm = term__functor(term__integer(Size), [], _).

:- pred parse_purity_promise(term::in, purity::out) is semidet.

parse_purity_promise(term__functor(term__atom("promise_pure"), [], _), 
		(pure)).
parse_purity_promise(term__functor(term__atom("promise_semipure"), [], _),
		(semipure)).

% parse a pragma foreign_code declaration

:- pred parse_pragma_foreign_code(module_name, pragma_foreign_proc_attributes,
	term, pragma_foreign_code_impl, varset, maybe1(item)).
:- mode parse_pragma_foreign_code(in, in, in, in, in, out) is det.

parse_pragma_foreign_code(ModuleName, Flags, PredAndVarsTerm0,
	PragmaImpl, VarSet0, Result) :-
    parse_pred_or_func_and_args(yes(ModuleName), PredAndVarsTerm0,
    	PredAndVarsTerm0, "`:- pragma c_code' declaration", PredAndArgsResult),
    (
    	PredAndArgsResult = ok(PredName, VarList0 - MaybeRetTerm),
    	(
	    % is this a function or a predicate?
	    MaybeRetTerm = yes(FuncResultTerm0)
	->
	    % function
	    PredOrFunc = function,
	    list__append(VarList0, [FuncResultTerm0], VarList)
	;
	    % predicate
	    PredOrFunc = predicate,
	    VarList = VarList0
	),
	parse_pragma_c_code_varlist(VarSet0, VarList, PragmaVars, Error),
	(
	    Error = no,
	    varset__coerce(VarSet0, VarSet),
	    Result = ok(pragma(foreign_proc(Flags, PredName,
		    PredOrFunc, PragmaVars, VarSet, PragmaImpl)))
	;
	    Error = yes(ErrorMessage),
	    Result = error(ErrorMessage, PredAndVarsTerm0)

	)
    ;
	PredAndArgsResult = error(Msg, Term),
	Result = error(Msg, Term)
    ).

	% parse the variable list in the pragma c code declaration.
	% The final argument is 'no' for no error, or 'yes(ErrorMessage)'.
:- pred parse_pragma_c_code_varlist(varset, list(term),
		list(pragma_var), maybe(string)).
:- mode parse_pragma_c_code_varlist(in, in, out, out) is det.

parse_pragma_c_code_varlist(_, [], [], no).
parse_pragma_c_code_varlist(VarSet, [V|Vars], PragmaVars, Error):-
	(
		V = term__functor(term__atom("::"), [VarTerm, ModeTerm], _),
		VarTerm = term__variable(Var)
	->
		(
			varset__search_name(VarSet, Var, VarName)
		->
			(
				convert_mode(allow_constrained_inst_var,
					ModeTerm, Mode0)
			->
				constrain_inst_vars_in_mode(Mode0, Mode),
				term__coerce_var(Var, ProgVar),
				P = (pragma_var(ProgVar, VarName, Mode)),
				parse_pragma_c_code_varlist(VarSet, 
					Vars, PragmaVars0, Error),
				PragmaVars = [P|PragmaVars0]
			;
				PragmaVars = [],
				Error = yes("unknown mode in pragma c_code")
			)
		;
			% if the variable wasn't in the varset it must be an
			% underscore variable.
			PragmaVars = [],	% return any old junk for that.
			Error = yes(
"sorry, not implemented: anonymous `_' variable in pragma c_code")
		)
	;
		PragmaVars = [],	% return any old junk in PragmaVars
		Error = yes("arguments not in form 'Var :: mode'")
	).


:- pred parse_tabling_pragma(module_name, string, eval_method, list(term), 
		term, maybe1(item)).
:- mode parse_tabling_pragma(in, in, in, in, in, out) is det.

parse_tabling_pragma(ModuleName, PragmaName, TablingType, PragmaTerms, 
		ErrorTerm, Result) :-
    (
        PragmaTerms = [PredAndModesTerm0]
    ->
	string__append_list(["`:- pragma ", PragmaName, "' declaration"],
		ParseMsg),
	parse_arity_or_modes(ModuleName, PredAndModesTerm0,
		ErrorTerm, ParseMsg, ArityModesResult),
        (
	    ArityModesResult = ok(arity_or_modes(PredName,
	    	Arity, MaybePredOrFunc, MaybeModes)),
            Result = ok(pragma(tabled(TablingType, PredName, Arity, 
                MaybePredOrFunc, MaybeModes)))
	;
	    ArityModesResult = error(Msg, Term),
	    Result = error(Msg, Term)
	)
    ;
    	string__append_list(["wrong number of arguments in `:- pragma ", 
            PragmaName, "' declaration"], ErrorMessage),
        Result = error(ErrorMessage, ErrorTerm)
    ).

:- type arity_or_modes
	--->	arity_or_modes(sym_name, arity,
			maybe(pred_or_func), maybe(list(mode))).

:- pred parse_arity_or_modes(module_name, term, term,
		string, maybe1(arity_or_modes)).
:- mode parse_arity_or_modes(in, in, in, in, out) is det.

parse_arity_or_modes(ModuleName, PredAndModesTerm0,
		ErrorTerm, ErrorMsg, Result) :-
	(
                % Is this a simple pred/arity pragma
            PredAndModesTerm0 = term__functor(term__atom("/"),
                [PredNameTerm, ArityTerm], _)
        ->
            (
                parse_implicitly_qualified_term(ModuleName,
			PredNameTerm, PredAndModesTerm0, "", ok(PredName, [])),
                ArityTerm = term__functor(term__integer(Arity), [], _)
            ->
		Result = ok(arity_or_modes(PredName, Arity, no, no))
            ;
                string__append("expected predname/arity for", ErrorMsg, Msg),
                Result = error(Msg, ErrorTerm)
            )
        ;
	    parse_pred_or_func_and_arg_modes(yes(ModuleName),
	    	PredAndModesTerm0, PredAndModesTerm0, ErrorMsg,
		PredAndModesResult),
	    (
	    	PredAndModesResult = ok(PredName - PredOrFunc, Modes),
                list__length(Modes, Arity0),
                ( PredOrFunc = function ->
                    Arity is Arity0 - 1
                ;
                    Arity = Arity0
                ),
	    	Result = ok(arity_or_modes(PredName, Arity,
			yes(PredOrFunc), yes(Modes)))
            ;
		PredAndModesResult = error(Msg, Term),
                Result = error(Msg, Term)
            )
	).

:- type maybe_pred_or_func_modes ==
		maybe2(pair(sym_name, pred_or_func), list(mode)).

:- pred parse_pred_or_func_and_arg_modes(maybe(module_name), term, term,
		string, maybe_pred_or_func_modes).
:- mode parse_pred_or_func_and_arg_modes(in, in, in, in, out) is det.

parse_pred_or_func_and_arg_modes(MaybeModuleName, PredAndModesTerm,
		ErrorTerm, Msg, Result) :-
	parse_pred_or_func_and_args(MaybeModuleName, PredAndModesTerm,
		ErrorTerm, Msg, PredAndArgsResult),
	(
	    PredAndArgsResult =
		ok(PredName, ArgModeTerms - MaybeRetModeTerm),
	    (
	    	convert_mode_list(allow_constrained_inst_var, ArgModeTerms,
	    		ArgModes0)
	    ->
		(
		    MaybeRetModeTerm = yes(RetModeTerm),
		    (
		    	convert_mode(allow_constrained_inst_var, RetModeTerm,
				RetMode)
		    ->
			list__append(ArgModes0, [RetMode], ArgModes1),
			list__map(constrain_inst_vars_in_mode, ArgModes1,
			    ArgModes),
			Result = ok(PredName - function, ArgModes)
		    ;
			string__append("error in return mode in ",
				Msg, ErrorMsg),
		    	Result = error(ErrorMsg, ErrorTerm)
		    )
		;
		    MaybeRetModeTerm = no,
		    Result = ok(PredName - predicate, ArgModes0)
		)
	    ;
		string__append("error in argument modes in ", Msg,
			ErrorMsg),
	    	Result = error(ErrorMsg, ErrorTerm)
	    )
	;
		PredAndArgsResult = error(ErrorMsg, Term),
		Result = error(ErrorMsg, Term)
	).

:- pred convert_bool_list(term::in, list(bool)::out) is semidet.

convert_bool_list(ListTerm, Bools) :-
	convert_list(ListTerm,
		(pred(Term::in, Bool::out) is semidet :-
			Term = term__functor(term__atom(Name), [], _),
			( Name = "yes", Bool = yes
			; Name = "no", Bool = no
			)
		),
		ok(Bools)).

:- pred convert_int_list(term::in, maybe1(list(int))::out) is det.

convert_int_list(ListTerm, Result) :-
	convert_list(ListTerm,
		lambda([Term::in, Int::out] is semidet, (
			Term = term__functor(term__integer(Int), [], _)
		)), Result).

	%
	% convert_list(T, P, M) will convert a term T into a list of
	% type X where P is a predicate that converts each element of
	% the list into the correct type.  M will hold the list if the
	% conversion succeded for each element of M, otherwise it will
	% hold the error.
	%
:- pred convert_list(term, pred(term, T), maybe1(list(T))).
:- mode convert_list(in, pred(in, out) is semidet, out) is det.

convert_list(term__variable(V),_, error("variable in list", term__variable(V))).
convert_list(term__functor(Functor, Args, Context), Pred, Result) :-
	( 
		Functor = term__atom("[|]"),
		Args = [Term, RestTerm],
		call(Pred, Term, Element)
	->	
		convert_list(RestTerm, Pred, RestResult),
		(
			RestResult = ok(List0),
			Result = ok([Element | List0])
		;
			RestResult = error(_, _),
			Result = RestResult
		)
	;
		Functor = term__atom("[]"),
		Args = []
	->
		Result = ok([])
	;
		Result = error("error in list",
				term__functor(Functor, Args, Context))
	).

:- pred convert_type_spec_pair(term::in, pair(tvar, type)::out) is semidet.

convert_type_spec_pair(Term, TypeSpec) :-
	Term = term__functor(term__atom("="), [TypeVarTerm, SpecTypeTerm0], _),
	TypeVarTerm = term__variable(TypeVar0),
	term__coerce_var(TypeVar0, TypeVar),
	convert_type(SpecTypeTerm0, SpecType),
	TypeSpec = TypeVar - SpecType.

