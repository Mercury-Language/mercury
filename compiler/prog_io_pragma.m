%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_pragma.m.
% Main authors: fjh, dgj.
%
% This module handles the parsing of pragma directives.

:- module prog_io_pragma.

:- interface.

:- import_module prog_data, prog_io_util.
:- import_module list, varset, term.

	% parse the pragma declaration. 
:- pred parse_pragma(module_name, varset, list(term), maybe1(item)).
:- mode parse_pragma(in, in, in, out) is semidet.

:- implementation.

:- import_module prog_io, prog_io_goal, prog_util.
:- import_module term_util, term_errors.
:- import_module int, map, string, std_util, bool, require.

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

parse_pragma_type(_, "c_header_code", PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
    	(
       	    PragmaTerms = [HeaderTerm]
        ->
	    (
    	        HeaderTerm = term__functor(term__string(HeaderCode), [], _)
	    ->
	        Result = ok(pragma(c_header_code(HeaderCode)))
	    ;
		Result = error("expected string for C header code", HeaderTerm)
	    )
	;
	    Result = error(
"wrong number of arguments in `:- pragma c_header_code' declaration", 
			    ErrorTerm)
        ).

parse_pragma_type(ModuleName, "c_code", PragmaTerms,
			ErrorTerm, VarSet, Result) :-
	(
    	    PragmaTerms = [Just_C_Code_Term]
	->
	    (
		Just_C_Code_Term = term__functor(term__string(Just_C_Code), [],
			_)
	    ->
	        Result = ok(pragma(c_code(Just_C_Code)))
	    ;
		Result = error("expected string for C code", Just_C_Code_Term)
	    )
	;
    	    PragmaTerms = [PredAndVarsTerm, C_CodeTerm]
	->
	    % XXX we should issue a warning; this syntax is deprecated.
	    % Result = error("pragma c_code doesn't say whether it can call mercury", PredAndVarsTerm)
		    % may_call_mercury is a conservative default.
	    default_attributes(Attributes),
	    (
		C_CodeTerm = term__functor(term__string(C_Code), [], Context)
	    ->
	        parse_pragma_c_code(ModuleName, Attributes, PredAndVarsTerm,
	    	    ordinary(C_Code, yes(Context)), VarSet, Result)
	    ;
		Result = error("invalid `:- pragma c_code' declaration -- expecting either `may_call_mercury' or `will_not_call_mercury', and a string for C code",
		    C_CodeTerm)
	    )
	;
    	    PragmaTerms = [PredAndVarsTerm, FlagsTerm, C_CodeTerm]
	->
	    (
		C_CodeTerm = term__functor(term__string(C_Code), [], Context)
	    ->
		( parse_pragma_c_code_attributes_term(FlagsTerm, Flags) ->
	            parse_pragma_c_code(ModuleName, Flags, PredAndVarsTerm,
			ordinary(C_Code, yes(Context)), VarSet, Result)
	        ; parse_pragma_c_code_attributes_term(PredAndVarsTerm, Flags) ->
		    % XXX we should issue a warning; this syntax is deprecated
	            parse_pragma_c_code(ModuleName, Flags, FlagsTerm,
			ordinary(C_Code, yes(Context)), VarSet, Result)
	        ;
		    Result = error("invalid second argument in `:- pragma c_code' declaration -- expecting a C code attribute or list of attributes'",
			FlagsTerm)
		)
	    ;
		Result = error("invalid third argument in `:- pragma c_code' declaration -- expecting string for C code",
		    C_CodeTerm)
	    )
	;
	    (
    	        PragmaTerms = [PredAndVarsTerm, FlagsTerm,
		    FieldsTerm, FirstTerm, LaterTerm],
		term__context_init(DummyContext),
		SharedTerm = term__functor(term__atom("common_code"),
			[term__functor(term__string(""), [], DummyContext)],
			DummyContext)
	    ;
    	        PragmaTerms = [PredAndVarsTerm, FlagsTerm,
		    FieldsTerm, FirstTerm, LaterTerm, SharedTerm]
	    )
	->
	    ( parse_pragma_c_code_attributes_term(FlagsTerm, Flags) ->
	        ( parse_pragma_keyword("local_vars", FieldsTerm, Fields,
			FieldsContext) ->
	            ( parse_pragma_keyword("first_code", FirstTerm, First,
		    		FirstContext) ->
	                ( parse_pragma_keyword("retry_code", LaterTerm, Later,
				LaterContext) ->
	                    ( parse_pragma_keyword("shared_code", SharedTerm,
			    		Shared, SharedContext) ->
	        	        parse_pragma_c_code(ModuleName, Flags,
				    PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					share, Shared, yes(SharedContext)),
				    VarSet, Result)
		            ; parse_pragma_keyword("duplicated_code",
			    		SharedTerm, Shared, SharedContext) ->
	        	        parse_pragma_c_code(ModuleName, Flags,
				    PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					duplicate, Shared, yes(SharedContext)),
				    VarSet, Result)
		            ; parse_pragma_keyword("common_code", SharedTerm,
			    		Shared, SharedContext) ->
	        	        parse_pragma_c_code(ModuleName, Flags,
				    PredAndVarsTerm,
				    nondet(Fields, yes(FieldsContext),
				    	First, yes(FirstContext),
					Later, yes(LaterContext),
					automatic, Shared, yes(SharedContext)),
				    VarSet, Result)
		            ;
		                Result = error("invalid sixth argument in `:- pragma c_code' declaration -- expecting `common_code(<code>)'",
			            LaterTerm)
			    )
		        ;
		            Result = error("invalid fifth argument in `:- pragma c_code' declaration -- expecting `retry_code(<code>)'",
			        LaterTerm)
			)
		    ;
		        Result = error("invalid fourth argument in `:- pragma c_code' declaration -- expecting `first_code(<code>)'",
			    FirstTerm)
		    )
		;
		    Result = error("invalid third argument in `:- pragma c_code' declaration -- expecting `local_vars(<fields>)'",
			FieldsTerm)
		)
	    ;
		Result = error("invalid second argument in `:- pragma c_code' declaration -- expecting pragma c_code attribute or list of attributes'",
			FlagsTerm)
	    )
	;
	    Result = error(
	    "wrong number of arguments in `:- pragma c_code' declaration", 
		    ErrorTerm)
	).

parse_pragma_type(ModuleName, "import", PragmaTerms,
			ErrorTerm, _VarSet, Result) :-
	(
	    (
		PragmaTerms = [PredAndModesTerm, FlagsTerm, C_FunctionTerm],
		( parse_pragma_c_code_attributes_term(FlagsTerm, Flags) ->
			FlagsResult = ok(Flags)
		;
			FlagsResult = error("invalid second argument in `:- pragma import/3' declaration -- expecting C code attribute or list of attributes'",
					FlagsTerm)
	        )
	    ;
		PragmaTerms = [PredAndModesTerm, C_FunctionTerm],
		default_attributes(Flags),
		FlagsResult = ok(Flags)
	    )	
 	-> 
	    (
		C_FunctionTerm = term__functor(term__string(C_Function), [], _)
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
				ArgModes, Attributes, C_Function)))
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
	"expected pragma import(PredName(ModeList), C_Function)",
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
       (
	    PragmaTerms = [PredAndModesTerm, C_FunctionTerm]
       ->
	    (
	        C_FunctionTerm = term__functor(term__string(C_Function), [], _)
	    ->
		parse_pred_or_func_and_arg_modes(no, PredAndModesTerm,
			ErrorTerm, "`:- pragma export' declaration",
			PredAndModesResult),
		(
			PredAndModesResult = ok(PredName - PredOrFunc, Modes),
		    	Result = ok(pragma(export(PredName, PredOrFunc,
					Modes, C_Function)))
		;    
			PredAndModesResult = error(Msg, Term),
			Result = error(Msg, Term)
		)
	    ;
	    	Result = error(
		     "expected pragma export(PredName(ModeList), C_Function)",
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
				MaybeModes, TypeSubn, TVarSet)))
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
	PredAndArityTerm = term__functor(term__atom("/"), 
		[PredNameTerm, ArityTerm], _)
    ->
	(
	    parse_implicitly_qualified_term(ModuleName,
		PredNameTerm, ErrorTerm, "", ok(PredName, [])),
	    ArityTerm = term__functor(term__integer(Arity), [], _)
	->
	    Result = ok(PredName, Arity)
	;
	    string__append_list(
		["expected predname/arity for `:- pragma ",
		 PragmaType, "' declaration"], ErrorMsg),
	    Result = error(ErrorMsg, PredAndArityTerm)
	)
    ;
	string__append_list(["expected predname/arity for `:- pragma ",
		 PragmaType, "' declaration"], ErrorMsg),
	Result = error(ErrorMsg, PredAndArityTerm)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_pragma_keyword(string, term, string, term__context).
:- mode parse_pragma_keyword(in, in, out, out) is semidet.

parse_pragma_keyword(ExpectedKeyword, Term, StringArg, StartContext) :-
	Term = term__functor(term__atom(ExpectedKeyword), [Arg], _),
	Arg = term__functor(term__string(StringArg), [], StartContext).

%-----------------------------------------------------------------------------%

:- type collected_pragma_c_code_attribute
	--->	may_call_mercury(may_call_mercury)
	;	thread_safe(thread_safe)
	.

:- pred parse_pragma_c_code_attributes_term(term, pragma_c_code_attributes).
:- mode parse_pragma_c_code_attributes_term(in, out) is semidet.

parse_pragma_c_code_attributes_term(Term, Attributes) :-
	default_attributes(Attributes0),
	parse_pragma_c_code_attributes_term0(Term, AttrList),
	( list__member(may_call_mercury(will_not_call_mercury), AttrList) ->
		( list__member(may_call_mercury(may_call_mercury), AttrList) ->
			% XXX an error message would be nice
			fail
		;
			set_may_call_mercury(Attributes0,
				will_not_call_mercury, Attributes1)
		)
	;
		Attributes1 = Attributes0
	),
	( list__member(thread_safe(thread_safe), AttrList) ->
		( list__member(thread_safe(not_thread_safe), AttrList) ->
			% XXX an error message would be nice
			fail
		;
			set_thread_safe(Attributes1, thread_safe, Attributes)
		)
	;
		Attributes = Attributes1
	).

:- pred parse_pragma_c_code_attributes_term0(term,
		list(collected_pragma_c_code_attribute)).
:- mode parse_pragma_c_code_attributes_term0(in, out) is semidet.

parse_pragma_c_code_attributes_term0(Term, Flags) :-
	(
		parse_single_pragma_c_code_attribute(Term, Flag)
	->
		Flags = [Flag]
	;
		(
			Term = term__functor(term__atom("[]"), [], _),
			Flags = []
		;
			Term = term__functor(term__atom("."), [Hd, Tl], _),
			Flags = [Flag|Flags0],
			parse_single_pragma_c_code_attribute(Hd, Flag),
			parse_pragma_c_code_attributes_term0(Tl, Flags0)
		)
	).

:- pred parse_single_pragma_c_code_attribute(term,
		collected_pragma_c_code_attribute).
:- mode parse_single_pragma_c_code_attribute(in, out) is semidet.

parse_single_pragma_c_code_attribute(Term, Flag) :-
	( parse_may_call_mercury(Term, MayCallMercury) ->
		Flag = may_call_mercury(MayCallMercury)
	; parse_threadsafe(Term, ThreadSafe) ->
		Flag = thread_safe(ThreadSafe)
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

% parse a pragma c_code declaration

:- pred parse_pragma_c_code(module_name, pragma_c_code_attributes, term,
	pragma_c_code_impl, varset, maybe1(item)).
:- mode parse_pragma_c_code(in, in, in, in, in, out) is det.

parse_pragma_c_code(ModuleName, Flags, PredAndVarsTerm0, PragmaImpl,
	VarSet0, Result) :-
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
	    Result = ok(pragma(c_code(Flags, PredName,
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
				convert_mode(ModeTerm, Mode)
			->
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
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

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
	    ( convert_mode_list(ArgModeTerms, ArgModes0) ->
		(
		    MaybeRetModeTerm = yes(RetModeTerm),
		    ( convert_mode(RetModeTerm, RetMode) ->
			list__append(ArgModes0, [RetMode], ArgModes),
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

:- pred parse_pred_or_func_and_args(maybe(sym_name), term, term, string,
		maybe_pred_or_func(term)).
:- mode parse_pred_or_func_and_args(in, in, in, in, out) is det.

parse_pred_or_func_and_args(MaybeModuleName, PredAndArgsTerm, ErrorTerm,
		Msg, PredAndArgsResult) :-
	(
		PredAndArgsTerm = term__functor(term__atom("="),
			[FuncAndArgsTerm, FuncResultTerm], _)
	->
		FunctorTerm = FuncAndArgsTerm,
		MaybeFuncResult = yes(FuncResultTerm)
	;
		FunctorTerm = PredAndArgsTerm,
		MaybeFuncResult = no
	),
	(
		MaybeModuleName = yes(ModuleName),
		parse_implicitly_qualified_term(ModuleName, FunctorTerm,
			ErrorTerm, Msg, Result)
	;
		MaybeModuleName = no,
		parse_qualified_term(FunctorTerm, ErrorTerm, Msg, Result)
	),
	(
		Result = ok(SymName, Args),
		PredAndArgsResult = ok(SymName, Args - MaybeFuncResult)
	;
		Result = error(ErrorMsg, Term),
		PredAndArgsResult = error(ErrorMsg, Term)
	).

:- pred convert_bool_list(term::in, list(bool)::out) is semidet.

convert_bool_list(term__functor(Functor, Args, _), Bools) :-
	(
		Functor = term__atom("."),
		Args = [term__functor(AtomTerm, [], _), RestTerm],
		( 
			AtomTerm = term__atom("yes"),
			Bool = yes
		;
			AtomTerm = term__atom("no"),
			Bool = no
		),
		convert_bool_list(RestTerm, RestList),
		Bools = [ Bool | RestList ]
	;
		Functor = term__atom("[]"),
		Args = [],
		Bools = []
	).

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
		Functor = term__atom("."),
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
	term__coerce(SpecTypeTerm0, SpecType),
	TypeSpec = TypeVar - SpecType.

