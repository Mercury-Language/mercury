%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: recompilation_check.m
% Main author: stayl
%
% Check whether a module should be recompiled.
%-----------------------------------------------------------------------------%
:- module recompilation__check.

:- interface.

:- import_module parse_tree__modules, parse_tree__prog_io.
:- import_module parse_tree__prog_data.
:- import_module list, io.

:- type modules_to_recompile
	--->	(all)
	;	some(list(module_name))
	.

:- type find_target_file_names ==
		pred(module_name, list(file_name), io__state, io__state).
:- inst find_target_file_names ==
		(pred(in, out, di, uo) is det).

:- type find_timestamp_file_names ==
		pred(module_name, list(file_name), io__state, io__state).
:- inst find_timestamp_file_names ==
		(pred(in, out, di, uo) is det).

	% recompilation__check__should_recompile(ModuleName, FindTargetFiles,
	%	FindTimestampFiles, ModulesToRecompile, ReadModules)
	%
	% Process the `.used'  files for the given module and all its
	% inline sub-modules to find out which modules need to be recompiled.
	% `FindTargetFiles' takes a module name and returns a list of
	% file names which need to be up-to-date to avoid recompilation.
	% `FindTimestampFiles' takes a module name and returns a list of
	% file names which should be touched if the module does not need
	% to be recompiled.
	% `ReadModules' is the list of interface files read during
	% recompilation checking, returned to avoid rereading them
	% if recompilation is required.
:- pred recompilation__check__should_recompile(module_name::in,
	find_target_file_names::in(find_target_file_names),
	find_timestamp_file_names::in(find_timestamp_file_names),
	modules_to_recompile::out, read_modules::out,
	io__state::di, io__state::uo) is det.	

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module recompilation__usage.
:- import_module recompilation__version.
:- import_module libs__timestamp.
:- import_module parse_tree__prog_io_util, parse_tree__prog_util.
:- import_module parse_tree__prog_out, hlds__error_util.
:- import_module libs__globals, libs__options.
:- import_module hlds__hlds_pred.	% for field_access_function_name,
				% type pred_id.
:- import_module hlds__hlds_data.	% for type field_access_type

:- import_module assoc_list, bool, exception, int, map, parser, require.
:- import_module set, std_util, string, term, term_io.

recompilation__check__should_recompile(ModuleName, FindTargetFiles,
		FindTimestampFiles, Info ^ modules_to_recompile,
		Info ^ read_modules) -->
	globals__io_lookup_bool_option(find_all_recompilation_reasons,
			FindAll),
	{ Info0 = recompilation_check_info(ModuleName, no, [], map__init,
			init_item_id_set(map__init, map__init, map__init),
			set__init, some([]), FindAll, []) },
	recompilation__check__should_recompile_2(no, FindTargetFiles,
		FindTimestampFiles, ModuleName, Info0, Info).
	
:- pred recompilation__check__should_recompile_2(bool::in,
	find_target_file_names::in(find_target_file_names),
	find_timestamp_file_names::in(find_timestamp_file_names),
	module_name::in, recompilation_check_info::in,
	recompilation_check_info::out, io__state::di, io__state::uo) is det.	

recompilation__check__should_recompile_2(IsSubModule, FindTargetFiles,
		FindTimestampFiles, ModuleName, Info0, Info) -->
	{ Info1 = (Info0 ^ module_name := ModuleName)
			^ sub_modules := [] },
	module_name_to_file_name(ModuleName, ".used", no, UsageFileName),
	io__open_input(UsageFileName, MaybeVersionStream),
	(
		{ MaybeVersionStream = ok(VersionStream0) },
		io__set_input_stream(VersionStream0, OldInputStream),

		promise_only_solution_io(
		    (pred(R::out, di, uo) is cc_multi -->
			try_io(
			    (pred(Info2::out, di, uo) is det -->
				recompilation__check__should_recompile_3(
					IsSubModule, FindTargetFiles,
					Info1, Info2)
		    	    ), R)
		    ),
		    Result),
		(
			{ Result = succeeded(Info3) },
			{ Reasons = Info3 ^ recompilation_reasons }
		;
			{ Result = failed },
			{ error("recompilation__check__should_recompile_2") }
		;
			{ Result = exception(Exception) },
			{ univ_to_type(Exception, RecompileException0) ->
				RecompileException = RecompileException0
			;
				rethrow(Result)
			},
			{ RecompileException =
				recompile_exception(Reason, Info3) },
			{ Reasons = [Reason] }
		),

		( { Reasons = [] } ->
			FindTimestampFiles(ModuleName, TimestampFiles),
			write_recompilation_message(
			    (pred(di, uo) is det -->
				io__write_string("Not recompiling module "),
				prog_out__write_sym_name(ModuleName),
				io__write_string(".\n")
			    )),
			list__foldl(touch_datestamp, TimestampFiles),
			{ Info4 = Info3 }
		;
			{ add_module_to_recompile(ModuleName, Info3, Info4) },
			write_recompilation_message(
			    (pred(di, uo) is det -->
				list__foldl(
					write_recompile_reason(ModuleName),
					list__reverse(Reasons))
			    ))
		),
		io__set_input_stream(OldInputStream, VersionStream),
		io__close_input(VersionStream),

		( { (all) = Info4 ^ modules_to_recompile } ->
			{ Info = Info4 }
		;
			{ Info5 = Info4 ^ is_inline_sub_module := yes },
			list__foldl2(
				recompilation__check__should_recompile_2(yes,
					FindTargetFiles, FindTimestampFiles),
				Info5 ^ sub_modules, Info5, Info)
		)
	;
		{ MaybeVersionStream = error(_) },
		write_recompilation_message(
		    (pred(di, uo) is det -->
			{ Reason = file_error(UsageFileName,
				"file `" ++ UsageFileName ++ "' not found.") },
			write_recompile_reason(ModuleName, Reason)
		    )),
		{ Info = Info1 ^ modules_to_recompile := (all) }
	).

:- pred recompilation__check__should_recompile_3(bool::in,
		find_target_file_names::in(find_target_file_names),
		recompilation_check_info::in, recompilation_check_info::out,
		io__state::di, io__state::uo) is det.

recompilation__check__should_recompile_3(IsSubModule, FindTargetFiles,
		Info0, Info) -->

	%
	% WARNING: any exceptions thrown before the sub_modules
	% field is set in the recompilation_check_info must set
	% the modules_to_recompile field to `all', or else
	% the nested sub-modules will not be checked and necessary
	% recompilations may be missed.
	%

	%
	% Check that the format of the usage file is the current format.
	%
	read_term_check_for_error_or_eof(Info0, "usage file version number",
		VersionNumberTerm),
	(
		{ VersionNumberTerm = term__functor(term__atom(","),
			[UsageFileVersionNumberTerm,
			VersionNumbersVersionNumberTerm], _) },
		{ UsageFileVersionNumberTerm = 
			term__functor(
				term__integer(usage_file_version_number),
				_, _) },
		{ VersionNumbersVersionNumberTerm = 
			term__functor(
				term__integer(version_numbers_version_number),
				_, _) }
	->
		[]
	;
		io__input_stream_name(UsageFileName),
		{ throw_syntax_error(
			file_error(UsageFileName,
				"invalid usage file version number in file `"
				++ UsageFileName ++ "'."),
			Info0) }
	), 

	%
	% Find the timestamp of the module the last time it was compiled.
	%
	read_term_check_for_error_or_eof(Info0, "module timestamp",
		TimestampTerm),
	{ parse_module_timestamp(Info0, TimestampTerm, _, ModuleTimestamp) },
	{ ModuleTimestamp = module_timestamp(_, RecordedTimestamp, _) },

	( { IsSubModule = yes } ->
		% For inline sub-modules we don't need to check
		% the module timestamp because we've already checked
		% the timestamp for the parent module.
		{ Info3 = Info0 }
	;
		%
		% If the module has changed, recompile.
		%
		{ ModuleName = Info0 ^ module_name },
		read_mod_if_changed(ModuleName, ".m", "Reading module",
			yes, RecordedTimestamp, Items, Error,
			FileName, MaybeNewTimestamp),
		{
			MaybeNewTimestamp = yes(NewTimestamp),
			NewTimestamp \= RecordedTimestamp
		->
			record_read_file(ModuleName,
				ModuleTimestamp ^ timestamp := NewTimestamp,
				Items, Error, FileName, Info0, Info1),
			Info2 = Info1 ^ modules_to_recompile := (all),
			record_recompilation_reason(module_changed(FileName),
				Info2, Info3)	
		;
			( Error \= no_module_errors
			; MaybeNewTimestamp = no
			)
		->
			throw_syntax_error(
				file_error(FileName,
				    "error reading file `"
				    ++ FileName ++ "'."),
				Info0)
		;
			Info3 = Info0
		}
	),

	%
	% Find out whether this module has any inline sub-modules.
	%
	read_term_check_for_error_or_eof(Info3, "inline sub-modules",
		SubModulesTerm),
	{ 
		SubModulesTerm = term__functor(term__atom("sub_modules"),
					SubModuleTerms, _),
		list__map(
			(pred(Term::in, SubModule::out) is semidet :-
				sym_name_and_args(Term, SubModule, [])
			),
			SubModuleTerms, SubModules)
	->
		Info4 = Info3 ^ sub_modules := SubModules
	;
		Reason1 = syntax_error(get_term_context(SubModulesTerm),
				"error in sub_modules term"),
		throw_syntax_error(Reason1, Info3)
	},

	%
	% Check whether the output files are present and up-to-date.
	%
	FindTargetFiles(Info4 ^ module_name, TargetFiles),
	list__foldl2(
	    (pred(TargetFile::in, RInfo0::in, RInfo::out, di, uo) is det -->
		io__file_modification_time(TargetFile, TargetModTimeResult),
		{
			TargetModTimeResult = ok(TargetModTime),
			compare(TargetModTimeCompare,
				time_t_to_timestamp(TargetModTime),
				RecordedTimestamp),
			TargetModTimeCompare = (>)
		->
			RInfo = RInfo0
		;
			Reason2 = output_file_not_up_to_date(TargetFile),
			record_recompilation_reason(Reason2, RInfo0, RInfo)
		}
	    ), TargetFiles, Info4, Info5),

	%
	% Read in the used items, used for checking for
	% ambiguities with new items.
	%
	read_term_check_for_error_or_eof(Info5, "used items",
		UsedItemsTerm),
	{ parse_used_items(Info5, UsedItemsTerm, UsedItems) },
	{ Info6 = Info5 ^ used_items := UsedItems },

	read_term_check_for_error_or_eof(Info6, "used classes",
		UsedClassesTerm),
	{ 
		UsedClassesTerm = term__functor(term__atom("used_classes"),
					UsedClassTerms, _),
		list__map(
			(pred(Term::in, UsedClass::out) is semidet :-
				parse_name_and_arity(Term,
					ClassName, ClassArity),			
				UsedClass = ClassName - ClassArity
			), UsedClassTerms, UsedClasses)
	->
		Info7 = Info6 ^ used_typeclasses :=
					set__list_to_set(UsedClasses)
	;
		Reason3 = syntax_error(get_term_context(UsedClassesTerm),
				"error in used_typeclasses term"),
		throw_syntax_error(Reason3, Info6)
	},
	check_imported_modules(Info7, Info).


%-----------------------------------------------------------------------------%

:- pred parse_module_timestamp(recompilation_check_info::in, term::in,
		module_name::out, module_timestamp::out) is det.

parse_module_timestamp(Info, Term, ModuleName, ModuleTimestamp) :-
	conjunction_to_list(Term, Args),
	(
		Args = [ModuleNameTerm, SuffixTerm,
				TimestampTerm | MaybeOtherTerms],
		sym_name_and_args(ModuleNameTerm, ModuleName0, []),
		SuffixTerm = term__functor(term__string(Suffix), [], _),
		Timestamp = term_to_timestamp(TimestampTerm),
		(
			MaybeOtherTerms = [term__functor(term__atom("used"),
						[], _)],
			NeedQualifier = must_be_qualified
		;
			MaybeOtherTerms = [],
			NeedQualifier = may_be_unqualified
		)
	->
		ModuleName = ModuleName0,
		ModuleTimestamp = module_timestamp(Suffix,
			Timestamp, NeedQualifier)
	;	
		Reason = syntax_error(get_term_context(Term),
				"error in module timestamp"),
		throw_syntax_error(Reason, Info)
	).

%-----------------------------------------------------------------------------%

:- pred parse_used_items(recompilation_check_info::in,
		term::in, resolved_used_items::out) is det.

parse_used_items(Info, Term, UsedItems) :-
	( Term = term__functor(term__atom("used_items"), UsedItemTerms, _) ->
	 	list__foldl(parse_used_item_set(Info), UsedItemTerms,
			init_item_id_set(map__init, map__init, map__init),
			UsedItems)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in used items"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_used_item_set(recompilation_check_info::in, term::in,
	resolved_used_items::in, resolved_used_items::out) is det.

parse_used_item_set(Info, Term, UsedItems0, UsedItems) :-
	(
		Term = term__functor(term__atom(ItemTypeStr), ItemTerms, _),
		string_to_item_type(ItemTypeStr, ItemType)
	->
		( is_simple_item_type(ItemType) ->
			list__foldl(parse_simple_item(Info),
				ItemTerms, map__init, SimpleItems),
			UsedItems = update_simple_item_set(UsedItems0,
				ItemType, SimpleItems)
		; is_pred_or_func_item_type(ItemType) ->
			list__foldl(parse_pred_or_func_item(Info),
				ItemTerms, map__init, PredOrFuncItems),	
			UsedItems = update_pred_or_func_set(UsedItems0,
				ItemType, PredOrFuncItems)
		; ItemType = functor ->
			list__foldl(parse_functor_item(Info),
				ItemTerms, map__init, CtorItems),
			UsedItems = UsedItems0 ^ functors := CtorItems
		;
			Reason = syntax_error(get_term_context(Term),
				string__append(
				    "error in used items: unknown item type :",
				    ItemTypeStr)),
			throw_syntax_error(Reason, Info)
		)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in used items"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_simple_item(recompilation_check_info::in, term::in,
		simple_item_set::in, simple_item_set::out) is det.

parse_simple_item(Info, Term, Set0, Set) :-
	(
		Term = term__functor(term__atom("-"),
				[NameArityTerm, MatchesTerm], _),
		parse_name_and_arity(NameArityTerm, SymName, Arity)
	->
		unqualify_name(SymName, Name),
		conjunction_to_list(MatchesTerm, MatchTermList),
		list__foldl(parse_simple_item_match(Info),
			MatchTermList, map__init, Matches),
		map__det_insert(Set0, Name - Arity, Matches, Set)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in simple items"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_simple_item_match(recompilation_check_info::in, term::in,
		map(module_qualifier, module_name)::in,
		map(module_qualifier, module_name)::out) is det.

parse_simple_item_match(Info, Term, Items0, Items) :-
	(
		(
			Term = term__functor(term__atom("=>"),
				[QualifierTerm, ModuleNameTerm], _)
		->
			sym_name_and_args(QualifierTerm, Qualifier, []),
			sym_name_and_args(ModuleNameTerm, ModuleName, [])
		;
			sym_name_and_args(Term, ModuleName, []),
			Qualifier = ModuleName
		)
	->
		map__det_insert(Items0, Qualifier, ModuleName, Items)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in simple item match"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_pred_or_func_item(recompilation_check_info::in,
		term::in, resolved_pred_or_func_set::in,
		resolved_pred_or_func_set::out) is det.

parse_pred_or_func_item(Info, Term, Set0, Set) :-
	parse_resolved_item_set(Info, parse_pred_or_func_item_match,
		Term, Set0, Set).

:- pred parse_pred_or_func_item_match(recompilation_check_info::in, term::in,
	resolved_pred_or_func_map::in, resolved_pred_or_func_map::out) is det.

parse_pred_or_func_item_match(Info, Term, Items0, Items) :-
	invalid_pred_id(PredId),
	(
		(
			Term = term__functor(term__atom("=>"),
				[QualifierTerm, MatchesTerm], _)
		->
			sym_name_and_args(QualifierTerm, Qualifier, []),
			conjunction_to_list(MatchesTerm, MatchesList),
			list__map(
			    (pred(MatchTerm::in, Match::out) is semidet :-
				sym_name_and_args(MatchTerm, MatchName, []),
				Match = PredId - MatchName
			    ),
			    MatchesList, Matches)
		;
			sym_name_and_args(Term, Qualifier, []),
			Matches = [PredId - Qualifier]
		)
	->
		map__det_insert(Items0, Qualifier, set__list_to_set(Matches),
			Items)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in pred or func match"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_functor_item(recompilation_check_info::in, term::in,
	resolved_functor_set::in, resolved_functor_set::out) is det.

parse_functor_item(Info, Term, Set0, Set) :-
	parse_resolved_item_set(Info, parse_functor_matches, Term, Set0, Set).

:- pred parse_functor_matches(recompilation_check_info::in, term::in,
	resolved_functor_map::in, resolved_functor_map::out) is det.

parse_functor_matches(Info, Term, Map0, Map) :-
	(
		Term = term__functor(term__atom("=>"),
			[QualifierTerm, MatchesTerm], _),
		sym_name_and_args(QualifierTerm, Qualifier, [])
	->
		conjunction_to_list(MatchesTerm, MatchesList),
		list__map(parse_resolved_functor(Info),
			MatchesList, Matches),
		map__det_insert(Map0, Qualifier,
			set__list_to_set(Matches), Map)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in functor match"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_resolved_functor(recompilation_check_info::in, term::in,
		resolved_functor::out) is det.

parse_resolved_functor(Info, Term, Ctor) :-
	(
		Term = term__functor(term__atom(PredOrFuncStr),
				[ModuleTerm, ArityTerm], _),
		( PredOrFuncStr = "predicate", PredOrFunc = predicate
		; PredOrFuncStr = "function", PredOrFunc = function
		),
		sym_name_and_args(ModuleTerm, ModuleName, []),
		ArityTerm = term__functor(term__integer(Arity), [], _)
	->
		invalid_pred_id(PredId),
		Ctor = pred_or_func(PredId, ModuleName,
				PredOrFunc, Arity)
	;
		Term = term__functor(term__atom("ctor"), [NameArityTerm], _),
		parse_name_and_arity(NameArityTerm, TypeName, TypeArity)
	->
		Ctor = constructor(TypeName - TypeArity)
	;
		Term = term__functor(term__atom("field"),
			[TypeNameArityTerm, ConsNameArityTerm], _),
		parse_name_and_arity(TypeNameArityTerm, TypeName, TypeArity),
		parse_name_and_arity(ConsNameArityTerm, ConsName, ConsArity)
	->
		Ctor = field(TypeName - TypeArity, ConsName - ConsArity)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in functor match"),
		throw_syntax_error(Reason, Info)
	).

:- type parse_resolved_item_matches(T) ==
		pred(recompilation_check_info, term, resolved_item_map(T),
			resolved_item_map(T)).
:- inst parse_resolved_item_matches == (pred(in, in, in, out) is det).

:- pred parse_resolved_item_set(recompilation_check_info::in,
	parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
	term::in, resolved_item_set(T)::in, resolved_item_set(T)::out) is det.

parse_resolved_item_set(Info, ParseMatches, Term, Set0, Set) :-
	(
		Term = term__functor(term__atom("-"),
				[NameTerm, MatchesTerm], _),
		NameTerm = term__functor(term__atom(Name), [], _)
	->
		conjunction_to_list(MatchesTerm, MatchTermList),
		list__map(
			parse_resolved_item_arity_matches(Info, ParseMatches),
			MatchTermList, Matches),
		map__det_insert(Set0, Name, Matches, Set)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in resolved item matches"),
		throw_syntax_error(Reason, Info)
	).

:- pred parse_resolved_item_arity_matches(recompilation_check_info::in,
	parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
	term::in, pair(arity, resolved_item_map(T))::out) is det.

parse_resolved_item_arity_matches(Info, ParseMatches,
		Term, Arity - MatchMap) :-
	(
		Term = term__functor(term__atom("-"),
			[ArityTerm, MatchesTerm], _),
		ArityTerm = term__functor(term__integer(Arity0), [], _),
		conjunction_to_list(MatchesTerm, MatchTermList)
	->
		Arity = Arity0,
		list__foldl(
			(pred(MatchTerm::in, Map0::in, Map::out) is det :-
				ParseMatches(Info, MatchTerm, Map0, Map)
			),
			MatchTermList, map__init, MatchMap)
	;
		Reason = syntax_error(get_term_context(Term),
				"error in resolved item matches"),
		throw_syntax_error(Reason, Info)
	).

%-----------------------------------------------------------------------------%

	%
	% Check whether the interface file read for a module
	% in the last compilation has changed, and if so whether
	% the items have changed in a way which should cause
	% a recompilation.
	%
:- pred check_imported_modules(recompilation_check_info::in,
	recompilation_check_info::out, io__state::di, io__state::uo) is det.

check_imported_modules(Info0, Info) -->
	parser__read_term(TermResult),
	(
		{ TermResult = term(_, Term) },
		( { Term = term__functor(term__atom("done"), [], _) } ->
			{ Info = Info0 }	
		;
			check_imported_module(Term, Info0, Info1),
			check_imported_modules(Info1, Info)
		)
	;
		{ TermResult = error(Message, Line) },
		io__input_stream_name(FileName),
		{ Reason = syntax_error(term__context(FileName, Line),
				Message) },
		{ throw_syntax_error(Reason, Info0) }
	;
		{ TermResult = eof },
		%
		% There should always be an item `done.' at the end of
		% the list of modules to check. This is used to make
		% sure that the writing of the `.used' file was not
		% interrupted.
		%
		io__input_stream_name(FileName),
		io__get_line_number(Line),
		{ Reason = syntax_error(term__context(FileName, Line),
				"unexpected end of file") },
		{ throw_syntax_error(Reason, Info0) }
	).
	
:- pred check_imported_module(term::in, recompilation_check_info::in,
	recompilation_check_info::out, io__state::di, io__state::uo) is det.

check_imported_module(Term, Info0, Info) -->
	{
		Term = term__functor(term__atom("=>"),
			[TimestampTerm0, UsedItemsTerm0], _)
	->
		TimestampTerm = TimestampTerm0,
		MaybeUsedItemsTerm = yes(UsedItemsTerm0)
	;
		TimestampTerm = Term,
		MaybeUsedItemsTerm = no
	},
	{ parse_module_timestamp(Info0, TimestampTerm,
		ImportedModuleName, ModuleTimestamp) },

	{ ModuleTimestamp = module_timestamp(Suffix,
			RecordedTimestamp, NeedQualifier) },
	(
		%
		% If we're checking a sub-module, don't re-read
		% interface files read for other modules checked
		% during this compilation.
		%
		{ Info0 ^ is_inline_sub_module = yes },
		{ find_read_module(Info0 ^ read_modules, ImportedModuleName,
			Suffix, yes, Items0, MaybeNewTimestamp0,
			Error0, FileName0) }
	->
		{ Items = Items0 },
		{ MaybeNewTimestamp = MaybeNewTimestamp0 },
		{ Error = Error0 },
		{ FileName = FileName0 },
		{ Recorded = bool__yes }
	;
		{ Recorded = bool__no },
		read_mod_if_changed(ImportedModuleName, Suffix,
			"Reading interface file for module",
			yes, RecordedTimestamp, Items0, Error,
			FileName, MaybeNewTimestamp),
		{ strip_off_interface_decl(Items0, Items) }
	),
	{
		MaybeNewTimestamp = yes(NewTimestamp),
		NewTimestamp \= RecordedTimestamp,
		Error = no_module_errors
	->
		( Recorded = no ->
			record_read_file(ImportedModuleName,
				ModuleTimestamp ^ timestamp := NewTimestamp,
				Items, Error, FileName, Info0, Info1)
		;
			Info1 = Info0
		),
		(
			MaybeUsedItemsTerm = yes(UsedItemsTerm),
			Items = [VersionNumberItem | OtherItems],
			VersionNumberItem = module_defn(_,
				version_numbers(_, VersionNumbers)) - _
		->
			check_module_used_items(ImportedModuleName,
				NeedQualifier, RecordedTimestamp,
				UsedItemsTerm, VersionNumbers,
				OtherItems, Info1, Info)
		;
			record_recompilation_reason(module_changed(FileName),
				Info1, Info)
		)
	;
		Error \= no_module_errors
	->
		throw_syntax_error(
			file_error(FileName,
				"error reading file `" ++ FileName ++ "'."),
			Info0)
	;
		Info = Info0
	}.

:- pred check_module_used_items(module_name::in, need_qualifier::in,
	timestamp::in, term::in, version_numbers::in, item_list::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_module_used_items(ModuleName, NeedQualifier, OldTimestamp,
		UsedItemsTerm, NewVersionNumbers,
		Items) -->

	{ recompilation__version__parse_version_numbers(UsedItemsTerm,
		UsedItemsResult) },
	=(Info0),
	{
		UsedItemsResult = ok(UsedVersionNumbers)
	;
		UsedItemsResult = error(Msg, ErrorTerm),
		Reason = syntax_error(get_term_context(ErrorTerm), Msg),
		throw_syntax_error(Reason, Info0)
	},

	{ UsedVersionNumbers = version_numbers(UsedItemVersionNumbers,
				UsedInstanceVersionNumbers) },
	{ NewVersionNumbers = version_numbers(NewItemVersionNumbers,
				NewInstanceVersionNumbers) },

	%
	% Check whether any of the items which were used have changed.
	%
	list__foldl(
		check_item_version_numbers(ModuleName,
			UsedItemVersionNumbers, NewItemVersionNumbers),
		[(type), type_body, (inst), (mode), (typeclass),
			predicate, function]),

	%
	% Check whether added or modified items could cause name
	% resolution ambiguities with items which were used.
	%
	list__foldl(
		check_for_ambiguities(NeedQualifier,
			OldTimestamp, UsedItemVersionNumbers),
		Items),

	%
	% Check whether any instances of used typeclasses have been
	% added, removed or changed.
	%
	check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
		NewInstanceVersionNumbers),

	%
	% Check for new instances for used typeclasses.
	%
	{ ModuleInstances = set__sorted_list_to_set(
			map__sorted_keys(NewInstanceVersionNumbers)) },
	{ UsedInstances = set__sorted_list_to_set(
			map__sorted_keys(UsedInstanceVersionNumbers)) },

	UsedClasses =^ used_typeclasses,
	{ set__difference(set__intersect(UsedClasses, ModuleInstances),
		UsedInstances, AddedInstances) },
	( { [AddedInstance | _] = set__to_sorted_list(AddedInstances) } ->
		{ Reason1 = changed_or_added_instance(ModuleName,
				AddedInstance) },
		record_recompilation_reason(Reason1)
	;
		[]
	).

:- func make_item_id(module_name, item_type, pair(string, arity)) = item_id.

make_item_id(Module, ItemType, Name - Arity) =
		item_id(ItemType, qualified(Module, Name) - Arity).

%-----------------------------------------------------------------------------%

:- pred check_item_version_numbers(module_name::in, item_version_numbers::in,
	item_version_numbers::in, item_type::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_numbers(ModuleName, UsedVersionNumbers,
		NewVersionNumbers, ItemType) -->
	{ NewItemTypeVersionNumbers = extract_ids(NewVersionNumbers,
						ItemType) },
	map__foldl(
	    (pred(NameArity::in, UsedVersionNumber::in, in, out) is det -->
		(
			{ map__search(NewItemTypeVersionNumbers,
				NameArity, NewVersionNumber) }
		->
			( { NewVersionNumber = UsedVersionNumber } ->
				[]
			;
				{ Reason = changed_item(
					make_item_id(ModuleName, ItemType,
						NameArity)) },
				record_recompilation_reason(Reason)
			)
		;
			{ Reason = removed_item(make_item_id(ModuleName,
						ItemType, NameArity)) },
			record_recompilation_reason(Reason)
		)
	    ),
	    extract_ids(UsedVersionNumbers, ItemType)).

:- pred check_instance_version_numbers(module_name::in,
	instance_version_numbers::in, instance_version_numbers::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
		NewInstanceVersionNumbers) -->
	map__foldl(
	    (pred(ClassId::in, UsedVersionNumber::in, in, out) is det -->
	  	(
			{ map__search(NewInstanceVersionNumbers,
				ClassId, NewVersionNumber) }
		->
			( { UsedVersionNumber = NewVersionNumber } ->
				[]
			;
				{ Reason = changed_or_added_instance(
					ModuleName, ClassId) },
				record_recompilation_reason(Reason)
			)
		;
			{ Reason = removed_instance(ModuleName, ClassId) },
			record_recompilation_reason(Reason)
		)
	    ), UsedInstanceVersionNumbers).

%-----------------------------------------------------------------------------%

	%
	% For each item which has changed since the last time
	% we read the interface file, check whether it introduces
	% ambiguities with items which were used when the current
	% module was last compiled.
	%
:- pred check_for_ambiguities(need_qualifier::in, timestamp::in,
	item_version_numbers::in, item_and_context::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_ambiguities(_, _, _, clause(_, _, _, _, _) - _) -->
	{ error("check_for_ambiguities: clause") }.	
check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
			type_defn(_, Name, Params, Body, _) - _) -->
	{ Arity = list__length(Params) },
	check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
		VersionNumbers, (type), Name, Arity, NeedsCheck),
	( { NeedsCheck = yes } ->
		check_type_defn_ambiguity_with_functor(NeedQualifier,
			Name - Arity, Body)
	;
		[]
	).
check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
			inst_defn(_, Name, Params, _, _) - _) -->
	check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
		VersionNumbers, (inst), Name, list__length(Params), _).
check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
			mode_defn(_, Name, Params, _, _) - _) -->
	check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
		VersionNumbers, (mode), Name, list__length(Params), _).
check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
			typeclass(_, Name, Params, Interface, _) - _) -->
	check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
		VersionNumbers, (typeclass), Name, list__length(Params),
		NeedsCheck),
	( { NeedsCheck = yes, Interface = concrete(Methods) } ->
		list__foldl(
		    (pred(ClassMethod::in, in, out) is det -->
		    	(
				{ ClassMethod = pred_or_func(_, _, _,
					PredOrFunc, MethodName, MethodArgs,
					MethodWithType, _, _, _, _, _, _) },
				check_for_pred_or_func_item_ambiguity(yes,
					NeedQualifier, OldTimestamp,
					VersionNumbers, PredOrFunc,
					MethodName, MethodArgs, MethodWithType)
			;
				{ ClassMethod = pred_or_func_mode(_, _, _, _,
					_, _, _, _) }
		    	)
		    ),
		    Methods)
	;
		[]
	).
check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
		pred_or_func(_, _, _, PredOrFunc, Name, Args,
			WithType, _, _, _, _, _) - _)
		-->
	check_for_pred_or_func_item_ambiguity(no, NeedQualifier, OldTimestamp,
		VersionNumbers, PredOrFunc, Name, Args, WithType).
check_for_ambiguities(_, _, _,
		pred_or_func_mode(_, _, _, _, _, _, _) - _) --> [].
check_for_ambiguities(_, _, _, pragma(_) - _) --> [].
check_for_ambiguities(_, _, _, promise(_, _, _, _) - _) --> [].
check_for_ambiguities(_, _, _, module_defn(_, _) - _) --> [].
check_for_ambiguities(_, _, _, instance(_, _, _, _, _, _) - _) --> [].
check_for_ambiguities(_, _, _, nothing(_) - _) --> [].

:- pred item_is_new_or_changed(timestamp::in, item_version_numbers::in,
		item_type::in, sym_name::in, arity::in) is semidet.

item_is_new_or_changed(UsedFileTimestamp, UsedVersionNumbers,
		ItemType, SymName, Arity) :-
	unqualify_name(SymName, Name),
	(
		map__search(extract_ids(UsedVersionNumbers, ItemType),
			Name - Arity, UsedVersionNumber)
	->
		% XXX This assumes that version numbers are timestamps.
		compare((>), UsedVersionNumber, UsedFileTimestamp)
	;
		true
	).

:- pred check_for_simple_item_ambiguity(need_qualifier::in, timestamp::in,
	item_version_numbers::in, item_type::in(simple_item), sym_name::in,
	arity::in, bool::out, recompilation_check_info::in,
	recompilation_check_info::out) is det.

check_for_simple_item_ambiguity(NeedQualifier, UsedFileTimestamp,
		VersionNumbers, ItemType, SymName, Arity, NeedsCheck) -->
	(
		{ item_is_new_or_changed(UsedFileTimestamp, VersionNumbers,
			ItemType, SymName, Arity) }
	->
		{ NeedsCheck = yes },
		UsedItems =^ used_items,
		{ UsedItemMap = extract_simple_item_set(UsedItems, ItemType) },
		{ unqualify_name(SymName, Name) },
		(
			{ map__search(UsedItemMap, Name - Arity,
				MatchingQualifiers) }
		->
			map__foldl(	
				check_for_simple_item_ambiguity_2(
					ItemType, NeedQualifier,
						SymName, Arity),
					MatchingQualifiers)	
		;
			[]
		)
	;
		{ NeedsCheck = no }
	).

:- pred check_for_simple_item_ambiguity_2(item_type::in, need_qualifier::in,
	sym_name::in, arity::in, module_qualifier::in, module_name::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity_2(ItemType, NeedQualifier,
		SymName, Arity, OldModuleQualifier, OldMatchingModuleName) -->
	{ unqualify_name(SymName, Name) },
	(
		% XXX This is a bit conservative in the
		% case of partially qualified names but that
		% hopefully won't come up too often.
		{ NeedQualifier = must_be_qualified },
		{ OldModuleQualifier = unqualified("") }
	->
		[]
	;
		{ QualifiedName = module_qualify_name(OldModuleQualifier,
						Name) },
		{ match_sym_name(QualifiedName, SymName) },
		\+ { SymName = qualified(OldMatchingModuleName, _) }
	->
		{ OldMatchingName = qualified(OldMatchingModuleName, Name) },
		{ Reason = item_ambiguity(item_id(ItemType, SymName - Arity), 
				[item_id(ItemType, OldMatchingName - Arity)]
			) },
		record_recompilation_reason(Reason)
	;
		[]
	).

:- pred check_for_pred_or_func_item_ambiguity(bool::in, need_qualifier::in,
	timestamp::in, item_version_numbers::in, pred_or_func::in,
	sym_name::in, list(type_and_mode)::in, maybe(type)::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity(NeedsCheck, NeedQualifier, OldTimestamp,
		VersionNumbers, PredOrFunc, SymName, Args, WithType) -->
	{
		WithType = no,
		adjust_func_arity(PredOrFunc, Arity, list__length(Args))
	;
		WithType = yes(_),
		Arity = list__length(Args)
	},
	{ ItemType = pred_or_func_to_item_type(PredOrFunc) },
	(
		{ NeedsCheck = yes
		; item_is_new_or_changed(OldTimestamp, VersionNumbers,
				ItemType, SymName, Arity)
		}
	->
		UsedItems =^ used_items,
		{ UsedItemMap = extract_pred_or_func_set(UsedItems,
					ItemType) },	
		{ unqualify_name(SymName, Name) },
		( { map__search(UsedItemMap, Name, MatchingArityList) } ->
		    list__foldl(
			(pred((MatchArity - MatchingQualifiers)::in,
					in, out) is det -->
			    (
			    	{
				    WithType = yes(_),
				    MatchArity >= Arity
				;
				    WithType = no,
				    MatchArity = Arity
				}
			    ->
				map__foldl(	
				    check_for_pred_or_func_item_ambiguity_2(
					ItemType, NeedQualifier,
					SymName, MatchArity),
				    MatchingQualifiers)
			    ;
			    	[]
			    )
			), MatchingArityList)
		;
		    []
		),

		{ invalid_pred_id(PredId) },
		( { SymName = qualified(ModuleName, _) } ->
			{
				WithType = yes(_),
				% We don't know the actual arity.
				AritiesToMatch = any
			;
				WithType = no,
				AritiesToMatch = less_than_or_equal(Arity)
			},
			check_functor_ambiguities(NeedQualifier,
				SymName, AritiesToMatch,
				pred_or_func(PredId, ModuleName,
					PredOrFunc, Arity))
		;
			{ error(
	"check_for_pred_or_func_item_ambiguity: unqualified predicate name") }
		)
	;
		[]
	).

:- pred check_for_pred_or_func_item_ambiguity_2(item_type::in,
	need_qualifier::in, sym_name::in, arity::in, module_qualifier::in,
	set(pair(pred_id, module_name))::in, recompilation_check_info::in,
	recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_2(ItemType, NeedQualifier,
		SymName, Arity, OldModuleQualifier, OldMatchingModuleNames) -->
	{ unqualify_name(SymName, Name) },
	(
		% XXX This is a bit conservative in the
		% case of partially qualified names but that
		% hopefully won't come up too often.
		{ NeedQualifier = must_be_qualified },
		{ OldModuleQualifier = unqualified("") }
	->
		[]
	;
		{ QualifiedName = module_qualify_name(OldModuleQualifier,
						Name) },
		{ match_sym_name(QualifiedName, SymName) },
		\+ {
			SymName = qualified(PredModuleName, _),
			set__member(_ - PredModuleName,
				OldMatchingModuleNames)
		}
	->
		{ AmbiguousDecls = list__map(
		    (func(_ - OldMatchingModule) = Item :-
			OldMatchingName = qualified(OldMatchingModule, Name),
			Item = item_id(ItemType, OldMatchingName - Arity)
		    ),
		    set__to_sorted_list(OldMatchingModuleNames)) },
		{ Reason = item_ambiguity(item_id(ItemType, SymName - Arity), 
				AmbiguousDecls
		) },
		record_recompilation_reason(Reason)
	;
		[]
	).

	%
	% Go over the constructors for a type which has changed
	% and check whether any of them could create an ambiguity
	% with functors used during the last compilation.
	%
:- pred check_type_defn_ambiguity_with_functor(need_qualifier::in,
		type_ctor::in, type_defn::in, recompilation_check_info::in,
		recompilation_check_info::out) is det.

check_type_defn_ambiguity_with_functor(_, _, abstract_type) --> [].
check_type_defn_ambiguity_with_functor(_, _, eqv_type(_)) --> [].
check_type_defn_ambiguity_with_functor(NeedQualifier,
			TypeCtor, du_type(Ctors, _)) -->
	list__foldl(check_functor_ambiguities(NeedQualifier, TypeCtor),
		Ctors).

:- pred check_functor_ambiguities(need_qualifier::in, type_ctor::in,
		constructor::in, recompilation_check_info::in,
		recompilation_check_info::out) is det.

check_functor_ambiguities(NeedQualifier, TypeCtor,
		ctor(_, _, Name, Args)) -->
	{ ResolvedCtor = constructor(TypeCtor) },
	{ Arity = list__length(Args) },
	check_functor_ambiguities(NeedQualifier, Name, exact(Arity),
		ResolvedCtor),
	list__foldl(
		check_field_ambiguities(NeedQualifier,
			field(TypeCtor, Name - Arity)),
		Args).

:- pred check_field_ambiguities(need_qualifier::in, resolved_functor::in,
		constructor_arg::in, recompilation_check_info::in,
		recompilation_check_info::out) is det.

check_field_ambiguities(_, _, no - _) --> [].
check_field_ambiguities(NeedQualifier, ResolvedCtor, yes(FieldName) - _) -->
	%
	% XXX The arities to match below will need to change if we ever
	% allow taking the address of field access functions.
	%
	{ field_access_function_name(get, FieldName, ExtractFuncName) },
	check_functor_ambiguities(NeedQualifier, ExtractFuncName,
		exact(1), ResolvedCtor),
	{ field_access_function_name(set, FieldName, UpdateFuncName) },
	check_functor_ambiguities(NeedQualifier, UpdateFuncName,
		exact(2), ResolvedCtor).

	%
	% Predicates and functions used as functors can match
	% any arity less than or equal to the predicate or function's
	% arity.
	%
:- type functor_match_arity
	--->	exact(arity)
	;	less_than_or_equal(arity)
	;	any
	.

:- pred check_functor_ambiguities(need_qualifier::in, sym_name::in,
	functor_match_arity::in, resolved_functor::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities(NeedQualifier, Name, MatchArity, ResolvedCtor) -->
	UsedItems =^ used_items,
	{ unqualify_name(Name, UnqualName) },
	{ UsedCtors = UsedItems ^ functors },	
	( { map__search(UsedCtors, UnqualName, UsedCtorAL) } ->
		check_functor_ambiguities_2(NeedQualifier, Name, MatchArity, 
			ResolvedCtor, UsedCtorAL)
	;
		[]
	).

:- pred check_functor_ambiguities_2(need_qualifier::in, sym_name::in,
	functor_match_arity::in, resolved_functor::in,
	assoc_list(arity, resolved_functor_map)::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_2(_, _, _, _, []) --> [].
check_functor_ambiguities_2(NeedQualifier, Name, MatchArity,
		ResolvedCtor, [Arity - UsedCtorMap | UsedCtorAL]) -->
	(
		{ MatchArity = exact(ArityToMatch) },
		{ ArityToMatch = Arity ->
			Check = bool__yes,
			Continue = bool__no
		;
			Check = no,
			( Arity < ArityToMatch ->
				Continue = yes
			;
				Continue = no
			)
		}
	;
		{ MatchArity = less_than_or_equal(ArityToMatch) },
		{ Arity =< ArityToMatch ->
			Check = yes,
			Continue = yes
		;
			Check = no,
			Continue = no
		}
	;
		{ MatchArity = any },
		{ Check = yes },
		{ Continue = yes }
	),
	( { Check = yes } ->
		map__foldl(
			check_functor_ambiguity(NeedQualifier,
				Name, Arity, ResolvedCtor),
			UsedCtorMap)
	;
		[]	
	),
	( { Continue = yes } ->
		check_functor_ambiguities_2(NeedQualifier, Name, MatchArity,
			ResolvedCtor, UsedCtorAL)
	;
		[]
	).

:- pred check_functor_ambiguity(need_qualifier::in,
	sym_name::in, arity::in, resolved_functor::in, 
	module_qualifier::in, set(resolved_functor)::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguity(NeedQualifier, SymName, Arity, ResolvedCtor,
		OldModuleQualifier, OldResolvedCtors) -->
	(
		% XXX This is a bit conservative in the
		% case of partially qualified names but that
		% hopefully won't come up too often.
		{ NeedQualifier = must_be_qualified },
		{ OldModuleQualifier = unqualified("") }
	->
		[]
	;
		{ unqualify_name(SymName, Name) },
		{ OldName = module_qualify_name(OldModuleQualifier, Name) },
		{ match_sym_name(OldName, SymName) },
		\+ { set__member(ResolvedCtor, OldResolvedCtors) }
	->
		{ Reason = functor_ambiguity(
				module_qualify_name(OldModuleQualifier, Name),
				Arity,
				ResolvedCtor,
				set__to_sorted_list(OldResolvedCtors)
			) },
		record_recompilation_reason(Reason)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- type recompilation_check_info
	---> recompilation_check_info(
		module_name :: module_name,
		is_inline_sub_module :: bool,
		sub_modules :: list(module_name),
		read_modules :: read_modules,
		used_items :: resolved_used_items,
		used_typeclasses :: set(item_name),
		modules_to_recompile :: modules_to_recompile,
		collect_all_reasons :: bool,
		recompilation_reasons :: list(recompile_reason)
	).

:- type recompile_exception
	---> recompile_exception(
		recompile_reason,
		recompilation_check_info
	).

:- type recompile_reason
	--->	file_error(
			file_name,
			string
		)

	;	output_file_not_up_to_date(
			file_name
		)
	
	;	syntax_error(
			term__context,
			string
		)

	;	module_changed(
			file_name
		)

	;	item_ambiguity(
			item_id,		% new item.
			list(item_id)		% ambiguous declarations.
		)

	;	functor_ambiguity(
			sym_name,
			arity,
			resolved_functor,	% new item.
			list(resolved_functor)	
						% ambiguous declarations.
		)

	;	changed_item(
			item_id
		)

	;	removed_item(
			item_id
		)

	;	changed_or_added_instance(
			module_name,
			item_name		% class name
		)

	;	removed_instance(
			module_name,
			item_name		% class name
		)
	.

:- pred add_module_to_recompile(module_name::in, recompilation_check_info::in,
		recompilation_check_info::out) is det.

add_module_to_recompile(Module, Info0, Info) :-
	ModulesToRecompile0 = Info0 ^ modules_to_recompile,
	(
		ModulesToRecompile0 = (all),
		Info = Info0
	;
		ModulesToRecompile0 = some(Modules0),
		Info = Info0 ^ modules_to_recompile :=
				some([Module | Modules0])
	).

:- pred record_read_file(module_name::in, module_timestamp::in, item_list::in,
		module_error::in, file_name::in, recompilation_check_info::in,
		recompilation_check_info::out) is det.

record_read_file(ModuleName, ModuleTimestamp, Items, Error, FileName) -->
	Imports0 =^ read_modules,
	{ map__set(Imports0, ModuleName - ModuleTimestamp ^ suffix,
		read_module(ModuleTimestamp, Items, Error, FileName),
		Imports) },
	^ read_modules := Imports.

%-----------------------------------------------------------------------------%

:- pred write_recompilation_message(pred(io__state, io__state),
		io__state, io__state).
:- mode write_recompilation_message(pred(di, uo) is det, di, uo) is det.

write_recompilation_message(P) -->
	globals__io_lookup_bool_option(verbose_recompilation, Verbose),
	( { Verbose = yes } ->
		P
	;
		[]
	).

:- pred write_recompile_reason(module_name::in, recompile_reason::in,
		io__state::di, io__state::uo) is det.

write_recompile_reason(ModuleName, Reason) -->
	{ recompile_reason_message(Reason, MaybeContext, ErrorPieces0) },
	{ ErrorPieces =
		[words("Recompiling module"),
		words(string__append(describe_sym_name(ModuleName), ":")),
		nl
		| ErrorPieces0] },
	write_error_pieces_maybe_with_context(MaybeContext, 0, ErrorPieces).

:- pred recompile_reason_message(recompile_reason::in, maybe(context)::out,
		list(format_component)::out) is det.

recompile_reason_message(file_error(_FileName, Msg), no, [words(Msg)]).
recompile_reason_message(output_file_not_up_to_date(FileName), no,
		[words("output file"), words(FileName),
			words("is not up to date.")]).
recompile_reason_message(syntax_error(Context, Msg), yes(Context),
		[words(Msg)]).
recompile_reason_message(module_changed(FileName), no,
		[words("file"), words("`" ++ FileName ++ "'"),
			words("has changed.")]).
recompile_reason_message(item_ambiguity(Item, AmbiguousItems), no, Pieces) :-
	AmbiguousItemPieces = component_lists_to_pieces(
		list__map(describe_item, AmbiguousItems)),
	Pieces = append_punctuation(
		list__condense([
			[words("addition of ") | describe_item(Item)],
			[words("could cause an ambiguity with")],
			AmbiguousItemPieces]),
		'.').
recompile_reason_message(functor_ambiguity(SymName, Arity,
			Functor, AmbiguousFunctors), no, Pieces) :-
	FunctorPieces = describe_functor(SymName, Arity, Functor),
	AmbiguousFunctorPieces = component_lists_to_pieces(
		list__map(describe_functor(SymName, Arity),
		AmbiguousFunctors)),
	Pieces = append_punctuation(
		list__condense([
			[words("addition of ") | FunctorPieces],
			[words("could cause an ambiguity with")],
			AmbiguousFunctorPieces]),
		'.').
recompile_reason_message(changed_item(Item), no,
		list__append(describe_item(Item), [words("was modified.")])).
recompile_reason_message(removed_item(Item), no,
		list__append(describe_item(Item), [words("was removed.")])).
recompile_reason_message(
		changed_or_added_instance(ModuleName, ClassName - ClassArity),
		no,
		[
		words("an instance for class"),
		words(describe_sym_name_and_arity(ClassName / ClassArity)),
		words("in module"),
		words(describe_sym_name(ModuleName)),
		words("was added or modified.")
		]).
recompile_reason_message(removed_instance(ModuleName, ClassName - ClassArity),
		no,
		[
		words("an instance for class "),
		words(describe_sym_name_and_arity(ClassName / ClassArity)),
		words("in module"),
		words(describe_sym_name(ModuleName)),
		words("was removed.")
		]).

:- func describe_item(item_id) = list(format_component).

describe_item(item_id(ItemType0, SymName - Arity)) = Pieces :-
	( body_item(ItemType0, ItemType1) ->
		ItemType = ItemType1,
		BodyWords = "body of "
	;
		ItemType = ItemType0,
		BodyWords = ""
	),
	string_to_item_type(ItemTypeStr, ItemType),
	Pieces = [
		words(string__append(BodyWords, ItemTypeStr)),
		words(describe_sym_name_and_arity(SymName / Arity))
		].

:- pred body_item(item_type::in, item_type::out) is semidet.

body_item(type_body, (type)).

:- func describe_functor(sym_name, arity, resolved_functor) =
		list(format_component).

describe_functor(SymName, _Arity,
		pred_or_func(_, ModuleName, PredOrFunc, PredArity)) =
		[words(ItemTypeStr), SymNameAndArityPiece] :-
	string_to_item_type(ItemTypeStr,
		pred_or_func_to_item_type(PredOrFunc)),	
	unqualify_name(SymName, UnqualName),
	SymNameAndArityPiece = words(describe_sym_name_and_arity(
		qualified(ModuleName, UnqualName) / PredArity)).
describe_functor(SymName, Arity, constructor(TypeName - TypeArity)) =
		[words("constructor"),
		words(describe_sym_name_and_arity(SymName / Arity)),
		words("of type"),
		words(describe_sym_name_and_arity(TypeName / TypeArity))
		].
describe_functor(SymName, Arity,
			field(TypeName - TypeArity, ConsName - ConsArity)) = 
		[words("field access function"),
		words(describe_sym_name_and_arity(SymName / Arity)),
		words("for constructor"),
		words(describe_sym_name_and_arity(ConsName / ConsArity)),
		words("of type"),
		words(describe_sym_name_and_arity(TypeName / TypeArity))
		].

%-----------------------------------------------------------------------------%

:- pred read_term_check_for_error_or_eof(recompilation_check_info::in,
		string::in, term::out, io__state::di, io__state::uo) is det.

read_term_check_for_error_or_eof(Info, Item, Term) -->
	parser__read_term(TermResult),
	(
		{ TermResult = term(_, Term) }
	;
		{ TermResult = error(Message, Line) },
		io__input_stream_name(FileName),
		{ Reason = syntax_error(term__context(FileName, Line),
				Message) },
		{ throw_syntax_error(Reason, Info) }
	;
		{ TermResult = eof },
		io__input_stream_name(FileName),
		io__get_line_number(Line),
		{ Reason = syntax_error(term__context(FileName, Line),
			string__append_list(
				["unexpected end of file, expected ",
				Item, "."])) },
		{ throw_syntax_error(Reason, Info) }
	).

:- func get_term_context(term) = term__context.

get_term_context(Term) =
	( Term = term__functor(_, _, Context) ->
		Context
	;
		term__context_init
	).

:- pred record_recompilation_reason(recompile_reason::in,
	recompilation_check_info::in, recompilation_check_info::out) is det.

record_recompilation_reason(Reason, Info0, Info) :-
	( Info0 ^ collect_all_reasons = yes ->
		Info = Info0 ^ recompilation_reasons :=
				[Reason | Info0 ^ recompilation_reasons]
	;
		throw(recompile_exception(Reason, Info0))
	).

:- pred throw_syntax_error(recompile_reason::in,
		recompilation_check_info::in) is erroneous.

throw_syntax_error(Reason, Info) :-
	% If there were syntax errors in a `.used' file written during
	% a compilation, all outputs of that compilation are slightly
	% suspect, so it's worth entirely redoing the compilation.
	RecompileInfo = Info ^ modules_to_recompile := (all),
	throw(recompile_exception(Reason, RecompileInfo)).

%-----------------------------------------------------------------------------%
