%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
:- module make__program_target.

:- interface.

:- pred make_linked_target(linked_target_file::in, bool::out,
		make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

:- pred make_misc_target(pair(module_name, misc_target_type)::in,
		bool::out, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__passes_aux.

make_linked_target(MainModuleName - FileType, Succeeded, Info0, Info) -->
    find_reachable_local_modules(MainModuleName, DepsSuccess,
		AllModules, Info0, Info1),
    globals__io_lookup_bool_option(keep_going, KeepGoing),
    ( { DepsSuccess = no, KeepGoing = no } ->
	{ Succeeded = no },
	{ Info = Info1 }
    ;
	globals__io_lookup_string_option(pic_object_file_extension, PicObjExt),
	globals__io_lookup_string_option(object_file_extension, ObjExt),
	{ FileType = shared_library, PicObjExt \= ObjExt ->
		ObjectCodeType = pic,
		ObjExtToUse = PicObjExt
	;
		ObjectCodeType = non_pic,
		ObjExtToUse = ObjExt
	},

	%
	% Build the `.c' files first so that errors are
	% reported as soon as possible.
	%
	globals__io_get_target(CompilationTarget),
	{
		CompilationTarget = c,
		IntermediateTargetType = c_code,
		ObjectTargetType = object_code(ObjectCodeType)
	;
		CompilationTarget = asm,
		IntermediateTargetType = asm_code(ObjectCodeType),
		ObjectTargetType = object_code(ObjectCodeType)
	;
		CompilationTarget = il,
		IntermediateTargetType = il_code,
		ObjectTargetType = il_asm
	;
		CompilationTarget = java,
		IntermediateTargetType = java_code,
		% XXX Whoever finishes the Java backend can fill this in.
		ObjectTargetType = object_code(non_pic)
	},

	get_target_modules(IntermediateTargetType,
		set__to_sorted_list(AllModules), ObjModules, Info1, Info4),
	{ IntermediateTargets = make_dependency_list(ObjModules,
					IntermediateTargetType) },
	{ ObjTargets = make_dependency_list(ObjModules, ObjectTargetType) },

	foldl2_maybe_stop_at_error(KeepGoing,
		foldl2_maybe_stop_at_error(KeepGoing, make_module_target),
		[IntermediateTargets, ObjTargets], _, Info4, Info5),

	linked_target_file_name(MainModuleName, FileType, OutputFileName),
	get_file_timestamp([dir__this_directory], OutputFileName,
		MaybeTimestamp, Info5, Info6),
	check_dependencies(OutputFileName, MaybeTimestamp,
		ObjTargets, BuildDepsResult, Info6, Info7),

	(
	    { DepsSuccess = yes },
	    { BuildDepsResult \= error }
	->
	    build_with_check_for_interrupt(
		build_with_module_options_and_output_redirect(
			MainModuleName, [],
			build_linked_target(MainModuleName, FileType,
				OutputFileName, MaybeTimestamp, AllModules,
				ObjModules, CompilationTarget, ObjExtToUse,
				DepsSuccess, BuildDepsResult)),
		linked_target_cleanup(MainModuleName, FileType, OutputFileName,
			CompilationTarget),
		Succeeded, Info7, Info)
    	;
    	    { Succeeded = no },
	    { Info = Info7 }
	)
    ).

:- pred get_target_modules(module_target_type::in, list(module_name)::in,
	list(module_name)::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

get_target_modules(TargetType, AllModules, TargetModules, Info0, Info) -->
    globals__io_get_target(CompilationTarget),
    ( 
	{
		TargetType = errors
	;
		CompilationTarget = asm,
		( TargetType = asm_code(_)
		; TargetType = object_code(_)
		)
	}
    ->
	% `.err' and `.s' files are only produced for the
	% top-level module in each source file.
	list__foldl3(
	    (pred(ModuleName::in, TargetModules0::in, TargetModules1::out,
			MInfo0::in, MInfo::out, di, uo) is det -->
		get_module_dependencies(ModuleName, MaybeImports,
			MInfo0, MInfo),
		{
			MaybeImports = yes(Imports),
			ModuleName = Imports ^ source_file_module_name
		->
			TargetModules1 = [ModuleName | TargetModules0]
		;	
			TargetModules1 = TargetModules0
		}
	    ),
	    AllModules, [], TargetModules, Info0, Info)
    ;
	{ Info = Info0 },
	{ TargetModules = AllModules }
    ).

:- pred build_linked_target(module_name::in, linked_target_type::in,
	file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
	list(module_name)::in, compilation_target::in, string::in, bool::in,
	dependencies_result::in, list(string)::in, io__output_stream::in,
	bool::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

build_linked_target(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
		AllModules, ObjModules, CompilationTarget, ObjExtToUse,
		DepsSuccess, BuildDepsResult, _, ErrorStream, Succeeded,
		Info0, Info) -->
	globals__io_lookup_maybe_string_option(pre_link_command,
		MaybePreLinkCommand),
	( { MaybePreLinkCommand = yes(PreLinkCommand0) } ->
		{ PreLinkCommand = substitute_user_command(PreLinkCommand0,
			MainModuleName, set__to_sorted_list(AllModules)) },
		invoke_shell_command(ErrorStream, verbose, PreLinkCommand,
			PreLinkSucceeded)
	;
		{ PreLinkSucceeded = yes }
	),	

	( { PreLinkSucceeded = yes } ->
		build_linked_target_2(MainModuleName, FileType, OutputFileName,
			MaybeTimestamp, AllModules, ObjModules,
			CompilationTarget, ObjExtToUse, DepsSuccess,
			BuildDepsResult, ErrorStream, Succeeded,
			Info0, Info)
	;
		{ Succeeded = no },
		{ Info = Info0 }
	).

:- pred build_linked_target_2(module_name::in, linked_target_type::in,
	file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
	list(module_name)::in, compilation_target::in, string::in, bool::in,
	dependencies_result::in, io__output_stream::in, bool::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

build_linked_target_2(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
		AllModules, ObjModules, CompilationTarget, ObjExtToUse,
		DepsSuccess, BuildDepsResult, ErrorStream, Succeeded,
		Info0, Info) -->
	globals__io_lookup_accumulating_option(link_objects, LinkObjects),

	% Clear the option -- we'll pass the list of files directly.
	globals__io_set_option(link_objects, accumulating([])),

	%
	% Remake the `_init.o' file.
	% XXX We should probably make a `_init.o' file for shared
	% libraries linked using dlopen().
	%
	{ AllModulesList = set__to_sorted_list(AllModules) },
	(
		{ FileType = executable },
		{ CompilationTarget = c ; CompilationTarget = asm }
	->
		compile_target_code__make_init_obj_file(ErrorStream,
			MainModuleName, AllModulesList, InitObjectResult),
		(
			{ InitObjectResult = yes(InitObject) },
			
			% We may need to update the timestamp
			% of the `_init.o' file.
			{ Info1 = Info0 ^ file_timestamps :=
				map__delete(Info0 ^ file_timestamps,
				InitObject) },
			{ InitObjects = [InitObject] },
			{ DepsResult2 = BuildDepsResult }
		;
			{ InitObjectResult = no },
			{ Info1 = Info0 },
			{ DepsResult2 = error },
			{ InitObjects = [] }
		)
	;
		{ DepsResult2 = BuildDepsResult },
		{ Info1 = Info0 },
		{ InitObjects = [] }
	),

	{ ObjectsToCheck = InitObjects ++ LinkObjects },
	list__map_foldl2(get_file_timestamp([dir__this_directory]),
		ObjectsToCheck, ExtraObjectTimestamps, Info1, Info2),
	check_dependency_timestamps(OutputFileName, MaybeTimestamp,
		ObjectsToCheck, io__write, ExtraObjectTimestamps,
		ExtraObjectDepsResult),

	{ DepsResult3 = ( DepsSuccess = yes -> DepsResult2 ; error ) },
	{ DepsResult3 = error, DepsResult = DepsResult3
	; DepsResult3 = out_of_date, DepsResult = DepsResult3
	; DepsResult3 = up_to_date, DepsResult = ExtraObjectDepsResult
	},
	(
		{ DepsResult = error },
		file_error(OutputFileName),
		{ Succeeded = no },
		{ Info = Info2 }
	;
		{ DepsResult = up_to_date },
		{ Succeeded = yes },
		{ Info = Info2 }
	;
		{ DepsResult = out_of_date },
		maybe_make_linked_target_message(OutputFileName),

		%
		% Find the extra object files for externally compiled
		% foreign procedures and fact tables. We don't need
		% to include these in the timestamp checking above --
		% they will have been checked when the module's object
		% file was built.
		%
		list__map_foldl2(
		    (pred(ModuleName::in, ForeignFiles::out,
		    	    MakeInfo0::in, MakeInfo::out, di, uo) is det -->
			get_module_dependencies(ModuleName, MaybeImports,
				MakeInfo0, MakeInfo),
			(
			    { MaybeImports = yes(Imports) },
			    external_foreign_code_files(Imports, ForeignFiles)
			;
			    { MaybeImports = no },
			    % This error should have been detected earlier.
			    { error(
			    "build_linked_target: error in dependencies") }
			)
		    ), AllModulesList, ExtraForeignFiles, Info2, Info3),
		{ ForeignObjects = list__map(
			(func(foreign_code_file(_, _, ObjFile)) = ObjFile),
			list__condense(ExtraForeignFiles)) },

		list__map_foldl(
		    (pred(ObjModule::in, ObjToLink::out, di, uo) is det -->
			module_name_to_file_name(ObjModule,
				ObjExtToUse, no, ObjToLink)
		    ), ObjModules, ObjList),

		% LinkObjects may contain `.a' files which must come
		% after all the object files on the linker command line.
		{ AllObjects = InitObjects ++ ObjList ++
				ForeignObjects ++ LinkObjects },

		(
			{ CompilationTarget = c },
			% Run the link in a separate process so it can
			% be killed if an interrupt is received.
			call_in_forked_process(
				compile_target_code__link(ErrorStream,
					FileType, MainModuleName, AllObjects),
				Succeeded)
		;
			{ CompilationTarget = asm },
			% Run the link in a separate process so it can
			% be killed if an interrupt is received.
			call_in_forked_process(
				compile_target_code__link(ErrorStream,
					FileType, MainModuleName, AllObjects),
				Succeeded)
		;
			%
			% IL doesn't need any linking. XXX Is this right?
			%
			{ CompilationTarget = il },
			{ Succeeded = yes }
		;
			{ CompilationTarget = java },
			{ Succeeded = no },
			io__write_string(
			"Sorry, not implemented, linking for `--target java'")
		),

		( { Succeeded = yes } ->
			{ Info = Info3 ^ file_timestamps :=
				map__delete(Info3 ^ file_timestamps,
					OutputFileName) }
		;
			file_error(OutputFileName),
			{ Info = Info3 }
		)
	),
	globals__io_set_option(link_objects, accumulating(LinkObjects)).

:- pred linked_target_cleanup(module_name::in, linked_target_type::in,
	file_name::in, compilation_target::in, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

linked_target_cleanup(MainModuleName, FileType, OutputFileName,
		CompilationTarget, Info0, Info) -->
	remove_file(OutputFileName, Info0, Info1),
	(
		{ FileType = executable },
		{ CompilationTarget = c
		; CompilationTarget = asm
		}
	->
		globals__io_lookup_string_option(object_file_extension,
			ObjExt),
		remove_file(MainModuleName, "_init.c", Info1, Info2),
		remove_file(MainModuleName, "_init" ++ ObjExt, Info2, Info)
	;
		{ Info = Info1 }
	).

%-----------------------------------------------------------------------------%

make_misc_target(MainModuleName - TargetType, Succeeded, Info0, Info) -->
	% Don't rebuild dependencies when cleaning up.
	{ RebuildDeps = Info0 ^ rebuild_dependencies },
	{ ( TargetType = clean ; TargetType = realclean ) ->
		Info1 = Info0 ^ rebuild_dependencies := no
	;
		Info1 = Info0
	},
	find_reachable_local_modules(MainModuleName, Succeeded0,
		AllModules0, Info1, Info2),
	{ Info3 = Info2 ^ rebuild_dependencies := RebuildDeps },

	{ AllModules = set__to_sorted_list(AllModules0) },
	(
		{ TargetType = clean },
		{ Succeeded = yes },
		list__foldl2(make_clean, AllModules, Info3, Info)
	;
		{ TargetType = realclean },	
		{ Succeeded = yes },
		list__foldl2(make_realclean, AllModules, Info3, Info4),
		globals__io_lookup_string_option(executable_file_extension,
			ExeExt),
		globals__io_lookup_string_option(library_extension, LibExt),
		globals__io_lookup_string_option(shared_library_extension,
			SharedLibExt),

		list__foldl2(remove_file(MainModuleName),
			[ExeExt, LibExt, SharedLibExt, "_init.c", "_init.o"],
			Info4, Info)
	;
		{ TargetType = build_all(ModuleTargetType) },
		get_target_modules(ModuleTargetType, AllModules,
			TargetModules, Info3, Info4),
		globals__io_lookup_bool_option(keep_going, KeepGoing),
		( { Succeeded0 = no, KeepGoing = no } ->
			{ Info = Info4 },
			{ Succeeded = no }
		;
			foldl2_maybe_stop_at_error(KeepGoing,
				make_module_target,
				make_dependency_list(TargetModules,
					ModuleTargetType),
				Succeeded1, Info4, Info),
			{ Succeeded = Succeeded0 `and` Succeeded1 }
		)
	;
		{ TargetType = build_library },
		{ ShortInts = make_dependency_list(AllModules,
				unqualified_short_interface) },
		{ LongInts = make_dependency_list(AllModules,
				long_interface) },
		globals__io_lookup_bool_option(intermodule_optimization,
			Intermod),
		{ Intermod = yes ->
			OptFiles = make_dependency_list(AllModules,
					long_interface)
		;
			OptFiles = []
		},
		globals__io_lookup_bool_option(keep_going, KeepGoing),
		foldl2_maybe_stop_at_error(KeepGoing,
			foldl2_maybe_stop_at_error(KeepGoing,
				make_module_target),
			[ShortInts, LongInts, OptFiles],
			IntSucceeded, Info3, Info4),
		( { IntSucceeded = yes } ->
		    % Errors while making the `.init' file should be very rare.
		    io__output_stream(ErrorStream),
		    compile_target_code__make_init_file(ErrorStream,
				MainModuleName, AllModules, InitSucceeded),
		    ( { InitSucceeded = yes } ->
			make_linked_target(MainModuleName - static_library,
				StaticSucceeded, Info4, Info5),
			( { StaticSucceeded = yes } ->
				make_linked_target(
					MainModuleName - shared_library,
					Succeeded, Info5, Info)
			;
				{ Succeeded = no },
				{ Info = Info5 }
			)
		    ;
			{ Succeeded = no },
		    	{ Info = Info4 }
		    )
		;
			{ Succeeded = no },
			{ Info = Info4 }
		)
	;
		{ TargetType = install_library },
		{ Succeeded = no },
		{ error("sorry, not implemented: mmc --make module.install") }
	).

:- pred make_clean(module_name::in, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

make_clean(ModuleName, Info0, Info) -->
	list__foldl2(remove_target_file(ModuleName),
		[errors, c_code, c_header(mih),
		object_code(pic), object_code(non_pic),
		asm_code(pic), asm_code(non_pic),
		il_code, java_code
		],
		Info0, Info1),

	globals__io_lookup_string_option(object_file_extension, ObjExt),
	list__foldl2(remove_file(ModuleName),
		["_init.c", "_init" ++ ObjExt, ".used", ".prof",
		".derived_schema", ".base_schema"],
		Info1, Info2),

	get_module_dependencies(ModuleName, MaybeImports, Info2, Info3),
	(
		{ MaybeImports = yes(Imports) },
		external_foreign_code_files(Imports, ForeignCodeFiles),
		list__foldl2(
		    (pred(ForeignCodeFile::in, MakeInfo0::in, MakeInfo::out,
		    		di, uo) is det -->
			{ ForeignCodeFile = foreign_code_file(_,
				TargetFile, ObjectFile) },
			remove_file(TargetFile, MakeInfo0, MakeInfo1),
			remove_file(ObjectFile, MakeInfo1, MakeInfo)
		    ), ForeignCodeFiles, Info3, Info)
	;
		{ MaybeImports = no },
		{ Info = Info3 }
	).

:- pred make_realclean(module_name::in, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

make_realclean(ModuleName, Info0, Info) -->
	make_clean(ModuleName, Info0, Info1),
	list__foldl2(remove_target_file(ModuleName),
		[private_interface, long_interface, short_interface,
		unqualified_short_interface, intermodule_interface,
		aditi_code, c_header(mh)
		],
		Info1, Info2),
	remove_file(ModuleName, module_dep_file_extension, Info2, Info3),
	linked_target_file_name(ModuleName, executable, ExeFileName),
	linked_target_file_name(ModuleName, static_library, LibFileName),
	linked_target_file_name(ModuleName, shared_library, SharedLibFileName),
	list__foldl2(remove_file,
		[ExeFileName, LibFileName, SharedLibFileName],
		Info3, Info).

%-----------------------------------------------------------------------------%
