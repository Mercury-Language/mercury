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
		list__foldl2(make_module_clean, AllModules, Info3, Info4),
		remove_init_files(MainModuleName, Info4, Info)
	;
		{ TargetType = realclean },	
		{ Succeeded = yes },
		make_main_module_realclean(MainModuleName, Info3, Info4),
		list__foldl2(make_module_realclean, AllModules, Info4, Info)
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
					intermodule_interface)
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
		make_misc_target(MainModuleName - build_library,
			LibSucceeded, Info3, Info4),
		( { LibSucceeded = yes } ->
			install_library(MainModuleName, Succeeded, Info4, Info)
		;
		    { Info = Info4 },
		    { Succeeded = no }
		)
	).

%-----------------------------------------------------------------------------%

:- pred install_library(module_name::in, bool::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

install_library(MainModuleName, Succeeded, Info0, Info) -->
    find_reachable_local_modules(MainModuleName, DepsSuccess,
		AllModules0, Info0, Info1),
    { AllModules = set__to_sorted_list(AllModules0) },
    make_install_dirs(DirSucceeded, LinkSucceeded),
    ( { DepsSuccess = yes, DirSucceeded = yes } ->
	globals__io_lookup_string_option(install_prefix, Prefix),

	{ ModulesDir = Prefix/"lib"/"mercury"/"modules" },
	module_name_to_file_name(MainModuleName, ".init", no, InitFileName),
	install_file(InitFileName, ModulesDir, InitSucceded),

        list__map_foldl2(install_ints_and_headers(LinkSucceeded), AllModules,
		IntsSucceeded, Info1, Info2),

	globals__io_get_globals(Globals),
	{ compute_grade(Globals, Grade) },
        install_library_grade_files(LinkSucceeded, Grade, MainModuleName,
		AllModules, GradeSucceeded, Info2, Info3),
	(
		{ InitSucceded = yes },
		{ bool__and_list(IntsSucceeded) = yes },
		{ GradeSucceeded = yes }
	->
		% XXX With Mmake, LIBGRADES is target-specific.
        	globals__io_lookup_accumulating_option(libgrades, LibGrades),
		globals__io_lookup_bool_option(keep_going, KeepGoing),
        	foldl2_maybe_stop_at_error(KeepGoing,
			install_library_grade(LinkSucceeded,
				MainModuleName, AllModules),
			LibGrades, Succeeded, Info3, Info)
    	;
		{ Info = Info3 },
		{ Succeeded = no }
    	)
    ;
        { Info = Info1 },
        { Succeeded = no }
    ).

:- pred install_ints_and_headers(bool::in, module_name::in, bool::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

install_ints_and_headers(SubdirLinkSucceeded, ModuleName,
		Succeeded, Info0, Info) -->
    get_module_dependencies(ModuleName, MaybeImports, Info0, Info),
    (
	{ MaybeImports = yes(Imports) },
	globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	{ Intermod = yes ->
		% `.int0' files are imported by `.opt' files.
		Exts =
		    ( Imports ^ children \= [] -> ["int0", "opt"] ; ["opt"] )
	;
		Exts = []
	},

	globals__io_lookup_string_option(install_prefix, Prefix),
	{ LibDir = Prefix/"lib"/"mercury" },
	list__map_foldl(
		install_subdir_file(SubdirLinkSucceeded,
			LibDir/"ints", ModuleName),
		["int", "int2", "int3", "module_dep" | Exts],
		Results),

	globals__io_get_target(Target),
	(
		% `.mh' files are only generated for modules containing
		% `:- pragma export' declarations.
		{ Target = c ; Target = asm },
		{ Imports ^ contains_foreign_export = contains_foreign_export }
	->
		install_subdir_file(SubdirLinkSucceeded, LibDir/"inc",
			ModuleName, "mh", HeaderSucceded1),

		% This is needed so that the file will be
		% found in Mmake's VPATH.
		install_subdir_file(SubdirLinkSucceeded, LibDir/"ints",
			ModuleName, "mh", HeaderSucceded2),

		{ HeaderSucceded = HeaderSucceded1 `and` HeaderSucceded2 }
	;
		{ HeaderSucceded = yes }
	),
	{ Succeeded = bool__and_list([HeaderSucceded | Results]) }
    ;
    	{ MaybeImports = no },
	{ Succeeded = no }
    ).

:- pred install_library_grade(bool::in, module_name::in, list(module_name)::in,
	string::in, bool::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

install_library_grade(LinkSucceeded0, ModuleName, AllModules, Grade,
		Succeeded, Info0, Info) -->
	%
	% Building the library in the new grade is done in a separate
	% process to make it easier to stop and clean up on an interrupt.
	%
	{ Cleanup = make_grade_clean(ModuleName, AllModules) },
	build_with_check_for_interrupt(
	    (pred(GradeSuccess::out, MInfo::in, MInfo::out, di, uo) is det -->
		call_in_forked_process(
		    (pred(GradeSuccess0::out, di, uo) is det -->
			install_library_grade_2(LinkSucceeded0,
			    Grade, ModuleName, AllModules,
			    MInfo, GradeSuccess0)
		    ), GradeSuccess)
	    ), Cleanup, Succeeded, Info0, Info).

:- pred install_library_grade_2(bool::in, string::in, module_name::in,
		list(module_name)::in, make_info::in,
		bool::out, io__state::di, io__state::uo) is det.

install_library_grade_2(LinkSucceeded0, Grade, ModuleName, AllModules,
		Info0, Succeeded) -->
	globals__io_get_globals(Globals),

	%
	% Set up so that grade-dependent files for the current grade
	% don't overwrite the files for the default grade.
	%
	{ OptionArgs0 = Info0 ^ option_args },
	{ OptionArgs = OptionArgs0 ++
			["--grade", Grade, "--use-grade-subdirs"] },

	verbose_msg(
		(pred(di, uo) is det -->
			io__write_string("Installing grade "),
			io__write_string(Grade),
			io__nl
		)),

	lookup_mmc_options(Info0 ^ options_variables, MaybeMCFlags),
	(
		{ MaybeMCFlags = yes(MCFlags) },
		handle_options(MCFlags ++ OptionArgs, OptionsError, _, _, _)
	;
		{ MaybeMCFlags = no },
		% Errors should have been caught before.
		{ error("install_library_grade: bad DEFAULT_MCFLAGS") }
	),

	( 
		{ OptionsError = yes(OptionsMessage) },
		usage_error(OptionsMessage),
		{ Succeeded = no }
	;
		{ OptionsError = no },
		%
		% Remove the grade-dependent targets from the status map
		% (we need to rebuild them in the new grade).
		%
		{ StatusMap0 = Info0 ^ dependency_status },
		{ StatusMap = map__from_assoc_list(list__filter(
			(pred((File - _)::in) is semidet :-
			    \+ (
				File = target(_ - Target),
				target_is_grade_or_arch_dependent(Target)
			    )
			), 
			map__to_assoc_list(StatusMap0))) },
		{ Info1 = (Info0 ^ dependency_status := StatusMap)
				^ option_args := OptionArgs },

		make_misc_target(ModuleName - build_library,
			LibSucceeded, Info1, Info2),
		( { LibSucceeded = yes } ->
			install_library_grade_files(LinkSucceeded0,
				Grade, ModuleName, AllModules,
				Succeeded, Info2, Info3),

			make_grade_clean(ModuleName, AllModules,
				Info3, _)
		;
			{ Succeeded = no }
		)
	),
	globals__io_set_globals(unsafe_promise_unique(Globals)).

	% Install the `.a', `.so, `.opt' and `.mih' files
	% for the current grade.
:- pred install_library_grade_files(bool::in, string::in, module_name::in,
	list(module_name)::in, bool::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

install_library_grade_files(LinkSucceeded0, Grade, ModuleName, AllModules,
		Succeeded, Info0, Info) -->
    make_grade_install_dirs(Grade, DirResult, LinkSucceeded1),
    { LinkSucceeded = LinkSucceeded0 `and` LinkSucceeded1 },
    ( { DirResult = yes } ->
	linked_target_file_name(ModuleName, static_library, LibFileName),
	linked_target_file_name(ModuleName, shared_library, SharedLibFileName),

	globals__io_lookup_string_option(install_prefix, Prefix),
	globals__io_lookup_string_option(fullarch, FullArch),
	{ GradeLibDir = Prefix/"lib"/"mercury"/"lib"/Grade/FullArch },

	install_file(LibFileName, GradeLibDir, LibSucceded),
	install_file(SharedLibFileName, GradeLibDir, SharedLibSucceded),

	list__map_foldl2(install_grade_ints_and_headers(LinkSucceeded, Grade),
		AllModules, IntsHeadersSucceded, Info0, Info),
	{ Succeeded = bool__and_list(
		[LibSucceded, SharedLibSucceded | IntsHeadersSucceded]) }
    ;
	{ Succeeded = no },
    	{ Info = Info0 }
    ).

	% Install the `.opt' and `.mih' files for the current grade.
:- pred install_grade_ints_and_headers(bool::in, string::in, module_name::in,
		bool::out, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

install_grade_ints_and_headers(LinkSucceeded, Grade, ModuleName,
		Succeeded, Info0, Info) -->
    get_module_dependencies(ModuleName, MaybeImports, Info0, Info),
    (
	{ MaybeImports = yes(Imports) },
	globals__io_lookup_string_option(install_prefix, Prefix),
	globals__io_lookup_string_option(fullarch, FullArch),
	{ LibDir = Prefix/"lib"/"mercury" },

	globals__io_get_target(Target),
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	(
		{ Target = c, HighLevelCode = yes
		; Target = asm,
			Imports ^ foreign_code = contains_foreign_code(_) 
		}
	->
		{ GradeIncDir = LibDir/"lib"/Grade/FullArch/"inc" },
		install_subdir_file(LinkSucceeded, GradeIncDir,
			ModuleName, "mih", HeaderSucceded1),

		% This is needed so that the file will be
		% found in Mmake's VPATH.
		{ IntDir = LibDir/"int" },
		install_subdir_file(LinkSucceeded, IntDir,
			ModuleName, "mih", HeaderSucceded2),

		{ HeaderSucceded = HeaderSucceded1 `and` HeaderSucceded2 }
	;
		{ HeaderSucceded = yes }
	),

	globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	( { Intermod = yes } ->
		{ GradeIntDir = LibDir/"ints"/Grade },
		install_subdir_file(LinkSucceeded, GradeIntDir,
			ModuleName, "opt", OptSucceded)
	;
		{ OptSucceded = yes }
	),
	{ Succeeded = HeaderSucceded `and` OptSucceded }
    ;
	{ MaybeImports = no },
	{ Succeeded = no }
    ).

	% Install a file in the given directory, and in
	% directory/Mercury/exts if the symlinks for the
	% subdirectories couldn't be created (e.g. on Windows).
:- pred install_subdir_file(bool::in, dir_name::in, module_name::in,
	string::in, bool::out, io__state::di, io__state::uo) is det.

install_subdir_file(SubdirLinkSucceeded, InstallDir,
		ModuleName, Ext, Succeeded) -->
	module_name_to_file_name(ModuleName, "." ++ Ext, no, FileName),
	install_file(FileName, InstallDir, Succeeded1),
	( { SubdirLinkSucceeded = no } ->
		install_file(FileName, InstallDir/"Mercury"/(Ext ++ "s"),
			Succeeded2),
		{ Succeeded = Succeeded1 `and` Succeeded2 }
	;
		{ Succeeded = Succeeded1 }
	).

:- pred install_file(file_name::in, dir_name::in, bool::out,
		io__state::di, io__state::uo) is det.

install_file(FileName, InstallDir, Succeeded) -->
	verbose_msg(
		(pred(di, uo) is det -->
			io__write_string("Installing file "),
			io__write_string(FileName),
			io__write_string(" in "),
			io__write_string(InstallDir),
			io__nl
		)),
	globals__io_lookup_string_option(install_command, InstallCommand),
	{ Command = string__join_list("	", list__map(quote_arg,
			[InstallCommand, FileName, InstallDir])) },
	io__output_stream(OutputStream),
	invoke_shell_command(OutputStream, verbose, Command, Succeeded).

:- pred make_install_dirs(bool::out, bool::out,
		io__state::di, io__state::uo) is det.

make_install_dirs(Result, LinkResult) -->
	globals__io_lookup_string_option(install_prefix, Prefix),
	{ LibDir = Prefix/"lib"/"mercury" },
	make_directory(LibDir/"inc", Result1),
	make_directory(LibDir/"modules", Result2),

	{ IntsSubdir = LibDir/"ints"/"Mercury" },
	make_directory(IntsSubdir, Result3),

	{ Result4 = Result1 `and` Result2 `and` Result3 },

	{ Subdirs = ["int", "int2", "int3",
			"opt", "trans_opt", "module_dep"] },
	list__map_foldl(make_install_symlink(IntsSubdir),
		Subdirs, LinkResults),
	{ LinkResult = bool__and_list(LinkResults) },
	( { LinkResult = yes } ->
		{ Result = Result4 }
	;
		list__map_foldl(
		    (pred(Ext::in, MkDirResult::out, di, uo) is det -->
		    	make_directory(IntsSubdir/(Ext ++ "s"), MkDirResult)
		    ), Subdirs, MkDirResults),
		{ Result = bool__and_list([Result4 | MkDirResults]) }
	).

:- pred make_grade_install_dirs(string::in, bool::out, bool::out,
		io__state::di, io__state::uo) is det.

make_grade_install_dirs(Grade, Result, LinkResult) -->
	globals__io_lookup_string_option(install_prefix, Prefix),
	globals__io_lookup_string_option(fullarch, FullArch),
	{ LibDir = Prefix/"lib"/"mercury" },

	{ GradeIntsSubdir = LibDir/"ints"/Grade/"Mercury" },
	make_directory(GradeIntsSubdir, Result1),

	{ GradeIncSubdir = LibDir/"lib"/Grade/FullArch/"inc"/"Mercury" },
	make_directory(GradeIncSubdir, Result2),

	{ Result3 = Result1 `and` Result2 },

	make_install_symlink(GradeIncSubdir, "mih", LinkResult0),
	list__map_foldl(make_install_symlink(GradeIntsSubdir),
		["opt", "trans_opt"], LinkResults),
	{ LinkResult = bool__and_list([LinkResult0 | LinkResults]) },
	( { LinkResult = yes } ->
		{ Result = Result3 }
	;
		make_directory(GradeIncSubdir/"mih", Result4),
		list__map_foldl(
		    (pred(Ext::in, MkDirResult::out, di, uo) is det -->
		    	make_directory(GradeIntsSubdir/(Ext ++ "s"),
				MkDirResult)
		    ), ["opt", "trans_opt"], MkDirResults),
		{ Result = bool__and_list([Result3, Result4 | MkDirResults]) }
	).

:- pred make_install_symlink(string::in, string::in, bool::out,
		io__state::di, io__state::uo) is det.

make_install_symlink(Subdir, Ext, Result) -->
	{ LinkName = Subdir/(Ext ++ "s") },
	make_symlink("..", LinkName, Result).

:- pred make_symlink(string::in, string::in, bool::out,
		io__state::di, io__state::uo) is det.

make_symlink(LinkTarget, LinkName, Result) -->
	io__output_stream(ErrorStream),
	{ string__format("rm -f %s && ln -s %s %s",
		[s(LinkName), s(LinkTarget), s(LinkName)], Command) },
	invoke_shell_command(ErrorStream, verbose, Command, Result).

%-----------------------------------------------------------------------------%

	% Clean up grade-dependent files.
:- pred make_grade_clean(module_name::in, list(module_name)::in,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

make_grade_clean(ModuleName, AllModules, Info0, Info) -->
	make_main_module_realclean(ModuleName, Info0, Info1),
	list__foldl2(make_module_clean, AllModules, Info1, Info).

:- pred make_main_module_realclean(module_name::in,
		make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

make_main_module_realclean(ModuleName, Info0, Info) -->
	linked_target_file_name(ModuleName, executable, ExeFileName),
	linked_target_file_name(ModuleName, static_library, LibFileName),
	linked_target_file_name(ModuleName, shared_library, SharedLibFileName),
	list__foldl2(remove_file,
		[ExeFileName, LibFileName, SharedLibFileName],
		Info0, Info1),
	remove_file(ModuleName, ".init", Info1, Info2),
	remove_init_files(ModuleName, Info2, Info).

:- pred remove_init_files(module_name::in, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

remove_init_files(ModuleName, Info0, Info) -->
	globals__io_lookup_string_option(object_file_extension, ObjExt),
	list__foldl2(remove_file(ModuleName), ["_init.c", "_init" ++ ObjExt],
		Info0, Info).

:- pred make_module_clean(module_name::in, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

make_module_clean(ModuleName, Info0, Info) -->
	list__foldl2(remove_target_file(ModuleName),
		[errors, c_code, c_header(mih),
		object_code(pic), object_code(non_pic),
		asm_code(pic), asm_code(non_pic),
		il_code, java_code
		],
		Info0, Info1),

	list__foldl2(remove_file(ModuleName),
		[".used", ".prof", ".derived_schema", ".base_schema"],
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

:- pred make_module_realclean(module_name::in, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

make_module_realclean(ModuleName, Info0, Info) -->
	make_module_clean(ModuleName, Info0, Info1),
	list__foldl2(remove_target_file(ModuleName),
		[private_interface, long_interface, short_interface,
		unqualified_short_interface, intermodule_interface,
		aditi_code, c_header(mh)
		],
		Info1, Info2),
	remove_file(ModuleName, module_dep_file_extension, Info2, Info).

%-----------------------------------------------------------------------------%
