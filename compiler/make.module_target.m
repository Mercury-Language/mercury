%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: make.module_target.m
% Main author: stayl
%
% Build targets which relate to a single module (e.g. C code, object code,
% interface files).
%-----------------------------------------------------------------------------%

:- module make__module_target.

:- interface.

	% make_module_target(Target, Success, Info0, Info).
	%
	% Make a target corresponding to a single module.
:- pred make_module_target(dependency_file::in, bool::out,
	make_info::in, make_info::out, io::di, io::uo) is det.

	% record_made_target(Target, Task, MakeSucceeded)
	%
	% Record whether building a target succeeded or not.
	% Makes sure any timestamps for files which may have changed
	% in building the target are recomputed next time they are needed.
	% Exported for use by make__module_dep_file__write_module_dep_file.
:- pred record_made_target(target_file::in, compilation_task_type::in,
	bool::in, make_info::in, make_info::out,
	io::di, io::uo) is det.

:- type foreign_code_file
	--->	foreign_code_file(
			foreign_language	:: foreign_language,

				% Name of the file produced by the Mercury
				% compiler, e.g. module_c_code.c.
			target_file		:: file_name,

				% Name of the file produced by the foreign
				% language compiler, e.g. module_c_code.o.
			object_file		:: file_name
		).

	% Find the foreign code files generated when a module is processed.
	% The `pic' field is only used for C foreign code.
:- pred external_foreign_code_files(pic::in, module_imports::in,
	list(foreign_code_file)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__passes_aux.

:- pred make_module_target(dependency_file::in, bool::in, bool::out,
	make_info::in, make_info::out, io::di, io::uo) is det.

make_module_target(TargetFile, Succeeded1, Succeeded1 `and` Succeeded2,
		Info0, Info) -->
	make_module_target(TargetFile, Succeeded2, Info0, Info).

make_module_target(file(_, _) @ Dep, Succeeded, Info0, Info) -->
    dependency_status(Dep, Status, Info0, Info),
    { Succeeded = ( Status = error -> no ; yes ) }.
make_module_target(target(TargetFile) @ Dep, Succeeded, Info0, Info) -->
    dependency_status(Dep, Status, Info0, Info1),
    (
	{ Status = not_considered },
	{ TargetFile = ModuleName - FileType },
	get_module_dependencies(ModuleName, MaybeImports, Info1, Info2),
	(
	    { MaybeImports = no },
	    { Succeeded = no },
	    { Info = Info2 ^ dependency_status ^ elem(Dep) := error }
	;
	    { MaybeImports = yes(Imports) },
	    globals__io_get_globals(Globals),
	    { CompilationTask = compilation_task(Globals, FileType) },
	    (
		% For a target built by processing a Mercury source file,
		% the target for a nested sub-module is produced as a side
		% effect of making the target for the top-level module in
		% the file.
		{ CompilationTask = process_module(_) - _ },
	    	{ Imports ^ source_file_module_name \= ModuleName }
	    ->
		make_module_target(
			target(Imports ^ source_file_module_name - FileType),
			Succeeded, Info2, Info)
	    ;
		{ CompilationTask = CompilationTaskType - _ },
		touched_files(TargetFile, CompilationTaskType,
			TouchedTargetFiles, TouchedFiles, Info2, Info3),
		{ list__foldl(update_target_status(being_built),
			TouchedTargetFiles, Info3, Info4) },

		debug_file_msg(TargetFile, "checking dependencies"),

		{ CompilationTask = process_module(_) - _ ->
		    ModulesToCheck = [ModuleName | Imports ^ nested_children]
		;
		    ModulesToCheck = [ModuleName]
		},

		foldl3_maybe_stop_at_error(Info4 ^ keep_going,
			union_deps(target_dependencies(Globals, FileType)),
			ModulesToCheck, DepsSuccess, set__init,
			DepFiles0, Info4, Info5),
		{ TargetFile = _ - private_interface ->
			% Avoid circular dependencies (the `.int0' files
			% for the nested sub-modules depend on this module's
			% `.int0' file).
			DepFilesToMake = set__to_sorted_list(
				set__delete_list(DepFiles0,
				make_dependency_list(ModulesToCheck,
					private_interface)))
		;
			DepFilesToMake = set__to_sorted_list(DepFiles0)
		},
		{ DepFiles = set__to_sorted_list(DepFiles0) },

		debug_msg(
		   (pred(di, uo) is det -->
			write_target_file(TargetFile),
			io__write_string(": dependencies:\n"),
			io__write_list(DepFiles, ", ", write_dependency_file),
			io__nl
		    )),

		globals__io_lookup_bool_option(keep_going, KeepGoing),
		( { DepsSuccess = no, KeepGoing = no } ->
			{ Info6 = Info5 },
			{ DepsResult = error }
		;
			make_dependency_files(TargetFile, DepFilesToMake,
				TouchedTargetFiles, TouchedFiles, DepsResult0,
				Info5, Info6),
			{ DepsResult =
				( DepsSuccess = yes -> DepsResult0 ; error ) }
		),
		(
			{ DepsResult = error },
			{ Succeeded = no },
			{ list__foldl(update_target_status(error),
	    			TouchedTargetFiles, Info6, Info) }
		;
			{ DepsResult = out_of_date },
			{ Info7 = Info6 ^ command_line_targets :=
				set__delete(Info6 ^ command_line_targets,
				    ModuleName - module_target(FileType)) },
			build_target(CompilationTask, TargetFile, Imports,
				TouchedTargetFiles, TouchedFiles, Succeeded,
				Info7, Info)
		;
			{ DepsResult = up_to_date },
			maybe_warn_up_to_date_target(
				ModuleName - module_target(FileType),
				Info6, Info7),

			debug_file_msg(TargetFile, "up to date"),
			{ Succeeded = yes },
			{ list__foldl(update_target_status(up_to_date),
	    			[TargetFile | TouchedTargetFiles],
				Info7, Info) }
		)
	    )
    	)
    ;
	{ Status = up_to_date },
	{ Succeeded = yes },
	{ Info = Info1 }
    ;
	{ Status = being_built },
	( { TargetFile = _FileName - foreign_il_asm(_Lang) } ->
		io__write_string(
			"Error: circular dependency detected while building\n"),
		io__write_string("  `"),
		write_dependency_file(Dep),
		io__write_string("'.\n"),
		io__write_string(
		"  This is due to a forbidden foreign_import_module cycle.\n"),
		io__set_exit_status(1)
	;
		{ error(
	"make_module_target: target being built, circular dependencies?"
		) }
	),
	{ Succeeded = no },
	{ Info = Info1 }
    ;
	{ Status = error },
	{ Succeeded = no },
	{ Info = Info1 }
    ).

:- pred make_dependency_files(target_file::in, list(dependency_file)::in,
	list(target_file)::in, list(file_name)::in, dependencies_result::out,
	make_info::in, make_info::out, io::di, io::uo) is det.

make_dependency_files(TargetFile, DepFilesToMake, TouchedTargetFiles,
		TouchedFiles, DepsResult, Info0, Info) -->
	%
	% Build the dependencies.
	%
	globals__io_lookup_bool_option(keep_going, KeepGoing),
	foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
		DepFilesToMake, MakeDepsSuccess, Info0, Info1),

	%
	% Check that the target files exist.
	%
	list__map_foldl2(get_target_timestamp(no), TouchedTargetFiles,
			TargetTimestamps, Info1, Info2),
	(
		{ MakeDepsSuccess = no }
	->
		debug_file_msg(TargetFile, "error making dependencies"),
		{ DepsResult = error },
		{ Info = Info2 }
	;
		{ list__member(error(_), TargetTimestamps) }
	->
		debug_file_msg(TargetFile, "target file does not exist"),
		{ DepsResult = out_of_date },
		{ Info = Info2 }
	;
		%
		% Compare the oldest of the timestamps of the touched
		% files with the timestamps of the dependencies.
		%
		list__map_foldl2(get_timestamp_file_timestamp,
			TouchedTargetFiles, TouchedTargetFileTimestamps,
			Info2, Info3),
		list__map_foldl2(get_file_timestamp([dir__this_directory]),
			TouchedFiles, TouchedFileTimestamps, Info3, Info4),
		{ MaybeOldestTimestamp0 = list__foldl(find_oldest_timestamp,
			TouchedTargetFileTimestamps, ok(newest_timestamp)) },
		{ MaybeOldestTimestamp = list__foldl(find_oldest_timestamp,
			TouchedFileTimestamps, MaybeOldestTimestamp0) },

		get_file_name(no, TargetFile, TargetFileName, Info4, Info5),
		check_dependencies(TargetFileName, MaybeOldestTimestamp,
			MakeDepsSuccess, DepFilesToMake,
			DepsResult, Info5, Info)
	).

%-----------------------------------------------------------------------------%

:- pred build_target(compilation_task_result::in, target_file::in,
	module_imports::in, list(target_file)::in, list(file_name)::in,
	bool::out, make_info::in, make_info::out,
	io::di, io::uo) is det.

build_target(CompilationTask, TargetFile, Imports, TouchedTargetFiles,
		TouchedFiles, Succeeded, Info0, Info) -->
	maybe_make_target_message(TargetFile),
	{ TargetFile = ModuleName - _FileType },
	{ CompilationTask = Task - TaskOptions },
	(
		{ CompilationTask =
			process_module(compile_to_target_code) - _ },
		\+ { can_fork }
	->
		% We need a temporary file to pass the arguments to
		% the mmc process which will do the compilation.
		% It's created here (not in invoke_mmc) so it can be
		% cleaned up by build_with_check_for_interrupt.
		io__make_temp(ArgFileName),
		{ MaybeArgFileName = yes(ArgFileName) }
	;
		{ MaybeArgFileName = no }
	),
	{ Cleanup =
		(pred(MakeInfo0::in, MakeInfo::out, di, uo) is det -->
			% XXX Remove `.int.tmp' files.
			list__foldl2(remove_target_file, TouchedTargetFiles,
				MakeInfo0, MakeInfo1),
			list__foldl2(remove_file, TouchedFiles,
				MakeInfo1, MakeInfo),
			(
				{ MaybeArgFileName = yes(ArgFileName2) },
				io__remove_file(ArgFileName2, _)
			;
				{ MaybeArgFileName = no }
			)
		) },
	build_with_check_for_interrupt(
	    build_with_module_options_and_output_redirect(ModuleName,
		TaskOptions,
		build_target_2(ModuleName, Task, MaybeArgFileName, Imports)),
	    Cleanup, Succeeded, Info0, Info1),
    	record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
	    TouchedFiles, Info1, Info).

:- pred build_target_2(module_name::in, compilation_task_type::in,
	maybe(file_name)::in, module_imports::in, list(string)::in,
	io__output_stream::in, bool::out, make_info::in, make_info::out,
	io::di, io::uo) is det.

build_target_2(ModuleName, process_module(ModuleTask), ArgFileName,
		_Imports, AllOptionArgs, ErrorStream,
		Succeeded, Info, Info) -->
	{ mdbcomp__prim_data__sym_name_to_string(ModuleName, ".", ModuleArg) },

	globals__io_lookup_bool_option(verbose_commands, Verbose),
	( { Verbose = yes } ->
		{ AllArgs = list__append(AllOptionArgs, [ModuleArg]) },
		io__write_string("Invoking command `mmc "),
		% XXX Don't write the default options.
		io__write_list(list__map(quote_arg, AllArgs), " ",
			io__write_string),
		io__write_string("'"),
		io__nl
	;
		[]
	),

	%
	% Run compilations to target code in a separate process.
	% This is necessary for `--target asm' because the GCC
	% backend can only be invoked once per process. It's a good
	% idea for other the backends because it avoids problems with
	% the Boehm GC retaining memory by scanning too much of the
	% Mercury stacks. If the compilation is run in a separate
	% process, it is also easier to kill if an interrupt arrives.
	%
	% We do the same for intermodule-optimization interfaces
	% because if type checking gets overloaded by ambiguities
	% it can be difficult to kill the compiler otherwise.
	io__set_output_stream(ErrorStream, OldOutputStream),
	(
		(
			{ ModuleTask = compile_to_target_code }
		;
			{ ModuleTask = make_optimization_interface }
		)	
	->
		call_in_forked_process(call_mercury_compile_main([ModuleArg]),
			invoke_mmc(ErrorStream, ArgFileName,
				AllOptionArgs ++ [ModuleArg]),
			Succeeded)
	;
		call_mercury_compile_main([ModuleArg], Succeeded)
	),
	io__set_output_stream(OldOutputStream, _),

	(
		{ ModuleTask = compile_to_target_code
		; ModuleTask = errorcheck
		}
	->
		% The `.err_date' file is needed because the `.err'
		% file is touched by all phases of compilation, including
		% writing interfaces.
		touch_interface_datestamp(ModuleName, ".err_date")
	;
		[]
	).

build_target_2(ModuleName, target_code_to_object_code(PIC), _,
		Imports, _, ErrorStream, Succeeded, Info, Info) -->
	globals__io_get_target(CompilationTarget),

	% Run the compilation in a child process so it can
	% be killed if an interrupt arrives.
	call_in_forked_process(
			build_object_code(ModuleName, CompilationTarget, PIC,
				ErrorStream, Imports),
			Succeeded).

build_target_2(ModuleName, foreign_code_to_object_code(PIC, Lang), _,
		Imports, _, ErrorStream, Succeeded, Info, Info) -->
	foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile),

	% Run the compilation in a child process so it can
	% be killed if an interrupt arrives.
	call_in_forked_process(
			compile_foreign_code_file(ErrorStream, PIC,
					Imports, ForeignCodeFile),
			Succeeded).

build_target_2(ModuleName, fact_table_code_to_object_code(PIC, FactTableFile),
		_, Imports, _, ErrorStream, Succeeded, Info, Info) -->
	fact_table_foreign_code_file(ModuleName, PIC, FactTableFile,
		FactTableForeignCode),

	% Run the compilation in a child process so it can
	% be killed if an interrupt arrives.
	call_in_forked_process(
			compile_foreign_code_file(ErrorStream, PIC,
					Imports, FactTableForeignCode),
			Succeeded).

:- pred build_object_code(module_name::in, compilation_target::in, pic::in,
	io__output_stream::in, module_imports::in, bool::out,
	io::di, io::uo) is det.

build_object_code(ModuleName, c, PIC, ErrorStream, _Imports, Succeeded) -->
	compile_target_code__compile_c_file(ErrorStream, PIC, ModuleName,
		Succeeded).
build_object_code(ModuleName, asm, PIC, ErrorStream, _Imports, Succeeded) -->
	compile_target_code__assemble(ErrorStream, PIC, ModuleName,
		Succeeded).
build_object_code(ModuleName, java, _, ErrorStream, _Imports, Succeeded) -->
	module_name_to_file_name(ModuleName, ".java", yes, JavaFile),
	compile_target_code__compile_java_file(ErrorStream, JavaFile,
		Succeeded).
build_object_code(ModuleName, il, _, ErrorStream, Imports, Succeeded) -->
	compile_target_code__il_assemble(ErrorStream, ModuleName,
		Imports ^ has_main, Succeeded).

:- pred compile_foreign_code_file(io__output_stream::in, pic::in,
	module_imports::in, foreign_code_file::in, bool::out,
	io::di, io::uo) is det.

compile_foreign_code_file(ErrorStream, PIC, _Imports,
		foreign_code_file(c, CFile, ObjFile), Succeeded) -->
	compile_target_code__compile_c_file(ErrorStream, PIC,
		CFile, ObjFile, Succeeded).
compile_foreign_code_file(ErrorStream, _, _Imports,
		foreign_code_file(il, ILFile, DLLFile), Succeeded) -->
	compile_target_code__il_assemble(ErrorStream, ILFile, DLLFile,
		no_main, Succeeded).
compile_foreign_code_file(ErrorStream, _, _Imports,
		foreign_code_file(java, JavaFile, _ClassFile), Succeeded) -->
	compile_target_code__compile_java_file(ErrorStream, JavaFile,
		Succeeded).
compile_foreign_code_file(ErrorStream, _, _Imports,
		foreign_code_file(managed_cplusplus, MCPPFile, DLLFile),
		Succeeded) -->
	compile_target_code__compile_managed_cplusplus_file(ErrorStream,
		MCPPFile, DLLFile, Succeeded).
compile_foreign_code_file(ErrorStream, _, Imports,
		foreign_code_file(csharp, CSharpFile, DLLFile),
		Succeeded) -->
	compile_target_code__compile_csharp_file(ErrorStream, Imports,
		CSharpFile, DLLFile, Succeeded).

%-----------------------------------------------------------------------------%

:- pred foreign_code_file(module_name::in, pic::in, foreign_language::in,
		foreign_code_file::out, io::di, io::uo) is det.

foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile) -->
	globals__io_get_globals(Globals),
	{
		ForeignModName0 = foreign_language_module_name(
				ModuleName, Lang),
		SrcExt0 = foreign_language_file_extension(Lang)
	->
		ForeignModName = ForeignModName0,
		SrcExt = SrcExt0
	;
		unexpected(this_file, "unsupported foreign language")
	},
	{ ObjExt = get_object_extension(Globals, PIC) },
	module_name_to_file_name(ForeignModName, SrcExt, yes, SrcFileName),
	module_name_to_file_name(ForeignModName, ObjExt, yes, ObjFileName),
	{ ForeignCodeFile = foreign_code_file(Lang, SrcFileName, ObjFileName) }.

:- pred fact_table_foreign_code_file(module_name::in, pic::in, string::in,
		foreign_code_file::out, io::di, io::uo) is det.

fact_table_foreign_code_file(ModuleName, PIC, FactTableName,
		ForeignCodeFile) -->
	globals__io_get_globals(Globals),
	{ ObjExt = get_object_extension(Globals, PIC) },
	fact_table_file_name(ModuleName, FactTableName, ".c", yes, CFile),
	fact_table_file_name(ModuleName, FactTableName, ObjExt, yes, ObjFile),
	{ ForeignCodeFile = foreign_code_file(c, CFile, ObjFile) }.

:- func get_object_extension(globals, pic) = string.

get_object_extension(Globals, PIC) = Ext :-
	globals__get_target(Globals, CompilationTarget),
	( CompilationTarget = c,
		maybe_pic_object_file_extension(Globals, PIC, Ext)
	; CompilationTarget = asm,
		maybe_pic_object_file_extension(Globals, PIC, Ext)
	; CompilationTarget = il,
		Ext = ".dll"
	; CompilationTarget = java,
		sorry(this_file, "object extension for java")
	).

%-----------------------------------------------------------------------------%

:- pred call_mercury_compile_main(list(string)::in, bool::out,
	io::di, io::uo) is det.

call_mercury_compile_main(Args, Succeeded) -->
	io__get_exit_status(Status0),
	io__set_exit_status(0),
	mercury_compile__main(Args),
	io__get_exit_status(Status),
	{ Succeeded = ( Status = 0 -> yes ; no ) },
	io__set_exit_status(Status0).

:- pred invoke_mmc(io__output_stream::in, maybe(file_name)::in,
	list(string)::in, bool::out, io::di, io::uo) is det.

invoke_mmc(ErrorStream, MaybeArgFileName, Args, Succeeded) -->
	io__progname("", ProgName),
	( { ProgName = "" } ->
		io__get_environment_var("MERCURY_COMPILER",
			MaybeMercuryCompiler),
		{ MaybeMercuryCompiler = yes(MercuryCompiler)
		; MaybeMercuryCompiler = no, MercuryCompiler = "mmc"
		}
	;
		{ MercuryCompiler = ProgName }
	),

	{ QuotedArgs = list__map(quote_arg, Args) },

	% Some operating systems (e.g. Windows) have shells with
	% ludicrously short limits on the length of command lines,
	% so we need to write the arguments to a file which will
	% be read by the child mmc process.
	% This code is only called if fork() doesn't work, so there's
	% no point checking whether the shell actually has this
	% limitation.
	% The temporary file is created by the caller so that it will be
	% removed by build_with_check_for_interrupt if an interrupt occurs.
	{
		MaybeArgFileName = yes(ArgFileName)
	;
		MaybeArgFileName = no,
		error(
		"make.module_target.invoke_mmc: argument file not created")
	},

	io__open_output(ArgFileName, ArgFileOpenRes),
	(
		{ ArgFileOpenRes = ok(ArgFileStream) },
		io__write_string(ArgFileStream, "MCFLAGS = "),
		io__write_list(ArgFileStream, QuotedArgs, " ",
			io__write_string),
		io__nl(ArgFileStream),
		io__close_output(ArgFileStream),

		{ Command = string__join_list(" ",
			[quote_arg(MercuryCompiler),
				"--arg-file", quote_arg(ArgFileName)]) },

		% We've already written the command.
		{ CommandVerbosity = verbose },
		invoke_system_command(ErrorStream,
			CommandVerbosity, Command, Succeeded)
	;
		{ ArgFileOpenRes = error(Error) },
		{ Succeeded = no },
		io__write_string("Error opening `"),
		io__write_string(ArgFileName),
		io__write_string("' for output: "),
		io__write_string(io__error_message(Error)),
		io__nl
	),
	io__remove_file(ArgFileName, _).

%-----------------------------------------------------------------------------%

record_made_target(TargetFile, CompilationTask, Succeeded, Info0, Info) -->
	touched_files(TargetFile, CompilationTask, TouchedTargetFiles,
		TouchedFiles, Info0, Info1),
	record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
		TouchedFiles, Info1, Info).

:- pred record_made_target_2(bool::in, target_file::in, list(target_file)::in,
	list(file_name)::in, make_info::in, make_info::out, io::di, io::uo)
	is det.

record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
		OtherTouchedFiles, Info0, Info) -->
	( { Succeeded = yes } ->
		{ TargetStatus = up_to_date },
		{ Info1 = Info0 }
	;
		{ TargetStatus = error },
		{ Info1 = Info0 },
		target_file_error(TargetFile)
	),

	{ list__foldl(update_target_status(TargetStatus),
	    TouchedTargetFiles, Info1, Info2) },

	{ DeleteTimestamp =
	    (pred(TouchedFile::in, MakeInfo0::in, MakeInfo::out) is det :-
		MakeInfo = MakeInfo0 ^ file_timestamps :=
			map__delete(MakeInfo0 ^ file_timestamps, TouchedFile)
	    ) },
	list__map_foldl2(get_file_name(no), TouchedTargetFiles,
		TouchedTargetFileNames, Info2, Info3),
	{ list__foldl(DeleteTimestamp, TouchedTargetFileNames, Info3, Info4) },
	{ list__foldl(DeleteTimestamp, OtherTouchedFiles, Info4, Info) }.

:- pred update_target_status(dependency_status::in, target_file::in,
		make_info::in, make_info::out) is det.

update_target_status(TargetStatus, TargetFile, Info,
	Info ^ dependency_status ^ elem(target(TargetFile)) := TargetStatus).

%-----------------------------------------------------------------------------%

:- type compilation_task_result == pair(compilation_task_type, list(string)).

:- func compilation_task(globals, module_target_type) =
	compilation_task_result.

compilation_task(_, source) = _ :- error("compilation_task").
compilation_task(_, errors) =
		process_module(errorcheck) - ["--errorcheck-only"].
compilation_task(_, unqualified_short_interface) =
		process_module(make_short_interface) -
				["--make-short-interface"].
compilation_task(Globals, short_interface) =
		compilation_task(Globals, long_interface).
compilation_task(_, long_interface) =
		process_module(make_interface) - ["--make-interface"].
compilation_task(_, private_interface) =
		process_module(make_private_interface) -
			["--make-private-interface"].
compilation_task(_, intermodule_interface) =
		process_module(make_optimization_interface) -
			["--make-optimization-interface"].
compilation_task(_, aditi_code) =
		process_module(compile_to_target_code) - ["--aditi-only"].
compilation_task(Globals, c_header(_)) = compilation_task(Globals, c_code).
compilation_task(_, c_code) = process_module(compile_to_target_code) -
					["--compile-to-c"].
compilation_task(_, il_code) = process_module(compile_to_target_code) -
					["--il-only"].
compilation_task(_, il_asm) = target_code_to_object_code(non_pic) - [].
compilation_task(_, java_code) = process_module(compile_to_target_code) -
					["--java-only"].
compilation_task(_, asm_code(PIC)) =
		process_module(compile_to_target_code) -
			( PIC = pic -> ["--pic"] ; [] ).
compilation_task(_, object_code(PIC)) =
	target_code_to_object_code(PIC) - get_pic_flags(PIC).
compilation_task(_, foreign_il_asm(Lang)) =
	foreign_code_to_object_code(non_pic, Lang) - [].
compilation_task(_, foreign_object(PIC, Lang)) =
	foreign_code_to_object_code(PIC, Lang) - get_pic_flags(PIC).
compilation_task(_, fact_table_object(PIC, FactTable)) =
	fact_table_code_to_object_code(PIC, FactTable) - get_pic_flags(PIC).

:- func get_pic_flags(pic) = list(string).

% `--pic-reg' is harmless for architectures and grades where
% it is not needed (it's only needed for grades using
% GCC global register variables on x86).
get_pic_flags(pic) = ["--pic", "--pic-reg"].
get_pic_flags(link_with_pic) = ["--pic-reg"].
get_pic_flags(non_pic) = [].

	% Find the files which could be touched by a compilation task.
:- pred touched_files(target_file::in, compilation_task_type::in,
	list(target_file)::out, list(file_name)::out,
	make_info::in, make_info::out, io::di, io::uo) is det.

touched_files(TargetFile, process_module(Task), TouchedTargetFiles,
		TouchedFileNames, Info0, Info) -->
	{ TargetFile = ModuleName - FileType },
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info1),
	{ MaybeImports = yes(Imports0) ->
		Imports = Imports0
	;
		% This error should have been caught earlier.
		% We shouldn't be attempting to build a target
		% if we couldn't find the dependencies for the
		% module.
		error("touched_files: no module dependencies")
	},

	{ NestedChildren = Imports ^ nested_children },
	{ SourceFileModuleNames = [ModuleName | NestedChildren] },

	list__map_foldl2(get_module_dependencies, NestedChildren,
		MaybeNestedImportsList, Info1, Info),
	{
	    list__map(
		(pred(yes(NestedModuleImports)::in,
	    			NestedModuleImports::out) is semidet),
		MaybeNestedImportsList, NestedImportsList)
	->
	    ModuleImportsList = [Imports | NestedImportsList]
	;
		% This error should have been caught earlier.
		% We shouldn't be attempting to build a target
		% if we couldn't find the dependencies for the
		% module or its nested sub-modules.
	    error("touched_files: no nested module dependencies")
	},

	globals__io_get_target(CompilationTarget),
	{ Task = compile_to_target_code, CompilationTarget = asm ->
		% For `--target asm' the code for the nested children
		% is placed in the `.s' file for the top-level module
		% in the source file.
		TargetModuleNames = [ModuleName]
	;
		TargetModuleNames = SourceFileModuleNames
	},

	%
	% Find out what header files are generated.
	%
	(
	    { Task = compile_to_target_code }
	->
	    list__map_foldl(
		    external_foreign_code_files(target_type_to_pic(FileType)),
	    	    ModuleImportsList, ForeignCodeFileList),
	    { ForeignCodeFiles = list__map(
	    		(func(ForeignFile) = ForeignFile ^ target_file),
			list__condense(ForeignCodeFileList)) },
	    (
		{ CompilationTarget = c },
		globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
		( { HighLevelCode = yes } ->
		    %
		    % When compiling to high-level C, we always generate
		    % a header file.
		    %
		    { HeaderModuleNames = SourceFileModuleNames },
		    { HeaderTargets0 = make_target_list(HeaderModuleNames,
		    			c_header(mih)) }
		;
		    { HeaderTargets0 = [] }
		)
	    ;
	        { CompilationTarget = asm },
		%
		% When compiling to assembler, we only generate
		% a header file if the module contains foreign code.
		%
		{ HeaderModuleNames =
		    list__filter_map(
			(func(MImports) = MImports ^ module_name is semidet :-
			    contains_foreign_code(_) = MImports ^ foreign_code
			), ModuleImportsList) },
		{ HeaderTargets0 = make_target_list(HeaderModuleNames,
					c_header(mih)) }
	    ;
	    	{ CompilationTarget = il },
		{ HeaderTargets0 = [] }
	    ;
	    	{ CompilationTarget = java },
		{ HeaderTargets0 = [] }
	    ),

	    { ( CompilationTarget = c ; CompilationTarget = asm ) ->
		Names = SourceFileModuleNames,
	    	HeaderTargets = make_target_list(Names, c_header(mh))
				++ HeaderTargets0
	    ;
		HeaderTargets = HeaderTargets0
	    },

	    { TouchedTargetFiles0 =
			make_target_list(TargetModuleNames, FileType) },
	    { TouchedTargetFiles = TouchedTargetFiles0 ++ HeaderTargets }
	;
	    { Task = make_interface }
	->
	    % Both long and short interface files are produced
	    % when making the interface.
    	    { ForeignCodeFiles = [] },
	    { TouchedTargetFiles =
		make_target_list(TargetModuleNames, long_interface)
		++ make_target_list(TargetModuleNames, short_interface) }
	;
    	    { ForeignCodeFiles = [] },
	    { TouchedTargetFiles =
			make_target_list(TargetModuleNames, FileType) }
	),

	globals__io_get_globals(Globals),
	list__foldl2(
	    (pred((TargetModuleName - TargetFileType)::in, TimestampFiles0::in,
			TimestampFiles1::out, di, uo) is det -->
		(
			{ TimestampExt =
				timestamp_extension(Globals, TargetFileType) }
		->
			module_name_to_file_name(TargetModuleName,
				TimestampExt, no, TimestampFile),
			{ TimestampFiles1 =
				[TimestampFile | TimestampFiles0] }
		;
			{ TimestampFiles1 = TimestampFiles0 }
		)
	    ), TouchedTargetFiles, [], TimestampFileNames),

	{ TouchedFileNames = list__condense([ForeignCodeFiles,
					TimestampFileNames]) }.

touched_files(TargetFile, target_code_to_object_code(_),
		[TargetFile], [], Info, Info) -->
	[].

touched_files(TargetFile, foreign_code_to_object_code(PIC, Lang),
		[TargetFile], [ForeignObjectFile], Info, Info) -->
	{ TargetFile = ModuleName - _ },
	foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile),
	{ ForeignObjectFile = ForeignCodeFile ^ object_file }.

touched_files(TargetFile, fact_table_code_to_object_code(PIC, FactTableName),
		[TargetFile], [FactTableObjectFile], Info, Info) -->
	{ TargetFile = ModuleName - _ },
	globals__io_get_globals(Globals),
	{ ObjExt = get_object_extension(Globals, PIC) },
	fact_table_file_name(ModuleName, FactTableName, ObjExt, yes,
		FactTableObjectFile).

external_foreign_code_files(PIC, Imports, ForeignFiles) -->
	%
	% Find externally compiled foreign code files for
	% `:- pragma foreign_proc' declarations.
	%
	maybe_pic_object_file_extension(PIC, ObjExt),
	globals__io_get_target(CompilationTarget),
	{ ModuleName = Imports ^ module_name },
	(
		{ CompilationTarget = asm },
		{ Imports ^ foreign_code = contains_foreign_code(Langs) },
		{ set__member(c, Langs) }
	->
		module_name_to_file_name(
			foreign_language_module_name(ModuleName, c), ".c",
			no, CCodeFileName),
		module_name_to_file_name(
			foreign_language_module_name(ModuleName, c), ObjExt,
			no, ObjFileName),
		{ ForeignFiles0 =
			[foreign_code_file(c, CCodeFileName, ObjFileName) ] }
	;
		{ CompilationTarget = il },
		{ Imports ^ foreign_code = contains_foreign_code(Langs) }
	->
		list__map_foldl(external_foreign_code_files_for_il(ModuleName),
			set__to_sorted_list(Langs), ForeignFilesList),
		{ list__condense(ForeignFilesList, ForeignFiles0) }
	;
		{ ForeignFiles0 = [] }
	),

	%
	% Find externally compiled foreign code files for fact tables.
	%
	( { CompilationTarget = c ; CompilationTarget = asm } ->
		list__map_foldl(
			(pred(FactTableFile::in, FactTableForeignFile::out,
					di, uo) is det -->
				fact_table_file_name(ModuleName, FactTableFile,
					".c", no, FactTableCFile),
				fact_table_file_name(ModuleName, FactTableFile,
					ObjExt, no, FactTableObjFile),
				{ FactTableForeignFile = foreign_code_file(c,
					FactTableCFile, FactTableObjFile) }
			), Imports ^ fact_table_deps, FactTableForeignFiles),
		{ ForeignFiles = ForeignFiles0 ++ FactTableForeignFiles }
	;
		{ ForeignFiles = ForeignFiles0 }
	).

:- pred external_foreign_code_files_for_il(module_name::in,
	foreign_language::in, list(foreign_code_file)::out,
	io::di, io::uo) is det.

external_foreign_code_files_for_il(ModuleName, Language, ForeignFiles, !IO) :-
	(
		ForeignModuleName = foreign_language_module_name(ModuleName,
			Language),
		ForeignExt = foreign_language_file_extension(Language)
	->
		module_name_to_file_name(ForeignModuleName, ForeignExt, yes,
			ForeignFileName, !IO),
		module_name_to_file_name(ForeignModuleName, ".dll", yes,
			ForeignDLLFileName, !IO),
		ForeignFiles = [foreign_code_file(Language, ForeignFileName,
			ForeignDLLFileName)]
	;
		% No external file is generated for this foreign language.
		ForeignFiles = []
	).

:- func target_type_to_pic(module_target_type) = pic.

target_type_to_pic(TargetType) = Result :-
	( TargetType = asm_code(PIC) ->
		Result = PIC
	; TargetType = object_code(PIC) ->
		Result = PIC
	;
		Result = non_pic
	).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "make.module_target.m".

%-----------------------------------------------------------------------------%
