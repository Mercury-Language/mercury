%-----------------------------------------------------------------------------%
% Copyright (C) 2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: compile_target_code.m
% Main authors: stayl
%
% Code to compile the generated `.c', `.s', `.o', etc, files.
%
%-----------------------------------------------------------------------------%
:- module backend_libs__compile_target_code.

:- interface.

:- import_module parse_tree__prog_data, parse_tree__prog_io.
:- import_module parse_tree__modules.

:- import_module bool, list, io, std_util.


	% Are we generating position independent code (for use in a
	% shared library)? On some architectures, pic and non-pic
	% code is incompatible, so we need to generate `.o' and `.pic_o'
	% files.
:- type pic
	--->    pic
	;       non_pic
	.

	% compile_c_file(ErrorStream, PIC, CFile, ObjFile, Succeeded).
:- pred compile_c_file(io__output_stream, pic, string, string, bool,
		io__state, io__state).
:- mode compile_c_file(in, in, in, in, out, di, uo) is det.

	% compile_c_file(ErrorStream, PIC, ModuleName, Succeeded).
:- pred compile_c_file(io__output_stream, pic, module_name, bool,
		io__state, io__state).
:- mode compile_c_file(in, in, in, out, di, uo) is det.

	% assemble(ErrorStream, PIC, ModuleName, Succeeded).
:- pred assemble(io__output_stream, pic, module_name,
		bool, io__state, io__state).
:- mode assemble(in, in, in, out, di, uo) is det.
	
	% compile_java_file(ErrorStream, ModuleName, Succeeded).
:- pred compile_java_file(io__output_stream, module_name, bool,
		io__state, io__state).
:- mode compile_java_file(in, in, out, di, uo) is det.

	% il_assemble(ErrorStream, ModuleName, HasMain, Succeeded).
:- pred il_assemble(io__output_stream, module_name,
		has_main, bool, io__state, io__state).
:- mode il_assemble(in, in, in, out, di, uo) is det.

	% il_assemble(ErrorStream, ILFile, DLLFile, HasMain, Succeeded).
:- pred il_assemble(io__output_stream, file_name, file_name,
		has_main, bool, io__state, io__state).
:- mode il_assemble(in, in, in, in, out, di, uo) is det.

	% compile_managed_cplusplus_file(ErrorStream,
	%		MCPPFile, DLLFile, Succeeded).
:- pred compile_managed_cplusplus_file(io__output_stream,
		file_name, file_name, bool, io__state, io__state).
:- mode compile_managed_cplusplus_file(in, in, in, out, di, uo) is det.

	% compile_csharp_file(ErrorStream, C#File, DLLFile, Succeeded).
:- pred compile_csharp_file(io__output_stream, file_name, file_name,
		bool, io__state, io__state).
:- mode compile_csharp_file(in, in, in, out, di, uo) is det.

	% make_init_obj_file(ErrorStream, MainModuleName,
	%		AllModuleNames, MaybeInitObjFileName).
:- pred make_init_obj_file(io__output_stream, module_name, list(module_name),
		maybe(file_name), io__state, io__state).
:- mode make_init_obj_file(in, in, in, out, di, uo) is det.

:- type linked_target_type
	--->	executable
	;	static_library
	;	shared_library
	.

	% link(TargetType, MainModuleName, ObjectFileNames, Succeeded).
:- pred link(io__output_stream, linked_target_type, module_name,
		list(string), bool, io__state, io__state).
:- mode link(in, in, in, in, out, di, uo) is det.

	% link_module_list(ModulesToLink, Succeeded).
	%
	% The elements of ModulesToLink are the output of
	% `module_name_to_filename(ModuleName, "", no, ModuleToLink)'
	% for each module in the program.
:- pred link_module_list(list(string), bool, io__state, io__state).
:- mode link_module_list(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
	% Code to deal with `--split-c-files'.

	% split_c_to_obj(ErrorStream, ModuleName, NumChunks, Succeeded).
	% Compile the `.c' files produced for a module with `--split-c-files'.
:- pred split_c_to_obj(io__output_stream, module_name,
		int, bool, io__state, io__state).
:- mode split_c_to_obj(in, in, in, out, di, uo) is det.

	% Write the number of `.c' files written by this
	% compilation with `--split-c-files'.
:- pred write_num_split_c_files(module_name, int, bool, io__state, io__state).
:- mode write_num_split_c_files(in, in, out, di, uo) is det.

	% Find the number of `.c' files written by a previous
	% compilation with `--split-c-files'.
:- pred read_num_split_c_files(module_name, maybe_error(int),
		io__state, io__state).
:- mode read_num_split_c_files(in, out, di, uo) is det.

	% remove_split_c_output_files(ModuleName, NumChunks).
	%
	% Remove the `.c' and `.o' files written by a previous
	% compilation with `--split-c-files'.
:- pred remove_split_c_output_files(module_name, int, io__state, io__state).
:- mode remove_split_c_output_files(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module libs__globals, libs__options, libs__handle_options.
:- import_module hlds__passes_aux, libs__trace_params.

:- import_module dir, int, require, string.

il_assemble(ErrorStream, ModuleName,
			HasMain, Succeeded) -->
	module_name_to_file_name(ModuleName, ".il", no, IL_File),
	( { HasMain = has_main } ->
		module_name_to_file_name(ModuleName, ".exe", no, TargetFile)
	;	
		module_name_to_file_name(ModuleName, ".dll", no, TargetFile)
	),
	il_assemble(ErrorStream, IL_File, TargetFile,
		HasMain, Succeeded).
	
il_assemble(ErrorStream, IL_File, TargetFile,
		HasMain, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(sign_assembly, SignAssembly),
	maybe_write_string(Verbose, "% Assembling `"),
	maybe_write_string(Verbose, IL_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(il_assembler, ILASM),
	globals__io_lookup_accumulating_option(ilasm_flags, ILASMFlagsList),
	{ join_string_list(ILASMFlagsList, "", "", " ", ILASMFlags) },
	{ SignAssembly = yes ->
		SignOpt = "/keyf=mercury.sn "
	;
		SignOpt = ""
	},
	{ Verbose = yes ->
		VerboseOpt = ""
	;
		VerboseOpt = "/quiet "
	},
	globals__io_lookup_bool_option(target_debug, Debug),
	{ Debug = yes ->
		DebugOpt = "/debug "
	;
		DebugOpt = ""
	},
	{ HasMain = has_main ->
		TargetOpt = ""
	;	
		TargetOpt = "/dll "
	},
	{ string__append_list([ILASM, " ", SignOpt, VerboseOpt, DebugOpt,
		TargetOpt, ILASMFlags, " /out=", TargetFile,
		" ", IL_File], Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

compile_managed_cplusplus_file(ErrorStream,
		MCPPFileName, DLLFileName, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, MCPPFileName),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(mcpp_compiler, MCPP),
	globals__io_lookup_accumulating_option(mcpp_flags, MCPPFlagsList),
	{ join_string_list(MCPPFlagsList, "", "", " ", MCPPFlags) },
	globals__io_lookup_bool_option(target_debug, Debug),
	{ Debug = yes ->
		DebugOpt = "" % XXX
	;
		DebugOpt = ""
	},

	% XXX Should we introduce a `--mcpp-include-directory' option?
	globals__io_lookup_accumulating_option(c_include_directory,
	 	C_Incl_Dirs),
	{ InclOpts = string__append_list(list__condense(list__map(
	 	(func(C_INCL) = ["-I", C_INCL, " "]), C_Incl_Dirs))) },

	% XXX Should we use a separate dll_directories options?
	globals__io_lookup_accumulating_option(link_library_directories,
	 	DLLDirs),
	{ DLLDirOpts = string__append_list(list__condense(list__map(
	 	(func(DLLDir) = ["-AI", DLLDir, " "]), DLLDirs))) },

	{ string__append_list([MCPP, " -CLR ", DebugOpt, InclOpts,
		DLLDirOpts, MCPPFlags, " ", MCPPFileName,
		"-link -noentry mscoree.lib -dll -out:", DLLFileName],
		Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

compile_csharp_file(ErrorStream,
		CSharpFileName, DLLFileName, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, CSharpFileName),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(csharp_compiler, CSC),
	globals__io_lookup_accumulating_option(csharp_flags, CSCFlagsList),
	{ join_string_list(CSCFlagsList, "", "", " ", CSCFlags) },
	globals__io_lookup_bool_option(target_debug, Debug),
	{ Debug = yes ->
		DebugOpt = "" % XXX
	;
		DebugOpt = ""
	},

	% XXX Should we use a separate dll_directories options?
	globals__io_lookup_accumulating_option(link_library_directories,
	 	DLLDirs),
	{ DLLDirOpts = string__append_list(list__condense(list__map(
	 	(func(DLLDir) = ["/lib:", DLLDir, " "]), DLLDirs))) },

	{ string__append_list([CSC, " -CLR ", DebugOpt,
		" /t:library ", DLLDirOpts, CSCFlags, " ",
		" /out:", DLLFileName, CSharpFileName], Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

%-----------------------------------------------------------------------------%

split_c_to_obj(ErrorStream, ModuleName, NumChunks, Succeeded) -->
	split_c_to_obj(ErrorStream, ModuleName, 0, NumChunks, Succeeded).

	% compile each of the C files in `<module>.dir'
:- pred split_c_to_obj(io__output_stream, module_name,
		int, int, bool, io__state, io__state).
:- mode split_c_to_obj(in, in, in, in, out, di, uo) is det.

split_c_to_obj(ErrorStream, ModuleName,
		Chunk, NumChunks, Succeeded) -->
	( { Chunk > NumChunks } ->
		{ Succeeded = yes }
	;
		globals__io_lookup_string_option(object_file_extension, Obj),
		module_name_to_split_c_file_name(ModuleName, Chunk,
			".c", C_File),
		module_name_to_split_c_file_name(ModuleName, Chunk,
			Obj, O_File),
		compile_c_file(ErrorStream, non_pic,
			C_File, O_File, Succeeded0),
		( { Succeeded0 = no } ->
			{ Succeeded = no }
		;
			{ Chunk1 is Chunk + 1 },
			split_c_to_obj(ErrorStream,
				ModuleName, Chunk1, NumChunks, Succeeded)
		)
	).

% WARNING: The code here duplicates the functionality of scripts/mgnuc.in.
% Any changes there may also require changes here, and vice versa.

:- type compiler_type ---> gcc ; lcc ; unknown.

compile_c_file(ErrorStream, PIC, ModuleName, Succeeded) -->
	module_name_to_file_name(ModuleName, ".c", yes, C_File),
	(
		{ PIC = pic },
		globals__io_lookup_string_option(pic_object_file_extension,
			ObjExt)
	;
		{ PIC = non_pic },
		globals__io_lookup_string_option(object_file_extension, ObjExt)
	),
	module_name_to_file_name(ModuleName, ObjExt, yes, O_File),
	compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded).

compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_string_option(c_flag_to_name_object_file,
			NameObjectFile),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, C_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },
	
	(
		{ PIC = pic },
		globals__io_lookup_string_option(cflags_for_pic,
			CFLAGS_FOR_PIC)
	;
		{ PIC = non_pic },
		{ CFLAGS_FOR_PIC = "" }
	),

	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	globals__io_lookup_bool_option(split_c_files, SplitCFiles),
	{ (UseSubdirs = yes ; SplitCFiles = yes) ->
		% the source file (foo.c) will be compiled in a subdirectory
		% (either Mercury/cs, foo.dir, or Mercury/dirs/foo.dir,
		% depending on which of these two options is set)
		% so we need to add `-I.' so it can
		% include header files in the source directory.
		SubDirInclOpt = "-I. "
	;
		SubDirInclOpt = ""
	},
	globals__io_lookup_accumulating_option(c_include_directory,
	 	C_Incl_Dirs),
	{ InclOpt = string__append_list(list__condense(list__map(
	 	(func(C_INCL) = ["-I", C_INCL, " "]), C_Incl_Dirs))) },
	globals__io_lookup_bool_option(split_c_files, Split_C_Files),
	{ Split_C_Files = yes ->
		SplitOpt = "-DMR_SPLIT_C_FILES "
	;
		SplitOpt = ""
	},
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	( { HighLevelCode = yes } ->
		{ HighLevelCodeOpt = "-DMR_HIGHLEVEL_CODE " }
	;
		{ HighLevelCodeOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_nested_functions,
		GCC_NestedFunctions),
	( { GCC_NestedFunctions = yes } ->
		{ NestedFunctionsOpt = "-DMR_USE_GCC_NESTED_FUNCTIONS " }
	;
		{ NestedFunctionsOpt = "" }
	),
	globals__io_lookup_bool_option(highlevel_data, HighLevelData),
	( { HighLevelData = yes } ->
		{ HighLevelDataOpt = "-DMR_HIGHLEVEL_DATA " }
	;
		{ HighLevelDataOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_global_registers, GCC_Regs),
	( { GCC_Regs = yes } ->
		globals__io_lookup_string_option(cflags_for_regs,
			CFLAGS_FOR_REGS),
		{ RegOpt = "-DMR_USE_GCC_GLOBAL_REGISTERS " }
	;
		{ CFLAGS_FOR_REGS = "" },
		{ RegOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos),
	( { GCC_Gotos = yes } ->
		{ GotoOpt = "-DMR_USE_GCC_NONLOCAL_GOTOS " },
		globals__io_lookup_string_option(cflags_for_gotos,
			CFLAGS_FOR_GOTOS)
	;
		{ GotoOpt = "" },
		{ CFLAGS_FOR_GOTOS = "" }
	),
	globals__io_lookup_bool_option(asm_labels, ASM_Labels),
	{ ASM_Labels = yes ->
		AsmOpt = "-DMR_USE_ASM_LABELS "
	;
		AsmOpt = ""
	},
	globals__io_lookup_bool_option(parallel, Parallel),
	( { Parallel = yes } ->
		globals__io_lookup_string_option(cflags_for_threads,
			CFLAGS_FOR_THREADS)
	;
		{ CFLAGS_FOR_THREADS = "" }
	),
	globals__io_get_gc_method(GC_Method),
	{ GC_Method = conservative ->
		GC_Opt = "-DMR_CONSERVATIVE_GC "
	; GC_Method = accurate ->
		GC_Opt = "-DMR_NATIVE_GC "
	;
		GC_Opt = ""
	},
	globals__io_lookup_bool_option(profile_calls, ProfileCalls),
	{ ProfileCalls = yes ->
		ProfileCallsOpt = "-DMR_MPROF_PROFILE_CALLS "
	;
		ProfileCallsOpt = ""
	},
	globals__io_lookup_bool_option(profile_time, ProfileTime),
	{ ProfileTime = yes ->
		ProfileTimeOpt = "-DMR_MPROF_PROFILE_TIME "
	;
		ProfileTimeOpt = ""
	},
	globals__io_lookup_bool_option(profile_memory, ProfileMemory),
	{ ProfileMemory = yes ->
		ProfileMemoryOpt = "-DMR_MPROF_PROFILE_MEMORY "
	;
		ProfileMemoryOpt = ""
	},
	globals__io_lookup_bool_option(profile_deep, ProfileDeep),
	{ ProfileDeep = yes ->
		ProfileDeepOpt = "-DMR_DEEP_PROFILING "
	;
		ProfileDeepOpt = ""
	},
	globals__io_lookup_bool_option(pic_reg, PIC_Reg),
	{ PIC_Reg = yes ->
		PIC_Reg_Opt = "-DMR_PIC_REG "
	;
		PIC_Reg_Opt = ""
	},
	globals__io_get_tags_method(Tags_Method),
	{ Tags_Method = high ->
		TagsOpt = "-DMR_HIGHTAGS "
	;
		TagsOpt = ""
	},
	globals__io_lookup_int_option(num_tag_bits, NumTagBits),
	{ string__int_to_string(NumTagBits, NumTagBitsString) },
	{ string__append_list(
		["-DMR_TAGBITS=", NumTagBitsString, " "], NumTagBitsOpt) },
	globals__io_lookup_bool_option(require_tracing, RequireTracing),
	{ RequireTracing = yes ->
		RequireTracingOpt = "-DMR_REQUIRE_TRACING "
	;
		RequireTracingOpt = ""
	},
	globals__io_lookup_bool_option(stack_trace, StackTrace),
	{ StackTrace = yes ->
		StackTraceOpt = "-DMR_STACK_TRACE "
	;
		StackTraceOpt = ""
	},
	globals__io_lookup_bool_option(target_debug, Target_Debug),
	{ Target_Debug = yes ->
		Target_DebugOpt = "-g "
	;
		Target_DebugOpt = ""
	},
	globals__io_lookup_bool_option(low_level_debug, LL_Debug),
	{ LL_Debug = yes ->
		LL_DebugOpt = "-DMR_LOW_LEVEL_DEBUG "
	;
		LL_DebugOpt = ""
	},
	{ string__sub_string_search(CC, "gcc", _) ->
		CompilerType = gcc
	; string__sub_string_search(CC, "lcc", _) ->
		CompilerType = lcc
	;
		CompilerType = unknown
	},
	globals__io_lookup_bool_option(use_trail, UseTrail),
	{ UseTrail = yes ->
		UseTrailOpt = "-DMR_USE_TRAIL "
	;
		UseTrailOpt = ""
	},
	globals__io_lookup_bool_option(reserve_tag, ReserveTag),
	{ ReserveTag = yes ->
		ReserveTagOpt = "-DMR_RESERVE_TAG "
	;
		ReserveTagOpt = ""
	},
	globals__io_lookup_bool_option(use_minimal_model, MinimalModel),
	{ MinimalModel = yes ->
		MinimalModelOpt = "-DMR_USE_MINIMAL_MODEL "
	;
		MinimalModelOpt = ""
	},
	globals__io_lookup_bool_option(type_layout, TypeLayoutOption),
	{ TypeLayoutOption = no ->
		TypeLayoutOpt = "-DMR_NO_TYPE_LAYOUT "
	;
		TypeLayoutOpt = ""
	},
	globals__io_lookup_bool_option(c_optimize, C_optimize),
	{ C_optimize = yes ->
		( CompilerType = gcc ->
			OptimizeOpt = "-O2 -fomit-frame-pointer "
		; CompilerType = lcc ->
			OptimizeOpt = ""
		;
			OptimizeOpt = "-O "
		)
	;
		OptimizeOpt = ""
	},
	globals__io_lookup_bool_option(inline_alloc, InlineAlloc),
	{ InlineAlloc = yes ->
		InlineAllocOpt = "-DMR_INLINE_ALLOC -DSILENT "
	;
		InlineAllocOpt = ""
	},
	{ CompilerType = gcc ->
		% We don't enable `-Wpointer-arith', because it causes
		% too many complaints in system header files.
		% This is fixed in gcc 3.0, though, so at some
		% point we should re-enable this.
		%
		% If --inline-alloc is enabled, don't enable missing-prototype
		% warnings, since gc_inline.h is missing lots of prototypes.
		%
		% For a full list of the other gcc warnings that we don't
		% enable, and why, see scripts/mgnuc.in.
		( InlineAlloc = yes ->
			WarningOpt = "-Wall -Wwrite-strings -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized "
		;
			WarningOpt = "-Wall -Wwrite-strings -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized -Wstrict-prototypes "
		)
	; CompilerType = lcc ->
		WarningOpt = "-w "
	;
		WarningOpt = ""
	},
	% Be careful with the order here!  Some options override others,
	% e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
	% it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([CC, " ", SubDirInclOpt, InclOpt,
		SplitOpt, OptimizeOpt,
		HighLevelCodeOpt, NestedFunctionsOpt, HighLevelDataOpt,
		RegOpt, GotoOpt, AsmOpt,
		CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
		CFLAGS_FOR_THREADS, " ", CFLAGS_FOR_PIC, " ",
		GC_Opt, ProfileCallsOpt, ProfileTimeOpt, ProfileMemoryOpt,
		ProfileDeepOpt, PIC_Reg_Opt, TagsOpt, NumTagBitsOpt,
		Target_DebugOpt, LL_DebugOpt,
		StackTraceOpt, RequireTracingOpt,
		UseTrailOpt, ReserveTagOpt, MinimalModelOpt, TypeLayoutOpt,
		InlineAllocOpt, WarningOpt, CFLAGS,
		" -c ", C_File, " ", NameObjectFile, O_File], Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

%-----------------------------------------------------------------------------%

compile_java_file(ErrorStream, ModuleName, Succeeded) -->
	module_name_to_file_name(ModuleName, ".java", no, JavaFile),
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, JavaFile),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(java_compiler, JavaCompiler),
	globals__io_lookup_accumulating_option(java_flags, JavaFlagsList),
	{ join_string_list(JavaFlagsList, "", "", " ", JAVAFLAGS) },

	globals__io_lookup_accumulating_option(java_classpath,
	 	Java_Incl_Dirs),
	( { Java_Incl_Dirs = [] } ->
		{ InclOpt = "" }
	;
		% XXX PathSeparator should be ";" on Windows
		{ PathSeparator = ":" },
		{ join_string_list(Java_Incl_Dirs, "", "",
			PathSeparator, ClassPath) },
		{ InclOpt = string__append_list([
			"-classpath ", ClassPath, " "]) }
	),
	globals__io_lookup_bool_option(target_debug, Target_Debug),
	{ Target_Debug = yes ->
		Target_DebugOpt = "-g "
	;
		Target_DebugOpt = ""
	},
	% Be careful with the order here!  Some options may override others.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([JavaCompiler, " ", InclOpt,
		Target_DebugOpt, JAVAFLAGS, JavaFile], Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

%-----------------------------------------------------------------------------%

assemble(ErrorStream, PIC, ModuleName, Succeeded) -->
	globals__io_lookup_bool_option(pic, Pic),
	{ ( Pic = yes ; PIC = pic ) ->
		AsmExt = ".pic_s",
		GCCFLAGS_FOR_PIC = ""
	;
		AsmExt = ".s",
		GCCFLAGS_FOR_PIC = "-fpic"
	},
	module_name_to_file_name(ModuleName, AsmExt, no, AsmFile),
	globals__io_lookup_string_option(object_file_extension, Obj),
	module_name_to_file_name(ModuleName, Obj, yes, ObjFile),

	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Assembling `"),
	maybe_write_string(Verbose, AsmFile),
	maybe_write_string(Verbose, "':\n"),
	% XXX should we use new asm_* options rather than
	% reusing cc, cflags, c_flag_to_name_object_file?
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_string_option(c_flag_to_name_object_file,
			NameObjectFile),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },
	% Be careful with the order here.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([CC, " ", CFLAGS, " ", GCCFLAGS_FOR_PIC,
		" -c ", AsmFile, " ", NameObjectFile, ObjFile], Command) },
	invoke_system_command(ErrorStream, verbose_commands,
		Command, Succeeded).

%-----------------------------------------------------------------------------%

link_module_list(Modules, Succeeded) -->
	globals__io_lookup_string_option(output_file_name, OutputFileName0),
	( { OutputFileName0 = "" } ->
	    ( { Modules = [Module | _] } ->
		{ OutputFileName = Module }
	    ;
		{ error("link_module_list: no modules") }
	    )
	;
	    { OutputFileName = OutputFileName0 }
	),

	{ file_name_to_module_name(OutputFileName, MainModuleName) },

	globals__io_lookup_string_option(object_file_extension, Obj),
	globals__io_get_target(Target),
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	io__output_stream(OutputStream),
	( { Target = asm } ->
	    % for --target asm, we generate everything into a single object file
	    ( { Modules = [FirstModule | _] } ->
		join_module_list([FirstModule], Obj, [], ObjectsList)
	    ;
		{ error("link_module_list: no modules") }
	    ),
	    { MakeLibCmdOK = yes }
	; { SplitFiles = yes } ->
	    globals__io_lookup_string_option(library_extension, LibExt),
	    module_name_to_file_name(MainModuleName, LibExt,
	    	yes, SplitLibFileName),
	    { string__append(".dir/*", Obj, DirObj) },
	    join_module_list(Modules, DirObj, [], ObjectList),
	    create_archive(OutputStream, SplitLibFileName,
	    	ObjectList, MakeLibCmdOK),
	    { ObjectsList = [SplitLibFileName] }
	;
	    { MakeLibCmdOK = yes },
	    join_module_list(Modules, Obj, [], ObjectsList)
	),
	( { MakeLibCmdOK = no } ->
    	    { Succeeded = no }
	;
	    { list__map(
	    	(pred(ModuleStr::in, ModuleName::out) is det :-
			dir__basename(ModuleStr, ModuleStrBase),
			file_name_to_module_name(ModuleStrBase, ModuleName)
		),
		Modules, ModuleNames) },
	    { MustCompile = yes },
	    make_init_obj_file(OutputStream,
	    	MustCompile, MainModuleName, ModuleNames, InitObjResult),
	    (
	    	{ InitObjResult = yes(InitObjFileName) },
		globals__io_lookup_accumulating_option(link_objects,
			ExtraLinkObjectsList),
	        link(OutputStream, executable, MainModuleName,
	    	    [InitObjFileName | ObjectsList] ++ ExtraLinkObjectsList,
		    Succeeded)
	    ;
		{ InitObjResult = no },
		{ Succeeded = no }
	    )
	).

make_init_obj_file(ErrorStream,
		ModuleName, ModuleNames, Result) -->
	{ MustCompile = no },
	make_init_obj_file(ErrorStream,
		MustCompile, ModuleName, ModuleNames, Result).

:- pred make_init_obj_file(io__output_stream, bool,
	module_name, list(module_name), maybe(file_name),
	io__state, io__state).
:- mode make_init_obj_file(in,
	in, in, in, out, di, uo) is det.

make_init_obj_file(ErrorStream, MustCompile, ModuleName,
		ModuleNames, Result) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Creating initialization file...\n"),

	globals__io_get_trace_level(TraceLevel),
	{ trace_level_is_none(TraceLevel) = no ->
		TraceOpt = "--trace "
	;
		TraceOpt = ""
	},
	globals__io_get_globals(Globals),
	{ compute_grade(Globals, Grade) },

	standard_library_directory_option(StdLibOpt),
	globals__io_lookup_string_option(object_file_extension, Obj),
	{ string__append("_init", Obj, InitObj) },
	module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName),
	module_name_to_file_name(ModuleName, InitObj, yes, InitObjFileName),

	list__map_foldl(
	    (pred(ThisModuleName::in, CFileName::out, di, uo) is det -->
		module_name_to_file_name(ThisModuleName, ".c", no,
			CFileName)
	    ), ModuleNames, CFileNameList),
	{ join_string_list(CFileNameList, "", "", " ", CFileNames) },

	globals__io_lookup_accumulating_option(link_flags, LinkFlagsList),
	{ join_string_list(LinkFlagsList, "", "", " ", LinkFlags) },

	globals__io_lookup_accumulating_option(init_file_directories,
		InitFileDirsList),
	{ join_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs) },

	globals__io_lookup_accumulating_option(init_files, InitFileNamesList),
	{ join_string_list(InitFileNamesList, "", "", " ", InitFileNames) },

	globals__io_lookup_accumulating_option(trace_init_files,
		TraceInitFileNamesList),
	{ join_string_list(TraceInitFileNamesList, "--trace-init-file ",
		"", " ", TraceInitFileNames) },

	{ TmpInitCFileName = InitCFileName ++ ".tmp" },
	{ MkInitCmd = string__append_list(
		["c2init --grade ", Grade, " ", TraceOpt, StdLibOpt, LinkFlags,
		" --init-c-file ", TmpInitCFileName, " ", InitFileDirs, " ",
		TraceInitFileNames, " ", InitFileNames, " ", CFileNames]) },
	invoke_shell_command(ErrorStream, verbose, MkInitCmd, MkInitOK0),
	maybe_report_stats(Stats),
	( { MkInitOK0 = yes } ->
	    update_interface(InitCFileName, MkInitOK1),
	    (
	    	{ MkInitOK1 = yes },

		(
		    { MustCompile = yes },
		    { Compile = yes }
		;
		    { MustCompile = no },
		    io__file_modification_time(InitCFileName,
				InitCModTimeResult),
		    io__file_modification_time(InitObjFileName,
				InitObjModTimeResult),
		    {
			InitObjModTimeResult = ok(InitObjModTime),
			InitCModTimeResult = ok(InitCModTime),
			compare(TimeCompare, InitObjModTime, InitCModTime),
			( TimeCompare = (=)
			; TimeCompare = (>)
			)
		    ->
			Compile = no
		    ;
			Compile = yes
		    }
		),

		(
		    { Compile = yes },
		    maybe_write_string(Verbose,
			"% Compiling initialization file...\n"),

		    compile_c_file(ErrorStream, non_pic, InitCFileName,
		    	InitObjFileName, CompileOK),
		    maybe_report_stats(Stats),
		    ( { CompileOK = no } ->
			{ Result = no }
		    ;
			{ Result = yes(InitObjFileName) }
		    )
	        ;
		    { Compile = no },
		    { Result = yes(InitObjFileName) }
		)
	    ;
	    	{ MkInitOK1 = no },
		{ Result = no }
	    )
	;
	    { Result = no }
	).

link(ErrorStream, LinkTargetType, ModuleName,
		ObjectsList, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	globals__io_get_trace_level(TraceLevel),
	{ trace_level_is_none(TraceLevel) = no ->
		TraceOpt = "--trace "
	;
		TraceOpt = ""
	},
	globals__io_get_globals(Globals),
	{ compute_grade(Globals, Grade) },

	maybe_write_string(Verbose, "% Linking...\n"),
	( { LinkTargetType = static_library } ->
	    	globals__io_lookup_string_option(library_extension, LibExt),
		module_name_to_lib_file_name("lib", ModuleName, LibExt,
			yes, LibName),
		create_archive(ErrorStream, LibName, ObjectsList, ArCmdOK),
		maybe_report_stats(Stats),
		( { ArCmdOK = no } ->
			{ Succeeded = no }
		;
			{ Succeeded = yes }
		)
	;
		( { LinkTargetType = shared_library } ->
			{ SharedLibOpt = "--make-shared-lib " },
			globals__io_lookup_string_option(
				shared_library_extension, SharedLibExt),
			module_name_to_lib_file_name("lib", ModuleName,
				SharedLibExt, yes, OutputFileName)
		;
			{ SharedLibOpt = "" },
			globals__io_lookup_string_option(
				executable_file_extension, ExeExt),
			module_name_to_file_name(ModuleName, ExeExt,
				yes, OutputFileName)
		),
		globals__io_lookup_bool_option(target_debug, Target_Debug),
		{ Target_Debug = yes ->
			Target_Debug_Opt = "--no-strip "
		;
			Target_Debug_Opt = ""
		},
		standard_library_directory_option(StdLibOpt),
		{ join_string_list(ObjectsList, "", "", " ", Objects) },
		globals__io_lookup_accumulating_option(link_flags,
				LinkFlagsList),
		{ join_string_list(LinkFlagsList, "", "", " ", LinkFlags) },
		globals__io_lookup_accumulating_option(
				link_library_directories,
				LinkLibraryDirectoriesList),
		{ join_string_list(LinkLibraryDirectoriesList, "-L", "",
				" ", LinkLibraryDirectories) },
		globals__io_lookup_accumulating_option(link_libraries,
				LinkLibrariesList),
		{ join_string_list(LinkLibrariesList, "-l", "", " ",
				LinkLibraries) },
		{ string__append_list(
			["ml --grade ", Grade, " ", SharedLibOpt,
			Target_Debug_Opt, TraceOpt, StdLibOpt, LinkFlags,
			" -o ", OutputFileName, " ", Objects, " ", 
			LinkLibraryDirectories, " ", LinkLibraries],
			LinkCmd) },
		invoke_shell_command(ErrorStream, verbose_commands,
			LinkCmd, Succeeded),
		maybe_report_stats(Stats)
	).

:- pred create_archive(io__output_stream, file_name, list(file_name),
		bool, io__state, io__state).
:- mode create_archive(in, in, in, out, di, uo) is det.

create_archive(ErrorStream, LibFileName, ObjectList, MakeLibCmdOK) -->
	globals__io_lookup_string_option(create_archive_command, ArCmd),
	globals__io_lookup_accumulating_option(
		create_archive_command_flags, ArFlagsList),
	{ join_string_list(ArFlagsList, "", "", " ", ArFlags) },
	globals__io_lookup_string_option(
		create_archive_command_output_flag, ArOutputFlag),
	globals__io_lookup_string_option(ranlib_command, RanLib),
	{ join_string_list(ObjectList, "", "", " ", Objects) },
	{ MakeLibCmd = string__append_list([
		ArCmd, " ", ArFlags, " ", ArOutputFlag, " ",
		LibFileName, " ", Objects,  
		" && ", RanLib, " ", LibFileName]) },
	invoke_system_command(ErrorStream, verbose_commands,
		MakeLibCmd, MakeLibCmdOK).

%-----------------------------------------------------------------------------%

:- pred standard_library_directory_option(string, io__state, io__state).
:- mode standard_library_directory_option(out, di, uo) is det.

standard_library_directory_option(Opt) -->
	globals__io_lookup_maybe_string_option(
		mercury_standard_library_directory, MaybeStdLibDir),
	{
		MaybeStdLibDir = yes(StdLibDir),
		Opt = "--mercury-standard-library-directory "
				++ StdLibDir ++ " "
	;
		MaybeStdLibDir = no,
		Opt = "--no-mercury-standard-library-directory "
	}.

%-----------------------------------------------------------------------------%

	% join_string_list(Strings, Prefix, Suffix, Serarator, Result)
	%
	% Appends the strings in the list `Strings' together into the
	% string Result. Each string is prefixed by Prefix, suffixed by
	% Suffix and separated by Separator.

:- pred join_string_list(list(string), string, string, string, string).
:- mode join_string_list(in, in, in, in, out) is det.

join_string_list([], _Prefix, _Suffix, _Separator, "").
join_string_list([String | Strings], Prefix, Suffix, Separator, Result) :-
	( Strings = [] ->
		string__append_list([Prefix, String, Suffix], Result)
	;
		join_string_list(Strings, Prefix, Suffix, Separator, Result0),
		string__append_list([Prefix, String, Suffix, Separator,
			Result0], Result)
	).

	% join_module_list(ModuleNames, Extension, Terminator, Result)
	%
	% The list of strings `Result' is computed from the list of strings
	% `ModuleNames', by removing any directory paths, and
	% converting the strings to file names and then back,
	% adding the specified Extension.  (This conversion ensures
	% that we follow the usual file naming conventions.)
	% Each file name is separated by a space from the next one, 
	% and the result is followed by the list of strings `Terminator'.

:- pred join_module_list(list(string), string, list(string), list(string),
			io__state, io__state).
:- mode join_module_list(in, in, in, out, di, uo) is det.

join_module_list([], _Extension, Terminator, Terminator) --> [].
join_module_list([Module | Modules], Extension, Terminator,
			[FileName, " " | Rest]) -->
	{ dir__basename(Module, BaseName) },
	{ file_name_to_module_name(BaseName, ModuleName) },
	module_name_to_file_name(ModuleName, Extension, no, FileName),
	join_module_list(Modules, Extension, Terminator, Rest).

%-----------------------------------------------------------------------------%

write_num_split_c_files(ModuleName, NumChunks, Succeeded) -->
	module_name_to_file_name(ModuleName, ".num_split", yes,
		NumChunksFileName),
       io__open_output(NumChunksFileName, Res),
	( { Res = ok(OutputStream) } ->
		io__write_int(OutputStream, NumChunks),
		io__nl(OutputStream),
		io__close_output(OutputStream),
		{ Succeeded = yes }
	;
		{ Succeeded = no },
		io__progname_base("mercury_compile", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(NumChunksFileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

read_num_split_c_files(ModuleName, MaybeNumChunks) -->
	module_name_to_file_name(ModuleName, ".num_split", no,
		NumChunksFileName),
	io__open_input(NumChunksFileName, Res),
	(
		{ Res = ok(FileStream) },
		io__read_word(FileStream, MaybeNumChunksString),
		io__close_input(FileStream),
		(
			{ MaybeNumChunksString = ok(NumChunksString) },
			(
				{ string__to_int(
					string__from_char_list(NumChunksString),
					NumChunks) }
			->
				{ MaybeNumChunks = ok(NumChunks) }
			;
				{ MaybeNumChunks = error(
					"Software error: error in `"
					++ NumChunksFileName
					++ "': expected single int.\n") }
			)
		;
			{ MaybeNumChunksString = eof },
			{ MaybeNumChunks = error(
				"Software error: error in `"
				++ NumChunksFileName
				++ "': expected single int.\n") }
		;
			{ MaybeNumChunksString = error(_) },
			{ MaybeNumChunks = error(
				"Software error: error in `"
				++ NumChunksFileName
				++ "': expected single int.\n") }
		)
	;
		{ Res = error(Error) },
		{ MaybeNumChunks = error(io__error_message(Error)) }
	).

remove_split_c_output_files(ModuleName, NumChunks) -->
	remove_split_c_output_files(ModuleName, 0, NumChunks).

:- pred remove_split_c_output_files(module_name, int, int,
		io__state, io__state).
:- mode remove_split_c_output_files(in,in, in, di, uo) is det.

remove_split_c_output_files(ModuleName, ThisChunk, NumChunks) -->
	( { ThisChunk =< NumChunks } ->
		globals__io_lookup_string_option(object_file_extension, Obj),
		module_name_to_split_c_file_name(ModuleName, ThisChunk,
			".c", CFileName),
		module_name_to_split_c_file_name(ModuleName, ThisChunk,
			Obj, ObjFileName),
		io__remove_file(CFileName, _),
		io__remove_file(ObjFileName, _),
		remove_split_c_output_files(ModuleName, ThisChunk, NumChunks)
	;
		[]	
	).

%-----------------------------------------------------------------------------%
