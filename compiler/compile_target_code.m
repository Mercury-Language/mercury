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
% XXX Code will be moved here from mercury_compile.m after the `mmc --make'
% change has been reviewed.
%-----------------------------------------------------------------------------%
:- module compile_target_code.

:- interface.

:- import_module prog_data.
:- import_module bool, io, std_util.

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

:- import_module modules, globals, options.
:- import_module int, string.

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

