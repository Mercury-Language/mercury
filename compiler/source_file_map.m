%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: source_file_map.m
% Author: stayl
%
% Maintain a mapping from module name to source file name.
%-----------------------------------------------------------------------------%
:- module parse_tree__source_file_map.

:- interface.

:- import_module parse_tree__prog_data, parse_tree__prog_io.
:- import_module io, list.

	% lookup_module_source_file(ModuleName, FileName, FileNameIsMapped).
	%
	% FileNameIsMapped is `yes' if ModuleName is in
	% the Mercury.modules file.
:- pred lookup_module_source_file(module_name::in, file_name::out, 
		io__state::di, io__state::uo) is det.

	% Return the default fully-qualified source file name.
:- func default_source_file(module_name) = file_name.

	% Given a list of file names, produce the Mercury.modules file.
:- pred write_source_file_map(list(string)::in,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_out, parse_tree__prog_util.
:- import_module parse_tree__modules.
:- import_module libs__globals, libs__options.
:- import_module bool, char, dir, map, std_util, string.

lookup_module_source_file(ModuleName, FileName) -->
	get_source_file_map(SourceFileMap),
	{ map__search(SourceFileMap, ModuleName, FileName0) ->
		FileName = FileName0
	;
		FileName = default_source_file(ModuleName)
	}.

default_source_file(ModuleName) = BaseFileName ++ ".m" :-
	prog_out__sym_name_to_string(ModuleName, ".", BaseFileName).

	% Read the Mercury.modules file (if it exists) to find
	% the mapping from module name to file name.
:- pred get_source_file_map(source_file_map::out,
		io__state::di, io__state::uo) is det.

get_source_file_map(SourceFileMap) -->
	globals__io_get_globals(Globals0),
	{ globals__get_source_file_map(Globals0, MaybeSourceFileMap) },
	( { MaybeSourceFileMap = yes(SourceFileMap0) } ->
		{ SourceFileMap = SourceFileMap0 }
	;
		globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
		io__open_input(modules_file_name(UseSubdirs), OpenRes),
		(
			{ OpenRes = ok(Stream) },
			io__set_input_stream(Stream, OldStream),
			read_source_file_map([], map__init, SourceFileMap),
			io__set_input_stream(OldStream, _),
			io__close_input(Stream)
		;
			{ OpenRes = error(_) },
			% If the file doesn't exist, then the mapping is empty.
			{ SourceFileMap = map__init }
		),
		globals__io_get_globals(Globals1),
		{ globals__set_source_file_map(Globals1,
			MaybeSourceFileMap, Globals2) },
		{ unsafe_promise_unique(Globals2, Globals) },
		globals__io_set_globals(Globals)
	).

:- pred read_source_file_map(list(char)::in, source_file_map::in,
		source_file_map::out, io__state::di, io__state::uo) is det.

read_source_file_map(ModuleChars, Map0, Map) -->
	read_until_char('\t', [], ModuleCharsResult),
	(
		{ ModuleCharsResult = ok(RevModuleChars) },
		{ string__from_rev_char_list(RevModuleChars, ModuleStr) },
		{ string_to_sym_name(ModuleStr, ":", ModuleName) },
		read_until_char('\n', [], FileNameCharsResult),
		(
			{ FileNameCharsResult = ok(FileNameChars) },
			{ string__from_rev_char_list(FileNameChars,
				FileName) },
			{ map__set(Map0, ModuleName, FileName, Map1) },
			read_source_file_map(ModuleChars, Map1, Map)
		;
			{ FileNameCharsResult = eof },
			{ Map = Map0 },
			io__set_exit_status(1),
			io__write_string(
	"mercury_compile: unexpected end of file in Mercury.modules file: ")
		;
			{ FileNameCharsResult = error(Error) },
			{ Map = Map0 },
			io__set_exit_status(1),
			io__write_string(
	"mercury_compile: error in Mercury.modules file: "),
			io__write_string(io__error_message(Error))
		)
	;
		{ ModuleCharsResult = eof },
		{ Map = Map0 }
	;
		{ ModuleCharsResult = error(Error) },
		{ Map = Map0 },
		io__set_exit_status(1),
		io__write_string(
			"mercury_compile: error in Mercury.modules file: "),
		io__write_string(io__error_message(Error))
	).

:- pred read_until_char(char::in, list(char)::in, io__result(list(char))::out,
		io__state::di, io__state::uo) is det.

read_until_char(EndChar, Chars0, Result) -->
	io__read_char(CharRes),
	(
		{ CharRes = ok(Char) },
		( { Char = EndChar } ->
			{ Result = ok(Chars0) }
		;
			read_until_char(EndChar, [Char | Chars0], Result)	
		)
	;
		{ CharRes = eof },
		{ Result = ( Chars0 = [] -> eof ; ok(Chars0) ) }
	;
		{ CharRes = error(Error) },
		{ Result = error(Error) }
	).

write_source_file_map(FileNames) -->
	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	( { UseSubdirs = yes } ->
		make_directory("Mercury")
	;
		[]
	),
	{ ModulesFileName = modules_file_name(UseSubdirs) },
	io__open_output(ModulesFileName, OpenRes),
	(
		{ OpenRes = ok(Stream) },
		list__foldl(write_source_file_map_2(Stream), FileNames),
		io__close_output(Stream)
	;
		{ OpenRes = error(Error) },
		io__set_exit_status(1),
		io__write_string("mercury_compile: error opening `"),
		io__write_string(ModulesFileName),
		io__write_string("' for output: "),
		io__write_string(io__error_message(Error))
	).

:- pred write_source_file_map_2(io__output_stream::in, file_name::in,
		io__state::di, io__state::uo) is det.

write_source_file_map_2(MapStream, FileName) -->
	find_module_name(FileName, MaybeModuleName),
	(
		{ MaybeModuleName = yes(ModuleName) },
		{ string__remove_suffix(FileName, ".m", PartialFileName0) ->
			PartialFileName = PartialFileName0
		;
			PartialFileName = FileName
		},
		{ file_name_to_module_name(dir__basename(PartialFileName),
			DefaultModuleName) },
		(
			% Only include a module in the mapping if the
			% name doesn't match the default.
			{ dir__dirname(PartialFileName) =
				dir__this_directory `with_type` string },
			{ ModuleName = DefaultModuleName }
		->
			[]
		;
			io__set_output_stream(MapStream, OldStream),
			prog_out__write_sym_name(ModuleName),
			io__write_string("\t"),
			io__write_string(FileName),
			io__nl,
			io__set_output_stream(OldStream, _)
		)
	;
		{ MaybeModuleName = no }
	).

:- func modules_file_name(bool) = string.

modules_file_name(yes) = "Mercury/Mercury.modules".
modules_file_name(no) = "Mercury.modules".
