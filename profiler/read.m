%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998,2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%
% read.m: Input predicates for use with mercury_profile
%
% Main author: petdr.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module read.

:- interface.

:- import_module int, io, std_util, string.
:- import_module globals.

:- pred maybe_read_label_addr(maybe(int), io__state, io__state).
:- mode	maybe_read_label_addr(out, di, uo) is det.

:- pred maybe_read_label_name(maybe(string), io__state, io__state).
:- mode	maybe_read_label_name(out, di, uo) is det.

:- pred read_label_addr(int, io__state, io__state).
:- mode	read_label_addr(out, di, uo) is det.

:- pred read_label_name(string, io__state, io__state).
:- mode	read_label_name(out, di, uo) is det.

:- pred read_string(string, io__state, io__state).
:- mode read_string(out, di, uo) is det.

:- pred read_int(int, io__state, io__state).
:- mode read_int(out, di, uo) is det.

:- pred read_float(float, io__state, io__state).
:- mode read_float(out, di, uo) is det.

:- pred read_what_to_profile(what_to_profile, io__state, io__state).
:- mode read_what_to_profile(out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, list, char.
:- import_module require.

:- import_module demangle, options.

%-----------------------------------------------------------------------------%


maybe_read_label_addr(MaybeLabelAddr) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelAddrStr) },
		( 
			{ string__base_string_to_int(10, LabelAddrStr, 
								LabelAddr) }
		->
			{ MaybeLabelAddr = yes(LabelAddr) }
		;
			(
				{ string__base_string_to_int(16, LabelAddrStr, 
								LabelAddrHex) }
			->
				{ MaybeLabelAddr = yes(LabelAddrHex) }
			;
				{ error("maybe_read_label_addr: Label address not hexadecimal or integer\n") }
			)
		)
	;
		{ WordResult = eof },
		{ MaybeLabelAddr = no }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("maybe_read_label_addr: ", ErrorStr, Str) },
		{ error(Str) }
	).
		

%-----------------------------------------------------------------------------%


maybe_read_label_name(MaybeLabelName) -->
	globals__io_lookup_bool_option(demangle, Demangle),
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList0) },
		{ string__from_char_list(CharList0, MangledLabelName) },
		(
			{ Demangle = yes },
			{ demangle(MangledLabelName, LabelName) },
			{ MaybeLabelName = yes(LabelName) }
		;
			{ Demangle = no },
			{ MaybeLabelName = yes(MangledLabelName) }
		)
	;
		{ WordResult = eof },
		{ MaybeLabelName = no }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("maybe_read_label_name: ", ErrorStr, Str) },
		{ error(Str) }
	).
		

%-----------------------------------------------------------------------------%

read_label_addr(LabelAddr) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelAddrStr) },
		( 
			{ string__base_string_to_int(10, LabelAddrStr, 
								LabelAddr0) }
		->
			{ LabelAddr = LabelAddr0 }
		;
			(
				{ string__base_string_to_int(16,LabelAddrStr,
								LabelAddrHex) }
			->
				{ LabelAddr = LabelAddrHex }
			;
				{ error("maybe_read_label_addr: Label address not hexadecimal or integer\n") }
			)
		)
	;
		{ WordResult = eof },
		{ error("read_label_addr: EOF reached") }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("read_label_addr: ", ErrorStr, Str) },
		{ error(Str) }
	).
		
%-----------------------------------------------------------------------------%

read_label_name(LabelName) -->
	globals__io_lookup_bool_option(demangle, Demangle),
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList0) },
		{ string__from_char_list(CharList0, MangledLabelName) },
		(
			{ Demangle = yes },
			{ demangle(MangledLabelName, LabelName) }
		;
			{ Demangle = no },
			{ LabelName = MangledLabelName }
		)
	;
		{ WordResult = eof },
		{ error("read_label_name: EOF reached") }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("read_label_name: ", ErrorStr, Str) },
		{ error(Str) }
	).


%-----------------------------------------------------------------------------%

read_string(String) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, String) }
	;
		{ WordResult = eof },
		{ error("read_string: EOF reached") }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("read_string: ", ErrorStr, Str) },
		{ error(Str) }
	).

%-----------------------------------------------------------------------------%

read_int(Int) -->
	read_string(IntStr),
	(
		{ string__to_int(IntStr, Int0) }
	->
		{ Int = Int0 }
	;
		io__write_string("\nInteger = "),
		io__write_string(IntStr),
		{ error("\nread_int: Not an integer\n") }
	).

%-----------------------------------------------------------------------------%

read_float(Float) -->
	read_string(FloatStr),
	(
		{ string__to_float(FloatStr, Float0) }
	->
		{ Float = Float0 }
	;
		io__write_string("\nFloat = "),
		io__write_string(FloatStr),
		{ error("\nread_float: Not an float\n") }
	).

%-----------------------------------------------------------------------------%

read_what_to_profile(WhatToProfile) -->
	read_string(Str),
	(
		{ what_to_profile(Str, WhatToProfile0) }
	->
		{ WhatToProfile = WhatToProfile0 }
	;
		io__write_string("\nWhatToProfile = "),
		io__write_string(Str),
		{ error("\nread_what_to_profile: invalid input\n") }
	).

%-----------------------------------------------------------------------------%
