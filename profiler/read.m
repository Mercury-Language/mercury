%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998,2000, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
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

:- import_module globals.
:- import_module int, io, std_util, string.

%-----------------------------------------------------------------------------%

:- pred maybe_read_label_addr(maybe(int)::out, io::di, io::uo) is det.

:- pred maybe_read_label_name(maybe(string)::out, io::di, io::uo) is det.

:- pred read_label_addr(int::out, io::di, io::uo) is det.

:- pred read_label_name(string::out, io::di, io::uo) is det.

:- pred read_string(string::out, io::di, io::uo) is det.

:- pred read_int(int::out, io::di, io::uo) is det.

:- pred read_float(float::out, io::di, io::uo) is det.

:- pred read_what_to_profile(what_to_profile::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module demangle.
:- import_module options.

:- import_module bool, list, char.
:- import_module require.

%-----------------------------------------------------------------------------%

maybe_read_label_addr(MaybeLabelAddr, !IO) :-
	io__read_word(WordResult, !IO),
	(
		WordResult = ok(CharList),
		string__from_char_list(CharList, LabelAddrStr),
		( 
			string__base_string_to_int(10, LabelAddrStr, 
				LabelAddr)
		->
			MaybeLabelAddr = yes(LabelAddr)
		;
			(
				string__base_string_to_int(16, LabelAddrStr, 
					LabelAddrHex)
			->
				MaybeLabelAddr = yes(LabelAddrHex)
			;
				error("maybe_read_label_addr: Label " ++ 
					"address not hexadecimal or integer\n")
			)
		)
	;
		WordResult = eof,
		MaybeLabelAddr = no
	;
		WordResult = error(Error),
		error("maybe_read_label_addr: " ++ io.error_message(Error))
	).
		
%-----------------------------------------------------------------------------%

maybe_read_label_name(MaybeLabelName, !IO) :-
	globals__io_lookup_bool_option(demangle, Demangle, !IO),
	io__read_word(WordResult, !IO),
	(
		WordResult = ok(CharList0),
		string__from_char_list(CharList0, MangledLabelName),
		(
			Demangle = yes,
			demangle(MangledLabelName, LabelName),
			MaybeLabelName = yes(LabelName)
		;
			Demangle = no,
			MaybeLabelName = yes(MangledLabelName)
		)
	;
		WordResult = eof,
		MaybeLabelName = no
	;
		WordResult = error(Error),
		error("maybe_read_label_name: " ++ io.error_message(Error))
	).
		
%-----------------------------------------------------------------------------%

read_label_addr(LabelAddr, !IO) :-
	io__read_word(WordResult, !IO),
	(
		WordResult = ok(CharList),
		string__from_char_list(CharList, LabelAddrStr),
		( 
			string__base_string_to_int(10, LabelAddrStr, 
				LabelAddr0)
		->
			LabelAddr = LabelAddr0
		;
			(
				string__base_string_to_int(16,LabelAddrStr,
					LabelAddrHex)
			->
				LabelAddr = LabelAddrHex
			;
				error("maybe_read_label_addr: " ++
					"Label address not hexadecimal or " ++ 
					"integer\n")
			)
		)
	;
		WordResult = eof,
		error("read_label_addr: EOF reached")
	;
		WordResult = error(Error),
		error("read_label_addr: " ++ io.error_message(Error))
	).
		
%-----------------------------------------------------------------------------%

read_label_name(LabelName, !IO) :-
	globals__io_lookup_bool_option(demangle, Demangle, !IO),
	io__read_word(WordResult, !IO),
	(
		WordResult = ok(CharList0),
		string__from_char_list(CharList0, MangledLabelName),
		(
			Demangle = yes,
			demangle(MangledLabelName, LabelName)
		;
			Demangle = no,
			LabelName = MangledLabelName
		)
	;
		WordResult = eof,
		error("read_label_name: EOF reached")
	;
		WordResult = error(Error),
		error("read_label_name: " ++ io.error_message(Error))

	).


%-----------------------------------------------------------------------------%

read_string(String, !IO) :-
	io__read_word(WordResult, !IO),
	(
		WordResult = ok(CharList),
		string__from_char_list(CharList, String)
	;
		WordResult = eof,
		error("read_string: EOF reached")
	;
		WordResult = error(Error),
		error("read_string: " ++ io.error_message(Error))
	).

%-----------------------------------------------------------------------------%

read_int(Int, !IO) :-
	read_string(IntStr, !IO),
	( string__to_int(IntStr, Int0) ->
		Int = Int0
	;
		Error = "\nIntger = " ++ IntStr,
		error("\nread_int: Not an integer\n" ++ Error)
	).

%-----------------------------------------------------------------------------%

read_float(Float, !IO) :-
	read_string(FloatStr, !IO),
	( string__to_float(FloatStr, Float0) ->
		Float = Float0
	;
		Error = "\nFloat = " ++ FloatStr,
		error("\nread_float: Not an float\n" ++ Error)
	).

%-----------------------------------------------------------------------------%

read_what_to_profile(WhatToProfile, !IO) :-
	read_string(Str, !IO),
	( what_to_profile(Str, WhatToProfile0) ->
		WhatToProfile = WhatToProfile0
	;
		Error = "\nWhatToProfile = " ++ Str,
		error("\nread_what_to_profile: invalid input\n" ++ Error)
	).

%-----------------------------------------------------------------------------%
:- end_module read.
%-----------------------------------------------------------------------------%
