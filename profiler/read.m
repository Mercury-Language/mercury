%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- import_module io.

:- pred maybe_read_label_addr(maybe(int), io__state, io__state).
:- mode	maybe_read_label_addr(out, di, uo) is det.

:- pred maybe_read_label_name(maybe(string), io__state, io__state).
:- mode	maybe_read_label_name(out, di, uo) is det.

:- pred read_label_addr(int, io__state, io__state).
:- mode	read_label_addr(out, di, uo) is det.

:- pred read_label_name(string, io__state, io__state).
:- mode	read_label_name(out, di, uo) is det.

:- pred read_int(int, io__state, io__state).
:- mode read_int(out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, string.
:- import_module std_util, require.


%-----------------------------------------------------------------------------%


maybe_read_label_addr(MaybeLabelAddr) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelAddrStr) },
		( 
			{ string__base_string_to_int(16, LabelAddrStr, LabelAddr) }
		->
			{ MaybeLabelAddr = yes(LabelAddr) }
		;
			{ error("maybe_read_label_addr: Label address not hexadecimal\n") }
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
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelName) },
		{ MaybeLabelName = yes(LabelName) }
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
			{ string__base_string_to_int(16, LabelAddrStr, LabelAddr0) }
		->
			{ LabelAddr = LabelAddr0 }
		;
			{ error("read_label_addr: Label address not hexadecimal\n") }
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
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, LabelName) }
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

read_int(Count) -->
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList) },
		{ string__from_char_list(CharList, CountStr) },
		(
			{ string__to_int(CountStr, Count0) }
		->
			{ Count = Count0 }
		;
			{ error("read_int: Not an integer\n") }
		)
	;
		{ WordResult = eof },
		{ error("read_int: EOF reached") }
	;
		{ WordResult = error(Error) },
		{ io__error_message(Error, ErrorStr) },
		{ string__append("read_int: ", ErrorStr, Str) },
		{ error(Str) }
	).

%-----------------------------------------------------------------------------%
