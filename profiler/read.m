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

:- import_module int, io.

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

:- import_module list, string, char.
:- import_module std_util, require.


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
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList0) },
		{ label_demangle(CharList0, CharList) },
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
	io__read_word(WordResult),
	(
		{ WordResult = ok(CharList0) },
		{ label_demangle(CharList0, CharList) },
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
			io__write_string("\nInteger = "),
			io__write_string(CountStr),
			{ error("\nread_int: Not an integer\n") }
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

:- pred label_demangle(list(char), list(char)).
:- mode label_demangle(in, out) is det.

label_demangle(CharList0, CharList) :-
	(
		CharList0 = ['m','e','r','c','u','r','y','_','_' | CharList1]
	->
		demangle_arity_and_mode(CharList1, CharList)
	;
		CharList = CharList0
	).


:- pred demangle_arity_and_mode(list(char), list(char)).
:- mode demangle_arity_and_mode(in, out) is det.

demangle_arity_and_mode(CharList0, CharList) :-
	list__reverse(CharList0, CharList1),
	CharList2 = [')' | CharList1],
	read__replace(CharList2, '_', '(', CharList3),
	read__replace(CharList3, '_', '/', CharList4),
	list__reverse(CharList4, CharList).


:- pred read__replace(list(char), char, char, list(char)).
:- mode read__replace(in, in, in, out) is det.

read__replace(L0, D, R, L) :-
	(
		list__replace_first(L0, D, R, L1)
	->
		L = L1
	;
		error("demangle_label: ill formed label\n")
	).
