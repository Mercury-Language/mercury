%------------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: text.m
% main author: conway@cs.mu.oz.au
%
% This module provides a byte-array module intended for storing text and
% binary byte-oriented data.
%
%------------------------------------------------------------------------------%
:- module text.

:- interface.

:- type text.

:- type byte	==	int.	% Using low 8 bits only.

	% text(String) = Text
	% takes a Mercury string `String' and returns the same data in the
	% text representation `Text'.
:- func text(string) = text.
:- mode (text(in) = uo) is det.

	% create(Size, Init, Text)
	% creates a text object of `Size' bytes each initialized to `Init',
	% and binds it to `Text'.
:- pred create(int, byte, text).
:- mode create(in, in, uo) is det.

	% index(Text, Index, Value)
	% binds `Value' to the `Index'th element of `Text'. Indices are
	% 0 offset (Like C arrays). index/3 aborts if `Index' is out of
	% range.
:- pred index(text, int, byte).
:- mode index(ui, in, out) is det.
:- mode index(in, in, out) is det.

	% update(Index, Value, OldText, NewText)
	% destroys `OldText' and binds `NewText' to a text object that
	% is the same as `OldText' except that the byte at `Index' is
	% replaced with `Value'. update/4 aborts if `Index' is out of
	% range.
:- pred update(int, byte, text, text).
:- mode update(in, in, di, uo) is det.

	% length(Text, Length)
	% binds `Length' to the number of bytes in the text object `Text'.
:- pred length(text, int).
:- mode length(ui, out) is det.
:- mode length(in, out) is det.

	% unique(SharedText) = UniqueText
	% performs an unsafe uniqueness cast on `SharedText' to make
	% `UniqueText'. This is useful if you're storing text objects
	% inside other data structures, but is of course risky since
	% it is unchecked by the compiler. USE AT OWN RISK!
:- func unique(text) = text.
:- mode unique(in) = uo is det.

	% split(WholeText, Index, FirstPart, SecondPart)
	% splits `WholeText' into two parts: `FirstPart' and `SecondPart'
	% on the boundary `Index'.
:- pred split(text, int, text, text).
:- mode split(di, in, uo, uo) is det.

	% combine(FirstPart, SecondPart, WholeText)
	% combines two text objects that were created by splitting `WholeText'.
	% combine/3 aborts if `FirstPart' and `SecondPart' did not come from
	% the same text object (in an operational sense, not a declarative
	% sense - this is a bug, or at least a missing feature).
:- pred combine(text, text, text).
:- mode combine(di, di, uo) is det.

%------------------------------------------------------------------------------%
:- implementation.

:- import_module char, int, list, std_util, string.

:- type text
	--->	text(c_pointer).

:- pragma c_header_code("
	#include ""text_header.h""

	/*
	** ME_words(amt) returns the number of words necessary to
	** to store `amt' bytes.
	*/
	#define ME_words(x)	(((x) + sizeof(Word) - 1) / sizeof(Word))
").

%------------------------------------------------------------------------------%

text(Str) = Text :-
	length(Str, Len),
	create(Len, 0, Text0),
	string__to_char_list(Str, Chars),
	text_2(Chars, 0, Text0, Text).

:- pred text_2(list(char), int, text, text).
:- mode text_2(in, in, di, uo) is det.

text_2([], _, Text, Text).
text_2([C|Cs], N, Text0, Text) :-
	char__to_int(C, I),
	update(N, I, Text0, Text1),
	text_2(Cs, N+1, Text1, Text).

%------------------------------------------------------------------------------%

:- pragma c_code(create(Len::in, Val::in, Txt::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;
	Word	tmp;
	int	i;

	incr_hp(Txt, ME_words(sizeof(ME_Text)));
	incr_hp_atomic(tmp, ME_words(Len));
	txtptr = (ME_Text *) Txt;
	txtptr->len = Len;
	txtptr->data = (char *) tmp;
	for (i=0; i < Len; i++)
		txtptr->data[i] = Val;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(index(Txt::ui, Ind::in, Val::out),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Txt;
	if (Ind < 0 || Ind >= txtptr->len) {
		MR_fatal_error(""text:index : index out of range"");
	}

	Val = txtptr->data[Ind];

}").

:- pragma c_code(index(Txt::in, Ind::in, Val::out),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Txt;
	if (Ind < 0 || Ind >= txtptr->len) {
		MR_fatal_error(""text:index : index out of range"");
	}

	Val = txtptr->data[Ind];

}").

%------------------------------------------------------------------------------%

:- pragma c_code(update(Ind::in, Val::in, Txt0::di, Txt::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Txt0;
	if (Ind < 0 || Ind >= txtptr->len) {
		MR_fatal_error(""text:index : index out of range"");
	}
	
	txtptr->data[Ind] = Val;

	Txt = Txt0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(length(Txt::ui, Len::out),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Txt;
	Len = txtptr->len;
}").

:- pragma c_code(length(Txt::in, Len::out),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Txt;
	Len = txtptr->len;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(unique(A::in) = (B::uo),
		[will_not_call_mercury, thread_safe], "{
	B = A;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(split(Text0::di, Where::in, Text1::uo, Text2::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr1, *txtptr2;

	txtptr1 = (ME_Text *) Text0;
	if (Where < 0 || Where >= txtptr1->len) {
		MR_fatal_error(""text:split : index out of range"");
	}

	Text1 = Text0;

	incr_hp(Text2, ME_words(sizeof(ME_Text)));
	txtptr2 = (ME_Text *) Text2;
	txtptr2->len = txtptr1->len - Where;
	txtptr2->data = txtptr1->data + Where;

	txtptr1->len = Where;

}").

%------------------------------------------------------------------------------%

:- pragma c_code(combine(Text0::di, Text1::di, Text::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr1, *txtptr2;

	txtptr1 = (ME_Text *) Text0;
	txtptr2 = (ME_Text *) Text1;

	if (txtptr1->data + txtptr1->len != txtptr2->data) {
		MR_fatal_error(""text:combine : not adjacent text"");
	}

	txtptr1->len = txtptr1->len + txtptr2->len;

	Text = Text0;
}").

