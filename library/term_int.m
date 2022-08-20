%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_in.m.
% Main author: fjh.
% Stability: medium.
%
% This file provides ways to test whether terms represent integers,
% and ways to construct terms representing integers.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_int.
:- interface.

:- import_module term.

%---------------------------------------------------------------------------%

:- pred decimal_term_to_int(term(T)::in, int::out) is semidet.
% NOTE_TO_IMPLEMENTORS This predicate was added for use by the compiler
% NOTE_TO_IMPLEMENTORS to process various numbers such as arities in pragmas.
% NOTE_TO_IMPLEMENTORS All of these should be decimal, and we have to check
% NOTE_TO_IMPLEMENTORS that they are. They are also signed, because their
% NOTE_TO_IMPLEMENTORS specifications were set before unsigned ints were
% NOTE_TO_IMPLEMENTORS added to the language. This is why we ourselves
% NOTE_TO_IMPLEMENTORS don't need versions of this predicate for sized
% NOTE_TO_IMPLEMENTORS and/or unsigned ints.
% NOTE_TO_IMPLEMENTORS
% NOTE_TO_IMPLEMENTORS Of course, we could still provide them for others.

:- pred term_to_int(term(T)::in, int::out) is semidet.
:- pred term_to_int8(term(T)::in, int8::out) is semidet.
:- pred term_to_int16(term(T)::in, int16::out) is semidet.
:- pred term_to_int32(term(T)::in, int32::out) is semidet.
:- pred term_to_int64(term(T)::in, int64::out) is semidet.

:- pred term_to_uint(term(T)::in, uint::out) is semidet.
:- pred term_to_uint8(term(T)::in, uint8::out) is semidet.
:- pred term_to_uint16(term(T)::in, uint16::out) is semidet.
:- pred term_to_uint32(term(T)::in, uint32::out) is semidet.
:- pred term_to_uint64(term(T)::in, uint64::out) is semidet.

:- func int_to_decimal_term(int, context) = term(T).
:- func int8_to_decimal_term(int8, context) = term(T).
:- func int16_to_decimal_term(int16, context) = term(T).
:- func int32_to_decimal_term(int32, context) = term(T).
:- func int64_to_decimal_term(int64, context) = term(T).

:- func uint_to_decimal_term(uint, context) = term(T).
:- func uint8_to_decimal_term(uint8, context) = term(T).
:- func uint16_to_decimal_term(uint16, context) = term(T).
:- func uint32_to_decimal_term(uint32, context) = term(T).
:- func uint64_to_decimal_term(uint64, context) = term(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module list.

%---------------------------------------------------------------------------%

decimal_term_to_int(Term, Int) :-
    Term = functor(Const, [], _Context),
    Const = integer(base_10, Integer, signed, size_word),
    integer.to_int(Integer, Int).

%---------------------%

term_to_int(Term, Int) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_word),
    integer.to_int(Integer, Int).

term_to_int8(Term, Int8) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_8_bit),
    integer.to_int8(Integer, Int8).

term_to_int16(Term, Int16) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_16_bit),
    integer.to_int16(Integer, Int16).

term_to_int32(Term, Int32) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_32_bit),
    integer.to_int32(Integer, Int32).

term_to_int64(Term, Int64) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_64_bit),
    integer.to_int64(Integer, Int64).

%---------------------%

term_to_uint(Term, UInt) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_word),
    integer.to_uint(Integer, UInt).

term_to_uint8(Term, UInt8) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_8_bit),
    integer.to_uint8(Integer, UInt8).

term_to_uint16(Term, UInt16) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_16_bit),
    integer.to_uint16(Integer, UInt16).

term_to_uint32(Term, UInt32) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_32_bit),
    integer.to_uint32(Integer, UInt32).

term_to_uint64(Term, UInt64) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_64_bit),
    integer.to_uint64(Integer, UInt64).

%---------------------%

int_to_decimal_term(Int, Context) = Term :-
    Const = integer(base_10, integer(Int), signed, size_word),
    Term = functor(Const, [], Context).

int8_to_decimal_term(Int8, Context) = Term :-
    Const = integer(base_10, integer.from_int8(Int8), signed,
        size_8_bit),
    Term = functor(Const, [], Context).

int16_to_decimal_term(Int16, Context) = Term :-
    Const = integer(base_10, integer.from_int16(Int16), signed,
        size_16_bit),
    Term = functor(Const, [], Context).

int32_to_decimal_term(Int32, Context) = Term :-
    Const = integer(base_10, integer.from_int32(Int32), signed,
        size_32_bit),
    Term = functor(Const, [], Context).

int64_to_decimal_term(Int64, Context) = Term :-
    Const = integer(base_10, integer.from_int64(Int64), signed,
        size_64_bit),
    Term = functor(Const, [], Context).

%---------------------%

uint_to_decimal_term(UInt, Context) = Term :-
    Const = integer(base_10, integer.from_uint(UInt), unsigned, size_word),
    Term = functor(Const, [], Context).

uint8_to_decimal_term(UInt8, Context) = Term :-
    Const = integer(base_10, integer.from_uint8(UInt8), unsigned,
        size_8_bit),
    Term = functor(Const, [], Context).

uint16_to_decimal_term(UInt16, Context) = Term :-
    Const = integer(base_10, integer.from_uint16(UInt16), unsigned,
        size_16_bit),
    Term = functor(Const, [], Context).

uint32_to_decimal_term(UInt32, Context) = Term :-
    Const = integer(base_10, integer.from_uint32(UInt32), unsigned,
        size_32_bit),
    Term = functor(Const, [], Context).

uint64_to_decimal_term(UInt64, Context) = Term :-
    Const = integer(base_10, integer.from_uint64(UInt64), unsigned,
        size_64_bit),
    Term = functor(Const, [], Context).

%---------------------------------------------------------------------------%
:- end_module term_int.
%---------------------------------------------------------------------------%
