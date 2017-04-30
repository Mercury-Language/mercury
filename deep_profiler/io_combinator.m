%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2001, 2005-2006, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: io_combinator.m.
% Authors: conway, zs.
% Stability: low
%
% This file implements I/O "combinators".
%
% Each of these predicates takes as its inputs N I/O actions, and a predicate
% that combines the results of these actions into a single result.
%
% The io_combinator.sequence_N forms combine actions that return io.result(T)
% and return io.result(T) themselves. They return ok if all the actions
% succeeded. They return error if an action resulted in an error after the
% previous ones succeeded, and they return eof if one of the actions found eof
% after the previous ones succeeded. In either case, the inputs of those
% earlier successful actions have already been consumed, but their results
% are not returned.
%
% The io_combinator.res_sequence_N forms are similar, except they combine
% actions that return io.res(T) and return io.res(T) themselves. This means
% that neither the individual actions nor the combinators can ever return a
% separate eof indication. These forms are for use in situations in which
% an action finding eof is an error.
%
% The io_combinator.maybe_error_sequence_N forms are identical to the
% io_combinator.res_sequence_N forms, except they return strings, not
% io.errors, when they find an error.
%
%---------------------------------------------------------------------------%

:- module io_combinator.

:- interface.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred io_combinator.sequence_2(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(T1, T2, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_3(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(T1, T2, T3, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_4(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(T1, T2, T3, T4, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_5(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(T1, T2, T3, T4, T5, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_6(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_7(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(io.result(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_8(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(io.result(T7), io, io),
    pred(io.result(T8), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_8(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_9(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(io.result(T7), io, io),
    pred(io.result(T8), io, io),
    pred(io.result(T9), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_9(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_10(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(io.result(T7), io, io),
    pred(io.result(T8), io, io),
    pred(io.result(T9), io, io),
    pred(io.result(T10), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_10(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.sequence_11(
    pred(io.result(T1), io, io),
    pred(io.result(T2), io, io),
    pred(io.result(T3), io, io),
    pred(io.result(T4), io, io),
    pred(io.result(T5), io, io),
    pred(io.result(T6), io, io),
    pred(io.result(T7), io, io),
    pred(io.result(T8), io, io),
    pred(io.result(T9), io, io),
    pred(io.result(T10), io, io),
    pred(io.result(T11), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, io.result(T)),
    io.result(T), io, io).
:- mode io_combinator.sequence_11(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

%---------------------------------------------------------------------------%

:- pred io_combinator.res_sequence_2(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(T1, T2, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_3(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(T1, T2, T3, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_4(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(T1, T2, T3, T4, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_5(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(T1, T2, T3, T4, T5, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_6(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_7(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(io.res(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_8(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(io.res(T7), io, io),
    pred(io.res(T8), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_8(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_9(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(io.res(T7), io, io),
    pred(io.res(T8), io, io),
    pred(io.res(T9), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_9(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_10(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(io.res(T7), io, io),
    pred(io.res(T8), io, io),
    pred(io.res(T9), io, io),
    pred(io.res(T10), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_10(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.res_sequence_11(
    pred(io.res(T1), io, io),
    pred(io.res(T2), io, io),
    pred(io.res(T3), io, io),
    pred(io.res(T4), io, io),
    pred(io.res(T5), io, io),
    pred(io.res(T6), io, io),
    pred(io.res(T7), io, io),
    pred(io.res(T8), io, io),
    pred(io.res(T9), io, io),
    pred(io.res(T10), io, io),
    pred(io.res(T11), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, io.res(T)),
    io.res(T), io, io).
:- mode io_combinator.res_sequence_11(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

%---------------------------------------------------------------------------%

:- pred io_combinator.maybe_error_sequence_2(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(T1, T2, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_3(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(T1, T2, T3, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_4(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(T1, T2, T3, T4, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_5(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(T1, T2, T3, T4, T5, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_6(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_7(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_8(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_8(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_9(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(maybe_error(T9), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_9(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_10(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(maybe_error(T9), io, io),
    pred(maybe_error(T10), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_10(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_11(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(maybe_error(T9), io, io),
    pred(maybe_error(T10), io, io),
    pred(maybe_error(T11), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_11(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_12(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(maybe_error(T9), io, io),
    pred(maybe_error(T10), io, io),
    pred(maybe_error(T11), io, io),
    pred(maybe_error(T12), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_12(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator.maybe_error_sequence_13(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(maybe_error(T8), io, io),
    pred(maybe_error(T9), io, io),
    pred(maybe_error(T10), io, io),
    pred(maybe_error(T11), io, io),
    pred(maybe_error(T12), io, io),
    pred(maybe_error(T13), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator.maybe_error_sequence_13(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

io_combinator.sequence_2(P1, P2, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(Combine, T1, T2, Res)
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_3(P1, P2, P3, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(Combine, T1, T2, T3, Res)
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_4(P1, P2, P3, P4, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(Combine, T1, T2, T3, T4, Res)
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_5(P1, P2, P3, P4, P5, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(Combine, T1, T2, T3, T4, T5, Res)
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(Combine, T1, T2, T3, T4, T5, T6, Res)
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_7(P1, P2, P3, P4, P5, P6, P7, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(Combine, T1, T2, T3, T4, T5, T6, T7, Res)
                            ;
                                Res7 = eof,
                                Res = eof
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_8(P1, P2, P3, P4, P5, P6, P7, P8, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res)
                                ;
                                    Res8 = eof,
                                    Res = eof
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = eof,
                                Res = eof
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9, Combine, Res,
        !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res)
                                    ;
                                        Res9 = eof,
                                        Res = eof
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = eof,
                                    Res = eof
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = eof,
                                Res = eof
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res)
                                        ;
                                            Res10 = eof,
                                            Res = eof
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = eof,
                                        Res = eof
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = eof,
                                    Res = eof
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = eof,
                                Res = eof
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.sequence_11(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(P11, Res11, !IO),
                                            (
                                                Res11 = ok(T11),
                                                call(Combine, T1, T2, T3, T4,
                                                    T5, T6, T7, T8, T9, T10,
                                                    T11, Res)
                                            ;
                                                Res11 = eof,
                                                Res = eof
                                            ;
                                                Res11 = error(Err),
                                                Res = error(Err)
                                            )
                                        ;
                                            Res10 = eof,
                                            Res = eof
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = eof,
                                        Res = eof
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = eof,
                                    Res = eof
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = eof,
                                Res = eof
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = eof,
                            Res = eof
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = eof,
                        Res = eof
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = eof,
                    Res = eof
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = eof,
                Res = eof
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = eof,
            Res = eof
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = eof,
        Res = eof
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

%---------------------------------------------------------------------------%

io_combinator.res_sequence_2(P1, P2, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(Combine, T1, T2, Res)
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_3(P1, P2, P3, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(Combine, T1, T2, T3, Res)
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_4(P1, P2, P3, P4, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(Combine, T1, T2, T3, T4, Res)
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_5(P1, P2, P3, P4, P5, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(Combine, T1, T2, T3, T4, T5, Res)
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(Combine, T1, T2, T3, T4, T5, T6, Res)
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_7(P1, P2, P3, P4, P5, P6, P7, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(Combine, T1, T2, T3, T4, T5, T6, T7, Res)
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_8(P1, P2, P3, P4, P5, P6, P7, P8, Combine, Res,
        !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res)
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res)
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res)
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.res_sequence_11(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(P11, Res11, !IO),
                                            (
                                                Res11 = ok(T11),
                                                call(Combine, T1, T2, T3, T4,
                                                    T5, T6, T7, T8, T9, T10,
                                                    T11, Res)
                                            ;
                                                Res11 = error(Err),
                                                Res = error(Err)
                                            )
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

%---------------------------------------------------------------------------%

io_combinator.maybe_error_sequence_2(P1, P2, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(Combine, T1, T2, Res)
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_3(P1, P2, P3, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(Combine, T1, T2, T3, Res)
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_4(P1, P2, P3, P4, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(Combine, T1, T2, T3, T4, Res)
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_5(P1, P2, P3, P4, P5, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(Combine, T1, T2, T3, T4, T5, Res)
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res, !IO)
:-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(Combine, T1, T2, T3, T4, T5, T6, Res)
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_7(P1, P2, P3, P4, P5, P6, P7,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(Combine, T1, T2, T3, T4, T5, T6, T7, Res)
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_8(P1, P2, P3, P4, P5, P6, P7, P8,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res)
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res)
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res)
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_11(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        P11, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(P11, Res11, !IO),
                                            (
                                                Res11 = ok(T11),
                                                call(Combine, T1, T2, T3, T4,
                                                    T5, T6, T7, T8, T9, T10,
                                                    T11, Res)
                                            ;
                                                Res11 = error(Err),
                                                Res = error(Err)
                                            )
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_12(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        P11, P12, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(P11, Res11, !IO),
                                            (
                                                Res11 = ok(T11),
                                                call(P12, Res12, !IO),
                                                (
                                                    Res12 = ok(T12),
                                                    call(Combine, T1, T2, T3,
                                                        T4, T5, T6, T7, T8, T9,
                                                        T10, T11, T12, Res)
                                                ;
                                                    Res12 = error(Err),
                                                    Res = error(Err)
                                                )
                                            ;
                                                Res11 = error(Err),
                                                Res = error(Err)
                                            )
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

io_combinator.maybe_error_sequence_13(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        P11, P12, P13, Combine, Res, !IO) :-
    call(P1, Res1, !IO),
    (
        Res1 = ok(T1),
        call(P2, Res2, !IO),
        (
            Res2 = ok(T2),
            call(P3, Res3, !IO),
            (
                Res3 = ok(T3),
                call(P4, Res4, !IO),
                (
                    Res4 = ok(T4),
                    call(P5, Res5, !IO),
                    (
                        Res5 = ok(T5),
                        call(P6, Res6, !IO),
                        (
                            Res6 = ok(T6),
                            call(P7, Res7, !IO),
                            (
                                Res7 = ok(T7),
                                call(P8, Res8, !IO),
                                (
                                    Res8 = ok(T8),
                                    call(P9, Res9, !IO),
                                    (
                                        Res9 = ok(T9),
                                        call(P10, Res10, !IO),
                                        (
                                            Res10 = ok(T10),
                                            call(P11, Res11, !IO),
                                            (
                                                Res11 = ok(T11),
                                                call(P12, Res12, !IO),
                                                (
                                                    Res12 = ok(T12),
                                                    call(P13, Res13, !IO),
                                                    (
                                                        Res13 = ok(T13),
                                                        call(Combine, T1, T2,
                                                            T3, T4, T5, T6, T7,
                                                            T8, T9, T10, T11,
                                                            T12, T13, Res)
                                                    ;
                                                        Res13 = error(Err),
                                                        Res = error(Err)
                                                    )
                                                ;
                                                    Res12 = error(Err),
                                                    Res = error(Err)
                                                )
                                            ;
                                                Res11 = error(Err),
                                                Res = error(Err)
                                            )
                                        ;
                                            Res10 = error(Err),
                                            Res = error(Err)
                                        )
                                    ;
                                        Res9 = error(Err),
                                        Res = error(Err)
                                    )
                                ;
                                    Res8 = error(Err),
                                    Res = error(Err)
                                )
                            ;
                                Res7 = error(Err),
                                Res = error(Err)
                            )
                        ;
                            Res6 = error(Err),
                            Res = error(Err)
                        )
                    ;
                        Res5 = error(Err),
                        Res = error(Err)
                    )
                ;
                    Res4 = error(Err),
                    Res = error(Err)
                )
            ;
                Res3 = error(Err),
                Res = error(Err)
            )
        ;
            Res2 = error(Err),
            Res = error(Err)
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

%---------------------------------------------------------------------------%
:- end_module io_combinator.
%---------------------------------------------------------------------------%
