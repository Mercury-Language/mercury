%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2001, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io.m.
% Authors: conway, zs.
% Stability: low
%
% This file implements I/O "combinators".
%
% Each of these predicates takes as its inputs N I/O actions, and a predicate
% that combines the results of these actions into a single result.
%
% The io_combinator__sequence_N forms combine actions that return io__result(T)
% and return io__result(T) themselves. They return ok if all the actions
% succeeded. They return error if an action resulted in an error after the
% previous ones succeeded, and they return eof if one of the actions found eof
% after the previous ones succeeded. In either case, the inputs of those
% earlier successful actions have already been consumed, but their results
% are not returned.
%
% The io_combinator__res_sequence_N forms are similar, except they combine
% actions that return io__res(T) and return io__res(T) themselves. This means
% that neither the individual actions nor the combinators can ever return a
% separate eof indication. These forms are for use in situations in which
% an action finding eof is an error.
%
% The io_combinator__maybe_error_sequence_N forms are identical to the
% io_combinator__res_sequence_N forms, except they return strings, not
% io__errors, when they find an error.
%
%-----------------------------------------------------------------------------%

:- module io_combinator.

:- interface.

:- import_module io.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- pred io_combinator__sequence_2(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(T1, T2, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_3(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(T1, T2, T3, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_4(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(T1, T2, T3, T4, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_5(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(T1, T2, T3, T4, T5, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_6(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(io__result(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_7(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(io__result(T6), io, io),
    pred(io__result(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__sequence_8(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(io__result(T6), io, io),
    pred(io__result(T7), io, io),
    pred(io__result(T8), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_8(
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

:- pred io_combinator__sequence_9(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(io__result(T6), io, io),
    pred(io__result(T7), io, io),
    pred(io__result(T8), io, io),
    pred(io__result(T9), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_9(
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

:- pred io_combinator__sequence_10(
    pred(io__result(T1), io, io),
    pred(io__result(T2), io, io),
    pred(io__result(T3), io, io),
    pred(io__result(T4), io, io),
    pred(io__result(T5), io, io),
    pred(io__result(T6), io, io),
    pred(io__result(T7), io, io),
    pred(io__result(T8), io, io),
    pred(io__result(T9), io, io),
    pred(io__result(T10), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, io__result(T)),
    io__result(T), io, io).
:- mode io_combinator__sequence_10(
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

%-----------------------------------------------------------------------------%

:- pred io_combinator__res_sequence_2(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(T1, T2, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_3(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(T1, T2, T3, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_4(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(T1, T2, T3, T4, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_5(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(T1, T2, T3, T4, T5, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_6(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(io__res(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_7(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(io__res(T6), io, io),
    pred(io__res(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__res_sequence_8(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(io__res(T6), io, io),
    pred(io__res(T7), io, io),
    pred(io__res(T8), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_8(
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

:- pred io_combinator__res_sequence_9(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(io__res(T6), io, io),
    pred(io__res(T7), io, io),
    pred(io__res(T8), io, io),
    pred(io__res(T9), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_9(
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

:- pred io_combinator__res_sequence_10(
    pred(io__res(T1), io, io),
    pred(io__res(T2), io, io),
    pred(io__res(T3), io, io),
    pred(io__res(T4), io, io),
    pred(io__res(T5), io, io),
    pred(io__res(T6), io, io),
    pred(io__res(T7), io, io),
    pred(io__res(T8), io, io),
    pred(io__res(T9), io, io),
    pred(io__res(T10), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, io__res(T)),
    io__res(T), io, io).
:- mode io_combinator__res_sequence_10(
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

%-----------------------------------------------------------------------------%

:- pred io_combinator__maybe_error_sequence_2(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(T1, T2, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_2(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_3(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(T1, T2, T3, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_3(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_4(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(T1, T2, T3, T4, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_4(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_5(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(T1, T2, T3, T4, T5, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_5(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_6(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(T1, T2, T3, T4, T5, T6, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_6(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_7(
    pred(maybe_error(T1), io, io),
    pred(maybe_error(T2), io, io),
    pred(maybe_error(T3), io, io),
    pred(maybe_error(T4), io, io),
    pred(maybe_error(T5), io, io),
    pred(maybe_error(T6), io, io),
    pred(maybe_error(T7), io, io),
    pred(T1, T2, T3, T4, T5, T6, T7, maybe_error(T)),
    maybe_error(T), io, io).
:- mode io_combinator__maybe_error_sequence_7(
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(out, di, uo) is det,
    pred(in, in, in, in, in, in, in, out) is det,
    out, di, uo) is det.

:- pred io_combinator__maybe_error_sequence_8(
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
:- mode io_combinator__maybe_error_sequence_8(
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

:- pred io_combinator__maybe_error_sequence_9(
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
:- mode io_combinator__maybe_error_sequence_9(
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

:- pred io_combinator__maybe_error_sequence_10(
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
:- mode io_combinator__maybe_error_sequence_10(
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

io_combinator__sequence_2(P1, P2, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            { call(Combine, T1, T2, Res) }
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_3(P1, P2, P3, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                { call(Combine, T1, T2, T3, Res) }
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_4(P1, P2, P3, P4, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    { call(Combine, T1, T2, T3, T4, Res) }
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_5(P1, P2, P3, P4, P5, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        { call(Combine, T1, T2, T3, T4,
                            T5, Res) }
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            { call(Combine, T1, T2,
                                T3, T4, T5,
                                T6, Res) }
                        ;
                            { Res6 = eof },
                            { Res = eof }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_7(P1, P2, P3, P4, P5, P6, P7, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                { call(Combine, T1, T2, T3, T4, T5, T6, T7,
                                    Res) }
                            ;
                                { Res7 = eof },
                                { Res = eof }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = eof },
                            { Res = eof }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_8(P1, P2, P3, P4, P5, P6, P7, P8, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    { call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res) }
                                ;
                                    { Res8 = eof },
                                    { Res = eof }
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = eof },
                                { Res = eof }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = eof },
                            { Res = eof }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        { call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res) }
                                    ;
                                        { Res9 = eof },
                                        { Res = eof }
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = eof },
                                    { Res = eof }
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = eof },
                                { Res = eof }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = eof },
                            { Res = eof }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
		Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        call(P10, Res10),
                                        (
                                            { Res10 = ok(T10) },
                                            { call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res) }
                                        ;
                                            { Res10 = eof },
                                            { Res = eof }
                                        ;
                                            { Res10 = error(Err) },
                                            { Res = error(Err) }
                                        )
                                    ;
                                        { Res9 = eof },
                                        { Res = eof }
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = eof },
                                    { Res = eof }
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = eof },
                                { Res = eof }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = eof },
                            { Res = eof }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = eof },
                        { Res = eof }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = eof },
                    { Res = eof }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = eof },
                { Res = eof }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = eof },
            { Res = eof }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = eof },
        { Res = eof }
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

%-----------------------------------------------------------------------------%

io_combinator__res_sequence_2(P1, P2, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            { call(Combine, T1, T2, Res) }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_3(P1, P2, P3, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                { call(Combine, T1, T2, T3, Res) }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_4(P1, P2, P3, P4, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    { call(Combine, T1, T2, T3, T4, Res) }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_5(P1, P2, P3, P4, P5, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        { call(Combine, T1, T2, T3, T4,
                            T5, Res) }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            { call(Combine, T1, T2,
                                T3, T4, T5,
                                T6, Res) }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_7(P1, P2, P3, P4, P5, P6, P7, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                { call(Combine, T1, T2, T3, T4, T5, T6, T7,
                                    Res) }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_8(P1, P2, P3, P4, P5, P6, P7, P8, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    { call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res) }
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        { call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res) }
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__res_sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        call(P10, Res10),
                                        (
                                            { Res10 = ok(T10) },
                                            { call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res) }
                                        ;
                                            { Res10 = error(Err) },
                                            { Res = error(Err) }
                                        )
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

%-----------------------------------------------------------------------------%

io_combinator__maybe_error_sequence_2(P1, P2, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            { call(Combine, T1, T2, Res) }
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_3(P1, P2, P3, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                { call(Combine, T1, T2, T3, Res) }
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_4(P1, P2, P3, P4, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    { call(Combine, T1, T2, T3, T4, Res) }
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_5(P1, P2, P3, P4, P5, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        { call(Combine, T1, T2, T3, T4,
                            T5, Res) }
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_6(P1, P2, P3, P4, P5, P6, Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            { call(Combine, T1, T2,
                                T3, T4, T5,
                                T6, Res) }
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_7(P1, P2, P3, P4, P5, P6, P7,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                { call(Combine, T1, T2, T3, T4, T5, T6, T7,
                                    Res) }
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_8(P1, P2, P3, P4, P5, P6, P7, P8,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    { call(Combine, T1, T2, T3, T4, T5, T6,
                                        T7, T8, Res) }
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_9(P1, P2, P3, P4, P5, P6, P7, P8, P9,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        { call(Combine, T1, T2, T3, T4, T5,
                                            T6, T7, T8, T9, Res) }
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

io_combinator__maybe_error_sequence_10(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10,
        Combine, Res) -->
    call(P1, Res1),
    (
        { Res1 = ok(T1) },
        call(P2, Res2),
        (
            { Res2 = ok(T2) },
            call(P3, Res3),
            (
                { Res3 = ok(T3) },
                call(P4, Res4),
                (
                    { Res4 = ok(T4) },
                    call(P5, Res5),
                    (
                        { Res5 = ok(T5) },
                        call(P6, Res6),
                        (
                            { Res6 = ok(T6) },
                            call(P7, Res7),
                            (
                                { Res7 = ok(T7) },
                                call(P8, Res8),
                                (
                                    { Res8 = ok(T8) },
                                    call(P9, Res9),
                                    (
                                        { Res9 = ok(T9) },
                                        call(P10, Res10),
                                        (
                                            { Res10 = ok(T10) },
                                            { call(Combine, T1, T2, T3, T4, T5,
                                                T6, T7, T8, T9, T10, Res) }
                                        ;
                                            { Res10 = error(Err) },
                                            { Res = error(Err) }
                                        )
                                    ;
                                        { Res9 = error(Err) },
                                        { Res = error(Err) }
                                    )
                                ;
                                    { Res8 = error(Err) },
                                    { Res = error(Err) }
                                )
                            ;
                                { Res7 = error(Err) },
                                { Res = error(Err) }
                            )
                        ;
                            { Res6 = error(Err) },
                            { Res = error(Err) }
                        )
                    ;
                        { Res5 = error(Err) },
                        { Res = error(Err) }
                    )
                ;
                    { Res4 = error(Err) },
                    { Res = error(Err) }
                )
            ;
                { Res3 = error(Err) },
                { Res = error(Err) }
            )
        ;
            { Res2 = error(Err) },
            { Res = error(Err) }
        )
    ;
        { Res1 = error(Err) },
        { Res = error(Err) }
    ).

%-----------------------------------------------------------------------------%
:- end_module io_combinator.
%-----------------------------------------------------------------------------%
