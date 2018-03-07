%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: arg_pack.m.
% Main author: wangp.
%
% Utilities for argument packing.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.arg_pack.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Packs an argument list such that consecutive arguments which should share
    % the same word are converted to a single argument.
    %
    % The predicate ShiftCombine takes an argument `A' and shift count `Shift'
    % (which may be zero). If it is also given the argument `yes(B)' then it
    % should produce the combined value `(A << Shift) \/ B'.
    % Otherwise, it should produce the value `(A << Shift)'.
    %
:- pred pack_args(pred(T, int, maybe(T), T, Acc1, Acc1, Acc2, Acc2)::in(
    pred(in, in, in, out, in, out, in, out) is det), list(arg_width)::in,
    list(T)::in, list(T)::out, Acc1::in, Acc1::out, Acc2::in, Acc2::out)
    is det.

    % Return the number of distinct words that would be required to hold the
    % list of arguments.
    %
:- func count_distinct_words(list(arg_width)) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

pack_args(ShiftCombine, ArgWidths, !Args, !Acc1, !Acc2) :-
    ( if list.member(partial_word_first(_), ArgWidths) then
        do_pack_args(ShiftCombine, ArgWidths, !Args, !Acc1, !Acc2)
    else
        true
    ).

:- pred do_pack_args(pred(T, int, maybe(T), T, Acc1, Acc1, Acc2, Acc2)::in(
    pred(in, in, in, out, in, out, in, out) is det), list(arg_width)::in,
    list(T)::in, list(T)::out, Acc1::in, Acc1::out, Acc2::in, Acc2::out)
    is det.

do_pack_args(_, [], [], [], !Acc1, !Acc2).
do_pack_args(ShiftCombine, [Width | Widths], [Arg0 | Args0], [Arg | Args],
        !Acc1, !Acc2) :-
    (
        Width = full_word,
        Shift = 0
    ;
        Width = double_word,
        Shift = 0
    ;
        Width = partial_word_first(_Mask),
        Shift = 0
    ;
        Width = partial_word_shifted(Shift, _Mask)
    ),
    ( if belongs_in_same_word(Width, Widths) then
        do_pack_args(ShiftCombine, Widths, Args0, Args1, !Acc1, !Acc2),
        (
            Args1 = [SecondArg | Args],
            ShiftCombine(Arg0, Shift, yes(SecondArg), Arg, !Acc1, !Acc2)
        ;
            Args1 = [],
            unexpected($module, $pred, "mismatched lists")
        )
    else
        ShiftCombine(Arg0, Shift, no, Arg, !Acc1, !Acc2),
        do_pack_args(ShiftCombine, Widths, Args0, Args, !Acc1, !Acc2)
    ).
do_pack_args(_, [], [_ | _], _, !Acc1, !Acc2) :-
    unexpected($module, $pred, "mismatched lists").
do_pack_args(_, [_ | _], [], _, !Acc1, !Acc2) :-
    unexpected($module, $pred, "mismatched lists").

:- pred belongs_in_same_word(arg_width::in, list(arg_width)::in) is semidet.

belongs_in_same_word(Prev, [Next | _]) :-
    ( Prev = partial_word_first(_)
    ; Prev = partial_word_shifted(_, _)
    ),
    Next = partial_word_shifted(_, _).

%-----------------------------------------------------------------------------%

count_distinct_words([]) = 0.
count_distinct_words([H | T]) = Words :-
    (
        H = full_word,
        Words = 1 + count_distinct_words(T)
    ;
        H = double_word,
        Words = 2 + count_distinct_words(T)
    ;
        H = partial_word_first(_),
        Words = 1 + count_distinct_words(T)
    ;
        H = partial_word_shifted(_, _),
        Words = count_distinct_words(T)
    ).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.arg_pack.
%-----------------------------------------------------------------------------%
