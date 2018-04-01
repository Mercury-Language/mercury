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

%-----------------------------------------------------------------------------%

    % Return the number of distinct words that would be required to hold the
    % list of arguments.
    %
:- func count_distinct_words(list(arg_width)) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

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
