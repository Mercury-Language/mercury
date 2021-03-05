%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2007 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: sized_pretty.m:
% Author: sthur.
%
% When printing a term during debugging this module allows the user to put
% a limit on the size of the term displayed. This limit is specified by
% setting the number of lines you want and the width of these lines.
%
% How to use sized_pretty.m :
% ---------------------------
%
% Call univ_to_string_line with the following variables:
% univ_to_string_line(Univ, LineWidth, Lines, String) where
% Univ      : is the Term (in univ type) you want to convert
% LineWidth : is the length of the lines
% Lines     : is the number of lines you want the term to be printed on
% String    : output string
%
% EXAMPLES
% --------
%
% Term Used in these examples:
%
%   Term = big(
%             big(
%                big(
%                   small,
%                   "Level 3",
%                   small
%                   ),
%               "Level 2",
%                small
%                ),
%             "Level 1",
%             big(
%                big(
%                   small,
%                   "Level 3",
%                   small
%                   ),
%                "Level 2",
%                small
%                )
%             ).
%
%---------------------------------------------------------------------------%
% Width = 18, Line(s) = 16
%
% big(
%   big(
%     big(
%       small,
%       "Level 3",
%       small),
%     "Level 2",
%     small),
%   "Level 1",
%   big(
%     big(
%       small,
%       "Level 3",
%       small),
%     "Level 2",
%     small))
%
%---------------------------------------------------------------------------%
% Width = 16, Line(s) = 16
%
% big(
%   big(
%     big/3,
%     "Level 2",
%     small),
%   "Level 1",
%   big(
%     big/3,
%     "Level 2",
%     small))
%
%---------------------------------------------------------------------------%
% Width = 50, Line(s) = 4
%
% big(
%   big(big/3, "Level 2", small),
%   "Level 1",
%   big(big/3, "Level 2", small))
%
%---------------------------------------------------------------------------%
% Width = 56, Line(s) = 4
%
% big(
%   big(big(small, "Level 3", small), "Level 2", small),
%   "Level 1",
%   big(big(small, "Level 3", small), "Level 2", small))
%
%---------------------------------------------------------------------------%
% Width = 30, Line(s) = 5
%
% big(big/3, "Level 1", big/3)
%
%---------------------------------------------------------------------------%
% Width = 33, Line(s) = 5
%
% big(
%   big(big/3, "Level 2", small),
%   "Level 1",
%   big(big/3, "Level 2", small))
%
%---------------------------------------------------------------------------%
% Width = 75, Line(s) = 1
%
% big(big(big/3, "Level 2", small), "Level 1", big(big/3, "Level 2", small))
%
%---------------------------------------------------------------------------%
% Width = 20, Line(s) = 10
%
% big(
%   big(
%     big/3,
%     "Level 2",
%     small),
%   "Level 1",
%   big(
%     big/3,
%     "Level 2",
%     small))
%
%---------------------------------------------------------------------------%
% Width = 40, Line(s) = 10
%
% big(
%   big(
%     big(small, "Level 3", small),
%     "Level 2",
%     small),
%   "Level 1",
%   big(
%     big(small, "Level 3", small),
%     "Level 2",
%     small))
%
%---------------------------------------------------------------------------%
% Width = 29, Line(s) = 1
%
% big(big/3, "Level 1", big/3)
%
%---------------------------------------------------------------------------%
% Width = 20, Line(s) = 1
%
% big/3
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.sized_pretty.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

:- import_module univ.

%---------------------------------------------------------------------------%

    % univ_to_string_line(Univ, LineWidth, Lines, String):
    %
    % Converts the term in Univ to a string that fits into Lines lines
    % of width LineWidth. It may throw an exception or cause a runtime
    % abort if the term in question has no canonical representation.
    %
:- pred univ_to_string_line(browser_db::in, univ::in,
    int::in, int::in, string::out) is cc_multi.

    % The same as univ_to_string_line, except works on browser_terms.
    %
:- pred browser_term_to_string_line(browser_db::in,
    browser_term::in, int::in, int::in, string::out) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module pprint.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

:- type no_measure_params
    --->    no_measure_params.

:- type measure_params
    --->    measure_params(int).    % This parameter specifies Linewidth

:- type maybe_deconstructed(T)
    --->    not_deconstructed
    ;       deconstructed(
                string,                 % Functor
                int,                    % Arity
                size_annotated_args(T)  % arguments
            ).

    % The exact type indicates the term has been fully deconstructed.
    % The at_least type indicates that the term has been deconstructed
    % upto the point where the specified limit was reached.
:- type size_annotated_term(T)
    --->    exact(
                browser_term,           % univ(Term)
                T,                      % size of the term
                string,                 % Functor
                int,                    % Arity
                size_annotated_args(T)  % arguments
            )
    ;       at_least(
                browser_term,           % univ(Term)
                T,                      % size of the term up to the
                                        % point where it is deconstructed
                maybe_deconstructed(T)
            ).

:- type size_annotated_args(T) ==
    list(maybe(pair(T, size_annotated_term(T)))).

:- typeclass measure(T) where [
    func max_measure(T, T) = T is det,
    func zero_measure = T is det,
    func compare_measures(T, T) = comparison_result is det
].

:- typeclass measure_with_params(T, MeasureParams) <= measure(T) where [
    func add_measures(T, T, MeasureParams) = T is det,
    func subtract_measures(T, T, MeasureParams) = T is det,

        % Given a measurement and the measure parameters,
        % calculate an upper bound on the number of functors
        % that could fit in that space.
    func maximum_functors(T, MeasureParams) = int,

        % Given a term, its arity, and a limit, this method decides
        % the partial limit that each of the arguments should be
        % given. Its arguments in order are:
        %   - the term to print,
        %   - measure parameter(s),
        %   - measurement of the available space,
        %   - arity of the principal functor of the term,
        %   - a boolean which hints that the size of the principal
        %     functors of arguments should be checked before
        %     analyzing the term,
        %   - size of the principal functor of the term,
        %   - maybe an initial estimate of the measurement for
        %     each argument,
        %   - adjusted measurement of available space,
        %   - adjusted measure parameter(s).
    pred measured_split(browser_db::in, browser_term::in,
        MeasureParams::in, T::in, int::in, bool::in, T::out,
        maybe(T)::out, T::out, MeasureParams::out) is cc_multi
].

%---------------------------------------------------------------------------%

    % When the space (to print the term) is given in number of lines there
    % is an initial check that has to be done. This is due to the fact that
    % size_count_split assumes that every term is part of a bigger term and
    % therefore assumes that a term will have a comma and a space after it.
    % This is not true for the biggest term (Head Term) therefore if the
    % Head Term is going to be printed on a single line then it should be
    % given a limit of character_count(LineWidth - 1) instead of
    % character_count(LineWidth - 3).
    %
univ_to_string_line(BrowserDb, Univ, LineWidth, Lines, String) :-
    browser_term_to_string_line(BrowserDb, plain_term(Univ),
        LineWidth, Lines, String).

browser_term_to_string_line(BrowserDb, BrowserTerm, LineWidth, Lines,
        String) :-
    Params = measure_params(LineWidth),
    functor_browser_term_cc(BrowserDb, BrowserTerm, _Functor, Arity,
        MaybeReturn),
    ( if
        Arity \= 0,
        Lines \= 0,
        (Lines - 1) // Arity = 0
    then
        % "- 1" is to account for the newline character
        Limit = character_count(LineWidth - 1)
    else
        Limit = line_count(Lines)
    ),
    annotate_with_size(BrowserDb, BrowserTerm, Params, Limit, AnnotTerm),
    ( if
        MaybeReturn = yes,
        BrowserTerm = synthetic_term(_, _, yes(ReturnValue))
    then
        annotate_with_size(BrowserDb, plain_term(ReturnValue), Params, Limit,
            AnnotReturn),
        to_doc_sized(AnnotTerm, AnnotTermStr),
        to_doc_sized(AnnotReturn, AnnotReturnStr),
        Doc = group(
            AnnotTermStr
            `<>` line
            `<>` nest(2, text(" = ") `<>` AnnotReturnStr)
        )
    else
        to_doc_sized(AnnotTerm, Doc)
    ),
    String = pprint.to_string(LineWidth, Doc).

%---------------------------------------------------------------------------%

    % first_pass gives an idea of how much space each term takes.
    % In this pass the space is unevenly distributed; first come first
    % served. And once the space runs out, the term is not deconstructed
    % further.
    % In the second pass, the space is evenly distributed between
    % the terms and therefore the subterms are deconstructed evenly.
    %
:- pred annotate_with_size(browser_db::in, browser_term::in, MeasureParams::in,
    T::in, size_annotated_term(T)::out) is cc_multi
    <= measure_with_params(T, MeasureParams).

annotate_with_size(BrowserDb, BrowserTerm, Params, Limit, SizedTerm2) :-
    first_pass(BrowserDb, BrowserTerm, Params, Limit, SizedTerm1),
    second_pass(BrowserDb, SizedTerm1, Params, Limit, SizedTerm2).

%---------------------------------------------------------------------------%

:- pred first_pass(browser_db::in, browser_term::in, MeasureParams::in, T::in,
    size_annotated_term(T)::out) is cc_multi
    <= measure_with_params(T, MeasureParams).

first_pass(BrowserDb, BrowserTerm, Params, Limit, Size) :-
    MaxFunctors = maximum_functors(Limit, Params),
    limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm,
        MaxFunctors, MaybeFunctorArityArgs, _MaybeReturn),
    (
        MaybeFunctorArityArgs = yes({Functor, Arity, Args}),
        measured_split(BrowserDb, BrowserTerm, Params, Limit, Arity, yes,
            FunctorSize, MaybeInitArgLimit, NewLimit, NewParams),
        ( if
            Arity \= 0,
            MaybeInitArgLimit = no
        then
            Exact0 = no
        else
            Exact0 = yes
        ),
        annotate_args_with_size(BrowserDb, Args, MaybeInitArgLimit, NewParams,
            NewLimit, FunctorSize, SoFar, Exact0, Exact, MaybeArgSizes),
        (
            Exact = no,
            Size = at_least(BrowserTerm, SoFar,
                deconstructed(Functor, Arity, MaybeArgSizes))
        ;
            Exact = yes,
            Size = exact(BrowserTerm, SoFar, Functor, Arity, MaybeArgSizes)
        )
    ;
        MaybeFunctorArityArgs = no,
        Size = at_least(BrowserTerm, zero_measure, not_deconstructed)
    ).

%---------------------------------------------------------------------------%

    % Annotating the arguments.
    %
:- pred annotate_args_with_size(browser_db::in, list(univ)::in, maybe(T)::in,
    MeasureParams::in, T::in, T::in, T::out, bool::in, bool::out,
    size_annotated_args(T)::out) is cc_multi
    <= measure_with_params(T, MeasureParams).

annotate_args_with_size(_, [], _, _, _, SoFar, SoFar, Exact, Exact, []).
annotate_args_with_size(BrowserDb, [Arg | Args], MaybeInitArgLimit, Params,
        Limit, SoFar0, SoFar, Exact0, Exact,
        [MaybeArgSize | MaybeArgSizes]) :-
    (
        MaybeInitArgLimit = yes(InitArgLimit),
        ( if compare_measures(SoFar0, Limit) = (>) then
            AppliedArgLimit = InitArgLimit
        else
            AppliedArgLimit = max_measure(InitArgLimit,
                subtract_measures(Limit, SoFar0, Params))
        ),
        first_pass(BrowserDb, plain_term(Arg), Params, AppliedArgLimit, Size),
        MaybeArgSize = yes(InitArgLimit - Size),
        extract_size_from_annotation(Size) = ArgSize,
        SoFar1 = add_measures(SoFar0, ArgSize, Params),
        (
            Size = exact(_, _, _, _, _),
            Exact1 = Exact0
        ;
            Size = at_least(_, _, _),
            Exact1 = no
        )
    ;
        MaybeInitArgLimit = no,
        MaybeArgSize = no,
        SoFar1 = SoFar0,
        Exact1 = Exact0
    ),
    ( if compare_measures(SoFar1, Limit) = (>) then
        Exact2 = no
    else
        Exact2 = Exact1
    ),
    annotate_args_with_size(BrowserDb, Args, MaybeInitArgLimit, Params,
        Limit, SoFar1, SoFar, Exact2, Exact, MaybeArgSizes).

%---------------------------------------------------------------------------%

:- func extract_size_from_annotation(size_annotated_term(T)) = T.

extract_size_from_annotation(exact(_, Size, _, _, _)) = Size.
extract_size_from_annotation(at_least(_, Size, _)) = Size.

%---------------------------------------------------------------------------%

:- func extract_browser_term_from_annotation(size_annotated_term(T)) =
    browser_term.

extract_browser_term_from_annotation(exact(BrowserTerm, _, _, _, _)) =
    BrowserTerm.
extract_browser_term_from_annotation(at_least(BrowserTerm, _, _)) =
    BrowserTerm.

%---------------------------------------------------------------------------%

    % This predicate basically ensures that the arguments that take up
    % smaller "Space" than their fair share are fully printed, and the rest
    % of Space is shared equally between the other terms, which could take up
    % more than their share. If a term can be fully printed within
    % the given space, ("exact" type) then the Term is not altered.
    %
:- pred second_pass(browser_db::in, size_annotated_term(T)::in,
    MeasureParams::in, T::in, size_annotated_term(T)::out) is cc_multi
    <= measure_with_params(T, MeasureParams).

second_pass(BrowserDb, OldSizeTerm, Params, Limit, NewSizeTerm) :-
    (
        OldSizeTerm = exact(_BrowserTerm, _Size, _, _Arity, _MaybeArgs),
        NewSizeTerm = OldSizeTerm
    ;
        OldSizeTerm = at_least(_BrowserTerm, _Size, not_deconstructed),
        NewSizeTerm = OldSizeTerm
    ;
        OldSizeTerm = at_least(BrowserTerm, _Size,
            deconstructed(Functor, Arity,MaybeArgs)),
        measured_split(BrowserDb, BrowserTerm, Params, Limit, Arity,
            yes, FSize, MaybeInitLimit, NewLimit, NewParams),
        (
            MaybeInitLimit = yes(InitLimit),
            check_args(NewParams, MaybeArgs, InitLimit, Passed, FSize, Used),
            LeftOver = add_measures(subtract_measures(NewLimit,
                Used, Params), FSize, Params),
            measured_split(BrowserDb, BrowserTerm, Params, LeftOver,
                Arity - Passed, no, _, MaybeSplitLimit, _, _),
            (
                MaybeSplitLimit = yes(SplitLimit),
                process_args(BrowserDb, NewParams, MaybeArgs,
                    InitLimit, SplitLimit, NewArgs, NewSize0),
                NewSize = add_measures(FSize, NewSize0, NewParams),
                Result0 = list.map(check_if_exact, NewArgs),
                list.remove_adjacent_dups(Result0, Result),
                ( if Result = [yes] then
                    NewSizeTerm = exact(BrowserTerm, NewSize, Functor, Arity,
                        NewArgs)
                else
                    NewSizeTerm = at_least(BrowserTerm, NewSize,
                    deconstructed(Functor, Arity, NewArgs))
                )
            ;
                MaybeSplitLimit = no,
                NewSizeTerm = at_least(BrowserTerm, FSize, not_deconstructed)
            )
        ;
            MaybeInitLimit = no,
            NewSizeTerm = at_least(BrowserTerm, FSize, not_deconstructed)
        )
    ).

%---------------------------------------------------------------------------%

    % Given a list of size annotated terms(ie arguments) and a Limit,
    % this predicate returns "Passed" and "Used", where "Passed"
    % represents the number of terms that obey the Limit and are fully
    % represented("exact"), and "Used" represents the space that
    % these terms take up.
    %
:- pred check_args(MeasureParams::in, size_annotated_args(T)::in, T::in,
    int::out, T::in, T::out) is det
    <= measure_with_params(T, MeasureParams).

check_args(_, [], _, 0, !Used).
check_args(Params, [HeadArg | Rest], ArgLimit, Passed, !Used) :-
    (
        HeadArg = yes(X),
        X = _ - STerm,
        Size = extract_size_from_annotation(STerm),
        (
            STerm = exact(_, _, _, _, _),
            ( if compare_measures(ArgLimit, Size) = (<) then
                check_args(Params, Rest, ArgLimit, Passed, !Used)
            else
                Passed = 1 + PassedRest,
                !:Used = add_measures(!.Used, Size, Params),
                check_args(Params, Rest, ArgLimit, PassedRest, !Used)
            )
        ;
            STerm = at_least(_, _, _),
            check_args(Params, Rest, ArgLimit, Passed, !Used)
        )
    ;
        HeadArg = no,
        check_args(Params, Rest, ArgLimit, Passed, !Used)
    ).

%---------------------------------------------------------------------------%

    % This predicate accepts a list of size annotated terms(paired with a flag)
    % and returns a list of the same type. This new list would consist of the
    % same number of terms as the other but the terms which do not obey the
    % limit or not fully represented would be annotated again with a new limit
    % (SplitLimit). The rest of the terms are left alone.
    %
:- pred process_args(browser_db::in, MeasureParams::in,
    size_annotated_args(T)::in, T::in, T::in, size_annotated_args(T)::out,
    T::out) is cc_multi <= measure_with_params(T, MeasureParams).

process_args(_, _, [], _, _, [], zero_measure).
process_args(BrowserDb, Params, [HeadArg | Rest], ArgLimit, SplitLimit,
        [NewHeadArg | NewRest], SizeOut) :-
    (
        HeadArg = yes(X),
        X = _ - STerm,
        Size = extract_size_from_annotation(STerm),
        BrowserTerm = extract_browser_term_from_annotation(STerm),
        ( if
            STerm = exact(_, _, _, _, _),
            (
                compare_measures(ArgLimit, Size) = (>)
            ;
                compare_measures(ArgLimit, Size) = (=)
            )
        then
            NewHeadArg = HeadArg
        else
            NewHeadArg = yes(pair(SplitLimit, NewSTerm)),
            annotate_with_size(BrowserDb, BrowserTerm, Params, SplitLimit,
                NewSTerm)
        )
    ;
        HeadArg = no,
        NewHeadArg = no
    ),
    (
        NewHeadArg = yes(_ - Term),
        NewSize = extract_size_from_annotation(Term),
        SizeOut = add_measures(NewSize, RestSize, Params)
    ;
        NewHeadArg = no,
        SizeOut = RestSize
    ),
    process_args(BrowserDb, Params, Rest, ArgLimit, SplitLimit, NewRest,
        RestSize).

%---------------------------------------------------------------------------%

    % Check if a size-annotated arg is an exact type (fully represented).
    %
:- func check_if_exact(maybe(pair(T, size_annotated_term(T)))) = bool.

check_if_exact(no) = no.
check_if_exact(yes(_ - Term)) = Result:-
    (
        Term = exact(_, _, _, _, _),
        Result = yes
    ;
        Term = at_least(_, _, _),
        Result = no
    ).

%---------------------------------------------------------------------------%

    % Convert a size annotated term to a 'doc'.
    % (The doc type is defined in pprint.m).
    %
:- pred to_doc_sized(size_annotated_term(T)::in, doc::out) is cc_multi.

to_doc_sized(at_least(BrowserTerm, _, not_deconstructed), Doc) :-
    (
        BrowserTerm = plain_term(Univ),
        functor(univ_value(Univ), include_details_cc, Functor, Arity),
        Doc = text(Functor) `<>` text("/") `<>` poly(i(Arity))
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        list.length(Args, Arity),
        (
            MaybeReturn = yes(_),
            Doc = text(Functor) `<>` text("/") `<>`
                poly(i(Arity)) `<>` text("+1")
        ;
            MaybeReturn = no,
            Doc = text(Functor) `<>` text("/") `<>` poly(i(Arity))
        )
    ).
to_doc_sized(at_least(_, _, deconstructed(Functor, Arity, MaybeArgs)), Doc) :-
    to_doc_sized_2(Functor, Arity, MaybeArgs, Doc).
to_doc_sized(exact(_, _, Functor, Arity, MaybeArgs), Doc) :-
    to_doc_sized_2(Functor, Arity, MaybeArgs, Doc).

%---------------------------------------------------------------------------%

    % Assumes that every argument must be on a different line
    % or all of them should be on the same line.
    %
:- pred to_doc_sized_2(string::in, int::in, size_annotated_args(T)::in,
    doc::out) is cc_multi.

to_doc_sized_2(Functor, _Arity, [], text(Functor)).
to_doc_sized_2(Functor, Arity, [HeadArg | Tail], Doc) :-
    list.map(handle_arg, [HeadArg | Tail], Args),
    list.remove_adjacent_dups(Args, NewArgs),
    ( if NewArgs = [nil] then
        Doc = text(Functor) `<>` text("/") `<>` poly(i(Arity))
    else
        Doc = text(Functor) `<>`
            parentheses(group(nest(2,
                line `<>` separated(id, comma_space_line, Args))))
    ).

%---------------------------------------------------------------------------%

:- pred handle_arg(maybe(pair(T,size_annotated_term(T)))::in, doc::out)
    is cc_multi.

handle_arg(yes(_ - Arg_Term), Doc) :-
    to_doc_sized(Arg_Term, Doc).
handle_arg(no, nil).

%---------------------------------------------------------------------------%

    % functor_count is a representation where the size of a term
    % is measured by the number of function symbols.
:- type functor_count
    --->    functor_count(int).     % No of function symbols

:- func add_functor_count(functor_count, functor_count,
    no_measure_params) = functor_count.

add_functor_count(functor_count(A), functor_count(B), _) =
    functor_count(A + B).

:- func subtract_functor_count(functor_count, functor_count,
    no_measure_params) = functor_count.

subtract_functor_count(functor_count(A), functor_count(B), _) =
    functor_count(A - B).

:- func maximum_functor_count(functor_count, no_measure_params) = int.

maximum_functor_count(functor_count(N), _) = N.

:- func compare_functor_count(functor_count, functor_count)
    = comparison_result.

compare_functor_count(functor_count(A), functor_count(B)) = R :-
    compare(R, A, B).

:- func max_functor_count(functor_count, functor_count) = functor_count.

max_functor_count(functor_count(A), functor_count(B)) = functor_count(Max) :-
    int.max(A, B, Max).

:- func zero_functor_count = functor_count.

zero_functor_count = functor_count(0).

:- pred functor_count_split(browser_db::in, browser_term::in,
    no_measure_params::in, functor_count::in, int::in, bool::in,
    functor_count::out, maybe(functor_count)::out, functor_count::out,
    no_measure_params::out) is cc_multi.

functor_count_split(_, _, Params, functor_count(Limit), Arity, _,
        functor_count(1), MaybeArgLimit, functor_count(Limit), Params) :-
    ( if Arity = 0 then
        % This artificial detism cast shuts up a warning about
        % the true detism of functor_count_split being det.
        MaybeArgLimit0 = no,
        cc_multi_equal(MaybeArgLimit0, MaybeArgLimit)
    else
        ( if Limit =< Arity + 1 then
            MaybeArgLimit = no
        else
            RoundUp = (Limit + Arity - 1) // Arity,
            MaybeArgLimit = yes(functor_count(RoundUp))
        )
    ).

:- instance measure(functor_count) where [
    func(compare_measures/2) is compare_functor_count,
    func(max_measure/2) is max_functor_count,
    func(zero_measure/0) is zero_functor_count
].

:- instance measure_with_params(functor_count, no_measure_params) where [
    func(add_measures/3) is add_functor_count,
    func(subtract_measures/3) is subtract_functor_count,
    func(maximum_functors/2) is maximum_functor_count,
    pred(measured_split/10) is functor_count_split
].

%---------------------------------------------------------------------------%

    % char_count is a representation where the size of a term is
    % measured by the number of characters.
:- type char_count
    --->    char_count(int).    % No of characters

:- func add_char_count(char_count, char_count, no_measure_params) = char_count.

add_char_count(char_count(A), char_count(B), _) = char_count(A + B).

:- func subtract_char_count(char_count, char_count,
    no_measure_params) = char_count.

subtract_char_count(char_count(A), char_count(B), _) =
    char_count(A - B).

:- func maximum_char_count(char_count, no_measure_params) = int.

maximum_char_count(char_count(N), _) = Max :-
    % Each functor except the first takes a minimum of three chars.
    Max = (N + 2) div 3.

:- func compare_char_count(char_count, char_count) = comparison_result.

compare_char_count(char_count(A), char_count(B)) = R :-
    compare(R, A, B).

:- func max_char_count(char_count, char_count) = char_count.

max_char_count(char_count(A), char_count(B)) = char_count(Max) :-
    int.max(A, B, Max).

:- func zero_char_count = char_count.

zero_char_count = char_count(0).

:- pred char_count_split(browser_db::in, browser_term::in,
    no_measure_params::in, char_count::in, int::in, bool::in,
    char_count::out, maybe(char_count)::out, char_count::out,
    no_measure_params::out) is cc_multi.

char_count_split(BrowserDb, BrowserTerm, Params, char_count(Limit), Arity,
        Check, char_count(FunctorSize), MaybeArgLimit, char_count(Limit),
        Params) :-
    deconstruct_browser_term_cc(BrowserDb, BrowserTerm, Functor, _, Args,
        MaybeReturn),
    (
        Check = yes,
        get_arg_length(Args, TotalLength, _)
    ;
        Check = no,
        TotalLength = 0
    ),
    (
        MaybeReturn = yes(_),
        % Arity-1 times the string ", ", once "()", and once " = "
        FunctorSize = string.length(Functor) + 2 * Arity + 3
    ;
        MaybeReturn = no,
        % Arity-1 times the string ", ", and once "()"
        FunctorSize = string.length(Functor) + 2 * Arity
    ),
    ( if Arity = 0 then
        MaybeArgLimit = no
    else
        ( if Limit =< (FunctorSize + TotalLength) then
            MaybeArgLimit = no
        else
            RoundUp = (Limit + Arity - FunctorSize) // Arity,
            MaybeArgLimit = yes(char_count(RoundUp))
        )
    ).

:- instance measure(char_count) where [
    func(compare_measures/2) is compare_char_count,
    func(max_measure/2) is max_char_count,
    func(zero_measure/0) is zero_char_count
].

:- instance measure_with_params(char_count, no_measure_params) where [
    func(add_measures/3) is add_char_count,
    func(subtract_measures/3) is subtract_char_count,
    func(maximum_functors/2) is maximum_char_count,
    pred(measured_split/10) is char_count_split
].

%---------------------------------------------------------------------------%

    % size_count is a representation where the size of a term is
    % measured by number of lines or number of characters.
:- type size_count
    --->    line_count(int)         % no of lines
    ;       character_count(int).   % no of characters

:- func add_size_count(size_count, size_count, measure_params) = size_count.

add_size_count(character_count(A), character_count(B), Params) = Result :-
    Params = measure_params(LineWidth),
    CharSum = A + B,
    ( if CharSum > LineWidth then
        Result = line_count(1)
    else
        Result = character_count(CharSum)
    ).

add_size_count(character_count(A), line_count(B), _) = Result :-
    ( if A > 0 then
        Result = line_count(B + 1)
    else
        Result = line_count(B)
    ).

add_size_count(line_count(A), character_count(B), _) = Result :-
    ( if B > 0 then
        Result = line_count(A + 1)
    else
        Result = line_count(A)
    ).

add_size_count(line_count(A), line_count(B), _) = line_count(A + B).

    % Rounding up the Lines and subtracting works because we assume that
    % each argument is a different line or they are all on the same line.
    % But this requires you to determine which case likely to happen
    % before hand. For example if a term is to be on one line, you should
    % do subtract_size_count(character_count(LineLength),
    % character_count(arglength)) rather than
    % subtract_size_count(line_count(1), character_count(arglength)).
    % The reason that this situation cannot be detected in this code is:
    % A term can be printed on a single line only if all of its arguments
    % can be printed on the same line. And you cannot determine which case
    % is likely to happen in this code using the information it has.
    % Therefore size_count_split determines which case is true,
    % and changes the limit accordingly.
    %
:- func subtract_size_count(size_count, size_count,measure_params)
    = size_count.

subtract_size_count(character_count(A), character_count(B), _) = Result :-
    CharDiff = A - B,
    ( if CharDiff < 0 then
        Result = character_count(0)
    else
        Result = character_count(CharDiff)
    ).

subtract_size_count(character_count(A), line_count(B), _) = Result :-
    ( if B = 0 then
        Result = character_count(A)
    else
        Result = character_count(0)
    ).

subtract_size_count(line_count(A), character_count(B), _) = Result :-
    ( if B = 0 then
        Result = line_count(A)
    else
        ( if A - 1 >= 0 then
            Result = line_count(A - 1)
        else
            Result = line_count(0)
        )
    ).

subtract_size_count(line_count(A), line_count(B), _) = Result :-
    ( if A - B >= 0 then
        Result = line_count(A - B)
    else
        Result = line_count(0)
    ).

:- func maximum_size_count(size_count, measure_params) = int.

maximum_size_count(character_count(N), _) = Max :-
    % Each functor except the first takes a minimum of three chars.
    Max = (N + 2) div 3.

maximum_size_count(line_count(N), measure_params(LineWidth)) = Max :-
    % We can't assume any particular layout, so we go by how many functors
    % will fit in the area covered by N lines. Each functor except the first
    % takes a minimum of three chars.
    Area = N * LineWidth,
    Max = (Area + 2) div 3.

:- func compare_size_count(size_count, size_count) = comparison_result.

compare_size_count(character_count(C1), character_count(C2)) = R :-
    compare(R, C1, C2).
compare_size_count(character_count(_), line_count(_)) = (<).
compare_size_count(line_count(_), character_count(_)) = (>).
compare_size_count(line_count(L1), line_count(L2)) = R :-
    compare(R, L1, L2).

:- func max_size_count(size_count, size_count) = size_count.

max_size_count(A, B) = Max :-
    ( if compare_size_count(A, B) = (>) then
        Max = A
    else
        Max = B
    ).

:- func zero_size_count = size_count.

zero_size_count = character_count(0).

    % We assume that all arguments have to be on separate lines, or
    % the whole term should be printed on a single line.
    %
:- pred size_count_split(browser_db::in, browser_term::in, measure_params::in,
    size_count::in, int::in, bool::in, size_count::out,
    maybe(size_count)::out, size_count::out, measure_params::out)
    is cc_multi.

size_count_split(BrowserDb, BrowserTerm, Params, Limit, Arity, Check,
        FunctorSize, MaybeArgLimit, NewLimit, NewParams) :-
    % LineWidth is length of the line in which the functor is printed.
    Params = measure_params(LineWidth),
    deconstruct_browser_term_cc(BrowserDb, BrowserTerm, Functor, ActualArity,
        Args, MaybeReturn),
    FSize = string.length(Functor) + 2 * (ActualArity),
    (
        Check = yes,
        get_arg_length(Args, TotalLength, MaxArgLength),
        int.max(MaxArgLength, (string.length(Functor) + 1), MaxLength)
    ;
        Check = no,
        TotalLength = 0,
        MaxLength = 0
    ),
    ( if Arity = 0 then
        MaybeArgLimit = no,
        FunctorSize = character_count(FSize),
        NewLimit = Limit,
        NewParams = Params
    else
        ( if
            Limit = line_count(LineLimit),
            % we need one line for the functor and at least one line
            % for each argument.
            LineLimit >= (Arity + 1),
            % Linewidth is decreased by two characters to account for
            % indentation.
            (LineWidth - 2) >= MaxLength
        then
            Line = (LineLimit - 1) // Arity,
            MaybeArgLimit = yes(line_count(Line)),
            FunctorSize = line_count(1),
            NewLimit = Limit,
            NewParams = measure_params(LineWidth - 2)
        else if
            Limit = line_count(LineLimit),
            LineLimit > 0,
            % Since every term is part of a bigger term (in this context
            % anyway) it will have a comma, space and a newline at the end
            % of it (Hence the "- 3").
            LineWidth - 3 >= (FSize + TotalLength)
        then
            % "Arity - 1" is for rounding up.
            Char = (LineWidth - 3 - FSize + Arity - 1) // Arity,
            MaybeArgLimit = yes(character_count(Char)),
            FunctorSize = character_count(FSize),
            NewLimit = character_count(LineWidth - 3),
            NewParams = Params
        else if
            Limit = character_count(CharLimit),
            CharLimit >= (FSize + TotalLength)
        then
            Char = (CharLimit - FSize + Arity - 1) // Arity,
            MaybeArgLimit = yes(character_count(Char)),
            FunctorSize = character_count(FSize),
            NewLimit = Limit,
            NewParams = Params
        else
            MaybeArgLimit = no,
            % If an ordinary term is not deconstructed, it is printed as
            % "functor/Arity". If a synthetic function term is not
            % deconstructed, it is printed as "functor/Arity+1".
            (
                MaybeReturn = yes(_),
                FuncSuffixChars = 2
            ;
                MaybeReturn = no,
                FuncSuffixChars = 0
            ),
            FunctorLength = string.length(Functor),
            string.int_to_string(Arity, ArityStr),
            string.length(ArityStr, ArityChars),
            FunctorSize = character_count(FunctorLength + 1
                + ArityChars + FuncSuffixChars),
            NewLimit = Limit,
            NewParams = Params
        )
    ).

:- instance measure(size_count) where [
    func(compare_measures/2) is compare_size_count,
    func(max_measure/2) is max_size_count,
    func(zero_measure/0) is zero_size_count
].

:- instance measure_with_params(size_count, measure_params) where [
    func(add_measures/3) is add_size_count,
    func(subtract_measures/3) is subtract_size_count,
    func(maximum_functors/2) is maximum_size_count,
    pred(measured_split/10) is size_count_split
].

%---------------------------------------------------------------------------%

    % This predicate determines how many characters it will take
    % to print the functors of the arguments. Also determines the
    % length of biggest functor.
    %
:- pred get_arg_length(list(univ)::in, int::out, int::out) is cc_multi.

get_arg_length([], 0, 0).
get_arg_length([HeadUniv | Rest], TotalLength, MaxLength) :-
    functor(univ_value(HeadUniv), include_details_cc, Functor, Arity),
    (
        Rest = [],
        Correction = 2
    ;
        Rest = [_ | _],
        Correction = 3
    ),
    ( if Arity = 0 then
        Length = string.length(Functor)
    else
        % 2 is added because if a term has arguments then the
        % shortest way to print it is "functor/Arity"
        % Assuming Arity is a single digit
        Length = string.length(Functor) + 2
    ),
    TotalLength = Length + RestTotalLength,
    int.max((Length + Correction), RestMaxLength, MaxLength),
    get_arg_length(Rest, RestTotalLength, RestMaxLength).

%---------------------------------------------------------------------------%
