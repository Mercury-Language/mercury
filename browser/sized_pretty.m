%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% sized_pretty-	When printing a term during debugging this module allows
%		the user to put a limit on the size of the term displayed.
%               This limit is specified by setting the number of lines you 
%               want and the width of these lines. 
%
%
% author: sthur
%
% How to use sized_pretty.m :
% ---------------------------
%
% Call univ_to_string_line with the follwing variables:
% 	univ_to_string_line(Univ, LineWidth, Lines, String)
%		where Univ 	: is the Term (in univ type) you want to convert
%		      LineWidth : is the length of the lines
%		      Lines 	: is the number of lines you want the term to be
%		            	  printed on
%		      String	: output string
%
% EXAMPLES
% --------
%
% Term Used in these examples:
%
%	Term = big(
%                 big(
%                    big(
%                       small,
%			"Level 3",
%                       small
%                       ),
%		     "Level 2",
%                    small
%                    ),
%		  "Level 1",
%                 big(
%                    big(
%                       small,
%		        "Level 3",
%			small
%                       ),
%		     "Level 2",
%		     small
%                    )).
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
%  "Level 1", 
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

:- module mdb__sized_pretty.

:- interface.

:- import_module std_util, int, string.

	% This may throw an exception or cause a runtime abort if the term
	% in question has user-defined equality. 
	% The Limit is number of lines.
:- pred univ_to_string_line(univ::in, int::in, int::in, string::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list, require, assoc_list, pprint, bool.

:- type no_measure_params --->	no_measure_params.
:- type measure_params
	--->	measure_params(int).	% This parameter specifies Linewidth


:- type maybe_deconstructed(T)
	--->	not_deconstructed
	;	deconstructed(
			string,			% Functor 
			int,			% Arity
			size_annotated_args(T) 	% arguments
		).

	% The exact type indicates the term has been fully deconstructed.
	% The at_least type indicates that the term has been deconstructed
	% upto the point where the specified limit was reached.
:- type size_annotated_term(T)
	--->	exact(
			univ,			% univ(Term)
			T,			% size of the term
			string,			% Functor
			int,			% Arity
			size_annotated_args(T) 	% arguments
		)
	;	at_least(
			univ,		% univ(Term)
			T,		% size of the term upto the point
					% where it's deconstructed
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

		% given a term, it's arity, and a limit, this method decides
		% the partial limit that each of the argument should be
		% given. It's arguments in order are term, measure parameter(s),
		% limit, arity, a flag (once the limit for the subterms 
		% is determined the subterms are flagged with this limit), size 
		% of the Functor, partial limit, adjusted limit, adjusted 
		% measure parameter(s).
		% Also a term is not deconstructed unless it has enough space
		% to print functor and the functors of it's arguments.
		% If the bool is `yes', we check that there is enough space to 
		% print the functor and the functors of its arguments before 
		% deconstructing the term. 
	pred measured_split(univ::in, MeasureParams::in, T::in, int::in,
	     bool::in, T::out, maybe(T)::out, T::out, MeasureParams::out) is det
		
].

%---------------------------------------------------------------------------%
	% When the space (to print the term) is given in number of lines there
	% is an inital check that has to be done. This is due to the fact that
	% size_count_split assumes that every term is part of a bigger term and
	% therefore assumes that a term will have a comma and a space after it.
	% This is not true for the biggest term (Head Term) therefore if the
	% Head Term is going to be printed on a single line then it should be
	% given a limit of character_count(LineWidth - 1) instead of
	% character_count(LineWidth - 3).
univ_to_string_line(Univ, LineWidth, Lines, String) :-
	Params = measure_params(LineWidth),
	functor(univ_value(Univ), _, Arity),
	( 	Arity \= 0,
		Lines \= 0,
		(Lines - 1) // Arity = 0 
	->
		% "- 1" is to account for the newline character
		Limit = character_count(LineWidth - 1)
	;
		Limit = line_count(Lines)
	),
	annotate_with_size(Univ, Params, Limit, AnnotTerm),
	Doc = to_doc_sized(AnnotTerm),
	String = pprint__to_string(LineWidth, Doc).

%---------------------------------------------------------------------------%
	% first_pass gives an idea of how much space each term takes
	% In this pass the space is unevenly distributed. First come first
	% served. And once the space runs out, the term is not deconstructed
	% further. 
	% In The Second pass the space is evenly distributed between
	% the terms and therefore the subterms are deconstructed evenly.
:- pred annotate_with_size(univ::in, MeasureParams::in, T::in,
	size_annotated_term(T)::out) is det
	<= measure_with_params(T, MeasureParams).

annotate_with_size(Univ, Params, Limit, Size2) :-
	first_pass(Univ, Params, Limit, Size1),
	second_pass(Size1, Params, Limit, Size2).

%---------------------------------------------------------------------------%
	
:- pred first_pass(univ::in, MeasureParams::in, T::in,
	size_annotated_term(T)::out) is det
	<= measure_with_params(T, MeasureParams).

first_pass(Univ, Params, Limit, Size) :-
	MaxFunctors = maximum_functors(Limit, Params),
	(
		limited_deconstruct(univ_value(Univ), MaxFunctors,
				Functor, Arity, UnivArgs)
	->
		measured_split(Univ, Params, Limit, Arity, yes, FunctorSize, 
						Flag, NewLimit, NewParams),
		( (Arity \= 0, Flag = no) ->
			Exact0 = no
		;
			Exact0 = yes
		),
		annotate_args_with_size(UnivArgs, Flag, NewParams, NewLimit, 
			FunctorSize, SoFar, Exact0, Exact, MaybeArgSizes),
		(
			Exact = no,
			Size = at_least(Univ, SoFar,
				deconstructed(Functor, Arity, MaybeArgSizes))
		;
			Exact = yes,
			Size = exact(Univ, SoFar, Functor, Arity, MaybeArgSizes)
		)
	;
		Size = at_least(Univ, zero_measure, not_deconstructed)
	).

%---------------------------------------------------------------------------%
	% annotating the arguments.
:- pred annotate_args_with_size(list(univ)::in, maybe(T)::in,
	MeasureParams::in, T::in, T::in, T::out, bool::in, bool::out, 
	size_annotated_args(T)::out) is det <= measure_with_params(T, 
	MeasureParams).

annotate_args_with_size([], _, _, _, SoFar, SoFar, Exact, Exact, []).
annotate_args_with_size([Arg | Args], Flag, Params, Limit,
		SoFar0, SoFar, Exact0, Exact,
		[MaybeFlaggedSize | MaybeFlaggedSizes]) :-
	(
		Flag = yes(ArgLimit),
		( compare_measures(SoFar0, Limit) = (>) ->
			AppliedArgLimit = ArgLimit
		;
			AppliedArgLimit = max_measure(ArgLimit,
				subtract_measures(Limit, SoFar0, Params))
		),
		first_pass(Arg, Params, AppliedArgLimit, Size),
		MaybeFlaggedSize = yes(ArgLimit - Size),
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
		Flag = no,
		MaybeFlaggedSize = no,
		SoFar1 = SoFar0,
		Exact1 = Exact0
	),
	( compare_measures(SoFar1, Limit) = (>) ->
		Exact2 = no
	;
		Exact2 = Exact1
	),
	annotate_args_with_size(Args, Flag, Params, Limit, SoFar1, SoFar, 
		Exact2, Exact, MaybeFlaggedSizes).

%---------------------------------------------------------------------------%

:- func extract_size_from_annotation(size_annotated_term(T)) = T.

extract_size_from_annotation(exact(_, Size, _, _, _)) = Size.
extract_size_from_annotation(at_least(_, Size, _)) = Size.

%---------------------------------------------------------------------------%

:- func extract_univ_from_annotation(size_annotated_term(T)) = univ.

extract_univ_from_annotation(exact(Univ, _, _, _, _)) = Univ.
extract_univ_from_annotation(at_least(Univ, _, _)) = Univ.

%---------------------------------------------------------------------------%
	% This predicate basically ensures that the arguments that
	% take up smaller "Space" than their fair share is fully
	% printed and the rest the Space is shared equally between
	% the other terms which could take up more than their share.
	% If a term can be fully printed within the given space,
	% ("exact" type) then the Term is not altered.
:- pred second_pass(size_annotated_term(T)::in, MeasureParams::in, T::in,
	size_annotated_term(T)::out) is det 
	<= measure_with_params(T, MeasureParams).

second_pass(OldSizeTerm, Params, Limit, NewSizeTerm) :-
	(
    		OldSizeTerm = exact(_Univ, _Size, _, _Arity, _MaybeArgs),
		NewSizeTerm = OldSizeTerm
	;
    		OldSizeTerm = at_least(_Univ, _Size, not_deconstructed),
		NewSizeTerm = OldSizeTerm
	;
    		OldSizeTerm = at_least(Univ, _Size, deconstructed(Functor, 
			Arity,MaybeArgs)),
		measured_split(Univ, Params, Limit, Arity, yes, FSize, Flag,
			NewLimit, NewParams),
		( if Flag = yes(X) then
	    		ArgLimit = X,
	    		check_args(NewParams, MaybeArgs, ArgLimit, Passed, 
				FSize, Used),
			LeftOver = add_measures(subtract_measures(NewLimit, 
			  	Used, Params), FSize, Params),
	    		measured_split(Univ, Params, LeftOver, Arity - Passed, 
				no, _, Flag2, _, _),
	    		( if Flag2 = yes(Y) then
	        		SplitLimit = Y,
	        		process_args(NewParams, MaybeArgs, ArgLimit, 
					SplitLimit, NewArgs, NewSize0),
				NewSize = add_measures(FSize, NewSize0, 
					NewParams),
				Result0 = list__map(check_if_exact, NewArgs),
    				list__remove_adjacent_dups(Result0, Result),
				( Result = [yes] ->
					NewSizeTerm = exact(Univ, NewSize, 
						Functor, Arity, NewArgs) 	
	        		;
					NewSizeTerm = at_least(Univ, NewSize, 
						deconstructed(Functor, Arity, 
						NewArgs))
				)
	    		else
	        		NewSizeTerm = at_least(Univ, FSize, 
					not_deconstructed)
	    		)
		else
	    	NewSizeTerm = at_least(Univ, FSize, not_deconstructed)
		)
	).
	
%---------------------------------------------------------------------------%
	% Given a list of size annotated terms(ie arguments) and a
	% Limit, this predicate returns the values "Passed" and 
	% "Used". Where "Passed" represents the number of terms that
	% obey the Limit and are fully represented("exact") and "Used"
	% represents the space that these terms take up.
:- pred check_args(MeasureParams::in, size_annotated_args(T)::in, T::in, 
	int::out, T::in, T::out) is det <= measure_with_params(T, 
	MeasureParams).

check_args(_, [], _, 0, Used0, Used0).
check_args(Params, [HeadArg | Rest], ArgLimit, Passed, Used0, Used) :-
	if HeadArg = yes(X) then
		X = _ - STerm,
		Size = extract_size_from_annotation(STerm), 
		( if STerm = exact(_, _, _, _, _) then
	    		( if compare_measures(ArgLimit, Size) = (<) then
	    			check_args(Params, Rest, ArgLimit, Passed, 
					Used0, Used)
	    		else
	    			Passed = 1 + PassedRest,
				UsedSofar = add_measures(Used0, Size, Params),
	    			check_args(Params, Rest, ArgLimit, PassedRest, 
					UsedSofar, Used)
	    		)
		else
	    		check_args(Params, Rest, ArgLimit, Passed, Used0, Used)
		)
    	else
		check_args(Params, Rest, ArgLimit, Passed, Used0, Used).

%---------------------------------------------------------------------------%
	% This predicate accepts a list of size annotated terms(paired
	% with a flag) and returns a list of the same type. This new
	% list would consist of the same number of terms as the other
	% but the terms which do not obey the limit or not fully 
	% represented would be annoted again with a new limit
	% (SplitLimit). The rest of the terms are left alone.
:- pred process_args(MeasureParams::in, size_annotated_args(T)::in, T::in, 
	T::in, size_annotated_args(T)::out, T::out) is det <= 
	measure_with_params(T, MeasureParams).

process_args(_, [], _, _, [], zero_measure).
process_args(Params, [HeadArg | Rest], ArgLimit, SplitLimit, 
		[NewHeadArg | NewRest], SizeOut) :-
    	( if HeadArg = yes(X) then
		X = _ - STerm,
		Size = extract_size_from_annotation(STerm), 
        	Univ = extract_univ_from_annotation(STerm), 
		( 
			STerm = exact(_, _, _, _, _),
	    		(
				compare_measures(ArgLimit, Size) = (>)
			;
	    			compare_measures(ArgLimit, Size) = (=)
			)
		->
			NewHeadArg = HeadArg
		;
			NewHeadArg = yes(pair(SplitLimit, NewSTerm)),
			annotate_with_size(Univ, Params, SplitLimit, NewSTerm)
		)
    	else
		NewHeadArg = no
    	),
    	( NewHeadArg = yes(_ - Term) ->
		NewSize = extract_size_from_annotation(Term),
		SizeOut = add_measures(NewSize, RestSize, Params)
    	;
		SizeOut = RestSize
    	),
    	process_args(Params, Rest, ArgLimit, SplitLimit, NewRest, RestSize).

%---------------------------------------------------------------------------%
	% checking if an size-annotated arg is an exact type (fully represented)
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
	% A function to convert a size annotated term to a 'doc' type,
	% a type defined in pprint.m.
:- func to_doc_sized(size_annotated_term(T)) = doc.

to_doc_sized(at_least(Univ, _, not_deconstructed)) = Doc :-
	functor(univ_value(Univ), Functor, Arity),
	Doc = text(Functor) `<>` text("/") `<>` poly(i(Arity)).

to_doc_sized(at_least(_, _, deconstructed(Functor, Arity, MaybeArgs))) = Doc :-
	Doc = to_doc_sized_2(Functor, Arity, MaybeArgs).

to_doc_sized(exact(_, _, Functor, Arity, MaybeArgs)) = Doc :-
	Doc = to_doc_sized_2(Functor, Arity, MaybeArgs).

%---------------------------------------------------------------------------%
	% Assumes that every argument must be on a different line
	% or all of them should be on the same line.
:- func to_doc_sized_2(string, int, size_annotated_args(T)) = doc.

to_doc_sized_2(Functor, _Arity, []) = text(Functor).

to_doc_sized_2(Functor, Arity, [HeadArg|Tail]) = Doc :-
    	Args = list__map(handle_arg, [HeadArg|Tail]),
    	list__remove_adjacent_dups(Args, NewArgs),
    	( NewArgs \= [nil] -> 
        	Doc = text(Functor) `<>` parentheses(group(nest(2, line `<>` 
			separated(id,comma_space_line, Args))))
    	;
        	Doc = text(Functor) `<>` text("/") `<>` poly(i(Arity))
    	).
	
%---------------------------------------------------------------------------%

:- func handle_arg(maybe(pair(T,size_annotated_term(T)))) = doc.

handle_arg(yes(_ - Arg_Term)) = to_doc_sized(Arg_Term). 
handle_arg(no) = nil.

%---------------------------------------------------------------------------%
	% functor_count is a representation where the size of a term
	% is measured by the number of function symbols.

:- type functor_count
	--->	functor_count(int). 	% No of function symbols

:- func add_functor_count(functor_count, functor_count, 
	no_measure_params) = functor_count.

add_functor_count(functor_count(A), functor_count(B), _) = functor_count(A + B).

:- func subtract_functor_count(functor_count, functor_count, 
	no_measure_params) = functor_count.

subtract_functor_count(functor_count(A), functor_count(B), _) =
	functor_count(A - B).

:- func maximum_functor_count(functor_count, no_measure_params) = int.

maximum_functor_count(functor_count(N), _) = N.

:- func compare_functor_count(functor_count, functor_count) = comparison_result.

compare_functor_count(functor_count(A), functor_count(B)) = R :-
	compare(R, A, B).

:- func max_functor_count(functor_count, functor_count) = functor_count.

max_functor_count(functor_count(A), functor_count(B)) = functor_count(Max) :-
	int__max(A, B, Max).

:- func zero_functor_count = functor_count.

zero_functor_count = functor_count(0).
	
:- pred functor_count_split(univ::in, no_measure_params::in, functor_count::in,
	int::in, bool::in, functor_count::out, maybe(functor_count)::out,
	functor_count::out, no_measure_params::out) is det.

functor_count_split(_, Params, functor_count(Limit), Arity, _, functor_count(1),
		Flag, functor_count(Limit), Params) :-
	( Arity = 0 ->
		Flag = no
	;
		( Limit =< (Arity + 1) ->			
			Flag = no
		;
			RoundUp = (Limit + Arity - 1) // Arity,
			Flag = yes(functor_count(RoundUp))
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
	pred(measured_split/9) is functor_count_split
].


%---------------------------------------------------------------------------%
	% char_count is a representation where the size of a term is
	% measured by the number of characters.

:- type char_count
	--->	char_count(int).	% No of characters

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
	int__max(A, B, Max).

:- func zero_char_count = char_count.

zero_char_count = char_count(0).

:- pred char_count_split(univ::in, no_measure_params::in, char_count::in,
	int::in, bool::in, char_count::out, maybe(char_count)::out,
	char_count::out, no_measure_params::out) is det.

char_count_split(Univ, Params, char_count(Limit), Arity, Check, 
		char_count(FunctorSize), Flag, char_count(Limit), Params) :-
	deconstruct(univ_value(Univ), Functor, _, Args),
	( Check = yes ->
		get_arg_length(Args, TotalLength, _)
	;
		TotalLength = 0
	),
	FunctorSize = string__length(Functor) + 2*(Arity),
	( Arity = 0 ->
		Flag = no
	;
		( Limit =< (FunctorSize + TotalLength) ->
			Flag = no
		;
			RoundUp = (Limit + Arity - FunctorSize) // Arity,
			Flag = yes(char_count(RoundUp))
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
        pred(measured_split/9) is char_count_split
].

%---------------------------------------------------------------------------%
	% size_count is representation where the size of a term is
	% measured by number of lines or number of characters.

:- type size_count
	--->	line_count(int)		% no of lines
	;	character_count(int).	% no of characters

:- func add_size_count(size_count, size_count, measure_params) = size_count.

add_size_count(character_count(A), character_count(B), Params) = Result :-
	Params = measure_params(LineWidth),
	CharSum = A + B,
	( CharSum > LineWidth ->
		Result = line_count(1)
	;
		Result = character_count(CharSum)
	).

add_size_count(character_count(A), line_count(B), _) = Result :-
	( A > 0 -> 
		Result = line_count(B + 1)
	;
		Result = line_count(B)
	).

add_size_count(line_count(A), character_count(B), _) = Result :-
	( B > 0 -> 
		Result = line_count(A + 1)
	;
		Result = line_count(A)
	).

add_size_count(line_count(A), line_count(B), _) = line_count(A + B).

	% Rounding up the Lines and subtracting works because we assume
	% that each argument is a different line or they are all on 
	% the same line. But this requires you to determine which case
	% likely to happen before hand. For example if a term is to be
	% on one line, you should do subtract_size_count(character_count(
	% LineLength), charater_count(arglength)) rather than
	% subtract_size_count(line_count(1), character_count(arglength)).
	% The reason that this situation cannot be detected in this code is:
	% A term can be printed on a single line only if, all of it's 
	% arguments can be printed on the same line. And you cannot determine
	% which case is likely to happen in this code using the information 
	% it has. Therefore size_count_split determines which case is true 
	% (and changes the limit accordingly).
:- func subtract_size_count(size_count, size_count,measure_params) = size_count.

subtract_size_count(character_count(A), character_count(B), _) = Result :-
	CharDiff = A - B,
	( CharDiff < 0 ->
		Result = character_count(0)
	;
		Result = character_count(CharDiff)
	).

subtract_size_count(character_count(A), line_count(B), _) = Result :-
	( B = 0 -> 
		Result = character_count(A)
	;
		Result = character_count(0)
	).

subtract_size_count(line_count(A), character_count(B), _) = Result :-
	( B = 0 -> 
		Result = line_count(A)
	;
		( A - 1 >= 0 ->
			Result = line_count(A - 1)
		;
			Result = line_count(0)
		)
	).

subtract_size_count(line_count(A), line_count(B), _) = Result :-
	( A - B >= 0 ->
		Result = line_count(A - B)
	;
		Result = line_count(0)
	).

:- func maximum_size_count(size_count, measure_params) = int.

maximum_size_count(character_count(N), _) = Max :-
	% Each functor except the first takes a minimum of three chars.
	Max = (N + 2) div 3.

maximum_size_count(line_count(N), measure_params(LineWidth)) = Max :-
	% We can't assume any particular layout, so we go by how many
	% functors will fit in the area covered by N lines.  Each functor
	% except the first takes a minimum of three chars.
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
	( compare_size_count(A, B) = (>) ->
		Max = A
	;
		Max = B
	).

:- func zero_size_count = size_count.

zero_size_count = character_count(0).

	% We assume that all arguments have to be on separate lines, or 
	% the whole term should be printed on a single line.
:- pred size_count_split(univ::in, measure_params::in, size_count::in,
	int::in, bool::in, size_count::out, maybe(size_count)::out,
	size_count::out, measure_params::out) is det.

size_count_split(Univ, Params, Limit, Arity, Check, FunctorSize, 
		Flag, NewLimit, NewParams) :-
	% LineWidth is length of the line in which the functor is printed.
	Params = measure_params(LineWidth),
    	deconstruct(univ_value(Univ), Functor, ActualArity, Args),
    	FSize = string__length(Functor) + 2 * (ActualArity),
    	( Check = yes ->
    		get_arg_length(Args, TotalLength, MaxArgLength),
		int__max(MaxArgLength, (string__length(Functor) + 1), MaxLength)
    	;
    		TotalLength = 0,
		MaxLength = 0
    	), 
    	( 
		Arity = 0 
	->
		Flag = no,
    		FunctorSize = character_count(FSize),
		NewLimit = Limit,
		NewParams = Params
    	;
		(
			Limit = line_count(LineLimit),
			% we need one line for the functor and atleast 
			% one line for each argument
			LineLimit >= (Arity + 1),
			% linewidth is decreased by two characters to account 
			% for indentation
			(LineWidth - 2) >= MaxLength
		->
			Line = (LineLimit - 1) // Arity,
			Flag = yes(line_count(Line)),
			FunctorSize = line_count(1),
	    		NewLimit = Limit,
	    		NewParams = measure_params(LineWidth - 2)
		;
			Limit = line_count(LineLimit),
			LineLimit > 0,
			% Since every term is part of a bigger term (in this 
			% context anyway) it will have a comma, space and a 
			% newline at the end of it (Hence the "- 3").
			LineWidth - 3 >= (FSize + TotalLength) 
		->
	    		% "Arity - 1" is for rounding up.
			Char = (LineWidth - 3 - FSize + Arity - 1) // Arity ,
	    		Flag = yes(character_count(Char)),
	    		FunctorSize = character_count(FSize),
	    		NewLimit = character_count(LineWidth - 3),
	    		NewParams = Params
   		;
			Limit = character_count(CharLimit),
			CharLimit >= (FSize + TotalLength)
		->
	   		Char = (CharLimit - FSize + Arity - 1) // Arity,
	   		Flag = yes(character_count(Char)),
	   		FunctorSize = character_count(FSize),
	   		NewLimit = Limit,
	   		NewParams = Params
		;
	   		Flag = no,
			% If a term is not deconstructed, it is printed as
			% "functor/Arity". The "+ 2" accounts for that.
	   		FunctorSize = 
				character_count(string__length(Functor) + 2),
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
	pred(measured_split/9) is size_count_split
].

%---------------------------------------------------------------------------%
	% This predicate determines how many characters it will take
	% to print the functors of the arguments. Also determines the
	% length of biggest functor.
:- pred get_arg_length(list(univ)::in, int::out, int::out) is det.

get_arg_length([], 0, 0).
get_arg_length([HeadUniv | Rest], TotalLength, MaxLength) :-
	functor(univ_value(HeadUniv), Functor, Arity),
	( Rest = [] ->
		Correction = 2
	;
		Correction = 3
	),
	( Arity = 0 -> 
		Length = string__length(Functor)
	;
		% 2 is added because if a term has arguments then the
		% shortest way to print it is "functor/Arity"
		% Assuming Arity is a single digit
		Length = string__length(Functor) + 2
	),
	TotalLength = Length + RestTotalLength,
	int__max((Length + Correction), RestMaxLength, MaxLength),
	get_arg_length(Rest, RestTotalLength, RestMaxLength).

%---------------------------------------------------------------------------%
