:- module typeclass_inlining_bug.

:- interface.

:- import_module list.

:- type analysis
	--->	some [Call, Answer] analysis(Call, Answer)
			=> analysis(Call, Answer).

:- typeclass analysis(Call, Answer) <=
		(call_pattern(Call), answer_pattern(Answer))
			where
[].

:- type analysis_name == string.

:- typeclass call_pattern(Call) where [].

:- typeclass answer_pattern(Answer) where [].

:- type analysis_info.

:- pred lookup_call_pattern(Call::in, list(analysis)::in,
	list(Answer)::out) is det <= analysis(Call, Answer).

:- implementation.

:- import_module map, require, set, std_util.

lookup_call_pattern(CallPattern, Results, AnswerPatterns) :-
	AnswerPatterns = list__filter_map(filter_results(CallPattern),
			Results).

:- func filter_results(Call, analysis) = Answer is semidet
		<= (call_pattern(Call), answer_pattern(Answer)).

filter_results(_, analysis(_, Answer0)) = Answer :-
	univ(Answer0) = univ(Answer).

