% 
% Regression test.
%
% Name: agc_ite.m
%
% Description of bug:
% 	The liveness of polymorphic type variables for accurate gc
% 	wasn't being computed correctly in the case of if-then-else.
%
% Symptom(s) of bug:
% 	A sanity check in liveness.m is failed.
%
% 		Software error: branches of if-then-else disagree on liveness
% 		Then: HeadVar__3 TypeInfo_for_Y 
%	 	Else: HeadVar__3 TypeInfo_for_X TypeInfo_for_Y 
%
%
% Date bug existed: 19-June-1997
%
% Author: trd

:- module agc_ite.

:- interface.

:- import_module list.

:- pred agc_ite__filter_map(pred(X, Y), list(X), list(Y)).
:- mode agc_ite__filter_map(pred(in, out) is semidet, in, out) is det.

:- implementation.
	
agc_ite__filter_map(_, [],  []).
agc_ite__filter_map(Pred, [Head0|Tail0], List) :-
	( call(Pred, Head0, Head) ->
		List = [Head|List1]
	;
		List = List1
	),
	agc_ite__filter_map(Pred, Tail0, List1).


