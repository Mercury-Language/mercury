% Regression test for a case where cse_detection.m, det_analysis.m 
% and simplify.m were not updating the instmap before processing
% a lambda expression, causing an abort in determinism analysis.
:- module lambda_instmap_bug.

:- interface.

:- import_module list, set, std_util.

:- type instr == pair(instruction, string).

:- type instruction
	--->	clear(int)
	;	drop(int)
	;	join(int, int, int).

:- pred detect_streams(set(int), list(instr), list(instr)).
:- mode detect_streams(in, in, out) is det.

:- implementation.

detect_streams(Streams, Instrs0, Instrs) :-

	% Don't attempt to clear or drop streams.
	IsNotStreamClear = 
		(pred(Instr::in) is semidet :-
			\+ (
				( Instr = clear(Rel) - _
				; Instr = drop(Rel) - _
				),
				set__member(Rel, Streams)
			)
		),
	list__filter(IsNotStreamClear, Instrs0, Instrs).
