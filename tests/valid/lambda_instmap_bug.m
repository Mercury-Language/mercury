%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for a case where cse_detection.m, det_analysis.m
% and simplify.m were not updating the instmap before processing
% a lambda expression, causing an abort in determinism analysis.

:- module lambda_instmap_bug.

:- interface.

:- import_module list.
:- import_module set.
:- import_module pair.

:- type instr == pair(instruction, string).

:- type instruction
    --->    clear(int)
    ;       drop(int)
    ;       join(int, int, int).

:- pred detect_streams(set(int)::in, list(instr)::in, list(instr)::out)
    is det.

:- implementation.

detect_streams(Streams, Instrs0, Instrs) :-
    % Don't attempt to clear or drop streams.
    IsNotStreamClear =
        ( pred(Instr::in) is semidet :-
            not (
                ( Instr = clear(Rel) - _
                ; Instr = drop(Rel) - _
                ),
                set.member(Rel, Streams)
            )
        ),
    list.filter(IsNotStreamClear, Instrs0, Instrs).
