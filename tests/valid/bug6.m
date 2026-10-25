%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% This regression test is for Mantis bug #6.
% This program caused the following assertion failure in the LLDS backend:
%
% Software Error: jumpopt.m: Unexpected: adjust_livevals:
%      BetweenLivevals and PrevLivevals differ
%
% This is a cut-down version of the original program, which can be found
% in bug6_full.m.
%
% The minimum set of optimization options required to trigger the bug
% was -O0 --optimize-frames --optimize-repeat=2.
%
%---------------------------------------------------------------------------%

:- module bug6.
:- interface.

:- import_module list.
:- import_module io.

:- type tree
    --->    l
    ;       br(tree, tree).

:- pred write_dnas(list(list(tree))::in, io::di, io::uo) is det.

:- implementation.

write_dnas(D0, !IO) :-
    ( dna_head_tail(D0, D) ->
        write_dnas(D, !IO)
    ;
       true
    ).

:- pred dna_head_tail(list(list(tree))::in, list(list(tree))::out)
    is semidet.

dna_head_tail([[] | Fs0], Fs) :-
    dna_head_tail(Fs0, Fs).
