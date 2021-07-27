%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for an abort during deforestation caused
% by det_analysis looking at an out-of-date map(var, type).

:- module deforest_rerun_det.

:- interface.

:- import_module int.

:- pred bug(int::in, int::in, int::out) is semidet.

:- implementation.

bug(Int1, Int2, Result) :-
    compare_int(Int1, Int2, Res),
    (
        Res = (<),
        Result = Int1
    ;
        Res = (=),
        fail
    ;
        Res = (>),
        Result = Int2
    ).

:- pred compare_int(int::in, int::in, comparison_result::out) is det.

compare_int(Int1, Int2, Res) :-
    ( if Int1 < Int2 then
        Res = (<)
    else if Int1 = Int2 then
        Res = (=)
    else
        Res = (>)
    ).
