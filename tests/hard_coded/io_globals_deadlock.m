%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test that io.update_globals/3 does not cause a deadlock in .par grades
% if the update closure throws an exception. The initial version of
% io.update_globals/3 did *not* do this.
%
%---------------------------------------------------------------------------%

:- module io_globals_deadlock.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module unit.
:- import_module univ.

main(!IO) :-
    io.set_globals(univ(3), !IO),
    try_io(update_pred_1, Result, !IO),
    (
        Result = succeeded(unit),
        io.write_string("update_pred_1 succeeded.\n", !IO)
    ;
        Result = exception(_),
        io.write_string("update_pred_1 threw exception.\n", !IO)
    ),

    % The following call to io.get_globals/3 will block if io.update_globals/3
    % fails to reset the lock after throwing an exception.
    io.get_globals(Globals, !IO),
    det_univ_to_type(Globals, FinalValue),
    io.format("Final value of Globals = %d\n", [i(FinalValue)], !IO).

:- pred update_pred_1(unit::out, io::di, io::uo) is det.

update_pred_1(unit, !IO) :-
    io.update_globals(update_1, !IO).

:- pred update_1(univ::in, univ::out) is det.

update_1(!Univ) :-
    ( if univ_to_type(!.Univ, N) then
        ( if N = 3 then
            throw("N = 3")
        else
            !:Univ = univ(561)
        )
    else
        throw("cannot convert univ to integer")
    ).
