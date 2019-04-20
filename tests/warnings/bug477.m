%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for infinite recursion through a try wrapper.
%
%---------------------------------------------------------------------------%

:- module bug477.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module string.

:- type logger
    --->    logger.
:- type jscenario
    --->    jscenario.
:- type jsolution
    --->    jsolution.

main(!IO) :-
    reconstruct_route(logger, jscenario, Solution),
    io.print_line(Solution, !IO).

:- pred reconstruct_route(logger::in, jscenario::in, jsolution::out)
    is cc_multi.

reconstruct_route(Log, JScenario, JSolution) :-
    ( try []
        reconstruct_route(Log, JScenario, JSolution0)
    then
        JSolution = JSolution0
    catch_any Excp ->
        trace [io(!IO)] (
            Msg = "Exception during route reconstruction: " ++ string(Excp),
            error(Log, Msg, !IO)
        ),
        JSolution = jsolution
    ).

:- pred error(logger::in, string::in, io::di, io::uo) is det.

error(_, _, !IO).
