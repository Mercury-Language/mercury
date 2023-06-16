%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_try_goal_helper_1.
:- interface.

:- import_module io.

:- pred catcher(pred(io, io), io, io).
:- mode catcher(pred(di, uo) is det, di, uo) is cc_multi.

:- implementation.

:- import_module string.

    % intermod.m must ignore this pragma until `try' goals can be written
    % properly to .opt files.
    %
:- pragma inline(catcher/3).

catcher(Pred, !IO) :-
    ( try [io(!IO)]
        Pred(!IO)
    then
        true
    catch_any Excp ->
        io.write_string("caught exception: " ++ string(Excp), !IO),
        io.nl(!IO)
    ).
