%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A type_info cell was not constructed statically by var_locn.m.
% It caused a compiler abort because that differed from the
% construct_statically hint set by mark_static_terms.m.
%
%---------------------------------------------------------------------------%

:- module bug457.
:- interface.

:- import_module io.

:- pred wrapper(pred(io, io)::in(pred(di, uo) is cc_multi),
    io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.

wrapper(Pred, !IO) :-
    Closure =
        ( pred({}::out, IO0::di, IO::uo) is cc_multi :-
            Pred(IO0, IO)
        ),
    try_io(Closure, _TryResult, !IO).

%---------------------------------------------------------------------------%
