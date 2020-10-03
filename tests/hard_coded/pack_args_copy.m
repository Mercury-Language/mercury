%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack_args_copy.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type enum    --->    aa ; bb ; cc ; dd.
:- type enum(T) --->    aa ; bb ; cc ; dd.

:- type struct(T)
    --->    struct(enum(T), enum, T, enum(T), enum(T)).

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = struct(aa, bb, "string", cc, dd),
    copy(S0, S),
    io.write_line(S0, !IO),
    io.write_line(S, !IO).
