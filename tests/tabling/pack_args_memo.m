%-----------------------------------------------------------------------------%

:- module pack_args_memo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type struct
    --->    struct(enum, enum, enum, enum).

:- type enum
    --->    aa ; bb ; cc ; dd.

%-----------------------------------------------------------------------------%

main(!IO) :-
    F = struct(aa, bb, cc, dd),
    copy(F, Fcopy),
    G1 = id(F),
    G2 = id(Fcopy),
    write(G1, !IO),
    nl(!IO),
    write(G2, !IO),
    nl(!IO).

:- func id(struct) = struct.
:- pragma memo(id/1).
:- pragma no_inline(id/1).

id(X) = X.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
