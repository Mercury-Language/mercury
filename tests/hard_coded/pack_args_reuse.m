%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check structure reuse takes into account argument packing.
%

:- module pack_args_reuse.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type struct
    --->    struct(enum, enum, enum, string, enum, enum). % 3 words

:- inst uniq_struct
    ==      unique(struct(ground, ground, ground, ground, ground, ground)).

:- type enum
    --->    aa ; bb ; cc ; dd ; ee ; xx.

%---------------------------------------------------------------------------%

main(!IO) :-
    T0 = struct(aa, bb, cc, str, dd, ee),
    update2(xx, T0, T),
    io.write_line(T, !IO).

:- pred update2(enum::in, struct::di(uniq_struct), struct::out(uniq_struct))
    is det.
:- pragma no_inline(update2/3).

update2(X, T0, T) :-
    T0 = struct(A, _, C, D, E, F),
    % field 0 (A, X, C)             needs update
    % field 1 (D)                   does not need update
    % field 2 (E, F)                does not need update
    T = struct(A, X, C, D, E, F).

:- func str = string.
:- pragma no_inline(str/0).

str = "str".
