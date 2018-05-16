%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type struct
    --->    struct(int32, int32, int32, int32).

%---------------------------------------------------------------------------%

main(!IO) :-
    Static = struct(-2_147_483_648_i32, 2_147_483_647_i32,
        -32_768_i32, 32_767_i32),
    write_struct(Static, !IO),

    Dynamic = struct(id(-2_147_483_648_i32), id(2_147_483_647_i32),
        id(-32_768_i32), id(32_767_i32)),
    write_struct(Dynamic, !IO),

    Dynamic2 = struct(id(-2_147_483_648_i32), _, _, _),
    Dynamic2 = struct(_, id(2_147_483_647_i32), _, _),
    Dynamic2 = struct(_, _, id(-32_768_i32), _),
    Dynamic2 = struct(_, _, _, id(32_767_i32)),
    write_struct(Dynamic2, !IO).

:- func id(int32) = int32.
:- pragma no_inline(id/1).

id(X) = X.

:- pred write_struct(struct::in, io::di, io::uo) is det.
:- pragma no_inline(write_struct/3).

write_struct(struct(A, B, C, D), !IO) :-
    io.write_string("struct(", !IO),
    io.write_int32(A, !IO), io.write_string(", ", !IO),
    io.write_int32(B, !IO), io.write_string(", ", !IO),
    io.write_int32(C, !IO), io.write_string(", ", !IO),
    io.write_int32(D, !IO),
    io.write_string(")\n", !IO).
