%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test structure sharing annotations on array operations.
%---------------------------------------------------------------------------%

:- module reuse_array.
:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.

:- type struct
    --->    struct(int, f :: int).

%---------------------------------------------------------------------------%

main(!IO) :-
    Struct1 = struct(1, dummy),
    impure addr(Struct1, Addr1),
    array.init(5, Struct1, Array0),
    % Structure reuse requires a deconstruction which it can consider the death
    % of a structure, so we oblige.
    unused(Struct1 ^ f),

    Struct2 = struct(2, dummy),
    impure addr(Struct2, Addr2),
    unused(Struct2 ^ f),

    % Struct1 should NOT be reused here, as it shares with Array0.
    % Struct2 can and should be reused instead.
    Struct3 = struct(3, dummy),
    impure addr(Struct3, Addr3),
    array.unsafe_set(3, Struct3, Array0, Array),
    unused(Struct3 ^ f),

    % Struct3 should NOT be reused for Struct4, as it is shared with Array.
    Struct4 = struct(4, dummy),
    impure addr(Struct4, Addr4),

    array.to_list(Array, List),
    io.write(List, !IO),
    io.nl(!IO),

    list.sort_and_remove_dups([Addr1, Addr2, Addr3, Addr4], Addrs),
    list.length(Addrs, NumAddrs),
    ( if
        NumAddrs = 4
    then
        io.write_string("4 distinct addresses - no reuse detected\n", !IO)
    else if
        NumAddrs = 3,
        Addr2 = Addr3
    then
        io.write_string("3 distinct addresses - reuse detected\n", !IO)
    else
        io.write_string("unexpected addresses: ", !IO),
        io.write(Addrs, !IO),
        io.nl(!IO)
    ).

:- impure pred addr(T::ui, int::uo) is cc_multi.
:- pragma foreign_proc("C",
    addr(T::ui, Ptr::uo),
    [will_not_call_mercury, thread_safe, no_sharing],
"
    Ptr = (MR_Integer) T;
").

    % This is used to force structures to be constructed dynamically.
:- func dummy = (int::uo).
:- pragma no_inline(dummy/0).

dummy = 999.

:- pred unused(int::unused) is det.

unused(_).
