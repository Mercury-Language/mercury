%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% code_table4.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Nov  9 15:01:55 GMT 2000
%---------------------------------------------------------------------------%

:- module code_table4.

:- interface.

:- import_module util, array.

:- type code_table.
:- type hash.
:- type key.

:- func key(code, byte) = key.

:- func new_code_table = code_table.
:- mode new_code_table = array_uo is det.

:- pred lookup(code, byte, code_table, hash, key, code).
:- mode lookup(in, in, array_ui, out, out, out) is det.

:- func set(code_table, hash, key, code) = code_table.
:- mode set(array_di, in, in, in) = array_uo is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.

:- type hash == int.

:- type key == int.

:- type code_table == array(int).

:- func table_size = int.
table_size = 69001.

:- func array_size = int.
array_size = table_size + table_size.

:- func key_idx(int) = int.
key_idx(I) = I.

:- func code_idx(int) = int.
code_idx(I) = I + table_size.

:- func hash(code, byte) = hash.
hash(C, B) = C `xor` (B `lshift` 8).

:- func hash_delta(hash) = int.
hash_delta(H) = ( if H = 0 then 1 else table_size - H ).

%---------------------------------------------------------------------------%

key(C, B) = (C `lshift` 8) \/ B.

%---------------------------------------------------------------------------%

new_code_table = array.init(array_size, empty_code).

%---------------------------------------------------------------------------%

lookup(C, B, T, H, K, Code) :-
    H0 = hash(C, B),
    K  = key(C, B),
    K0 = array.lookup(T, key_idx(H0)),
    ( if (K0 = K ; K0 = empty_code) then
        H = H0,
        Code = array.lookup(T, code_idx(H0))
    else
        lookup_0(H0, hash_delta(H0), K, T, H, Code)
    ).

%---------------------------------------------------------------------------%

:- pred lookup_0(hash, int, key, code_table, hash, code).
:- mode lookup_0(in, in, in, array_ui, out, out) is det.

lookup_0(H0, DH, K, T, H, Code) :-
    H1 = H0 - DH + ( if DH =< H0 then 0 else table_size ),
    K0 = array.lookup(T, key_idx(H1)),
    ( if (K0 = K ; K0 = empty_code) then
        H = H1,
        Code = array.lookup(T, code_idx(H1))
    else
        lookup_0(H1, DH, K, T, H, Code)
    ).

%---------------------------------------------------------------------------%

set(T0, H, K, C) = T :-
    T1 = array.set(T0, key_idx(H), K),
    T  = array.set(T1, code_idx(H), C).

%---------------------------------------------------------------------------%
