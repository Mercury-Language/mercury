%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module hash_table_delete.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hash_table.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    some [!HT] (
        !:HT = hash_table.init_default(string.hash),
        myfoldl(fill, keys, !HT),
        myfoldl(hash_table.delete, keys, !HT),
        Residue = hash_table.to_assoc_list(!.HT),
        io.write(Residue, !IO),
        io.nl(!IO)
    ).

:- pred myfoldl(pred(T, A, A), list(T), A, A).
:- mode myfoldl(in(pred(in, hash_table_di, hash_table_uo) is det), in,
    hash_table_di, hash_table_uo) is det.

myfoldl(_, [], !HT).
myfoldl(P, [T | Ts], !HT) :-
    P(T, !HT),
    myfoldl(P, Ts, !HT).

:- func keys = list(string).

keys =
    ["aback", "abaft", "abandon", "abandoned", "abandoning", "abandonment",
    "abandons", "abase", "abased", "abasement", "abasements", "abases"].

:- pred fill(string::in, hash_table(string, int)::hash_table_di,
    hash_table(string, int)::hash_table_uo) is det.

fill(Key, !HT) :-
    hash_table.det_insert(Key, string.length(Key), !HT).
