%-----------------------------------------------------------------------------%

:- module version_hash_table_delete.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module version_hash_table.

%-----------------------------------------------------------------------------%

main(!IO) :-
    some [!HT] (
        !:HT = version_hash_table.init_default(generic_hash),
        list.foldl(fill, keys, !HT),
        list.foldl(version_hash_table.delete, keys, !HT),
        Residue = version_hash_table.to_assoc_list(!.HT),
        io.write(Residue, !IO),
        io.nl(!IO)
    ).

:- func keys = list(string).

keys =
    ["aback", "abaft", "abandon", "abandoned", "abandoning", "abandonment",
    "abandons", "abase", "abased", "abasement", "abasements", "abases"].

:- pred fill(string::in, version_hash_table(string, int)::in,
    version_hash_table(string, int)::out) is det.

fill(Key, !HT) :-
    version_hash_table.det_insert(Key, string.length(Key), !HT).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 et wm=0 tw=0
