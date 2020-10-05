%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module hash_init_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module exception.
:- import_module hash_table.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    HashPred =
        ( pred(Name::in, Hash::out) is det :-
            sym_name_to_string(Name, ".", Str),
            Hash = hash(Str)
        ),
    HT0 = init(HashPred, 10, 0.8),
    build_table(entries, HT0, HT),
    ( if
        hash_table.search(HT,
            qualified(unqualified("io"), "read_word"), _)
    then
        io.write_string("error: search succeeded\n", !IO)
    else
        io.write_string("search failed as expected\n", !IO)
    ).

:- pred build_table(list(sym_name)::in, name_ht::hash_table_di,
    name_ht::hash_table_uo) is det.

build_table([], HT, HT).
build_table([K | T], HT0, HT) :-
    build_table(T, det_insert(HT0, K, 1), HT).

:- type name_ht == hash_table(sym_name, int).

:- type sym_name
    --->    unqualified(string)
    ;       qualified(sym_name, string).

    % sym_name_to_string(SymName, Separator, String):
    %
    % convert a symbol name to a string,
    % with module qualifiers separated by Separator.
    %
:- pred sym_name_to_string(sym_name, string, string).
:- mode sym_name_to_string(in, in, out) is det.

sym_name_to_string(SymName, Separator, String) :-
    sym_name_to_string_2(SymName, Separator, Parts, []),
    string.append_list(Parts, String).

:- pred sym_name_to_string_2(sym_name, string,
    list(string), list(string)).
:- mode sym_name_to_string_2(in, in, out, in) is det.

sym_name_to_string_2(qualified(ModuleSpec, Name), Separator) -->
    sym_name_to_string_2(ModuleSpec, Separator),
    [Separator, Name].
sym_name_to_string_2(unqualified(Name), _) -->
    [Name].

:- func entries = list(sym_name).

entries = [
    unqualified("main")
].
