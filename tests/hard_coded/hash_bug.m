%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module hash_bug.

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
    build_table(T, set(HT0, K, 1), HT).

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
    unqualified("main"),
    qualified(unqualified("hash_bug"), "main"),
    unqualified("build_table"),
    qualified(unqualified("hash_bug"), "build_table"),
    unqualified("sym_name_to_string"),
    qualified(unqualified("hash_bug"), "sym_name_to_string"),
    unqualified("sym_name_to_string_2"),
    qualified(unqualified("hash_bug"), "sym_name_to_string_2"),
    unqualified("copy"),
    qualified(unqualified("builtin"), "copy"),
    unqualified("unsafe_promise_unique"),
    qualified(unqualified("builtin"), "unsafe_promise_unique"),
    unqualified("unsafe_promise_unique"),
    qualified(unqualified("builtin"), "unsafe_promise_unique"),
    unqualified("unsafe_promise_unique"),
    qualified(unqualified("builtin"), "unsafe_promise_unique"),
    unqualified("false"),
    qualified(unqualified("builtin"), "false"),
    unqualified("promise_only_solution"),
    qualified(unqualified("builtin"), "promise_only_solution"),
    unqualified("promise_only_solution"),
    qualified(unqualified("builtin"), "promise_only_solution"),
    unqualified("promise_only_solution"),
    qualified(unqualified("builtin"), "promise_only_solution"),
    unqualified("promise_only_solution"),
    qualified(unqualified("builtin"), "promise_only_solution"),
    unqualified("promise_only_solution"),
    qualified(unqualified("builtin"), "promise_only_solution"),
    unqualified("promise_only_solution_io"),
    qualified(unqualified("builtin"), "promise_only_solution_io"),
    unqualified("unify"),
    qualified(unqualified("builtin"), "unify"),
    unqualified("compare"),
    qualified(unqualified("builtin"), "compare"),
    unqualified("ordering"),
    qualified(unqualified("builtin"), "ordering"),
    unqualified("@<"),
    qualified(unqualified("builtin"), "@<"),
    unqualified("@=<"),
    qualified(unqualified("builtin"), "@=<"),
    unqualified("@>"),
    qualified(unqualified("builtin"), "@>"),
    unqualified("@>="),
    qualified(unqualified("builtin"), "@>="),
    unqualified("get_one_solution"),
    qualified(unqualified("builtin"), "get_one_solution"),
    unqualified("get_one_solution"),
    qualified(unqualified("builtin"), "get_one_solution"),
    unqualified("get_one_solution"),
    qualified(unqualified("builtin"), "get_one_solution"),
    unqualified("get_one_solution_io"),
    qualified(unqualified("builtin"), "get_one_solution_io"),
    unqualified("compare_representation"),
    qualified(unqualified("builtin"), "compare_representation"),
    unqualified("call_rtti_generic_unify"),
    qualified(unqualified("builtin"), "call_rtti_generic_unify"),
    unqualified("call_rtti_generic_compare"),
    qualified(unqualified("builtin"), "call_rtti_generic_compare"),
    unqualified("read_char"),
    qualified(unqualified("io"), "read_char")
].
