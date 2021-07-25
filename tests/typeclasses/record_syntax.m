%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module record_syntax.

:- interface.

:- import_module char.
:- import_module io.
:- import_module string.

:- pred main(io::di, io::uo) is det.

:- type foo
    --->    foo(
                arg1 :: int,
                arg2 :: int,
                arg3 :: foo2
            ).

:- type foo2
    --->    foo2(
                arg4 :: int,
                arg5 :: int
            ).

:- type my_list(T)
    --->    cons(data :: T, next :: my_list(T))
    ;       nil.

:- type e_list
    --->    some [T] e_cons(e_data :: T, e_next :: e_list) => has_size(T)
    ;       e_nil.

:- type type_and_size(T)
    --->    type_and_size(
                type_and_size_data  :: T,
                data_size           :: int
            ).

:- typeclass has_size(T) where [
    func size(T) = int
].

:- instance has_size(int).
:- instance has_size(string).
:- instance has_size(char).
:- instance has_size(type_and_size(T)).

:- type my_pair(T, U).

:- func fst(my_pair(T, U)) = T.
:- func snd(my_pair(T, U)) = U.
:- func 'fst :='(my_pair(T, U), V) = my_pair(V, U).
:- func 'snd :='(my_pair(T, U), V) = my_pair(T, V).

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module std_util.

:- type my_pair(T, U)
    --->    (fst::T) - (snd::U).

main -->
    { X = foo(1, 2, foo2(3, 4)) },
    write_arg("X ^ arg1", X ^ arg1),
    write_arg("X ^ arg2", X ^ arg2),
    write_arg("X ^ arg3", X ^ arg3),
    write_arg("X ^ arg3 ^ arg4", X ^ arg3 ^ arg4),

    write_arg("updated arg1", X ^ arg1 := 5),
    write_arg("updated arg2", X ^ arg2 := 6),
    write_arg("updated arg3 ^ arg4", X ^ arg3 ^ arg4 := 7),

    { List0 = cons(1, cons(2, cons(3, nil))) },
    { List1 = List0 ^ next ^ next ^ data := 4 },
    write_arg("List1", List1),

    % Test updates of existentially typed fields.
    { List2 = 'new e_cons'(1, 'new e_cons'('b', e_nil)) },
    write_arg("List2 ^ e_next ^ e_data", List2 ^ e_next ^ e_data),

    { List3 = List2 ^ e_next ^ e_data := "new value" },
    write_arg("List3", List3),
    { NewValue = List3 ^ e_next ^ e_data },
    write_arg("List3 ^ e_next ^ e_data", NewValue),
    write_arg("size(List3 ^ e_next ^ e_data)",
        size(NewValue)),

    % Test updates that return a term of a different type to the input.
    { Pair0 = 1 - 2 },
    write_arg("Pair0 ^ fst", Pair0 ^ fst),
    { Pair = Pair0 ^ fst := "new first elem" },
    write_arg("Pair", Pair),

    { TypeAndSize = type_and_size("string", 6) },
    io.write_string("size of `type_and_size(""string"", 6)' = "),
    io.write_int(size(TypeAndSize)),
    io.nl,

    % Test taking the address of an update function
    % for which a mode declaration has been supplied.
    { Pairs = list.map('fst :='(Pair), [4, 5, 6]) },
    write_arg("'fst :=' [4, 5, 6]", Pairs),

    =(IO0),
    { dcg_syntax(IO0, IO, X, _) },
    :=(IO),

    { Map0 = map.from_assoc_list(['a' - "a", 'b' - "b", 'd' - "D"]) },
    write_arg("Map0 ^ det_elem('a')", Map0 ^ det_elem('a')),
    { Map = Map0 ^ elem('c') := "c" },
    write_arg("Map ^ det_elem('c')", Map ^ det_elem('c')).

:- instance has_size(int) where [
    func(size/1) is id
].

:- instance has_size(string) where [
    func(size/1) is string.length
].

:- instance has_size(char) where [
    func(size/1) is char.to_int
].

:- instance has_size(type_and_size(T)) where [
    func(size/1) is data_size
].

:- pred dcg_syntax(io::di, io::uo, foo::in, foo::out) is det.

dcg_syntax(IO0, IO) -->
    Arg1 =^ arg1,
    { write_arg("DCG ^ arg1", Arg1, IO0, IO1) },
    Arg4 =^ arg3 ^ arg4,
    { write_arg("DCG ^ arg3 ^ arg4", Arg4, IO1, IO2) },

    ^ arg1 := 8,
    =(DCG1),
    { write_arg("updated DCG arg1", DCG1, IO2, IO3) },

    ^ arg3 ^ arg4 := 9,
    =(DCG2),
    { write_arg("updated DCG arg3 ^ arg4", DCG2, IO3, IO) }.

:- pred write_arg(string::in, T::in, io::di, io::uo) is det.

write_arg(Descr, Arg, !IO) :-
    io.write_string(Descr, !IO),
    io.write_string(" = ", !IO),
    io.write_line(Arg, !IO).
