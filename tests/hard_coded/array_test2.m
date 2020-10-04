%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module array_test2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Exercise each of the native types for the Java backend.
    test([10, 11, 12, 13, 14], !IO),
    test([10.0, 11.1, 12.2, 13.3, 14.4], !IO),
    test(['p', 'o', 'k', 'e', 'y'], !IO),
    test([yes, no, yes, yes, no], !IO),
    test(["ten", "eleven", "twelve", "thirteen", "fourteen"], !IO).

:- pred test(list(T)::in, io::di, io::uo) is det.

test(List, !IO) :-
    io.write_string("----\n", !IO),

    % Calls array.init, array.unsafe_insert_items.
    array.from_list(List, Array),

    % Calls array.bounds, array.fetch_items, array.foldr, array.elem,
    % array.lookup.
    array.to_list(Array, Elements),

    io.write_string("size: ", !IO),
    io.write_int(array.size(Array), !IO),
    io.nl(!IO),

    io.write_string("elements: ", !IO),
    io.write(Elements, !IO),
    io.nl(!IO),

    ( if semidet_lookup(Array, -1, _) then
        io.write_string("error: out of bounds lookup suceeded\n", !IO)
    else
        io.write_string("good: out of bounds lookup averted\n", !IO)
    ),
    ( if semidet_lookup(Array, list.length(List), _) then
        io.write_string("error: out of bounds lookup suceeded\n", !IO)
    else
        io.write_string("good: out of bounds lookup averted\n", !IO)
    ),

    Elem = list.det_head(List),

    array.copy(Array, ArrayB),
    ( if semidet_set(-1, Elem, Array, _) then
        io.write_string("error: out of bounds set succeeded\n", !IO)
    else
        io.write_string("good: out of bounds set averted\n", !IO)
    ),
    ( if semidet_set(5, Elem, ArrayB, _) then
        io.write_string("error: out of bounds set succeeded\n", !IO)
    else
        io.write_string("good: out of bounds set averted\n", !IO)
    ),

    some [!A] (
        array.from_list(List, !:A),
        array.resize(list.length(List), Elem, !A),
        io.write_string("resize without resizing: ", !IO),
        io.write(!.A, !IO),
        io.nl(!IO),

        array.resize(1 + list.length(List)//2, Elem, !A),
        io.write_string("shrink: ", !IO),
        io.write(!.A, !IO),
        io.nl(!IO),

        array.resize(list.length(List), Elem, !A),
        io.write_string("enlarge: ", !IO), % embiggen
        io.write(!.A, !IO),
        io.nl(!IO),

        array.resize(0, Elem, !A),
        io.write_string("empty: ", !IO),
        io.write(!.A, !IO),
        io.nl(!IO),

        array.resize(0, Elem, !A),
        io.write_string("still empty: ", !IO),
        io.write(!.A, !IO),
        io.nl(!IO),

        array.resize(3, Elem, !A),
        io.write_string("nonempty from empty: ", !IO),
        io.write(!.A, !IO),
        io.nl(!IO)
    ).
