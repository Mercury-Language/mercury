%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module test_generic_ref.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module deconstruct.
:- import_module float.
:- import_module int.
:- import_module int64.
:- import_module store.
:- import_module uint64.

%---------------------------------------------------------------------------%

main(!IO) :-
    do_test(test1_orig, test1_expected, !IO),
    io.nl(!IO),
    do_test(test2_orig, test2_expected, !IO),
    io.nl(!IO),
    do_test(test3_orig, test3_expected, !IO),
    io.nl(!IO),
    do_test(test4_orig, test4_expected, !IO),
    io.nl(!IO),
    do_test(test5_orig, test5_expected, !IO),
    io.nl(!IO),
    do_test(test6_orig, test6_expected, !IO),
    io.nl(!IO),
    do_test(test7_orig, test7_expected, !IO),
    io.nl(!IO),
    do_test(test8_orig, test8_expected, !IO),
    io.nl(!IO),
    do_test(test9_orig, test9_expected, !IO).

%---------------------------------------------------------------------------%

    % do_test(TermA, TermB, !IO):
    % If TermA and TermB have the same top-level functor then place a copy
    % of TermA in a generic_ref and then individually update each of its
    % fields using arg_ref and set_ref_value so they match those of TermB.
    % When the term is extracted from the generic ref it should match TermB.
    %
:- pred do_test(T::in, T::in, io::di, io::uo) is det.

do_test(OrigTerm, ExpectedTerm, !IO) :-
    io.print("   Orig. Term: ", !IO),
    io.print_line(OrigTerm, !IO),
    % Note that 'canonicalize' is what ref_functor/4 does.
    deconstruct.functor(ExpectedTerm, canonicalize, ExpectedFunctor,
        ExpectedArity),
    some [!Store] (
        copy(OrigTerm, UniqTerm),
        store.init(!:Store),
        store.new_ref(UniqTerm, Ref, !Store),
        store.ref_functor(Ref, Functor, Arity, !Store),
        ( if
            ExpectedFunctor = Functor,
            ExpectedArity = Arity
        then
            int.fold_up(update_arg(Ref, ExpectedTerm), 0, Arity - 1, !Store),
            store.extract_ref_value(!.Store, Ref, FinalTerm),
            io.print(" Updated Term: ", !IO),
            io.print_line(FinalTerm, !IO),
            io.print("Expected Term: ", !IO),
            io.print_line(ExpectedTerm, !IO),
            ( if FinalTerm \= ExpectedTerm  then
                io.print_line("*** MATCH FAILED ***", !IO)
            else
                true
            )
        else
            io.print_line("Cannot update - terms have different functors.", !IO)
        )
    ).

:- pred update_arg(generic_ref(T, S)::in, T::in, int::in, S::di, S::uo)
    is det <= store(S).

update_arg(Ref, ExpectedTerm, N, !Store) :-
    deconstruct.det_arg(ExpectedTerm, do_not_allow, N, Value),
    store.arg_ref(Ref, N, ArgNRef, !Store),
    copy(Value, UniqValue),
    store.set_ref_value(ArgNRef, UniqValue, !Store).

%---------------------------------------------------------------------------%

:- type test1_struct
    --->    test1_struct(int).

:- func test1_orig = test1_struct.

test1_orig = test1_struct(561).

:- func test1_expected = test1_struct.

test1_expected = test1_struct(41041).

%---------------------------------------------------------------------------%

:- type test2_struct --->
    test2_struct(int, int, int).

:- func test2_orig = test2_struct.

test2_orig = test2_struct(111, 111, 111).

:- func test2_expected = test2_struct.

test2_expected = test2_struct(222, 222, 222).

%---------------------------------------------------------------------------%

:- type test3_struct
    --->   test3_struct(bool, bool, bool, uint16).

:- func test3_orig = test3_struct.

test3_orig = test3_struct(yes, yes, yes, 0xffffu16).

:- func test3_expected = test3_struct.

test3_expected = test3_struct(no, no, no, 0x0000u16).

%---------------------------------------------------------------------------%

:- type animal
    --->    ant
    ;       bat
    ;       cat
    ;       dog
    ;       eel
    ;       fox
    ;       gnu
    ;       hog
    ;       ibis
    ;       jay
    ;       kea
    ;       lark
    ;       moa
    ;       newt
    ;       owl
    ;       pug.

:- type test4_struct
    --->    test4_struct(
                animal, animal,
                int,
                string,
                animal, animal, animal, animal
            ).

:- func test4_orig = test4_struct.

test4_orig = test4_struct(bat, bat, 561, "Hello", owl, owl, owl, owl).

:- func test4_expected = test4_struct.

test4_expected = test4_struct(fox, fox, 41041, "World", hog, hog, hog, hog).

%---------------------------------------------------------------------------%

:- type dummy
    --->    dummy.

:- type nested_dummy
    --->    nested_dummy(dummy).

:- type test5_struct
    --->    test5_struct(int, dummy, nested_dummy, string, bool, bool).

:- func test5_orig = test5_struct.

test5_orig =
    test5_struct(561, dummy, nested_dummy(dummy), "Hello", yes, yes).

:- func test5_expected = test5_struct.

test5_expected =
    test5_struct(41041, dummy, nested_dummy(dummy), "World", no, no).

%---------------------------------------------------------------------------%

:- type test6_struct
    --->    test6_struct(uint8, uint8, dummy, uint16).

:- func test6_orig = test6_struct.

test6_orig =
    test6_struct(0xffu8, 0xffu8, dummy, 0xffffu16).

:- func test6_expected = test6_struct.

test6_expected =
    test6_struct(0x1u8, 0x1u8, dummy, 0x1u16).

%---------------------------------------------------------------------------%
%
% Tests 7-8.

:- type packed(T)
    --->    packed_1(int8, uint8, dummy, dummy, int8, uint8, T)
    ;       some [U] packed_2(dummy, U, int8, uint8, dummy, int8, uint8)
    ;       packed_3(int)
    ;       packed_4(int)
    ;       packed_5(int)
    ;       packed_6(int)
    ;       packed_7(int)
    ;       packed_8(T, dummy, int8, uint8, dummy, dummy, int8, uint8,
                float, int)
    ;       some [U] packed_9(T, dummy, U, int8, uint8, dummy, int8, uint8).

:- func test7_orig = packed(float).

test7_orig =
    'new packed_2'(dummy, 561.0, 1i8, 1u8, dummy, 2i8, 3u8).

:- func test7_expected = packed(float).

test7_expected =
    'new packed_2'(dummy, 41041.0, 127i8, 255u8, dummy, 127i8, 255u8).

:- func test8_orig = packed(char).

test8_orig = packed_1(1i8, 1u8, dummy, dummy, 0i8, 0u8, 'A').

:- func test8_expected = packed(char).

test8_expected = packed_1(127i8, 255u8, dummy, dummy, 127i8, 255u8, 'Z').

%---------------------------------------------------------------------------%

:- type test9_struct
    --->    test9_struct(int64, uint64, float, uint64).

:- func test9_orig = test9_struct.

test9_orig = test9_struct(int64.max_int64, uint64.max_uint64, float.infinity,
    uint64.max_uint64).

:- func test9_expected = test9_struct.

test9_expected = test9_struct(1i64, 1u64, -1.0, 1u64).

%---------------------------------------------------------------------------%
