%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test array.all_{true, false}/2.

:- module array_all_tf.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.

main(!IO) :-
    EvensArray = array([2, 4, 6, 8, 10]),
    io.write_string("TEST: all_true(even, ", !IO),
    io.write(EvensArray, !IO),
    io.write_string("): ", !IO),
    ( if array.all_true(int.even, EvensArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ),

    io.write_string("TEST: all_false(odd, ", !IO),
    io.write(EvensArray, !IO),
    io.write_string("): ", !IO),
    ( if array.all_false(int.odd, EvensArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ),

    array.make_empty_array(EmptyArray : array(int)),
    io.write_string("TEST: all_true(even, ", !IO),
    io.write(EmptyArray, !IO),
    io.write_string("): ", !IO),
    ( if array.all_true(int.even, EmptyArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ),

    io.write_string("TEST: all_false(even, ", !IO),
    io.write(EmptyArray, !IO),
    io.write_string("): ", !IO),
    ( if array.all_false(int.even, EmptyArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ),

    io.write_string("TEST: not all_true(odd, ", !IO),
    io.write(EvensArray, !IO),
    io.write_string("): ", !IO),
    ( if not array.all_true(int.odd, EvensArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ),

    io.write_string("TEST: not all_false(even, ", !IO),
    io.write(EvensArray, !IO),
    io.write_string("): ", !IO),
    ( if not array.all_false(int.even, EvensArray)
    then io.write_string("PASSED\n", !IO)
    else io.write_string("FAILED\n", !IO)
    ).
