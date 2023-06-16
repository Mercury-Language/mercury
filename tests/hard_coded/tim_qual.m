%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests module qualification of types, insts and modes.
% Should get syntax errors if the qualified types, insts and modes are
% not correctly parsed, module qualification errors if the correct match
% cannot be determined or type or determinism errors if the wrong type
% or mode is chosen.

:- module tim_qual.
:- interface.

:- import_module io.
:- import_module tim_qual_helper_1.
:- import_module tim_qual_helper_2.

:- pred main(io.state::di, io.state::uo) is det.

:- pred test(tim_qual_helper_1.test_type::tim_qual_helper_2.test_mode) is det.

:- pred test2(tim_qual_helper_1.test_type::test_mode2) is det.

:- mode test_mode2 == tim_qual_helper_1.inst1 >> tim_qual_helper_2.inst1.

:- implementation.

main(!IO) :-
    ( if test(ok), test2(ok) then
        io.write_string("ok\n", !IO)
    else
        io.write_string("error\n", !IO)
    ).

test2(ok).

test(ok).
