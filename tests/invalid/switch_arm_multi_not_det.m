%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test. Versions of the compiler before 2016 may 13
% did not generate an error message for the violation of the requirement
% imposed by the require_switch_arms_detism scope below, because the error
% it was meant to detect imposed a commit scope between the
% require_switch_arms_det scope and the switch itself.

:- module switch_arm_multi_not_det.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if test_switch_arms("c") then
        io.write_string("Success\n", !IO)
    else
        io.write_string("Failure\n", !IO)
    ).

:- pred test_switch_arms(string::in) is semidet.

test_switch_arms(Name) :-
    require_switch_arms_det [Name]
    ( Name = "a"
    ; Name = "b"
    ; Name = "c"
    ; Name = "c"
    ; Name = "d"
    ; Name = "d"
    ).
