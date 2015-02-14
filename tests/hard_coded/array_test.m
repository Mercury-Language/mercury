%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module array_test.

:- interface.

:- import_module array.
:- import_module bool.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- pred test(array(bool)::in(uniq_array(bound(yes))),
    array(bool)::in(uniq_array(bound(yes)))) is semidet.

:- pred test2(array(bool)::in(uniq_array(bound(no))),
    array(bool)::in(uniq_array(bound(yes)))) is semidet.

:- implementation.

main(!IO) :-
    io.write_string("succeeded\n", !IO).

test(X, X).

test2(X, X).
