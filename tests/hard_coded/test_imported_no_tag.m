%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test handling of no_tag types without inlining.

:- module test_imported_no_tag.
:- interface.

:- import_module imported_no_tag.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pred test_int(int::in) is semidet.

test_int(99).

main(!IO) :-
    ( if pwrite(class(test_int), 99) then
        io.write_string("ok\n", !IO)
    else
        io.write_string("uh oh\n", !IO)
    ).

