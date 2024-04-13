%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the suppression of error messages like this:
%
% det_errors_and_io.m:038: In `bad_pred'(in, di, uo):
% det_errors_and_io.m:038:   error: `semidet' is not a valid determinism for a
% det_errors_and_io.m:038:   predicate that has I/O state arguments. The valid
% det_errors_and_io.m:038:   determinisms for such predicates are `det',
% det_errors_and_io.m:038:   `cc_multi' and `erroneous', since the I/O state
% det_errors_and_io.m:038:   can be neither duplicated nor destroyed.
%
% Given that the predicate is declared to be det, the actual bug is that
% its actual determinism is semidet (due to the missing arm of the switch),
% NOT any misunderstanding by the programmer about what determinisms are
% compatible with I/O.
%

:- module det_errors_and_io.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    a
    ;       b
    ;       c
    ;       d.

main(!IO) :-
    bad_pred(b, !IO).

:- pred bad_pred(t::in, io::di, io::uo) is det.

bad_pred(T, !IO) :-
    (
        T = a,
        io.write_string("T = a\n", !IO)
    ;
        T = b,
        io.write_string("T = b\n", !IO)
    ;
        T = c,
        io.write_string("T = c\n", !IO)
%   ;
%       T = d,
%       io.write_string("T = d\n", !IO)
    ).
