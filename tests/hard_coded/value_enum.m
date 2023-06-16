%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The Java backend was using `value' as a field name inside enumeration
% classes, which conflicted with `value' function symbols in Mercury code.

:- module value_enum.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type ref_or_value
    --->    value
    ;       ref.

main(!IO) :-
    io.write_line(value, !IO).
