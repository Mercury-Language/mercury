%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Older versions of Mercury used to accept unterminated octal escapes
% in order to be bug-for-bug compatible with NU-Prolog.

:- module unterminated_octal_escape.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("\43", !IO),
    io.nl(!IO).
