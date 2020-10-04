%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Bug 158 and related issues with 8-bit characters due to MR_Char being
% signed (potentially).
%

:- module char_signed.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.

main(!IO) :-
    ( if char.det_from_int(255) = '\xFF\' then
        io.write_string("unify succeed\n", !IO)
    else
        io.write_string("unify fail\n", !IO)
    ),
    ( if aa @< '\xFF\' then
        io.write_string("'A' < 0xFF\n", !IO)
    else
        io.write_string("'A' >= 0xFF (wrong)\n", !IO)
    ),
    ( if '\xFF\' @< aa then
        io.write_string("0xFF < 'A' (wrong)\n", !IO)
    else
        io.write_string("0xFF >= 'A'\n", !IO)
    ),
    ( if char.to_int('A') < char.to_int('\xFF\') : int then
        io.write_string("'A' < 0xFF\n", !IO)
    else
        io.write_string("'A' >= 0xFF (wrong)\n", !IO)
    ).

:- func aa = char.
:- pragma no_inline(aa/0).

aa = char.det_from_int(65).
