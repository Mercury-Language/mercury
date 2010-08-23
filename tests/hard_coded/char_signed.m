% Bug 158 and related issues with 8-bit characters due to MR_Char being
% signed (potentially).

:- module char_signed.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.

main(!IO) :-
    ( char.det_from_int(255) = '\xFF\' ->
        io.write_string("unify succeed\n", !IO)
    ;
        io.write_string("unify fail\n", !IO)
    ),
    ( aa @< '\xFF\' ->
        io.write_string("'A' < 0xFF\n", !IO)
    ;
        io.write_string("'A' >= 0xFF (wrong)\n", !IO)
    ),
    ( '\xFF\' @< aa ->
        io.write_string("0xFF < 'A' (wrong)\n", !IO)
    ;
        io.write_string("0xFF >= 'A'\n", !IO)
    ),
    ( char.to_int('A') < char.to_int('\xFF\') : int ->
        io.write_string("'A' < 0xFF\n", !IO)
    ;
        io.write_string("'A' >= 0xFF (wrong)\n", !IO)
    ).

:- func aa = char.
:- pragma no_inline(aa/0).

aa = char.det_from_int(65).

% vim: ft=mercury ts=8 sts=4 sw=4 et
