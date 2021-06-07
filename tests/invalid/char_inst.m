%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module char_inst.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char.

main(!IO) :-
    test('s', !IO),     % This should be accepted without complaint.
    test('x', !IO).     % This should generate an error message.

:- inst key_char for char/0
    --->    ('s')
    ;       ('i')
    ;       ('f').

:- pred test(char::in(key_char), io::di, io::uo) is det.

test(KeyChar, !IO) :-
    (
        KeyChar = 's',
        io.write_string("string\n", !IO)
    ;
        KeyChar = 'i',
        io.write_string("int\n", !IO)
    ;
        KeyChar = 'f',
        io.write_string("float\n", !IO)
    ).
