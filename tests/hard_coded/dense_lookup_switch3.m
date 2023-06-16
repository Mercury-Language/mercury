%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dense_lookup_switch3.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.

main(!IO) :-
    test_all(0, 255, !IO).

:- pred test_all(int::in, int::in, io::di, io::uo) is det.

test_all(Cur, Max, !IO) :-
    ( if Cur < Max then
        test(Cur, !IO),
        test_all(Cur + 1, Max, !IO)
    else
        true
    ).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    io.write_int(N, !IO),
    NC = char.det_from_int(N),
    io.write_string(": ", !IO),
    ( if local_lower_upper(NC, UC) then
        io.write_string("upper ", !IO),
        io.write_char(UC, !IO)
    else
        io.write_string("no upper", !IO)
    ),
    io.write_string(" ", !IO),
    ( if local_lower_upper(LC, NC) then
        io.write_string("lower ", !IO),
        io.write_char(LC, !IO)
    else
        io.write_string("no lower", !IO)
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%

    % local_lower_upper(Lower, Upper) is true iff
    % Lower is a lower-case letter and Upper is the corresponding
    % upper-case letter.
    %
:- pred local_lower_upper(char, char).
:- mode local_lower_upper(in, out) is semidet.
:- mode local_lower_upper(out, in) is semidet.
:- pragma no_inline(local_lower_upper/2).

local_lower_upper('a', 'A').
local_lower_upper('b', 'B').
local_lower_upper('c', 'C').
local_lower_upper('d', 'D').
local_lower_upper('e', 'E').
local_lower_upper('f', 'F').
local_lower_upper('g', 'G').
local_lower_upper('h', 'H').
local_lower_upper('i', 'I').
local_lower_upper('j', 'J').
local_lower_upper('k', 'K').
local_lower_upper('l', 'L').
local_lower_upper('m', 'M').
local_lower_upper('n', 'N').
local_lower_upper('o', 'O').
local_lower_upper('p', 'P').
local_lower_upper('q', 'Q').
local_lower_upper('r', 'R').
local_lower_upper('s', 'S').
local_lower_upper('t', 'T').
local_lower_upper('u', 'U').
local_lower_upper('v', 'V').
local_lower_upper('w', 'W').
local_lower_upper('x', 'X').
local_lower_upper('y', 'Y').
local_lower_upper('z', 'Z').
