%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_count_codepoints_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S1 = string.between(S0, 0, 1),
    S2 = string.between(S0, 1, count_code_units(S0)),
    S = S0 ++ S1 ++ "," ++ S2 ++ S0,
    string.count_codepoints(S, Num),
    io.write_int(Num, !IO),
    io.nl(!IO).
