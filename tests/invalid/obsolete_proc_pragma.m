%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether obsolete_proc pragma declarations work correctly.
%
%---------------------------------------------------------------------------%

:- module obsolete_proc_pragma.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module bool.
:- import_module list.
:- import_module string.

main(!IO) :-
    % This call SHOULD get an obsolete warning.
    bool_vs_int(no, PredX),
    io.format("%d\n", [i(PredX)], !IO),
    % This call SHOULD NOT get an obsolete warning.
    ( if bool_vs_int(PredY, 42) then
        io.write_line(PredY, !IO)
    else
        io.write_string("pred failed\n", !IO)
    ),

    % This call SHOULD NOT get an obsolete warning.
    bool_vs_int_func(no) = FuncX,
    io.format("%d\n", [i(FuncX)], !IO),
    % This call SHOULD get an obsolete warning.
    ( if bool_vs_int_func(FuncY) = 42 then
        io.write_line(FuncY, !IO)
    else
        io.write_string("func failed\n", !IO)
    ).

:- pred bool_vs_int(bool, int).
:- mode bool_vs_int(in, out) is det.
:- mode bool_vs_int(out, in) is semidet.

:- pragma obsolete_proc(bool_vs_int(in, out), [xyzzy/2]).

bool_vs_int(no, 0).
bool_vs_int(yes, 1).

:- func bool_vs_int_func(bool) = int.
:- mode bool_vs_int_func(in) = out is det.
:- mode bool_vs_int_func(out) = in is semidet.

:- pragma obsolete_proc(bool_vs_int_func(out) = in, [xyzzy_func/1]).

bool_vs_int_func(no) = 0.
bool_vs_int_func(yes) = 1.

%---------------------------------------------------------------------------%
