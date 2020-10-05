%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that finalisers that terminate with an uncaught exception cause
% the program to abort rather than keep running (i.e. execute
% other finalisers).
%

:- module final_excp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.

:- finalise final_pred_a/2. % Should be called first and succeed.
:- finalise final_pred_b/2. % Should be called second and abort.
:- finalise final_pred_c/2. % Should *not* be called.

main(!IO).

:- pred final_pred_a(io::di, io::uo) is cc_multi.

final_pred_a(!IO) :-
    io.write_string("This is final_pred_a/2\n", !IO),
    try(get_string(561), Result),
    (
        Result = succeeded(_),
        io.write_string("SUCCEEDED\n", !IO)
    ;
        Result = exception(_),
        io.write_string("EXCEPTION\n", !IO)
    ),
    io.flush_output(!IO).

:- pred final_pred_b(io::di, io::uo) is det.

final_pred_b(!IO) :-
    get_magic_number(X, !IO),
    ( if X = 3 then
        throw(magic_number_exception)
    else
        io.write_string("This is final_pred_b/2\n", !IO)
    ).

:- pred final_pred_c(io::di, io::uo) is det.

final_pred_c(!IO) :-
    io.write_string("final_pred_c/2: I should not be running.\n", !IO).

:- type magic_number_exception ---> magic_number_exception.

:- pred get_string(int::in, string::out) is det.

get_string(X, Str) :-
    ( if X = 561 then
        throw(magic_number_exception)
    else
        Str = "not 561"
    ).

:- pred get_magic_number(int::out, io::di, io::uo) is det.

get_magic_number(3, !IO).
