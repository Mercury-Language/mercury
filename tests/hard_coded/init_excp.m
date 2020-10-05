%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that initialisers that terminate with an uncaught exception
% cause the program to abort rather than keep running.
%
:- module init_excp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.

:- initialise init_pred_a/2.    % Should be called first and succeed.
:- initialise init_pred_b/2.    % Should be called second and abort.
:- initialise init_pred_c/2.    % Should *not* be called.
:- finalise final_pred/2.   % Should *not* be called.

   % Catching an exception within an initialiser is okay.
   %
:- pred init_pred_a(io::di, io::uo) is cc_multi.

init_pred_a(!IO) :-
    io.write_string("This is init_pred_a/2\n", !IO),
    try(get_string(561), Result),
    (
        Result = succeeded(_),
        io.write_string("SUCCEEDED\n", !IO)
    ;
        Result = exception(_),
        io.write_string("EXCEPTION\n", !IO)
    ),
    io.flush_output(!IO).

:- pred get_string(int::in, string::out) is det.

get_string(X, Str) :-
    ( if X = 561 then
        throw(magic_number_exception)
    else
        Str = "not 561"
    ).

   % Not catching one should cause the program to abort.
   %
:- pred init_pred_b(io::di, io::uo) is det.

init_pred_b(!IO) :-
    get_magic_number(X, !IO),
    ( if X = 3 then
        throw(magic_number_exception)
    else
        io.write_string("This is init_pred_b/2\n", !IO)
    ).

:- type magic_number_exception ---> magic_number_exception.

:- pred get_magic_number(int::out, io::di, io::uo) is det.

get_magic_number(3, !IO).

   % Subsequent initialisers should not be run if this happens.
   %
:- pred init_pred_c(io::di, io::uo) is det.

init_pred_c(!IO) :-
    io.write_string("I should not be running.\n", !IO).

   % Nor should main/2.
   %
main(!IO) :-
    io.write_string("This is main/2\n", !IO).

   % No finalisers should be run.
   %
:- pred final_pred(io::di, io::uo) is det.

final_pred(!IO) :-
    io.write_string("I should not be running either.\n", !IO).
