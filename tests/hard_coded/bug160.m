%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is regression test for bug 160 in the Mantis database.
%
% The bug it tests for is in gcc. Compiling this source file in grade
% asm_fast.gc with the option --no-optimize-peep-mkword yields a .c file
% that gcc 4.4.1 generates incorrect code for at the default optimization level
% unless you give it the option --fno-tree-loop-im.
%
% Mercury.options specifies --optimize-peep-mkword for this program, so
% we should pass this test case even on systems with buggy gcc installations.

:- module bug160.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.

main(!IO) :-
    bug(R),
    io.write_string(R, !IO),
    io.nl(!IO).

:- type t1
    --->    a
    ;       b.

:- type t2
    --->    c(t3)
    ;       d
    ;       e.

:- type t3
    --->    f(t1, t1).

:- pred bug(string::out) is det.

bug(R) :-
    ( if p then
        R = "RESULT: BAD A"
    else
        ( if q(R0) then
            R1 = R0
        else
            R1 = c(f(a, a))
        ),
        % Enabling the following causes R1 to be bound to the correct value.
        % trace [io(!IO)] io.print(R1, !IO),
        (
            R1 = c(_),
            R = "RESULT: OK"
        ;
            R1 = d,
            R = "RESULT: BAD D"
        ;
            R1 = e,
            R = "RESULT: BAD E"
        )
    ).

:- pred p is semidet.

p :-
    ( if 1 = 2 then
        true
    else
        fail
    ).

:- pred q(t2::out) is semidet.

q(Out) :-
    ( if 1 = 2 then
        Out = d
    else
        fail
    ).

:- end_module bug160.
