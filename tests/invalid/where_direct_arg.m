%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module where_direct_arg.
:- interface.

:- type bad_example
    --->    zero
    ;       two(int, int)
    ;       string(string)
    ;       int(int)
    ;       tuple({int, int})
    where   direct_arg is [
                zero/0, two/2, string/1, int/1, tuple/1, nonexistent/1
            ].

:- type bad_example2 where direct_arg is [struct/1].

:- solver type bad_example3 where direct_arg is [].

:- type dummy
    --->    dummy.
