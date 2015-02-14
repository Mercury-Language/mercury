%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module double_error2.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module require.
:- import_module std_util.

main -->
    (
        { semidet_succeed }
    ->
        io__write_string("yes\n")
    ;
        io__progname("foo", Name),
        { error(Name) }
    ),
    (
        { semidet_succeed }
    ->
        io__write_string("yes\n")
    ;
        io__progname("bar", Name),
        { error(Name) }
    ).
