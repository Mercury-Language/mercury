%-----------------------------------------------------------------------------%
% backquoted_qualified_ops.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Dec 22 13:45:14 EST 2003
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Tests that the parser recognises backquoted, module-qualified operator
% names.
%
%-----------------------------------------------------------------------------%

:- module backquoted_qualified_ops.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, string, list.
:- import_module backquoted_qualified_ops.a.
:- import_module backquoted_qualified_ops.a.b.
:- import_module backquoted_qualified_ops.a.b.c.

%-----------------------------------------------------------------------------%

    :- module a.
    :- interface.
    :- func int `add` int = int.

        :- module b.
        :- interface.
        :- func int `mul` int = int.

            :- module c.
            :- interface.
            :- func int `div` int = int.

            :- end_module c.

        :- end_module b.

    :- end_module a.

    :- module a.
    :- implementation.
    X `add` Y = X + Y.

        :- module b.
        :- implementation.
        X `mul` Y = X * Y.

            :- module c.
            :- implementation.
            X `div` Y = X / Y.
            :- end_module c.

        :- end_module b.

    :- end_module a.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.format("2 `a.add` 2 = %d\n2 `a.b.mul` 2 = %d\n2 `a.b.c.div` 2 = %d\n",
        [i(2 `a.plus` 2), i(2 `a.b.mul` 2), i(2 `a.b.c.div` 2)]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
