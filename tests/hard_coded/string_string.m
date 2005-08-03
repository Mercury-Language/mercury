%-----------------------------------------------------------------------------%
% string_string.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Oct  5 13:10:27 EST 2004
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module string_string.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string, list, std_util.

:- type tree ---> leaf; branch(tree, tree).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string(string.string(leaf) ++ "\n", !IO),
    io.write_string(string.string(branch(leaf, leaf)) ++ "\n", !IO),
    io.write_string(string.string([1,2,3]) ++ "\n", !IO),
    io.write_string(string.string(1.234 - 5) ++ "\n", !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
