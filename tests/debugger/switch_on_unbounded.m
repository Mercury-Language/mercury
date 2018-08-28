%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the debugger's formatting of goal paths that include
% switches on types with an unbounded number of function symbols.

:- module switch_on_unbounded.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.
:- import_module list.

main(!IO) :-
    ( if edge(2, Two) then
        io.write_int(Two, !IO),
        io.nl(!IO)
    else
        io.write_string("edge(2, _) has no solution\n", !IO)
    ),
    ( if edge_str("2", TwoStr) then
        io.write_string(TwoStr, !IO),
        io.nl(!IO)
    else
        io.write_string("edge_str(2, _) has no solution\n", !IO)
    ).

:- pred edge(int::in, int::out) is semidet.
:- pragma no_inline(edge/2).

edge(1, 2).
edge(2, 1).
edge(3, 4).

:- pred edge_str(string::in, string::out) is semidet.
:- pragma no_inline(edge_str/2).

edge_str("1", "2").
edge_str("2", "1").
edge_str("3", "4").
