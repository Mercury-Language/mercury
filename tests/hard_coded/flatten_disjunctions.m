%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This test case tests whether the parser flattens nested disjunctions,
% both in normal clauses (predicate p), and in DCG clauses (predicate dcg_p).
%
% The main bodies of those predicates are disjunctions that are effectively
% switches on the value of A. Switch detection looks for unifications
% that could allow it to turn a disjunction into a switch in disjuncts
% of that disjunction, and in disjuncts inside those disjuncts; it does NOT
% look for them in disjuncts inside disjuncts inside disjuncts. In other
% words, it looks at a unifications at a maximum depth of two levels.
%
% In the written form of these predicates, the unifications in the innermost
% disjunction "( A = 4 ; A = 5 )" are at a depth of three. Switch detection
% can nevertheless turn both predicate bodies into switches, because parsing
% has traditionally flattened disjunctions, which means that if a disjunct
% consists entirely of another disjunction, then it replaced that outer
% disjunct with the arms of the inner disjunction. In this case, this
% flattening brings the A = 4 and A = 5 unifications that used to be
% at depth three to depth two, where switch detection can see them.
%
% This test case tests that the parsers (parse_goal.m and parse_dcg_goal.m)
% do flatten disjunctions. If they don't, then switch detection will leave
% at least one disjunction in p and/or dcg_p, and the compilation of the
% affected predicate(s) will fail with a determinism error.
%
% The original code from which this test code is distilled is the
% read_parse_tree_src_components predicate in compiler/parse_module.m,
% which (as of 2022 may 5) has a switch on IOM that switch detection
% recognizes *only* if disjunctions have been flattened by then.
%
%---------------------------------------------------------------------------%

:- module flatten_disjunctions.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    ( if p(6, B, 4, X) then
        io.format("p(6, %d, 4, %d) succeeded.\n", [i(B), i(X)], !IO)
    else
        io.format("p(6, _, 4, _) failed.\n", [], !IO)
    ),
    ( if dcg_p(6, DCG_B, 4, DCG_X) then
        io.format("dcg_p(6, %d, 4, %d) succeeded.\n",
            [i(DCG_B), i(DCG_X)], !IO)
    else
        io.format("dcg_p(6, _, 4, _) failed.\n", [], !IO)
    ).

:- pred p(int::in, int::out, int::in, int::out) is semidet.
:- pragma no_inline(pred(p/4)).

p(A, B, !X) :-
    (
        A = 1, B = 11
    ;
        ( A = 4
        ; A = 5
        ; A = 6
        ; A = 7
        ),
        (
            (
                ( A = 4
                ; A = 5
                )
            ;
                A = 6,
                !:X = !.X + 6
            ),
            (
                !.X = 10,
                B = 5
            ;
                !.X = 11,
                B = 6
            )
        ;
            A = 7,
            B = A
        )
    ).

:- pred dcg_p(int::in, int::out, int::in, int::out) is semidet.
:- pragma no_inline(pred(dcg_p/4)).

dcg_p(A, B) -->
    (
        { A = 1, B = 11 }
    ;
        ( { A = 4 }
        ; { A = 5 }
        ; { A = 6 }
        ; { A = 7 }
        ),
        (
            (
                ( { A = 4 }
                ; { A = 5 }
                )
            ;
                { A = 6 },
                =(X0),
                :=(X0 + 6)
            ),
            (
                =(X1),
                { X1 = 10 },
                { B = 5 }
            ;
                =(X2),
                { X2 = 11 },
                { B = 6 }
            )
        ;
            { A = 7 },
            { B = A }
        )
    ).
