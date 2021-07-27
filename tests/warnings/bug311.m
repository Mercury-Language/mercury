%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Author: original version written by Pedro Mariano; reformatted for our
% standard style, cut down to minimal size and the problem diagnosed by zs.
%
% Since the two predicates lattice_good and lattice_bad have the same
% semantics, invoking solutions.solutions on them should yield the same
% results. The bug is that the two calls to solutions yield different results:
% calling solutions on lattice_good returns both expected results, while
% calling solutions on lattice_bad returns only one.
%
% The source of the problem is that the C code we generate for lattice_bad
%
% 1.  creates a memory cell for G and fills in the xsize and ysize fields;
% 2.  creates a choice point for the disjunction;
% 3a. fills in the boundary field with `torus' in the first arm of the
%     disjunction, and
% 3b. fills in the boundary field with `closed' in the second arm of the
%     disjunction; and then
% 4.  the code after the disjunction returns G.
%
% When you print the result after each exit from lattice_bad, you get the
% the right answer. The problem is that the code in the second arm of the
% disjunction CLOBBERS a field in the memory cell returned by the first arm.
% EVERY ANSWER returned by lattice_bad specifies the same memory address for G.
% When the solutions predicate sorts the set of solutions, all the solutions
% it sorts look exactly alike, because they are all stored at the same address.
% At that time, that cell will contain the last solution returned.
%
% In a sense, neither the code generated for lattice_bad nor the solutions
% predicate are to blame; they are both doing what they are supposed to.
% The actual problem is that the code of solutions makes an assumption
% (that the memory cells containing the solutions are static and won't change)
% that is incorrect in the presence of code that fills in some parts of a
% memory cell *before* a disjunction (creating the cell, and thus establishing
% its address), and some other parts *inside* the disjunction.
%
% We could fix the problem in several ways.
%
% The quickest fix would be to make solutions deep copy all answers. Due to
% the potentially horrendous runtime cost, I don't think we should do this.
%
% We could generate an error message for disjunctions that have variables
% that are bound but not ground at the start of the disjunction and become
% more instantiated (maybe ground, maybe not) during the disjunction.
%
% The most complete solution would be to look for such variables, but instead
% of generating an error for them, we could modify the HLDS to eliminate the
% condition that causes the problem. We can do this by introducing a proxy
% for each such variable, a proxy that is set entirely inside the disjuncts.
%
% The idea is to replace the original HLDS
%
% bug311.lattice_bad(X, Y, G) :-
%   (
%     G = bug311.lattice(X, V_9, V_10),
%     G = bug311.lattice(V_11, Y, V_12),
%     (
%       V_8 = bug311.torus,
%       G = bug311.lattice(V_13, V_14, V_8)
%     ;
%       V_7 = bug311.closed,
%       G = bug311.lattice(V_15, V_16, V_7)
%     )
%   ).
%
% with
%
% bug311.lattice_bad(X, Y, G) :-
%   (
%     G_prime1 = bug311.lattice(X, V_9, V_10),
%     G_prime1 = bug311.lattice(V_11, Y, V_12),
%     (
%       G_prime1 = bug311.lattice(G_prime1_f1, G_prime1_f2, _),
%       V_8 = bug311.torus,
%       G = bug311.lattice(G_prime1_f1, G_prime1_f2, V_8)
%     ;
%       G_prime1 = bug311.lattice(G_prime1_f1, G_prime1_f2, _),
%       V_7 = bug311.closed,
%       G = bug311.lattice(G_prime1_f1, G_prime1_f2, V_7)
%     )
%   ).
%
% Note that in the original test case, G has TWO of its fields filled out
% by two SEPARATE disjunctions, so we would have to create TWO separate
% proxy variables for it.
%
% This change would have to be done by a new pass in the compiler, but
% this pass would have to be invoked only for predicates that (a) contain
% disjunctions, and (b) some of these disjunctions further instantiate
% already-partially-filled-in memory cells. We could have the pre-codegen
% simplify pass look for such situations, and record the set of variables
% involved (G in this case); the new pass would have to be invoked
% only if the set of these variables is not empty. Since this problem
% has existed for a long time and does not seem to have been noticed before,
% this definitely will be a rare occurrence.
%
% Note that the problem exists in both for low and high level C grades, and
% probably exists for the other MLDS grades as well.
%
% Note also that although this solution will fix the bug by making solutions
% generate the right set of solutions, the copying involved will slow down
% the generated code. The more fields filled in piecemeal, the more the cell
% will be copied, and the slower the code will be.
%
% Therefore for the original submitter, I propose replacing lattice_bad
% (which exhibits the bug) with lattice_bad_fixed (which does not).
%
%---------------------------------------------------------------------------%

:- module bug311.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

:- type geometry
    --->    wellmixed
    ;       lattice(
                xsize         :: int,
                ysize         :: int,
                boundary      :: boundary
            ) .

:- type boundary
    --->    torus
    ;       closed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(!IO) :-
    X = 2,
    Y = 3,
    solutions.solutions(lattice_good(X, Y), SG),
    io.print_line(SG, !IO),
    solutions.solutions(lattice_bad(X, Y), SB),
    io.print_line(SB, !IO),
    solutions.solutions(lattice_bad_fixed(X, Y), SBF),
    io.print_line(SBF, !IO),

    solutions.solutions(lattice_bad_3(X, Y), SB3),
    io.print_line(SB3, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred lattice_good(int::in, int::in, geometry::out) is multi.

lattice_good(X, Y, lattice(X, Y, torus)).
lattice_good(X, Y, lattice(X, Y, closed)).

:- pred lattice_bad(int::in, int::in, geometry::out) is multi.

lattice_bad(X, Y, G) :-
    G ^ xsize = X,
    G ^ ysize = Y,
    (
        G ^ boundary = torus
    ;
        G ^ boundary = closed
    ).

:- pred lattice_bad_3(int::in, int::in, {geometry, geometry, geometry}::out)
    is multi.

lattice_bad_3(X, Y, Tuple) :-
    G1 ^ xsize = X,
    G1 ^ ysize = Y,
    G2 ^ xsize = X,
    G2 ^ ysize = Y,
    G3 ^ xsize = X,
    G3 ^ ysize = Y,
    (
        G1 ^ boundary = torus,
        G2 ^ boundary = torus,
        G3 ^ boundary = torus
    ;
        G1 ^ boundary = closed,
        G2 ^ boundary = closed,
        G3 ^ boundary = closed
    ),
    Tuple = {G1, G2, G3}.

:- pred lattice_bad_lambda(int::in, int::in, geometry::out) is multi.

lattice_bad_lambda(X, Y, G) :-
    P =
        ( pred(YY::in, GG::out) is multi :-
            GG ^ xsize = X,
            GG ^ ysize = YY,
            (
                GG ^ boundary = torus
            ;
                GG ^ boundary = closed
            )
        ),
    P(Y, G).

:- pred lattice_bad_fixed(int::in, int::in, geometry::out) is multi.

lattice_bad_fixed(X, Y, G) :-
    (
        B = torus
    ;
        B = closed
    ),
    G = lattice(X, Y, B).
