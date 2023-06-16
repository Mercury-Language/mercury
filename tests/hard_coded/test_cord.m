%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% test_cord.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Feb 11 16:05:29 EST 2003
%---------------------------------------------------------------------------%

:- module test_cord.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Z    = empty,
    A    = cons(a, Z),
    B    = from_list([b]),
    AB   = A ++ B,
    BA   = B ++ A,
    ABA1 = A ++ BA,
    ABA2 = AB ++ A,
    BAB1 = B ++ AB,
    BAB2 = BA ++ B,
    ABBA = AB ++ BA,
    BAAB = BA ++ AB,

    io.format("\ncords:\n", [], !IO),
    io.format("Z    = ", [], !IO), io.print_line(Z,    !IO),
    io.format("A    = ", [], !IO), io.print_line(A,    !IO),
    io.format("B    = ", [], !IO), io.print_line(B,    !IO),
    io.format("AB   = ", [], !IO), io.print_line(AB,   !IO),
    io.format("BA   = ", [], !IO), io.print_line(BA,   !IO),
    io.format("ABA1 = ", [], !IO), io.print_line(ABA1, !IO),
    io.format("ABA2 = ", [], !IO), io.print_line(ABA2, !IO),
    io.format("BAB1 = ", [], !IO), io.print_line(BAB1, !IO),
    io.format("BAB2 = ", [], !IO), io.print_line(BAB2, !IO),
    io.format("ABBA = ", [], !IO), io.print_line(ABBA, !IO),
    io.format("BAAB = ", [], !IO), io.print_line(BAAB, !IO),

    ConstructionResult =
        (    if list(Z) \= []  then "list(Z) \\= []"
        else if list(A) \= [a] then "list(A) \\= [a]"
        else if list(B) \= [b] then "list(B) \\= [b]"
        else                        "ok"
        ),

    io.format("\nconstruction: %s\n", [s(ConstructionResult)], !IO),

    ConcatenationResult =
        (    if list(AB)   \= [a, b]       then "list(AB)   \\= [a, b]"
        else if list(BA)   \= [b, a]       then "list(BA)   \\= [b, a]"
        else if list(ABA1) \= [a, b, a]    then "list(ABA1) \\= [a, b, a]"
        else if list(ABA2) \= [a, b, a]    then "list(ABA2) \\= [a, b, a]"
        else if list(BAB1) \= [b, a, b]    then "list(BAB1) \\= [b, a, b]"
        else if list(BAB2) \= [b, a, b]    then "list(BAB2) \\= [b, a, b]"
        else if list(ABBA) \= [a, b, b, a] then "list(ABBA) \\= [a, b, b, a]"
        else if list(BAAB) \= [b, a, a, b] then "list(BAAB) \\= [b, a, a, b]"
        else                                    "ok"
        ),

    io.format("\nconcatenation: %s\n", [s(ConcatenationResult)], !IO),

    EqualsResult =
        (    if not equal(ABA1, ABA2) then "not equal(ABA1, ABA2)"
        else if not equal(BAB1, BAB2) then "not equal(BAB1, BAB2)"
        else                               "ok"
        ),

    io.format("\nequals: %s\n", [s(EqualsResult)], !IO),

    IdentityResult =
        (    if AB   ++ empty \= AB   then "AB   ++ empty \\= AB  "
        else if BA   ++ empty \= BA   then "BA   ++ empty \\= BA  "
        else if ABA1 ++ empty \= ABA1 then "ABA1 ++ empty \\= ABA1"
        else if ABA2 ++ empty \= ABA2 then "ABA2 ++ empty \\= ABA2"
        else if BAB1 ++ empty \= BAB1 then "BAB1 ++ empty \\= BAB1"
        else if BAB2 ++ empty \= BAB2 then "BAB2 ++ empty \\= BAB2"
        else if ABBA ++ empty \= ABBA then "ABBA ++ empty \\= ABBA"
        else if BAAB ++ empty \= BAAB then "BAAB ++ empty \\= BAAB"

        else if empty ++ AB   \= AB   then "empty ++ AB   \\= AB  "
        else if empty ++ BA   \= BA   then "empty ++ BA   \\= BA  "
        else if empty ++ ABA1 \= ABA1 then "empty ++ ABA1 \\= ABA1"
        else if empty ++ ABA2 \= ABA2 then "empty ++ ABA2 \\= ABA2"
        else if empty ++ BAB1 \= BAB1 then "empty ++ BAB1 \\= BAB1"
        else if empty ++ BAB2 \= BAB2 then "empty ++ BAB2 \\= BAB2"
        else if empty ++ ABBA \= ABBA then "empty ++ ABBA \\= ABBA"
        else if empty ++ BAAB \= BAAB then "empty ++ BAAB \\= BAAB"

        else    "ok"
        ),

    io.format("\nidentity: %s\n", [s(IdentityResult)], !IO),

    LengthResult =
        (    if length(Z)    \= 0 then "list(Z)    \\= 0"
        else if length(A)    \= 1 then "list(A)    \\= 1"
        else if length(B)    \= 1 then "list(B)    \\= 1"
        else if length(AB)   \= 2 then "list(AB)   \\= 2"
        else if length(BA)   \= 2 then "list(BA)   \\= 2"
        else if length(ABA1) \= 3 then "list(ABA1) \\= 3"
        else if length(ABA2) \= 3 then "list(ABA2) \\= 3"
        else if length(BAB1) \= 3 then "list(BAB1) \\= 3"
        else if length(BAB2) \= 3 then "list(BAB2) \\= 3"
        else if length(ABBA) \= 4 then "list(ABBA) \\= 4"
        else if length(BAAB) \= 4 then "list(BAAB) \\= 4"
        else                           "ok"
        ),

    io.format("\nlength: %s\n", [s(LengthResult)], !IO),

    ( if solutions((pred(X::out) is nondet :- member(X, ABBA))) \= [a, b] then
        MemberResult = "members(ABBA) \\= [a, b]"
    else
        MemberResult = "ok"
    ),

    io.format("\nmembership: %s\n", [s(MemberResult)], !IO),

    ( if list(map(func(X) = ( if X = a then p else q ), AB)) \= [p, q] then
        MapResult = "map( | a -> p, b -> q | , AB) \\= [p, q]"
    else
        MapResult = "ok"
    ),

    io.format("\nmap: %s\n", [s(MapResult)], !IO),

    ( if foldl(func(X, Xs) = [X | Xs], AB, []) \= [b, a] then
        FoldlResult = "foldl(cons, AB, []) \\= [b, a]"
    else if foldl(func(X, Xs) = [X | Xs], from_list([a, b]), []) \= [b, a] then
        FoldlResult = "foldl(cons, from_list([a, b]), []) \\= [b, a]"
    else
        FoldlResult = "ok"
    ),

    io.format("\nfoldl: %s\n", [s(FoldlResult)], !IO),

    ( if  foldr(func(X, Xs) = [X | Xs], AB, []) \= [a, b] then
        FoldrResult = "foldr(cons, AB, []) \\= [a, b]"
    else if foldr(func(X, Xs) = [X | Xs], from_list([a, b]), []) \= [a, b] then
        FoldrResult = "foldr(cons, from_list([a, b]), []) \\= [a, b]"
    else
        FoldrResult = "ok"
    ),

    io.format("\nfoldr: %s\n", [s(FoldrResult)], !IO),

    CAB   = condense(from_list([A, B])),
    CBA   = condense(from_list([B, A])),
    CABA1 = condense(from_list([A, CBA])),
    CABA2 = condense(from_list([CAB, A])),
    CBAB1 = condense(from_list([B, CAB])),
    CBAB2 = condense(from_list([CBA, B])),
    CABBA = condense(from_list([CAB, CBA])),
    CBAAB = condense(from_list([CBA, CAB])),

    CondenseResult =
        (    if list(CAB)   \= [a, b]       then "list(CAB)   \\= [a, b]"
        else if list(CBA)   \= [b, a]       then "list(CBA)   \\= [b, a]"
        else if list(CABA1) \= [a, b, a]    then "list(CABA1) \\= [a, b, a]"
        else if list(CABA2) \= [a, b, a]    then "list(CABA2) \\= [a, b, a]"
        else if list(CBAB1) \= [b, a, b]    then "list(CBAB1) \\= [b, a, b]"
        else if list(CBAB2) \= [b, a, b]    then "list(CBAB2) \\= [b, a, b]"
        else if list(CABBA) \= [a, b, b, a] then "list(CABBA) \\= [a, b, b, a]"
        else if list(CBAAB) \= [b, a, a, b] then "list(CBAAB) \\= [b, a, a, b]"
        else                                     "ok"
        ),

    io.format("\ncondense: %s\n", [s(CondenseResult)], !IO).

%---------------------------------------------------------------------------%
