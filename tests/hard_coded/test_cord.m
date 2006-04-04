%-----------------------------------------------------------------------------%
% test_cord.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Feb 11 16:05:29 EST 2003
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module test_cord.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord, list, string, solutions.

%-----------------------------------------------------------------------------%

main -->

  { Z    = empty,
    A    = cons(a, Z),
    B    = from_list([b]),
    AB   = A ++ B,
    BA   = B ++ A,
    ABA1 = A ++ BA,
    ABA2 = AB ++ A,
    BAB1 = B ++ AB,
    BAB2 = BA ++ B,
    ABBA = AB ++ BA,
    BAAB = BA ++ AB
  },

    io.format("\ncords:\n", []),
    io.format("Z    = ", []), io.print(Z   ), io.nl,
    io.format("A    = ", []), io.print(A   ), io.nl,
    io.format("B    = ", []), io.print(B   ), io.nl,
    io.format("AB   = ", []), io.print(AB  ), io.nl,
    io.format("BA   = ", []), io.print(BA  ), io.nl,
    io.format("ABA1 = ", []), io.print(ABA1), io.nl,
    io.format("ABA2 = ", []), io.print(ABA2), io.nl,
    io.format("BAB1 = ", []), io.print(BAB1), io.nl,
    io.format("BAB2 = ", []), io.print(BAB2), io.nl,
    io.format("ABBA = ", []), io.print(ABBA), io.nl,
    io.format("BAAB = ", []), io.print(BAAB), io.nl,

  { ConstructionResult =
        (      if list(Z) \= []  then "list(Z) \\= []"
          else if list(A) \= [a] then "list(A) \\= [a]"
          else if list(B) \= [b] then "list(B) \\= [b]"
          else                        "ok"
        )
  },

    io.format("\nconstruction: %s\n", [s(ConstructionResult)]),

  { ConcatenationResult =
        (      if list(AB)   \= [a, b]       then "list(AB)   \\= [a, b]"
          else if list(BA)   \= [b, a]       then "list(BA)   \\= [b, a]"
          else if list(ABA1) \= [a, b, a]    then "list(ABA1) \\= [a, b, a]"
          else if list(ABA2) \= [a, b, a]    then "list(ABA2) \\= [a, b, a]"
          else if list(BAB1) \= [b, a, b]    then "list(BAB1) \\= [b, a, b]"
          else if list(BAB2) \= [b, a, b]    then "list(BAB2) \\= [b, a, b]"
          else if list(ABBA) \= [a, b, b, a] then "list(ABBA) \\= [a, b, b, a]"
          else if list(BAAB) \= [b, a, a, b] then "list(BAAB) \\= [b, a, a, b]"
          else                                    "ok"
        )
  },

    io.format("\nconcatenation: %s\n", [s(ConcatenationResult)]),

  { EqualsResult =
        (      if not equal(ABA1, ABA2) then "not equal(ABA1, ABA2)"
          else if not equal(BAB1, BAB2) then "not equal(BAB1, BAB2)"
          else                               "ok"
        )
  },

    io.format("\nequals: %s\n", [s(EqualsResult)]),

  { IdentityResult =
        (      if AB   ++ empty \= AB   then "AB   ++ empty \\= AB  "
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
        )
  },

    io.format("\nidentity: %s\n", [s(IdentityResult)]),

  { LengthResult =
        (      if length(Z)    \= 0 then "list(Z)    \\= 0"
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
        )
  },

    io.format("\nlength: %s\n", [s(LengthResult)]),

  { MemberResult =
        ( if   solutions((pred(X::out) is nondet :- member(X, ABBA))) \= [a, b]
          then "members(ABBA) \\= [a, b]"
          else "ok"
        )
  },

    io.format("\nmembership: %s\n", [s(MemberResult)]),

  { MapResult =
        ( if   list(map(func(X) = ( if X = a then p else q ), AB)) \= [p, q]
          then "map(|a -> p, b -> q|, AB) \\= [p, q]"
          else "ok"
        )
  },

    io.format("\nmap: %s\n", [s(MapResult)]),

  { FoldlResult =
        ( if   foldl(func(X, Xs) = [X | Xs], AB, []) \= [b, a]
          then "foldl(cons, AB, []) \\= [b, a]"
          else
          if   foldl(func(X, Xs) = [X | Xs], from_list([a, b]), []) \= [b, a]
          then "foldl(cons, from_list([a, b]), []) \\= [b, a]"
          else "ok"
        )
  },

    io.format("\nfoldl: %s\n", [s(FoldlResult)]),

  { FoldrResult =
        ( if   foldr(func(X, Xs) = [X | Xs], AB, []) \= [a, b]
          then "foldr(cons, AB, []) \\= [a, b]"
          else
          if   foldr(func(X, Xs) = [X | Xs], from_list([a, b]), []) \= [a, b]
          then "foldr(cons, from_list([a, b]), []) \\= [a, b]"
          else "ok"
        )
  },

    io.format("\nfoldr: %s\n", [s(FoldrResult)]),

    [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
