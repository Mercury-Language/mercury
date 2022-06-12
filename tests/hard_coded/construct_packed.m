%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for the various methods of term construction in the presence
% of arguments packed next to both local and remote secondary tags.
%
% The methods are:
%
% - construct unifications on dynamic data
%   ({ml_,}generate_construction_unification in
%   compiler/{ml_,}unify_gen_construct.m)
%
% - constructions of ground terms
%   ({ml_,}generate_ground_term in compiler/{ml_,}unify_gen_construct.m)
%
% - construct unifications on static data
%   ({ml_,}generate_const_structs} in {ml_,}unify_gen_construct.m)
%
% - construction at runtime
%   (in library/construct.m)
%
% - copying at runtime
%   (in library/builtin.m and in runtime/mercury_deep_copy*)
%
% - field updates (reconstructions)
%   ({ml_,}generate_deconstruction_unification in
%   compiler/{ml_,}unify_gen_deconstruct.m, as well as
%   {ml_,}generate_construction_unification in
%   compiler/{ml_,}unify_gen_construct.m)
%

:- module construct_packed.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

    % An enum type whose representation takes 1 bit.
:- type flag
    --->    flag_clear
    ;       flag_set.

    % An enum type whose representation takes 2 bits.
:- type color
    --->    red
    ;       green
    ;       blue.

    % An enum type whose representation takes 3 bits.
:- type fruit
    --->    apple
    ;       pear
    ;       peach
    ;       orange
    ;       banana.

    % Every function symbol fN* should be allocated primary tag N.
    % The function symbols whose names end in _[abc] share their
    % primary tags. being distinguished by a local (f0) or remote (f7)
    % secondary tag. These all have an initial sequence of subword-sized
    % arguments that the compiler should pack next to the secondary tag.
:- type t
    --->    f0_a
    ;       f0_b(f0_b1 :: flag, f0_b2 :: color, f0_b3 :: fruit)
    ;       f0_c(f0_c1 :: color, f0_c2 :: fruit, f0_c3 :: flag)
    ;       f1(int)
    ;       f2(int)
    ;       f3(int)
    ;       f4(int)
    ;       f5(int)
    ;       f6(int)
    ;       f7_a(f7_a1 :: int)
    ;       f7_b(f7_b1 :: flag, f7_b2 :: color, f7_b3 :: fruit, f7_b4 :: float)
    ;       f7_c(f7_c1 :: fruit, f7_c2 :: flag, f7_c3 :: color, f7_c4 :: int).

main(!IO) :-
    TypeDesc = type_desc.type_of(f0_a),

    % Test construction of structured terms using each of the three
    % construction methods that have separate code for them in the compiler:
    % dynamic construct unification, static construct unification, and static
    % ground term, as well as using the construct.m library module.

    io.write_string("Construct for terms with packed local sectags\n\n", !IO),

    DynamicLocalA0 =      f0_a,
    ConstructLocalA0 = make_term(TypeDesc, "f0_a", []),
    builtin.copy(DynamicLocalA0, CopyLocalA0),

    io.write_line(DynamicLocalA0, !IO),
    write_univ_line(ConstructLocalA0, !IO),
    io.write_line(CopyLocalA0, !IO),
    io.nl(!IO),

    DynamicLocalB0 =      f0_b(flag_set, blue, id(peach)),
    StaticLocalB0 =       f0_b(flag_set, blue, peach),
    GroundTermsLocalB0 = [f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach),
 
                          f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach),
                          f0_b(flag_set, blue, peach)],
    list.det_head(GroundTermsLocalB0, GroundTermLocalB0),
    ConstructLocalB0 = make_term(TypeDesc, "f0_b",
        [univ(flag_set), univ(blue), univ(peach)]),
    builtin.copy(DynamicLocalB0, CopyLocalB0),

    io.write_line(DynamicLocalB0, !IO),
    io.write_line(StaticLocalB0, !IO),
    io.write_line(GroundTermLocalB0, !IO),
    write_univ_line(ConstructLocalB0, !IO),
    io.write_line(CopyLocalB0, !IO),
    io.nl(!IO),

    DynamicLocalC0 =      f0_c(green, orange, id(flag_clear)),
    StaticLocalC0 =       f0_c(green, orange, flag_clear),
    GroundTermsLocalC0 = [f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear),
 
                          f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear),
                          f0_c(green, orange, flag_clear)],
    list.det_head(GroundTermsLocalC0, GroundTermLocalC0),
    ConstructLocalC0 = make_term(TypeDesc, "f0_c",
        [univ(green), univ(orange), univ(flag_clear)]),
    builtin.copy(DynamicLocalC0, CopyLocalC0),

    io.write_line(DynamicLocalC0, !IO),
    io.write_line(StaticLocalC0, !IO),
    io.write_line(GroundTermLocalC0, !IO),
    write_univ_line(ConstructLocalC0, !IO),
    io.write_line(CopyLocalC0, !IO),
    io.nl(!IO),

    io.write_string("Reconstruct for terms with packed local sectags\n\n",
        !IO),
    DynamicLocalB1 = DynamicLocalB0 ^ f0_b1 := flag_clear,
    DynamicLocalB2 = DynamicLocalB1 ^ f0_b2 := red,
    DynamicLocalB3 = DynamicLocalB2 ^ f0_b3 := orange,
    DynamicLocalC1 = DynamicLocalC0 ^ f0_c1 := blue,
    DynamicLocalC2 = DynamicLocalC1 ^ f0_c2 := pear,
    DynamicLocalC3 = DynamicLocalC2 ^ f0_c3 := flag_set,

    io.write_line(DynamicLocalB1, !IO),
    io.write_line(DynamicLocalB2, !IO),
    io.write_line(DynamicLocalB3, !IO),
    io.write_line(DynamicLocalC1, !IO),
    io.write_line(DynamicLocalC2, !IO),
    io.write_line(DynamicLocalC3, !IO),
    io.nl(!IO),

    io.write_string("Construct for terms with packed remote sectags\n\n", !IO),
    DynamicRemoteA0 =      f7_a(id(41)),
    StaticRemoteA0 =       f7_a(41),
    GroundTermsRemoteA0 = [f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),

                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41),
                           f7_a(41), f7_a(41), f7_a(41), f7_a(41)],
    list.det_head(GroundTermsRemoteA0, GroundTermRemoteA0),
    ConstructRemoteA0 = make_term(TypeDesc, "f7_a", [univ(41)]),
    builtin.copy(DynamicRemoteA0, CopyRemoteA0),

    io.write_line(DynamicRemoteA0, !IO),
    io.write_line(StaticRemoteA0, !IO),
    io.write_line(GroundTermRemoteA0, !IO),
    write_univ_line(ConstructRemoteA0, !IO),
    io.write_line(CopyRemoteA0, !IO),
    io.nl(!IO),

    DynamicRemoteB0 =      f7_b(flag_clear, red, id(peach), 98.7),
    StaticRemoteB0 =       f7_b(flag_clear, red, peach, 98.7),
    GroundTermsRemoteB0 = [f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7),

                           f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7),
                           f7_b(flag_clear, red, peach, 98.7)],
    list.det_head(GroundTermsRemoteB0, GroundTermRemoteB0),
    ConstructRemoteB0 = make_term(TypeDesc, "f7_b",
        [univ(flag_clear), univ(red), univ(peach), univ(98.7)]),
    builtin.copy(DynamicRemoteB0, CopyRemoteB0),

    io.write_line(DynamicRemoteB0, !IO),
    io.write_line(StaticRemoteB0, !IO),
    io.write_line(GroundTermRemoteB0, !IO),
    write_univ_line(ConstructRemoteB0, !IO),
    io.write_line(CopyRemoteB0, !IO),
    io.nl(!IO),

    DynamicRemoteC0 =      f7_c(pear, flag_set, green, id(43)),
    StaticRemoteC0 =       f7_c(pear, flag_set, green, 43),
    GroundTermsRemoteC0 = [f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43),

                           f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43),
                           f7_c(pear, flag_set, green, 43)],
    list.det_head(GroundTermsRemoteC0, GroundTermRemoteC0),
    ConstructRemoteC0 = make_term(TypeDesc, "f7_c",
        [univ(pear), univ(flag_set), univ(green), univ(43)]),
    builtin.copy(DynamicRemoteC0, CopyRemoteC0),

    io.write_line(DynamicRemoteC0, !IO),
    io.write_line(StaticRemoteC0, !IO),
    io.write_line(GroundTermRemoteC0, !IO),
    write_univ_line(ConstructRemoteC0, !IO),
    io.write_line(CopyRemoteC0, !IO),
    io.nl(!IO),

    io.write_string("Reconstruct for terms with packed remote sectags\n\n",
        !IO),

    DynamicRemoteA1 = DynamicRemoteA0 ^ f7_a1 := 51,
    DynamicRemoteB1 = DynamicRemoteB0 ^ f7_b1 := flag_set,
    DynamicRemoteB2 = DynamicRemoteB1 ^ f7_b2 := blue,
    DynamicRemoteB3 = DynamicRemoteB2 ^ f7_b3 := orange,
    DynamicRemoteB4 = DynamicRemoteB3 ^ f7_b4 := 87.6,
    DynamicRemoteC1 = DynamicRemoteC0 ^ f7_c1 := apple,
    DynamicRemoteC2 = DynamicRemoteC1 ^ f7_c2 := flag_clear,
    DynamicRemoteC3 = DynamicRemoteC2 ^ f7_c3 := blue,
    DynamicRemoteC4 = DynamicRemoteC3 ^ f7_c4 := 53,

    io.write_line(DynamicRemoteA1, !IO),
    io.write_line(DynamicRemoteB1, !IO),
    io.write_line(DynamicRemoteB2, !IO),
    io.write_line(DynamicRemoteB3, !IO),
    io.write_line(DynamicRemoteB4, !IO),
    io.write_line(DynamicRemoteC1, !IO),
    io.write_line(DynamicRemoteC2, !IO),
    io.write_line(DynamicRemoteC3, !IO),
    io.write_line(DynamicRemoteC4, !IO).

:- func id(T) = T.
:- pragma no_inline(id/1).

id(N) = N.

:- func make_term(type_desc, string, list(univ)) = univ is det.

make_term(TypeDesc, Functor, ArgUnivs) = TermUniv :-
    list.length(ArgUnivs, Arity),
    ( if
        find_functor(TypeDesc, Functor, Arity, Lex, _ArgTypeDescs),
        construct(TypeDesc, Lex, ArgUnivs) = TermUnivPrime
    then
        TermUniv = TermUnivPrime
    else
        TermUniv = univ("construct failed")
    ).

:- pred write_univ_line(univ::in, io::di, io::uo) is det.

write_univ_line(Univ, !IO) :-
    ( if univ_to_type(Univ, T : t) then
        io.write_line(T, !IO)
    else if univ_to_type(Univ, S : string) then
        io.write_string(S, !IO),
        io.nl(!IO)
    else
        io.write_string("unexpected kind of univ", !IO)
    ).
