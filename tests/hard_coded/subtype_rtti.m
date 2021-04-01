%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for C backends.
% The .exp2 file is for Java and C# backends.
%
%---------------------------------------------------------------------------%

:- module subtype_rtti.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

:- type dummy(T)
    --->    dummy.

:- type sub_dummy =< dummy(sub_color)
    --->    dummy.

:- type notag
    --->    notag(notag0(color)).

:- type sub_notag =< notag
    --->    notag(sub_notag0(sub_color)).

:- type notag0(T)
    --->    notag0(T).

:- type sub_notag0(T) =< notag0(T)
    --->    notag0(T).

:- type tagged
    --->    f0
    ;       f1(int).

:- type sub_tagged =< tagged
    --->    f1(int).

    % An enum type whose representation takes 1 bit.
:- type flag
    --->    flag_clear
    ;       flag_set.

    % This is an enum with one constructor, not a dummy type.
:- type sub_flag =< flag
    --->    flag_set.   % lex 0, ord 0

    % An enum type whose representation takes 2 bits.
:- type color
    --->    red
    ;       green
    ;       blue
    ;       orange.

    % This is also an enum. The functor ordinals differ from the base type.
:- type sub_color =< color
    --->    orange      % lex 2, ord 0
    ;       blue        % lex 0, ord 1
    ;       green.      % lex 1, ord 2

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
    % secondary tag.
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

    % The function ordinals differ from the base type.
:- type sub_t =< t
    --->    f7_c(fruit, flag, color, int)           % lex 5, ord 0
    ;       f7_b(                                   % lex 4, ord 1
                sub_f7_b1 :: sub_flag,
                sub_f7_b2 :: sub_color,
                sub_f7_b3 :: fruit,
                sub_f7_b4 :: float
            )
    ;       f6(int)                                 % lex 3, ord 2
    ;       f2(int)                                 % lex 2, ord 3
    ;       f0_b(sub_flag, sub_color, fruit)        % lex 1, ord 4
    ;       f0_a.                                   % lex 0, ord 5

main(!IO) :-
    % dummy
    Dummy = dummy : sub_dummy,
    test(Dummy, !IO),

    % notag
    Notag = notag(notag0(orange)) : sub_notag,
    test(Notag, !IO),

    % tagged
    Tagged = f1(42) : sub_tagged,
    test(Tagged, !IO),

    % enum
    Color = blue : sub_color,
    test(Color, !IO),

    % MR_SECTAG_LOCAL_BITS
    F0_A = f0_a : sub_t,
    test(F0_A, !IO),

    % MR_SECTAG_LOCAL_BITS with --allow-packing-local-sectags
    F0_B = f0_b(flag_set, blue, banana) : sub_t,
    test(F0_B, !IO),

    % MR_SECTAG_NONE
    F2 = f2(42) : sub_t,
    test(F2, !IO),

    % MR_SECTAG_REMOTE_BITS with --allow-packing-remote-sectags
    F7_B = f7_b(flag_set, blue, banana, 3.14) : sub_t,
    test(F7_B, !IO).

:- pred test(T::in, io::di, io::uo) is cc_multi.

test(Term, !IO) :-
    io.write_string("--------------------\n", !IO),
    io.write_string("term\n\t", !IO),
    io.print_line(Term, !IO),

    ( if deconstruct.functor(Term, do_not_allow, Functor, Arity) then
        io.write_string("deconstruct.functor\n", !IO),
        print_value("name", Functor, !IO),
        print_value("arity", Arity, !IO)
    else
        io.write_string("deconstruct.functor failed\n", !IO)
    ),

    % deconstruct.functor_number is not yet implemented for Java/C#
    % so use functor_number_cc.
    ( if deconstruct.functor_number_cc(Term, FunctorLexA, ArityA) then
        io.write_string("deconstruct.functor_number_cc\n", !IO),
        print_value("functor lex", FunctorLexA, !IO),
        print_value("arity", ArityA, !IO)
    else
        io.write_string("deconstruct.functor_number_cc failed\n", !IO)
    ),

    deconstruct.deconstruct(Term, do_not_allow, Functor, Arity, Args),
    io.write_string("deconstruct.deconstruct\n", !IO),
    print_value("functor name", Functor, !IO),
    print_value("arity", Arity, !IO),
    print_value("args", Args, !IO),

    TypeDesc = type_of(Term),

    ( if
        deconstruct.deconstruct_du(Term, do_not_allow, FunctorLex, Arity, Args)
    then
        io.write_string("deconstruct.deconstruct_du\n", !IO),
        print_value("functor lex", FunctorLex, !IO),

        ( if
            construct.get_functor_ordinal(TypeDesc, FunctorLex, FunctorOrdinal)
        then
            io.write_string("construct.get_functor_ordinal\n", !IO),
            print_value("functor ord", FunctorOrdinal, !IO),

            ( if
                construct.get_functor_lex(TypeDesc, FunctorOrdinal) =
                    FunctorLexB
            then
                io.write_string("construct.get_functor_lex\n", !IO),
                print_value("functor lex", FunctorLexB, !IO)
            else
                io.write_string("construct.get_functor_lex failed\n", !IO)
            )
        else
            io.write_string("construct.get_functor_ordinal failed\n", !IO)
        ),

        ( if construct.num_functors(TypeDesc) = NumFunctors then
            io.write_string("construct.num_functors\n", !IO),
            print_value("num functors", NumFunctors, !IO)
        else
            io.write_string("construct.num_functors failed\n", !IO)
        ),

        ( if
            construct.get_functor_with_names(TypeDesc, FunctorLex, Name,
                Arity, ArgTypes, ArgNames)
        then
            io.write_string("construct.get_functor_with_names\n", !IO),
            print_value("functor lex", FunctorLex, !IO),
            print_value("functor name", Name, !IO),
            print_value("arity", Arity, !IO),
            print_value("arg types", ArgTypes, !IO),
            print_value("arg names", ArgNames, !IO),

            ( if
                construct.find_functor(TypeDesc, Name, Arity, FunctorLex,
                    ArgTypeDescs)
            then
                io.write_string("construct.find_functor\n", !IO),
                print_value("arg type descs", ArgTypeDescs, !IO)
            else
                io.write_string("construct.find_functor failed\n", !IO)
            )
        else
            io.write_string("construct.find_functor failed\n", !IO)
        ),

        ( if construct.construct(TypeDesc, FunctorLex, Args) = Univ then
            io.write_string("construct.construct\n\t", !IO),
            io.print_line(Univ, !IO),
            ( if univ_to_type(Univ, Term) then
                io.write_string("unify ok\n", !IO)
            else
                io.write_string("unify failed\n", !IO)
            )
        else
            io.write_string("construct failed\n", !IO)
        )
    else
        io.write_string("deconstruct_du failed\n", !IO)
    ),
    io.nl(!IO).

:- pred print_value(string::in, T::in, io::di, io::uo) is det.

print_value(Label, Value, !IO) :-
    % We do not use io.format because this test case is compiled with
    % --allow-packing-local-sectags but the standard library likely is not,
    % which leads to mismatching data representations inside the format
    % implementation resulting in a crash.
    io.write_string("\t", !IO),
    io.write_string(Label, !IO),
    io.write_string(":\t", !IO),
    ( if string.length(Label) < 7 then
        io.write_string("\t", !IO)
    else
        true
    ),
    io.print(Value, !IO),
    io.nl(!IO).
