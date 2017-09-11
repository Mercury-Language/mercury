%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
% Test case for functor, arg, deconstruct and their variants.
%
% Author: zs

:- module deconstruct_arg.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module array.
:- import_module list.
:- import_module string.
:- import_module deconstruct.
:- import_module maybe.
:- import_module pair.
:- import_module univ.

:- type enum
    --->    one
    ;       two
    ;       three.

:- type fruit
    --->    apple(list(int))
    ;       banana(list(enum)).

:- type thingie
    --->    foo
    ;       bar(int)
    ;       bar(int, int)
    ;       qux(int)
    ;       quux(int)
    ;       quuux(int, int)
    ;       wombat
    ;       zoom(int)
    ;       zap(int, float, int)
    ;       zip(int, int, int, int)
    ;       zop(float, float)
    ;       moomoo(
                moo :: int,
                'mooo!' :: string
            )
    ;       packed(
                packed1 :: int,
                packed2 :: enum,
                packed3 :: enum,
                packed4 :: enum,
                packed5 :: string
            ).

:- type poly(A, B)
    --->    poly_one(A)
    ;       poly_two(B)
    ;       poly_three(B, A, poly(B, A)).

:- type no_tag
    --->    qwerty(int).

:- type set(T)
    --->    set_rep(list(T))
    where equality is set_equal.

%---------------------------------------------------------------------------%

% convert list to set
:- func set(list(T)) = set(T).

set(List) = set_rep(List).

% convert set to sorted list
:- func set_to_sorted_list(set(T)) = list(T).

set_to_sorted_list(Set) = Sorted :-
    promise_equivalent_solutions [Sorted] (
        Set = set_rep(Unsorted),
        list.sort_and_remove_dups(Unsorted, Sorted)
    ).

:- pred set_equal(set(T)::in, set(T)::in) is semidet.

set_equal(Set1, Set2) :-
    set_to_sorted_list(Set1) = set_to_sorted_list(Set2).

%---------------------------------------------------------------------------%

main -->
        % test enumerations
    % test_all(one), newline,
        % test primary tags
    test_all(apple([])), newline,
    test_all(apple([9, 5, 1])), newline,
        % test remote secondary tags
    test_all(zop(3.3, 2.03)), newline,
    test_all(zap(50, 51.0, 52)), newline,
    test_all(zip(50, 51, 52, 53)), newline,
        % test local secondary tags
    test_all(wombat), newline,
        % test notag
    test_all(qwerty(5)), newline,
        % test named arguments
    test_all(moomoo(50, "moo.")), newline,
        % test characters
    test_all('a'), newline,
        % test a float which requires 17 digits of precision
    test_all(0.12345678901234566), newline,
        % test integers
    test_all(4), newline,
        % test unigned integers
    test_all(561u), newline,
        % test fixed size integers.
    test_all(42i8), newline,
    test_all(42u8), newline,
    test_all(42i16), newline,
    test_all(42u16), newline,
    test_all(42i32), newline,
    test_all(43u32), newline,
        % test univ.
    { type_to_univ(["hi! I'm a univ!"], Univ) },
    test_all(Univ), newline,
        % test noncanonical type
    test_all(set([1, 2, 3, 3])), newline,
        % test predicates
    test_all(newline), newline,
    test_all(test_all([1, 2])), newline,
    test_all(p(1, 2.2, "three")), newline,
        % test tuples
    test_all({1, 'b'}), newline,
    test_all({1, 'b', "third"}), newline,
    test_all({1, 'b', "third", {1, 2, 3, 4}}), newline,
        % test arrays
    test_all(array([1000, 2000])), newline,
    test_all(array([100, 200, 300])), newline,
    test_all(array([10, 20, 30, 40])), newline,
        % test packed fields
    test_all(packed(100, one, two, three, "four")), newline.

:- pred p(int::in, float::in, string::in, io::di, io::uo) is det.

p(_, _, _, !IO).

%---------------------------------------------------------------------------%

:- pred test_all(T::in, io.state::di, io.state::uo) is cc_multi.

test_all(T) -->
    test_deconstruct_functor(T),
    test_deconstruct_arg(T, 0),
    test_deconstruct_arg(T, 1),
    test_deconstruct_arg(T, 2),
    test_deconstruct_named_arg(T, "moo"),
    test_deconstruct_named_arg(T, "mooo!"),
    test_deconstruct_named_arg(T, "packed1"),
    test_deconstruct_named_arg(T, "packed2"),
    test_deconstruct_named_arg(T, "packed3"),
    test_deconstruct_deconstruct(T),
    test_deconstruct_limited_deconstruct(T, 3).

%---------------------------------------------------------------------------%

:- pred test_deconstruct_functor(T::in, io.state::di, io.state::uo)
    is cc_multi.

test_deconstruct_functor(T) -->
    io.write_string("deconstruct functor: "),
    { deconstruct.functor(T, include_details_cc, Functor, Arity) },
    io.write_string(Functor),
    io.write_string("/"),
    io.write_int(Arity),
    io.write_string("\n").

:- pred test_deconstruct_arg(T::in, int::in, io.state::di, io.state::uo)
    is cc_multi.

test_deconstruct_arg(T, ArgNum) -->
    { string.format("deconstruct argument %d of ", [i(ArgNum)], Str) },
    io.write_string(Str),
    io.print(T),
    { deconstruct.arg_cc(T, ArgNum, MaybeArg) },
    (
        { MaybeArg = arg(Arg) },
        io.write_string(" is "),
        io.write(Arg),
        io.write_string("\n")
    ;
        { MaybeArg = no_arg },
        io.write_string(" doesn't exist\n")
    ).

:- pred test_deconstruct_named_arg(T::in, string::in, io::di, io::uo)
    is cc_multi.

test_deconstruct_named_arg(T, Name, !IO) :-
    io.format("deconstruct argument '%s'", [s(Name)], !IO),
    deconstruct.named_arg_cc(T, Name, MaybeArg),
    (
        MaybeArg = arg(Arg),
        io.write_string(" is ", !IO),
        io.write(Arg, !IO),
        io.nl(!IO)
    ;
        MaybeArg = no_arg,
        io.write_string(" doesn't exist\n", !IO)
    ).

:- pred test_deconstruct_deconstruct(T::in, io.state::di, io.state::uo)
    is cc_multi.

test_deconstruct_deconstruct(T) -->
    { deconstruct.deconstruct(T, include_details_cc,
        Functor, Arity, Arguments) },
    { string.format("deconstruct deconstruct: functor %s arity %d\n",
        [s(Functor), i(Arity)], Str) },
    io.write_string(Str),
    io.write_string("["),
    io.write_list(Arguments, ", ", io.print),
    io.write_string("]\n").

:- pred test_deconstruct_limited_deconstruct(T::in, int::in,
    io.state::di, io.state::uo) is cc_multi.

test_deconstruct_limited_deconstruct(T, Limit) -->
    { string.format("deconstruct limited deconstruct %d of ",
        [i(Limit)], Str) },
    io.write_string(Str),
    io.print(T),
    io.write_string("\n"),
    { deconstruct.limited_deconstruct_cc(T, Limit, Result) },
    (
        { Result = yes({Functor, Arity, Arguments}) },
        { string.format("functor %s arity %d ",
            [s(Functor), i(Arity)], Str2) },
        io.write_string(Str2),
        io.write_string("["),
        io.write_list(Arguments, ", ", io.print),
        io.write_string("]\n")
    ;
        { Result = no },
        io.write_string("failed\n")
    ).

%---------------------------------------------------------------------------%

:- pred newline(io.state::di, io.state::uo) is det.

newline -->
    io.write_char('\n').
