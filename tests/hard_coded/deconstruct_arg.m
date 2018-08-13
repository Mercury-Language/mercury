%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for functor, arg, deconstruct and their variants.
%
% Author: zs
%
% There are two expected output files for this test case.
% The .exp file is for LLDS grades.
% The .exp2 file is for MLDS grades.
%
% According to the first log entry of the .exp2 file, they are different
% because of missing information in closure layout structures in hlc grades.
%
%---------------------------------------------------------------------------%

:- module deconstruct_arg.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module array.
:- import_module assoc_list.
:- import_module float.
:- import_module list.
:- import_module string.
:- import_module deconstruct.
:- import_module maybe.
:- import_module pair.
:- import_module stream.
:- import_module stream.string_writer.
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

main(!IO) :-
    % test enumerations
    % test_all(one, !IO),

    % test primary tags
    test_all(apple([]), !IO),
    test_all(apple([9, 5, 1]), !IO),

    % test remote secondary tags
    test_all(zop(3.3, 2.03), !IO),
    test_all(zap(50, 51.0, 52), !IO),
    test_all(zip(50, 51, 52, 53), !IO),

    % test local secondary tags
    test_all(wombat, !IO),

    % test notag
    test_all(qwerty(5), !IO),

    % test named arguments
    test_all(moomoo(50, "moo."), !IO),

    % test characters
    test_all('a', !IO),
    test_all(' ', !IO),
    test_all('\a', !IO),
    test_all('\b', !IO),
    test_all('\r', !IO),
    test_all('\f', !IO),
    test_all('\t', !IO),
    test_all('\n', !IO),
    test_all('\v', !IO),
    test_all('\\', !IO),
    test_all('\'', !IO),
    test_all('~', !IO),

    % test C0 control characters
    test_all('\001\', !IO),
    test_all('\037\', !IO),
    test_all('\177\', !IO),
    % test C1 control characters
    test_all('\200\', !IO),
    test_all('\237\', !IO),
    % No-break space (next codepoint after C1 control characters)
    test_all('\240\', !IO),

    % test a character that requires more than one byte in its
    % UTF-8 encoding.
    test_all('Ω', !IO),

    % test strings (that do not require escapes)
    test_all("", !IO),
    test_all("azBZ09", !IO),
    test_all("\u03b1\u2200\U0001f713", !IO),

    % test strings (that do require escapes)
    test_all("\a\b\f\n\t\r\v\"\\", !IO),
    test_all("\x1\\a\x1f\AZ[`az~\x7f\", !IO),
    test_all("\x80\\a\x9f\\xa0\\xc0\\xff\", !IO),
    test_all("α\nβ\tγ,a\nα\001\α\001\a\001\α", !IO),

    % test a float which requires 17 digits of precision
    test_all(0.12345678901234566, !IO),

    % test infinite floats
    test_all(float.infinity, !IO),
    NegInf : float = -float.infinity,
    test_all(NegInf, !IO),

    % test integers
    test_all(4, !IO),

    % test unsigned integers
    test_all(561u, !IO),

    % test fixed size integers.
    test_all(42i8, !IO),
    test_all(42u8, !IO),
    test_all(42i16, !IO),
    test_all(42u16, !IO),
    test_all(42i32, !IO),
    test_all(43u32, !IO),
    test_all(66i64, !IO),
    test_all(67u64, !IO),

    % test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    test_all(Univ, !IO),

    % test noncanonical type
    test_all(set([1, 2, 3, 3]), !IO),

    % test predicates
    test_all(newline, !IO),
    test_all(test_all([1, 2]), !IO),
    test_all(p(1, 2.2, "three"), !IO),

    % test tuples
    test_all({1, 'b'}, !IO),
    test_all({1, 'b', "third"}, !IO),
    test_all({1, 'b', "third", {1, 2, 3, 4}}, !IO),

    % test arrays
    test_all(array([1000, 2000]), !IO),
    test_all(array([100, 200, 300]), !IO),
    test_all(array([10, 20, 30, 40]), !IO),

    % test packed fields
    test_all(packed(100, one, two, three, "four"), !IO).

:- pred p(int::in, float::in, string::in, io::di, io::uo) is det.

p(_, _, _, !IO).

:- pred newline(io::di, io::uo) is det.

newline(!IO) :-
    io.write_char('\n', !IO).

%---------------------------------------------------------------------------%

:- pred test_all(T::in, io::di, io::uo) is cc_multi.

test_all(T, !IO) :-
    io.write_string("test term: ", !IO),
    io.write_line(T, !IO),
    test_deconstruct_functor(T, MaybeConstant, !IO),

    some [!RevPairs] (
        !:RevPairs = [],
        test_deconstruct_arg(T, 0, !RevPairs),
        test_deconstruct_arg(T, 1, !RevPairs),
        test_deconstruct_arg(T, 2, !RevPairs),
        test_deconstruct_named_arg(T, "moo", !RevPairs),
        test_deconstruct_named_arg(T, "mooo!", !RevPairs),
        test_deconstruct_named_arg(T, "packed1", !RevPairs),
        test_deconstruct_named_arg(T, "packed2", !RevPairs),
        test_deconstruct_named_arg(T, "packed3", !RevPairs),
        list.reverse(!.RevPairs, Pairs),

        % Do not bore readers with each negative result individually;
        % print just one message for them all collectively, and even that
        % only if is not the expected result.
        list.filter(has_arg, Pairs, HasArgPairs, NoArgPairs),
        (
            HasArgPairs = [],
            (
                MaybeConstant = constant(_)
                % Not being able to get any arguments is not surprising
                % for terms that do not have any.
            ;
                MaybeConstant = not_constant(_, _),
                io.write_string("no argument access succeeded\n", !IO)
            )
        ;
            HasArgPairs = [_ | _],
            list.foldl(write_arg_pair, HasArgPairs, !IO),
            (
                NoArgPairs = [_ | _],
                io.write_string("no other argument access succeeded\n", !IO)
            ;
                NoArgPairs = []
            )
        )
    ),

    test_deconstruct_deconstruct(MaybeConstant, T, !IO),
    test_deconstruct_limited_deconstruct(MaybeConstant, T, 3, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- type maybe_constant
    --->    not_constant(string, int)
    ;       constant(string).

:- pred test_deconstruct_functor(T::in, maybe_constant::out,
    io::di, io::uo) is cc_multi.

test_deconstruct_functor(T, MaybeConstant, !IO) :-
    deconstruct.functor(T, include_details_cc, Functor, Arity),
    ( if Arity = 0 then
        MaybeConstant = constant(Functor)
    else
        MaybeConstant = not_constant(Functor, Arity)
    ),
    io.format("deconstruct functor: %s/%d\n", [s(Functor), i(Arity)], !IO).

%---------------------------------------------------------------------------%

:- pred test_deconstruct_arg(T::in, int::in,
    assoc_list(string, maybe_arg)::in, assoc_list(string, maybe_arg)::out)
    is cc_multi.

test_deconstruct_arg(T, ArgNum, !RevPairs) :-
    string.format("argument #%d: ", [i(ArgNum)], Desc),
    deconstruct.arg_cc(T, ArgNum, MaybeArg),
    !:RevPairs = [Desc - MaybeArg | !.RevPairs].

:- pred test_deconstruct_named_arg(T::in, string::in,
    assoc_list(string, maybe_arg)::in, assoc_list(string, maybe_arg)::out)
    is cc_multi.

test_deconstruct_named_arg(T, Name, !RevPairs) :-
    string.format("argument named '%s': ", [s(Name)], Desc),
    deconstruct.named_arg_cc(T, Name, MaybeArg),
    !:RevPairs = [Desc - MaybeArg | !.RevPairs].

%---------------------------------------------------------------------------%

:- pred has_arg(pair(string, maybe_arg)::in) is semidet.

has_arg(_Desc - arg(_)).

:- pred write_arg_pair(pair(string, maybe_arg)::in, io::di, io::uo) is det.

write_arg_pair(Desc - MaybeArg, !IO) :-
    io.write_string(Desc, !IO),
    (
        MaybeArg = arg(Arg),
        io.write(Arg, !IO),
        io.nl(!IO)
    ;
        MaybeArg = no_arg,
        io.write_string("does not exist\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_deconstruct_deconstruct(maybe_constant::in, T::in,
    io::di, io::uo) is cc_multi.

test_deconstruct_deconstruct(MaybeConstant, T, !IO) :-
    Desc = "plain deconstruct:",
    deconstruct.deconstruct(T, include_details_cc, Functor, Arity, ArgUnivs),
    write_deconstruct_results_if_interesting(Desc, MaybeConstant,
        Functor, Arity, ArgUnivs, !IO).

:- pred test_deconstruct_limited_deconstruct(maybe_constant::in, T::in,
    int::in, io::di, io::uo) is cc_multi.

test_deconstruct_limited_deconstruct(MaybeConstant, T, Limit, !IO) :-
    string.format("limited deconstruct %d:", [i(Limit)], Desc),
    deconstruct.limited_deconstruct_cc(T, Limit, Result),
    (
        Result = yes({Functor, Arity, ArgUnivs}),
        write_deconstruct_results_if_interesting(Desc, MaybeConstant,
            Functor, Arity, ArgUnivs, !IO)
    ;
        Result = no,
        io.format("%s failed\n", [s(Desc)], !IO)
    ).

:- pred write_deconstruct_results_if_interesting(string::in,
    maybe_constant::in, string::in, int::in, list(univ)::in,
    io::di, io::uo) is det.

write_deconstruct_results_if_interesting(Desc, MaybeConstant,
        Functor, Arity, ArgUnivs, !IO) :-
    ( if
        MaybeConstant = constant(ConstantFunctor),
        Functor = ConstantFunctor,
        Arity = 0,
        ArgUnivs = []
    then
        % The result of the deconstruct operation is exactly what we expected.
        % The test_deconstruct_functor predicate has already printed
        % the functor and the arity, so there is nothing interesting for us
        % to print here.
        true
    else
        io.format("%s\n", [s(Desc)], !IO),
        io.format("functor %s arity %d ", [s(Functor), i(Arity)], !IO),
        io.write_string("[", !IO),
        io.write_list(ArgUnivs, ", ", write_arg_univ, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred write_arg_univ(univ::in, io::di, io::uo) is det.

write_arg_univ(Univ, !IO) :-
    io.stdout_stream(Stdout, !IO),
    stream.string_writer.write_univ(Stdout, Univ, !IO).

%---------------------------------------------------------------------------%
