%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
%
% The Mercury compiler of 26/10/1999 failed the first part of this test
% (the part concerned with the implied mode of append).
%
% The Mercury compiler of 30/3/2000 failed the second part of this test
% (the part with comparison_test1), due to overeager specialization of
% comparisons involving ENUM_USEREQ types.
%
% The Mercury compiler still fails the third part of this test (the part
% with comparison_test2) with --no-special-preds, because the exception
% is not propagated across MR_call_engine properly. (It should work fine
% with the default --special-preds.)

:- module user_defined_equality.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module list.
:- import_module std_util.
:- import_module exception.

:- type foo
    --->    bar
    ;       baz
    where equality is foo_equal.

foo_equal(_, _) :-
    semidet_succeed.

main(!IO) :-
    ( if append([bar], [baz], [baz, bar]) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    perform_comparison_test(comparison_test1, !IO),
    perform_comparison_test(comparison_test2, !IO).

:- pred perform_comparison_test(pred(T), io.state, io.state).
:- mode perform_comparison_test(pred(out) is det, di, uo) is cc_multi.

perform_comparison_test(Test, !IO) :-
    try(Test, TryResult),
    (
        TryResult = failed,
        io.write_string("failed\n", !IO)
    ;
        TryResult = succeeded(Result),
        io.write_string("succeeded: ", !IO),
        io.write_line(Result, !IO)
    ;
        TryResult = exception(Exception),
        io.write_string("threw exception: ", !IO),
        io.write_line(Exception, !IO)
    ).

:- pred comparison_test1(comparison_result::out) is det.

comparison_test1(R) :-
    compare(R, bar, baz).

:- pred comparison_test2(comparison_result::out) is det.

comparison_test2(R) :-
    compare(R, [bar], [baz]).
