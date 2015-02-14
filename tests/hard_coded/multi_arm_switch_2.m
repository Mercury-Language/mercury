%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module multi_arm_switch_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module require.
:- import_module solutions.

:- type t
    --->    f1
    ;       f2
    ;       f3
    ;       f4(int)
    ;       f5(int)
    ;       f6(int)
    ;       f7(int)
    ;       f8(int)
    ;       f9(int)
    ;       f10(int)
    ;       f11(int)
    ;       f12(int)
    ;       f13(int).

main(!IO) :-
    test(f1, !IO),
    test(f2, !IO),
    test(f3, !IO),
    test(f4(104), !IO),
    test(f5(105), !IO),
    test(f6(106), !IO),
    test(f7(107), !IO),
    test(f8(108), !IO),
    test(f9(109), !IO),
    test(f10(110), !IO),
    test(f11(111), !IO),
    test(f12(112), !IO),
    test(f13(113), !IO).

:- pred test(t::in, io::di, io::uo) is det.

test(X, !IO) :-
    p(X, N),
    io.write(X, !IO),
    io.write_string(" -> ", !IO),
    io.write_int(N, !IO),
    io.nl(!IO).

:- pred p(t::in, int::out) is det.

p(X, N) :-
    (
        % This tests the sharing of code between functors with the same primary
        % tag and different local secondary tags.
        ( X = f1
        ; X = f2
        ),
        N = 1
    ;
        % The presence of f3 here tests the proper handling of primary tags
        % with local secondary tags, not all of which are in the first switch
        % arm.
        % The presence of f4 and f5 tests the sharing of code between functors
        % with their own primary tags.
        ( X = f3
        ; X = f4(_)
        ; X = f5(_)
        ),
        N = 2
    ;
        % The presence of f10 and f11 tests the sharing of code between
        % functors with the same primary tag and different remote secondary
        % tags. (On 64 bit machines, the other functors here will have their
        % own primary tags.)
        ( X = f6(_)
        ; X = f7(_)
        ; X = f8(_)
        ; X = f9(_)
        ; X = f10(_)
        ; X = f11(_)
        ),
        N = 3
    ;
        % The presence of f12 and f13 here tests the proper handling of
        % primary tags with remote secondary tags, not all of which are
        % in the third switch arm.
        ( X = f12(_)
        ; X = f13(_)
        ),
        N = 4
    ).
