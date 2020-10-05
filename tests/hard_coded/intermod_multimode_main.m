%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module intermod_multimode_main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module intermod_multimode.

:- pragma promise_pure(main/2).
main(!IO) :-
    In = 42,
    In2 = In,       % This line (and the use of `In2' below,
                    % rather than `In') is needed to avoid
                    % triggering an unrelated bug -- see
                    % tests/valid/mode_selection.m.

    % test pure functions
    print(func0, !IO), nl(!IO),
    print(func1(In), !IO), nl(!IO),
    print(func1(_Out0), !IO), nl(!IO),
    print(func2(In, In2), !IO), nl(!IO),
    print(func2(In, _Out1), !IO), nl(!IO),
    print(func2(_Out2, In), !IO), nl(!IO),
    print(func2(_Out3, _Out4), !IO), nl(!IO),

    % test impure predicates
    impure test0,
    impure test1(In),
    impure test1(_Out10),
    impure test2(In, In),
    impure test2(In, _Out11),
    impure test2(_Out12, In),
    impure test2(_Out13, _Out14),

    get_determinism((pred(1::out) is semidet), Det),
    io.write_line(Det, !IO).
