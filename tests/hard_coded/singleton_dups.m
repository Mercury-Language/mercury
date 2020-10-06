%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Regression test for a problem in Mercury 11.07 where checking whether
% a set represented by an unordered list was singleton didn't account for
% the representation containing duplicate elements.
%

:- module singleton_dups.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module set_unordlist.

main(!IO) :-
    some [!Set] (
        set_unordlist.init(!:Set),
        set_unordlist.insert(1, !Set),
        set_unordlist.insert(1, !Set),
        set_unordlist.insert(1, !Set),
        ( if set_unordlist.singleton_set(X, !.Set) then
            io.write_string("Singleton with element ", !IO),
            io.write_int(X, !IO),
            io.nl(!IO)
        else
            io.write_string("Not a singleton set\n", !IO)
        )
    ),

    % Check that the implied (in, in) is semidet mode works correctly.
    %
    some [!Set2] (
        set_unordlist.init(!:Set2),
        set_unordlist.insert(2, !Set2),
        set_unordlist.insert(2, !Set2),
        set_unordlist.insert(2, !Set2),
        ( if set_unordlist.singleton_set(2, !.Set2) then
            io.write_string("(in, in) test passed.\n", !IO)
        else
            io.write_string("(in, in) test FAILED.\n", !IO)
        )
    ).
