%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
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
    ).
