%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module warn_stubs.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.
:- impure pred foo is det.

:- implementation.

:- pred main1(io::di, io::uo) is det.
:- pred main2(io::di, io::uo) is det.

main1(!IO) :-
    main2(!IO).
