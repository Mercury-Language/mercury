%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we can write out and read back in `no_sharing', `unknown_sharing'
% and `sharing' annotations on foreign_procs.
%
% This test was originally called intermod_user_sharing.
%

:- module sharing_in_opt.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module sharing_in_opt_helper_1.

%---------------------------------------------------------------------------%

main(!IO) :-
    p_no_sharing(!IO),
    p_unknown_sharing("bar", Bar),
    io.write(Bar, !IO),
    p_sharing(1, "foo", Array),
    io.write(Array, !IO).
