%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A tricky situation for the direct argument type representation optimisation.
%

:- module direct_arg_intermod.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module direct_arg_intermod_helper_1.

%---------------------------------------------------------------------------%

main(!IO) :-
    M1 = mk_maybe_inline(one, 1),
    write_maybe_inline(M1, !IO),
    write_maybe_no_inline(M1, !IO),

    M2 = mk_maybe_no_inline(one, 2),
    write_maybe_inline(M2, !IO),
    write_maybe_no_inline(M2, !IO).

:- func one = int.
:- pragma no_inline(one/0).

one = 1.
