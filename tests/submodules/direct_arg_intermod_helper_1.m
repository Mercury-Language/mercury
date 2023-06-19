%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_intermod_helper_1.
:- interface.

:- import_module io.

:- type maybe.

:- func mk_maybe_inline(int, int) = maybe.

:- func mk_maybe_no_inline(int, int) = maybe.

:- pred write_maybe_inline(maybe::in, io::di, io::uo) is det.

:- pred write_maybe_no_inline(maybe::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module direct_arg_intermod_helper_2.

:- type maybe == inner_maybe.

    % inner_maybe is not exported so the direct arg represention should be safe
    % for yes/1.  But when it is opt-exported, the importing module also needs
    % to use the direct arg represention, without necessarily knowing the
    % definition of foo.
    %
:- type inner_maybe
    --->    no
    ;       yes(foo).

%---------------------------------------------------------------------------%

:- pragma inline(mk_maybe_inline/2).

mk_maybe_inline(A, B) = yes(foo(A, B)).

:- pragma no_inline(mk_maybe_no_inline/2).

mk_maybe_no_inline(A, B) = mk_maybe_inline(A, B).

:- pragma inline(write_maybe_inline/3).

write_maybe_inline(M, !IO) :-
    (
        M = no,
        write_string("no\n", !IO)
    ;
        M = yes(Foo),
        write_string("yes(", !IO),
        write(Foo, !IO),
        write_string(")\n", !IO)
    ).

:- pragma no_inline(write_maybe_no_inline/3).

write_maybe_no_inline(M, !IO) :-
    write_maybe_inline(M, !IO).
