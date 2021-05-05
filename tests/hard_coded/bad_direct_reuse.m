%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.
%---------------------------------------------------------------------------%

:- module bad_direct_reuse.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- type sym_name
    --->    unqualified(string)
    ;       qualified(sym_name, string).

main(!IO) :-
    SymName0 = mk,
    (
        SymName0 = qualified(_, Unqual)
    ;
        SymName0 = unqualified(Unqual)
    ),
    % Bug: this reused the space for SymName0 even when
    % SymName0 = unqualified(_), which is not wide enough.
    SymName = qualified(mk, Unqual),
    io.write(SymName, !IO),
    io.nl(!IO).

:- func mk = (sym_name::uo).
:- pragma no_inline(func(mk/0)).

mk = SymName :-
    copy(unqualified("builder"), SymName).
