%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% mmc --optimize-constructor-last-call lco_bug 
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%         Key Type: term.var(parse_tree.prog_data.prog_var_type)
%         Key Value: var(2)
%         Value Type: ll_backend.var_locn.var_state

:- module bug103.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type foo
    --->    nil
    ;       cons(int, foo).

main(!IO) :-
    mk([yes, yes, no], _, Foo),
    io.write(Foo, !IO),
    io.nl(!IO).

:- pred mk(list(bool)::in, int::out, foo::out) is det.

mk([], -1, nil).
mk([H | T], N, R) :-
    (
        H = yes,
        mk(T, N, RT),
        R = cons(N, RT)
    ;
        H = no,
        N = 42,
        R = nil
    ).
