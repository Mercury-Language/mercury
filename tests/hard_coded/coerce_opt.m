%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_opt.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module type_desc.

:- import_module coerce_opt_2.

main(!IO) :-
    Xs = [1, 2, 3],
    io.print_line(type_of(Xs), !IO),
    io.print_line(Xs, !IO),

    Ys = to_list(Xs),
    io.print_line(type_of(Ys), !IO),
    io.print_line(Ys, !IO).
