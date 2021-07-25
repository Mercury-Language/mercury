%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for extracting type_infos from typeclass infos with
% extra instance arguments.

:- module extract_typeinfo2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module enum.
:- import_module int.
:- import_module list.

:- type xvar(T)
    --->    xvar(int).

:- type dummy
    --->    dummy.

:- instance enum(xvar(_)).

:- instance enum(xvar(_)) where [
    to_int(xvar(I)) = I,
    from_int(I) = xvar(I)
].

main(!IO) :-
    comp(R, xvar(63), xvar(61) : xvar(dummy)),
    io.print_line(R, !IO).

:- pred comp(comparison_result::out, T::in, T::in) is det <= enum(T).
:- pragma no_inline(comp/3).

comp(R, A, B) :-
    % The Erlang backend was extracting the 'dummy' type_info rather than the
    % 'xvar(int)' typeinfo, therefore comparing A and B as dummy values, i.e.
    % always equal.
    compare(R, A, B).
