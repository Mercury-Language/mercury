%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_type_class_constraint_intermodule.
:- interface.

:- import_module io.

:- typeclass output(S, U) <= (U -> S) where [
    pred output_int(S::in, int::in, U::di, U::uo) is det
].

:- instance output(io.text_output_stream, io.state).

:- pred bad_pred(S::in, int::in, U::di, U::uo) is det <= output(U).

:- type bad_type_ctor
    --->    some [U] f(U) => output(U).

%---------------------------------------------------------------------------%

:- implementation.

:- instance output(io.text_output_stream, io.state) where [
    pred(do_stuff/4) is io.write_string
].

bad_pred(S, N, !U) :-
    output_int(S, N, !U).

%---------------------------------------------------------------------------%

