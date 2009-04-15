% Type error where inferred type is ambiguous.  The error message used to
% contain the same expected type multiple times.

:- module type_error_ambiguous.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type t1
    --->    f
    ;       g.

:- type t2
    --->    f
    ;       g.

:- pred f(int::out, string::out) is det.

f(1, "s").

main(!IO) :-
    X = f,
    io.write_int(X, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
