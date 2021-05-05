%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test switches on foreign enum types.
%
%---------------------------------------------------------------------------%

:- module foreign_enum_switch.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    p(foo, Out1),
    io.write_int(Out1, !IO),
    io.nl(!IO),
    p(bar, Out2),
    io.write_int(Out2, !IO),
    io.nl(!IO),
    p(baz, Out3),
    io.write_int(Out3, !IO),
    io.nl(!IO).

:- pragma no_inline(p/2).

:- pred p(t::in, int::out) is det.

p(foo, 42).
p(bar, 43).
p(baz, 44).

:- type t
    --->    foo
    ;       bar
    ;       baz.

:- pragma foreign_decl("C",
"
    #define CONSTANT1 300
    #define CONSTANT2 400
    #define CONSTANT3 500
").

:- pragma foreign_enum("C", t/0,
    [
        foo - "CONSTANT1",
        bar - "CONSTANT2",
        baz - "CONSTANT3"
    ]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
