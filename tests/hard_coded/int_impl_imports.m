%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module int_impl_imports.
:- interface.

:- import_module io.
:- use_module int_impl_imports_2.

:- pred main(io::di, io::uo) is det.

% This tests whether we can use bar in the interface when module qualified.
:- type foo == int_impl_imports_2.bar.
:- pred write_foo(foo::in, io::di, io::uo) is det.

:- implementation.

:- import_module int_impl_imports_2.

main(!IO) :-
    % This tests whether we can refer to one_bar in the implementation section
    % *without* module qualification.
    write_foo(add_bars(one_bar, one_bar), !IO),
    io.nl(!IO).

write_foo(Foo, !IO) :-
    io.write_int(Foo, !IO).
