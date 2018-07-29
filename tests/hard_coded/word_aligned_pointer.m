%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test foreign type assertion `word_aligned_pointer'.
%---------------------------------------------------------------------------%

:- module word_aligned_pointer.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module word_aligned_pointer_2.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Construct values in different modules.
    X = yes(make_foo),
    Y = word_aligned_pointer_2.make_bar,

    % Deconstruct in this module.
    word_aligned_pointer.write_bar(X, !IO),
    word_aligned_pointer.write_bar(Y, !IO),

    % Deconstruct in the other module.
    word_aligned_pointer_2.write_bar(X, !IO),
    word_aligned_pointer_2.write_bar(Y, !IO).

%---------------------------------------------------------------------------%

:- pred write_bar(bar::in, io::di, io::uo) is det.

write_bar(Bar, !IO) :-
    (
        Bar = yes(Foo),
        io.format("yes(0x%x)\n", [i(get_foo(Foo))], !IO)
    ;
        Bar = no,
        io.write_string("no", !IO)
    ).

%---------------------------------------------------------------------------%
