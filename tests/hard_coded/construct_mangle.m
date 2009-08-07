%-----------------------------------------------------------------------------%
% Test construction of name mangled functors (e.g. on Java).

:- module construct_mangle.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module construct.
:- import_module type_desc.
:- import_module univ.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    Type = type_of(_ : requires_mangling),
    ( NumFunctors = num_functors(Type) ->
        list.foldl(test_functor(Type), 0 .. NumFunctors - 1, !IO)
    ;
        io.write_string("failed\n", !IO)
    ).

    % This must not be an enumeration as enumerations on Java are represented
    % in a way such that name mangling doesn't apply.
    %
:- type requires_mangling
    --->    (' ')
    ;       ('!')
    ;       ('"')
    ;       ('#')
    ;       ('$')
    ;       ('%')
    ;       ('&')
    ;       ('''')
    ;       ('(')
    ;       (')')
    ;       ('*')
    ;       ('+')
    ;       (',')
    ;       ('-')
    ;       ('.')
    ;       ('/')
    ;       ('0')
    ;       ('1')
    ;       ('2')
    ;       ('3')
    ;       ('4')
    ;       ('5')
    ;       ('6')
    ;       ('7')
    ;       ('8')
    ;       ('9')
    ;       (':')
    ;       (';')
    ;       ('<')
    ;       ('=')
    ;       ('>')
    ;       ('?')
    ;       ('@')
    ;       ('[')
    ;       ('\\')
    ;       (']')
    ;       ('^')
    ;       ('_')
    ;       ('`')
    ;       ('{')
    ;       ('|')
    ;       ('}')
    ;       ('~')
    ;       ('\\=')
    ;       ('>=')
    ;       ('=<')
    ;       ('{}')
    ;       ('[|]')
    ;       ('[]')
    ;       ('abc~!@#$%^&*()_+|xyz')
    ;       f_this_also_requires_mangling
    ;       force_non_enum(int).

:- pred test_functor(type_desc::in, int::in, io::di, io::uo) is det.

test_functor(Type, FunctorNumber, !IO) :-
    ( get_functor(Type, FunctorNumber, Name, Arity, ArgTypes) ->
        (
            ArgTypes = [],
            (
                find_functor(Type, Name, Arity, FunctorNumber, _),
                Univ = construct(Type, FunctorNumber, [])
            ->
                io.write(Univ, !IO),
                io.nl(!IO)
            ;
                io.write_string("failed FunctorNumber = ", !IO),
                io.write_int(FunctorNumber, !IO),
                io.nl(!IO)
            )
        ;
            ArgTypes = [_ | _]
        )
    ;
        io.write_string("failed FunctorNumber = ", !IO),
        io.write_int(FunctorNumber, !IO),
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
