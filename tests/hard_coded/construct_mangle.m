%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test construction of name mangled functors (e.g. on Java).
%---------------------------------------------------------------------------%

:- module construct_mangle.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module construct.
:- import_module type_desc.
:- import_module univ.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_type(type_of(_ : requires_mangling), !IO),
    test_type(type_of(_ : '$singleton'(int)), !IO).

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
    ;       force_non_enum(int)
    ;       requires_mangling       % functor has same name/arity as type
    ;       requires_mangling(int). % same name, different arity

:- type '$singleton'(T)
    --->    '$singleton'(T).

% XXX the Java backend currently generates invalid code for this type
% :- type '$blah'
%     --->    '$blah'(int)
%     ;       '$blah2'.

:- pred test_type(type_desc::in, io::di, io::uo) is det.

test_type(Type, !IO) :-
    ( NumFunctors = num_functors(Type) ->
        list.foldl(test_functor(Type), 0 .. NumFunctors - 1, !IO)
    ;
        io.write_string("failed\n", !IO)
    ),
    io.write_string("----\n", !IO).

:- pred test_functor(type_desc::in, int::in, io::di, io::uo) is det.

test_functor(Type, FunctorNumber, !IO) :-
    ( if get_functor(Type, FunctorNumber, Name, Arity, _ArgTypes) then
        % Assume that any arguments are ints.
        ArgUnivs = list.map(int_univ, 1 .. Arity),
        ( if
            find_functor(Type, Name, Arity, FunctorNumber, _),
            Univ = construct(Type, FunctorNumber, ArgUnivs)
        then
            io.write(Univ, !IO),
            io.nl(!IO)
        else
            io.write_string("failed FunctorNumber = ", !IO),
            io.write_int(FunctorNumber, !IO),
            io.nl(!IO)
        )
    else
        io.write_string("failed FunctorNumber = ", !IO),
        io.write_int(FunctorNumber, !IO),
        io.nl(!IO)
    ).

:- func int_univ(int) = univ.

int_univ(I) = univ(I).
