%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack_args_float.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type animal
    --->    ant         % 0
    ;       bat         % 1
    ;       cat         % 2
    ;       dog         % 3
    ;       eel         % 4
    ;       fox         % 5
    ;       gnu         % 6
    ;       hog         % 7
    ;       ibis        % 8
    ;       jay         % 9
    ;       kea         % 10
    ;       lark        % 11
    ;       moa         % 12
    ;       newt        % 13
    ;       owl         % 14
    ;       pug.        % 15

:- type struct
    --->    struct(
                animal, animal,                 % word 0    | word 0
                float,                          % word 1    | word 1+2
                float,                          % word 2    | word 3+4
                animal, animal, animal, animal  % word 3    | word 5
            ).

%---------------------------------------------------------------------------%

main(!IO) :-
    Static = struct(ant, bat, 1.1, 2.2, cat, dog, eel, fox),
    write_struct(Static, !IO),
    io.nl(!IO),

    Dynamic = struct(ani(pug), ani(owl), 101.101, 202.202,
        ani(newt), ani(moa), lark, jay),
    write_struct(Dynamic, !IO),
    io.nl(!IO).

:- func ani(animal) = animal.
:- pragma no_inline(ani/1).

ani(X) = X.

:- pred write_struct(struct::in, io::di, io::uo) is det.
:- pragma no_inline(write_struct/3).

write_struct(struct(A, B, X, Y, C, D, E, F), !IO) :-
    write_animal(A, !IO), write_string(", ", !IO),
    write_animal(B, !IO), write_string(", ", !IO),
    write_float(X, !IO),  write_string(", ", !IO),
    write_float(Y, !IO),  write_string(", ", !IO),
    write_animal(C, !IO), write_string(", ", !IO),
    write_animal(D, !IO), write_string(", ", !IO),
    write_animal(E, !IO), write_string(", ", !IO),
    write_animal(F, !IO).

:- pred write_animal(animal::in, io::di, io::uo) is det.

write_animal(Animal, !IO) :-
    io.write(Animal, !IO).
