%-----------------------------------------------------------------------------%

:- module pack_args.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
                animal, animal,                 % word 0
                int,                            % word 1
                string,                         % word 2
                animal, animal, animal, animal  % word 3
            ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    Static = struct(ant, bat, 1000, "string", cat, dog, eel, fox),
    write_struct(Static, !IO),
    io.nl(!IO),

    Dynamic = struct(ani(pug), ani(owl), 1000, "string",
        ani(newt), ani(moa), lark, jay),
    write_struct(Dynamic, !IO),
    io.nl(!IO),

    Dynamic2 = struct(ani(pug), _, _, _, _, _, _, _),
    Dynamic2 = struct(_, ani(owl), _, _, _, _, _, _),
    Dynamic2 = struct(_, _, 1000, _, _, _, _, _),
    Dynamic2 = struct(_, _, _, "string", _, _, _, _),
    Dynamic2 = struct(_, _, _, _, ani(newt), moa, lark, _),
    Dynamic2 = struct(_, _, _, _, _, _, _, jay),
    write_struct(Dynamic2, !IO),
    io.nl(!IO).

:- func ani(animal) = animal.
:- pragma no_inline(ani/1).

ani(X) = X.

:- pred write_struct(struct::in, io::di, io::uo) is det.
:- pragma no_inline(write_struct/3).

write_struct(struct(A, B, I, S, C, D, E, F), !IO) :-
    write_animal(A, !IO), write_string(", ", !IO),
    write_animal(B, !IO), write_string(", ", !IO),
    write_int(I, !IO),    write_string(", ", !IO),
    write_string(S, !IO), write_string(", ", !IO),
    write_animal(C, !IO), write_string(", ", !IO),
    write_animal(D, !IO), write_string(", ", !IO),
    write_animal(E, !IO), write_string(", ", !IO),
    write_animal(F, !IO).

:- pred write_animal(animal::in, io::di, io::uo) is det.

write_animal(Animal, !IO) :-
    write(Animal, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
