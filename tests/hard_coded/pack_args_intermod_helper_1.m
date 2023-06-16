%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack_args_intermod_helper_1.
:- interface.

:- import_module io.

:- type animal.

:- type struct
    --->    struct(
                animal, animal,                 % word 0
                int,                            % word 1
                string,                         % word 2
                animal, animal, animal, animal  % word 3
            ).

:- func ani(int) = animal.

:- pred write_struct(struct::in, io::di, io::uo) is det.

:- pred write_animal(animal::in, io::di, io::uo) is det.

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
    ;       hog.        % 7

ani(I) =
    ( if I = 0 then ant
    else if I = 10 then bat
    else if I = 20 then cat
    else if I = 30 then dog
    else if I = 40 then eel
    else if I = 50 then fox
    else if I = 60 then gnu
    else hog
    ).

write_struct(struct(A, B, I, S, C, D, E, F), !IO) :-
    write_animal(A, !IO),    io.write_string(", ", !IO),
    write_animal(B, !IO),    io.write_string(", ", !IO),
    io.write_int(I, !IO),    io.write_string(", ", !IO),
    io.write_string(S, !IO), io.write_string(", ", !IO),
    write_animal(C, !IO),    io.write_string(", ", !IO),
    write_animal(D, !IO),    io.write_string(", ", !IO),
    write_animal(E, !IO),    io.write_string(", ", !IO),
    write_animal(F, !IO).

write_animal(Animal, !IO) :-
    io.write(Animal, !IO).
