%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack_args_intermod2.
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
    ( I = 0 -> ant
    ; I = 10 -> bat
    ; I = 20 -> cat
    ; I = 30 -> dog
    ; I = 40 -> eel
    ; I = 50 -> fox
    ; I = 60 -> gnu
    ;           hog
    ).

write_struct(struct(A, B, I, S, C, D, E, F), !IO) :-
    write_animal(A, !IO), write_string(", ", !IO),
    write_animal(B, !IO), write_string(", ", !IO),
    write_int(I, !IO),    write_string(", ", !IO),
    write_string(S, !IO), write_string(", ", !IO),
    write_animal(C, !IO), write_string(", ", !IO),
    write_animal(D, !IO), write_string(", ", !IO),
    write_animal(E, !IO), write_string(", ", !IO),
    write_animal(F, !IO).

write_animal(Animal, !IO) :-
    write(Animal, !IO).
