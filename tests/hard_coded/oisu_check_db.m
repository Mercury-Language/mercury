%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module oisu_check_db.
:- interface.

:- import_module bool.

:- type db.

:- pragma oisu(db/0,
        creators([pred(create_db1/1), pred(create_db2/2)]),
        mutators([pred(mutate_db1/3), pred(mutate_db2/3)]),
        destructors([pred(destroy_db/3)])
    ).

:- pred create_db1(db::out) is det.
:- pred create_db2(int::in, db::out) is det.

:- pred mutate_db1(bool::in, db::in, db::out) is det.
:- pred mutate_db2(int::in, db::in, db::out) is det.

:- pred destroy_db(db::in, bool::out, int::out) is det.

:- implementation.

:- type db
    --->    db(bool, int).

create_db1(DB) :-
    DB = db(no, 0).

create_db2(N, DB) :-
    DB = db(no, N).

mutate_db1(B, !DB) :-
    !.DB = db(_, N),
    !:DB = db(B, N).

mutate_db2(N, !DB) :-
    !.DB = db(B, _),
    !:DB = db(B, N).

destroy_db(DB, B, N) :-
    DB = db(B, N).
