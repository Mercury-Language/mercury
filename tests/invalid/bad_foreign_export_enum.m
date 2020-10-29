%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

:- module bad_foreign_export_enum.
:- interface.

:- type fruit
    --->    orange
    ;       lemon
    ;       apple.

:- implementation.

    % Wrong number of arguments.
    %
:- pragma foreign_export_enum("C").

    % Invalid foreign language.
    %
:- pragma foreign_export_enum("InvalidLanguage", fruit/0).

    % Second arg is not name / arity.
    %
:- pragma foreign_export_enum("C", fruit).

:- pragma foreign_export_enum(
    "InvalidLanguage",
    fruit,
    1234,
    5678
).

:- pragma foreign_export_enum(erlang, fruit/0).
