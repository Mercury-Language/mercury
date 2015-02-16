%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module export_before_func.

:- interface.

:- pred foo is det.

:- implementation.

:- import_module maybe.
:- import_module string.

foo.

:- pragma foreign_export("C",
    return_yes(in) = out,
"
    EXPORTED_return_yes
").

:- func return_yes(string) = maybe(string).

return_yes(Str) = yes(Str).
