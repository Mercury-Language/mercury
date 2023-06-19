%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module use_submodule_helper_1__use_submodule_helper_3.

:- interface.

% The parent module includes io.

:- pred hello(io::di, io::uo) is det.

    :- module use_submodule_helper_1__use_submodule_helper_3__nested.
    :- interface.
    :- pred hello(io::di, io::uo) is det.
    :- end_module use_submodule_helper_1__use_submodule_helper_3__nested.

:- implementation.

hello(!IO) :-
    io.write_string(
        "use_submodule_helper_1__use_submodule_helper_3: hello\n", !IO).

    :- module use_submodule_helper_1__use_submodule_helper_3__nested.
    :- implementation.
    hello(!IO) :-
        io.write_string(
            "use_submodule_helper_1__use_submodule_helper_3__nested: hello\n",
            !IO).
    :- end_module use_submodule_helper_1__use_submodule_helper_3__nested.
