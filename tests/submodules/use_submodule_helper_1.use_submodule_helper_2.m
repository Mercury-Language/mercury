%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module use_submodule_helper_1__use_submodule_helper_2.

:- interface.

% The parent module includes io.

:- pred hello(io::di, io::uo) is det.

:- pred hello2(io::di, io::uo) is det.

    :- module use_submodule_helper_1__use_submodule_helper_2__nested.
    :- interface.
    :- pred hello3(io::di, io::uo) is det.
    :- end_module use_submodule_helper_1__use_submodule_helper_2__nested.

:- implementation.

hello(!IO) :-
    io.write_string(
        "use_submodule_helper_1__use_submodule_helper_2: hello\n", !IO).

hello2(!IO) :-
    io.write_string(
        "use_submodule_helper_1__use_submodule_helper_2: hello2\n", !IO).

    :- module use_submodule_helper_1__use_submodule_helper_2__nested.
    :- implementation.

    hello3(!IO) :-
        io.write_string(
            "use_submodule_helper_1__use_submodule_helper_2__nested: hello\n",
            !IO).
    :- end_module use_submodule_helper_1__use_submodule_helper_2__nested.
