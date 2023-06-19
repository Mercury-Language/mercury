%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we can use both nested and separate sub-modules.
%

:- module use_submodule.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module use_submodule_helper_1.
:- import_module use_submodule_helper_1.nested.
:- import_module use_submodule_helper_1.use_submodule_helper_2.
:- import_module use_submodule_helper_1.use_submodule_helper_2.nested.
:- use_module    use_submodule_helper_1.use_submodule_helper_3.
:- use_module    use_submodule_helper_1.use_submodule_helper_3.nested.

main(!IO) :-
    use_submodule_helper_1.hello(!IO),
    use_submodule_helper_1.nested.hello(!IO),
    nested.hello(!IO),
    use_submodule_helper_1.use_submodule_helper_2.hello(!IO),
    use_submodule_helper_2.hello(!IO),
    use_submodule_helper_1.use_submodule_helper_2.hello2(!IO),
    use_submodule_helper_2.hello2(!IO),
    hello2(!IO),
    hello3(!IO),
    use_submodule_helper_1.use_submodule_helper_3.hello(!IO),
    use_submodule_helper_1.use_submodule_helper_3.nested.hello(!IO).
