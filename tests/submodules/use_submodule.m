%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we can use both nested and separate sub-modules.

:- module use_submodule.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module include_parent.
:- import_module include_parent.nested.
:- import_module include_parent.separate.
:- import_module include_parent.separate.nested.
:- use_module    include_parent.separate2.
:- use_module    include_parent.separate2.nested.

main(!IO) :-
    include_parent.hello(!IO),
    include_parent.nested.hello(!IO),
    nested.hello(!IO),
    include_parent.separate.hello(!IO),
    separate.hello(!IO),
    include_parent.separate.hello2(!IO),
    separate.hello2(!IO),
    hello2(!IO),
    hello3(!IO),
    include_parent.separate2.hello(!IO),
    include_parent.separate2.nested.hello(!IO).
