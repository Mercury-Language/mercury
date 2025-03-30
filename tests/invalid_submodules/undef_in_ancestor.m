%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Check that we do *not* produce an error message component stating that
% "No module named `undef_in_ancestor' has been imported" when compiling
% the child submodule.
%
% This test case is currently not enabled due to the lack of a compatible
% test setup in the invalid test directories. To reproduce the diagnostic
% it is intended to test for, do:
%
%     $ mmc --generate-dependencies undef_in_ancestor
%     $ mmc --make-short-interface undef_in_ancestor
%     $ mmc --make-private-interface undef_in_ancestor
%     $ mmc --make-interface undef_in_ancestor
%
%---------------------------------------------------------------------------%

:- module undef_in_ancestor.
:- interface.

:- import_module list.
:- import_module io.

:- type use_list == list(int).
:- type use_io == io.

%---------------------------------------------------------------------------%

    :- module child.
    :- interface.

    :- pred test(list(int)::in(list(undef_in_ancestor.foo)), io::di, io::uo)
        is det.

    :- end_module child.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

    :- module undef_in_ancestor.child.
    :- implementation.

    test(_, !IO).

    :- end_module child.

%---------------------------------------------------------------------------%
:- end_module undef_in_ancestor.
%---------------------------------------------------------------------------%
