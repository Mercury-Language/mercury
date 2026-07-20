%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module accessibility_t3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

    :- module child.
    :- interface.

    :- type dummy ---> dummy.

        :- module grandchild.
        :- interface.

        :- type dummy ---> dummy.

        :- end_module grandchild.

    :- end_module child.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module accessibility_t3.sub2.

    :- module child.
    :- implementation.

    :- use_module accessibility_t3.sub2.

    % The `use_module' in this module means foo must be qualified here.
    :- pred child_pred(accessibility_t3.sub2.foo::in) is det.

    child_pred(_).

        :- module grandchild.
        :- implementation.

        % This sees both the `import_module' and `use_module' from ancestors,
        % so foo can be unqualified here.
        :- pred grandchild_pred(foo::in) is det.

        grandchild_pred(_).

        :- end_module grandchild.

    :- end_module child.

%---------------------------------------------------------------------------%

    :- module sub2.
    :- interface.

    :- type foo ---> foo.

    :- end_module sub2.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("Hello.\n", !IO).
