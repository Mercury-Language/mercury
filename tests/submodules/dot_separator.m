%---------------------------------------------------------------------------%
% dot_separator.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Jan 17 14:10:30 EST 2003
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% This tests that the compiler correctly quotes '.' where necessary
% as a module separator in the output files.
%
%---------------------------------------------------------------------------%

:- module dot_separator.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

    :- module foo_1.
    :- interface.

    :- type foo_1.bar ---> bar.
    :- type foo_1.'234' ---> foo_1.'234'.
    :- type foo_1.'345bar' ---> '345bar'.
    :- type foo_1.'.bar' ---> '.bar'.

    :- end_module foo_1.

    % The submodules below are commented out because the compiler tools
    % really don't like symbols that contain `.'s.

%     :- module 'foo.'.
%     :- interface.
%
%     :- type 'foo.'.baz ---> baz.
%     :- type 'foo.'.'234' ---> 'foo.'.'234'.
%     :- type 'foo.'.'345baz' ---> '345baz'.
%     :- type 'foo.'.'.baz' ---> '.baz'.
%
%     :- end_module 'foo.'.

%     :- module ('.').
%     :- interface.
%
%     :- type ('.').quux ---> quux.
%     :- type ('.').'456' ---> '456'.
%     :- type ('.').'567quux' ---> '567quux'.
%     :- type ('.').'.quux' ---> '.quux'.
%
%     :- end_module ('.').

:- implementation.

main(!IO) :-
    io.write_string("All's well that ends well.\n", !IO).
