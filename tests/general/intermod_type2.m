%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module intermod_type2.

:- interface.

:- type win.

:- type wopt
    --->    border
    ;       title(string).

:- implementation.

%---------------------------------------------------------------------------%

:- import_module array.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.
:- import_module store.
:- import_module string.

:- type my_store_type
    --->    my_store_type.

:- type curse == store(my_store_type).

:- type win == store_mutvar(window, my_store_type).

:- type window
    --->    win(
                win,            % parent
                int,            % width
                int,            % height
                list(wopt),
                array(char),    % contents
                list(child),    % visible
                list(child)     % hidden
            ).

:- type child
    --->    child(
                int,        % x
                int,        % y
                win
            ).

:- type cursor
    --->    cursor(int, int).

%---------------------------------------------------------------------------%
