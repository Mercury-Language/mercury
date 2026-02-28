%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%
%
% Top-level of the curses binding.
%
% Please note that this is still a partial binding; it does not provide
% complete curses functionality.
%
% Major things this binding implements:
%
% - Creating, destroying, clearing, raising, and lowering of arbitary windows.
% - Scrolling.
% - Setting colour on a character by character basis.
%
% See the man pages for ncurses for detailed information about using the
% curses libraries.
%
%---------------------------------------------------------------------------%

:- module mcurses.
:- interface.

:- include_module basics.
:- include_module misc.
:- include_module user.

%---------------------------------------------------------------------------%
:- end_module mcurses.
%---------------------------------------------------------------------------%
