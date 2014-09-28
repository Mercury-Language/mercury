%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
:- module net.

:- interface.

:- include_module netdb.
:- include_module sockets.
:- include_module tcp.
:- include_module types.

:- pred version(string::out) is det.

:- implementation.

:- include_module errno.

version("DEV").
