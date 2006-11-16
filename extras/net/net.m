%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
:- module net.
:- interface.
:- pred version(string::out) is det.
:- implementation.
:- import_module sockets, tcp.
version("DEV").
