%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%

:- module net.

:- interface.

:- include_module netdb.
:- include_module sockets.
:- include_module streams.
:- include_module tcp.
:- include_module types.

:- pred version(string::out) is det.

:- implementation.

:- include_module errno.
:- include_module getaddrinfo.

version("DEV").
