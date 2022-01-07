Networking Library
==================

This library is inteded to provide support for networking with both
1) a simple binding around the BSD sockets interface, and
2) a higher-level set of predicates and functions for common patterns.
This is a work in progress, and many things are unimplemented.

Copying
-------

Copyright (C) 2014 The Mercury Team
This file may only be copied under the terms of the GNU Library General
Public Licence - see the file COPYING in the Mercury distribution.

TODO
----

    + Currently no method is provided to connect these sockets to the
      standard libraries IO or stream modules.
    + sendmsg()/recvmsg().
    + Cross platform functionality (Only tested on Linux so far).
    + Non blocking support.
    + Improved name lookup / reverse lookup
    + Network layer:

        + IPv6
        + Unix domain sockets

    + Protocol layer:

        + UDP
        + SCTP

    + High level interface


Modules
-------

    + net.

        Main library module

    + net.types.

        Common datatypes

    + net.sockets.

        Sockets predicates.  This includes the most fundermental operations
        such as listen/connect.

    + net.netdb.

        Network name lookups.

    + net.tcp

        Deprecated module.

    + net.errno

        Internal module with errno functionality.

    + echo

        An example echo server (incomplete).

