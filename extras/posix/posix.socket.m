%------------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module posix:socket.
% main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%
:- module posix:socket.

:- interface.

:- import_module std_util.

:- type posix:socket:domain
	--->	unix
	;	inet
	.

:- type posix:socket:(type)
	--->	stream
	;	dgram
	;	raw
	;	seqpacket
	;	rdm
	.

:- type protocol
	--->	protocol(int)
	.

:- type sockaddr
	--->	inet(port, inet_addr)
	.

:- type port
	--->	port(int).

:- type inet_addr
	--->	inet_addr(int).

:- pred socket(domain, (type), protocol, posix:result(fd),
		io__state, io__state).
:- mode socket(in, in, in, out, di, uo) is det.

:- pred accept(fd, posix:result(fd), io__state, io__state).
:- mode accept(in, out, di, uo) is det.

:- pred bind(fd, sockaddr, posix:result, io__state, io__state).
:- mode bind(in, in, out, di, uo) is det.

:- pred connect(fd, sockaddr, posix:result, io__state, io__state).
:- mode connect(in, in, out, di, uo) is det.

:- pred listen(fd, int, posix:result, io__state, io__state).
:- mode listen(in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <string.h>
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <arpa/inet.h>
").

%------------------------------------------------------------------------------%

socket(Dom, Typ, protocol(Prot), Result) -->
	socket0(domain(Dom), type(Typ), Prot, FdNo),
	( { FdNo < 0 } ->
		errno(Err),
		{ Result = error(Err) }
	;
		{ Result = ok(fd(FdNo)) }
	).

:- pred socket0(int, int, int, int, io__state, io__state).
:- mode socket0(in, in, in, out, di, uo) is det.

:- pragma c_code(socket0(Dom::in, Typ::in, Prot::in, Fd::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Fd = socket(Dom, Typ, Prot);
	IO = IO0;
}").

:- func domain(domain) = int.
:- mode (domain(in) = out) is det.

:- pragma c_code(domain(D::in) = (V::out),
		[will_not_call_mercury, thread_safe], "{
	static int domain_values[] = {
		AF_UNIX, AF_INET
	};

	V = domain_values[D];
}").

:- func type(type) = int.
:- mode (type(in) = out) is det.

:- pragma c_code(type(T::in) = (V::out),
		[will_not_call_mercury, thread_safe], "{
	static int type_values[] = {
		SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET, SOCK_RDM
	};

	V = type_values[T];
}").

%------------------------------------------------------------------------------%

:- type sockaddr_ptr
	--->	sockaddr_ptr(c_pointer).

bind(Fd, SockAddr, Result) -->
	{ mksockaddr_struct(SockAddr, Ptr, Len) },
	bind0(Fd, Ptr, Len, Res0),
	( { Res0 = 0 } ->
		{ Result = ok }
	;
		errno(Errno),
		{ Result = error(Errno) }
	).

:- pred bind0(fd, sockaddr_ptr, int, int, io__state, io__state).
:- mode bind0(in, in, in, out, di, uo) is det.

:- pragma c_code(bind0(Fd::in, Addr::in, Len::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Res = bind(Fd, (struct sockaddr *) Addr, Len);
	IO = IO0;
}").

:- pred mksockaddr_struct(sockaddr, sockaddr_ptr, int).
:- mode mksockaddr_struct(in, out, out) is det.

mksockaddr_struct(inet(Port, Addr), Ptr, Len) :-
	mkinet_addr(Addr, Port, Ptr, Len).

:- pred mkinet_addr(inet_addr, port, sockaddr_ptr, int).
:- mode mkinet_addr(in, in, out, out) is det.

:- pragma c_code(mkinet_addr(A::in, P::in, Ptr::out, Len::out), 
		[will_not_call_mercury, thread_safe], "{
	struct sockaddr_in *ptr;

	incr_hp(Ptr, (1 + sizeof(struct sockaddr_in)/sizeof(Word)));
	ptr = (struct sockaddr_in *) Ptr;

	memset((void *) ptr, 0, sizeof(struct sockaddr_in));
	ptr->sin_family = AF_INET;
	ptr->sin_addr.s_addr = A;
	ptr->sin_port = htons(P);

	Len = sizeof(struct sockaddr_in);
}").

%------------------------------------------------------------------------------%

connect(Fd, SockAddr, Result) -->
	{ mksockaddr_struct(SockAddr, Ptr, Len) },
	connect0(Fd, Ptr, Len, Res),
	( { Res = 0 } ->
		{ Result = ok }
	;
		errno(Err),
		{ Result = error(Err) }
	).

:- pred connect0(fd, sockaddr_ptr, int, int, io__state, io__state).
:- mode connect0(in, in, in, out, di, uo) is det.

:- pragma c_code(connect0(Fd::in, Addr::in, Len::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Res = connect(Fd, (struct sockaddr *) Addr, Len);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

listen(Fd, N, Result) -->
	listen0(Fd, N, Res0),
	( { Res0 = 0 } ->
		{ Result = ok }
	;
		errno(Errno),
		{ Result = error(Errno) }
	).

:- pred listen0(fd, int, int, io__state, io__state).
:- mode listen0(in, in, out, di, uo) is det.

:- pragma c_code(listen0(Fd::in, N::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Res = listen(Fd, N);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

accept(Fd, Result) -->
	accept0(Fd, Ptr, NewFd),
	( { NewFd < 0 } ->
		errno(Errno),
		{ Result = error(Errno) }
	;
		% { cons_sockaddr(Ptr, SockAddr) },
		% { Result = ok(SockAddr - fd(NewFd)) }
		{ Result = ok(fd(NewFd)) }
	).

:- pred accept0(fd, sockaddr_ptr, int, io__state, io__state).
:- mode accept0(in, out, out, di, uo) is det.

:- pragma c_code(accept0(Fd::in, Ptr::out, NewFd::out, IO0::di, IO::uo), 
		[will_not_call_mercury, thread_safe], "{
	struct sockaddr_in *ptr;
	int	len = sizeof(struct sockaddr_in);

	incr_hp(Ptr, (1 + sizeof(struct sockaddr_in)/sizeof(Word)));
	ptr = (struct sockaddr_in *) Ptr;

	NewFd = accept(Fd, ptr, &len);
	IO = IO0;
}").

:- pred cons_sockaddr(sockaddr_ptr, sockaddr).
:- mode cons_sockaddr(in, out) is det.

:- pragma c_code(cons_sockaddr(Ptr::in, Sok::out),
		[will_not_call_mercury, thread_safe], "{
	struct sockaddr_in *ptr;

	ptr = (struct sockaddr_in *) Ptr;

	if (ptr->sin_family == AF_INET) {
		incr_hp(Ptr, 2);
		field(MR_mktag(0), Ptr, 0) = ntohs(ptr->sin_port);
		field(MR_mktag(0), Ptr, 1) = ptr->sin_addr.s_addr;
	} else {
		fatal_error(""cons_sockaddr: unknown type"");
	}
}").

