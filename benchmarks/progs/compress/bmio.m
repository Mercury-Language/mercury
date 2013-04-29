% ---------------------------------------------------------------------------- %
% bmio.m
%
% Simple test harness for 129.compress benchmark.  Uses C because it's
% easier...
%
% ---------------------------------------------------------------------------- %

:- module bmio.

:- interface.

:- import_module io.

:- type bmio.state == io.state.

:- pred bmio.init(string, int, bmio.state, bmio.state).
:- mode bmio.init(in, in, di, uo) is det.

:- pred bmio.use_compression_io(bmio.state, bmio.state).
:- mode bmio.use_compression_io(di, uo) is det.

:- pred bmio.use_decompression_io(bmio.state, bmio.state).
:- mode bmio.use_decompression_io(di, uo) is det.

:- pred bmio.read_byte(io.result(int), bmio.state, bmio.state).
:- mode bmio.read_byte(out, di, uo) is det.

:- pred bmio.write_byte(int, bmio.state, bmio.state).
:- mode bmio.write_byte(in, di, uo) is det.

:- pred bmio.mark_decompression_eof(bmio.state, bmio.state).
:- mode bmio.mark_decompression_eof(di, uo) is det.

:- pred bmio.write_byte_checked(int, bmio.state, bmio.state).
:- mode bmio.write_byte_checked(in, di, uo) is det.

:- pred bmio.report_stats(bmio.state, bmio.state).
:- mode bmio.report_stats(di, uo) is det.

:- implementation.

:- import_module list, string.

:- pragma foreign_decl("C", "

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

unsigned int	 bmio__buf_size;
unsigned char	*bmio__plain_buf;
unsigned char	*bmio__plain_buf_eof;
unsigned char	*bmio__zipped_buf;
unsigned char	*bmio__zipped_buf_eof;
unsigned char	*bmio__rd_buf;
unsigned char	*bmio__rd_ptr;
unsigned char	*bmio__rd_eof;
unsigned char	*bmio__wr_buf;
unsigned char	*bmio__wr_ptr;
unsigned char	*bmio__wr_eof;
FILE		*bmio__fp;

").

% :- pragma no_inline(bmio.init/4).

:- pragma foreign_proc("C",
	bmio.init(FileName::in, NumBytes::in, _IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"

 /* printf(""starting bmio__init\n""); */

	assert(NumBytes > 0);
	bmio__buf_size = NumBytes;

 assert((
	bmio__plain_buf = malloc(NumBytes * sizeof(char))
		) != NULL);
	bmio__plain_buf_eof = bmio__plain_buf + NumBytes;

 assert((
	bmio__fp = fopen(FileName, ""rb"")
		) != NULL);
 assert((
	fread(bmio__plain_buf, sizeof(char), NumBytes, bmio__fp)
		) == NumBytes);
	fclose(bmio__fp);

 assert((
	bmio__zipped_buf = malloc(NumBytes * sizeof(char))
		) != NULL);
	bmio__zipped_buf_eof = bmio__zipped_buf + NumBytes;

 /* printf(""finished bmio__init\n""); */

").

% :- pragma no_inline(bmio.use_compression_io/2).

:- pragma foreign_proc("C",
	bmio.use_compression_io(_IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"

	bmio__rd_buf = bmio__plain_buf;
	bmio__rd_ptr = bmio__plain_buf;
	bmio__rd_eof = bmio__plain_buf_eof;

	bmio__wr_buf = bmio__zipped_buf;
	bmio__wr_ptr = bmio__zipped_buf;
	bmio__wr_eof = bmio__zipped_buf_eof;
").

% :- pragma no_inline(bmio.use_decompression_io/2).

:- pragma foreign_proc("C",
	bmio.use_decompression_io(_IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"
	bmio__rd_buf = bmio__zipped_buf;
	bmio__rd_ptr = bmio__zipped_buf;
	bmio__rd_eof = bmio__zipped_buf_eof;

	bmio__wr_buf = bmio__plain_buf;
	bmio__wr_ptr = bmio__plain_buf;
	bmio__wr_eof = bmio__plain_buf_eof;
").

:- pragma inline(bmio.read_byte/3).

bmio.read_byte(Result) -->
	rd(Byte),
	{ if Byte = -1 then Result = eof else Result = ok(Byte) }.

bmio.write_byte(Byte) -->
	wr(Byte).

% :- pragma no_inline(bmio.mark_decompression_eof/2).

:- pragma foreign_proc("C",
	bmio.mark_decompression_eof(_IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"
	bmio__zipped_buf_eof = bmio__wr_ptr;
").

bmio.write_byte_checked(Byte) -->
	chk_wr(Byte).

% :- pragma no_inline(rd/3).

:- pred rd(int::out, bmio.state::di, bmio.state::uo) is det.

:- pragma foreign_proc("C",
	rd(Byte::out, _IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"
 /* assert(bmio__rd_buf <= bmio__rd_ptr); */
 /* assert(bmio__rd_ptr <= bmio__rd_eof); */

	if(bmio__rd_ptr < bmio__rd_eof)
		Byte = (unsigned char)(*bmio__rd_ptr++);
	else
		Byte = -1;
").

% :- pragma no_inline(wr/3).

:- pred wr(Byte::in, bmio.state::di, bmio.state::uo) is det.

:- pragma foreign_proc("C",
	wr(Byte::in, _IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"

 /* if(bmio__wr_buf > bmio__wr_ptr || bmio__wr_ptr >= bmio__wr_eof) {
  fprintf(stderr, ""bmio__wr_buf = %p\n"", bmio__wr_buf);
  fprintf(stderr, ""bmio__wr_ptr = %p\n"", bmio__wr_ptr);
  fprintf(stderr, ""bmio__wr_eof = %p\n"", bmio__wr_eof);
 } */

 /* assert(bmio__wr_buf <= bmio__wr_ptr); */
 /* assert(bmio__wr_ptr <  bmio__wr_eof); */

	*bmio__wr_ptr++ = Byte;
").

% :- pragma no_inline(chk_wr/3).

:- pred chk_wr(Byte::in, bmio.state::di, bmio.state::uo) is det.

:- pragma foreign_proc("C",
	chk_wr(Byte::in, _IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury],
"

 /* assert(bmio__wr_buf <= bmio__wr_ptr); */
 /* assert(bmio__wr_ptr <  bmio__wr_eof); */

 assert((
	*bmio__wr_ptr++
		) == Byte);
").

bmio.report_stats -->
    rd_bytes(R),
    wr_bytes(W),
    io.stderr_stream(E),
    io.write_many(E, [s("bmio: read    "), i(R), s(" bytes\n")]),
    io.write_many(E, [s("bmio: written "), i(W), s(" bytes\n")]).

:- pred rd_bytes(int::out, bmio.state::di, bmio.state::uo) is det.

:- pragma foreign_proc("C",
	rd_bytes(R::out, _IO0::di, _IO::uo),
    	[promise_pure, will_not_call_mercury],
"

    R = bmio__rd_ptr - bmio__rd_buf;
").

:- pred wr_bytes(int::out, bmio.state::di, bmio.state::uo) is det.

:- pragma foreign_proc("C",
	wr_bytes(W::out, _IO0::di, _IO::uo),
    	[promise_pure, will_not_call_mercury], "

    W = bmio__wr_ptr - bmio__wr_buf;
").
