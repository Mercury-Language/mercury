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

:- type bmio__state == io__state.

:- pred bmio__init(string, int, bmio__state, bmio__state).
:- mode bmio__init(in, in, di, uo) is det.

:- pred bmio__use_compression_io(bmio__state, bmio__state).
:- mode bmio__use_compression_io(di, uo) is det.

:- pred bmio__use_decompression_io(bmio__state, bmio__state).
:- mode bmio__use_decompression_io(di, uo) is det.

:- pred bmio__read_byte(io__result(int), bmio__state, bmio__state).
:- mode bmio__read_byte(out, di, uo) is det.

:- pred bmio__write_byte(int, bmio__state, bmio__state).
:- mode bmio__write_byte(in, di, uo) is det.

:- pred bmio__mark_decompression_eof(bmio__state, bmio__state).
:- mode bmio__mark_decompression_eof(di, uo) is det.

:- pred bmio__write_byte_checked(int, bmio__state, bmio__state).
:- mode bmio__write_byte_checked(in, di, uo) is det.

:- pred bmio__report_stats(bmio__state, bmio__state).
:- mode bmio__report_stats(di, uo) is det.

:- implementation.

:- import_module list, string.

:- pragma c_header_code("

#include <stdio.h>
#include <malloc.h>
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

% :- pragma no_inline(bmio__init/4).

:- pragma c_code(bmio__init(FileName::in, NumBytes::in, IO0::di, IO::uo),
		will_not_call_mercury, "

 /* printf(""starting bmio__init\n""); */

	assert(NumBytes > 0);
	bmio__buf_size = NumBytes;

 assert((
	bmio__plain_buf = (char *)malloc(NumBytes * sizeof(char))
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
	bmio__zipped_buf = (char *)malloc(NumBytes * sizeof(char))
		) != NULL);
	bmio__zipped_buf_eof = bmio__zipped_buf + NumBytes;

 /* printf(""finished bmio__init\n""); */

	IO = IO0;
").

% :- pragma no_inline(bmio__use_compression_io/2).

:- pragma c_code(bmio__use_compression_io(IO0::di, IO::uo),
		will_not_call_mercury, "

	bmio__rd_buf = bmio__plain_buf;
	bmio__rd_ptr = bmio__plain_buf;
	bmio__rd_eof = bmio__plain_buf_eof;

	bmio__wr_buf = bmio__zipped_buf;
	bmio__wr_ptr = bmio__zipped_buf;
	bmio__wr_eof = bmio__zipped_buf_eof;

	IO0 = IO;
").

% :- pragma no_inline(bmio__use_decompression_io/2).

:- pragma c_code(bmio__use_decompression_io(IO0::di, IO::uo),
		will_not_call_mercury, "

	bmio__rd_buf = bmio__zipped_buf;
	bmio__rd_ptr = bmio__zipped_buf;
	bmio__rd_eof = bmio__zipped_buf_eof;

	bmio__wr_buf = bmio__plain_buf;
	bmio__wr_ptr = bmio__plain_buf;
	bmio__wr_eof = bmio__plain_buf_eof;

	IO0 = IO;
").

:- pragma inline(bmio__read_byte/3).

bmio__read_byte(Result) -->
	rd(Byte),
	{ if Byte = -1 then Result = eof else Result = ok(Byte) }.

bmio__write_byte(Byte) -->
	wr(Byte).

% :- pragma no_inline(bmio__mark_decompression_eof/2).

:- pragma c_code(bmio__mark_decompression_eof(IO0::di, IO::uo),
		will_not_call_mercury, "

	bmio__zipped_buf_eof = bmio__wr_ptr;

	IO0 = IO;
").

bmio__write_byte_checked(Byte) -->
	chk_wr(Byte).

% :- pragma no_inline(rd/3).

:- pred rd(int::out, bmio__state::di, bmio__state::uo) is det.

:- pragma c_code(rd(Byte::out, IO0::di, IO::uo),
		will_not_call_mercury, "

 /* assert(bmio__rd_buf <= bmio__rd_ptr); */
 /* assert(bmio__rd_ptr <= bmio__rd_eof); */

	if(bmio__rd_ptr < bmio__rd_eof)
		Byte = (unsigned char)(*bmio__rd_ptr++);
	else
		Byte = -1;

	IO0 = IO;
").

% :- pragma no_inline(wr/3).

:- pred wr(Byte::in, bmio__state::di, bmio__state::uo) is det.

:- pragma c_code(wr(Byte::in, IO0::di, IO::uo),
		will_not_call_mercury, "

 /* if(bmio__wr_buf > bmio__wr_ptr || bmio__wr_ptr >= bmio__wr_eof) {
  fprintf(stderr, ""bmio__wr_buf = %p\n"", bmio__wr_buf);
  fprintf(stderr, ""bmio__wr_ptr = %p\n"", bmio__wr_ptr);
  fprintf(stderr, ""bmio__wr_eof = %p\n"", bmio__wr_eof);
 } */

 /* assert(bmio__wr_buf <= bmio__wr_ptr); */
 /* assert(bmio__wr_ptr <  bmio__wr_eof); */

	*bmio__wr_ptr++ = Byte;

	IO0 = IO;
").

% :- pragma no_inline(chk_wr/3).

:- pred chk_wr(Byte::in, bmio__state::di, bmio__state::uo) is det.

:- pragma c_code(chk_wr(Byte::in, IO0::di, IO::uo),
		will_not_call_mercury, "

 /* assert(bmio__wr_buf <= bmio__wr_ptr); */
 /* assert(bmio__wr_ptr <  bmio__wr_eof); */

 assert((
	*bmio__wr_ptr++
		) == Byte);

	IO0 = IO;
").

bmio__report_stats -->
    rd_bytes(R),
    wr_bytes(W),
    io__stderr_stream(E),
    io__write_many(E, [s("bmio: read    "), i(R), s(" bytes\n")]),
    io__write_many(E, [s("bmio: written "), i(W), s(" bytes\n")]).

:- pred rd_bytes(int::out, bmio__state::di, bmio__state::uo) is det.

:- pragma c_code(rd_bytes(R::out, IO0::di, IO::uo),
    will_not_call_mercury, "

    R = bmio__rd_ptr - bmio__rd_buf;
    IO0 = IO;
").

:- pred wr_bytes(int::out, bmio__state::di, bmio__state::uo) is det.

:- pragma c_code(wr_bytes(W::out, IO0::di, IO::uo),
    will_not_call_mercury, "

    W = bmio__wr_ptr - bmio__wr_buf;
    IO0 = IO;
").
