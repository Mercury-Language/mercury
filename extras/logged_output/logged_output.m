%------------------------------------------------------------------------------%
% Copyright (C) 2000, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: 	logged_output.m
% main author:	Peter Ross (petdr@miscrit.be)
%
% This provides an implementation of a stream which writes to stdout and
% logs to a file at the same time.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module logged_output.

:- interface.

:- import_module io.

:- pred logged_output__init(string::in, io__result(io__output_stream)::out,
		io__state::di, io__state::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

logged_output__init(FileName, Result) -->
	create_stream(FileName, Stream),
	{ Result = ok(Stream) }.
	
%------------------------------------------------------------------------------%

:- pred create_stream(string::in, io__output_stream::out, io::di, io::uo)
	is det.

:- pragma foreign_proc("C",
	create_stream(FileName::in, IOStream::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	MercuryFile	*stream;
	FILE		*file;

	file = fopen(FileName, ""w"");

	MR_incr_hp((MR_Word) stream, ((sizeof(MercuryFile) / sizeof(MR_Word)) + 1));

	stream->stream_type	= MR_FILE_STREAM;
	stream->stream_info.file= file;
	stream->line_number	= 1;

	stream->close		= ME_logged_output_close;
	stream->read		= ME_logged_output_read;
	stream->write		= ME_logged_output_write;

	stream->flush		= ME_logged_output_flush;
	stream->ungetc		= ME_logged_output_ungetch;
	stream->getc		= ME_logged_output_getch;
	stream->vprintf		= ME_logged_output_vfprintf;
	stream->putc		= ME_logged_output_putch;

	IOStream = (MercuryFilePtr) stream;

	IO = IO0;
").


%------------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#ifndef MR_NEW_MERCURYFILE_STRUCT
  #error ""you need to use version of the mercury compiler configured with --enable-new-mercuryfile-struct""
#endif

#include ""stdio.h""

int ME_logged_output_putch(MR_StreamInfo *info, int);
int ME_logged_output_close(MR_StreamInfo *info);
int ME_logged_output_vfprintf(MR_StreamInfo *info,
		const char *format, va_list ap);
int ME_logged_output_write(MR_StreamInfo *info,
		const void *buffer, size_t size);

int ME_logged_output_getch(MR_StreamInfo *info);
int ME_logged_output_ungetch(MR_StreamInfo *info,int);
int ME_logged_output_flush(MR_StreamInfo *info);
int ME_logged_output_read(MR_StreamInfo *info, void *buffer, size_t size);
").

:- pragma foreign_code("C", "
int
ME_logged_output_putch(MR_StreamInfo *info, int ch)
{
	putc(ch, stdout);
	return putc(ch, info->file);
}

int
ME_logged_output_close(MR_StreamInfo *info)
{
	return fclose(info->file);
}
  
int
ME_logged_output_vfprintf(MR_StreamInfo *info, const char *format, va_list ap)
{
	int rc;							  
	vfprintf(stdout, format, ap);
	rc = vfprintf(info->file, format, ap);
	return rc;
}

int
ME_logged_output_write(MR_StreamInfo *info, const void *buffer, size_t size)
{
	int rc;							       
	fwrite(buffer, sizeof(unsigned char), size, stdout);
	rc = fwrite(buffer, sizeof(unsigned char), size, info->file);
	return (rc < size ? -1 : rc);
}
  
/*
** We are creating an output stream so none of these functions will ever
** be called.
*/
int
ME_logged_output_getch(MR_StreamInfo *info) 
{
	MR_fatal_error(""ME_logged_output_getch"");
}
	  
int
ME_logged_output_ungetch(MR_StreamInfo *info, int ch)
{
	MR_fatal_error(""ME_logged_output_ungetch"");
}

int
ME_logged_output_flush(MR_StreamInfo *info)
{
	fflush(stdout);
	return fflush(info->file);
}
  
int
ME_logged_output_read(MR_StreamInfo *info, void *buffer, size_t size)
{ 
	MR_fatal_error(""ME_logged_output_read"");
}
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
