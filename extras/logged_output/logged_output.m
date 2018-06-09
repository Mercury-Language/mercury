%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2005, 2012 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% module:   logged_output.m
% main author:  Peter Ross (petdr@miscrit.be)
%
% This provides an implementation of a stream which writes to stdout and
% logs to a file at the same time.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module logged_output.

:- interface.

:- import_module io.

:- pred logged_output.init(string::in, io.result(io.text_output_stream)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

logged_output.init(FileName, Result, !IO) :-
    create_stream(FileName, Stream, !IO),
    Result = ok(Stream).
    
%-----------------------------------------------------------------------------%

:- pred create_stream(string::in, io.output_stream::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    create_stream(FileName::in, IOStream::out, _IO0::di, _IO::uo), 
    [will_not_call_mercury, promise_pure],
"
    MercuryFile *stream;
    FILE        *file;

    file = fopen(FileName, ""w"");

    MR_incr_hp_type(stream, MercuryFile);

    stream->stream_type = MR_FILE_STREAM;
    stream->stream_info.file = file;
    stream->line_number = 1;

    stream->close       = ME_logged_output_close;
    stream->read        = ME_logged_output_read;
    stream->write       = ME_logged_output_write;

    stream->flush       = ME_logged_output_flush;
    stream->ungetc      = ME_logged_output_ungetch;
    stream->getc        = ME_logged_output_getch;
    stream->vprintf     = ME_logged_output_vfprintf;
    stream->putc        = ME_logged_output_putch;

    IOStream = MR_wrap_output_stream(stream);
").


%------------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#ifndef MR_NEW_MERCURYFILE_STRUCT
  #error ""you need to use version of the mercury compiler configured with --enable-new-mercuryfile-struct""
#endif

#include <stdio.h>
#include <stdarg.h>

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
    va_list log_ap;
    /*
    ** XXX we are not allowed to resue ap after a call to vfprintf
    ** (as this code originally did) -- doing so results in undefined
    ** behaviour.  Making a copy of ap (here log_ap) in the following manner
    ** is NOT portable.  (C99 provides the macro va_copy, which would allow
    ** us to do this portably.)
    */
    *log_ap = *ap;
    vfprintf(stdout, format, ap);
    rc = vfprintf(info->file, format, log_ap);
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
:- end_module logged_output.
%------------------------------------------------------------------------------%
