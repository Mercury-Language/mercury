/*
** Copyright (C) 1998-2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_library_types.h - definitions of some basic types used by the
** Mercury library.
*/

#ifndef MERCURY_LIBRARY_TYPES_H
#define MERCURY_LIBRARY_TYPES_H

#include <stdio.h>		/* for `FILE' */
#include <stdarg.h>		/* for `va_list' */
#include "mercury_types.h"	/* for `MR_Word' and `MR_Integer' */
#include "mercury_std.h"	/* for MR_VARIABLE_SIZED */

/*
** The C `MercuryFile' type is used for the Mercury `io__stream' type
** in library/io.m.
** Mercury files are not quite the same as C stdio FILEs,
** because we keep track of a lot more information.
*/
#ifndef MR_NEW_MERCURYFILE_STRUCT
  typedef struct mercury_file {
	FILE *file1;
	int line_number1;
  #ifdef NATIVE_GC
	int id;
  #endif
  } MercuryFile;

  #define MR_file(mf)		(mf).file1
  #define MR_line_number(mf)	(mf).line_number1

  #define MR_IS_FILE_STREAM(mf)	( TRUE )

  #define MR_MERCURYFILE_INIT(file, line_number)		\
  		{ (file), (line_number) }

  #define MR_CLOSE(mf)		fclose(MR_file(mf))
  #define MR_FLUSH(mf)		fflush(MR_file(mf))

  #define MR_READ(mf, ptr, size)	\
  		fread((ptr), sizeof(unsigned char), (size), MR_file(mf))
  #define MR_WRITE(mf, ptr, size)	\
  		fwrite((ptr), sizeof(unsigned char), (size), MR_file(mf))

  #define MR_UNGETCH(mf, ch)	ungetc((ch), MR_file(mf))
  #define MR_GETCH(mf)		getc(MR_file(mf))

  #define MR_VFPRINTF(mf, fmt, args)	\
  		vfprintf(MR_file(mf), (fmt), (args))

  #define MR_PUTCH(mf, ch)	putc((ch), MR_file(mf))

#else /* MR_NEW_MERCURYFILE_STRUCT */

	/* Possible types of a MercuryFile */
  typedef enum {
	MR_FILE_STREAM		= 1,
	MR_SOCKET_STREAM	= 2,
	MR_PIPE_STREAM		= 3,
	MR_USER_STREAM		= 4
  } MR_StreamType;

  /*
  ** A pointer to the data which can be used to access the MercuryFile.
  */
  typedef union {
	FILE *file;
	void *data;
  } MR_StreamInfo;

  typedef int (MR_Stream_close)(MR_StreamInfo *);
  typedef int (MR_Stream_read)(MR_StreamInfo *, void *, size_t);
  typedef int (MR_Stream_write)(MR_StreamInfo *, const void *, size_t);

  typedef int (MR_Stream_flush)(MR_StreamInfo *);
  typedef int (MR_Stream_ungetc)(MR_StreamInfo *, int);
  typedef int (MR_Stream_getc)(MR_StreamInfo *);
  typedef int (MR_Stream_vprintf)(MR_StreamInfo *, const char *, va_list);
  typedef int (MR_Stream_putc)(MR_StreamInfo *, int);

  /*
  ** The MercuryFile structure records:
  **	- the type of the stream
  **	- a pointer to the information which describes the stream
  **	- the line number we are up to in the stream
  **
  **	- pointer to functions which provide the same functionality
  **	  as close/read/write of fds.
  **
  **	- pointers to functions which provide the same functionality
  **	  as flush/ungetc/getc/vprintf/putc on stdio files.
  **
  ** MercuryFiles record all this extra information so that users can use all
  ** the functionality of io.m on their own streams.  For instance see
  ** extras/logged_output.
  */
  typedef struct mercury_file {
	MR_StreamType	stream_type;
	MR_StreamInfo	stream_info;
	int		line_number;
  #ifdef NATIVE_GC
	int id;
  #endif

		/* UNBUFFERED FUNCTIONS */
	MR_Stream_close	*close;    
	MR_Stream_read	*read;
	MR_Stream_write	*write;

		/* BUFFERED FUNCTIONS */
	MR_Stream_flush		*flush;
	MR_Stream_ungetc	*ungetc;
	MR_Stream_getc		*getc;
	MR_Stream_vprintf	*vprintf;
	MR_Stream_putc		*putc;
  } MercuryFile;

  /*
  ** access the file and line number fields
  */
  #define MR_file(mf)		(mf).stream_info.file
  #define MR_line_number(mf)	(mf).line_number

  /*
  ** Call the functions associated with the MercuryFile structure
  */
  #define MR_CLOSE(mf)	((mf).close)(&((mf).stream_info))
  #define MR_READ(mf, ptr, size)	\
  		((mf).read)(&((mf).stream_info), (ptr), (size))
  #define MR_WRITE(mf, ptr, size)	\
  		((mf).write)(&((mf).stream_info), (ptr), (size))

  #define MR_FLUSH(mf)	((mf).flush)(&((mf).stream_info))
  #define MR_UNGETCH(mf, ch)	\
  		((mf).ungetc)(&((mf).stream_info), (ch))
  #define MR_GETCH(mf)	((mf).getc)(&((mf).stream_info))
  #define MR_VFPRINTF(mf, fmt, args)	\
  		((mf).vprintf)(&((mf).stream_info), (fmt), (args))
  #define MR_PUTCH(mf, ch)	\
  		((mf).putc)(&((mf).stream_info), (ch))

#endif	/* MR_NEW_MERCURYFILE_STRUCT */

/*
** definitions for accessing the representation of the
** Mercury `array' type
*/

typedef struct {
	MR_Integer size;
	MR_Word elements[MR_VARIABLE_SIZED];
} MR_ArrayType;

#define MR_make_array(sz) ((MR_ArrayType *) MR_GC_NEW_ARRAY(MR_Word, (sz) + 1))

#endif /* not MERCURY_LIBRARY_TYPES_H */
