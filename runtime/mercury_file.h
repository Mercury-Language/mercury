// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_FILE_H
#define MERCURY_FILE_H

#include "mercury_library_types.h"

// Initialise a MercuryFile structure to use the C stdlib FILE *type.

void MR_mercuryfile_init(FILE *file, int line_number, MercuryFile *mf);

#ifdef MR_NEW_MERCURYFILE_STRUCT
  #define MR_IS_FILE_STREAM(mf) ( (mf).stream_type == MR_FILE_STREAM )

  int MR_getch(MR_StreamInfo *info);
  int MR_putch(MR_StreamInfo *info, int);
  int MR_ungetch(MR_StreamInfo *info, int);
  int MR_close(MR_StreamInfo *info);
  int MR_flush(MR_StreamInfo *info);
  int MR_vfprintf(MR_StreamInfo *info, const char *format, va_list ap);
  int MR_read(MR_StreamInfo *info, void *buffer, size_t size);
  int MR_write(MR_StreamInfo *info, const void *buffer, size_t size);
  int MR_ferror(MR_StreamInfo *info);
#endif

#endif // MERCURY_FILE_H
