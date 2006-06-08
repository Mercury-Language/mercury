/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#define DECLARE_PROBE_COUNT
#define record_probe_count()            ((void) 0)
#define record_lookup_count()           ((void) 0)
#define record_insert_count()           ((void) 0)
#define record_resize_count(old, new)   ((void) 0)
#define record_alloc_count()            ((void) 0)
