/*
** Copyright (C) 1999-2000,2002 University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/

/*
** This file exposes the C types used by the reference and nb_reference
** modules, so that one can allocate them somewhere other than the heap if
** one so desires. Normally this should not be necessary, and the cleaner,
** safer interfaces provided by reference.m and nb_reference.m should be
** used instead. However, sometimes it is useful; for example, the HAL
** compiler would like to be able to allocate them at compile-time-known
** locations, in order to implement global variables.
**
** These types should be treated as abstract in case their implementation
** changes in the future.
**
** Sample usage:
**
** The following example declares an ME_Reference `foo' at a
** compile-time-known location, and provides a zero-arity function for
** returning the corresponding Mercury object of type `reference/1'.  Note
** that this reference should be initialised with the `reference.init/2'
** predicate before use: see the documentation of that predicate in
** reference.m for more caveats.
**
** :- pragma foreign_decl("C", "
**     #include ""c_reference.h""
**     extern ME_Reference foo;
** ").
**
** :- pragma foreign_code("C", "
**     ME_Reference foo;
** ").
**
** :- pragma foreign_proc(
**     foo_reference = (X::out),
**     [will_not_call_mercury],
** "
**     X = (MR_Word) &foo;
** ").
*/

#ifndef	C_REFERENCE_H
#define	C_REFERENCE_H

#include "mercury_trail.h"

typedef struct {
    void *value;
    MR_ChoicepointId id;
} ME_Reference;

typedef MR_Word ME_NbReference;

#endif	/* not C_REFERENCE_H */
