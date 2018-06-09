// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000, 2003, 2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_type_tables.h -
//  This module manages tables that list the definitions of the types and
//  type class instances defined in the program.

#ifndef MERCURY_TYPE_TABLES_H
#define MERCURY_TYPE_TABLES_H

#include "mercury_type_info.h"
#include "mercury_typeclass_info.h"
#include "mercury_dlist.h"

// Register the given type_ctor_info in the type table, so that it can be found
// by later calls to MR_lookup_type_ctor_info.
//
// The mercury_<module>_init_type_tables function generated automatically
// by the Mercury compiler for every module should call this function to
// register the type_ctor_infos of all the types defined in that module.
//
// The caller must not change anything in or reachable from type_ctor_info
// at any time after this call.

extern  void            MR_register_type_ctor_info(
                            MR_TypeCtorInfo type_ctor_info);

// Register the declaration of the given type class in the table of type
// classes, so that it can be found by later calls to
// MR_lookup_type_class_decl.
//
// The mercury_<module>_init_type_tables function generated automatically
// by the Mercury compiler for every module should call this function to
// register all the type classes defined in that module.
//
// The caller must not change anything in or reachable from type_class_decl
// at any time after this call.

extern  void            MR_register_type_class_decl(
                            MR_TypeClassDecl type_class_decl);

// Register the given type class instance declaration in the table of type
// class instances, so that it can be found by later calls to
// MR_lookup_type_class_instances.
//
// The mercury_<module>_init_type_tables function generated automatically
// by the Mercury compiler for every module should call this function to
// register all the type class instances defined in that module.
//
// Since you can reach the MR_TypeClassDecl of the type class that
// this function registers an instance of from the value of the argument,
// it is OK to register an instance of a class whose declaration hasn't been
// registered yet.
//
// The caller must not change anything in or reachable from instance
// at any time after this call.

extern  void            MR_register_type_class_instance(
                            MR_Instance instance);

// Find out if there is a type named type_name defined in module module_name
// with the given arity. If there is, return its type_ctor_info; if not, return
// NULL.
//
// The returned value is not guaranteed to be valid after the next call
// to any of the MR_register_* functions in this module.

extern  MR_TypeCtorInfo
                        MR_lookup_type_ctor_info(
                            const char *module_name,
                            const char *type_name, int arity);

// Each MR_TypeClassDeclInfo structure gives the MR_TypeClassDecl of a
// type class and a list of all its instances.

typedef struct {
    MR_TypeClassDecl    MR_tcd_info_decl;
    MR_Dlist            *MR_tcd_info_instances;
                        // The list elements are of type MR_Instance.
} MR_TypeClassDeclInfo;

// Find out if there is a type class named class_name defined in module
// module_name with the given arity. If there is, return a structure
// giving its declaration and a list of all its instances. If not, return NULL.
//
// The returned value is not guaranteed to be valid after the next call
// to any of the MR_register_* functions in this module.

extern  MR_TypeClassDeclInfo
                        *MR_lookup_type_class_decl_info(
                            const char *module_name,
                            const char *class_name, int arity);

// Find out if there is a type class named class_name defined in module
// module_name with the given arity. If there is, return its declaration.
// If not, return NULL.
//
// The returned value is not guaranteed to be valid after the next call
// to any of the MR_register_* functions in this module.

extern  MR_TypeClassDecl
                        MR_lookup_type_class_decl(
                            const char *module_name,
                            const char *class_name, int arity);

// Find out if there is a type class named class_name defined in module
// module_name with the given arity. If there is, return a list of its
// instances (which may be of length zero but won't be NULL). If not,
// return NULL.
//
// The returned value is not guaranteed to be valid after the next call
// to any of the MR_register_* functions in this module.

extern  MR_Dlist        *MR_lookup_type_class_instances(
                            const char *module_name,
                            const char *class_name, int arity);

// Return a list of all the type constructors registered so far, and return
// their number in *num_ptr if num_ptr is not NULL. The list elements are
// of type MR_TypeCtorInfo. The caller must not modify anything reachable
// from the returned list.

extern  MR_Dlist        *MR_all_type_ctor_infos(int *num_ptr);

// Return a list of all the type classes registered so far, and return their
// number in *num_ptr if num_ptr is not NULL. The list elements are of type
// MR_TypeClassDeclInfo. The caller must not modify anything reachable
// from the returned list.

extern  MR_Dlist        *MR_all_type_class_decl_infos(int *num_ptr);

#endif // not MERCURY_TYPE_TABLES
