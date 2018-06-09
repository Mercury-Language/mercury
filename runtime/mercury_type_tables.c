// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000,2002-2005, 2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module manages tables that list the definitions of the type
// constructors, type classes and type class instances defined in the program.
//
// The sizes of these tables can vary by several orders of magnitude,
// so using a fixed size hash table would not be a good idea. This is why
// we build on the implementation of expandable hash tables in the
// mercury_tabling module.

#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_type_tables.h"
#include "mercury_tabling.h"
#include "mercury_misc.h"
#include "mercury_memory.h"
#include <string.h>

// This module maintains four data structures: two hash tables and two lists.
// One hash table and one list contain information about type constructors,
// with the elements in the hash table and the list being MR_TypeCtorInfos,
// while the other hash table and list contain information about type
// classes and their instances, with the elements in the hash table and the
// list being MR_TypeClassDeclInfo pointers.
//
// All four data structures are monotonic: you can insert information into
// them, but you cannot remove anything from them.
//
// We assume that two registered structures (whether MR_TypeCtorInfos,
// MR_TypeClassDecls, or MR_Instances) with different addresses contain
// different information. This is OK because the registered structures are
// supposed to be compiler generated, and the compiler ensures this invariant.

static  MR_TableNode    MR_type_ctor_table = { 0 };
static  MR_TableNode    MR_type_class_decl_info_table = { 0 };
static  MR_Dlist        *MR_type_ctor_list = NULL;
static  MR_Dlist        *MR_type_class_decl_info_list = NULL;
static  int             MR_num_type_ctors = 0;
static  int             MR_num_type_class_decls = 0;

static  MR_TypeClassDeclInfo    *MR_do_register_type_class_decl(
                                    MR_TypeClassDecl type_class_decl);

#define type_names_match(tc1, module_name, type_name, arity)            \
    ( MR_streq(MR_type_ctor_name(tc1), (type_name))                     \
    && MR_streq(MR_type_ctor_module_name(tc1), (module_name))           \
    && (tc1)->MR_type_ctor_arity == (arity))

#define type_names_match_ctor(tc1, tc2)                                 \
    ( MR_streq(MR_type_ctor_name(tc1), MR_type_ctor_name(tc2))          \
    && MR_streq(MR_type_ctor_module_name(tc1),                          \
        MR_type_ctor_module_name(tc2))                                  \
    && (tc1)->MR_type_ctor_arity == (tc2)->MR_type_ctor_arity )

#define class_names_match(tc_id1, module_name, class_name, arity)       \
    ( MR_streq(tc_id1->MR_tc_id_name, (class_name))                     \
    && MR_streq(tc_id1->MR_tc_id_module_name, (module_name))            \
    && tc_id1->MR_tc_id_arity == (arity))

#define class_names_match_id(tc_id1, tc_id2)                            \
    ( MR_streq((tc_id1)->MR_tc_id_name, (tc_id2)->MR_tc_id_name)        \
    && MR_streq((tc_id1)->MR_tc_id_module_name,                         \
        (tc_id2)->MR_tc_id_module_name)                                 \
    && (tc_id1)->MR_tc_id_arity == (tc_id2)->MR_tc_id_arity)

void
MR_register_type_ctor_info(MR_TypeCtorInfo type_ctor_info)
{
    MR_TrieNode     slot;
    MR_Dlist        *element_ptr;
    MR_TypeCtorInfo cur_type_ctor_info;

    MR_assert(type_ctor_info != NULL);
    slot = MR_string_hash_lookup_or_add(&MR_type_ctor_table,
        MR_type_ctor_name(type_ctor_info));

    MR_for_dlist (element_ptr, slot->MR_type_table) {
        cur_type_ctor_info = (MR_TypeCtorInfo) MR_dlist_data(element_ptr);

        if (type_names_match_ctor(type_ctor_info, cur_type_ctor_info)) {
            if (cur_type_ctor_info == type_ctor_info) {
                // type_ctor_info has been registered before.
                return;
            } else {
                MR_fatal_error("MR_register_type_ctor_info: "
                    "ambiguous type ctor");
            }
        }
    }

    slot->MR_type_table = MR_dlist_addhead(slot->MR_type_table,
        type_ctor_info);
    MR_type_ctor_list = MR_dlist_addtail(MR_type_ctor_list,
        type_ctor_info);
    MR_num_type_ctors++;
}

static MR_TypeClassDeclInfo *
MR_do_register_type_class_decl(MR_TypeClassDecl type_class_decl)
{
    MR_TrieNode             slot;
    MR_Dlist                *element_ptr;
    MR_TypeClassDeclInfo    *cur_type_class_decl_info;
    MR_TypeClassDecl        cur_type_class_decl;
    const MR_TypeClassId    *cur_type_class_id;
    MR_TypeClassDeclInfo    *type_class_decl_info;
    const MR_TypeClassId    *type_class_id;

    MR_assert(type_class_decl != NULL);
    type_class_id = type_class_decl->MR_tc_decl_id;

    slot = MR_string_hash_lookup_or_add(&MR_type_class_decl_info_table,
        type_class_id->MR_tc_id_name);

    MR_for_dlist (element_ptr, slot->MR_type_table) {
        cur_type_class_decl_info =
            (MR_TypeClassDeclInfo *) MR_dlist_data(element_ptr);
        cur_type_class_decl = cur_type_class_decl_info->MR_tcd_info_decl;
        cur_type_class_id = cur_type_class_decl->MR_tc_decl_id;

        if (class_names_match_id(type_class_id, cur_type_class_id)) {
            if (cur_type_class_decl == type_class_decl) {
                // type_ctor_info has been registered before
                return cur_type_class_decl_info;
            } else {
                MR_fatal_error("MR_do_register_type_class_decl: "
                    "ambiguous type class decl");
            }
        }
    }

    type_class_decl_info = MR_NEW(MR_TypeClassDeclInfo);
    type_class_decl_info->MR_tcd_info_decl = type_class_decl;
    type_class_decl_info->MR_tcd_info_instances = MR_dlist_makelist0();
    slot->MR_type_table = MR_dlist_addhead(slot->MR_type_table,
        type_class_decl_info);
    MR_type_class_decl_info_list = MR_dlist_addtail(
        MR_type_class_decl_info_list, type_class_decl_info);
    MR_num_type_class_decls++;
    return type_class_decl_info;
}

void
MR_register_type_class_decl(MR_TypeClassDecl type_class_decl)
{
    (void) MR_do_register_type_class_decl(type_class_decl);
}

void
MR_register_type_class_instance(MR_Instance type_class_instance)
{
    MR_TypeClassDeclInfo    *type_class_decl_info;
    MR_Dlist                *element_ptr;
    MR_Instance             cur_instance;

    type_class_decl_info = MR_do_register_type_class_decl(
        type_class_instance->MR_tc_inst_type_class);
    MR_assert(type_class_decl_info != NULL);

    MR_for_dlist (element_ptr, type_class_decl_info->MR_tcd_info_instances)
    {
        cur_instance = (MR_Instance) MR_dlist_data(element_ptr);
        if (cur_instance == type_class_instance) {
            // type_class_instance has been registered before.
            return;
        }
    }

    // type_class_instance has not been registered before.
    type_class_decl_info->MR_tcd_info_instances =
        MR_dlist_addtail(type_class_decl_info->MR_tcd_info_instances,
            type_class_instance);
}

MR_TypeCtorInfo
MR_lookup_type_ctor_info(const char *module_name, const char *type_name,
    int arity)
{
    MR_TrieNode     slot;
    MR_Dlist        *element_ptr;
    MR_TypeCtorInfo cur_type_ctor_info;

    slot = MR_string_hash_lookup_or_add(&MR_type_ctor_table, type_name);

    MR_for_dlist (element_ptr, slot->MR_type_table) {
        cur_type_ctor_info = (MR_TypeCtorInfo) MR_dlist_data(element_ptr);

        if (type_names_match(cur_type_ctor_info, module_name,
            type_name, arity))
        {
            return cur_type_ctor_info;
        }
    }

    return NULL;
}

MR_TypeClassDeclInfo *
MR_lookup_type_class_decl_info(const char *module_name, const char *class_name,
    int arity)
{
    MR_TrieNode             slot;
    MR_Dlist                *element_ptr;
    MR_TypeClassDeclInfo    *cur_type_class_decl_info;
    MR_TypeClassDecl        cur_type_class_decl;
    const MR_TypeClassId    *cur_type_class_id;

    slot = MR_string_hash_lookup_or_add(&MR_type_class_decl_info_table,
        class_name);

    MR_for_dlist (element_ptr, slot->MR_type_table) {
        cur_type_class_decl_info =
            (MR_TypeClassDeclInfo *) MR_dlist_data(element_ptr);
        cur_type_class_decl = cur_type_class_decl_info->MR_tcd_info_decl;
        cur_type_class_id = cur_type_class_decl->MR_tc_decl_id;

        if (class_names_match(cur_type_class_id, module_name,
            class_name, arity))
        {
            return cur_type_class_decl_info;
        }
    }

    return NULL;
}

MR_TypeClassDecl
MR_lookup_type_class_decl(const char *module_name, const char *class_name,
    int arity)
{
    MR_TypeClassDeclInfo    *type_class_decl_info;

    type_class_decl_info = MR_lookup_type_class_decl_info(module_name,
        class_name, arity);

    if (type_class_decl_info == NULL) {
        return NULL;
    } else {
        return type_class_decl_info->MR_tcd_info_decl;
    }
}

MR_Dlist *
MR_lookup_type_class_instances(const char *module_name, const char *class_name,
    int arity)
{
    MR_TypeClassDeclInfo    *type_class_decl_info;

    type_class_decl_info = MR_lookup_type_class_decl_info(module_name,
        class_name, arity);

    if (type_class_decl_info == NULL) {
        return NULL;
    } else {
        return type_class_decl_info->MR_tcd_info_instances;
    }
}

MR_Dlist *
MR_all_type_ctor_infos(int *num_ptr)
{
    if (num_ptr != NULL) {
        *num_ptr = MR_num_type_ctors;
    }

    return MR_type_ctor_list;
}

MR_Dlist *
MR_all_type_class_decl_infos(int *num_ptr)
{
    if (num_ptr != NULL) {
        *num_ptr = MR_num_type_class_decls;
    }

    return MR_type_class_decl_info_list;
}
