// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2003-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_proc_id.h - definitions for recording procedure ids

#ifndef MERCURY_PROC_ID_H
#define MERCURY_PROC_ID_H

#include "mercury_types.h"      // for MR_ConstString etc
#include "mercury_tags.h"       // for MR_DEFINE_BUILTIN_ENUM_CONST

// The MR_PredFunc type indicates whether a procedure came from a predicate
// or a function. The constants defined by this enum should have values
// that correspond directly to the values in the representation of the
// `pred_or_func' type in mdbcomp/prim_data.m, so that it is possible to
// cast to/from MR_Word in order to interface with Mercury code.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_PREDICATE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_FUNCTION)
} MR_PredFunc;

// This value should be distinct from any of the values in MR_PredFuncEnum.
// It is used in MR_ProcId in the place where an MR_PredFunc field would be,
// and indicates that no proc id exists.

#define MR_NO_PROC_ID -1

// MR_ProcId is a union. The first alternative identifies ordinary
// procedures, while the second alternative identifies automatically generated
// unification, comparison, index and initialisation procedures. The third
// alternative indicates that no proc id exists. The meanings of the fields
// in the first two forms are the same as in procedure labels. The runtime
// system can figure out if a proc id exists by using the macro
// MR_PROC_ID_EXISTS, and it can figure out which form is present by using
// the macro MR_PROC_ID_IS_UCI, which will return true only if the proc id
// exists and the procedure is of the second type.
//
// The compiler generates MR_UserProcId and MR_UCIProcId structures
// in order to avoid having to initialize the MR_ProcId union through the
// inapplicable alternative, since the C standard in widespread use now
// doesn't support that.
//
// The places that know about the structure of procedure ids include
// browser/dl.m and besides all the places that refer to the C types below.

struct MR_UserProcId_Struct {
    MR_PredFunc     MR_user_pred_or_func;
    MR_ConstString      MR_user_decl_module;
    MR_ConstString      MR_user_def_module;
    MR_ConstString      MR_user_name;
    // The arity stored in this field is what the compiler calls
    // a pred_form_arity, i.e. an arity that includes the return value
    // for functions. It does not include non-user-visible arguments, such as
    // any type_info and/or typeclass_info arguments added by polymorphism.
    MR_int_least16_t    MR_user_pred_form_arity;
    MR_int_least16_t    MR_user_mode;
};

struct MR_UCIProcId_Struct {
    MR_ConstString      MR_uci_type_name;
    MR_ConstString      MR_uci_type_module;
    MR_ConstString      MR_uci_def_module;
    MR_ConstString      MR_uci_pred_name;
    MR_int_least16_t    MR_uci_type_arity;
    MR_int_least16_t    MR_uci_mode;
};

struct MR_NoProcId_Struct {
    // This field should align with the first field of MR_UserProcId_Struct,
    // which means that it should have the same size as MR_PredFunc.
    // Its value should always be equal to MR_NO_PROC_ID.

    int         MR_no_proc_id_flag;
};

union MR_ProcId_Union {
    MR_UserProcId       MR_proc_user;
    MR_UCIProcId        MR_proc_uci;
    MR_NoProcId     MR_proc_none;
};

#define MR_PROC_ID_EXISTS(proc_id)                                      \
    ((proc_id).MR_proc_none.MR_no_proc_id_flag != MR_NO_PROC_ID)

#define MR_PROC_ID_IS_UCI(proc_id)                                      \
    ((MR_Unsigned) (proc_id).MR_proc_user.MR_user_pred_or_func > MR_FUNCTION)

#endif // not MERCURY_PROC_ID_H
