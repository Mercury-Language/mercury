// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1999-2003, 2005-2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_ho_call.h - defines the structure of closures.
//
// The places in the system that know about the layouts of closures are
//
//  compiler/unify_gen.m (unify_gen__generate_construction_2)
//  runtime/mercury_ho_call.[ch]
//
// Any changes here will need to be reflected in the other places as well.

#ifndef MERCURY_HO_CALL_H
#define MERCURY_HO_CALL_H

#include "mercury_stack_layout.h"   // for MR_ClosureId etc
#include "mercury_type_info.h"      // for MR_PseudoTypeInfo
#include "mercury_types.h"          // for MR_Closure
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_goto.h"         // for MR_declare_entry
  #ifdef MR_DO_CALL_STATS
    #include  <stdio.h>             // for FILE
  #endif
#endif

// A closure layout structure identifies a procedure, and contains
// the information required to identify the types of the arguments
// in any closure that calls that procedure. It is represented as a
// vector of words containing
//
//  a pointer to an MR_ClosureId structure
//  a pointer to information about the locations of typeinfos
//      for the type parameters of the procedure
//      (NULL if there are no type parameters)
//  one word giving the number of arguments of the procedure (M)
//  M words giving pseudotypeinfos for the arguments
//
// A closure that refers to the procedure may not (and probably will not)
// contain values for all the arguments of the procedure, but the closure
// layout structure has information about all arguments. This is to make
// the creation of a closure from another closure by adding some more
// hidden arguments as fast as possible.
//
// Without float registers, there is no problem in finding out which
// pseudotypeinfo describes which hidden argument, because if the closure
// contains n hidden arguments, these must be the first n arguments of the
// procedure. With float registers, the hidden arguments are reordered so that
// float register arguments come after the regular register arguments. To tell
// if an argument is passed via a float register, check if the pseudotypeinfo
// is MR_FLOAT_CTOR_ADDR. If a float argument is passed via a regular register,
// the pseudotypeinfo must be replaced by the type_ctor_info for
// private_builtin.float_box.
//
// The typeinfo and typeclassinfo arguments describing the actual types bound
// to type vars are always at the start of the argument list. A closure can
// contain arg i but not arg j only if i < j; this means that if a closure
// contains a non-typeinfo argument j, it also contains all the typeinfo
// and typeclassinfo arguments of the procedure and therefore (directly or
// indirectly) all the typeinfos that may be referred to in the pseudotypeinfo
// for argument j. (If we ever allow code to take the address of a procedure
// whose signature includes an existential type, we may have to rethink this.)
//
// The MR_Live_Lvals inside MR_TypeParamLocns, which encode the locations
// of the typeinfos for the type variables in the signature of the procedure,
// assume that argument i is in register ri. While this will be true at the
// time of the call, code that wants to manipulate the closure as an
// independent entity will have to substitute the argument vector in the
// closure itself for the register file.
//
// Note that if a module is compiled without typeinfo liveness, then closures
// will not have any layout information. This will be indicated by the value
// of num_all_args being negative, which says that the only field of this
// structure containing valid information is proc_id.
//
// The Dyn_Link variant is for closures created by browser/dl.m. The closure_id
// field of such closures will contain an invalid proc_id (which you can tell
// from the negative arity) and a creation context that is also different from
// other closures: instead of specifying the source context where the closure
// is created, it puts a sequence number into the field that normally contains
// the line number.

typedef struct MR_Closure_Layout_Struct {
    MR_ClosureId            *MR_closure_id;
    MR_TypeParamLocns       *MR_closure_type_params;
    MR_Integer              MR_closure_num_all_args;
    MR_PseudoTypeInfo       MR_closure_arg_pseudo_type_info[MR_VARIABLE_SIZED];
} MR_Closure_Layout;

typedef struct MR_Closure_Dyn_Link_Layout_Struct {
    MR_ClosureId            *MR_closure_dl_id;
    MR_TypeParamLocns       *MR_closure_dl_type_params;
    MR_Integer              MR_closure_dl_num_all_args;
} MR_Closure_Dyn_Link_Layout;

// A closure is a vector of words containing:
//
//  one word pointing to the closure layout structure of the procedure
//  one word pointing to the code of the procedure
//  one word giving the number of hidden arguments: (R | F<<16)
//  R words representing the R hidden regular register arguments
//  F words representing the F hidden float register arguments (in boxed form)
//
// The num_hidden_args_rf field holds the number of arguments to place into
// regular registers in the lower 16-bits, and the number of arguments to place
// into float registers in the high bits. If float registers are not used
// then F = 0 so num_hidden_args_rf = R.
//
// The reason why the closure layout pointer is first is that most operations
// on closures do not need to access that word, and this way it does not
// have be brought into the cache.
//
// Note that the arguments are numbered from one, but the array subscripts
// start at zero. To prevent this from being a problem, we use a deliberately
// ugly name for the field containing the array, and provide a nicer way of
// referring to the array via a macro. The arguments of this macro number
// the arguments from one.

struct MR_Closure_Struct {
    MR_Closure_Layout       *MR_closure_layout;
    MR_Code                 *MR_closure_code;
    MR_Unsigned             MR_closure_num_hidden_args_rf;
    MR_Word                 MR_closure_hidden_args_0[MR_VARIABLE_SIZED];
};

// in mercury_types.h: typedef struct MR_Closure_Struct MR_Closure;

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_BOXED_FLOAT)
    #define MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
    #define MR_closure_num_hidden_r_args(c)                             \
        ((c)->MR_closure_num_hidden_args_rf & 0xffff)
    #define MR_closure_num_hidden_f_args(c)                             \
        ((c)->MR_closure_num_hidden_args_rf >> 16)
#else
    #define MR_closure_num_hidden_r_args(c)                             \
        ((c)->MR_closure_num_hidden_args_rf)
    #define MR_closure_num_hidden_f_args(c) 0
#endif

#define MR_closure_hidden_args(i)   MR_closure_hidden_args_0[(i) - 1]

#ifndef MR_HIGHLEVEL_CODE
  #ifdef MR_DO_CALL_STATS
    extern  void            MR_print_hidden_arg_stats(FILE *fp);
  #endif
#endif

// Build a closure for the given procedure address.
// This is used by browser/dl.m.
// MR_make_closure allocates heap, so call MR_{save,restore}_transient_hp()
// around calls to it.

extern  MR_Closure  *MR_make_closure(MR_Code *address);

#ifdef  MR_HIGHLEVEL_CODE

// Function declarations.

MR_bool MR_CALL mercury__builtin__unify_2_p_0(MR_Mercury_Type_Info,
                    MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_0(MR_Mercury_Type_Info,
                    MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_1(MR_Mercury_Type_Info,
                    MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_2(MR_Mercury_Type_Info,
                    MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_3(MR_Mercury_Type_Info,
                    MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_representation_3_p_0(
                    MR_Mercury_Type_Info, MR_Comparison_Result *,
                    MR_Box, MR_Box);

#else   // ! MR_HIGHLEVEL_CODE

MR_declare_entry(mercury__builtin__unify_2_0);
MR_declare_entry(mercury__builtin__compare_3_0);
MR_declare_entry(mercury__builtin__compare_3_1);
MR_declare_entry(mercury__builtin__compare_3_2);
MR_declare_entry(mercury__builtin__compare_3_3);
MR_declare_entry(mercury__builtin__compare_representation_3_0);

#endif  // MR_HIGHLEVEL_CODE

// Special predicates implemented in the standard library
// The structure is initialized by init_runtime_hooks in builtin.m.

typedef struct MR_SpecialPredHooks_Struct {
  #ifdef MR_HIGHLEVEL_CODE
    MR_bool MR_CALL (*MR_unify_tuple_pred)(MR_Word ti, MR_Word x, MR_Word y);
    void MR_CALL    (*MR_compare_tuple_pred)(MR_Word ti, MR_Word *res,
                        MR_Word x, MR_Word y);
    void MR_CALL    (*MR_compare_rep_tuple_pred)(MR_Word ti, MR_Word *res,
                        MR_Word x, MR_Word y);
  #else
    MR_Code         *MR_unify_tuple_pred;
    MR_Code         *MR_compare_tuple_pred;
    MR_Code         *MR_compare_rep_tuple_pred;
  #endif
} MR_SpecialPredHooks;

extern MR_SpecialPredHooks  MR_special_pred_hooks;

#endif  // not MERCURY_HO_CALL_H
