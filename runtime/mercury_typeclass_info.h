/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains design for a new runtime representation of typeclass
** information. It is not used yet. The mail that introduced this file
** follows.
*/

/*
The existing RTTI for typeclasses is sufficient for several purposes:

- to allow the runtime to perform method calls,
- to allow dictionaries to be constructed at runtime as needed, and
- to allow the debugger to discover the types of all arguments
  in the presence of typeclass constraints.

However, that is about all the functionality it supports. Its limitations
include

- one cannot deconstruct a typeclass_info and get back meaningful information
  (whereas deconstructing a typeinfo now works fine),
- one therefore cannot print typeclass_infos, e.g. in the debugger,
- one therefore cannot table typeclass_infos, and
- one cannot test at runtime whether a type vector is a member of a type class.

In addition, the existing implementation uses C in non-type-safe manner, which
makes direct ports to the Java and IL backends impossible, and makes changes
difficult to debug even in the C backends. One reason why this is a problem 
is that we wish to relax some of the current assumptions embedded in the
current system, including the RTTI. For example, type class constraints
on typeclass and instance declarations should be allowed to contain arbitrary
ground types; this would make some things easier for the HAL implementation.
Eventually we also want to support constructor classes and functional
dependencies.

I discussed these problems with dgj, dmo and rafe. During those discussions,
we came up with a draft of a new RTTI system for typeclasses, which I have
made concrete this weekend by turning it into the proposed set of C type
definitions contained in the attached file.

The new RTTI design records a lot more information than the old one, but
the growth is mostly in statically allocated data structures. The new design
also in type-safe C, for ease of debugging its implementation. There should
be a type-safe Mercury version of it as well, for future use in the Java and
IL backends. The reason why there isn't one yet is that some these structures
must contain references to polymporphic procedures, since methods may be
polymorphic. Mercury doesn't (yet) support polymorphic procedures, even if
(as here) only the compiler can create them.

The new design has three main data structures: one corresponding to a
typeclass declaration, one corresponding to an instance declaration, and one
corresponding to a fully solved type class constraint. The first two are
fully static; only the last, which contains the dictionaries that the runtime
needs for method invocation, may be dynamic.

Comments on welcome, on the design approach, on the design details, on the
naming scheme (which is very different from the naming scheme used by the
existing RTTI data structures), on the documentation, etc.

If there are no major objections, I will commit a version of this file,
unused but enhanced with macros that will allow the runtime to distinguish
the data structures used by the old RTI design from those used by the new,
in the runtime. DJ can then update the compiler to generate the new RTTI
structures and the runtime to handle the new RTTI structures as well as the old
(for bootstrapping). After that, I will add the functionality required to
deconstruct, print and table typeclass dictionaries. At present, we have
no plans to implement runtime typeclass membership tests, but it should not be
difficult to add later.

Zoltan.
*/

/*****************************************************************************/

typedef const struct MR_TypeClass_Struct                MR_TypeClassStruct;
typedef const struct MR_TypeClass_Struct                *MR_TypeClass;
typedef const struct MR_Instance_Struct                 MR_InstanceStruct;
typedef const struct MR_Instance_Struct                 *MR_Instance;
typedef       struct MR_Dictionary_Struct               MR_DictionaryStruct;
typedef       struct MR_Dictionary_Struct               *MR_Dictionary;

/*
** A typeclass skeleton is intended to represent a typeclass constraint
** being applied to a vector of possibly nonground types, as one may find
** constraining a typeclass declaration, an instance declaration, or a
** predicate/function declaration.
**
** Type class skeletons for type classes with arity N will be of type
** MR_TypeClassSkel_N. Generic code will manipulate them as if they were of
** type MR_TypeClassSkel, getting the actual number of arguments from
** MR_tc_skel_type_class_info->MR_tc_id->MR_tc_id_arity.
**
** Note that the arity cannot be zero, so we do not have to worry about
** zero-size arrays. On the other hand, type classes with more than even two
** arguments can be expected to be very rare, so have five as a fixed limit
** should not be a problem. If it is, we can lift the limit by defining
** MR_TypeClassSkel_N on demand for all N > 5.
**
** We will have to rethink this structure once we start supporting constructor
** classes.
*/

#define MR_DEFINE_TYPECLASS_SKEL_STRUCT(NAME, ARITY)                        \
    typedef struct MR_PASTE2(NAME, _Struct) {                               \
        MR_TypeClass        MR_tc_skel_type_class_info;                     \
        MR_PseudoTypeInfo   MR_tc_skel_arg_ptis[ARITY];                     \
    } MR_PASTE2(NAME, Struct)

MR_DEFINE_TYPECLASS_SKEL_STRUCT(MR_TypeClassSkel_1, 1);
MR_DEFINE_TYPECLASS_SKEL_STRUCT(MR_TypeClassSkel_2, 2);
MR_DEFINE_TYPECLASS_SKEL_STRUCT(MR_TypeClassSkel_3, 3);
MR_DEFINE_TYPECLASS_SKEL_STRUCT(MR_TypeClassSkel_4, 4);
MR_DEFINE_TYPECLASS_SKEL_STRUCT(MR_TypeClassSkel_5, 5);

typedef MR_TypeClassSkel_5Struct        MR_TypeClassSkelStruct;
typedef MR_TypeClassSkelStruct          *MR_TypeClassSkel;

#define MR_STD_TYPECLASS_SKEL_ADDR(p)   ((MR_TypeClassSkel)                 \
                                        &((p).MR_tc_skel_type_class_info))

/*
** We generate one static MR_TypeClassMethod structure for every method in
** every typeclass declaration in the program.
**
** The MR_tc_method_pred_func field says whether the method is a predicate
** or a function. The MR_tc_method_name gives the method's name.
** The MR_tc_method_arity field gives the number of programmer-visible
** arguments of the method (including the return value for functions).
** These are sufficient to uniquely identify a method within its
** type class declaration.
**
** We may wish to extend this structure later, for two purposes.
**
** The first is to record information that can make it easier to perform
** a method call, such as the number of typeinfos and (old-style)
** typeclassinfos in the method's signature.
**
** The second is to record information that will enable us to test at runtime
** whether a given predicate or function matches the signature of the method;
** we need this if we want to test at runtime whether a type vector is a member
** of a given type class.
*/

typedef struct {
    MR_ConstString              MR_tc_method_name;
    const MR_int_least8_t       MR_tc_method_arity;
    const MR_PredFunc           MR_tc_method_pred_func;
} MR_TypeClassMethod;

/*
** MR_TypeClassId structures are intended to provide a printable representation
** for typeclass declarations, for use in e.g. the debugger. There will be one
** static MR_TypeClassId structure for every typeclass declaration.
**
** The MR_tc_id_module field contains the name of the module that defines the
** type class, the MR_tc_id_name field contains the name of the type class
** itself, while the MR_tc_id_arity field gives its arity (i.e. how many types
** it applies to). These are sufficient to uniquely identify the type class
** declaration.
**
** The remaining fields are there to enable the type class declaration to be
** reconstructed, if need be.
**
** The MR_tc_id_num_type_vars field gives the number of type variables in the
** whole declaration, and the MR_tc_id_type_var_names field gives their names,
** with the name of type variable N being stored at index N-1. Type variables
** 1 .. MR_tc_id_arity will be the ones in the head of the declaration.
**
** The MR_tc_id_num_methods field gives the number of methods required by the
** type class, while the MR_tc_id_methods field points to a vector of pointers
** to method descriptors, one for each method.
*/

typedef struct {
    MR_ConstString              MR_tc_id_module;
    MR_ConstString              MR_tc_id_name;
    const MR_int_least8_t       MR_tc_id_arity;
    const MR_int_least8_t       MR_tc_id_num_type_vars;
    const MR_int_least8_t       MR_tc_id_num_methods;
    const MR_ConstString        *MR_tc_id_type_var_names;
    const MR_TypeClassMethod    **MR_tc_id_methods;
} MR_TypeClassId;

/*
** We generate one static MR_TypeClass structure for each typeclass
** declaration in the program.
**
** The MR_tc_id field gives a printable representation of the declaration.
** We point to it instead of including it because we want to allow its size to
** change without affecting binary compatibility.
**
** The MR_tc_num_super field gives the number of superclasses, while the
** MR_tc_supers field points to a vector of pointers to superclass descriptors,
** one for each superclass. (The reason why the vector elements are pointers to
** descriptors instead of descriptors themselves is that superclasses with
** different arities have different sizes, so putting them into an array is not
** practical.)
*/

struct MR_TypeClass_Struct {
    const MR_TypeClassId        *MR_tc_id;
    const MR_int_least8_t       MR_tc_num_super;
    const MR_TypeClassSkel      *MR_tc_supers;
};

/*
** We generate one static MR_Instance structure for each instance declaration
** in the program.
**
** The MR_tc_inst_type_class field specifies the typeclass that the instance
** declaration creates new instances of.
**
** The MR_tc_inst_type_args field points to a vector of MR_PseudoTypeInfos
** whose length is MR_tc_inst_type_class->MR_tc_id->MR_tc_id_arity; each
** pseudotypeinfo in this vector will describe the (possibly nonground) type
** in the corresponding position in the head of the instance declaration.
**
** The MR_tc_inst_num_type_vars field gives the number of type variables
** occurring in the instance declaration; for now, all these type variables
** must occur on the left hand side of the declaration. No type variable
** occurring in the pseudo-typeinfos in the vector pointed to by the
** MR_tc_inst_type_args field may have a number higher than the value of this
** field.
**
** The MR_tc_inst_num_instance_constraints field gives the number of typeclass
** constraints on the instance declaration itself, while the
** MR_tc_inst_instance_constraints field gives the constraints themselves.
**
** The MR_tc_inst_methods field gives the methods declared by the instance
** declaration. It points to a vector of code addresses, one for each method;
** the length of the vector is MR_tc_inst_type_class->MR_tc_id->
** MR_tc_id_num_methods. The procedures being pointed to may be polymorphic,
** for either one of two reasons: the instance declaration may specify
** nonground types, and the method may have universally quantified type
** variables in its signature in any case.
*/

struct MR_Instance_Struct {
    const MR_TypeClass          MR_tc_inst_type_class;
    const MR_PseudoTypeInfo     *MR_tc_inst_type_args;
    const MR_int_least8_t       MR_tc_inst_num_type_vars;
    const MR_int_least8_t       MR_tc_inst_num_instance_constraints;
    const MR_TypeClassSkel      *MR_tc_inst_instance_constraints;
    const MR_Code               *MR_tc_inst_methods;
};

/*
** An MR_ClassDict structure gives the methods for a ground instance of a type
** class. These structures can be static or dynamic. Whenever the type bindings
** are known, the compiler should generate them statically; in cases where
** the type bindings become known only at runtime, we need to create them
** dynamically.
**
** The MR_class_dict_class field identifies the type class, while the
** MR_class_dict_type_binding field, which points to a vector of typeinfos
** whose length is MR_class_dict_class->MR_tc_id->MR_tc_id_arity, identifies
** the ground instance.
**
** The MR_class_dict_methods field, which points to a vector whose length is
** MR_class_dict_class->MR_tc_id->MR_tc_id_num_methods, gives the methods
** themselves. A method procedure will be polymorphic only if its signature
** includes type variables that are not parameters of the type class.
** 
** The MR_class_dict_version_number field is needed only for bootstrapping.
** Initially, it should always contain zero. Do_call_class_method in the
** runtime system can then distingish distinguish MR_ClassDicts from the
** old-style typeclass_infos filling the same role, which all contain a
** non-null pointer to a base_typeclass_info in their first word. Later,
** we can use different values in this field to distinguish different versions
** of this design at runtime, just as we do for type_ctor_infos.
*/

typedef struct {
    MR_Integer              MR_class_dict_version_number;
    MR_TypeClass            MR_class_dict_class;
    MR_TypeInfo             *MR_class_dict_type_binding;
    MR_Code                 *MR_class_dict_methods;
} MR_ClassDict;

#define MR_TYPECLASS_VERSION    0

/*
** A MR_Dictionary_Struct structure corresponds to a fully solved type class
** constraint. They can be either static and dynamic, for the same reasons as
** MR_ClassDict structures.
**
** The MR_dict_top_instance field gives the top instance declaration that this
** proof derives from.
**
** The MR_dict_class_methods field points to a single class dictionary that
** gives the implementations of the methods of this type class.
**
** The MR_dict_superclass_dicts field points to a vector of pointers to
** dictionaries. The number of elements in the vector will be given by
** MR_dict_top_instance->MR_class_dict_class->MR_tc_id->MR_tc_id_num_supers.
** The element in the vector at index N+1 will specify the dictionary for this
** instance of the Nth superclass constraint on the typeclass declaration.
** (The +1 is because array numbering starts at 0.)
*/

typedef struct {
    MR_Instance             *MR_dict_top_instance;
    MR_ClassDict            *MR_dict_class_methods;
    MR_ClassDict            **MR_dict_superclass_dicts;
} MR_Dictionary_Struct;
