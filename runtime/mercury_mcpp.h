//
// Copyright (C) 2000-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

// mercury_mcpp.h - This file defines the system runtime types and
// macros that are used when generating code for the .NET backend.
// It is written using Managed Extensions for C++ (usually called Managed C++ 
// or MC++).


// We need a definition of NULL
#include <stddef.h>


namespace mercury {

typedef int		MR_Integer;
typedef System::Int32	MR_BoxedInt;
typedef System::Boolean	MR_Bool;
typedef System::Boolean	MR_bool;

typedef System::Char	MR_Char; // `Char' is MS's name for unicode characters

typedef double 		MR_Float;
	// XXX using a typedef doesn't seem to work properly when we want
	// to use methods -- MR_BoxedFloat::somemethod() doesn't seem to work.
	// This might be an issue with value classes and typedefs.
#define MR_BoxedFloat System::Double

typedef System::String *MR_String;
typedef void (*MR_Cont) (void *);


// Should these be MR_ qualified?
#define TRUE 1
#define FALSE 0
#define MR_TRUE 1
#define MR_FALSE 0



typedef __gc public class System::Object * MR_Word[];
typedef __gc public class System::Object * MR_Box;
typedef __gc public class System::Array  * MR_Array;

#define MR_Ref(type) type __gc *
typedef MR_Ref(MR_Box) MR_Box_Ref;
typedef MR_Ref(MR_Word) MR_Word_Ref;

/*
#ifdef MR_HIGHLEVEL_DATA
  typedef __gc public class mercury::private_builtin::type_info_1* MR_TypeInfo;
  typedef __gc public class mercury::rtti_implementation::type_info_0* MR_TypeInfo_0;
  typedef __gc public class mercury::builtin::comparison_result_0 *MR_ComparisonResult;
  typedef __gc public class mercury::std_util::univ_0 *MR_Univ;
#else
*/
  typedef __gc public class System::Object * MR_TypeInfo[];
  typedef __gc public class System::Object * MR_TypeInfo_0[];
  typedef __gc public class System::Object * MR_ComparisonResult[];
  typedef __gc public class System::Object * MR_Univ[];
// #endif

typedef __gc public class System::Object * MR_TypeCtorInfo[];
typedef __gc public class System::Object * MR_TypeInfoParams[];
typedef __gc public class System::Object * MR_TypeClassInfo[];


// XXX This code is duplicated in mercury_type_info.h.
// We should factor out these definitions and use a shared version.

#define MR_COMPARE_EQUAL 0
#define MR_COMPARE_LESS 1
#define MR_COMPARE_GREATER 2

#define MR_STRINGIFY(x)			MR_STRINGIFY_2(x)
#define MR_STRINGIFY_2(x)		#x

#define MR_PASTE2(a,b)			MR_PASTE2_2(a,b)
#define MR_PASTE2_2(a,b)		a##b
#define MR_PASTE3(a,b,c)		MR_PASTE3_2(a,b,c)
#define MR_PASTE3_2(a,b,c)		a##b##c
#define MR_PASTE4(a,b,c,d)		MR_PASTE4_2(a,b,c,d)
#define MR_PASTE4_2(a,b,c,d)		a##b##c##d
#define MR_PASTE5(a,b,c,d,e)		MR_PASTE5_2(a,b,c,d,e)
#define MR_PASTE5_2(a,b,c,d,e)		a##b##c##d##e
#define MR_PASTE6(a,b,c,d,e,f)		MR_PASTE6_2(a,b,c,d,e,f)
#define MR_PASTE6_2(a,b,c,d,e,f)	a##b##c##d##e##f
#define MR_PASTE7(a,b,c,d,e,f,g)	MR_PASTE7_2(a,b,c,d,e,f,g)
#define MR_PASTE7_2(a,b,c,d,e,f,g)	a##b##c##d##e##f##g

// The code to generate RTTI structures is somewhat complicated.
// For each RTTI symbol, we generate an initializer method, 
// and a field.  The initializer method returns a value for the field.
// Since it is a static field, the initializer will be run in the class
// constructor.
//
// This code is intended to be more general than the macros in
// mercury_typeinfo.h -- by conditionally defining the appropriate 
// #defines for MR_CLASS_INIT* and MR_STRUCT_INIT* you should be able to 
// re-use this code, however this has not been done yet.

// In the .NET backend, we don't need to forward declare RTTI structures.
#define MR_Declare_entry(a) 	
#define MR_Declare_struct(a) 

// We have to jump through a few hoops to get function pointers -- we do
// it in IL currently.  We treat function pointers as integers and have
// to box them.
#define MR_BOX_INT(a) __box(a)
#define MR_MAYBE_STATIC_CODE(a) \
	MR_BOX_INT(mercury::runtime::TempHack::get_ftn_ptr_##a())
#define MR_ENTRY(a) a
// XXX MR_ENTRY appears to be unused

// Code to handle initialization of fields.
#define MR_STRUCT_INIT(a) 
#define MR_STRUCT_INIT_END(a) 
#define MR_CLASS_INIT(a) \
	static MR_Word a(void) { \
		System::Object *arr[] = { 
#define MR_CLASS_INIT_END(m, f, i)	\
		};			\
		return arr;		\
	}				\
	static MR_Word f = i();		\
	static MR_Word MR_PASTE2(m, f) = i();

#define MR_string_const(a, s) ((MR_String) a)
#define MR_TYPECTOR_REP(a) MR_BOX_INT(mercury::runtime::Constants::a)

// XXX This is hardcoded
#define MR_RTTI_VERSION MR_BOX_INT(7)

// XXX It is intended that we eventually define the constants in
// private_builtin.m and mercury_mcpp.cpp in terms of these #defines
// instead of hard-coding the values. 
#define MR_TYPECTOR_REP_ENUM_val 			0
#define MR_TYPECTOR_REP_ENUM_USEREQ_val 		1
#define MR_TYPECTOR_REP_DU_val				2
#define MR_TYPECTOR_REP_DU_USEREQ_val			3
#define MR_TYPECTOR_REP_NOTAG_val			4
#define MR_TYPECTOR_REP_NOTAG_USEREQ_val		5
#define MR_TYPECTOR_REP_EQUIV_val			6
#define MR_TYPECTOR_REP_FUNC_val			7
#define MR_TYPECTOR_REP_INT_val				8
#define MR_TYPECTOR_REP_CHAR_val			9
#define MR_TYPECTOR_REP_FLOAT_val			10
#define MR_TYPECTOR_REP_STRING_val			11
#define MR_TYPECTOR_REP_PRED_val			12
	// MR_TYPECTOR_REP_UNIV_val is unused - it is retained
	// only for backwards compatability.
#define MR_TYPECTOR_REP_UNIV_val			13
#define MR_TYPECTOR_REP_VOID_val			14
#define MR_TYPECTOR_REP_C_POINTER_val			15
#define MR_TYPECTOR_REP_TYPEINFO_val			16
#define MR_TYPECTOR_REP_TYPECLASSINFO_val		17
#define MR_TYPECTOR_REP_ARRAY_val			18
#define MR_TYPECTOR_REP_SUCCIP_val			19
#define MR_TYPECTOR_REP_HP_val				20
#define MR_TYPECTOR_REP_CURFR_val			21
#define MR_TYPECTOR_REP_MAXFR_val			22
#define MR_TYPECTOR_REP_REDOFR_val			23
#define MR_TYPECTOR_REP_REDOIP_val			24
#define MR_TYPECTOR_REP_TRAIL_PTR_val			25
#define MR_TYPECTOR_REP_TICKET_val			26
#define MR_TYPECTOR_REP_NOTAG_GROUND_val		27
#define MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ_val		28
#define MR_TYPECTOR_REP_EQUIV_GROUND_val		29
#define MR_TYPECTOR_REP_TUPLE_val			30
#define MR_TYPECTOR_REP_RESERVED_ADDR_val		31
#define MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ_val	32
#define MR_TYPECTOR_REP_TYPECTORINFO_val		33
#define MR_TYPECTOR_REP_BASETYPECLASSINFO_val		34
#define MR_TYPECTOR_REP_TYPEDESC_val			35
#define MR_TYPECTOR_REP_TYPECTORDESC_val		36
#define MR_TYPECTOR_REP_FOREIGN_val			37
#define MR_TYPECTOR_REP_UNKNOWN_val			38

// XXX we should integrate this macro in with the version in 
// mercury_typeinfo.h
#define MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(m, n, a, cr, u, c)	\
    MR_Declare_entry(u)							\
    MR_Declare_entry(c)							\
    MR_Declare_struct(MR_STATIC_CODE_CONST struct MR_TypeCtorInfo_Struct)  \
    MR_STRUCT_INIT(MR_PASTE5(mercury_data_, __type_ctor_info_, n, _, a) = {)   \
    MR_CLASS_INIT(MR_PASTE4(type_ctor_init_, n, _, a))   		\
	MR_BOX_INT(a),							\
	MR_RTTI_VERSION,						\
	MR_BOX_INT(-1),							\
	MR_TYPECTOR_REP(cr),						\
	MR_MAYBE_STATIC_CODE(n##_unify),				\
	MR_MAYBE_STATIC_CODE(n##_compare),				\
	MR_string_const(MR_STRINGIFY(m), sizeof(MR_STRINGIFY(m))-1),	\
	MR_string_const(MR_STRINGIFY(n), sizeof(MR_STRINGIFY(n))-1),	\
	MR_BOX_INT(-1),							\
	MR_BOX_INT(-1),							\
	MR_BOX_INT(-1)							\
    MR_STRUCT_INIT_END(})						\
    MR_CLASS_INIT_END(m, MR_PASTE5(__, type_ctor_info_, n, _, a), MR_PASTE4(type_ctor_init_, n, _, a))

#define MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(m, n, a, cr, u, c)	\
    MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(m, n, a, cr, u, c)

#define MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(m, n, a, cr)		\
    MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(m, n, a, cr,		\
	MR_PASTE7(mercury::, m, ::do_unify__, n, _, a, _0),     \
	MR_PASTE7(mercury::, m, ::do_compare__, n, _, a, _0))  

#define MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(n, a, cr)       \
    MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, n, a, cr,  \
	mercury__unused_0_0,					\
	mercury__unused_0_0)


// Some definitions for writing code by hand that constructs lists.
// Note that this is very dependent on the data representation chosen
// by the compiler.

#define MR_list_cons(List, Head, Tail)				\
    	do {							\
		MR_Word _tmp;					\
		MR_newobj((_tmp), 1, 2);			\
		MR_objset((_tmp), 1, (Head));			\
		MR_objset((_tmp), 2, (Tail));			\
		List = _tmp;					\
	} while (0)

#define MR_list_nil(List)					\
    	MR_newobj(List, 0, 0);

#define MR_list_is_cons(List)	\
	(System::Convert::ToInt32((List)->GetValue(0)))

#define MR_list_is_nil(List)	\
	(System::Convert::ToInt32((List)->GetValue(0)) == 0)

#define MR_list_head(List)	\
	((List)->GetValue(1))

#define MR_list_tail(List)	\
	(dynamic_cast<MR_Word>((List)->GetValue(2)))


// Some definitions for writing code by hand that constructs any type.

#define MR_newobj(Obj, Tag, Size)					\
    do {								\
	(Obj) = new System::Object __gc * __gc[(Size + 1)];		\
	(Obj)[0] = MR_BOX_INT(Tag);					\
    } while (0)

#define MR_untagged_newobj(Obj, Size)					\
    do {								\
	(Obj) = new System::Object __gc * __gc[(Size)];			\
    } while (0)

#define MR_newobj_preboxed_tag(Obj, Tag, Size)				\
    do {								\
	(Obj) = new System::Object __gc * __gc[(Size + 1)];		\
	(Obj)[0] = (Tag);						\
    } while (0)

#define MR_objset(Obj, Offset, Element)					\
    do {								\
	(Obj)[(Offset)] = Element;					\
    } while (0)

#define MR_c_pointer_to_word(Obj, CPointer)				\
    	MR_newobj_preboxed_tag(Obj, CPointer, 0)

#define MR_word_to_c_pointer(CPointer)					\
    	( (CPointer)[0] )

#define MR_newenum(Obj, Tag)						\
	MR_newobj(Obj, Tag, 0)


// A few macros to define some RTTI slots.
// At the moment RTTI support in the .NET backend is very minimal.


#define MR_TYPEINFO_TYPE_CTOR_INFO_SLOT		0

#define MR_TYPE_CTOR_INFO_ARITY_SLOT		0
#define MR_TYPE_CTOR_INFO_UNIFY_PRED_SLOT	5
#define MR_TYPE_CTOR_INFO_COMPARE_PRED_SLOT	6

} /* end namespace mercury */

