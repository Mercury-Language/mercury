//
// Copyright (C) 2000-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

// mercury_mcpp.cpp - This file defines the system runtime types and
// methods that are used when generating code for the .NET backend.
// It is written using Microsoft's Managed Extensions for C++ (usually
// called Managed C++ or MC++).

// vi: ts=4 sw=4 et tw=0 wm=0

#using <mscorlib.dll>
#using "mercury_il.dll"

    // This line (somehow) stops the compiler from
    // linking in the C library (and it will then complain about main being
    // missing)
extern "C" int _fltused=0;

#include "mercury_mcpp.h"

namespace mercury {

namespace runtime {

	// A user exception -- really just a wrapper for the exception
	// data.

__gc public class Exception : public System::Exception
{
public:
   Exception(MR_Word data) 
   {
   	mercury_exception = data;	
   }
   MR_Word mercury_exception;
};

__gc public class SystemException : public System::Exception
{
public:
	SystemException(MR_String Msg) : System::Exception(Msg)
	{	
		// the parent constructor sets the error message that
		// will be printed.
	}
};


__gc public class LowLevelData
{

public:
	// Make a Mercury enumeration with the given integer value.
static MR_Word make_enum(int enum_value) {

	MR_Word e;
	MR_newenum(e, enum_value);
	return e;

}

	// Make an MR_Word with the given tag and arity.
static MR_Word make_MR_Word(int tag, int arity) {
	MR_Word e;
	MR_newobj(e, tag, arity);
	return e;

}
	// Set a field of an MR_Word with a given value.
	// The first field is at index 1.
static void set_MR_Word_field(MR_Word w, int index, System::Object *value) {
	MR_objset(w, index, value);
}
	// Get the value from an MR_Word.
	// The first field is at index 1.
static System::Object * get_MR_Word_field(MR_Word w, int index) {
	return w[index];
}

static bool list_is_cons(MR_Word w) {
	return (System::Convert::ToInt32(w[0]) != 0);
}

static MR_Box list_get_head(MR_Word w) {
	return w[1];
}

static MR_Word list_get_tail(MR_Word w) {
	return dynamic_cast<MR_Word>(w[2]);
}

};

__gc public class Errors {
    public:
    static void SORRY(MR_String s) 
    {
        MR_String msg;
        msg = System::String::Concat("Sorry, unimplemented: ", s);
        throw new mercury::runtime::SystemException(msg);
    }

    static void fatal_error(MR_String s)
    {
        MR_String msg;
        msg = System::String::Concat("Fatal error: ", s);
        throw new mercury::runtime::SystemException(msg);
    }
};


__gc public class Constants {
    public:

        // These constants are duplicated in library/private_builtin.m.
        // They must be kept sychronized.

	// XXX it would be nice if these could be const or an enum.  But
	// there are some problems with accessing the values from IL if we do
	// that because neither alternatives seem to define field names we
	// can reference from IL.

    static int MR_TYPECTOR_REP_ENUM 			= MR_TYPECTOR_REP_ENUM_val;
    static int MR_TYPECTOR_REP_ENUM_USEREQ 		= MR_TYPECTOR_REP_ENUM_USEREQ_val;
    static int MR_TYPECTOR_REP_DU			= MR_TYPECTOR_REP_DU_val;
    static int MR_TYPECTOR_REP_DU_USEREQ		= 3;
    static int MR_TYPECTOR_REP_NOTAG			= 4;
    static int MR_TYPECTOR_REP_NOTAG_USEREQ		= 5;
    static int MR_TYPECTOR_REP_EQUIV			= 6;
    static int MR_TYPECTOR_REP_FUNC			= 7;
    static int MR_TYPECTOR_REP_INT		    	= 8;
    static int MR_TYPECTOR_REP_CHAR		    	= 9;
    static int MR_TYPECTOR_REP_FLOAT			=10;
    static int MR_TYPECTOR_REP_STRING			=11;
    static int MR_TYPECTOR_REP_PRED		    	=12;
	// MR_TYPECTOR_REP_UNIV is unused - it is retained
	// only for backwards compatability.
    static int MR_TYPECTOR_REP_UNIV		    	=13;
    static int MR_TYPECTOR_REP_VOID		    	=14;
    static int MR_TYPECTOR_REP_C_POINTER		=15;
    static int MR_TYPECTOR_REP_TYPEINFO			=16;
    static int MR_TYPECTOR_REP_TYPECLASSINFO		=17;
    static int MR_TYPECTOR_REP_ARRAY			=18;
    static int MR_TYPECTOR_REP_SUCCIP			=19;
    static int MR_TYPECTOR_REP_HP			=20;
    static int MR_TYPECTOR_REP_CURFR			=21;
    static int MR_TYPECTOR_REP_MAXFR			=22;
    static int MR_TYPECTOR_REP_REDOFR			=23;
    static int MR_TYPECTOR_REP_REDOIP			=24;
    static int MR_TYPECTOR_REP_TRAIL_PTR		=25;
    static int MR_TYPECTOR_REP_TICKET			=26;
    static int MR_TYPECTOR_REP_NOTAG_GROUND		=27;
    static int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ	=28;
    static int MR_TYPECTOR_REP_EQUIV_GROUND		=29;
    static int MR_TYPECTOR_REP_TUPLE			=30;
    static int MR_TYPECTOR_REP_RESERVED_ADDR		=31;
    static int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ	=32;
    static int MR_TYPECTOR_REP_TYPECTORINFO	        =33;
    static int MR_TYPECTOR_REP_BASETYPECLASSINFO        =34;
    static int MR_TYPECTOR_REP_UNKNOWN		        =35;

    static int MR_SECTAG_NONE				= 0;
    static int MR_SECTAG_LOCAL				= 1;
    static int MR_SECTAG_REMOTE				= 2;
};

__gc public class Environment
{
public:
};

__gc public class Commit : public System::Exception
{
public:
};

}

}

