//
// Copyright (C) 2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

// mercury_mcpp.cpp - This file defines the system runtime types and
// methods that are used when generating code for the .NET backend.

// vi: ts=4 sw=4 et tw=0 wm=0

namespace mercury {

namespace runtime {

public class SystemException : System.Exception
{
    public SystemException(string Msg) : base(Msg)
    {
		// the parent constructor sets the error message that
		// will be printed.
    }
}

public class Errors
{
    public static void SORRY(string s)
    {
        string msg;
        msg = System.String.Concat("Sorry, unimplemented: ", s);
        throw new mercury.runtime.SystemException(msg);
    }

    public static void fatal_error(string s)
    {
        string msg;
        msg = System.String.Concat("Fatal error: ", s);
        throw new mercury.runtime.SystemException(msg);
    }
}

public class Environment
{
}

public class Commit : System.Exception
{
}

public class Constants
{
    // These constants are duplicated in library/private_builtin.m.
    // They must be kept sychronized.

	// XXX it would be nice if these could be const or an enum.  But
	// there are some problems with accessing the values from IL if we do
	// that because neither alternatives seem to define field names we
	// can reference from IL.

    public static int MR_TYPECTOR_REP_ENUM 			= MR_TYPECTOR_REP_ENUM_val;
    public static int MR_TYPECTOR_REP_ENUM_USEREQ 	= MR_TYPECTOR_REP_ENUM_USEREQ_val;
    public static int MR_TYPECTOR_REP_DU			= MR_TYPECTOR_REP_DU_val;
    public static int MR_TYPECTOR_REP_DU_USEREQ		= 3;
    public static int MR_TYPECTOR_REP_NOTAG			= 4;
    public static int MR_TYPECTOR_REP_NOTAG_USEREQ	= 5;
    public static int MR_TYPECTOR_REP_EQUIV			= 6;
    public static int MR_TYPECTOR_REP_FUNC			= 7;
    public static int MR_TYPECTOR_REP_INT		   	= 8;
    public static int MR_TYPECTOR_REP_CHAR		   	= 9;
    public static int MR_TYPECTOR_REP_FLOAT			=10;
    public static int MR_TYPECTOR_REP_STRING		=11;
    public static int MR_TYPECTOR_REP_PRED		   	=12;
    public static int MR_TYPECTOR_REP_SUBGOAL	   	=13;
    public static int MR_TYPECTOR_REP_VOID		  	=14;
    public static int MR_TYPECTOR_REP_C_POINTER		=15;
    public static int MR_TYPECTOR_REP_TYPEINFO		=16;
    public static int MR_TYPECTOR_REP_TYPECLASSINFO	=17;
    public static int MR_TYPECTOR_REP_ARRAY			=18;
    public static int MR_TYPECTOR_REP_SUCCIP		=19;
    public static int MR_TYPECTOR_REP_HP			=20;
    public static int MR_TYPECTOR_REP_CURFR			=21;
    public static int MR_TYPECTOR_REP_MAXFR			=22;
    public static int MR_TYPECTOR_REP_REDOFR		=23;
    public static int MR_TYPECTOR_REP_REDOIP		=24;
    public static int MR_TYPECTOR_REP_TRAIL_PTR		=25;
    public static int MR_TYPECTOR_REP_TICKET		=26;
    public static int MR_TYPECTOR_REP_NOTAG_GROUND	=27;
    public static int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ	=28;
    public static int MR_TYPECTOR_REP_EQUIV_GROUND	=29;
    public static int MR_TYPECTOR_REP_TUPLE			=30;
    public static int MR_TYPECTOR_REP_RESERVED_ADDR	=31;
    public static int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ	=32;
    public static int MR_TYPECTOR_REP_TYPECTORINFO	        =33;
    public static int MR_TYPECTOR_REP_BASETYPECLASSINFO     =34;
    public static int MR_TYPECTOR_REP_TYPEDESC	        	=35;
    public static int MR_TYPECTOR_REP_TYPECTORDESC	        =36;
    public static int MR_TYPECTOR_REP_FOREIGN			    =37;
    public static int MR_TYPECTOR_REP_REFERENCE		        =38;
    public static int MR_TYPECTOR_REP_STABLE_C_POINTER	    =39;
    public static int MR_TYPECTOR_REP_UNKNOWN		        =40;

    public static int MR_SECTAG_NONE				= 0;
    public static int MR_SECTAG_LOCAL				= 1;
    public static int MR_SECTAG_REMOTE				= 2;
}

}
}
