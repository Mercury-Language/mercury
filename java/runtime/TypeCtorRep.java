//
// Copyright (C) 2001-2005 The University of Melbourne.
// Copyright (C) 2016-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class TypeCtorRep implements java.io.Serializable {

        public static final int MR_TYPECTOR_REP_ENUM                    = 0;
        public static final int MR_TYPECTOR_REP_ENUM_USEREQ             = 1;
        public static final int MR_TYPECTOR_REP_DU                      = 2;
        public static final int MR_TYPECTOR_REP_DU_USEREQ               = 3;
        public static final int MR_TYPECTOR_REP_NOTAG                   = 4;
        public static final int MR_TYPECTOR_REP_NOTAG_USEREQ            = 5;
        public static final int MR_TYPECTOR_REP_EQUIV                   = 6;
        public static final int MR_TYPECTOR_REP_FUNC                    = 7;
        public static final int MR_TYPECTOR_REP_INT                     = 8;
        public static final int MR_TYPECTOR_REP_UINT                    = 9;
        public static final int MR_TYPECTOR_REP_CHAR                    = 10;
        public static final int MR_TYPECTOR_REP_FLOAT                   = 11;
        public static final int MR_TYPECTOR_REP_STRING                  = 12;
        public static final int MR_TYPECTOR_REP_PRED                    = 13;
        public static final int MR_TYPECTOR_REP_SUBGOAL                 = 14;
        public static final int MR_TYPECTOR_REP_VOID                    = 15;
        public static final int MR_TYPECTOR_REP_C_POINTER               = 16;
        public static final int MR_TYPECTOR_REP_TYPEINFO                = 17;
        public static final int MR_TYPECTOR_REP_TYPECLASSINFO           = 18;
        public static final int MR_TYPECTOR_REP_ARRAY                   = 19;
        public static final int MR_TYPECTOR_REP_SUCCIP                  = 20;
        public static final int MR_TYPECTOR_REP_HP                      = 21;
        public static final int MR_TYPECTOR_REP_CURFR                   = 22;
        public static final int MR_TYPECTOR_REP_MAXFR                   = 23;
        public static final int MR_TYPECTOR_REP_REDOFR                  = 24;
        public static final int MR_TYPECTOR_REP_REDOIP                  = 25;
        public static final int MR_TYPECTOR_REP_TRAIL_PTR               = 26;
        public static final int MR_TYPECTOR_REP_TICKET                  = 27;
        public static final int MR_TYPECTOR_REP_NOTAG_GROUND            = 28;
        public static final int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ     = 29;
        public static final int MR_TYPECTOR_REP_EQUIV_GROUND            = 30;
        public static final int MR_TYPECTOR_REP_TUPLE                   = 31;
        public static final int MR_TYPECTOR_REP_RESERVED_ADDR           = 32;
        public static final int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ    = 33;
        public static final int MR_TYPECTOR_REP_TYPECTORINFO            = 34;
        public static final int MR_TYPECTOR_REP_BASETYPECLASSINFO       = 35;
        public static final int MR_TYPECTOR_REP_TYPEDESC                = 36;
        public static final int MR_TYPECTOR_REP_TYPECTORDESC            = 37;
        public static final int MR_TYPECTOR_REP_FOREIGN                 = 38;
        public static final int MR_TYPECTOR_REP_REFERENCE               = 39;
        public static final int MR_TYPECTOR_REP_STABLE_C_POINTER        = 40;
        public static final int MR_TYPECTOR_REP_STABLE_FOREIGN          = 41;
        public static final int MR_TYPECTOR_REP_PSEUDOTYPEDESC          = 42;
        public static final int MR_TYPECTOR_REP_DUMMY                   = 43;
        public static final int MR_TYPECTOR_REP_BITMAP                  = 44;
        public static final int MR_TYPECTOR_REP_FOREIGN_ENUM            = 45;
        public static final int MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ     = 46;
        public static final int MR_TYPECTOR_REP_INT8                    = 47;
        public static final int MR_TYPECTOR_REP_UINT8                   = 48;
        public static final int MR_TYPECTOR_REP_INT16                   = 49;
        public static final int MR_TYPECTOR_REP_UINT16                  = 50;
        public static final int MR_TYPECTOR_REP_INT32                   = 51;
        public static final int MR_TYPECTOR_REP_UINT32                  = 52;
        public static final int MR_TYPECTOR_REP_INT64                   = 53;
        public static final int MR_TYPECTOR_REP_UINT64                  = 54;
        public static final int MR_TYPECTOR_REP_UNKNOWN                 = 55;
        public static final int MR_TYPECTOR_REP_MAX                     = 56;

        // Instance variable for TypeCtorRep objects.
	
	public int value;

	// Constructor

	public TypeCtorRep(int arg) {
		this.value = arg;
	}
}
	
// vim: set ts=8 sts=8 sw=8 noet:
