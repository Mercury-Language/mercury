//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
	
package mercury.runtime;

public class TypeLayout {
		//
		// In runtime/mercury_type_info.h:
		// typedef MR_DuPtagLayout *MR_DuTypeLayout;
		// so here we just use DuPtagLayout[]
		//
	public mercury.runtime.DuPtagLayout[] layout_du;
		//
		// In runtime/mercury_type_info.h:
		// typedef MR_EnumFunctorDesc **EnumTypeLayout;
		// so here we just use EnumFunctorDesc[][]
		//
	public mercury.runtime.EnumFunctorDesc[] layout_enum;
		//
		// In runtime/mercury_type_info.h:
		// typedef MR_NotagFunctorDesc *MR_NotagTypeLayout;
		// so here we just us NotagFunctorDesc[]
		//
	public mercury.runtime.NotagFunctorDesc[] layout_notag;
		//
		// In runtime/mercury_type_info.h:
		// typedef MR_PseudoTypeInfo MR_EquivType;
		// so here we just use MR_PseudoTypeInfo
		//
	public mercury.runtime.PseudoTypeInfo layout_equiv;
}
	
