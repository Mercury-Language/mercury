//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
	
package mercury.runtime;

public class TypeLayout {
		// This should hold a value of one of the types
		// accessible by the access functions that follow.
	public java.lang.Object layout_init;

		//
		// In runtime/mercury_type_info.h:
		// typedef MR_DuPtagLayout *MR_DuTypeLayout;
		// so here we just use DuPtagLayout[]
		//
	public mercury.runtime.DuPtagLayout[] layout_du() {
		return (mercury.runtime.DuPtagLayout[]) layout_init;
	}

		//
		// In runtime/mercury_type_info.h:
		// typedef MR_EnumFunctorDesc **EnumTypeLayout;
		// so here we just use EnumFunctorDesc[][]
		//
	public mercury.runtime.EnumFunctorDesc[] layout_enum() {
		return (mercury.runtime.EnumFunctorDesc[]) layout_init;
	}

		//
		// In runtime/mercury_type_info.h:
		// typedef MR_NotagFunctorDesc *MR_NotagTypeLayout;
		// so here we just us NotagFunctorDesc[]
		//
	public mercury.runtime.NotagFunctorDesc[] layout_notag() {
		return (mercury.runtime.NotagFunctorDesc[]) layout_init;
	}
		//
		// In runtime/mercury_type_info.h:
		// typedef MR_PseudoTypeInfo MR_EquivType;
		// so here we just use MR_PseudoTypeInfo
		//
	public mercury.runtime.PseudoTypeInfo layout_equiv() {
		return (mercury.runtime.PseudoTypeInfo) layout_init;
	}

	public TypeLayout(java.lang.Object init) {
		layout_init = init;
	}
}
