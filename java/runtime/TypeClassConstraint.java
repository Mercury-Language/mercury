//
// Copyright (C) 2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

// This corresponds to the C type MR_TypeClassConstraint
// in runtime/mercury_type_info.h.

public class TypeClassConstraint {
	public TypeClassDeclStruct	tc_constr_type_class;
	public PseudoTypeInfo		tc_constr_arg_ptis[];

	public TypeClassConstraint()
	{
	}

	public void init(TypeClassDeclStruct type_class,
		// XXX Object[] should be mercury.runtime.PseudoTypeInfo[],
		//     but mlds_to_java.m generates Object[] since
		//     init_array/1 doesn't give type info
		Object[] ptis)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[ptis.length];
		for (int i = 0; i < ptis.length; i++) {
			tc_constr_arg_ptis[i] = (PseudoTypeInfo) ptis[i];
		}
	}
}
