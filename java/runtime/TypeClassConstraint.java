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

	public TypeClassConstraint(TypeClassDeclStruct type_class)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[] {};
	}

	public TypeClassConstraint(TypeClassDeclStruct type_class,
		PseudoTypeInfo pti1)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[] { pti1 };
	}

	public TypeClassConstraint(TypeClassDeclStruct type_class,
		PseudoTypeInfo pti1, PseudoTypeInfo pti2)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[] { pti1, pti2 };
	}

	public TypeClassConstraint(TypeClassDeclStruct type_class,
		PseudoTypeInfo pti1, PseudoTypeInfo pti2,
		PseudoTypeInfo pti3)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[] { pti1, pti2, pti3 };
	}

	public TypeClassConstraint(TypeClassDeclStruct type_class,
		PseudoTypeInfo pti1, PseudoTypeInfo pti2,
		PseudoTypeInfo pti3, PseudoTypeInfo pti4)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[]
			{ pti1, pti2, pti3, pti4 };
	}

	public TypeClassConstraint(TypeClassDeclStruct type_class,
		PseudoTypeInfo pti1, PseudoTypeInfo pti2,
		PseudoTypeInfo pti3, PseudoTypeInfo pti4,
		PseudoTypeInfo pti5)
	{
		tc_constr_type_class = type_class;
		tc_constr_arg_ptis = new PseudoTypeInfo[] {
			pti1, pti2, pti3, pti4, pti5 };
	}

	// XXX type classes with arity > 5 not supported
}
