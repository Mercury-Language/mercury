//
// Copyright (C) 2001-2004, 2011 The University of Melbourne.
// Copyright (C) 2015, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class DuFunctorDesc implements java.io.Serializable {
    
	public java.lang.String du_functor_name;
	public int du_functor_orig_arity;
	public int du_functor_arg_type_contains_var;
	public Sectag_Locn du_functor_sectag_locn;
	public int du_functor_primary;
	public int du_functor_secondary;
	public int du_functor_ordinal;
	// XXX PseudoTypeInfo's have not been implemented properly
	//     yet, so this may not be correct.
	public /*final*/ PseudoTypeInfo[] du_functor_arg_types;
	public /*final*/ java.lang.String[] du_functor_arg_names;
	public /*final*/ DuArgLocn[] du_functor_arg_locns;
	public /*final*/ DuExistInfo du_functor_exist_info;
	public FunctorSubtypeInfo du_functor_subtype_info;

	public DuFunctorDesc()
	{
	}

	public void init(java.lang.String functor_name, int orig_arity,
		int arg_type_contains_var, int sectag_locn, int primary,
		int secondary, int ordinal,
		// XXX why do we need to use Object here?
		java.lang.Object arg_types,
		java.lang.Object arg_names,
		java.lang.Object arg_locns,
		java.lang.Object exist_info,
		int functor_subtype_info)
	{
		du_functor_name = functor_name;
		du_functor_orig_arity = orig_arity;
		du_functor_ordinal = ordinal;
		du_functor_arg_type_contains_var = arg_type_contains_var;
		du_functor_sectag_locn = new Sectag_Locn(sectag_locn);
		du_functor_primary = primary;
		du_functor_secondary = secondary;
		du_functor_ordinal = ordinal;
		du_functor_arg_types = (PseudoTypeInfo []) arg_types;
		du_functor_arg_names = (java.lang.String []) arg_names;
		du_functor_arg_locns = (DuArgLocn []) arg_locns;
		du_functor_exist_info = (DuExistInfo) exist_info;
		du_functor_subtype_info =
			new FunctorSubtypeInfo(functor_subtype_info);
	}
}
