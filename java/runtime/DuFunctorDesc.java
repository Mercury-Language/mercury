//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class DuFunctorDesc {
    
	public java.lang.String du_functor_name;
	public int du_functor_orig_arity;
	public int du_functor_arg_type_contains_var;
	public mercury.runtime.Sectag_Locn du_functor_sectag_locn;
	public int du_functor_primary;
	public int du_functor_secondary;
	public int du_functor_ordinal;
	// XXX PseudoTypeInfo's have not been implemented properly
	//     yet, so this may not be correct.
	public /*final*/ mercury.runtime.PseudoTypeInfo[] du_functor_arg_types;
	public /*final*/ java.lang.String[] du_functor_arg_names;
	public /*final*/ mercury.runtime.DuExistInfo[] du_functor_exist_info;

	public DuFunctorDesc(java.lang.String functor_name, int orig_arity,
		int arg_type_contains_var, int sectag_locn, int primary,
		int secondary, int ordinal,
		// XXX why do we need to use Object here?
		java.lang.Object arg_types,
		java.lang.Object arg_names,
		java.lang.Object exist_info)
	{
		du_functor_name = functor_name;
		du_functor_orig_arity = orig_arity;
		du_functor_ordinal = ordinal;
		du_functor_arg_type_contains_var = arg_type_contains_var;
		du_functor_sectag_locn =
			new mercury.runtime.Sectag_Locn(sectag_locn);
		du_functor_primary = primary;
		du_functor_secondary = secondary;
		du_functor_ordinal = ordinal;
		du_functor_arg_types = (mercury.runtime.PseudoTypeInfo [])
			arg_types;
		du_functor_arg_names = (java.lang.String []) arg_names;
		du_functor_exist_info =
			(mercury.runtime.DuExistInfo[]) exist_info;
	}
}
