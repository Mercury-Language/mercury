
//
// Copyright (C) 2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class private_builtin 
 { 
    public static class type_info_1
     {
	public int                              arity;
	public int                              type_ctor_version;
	public mercury.runtime.TypeCtorRep      type_ctor_rep;
	public int                              type_ctor_num_ptags; // if DU
	public mercury.runtime.MethodPtr        unify_pred;
	public mercury.runtime.MethodPtr        compare_pred;
	public java.lang.String                 type_ctor_module_name;
	public java.lang.String                 type_ctor_name;
	public mercury.runtime.TypeFunctors     type_functors;
	public mercury.runtime.TypeLayout       type_layout;
	public int 			        type_ctor_num_functors;
   }
   
    public static mercury.builtin.comparison_result_0 builtin_compare_int_3_p_0(int val1, int val2)
     {
        if(val1 == val2)
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_equal);
	else if(val1 < val2)
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_less_than);
	else
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_greater_than);
	
     }

    public static mercury.builtin.comparison_result_0 builtin_compare_string_3_p_0(java.lang.String string1, java.lang.String string2)
     {
	int value = string1.compareTo(string2);
	
        if(value == 0)
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_equal);
	else if(value < 0)
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_less_than);
	else
	     return new mercury.builtin.comparison_result_0(mercury.builtin.comparison_result_0.f_greater_than);
     }
    

    public static void compare_error_0_p_0()
     {
	throw new java.lang.Error("internal error in compare/3");
     }
 }

