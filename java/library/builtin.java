
//
// Copyright (C) 2002-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class builtin 
 { 

    public static mercury.private_builtin.type_info_1 builtin__type_ctor_info_int_0 = new mercury.private_builtin.type_info_1();
    
    public static mercury.private_builtin.type_info_1 builtin__type_ctor_info_string_0 = new mercury.private_builtin.type_info_1();

    public static mercury.private_builtin.type_info_1 builtin__type_ctor_info_character_0 = new mercury.private_builtin.type_info_1();

    public static class comparison_result_0 {
      public static final int f_equal = (int) 0;
      public static final int f_less_than = (int) 1;
      public static final int f_greater_than = (int) 2;
      public int value;

      public comparison_result_0(int val) {
        this.value = val;
        return;
      }
    }

    public static boolean unify_2_p_0(mercury.private_builtin.type_info_1 ti,
		    java.lang.Object x, java.lang.Object y)
    {
      throw new java.lang.Error("unify/3 not implemented");
    }

    public static comparison_result_0 compare_3_p_0(
	mercury.private_builtin.type_info_1 ti,
	java.lang.Object x, java.lang.Object y)
    {
      throw new java.lang.Error("compare/3 not implemented");
    }
 }


