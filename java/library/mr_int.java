//
// Copyright (C) 2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class mr_int
 {
    public static int mod_2_f_0(int val1, int val2)
     {
        return val1 - (val1 / val2) * val2;
     }
    
    public static int f_47_47_2_f_0(int val1, int val2)
     {
        int div_res = val1 / val2;
	int rem_res = val1 % val2;

	return div_res;
     }
    
    public static int f_plus_2_f_0(int val1, int val2)
     {
	return val1 + val2;
     }
 
 }

