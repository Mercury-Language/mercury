//
// Copyright (C) 2002-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class string
 {
    public static java.lang.String append_3_p_2(java.lang.String s1, java.lang.String s2)
     {
        return s1.concat(s2);
     }

    public static java.lang.Object [] append_3_p_1(java.lang.String s1, java.lang.String s2)
     {
	java.lang.Boolean success = new java.lang.Boolean(s2.startsWith(s1));
	java.lang.String answer = s2.substring(s1.length());
	java.lang.Object [] result = {(java.lang.Object) success, (java.lang.Object) answer};
        return result;
     }
 }

