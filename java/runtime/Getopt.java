// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

import java.util.*;

/**
 * Getopt style option passing.
 * This is currently very simple, it does only what is necessary.
 */
public class Getopt implements Iterable<Getopt.Option>
{
    String              option_string;
    List<Option>        options;
    List<String>        arguments;

    /**
     * Create a Getopt object to process the command line options.
     */
    public Getopt(String option_string)
    {
        this.option_string = option_string;
        this.options = null;
    }

    public Iterator<Option> iterator()
    {
        if (options == null) {
            process();
        }

        return options.iterator();
    }

    /**
     * Process the options. This is called automatically and the result is
     * cached.
     */
    public void process()
    {
        StringTokenizer tok = new StringTokenizer(option_string);
        Option          option = null;

        options = new LinkedList<Option>();
        arguments = new LinkedList<String>();

        while (tok.hasMoreTokens()) {
            String str;

            str = tok.nextToken();
            if (str.startsWith("--")) {
                if (option != null) {
                    options.add(option);
                    option = null;
                }
                option = new Option(str.substring(2));
            } else if (str.startsWith("-")) {
                if (option != null) {
                    options.add(option);
                    option = null;
                }
                option = new Option(str.substring(1));
            } else {
                if (option != null) {
                    option.setValue(str);
                    options.add(option);
                    option = null;
                } else {
                    arguments.add(str);
                    // The token we have got is an argument, not an option.
                    // Store the remaining strings as arguments, and
                    // stop processing.
                    while (tok.hasMoreTokens()) {
                        str = tok.nextToken();
                        arguments.add(str);
                    }
                    break;
                }
            }
        }
    }

    /**
     * Get the arguments from the command line.  Arguments are not options,
     * they occur after the options list.
     */
    public List<String> getArguments() {
        return arguments;
    }

    /**
     * An option.  Call getopt.nextOption() to get each option.
     */
    public static class Option
    {
        // the name of the option.  This is stored without either the - or
        // -- that may proceed option names.
        private String option;
        // the value of the option or null.
        private String value;

        /**
         * Create a new option.
         * @param option The name of the option, excluding any leading - or
         * -- substring.
         * @param value The value of the option.
         */
        public Option(String option, String value)
        {
            this.option = option;
            this.value = value;
        }

        /**
         * Create a new option.
         * The option's value will be initialised to null.
         * @param option The name of the option, excluding any leading - or
         * -- substring.
         */
        public Option(String option)
        {
            this(option, null);
        }

        public void setValue(String value)
        {
            this.value = value;
        }

        /**
         * True if this option matches the given option name.
         * @param option The option name for comparison without leading --
         * or -.
         */
        public boolean optionIs(String option)
        {
            return this.option.equals(option);
        }

        public int getValueInt()
            throws NumberFormatException
        {
            return Integer.parseInt(value);
        }

        public String toString()
        {
            StringBuilder builder = new StringBuilder();

            // Add dashes.
            if (option.length() == 1) {
                builder.append("-");
            } else {
                // This must be a double dash argument.
                builder.append("--");
            }
            builder.append(option);
            builder.append(" ");
            builder.append(value);

            return builder.toString();
        }
    }
}
