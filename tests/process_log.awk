# process_log.awk
# 
# Process the output of `mmake runtests' collecting
# the mmake output for failing tests. See the
# `runtests_local' target in Mmake.common for the
# code which produces the markers used by this script.

BEGIN			{ test_line = -1 }

/^RUNNING TEST/		{ test_line = 0 }

/^FINISHED TEST/	{ test_line = -1 }

test_line != -1		{
	test_lines[test_line] = $0
	test_line++
}

test_line != -1 && /^FAILED TEST/ {
	for (line = 0; line < test_line; line++) {
		print test_lines[line]
	}
	test_line = -1
}

