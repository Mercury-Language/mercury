#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test handling of :- external procedures with intermodule analysis.

. ../analysis/common.sh

set -e
set -x

rm -rf Mercury

$MMCMAKE ext_2.analyse --analysis-repeat 0 || failed

check_result ext_2 'mm_tabling.*foo'
check_statuses 'optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
