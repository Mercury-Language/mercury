#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test handling of :- external procedures with intermodule analysis.

source ../common.sh

set -e
set -x

rm -rf Mercury

$MMCMAKE ext2.analyse --analysis-repeat 0 || failed

check_result ext2 'mm_tabling.*foo'
check_statuses 'optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
