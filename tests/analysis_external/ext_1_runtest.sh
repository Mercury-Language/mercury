#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test handling of :- external procedures with intermodule analysis.

. ../analysis/common.sh

set -e
set -x

rm -rf Mercury

$MMCMAKE ext_1.analyse --analysis-repeat 0 || failed

check_result ext_1 'exception.*foo'
check_result ext_1 'structure_reuse.*foo'
check_result ext_1 'structure_sharing.*foo'
check_result ext_1 'trail.*foo'
check_result ext_1 'unused_args.*foo'
check_statuses 'optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
