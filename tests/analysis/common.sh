#-----------------------------------------------------------------------------#
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Common functions shared by the test scripts in the analysis_* directories.

MC=${MC:-mmc}
MMCMAKE="$MC --make"

failed () {
    exit 1
}

check_result () {
    # args: file pattern
    if ! grep -q -e "$2" Mercury/analyses/$1.analysis
    then
        failed
    fi
}

check_statuses () {
    case "`cat Mercury/analysis_statuss/* | tr -d '\n'`" in
        $1)
            ;;
        *)
            failed
            ;;
    esac
}

check_no_requests () {
    case "`ls Mercury/requests`" in
        "")
            ;;
        *)
            failed
            ;;
    esac
}

check_request () {
    # args: file pattern
    if ! grep -q -e "$2" Mercury/requests/$1.request
    then
        failed
    fi
}

check_imdg () {
    # args: file pattern
    if ! grep -q -e "$2" Mercury/imdgs/$1.imdg
    then
        failed
    fi
}

#-----------------------------------------------------------------------------#
