#!/bin/sh
$(import util.sh)
die_if_program_doesnt_exist inotifywait

_dowait () {
    inotifywait $@ 2>/dev/null >&2
}

_var_loop () {
    base="$1"
    shift
    while [ 1 ] ; do
        if [ -f "$base/wacom-status" ] ; then
            _dowait "$base/wacom-status"
            if [ $? -eq 0 ] ; then
                while [ -z "$(xsetwacom --list)" ] ; do
                    sleep .1
                done
                $@
            else
                echo "Inotify error. Exiting" >&2
                exit 2
            fi
        else
            error "Couldn't find the status file. Waiting for it to be created"
            _dowait -e create "$base"
        fi
    done

}

on_wacom_status_change () {
    if [ -d /var/run ] ; then 
        _var_loop /var/run $@
    elif [ -d /run ] ; then
        _var_loop /run $@
    else
        error "No /var/run/ and no /run directory was found. Exiting."
        exit 3
    fi
}

wacom_get_device_id () {
    X=$1
    die_if_program_doesnt_exist xsetwacom
    echo -n $(xsetwacom --list devices | grep $X | cut -f 2 | cut -d' ' -f 2 | head -n 1)
}
