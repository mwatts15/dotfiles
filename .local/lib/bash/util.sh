#!/bin/sh

error () {
    echo $@ >&2
}

die_if_program_doesnt_exist () {
    which "$1" 2>/dev/null >&2
    if [ $? -ne 0 ] ; then
        error "$1 is not available. Exiting."
        exit 8
    fi
}
