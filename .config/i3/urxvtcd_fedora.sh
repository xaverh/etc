#!/bin/sh

urxvt256c-mlc "$@"
if [ $? -eq 2 ]; then
    urxvt256c-mld -q -f
    exec urxvt256c-mlc "$@"
fi
