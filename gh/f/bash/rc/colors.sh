if [ "$TERM" != "dumb" ]; then
    #export CURSOR_BOX=$(echo -e '\001\033[\017

    export RESTORE=$(echo -en '\001\033[0m\002')
    export STANDOUT=$(echo -en '\001\033[00;44;37m\002')
    export RED=$(echo -en '\001\033[00;31m\002')
    export GREEN=$(echo -en '\001\033[00;32m\002')
    export YELLOW=$(echo -en '\001\033[00;33m\002')
    export BLUE=$(echo -en '\001\033[00;34m\002')
    export MAGENTA=$(echo -en '\001\033[00;35m\002')
    export PURPLE=$(echo -en '\001\033[00;35m\002')
    export CYAN=$(echo -en '\001\033[00;36m\002')
    export LIGHTGRAY=$(echo -en '\001\033[00;37m\002')
    export LRED=$(echo -en '\001\033[01;31m\002')
    export LGREEN=$(echo -en '\001\033[01;32m\002')
    export LYELLOW=$(echo -en '\001\033[01;33m\002')
    export LBLUE=$(echo -en '\001\033[01;34m\002')
    export LMAGENTA=$(echo -en '\001\033[01;35m\002')
    export LPURPLE=$(echo -en '\001\033[01;35m\002')
    export LCYAN=$(echo -en '\001\033[01;36m\002')
    export WHITE=$(echo -en '\001\033[01;37m\002')
else
    export RESTORE=""
    export STANDOUT=""
    export RED=""
    export GREEN=""
    export YELLOW=""
    export BLUE=""
    export MAGENTA=""
    export PURPLE=""
    export CYAN=""
    export LIGHTGRAY=""
    export LRED=""
    export LGREEN=""
    export LYELLOW=""
    export LBLUE=""
    export LMAGENTA=""
    export LPURPLE=""
    export LCYAN=""
    export WHITE=""
fi

export LESS_TERMCAP_mb="${LRED}"
export LESS_TERMCAP_md="${LRED}"
export LESS_TERMCAP_me="${RESTORE}"
export LESS_TERMCAP_se="${RESTORE}"
export LESS_TERMCAP_so="${STANDOUT}"
#export LESS_TERMCAP_so=$'\E[00;44;37m'
export LESS_TERMCAP_ue="${RESTORE}"
export LESS_TERMCAP_us="${LGREEN}"

# fix for less as MANPAGER
# https://bbs.archlinux.org/viewtopic.php?id=287185
export GROFF_NO_SGR=1
