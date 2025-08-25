#!/bin/sh
export PS_INFO=""
export PS_PROMPT=""

# TODO: use PROMPT_COMMAND and an array of commands mutating the state

PS_GIT=""
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_DESCRIBE_STYLE=branch
GIT_PS1_SHOWUPSTREAM=name

PS_PROMPT="${LYELLOW}\A ${LGREEN}\u${RED}@${LCYAN}\h ${RED}:: ${YELLOW}\w"
if [ -n "$GUIX_ENVIRONMENT" ]; then
    PS_INFO="${LMAGENTA}g${RESTORE}"
fi

if [ "$TERM" = "dumb" ]; then
    PS1='$ '
else
    PS_GIT='$(__git_ps1 "«%s»") '
    PS_INFO="$PS_GIT $PS_INFO"
    PS1="$PS_INFO \n$PS_PROMPT"

    # calc number of cols with $((COLUMNS -n ))
    if [ -n "$PS_GIT" ]; then
       PS1="$PS1"
    fi
    PS1="$PS1${RED}$ ${RESTORE}"
fi
