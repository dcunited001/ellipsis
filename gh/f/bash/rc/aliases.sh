# unless dumbterm
if [ "$TERM" != "dumb" ]; then
    # commandline color by default
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias diff='diff --color=auto'
    alias grep='grep --color=auto'
    alias vdir='vdir --color=auto'
    alias less='less -R'
    alias ress='less'
# else
    # no color
fi
