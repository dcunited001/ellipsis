# [[file:Bash.org::*RC][RC:1]]
[[ $- != *i* ]] && return
shopt -s histappend

[[ "$TERM" == "dumb" ]] || export TERM="xterm-256color"

# RC
export DOTS_RC_D=$DOTS_CFG_SHELL/rc.d
[[ -f $DOTS_CFG_SHELL/_before_rc.d.sh ]] && source $DOTS_CFG_SHELL/_before_rc.d.sh
[[ -f $DOTS_CFG_SHELL/_load_rc.d.sh ]] && source $DOTS_CFG_SHELL/_load_rc.d.sh
# RC:1 ends here

# [[file:Bash.org::*Send to clipboard without =xclip=][Send to clipboard without =xclip=:1]]
# enable set-mark (overrides control sequences like unix-word-rubout)
# set bind-tty-special-chars off

# for some reason, i still need to undef C-w (so i don't need the above
# ... removing, since my changes did not fix the problem)
#
# stty werase undef

# enable copy/paste
bind '"\C-@":set-mark'
bind '"\e ":set-mark'
bind '"\C-w":kill-region'
bind '"\ew":copy-region-as-kill'

# now i know this is more common
bind '"\e/":dabbrev-expand'

# also M-tab for `dynamic-complete-history`
# Send to clipboard without =xclip=:1 ends here

# [[file:Bash.org::*GPG/SSH][GPG/SSH:1]]
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
# GPG/SSH:1 ends here

# [[file:Bash.org::*Direnv][Direnv:1]]
[[ -e "$(command -v direnv)" ]] && eval "$(direnv hook bash)"
# Direnv:1 ends here
