#!/bin/sh
# [[file:Bash.org::*RC][RC:1]]
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias sysu='systemctl --user'

alias emacs-debug-wayland='WAYLAND_DEBUG=1 emacs --fg-daemon > ~/.cache/log/emacs.wayland.`date +%Y-%m%d-%H%M`.log 2>&1'

PS1='[\u@\h \W]\$ '

# TODO: source ~/.config/sh/rc.d files here
# RC:1 ends here

# [[file:Bash.org::*Color][Color:1]]
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[00;44;37m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
# Color:1 ends here
