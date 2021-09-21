#!/bin/sh
# [[file:Bash.org::*=.bashrc=][=.bashrc=:1]]
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# TODO: source ~/.config/sh/rc.d files here
# =.bashrc=:1 ends here
