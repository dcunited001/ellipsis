#!/bin/sh
# [[file:Bash.org::*RC][RC:1]]
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# TODO: source ~/.config/sh/rc.d files here
# RC:1 ends here

# [[file:Bash.org::*Color][Color:1]]

# Color:1 ends here
