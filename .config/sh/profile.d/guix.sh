#!/bin/sh
alias guix-all-profiles='find /gnu/store -maxdepth 1 -type d -name "*profile" -exec ls -al \{\} +'

GUIX_PROFILE=/var/guix/profiles/per-user/saikoupomp/guix-profile
source $GUIX_PROFILE/etc/profile
