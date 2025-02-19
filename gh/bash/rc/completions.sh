# TODO Bash: make completions compatible for arch/guix

# all completions in guix system profile
# ls  /run/current-system/profile/share/bash-completion/{completions,helpers}

# files: {completions,helpers}
export GUIX_COMPLETE=/run/current-system/profile/share/bash-completion

bash_completion_guix_system() {
    # this will also source completions in ~/.config/bash_completion
    local __completion="/run/current-system/profile/etc/profile.d/bash_completion.sh"
    if [[ -r "$__completion" ]]; then
        source "$__completion"
    fi

    local guixbcload=(gcc gdb g++ gccgo gdbus gnumake
                      gpg gpgv gpg2 gpgv2

                      declare
                      info man local-gen

                      dhclient arp arping host hostname hping*
                      ip ipcalc ipsec iptables ipv6calc ebtables
                      ifdown ifquery ifstat ifstatus iftop ifop
                      nmcli nmap

                      dpkg dpkg-* insmod* kmod

                      kill killall htop

                      chgrp chown chmod groupadd groupdel groupmems groupmod
                      loginctl ldap* luser*

                      inotify*
                      ngrep pgrep

                      lsof lsscsi lsusb
                      hciattach hcitool hciconfig
                      cpio tar gzip lzip lzma

                      crontab

                      dd dumpe2fs e2freefrag e2label ebtables hd
                      lvchange lvcreate lvdisplay lvextend lvm lvs
                      lvremove lvreduce lvrename lvresize lvs
                      pv pvchange pvcreate pvdisplay pvmove pvremove pvs

                      feh gsettings
                      curl

                      sqlite3 jq jsonschema
                      python python3 flake8
                      java jar javac
                      lua luac
                      dot)

    for bc in ${guixbcload[@]}; do
        # echo $bc: $GUIX_COMPLETE/completions/$bc
        if [[ -r "$bc" ]]; then
            source $bc
        fi
    done
}

bash_completion_guix_system

# =============================================
# # /run/current-system/profile/etc/profile.d/bash_completion.sh
#
# # shellcheck shell=sh disable=SC1091,SC2039,SC2166
# # Check for interactive bash and that we haven't already been sourced.
# if [ "x${BASH_VERSION-}" != x -a "x${PS1-}" != x -a "x${BASH_COMPLETION_VERSINFO-}" = x ]; then
# 
#     # Check for recent enough version of bash.
#     if [ "${BASH_VERSINFO[0]}" -gt 4 ] ||
#         [ "${BASH_VERSINFO[0]}" -eq 4 -a "${BASH_VERSINFO[1]}" -ge 2 ]; then
#         [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion" ] &&
#             . "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
#         if shopt -q progcomp && [ -r /gnu/store/5774mb64pqw93fpcchndiiq9fh80ngga-bash-completion-2.11/share/bash-completion/bash_completion ]; then
#             # Source completion code.
#             . /gnu/store/5774mb64pqw93fpcchndiiq9fh80ngga-bash-completion-2.11/share/bash-completion/bash_completion
#         fi
#     fi
# 
# fi
