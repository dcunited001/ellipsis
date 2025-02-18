# [[file:../../../Bash.org::*Aliases][Aliases:1]]
#* alias @ALIAS

#** color @ALIAS

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
    alias screen='screen -h 2000'
# else
    # no color
fi

#* ps @ALIAS

# pgrep -u $UID
pskill() {
    [[ -z "$1" ]] && echo "Requires command name" && return 1
    pkill -u $UID -x "$1"
}
alias psid='ps -opid,uid,command h'
# h sorts
alias pspri='ps -eo pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm k pri'
alias ps_pri='ps -eo pri k +pri h | uniq -c'

# emacs stays at -4: https://github.com/Nefelim4ag/Ananicy
alias ps_nice='ps axo pid,comm,nice,cls --sort=-nice'
alias psnice='ps -o pid,comm,nice' # $pid

alias ptrgb="pstree -C age -pT"

#*renice @alias
# alias renoice="renice --priority 15 $(pgrep emacs-29)"

#* grep @ALIAS
alias grepnobin="grep -I"

#* s--ystem @ALIAS

#* updates @ALIAS
alias grubup="sudo update-grub"
alias upd='/usr/bin/update'

#* hardware @ALIAS
alias hw='hwinfo --short'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
alias psmem='ps auxf | sort -nr -k 4'

#** disk @ALIAS
alias iotopa='iotop -oa'
# atop

#* archive @ALIAS
alias tarnow='tar -acf '
alias untar='tar -zxvf '
alias wget='wget -c '

#* systemd @ALIAS
alias jctl="journalctl -p 3 -xb"
alias jctlu="journalctl --user -u"
alias sysu='systemctl --user'
alias sysupath='systemd-path user-shared'
alias sysdpath='systemd-path system-shared'

#* pkg
#** pacman @ALIAS
alias fixpacman="sudo rm /var/lib/pacman/db.lck"
alias rmpkg="sudo pacman -Rdd"
alias cleanup='sudo pacman -Rns `pacman -Qtdq`'
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"

#* shelltools @ALIAS
alias pathtr="tr ':' '\n'"
alias shitbin='echo -e "\033c"'

#* crypto @ALIAS

#** ssh @ALIAS

alias sshddump='sudo sshd -T'
#alias sship='ssh -i $

#* gpg @ALIAS
alias gpga='gpg --armor'
alias gpguptty='gpg-connect-agent updatestartuptty /bye'
alias gpgrel='gpg-connect-agent reloadagent /bye'
alias gpgk='gpg-connect-agent killagent /bye'

#* git @ALIAS

#** git-stack @ALIAS
alias gkg='git stack'
alias gksy='git stack sync'
alias gkcfg='git stack --dump-config -'

# TODO: ascii git tree
# git log --graph --pretty=format:'%Cred%h%Creset%n %d' --abbrev-commit --decorate -n32 --all HEAD~

# Advanced command-not-found hook
# source /usr/share/doc/find-the-command/ftc.bash

#* git-stack @ALIAS
alias emacs-debug-wayland='WAYLAND_DEBUG=1 emacs --fg-daemon > $HOME/.cache/log/emacs.wayland.`date +%Y-%m%d-%H%M`.log 2>&1'

alias nodenpm_lsparse="npm ls -g --parseable | grep node_modules | sed -e 's/.*node_modules\///g'"
# Aliases:1 ends here

# [[file:../../../Bash.org::*Window Control][Window Control:1]]
#* window-mgmt @ALIAS
alias @terminator='terminator --title "\$${__title:-TERM}"'
# Window Control:1 ends here

# [[file:../../../Bash.org::*File Listing][File Listing:2]]
#* tree @ALIAS

# nevermind, --prune removes empty subdirectories not containing -P $pattern
alias treef='tree --prune -aP'
# File Listing:2 ends here
