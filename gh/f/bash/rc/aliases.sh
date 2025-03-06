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

#* docs @ALIAS
alias imacs='emacs -f info-standalone --eval="(load-theme (intern \"wombat\"))"'
manhtml() {
    [[ -z "$1" ]] && echo "Requires man page name" && return 1
    man -Thtml "$1" \
        | sed -e 's/margin-top: 1em//g' \
        | sed -E 's/(<br>|<hr>)//g' \
        | sed -E 's/<a href="#.*>//g'
}

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

#* renice @alias
# alias renoice="renice --priority 15 $(pgrep emacs-29)"

#* shell @ALIAS

#** redirect @ALIAS
alias wordcat="tee >(xargs -n1 cat) | wc -w"

#** curl @ALIAS

# use with parameter expansion: echo -e https://fdsa.com/path/to/{0,1,2,3,4,5}.jpg | curlist -o

# curl --remote-name-all --output-dir /data/xdg/Documents/cheatsheets/lisp/folding/ -K <( ... )
# <(echo -e https://page.com/path/to/{4,5,6,7,8,9,10,11}.jpg | sed -e 's/ /\n/g' | sed -E 's/^(.*)$/url="\1"/g')

#* data
alias tyxy="tidy --quiet yes --tidy-mark no --vertical-space yes -indent -xml"

#** jqyq @ALIAS

#** grep @ALIAS
alias grepnobin="grep -I"

#* system @ALIAS

#** updates @ALIAS
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

#* services @ALIAS
#** systemd @ALIAS
alias jctl="journalctl -p 3 -xb"
alias jctlu="journalctl --user -u"
alias sysu='systemctl --user'
# also: systemd-search-shared
alias sysupath='systemd-path user-shared'
alias sysdpath='systemd-path system-shared'
# sysu cat doom
# sysu show -p Type $doom
# sysu show -vp Type $doom # only values
# alias sysed='systemctl --user edit --drop-in=$overridename $svc'

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

# passes NUL byte to xargs, needs to be function
# alias gitls_bydate='git ls-tree -r --name-only HEAD -z | TZ=UTC xargs -0n1 -I_ git --no-pager log -1 --date=iso-local --format="%ad _" -- _ | sort'

#** git-stack @ALIAS
alias gkg='git stack'
alias gksy='git stack sync'
alias gkcfg='git stack --dump-config -'

# TODO: ascii git tree
# git log --graph --pretty=format:'%Cred%h%Creset%n %d' --abbrev-commit --decorate -n32 --all HEAD~

# Advanced command-not-found hook
# source /usr/share/doc/find-the-command/ftc.bash

# alias emacs-debug-wayland='WAYLAND_DEBUG=1 emacs --fg-daemon > $HOME/.cache/log/emacs.wayland.`date +%Y-%m%d-%H%M`.log 2>&1'

alias nodenpm_lsparse="npm ls -g --parseable | grep node_modules | sed -e 's/.*node_modules\///g'"

#* tree @ALIAS

# nevermind, --prune removes empty subdirectories not containing -P $pattern
alias treef='tree --prune -aP'

#* guix @ALIAS

# TODO update load path for guix aliases (or remove)
# alias gsl="guix shell -L $HOME/.dotfiles"
# alias gskl="guix shell --keep-failed -L $HOME/.dotfiles"
# alias gbl="guix build -L $HOME/.dotfiles"
# alias gbkl="guix build --keep-failed -L $HOME/.dotfiles"

#* window-mgmt @ALIAS
#
# TODO: aliases: remove window-mgmt alises
#
alias @terminator='terminator --title "\$${__title:-TERM}"'
