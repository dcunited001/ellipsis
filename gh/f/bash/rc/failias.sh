#!/bin/bash

# =============================================
# Finding executables

# will dump binary to shell
cwix() {
    [[ $# -lt 1 ]] && echo "Error: Requires executable command name" >&2 && return 1;
    local _cwix_error=0
    for c in $@; do
        if ! command -v "$c" >/dev/null; then
            _cwix_error=1;
            echo "### CWIX: Error: $c not found" >&2;
        else
            echo "### CWIX: $c"
            cat "$(which "$c")"
        fi
    done
    return $_cwix_error
}

# ---------------------------------------------
# Guix and Nix

gwix() {
    [[ $# -ne 1 ]] && echo "Error: Requires executable command name" >&2 && return 1;
    cmd=$(which "$1") # test -x $cmdlink && echo "it passes"
    [[ ! -x "$cmd" ]] && echo "Requires valid executable file" >&2 && return 1;
    actualcmd="$cmd" # possible to use "${actualcmd:-$cmd}" or something after read
    [[ -L "$cmd" ]] && actualcmd="$(readlink -f "$cmd")"
    [[ $? -ne 0 ]] && echo "Readlink failed" >&2 && return 1;
    ### this will always fail if it's not executable
    # if [ $(grep -E '.*/(nix|gnu)/store') ]; then ...
    echo "$actualcmd" | sed -E 's|^(.*/store/.*)/bin.*$|\1|g'
    # echo -n is unnecessary?
}

nwix() {
    gwix "$1"
}

# =============================================
# Perf

# https://gist.github.com/thefotios/2646996
function recnice() {
    local newnice=$1;
    pid=$2;
    renice "$newnice" "$pid";
    for p in $(pgrep -d' ' -P $pid); do
        recnice $newnice $p;
    done;
}

# =============================================
# Tracing

# ---------------------------------------------
# Strace


# NOTE: can alternatively use `strace -o "!cmd"` or `strace -o "|cmd"`
strep() {
    local _output=""
    local _regexp=".*"
    local _strace_e="all"
    local _strace_f=""
    local _strace_append=
    while getopts :E:e:f OPT; do # o:A OPT; do
        case $OPT in
            E) _regexp="$OPTARG" ;;
            e|+e) _strace_e="$OPTARG" ;;
            f|+f) _strace_f="-f" ;;
            # A|+A) _strace_append="-A" ;;
            # o|+o) _output="$OPTARG" ;;
            --) break ;;
            *)
                echo "usage: `basename $0` -E REGEXP [+-e STRACE_EXPR} [+-f} -- [STRACE_CMD]...

-E REGEXP           Regexp for 'grep -e'. Default: '.*'
-e                  Strace expression. Default: 'all'
-f --follow-forks   Follows forked processes and produces a single output
"
                return 2
        esac
    done
    shift `expr $OPTIND - 1`
    OPTIND=1

    [[ -z "$_regexp" ]] && echo "Error: Requires '\$regexp' for 'grep -E'" >&2 && return 1;
    [[ $# -lt 1 ]] && echo "Error: Requires a command to strace" >&2 && return 1;
    echo "grep -E \"$_regexp\""
    echo "strace -e \"$_strace_e\" \"${_strace_f}\" $@ 2>&1"
    grep -E "$_regexp" <(strace -e "$_strace_e" ${_strace_f} $@ 2>&1)
    # grep -E "$_regexp" <(strace -e "$_strace_e" -o "$output" "${_strace_append}")

    return $?
}

# =============================================
# gpg
# KEYID=$(gpg_id_master)
gpg_id_master() {
    [[ -z "$1" ]] && echo '$1 Required: IDENTITY' && return 1;
    gpg -k --with-colons "$1" \
        | awk -F: '/^pub:/ {print $5; exit}'
}

# KEYFP=$(gpg_fp_master)
gpg_fp_master() {
    [[ -z "$1" ]] && echo '$1 Required: IDENTITY' && return 1;
    gpg -k --with-colons "$1" \
        | awk -F: '/^fpr:/ {print $10; exit}'
}

# =============================================
# SystemD

syutarget() {
    [[ $# -ne 0 ]] && echo 'Error: does not accept arguments' >&2 && return 1;

    systemctl --user list-units | grep -e '\.target' | cut -f3 -d' '
}

# syusvc() {
#   svc=$(systemctl --user list-units \
#   | cut -f3 -d' ' \
#   | grep -e 'service$' \
#   | grep -v @ \
#   | tr '\n' ',' \
#   | sed -E 's/,$//g')

#   if [ $? -ne 0 ]; then echo "Error: couldn't list units" && return 1; fi

#   echo "$svc"
# }

alias syusvc="systemctl --user list-units \
  | cut -f3 -d' ' \
  | grep -e 'service$'"

alias syutgt="systemctl --user list-units \
    | cut -f3 -d' ' \
    | grep -e '\.target$'"

# alias syutgtsvcdot='systemd-analyze --user dot \
#     --from-pattern={$(syutgt),$(syusvc)} \
#     --to-pattern={$(syutgt),$(syusvc)}'

# neither is valid... and *.globs can't "negate@.service"
#
# systemd-analyze --user dot \
#     --from-pattern="basic.target,default.target" \
#     --to-pattern="{basic.target,default.target}"
#
# docs mention that you can reuse these patterns, in which case, the easiest
# way to deal (maybe) is to use `dot -Tjson`
#
# jq '[..| paths | join("/")] | sort | unique' $t/all.targets.json \
#   | sed -E 's/[0-9]+/א/g' \
#   | grep '"' \
#   | tr -d '",' \
#   | sort \
#   | uniq \
#   | tree --fromfile . > $t/tree.tree

# for the complete list:
#
# for k in $(syskeys all); do syskeys $k; done | sort | uniq
#
# probably breaks on man formatting #$%^%*@#^!%

syskgr() {
    [[ $# -ne 1 ]] && echo 'Error: Requires query for systemd keys' >&2 && return 1;
    local q=$1;
    for k in $(syskeys all); do
        syskeys "$k" | grep -iE "$q"
    done
}

syskgrep() {
    [[ $# -ne 1 ]] && echo 'Error: Requires query for systemd keys' >&2 && return 1;
    local q=$1;
    for k in $(syskeys all); do
        echo $k;
        syskeys "$k" | grep -iE "$q"
    done
}

syskeys_validpages() {
    man -S5 -k '^systemd.*$' \
        | cut -f1 -d' ' \
        | grep -e '^systemd' \
        | grep -vE '(conf$|dns-delegate|user-runtime-dir)' \
        | sed -e 's/systemd\.//g'
}

syskeys() {
    [[ $# -ne 1 ]] && echo 'Error: Requires arg for systemd manpage' >&2 && return 1;
    local validpages="$(syskeys_validpages)"

    # query valid pages
    [[ $1 == "all" ]] && echo "$validpages" | sed -e 's/systemd.//g' | sort && return 0;

    local page
    case "$1" in
      # sleep.conf) page='systemd-sleep.conf';;
      # system.conf) page='systemd-system.conf';;
      # user-runtime-dir) page='systemd-user-runtime-dir';;
      # user.conf) page='systemd-user.conf';;
      automount) page='systemd.automount';;
      device) page='systemd.device';;
      dnssd) page='systemd.dnssd';;
      exec) page='systemd.exec';;
      kill) page='systemd.kill';;
      link) page='systemd.link';;
      mount) page='systemd.mount';;
      negative) page='systemd.negative';;
      netdev) page='systemd.netdev';;
      network) page='systemd.network';;
      nspawn) page='systemd.nspawn';;
      path) page='systemd.path';;
      pcrlock) page='systemd.pcrlock';;
      pcrlock.d) page='systemd.pcrlock.d';;
      positive) page='systemd.positive';;
      preset) page='systemd.preset';;
      resource-control) page='systemd.resource-control';;
      scope) page='systemd.scope';;
      service) page='systemd.service';;
      slice) page='systemd.slice';;
      socket) page='systemd.socket';;
      swap) page='systemd.swap';;
      target) page='systemd.target';;
      timer) page='systemd.timer';;
      unit) page='systemd.unit';;
      *) echo 'Error requires a valid suffix in a systemd (5) man page' >&2 && echo "$validpages" >&2 && return 1;;
    esac
    # echo man -S5 -k "$page"
    man -S5 "$page" \
        | grep -E '^       [A-Z][a-zA-Z0-9]+=' \
        | grep -E '=$' \
        | tr -d ' ' \
        | tr ',' '\n' \
        | sort \
        | uniq
}
