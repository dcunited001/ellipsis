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

syutarget() {
    [[ $# -ne 0 ]] && echo 'Error: does not accept arguments' >&2 && return 1;

    systemctl --user list-units | grep -e '\.target' | cut -f3 -d' '
}
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
syskeys() {
    [[ $# -ne 1 ]] && echo 'Error: Requires arg for systemd manpage' >&2 && return 1;
    local validpages="$(man -S5 -k '^systemd.*$' | cut -f1 -d' ' | grep -e '^systemd')"

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
