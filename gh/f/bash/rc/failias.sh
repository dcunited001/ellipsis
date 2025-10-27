gwix() {
    [[ $# -eq 1 ]] || (echo "Error: Requires executable command name" >&2 && return 1);
    cmd=$(which "$1") # test -x $cmdlink && echo "it passes"
    [[ -x "$cmd" ]] || (echo "Requires valid executable file" >&2 && return 1);
    actualcmd="$cmd" # possible to use "${actualcmd:-$cmd}" or something after read
    [[ -L "$cmd" ]] && actualcmd="$(readlink -f "$cmd")"
    [[ $? -ne 0 ]] && (echo "Readlink failed" >&2 && return 1);
    ### this will always fail if it's not executable
    # if [ $(grep -E '.*/(nix|gnu)/store') ]; then ...
    echo "$actualcmd" | sed -E 's|^(.*/store/.*)/bin.*$|\1|g'
    # echo -n is unnecessary?
}

nwix() {
    gwix "$1"
}
