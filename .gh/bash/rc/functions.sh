
diffpair() {
    [[ $# -gt 1 ]] || return 1;
    globbed=$2
    prefix=$1

    for f in $globbed; do diff $prefix/$f $f; done
}


# clear all the fat fingers, so sync happens without dired
repo_reset_repos() {
    [[ -z "$1" ]] && \
        printf "quoted glob pattern: \$1\nargs to git reset \$@" && \
        return 1

    local _pattern=$1
    shift 1

    # pattern is optional .... oh
    if [[ $# -gt 0 ]]; then
        # which is probably why there's no explicit command for this
        repo forall $_pattern -c git reset $@
    else
        repo forall $_pattern -c git reset --hard
    fi

}

# alias rprr=repo_reset_repos
# Git Repo:1 ends here

tre() {
    local -a tree_opts=()
    local -a tree_args=()

    while [ "$#" -gt 0 ]; do
        if [[ "$1" = "--" ]]; then
            shift 1
            tree_opts=$@
            break;
        fi
        tree_args+=($1)
        shift 1
    done

    echo ${tree_opts[@]}
    echo ${tree_args[@]}
}
