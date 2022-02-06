#!/bin/sh

GREEN='\033[1;32m'
RED='\033[1;30m'
NC='\033[0m'
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles

__profiles=""
while [ "$1" != "" ]; do
    echo $1

    case $1 in
        --substitute-urls)
            __substitute_urls="$2"
            shift 2
            ;;
        -*|--*=) # unsupported flags
        echo "Error: Unsupported flags $1" >&2
        exit 1
        ;;
        *)
            __profiles="$__profiles $1"
            shift
            ;;
    esac
done

if [ -z "$__profiles" ]; then
    __profiles="$HOME/.config/guix/manifests/*.scm"
fi

__substitute_urls=${__substitute_urls:-https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org}

echo "PROFS: $__profiles"
echo "SUBS: $__substitute_urls"

# eval set -- "$PARAMS"
