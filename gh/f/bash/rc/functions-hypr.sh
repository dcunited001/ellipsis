#!/usr/bin/env sh

return 1; # i should use dmenu/etc for this, but i've never fully set it up

function hbkeyfdsa () {
    if [ $# -lt 1 ]; then echo "hbkey: Requires a key"; return 1; fi
    if [ ! -x "$(which hjbinds)" ]; then echo "hbkey: Requires hjbinds command"; return 1; fi

    local hbkey="$1"
    local hbmods=(0
                  32 33 36 37 40 41 44 45
                  64 65 68 69 72 73 76 77
                  80 81 84 85 88 89 92 93
                  96 97 100 101 104 105 108 109)

    # local hbkeys=(z x c v b n m , . a s d f g h j k l ;)
    for m in ${hbmods[@]}; do
        hjbinds "$m" | jq 'map(select(.key == "injection"))' "$hbkey"
    done
}
