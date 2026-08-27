-- This permits a bit of functionality for discovery/troubleshooting across
-- both devices that should avoid any dependencies (other than
-- walker/elephant, for now).
--
-- I need to learn the window ID/referece for windows getting thrown behind
-- tiled windows, like the portal sharing thing

local sub_walker = { name = "歩行", mod = "MOD3 + SHIFT", key = "F1" }
local walkerNotifyMsg = [[Walker Test

; providerlist
> runner
/ files
. symbols
! todo
= calc
@ websearch
: clipboard
$ windows
, unicode]];

-- local walkerNotifyCmd = hyprctl


hl.bind(sub_walker.mod .. "+" .. sub_walker.key, function()
    hl.notification.create({
        text = walkerNotifyMsg,
        timeout = 5000,
        color = "rgb(CC00CC)",
        -- icon? integer|string,
        -- font_size? number
    })
end)

hl.bind(sub_walker.mod .. "+" .. sub_walker.key, hl.dsp.submap(sub_walker.name))

hl.define_submap(sub_walker.name, function()
    hl.bind("semicolon", hl.dsp.exec_cmd("walker -m providerlist -p 'Providerlist...'"))
    hl.bind("SHIFT + period", hl.dsp.exec_cmd("walker -m runner       -p 'Runner...'"))
    hl.bind("slash", hl.dsp.exec_cmd("walker -m files        -p 'Files...'"))
    hl.bind("period", hl.dsp.exec_cmd("walker -m symbols      -p 'Symbols...'"))
    hl.bind("SHIFT + code:10", hl.dsp.exec_cmd("walker -m todo         -p 'Todo...'"))
    hl.bind("SHIFT + code:11", hl.dsp.exec_cmd("walker -m websearch    -p 'Websearch...'"))
    hl.bind("SHIFT + code:13", hl.dsp.exec_cmd("walker -m windows      -p 'Windows...'"))
    hl.bind("equal", hl.dsp.exec_cmd("walker -m calc         -p 'Calc...'"))
    hl.bind("SHIFT + semicolon", hl.dsp.exec_cmd("walker -m clipboard    -p 'Clipboard...'"))
    hl.bind("comma", hl.dsp.exec_cmd("walker -m unicode      -p 'Unicode...'"))

    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("catchall", hl.dsp.submap("reset"))
end)


-- # ---------------------------------------------
-- # WALKER submap

-- # holy crap it really do not let no newlines through
-- $walkerNotify=-1 5000 ""
-- $walkerNotifyMsg="$(echo " "; echo " "; for i in '; providerlist' '> runner' '/ files' '. symbols' '! todo' '= calc' '@ websearch' ': clipboard' '$ windows' ', unicode'; do echo $i; done )"
-- bind = MOD3, W, exec, hyprctl notify $walkerNotify 'TEST WALKER' $walkerNotifyMsg

-- bind = MOD3, W, submap, WALKER

-- submap=WALKER, reset
-- bind =      ,  exec,
-- bind = SHIFT,  exec,
-- bind =      ,  exec,
-- bind =      ,  exec,
-- bind = SHIFT,  exec,
-- bind = SHIFT,  exec,
-- bind = SHIFT,  exec,
-- bind =      ,  exec,
-- bind = SHIFT,  exec,
-- bind =      ,  exec,

-- bind =      , M        , exec, dwalker-man

-- # bind = ,greater, exec, walker -m runner -p "Runner..."
-- # bind = ,exclam, exec, walker -m todo -p "Todo..."
-- # bind = ,at, exec, walker -m websearch -p "Websearch..."
-- # bind = ,dollar, exec, walker -m windows -p "Windows..."
-- # bind =  semicolon, exec, walker -m clipboard -p "Clipboard..."
-- # bind = ,catchall, submap, reset

-- bind = ,escape, submap, reset
-- submap=reset
-- # ---------------------------------------------
