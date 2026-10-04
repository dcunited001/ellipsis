dc = dc or {}
dc.lo = (dc.lo or {})
dc.lo.master = (dc.lo.master or {})

local function ws_is_master()
    local ws = hl.get_active_workspace()
    if hl.get_active_special_workspace() then
        ws = hl.get_active_special_workspace()
    end

    return (ws ~= nil and ws.tiled_layout == "master")
end

-- {add,remove,focus}master
-- cycle{next,prev}
-- orientation{top,bottom,center,left,right}
-- orientationcycle top/bottom/center/left/right
-- roll{next,prev}
-- swap{next,prev,withmaster}

-- mfact: relative adjustment
function dc.lo.master.bind_submap(opts)
    opts = opts or {}
    opts.name = opts.name or "MasterLayoutSubmap"
    opts.key = opts.key or "S"
    opts.mod = opts.mod or "MOD3 + SUPER"
    opts.notify = opts.notify or true

    local masterSubmapOtherMsg = opts.name .. [[: not active on this workspace.]]
    local masterSubmapMsg = opts.name .. [[
p	↑	swapprev
n	↓	swapnext
P	S-↑	rollprev
N	S-↓	rollnext
c	←	cycle
C	→	cycle

t	top
b bottom
l left
r right
C-RET center

TAB add master
S-TAB remove master
SPACE	focus master
RET	swap master]]

    hl.define_submap(opts.name, function()
        -- cycle
        hl.bind("left", hl.dsp.layout("cycleprev"))
        hl.bind("SHIFT + C", hl.dsp.layout("cycleprev"))
        hl.bind("c", hl.dsp.layout("cyclenext"))
        hl.bind("right", hl.dsp.layout("cyclenext"))

        -- swap
        hl.bind("p", hl.dsp.layout("swapprev"))
        hl.bind("up", hl.dsp.layout("swapprev"))
        hl.bind("n", hl.dsp.layout("swapnext"))
        hl.bind("down", hl.dsp.layout("swapnext"))

        -- roll
        hl.bind("SHIFT + p", hl.dsp.layout("rollprev"))
        hl.bind("SHIFT + up", hl.dsp.layout("rollprev"))
        hl.bind("SHIFT + n", hl.dsp.layout("rollnext"))
        hl.bind("SHIFT + down", hl.dsp.layout("rollnext"))

        -- orientation
        hl.bind("t", hl.dsp.layout("orientationtop"))
        hl.bind("b", hl.dsp.layout("orientationbottom"))
        hl.bind("l", hl.dsp.layout("orientationleft"))
        hl.bind("r", hl.dsp.layout("orientationright"))
        hl.bind("SHIFT + return", hl.dsp.layout("orientationcenter"))

        hl.bind("tab", hl.dsp.layout("addmaster"))
        hl.bind("SHIFT + tab", hl.dsp.layout("removemaster"))
        hl.bind("space", hl.dsp.layout("swapwithmaster"))
        hl.bind("return", hl.dsp.layout("focusmaster"))
        hl.bind("escape", hl.dsp.submap("reset"))
    end)

    hl.bind(opts.mod .. "+" .. opts.key, (function()
        if ws_is_master() then
            hl.dispatch(hl.dsp.submap(opts.name))
        else
            hl.notification.create({
                text = masterSubmapOtherMsg,
                timeout = 5000,
                color = "rgb(FF0000)",
                -- icon? integer|string,
                -- font_size? number
            })
        end
    end))

    if opts.notify then
        hl.bind(opts.mod .. "+" .. opts.key, function()
            if ws_is_master() then
                hl.notification.create({
                    text = masterSubmapMsg,
                    timeout = 8000,
                    color = "rgb(0088CC)",
                    -- icon? integer|string,
                    -- font_size? number
                })
            end
        end)
    end
end
