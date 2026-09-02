function ws_is_manual()
    local ws = hl.get_active_workspace()
    if hl.get_active_special_workspace() then
        ws = hl.get_active_special_workspace()
    end

    -- hl.notification.create({
    --     text = (ws ~= nil and ws.tiled_layout) or "fdsa",
    --     timeout = 5000,
    --     color = "rgb(FF0000)",
    --     -- icon? integer|string,
    --     -- font_size? number
    -- })

    return (ws ~= nil and ws.tiled_layout == "lua:manual")
end

-- [split]h [split]v [split]toggle promote swapnext swapprev rotate
function dc.lo.manual.bind_submap(opts)
    opts = opts or {}
    opts.name = opts.name or "ManualLayoutSubmap"
    opts.key = opts.key or "L"
    opts.mod = opts.mod or "MOD3"
    opts.notify = opts.notify or true

    local manualSubmapOtherMsg = opts.name .. [[: not active on this workspace.]]
    local manualSubmapMsg = opts.name .. [[

h	splith
v	splitv
t	toggle

p	↑	swapprev
n	↓	swapnext
c	←	cycle
C	→	cycle

r	SPACE	rotate
	RET		promote]]

    hl.define_submap(opts.name, function()
        hl.bind("h", hl.dsp.layout("splith"))
        hl.bind("v", hl.dsp.layout("splitv"))
        hl.bind("t", hl.dsp.layout("toggle"))

        -- cycle
        hl.bind("c", hl.dsp.window.cycle_next())
        hl.bind("left", hl.dsp.window.cycle_next())
        hl.bind("SHIFT + C", hl.dsp.window.cycle_next({ next = false }))
        hl.bind("right", hl.dsp.window.cycle_next({ next = false }))

        -- swap
        hl.bind("p", hl.dsp.layout("swapprev"))
        hl.bind("up", hl.dsp.layout("swapprev"))
        hl.bind("n", hl.dsp.layout("swapnext"))
        hl.bind("down", hl.dsp.layout("swapnext"))

        -- rotate
        hl.bind("r", hl.dsp.layout("rotate"))
        hl.bind("space", hl.dsp.layout("rotate"))

        hl.bind("return", hl.dsp.layout("promote"))
        hl.bind("escape", hl.dsp.submap("reset"))
    end)

    hl.bind(opts.mod .. "+" .. opts.key, (function()
        if ws_is_manual() then
            hl.dispatch(hl.dsp.submap(opts.name))
        else
            hl.notification.create({
                text = manualSubmapOtherMsg,
                timeout = 5000,
                color = "rgb(FF0000)",
                -- icon? integer|string,
                -- font_size? number
            })
        end
    end))

    if opts.notify then
        hl.bind(opts.mod .. "+" .. opts.key, function()
            if ws_is_manual() then
                hl.notification.create({
                    text = manualSubmapMsg,
                    timeout = 5000,
                    color = "rgb(0088CC)",
                    -- icon? integer|string,
                    -- font_size? number
                })
            end
        end)
    end
end
