--* Qemu
local ws_qemu = { name = "qemu", mod = "MOD3", key = "Q" }
ws_qemu.border_color = { colors = { "rgba(E2A61099)", "rgba(E276A699)" }, angle = 130 }
local wsname = "special:" .. ws_qemu.name

-- local hyprhost = os.getenv("HYPRHOST")
-- if hyprhost == "kharis" then
--     ws_qemu.monitor = dc.m2.output
-- elseif hyprhost == "kratos" then
--     ws_qemu.monitor = dc.m3.output
-- end

--** Binds
dc.ws.binds_ws(ws_qemu)

--** Workspace
hl.workspace_rule({
    workspace = wsname,
    border_size = 10
})

--** Rules

hl.window_rule({
    match = { title = "QEMU" },
    workspace = wsname,
    float = true,
    size = { 960, 960 },
    border_color = ws_qemu.border_color
})

-- local qemu_classes = { "(chrome-qemu.com.*)", "qemu", "vesktop" }

-- for i, klass in ipairs(qemu_classes) do
--     hl.window_rule({
--         match = { class = klass },
--         workspace = wsname,
--         size = ws_qemu.size,
--         move = "10% 10%",
--     })
-- end
