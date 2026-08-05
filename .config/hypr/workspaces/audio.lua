--* Audio Bindings
local ws_music = { name = "music", mod = "MOD3", key = "code:121" }
ws_music.border_color = "rgba(CC3344DD) rgba(44CC6688) rgba(CC3344DD) 45deg"
ws_music.launch = "uwsm app -- vlc"

local ws_audio = { name = "audio", mod = "MOD3", key = "code:123" }
ws_audio.border_color = ws_music.border_color
ws_audio.launch = "uwsm app -- coppwr"

--* Workspaces

--** Audio Workspace

--*** Binds
dc.ws.binds_special(ws_audio.mod, ws_audio.key, ws_audio.name)

--*** Rules
hl.workspace_rule({
    workspace = "special:" .. ws_audio.name,
    border_size = 10,
    on_created_empty = "[float] " .. ws_audio.launch
})

-- helvum
hl.window_rule({
    match = { class = "(org.pipewire.Helvum)" },
    workspace = "special:" .. ws_audio.name,
    float = true,
    move = "5% 5%",
    border_color = ws_audio.border_color
})

hl.window_rule({
    match = { class = "(io.github.dimtpap.coppwr)" },
    workspace = "special:" .. ws_audio.name,
    float = true,
    size = "75% 75%",
    move = "15% 15%",
    border_color = ws_audio.border_color
})

hl.window_rule({
    match = { class = "(org.rncbc.qpwgraph)" },
    workspace = "special:" .. ws_audio.name,
    float = true,
    size = "48% 27%",
    move = "100%-w-5% 100%-w-5%",
    border_color = ws_audio.border_color
})

hl.window_rule({
    match = { class = "(org.pulseaudio.pavucontrol)" },
    workspace = "special:" .. ws_audio.name,
    float = true,
    size = "25% 25%",
    move = "100%-2-5% 5%",
    border_color = ws_audio.border_color
})

--** Music Workspace

--*** Binds
dc.ws.binds_special(ws_music.mod, ws_music.key, ws_music.name)

--*** Rules
hl.workspace_rule({
    workspace = "special:" .. ws_music.name,
    border_size = 10,
    on_created_empty = "[float] " .. ws_music.launch
})

hl.window_rule({
    match = { class = "(vlc)" },
    workspace = "special:" .. ws_music.name,
    float = true
})

hl.window_rule({
    match = { class = "(vlc)", title = "VLC media player" },
    size = "1280 720",
    move = "10% 10%",
    keep_aspect_ratio = true,
    border_color = ws_music.border_color
})
