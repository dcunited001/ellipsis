--* prelude

--** env

--** omarchy prelude

-- Omarchy's bootstrap keeps path setup out of this user config.
dofile((os.getenv("OMARCHY_PATH") or "/usr/share/omarchy") .. "/default/hypr/bootstrap.lua")

-- omarchy_default_bindings = false, omarchy_preinstalled_bindings = false
require("default.hypr.omarchy")

--* config
require("hypr.common.config")
require("hypr." .. dc.host .. ".config")

--** common
require("hypr.common.helpers")
require("hypr.common.keycodes")
require("hypr.common.tags")
require("hypr.common.looknfeel")
require("hypr.common.workspaces")
require("hypr.common.bindings.utilities")

--** per-host
require("hypr." .. dc.host .. ".monitors")
require("hypr." .. dc.host .. ".workspaces")
require("hypr." .. dc.host .. ".input")
require("hypr." .. dc.host .. ".autostart")

--** layouts
require("hypr.layouts.manual.layout")
require("hypr.layouts.manual.submap")
dc.lo.manual.bind_submap({})

-- TODO: compare $HOST/bindings/tiling.lua

--** workspaces

--*** per-app
require("hypr.workspaces.audio")
require("hypr.workspaces.blender")
require("hypr.workspaces.discord")
require("hypr.workspaces.docs")
require("hypr.workspaces.email")
require("hypr.workspaces.dotfiles")
require("hypr.workspaces.forum")
require("hypr.workspaces.frc")
require("hypr.workspaces.irc")
require("hypr.workspaces.man")
require("hypr.workspaces.obs")
require("hypr.workspaces.pass")
require("hypr.workspaces.qemu")
require("hypr.workspaces.orgmode")
require("hypr.workspaces.scratchterm")
require("hypr.workspaces.tuitray")

--** submaps
require("hypr.submaps.hjinspect")
require("hypr.submaps.oinspect")
require("hypr.submaps.walker")

--** rules
require("hypr.rules")

--* finalize
-- Toggle config flags dynamically.
require("default.hypr.toggles")
