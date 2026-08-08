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
require("hypr." .. dc.host .. ".bindings")
require("hypr." .. dc.host .. ".autostart")

-- TODO: compare $HOST/bindings/tiling.lua

--** workspaces

--*** per-app
require("workspaces.audio")
require("workspaces.blender")
require("workspaces.discord")
require("workspaces.docs")
-- require("workspaces.email")
require("workspaces.dotfiles")
require("workspaces.forum")
-- require("workspaces.irc")
require("workspaces.man")
-- require("workspaces.obs")
require("workspaces.orgmode")
require("workspaces.tuitray")

--* finalize
-- Toggle config flags dynamically.
require("default.hypr.toggles")
