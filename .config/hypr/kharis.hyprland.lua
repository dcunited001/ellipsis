--* prelude

--** env
local host = "kharis"

--** omarchy prelude

-- Omarchy's bootstrap keeps path setup out of this user config.
dofile((os.getenv("OMARCHY_PATH") or "/usr/share/omarchy") .. "/default/hypr/bootstrap.lua")

-- omarchy_default_bindings = false, omarchy_preinstalled_bindings = false
require("default.hypr.omarchy")

--* config
require("hypr.common.config")
require("hypr." .. host .. ".config")

--** common
require("hypr.common.helpers")
require("hypr.common.keycodes")
require("hypr.common.tags")
require("hypr.common.looknfeel")
require("hypr.common.workspaces")

--** per-host
require("hypr." .. host .. ".monitors")
require("hypr." .. host .. ".workspaces")
require("hypr." .. host .. ".input")
require("hypr." .. host .. ".bindings")
require("hypr." .. host .. ".autostart")

--** workspaces

--*** per-app
require("workspaces.audio")
require("workspaces.blender")
require("workspaces.discord")
-- require("workspaces.docs")
-- require("workspaces.email")
require("workspaces.dotfiles")
require("workspaces.forum")
-- require("workspaces.irc")
require("workspaces.man")
-- require("workspaces.obs")
require("workspaces.orgmode")

--* finalize
-- Toggle config flags dynamically.
require("default.hypr.toggles")
