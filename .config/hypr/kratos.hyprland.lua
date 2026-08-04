--* prelude

--** env
local host = "kratos"

-- TODO: hypr: test package.path without omarchy setup
require("hypr.common.helpers")
require("hypr.common.keycodes")
require("hypr.common.tags")

--** omarchy prelude
require(host .. ".bindings.media")
require(host .. ".bindings.tiling")
require(host .. ".bindings.utilities")
require(host .. ".bindings.apps")

--* common
require("common.bindings.tiling")
require("common.bindings.utilities")

--* config

--** per-host
require(host .. ".monitors")
require(host .. ".input")
require(host .. ".workspaces")
-- require(host .. ".autostart")
require(host .. ".g502")

--** workspaces
require("looknfeel")

--*** kratos only
require("workspaces.scratchpad")

--*** per-app
require("workspaces.audio")
require("workspaces.blender")
require("workspaces.discord")
require("workspaces.docs")
require("workspaces.email")
require("workspaces.dotfiles")
require("workspaces.forum")
require("workspaces.irc")
require("workspaces.man")
require("workspaces.obs")
require("workspaces.orgmode")
-- require("workspaces.pass")
require("workspaces.tuitray")
-- require("workspaces.waydroid")
