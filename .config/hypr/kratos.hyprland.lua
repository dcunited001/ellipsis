--* prelude
local host = "kratos"

require("common.helpers")
require("defaults")
require("keycodes")

--** omarchy prelude
require(host .. ".bindings.media")
require(host .. ".bindings.tiling")
require(host .. ".bindings.utilities")
require(host .. ".bindings.apps")

--* common
require("common.bindings.tiling")
require("common.bindings.utilities")

--* config

--** host-specific
require(host .. ".monitors")
require(host .. ".input")
require(host .. ".workspaces")
-- require(host .. ".autostart")
require(host .. ".g502")

--** workspaces
require("theme")
require("common.workspaces")
require("workspaces.audio")
require("workspaces.blender")
require("workspaces.discord")
require("workspaces.docs")
require("workspaces.email")
-- require("workspaces.dotfiles")
-- require("workspaces.forum")
require("workspaces.irc")
require("workspaces.man")
require("workspaces.obs")
require("workspaces.orgmode")
require("workspaces.tuitray")
-- require("workspaces.pass")
-- require("workspaces.waydroid")
