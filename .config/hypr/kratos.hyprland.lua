--* prelude

--** env
local host = "kratos"

-- TODO: hypr: organize omarchy modules for nixos (check bootstrap.lua)
local home = os.getenv("HOME")
package.path = home .. "/.config/?.lua;" .. package.path

require("hypr.common.config")
require("hypr.common.ohelpers")
require("hypr.common.oshim")
require("hypr.common.helpers")
require("hypr.common.keycodes")
require("hypr.common.tags")
require("hypr.common.looknfeel")

--** omarchy prelude
require("hypr." .. host .. ".bindings.media")
require("hypr." .. host .. ".bindings.tiling")
require("hypr." .. host .. ".bindings.utilities")
require("hypr." .. host .. ".bindings.apps")

--* config

--** common
require("common.bindings.tiling")
require("common.bindings.utilities")

--** per-host
require("hypr." .. host .. ".config")

require(host .. ".monitors")
require(host .. ".input")
require(host .. ".workspaces")
require(host .. ".g502")

--** workspaces

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
