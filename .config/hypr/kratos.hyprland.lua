--* prelude

--** env

-- TODO: hypr: organize omarchy modules for nixos (check bootstrap.lua)
local home = os.getenv("HOME")
package.path = home .. "/.config/?.lua;" .. package.path

require("hypr.common.config")
require("hypr." .. dc.host .. ".config")

require("hypr.common.ohelpers")
require("hypr.common.oshim")
require("hypr.common.helpers")
require("hypr.common.keycodes")
require("hypr.common.tags")
require("hypr.common.looknfeel")

--** omarchy prelude
require("hypr." .. dc.host .. ".bindings.media")
require("hypr." .. dc.host .. ".bindings.tiling")
require("hypr." .. dc.host .. ".bindings.utilities")
require("hypr." .. dc.host .. ".bindings.apps")

--* config

--** common
require("common.bindings.tiling")
require("common.bindings.utilities")

--** per-host
require("hypr." .. dc.host .. ".monitors")
require("hypr." .. dc.host .. ".workspaces")
require("hypr." .. dc.host .. ".input")
require("hypr." .. dc.host .. ".workspaces")
require("hypr." .. dc.host .. ".g502")

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
