-- Omarchy's bootstrap keeps path setup out of this user config.
dofile((os.getenv("OMARCHY_PATH") or "/usr/share/omarchy") .. "/default/hypr/bootstrap.lua")

local host = "kharis"

-- omarchy_default_bindings = false
-- omarchy_preinstalled_bindings = false

-- Load Omarchy defaults.
require("default.hypr.omarchy")
require("hypr.common.helpers")
require("hypr.common.keycodes")


-- hypr.kharis.monitors works as symlink from ./monitors.lua -> kharis/monitors.lua
require("hypr.kharis.monitors") -- loads and then unloads (resets scale only)
-- require("hypr.monitors") -- works as file ./monitors.lua
require("hypr.looknfeel")
require("hypr." .. host .. ".input")
require("hypr." .. host .. ".bindings")
require("hypr." .. host .. ".autostart")

-- Toggle config flags dynamically.
require("default.hypr.toggles")
