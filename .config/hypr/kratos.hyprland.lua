--* prelude
local host = "kratos"

require("defaults")
require("keycodes")
require("common.functions")

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
