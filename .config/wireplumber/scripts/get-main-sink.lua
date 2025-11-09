#!/usr/bin/wpexec
local argv = ...
-- local node_id = ...
local node_name = "Main-Output-Proxy"
if argv then
  argv = argv:parse()
  -- print ("Command-line arguments:")
  -- Debug.dump_table (argv)


  -- if argv["node.id"] then
  --   node_id = argv["node.id"]
  -- end
  if argv["node.name"] then
    node_name = argv["node.name"]
  end
end

obj_mgr = ObjectManager {
  Interest { type = "client" },
  Interest { type = "device" },
  Interest { type = "node" },
}

obj_mgr:connect("installed", function (om)

  -- NOTE there's also "port.group"

  local interest = Interest { type = "node",
    Constraint { "media.class", "equals", "Audio/Sink" },
    Constraint { "node.name", "equals", node_name }
  }

  -- Constraint { "node.name", "matches", node_name } -- newwwp newp newp
  -- Constraint { "node.id", "matches", node_id }     -- also a string (for now)
  local selected_node = nil
  for obj in om:iterate(interest) do
    local node_id = obj["bound-id"]
    local global_props = obj["global-properties"]
    selected_node = Json.Object { id = node_id, name = node_name }
  end

  local node_json = Json.Object { node = selected_node }
  print(node_json:get_data())

  Core.quit()
end)

obj_mgr:activate()
