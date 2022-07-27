alsa_monitor.rules = {
  {
    -- Rules for matching a device or node. It is an array of
    -- properties that all need to match the regexp. If any of the
    -- matches work, the actions are executed for the object.
    matches = {
      {
        -- This matches all cards.
        { "device.name", "matches", "alsa_card.*" },
      },
    },
    apply_properties = {
      ["vm.node.defaults"] = {
        ["api.alsa.period-size"] = 1024,
        ["api.alsa.headroom"] = 8192,
      },
    }
  }
}
