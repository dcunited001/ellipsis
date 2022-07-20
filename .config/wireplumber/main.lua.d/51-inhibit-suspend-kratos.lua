-- alsa_output.pci-0000_06_1b.0.pro-output-0
inhibit_suspend = { ["session.suspend-timeout-seconds"] = 0 }

sc_starship_out = {
  matches = {
    {
      {"node.name", "matches", "alsa_output.pci-0000_06_1b*"}
    },
  },
  apply_properties = inhibit_suspend
}

sc_navi_out = {
  matches = {
    {
      {"node.name", "matches", "alsa_output.pci-0000_06_11*"}
    },
  },
  apply_properties = inhibit_suspend
}

table.insert(alsa_monitor.rules, sc_starship_out)
table.insert(alsa_monitor.rules, sc_navi_out)
