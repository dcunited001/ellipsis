hl.on("hyprland.start", function()
    hl.exec_cmd("omarchy-launch-shell")
    hl.exec_cmd("omarchy-provision-first-run")
    hl.exec_cmd("omarchy-powerprofiles-init")
    hl.exec_cmd(o.launch("omarchy-hyprland-monitor-watch"))

    -- Run post-boot hooks after startup config has loaded.
    hl.exec_cmd("sleep 2 && omarchy-hook post-boot")
end)
