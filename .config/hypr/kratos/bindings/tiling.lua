--* Misc

-- These were organized to make diffing with omarchy easier

--** Close windows

hl.bind("SUPER + W", hl.dsp.window.close(), { description = "Close window" })

--** Control tiling

-- dwindle-specific
-- bindd = SUPER, J, Toggle window split, togglesplit, -- dwindle
hl.bind("SUPER + P", hl.dsp.window.pseudo(), { description = "Pseudo window" })

hl.bind("SUPER + T", hl.dsp.window.float(), { description = "Toggle window floating/tiling" })
-- bindd = SUPER, O, Pop window out (float & pin), exec, omarchy-hyprland-window-pop

hl.bind("SUPER + SHIFT + F11", hl.dsp.window.fullscreen(), { description = "Full width" })
hl.bind("SUPER + ALT + F11", hl.dsp.window.fullscreen(), { description = "Force full screen" })

--** Move focus with SUPER + arrow keys
hl.bind("SUPER + LEFT", hl.dsp.focus({ direction = "left" }), { description = "Move window focus left" })
hl.bind("SUPER + RIGHT", hl.dsp.focus({ direction = "right" }), { description = "Move window focus right" })
hl.bind("SUPER + UP", hl.dsp.focus({ direction = "up" }), { description = "Move window focus up" })
hl.bind("SUPER + DOWN", hl.dsp.focus({ direction = "down" }), { description = "Move window focus down" })

--* Workspaces

--** Workspaces + [1-9; 0]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind("SUPER + " .. key, hl.dsp.focus({ workspace = i }),
        { description = "Switch to workspace " .. i })
    hl.bind("SUPER + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }),
        { description = "Move window to workspace " .. i })
end

--** Control scratchpad
-- bindd = SUPER, S, Toggle scratchpad, togglespecialworkspace, scratchpad
-- bindd = SUPER ALT, S, Move window to scratchpad, movetoworkspacesilent, special:scratchpad

--** Tab between workspaces
hl.bind("SUPER + TAB", hl.dsp.layout("swapwithmaster"), { description = "Swap window with master" })
-- bindd = SUPER SHIFT, TAB, layoutmsg, Roll next, rollnext

--** Move workspaces to other monitors
hl.bind("SUPER + Grave", hl.dsp.window.cycle_next(), { description = "Swap window with master" })
hl.bind("SUPER + SHIFT + Grave", hl.dsp.window.cycle_next(), { description = "Swap prev window with master" })

--* Windows

--** Swap active window with the one next to it with SUPER + SHIFT + arrow keys
hl.bind("SUPER + SHIFT + LEFT", hl.dsp.window.swap({ direction = "left" }),
    { description = "Swap window to the left" })
hl.bind("SUPER + SHIFT + RIGHT", hl.dsp.window.swap({ direction = "right" }),
    { description = "Swap window to the right" })
hl.bind("SUPER + SHIFT + UP", hl.dsp.window.swap({ direction = "up" }),
    { description = "Swap window up" })
hl.bind("SUPER + SHIFT + DOWN", hl.dsp.window.swap({ direction = "down" }),
    { description = "Swap window down" })

--** Cycle through applications on active workspace

-- note: conflicts with group bindings

-- bindd = SUPER ALT, TAB, Cycle to next window, layoutmsg, rollnext
-- bindd = SUPER ALT SHIFT, Tab, Cycle to prev window, layoutmsg, rollprev

--** Resize active window

hl.bind("SUPER + SHIFT + right", hl.dsp.window.resize({ x = 100, y = 0, relative = true }), { repeating = true },
    { description = "Expand window left" })
hl.bind("SUPER + SHIFT + left", hl.dsp.window.resize({ x = -100, y = 0, relative = true }), { repeating = true },
    { description = "Shrink window width" })
hl.bind("SUPER + SHIFT + down", hl.dsp.window.resize({ x = 0, y = 100, relative = true }), { repeating = true },
    { description = "Expand window down" })
hl.bind("SUPER + SHIFT + up", hl.dsp.window.resize({ x = 0, y = -100, relative = true }), { repeating = true },
    { description = "Shrink window up" })

--** Scroll through existing workspaces with SUPER + scroll
hl.bind("SUPER + mouse_down", hl.dsp.focus({ workspace = "e+1" }), { description = "Scroll active workspace forward" })
hl.bind("SUPER + mouse_up", hl.dsp.focus({ workspace = "e-1" }), { description = "Scroll active workspace backward" })

--** Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind("SUPER + mouse:272", hl.dsp.window.drag(), { mouse = true, description = "Move window" })
hl.bind("SUPER + mouse:273", hl.dsp.window.resize(), { mouse = true, description = "Resize window" })

--* Groups

--** Toggle groups
hl.bind("SUPER + G", hl.dsp.group.toggle(), { description = "Toggle window grouping" })
hl.bind("SUPER + ALT + G", hl.dsp.window.move({ out_of_group = true }),
    { description = "Move active window out of group" })

--** Join groups
hl.bind("SUPER + ALT + LEFT", hl.dsp.window.move({ into_group = "left" }),
    { description = "Move window to group on left" })
hl.bind("SUPER + ALT + RIGHT", hl.dsp.window.move({ into_group = "right" }),
    { description = "Move window to group on right" })
hl.bind("SUPER + ALT + UP", hl.dsp.window.move({ into_group = "up" }),
    { description = "Move window to group on up" })
hl.bind("SUPER + ALT + DOWN", hl.dsp.window.move({ into_group = "down" }),
    { description = "Move window to group on down" })

--** Navigate a single set of grouped windows
hl.bind("SUPER + ALT + TAB", hl.dsp.group.next(), { description = "Next window in group" })
hl.bind("SUPER + ALT + SHIFT + TAB", hl.dsp.group.next({ forward = false }), { description = "Previous window in group" })

--** Overload lateral window navigation for grouped windows

hl.bind("SUPER + ALT + LEFT", hl.dsp.group.next({ forward = false }), { description = "Move grouped window focus left" })
hl.bind("SUPER + ALT + RIGHT", hl.dsp.group.next(), { description = "Move grouped window focus right" })

--** Scroll through a set of grouped windows with SUPER + ALT + scroll

hl.bind("SUPER + ALT + mouse_down", hl.dsp.group.next(), { description = "Next window in group" })
hl.bind("SUPER + ALT + mouse_up", hl.dsp.group.next({ forward = false }), { description = "Previous window in group" })

--** Activate window in a group by number

hl.bind("SUPER + ALT + code:10", hl.dsp.group.next({ forward = false }), { description = "Switch to group window 1" })
hl.bind("SUPER + ALT + code:11", hl.dsp.group.next({ forward = false }), { description = "Switch to group window 2" })
hl.bind("SUPER + ALT + code:12", hl.dsp.group.next({ forward = false }), { description = "Switch to group window 3" })
hl.bind("SUPER + ALT + code:13", hl.dsp.group.next({ forward = false }), { description = "Switch to group window 4" })
hl.bind("SUPER + ALT + code:14", hl.dsp.group.next({ forward = false }), { description = "Switch to group window 5" })
