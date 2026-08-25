--* Misc

--** Ignore maximize requests from apps

-- "You'll probably like this."

hl.window_rule({ match = { class = ".*" }, suppress_event = "maximize" })

--** Fix some dragging issues with XWayland
hl.window_rule({ match = { class = "^$", title = "^$", xwayland = 1, float = "yes", fullscreen = 0, pin = 0 }, })

--** Inhibit Idle

--*** Browsers
for i, c in ipairs({ "firefox", "chromium", "chromium-browser" }) do
    hl.window_rule({ match = { class = "(" .. c .. ") match:fullscreen", }, idle_inhibit = "fullscreen" })
end

--* utils
hl.window_rule({ match = { class = "(wev)", }, stay_focused = 1, float = 1, size = "420 42" })

-- make it look wierd

--* Hardware

--** Peripherals
hl.window_rule({ match = { class = "(.piper-wrapped)", }, float = 1, })

--*** piper app (via nix)

--** Bluetooth
hl.window_rule({ match = { class = "(.blueman-manager)(-wrapped)?", }, size = "720 640", move = "100%-w-5% 5%", float = 1, })

-- match:title (Bluetooth Devices) # popups are handled by hyprutils-qt

-- move match:random  title =! Bluetooth Devices

--** Graphics
hl.window_rule({ match = { title = "(GPU-Viewer)(.*)$", }, float = 1, })
hl.window_rule({ match = { class = "(gpu_viewer.py)$", }, float = 1, })

--* XDG and Desktop
hl.window_rule({ match = { class = "(nwg-look)", }, float = 1, })
hl.window_rule({ match = { class = "(nwg-displays)", }, float = 1, })
hl.window_rule({ match = { class = "(xdg-desktop-portal-gtk)", }, float = 1, })
hl.window_rule({ match = { class = "(xdg-desktop-portal-gtk)", title = "^(Open File)$", }, size = "960 960", })

--** FCITX
hl.window_rule({ match = { class = "(fcitx5-config-qt)", }, float = 1, })
hl.window_rule({ match = { title = "(Fcitx Configuration)", }, float = 1, })

--** D-Bus
hl.window_rule({ match = { class = "(org.freedesktop.Bustle)", }, float = 1, })
hl.window_rule({ match = { class = "(qdbusviewer)", }, float = 1, })

--** Flatpak
hl.window_rule({ match = { class = "(com.github.tchx84.Flatseal)", }, float = 1, })

--* Programming

--** Open Cascade (TCL Wish)
hl.window_rule({ match = { title = "(AXON)", }, float = 1, })
hl.window_rule({ match = { title = "(\\+X\\+Z)", }, float = 1, })
hl.window_rule({ match = { title = "(\\+Y\\+Z)", }, float = 1, })
hl.window_rule({ match = { title = "(\\+X\\+Y)", }, float = 1, })
hl.window_rule({ match = { title = "(-2D-)", }, float = 1, })
hl.window_rule({ match = { title = "(-Y\\+Z)", }, float = 1, })

--** plots
hl.window_rule({ match = { class = "(Gnuplot)", }, float = 1, })
hl.window_rule({ match = { class = "(python3)", title = "(Figure.*)", }, float = 1, })

--* Browsers

--** Chromium

--*** Profile Selection
hl.window_rule({ match = { title = "(Chromium)", }, float = 1, size = "1150 1050" })

for i, c in ipairs({ "chromium", "chromium-browser" }) do
    hl.window_rule({ match = { class = "(" .. c .. ")", title = "(DevTools)", }, float = 1, })
end

--*** Devtools

--** Chrome

--*** Welcome Screen
hl.window_rule({ match = { title = "(Welcome [tT]o Google Chrome)$", }, float = 1, })

--*** Profile Selection
hl.window_rule({ match = { title = "(Google Chrome)$", }, float = 1, })

-- signin/profile selection

--** Firefox
hl.window_rule({ match = { title = "(Picture-in-Picture)", }, float = 1, })

-- not gonna match:work  match:https //www.reddit.com/r/hyprland/comments/1h7wxrg/comment/m13iedj/
-- windowrule = float yes, match:class ^(firefox)$, match:title ^(Developer Tools)(.*)$


--* KDE Import

hl.window_rule({ match = { class = "(Gnuplot)", }, float = 1, })

--** KDE Float by Class

-- windowrule = float yes, match:class (Conky)
-- windowrule = float yes, match:class (systemsettings)

hl.window_rule({ match = { class = "(Lxtask)", }, float = 1, })
hl.window_rule({ match = { class = "(com.github.qarmin.czkawka)", }, float = 1, })
hl.window_rule({ match = { class = "(fpakman)", }, float = 1, })
hl.window_rule({ match = { class = "(Clipgrab)", }, float = 1, })

-- windowrule = float yes, match:class (Exo-helper*)

hl.window_rule({ match = { class = "(Pavucontrol)", }, float = 1, })
hl.window_rule({ match = { class = "(Audacious)", }, float = 1, })
hl.window_rule({ match = { class = "(Lxappearance)", }, float = 1, })

-- windowrule = float yes, match:class (Galculator

hl.window_rule({ match = { class = "(zoom)", }, float = 1, })
hl.window_rule({ match = { class = "(Timeshift-gtk)", }, float = 1, })
hl.window_rule({ match = { class = "(Lxrandr)", }, float = 1, })
hl.window_rule({ match = { class = "(pwsafe)", }, float = 1, })
hl.window_rule({ match = { class = "(kwin_rules_dialog)", }, float = 1, })
hl.window_rule({ match = { class = "(Arandr)", }, float = 1, })
hl.window_rule({ match = { class = "(mpv)", }, float = 1, })
hl.window_rule({ match = { class = "(Manjaro-hello)", }, float = 1, })
hl.window_rule({ match = { class = "(Viewnior)", }, float = 1, })
hl.window_rule({ match = { class = "(feh)", }, float = 1, })
hl.window_rule({ match = { class = "(garuda-welcome)", }, float = 1, })
hl.window_rule({ match = { class = "(Xfburn)", }, float = 1, })

-- windowrule = float yes, match:class (org.kde.dolphin

hl.window_rule({ match = { class = "(File-roller)", }, float = 1, })
hl.window_rule({ match = { class = "(org.gnome.FileRoller)", }, float = 1, })
hl.window_rule({ match = { class = "^(kdialog)(.*)$", }, float = 1, })
hl.window_rule({ match = { class = "(obs com.obsproject.Studio)", }, float = 1, })
hl.window_rule({ match = { class = "(ksecretd org.kde.ksecretd)", }, float = 1, })

--** KDE Float by Title
hl.window_rule({ match = { title = "(alsamixer)", }, float = 1, })
hl.window_rule({ match = { title = "(Event Tester)", }, float = 1, })
hl.window_rule({ match = { title = "(Speedbar)", }, float = 1, })

-- ---------------------------------------------

--** KDE Ignore by Class
hl.window_rule({ match = { class = "(Alacritty)$", }, tile = 1 })
hl.window_rule({ match = { class = "(virtualbox)", }, float = 1, })
hl.window_rule({ match = { class = "(Gnuplot)", }, float = 1, })
hl.window_rule({ match = { class = "([tT]hunar)", }, float = 1, })
hl.window_rule({ match = { class = "(org.gnome.Nautilus)", }, float = 1, })
hl.window_rule({ match = { class = "(manjaro-settings-manager)", }, float = 1, })
hl.window_rule({ match = { class = "(GParted)", }, float = 1, })
hl.window_rule({ match = { class = "(Timeset-gui)", }, float = 1, })

-- windowrule = float yes, match:class (garuda-settings-manager)

hl.window_rule({ match = { class = "(Lxappearance)", }, float = 1, })
hl.window_rule({ match = { class = "(Lightdm-settings)", }, float = 1, })
hl.window_rule({ match = { class = "(Anki)", }, float = 1, })

-- windowrule = float yes, match:class (kded5)

hl.window_rule({ match = { class = "(Nitrogen)", }, float = 1, })
hl.window_rule({ match = { class = "(Qtconfig-qt4)", }, float = 1, })
hl.window_rule({ match = { class = "(ykman-gui)", }, float = 1, })

-- windowrule = float yes, match:class (kded)

hl.window_rule({ match = { class = "(Oblogout)", }, float = 1, })
hl.window_rule({ match = { class = "(Simple-scan)", }, float = 1, })
hl.window_rule({ match = { class = "(syncthing-gtk)", }, float = 1, })

-- windowrule = float yes, match:class (spectacle)
-- windowrule = float yes, match:class (octopi)
-- windowrule = float yes, match:class (Pamac-updater)

hl.window_rule({ match = { class = "(freecad)", }, float = 1, })

-- windowrule = float yes, match:class (org.kde.yakuake)

hl.window_rule({ match = { class = "(Skype)", }, float = 1, })
hl.window_rule({ match = { class = "(Pamac-manager)", }, float = 1, })
hl.window_rule({ match = { class = "(tk)", }, float = 1, })
hl.window_rule({ match = { class = "(plasmashell)", }, float = 1, })
hl.window_rule({ match = { class = "(Xfburn)", }, float = 1, })
hl.window_rule({ match = { class = "(azote)", }, float = 1, })
hl.window_rule({ match = { class = "(toplevel)", }, float = 1, })
hl.window_rule({ match = { class = "(polkit)", }, float = 1, })
hl.window_rule({ match = { class = "(qt5ct)", }, float = 1, })
hl.window_rule({ match = { class = "(GParted)", }, float = 1, })

-- windowrule = float yes, match:class (krunner)
-- windowrule = float yes, match:class (kcalc)
-- windowrule = float yes, match:class (kcm_kwinrules)
-- windowrule = float yes, match:title (Btrfs Assistant)

--** KDE Ignore By Title
hl.window_rule({ match = { title = "(Preferences)", }, float = 1, })
hl.window_rule({ match = { title = "(PipeControl)", }, float = 1, })
hl.window_rule({ match = { title = "(Configure Krita)", }, float = 1, })
hl.window_rule({ match = { title = "(Timer)", }, float = 1, })
hl.window_rule({ match = { title = "(Remmina)", }, float = 1, })
hl.window_rule({ match = { title = "(Blender Preferences)", }, float = 1, })
hl.window_rule({ match = { title = "(PERS)", }, float = 1, })

--** Browser
hl.window_rule({ match = { title = "(about blank- Group )(.*)$", }, float = 1, })

--* Krita (everything?)
hl.window_rule({ match = { title = "(- Krita)(.*)", }, float = 1, })

--* Steam
hl.window_rule({ match = { title = "(Steam- News)", }, float = 1, })
hl.window_rule({ match = { title = "(Friends List)", }, float = 1, })
hl.window_rule({ match = { title = "(Steam Guard)(.*)$", }, float = 1, })
hl.window_rule({ match = { title = "(Steam Login)", }, float = 1, })
hl.window_rule({ match = { class = "(steam)", }, float = 1, })
