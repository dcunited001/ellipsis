{
  config,
  lib,
  pkgs,
  ...
}:
{
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
  };

  # fcitx5link=$(which fcitx5)
  # fcitx5bin=$(readlink $fcitx5link)
  # fcitx5base=$(dirname $fcitx5bin)/..
  #
  # # this fcitx5 MUST use the bundled autostart
  #
  # cat "$fcitx5base/etc/xdg/autostart/org.fcitx.Fcitx5.desktop"
  #
  # to see how addons relate:
  #
  # grep -C5 -ne dbus $fcitx5base/share/fcitx5/addon/*
  # grep -C5 -ne virtual $fcitx5base/share/fcitx5/addon/*

  # The GTK theme is still messed up (contrast zeros out on focus)

  i18n.inputMethod.fcitx5 = {
    addons = [
      pkgs.fcitx5-mozc
      # pkgs.fcitx5-nord
    ];

    waylandFrontend = true;

    settings.inputMethod = {
      GroupOrder."0" = "Default";
      "Groups/0" = {
        Name = "Default";
        "Default Layout" = "us";
        DefaultIM = "keyboard-us-altgr-intl";
      };

      # the io keyboard is defined in ~/.config/xkb/*
      # so this blows up for other users

      "Groups/0/Items/0".Name = "keyboard-us-altgr-intl";
      "Groups/0/Items/1".Name = "keyboard-io-altgr-intl";
      "Groups/0/Items/2".Name = "mozc";
    };

    settings.addons = {
      #   pinyin.globalSection.EmojiEnabled = "True";

      classicui.globalSection.Theme = "default-dark";
      classicui.globalSection.DarkTheme = "default";
      classicui.globalSection.UseDarkTheme = "True";
      classicui.globalSection.UseAccentColor = "True";
    };

    # ignoreUserConfig = true;
    settings.globalOptions = {
      "Behavior" = {
        "ActiveByDefault" = "True";
        "AltTriggerKeys" = "";
        "CustomXkbOption" = "lv3:ralt_alt,altwin:menu_win,mylvl5:ins_switch,mylvl3:sclk_switch";
        "ShareInputState" = "All";
        "resetStateWhenFocusIn" = "No";
      };

      # TriggerKeys give fctix5 focus.
      # idk whether Behavior/ActiveByDefault needs to be on
      "Hotkey/TriggerKeys" = {
        "0" = "Alt+Shift+Super+space";
        "1" = "Zenkaku_Hankaku";
        "2" = "Hangul";
      };

      # Without a trigger key, this is insufficient (requires focus?)
      "Hotkey/EnumerateGroupForwardKeys" = { };
      # "0" = "Alt+Shift+Super+space";

      # more packages should maybe be disabled
      "Behavior/DisabledAddons" = {
        # Nope
        "0" = "clipboard";
        # M-x describe-emoji
        "1" = "emoji";
        # Not in use
        "2" = "fcitx4frontend";
        # KDE/plasma only
        "3" = "kimpanel";
        # what is this? idk?
        "4" = "quickphrase";
        # latin vocabulary conjugates, kanji not so much
        "5" = "spell";
        #M-x insert-char
        "6" = "unicode";
      };
    };
  };
}
