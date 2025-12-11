{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
{
  hjem.users.dc.xdg.data = {
    files = {
      "autostart/thunderbird.desktop" = {
        text = ''
          [Desktop Entry]
          Actions=profile-manager-window;compose-message;calendar;open-address-book;open-file;keymanager;settings
          Categories=Network;Chat;Email;Feed;GTK;News
          Comment=Read and write e-mails or RSS feeds, or manage tasks on calendars.
          Exec=thunderbird --name org.mozilla.Thunderbird -mail -calendar
          GenericName=Email Client
          Icon=thunderbird
          # Icon=org.mozilla.Thunderbird
          Keywords=mail;email;e-mail;messages;rss;calendar;address book;addressbook;chat
          MimeType=message/rfc822;x-scheme-handler/mailto;text/calendar;text/vcard;text/x-vcard;x-scheme-handler/webcal;x-scheme-handler/webcals;x-scheme-handler/mid;
          Name=Thunderbird (Hjem)
          StartupNotify=true
          StartupWMClass=org.mozilla.Thunderbird
          Terminal=false
          Type=Application
          Version=1.5
        '';
      };
      "applications/thunderbird.desktop" = {
        text = ''
          [Desktop Entry]
          Actions=profile-manager-window;compose-message;calendar;open-address-book;open-file;keymanager;settings
          Categories=Network;Chat;Email;Feed;GTK;News
          Comment=Read and write e-mails or RSS feeds, or manage tasks on calendars.
          Exec=thunderbird --name org.mozilla.Thunderbird %U
          GenericName=Email Client
          Icon=thunderbird
          # Icon=org.mozilla.Thunderbird
          Keywords=mail;email;e-mail;messages;rss;calendar;address book;addressbook;chat
          MimeType=message/rfc822;x-scheme-handler/mailto;text/calendar;text/vcard;text/x-vcard;x-scheme-handler/webcal;x-scheme-handler/webcals;x-scheme-handler/mid;
          Name=Thunderbird (Hjem)
          StartupNotify=true
          StartupWMClass=org.mozilla.Thunderbird
          Terminal=false
          Type=Application
          Version=1.5

          [Desktop Action profile-manager-window]
          Exec=thunderbird --ProfileManager
          Name=Profile Manager (Hjem)

          [Desktop Action compose-message]
          Exec=thunderbird -compose
          Name=Compose Message (Hjem)

          [Desktop Action calendar]
          Exec=thunderbird -calendar
          Name=Calendar (Hjem)

          [Desktop Action open-file]
          Exec=thunderbird -file %U
          Name=Open File (.ical) (Hjem)
          # TODO ensure .ical mimetype handles this

          [Desktop Action open-address-book]
          Exec=thunderbird -addressbook
          Name=Address Book (Hjem)

          [Desktop Action settings]
          Exec=thunderbird -settings
          Name=Settings (Hjem)

          [Desktop Action keymanager]
          Exec=thunderbird -keymanager
          Name=Manage OpenPGP Keys (Hjem)
        '';
        clobber = true;
      };
    };
  };
}
