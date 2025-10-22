{
  pkgs,
  ...
}:

{
  services.usbmuxd.enable = true;
  services.usbmuxd.package = pkgs.usbmuxd2;

  environment.systemPackages = with pkgs; [
    libimobiledevice
    ifuse # optional, to mount using 'ifuse'
  ];
}
