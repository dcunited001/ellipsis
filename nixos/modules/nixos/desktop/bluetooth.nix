{ pkgs, ... }:
{
  # TODO check the list of blueman plugins
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  services = {
    blueman.enable = true;
  };

  # blueman.enable does this
  # environment.systemPackages = [ pkgs.blueman ];
}
