{
  config,
  lib,
  pkgs,
  ...
}:
{
  # blender seems to cause python3 to be included
  #
  # - launches with `blender --python-use-system-env` ... didn't know 'bout that...
  environment.systemPackages = with pkgs; [
    blender-hip
  ];

  # seems to have installed these:

  # | net        | certifi  | idna       |           |                    |
  # | web        | requests | urllib3    | zstandard | charset-normalizer |
  # | graphics   | openusd  | materialx  | numpy     | pyopengl           |
  # | templating | jinja2   | markupsafe |           |                    |

}
