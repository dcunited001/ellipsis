{ pkgs, ... }: {
  environment.sessionVariables = {
    # TODO: nixos: remove unnecessary reliance on $HOME/bin (use writeScriptBin)
    PATH = "$HOME/bin";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };
}
