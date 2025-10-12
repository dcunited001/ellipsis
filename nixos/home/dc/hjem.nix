{ inputs, lib, pkgs, ... }:
let
  inherit (lib.fileset) unions fromSource toSource toList;
  inherit (lib) fileContents;
  dcstaticdots = (pkgs.callPackage
    (lib.custom.relativeToRoot "pkgs/common/dcstaticdots/package.nix") { });
in {
  hjem = {
    linker = inputs.hjem.packages.${pkgs.stdenv.hostPlatform.system}.smfh;

    users = {
      dc = {
        enable = true;

        # fdsa = lib.mapAttrsToList; # lib.fileContents;

        # let root = ./../gh; in {fdsa = lib.fileset.unions [(root + /f/bash)];}
        # let root = ./../gh;
        # in lib.fileset.toSource {
        #   inherit root;
        #   fileset = lib.fileset.unions [(root + /f/.bashrc)];
        #   }

        files = {
          ".foo".text = ''
            [[ $- != *i* ]] && return
            shopt -s histappend

            [[ "$TERM" == "dumb" ]] || export TERM="xterm-256color"

            export INSIDE_TRAMP="$\{INSIDE_EMACS/*tramp*/tramp}"
            export DOTS_RC_D=$DOTS_CFG_SHELL/rc.d
            [[ -f $DOTS_CFG_SHELL/_load_rc.d.sh ]] && source $DOTS_CFG_SHELL/_load_rc.d.sh

            unset SSH_AGENT_PID
            if [ "$\{gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
              export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
            fi

            [[ -e "$(command -v direnv)" ]] && eval "$(direnv hook bash)"

          '';
          ".bar".source = "/.bash_logout";
        }; # // dfList;
      };
    };
  };
}

# getContents = (dest: src: lib.fileContents src);
# root = "./..";

# let?
#   bashFiles =
#     lib.mapAttrs getContents { ".bashrc" = ../../../../gh/f/.bashrc; };
# in { ".foo".text = "bar"; } // bashFiles;

# nix eval .#nixosConfigurations.kratos.config.hjem.users.dc.files.'".foo"' --json | jq

