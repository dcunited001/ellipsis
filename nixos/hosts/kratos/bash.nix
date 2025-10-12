{ lib, pkgs, ... }: {
  programs.bash = {
    enableCompletion = true;

    # https://github.com/nix-community/home-manager/blob/2f607e07f3ac7e53541120536708e824acccfaa8/modules/programs/bash.nix#L173-L175
    shellAliases = {
      pathtr = ''tr ":" "\n"'';
      shitbin = ''echo -e "\033c"'';
      envia = "grep -e '^[A-Za-z0-9]*PATH=' | sort | uniq";
      covia = ''sed -e "s/:/\n:\t/g"'';
      nenvia = "grep -ve '^[A-Za-z0-9_]*PATH=' | sort | uniq";
      diffia = "envia | covia | diff - <(env | envia | covia)";
      difnia = "nenvia | diff - <(env | nenvia)";
      wget = "wget -c ";
      imacs = ''
        emacs -T "¤ INFO" -f info-standalone --eval="(load-theme (intern \"wombat\"))"'';
      wordcat = "tee >(xargs -n1 cat) | wc -w";
      gkcfg = "git stack --dump-config -";
      gkg = "git stack";
      gksy = "git stack sync";
      gpga = "gpg --armor";
      gpgk = "gpg-connect-agent killagent /bye";
      gpguptty = "gpg-connect-agent updatestartuptty /bye";
      gpgrel = "gpg-connect-agent reloadagent /bye";
      hw = "hwinfo --short";
      iotopa = "iotop -oa";
      grubup = "sudo update-grub";
      dfs = "df -h | tail -n+2 | sort -nk5";
      dfsh = "df -sh | tail -n+2 | sort -nk5";
      tarnow = "tar -acf ";
      untar = "tar -zxvf ";
      ps_nice = "ps axo pid,comm,nice,cls --sort=-nice";
      ps_pri = "ps -eo pri k +pri h | uniq -c";
      psid = "ps -opid,uid,command h";
      psmem = "ps auxf | sort -nr -k 4";
      psmem10 = "ps auxf | sort -nr -k 4 | head -10";
      psnice = "ps -o pid,comm,nice";
      pspri =
        "ps -eo pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm k pri";
      ptrgb = "pstree -C age -pT";
      treef = "tree --prune -aP";
      jqr = "jq -r ";
      jqrj = "jq -rj ";
      # nodenpm_lsparse = "npm ls -g --parseable | grep node_modules | sed -e '\\''s/.*node_modules\\///g'\\''";
      tyxy =
        "tidy --quiet yes --tidy-mark no --vertical-space yes -indent -xml";
      jctlu = "journalctl --user -u";
      jctlb = "journalctl -p 3 -xb";
      sysu = "systemctl --user";
      sysdpath = "systemd-path system-shared";
      sysupath = "systemd-path user-shared";
    };

    # for f in "$HOME/.nix-profile/share"/gh/f/bash/*; do
    #  if [ -f "$f" ]; then source $f; fi; done
    interactiveShellInit = ''
      if [ "$TERM" != "dumb" ]; then
          #export CURSOR_BOX=$(echo -e '\001\033[\017

          export RESTORE=$(echo -en '\001\033[0m\002')
          export STANDOUT=$(echo -en '\001\033[00;44;37m\002')
          export RED=$(echo -en '\001\033[00;31m\002')
          export GREEN=$(echo -en '\001\033[00;32m\002')
          export YELLOW=$(echo -en '\001\033[00;33m\002')
          export BLUE=$(echo -en '\001\033[00;34m\002')
          export MAGENTA=$(echo -en '\001\033[00;35m\002')
          export PURPLE=$(echo -en '\001\033[00;35m\002')
          export CYAN=$(echo -en '\001\033[00;36m\002')
          export LIGHTGRAY=$(echo -en '\001\033[00;37m\002')
          export LRED=$(echo -en '\001\033[01;31m\002')
          export LGREEN=$(echo -en '\001\033[01;32m\002')
          export LYELLOW=$(echo -en '\001\033[01;33m\002')
          export LBLUE=$(echo -en '\001\033[01;34m\002')
          export LMAGENTA=$(echo -en '\001\033[01;35m\002')
          export LPURPLE=$(echo -en '\001\033[01;35m\002')
          export LCYAN=$(echo -en '\001\033[01;36m\002')
          export WHITE=$(echo -en '\001\033[01;37m\002')
      else
          export RESTORE=""
          export STANDOUT=""
          export RED=""
          export GREEN=""
          export YELLOW=""
          export BLUE=""
          export MAGENTA=""
          export PURPLE=""
          export CYAN=""
          export LIGHTGRAY=""
          export LRED=""
          export LGREEN=""
          export LYELLOW=""
          export LBLUE=""
          export LMAGENTA=""
          export LPURPLE=""
          export LCYAN=""
          export WHITE=""
      fi

      export LESS_TERMCAP_mb="''${LRED}"
      export LESS_TERMCAP_md="''${LRED}"
      export LESS_TERMCAP_me="''${RESTORE}"
      export LESS_TERMCAP_se="''${RESTORE}"
      export LESS_TERMCAP_so="''${STANDOUT}"
      #export LESS_TERMCAP_so=$'\E[00;44;37m'
      export LESS_TERMCAP_ue="''${RESTORE}"
      export LESS_TERMCAP_us="''${LGREEN}"

      # fix for less as MANPAGER
      # https://bbs.archlinux.org/viewtopic.php?id=287185
      export GROFF_NO_SGR=1
    '';
  };
}

# programs.bash.initExtra = mkIf cfg.enableCompletion (mkOrder 100 ''
#   if [[ ! -v BASH_COMPLETION_VERSINFO ]]; then
#     . "${pkgs.bash-completion}/etc/profile.d/bash_completion.sh"
#   fi
# '');

# home.file.".profile".source = writeBashScript "profile" ''
#   . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
#   ${sessionVarsStr}
#   ${cfg.profileExtra}
# '';

# home.file.".bashrc".source = writeBashScript "bashrc" ''
#   ${cfg.bashrcExtra}
#   # Commands that should be applied only for interactive shells.
#   [[ $- == *i* ]] || return
#   ${historyControlStr}
#   ${shoptsStr}
#   ${aliasesStr}
#   ${cfg.initExtra}
# '';
