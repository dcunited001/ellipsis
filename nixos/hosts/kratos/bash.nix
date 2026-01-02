{ lib, pkgs, ... }:
{
  programs.bash = {
    completion.enable = true;

    ### alacritty1 -----------------------
    #
    # nix shell carla
    # t=$(mktemp -d)
    # env > $t/nixshell.env
    # covia $t/nixshell.env         # pathtr colon-delmited values
    # echo $t | wl-copy

    ### alacritty2 -----------------------
    #
    # t=$(wl-paste)
    # env > $t/env
    # covia $t/env
    # cat $t/env | envia            # lines matching .*PATH=
    # cat $t/env | nenvia           # not matching .*PATH=
    # cat $t/nixshell.env | diffia  # diffing .*PATH= matches
    # cat $t/nixshell.env | difnia  # diffing .*PATH= non-matches

    shellAliases = {
      pathtr = ''tr ":" "\n"'';
      shitbin = ''echo -e "\033c"'';
      envia = "grep -e '^[A-Za-z0-9_]*PATH=' | sort | uniq";
      covia = ''sed -e "s/:/\n:\t/g"'';
      nenvia = "grep -ve '^[A-Za-z0-9_]*PATH=' | sort | uniq";
      diffia = "envia | covia | diff - <(env | envia | covia)";
      difnia = "nenvia | diff - <(env | nenvia)";
      bashbinds = ''bind -p | grep -ve '^#' | tr -d ':\"' | sed -e 's/\\e/M /g' | sed -e 's/\\C-/C /g' | grep -v 'self-insert' | sort -k2'';
      bashbindtree = ''bind -P | cut -f1 -d' ' | tr '-' '/' | tree --noreport --fromfile .'';
      wget = "wget -c ";
      hcx = "hyprctl";
      hcbinds = ''hyprctl binds | tr -d "\n" | sed -e "s/bind/\nbind/g"'';
      hcbdoops = ''hcbinds | sort | cut -f2-5 | uniq -c | grep -vE "^\s+1"'';
      imacs = ''emacs -T "¤ INFO" -f info-standalone --eval="(load-theme (intern \"wombat\"))"'';
      wordcat = "tee >(xargs -n1 cat) | wc -w";
      gkcfg = "git stack --dump-config -";
      gkg = "git stack";
      gksy = "git stack sync";
      gpga = "gpg --armor";
      gpgk = "gpg-connect-agent killagent /bye";
      gpguptty = "gpg-connect-agent updatestartuptty /bye";
      gpgrel = "gpg-connect-agent reloadagent /bye";
      hw = "hwinfo --short";
      diffusb = "diff <(lsusb) <(sleep 3; lsusb)";
      iotopa = "iotop -oa";
      grubup = "sudo update-grub";
      dfs = "df -h | tail -n+2 | sort -nk5";
      dfsh = "df -sh | tail -n+2 | sort -nk5";
      tarnow = "tar -acf ";
      untar = "tar -zxvf ";
      ps_nice = "ps -o pid,ppid,uname,cls,nice,pri,rtprio,comm,args --sort=-nice";
      ps_pri = "ps -eo pri k +pri h | uniq -c";
      psid = "ps -opid,uid,command h";
      psmem = "ps auxf | sort -nr -k 4";
      psmem10 = "ps auxf | sort -nr -k 4 | head -10";
      psnice = "ps axo pid,ppid,uname,cls,nice,pri,rtprio,comm,args --sort=-nice";
      pspri = "ps -eo pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm k pri";
      psz = "ps axo s,pid,ppid,uname,comm,args | grep -e'^Z'";
      pssyuz = "ps axo s,pid,ppid,unit,ouid,uunit,slice,uname,comm,args | grep -e'^Z'";
      ptrgb = "pstree -C age -pT";
      treef = "tree --prune -aP";
      jqr = "jq -r ";
      jqrj = "jq -rj ";
      sqli = "rlwrap sqlite3";
      gxpl = "rlwap guix repl";
      # nodenpm_lsparse = "npm ls -g --parseable | grep node_modules | sed -e '\\''s/.*node_modules\\///g'\\''";
      tyxy = "tidy --quiet yes --tidy-mark no --vertical-space yes -indent -xml";
      jctl = "isd";
      jour = "isd";
      jctlu = "journalctl --user";
      jctlu7 = "journalctl --user -p7";
      jctlb = "journalctl -xb";
      jctlb7 = "journalctl -xb -p7";
      systat = "systemctl status";
      sycat = "SYSTEMD_COLORS=256 systemctl cat --no-pager";
      sysu = "isd";
      syu = "systemctl --user";
      syucat = "SYSTEMD_COLORS=256 systemctl cat --user --no-pager";
      syustat = "systemctl --user status";
      sysdpath = "systemd-path system-shared";
      sysupath = "systemd-path user-shared";
    };

    promptInit = ''
      export PS_PROMPT=""

      # TODO: use PROMPT_COMMAND and an array of commands mutating the state

      PS_GIT=""
      GIT_PS1_SHOWCOLORHINTS=1
      GIT_PS1_DESCRIBE_STYLE=branch
      GIT_PS1_SHOWUPSTREAM=name

      PS_PROMPT="''${LYELLOW}\A ''${LGREEN}\u''${RED}@''${LCYAN}\h ''${RED}:: ''${YELLOW}\w"
      if [ -n "$GUIX_ENVIRONMENT" ]; then
          PS_INFO="''${LMAGENTA}g''${RESTORE}"
      fi

      if [ "$TERM" = "dumb" ]; then
          PS1='$ '
      else
          PS_GIT='$(__git_ps1 "«%s»") '
          PS_INFO="$PS_GIT $PS_INFO"
          PS1="$PS_INFO \n$PS_PROMPT"

          # calc number of cols with $((COLUMNS -n ))
          if [ -n "$PS_GIT" ]; then
             PS1="$PS1"
          fi
          PS1="$PS1''${RED}$ ''${RESTORE}"
      fi
    '';

    # for f in "$HOME/.nix-profile/share"/gh/f/bash/*; do
    #  if [ -f "$f" ]; then source $f; fi; done
    interactiveShellInit = ''
      if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
        alias dir='dir --color=auto'
        alias egrep='egrep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias diff='diff --color=auto'
        alias grep='grep --color=auto'
        alias vdir='vdir --color=auto'
        # else
        # no color
      fi

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
