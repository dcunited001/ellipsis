{ pkgs, ... }:
{
  # https://ryanseipp.com/post/nixos-secure-ssh/
  # ensure it binds to a specific ip/interface (not 0.0.0.0)
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      X11Forwarding = false;
      PermitRootLogin = "no";

      Ciphers = ["chacha20-poly1305@openssh.com" "aes256-gcm@openssh.com"];
      Macs = ["hmac-sha2-512-etm@openssh.com" "hmac-sha2-256-etm@openssh.com"];
      KexAlgorithms = ["curve25519-sha256@libssh.org" "diffie-hellman-group-exchange-sha256"];

      # HostKeyAlgorithms = ["rsa-sha2-256" "rsa-sha2-512"
      #                      "ecdsa-sha2-nistp521" "ecdsa-sha2-nistp384" "ecdsa-sha2-nistp256"
      #                      "sk-ecdsa-sha2-nistp256@openssh.com"
      #                      "rsa-sha2-512-cert-v01@openssh.com" "rsa-sha2-256-cert-v01@openssh.com"
      #                      "ecdsa-sha2-nistp521-cert-v01@openssh.com" "ecdsa-sha2-nistp384-cert-v01@openssh.com" "ecdsa-sha2-nistp256-cert-v01@openssh.com"
      #                      "sk-ecdsa-sha2-nistp256-cert-v01@openssh.com"];
    };

    extraConfig = ''
ClientAliveCountMax 0
ClientAliveInterval 300

AllowTcpForwarding yes
# AllowAgentForwarding no
# MaxAuthTries 3
MaxSessions 4
# TCPKeepAlive no

HostKeyAlgorithms rsa-sha2-256,rsa-sha2-512,\
ecdsa-sha2-nistp521,ecdsa-sha2-nistp384,ecdsa-sha2-nistp256,\
sk-ecdsa-sha2-nistp256@openssh.com,\
rsa-sha2-512-cert-v01@openssh.com,rsa-sha2-256-cert-v01@openssh.com,\
ecdsa-sha2-nistp521-cert-v01@openssh.com,ecdsa-sha2-nistp384-cert-v01@openssh.com,ecdsa-sha2-nistp256-cert-v01@openssh.com,\
sk-ecdsa-sha2-nistp256-cert-v01@openssh.com
    '';
  };
}
