{ ... }: {
  # zerotierone.joinNetworks = ['0123456789abcdef']
  #
  # - this isn't secret, per-se ... but it could ID your network(s)
  # - instead, manually join using zerotier-cli join $ztNetwork
  # - zt.joinNetworks also doesn't leave networks
  # - if the network is private, approve the node in the UI

  services.zerotierone = { enable = true; };

  # TODO: test delaying the start by adding a systemd target
  # services.systemd.services
}
