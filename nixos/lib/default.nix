{ lib, ... }: {
  # from EmergentMind/dotfiles
  relativeToRoot = lib.path.append ../.;
  pow = lib.fix (self: base: power:
    if power != 0 then base * (self base (power - 1)) else 1);
}
