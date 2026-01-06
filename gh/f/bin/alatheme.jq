#!/usr/bin/env -S tomlq -rf
. as $o | paths(type=="string") as $p | $o
  | [($p | join(".")), getpath($p)] | join("=")
