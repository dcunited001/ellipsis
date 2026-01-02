#!/usr/bin/env -S jq -rf

"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"
