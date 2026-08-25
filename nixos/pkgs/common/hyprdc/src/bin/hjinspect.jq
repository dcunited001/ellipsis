#!/usr/bin/env -S jq -rf

"Title: \(.title) (\(.initialTitle))
Class: \(.class) (\(.initialClass))
Workspace \(.workspace.id) (\(.workspace.name))
On Monitor \(.monitor)
(\(.size | @text)) @ (\(.at | @text))"
