#!/etc/profiles/per-user/dc/bin/jq -rf

# TODO fix shebang for nix/guix
"Title: \(.title) (\(.initialTitle))
Class: \(.class) (\(.initialClass))
Workspace \(.workspace.id) (\(.workspace.name))
On Monitor \(.monitor)
(\(.size | @text)) @ (\(.at | @text))"
