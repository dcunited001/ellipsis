xmodel="pc(pc105)"
xlayout=io
xrules=evdev
xvariant=""
xoptions=""

xkbcli compile-keymap \
       --model=$xmodel \
       --rules=$xrules \
       --layout=$xlayout \
       --variant=$xvariant \
       --options=$xoptions \
       --verbose &2>1 > /tmp/xkb/testio
