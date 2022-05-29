#!bash

KEYB="$(cat /proc/bus/input/devices | grep "MX Keys Mini Keyboard" -A 5 | grep -oE 'event[0-9]+')"
echo $KEYB
sed -i -E "s;input \(.+\);input (device-file \"/dev/input/$KEYB\");" ./mx-mini-colemak.kbd
sudo kmonad ~/src/dotfiles/mx-mini-colemak.kbd



