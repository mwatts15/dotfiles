#!/bin/sh

# I source my profile since that's where I mount my user-specified disks
[ -e $HOME/.profile ] && . $HOME/.profile

##- Variables -

if [ -f $HOME/.Xresources ] ; then
    xrdb -load .Xresources
fi
#export OOO_FORCE_DESKTOP=gnome
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=ibus

MY_TERM=my-term

#- X Server -
xset b 0
xset m 9/6 0
xscreensaver -no-splash &
autocutsel &

if [ -f $HOME/.Xresources ] ; then
    xrdb -load .Xresources
fi

gnome-keyring-daemon -s
if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
       eval `dbus-launch --sh-syntax --exit-with-session`
fi
# Connect to a wireless access point
#$HOME/bin/wifi_connect.sh --auto &

#ibus-daemon &

#- Desktop -
$HOME/.fehbg
#xcompmgr -n -o .1 -f -O .027 -D 2 &

##- Autostart Applications -
##firefox &
#sleep 2 && $MY_TERM -e irssi &
# show my notes on the last  desktop
#sleep 2 && keepnote &
# disable touchpad on when typing
xmodmap ~/.xmodmap
synclient VertScrollDelta=-112 VertEdgeScroll=1 HorizEdgeScroll=1 TouchpadOff=0 TapButton1=1 RTCornerButton=2
syndaemon -d -i .3 -K -R &
numlockx on 

vgareset
map-wacom-on-connect &
set-wacom-keys-on-connect &
# pretty notifications
#twmnd &
nohup battery_status.sh & 

WMNAME=xmonad

PATH=$PATH:$HOME/.cabal/bin
(sleep 2 && xmobar .xmobarrc_) &
xmobar_vol.sh Master &
xmms2-string.rb &
xmonoti &

OUT="/tmp/${USER}-dynamic-log-pipe"
mkfifo -m 0700 "$OUT" || echo "couldn't make the pipe!" >&2
(sleep 2 && xmobar < $OUT) &
exec $WMNAME > "$OUT"
