###############$
#singlehead external
#
#xrandr --output LVDS1 --off
#xrandr --output VGA1 --mode 1920x1200  #home
#xrandr --output VGA1 --mode 1920x1080  #office
#
################
#multihead
#
#xrandr --output VGA1 --primary
#xrandr --output VGA1 --mode 1920x1080
#xrandr --output LVDS1 --mode 1280x800 --left-of VGA1
#
################
#singlehead laptop
#
#xrandr --output VGA1 --off
#xrandr --output LVDS1 --mode 1280x800
#xrandr --output LVDS1 --primary
################
#feh --bg-scale http://onlyhdwallpapers.com/wallpaper/green_ship_black_vector_the_pirate_bay_piratebay_monotone_desktop_1280x800_wallpaper-105606.jpeg
###############$
exec xmonad
/usr/bin/start-pulseaudio-x11
