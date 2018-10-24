# hdmi display on
alias hdmi-on='xrandr --output eDP-1 --auto --primary --output HDMI-1 --mode 1920x1080 --right-of eDP-1'

# hdmi display off
alias hdmi-off='xrandr --output eDP-1 --auto --primary --output HDMI-1 --off'


# keyboard backlight on
alias flame-on='sysctl dev.asmc.0.light.control:255'

# keyboard backlight off
alias flame-off='sysctl dev.asmc.0.light.control:0'
