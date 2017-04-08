# hdmi display on
alias hdmi-on='xrandr --output eDP1 --auto --primary --output HDMI1 --mode 1920x1080 --right-of eDP1'

# hdmi display off
alias hdmi-off='xrandr --output eDP1 --auto --primary --output HDMI1 --off'


# keyboard backlight on
alias flame-on='sysctl dev.asmc.0.light.control:255'

# keyboard backlight off
alias flame-off='sysctl dev.asmc.0.light.control:0'
