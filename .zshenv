# ~/.zshenv

# Path
typeset -U PATH path
path=("$HOME/bin" "/usr/local/bin" "$path[@]")
export PATH

# xdg directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# XDG_RUNTIME_DIR automatically set to /var/run/xdg/djwilcox

# firefox
export MOZ_ENABLE_WAYLAND=1
# need for firefox 123.01,2
#export MOZ_DISABLE_WAYLAND_PROXY=1

# qt5
export QT_QPA_PLATFORMTHEME=qt5ct

# ssh-add
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# less
export LESSHISTFILE="${XDG_CONFIG_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"

# set emacsclient as editor
#export ALTERNATE_EDITOR=""
#export EDITOR="emacsclient -a emacs"
#export VISUAL="emacsclient -a emacs"

# tell ls to be colourfull
#export LSCOLORS=ExFxCxDxBxegedabagacad
#export CLICOLOR=1

# vi mode
export KEYTIMEOUT=1

# mpd host variable for mpc
export MPD_HOST="/home/djwilcox/.config/mpd/socket"

# dark theme needed for handbrake
export GTK_THEME=Adwaita-dark:dark
