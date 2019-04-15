# Lines configured by zsh-newuser-install
# ssh zsh fix
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/djwilcox/.zshrc'

# add custom completion scripts
fpath=(~/.zsh/completion $fpath) 

autoload -Uz compinit
compinit
# End of lines added by compinstall

# editor
#=======

# set emacslient as editor
ALTERNATE_EDITOR=""; export ALTERNATE_EDITOR
EDITOR="/usr/local/bin/emacsclient"; export EDITOR
VISUAL="/usr/local/bin/emacsclient -c -a emacs"; export VISUAL

# emacsclient function e
function e {
/usr/local/bin/emacsclient "$@"
}

# directories
#============

# home bin 
if [ -d "$HOME/bin" ]; then
   PATH="$HOME/bin:$PATH"
fi

# home local python bin 
if [ -d "$HOME/.local/bin" ]; then
   PATH="$HOME/.local/bin:$PATH"
fi

# git prompt
if [ -f "/usr/local/share/git-core/contrib/completion/git-prompt.sh" ]; then
   source "/usr/local/share/git-core/contrib/completion/git-prompt.sh"
fi

# prompt
setopt prompt_subst
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWUPSTREAM="auto verbose name git"

PS1=$'[%n@%M %~]\nYes Master ? '
RPROMPT=$'%F{cyan}$(__git_ps1 "%s")%f'

# general
#========

# ranger dont load default
RANGER_LOAD_DEFAULT_RC="FALSE"

# mpd socket
export MPD_HOST=${HOME}/.mpd/socket

# tell ls to be colourfull
export LSCOLORS=ExFxCxDxBxegedabagacad
export CLICOLOR=1

# qt5 
export QT_QPA_PLATFORMTHEME=qt5ct

# zstyle
#=======

# Set/unset  shell options
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# kill - red, green, blue
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=22=31=34'

# list optiones colour, white + cyan
zstyle ':completion:*:options' list-colors '=(#b) #(-[a-zA-Z0-9,]#)*(-- *)=36=37'

# rehash commands
zstyle ':completion:*' rehash true

# cdpath
setopt auto_cd
#cdpath=($HOME)
cdpath=(~)

# aliases
#========

# hdmi display on - and reset wallpaper
alias hdmi-on='xrandr --output eDP-1 --auto --primary --output HDMI-1 --mode 1920x1080 --right-of eDP-1 && ~/.fehbg &>/dev/null'

# hdmi display off - and reset wallpaper
alias hdmi-off='xrandr --output eDP-1 --auto --primary --output HDMI-1 --off && ~/.fehbg &>/dev/null'

# keyboard backlight on
alias flame-on='sysctl dev.asmc.0.light.control:255'

# keyboard backlight off
alias flame-off='sysctl dev.asmc.0.light.control:0'

# syntax highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[precommand]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[arg0]=fg=cyan
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
ZSH_HIGHLIGHT_PATTERNS=('rm -rf *' 'fg=white,bold,bg=red')
