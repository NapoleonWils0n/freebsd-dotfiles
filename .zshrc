#===============================================================================
# ~/.zshrc
#===============================================================================

#===============================================================================
# ssh zsh fix
#===============================================================================

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return


#===============================================================================
# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
#===============================================================================

HISTSIZE=1000


#===============================================================================
# variables for PS3 prompt
#===============================================================================

newline=$'\n'
yesmaster='Yes Master ? '


#===============================================================================
# source git-prompt.sh
#===============================================================================

# Load bash compatibility so we can use the git-prompt function
autoload -Uz bashcompinit && bashcompinit

# Point to the actual function script provided by the git package
if [ -f /usr/local/share/git-core/contrib/completion/git-prompt.sh ]; then
    source /usr/local/share/git-core/contrib/completion/git-prompt.sh
fi

#===============================================================================
# export git status options
#===============================================================================

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS=true


#===============================================================================
# PS3 prompt function
#===============================================================================

function zle-line-init zle-keymap-select {
    # 1. Determine the mode string based on Zsh's internal KEYMAP variable
    local MODE_INDICATOR
    case $KEYMAP in
        vicmd)
            MODE_INDICATOR="[n] " # Normal Mode Indicator + a space
            ;;
        viins|main)
            MODE_INDICATOR="[i] " # Insert Mode Indicator + a space
            ;;
        *)
            MODE_INDICATOR="" # Fallback
            ;;
    esac

    # 2. Rebuild the PS1, inserting the MODE_INDICATOR on the second line
    PS1="[%n@%M %~]$(__git_ps1 "(%s) ")${newline}${MODE_INDICATOR}${yesmaster}"

    zle reset-prompt
}


#===============================================================================
# run PS3 prompt function
#===============================================================================

zle -N zle-line-init
zle -N zle-keymap-select


#===============================================================================
# set terminal window title to program name
#===============================================================================

case $TERM in
  (*xterm* | xterm-256color)
    function precmd {
      print -Pn "\e]0;%(1j,%j job%(2j|s|); ,)%~\a"
    }
    function preexec {
      printf "\033]0;%s\a" "$1"
    }
  ;;
esac


#===============================================================================
# Fix bugs when switching modes
#===============================================================================

bindkey -v # vi mode
bindkey "^?" backward-delete-char
bindkey "^u" backward-kill-line
bindkey "^a" beginning-of-line
bindkey "^e" end-of-line
bindkey "^k" kill-line


#===============================================================================
# Use modern completion system
#===============================================================================

autoload -Uz compinit
compinit


#===============================================================================
# Set/unset  shell options
#===============================================================================

setopt notify globdots pushdtohome cdablevars autolist
setopt recexact longlistjobs
setopt autoresume histignoredups pushdsilent noclobber
setopt autopushd pushdminus extendedglob rcquotes mailwarning
setopt histignorealldups sharehistory
cdpath=($HOME)
unsetopt bgnice autoparamslash


#===============================================================================
# Completion Styles
#===============================================================================

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

#eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro' '.hidden'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# kill - red, green, blue
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=22=31=34'

# list optiones colour, white + cyan
zstyle ':completion:*:options' list-colors '=(#b) #(-[a-zA-Z0-9,]#)*(-- *)=36=37'

# zsh autocompletion for sudo and doas
zstyle ":completion:*:(sudo|su|doas):*" command-path /usr/local/bin /usr/sbin /home/djwilcox/bin

# rehash commands
zstyle ':completion:*' rehash true


#===============================================================================
# highlighting
#===============================================================================

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[precommand]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[arg0]=fg=cyan
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
ZSH_HIGHLIGHT_PATTERNS=('rm -rf *' 'fg=white,bold,bg=red')


#===============================================================================
# Edit and Auto-Execute Command Line in External Editor (CTRL-X CTRL-E)
#===============================================================================

# Load the 'edit-command-line' function from the Zsh distribution
# -U: Ignore aliases for this function; -z: Use zsh-style autoloading
autoload -Uz edit-command-line

# Register 'edit-command-line' as a ZLE widget so it can be bound to keys
zle -N edit-command-line

# Define a wrapper function to both open the editor and execute the line on exit
edit-and-execute-command() {
  # Call the built-in widget to open the buffer in $EDITOR (Emacs)
  zle edit-command-line
  
  # Once the editor closes, 'accept' the line to execute it immediately
  zle accept-line
}

# Register the wrapper function as a new ZLE widget
zle -N edit-and-execute-command

# Map the shortcut for different Zsh modes:
# 1. Standard/Main keymap
bindkey '^X^E' edit-and-execute-command

# 2. Vi-Insert mode (while typing, indicated by your [i] prompt)
bindkey -M viins '^X^E' edit-and-execute-command

# 3. Vi-Command mode (after hitting Escape, indicated by your [n] prompt)
bindkey -M vicmd '^X^E' edit-and-execute-command


#===============================================================================
# aliases
#===============================================================================

# keyboard backlight on
alias backlight-on='sysctl dev.asmc.0.light.control:255'

# keyboard backlight off
alias backlight-off='sysctl dev.asmc.0.light.control:0'
