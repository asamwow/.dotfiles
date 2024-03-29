#######################################################
####### Anarchy ZSH configuration file    #######
#######################################################

### Set/unset ZSH options
#########################
# setopt NOHUP
# setopt NOTIFY
# setopt NO_FLOW_CONTROL
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt APPEND_HISTORY
# setopt AUTO_LIST
# setopt AUTO_REMOVE_SLASH
# setopt AUTO_RESUME
unsetopt BG_NICE
setopt CORRECT
setopt EXTENDED_HISTORY
# setopt HASH_CMDS
setopt MENUCOMPLETE
setopt ALL_EXPORT
setopt histignorespace

### Set/unset  shell options
############################
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash
unsetopt nomatch

### Autoload zsh modules when they are referenced
#################################################
autoload -U history-search-end
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
#zmodload -ap zsh/mapfile mapfile
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

### Set variables
#################
PATH="/usr/local/bin:/usr/local/sbin/:$HOME/.emacs.d/bin/:$HOME/.snowsql:$PATH"
HISTFILE=$HOME/.zhistory
HISTSIZE=1000
SAVEHIST=1000
HOSTNAME="`hostname`"
LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
PASSWORD_STORE_DIR=$HOME/.password-store
EDITOR="emacs -nw"
ASPNETCORE_ENVIRONMENT="Development"
DOTNET_ROOT="$HOME/.dotnet"
DOTNET_BASE="${DOTNET_ROOT}/sdk/2.2.402/"
PATH="$DOTNET_ROOT:$HOME/.dotnet/tools:$PATH"
XDG_CONFIG_HOME="$HOME/.config"

### Load colors
###############
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
   colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
   eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
   eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
   (( count = $count + 1 ))
done

### Set prompt
##############
PR_NO_COLOR="%{$terminfo[sgr0]%}"
# PS1="[%(!.${PR_RED}%n.$PR_LIGHT_YELLOW%n)%(!.${PR_LIGHT_YELLOW}@.$PR_RED@)$PR_NO_COLOR%(!.${PR_LIGHT_RED}%U%m%u.${PR_LIGHT_GREEN}%U%m%u)$PR_NO_COLOR:%(!.${PR_RED}%2c.${PR_BLUE}%2c)$PR_NO_COLOR]%(?..[${PR_LIGHT_RED}%?$PR_NO_COLOR])%(!.${PR_LIGHT_RED}#.${PR_LIGHT_GREEN}$) "
PS1="%(!.${PR_RED}%n.$PR_LIGHT_YELLOW)%(!.${PR_LIGHT_YELLOW}.$PR_RED)$PR_NO_COLOR%(!.${PR_LIGHT_RED}%U%m%u.${PR_LIGHT_GREEN})$PR_NO_COLOR%(!.${PR_RED}%2c.${PR_BLUE}%2c)$PR_NO_COLOR%(?..${PR_LIGHT_RED}$PR_NO_COLOR)%(!.${PR_LIGHT_RED}#.${PR_LIGHT_GREEN}) "
#RPS1="$PR_LIGHT_YELLOW(%D{%m-%d %H:%M})$PR_NO_COLOR"
unsetopt ALL_EXPORT

### Set alias
#############
alias ll='ls -al'
alias ls='ls --color=auto '
alias ec='emacs -nw'
alias hearthstone='env WINEPREFIX="$HOME/.wine" /usr/bin/wine C:\\\\windows\\\\command\\\\start.exe /Unix $HOME/.wine/dosdevices/c:/users/Public/Desktop/Hearthstone.lnk'
alias set-cpu-frequency='sudo cpupower frequency-set -f 1.40GHz'
alias drivesync='ruby drivesync/drivesync.rb'
alias pac='sudo pacman'
alias sys='sudo systemctl'
alias syu='systemctl --user'
alias zshrc='emacs -nw ~/.zshrc'
alias brightness='xbacklight -set'
alias pipes='pipes -f 20 -r 2000 -B -t 1'
alias lock='xscreensaver-command -lock'
alias search='grep -rnwiI'
alias gl='git log --oneline -n 10'
alias remove-all-docker-containers='sudo docker container rm -f `sudo docker container list -aq`'
alias remove-all-docker-images='sudo docker image rm -f `sudo docker image list -aq`'
alias gitpush="git push -u origin HEAD"
alias gitpushf="git push -u origin HEAD --force-with-lease"
alias gitsubmodules="git submodule update --init --recursive"
alias gs="git status"
alias gd="git diff"
alias gr="git reset --hard HEAD"
alias grl="git reset --hard 'HEAD^'"
alias ghead="git diff \"HEAD^\" HEAD"
alias gb="git branch"
alias gfor="git submodule foreach"
alias copy="xclip -sel c"
alias smake="cd ~ && make"
alias gsearch="git grep --recurse-submodules"
alias s="./say.py"
alias taler="~/deployment/bin/WIP/taler-local"

### Bind keys
#############
autoload -U compinit
compinit
bindkey '^?'      backward-delete-char
bindkey '^[OH'    beginning-of-line
bindkey '^[OF'    end-of-line
bindkey '^[[5~'   up-line-or-history
bindkey '^[[6~'   down-line-or-history
bindkey '^[[A'    history-beginning-search-backward-end
bindkey '^[[B'    history-beginning-search-forward-end
bindkey '^r'      history-incremental-search-backward
bindkey ' '       magic-space    # also do history expansion on space
bindkey '^I'      complete-word # complete on tab, leave expansion to _expand
bindkey '\e[3~'   delete-char
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey '^H'      backward-delete-word
bindkey '^['      delete-word
bindkey '^X'      kill-region
bindkey '^V'      yank
bindkey '^[[3;5~' delete-word
bindkey '^[[3;3~' delete-word

### GPG AGENT
#############
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
export GNUPGHOME="$HOME/.gnupg"

# Completion Styles
###################
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'

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

# command for process lists, the local web server details and host completion
# on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au$USER'

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}')
# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
        named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs avahi-autoipd\
        avahi backup messagebus beagleindex debian-tor dhcp dnsmasq fetchmail\
        firebird gnats haldaemon hplip irc klog list man cupsys postfix\
        proxy syslog www-data mldonkey sys snort
# SSH Completion
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

source ~/stratagem.sh

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
