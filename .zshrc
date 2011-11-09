# History stuff
HISTFILE=~/.zsh_history
HISTSIZE=5000000
SAVEHIST=5000000
eval `dircolors -b`

# Variables
export EDITOR="vim"

setopt appendhistory
setopt autopushd pushdminus pushdsilent pushdtohome
setopt autocd
setopt cdablevars
setopt ignoreeof
setopt interactivecomments
setopt nobanghist
setopt noclobber
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SH_WORD_SPLIT
setopt nohup
setopt notify
setopt hist_ignore_all_dups
setopt extendedglob
# Allow for functions in the prompt.
setopt PROMPT_SUBST
unsetopt beep

PROMPT='%{${fg[green]}%}[%n@%m][\$ ' # default prompt
RPROMPT='][%{${fg[cyan]}%}%B%~%b$(prompt_git_info)%{${fg[default]}%} %T]'

typeset -gU path cdpath manpath fpath

# Enable auto-execution of functions.
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

autoload -Uz compinit promptinit colors 
compinit
promptinit 
colors

# Autoload zsh functions.
fpath=(~/.zsh/functions $fpath)
autoload -U ~/.zsh/functions/*(:t)

bindkey -e
bindkey ' ' magic-space # also do history expansion on space
typeset -g -A key
#bindkey '\e[3~' delete-char
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
#bindkey '\e[2~' overwrite-mode
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char 
# home/end for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey "^D" clear-and-exit 
bindkey . rationalise-dot

# Comp stuff
zmodload zsh/complist 

zstyle :compinstall filename '${HOME}/.zshrc'

zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(|.|..) ]] && reply=(..)'
zstyle ':completion:*:descriptions' format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %P Lines: %m
zstyle ':completion:*:corrections' format $'%{\e[0;31m%}%d (errors: %e)%}'

zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'
zstyle ':completion:*' file-sort name
zstyle ':completion:*' menu select=long
zstyle ':completion:*-case' menu select=5
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:expand:*' tag-order all-expansions
 
# allow approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric


# tab completion for PID
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'   force-list always

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:killall:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# cd not select parent dir.
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle ':completion:*' squeeze-slashes true

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache


# aliases
alias ls='ls -lha --color=always'
alias sl='ls -lha --color=always'
alias ll='ls -lha --color=always'
alias grep='grep -rni --color=always'
alias df='df -hT'
alias ping='ping -c 3'
alias rm="rm -v"
alias mv="mv -v"
alias top='htop'
alias mc='mc -d -t'
alias ps='ps faux'
alias du='du -sh'
alias free='free -m'
alias untbz2='tar -xjvf'
alias untgz='tar -xzvf'
alias brzinski='python2.7 -m SimpleHTTPServer'
alias mount='mount -vv'
alias gping='ping google.com'
alias myip="curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+'"
alias screencast="ffmpeg -f x11grab -r 25 -s 1280x800 -i :0.0 /tmp/screencast.mpg"
alias sshradioswap='ssh playahater@radioswap.net -p 2222'
alias rts1="mplayer mms://rts.videostreaming.rs/rts"
alias rts2="mplayer rtsp://helix.beotel.net/encoder/rts2.rm"
alias b92="mplayer http://stream.b92.net:7999/tv-b92.ogg"
alias hrt="mplayer rtsp://195.29.5.148/encoder/htv1.rm"
alias rtvpink="mplayer mms://beotelmedia.beotel.net/rtvpink"
alias twit.tv='mplayer http://bglive-a.bitgravity.com/twit/live/high'
alias webcam="mplayer -fps 24 tv://"
#alias webcam="mplayer tv:// -tv driver=v4l:width=352:height=288:device=/dev/video0 "
alias begraund='feh --bg-scale'
alias gb='git branch'
alias gba='git branch -a'
alias gc='git commit -v'
alias gl='git pull --rebase'
alias gp='git push'
alias gst='git status'
alias gpld='git pull origin develop'
alias gpsd='git push origin develop'
alias gplm='git pull origin master'
alias gpsm='git push origin master'
alias gpull='git pull origin'
alias gpush='git push origin'
alias git_diff='git diff --stat --color'
alias firefox='firefox -no-remote -no-remote -ProfileManager'
alias devping='ping 192.168.0.240'
#alias netbeans='netbeans --laf GTK'
alias alsamixer='alsamixer -c 0'
alias plantronics='bluez-simple-agent hci0 00:1C:EF:7E:A1:88'
alias pmplayer='mplayer -ao alsa:device=btheadset'
alias drushall='/usr/bin/php -d memory_limit=512M /home/play/stuff/drupal/drush/drush.php --php="/usr/bin/php -d memory_limit=512M" cc all'
alias drushcc='/usr/bin/php -d memory_limit=512M /home/play/stuff/drupal/drush/drush.php --php="/usr/bin/php -d memory_limit=512M" cc'
alias drush='/usr/bin/php -d memory_limit=512M /home/play/stuff/drupal/drush/drush.php --php="/usr/bin/php -d memory_limit=512M"'
alias git-diff-branch='git diff --stat --color master..staging'
#alias firedatabases="for database in ~/.mozilla/firefox/*/*.sqlite; do echo processing $database... ; sqlite3 $database 'VACUUM;'; done ;"
alias ssh240='ssh root@192.168.0.240 -p 40'
alias ssh242='ssh root@192.168.0.242 -p 40'
alias ssh246='ssh root@192.168.0.246'
alias linode='ssh root@178.79.129.19 -p 49752'

alias vpnserver='VBoxManage startvm --type headless sept'

alias tvfeed="curl 'http://api.dailytvtorrents.org/1.0/shows.getTextInfo?show_names=dexter,the-big-bang-theory,breaking-bad,eureka,merlin,the-mentalist,weeds,warehouse-13,house,fringe,burn-notice,top-gear,castle,game-of-thrones&colors=yes&links=yes'"


# custom completion commands
 
local _myhosts
_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

rmmodcomplete () { reply=(`/sbin/lsmod|cut -f1 -d" "|grep -v Module`) }
compctl -K rmmodcomplete rmmod
 
function insmodcomplete() { reply=(`find /lib/modules/$(uname -r)/* ! -type d -printf "%f\n"|sed "s/\.o$//"`) }
compctl -K insmodcomplete insmod modprobe
 
compile=(install clean remove uninstall deinstall)
compctl -k compile make
 
compctl -k '(up commit checkout update status)' svn
compctl -k ping telnet ncftp host nslookup irssi rlogin ftp
 
compctl -j -P '%' fg jobs disown
compctl -g '*.(gz|z|Z|t[agp]z|tarZ|tz)' + -g '*(-/)' gunzip gzcat zcat
compctl -g '*.(mp3|MP3|ogg|OGG|wav|WAV)' + -g '*(-/)' mpc mpg123 mpg321 xmms
compctl -g "*.html *.htm" + -g "*(-/) .*(-/)" + -H 0 '' remurl lynx links wget opera
compctl -g '*.(pdf|PDF)(ps|PS)' + -g '*(-/)' gv
compctl -g '*(-/)' + -g '.*(/)' cd chdir dirs pushd rmdir dircmp cl tree
compctl -g '*.(jpg|JPG|jpeg|JPEG|gif|GIF|png|PNG|bmp)' + -g '*(-/)' qiv gimp feh
compctl -g '*.tex*' + -g '*(-/)' {,la,gla,ams{la,},{g,}sli}tex texi2dvi
compctl -g '*.dvi' + -g '*(-/)' xdvi
compctl -g '[^.]*(-/) *.(c|C|cc|c++|cxx|cpp)' + -f cc CC c++ gcc g++

clear-and-exit() {
  zle kill-whole-line
  exit
}
zle -N clear-and-exit

rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot

# Append git functions needed for prompt.
preexec_functions+='preexec_update_git_vars'
precmd_functions+='precmd_update_git_vars'
chpwd_functions+='chpwd_update_git_vars'
