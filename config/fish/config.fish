set PATH /home/simon/.local/bin /home/simon/.npm-packages/bin /home/simon/.cabal/bin /home/simon/.gem/ruby/2.5.0/bin $PATH

alias b='upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "state|time\ to\ full|percentage"'
alias t='date'
alias sus='systemctl suspend'
alias shut='systemctl poweroff'
alias lu='light -U 10'
alias la='light -A 10'
abbr -a g git
abbr -a gs 'git status'

set -x ANDROID_HOME /home/simon/Android/Sdk
set -x EDITOR nvim
set -x PASSWORD_STORE_DIR /home/simon/Dropbox/password-store

# function fish_prompt
  # powerline-shell --shell bare $status
# end

# begin
    # set --local AUTOJUMP_PATH /usr/share/autojump/autojump.fish
    # if test -e $AUTOJUMP_PATH
        # source $AUTOJUMP_PATH
    # end
# end
