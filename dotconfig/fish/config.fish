fish_add_path ~/.local/bin
fish_add_path npm-packages/bin
fish_add_path ~/.cabal/bin
# fish_add_path ~/.gem/ruby/2.5.0/bin
fish_add_path $HOME/go/bin
fish_add_path ~/.cargo/bin
# fish_add_path ~/flutter/bin
fish_add_path ~/projects/flutter/bin
# fish_add_path /opt/flutter/bin
# fish_add_path ~/projects/projectdo
fish_add_path ~/scripts

# environment variables
# set -x CHROME_EXECUTABLE chromium
set -x EDITOR nvim
set -x BAT_THEME OneHalfLight
set -x LANG en_US.UTF-8

# simple aliases
alias cat='bat'
alias l='exa'
alias nn='cd ~/Sync/notes; nvim'
alias lvim='NVIM_APPNAME=lazyvim nvim'

# projectdo setup
abbr -a b --function projectdo_build
abbr -a r --function projectdo_run
abbr -a t --function projectdo_test
abbr -a p --function projectdo_tool

function get_editor
    echo $EDITOR
end
abbr -a e --function get_editor

function last_history_item
    echo $history[1]
end
abbr -a !! --position anywhere --function last_history_item

function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function multicd
abbr -a g git
abbr -a gs 'git status'
abbr -a gc --set-cursor "git commit -m '%'"

# begin
    # set --local AUTOJUMP_PATH /usr/share/autojump/autojump.fish
    # if test -e $AUTOJUMP_PATH
        # source $AUTOJUMP_PATH
    # end
# end

# Emit an OSC-133;A (\E]133;A\E\\) sequence before each prompt to make jumping between prompts work in Foot
function mark_prompt_start --on-event fish_prompt
    echo -en "\e]133;A\e\\"
end

# # A very simple prompt
if set -q fish_private_mode
    # A simple terminal in private mode that does not show pwd, etc.
    function fish_prompt -d "Write out the prompt"
        # printf '%s%s%s> ' (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
        printf '%s%s%s> ' (set_color $fish_color_cwd) (path basename $PWD) (set_color normal)
    end
end

# opam configuration
source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# iTerm2 shell integration
# source ~/.iterm2_shell_integration.(basename $SHELL)

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /Users/simon/.ghcup/bin # ghcup-env

# fzf functions

function fzf_checkout;
  set branch (git --no-pager branch -vv | fzf | string sub -s 3 | string split " " -f  1)
  echo "git checkout $branch"
end

abbr -a fch --function fzf_checkout
