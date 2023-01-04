set PATH ~/.local/bin ~/.npm-packages/bin ~/.cabal/bin ~/.gem/ruby/2.5.0/bin $PATH
set PATH $HOME/go/bin $PATH

fish_add_path ~/.cargo/bin

set -x LANG en_US.UTF-8
alias cat='bat'
alias l='exa'
abbr -a g git
abbr -a gs 'git status'

set -x EDITOR nvim
set -x BAT_THEME OneHalfLight

# function fish_prompt
  # powerline-shell --shell bare $status
# end

# begin
    # set --local AUTOJUMP_PATH /usr/share/autojump/autojump.fish
    # if test -e $AUTOJUMP_PATH
        # source $AUTOJUMP_PATH
    # end
# end

# opam configuration
source /Users/simon/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# iTerm2 shell integration
source ~/.iterm2_shell_integration.(basename $SHELL)

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /Users/simon/.ghcup/bin # ghcup-env
