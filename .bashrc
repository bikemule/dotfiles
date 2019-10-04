# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Add maven path to PATH.
# TODO: Find something like virtualenv for Java
#export PATH=$PATH:$HOME/src/apache-maven-3.3.9/bin  # Don't really use this anymore

# Homebrew bash completion
# new version of Homebrew + $PS1 test from github.com/scop/bash-completion
[[ $PS1 && -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion

# old version
# if which brew &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
#    . $(brew --prefix)/etc/bash_completion
# fi

# TODO: add tests for standard other locations of bash-completion dirs

# Docker setup
if command -v docker-machine 1>/dev/null 2>&1; then
    eval `docker-machine env 2>/dev/null`
fi

# Fix paging inside Emacs
if [[ $INSIDE_EMACS ]]; then
    export PAGER="/bin/cat"
fi

# Aliases
alias ll="ls -lAh"
alias moon="curl wttr\.in/Moon"  # Get the current phase of the moon
alias weather="curl wttr\.in"    # Get weather w/ auto geolocation
alias ec="emacsclient -nc"       # Open an actual window for emacs and don't wait
alias g="git"                    # Shortcut for git


# makes pyenv-virtualenvwrapper
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# pyenv-virtualenv plugin
if which pyenv-virtualenv-init > /dev/null; then
    eval "$(pyenv virtualenv-init -)";
fi
