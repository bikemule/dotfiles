# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Add maven path to PATH.
# TODO: Find something like virtualenv for Java
#export PATH=$PATH:$HOME/src/apache-maven-3.3.9/bin  # Don't really use this anymore

# Homebrew bash completion
# new version of Homebrew + $PS1 test from github.com/scop/bash-completion
[[ $PS1 && -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion

# old version
if which brew &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# TODO: add tests for standard other locations of bash-completion dirs

# TODO: add test for docker install
# Docker setup
eval `docker-machine env 2>/dev/null`

# Fix paging inside Emacs
if [[ $INSIDE_EMACS ]]; then
    export PAGER="/bin/cat"
fi

# Aliases
alias ll="ls -lAh"
alias moon="curl wttr\.in/Moon"  # Get the current phase of the moon
alias weather="curl wttr\.in"    # Get weather w/ auto geolocation
