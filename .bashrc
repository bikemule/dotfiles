# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Add maven path to PATH.
# TODO: Find something like virtualenv for Java
export PATH=$PATH:$HOME/src/apache-maven-3.3.9/bin

# Homebrew bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

alias ll="ls -lAh"
alias moon="curl wttr\.in/Moon"  # Get the current phase of the moon
alias weather="curl wttr\.in"    # Get weather w/ auto geolocation
