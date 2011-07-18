# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="bira"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew github osx svn autojump)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

if [[ `hostname` =~ elephant(\..*)? ]]; then
    export PATH=$HOME/Library/Python/2.7/bin:$HOME/.local/bin:/usr/local/bin:$PATH
else
    export PATH=$HOME/.local/bin:/usr/local/bin:$PATH
fi

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

# ssh keys
keychain ~/.ssh/*id_rsa
[[ -f ~/.keychain/`hostname`-sh ]] && source ~/.keychain/`hostname`-sh

# emacs
if [[ `hostname` =~ elephant(\..*)? ]]; then
    function e {
        /Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
            -n \
            -f ~/.emacs.d/server/server $*
    }
else
    function e {
        # requires:
        #  scp ~/.emacs.d/server/server $hostname:
        F=${*:a}
	    F=${F/$HOME/\/Volumes\/`hostname`}
	    echo "Opening $F"
        emacsclient -n -f ~/server $F
    }
fi
