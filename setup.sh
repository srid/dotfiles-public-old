# Shell script to setup symlinks in $HOME to the dotfiles here
# This script is meant to be run on new machines with newly created user
# account.

function symlink {
    target=$1
    link=$2
    echo "symlink: $link => $target"
    rm $link
    ln -sf $target $link
}

for dotfile in .emacs .bashrc-custom.sh .fonts .gitconfig .hgrc .screenrc \
               .xbindkeysrc .zshrc .vimrc .buildout .sources .tmux.conf;
do
    symlink `pwd`/$dotfile ~/$dotfile;
done

# on mac, something needs to be done
if [ `uname` == "Darwin" ]; then
    echo
    echo
    echo "!!!!!! READ THIS !!!!!"
    echo
    echo "You're on a Mac."
    echo "Add '. ~/.bashrc-custom.sh' in ~/.profile"
fi

echo "Read this script to see programs to install"

# apt-get install emacs-snapshot-gtk tmux zsh git-core wmctrl \
#   build-essential python-dev ack-grep colordiff multitail tig \
#   msttcorefonts

# brew install ...
