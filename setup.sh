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

echo "Read this script to see programs to install."
# sudo apt-get install tmux zsh git-core wmctrl build-essential python-dev ack-grep colordiff multitail tig  msttcorefonts autojump

# brew install ...
