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

for dotfile in .bashrc-custom.sh .fonts .gitconfig .hgrc .screenrc \
               .xbindkeysrc .zshrc .vimrc .buildout .sources;
do
    symlink `pwd`/$dotfile ~/$dotfile;
done


# symlink .emacs
symlink `pwd`/emacs/.emacs ~/.emacs

# write .freenode.el (without password)
if [ -a ~/.freenode.el ]; then
    echo 'retaining ~/.freenode.el'
    else
    echo 'writing ~/.freenode.el'
    echo '(setq passwd "")' > ~/.freenode.el
    echo 'setting perms ~/.freenode.el'
    chmod og-rwx ~/.freenode.el
fi


# on mac, something needs to be done
if [ `uname` == "Darwin" ]; then
    echo
    echo
    echo "!!!!!! READ THIS !!!!!"
    echo
    echo "You're on a Mac."
    echo "Add '. ~/.bashrc-custom.sh' in ~/.profile"
fi
