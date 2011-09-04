# script to install often used programs on a newly installed Ubuntu machine
# .. split by various sections of priority

INSTALL="apt-get install -y "

# the essentials
$INSTALL emacs-snapshot-gtk tmux zsh git-core wmctrl

# dev setup
$INSTALL build-essential python-dev

# nice to have
$INSTALL ack-grep colordiff multitail tig

# multimedia
$INSTALL msttcorefonts


# TODO: automatically download and extract other programs?
# list -- komodo, thunderbird3, dropbox

