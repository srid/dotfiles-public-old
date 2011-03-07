# Auto-screen invocation. see: http://taint.org/wk/RemoteLoginAutoScreen
# if we're coming from a remote SSH connection, in an interactive session
# then automatically put us into a screen(1) session.   Only try once
# -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
# if screen fails for some reason.
if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" != x ]
then
  STARTED_SCREEN=1 ; export STARTED_SCREEN
  [ -d $HOME/lib/screen-logs ] || mkdir -p $HOME/lib/screen-logs
  sleep 1
  screen -RR && exit 0
  # normally, execution of this rc script ends here...
  echo "Screen failed! continuing with normal bash startup"
fi
# [end of auto-screen snippet]



# *****************
# My customizations 
# *****************

# Aliases
function e {
    # open in emacs
    PATH=/Applications/Emacs.app/Contents/MacOS/bin/:$PATH emacsclient -n $*
}
function e2 {
    # open in emacs - readonly
    FILE=$(cd $(dirname "$1") && pwd)/$(basename "$1"); PATH=/Applications/Emacs.app/Contents/MacOS/bin/:$PATH emacsclient -n --eval "(find-file-read-only \"$FILE\")";
}

function run_cmd {
  echo "> $1"
  $1
}

if [ `uname` == "Darwin" ]; then
  export APY26DIR=/Library/Frameworks/Python.framework/Versions/2.6
  export APY27DIR=/Library/Frameworks/Python.framework/Versions/2.7
  export APY31DIR=/Library/Frameworks/Python.framework/Versions/3.1
else
  export APY25DIR=/opt/ActivePython-2.5
  export APY26DIR=/opt/ActivePython-2.6
  export APY27DIR=/opt/ActivePython-2.7
  export APY31DIR=/opt/ActivePython-3.1
fi

export APYDIR=$APY27DIR
export APY3DIR=$APY31DIR
alias apy=$APYDIR/bin/python
alias apy2.6=$APY26DIR/bin/python
alias apy3=$APY3DIR/bin/python3
function apy_as_root {
  run_cmd "sudo $APYDIR/bin/python $*"
}
function apyez {
  run_cmd "sudo $APYDIR/bin/easy_install $*"
}

# http://chris-lamb.co.uk/2010/04/22/locating-source-any-python-module/
cdp () {
  cd "$(python -c "import os.path as _, ${1}; \
    print _.dirname(_.realpath(${1}.__file__[:-1]))"
  )"
}

# GAE aliases
function gae_run {
    run_cmd "$APYDIR/bin/python ~/src/google_appengine/dev_appserver.py --datastore_path=~/.$1-dev-datastore ."
}

function gae_update {
    rum_cmd "$APYDIR/bin/python /home/srid/src/google_appengine/appcfg.py update ."
}

# dev helpers
function dif {
    test -d .hg
    if [ $? = 0 ]; then
        VC='hg'
    else
        test -d .svn
        if [ $? = 0 ]; then
            VC='svn'
	else
	    test -d .git
	    if [ $? = 0 ]; then
                VC='git'
	    else
                echo "cannot find VC type"; return 1
	    fi
        fi
    fi
    $VC diff $* | colordiff | less -R
}
function pyg {
    pygmentize $1 | less -R
}
function K {
    killall -KILL $1
}
function hgu {
    hg pull -u
}
function hguu {
    hgu && hg heads
}
function head1 {
    head -n 1 $1
}

mkdir -p ~/.virtualenvs
# http://bitbucket.org/dhellmann/virtualenvwrapper/issue/45 -- . ~/.local/bin/virtualenvwrapper.sh
mkdir -p ~/.buildout-downloads
mkdir -p ~/.buildout-eggs


# PEP 370
export PATH=~/Library/Python/2.7/bin:~/.local/bin:/usr/local/bin:$PATH

function restart_xbindkeys {
  killall xbindkeys; xbindkeys;
}

if [ -f ~/.keychain/${HOSTNAME}-sh ]
then
  . ~/.keychain/${HOSTNAME}-sh
  ssh-add ~/.ssh/*.id_rsa
fi


if [ `uname` == "Darwin" ]; then
  alias ldd='otool -L'
  alias ls='ls -G'
  export PS1="\u@\H:\w > "
  # Komodo
  alias k='open -a "Komodo IDE"'
  alias v='open -a MacVIM'
else
  export PATH=$APYDIR/bin:$APY26DIR/bin:$APY3DIR/bin:$APY25DIR/bin:$PATH
  alias ls='ls --color'
  xbindkeys
  function e {
      url="http://elephant:8998/edit?host=`hostname`&path=`readlink -f $*`"
      echo $url
      curl $url
  }
fi

# git
function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
export PS1='\u@\h \[\033[1;33m\]\w\[\033[0m\]$(parse_git_branch)$ '


export PATH=$PATH:$HOME/as/mk/bin

GREP_OPTIONS="--exclude-dir=.svn"


alias sux='svn up --ignore-externals'
alias suxx='svn up --ignore-externals && svn st --ignore-externals'

. ~/Dropbox/dotfiles/z.sh


rex=/net/nas/data/languages/pypm/sites/rex
