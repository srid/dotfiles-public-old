import os.path as P
from fabric.api import *

from applib import sh


def emacs_update():
    """Update emacs dependencies
    """
    with sh.cd(P.join('emacs', 'external')):
        local('wget -N http://www.emacswiki.org/emacs/download/anything.el')
        local('wget -N http://www.emacswiki.org/emacs/download/anything-config.el')
        local('wget -N http://www.emacswiki.org/emacs/download/anything-match-plugin.el')

        local('wget -N http://www.emacswiki.org/emacs/download/workspaces.el')
    

