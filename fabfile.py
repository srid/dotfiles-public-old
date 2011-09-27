# pypm install fabric

import os.path as P
from fabric.api import *

import pypm
from applib import sh


def emacs_update():
    """Update emacs dependencies
    """
    with sh.cd(P.join('emacs', 'external')):
        local('wget -N http://www.emacswiki.org/emacs/download/anything.el')
        local('wget -N http://www.emacswiki.org/emacs/download/anything-config.el')
        local('wget -N http://www.emacswiki.org/emacs/download/anything-match-plugin.el')

        local('wget -N http://www.emacswiki.org/emacs/download/workspaces.el')
        local('wget -N http://jblevins.org/projects/markdown-mode/markdown-mode.el')
        local('wget -N https://github.com/overtone/live-coding-emacs/raw/master/lib/blackbored.el')
        local('wget -N http://adamspiers.org/computing/elisp/smooth-scrolling.el')
    

up = emacs_update
