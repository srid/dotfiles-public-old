;; requires emacs 24 or greater
;; TODO:
;;  - uniquify

(setq user-full-name "Sridhar Ratnakumar")
(setq user-mail-address "emacs@srid.name")

(setq osx (equal system-type 'darwin))

;; More key bindings:
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/modules/starter-kit-bindings.el

;; Starter Kit prelude:
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; It's recommended to create a list of packages in .emacs which will
;; be installed if they are found to not be present:
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(starter-kit
    starter-kit-lisp starter-kit-ruby starter-kit-js starter-kit-bindings starter-kit-eshell
    yasnippet color-theme color-theme-solarized
    clojurescript-mode clojure-mode clojure-test-mode
    markdown-mode yaml-mode)
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; http://www.emacswiki.org/emacs/LayoutRestore
(require 'layout-restore)
(global-set-key [?\C-c ?l] 'layout-save-current)
(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)

;; parentheses friendly
;; http://stackoverflow.com/questions/2413047/
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look like this
        "orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; Peepopen
(when osx
  (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
  (require 'textmate)
  (add-to-list 'load-path "~/.emacs.d/vendor/")
  (require 'peepopen)
  (textmate-mode)
  (setq ns-pop-up-frames nil))

(require 'color-theme)
;; broken -- (color-theme-initialize)

