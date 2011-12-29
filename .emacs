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

;; .local/bin to PATH
(setenv "PATH" (mapconcat 'identity (list (expand-file-name "~/.local/bin")
                                          (getenv "PATH")) ":"))

;; Alt+i
(global-set-key [?\M-i] 'ido-switch-buffer)

;; parentheses friendly
;; http://stackoverflow.com/questions/2413047/
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look like this
        "orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; repl synhl
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; Peepopen
(when osx
  (add-to-list 'load-path "~/.emacs.d/vendor/")
  (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
  (require 'textmate)
  (require 'peepopen)
  (textmate-mode)
  (setq ns-pop-up-frames nil))

;; Coffee Script indentation
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; Solarized theme
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;(require 'color-theme-solarized)
;(color-theme-solarized-light)
(if osx
  (set-default-font "Consolas-13")
  (set-default-font "Consolas-12"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
