;; .emacs of srid
;; KISS (Keep It Simple Stupid)

;; Windows setup instructions:
;;  %HOME%> mklink .emacs "code\dotfiles\emacs\.emacs"

(let* ((emacsdir (concat (getenv "HOME")  "/code/dotfiles/emacs/"))
       (extdir   (concat emacsdir "external/"))
       (datadir  (concat emacsdir "data")))

  ;; PATHs
  (add-to-list 'load-path extdir)
  (setq osx (equal system-type 'darwin))
  (when osx
    (setenv "PATH"
            (mapconcat
             'identity
             (list "/usr/local/bin"
                   (expand-file-name "~/Library/Python/2.7/bin")
                   (getenv "PATH"))
             ":"))
    (push "/usr/local/bin" exec-path)
    (push (expand-file-name "~/Library/Python/2.7/bin") exec-path))

  ;; Basics
  (setq user-full-name "srid")
  (setq user-mail-address "emacs@srid.name")
  (setq inhibit-startup-message t)
  (setq backup-inhibited t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-default 'fill-column 80)
  (setq frame-title-format "emacs")

  (when (< emacs-major-version 24)
    (when
        (load
         (expand-file-name "~/.emacs.d/elpa/package.el"))))
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (defun kill-all-buffers ()
    "Kill all buffers."
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

  ;; C-x o C-x o ==> C-x p
  (defun other-other-window ()
    (interactive)
    (select-window (previous-window)))
  (global-set-key (kbd "C-x p") 'other-other-window)

  ;; buffer switch -- M-i and M-n
  (require 'anything)
  (require 'anything-config)
  (require 'anything-match-plugin)
  (global-set-key (kbd "M-i") 'anything)
  (setq anything-sources
        (list anything-c-source-bookmarks
              anything-c-source-buffers
              anything-c-source-recentf
              anything-c-source-file-name-history
              anything-c-source-files-in-current-dir
              anything-c-source-file-cache))
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (global-set-key (kbd "M-n") 'ido-switch-buffer)
  (require 'smex) ;; better M-x
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old

  ;; appearance
  (if osx
    (set-default-font "Consolas-14")
    (set-default-font "Consolas-12"))
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-deep-blue)
  (global-hl-line-mode 1)
  (require 'smooth-scrolling)
  
  ;; Programming
  (setq-default indent-tabs-mode nil)         ;; spaces, not tabs!
  (require 'yasnippet-bundle)                 ;; snippets
  (require 'uniquify)                         ;; duplicate buffer names --
  (setq uniquify-buffer-name-style 'forward)

  ;; major modes
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.ppatch$" . diff-mode))
  (require 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (require 'rainbow-delimiters)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  ;(setq-default frame-background-mode 'dark)

  ;; Peepopen
  (when osx
    (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
    (require 'textmate)
    (add-to-list 'load-path "~/.emacs.d/vendor/")
    (require 'peepopen)
    (textmate-mode)
    (setq ns-pop-up-frames nil))

  ;; setup for remote emacsclient runs
  (defun tcp-server-start ()
    (interactive)
    (setq server-use-tcp t)
    (let ((hostname (shell-command-to-string "hostname")))
      (setq server-host (replace-regexp-in-string "\n" "" hostname)))
    (server-force-delete)
    (server-start))

  ;; invoke this when switching loc context : work <-> home
  (defun reset-location ()
    (interactive)
    (kill-all-buffers)
    (tcp-server-start))
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying-when-linked t)
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(column-number-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnuserv-frame (quote gnuserv-main-frame-function))
 '(hfy-optimisations (quote (skip-refontification keep-overlays)))
 '(inferior-lisp-program "lein repl")
 '(inhibit-startup-screen t)
 '(javascript-indent-level 2)
 '(line-number-mode t)
 '(python-python-command "/usr/local/bin/python")
 '(rcirc-default-nick "srid")
 '(rcirc-default-user-name "srid")
 '(rcirc-server-alist (quote (("irc.mozilla.org" :nick "srid" :channels nil \#komodo nil) ("irc.freenode.net" :channels ("#emacs")))))
 '(rcirc-track-minor-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(visible-bell t)
 '(vm-mime-text/html-handler (quote lynx)))

(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
