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
         (expand-file-name "~/.emacs.d/elpa/package.el"))
      (package-initialize)))


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

  ;; hilight current line
  (global-hl-line-mode 1)
  ;; (light-symbol-mode)

  ;; C-x o C-x o ==> C-x p
  (defun other-other-window()
    (interactive)
    (select-window (previous-window)))
  (global-set-key (kbd "C-x p") 'other-other-window)

  ;; anything
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
              ;; XXX: info/man sources are undesirably slow
              ;; anything-c-source-info-pages
              ;; anything-c-source-man-pages
              anything-c-source-file-cache
              ;; anything-c-source-emacs-commands
              ))

  ;; ido
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (global-set-key (kbd "M-n") 'ido-switch-buffer)

  ;; Look and feel
  ;; =============
  (if (equal system-type 'darwin)
    (set-default-font "Consolas-14")
    (set-default-font "Consolas-12"))
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-deep-blue)

  
  ;; Programming
  ;; ===========
  ;; give me spaces
  (setq-default indent-tabs-mode nil)
  
  (require 'tramp)
  (setq tramp-default-method "plink")

  (require 'yasnippet-bundle)

  (add-to-list 'load-path (concat extdir "git-contrib-emacs"))
  (require 'git)

  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

  (add-to-list 'auto-mode-alist '("\\.ppatch$" . diff-mode))

  (require 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; Peepopen
  ;; ========
  (when (equal system-type 'darwin)
    (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
    (require 'textmate)
    (add-to-list 'load-path "~/.emacs.d/vendor/")
    (require 'peepopen)
    (textmate-mode)
    (setq ns-pop-up-frames nil))


  ;; remote editing
  (setq server-use-tcp t)
  (setq server-host
        (replace-regexp-in-string "\n" ""
                                  (shell-command-to-string "hostname")))
  (server-start))


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


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
