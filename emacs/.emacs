;; .emacs of srid
;; KISS (Keep It Simple Stupid)

;; Windows setup instructions:
;;  %HOME%> mklink .emacs "Dropbox\dotfiles\emacs\.emacs"

(let* ((emacsdir (concat (getenv "HOME")  "/Dropbox/dotfiles/emacs/"))
       (extdir   (concat emacsdir "external/"))
       (datadir  (concat emacsdir "data")))
  ;; Load paths
  (add-to-list 'load-path extdir)

  ; already in emacs24
  ;(when
  ;  (load
  ;   (expand-file-name "~/.emacs.d/elpa/package.el"))
  ;(package-initialize))

  ;; Personal information
  ;; ====================
  (setq user-full-name "srid")
  (setq user-mail-address "emacs@srid.name")


  ;; My default settings
  ;; ===================
  (setq inhibit-startup-message t)
  (setq backup-inhibited t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-default 'fill-column 80)
  (setq frame-title-format "emacs")

  ;; Window management
  ;; =================

  ;; M-x maximize-frame; M-x restore-frame
  (require 'maxframe) 
  (add-hook 'window-setup-hook 'maximize-frame t)

  ;; darkroom
  (require 'darkroom-mode)

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
  (global-set-key (kbd "M-i") 'anything)

  ;; ido
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (global-set-key (kbd "M-n") 'ido-switch-buffer)


  ;; Look and feel
  ;; =============
  (set-default-font "Consolas-14")

  (require 'color-theme)
  (load "color-theme-library.el")
  (color-theme-katester)


  
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

  ;; smart buffer names
  (defun smart-buffer-name ()
    "Rename the buffer to contain SERVER name"
    (let ((file-name (buffer-file-name)))
    (if (equal 0 (string-match "^/Volumes/\\([^/]+\\)/" file-name))
        (let ((serv-name (match-string 1 file-name)))
          (rename-buffer (concat "[" serv-name "] " (buffer-name)))))))

  (add-hook 'find-file-hook 'smart-buffer-name)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'nil)
  
  
  ;; IRC (not used)
  ;; ==============
  (eval-after-load 'rcirc '(require 'rcirc-color))

  (defun browse-url-default-macosx-browser (url &optional new-window)
    (interactive (browse-url-interactive-arg "URL: "))
    (if (and new-window (>= emacs-major-version 23))
        (ns-do-applescript
         (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
                 "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))

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
 '(inhibit-startup-screen t)
 '(javascript-indent-level 2)
 '(line-number-mode t)
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
