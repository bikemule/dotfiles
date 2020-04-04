;;; package --- Not really a package
;;; emacs.d

;;; Commentary:

;;; Not sure where I got the idea for these package headers or why I only put
;;; these here.

;;; Code:


;; For calculating load time
;; This and end code copied from notnarb/dotfiles who got it from jwiegley

(defconst emacs-start-time (current-time))

;; Turn on to debug loading errors, turn off at the end of file
(setq debug-on-error t)

;; Disable garbage collection during init for faster startup
(defconst krkn/init-gc-cons-threshold gc-cons-threshold)  ; Save initial gc limit
(setq gc-cons-threshold most-positive-fixnum)  ; Set to max

;; Package setup/init
;; ------------------

;; Mostly taken from:
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;; https://github.com/CachesToCaches/getting_started_with_use_package/blob/master/init-use-package.el

(require 'package)

(setq package-enable-at-startup nil)  ;; Don't load packages immediately
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Old archives
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Emacs > 27 will auto-initialize packages. This was causing a warning when evaluating Python code with Elpy
(unless package--initialized (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Necessary to use :diminish and :bind with use-package
(use-package diminish
  :defer t
  :ensure t)
(use-package bind-key
  :defer t
  :ensure t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))


;; Ideas from https://github.com/kpurdon/.emacs.d/ and https://realpython.com/blog/python/emacs-the-best-python-editor/

;; Basic customization
;; -------------------

;; Emacs internal config
;; ---------------------

;; For server mode
;; (unless (server-running-p) (server-start))


;; Not that into desktop mode anymore
;; Desktop
;; (desktop-save-mode 1)  ; Save windows/buffers

(show-paren-mode 1)  ; highlight matching parens
(column-number-mode 1)  ; show col # in mode line
(setq visible-bell t)

;; Get rid of extraneous UI
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Autosave files in Emacs dir rather than cluttering the file's folder
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Backup files
(setq
     backup-by-copying t      ; don't clobber symlinks
     ;; Save all backup files in this directory.
     backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t        ; use versioned backups
)

;; Visuals
;; -------


;; For fun
(use-package nyan-mode
  :defer t
  :ensure t
  :config
  (nyan-mode))

;; TODO: Doesn't seem to be working with Dockerized emacs
;; Theme 
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; Make URLs clickable, Emacs built-in func
(add-hook 'after-init-hook
	  (lambda () (goto-address-mode)))

;; Git
;; ---

(use-package git-gutter
  :ensure t
  :commands global-git-gutter-mode
  :config
  ;; Hide git-gutter when there are no changes
  (setq git-gutter:hide-gutter t)
  (setq git-gutter:lighter " GG")
  (global-git-gutter-mode t))


;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ido
(use-package ido
  :ensure t
  :init
  (setq
    ido-enable-flex-matching t  ; Allows matching of any chars in any order
    ido-use-filename-at-point 'guess
    ido-use-url-at-point 'guess
    ido-default-file-method 'selected-window)
  :config
  (ido-mode t)
  (ido-everywhere t))

;; Python
;; ------

;; Below was taken from http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
;; BEGIN RAKAN.ME
(use-package elpy
  :defer t
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :bind (:map elpy-mode-map
	      ("<M-left>" . nil)
	      ("<M-right>" . nil)
	      ("<M-S-left>" . elpy-nav-indent-shift-left)
	      ("<M-S-right>" . elpy-nav-indent-shift-right)
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark))
  :config
  ;; Set RPC Python to that of project
  ;; This requires setting current pyenv, which should?? be done by pyenv-init
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-rpc-backend "jedi")

  ;; From https://github.com/aiguofer/dotfiles/blob/master/user/.emacs.d/init.el
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
					; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 2))

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))

(use-package pyenv-mode
  :defer t
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(add-hook 'after-init-hook 'pyenv-init)
;; END RAKAN.ME

;; From https://github.com/aiguofer/dotfiles/blob/master/user/.emacs.d/init.el
;; BEGIN aiguiofer
(use-package blacken
  :defer t
  :ensure t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '88))
;; END aguiofer

;; Package to jump to last change in a buffer
(use-package goto-last-change
  :ensure t)

;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; Tried debugging for hours to get this work and keep getting this error on every save.
;; /usr/local/Cellar/emacs/HEAD-4ad214f/Emacs.app/Contents/MacOS/Emacs: /usr/local/bin/autopep8: No such file or directory

;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Old Python stuff
;; ----------------

;; I have a feeling this wasn't being used or was being overriden by elpy
;; Not sure why this is first, but maybe it needs to be?
;;(setq standard-indent 1)  ; more python tab fixing

;; fix tabbing for python
(defun my-pystuff ()
  (flycheck-mode)
  (which-function-mode)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil
        py-smart-indentation nil))

; (add-hook 'python-mode-hook 'my-pystuff)

(use-package flycheck
  :ensure t)

(use-package restclient
  :ensure t
  :defer t)

;; smex
;; ------

;; Useful smex commands
;; C-h f, while Smex is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command. (Via where-is.)

(use-package smex
  :defer t
  :ensure t
  :commands (smex smex-major-mode-commands execute-extended-command)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ; Normal M-x
	 ("C-c C-c M-x" . execute-extended-command)))


;; Old, probably useless stuff
;; ---------------------------

; Jedi
;(autoload 'jedi:setup "jedi" nil t)
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

;; yasnippet
;(yas-global-mode 1)


;(require 'golden-ratio)
;(setq golden-ratio-exclude-modes '("Speedbar-mode"))

;(require 'sr-speedbar)
;(setq sr-speedbar-skip-other-window-p 1)

; To fix annoyingly fast scrolling w/ touchpad on OS X
; From http://www.emacswiki.org/emacs/SmoothScrolling
; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; Store customizations in a separate file
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

;; Cleanup
;; -------

(setq gc-cons-threshold krkn/init-gc-cons-threshold)  ; Restore garbage collection

(setq debug-on-error nil)  ;; Turn off debugging

;; Output load time, taken mostly from jwiegley
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed
		    (float-time (time-subtract (current-time) emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed))))
