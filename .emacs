;;; package --- Not really a package
;;; emacs.d

;;; Commentary:

;;; Not sure where I got the idea for these package headers or why I only put
;;; these here.

;;; Code:

;; Turn on if there are any errors. Doesn't need to be on.
;; (setq debug-on-error t)

;; Install Packages
;; ----------------

;; This was an attempt to get py-autopep8 working. Can't remember if it worked?
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))


;; Ideas from https://github.com/kpurdon/.emacs.d/ and https://realpython.com/blog/python/emacs-the-best-python-editor/

(package-initialize)

;; List of packages to install/are required to run this file
(defvar my-packages
  '(academic-phrases
    better-defaults
    hc-zenburn-theme
    elpy
    flycheck
    js2-mode
    json-mode
    magit
    markdown-mode
    markdown-preview-mode
    py-autopep8
    smart-mode-line
    web-mode
    nyan-mode
    exec-path-from-shell
    sr-speedbar))

;; Makes sure packages are up to date?
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if they are not installed.
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Basic customization
;; -------------------

;; Emacs internal config
;; ---------------------

;; For server mode
;; (unless (server-running-p) (server-start))

;; Desktop
(desktop-save-mode 1)  ; Save windows/buffers

(show-paren-mode 1)			; highlight matching parens
(column-number-mode 1)  ; show col # in mode line
(setq visible-bell t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Autosave files in Emacs dir rather than cluttering the file's folder
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Backup files
(setq
     version-control t
     backup-by-copying t      ; don't clobber symlinks
     ;; Save all backup files in this directory.
     backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t        ; use versioned backups
)

;; For fun
(require 'nyan-mode)
(nyan-mode)

(add-hook 'after-init-hook
	  (lambda () (load-theme 'hc-zenburn t)))


;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ido
(require 'ido)
(ido-mode t)
(setq
    ido-enable-flex-matching t  ; Allows matching of any chars in any order
    ido-use-filename-at-point 'guess
    ido-use-url-at-point 'guess
    ido-default-file-method 'selected-window
)

;; Python
;; ------

(elpy-enable)
; (elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; TODO: Doesn't seem to install autopep8 by default, causing an error on any save.
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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

;; PHP - Ew. Old, too
;; ------------------

; The following two functions are from http://truongtx.me/2014/07/22/setup-php-development-environment-in-emacs/
(require 'flycheck)
(flycheck-define-checker my-php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode web-mode))

(defun my-setup-php ()
  ;; enable web mode
  ;; Disabled because it loses a lot of useful funcitonality:
  ;;   Can't go to function end/beginnning, goes to opening/closing tag instead.

  ;(web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  ;(flycheck-select-checker my-php)
  (flycheck-mode t))


;(add-hook 'php-mode-hook 'my-setup-php)

; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))  ; Cake template files

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sr-speedbar academic-phrases exec-path-from-shell jedi nyan-mode web-mode smart-mode-line py-autopep8 markdown-preview-mode magit json-mode js2-mode hc-zenburn-theme flycheck elpy better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
