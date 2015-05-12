;;; package --- Not really a package

;;; Commentary:

;;; Code:
(setq visible-bell t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;Emacs 24 color themes
(load-theme 'deeper-blue)

(setq debug-on-error t)

;; MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; ido

(require 'ido)
(ido-mode t)
(setq
    ido-enable-flex-matching t		; Allows matching of any chars in any order
    ido-use-filename-at-point 'guess
    ido-use-url-at-point 'guess
    ido-default-file-method 'selected-window
)

;; Chris Poyzer's Python modifications

(setq standard-indent 1)		; more python tab fixing

; fix tabbing for python
(defun my-pystuff ()
  (flycheck-mode)
  (which-function-mode)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil
        py-smart-indentation nil))

(add-hook 'python-mode-hook 'my-pystuff)

;; For server mode
(server-start)

;; Desktop
(desktop-save-mode 1)

(show-paren-mode 1)			; highlight matching parens

; show col # in mode line
(column-number-mode 1)

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

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")

;; Python
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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


(add-hook 'php-mode-hook 'my-setup-php)

; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))  ; Cake template files

; Jedi
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; yasnippet
;(yas-global-mode 1)

(nyan-mode)

(add-hook 'after-init-hook 
	  (lambda () (load-theme 'hc-zenburn t)))

(require 'golden-ratio)
(setq golden-ratio-exclude-modes '("Speedbar-mode"))

(require 'sr-speedbar)
(setq sr-speedbar-skip-other-window-p 1)

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
 '(flycheck-display-errors-delay 0.05)
 '(flycheck-python-flake8-executable "/usr/local/bin/flake8")
 '(flycheck-python-pylint-executable "/usr/local/bin/pylint")
 '(nyan-bar-length 12))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
