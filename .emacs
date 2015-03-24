(setq visible-bell t)

;Emacs 24 color themes
(load-theme 'deeper-blue)

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

(require 'flycheck)
;; Python

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

; PHP

(add-to-list 'auto-mode-alist '("\\.ctp\\.php\\'" . web-mode))


; The following two functions are from http://truongtx.me/2014/07/22/setup-php-development-environment-in-emacs/

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
  (web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  (flycheck-select-checker my-php)
  (flycheck-mode t))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(require 'sr-speedbar)

(require 'nyan-mode)
(nyan-mode)

; To fix annoyingly fast scrolling w/ touchpad on OS X
; From http://www.emacswiki.org/emacs/SmoothScrolling
; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
