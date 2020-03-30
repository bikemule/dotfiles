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

;; Required for use-package
(eval-when-compile
  ;; The following commented lines are from the use-package README on Github,
  ;; seems unnecessary because the package dirs are autoloaded?
  
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;(add-to-list 'load-path "<path where use-package is installed>")

  (require 'use-package))

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

;; Emacs > 27 will auto-initialize packages. This was causing a warning when evaluating Python code with Elpy
(unless package--initialized (package-initialize t))

;; Makes sure list of package-list-packages is up to date
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if they are not installed.
;(dolist (p my-packages)
;  (unless (package-installed-p p)
;    (package-install p)))

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
(require 'nyan-mode)
(nyan-mode)

;; Theme 
(add-hook 'after-init-hook
	  (lambda () (load-theme 'hc-zenburn t)))


;; Make URLs clickable
(add-hook 'after-init-hook
	  (lambda () (goto-address-mode)))

;; Git
;; ---

(global-git-gutter-mode +1)

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ido
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq
    ido-enable-flex-matching t  ; Allows matching of any chars in any order
    ido-use-filename-at-point 'guess
    ido-use-url-at-point 'guess
    ido-default-file-method 'selected-window
)

;; Python
;; ------

;; Below was taken from http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
;; BEGIN RAKAN.ME
(use-package elpy
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
    (setq elpy-rpc-backend "jedi")

    ;; From https://github.com/aiguofer/dotfiles/blob/master/user/.emacs.d/init.el
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
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
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))
;; END aguiofer

;; Package to jump to last change in a buffer
(require 'goto-last-change)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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

(require 'restclient)

;; smex
;; ------

;; Useful smex commands
;; C-h f, while Smex is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command. (Via where-is.)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


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
   '(smex goto-last-change company-restclient blacken git-gutter use-package pyenv-mode sr-speedbar academic-phrases exec-path-from-shell jedi nyan-mode web-mode smart-mode-line py-autopep8 markdown-preview-mode magit json-mode js2-mode hc-zenburn-theme flycheck elpy better-defaults))
 ;; Hide git-gutter when there are no changes
 '(git-gutter:hide-gutter t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
