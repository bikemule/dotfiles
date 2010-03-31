; load Pymacs and other vendor dir extensions
; Does this work?
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))


;; color-theme
;;;;;;;;;;;;;;;;;;

;; This is the location on Ubuntu
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/color-theme.el")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-charcoal-black)


;; ido
;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq 
    ido-enable-flex-matching t		; Allows matching of any chars in any order
    ido-use-filename-at-point t
    ido-use-url-at-point t
)

;; Chris Poyzer's modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq standard-indent 1)		; more python tab fixing

; fix tabing for python
(defun my-pystuff ()
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil
        py-smart-indentation nil))

(add-hook 'python-mode-hook 'my-pystuff)

;; Misc additional stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For server mode
(server-start)

;; Desktop
(desktop-save-mode 1)

(show-paren-mode 1)			; highlight matching parens

(require 'pycomplete)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))

;; Ropemacs - May be able to put this in the autoload list above
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")


(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)

(require 'auto-install)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
