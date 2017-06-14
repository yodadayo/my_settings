;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-

;; load-path settings
(add-to-list 'load-path "~/.emacs.d/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Package Management
;;;

;; Please configure the path to cask.el. Examples are following:
;; For Ubuntu
(require 'cask "~/.cask/cask.el")
;; For OSX
;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;;(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Global Settings
;;;

;; Keybindings
(global-set-key "\C-m" 'newline)
(global-set-key "\C-j" 'newline-and-indent)
(global-set-key "\C-h" 'delete-backward-char)
;; Disable Ctrl+Z for minimizing the window
(define-key global-map "\C-z" 'scroll-down)
;; Hide mouse cursor when you emacs
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)
;; load theme
(load-theme 'wombat)
;; line-spacing
(setq-default line-spacing 0.15)
;; column-number and line-number in status bar
(column-number-mode t)
(line-number-mode t)
;; clock in status bar
(display-time)
;; disable blinking cursor
(blink-cursor-mode nil)
;; disable menu-bar and tool-bar
(tool-bar-mode -1)
;; completion
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; disable startup message
(setq inhibit-startup-message t)
;; IME settings
(require 'fcitx)
(setq fcitx-use-dbus t)
(fset 'yes-or-no-p 'y-or-n-p)          ; (yes/no) to (y/n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal Functions
;;;

;;
;; Folding
;;

;; C coding style
(add-hook 'c-mode-hook
          '(lambda ()
	     (hs-minor-mode 1)))
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)))
;; Scheme coding style
(add-hook 'scheme-mode-hook
          '(lambda ()
	     (hs-minor-mode 1)))
;; Elisp coding style
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (hs-minor-mode 1)))
;; Lisp coding style
(add-hook 'lisp-mode-hook
          '(lambda ()
	     (hs-minor-mode 1)))
;; Python coding style
(add-hook 'python-mode-hook
          '(lambda ()
	     (hs-minor-mode 1)))
(define-key
  global-map
  (kbd "C-#") 'hs-toggle-hiding)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Settings for Each Package
;;;


;;
;; migemo
;;
(setq migemo-command "cmigemo")
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(fcitx-aggressive-setup)
(fcitx-isearch-turn-on)
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))
  
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  )


;; 
;; dired
;;
(require 'dired)
(define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd ")") 'dired-hide-details-mode)
;; enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)
;; 'RET' does not create a new buffer although 'a' creates
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)


;;
;; windows.el
;;
(require 'windows)
;; $B?75,$K%U%l!<%`$r:n$i$J$$(B
(setq win:use-frame nil)
(win:startup-with-window)
(define-key ctl-x-map "C" 'see-you-again)


;;
;; For C/C++-mode
;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


;;
;; For Javascript-mode
;;
(setq auto-mode-alist
      (append '(("\\.sjs$" . javascript-mode))
	      auto-mode-alist))


;;
;; expand-region
;; https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;
;; For eww
;;

(require 'eww)
(setq eww-search-prefix "http://www.google.co.jp/search?q=")

(defun eww-disable-images ()
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
(defun eww-enable-images ()
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))
(provide 'mylisp-eww-image)
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

(defvar eww-disable-colorize t)
(defun shr-colorize-region-disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region-disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region-disable)
(defun eww-disable-color ()
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))


;;
;; For YaTeX
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp")
;; $(I)/$(GZ/$AWS$,(B .tex $A$J$i(B yatex-mode $A$K(B
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX $A$,@{SC$9$kDZ2?%3%^%s%I$r6($(Gex$A$9$k(B
(setq tex-command "platex2pdf") ;; $AWTWw$7$?%3%^%s%I$r(B
(cond
  ((eq system-type 'gnu/linux) ;; GNU/Linux $A$J$i(B
    (setq dvi2-command "evince"))) ;; evince $A$G(B PDF $A$r$(IOURf(B
;;  ((eq system-type 'darwin) ;; Mac $A$J$i(B
;;    (setq dvi2-command "open -a Preview"))) ;; $A%W%l%S%e$B!<$A$G(B
(add-hook 'yatex-mode-hook '(lambda () (setq auto-fill-function nil)))
(add-hook 'yatex-mode-hook 'turn-on-reftex)
     
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))


;;
;; For emacs-mozc
;;
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)
(setq its-hira-period".")
(setq its-hira-comma",")

;; tab settings
(setq default-tab-width 2)

