;;; init.el --- An emacs configuration by jameslintaylor


;;
;; ## Setup
;;

(setq emacs-start-time (current-time))

(eval-when-compile
  (let ((default-directory "~/.emacs.d/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/alabaster-theme/")
  (require 'use-package)
  (require 'better-defaults))

(use-package package
  :custom
  (package-archives '(("org" . "https://orgmode.org/elpa/")
                      ("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/")))
  :init
  (package-initialize))

;;
;; ## Evil
;;

(use-package evil
  :delight defining-kbd-macro '(:eval (concat " @" (make-string 1 evil-this-macro)))

  :custom
  (evil-insert-state-cursor '((hbar . 2) "black"))
  (evil-motion-state-cursor '(box "black"))
  (evil-normal-state-cursor '(box "black"))
  (evil-emacs-state-cursor '(box "#f40"))
  (evil-move-cursor-back nil)

  :init
  (evil-mode t))

(use-package evil-visualstar
  :init
  (global-evil-visualstar-mode t))

;;
;; ## Hydra
;;

(use-package hydra)

;;
;; ## Ivy + Counsel + Swiper
;;

(use-package ivy
  :init
  (ivy-mode t))

(use-package counsel
  :init
  (counsel-mode t)
  :bind (("C-x C-r" . counsel-recentf)))

(use-package swiper
  :custom
  (swiper-goto-start-of-match t)
  :bind (:map evil-motion-state-map
              ("/" . swiper)
              ("?" . swiper-backward)))

(use-package ivy-hydra)

;;
;; ## Magit
;;

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-c g s" . magit-status)
   ("C-c g c" . magit-clone)))

;;
;; ## Lisp
;;

(defvar generic-lisp-mode-hook nil)
(add-hook 'lisp-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))
(add-hook 'clojure-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))

(use-package paren
  :hook (generic-lisp-mode . show-paren-mode))

(use-package lispy
  :custom
  (lispy-compat '(edebug cider))
  :hook (generic-lisp-mode . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(mark-toggle
                              additional-movement)))

(use-package rainbow-delimiters
  :hook (generic-lisp-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :config
  (define-clojure-indent
    (try-req 0)
    (context 2)
    (POST 2)
    (ex/if-any-fails 1)
    (ex/tfn 1)
    (match-some 1)
    (pair-repr-json-ok 2)))

(use-package cider
  :load-path "site-lisp/cider"
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-in-current-window t)
  (cider-inject-dependencies-at-jack-in t)
  :config
  (define-clojure-indent
    (are-responses 1))
  (evil-define-key '(normal visual) 'cider-mode-map
    "gd" 'cider-find-var
    "gh" 'magit-status))

;;
;; ## Swift
;;

(use-package swift-mode)

;;
;; ## Projectile
;;

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :init
  (projectile-mode 1)

  :config
  (defun my/projectile-ag (search-term &optional arg)
    "Like `projectile-ag` but preserves the prefix argument
semantics of `ag`"
    (interactive
     (list (projectile--read-search-string-with-default "Ag search for")
           current-prefix-arg))
    (if (require 'ag nil 'noerror)
        (let ((ag-ignore-list (delq nil
                                    (delete-dups
                                     (append
                                      ag-ignore-list
                                      (projectile--globally-ignored-file-suffixes-glob)
                                      ;; ag supports git ignore files directly
                                      (unless (eq (projectile-project-vcs) 'git)
                                        (append (projectile-ignored-files-rel)
                                                (projectile-ignored-directories-rel)
                                                grep-find-ignored-files
                                                grep-find-ignored-directories
                                                '())))))))
          (funcall 'ag search-term (projectile-project-root)))
      (error "Package 'ag' is not available")))

  (fset 'projectile-ag 'my/projectile-ag))

;;
;; ## Silver searcher
;;

(use-package ag
  :custom
  (ag-group-matches nil))

(use-package winnow
  :hook
  (compilation-mode . winnow-mode))

;;
;; ## Eshell
;;

(use-package eshell
  :init
  (setq eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(use-package rust-mode)

;;
;; ## Everything else
;;

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hl-line
  :init
  (global-hl-line-mode))

(use-package ace-window
  :bind
  (("C-x C-o" . ace-window)
   ("C-x 4 C-t" . ace-swap-window)
   ("C-x 4 0" . ace-delete-window)
   ("C-x 4 1" . ace-delete-other-windows)))

(use-package recentf
  :custom
  (recentf-max-saved-items 150)
  :init
  (recentf-mode 1))

(use-package winner
  :bind
  (("C-x 4 u" . winner-undo)
   ("C-x 4 U" . winner-redo))
  :init
  (winner-mode 1))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (evil-motion-state-modes (cons 'dired-mode evil-motion-state-modes))
  :bind (("C-x D" . (lambda ()
                      (interactive)
                      (dired (file-name-directory buffer-file-name)))))
  :config
  (evil-define-key 'motion dired-mode-map (kbd "<return>") 'dired-find-file)
  (evil-define-key 'motion dired-mode-map (kbd "^") 'dired-up-directory))

(use-package docker
  :bind
  (("C-c d" . docker)))

(use-package ibuffer
  :custom (ibuffer-expert t)
  :bind (("C-x C-b" . ibuffer)))

(use-package hippie-exp
  :custom
  ;; try-expand-line and try-expand-list are often problematic in that
  ;; they introduce unbalanced expressions.
  (hippie-expand-try-functions-list
   (seq-reduce (lambda (a x) (remove x a))
               '(try-expand-line
                 try-expand-list)
               hippie-expand-try-functions-list))
  :bind (("C-/" . hippie-expand)))

(defun my/toggle-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/transpose-windows (arg)
  (interactive "p")
  (let ((b1 (progn (other-window arg) (current-buffer)))
        (b2 (progn (other-window (- 0 arg)) (current-buffer))))
    (switch-to-buffer b1)
    (other-window arg)
    (switch-to-buffer b2)))

(use-package repeat
  :bind (("C-x ." . repeat)))

(global-set-key (kbd "C-x B") 'my/toggle-buffer)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x 4 t") 'my/transpose-windows)

(global-set-key (kbd "M-c") 'completion-at-point)
(global-set-key (kbd "M-e") 'hippie-expand)

;; Scratch buffer

(setq initial-scratch-message "\
;; You're using a silly laptop keyboard again?
(setq mac-command-modifier (quote meta))")

(setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))

(use-package ruler-mode
  :hook (find-file . ruler-mode))

(use-package simple
  :custom
  (column-number-mode t))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(setq js-indent-level 2)

;; Fira Code
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Theme
(load-theme 'alabaster t)

;;; Customize file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
