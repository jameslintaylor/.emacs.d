;;
;; # An emacs configuration by jameslintaylor
;;

;; 
;; ## Setup
;; 

(setq emacs-start-time (current-time))

(eval-when-compile
  (let ((default-directory "~/.emacs.d/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/alabaster-theme/")
  (require 'use-package)
  (require 'better-defaults)
  ;; the above sets ido-mode, I am trying out ivy.
  (ido-mode nil))

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
  (evil-mode 1)

  :config
  ;; minibuffer needs some special love
  (add-hook 'minibuffer-setup-hook
            '(lambda ()
               (set (make-local-variable 'evil-echo-state) nil)
               (evil-emacs-state 1))))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

;;
;; ## Ivy
;;

(use-package ivy
  :init
  (ivy-mode t))

(use-package counsel
  :init
  (counsel-mode t))

(use-package swiper
  :custom
  (swiper-goto-start-of-match t)
  :bind
  (:map evil-motion-state-map
        ("/" . swiper)
        ("?" . swiper-backward)))

;;
;; ## Hydra
;;

(use-package hydra)

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
  :hook (generic-lisp-mode . lispy-mode)
  :custom
  (lispy-compat '(edebug cider))
  :config
  (use-package lispyville
    :hook (lispy-mode . lispyville-mode)
    :config
    (lispyville-set-key-theme '(mark-toggle
                                additional-movement))))

;;
;; ## Everything else
;;

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :bind (("C-x D" . (lambda ()
                      (interactive)
                      (dired (file-name-directory buffer-file-name))))))

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

(use-package yasnippet
  :delight yas-minor-mode
  :bind (("C-M-/" . yas-expand))
  :custom
  (yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode 1))

(use-package hl-line
  :init
  (global-hl-line-mode))

(use-package recentf
  :bind
  (("C-x C-r" . my/recentf-find-file)
   ("C-x 4 C-r" . my/recentf-find-file-other-window))
  :custom
  (recentf-max-saved-items 150)
  :init
  (recentf-mode 1)
  (defun my/recentf-find-file ()
    "Use `completing-read' to find a recent file."
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  (defun my/recentf-find-file-other-window ()
    "Use `completing-read' to find a recent file."
    (interactive)
    (if (find-file-other-window (completing-read "Find recent file (other-window): " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))

(use-package undo-tree
  :bind
  (:map undo-tree-map
        ("C-r" . nil)))

(use-package docker
  :bind
  (("C-c d" . docker)))

(use-package ace-window
  :bind
  (("C-x C-o" . ace-window)
   ("C-x 4 C-t" . ace-swap-window)
   ("C-x 4 0" . ace-delete-window)
   ("C-x 4 1" . ace-delete-other-windows)))

(use-package rainbow-delimiters
  :hook (generic-lisp-mode . rainbow-delimiters-mode))

(use-package cider
  :load-path "site-lisp/cider"
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-in-current-window t)
  :config
  (evil-define-key '(normal visual) 'cider-mode-map
    "gd" 'cider-find-var
    "gh" 'magit-status))

(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (checked-context 'defun)
    (if-any-fails 'defun)
    (as-> 'macro)
    (defconfig 'macro)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

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

(use-package ag
  :custom
  (ag-group-matches nil))

(use-package winnow
  :hook
  (compilation-mode . winnow-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package winner
  :bind
  (("C-x 4 u" . winner-undo)
   ("C-x 4 U" . winner-redo))
  :init
  (winner-mode 1))

(use-package eshell
  :init
  (setq eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(add-hook 'eshell-mode-hook
          (lambda ()

            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")

            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                        "/bin/ls")))
              (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

(use-package rust-mode)

;;; needs to find a home

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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-x $") 'eshell-here)

(global-set-key (kbd "C-x .") 'repeat)
(global-set-key (kbd "C-x B") 'my/toggle-buffer)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x 4 t") 'my/transpose-windows)
(global-set-key (kbd "M-c") 'completion-at-point)
(global-set-key (kbd "M-e") 'hippie-expand)

;; Fira Code ligatures
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Scratch buffer

(setq initial-scratch-message "\
;; You're using a silly laptop keyboard again?
(setq mac-command-modifier (quote meta))")

(setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))

;; Theme
(load-theme 'alabaster t)

;;; Customize file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
