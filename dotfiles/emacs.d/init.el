;;;; PACKAGE LOADING

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")

(require 'use-package)
(require 'bind-key)

;;;; NO LITTERING (of emacs.d and working dir)

(use-package no-littering :load-path "site-lisp/no-littering")

(setq create-lockfiles nil)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;;; EMACS-LISP ENHANCEMENTS

;;; GC after 8MB
(setq gc-cons-threshold (* 8 1024 1024))

(use-package s :load-path "site-lisp/s.el")

(use-package dash :load-path "site-lisp/dash.el")

(use-package crux :load-path "site-lisp/crux")

(defun map-thing-at-point (thing func)
  (let* ((bounds (bounds-of-thing-at-point thing))
         (start (car bounds))
         (end (cdr bounds))
         (thing-value (buffer-substring start end)))
    (when bounds
      (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert (apply func (list thing-value)))))))

(defun my-bind-keys (bindings)
  (if (eq (cdr bindings) nil)
      t
    (progn
      (let ((key (car bindings))
	    (function (cadr bindings)))
	(my-bind-key key function)
	(my-bind-keys (cddr bindings))))))

(defmacro my-bind-key (key function)
  `(progn
     (global-set-key (read-kbd-macro ,key) ,function)))

;;;; VISUALS

(setq custom-file "~/.emacs.d/site-lisp/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(load-theme 'coffee)

(setq-default left-fringe-width 4
              right-fringe-width 0
              left-margin-width 1
              right-margin-width 1
              truncate-lines t
              truncate-partial-width-windows nil)

(set-frame-parameter (selected-frame) 'internal-border-width 4)

(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;;; FILE FORMATTING

;;; No tabs
(setq-default indent-tabs-mode nil)

;;; UTF-8 everywhere
(prefer-coding-system 'utf-8)

;;;; GENERAL BEHAVIOUR

;;; Turn on to debug errors
(setq debug-on-error nil)

;;; No splash screen
(setq inhibit-startup-message t)

;;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Do not ask when killing buffers with an active process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;; Highlight current line
(global-hl-line-mode t)

;;; Yank where the point is, not where clicked
(setq mouse-yank-at-point t)

;;;; WINDOWS

;;; No popups unless explictly permitted/requested, by default use current window
(setq display-buffer-alist `((".*" . (display-buffer-same-window . ((inhibit-same-window . nil))))))

;;; Open new windows below current one
(setq split-width-threshold 9999)

;;; Do not resize windows automatically
(setq even-window-sizes nil)

;;; Navigate between windows easily
(windmove-default-keybindings 'super)

;;;; SESSIONS

;;; Preserve minibuffer history
(use-package savehist
  :config
  (setq savehist-autosave-interval 60
        savehist-additional-variables '(compile-command))
  (savehist-mode))

;;; Preserve position in buffers
(save-place-mode 1)

;;;; NAVIGATION

;;; Scroll to top/bottom before signaling error from PageUp/PageDown etc.
(setq scroll-error-top-bottom t)

;;; Preserve point position relatively to the screen when scrolling
(setq scroll-preserve-screen-position 'always)

;;; Scroll one line at a time near window boundary
(setq scroll-conservatively 10000)

;;; Word by word navigation in CamelCase names
(global-subword-mode t)
(my-bind-keys
 '("<C-left>" subword-backward
   "<C-right>" subword-forward))

;;; Center the screen on specific line, when jumping to compilation error:
(setq next-error-recenter '(4))

;;; when jumping to a bookmark:
(add-hook 'bookmark-after-jump-hook #'recenter-top-bottom)

;;; when opening an org mode hyperlink:
(advice-add 'org-open-file :after
            (lambda (path &optional in-emacs line search)
              (if line
                  (recenter-top-bottom))))

;;;; GENERAL TEXT EDITING

;;; Move lines of text up/down easily
(use-package move-text
  :load-path "site-lisp/move-text"
  :config
  (move-text-default-bindings))

;;; Enable useful commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Typing and pasting with an active selection overwrites the selection
(delete-selection-mode 1)

;;;; GENERAL MODES

;;;; PROJECTILE

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :commands projectile-mode
  :bind-keymap ("M-p" . projectile-command-map)
  :defer 5
  :config
  (setq projectile-completion-system 'ivy
        projectile-keymap-prefix "M-p"
        projectile-switch-project-action #'projectile-dired)
  ;;; Recognize every subdir of ~/[Pp]rojects/ as a project
  (setq my-projectile-project-dir-re
        (concat "\\(" (expand-file-name "~/[Pp]rojects/") "[^/]+\\)[/]?.*"))
  (defun my-projectile-project-root (dir)
      (when (string-match my-projectile-project-dir-re dir)
        (match-string 1 dir)))
  (add-to-list 'projectile-project-root-files-functions 'my-projectile-project-root)
  (projectile-mode 1))

;;;; IVY/COUNSEL/SWIPER/FLX

(use-package ivy
  :load-path "site-lisp/swiper"
  :commands ivy-mode
  :bind
  ("C-c C-r" . ivy-resume)
  :init
  ;; do not show candidate count as part of the prompt
  (setq ivy-count-format "")
  ;; highlight whole line of the selected candidate
  (setq ivy-format-function 'ivy-format-function-line)
  ;; do not separately highlight the matching part of the candidate
  (setq ivy-display-style 'plain)
  ;; case insensitive matching, even when uppercase letter present in input
  (setq ivy-case-fold-search-default 'always)
  ;; show non-buffers as candidates, e.g. recently closed buffers
  (setq ivy-use-virtual-buffers t)
  ;; no . and .. candidates when selecting files
  (setq ivy-extra-directories '())
  ;; do not do anything special when typing / at the end of non-directory-name
  (setq ivy-magic-slash-non-match-action nil)
  (ivy-mode t))

(use-package counsel
  :bind
  ("C-x r b" . counsel-bookmark))

;;;; GREP

(use-package grep
  :config
  (grep-apply-setting 'grep-find-command '("fdfind . -t f -exec rg -n -H '' \{\}" . 31)))

(defun my-projectile-find-grep-in-project ()
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'find-grep)))

(use-package wgrep
  :load-path "site-lisp/Emacs-wgrep"
  :config
  (setq wgrep-auto-save-buffer t))

;;;; DIRED

(use-package dired
  :config

  ;;; Extended display
  (use-package dired-x)

  ;;; Do not clash with compile mode
  (unbind-key "M-g" dired-mode-map)

  ;;; Do not clash with projectile
  (unbind-key "M-p" dired-mode-map)

  ;;; C-j opens directory just like RET
  (bind-key "C-j" 'dired-find-file dired-mode-map)

  ;;; Hidden files hidden by default
  (setq dired-omit-files "^\\.[^\\.].+$")
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

  ;; Auto refresh
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;;; Directories first
  (setq dired-listing-switches "-alh --group-directories-first"))

;;;; MAGIT

(use-package graphql :load-path ("site-lisp/graphql.el"))

(use-package treepy :load-path ("site-lisp/treepy.el"))

(use-package ghub :load-path ("site-lisp/ghub"))

(use-package magit-popup :load-path ("site-lisp/magit-popup"))

(use-package magit
  :load-path ("site-lisp/magit/lisp" "site-lisp/with-editor")
  :config
  ;;; Do not clash with projectile mode
  (unbind-key "M-p" magit-mode-map))

;;;; PROGRAMMING

;; Do not ask about saving unsaved buffers when running compile
(setq compilation-save-buffers-predicate 'ignore)

;; Do not ask whether to kill existing compilation process when running a new one
;; Also takes care of the same for grep
(add-hook 'compilation-start-hook
          (lambda (proc)
            (set-process-query-on-exit-flag proc nil)))

;;; Highlight matching parens
(show-paren-mode)

;;; Auto-indent
(electric-indent-mode t)
(setq electric-indent-functions
      (list
	(lambda (arg)
          (if (eq major-mode 'org-mode)
              'no-indent
            nil))))

;;;; PROGRAMMING - HTML/CSS/JAVASCRIPT/JSX

(use-package web-mode
  :mode "\\.\\(js\\|jsx\\|html\\)\\'"
  :load-path "site-lisp/web-mode"
  :config
  ;;; Do not clash with ivy-resume
  (unbind-key "C-c C-r" web-mode-map)
  (setq js-indent-level 2
        css-indent-offset 2
        web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
        web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-enable-auto-quoting nil))

;;;; PROGRAMMING - YAML

(use-package yaml-mode
  :mode "\\.yml\\'"
  :load-path "site-lisp/yaml-mode")

;;;; KEY BINDINGS

(defun notes ()
  "Opens up the notebook file."
  (interactive)
  (find-file "~/txt/notes.txt"))

;;; No suspending
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(my-bind-keys
 '(;;; overrides
   "C-a" crux-move-beginning-of-line
   "<home>" crux-move-beginning-of-line
   "C-o" crux-smart-open-line-above
   ;;; function keys
   "<f5>" dired-jump
   ;;; C-c - compilation
   "C-c c" compile
   "C-c r" recompile
   ;;; C-c - grep
   "C-c g" my-projectile-find-grep-in-project
   "C-c G" find-grep
   ;;; C-c - rest
   "C-c k" kill-this-buffer
   "C-c n" notes))

;;;; STARTUP

(find-file "~/txt/notes.txt")
