;;;; EMACS-LISP

;;; Turn on to debug errors
(setq debug-on-error nil)

;;; GC after 8MB
(setq gc-cons-threshold (* 8 1024 1024))

;;; Use-package
(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

;;; Utility libraries
(use-package crux :load-path "site-lisp/crux")
(use-package dash :load-path "site-lisp/dash.el")
(use-package s :load-path "site-lisp/s.el")

;;; My own utilities
(use-package jr-bind-keys :load-path "my-lisp/" :commands 'jr/bind-keys)

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

;;; Highlight current line
(global-hl-line-mode t)

;;;; GENERAL SETUP

;;;; Do not litter the fs with temporary files
(use-package no-littering :load-path "site-lisp/no-littering")
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; No splash screen
(setq inhibit-startup-message t)

;;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Do not ask when killing buffers with an active process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;; Yank where the point is, not where clicked
(setq mouse-yank-at-point t)

;;;; WINDOWS

(setq jr/utility-buffers-list '("*grep*" "*compilation*"))

(setq jr/utility-window-height 16)

(defun jr/utility-buffer-p (buffer-or-buffer-name &optional props)
  (let ((buffer-name
         (if (bufferp buffer-or-buffer-name)
             (buffer-name buffer-or-buffer-name)
           buffer-or-buffer-name)))
    (member buffer-name jr/utility-buffers-list)))

(defun jr/utility-window-p (window)
  (jr/utility-buffer-p (window-buffer window)))

(defun jr/in-utility-window-p ()
  (jr/utility-buffer-p (current-buffer)))

(defun jr/window-list ()
  (mapcan #'window-list (frame-list)))

(defun jr/display-utility-buffer (buffer props)
  (if (jr/utility-buffer-p buffer)
      (let ((window (or (find-if #'jr/utility-window-p (jr/window-list))
                        (split-window (frame-root-window) (- jr/utility-window-height) 'below))))
        (set-window-buffer window buffer)
        window)
    nil))

(setq display-buffer-alist
      `(;;; Display utility buffers in the utility window, popping one
        ;;; up if not already present
        (jr/utility-buffer-p . (jr/display-utility-buffer . ()))
        ;;; Do not display non-utility buffers in the utility window
        ((lambda (w p) (and (jr/in-utility-window-p) (not (jr/utility-buffer-p w p))))
         . (display-buffer-use-some-window . ((inhibit-same-window . t))))
        ;;; No popups unless explictly permitted/requested, by default
        ;;; reuse the existing window displaying the buffer or use the
        ;;; current window
        (".*"
         . ((display-buffer-reuse-window display-buffer-same-window)
            . (;; permit using the current window instead of popup
               ;; (needed even with display-buffer-same-window)
               (inhibit-same-window . nil)
               ;; consider windows on all frames for reuse
               (reusable-frames . t)
               ;; do not raise the other frame if a window there was chosen
               (inhibit-switch-frame . t))))))

;;; Open new windows below current one
(setq split-width-threshold 9999)

;;; Do not resize windows automatically
(setq even-window-sizes nil)

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

;;; Center the screen on specific line, when jumping to compilation error:
(setq next-error-recenter '(4))

;;; when jumping to a bookmark:
(add-hook 'bookmark-after-jump-hook #'recenter)

;;; when opening an org mode hyperlink:
(advice-add 'org-open-file :after
            (lambda (path &optional in-emacs line search)
              (if line
                  (recenter))))

;;; when isearching:
(advice-add 'isearch-update :after #'recenter)

;;; Word by word navigation in CamelCase names
(global-subword-mode t)

;;;; GENERAL TEXT EDITING

;;; No tabs
(setq-default indent-tabs-mode nil)

;;; UTF-8 everywhere
(prefer-coding-system 'utf-8)

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

(defun jr/kill-region-or-backward-delete-word ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (save-mark-and-excursion
      (let ((here (point)))
        (backward-word)
        (delete-region (point) here)))))

;;;; GENERAL MODES

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
  :commands counsel-mode
  :init
  (counsel-mode t))

(use-package counsel-projectile
  :load-path "site-lisp/counsel-projectile"
  :commands counsel-projectile-mode
  :init
  (counsel-projectile-mode t)
  ;; sort projects list by name
  (setq counsel-projectile-sort-projects 'string-lessp))

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

;;;; GREP

(use-package grep
  :config
  ;;; Do not prompt about saving unsaved buffers
  (setq grep-save-buffers nil)
  ;;; Use fd and ripgrep for grep-find
  (grep-apply-setting 'grep-find-command '("fdfind . -t f -exec rg -n -H '' \{\}" . 31)))

(defun jr/projectile-grep-find-in-project ()
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'grep-find)))

(use-package wgrep
  :load-path "site-lisp/Emacs-wgrep"
  :config
  (setq wgrep-auto-save-buffer t))

;;;; PROJECTILE

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :commands projectile-mode
  :bind-keymap ("M-p" . projectile-command-map)
  :defer 5
  :config
  (setq projectile-completion-system 'ivy
        projectile-git-command "fdfind . -tf -0"
        projectile-generic-command "fdfind . -tf -0"
        projectile-keymap-prefix "M-p"
        projectile-switch-project-action #'projectile-dired)
  ;;; Recognize every subdir of ~/[Pp]rojects/ as a project
  (setq jr/projectile-project-dir-re
        (concat "\\(" (expand-file-name "~/[Pp]rojects/") "[^/]+\\)[/]?.*"))
  (defun jr/projectile-project-root (dir)
      (when (string-match jr/projectile-project-dir-re dir)
        (match-string 1 dir)))
  (add-to-list 'projectile-project-root-files-functions 'jr/projectile-project-root)
  (projectile-mode 1))

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

(use-package compile
  :config
  ;;; Do not clash with projectile
  (unbind-key "M-p" compilation-mode-map)
  ;; Do not ask about saving unsaved buffers when running compile
  (setq compilation-save-buffers-predicate 'ignore)
  ;; Do not ask whether to kill existing compilation process when running a new one
  ;; Also takes care of the same for grep
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (set-process-query-on-exit-flag proc nil))))

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

(defun jr/notes ()
  "Opens up the notebook file."
  (interactive)
  (find-file "~/txt/notes.txt"))

;;; No suspending
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(jr/bind-keys
 '(;;; overrides
   "C-a" crux-move-beginning-of-line
   "<home>" crux-move-beginning-of-line
   "C-o" crux-smart-open-line-above
   "C-w" jr/kill-region-or-backward-delete-word
   ;;; function keys
   "<f5>" dired-jump
   ;;; C-c - windmove
   "C-c <left>" windmove-left
   "C-c <right>" windmove-right
   "C-c <up>" windmove-up
   "C-c <down>" windmove-down
   ;;; C-c - compilation
   "C-c c" compile
   "C-c r" recompile
   ;;; C-c - grep
   "C-c g" jr/projectile-grep-find-in-project
   "C-c G" find-grep
   ;;; C-c - rest
   "C-c k" kill-this-buffer
   "C-c n" jr/notes))

;;;; STARTUP

(find-file "~/txt/notes.txt")
