(deftheme coffee "Coffee color theme")

(defgroup coffee nil "Coffee color theme" :prefix "coffee-")

(defcustom coffee-vary-weights nil
  "Whether to use different font weights in some faces to indicate importance"
  :group 'coffee
  :type 'boolean)

(defun coffee-bold ()
  (if coffee-vary-weights '(:weight bold) '(:weight normal)))

(defun coffee-demibold ()
  (if coffee-vary-weights '(:weight demibold) '(:weight normal)))

(defun coffee-extralight ()
  (if coffee-vary-weights '(:weight extralight) '(:weight normal)))

(setq compilation-message-face 'default)

(let* (;;; Basic colors
       (bgcolor "#121212")
       (fgcolor "#ffffff")
       (prio-a-color "#fff590")
       (prio-b-color "#ffc890")
       (prio-c-color "#7cc2d6")
       (prio-d-color "#ffe4c8")
       (prio-e-color "#c5e5ee")
       (prio-f-color "#fffac8")
       (green "#90ffb5")
       (red "#ff9090")
       (highlight "#000000")
       ;;; Basic faces
       (prio-a `(:foreground ,prio-a-color))
       (prio-b `(:foreground ,prio-b-color))
       (prio-c `(:foreground ,prio-c-color))
       (prio-d `(:foreground ,prio-d-color))
       (prio-e `(:foreground ,prio-e-color))
       (prio-f `(:foreground ,prio-f-color))
       (error `(:foreground "#fc5161" ,@(coffee-bold)))
       (highlight-error `(:background "#ff2244" :foreground "#ffffff"))
       (warning `(:foreground "#ff8866" ,@(coffee-bold)))
       (hyperlink `(:foreground "#88ccff" :underline nil))
       (directory `(:background unspecified ,@prio-b :weight normal))
       ;;; Highlights
       (parens `(:background "#ffffff" :foreground ,bgcolor))
       (region `(:background ,prio-b-color :foreground ,bgcolor))
       (search-primary `(:background ,prio-b-color :foreground ,bgcolor))
       (search-secondary `(:background ,prio-c-color :foreground ,bgcolor))
       (current-line `(:background "#213b43"))
       (codalog-project `(:foreground ,prio-a-color))
       ;;; Interface faces
       (interface `(:background ,bgcolor))
       (text `(:background ,bgcolor :foreground ,fgcolor))
       (cursor `(:background ,prio-a-color))
       (widget `(:background "#4d4e4f" :foreground ,fgcolor :weight normal :box (:line-width 1 :color "#5d5e5f")))
       (widget-inactive `(:background "#3a3c3d" :foreground "#adaaa0" ,@(coffee-extralight) :box (:line-width 1 :color "#4a4c4d")))
       (header `(:background "#202020" :foreground ,prio-a-color :box (:color "#202020" :line-width 8)))
       (subheader `(:background "#202020" :foreground ,prio-b-color :box (:color "#202020" :line-width 8))))

  (custom-theme-set-faces 'coffee
    ;;; Basic faces
    `(default ((t ,text)))
    `(cursor ((t ,cursor)))
    `(bold ((t ,@(coffee-bold))))
    `(highlight ((t ,current-line)))
    `(region ((t ,region)))
    `(fringe ((t ,interface)))
    `(minibuffer-prompt ((t ,text)))
    `(mode-line ((t ,widget)))
    `(mode-line-inactive ((t ,widget-inactive)))
    `(mode-line-buffer-id ((t (:foreground unspecified :weight unspecified))))
    `(link ((t ,hyperlink)))
    `(warning ((t ,warning)))
    `(success ((t ,@(coffee-demibold))))
    `(header-line ((t ,header)))

    ;;; Font lock
    `(font-lock-function-name-face ((t ,prio-a)))
    `(font-lock-comment-face ((t ,prio-b)))
    `(font-lock-doc-face ((t ,prio-b)))
    `(font-lock-keyword-face ((t ,prio-c)))
    `(font-lock-variable-name-face ((t ,prio-d)))
    `(font-lock-string-face ((t ,prio-e)))
    `(font-lock-builtin-face ((t ,prio-f)))
    `(font-lock-constant-face ((t ,prio-f)))
    `(font-lock-warning-face ((t ,error)))
    `(font-lock-type-face ((t ,text)))

    ;;; Web mode
    `(web-mode-html-tag-face ((t ,prio-a)))
    `(web-mode-html-attr-name-face ((t ,prio-b)))

    ;;; Python
    `(py-number-face ((t ,prio-d)))
    `(py-decorators-face ((t ,prio-e)))
    `(py-builtins-face ((t ,prio-e)))
    `(py-variable-name-face ((t ,text)))
    `(py-pseudo-keyword-face ((t ,text)))

    ;;; Shell scripts
    `(sh-heredoc ((t ,prio-e)))
    
    ;;; Search & replace
    `(isearch ((t ,search-primary)))
    `(match ((t ,search-primary)))
    `(lazy-highlight ((t ,search-secondary)))
    ;;; so that isearch match is visible "on top" of ag-match-face
    `(ag-match-face ((t ,search-secondary)))

    ;;; Magit
    `(magit-section-heading ((t ,prio-a)))
    `(magit-section-secondary-heading ((t (:weight normal))))
    `(magit-blame-heading ((t ,subheader)))
    `(magit-section-highlight ((t (:background unspecified))))
    `(magit-item-highlight ((t (:background "#202020"))))
    `(magit-diff-file-heading ((t (:weight normal))))
    `(magit-diff-context-highlight ((t (:background "#202020"))))
    `(magit-diff-add ((t (:foreground ,green))))
    `(magit-diff-added ((t (:foreground ,green))))
    `(magit-diff-added-highlight ((t (:background "#202020" :foreground ,green))))
    `(magit-diff-del ((t (:foreground ,red))))
    `(magit-diff-removed ((t (:foreground ,red))))
    `(magit-diff-removed-highlight ((t (:background "#202020" :foreground ,red))))
    `(magit-branch ((t ,prio-b)))
    `(magit-branch-local ((t ,prio-b)))
    `(magit-branch-remote ((t ,prio-b)))
    `(magit-log-sha1 ((t ,prio-c)))
    `(magit-hash ((t ,prio-c)))

    ;;; Org mode
    `(org-level-1 ((t (,@prio-a :height 1.15))))
    `(org-level-2 ((t (,@prio-b :height 1.10))))
    `(org-level-3 ((t (,@prio-c :height 1.05))))
    `(org-level-4 ((t ,prio-d)))
    `(org-date ((t ,prio-c)))
    `(org-link ((t ,hyperlink)))

    ;;; Dired
    `(dired-marked ((t (,@prio-a ,@(coffee-bold)))))
    `(dired-directory ((t ,directory)))
    `(dired-symlink ((t ,prio-c)))
    `(dired-perm-write ((t ,text)))
    
    ;;; Eshell
    `(eshell-prompt ((t (,@prio-a :weight normal :underline nil))))
    `(eshell-ls-directory ((t ,directory)))
    `(eshell-ls-executable ((t (,@prio-a ,@(coffee-demibold)))))

    ;;; Comint
    `(comint-highlight-input ((t ,text)))

    ;;; Info
    `(info-xref ((t ,hyperlink)))
    `(info-xref-visited ((t ,hyperlink)))
    `(info-menu-star ((t ,text)))

    ;;; Compilation mode
    `(compilation-error ((t (,@error :weight normal))))
    `(compilation-info ((t (,@prio-a ,@(coffee-demibold)))))
    `(compilation-line-number ((t ,text)))
    `(compilation-column-number ((t ,text)))

    ;;; Codalog.el
    `(codalog-mode-line-project-name ((t ,@codalog-project)))

    ;;; Show paren mode
    `(show-paren-match ((t ,@parens)))

    ;;; Mic-paren mode
    `(show-paren-match ((t ,@parens)))
    `(show-paren-mismatch ((t (,@highlight-error ,@(coffee-bold)))))))
