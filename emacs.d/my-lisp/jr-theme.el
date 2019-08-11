;;; References:
;;; List of default 256 colors in the terminal:
;;; https://jonasjacek.github.io/colors/

(deftheme jr "My color theme")

(defgroup jr-theme nil "My color theme" :prefix "jr-theme-")

(defface jr-theme-modeline-project-name '((t ()))
  "Face for displaying the name of the current project in the modeline")

(defcustom jr-theme-vary-weights nil
  "Whether to use different font weights in some faces to indicate importance"
  :group 'jr-theme
  :type 'boolean)

(defun jr-theme-bold ()
  (if jr-theme-vary-weights '(:weight bold) '(:weight normal)))

(defun jr-theme-demibold ()
  (if jr-theme-vary-weights '(:weight demibold) '(:weight normal)))

(defun jr-theme-extralight ()
  (if jr-theme-vary-weights '(:weight extralight) '(:weight normal)))

(set-frame-parameter (selected-frame) 'internal-border-width 4)

(setq-default left-fringe-width 0
              right-fringe-width 0
              left-margin-width 2
              right-margin-width 2
              truncate-lines t
              truncate-partial-width-windows nil)

(defun jr-theme-modeline-project-prefix ()
  (if (featurep 'projectile)
    (let* ((project (projectile-project-root))
           (project-name-prefix (projectile-project-name)))
      (if project
          (concat (propertize (concat " " project-name-prefix " ") 'face 'jr-theme-modeline-project-name) " ")
        ""))
    ""))

(defun jr-theme-modeline-buffer-id ()
   (concat (jr-theme-modeline-project-prefix) (buffer-name)))

(defun jr-theme-frame-title-buffer-id ()
  (let ((buf-file-name (buffer-file-name (current-buffer))))
           (cond
            (buf-file-name (abbreviate-file-name buf-file-name))
            ((equal major-mode 'dired-mode)
             default-directory)
            (t (buffer-name)))))

(setq-default mode-line-format '(" %+ " (:eval (jr-theme-modeline-buffer-id)) " %I (%l,%c)"))

(setq frame-title-format '((:eval (jr-theme-frame-title-buffer-id))))

(setq compilation-message-face 'default)

(let* (;;; Basic colors
       (bgcolor      "#121212") ; #233
       (fgcolor      "#ffffff") ; #15
       (prio-a-color "#ffffaf") ; #229 - hsl(60,100%,84%)
       (prio-b-color "#ff875f") ; #209 - hsl(15,100%,68%)
       (prio-c-color "#ffd7af") ; #223 - hsl(30,100%,84%)
       (prio-d-color "#ffffd7") ; #230 - hsl(60,100%,92%)
       (prio-e-color "#ffd7d7") ; #224 - hsl(0,100%,92%)
       (prio-f-color "#dadada") ; #253 - hsl(0,0%,85%)
       (red          "#ff8787") ; #210 - hsl(0,100%,76%)
       (green        "#87ffd7") ; #122 - hsl(160,100%,76%)
       (blue         "#87d7ff") ; #117 - hsl(200,100%,76%)
       (grey1        "#262626") ; #235
       (grey2        "#303030") ; #236
       (grey3        "#444444") ; #238
       (grey4        "#585858") ; #240
       (grey5        "#6c6c6c") ; #242
       (grey6        "#808080") ; #244
       (grey7        "#949494") ; #246
       (grey8        "#a8a8a8") ; #248
       (grey9        "#bcbcbc") ; #250
       (grey10       "#d0d0d0") ; #252
       (grey11       "#e4e4e4") ; #254
       ;;; Basic faces
       (prio-a `(:foreground ,prio-a-color))
       (prio-b `(:foreground ,prio-b-color))
       (prio-c `(:foreground ,prio-c-color))
       (prio-d `(:foreground ,prio-d-color))
       (prio-e `(:foreground ,prio-e-color))
       (prio-f `(:foreground ,prio-f-color))
       (error `(:foreground ,red ,@(jr-theme-bold)))
       (highlight-error `(:background "#ff2244" :foreground "#ffffff"))
       (warning `(:foreground "#ff8866" ,@(jr-theme-bold)))
       (hyperlink `(:foreground ,blue :underline nil))
       (directory `(:background unspecified ,@prio-b :weight normal))
       ;;; Highlights
       (parens `(:foreground ,prio-a-color :weight bold :underline t))
       (region `(:background ,prio-a-color :foreground ,bgcolor))
       (search-primary `(:background ,prio-a-color :foreground ,bgcolor))
       (search-secondary `(:background ,prio-b-color :foreground ,bgcolor))
       (current-line `(:background ,grey1))
       (highlight `(:background ,grey1 :foreground ,prio-a-color))
       ;;; Interface faces
       (interface `(:background ,bgcolor))
       (border `(:background ,grey1 :foreground ,grey1))
       (text `(:background ,bgcolor :foreground ,fgcolor))
       (cursor `(:background ,prio-a-color :foreground ,fgcolor))
       (widget `(:background ,grey2 :foreground ,fgcolor :weight normal :box (:line-width 1 :color ,grey2)))
       (widget-inactive `(:background ,grey3 :foreground ,grey10 ,@(jr-theme-extralight) :box (:line-width 1 :color ,grey3)))
       (header `(:background "#202020" :foreground ,prio-a-color :box (:color "#202020" :line-width 8)))
       (subheader `(:background "#202020" :foreground ,prio-b-color :box (:color "#202020" :line-width 8))))

  (custom-theme-set-faces 'jr
    ;;; Basic faces
    `(default ((t ,text)))
    `(cursor ((t ,cursor)))
    `(bold ((t ,@(jr-theme-bold))))
    `(hl-line ((t ,current-line)))
    `(highlight ((t ,current-line)))
    `(region ((t ,region)))
    `(fringe ((t ,interface)))
    `(minibuffer-prompt ((t ,text)))
    `(vertical-border ((t ,border)))
    `(mode-line ((t ,widget)))
    `(mode-line-inactive ((t ,widget-inactive)))
    `(mode-line-buffer-id ((t (:foreground unspecified :weight unspecified))))
    `(link ((t ,hyperlink)))
    `(warning ((t ,warning)))
    `(success ((t ,@(jr-theme-demibold))))
    `(header-line ((t ,header)))
    `(trailing-whitespace ((t (:background ,grey2))))

    ;;; Font lock
    `(font-lock-function-name-face ((t ,prio-a)))
    `(font-lock-keyword-face ((t ,prio-b)))
    `(font-lock-type-face ((t ,prio-c)))
    `(font-lock-comment-face ((t ,prio-d)))
    `(font-lock-doc-face ((t ,prio-d)))
    `(font-lock-string-face ((t ,prio-e)))
    `(font-lock-builtin-face ((t ,prio-f)))
    `(font-lock-constant-face ((t ,prio-f)))
    `(font-lock-variable-name-face ((t ,prio-f)))
    `(font-lock-warning-face ((t ,error)))

    ;;; Ivy
    `(ivy-current-match ((t ,highlight)))

    ;;; Company
    `(company-tooltip ((t (:background ,grey2))))
    `(company-tooltip-selection ((t (:background ,grey4))))
    `(company-tooltip-common ((t (:foreground ,prio-b-color))))
    `(company-tooltip-annotation ((t (:foreground ,prio-d-color))))
    `(company-scrollbar-bg ((t (:background ,grey2))))
    `(company-scrollbar-fg ((t (:background ,grey5))))
    `(company-preview ((t (:background ,grey5))))
    `(company-preview-common ((t (:foreground ,fgcolor))))

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

    ;;; Diff
    `(diff-added ((t (:foreground ,green))))
    `(diff-removed ((t (:foreground ,red))))

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
    `(org-level-5 ((t ,prio-e)))
    `(org-level-6 ((t ,prio-f)))
    `(org-date ((t ,prio-c)))
    `(org-link ((t ,hyperlink)))

    ;;; Dired
    `(dired-marked ((t (,@prio-a ,@(jr-theme-bold)))))
    `(dired-directory ((t ,directory)))
    `(dired-symlink ((t ,prio-c)))
    `(dired-perm-write ((t ,text)))

    ;;; Eshell
    `(eshell-prompt ((t (,@prio-a :weight normal :underline nil))))
    `(eshell-ls-directory ((t ,directory)))
    `(eshell-ls-executable ((t (,@prio-a ,@(jr-theme-demibold)))))

    ;;; Comint
    `(comint-highlight-input ((t ,text)))

    ;;; Info
    `(info-xref ((t ,hyperlink)))
    `(info-xref-visited ((t ,hyperlink)))
    `(info-menu-star ((t ,text)))

    ;;; Compilation mode
    `(compilation-error ((t (,@error :weight normal))))
    `(compilation-info ((t (,@prio-a ,@(jr-theme-demibold)))))
    `(compilation-line-number ((t ,text)))
    `(compilation-column-number ((t ,text)))

    ;;; Show paren mode
    `(show-paren-match ((t ,@parens)))

    ;;; Mic-paren mode
    `(show-paren-match ((t ,@parens)))
    `(show-paren-mismatch ((t (,@highlight-error ,@(jr-theme-bold)))))

    ;;; Own faces
    `(jr-theme-modeline-project-name ((t (:background ,grey1 :foreground ,prio-a-color :box (:color ,grey1 :line-width 1)))))))
