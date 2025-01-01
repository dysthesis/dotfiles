;; Elpaca package manager
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package evil 
               :ensure t
               :config
               (evil-mode 1))
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-h") #'evil-window-left)
(global-set-key (kbd "C-j") #'evil-window-down)
(global-set-key (kbd "C-k") #'evil-window-up)
(global-set-key (kbd "C-l") #'evil-window-right)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(set-face-attribute 'default nil :font "JBMono Nerd Font" :height 130)
(setq-default line-spacing 0.2)
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "SF Pro Display" :height 160))))
 '(fixed-pitch ((t ( :family "JBMono Nerd Font" :height 130)))))

(use-package catppuccin-theme
             :ensure t
             :config
             (load-theme 'catppuccin
                         :no-confirm
                         t)
             (catppuccin-set-color 'base "#000000")
             (catppuccin-set-color 'crust "#000000")
             (catppuccin-set-color 'mantle "#11111b")
             (catppuccin-set-color 'surface0 "#181825")
             (catppuccin-set-color 'surface1 "#313244")
             (catppuccin-set-color 'surface2 "#45475a")
             (catppuccin-set-color 'overlay0 "#585b70")
             (catppuccin-set-color 'overlay1 "#6c7086")
             (catppuccin-set-color 'overlay2 "#7f849c")
             (catppuccin-set-color 'text "#ffffff")
             (catppuccin-reload))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-bar-width 4))

(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

(use-package olivetti
  :ensure t
  :config
  (defun dysthesis/org-mode-setup ()
    (olivetti-mode)
    (display-line-numbers-mode 0)
    (olivetti-set-width 90)
    (setq-local company-backends (remove 'company-dabbrev company-backends))
    (setq-local company-backends (remove 'company-ispell company-backends))) (add-hook 'org-mode-hook 'dysthesis/org-mode-setup))

(use-package mixed-pitch
  :ensure t
  :hook
  ;; You might want to enable it only in org-mode or both text-mode and org-mode
  ((org-mode) . mixed-pitch-mode)
  ((markdown-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-table
                  org-code
                  org-block
                  org-block-begin-line
                  org-block-end-line
                  org-meta-line
                  org-document-info-keyword
                  org-tag
                  org-time-grid
                  org-todo
                  org-done
                  org-agenda-date
                  org-date
                  org-drawer
                  org-modern-tag
                  org-modern-done
                  org-modern-label
                  org-scheduled
                  org-scheduled-today
                  neo-file-link-face
                  org-scheduled-previously))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  ;; Optional customizations
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  ;;(add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package yasnippet-snippets
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :foreground "#ffffff" :height 1.4 :weight bold))))
  '(org-level-2 ((t (:inherit outline-2 :foreground "#ffffff" :height 1.2 :weight bold))))
  '(org-level-3 ((t (:inherit outline-3 :foreground "#ffffff" :height 1.1 :weight bold))))
  '(org-level-4 ((t (:inherit outline-4 :foreground "#ffffff" :height 1.0 :weight bold))))
  '(org-level-5 ((t (:inherit outline-5 :foreground "#ffffff" :height 0.9 :weight bold))))
  (set-face-attribute 'org-document-title nil :foreground "#ffffff" :height 2.0)))

(setq org-hide-emphasis-markers t)
(use-package org-appear
  :ensure (:type git :host github :repo
           "awth13/org-appear")
  :config ; add late to hook
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-modern
  :ensure t
  :config
  (package-initialize)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (dolist (face '(window-divider
      	   window-divider-first-pixel
      	   window-divider-last-pixel))
   (face-spec-reset-face face)
   (set-face-foreground face (face-attribute 'default :background)))
 (set-face-background 'fringe (face-attribute 'default :background))
 (setq org-hide-emphasis-markers t)
 (setq  org-modern-list
       '((42 . "•")
         (43 . "◈")
         (45 . "➤")))
 (setq org-modern-block-name
       '((t . t)
         ("src" "»" "«")
         ("example" "»–" "–«")
         ("quote" "" "")
         ("export" "⏩" "⏪")))
 (setq org-modern-block-fringe 6)
 (setq org-modern-keyword
       '((t . t)
         ("title" . "𝙏 ")
         ("filetags" . "󰓹 ")
         ("auto_tangle" . "󱋿 ")
         ("subtitle" . "𝙩 ")
         ("author" . "𝘼 ")
         ("email" . #(" " 0 1 (display (raise -0.14))))
         ("date" . "𝘿 ")
         ("property" . "☸ ")
         ("options" . "⌥ ")
         ("startup" . "⏻ ")
         ("macro" . "𝓜 ")
         ("bind" . #(" " 0 1 (display (raise -0.1))))
         ("bibliography" . " ")
         ("print_bibliography" . #(" " 0 1 (display (raise -0.1))))
         ("cite_export" . "⮭ ")
         ("print_glossary" . #("ᴬᶻ " 0 1 (display (raise -0.1))))
         ("glossary_sources" . #(" " 0 1 (display (raise -0.14))))
         ("include" . "⇤ ")
         ("setupfile" . "⇚ ")
         ("html_head" . "🅷 ")
         ("html" . "🅗 ")
         ("latex_class" . "🄻 ")
         ("latex_class_options" . #("🄻 " 1 2 (display (raise -0.14))))
         ("latex_header" . "🅻 ")
         ("latex_header_extra" . "🅻⁺ ")
         ("latex" . "🅛 ")
         ("beamer_theme" . "🄱 ")
         ("beamer_color_theme" . #("🄱 " 1 2 (display (raise -0.12))))
         ("beamer_font_theme" . "🄱𝐀 ")
         ("beamer_header" . "🅱 ")
         ("beamer" . "🅑 ")
         ("attr_latex" . "🄛 ")
         ("attr_html" . "🄗 ")
         ("attr_org" . "⒪ ")
         ("call" . #(" " 0 1 (display (raise -0.15))))
         ("name" . "⁍ ")
         ("header" . "› ")
         ("caption" . "☰ ")
         ("results" . "🠶")))
 (setq org-agenda-tags-column 0
       org-agenda-block-separator ?─
       org-agenda-time-grid
       '((daily today require-timed)
         (800 1000 1200 1400 1600 1800 2000)
         " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
       org-agenda-current-time-string
       "⭠ now ─────────────────────────────────────────────────")
 (setq org-modern-todo-faces
       '(("WAIT"
          :inverse-video t
          :inherit +org-todo-onhold)
         ("NEXT"
          :inverse-video t
          :foreground "#89b4fa")
         ("PROG"
          :inverse-video t
          :foreground "#a6e3a1")
         ("TODO"
          :inverse-video t
          :foreground "#fab387")))
 (global-org-modern-mode)
 (setq org-ellipsis " ↪")
 (setq org-pretty-entities t))

(use-package org-modern-indent
  :ensure (:type git :host github :repo
      	   "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))