;; https://github.com/progfolio/elpaca
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

(use-package emacs
  :demand t
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager
  (setq show-trailing-whitespace t) ;; self-explanatory
  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short 
  (setq indent-tabs-mode nil) ;; no tabs
  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))
  (setq create-lockfiles nil) ;; no need to create lockfiles
  (set-charset-priority 'unicode) ;; utf8 everywhere
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything
  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings
  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.
  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  (show-paren-mode t)

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(set-face-attribute 'default nil :font "JBMono Nerd Font" :height 130)
(set-fontset-font t nil (font-spec :size 20 :name "JBMono Nerd Font"))
(setq-default line-spacing 0.2)
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "SF Pro Display" :height 200))))
 '(fixed-pitch ((t ( :family "JBMono Nerd Font" :height 130)))))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
(column-number-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 34)
  (setq doom-modeline-bar-width 4))

(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package olivetti
  :ensure t
  :config
  (defun dysthesis/org-mode-setup ()
    (org-indent-mode)
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
  (setq mixed-pitch-face 'variable-pitch)
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

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package evil 
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode 1))

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-h") #'evil-window-left)
(global-set-key (kbd "C-j") #'evil-window-down)
(global-set-key (kbd "C-k") #'evil-window-up)
(global-set-key (kbd "C-l") #'evil-window-right)
(global-set-key (kbd "TAB") #'evil-toggle-fold)

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)) ;; globally enable evil-surround

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer dysthesis/leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix "SPC" ;; set leader
			  :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer dysthesis/local-leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix "," ;; set local leader
			  :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
   "C-x C-r"   ;; unbind find file read only
   "C-x C-z"   ;; unbind suspend frame
   "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click


  (dysthesis/leader-keys
   "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
   "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (dysthesis/leader-keys
   "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (dysthesis/leader-keys
   "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (dysthesis/leader-keys
   "h" '(:ignore t :wk "help"))

  ;; file
  (dysthesis/leader-keys
   "f" '(:ignore t :wk "file")
   "ff" '(find-file :wk "find file") ;; gets overridden by consult
   "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (dysthesis/leader-keys
   "b" '(:ignore t :wk "buffer")
   "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
   "bk" '(kill-this-buffer :wk "kill this buffer")
   "br" '(revert-buffer :wk "reload buffer"))

  ;; bookmark
  (dysthesis/leader-keys
   "B" '(:ignore t :wk "bookmark")
   "Bs" '(bookmark-set :wk "set bookmark")
   "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (dysthesis/leader-keys
   "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (dysthesis/leader-keys
   "n" '(:ignore t :wk "notes")
   ;; see org-roam and citar sections
   "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (dysthesis/leader-keys
   "c" '(:ignore t :wk "code"))

  ;; open
  (dysthesis/leader-keys
   "o" '(:ignore t :wk "open")
   "os" '(speedbar t :wk "speedbar")
   "op" '(elpaca-log t :wk "elpaca"))


  ;; search
  ;; see 'consult'
  (dysthesis/leader-keys
   "s" '(:ignore t :wk "search"))

  ;; templating
  ;; see 'tempel'
  (dysthesis/leader-keys
   "t" '(:ignore t :wk "template")))

;; "c" '(org-capture :wk "capture")))

(use-package avy
    :ensure t
    :init
(defun patrl/avy-action-insert-newline (pt)
      (save-excursion
	(goto-char pt)
	(newline))
      (select-window
       (cdr
	(ring-ref avy-ring 0))))
    (defun patrl/avy-action-kill-whole-line (pt)
      (save-excursion
	(goto-char pt)
	(kill-whole-line))
      (select-window
       (cdr
	(ring-ref avy-ring 0))))
    (defun patrl/avy-action-embark (pt)
      (unwind-protect
	  (save-excursion
	    (goto-char pt)
	    (embark-act))
	(select-window
	 (cdr (ring-ref avy-ring 0))))
      t) ;; adds an avy action for embark
    :general
    (general-def '(normal motion)
      "s" 'evil-avy-goto-char-timer
      "f" 'evil-avy-goto-char-in-line
      "gl" 'evil-avy-goto-line ;; this rules
      ";" 'avy-resume)
    :config
    (setf (alist-get ?. avy-dispatch-alist) 'patrl/avy-action-embark ;; embark integration
	  (alist-get ?i avy-dispatch-alist) 'patrl/avy-action-insert-newline
	  (alist-get ?K avy-dispatch-alist) 'patrl/avy-action-kill-whole-line)) ;; kill lines with avy

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
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex                       ; Basically fuzzy finding
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
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

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Projects")
    (setq projectile-project-search-path '("~/Documents/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :ensure t
  :after magit)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config (setq rustic-lsp-client 'eglot))

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4)
  :config
  (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :foreground "#ffffff" :height 1.4 :weight bold))))
     '(org-level-2 ((t (:inherit outline-2 :foreground "#ffffff" :height 1.2 :weight bold))))
     '(org-level-3 ((t (:inherit outline-3 :foreground "#ffffff" :height 1.1 :weight bold))))
     '(org-level-4 ((t (:inherit outline-4 :foreground "#ffffff" :height 1.0 :weight bold))))
     '(org-level-5 ((t (:inherit outline-5 :foreground "#ffffff" :height 0.9 :weight bold))))
     (set-face-attribute 'org-document-title nil :foreground "#ffffff" :height 2.0)))
(require 'org-indent)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :foreground "#ffffff" :height 1.4 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "#ffffff" :height 1.2 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :foreground "#ffffff" :height 1.1 :weight bold))))
   '(org-level-4 ((t (:inherit outline-4 :foreground "#ffffff" :height 1.0 :weight bold))))
   '(org-level-5 ((t (:inherit outline-5 :foreground "#ffffff" :height 0.9 :weight bold))))
   (set-face-attribute 'org-document-title nil :foreground "#ffffff" :height 2.0))

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
  (org-indent-mode)
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
  (setq org-modern-fold-stars '((" 󰫈 " . " 󰫈 ") (" 󰫇 " . " 󰫇 ") (" 󰫆 " . " 󰫆 ") (" 󰫅 " . " 󰫅 ") (" 󰫄 " . " 󰫄 ") (" 󰫃 " . " 󰫃 ")))
  (setq org-modern-block-name
  	'((t . t)
            ("src" "»" "«")
            ("example" "»–" "–«")
            ("quote" "" "")
            ("export" "⏩" "⏪")))
  (setq org-modern-block-fringe 6)
  
  (setq org-ellipsis " ↪")
  (global-org-modern-mode)
  (setq org-pretty-entities t))

(setq org-ellipsis " ↪")

(setq org-modern-fold-stars '((" 󰫈 " . " 󰫈 ") (" 󰫇 " . " 󰫇 ") (" 󰫆 " . " 󰫆 ") (" 󰫅 " . " 󰫅 ") (" 󰫄 " . " 󰫄 ") (" 󰫃 " . " 󰫃 ")))

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
