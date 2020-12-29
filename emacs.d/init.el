(if (display-graphic-p)
    (progn
      (set-fringe-mode 10)
      (scroll-bar-mode -1)))
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq backup-directory-alist `(("." . "~/.saves")))

(defvar mb/default-font-size 120)
(defvar mb/default-variable-font-size 140)

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height mb/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height mb/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Georgia" :height mb/default-variable-font-size :weight 'regular)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(require 'package)

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mb/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer mb/evil-leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,"))

(use-package which-key
  :init (which-key-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun mb/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(mb/evil-leader-key-def
  "w" '(save-buffer :which-key "Save buffer")
  "x" '(mb/kill-this-buffer :which-key "Kill current buffer"))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-posframe
  :ensure t
  :delight
  :custom
  (ivy-posframe-parameters
   '((left-fringe . 2)
     (right-fringe . 2)
     (internal-border-width . 2)))
  (ivy-posframe-height-alist
   '((swiper . 15)
     (swiper-isearch . 15)
     (t . 10)))
  (ivy-posframe-display-functions-alist
   '((complete-symbol . ivy-posframe-display-at-point)
     (swiper . ivy-display-function-fallback)
     (swiper-isearch . ivy-display-function-fallback)
     (t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))

(mb/leader-key-def
  "x" '(counsel-M-x :which-key "Counsel execute function")
  ;; File completions
  "ff" '(counsel-find-file :which-key "Counsel find file")
  "fd" '(delete-file :which-key "Delete file")
  ;; Buffer commands
  "bb" '(counsel-ibuffer :which-key "Counsel switch buffer")
  "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer"))

(use-package ripgrep)

(use-package mode-line-bell
  :config (mode-line-bell-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package yaml-mode)
(use-package json-mode)
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist
               '("Dockerfile\\'" . dockerfile-mode)))

(use-package magit
  :init
  (setq magit-repository-directories '(("~/code/" . 3)
                                       ("~/.dotfiles/" . 1))))

(use-package projectile
  :after magit
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  (mapc #'projectile-add-known-project
        (mapcar #'file-name-as-directory (magit-list-repos)))
  (projectile-save-known-projects))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(mb/leader-key-def
  "p" '(projectile-command-map :which-key "Projectile commands"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-completion-provider :capf)
  :config
  (lsp-enable-which-key-integration t))
(use-package lsp-ui
  :commands (lsp-ui-mode))
(use-package lsp-treemacs
  :after lsp)

(defun mb/company-complete-selection ()
  "Insert the selected candidate or the first if none are selected."
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . mb/company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "src emacs-lisp")
        ("p" . "src python")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")))

(defun mb/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Georgia" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun mb/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(use-package org
  :pin manual
  :hook (org-mode . mb/org-mode-setup)
  :config
  (require 'org-tempo)
  (mb/org-font-setup)
  :custom
  (org-ellipsis " ▾")
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(defun mb/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mb/org-mode-visual-fill))

(defun mb/org-insert-heading-below ()
    (interactive)
    (org-insert-heading-respect-content)
    (evil-insert-state))
(defun mb/org-insert-todo-heading-below ()
    (interactive)
    (org-insert-todo-heading-respect-content)
    (evil-insert-state))

(general-define-key
 :states '(normal emacs motion)
 :keymaps 'org-mode-map
 "C-j" 'org-next-visible-heading
 "C-k" 'org-previous-visible-heading
 [remap org-insert-heading-respect-content] 'mb/org-insert-heading-below
 [remap org-insert-todo-heading-respect-content] 'mb/org-insert-todo-heading-below
 ">>" 'org-demote-subtree
 "<<" 'org-promote-subtree)

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-babel-tangle :append :local)))

(mb/evil-leader-key-def
  "c" '(org-capture :which-key "Org capture")
  "a" '(org-agenda :which-key "Org agenda")
  "l" '(org-store-link :which-key "Org store link"))

(setq org-agenda-files '("~/org"))
(setq org-log-done t)
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/inbox.org" "Inbox")
                               "* TODO %i%?\n:PROPERTIES:\n:CreatedOn: %U\n:END:")
                              ("T" "Tickler" entry
                               (file+headline "~/org/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ("b" "Bookmark" entry
                               (file+headline "~/org/lists.org" "Bookmarks")
                               "* %?\n:PROPERTIES:\n:CreatedOn: %U\n:END:\n\n" :empty-lines 1)))
(setq org-tag-alist '(("@home" . ?w)
                      ("@errand" . ?e)
                      ("@computer" . ?c)
                      ("@phone" . ?p)))


(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 2)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))

(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . evil-insert-state))
