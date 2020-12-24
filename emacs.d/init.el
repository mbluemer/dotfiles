(if (display-graphic-p)
(progn
(set-fringe-mode 10)
(scroll-bar-mode -1)))
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq backup-directory-alist `(("." . "~/.saves")))

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
     (swiper . nil)
     (swiper-isearch . nil)
     (t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))

(mb/leader-key-def
 "x" '(counsel-M-x :which-key "Counsel execute function")
 ;; File completions
 "ff" '(counsel-find-file :which-key "Counsel find file")
 "fd" '(delete-file :which-key "Delete file")
 ;; Buffer commands
 "bb" '(counsel-ibuffer :which-key "Counsel switch buffer"))

(use-package ripgrep)

(use-package mode-line-bell
  :config (mode-line-bell-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-classic t))
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook))
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
