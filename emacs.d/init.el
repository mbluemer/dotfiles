;;; This is my Emacs configuration!
;;
;; Inspiration has been heavily borrowed from the following sources:
;; * https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
;; * https://gist.github.com/martinklepsch/4e5f2c52a5d9797278d1
;; * https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ
;; * https://emacs.stackexchange.com/a/32635

;; Prepare standard package repositories
(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;; Get use-package in here
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure))
(setq use-package-always-ensure t)

;; Change some visual settings
(if (display-graphic-p)
    (progn
      (set-fringe-mode 10)
      (scroll-bar-mode -1)))
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Enable line numbers, disable for certain modes
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Install packages!
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-classic t))
(use-package powerline
  :config
  (powerline-center-evil-theme))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package mode-line-bell
  :config (mode-line-bell-mode))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mb/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer mb/evil-leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix ","))
(use-package ivy
  :config
  (ivy-mode 1))
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
(use-package ivy-rich
  :init (ivy-rich-mode 1))

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

(use-package projectile
  :after magit
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

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
(use-package magit
  :init
  (setq magit-repository-directories '(("~/code/" . 3)
				       ("~/.dotfiles/" . 1))))

(use-package org)

;; Create some custom keybindings!
(mb/leader-key-def
 "x" '(counsel-M-x :which-key "Counsel execute function"))
(mb/evil-leader-key-def
  "w" '(save-buffer :which-key "Save current buffer"))

(mb/leader-key-def
 "ff" '(counsel-find-file :which-key "Counsel find file")
 "fd" '(delete-file :which-key "Delete file"))
(mb/leader-key-def
 "bb" '(counsel-ibuffer :which-key "Counsel switch buffer"))

(use-package yaml-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode magit helpful which-key counsel-projectile counsel projectile evil-collection evil ivy-rich ivy-posframe ivy general mode-line-bell rainbow-delimiters powerline doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
