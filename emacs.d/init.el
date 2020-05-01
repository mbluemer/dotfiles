(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Backups configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Vim mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :init
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-candidate=number-list 50
	; Taken from https://gist.github.com/antifuchs/9238468
	helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                    ; reeeelatively quickly.
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t))

;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Org mode
(use-package org
  :ensure t
  :init
  (setq org-modules '(org-habit)))

;; Use General for keybindings
(use-package general
  :ensure t
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "SPC" '(helm-M-x :which-key "M-x")
   ;; Windows
   "wl" '(windmove-right :which-key "move right")
   "wh" '(windmove-left :which-key "move left")
   "wj" '(windmove-down :which-key "move down")
   "wk" '(windmove-up :which-key "move up")
   "w%" '((lambda () (interactive) (progn (split-window-right) (windmove-right))) :which-key "split right")
   "w\"" '((lambda () (interactive) (progn (split-window-below) (windmove-down))) :which-key "split down")
   "wd" '(delete-window :which-key "delete window")
   ;; Buffers
   "bb"  '(helm-buffers-list :which-key "buffers list")
   "bd" '(kill-buffer :which-key "kill buffer")
   "be" '(eval-buffer :which-key "eval buffer")
   ;; Files
   "ff" '(helm-find-files :which-key "find files")
 ))

;; Monokai theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Minimal UI
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-display-line-numbers-mode t)

;; Random other things
(fset 'yes-or-no-p 'y-or-n-p)
