(setq use-package-always-ensure t)

;; NAVIGATION
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package helm
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

(use-package which-key
  :config (which-key-mode))

(use-package popwin
  :config (popwin-mode))

;; ORG
(use-package org
  :init
  (setq org-modules '(org-habit)
	org-startup-truncated nil
	org-blank-before-new-entry nil))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :init
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; KEYBINDINGS
(use-package general
  :config (general-evil-setup t))

;; THEME
(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package powerline
  :config (powerline-default-theme))
