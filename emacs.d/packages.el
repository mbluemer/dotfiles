;; EVIL 
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
  (general-evil-setup t))

;; Monokai theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))
