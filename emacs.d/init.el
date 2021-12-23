;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; https://labs.phundrak.com/phundrak/dotfiles/src/branch/master/org/config/emacs.org

;; Set up straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Some core system stuff
;; backup in one place. flat, no tree structure
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(setq create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default t)

(setq user-full-name       "Mark Bluemer"
      user-real-login-name "Mark Bluemer"
      user-login-name      "mbluemer"
      user-mail-address    "mark@theblue.dev")

(setq truncate-string-ellipsis "…")

;; Some visual and audio stuff
(setq ring-bell-function 'ignore)
(set-face-attribute 'default nil
		    :family "JetBrainsMono Nerd Font"
		    :height 140
		    :weight 'normal
		    :width 'normal)
(set-face-attribute 'fixed-pitch nil
		    :family "JetBrainsMono Nerd Font"
		    :height 140
		    :weight 'normal
		    :width 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "Source Code Sans Pro"
		    :height 140
		    :weight 'normal
		    :width 'normal)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Cleanup extra whitespace on save
(add-hook 'before-save-hook #'whitespace-cleanup)

(column-number-mode)
(setq display-time-format "%Y-%m-%d %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))
(use-package doom-modeline
  :config (doom-modeline-mode 1))

;; Set up evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Disable some evil keybindings that are getting in the way
  (evil-global-set-key 'insert (kbd "C-n") nil)
  (evil-global-set-key 'insert (kbd "C-p") nil)
  ;; navigate on visual lines rather than regular lines
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :config (which-key-mode 1))
(use-package vertico
  :init
  (vertico-mode))
(use-package consult)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
	(which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
	   "Become"
	 (format "Act on %s '%s'%s"
		 (plist-get (car targets) :type)
		 (embark--truncate-target (plist-get (car targets) :target))
		 (if (cdr targets) "…" "")))
       (if prefix
	   (pcase (lookup-key keymap prefix 'accept-default)
	     ((and (pred keymapp) km) km)
	     (_ (key-binding prefix 'accept-default)))
	 keymap)
       nil nil t (lambda (binding)
		   (not (string-suffix-p "-argument" (cdr binding))))))))
(use-package embark
  :custom
  (embark-indicators
   '(embark-which-key-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :bind
  (("C-." . embark-act)))
(use-package embark-consult
  :after (embark consult))
(use-package savehist
  :init
  (savehist-mode))
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))
(use-package marginalia
  :init
  (marginalia-mode))

(use-package lsp-mode
  :init
  (setq lsp-pylsp-plugins-pylint-enabled t)
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui)
;(use-package company
;  :init
;  (add-hook 'after-init-hook 'global-company-mode))

(use-package general
  :config
  (general-evil-setup))

(use-package corfu
  :init
  (corfu-global-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  :general
  (:keymaps 'corfu-map
	    "C-n" 'corfu-next
	    "C-p" 'corfu-previous))


(use-package flycheck
  :init (global-flycheck-mode))

(defun mb/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(defun mb/org-font-setup ()
    ;; Set faces for heading levels
    (let* ((variable-tuple
	    (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
		((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
		((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
		((x-list-fonts "Verdana")         '(:font "Verdana"))
		((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
		(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	    (base-font-color     (face-foreground 'default nil 'default))
	    (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
	'user
	'(org-level-8 ((t (,@headline ,@variable-tuple))))
	'(org-level-7 ((t (,@headline ,@variable-tuple))))
	'(org-level-6 ((t (,@headline ,@variable-tuple))))
	'(org-level-5 ((t (,@headline ,@variable-tuple))))
	'(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
	'(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
	'(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
	'(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
	'(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
	'(org-block ((t (:inherit fixed-pitch))))
	'(org-code ((t (:inherit (shadow fixed-pitch)))))
	'(org-table ((t (:inherit (shadow fixed-pitch)))))
	'(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
	'(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	'(org-checkbox ((t (:inherit fixed-pitch)))))))

(use-package org
  :hook (org-mdoe . mb/org-mode-setup)
  :config
  (mb/org-font-setup))
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-journal
  :custom
  (org-journal-dir "~/org/journal"))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))
