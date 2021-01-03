;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mark Bluemer"
      user-mail-address "mark@theblue.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! "C-s" #'+default/search-buffer)
(after! lsp-pyright
  (setq lsp-pyright-typechecking-mode "off"))

(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp 'python-flake8)))

;; Disable line numbers in certain modes
(add-hook! org-mode #'doom-disable-line-numbers-h)

;; Set up magit and projectile repository confguration
(after! magit
  (setq magit-repository-directories '(("~/code/" . 3)
                                       ("~/.dotfiles/" . 1))))
(after! projectile
  (setq projectile-project-search-path '("~/code/")))

;; Fix autocomplete the way I like it
(defun mb/company-complete-selection ()
  "Insert the selected candidate or the first if none are selected."
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))
(after! company
  (map! :map company-active-map "<tab>" #'mb/company-complete-selection)
  (map! :map lsp-mode-map "<tab>" #'company-indent-or-complete-common))

;; Some org Configuration
(after! org
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-structure-template-alist
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
                  ("v" . "verse"))
                org-agenda-files '("~/org")
                org-log-done t
                org-capture-templates '(("t" "Todo [inbox]" entry
                                         (file+headline "~/org/inbox.org" "Inbox")
                                         "* TODO %i%?\n:PROPERTIES:\n:CreatedOn: %U\n:END:")
                                        ("T" "Tickler" entry
                                         (file+headline "~/org/tickler.org" "Tickler")
                                         "* %i%? \n %U")
                                        ("b" "Bookmark" entry
                                         (file+headline "~/org/lists.org" "Bookmarks")
                                         "* %?\n:PROPERTIES:\n:CreatedOn: %U\n:END:\n\n" :empty-lines 1)
                                        ("j" "Journal" entry
                                         (file+olp+datetree +org-capture-journal-file)

                                         "* %U %?\n%i\n%a" :prepend t)
                                        ("p" "Templates for projects")
                                        ("pt" "Project-local todo" entry
                                         (file+headline +org-capture-project-todo-file "Inbox")
                                         "* TODO %?\n%i\n%a" :prepend t)
                                        ("pn" "Project-local notes" entry
                                         (file+headline +org-capture-project-notes-file "Inbox")
                                         "* %U %?\n%i\n%a" :prepend t)
                                        ("pc" "Project-local changelog" entry
                                         (file+headline +org-capture-project-changelog-file "Unreleased")
                                         "* %U %?\n%i\n%a" :prepend t)
                                        ("o" "Centralized templates for projects")
                                        ("ot" "Project todo" entry
                                         #'+org-capture-central-project-todo-file
                                         "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                        ("on" "Project notes" entry
                                         #'+org-capture-central-project-notes-file
                                         "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                        ("oc" "Project changelog" entry
                                         #'+org-capture-central-project-changelog-file
                                         "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
                org-tag-alist '(("@home" . ?w)
                                ("@errand" . ?e)
                                ("@computer" . ?c)
                                ("@phone" . ?p))
                org-refile-targets '(("~/org/gtd.org" :maxlevel . 2)
                                     ("~/org/someday.org" :level . 1)
                                     ("~/org/tickler.org" :maxlevel . 2)))
  (map! :map evil-org-mode-map
        :after evil-org
        :n "C-j" #'org-next-visible-heading
        :n "C-k" #'org-previous-visible-heading))
(add-hook! org-mode org-bullets-mode)

;; Fill the column so we have centered text
(defun mb/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mb/org-mode-visual-fill))
