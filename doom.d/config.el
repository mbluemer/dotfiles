;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Set contact information
(setq user-full-name "Mark Bluemer"
      user-mail-address "mark@theblue.dev")

;; Theming
(setq doom-theme 'doom-flatwhite)
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14))

;; Set line numbers
(setq display-line-numbers-type t)
;; Disable line numbers in certain modes thought
(add-hook! org-mode #'doom-disable-line-numbers-h)

;; Remap what search buffer appears with Emacs' default
(map! "C-s" #'+default/search-buffer)

;; LSP Configuration
(setq lsp-enable-file-watchers nil)

;; LSP Python setup
(after! lsp-pyright
  (setq lsp-pyright-typechecking-mode "off"))
(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp 'python-flake8)))
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

;; Magit and Projectile repository confguration
(after! magit
  (setq magit-repository-directories '(("~/code/" . 3)
                                       ("~/.dotfiles/" . 1))))
(after! (:and projectile magit)
  (mapc #'projectile-add-known-project
        (mapcar #'file-name-as-directory (magit-list-repos)))
  (projectile-save-known-projects))

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


;; Org configuration
(after! org
  (setq org-directory "~/org/"
        org-hide-emphasis-markers t
        org-ellipsis " ▾")

  ;; Org templates
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

  ;; Org Agenda basic configuration
  (setq org-agenda-files '("~/org")
        org-log-done t
        org-log-into-drawer t)

  ;; Org Agenda capture templates and refiling
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
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

           "* %U %?\n%i\n%a" :prepend t))
        org-refile-targets '(("~/org/work.org" :maxlevel . 2)
                             ("~/org/personal.org" :maxlevel . 2)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))


  ;; Org Tags
  (setq org-tag-alist '(("@home" . ?h)
                        ("@errand" . ?e)
                        ("@computer" . ?c)
                        ("@phone" . ?p)))

  ;; Org agenda todo states and faces
  (setq org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . "#e76f51")
                                ("NEXT" . "#d62828")
                                ("STARTED" . "#2a9d8f")
                                ("WAITING" . "#e9c46a")
                                ("DONE" . "#264653")
                                ("CANCELLED" . "#264653")))
  (setq org-clock-in-switch-to-state "STARTED")

  ;; Org agenda defaults
  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  ;; Create a custom agenda
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\"-TODO=\"DONE\"-TODO=\"CANCELLED\""
                ((org-agenda-overriding-header "High-priority tasks:")))
          (agenda "" ((org-agenda-span 3)
                      (org-agenda-start-day "+0d")))
          (tags "CATEGORY=\"Work\"/WAITING"
                ((org-agenda-skip-function
                  '(org-agenda-skip-if nil '(scheduled deadline)))
                 (org-agenda-sorting-strategy '(priority-down todo-state-down))
                 (org-agenda-overriding-header "Waiting")))
          (tags "-PRIORITY=\"A\"+CATEGORY=\"Work\"/!+TODO|+NEXT|+STARTED"
                ((org-agenda-skip-function
                  '(org-agenda-skip-if nil '(scheduled deadline)))
                 (org-agenda-sorting-strategy '(priority-down todo-state-down))
                 (org-agenda-overriding-header "Next Work Tasks")))
          (tags "-REFILE-PRIORITY=\"A\"-CATEGORY=\"Work\"/!+TODO|+NEXT|+STARTED"
                ((org-agenda-skip-function
                  '(org-agenda-skip-if nil '(scheduled deadline)))
                 (org-agenda-sorting-strategy '(priority-down todo-state-down timestamp-down category-up))
                 (org-agenda-overriding-header "Personal Tasks")))
          (tags "CATEGORY=\"Habits\"/TODO"
                ((org-agenda-overriding-header "Habits")))))))

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; A little bit of remapping
  (map! :map evil-org-mode-map
        :after evil-org
        :n "C-j" #'org-next-visible-heading
        :n "C-k" #'org-previous-visible-heading))

(after! hl-todo
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces '(("TODO" . "#e76f51")
                                ("NEXT" . "#d62828")
                                ("STARTED" . "#2a9d8f")
                                ("WAITING" . "#e9c46a")
                                ("DONE" . "#264653")
                                ("CANCELLED" . "#264653"))))
(add-hook! org-mode org-bullets-mode)
