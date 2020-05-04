(general-create-definer leader
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(leader
  :states '(normal visual insert emacs)
  :keymaps 'override
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
  "b["  '(previous-buffer :which-key "previous buffer")
  "b]"  '(next-buffer :which-key "next buffer")
  "bd" '(kill-buffer :which-key "kill buffer")
  "be" '(eval-buffer :which-key "eval buffer")
  ;; Files
  "ff" '(helm-find-files :which-key "find files")
  ;; Help
  "hf" '(describe-function :which-key "describe function")
  ;; Org
  "oc" '(org-capture :which-key "org capture")
  "oa" '(org-agenda :which-key "org agenda"))

;; org-mode-map overrides
(nvmap
  :keymaps 'org-mode-map
  :prefix "g"
  "j" 'org-next-visible-heading
  "k" 'org-previous-visible-heading)
