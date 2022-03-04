;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Patrick Salmers"
      user-mail-address "patricksalmers@gmail.com")

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
(setq doom-gruvbox-dark-variant 'soft)
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(after! org
  (setq org-agenda-dim-blocked-tasks nil) ;; Disabled because I am using NEXT
  (setq org-agenda-todo-ignore-scheduled `future)
  (setq org-log-into-drawer t)
  (setq org-log-done `time)
  (setq org-agenda-show-future-repeats nil)
  (setq org-startup-folded t)
  (setq org-journal-date-format "%A, %D")

  (setq org-todo-keywords '((sequence "TODO(t) NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "KILL(k@)")
                            (type "PROJ(p)" "HOLD(h)" "IDEA(i)" "|")))

  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)

  (add-to-list 'org-modules 'org-id)
  (setq org-id-link-to-org-use-id t)

  (add-to-list 'org-modules 'org-checklist)

  (map! :map org-mode-map
        :localleader
        "S" #'org-save-all-org-buffers
        "z" #'org-add-note
        "RET" #'+org/insert-item-below
        "B" #'org-revert-all-org-buffers
        )
  (map! :map org-agenda-mode-map
        :localleader
        "S" #'org-save-all-org-buffers
        "a" #'org-add-note
        "l" #'org-agenda-log-mode
        )
  (map! :map doom-leader-open-map
        :prefix ("a" . "org agenda")
         :desc "Agenda" "A" #'org-agenda
         :desc "Next Actions" "n" (cmd! (org-todo-list 1))
         :desc "Weekly Agenda" "a" #'org-agenda-list)
  (map! :leader "x" #'org-capture)
  (setq! org-capture-templates '(("i" "inbox" entry
                                  (file "inbox.org")
                                  "* %u %?" :prepend t)
                                 ("n" "Next Action" entry
                                  (file "tasks.org")
                                  "* NEXT %?")
                                 ("g" "Grocery" checkitem
                                  (file+headline "tasks.org" "Shopping List"))
                                 ("c" "Currently clocked-in" item (clock)
                                  "Note taken on %U \\\\ \n%?"
                                  :prepend t))))

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; (set-email-account! "patricksalmers@gmail.com"
;;   '((mu4e-sent-folder       . "/patricksalmers@gmail.com/Sent Mail")
;;     (smtpmail-smtp-user     . "patricksalmers@gmail.com")
;;     (user-mail-address      . "patricksalmers@gmail.com")    ;; only needed for mu < 1.4
;;     (mu4e-compose-signature . "---\nPatrick Salmers"))
;;   t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)

;; Keybinds

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

(after! evil
  (undefine-key! evil-normal-state-map "DEL")
  (evilem-default-keybindings "DEL")
  )
