(tool-bar-mode 0)
(menu-bar-mode 0)
(fido-mode)
(setq mac-option-modifier 'control
      mac-command-modifier 'meta)
(global-set-key (kbd "C-x i") (lambda () (interactive) (find-file "~/org-custom/init.el")))
(setq inhibit-splash-screen t)
(global-set-key (kbd "C-c i") 'completion-at-point)

(grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

(setq make-backup-files nil) ; I use git instead

;; iSpell for spell check
; Skip UUIDs in org mode
(add-to-list 'ispell-skip-region-alist '("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}"))
; Skip link locations in org mode (often HTTP URLs)
(add-to-list 'ispell-skip-region-alist '("\\[\\[" . "\\]\\["))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

; fetch the list of packages available. I have commented this out to avoid needlessly pinging melpa etc on startup
 (unless package-archive-contents
  (package-refresh-contents))

(setq package-list
      '(avy
	org-roam
	magit
	modus-themes))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(package-initialize)

(load-theme 'modus-operandi t)

;; Avy Settings
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

;; Org Configuration
(require 'org)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c s") 'org-store-link)
(define-key org-mode-map (kbd "C-c j") (lambda () (interactive) (org-refile 1)))

(setq org-directory "~/org")

; Using personal dictionary
(setq ispell-personal-dictionary (concat org-directory "/.aspell.en.pws"))

; org-roam
(setq org-roam-directory (concat org-directory "/roam/")
      org-roam-dailies-directory "daily/"
      org-roam-dailies-capture-templates '(("d" "default" entry
					    "* %?"
					    :target (file+head "%<%Y-%m-%d>.org"
							       "#+title: %<%Y-%m-%d>\n"))))
(global-set-key (kbd "C-c r t") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c r i") 'org-roam-node-insert)
(global-set-key (kbd "C-c r r") 'org-roam-buffer)
(global-set-key (kbd "C-c r f") 'org-roam-node-find)

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-id)

(setq org-agenda-files (list org-directory)
      org-refile-targets '((nil :maxlevel . 10))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-tags-column 0
      org-use-speed-commands t
      org-speed-commands-user '(("g" . '(org-refile 1))
				("d" . org-deadline)
				("s" . org-schedule)
				("x" . org-capture))
      org-agenda-custom-commands '(("n" "Next Actions" todo "NEXT")
				   ("r" "Schedule and NEXT" ((agenda "" ((org-agenda-span 'day)))
							     (todo "NEXT"))))
      org-startup-indented t
      org-link-frame-setup '((file . find-file)) ; opens links to org file in same window
      org-indent-mode-hides-stars t
      org-agenda-dim-blocked-tasks nil ;; Disabled because I am using NEXT
      org-agenda-todo-ignore-scheduled `future
      org-agenda-todo-ignore-time-comparison-use-seconds t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-deadline-prewarning-if-done t
      org-log-into-drawer t
      org-log-done `time
      org-agenda-show-future-repeats nil
      org-journal-date-format "%A, %D"
      org-todo-keywords '((sequence "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "KILL(k@)")
			  (type "PROJ(p)" "HOLD(h)" "IDEA(i)" "TODO(t)" "|"))
      org-todo-keyword-faces `(("NEXT" . (:background ,(modus-themes-color 'green-subtle-bg)))
			       ("DONE" . (:background ,(modus-themes-color 'bg-special-calm)))
			       ("KILL" . (:background ,(modus-themes-color 'red-subtle-bg)))
			       ("PROJ" . (:background ,(modus-themes-color 'blue-subtle-bg)))
			       ("WAIT" . (:background ,(modus-themes-color 'yellow-subtle-bg)))
			       ("HOLD" . (:background ,(modus-themes-color 'yellow-subtle-bg)))
			       ("IDEA" . (:background ,(modus-themes-color 'yellow-subtle-bg))))
      org-habit-show-all-today nil
      org-id-link-to-org-use-id t)

(setq org-capture-templates '(("i" "inbox" entry
			       (file "inbox.org")
			       "* %u %?" :prepend t)
			      ("n" "Next Action" entry
			       (file "tasks.org")
			       "* NEXT %?")
			      ("j" "Journal note" entry
			       (file+olp+datetree "journal.org")
			       "* %U %?\n%i\n%a\n** NEXT Process this note" :jump-to-captured t)
			      ("J" "Journal" entry
			       (file+olp+datetree "journal.org")
			       "* %U %?\n%i\n%a" :jump-to-captured t)
			      ("g" "Grocery" checkitem
			       (file+headline "tasks.org" "Shopping List"))
			      ("c" "Currently clocked-in" item (clock)
			       "Note taken on %U \\\\ \n%?"
			       :prepend t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73" "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db" default))
 '(package-selected-packages '(modus-themes which-key org-roam god-mode avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; God mode configuration
;; I do not know if I want to use god mode or not, but if I do, this is the config I may use
;; (defun my-god-mode-update-cursor-type ()
  ;; (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
;; (global-set-key (kbd "<escape>") #'god-mode-all)

