(if window-system (tool-bar-mode 0) nil)
(menu-bar-mode 0)
(fido-vertical-mode)
(setq mac-option-modifier 'control
      mac-command-modifier 'meta)
(global-set-key (kbd "C-x i") (lambda () (interactive) (find-file "~/org-custom/init.el")))
(setq inhibit-splash-screen t)
(global-set-key (kbd "C-c i") 'completion-at-point)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-.") 'repeat)

(setq calendar-week-start-day 1)

;; Keeps customizations from polluting my init
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; For acronyms I will use all caps, and for code I will use src blocks. So I will use the more natural sentence ending.
(setq sentence-end-double-space nil)

(setq
   grep-find-command
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
	plantuml-mode
	magit
	org-roam ; Roam is a bit heavyweight, but I know it supports all the workflows I need
	; visual-fill-column ; This mode does not indent the fill column in org-indent mode
       ))

(package-initialize)

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(package-initialize)

(load-theme 'modus-operandi t)

;; Avy Settings
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; Org Configuration
(require 'org)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c s") 'org-store-link)
(define-key org-mode-map (kbd "C-c j") (lambda () (interactive) (org-refile 1)))

(setq org-directory "~/org")

; Using personal dictionary
(setq ispell-personal-dictionary (concat org-directory "/.aspell.en.pws"))


;; Org Roam settings
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
				   ("d" "Schedule and NEXT" ((agenda "" ((org-agenda-span 'day)))
							     (todo "NEXT"))))
      org-startup-indented t
      org-link-frame-setup '((file . find-file)) ; opens links to org file in same window
      org-indent-mode-hides-stars t
      org-attach-store-link-p t
      org-attach-auto-tag nil
      org-attach-id-dir (concat org-directory "/data/")
      org-image-actual-width (list 500)
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
			      ("s" "Sleep Journal" plain
			       (file+olp+datetree "sleep-journal.org")
			       "- start-finish of attempt :: %?\n- medicine used :: \n- Restedness 1-10 :: ")
			      ("f" "Fitness Journal" plain
			       (file+olp+datetree "fitness-journal.org")
			       "- Activity :: %?\n- start-finish :: \n- Avg HR :: ")
			      ("g" "Grocery" checkitem
			       (file+headline "tasks.org" "Shopping List"))
			      ("c" "Currently clocked-in" item (clock)
			       "Note taken on %U \\\\ \n%?"
			       :prepend t)))

;; Plant UML Setup
;; active Org-babel languages
(require 'plantuml-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path (expand-file-name "~/src/plantuml.jar")
      plantuml-default-exec-mode 'jar
      plantuml-jar-path "~/src/plantuml.jar")
;; This value will also need to be passed as a :java header argument to plantuml src blocks
(add-to-list 'plantuml-java-args "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
(setq org-babel-default-header-args:plantuml
      (cons '(:java . "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
	    (assq-delete-all :java org-babel-default-header-args:plantuml)))
