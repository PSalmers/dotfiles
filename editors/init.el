;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Installs packages that cannot be installed via use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

; fetch the list of packages available. I have commented this out to avoid needlessly pinging melpa etc on startup
;(unless package-archive-contents
; (package-refresh-contents))

(setq package-list
      '(use-package))
(package-initialize)
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Sets directory from which others are derived
(setq root-dir (if (eq system-type 'windows-nt) "C:/Users/patri/" "~/"))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Visual tweaks to the UI
(setq echo-keystrokes 0.01)
(setq visible-bell t)

;; Gruvbox is easy for my coloublind self to parse, and well-supported
;; I keep coming back to it. Should just stick with it.
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

;; Don't try to disable tool-bar if there is no toolbar, or you will have a bad time
(if window-system (tool-bar-mode 0) nil)

(menu-bar-mode 0)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Mouse

;; Enable mouse if in terminal. Useful for SSH situations.
(xterm-mouse-mode 1)

(setq mouse-drag-and-drop-region t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; CUA Mode

;; I gain nothing from cua-mode because I often switch between mac, linux, and windows, so my copy-paste hotkeys are changing frequently anyways. Additionally, I find that cua-mode conflicts with org-mode too much. There are replacement hotkeys but I find them confusing. Overall, it has not at all been a boon to me to use cua-mode, and has sometimes gotten in my way.
;; (cua-mode 1)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Windmove
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<left>") 'windmove-left)
(global-set-key (kbd "C-c C-<up>") 'windmove-up)
(global-set-key (kbd "C-c C-<down>") 'windmove-down)
(setq windmove-create-window t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Dired
(setq dired-dwim-target t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Mac Hotkeys

;; When using a mac-ish board
(setq mac-command-modifier nil)

;; When usiong a normal ANSI or ISO board
(setq mac-option-modifier 'meta)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Custom global keys

;; Open init.el
(global-set-key (kbd "C-c fi") (lambda () (interactive) (find-file (concat root-dir ".emacs.d/init.el"))))

(global-set-key (kbd "C-c v") 'visual-line-mode)

;; Text editing tweaks
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c r") 'replace-string)
(setq sentence-end-double-space nil)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Tweaks to Emacs' auto file generation

(auto-save-visited-mode 1)
(setq make-backup-files nil) ; I use git instead

;; Keeps customizations from polluting my init
(setq custom-file (concat user-emacs-directory "/custom.el"))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Grep

(setq
   grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Spell Checking

(use-package ispell
  :after (org) ; Spellings are defined in my org project
  :config
					; Skip UUIDs in org mode
  (add-to-list 'ispell-skip-region-alist '("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}"))
					; Skip link locations in org mode (often HTTP URLs)
  (add-to-list 'ispell-skip-region-alist '("\\[\\[" . "\\]\\["))

					; Using personal dictionary
  (setq ispell-personal-dictionary (concat org-directory "/.aspell.en.pws")))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Completion
(use-package counsel
  :config
  (counsel-mode)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer))

(use-package ivy
  :config
  (ivy-mode))
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Swiper
(use-package swiper
  :config
  (global-set-key (kbd "C-c s") 'swiper))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Magit
(use-package magit)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Avy
(use-package avy
  :config
  (global-set-key (kbd "C-t") 'avy-goto-char-timer)
  (global-set-key (kbd "C-S-p") 'avy-goto-line-above)
  (global-set-key (kbd "C-S-n") 'avy-goto-line-below)
  (global-set-key (kbd "C-S-k") 'avy-kill-region)
  (defun psalm-set-avy-keys-colemak ()
    (interactive)
    (setq avy-keys '(?a ?r ?s ?t ?o ?i ?e ?n ?d ?h)))

  (defun psalm-set-avy-keys-qwerty ()
    (interactive)
    (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Dabbrev
(use-package popup)
(use-package fancy-dabbrev
  :after (popup)
  :config
  ;; Enable fancy-dabbrev previews everywhere:
  ;; (global-fancy-dabbrev-mode)
  (setq fancy-dabbrev-expansion-context 'almost-everywhere)

  ;; Let dabbrev searches ignore case and expansions preserve case:
  (setq dabbrev-case-distinction nil
	dabbrev-case-fold-search t
	dabbrev-case-replace nil)

  ;; I bind to the traditional dabbev bindings so that I do not interfere with TAB's behaviour
  (global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
  (global-set-key (kbd "C-M-/") 'fancy-dabbrev-backward))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org
(use-package org
  :after (counsel)
  :config
  
  ;; Requires needed for config
  (require 'org-id)
  (require 'org-list)
  (require 'org-agenda)
  
  ;; Basic global keys
  (global-set-key (kbd "C-c o c") 'org-capture)
  (global-set-key (kbd "C-c o s") 'org-store-link)
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o j") 'org-clock-goto)
  (global-set-key (kbd "C-c o g") 'counsel-org-goto-all)

  ;; Counsel goto but leave mark behind
  (defun my-org-goto () (interactive)
	 (org-mark-ring-push)
	 (org-refile 1))
  (define-key org-mode-map (kbd "C-c C-j") 'counsel-org-goto)

  ;; Org directory
  (setq org-directory (concat root-dir "org"))

  ;; Modules
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)

  ;; Org map
  (defun psalm/insert-header-eof () ""
    (interactive)
    (end-of-buffer)
    (insert "\n* "))
  (define-key org-mode-map (kbd "C-c RET") 'psalm/insert-header-eof)
  (define-key org-mode-map (kbd "C-c n") 'org-next-item)
  (define-key org-mode-map (kbd "C-c p") 'org-previous-item)

  ;; Org agenda map
  (define-key org-agenda-mode-map (kbd "s") 'org-agenda-schedule)
  (define-key org-agenda-mode-map (kbd "d") 'org-agenda-deadline)
  (define-key org-agenda-mode-map (kbd "i") 'org-agenda-clock-in)
  (define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "N") 'org-agenda-forward-block)
  (define-key org-agenda-mode-map (kbd "P") 'org-agenda-backward-block)

  ;; Journal file
  (setq journal-file-path (concat org-directory "/journal.org"))
  (defun psalm/org-journal-jump ()
    (interactive)
    (find-file journal-file-path)
    (org-show-all)
    (end-of-buffer))
  (global-set-key (kbd "C-c o $") 'psalm/org-journal-jump)

  ;; Agenda
  (setq org-agenda-files (list org-directory (concat org-directory "/agenda/")) ; excludes /archive by default
	org-agenda-custom-commands '(("n" "Next Actions" todo "NEXT")
				     ("d" "Schedule and NEXT" ((agenda "" ((org-agenda-span 'day)))
							       (todo "WAIT")
							       (todo "NEXT")))
				     ("j" "Journal" agenda "" ((org-agenda-span 'day)
							       (org-agenda-prefix-format "%-12t %s")
							       (org-agenda-start-with-log-mode "clockcheck")
							       (org-agenda-include-inactive-timestamps t))))
	org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4)
	org-deadline-warning-days 10000
	org-agenda-dim-blocked-tasks t
	org-agenda-todo-ignore-scheduled 'future
	org-agenda-todo-ignore-time-comparison-use-seconds t
	org-agenda-skip-deadline-prewarning-if-scheduled t
	org-agenda-skip-deadline-prewarning-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-show-future-repeats nil
		;; org-agenda-prefix-format '((agenda . "%i %?-12t% s")
	;; 				(todo . "")
	;; 				(tags . "")
	;; 				(search . " %i %-12:c"))
	org-habit-show-all-today nil)

  ;; Refile
  (setq org-refile-targets '((nil :maxlevel . 10)
			     (org-agenda-files :maxlevel . 10))
	org-refile-use-outline-path t)

  ;; Completion
  (setq	org-outline-path-complete-in-steps nil)
  
  ;; Tags
  (setq org-use-tag-inheritance nil
	org-tags-column 0
	org-tag-persistent-alist '(("decisions" . ?d)
				   ("references" . ?r)
				   ("solved_problems" . ?s)
				   ("obsolete" . ?o)))

  ;; Speed commands
  (setq	org-use-speed-commands t
	org-speed-commands (nconc '(("User Commands")
				    ("d" . org-deadline)
				    ("s" . org-schedule)
				    ("e" . move-end-of-line)
				    ("E" . psalm/org-end-of-meta-data)
				    ("A" . psalm/org-archive-logbook)
				    ("x" . org-capture))
				  org-speed-commands))
  ;; Startup
  (setq	org-startup-indented t)
  
  ;; Links
  (setq org-link-frame-setup '((file . find-file)) ; Open link in same window
	org-id-link-to-org-use-id t)

  ;; Visual tweaks
  (setq org-indent-mode-hides-stars t
	org-image-actual-width (list 500))

  ;; Archive
  (setq org-archive-location (concat journal-file-path "::datetree/")
	org-archive-save-context-info nil)

  ;; Special Emacs Navigations
  (setq org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-special-ctrl-o t)

  ;; Attachments
  (setq org-attach-store-link-p t
	org-attach-auto-tag nil
	org-attach-id-dir (concat org-directory "/data/")
	org-attach-use-inheritance t)

  ;; Logging
  (setq org-log-into-drawer t
	org-log-done 'time
	org-log-reschedule 'note
	org-log-redeadline 'note)

  ;; Todos States
  (setq org-use-fast-todo-selection 'expert
	org-enforce-todo-dependencies t
	org-todo-keywords '((sequence "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "KILL(k@)")
			    (type "PROJ(p)" "HOLD(h)" "IDEA(i)" "TODO(t)" "|"))
	org-todo-keyword-faces `(("NEXT" . (:foreground ,"black"
							:background ,"light green"))
				 ("DONE" . (:foreground ,"dim gray"
							:background ,"light gray"))
				 ("KILL" . (:foreground ,"indian red"
							:background ,"light gray"))
				 ("PROJ" . (:foreground ,"black"
							:background ,"light blue"))
				 ("WAIT" . (:foreground ,"black"
							:background ,"khaki"))
				 ("HOLD" . (:foreground ,"black"
							:background ,"yellow"))
				 ("IDEA" . (:foreground ,"black"
							:background ,"violet"))))
  ;; Capture templates
  (setq org-capture-templates '(("i" "inbox" entry
				 (file+olp "staging.org" "Inbox")
				 "* %u %?" :prepend t)
				("n" "Next Action" entry
				 (file+olp "staging.org" "Tasks")
				 "* NEXT %?")
				("w" "Workflow Idea" entry
				 (file+olp "staging.org" "Inbox")
				 "* IDEA %?")
				("j" "Journal note" entry
				 (file+olp+datetree "journal.org")
				 "* %U %?" :jump-to-captured t)
				("c" "Code Review" entry
				 (file+headline "staging.org" "Code Reviewing Ideas")
				 "* IDEA %u %?")
				("s" "Sleep Journal" plain
				 (file+olp+datetree "sleep-journal.org")
				 "- start-finish of attempt :: %?\n- medicine used :: \n- Restedness 1-10 :: ")
				("g" "Grocery" checkitem
				 (file+headline "staging.org" "Shopping List"))))

  (defun psalm/org-repeat-hook ()
    (interactive)
    (org-reset-checkbox-state-subtree)
    (org-previous-visible-heading 1)
    (org-cycle))
  (add-hook 'org-todo-repeat-hook #'psalm/org-repeat-hook)
  (add-hook 'org-checkbox-statistics-hook #'org-mark-ring-push)

  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (org-id-get-with-outline-path-completion)))

  (org-link-set-parameters "id"
			   :complete 'org-id-complete-link))

;; Org Babel
(use-package ob-mermaid
  :after (org)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((mermaid . t)))
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
  (add-to-list 'org-structure-template-alist '("m" . "src mermaid :file ~/Pictures/mermaid.png")))

(use-package plantuml-mode
  :after (org)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (setq org-plantuml-jar-path (expand-file-name "~/src/plantuml.jar")
	plantuml-default-exec-mode 'jar
	plantuml-jar-path "~/src/plantuml.jar")
  ;; This value will also need to be passed as a :java header argument to plantuml src blocks
  (add-to-list 'plantuml-java-args "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
  (setq org-babel-default-header-args:plantuml
	(cons '(:java . "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
	      (assq-delete-all :java org-babel-default-header-args:plantuml)))

  (add-to-list 'org-structure-template-alist '("p" . "src plantuml :file ~/Pictures/plantuml.png")))
			      


