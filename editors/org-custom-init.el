
;; Disable interface bt default
(if window-system (tool-bar-mode 0) nil)
(menu-bar-mode 0)

;; this makes the screen startup fullscreen but does not let you resize the window after
;; (setq initial-frame-alist '((fullscreen . maximized)))

;; Dired
(setq dired-dwim-target t)

;; Mac Hotkeys
(setq mac-option-modifier 'control
      mac-command-modifier 'meta)

;; Custom global keys
(global-set-key (kbd "C-c fi") (lambda () (interactive) (find-file "~/org-custom/init.el")))
(global-set-key (kbd "C-c v") 'visual-line-mode)

;; Text editing tweaks
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-.") 'repeat)

(setq calendar-week-start-day 1)

;; Auto save
(auto-save-visited-mode 1)

;; Keeps customizations from polluting my init
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; For acronyms I will use all caps, and for code I will use src blocks. So I will use the more natural sentence ending.
(setq sentence-end-double-space nil)

;; grep
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
	use-package
	popup
	fancy-dabbrev
	counsel
	ivy
	which-key
	hydra
	swiper
	org-roam ; Roam is a bit heavyweight, but I know it supports all the workflows I need
	; visual-fill-column ; This mode does not indent the fill column in org-indent mode
       ))

(package-initialize)

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(counsel-mode)
(ivy-mode)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;;; Themes
;; Modus
(setq modus-themes-org-blocks 'gray-background) ; gray instead of tinted because tinted does not work for languages I haven't installed the mode for
(load-theme 'modus-operandi t)

;; Zenburn
;;(load-theme 'zenburn t)
(defun my-zenburn-colour (name)
  (cdr (assoc name zenburn-default-colors-alist)))

;; Tramp and spin.el Setup
(setq remote-shell "/usr/bin/zsh"
      tramp-encoding-shell "/bin/zsh"
      ; needed for project to work vc-handled-backends (remove 'Git vc-handled-backends) ; very slow over ssh, and I use magit anyways
      )
(require 'use-package)
(use-package spin
  :if (file-exists-p "~/src/github.com/Shopify/spin.el")
  :load-path "~/src/github.com/Shopify/spin.el")


;; Avy Settings
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-S-p") 'avy-goto-line-above)
(global-set-key (kbd "C-S-n") 'avy-goto-line-below)


;; Fancy Dabbrev
;; Load fancy-dabbrev.el:
(require 'fancy-dabbrev)

;; Enable fancy-dabbrev previews everywhere:
;; (global-fancy-dabbrev-mode)
(setq fancy-dabbrev-expansion-context 'almost-everywhere)

;; Let dabbrev searches ignore case and expansions preserve case:
(setq dabbrev-case-distinction nil
      dabbrev-case-fold-search t
      dabbrev-case-replace nil)

;; I bind to the traditional dabbev bindings so that I do not interfere with TAB's behaviour
(global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
(global-set-key (kbd "C-M-/") 'fancy-dabbrev-backward)


;; Org Configuration
(require 'org)

(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o s") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o j") 'org-clock-goto)
(global-set-key (kbd "C-c o g") 'counsel-org-goto-all)

(add-hook 'org-mode-hook 'visual-line-mode)

(defun my-org-goto () (interactive)
       (org-mark-ring-push)
       (org-refile 1))
(define-key org-mode-map (kbd "C-c C-j") 'counsel-org-goto)

(setq org-directory "~/org")

; Startup with org project open
(setq inhibit-splash-screen t)
(defun psalm-startup ()
  (interactive)
  (dired org-directory)
  (org-agenda nil "d"))
(add-hook 'window-setup-hook 'psalm-startup)

; Using personal dictionary
(setq ispell-personal-dictionary (concat org-directory "/.aspell.en.pws"))

;; Resetting checkboxes
(define-key org-mode-map (kbd "C-c b") 'org-reset-checkbox-state-subtree)

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
(global-set-key (kbd "C-c r n") 'org-roam-dailies-goto-next-note)
(global-set-key (kbd "C-c r p") 'org-roam-dailies-goto-previous-note)

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))


(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-id)

(defun psalm-insert-header-eof () ""
    (interactive)
    (end-of-buffer)
    (insert "\n* "))

(defun psalm-archive-subtree () ""
       (interactive)
       (org-cut-subtree)
       (org-roam-dailies-goto-today)
       (end-of-buffer)
       (yank)
       (previous-buffer))

(define-key org-mode-map (kbd "C-c RET") 'psalm-insert-header-eof)

(setq org-agenda-files (list org-directory org-roam-directory (concat org-roam-directory org-roam-dailies-directory))
      org-refile-targets '((nil :maxlevel . 10))
      org-refile-use-outline-path t
      org-use-tag-inheritance nil
      org-outline-path-complete-in-steps nil
      org-tags-column 0
      org-tag-persistent-alist '(("decisions" . ?d)
				 ("references" . ?r)
				 ("obsolete" . ?o))
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
      org-attach-use-inheritance t
      org-image-actual-width (list 500)
      org-agenda-dim-blocked-tasks nil ;; Disabled because I am using NEXT
      org-agenda-todo-ignore-scheduled `future
      org-agenda-todo-ignore-time-comparison-use-seconds t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-deadline-prewarning-if-done t
      org-agenda-skip-deadline-if-done t
      org-log-into-drawer t
      org-log-done `time
      org-agenda-show-future-repeats nil
      org-journal-date-format "%A, %D"
      org-use-fast-todo-selection 'expert
      org-todo-keywords '((sequence "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "KILL(k@)")
			  (type "PROJ(p)" "HOLD(h)" "IDEA(i)" "TODO(t)" "|")
			  (sequence "SHAREABLE(s)" "|" "SHARED(S@)"))
      ;; org-todo-keyword-faces `(("NEXT" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-green")))
      ;; 			       ("DONE" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-bg+2")))
      ;; 			       ("KILL" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-red")))
      ;; 			       ("PROJ" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-blue")))
      ;; 			       ("WAIT" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-yellow")))
      ;; 			       ("HOLD" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-yellow")))
      ;; 			       ("IDEA" . (:foreground ,(my-zenburn-colour "zenburn-bg")
      ;; 						      :background ,(my-zenburn-colour "zenburn-yellow"))))
      org-todo-keyword-faces `(("NEXT" . (:foreground ,(modus-themes-color 'fg-main)
						      :background ,(modus-themes-color 'green-subtle-bg)))
			       ("DONE" . (:foreground ,(modus-themes-color 'fg-dim)
						      :background ,(modus-themes-color 'bg-main)))
			       ("KILL" . (:foreground ,(modus-themes-color 'red-faint)
						      :background ,(modus-themes-color 'bg-main)))
			       ("PROJ" . (:foreground ,(modus-themes-color 'fg-main)
						      :background ,(modus-themes-color 'blue-subtle-bg)))
			       ("WAIT" . (:foreground ,(modus-themes-color 'fg-main)
						      :background ,(modus-themes-color 'yellow-subtle-bg)))
			       ("HOLD" . (:foreground ,(modus-themes-color 'fg-main)
						      :background ,(modus-themes-color 'yellow-subtle-bg)))
			       ("IDEA" . (:foreground ,(modus-themes-color 'fg-main)
						      :background ,(modus-themes-color 'magenta-subtle-bg))))
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
			       (file+headline "tasks.org" "Shopping List"))))
			      
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
