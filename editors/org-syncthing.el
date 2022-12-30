(setq psalm-sync-dir (if (eq system-type 'windows-nt) "C:/Users/patri/Sync/" "~/Sync/"))

(setq visible-bell t)

;; Interface defaults

;; I want to work toward using the mouse more, so I am leaving these on to inspire mouse-based configuration.
;; My reason for this is that it may be less cognitive overhead to click buttons, and less cognitive overhead is more resilient to age or illness.
;;(if window-system (tool-bar-mode 0) nil)
;;(menu-bar-mode 0)

(xterm-mouse-mode 1)
(global-display-line-numbers-mode)
(cua-mode 1)
(setq mouse-drag-and-drop-region t)


;; this makes the screen startup fullscreen but does not let you resize the window after
;; (setq initial-frame-alist '((fullscreen . maximized)))

;; Windmove
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<left>") 'windmove-left)
(global-set-key (kbd "C-c C-<up>") 'windmove-up)
(global-set-key (kbd "C-c C-<down>") 'windmove-down)
(setq windmove-create-window t)

;; Dired
(setq dired-dwim-target t)

;; Mac Hotkeys
;; When using a mac-ish board
;;(setq mac-option-modifier 'control
;;      mac-command-modifier 'meta)

;; When usiong a normal ANSI or ISO board
(setq mac-option-modifier 'meta)

;; Custom global keys
(global-set-key (kbd "C-c fi") (lambda () (interactive) (find-file (concat psalm-sync-dir "init.el"))))
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c R") 'replace-string)

;; Text editing tweaks
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-.") 'repeat)

(setq calendar-week-start-day 0) ; The default value of "Sunday" keeps it consistent with more calendars around me.

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
	counsel
	fancy-dabbrev
	god-mode
	ivy
	magit
	ob-mermaid
	plantuml-mode
	popup
	smex
	swiper
	solarized-theme
	use-package
	which-key
	; visual-fill-column ; This mode does not indent the fill column in org-indent mode
	))

(package-initialize)

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(counsel-mode)
(ivy-mode)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;;; Themes
;; Modus
(setq modus-themes-org-blocks 'gray-background) ; gray instead of tinted because tinted does not work for languages I haven't installed the mode for
;; (load-theme 'modus-operandi t)

;; Selenized
(load-theme 'solarized-selenized-light t)

;; Zenburn
;;(load-theme 'zenburn t)
;;(defun my-zenburn-colour (name)
;;  (cdr (assoc name zenburn-default-colors-alist)))

;; Tramp and spin.el Setup
(setq remote-shell "/usr/bin/zsh"
      tramp-encoding-shell "/bin/zsh"
      ; needed for project to work vc-handled-backends (remove 'Git vc-handled-backends) ; very slow over ssh, and I use magit anyways
      )
(require 'use-package)

(use-package god-mode
  :config
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (global-set-key (kbd "<escape>") #'god-mode-all)
  (global-set-key (kbd "M-o") #'god-mode-all)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key isearch-mode-map (kbd "M-o") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)
  (define-key god-mode-isearch-map (kbd "M-o") #'god-mode-isearch-disable)
  (defun my-god-mode-update-mode-line ()
    (cond
     (god-local-mode
      (set-face-attribute 'mode-line nil
                          :foreground "#604000"
                          :background "#fff29a")
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#3f3000"
                          :background "#fff3da"))
     (t
      (set-face-attribute 'mode-line nil
			  :foreground "#0a0a0a"
			  :background "#d7d7d7")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#404148"
			  :background "#efefef"))))

  (add-hook 'post-command-hook 'my-god-mode-update-mode-line))

(use-package spin
  :if (file-exists-p "~/src/github.com/Shopify/spin.el")
  :load-path "~/src/github.com/Shopify/spin.el")


;; Avy Settings
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-S-p") 'avy-goto-line-above)
(global-set-key (kbd "C-S-n") 'avy-goto-line-below)
(global-set-key (kbd "C-S-k") 'avy-kill-region)

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


(defun org-id-complete-link (&optional arg)
  "Create an id: link using completion"
  (concat "id:"
          (org-id-get-with-outline-path-completion)))

(org-link-set-parameters "id"
			 :complete 'org-id-complete-link)

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

(setq org-directory (concat psalm-sync-dir "org"))

; Startup with org project open
(setq inhibit-splash-screen t)
(defun psalm/startup ()
  (interactive)
  (org-agenda nil "d")
  (delete-other-windows))
(add-hook 'window-setup-hook 'psalm/startup)

; Using personal dictionary
(setq ispell-personal-dictionary (concat org-directory "/.aspell.en.pws"))

;; Resetting checkboxes
(define-key org-mode-map (kbd "C-c b") 'org-reset-checkbox-state-subtree)

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-id)

(defun psalm/insert-header-eof () ""
    (interactive)
    (end-of-buffer)
    (insert "\n* "))

(define-key org-mode-map (kbd "C-c RET") 'psalm/insert-header-eof)
(define-key org-mode-map (kbd "C-c n") 'org-next-item)
(define-key org-mode-map (kbd "C-c p") 'org-previous-item)

(defun psalm/org-end-of-meta-data () ""
       (interactive)
       (org-end-of-meta-data t))

;; org-archive functions from stack overflow
(defun psalm/org-archive-delete-logbook ()
  (save-excursion
   (org-end-of-meta-data)
   (let ((elm (org-element-at-point)))
     (when (and
            (equal (org-element-type elm) 'drawer)
            (equal (org-element-property :drawer-name elm) "LOGBOOK"))
       (delete-region (org-element-property :begin elm)
                      (org-element-property :end elm))))))

(defun psalm/org-archive-without-delete ()
  (cl-letf (((symbol-function 'org-cut-subtree) (lambda () nil)))
    (org-archive-subtree-default)))

(defun psalm/org-archive-logbook ()
  (interactive)
  (psalm/org-archive-without-delete)
  (psalm/org-archive-delete-logbook))

(defun psalm/org-insert-current-time-inactive ()
  (interactive)
  (org-time-stamp-inactive '(16)))
(define-key org-mode-map (kbd "C-c T") 'psalm/org-insert-current-time-inactive)

(defun psalm/org-table-insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))
(define-key org-mode-map (kbd "C-c t") 'psalm/org-table-insert-current-time)

(defun psalm/start-new-workout ()
  (interactive)
  (goto-char (org-table-end))
  (backward-char)
  (org-table-goto-column 1)
  (org-return)
  (psalm/org-insert-current-time-inactive)
  (org-table-next-field))
(define-key orgtbl-mode-map (kbd "C-c w") 'psalm/start-new-workout)

(setq org-agenda-files (list org-directory (concat org-directory "/agenda/")) ; excludes /archive by default
      org-refile-targets '((nil :maxlevel . 10))
      org-refile-use-outline-path t
      org-use-tag-inheritance nil
      org-outline-path-complete-in-steps nil
      org-tags-column 0
      org-tag-persistent-alist '(("decisions" . ?d)
				 ("references" . ?r)
				 ("obsolete" . ?o))
      org-use-speed-commands t
      org-speed-commands (nconc '(("User Commands")
				  ("d" . org-deadline)
				  ("s" . org-schedule)
				  ("e" . move-end-of-line)
				  ("E" . psalm/org-end-of-meta-data)
				  ("A" . psalm/org-archive-logbook)
				  ("x" . org-capture))
				 org-speed-commands)
      org-agenda-custom-commands '(("n" "Next Actions" todo "NEXT")
				   ("d" "Schedule and NEXT" ((agenda "" ((org-agenda-span 'day)))
							     (todo "NEXT")
							     (todo "WAIT"))))
      org-startup-indented t
      org-link-frame-setup '((file . find-file)) ; opens links to org file in same window
      org-indent-mode-hides-stars t
      org-archive-location (concat org-directory "/journal.org::datetree/")
      org-archive-save-context-info nil
      org-attach-store-link-p t
      org-attach-auto-tag nil
      org-attach-id-dir (concat org-directory "/data/")
      org-attach-use-inheritance t
      org-image-actual-width (list 500)
      org-agenda-dim-blocked-tasks nil ;; Disabled because I am using NEXT
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-time-comparison-use-seconds t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-deadline-prewarning-if-done t
      org-agenda-skip-deadline-if-done t
      org-log-into-drawer t
      org-log-done 'time
      org-agenda-show-future-repeats nil
      org-use-fast-todo-selection 'expert
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
						      :background ,"violet")))
      org-agenda-prefix-format '((agenda . "%?-12t% s")
				(todo . "")
				(tags . "")
				(search . " %i %-12:c"))
      org-habit-show-all-today nil
      org-id-link-to-org-use-id t)

(setq org-capture-templates '(("i" "inbox" entry
			       (file+olp "staging.org" "Inbox")
			       "* %u %?" :prepend t)
			      ("n" "Next Action" entry
			       (file+olp "staging.org" "Tasks")
			       "* NEXT %?")
			      ("j" "Journal note" entry
			       (file+olp+datetree "journal.org")
			       "* Journal" :jump-to-captured t)
			      ("s" "Sleep Journal" plain
			       (file+olp+datetree "sleep-journal.org")
			       "- start-finish of attempt :: %?\n- medicine used :: \n- Restedness 1-10 :: ")
			      ("f" "Fitness Journal" plain
			       (file+olp+datetree "fitness-journal.org")
			       "- Activity :: %?\n- start-finish :: \n- Avg HR :: ")
			      ("g" "Grocery" checkitem
			       (file+headline "staging.org" "Shopping List"))))

;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)
   (mermaid . t)))
			      
;; Plant UML Setup
;; active Org-babel languages
(require 'plantuml-mode)
(setq org-plantuml-jar-path (expand-file-name "~/src/plantuml.jar")
      plantuml-default-exec-mode 'jar
      plantuml-jar-path "~/src/plantuml.jar")
;; This value will also need to be passed as a :java header argument to plantuml src blocks
(add-to-list 'plantuml-java-args "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
(setq org-babel-default-header-args:plantuml
      (cons '(:java . "-Dplantuml.include.path=\"/Users/psalmers/src/C4-PlantUML\"")
	    (assq-delete-all :java org-babel-default-header-args:plantuml)))

(add-to-list 'org-structure-template-alist '("p" . "src plantuml :file ~/Pictures/plantuml.png"))

;; Mermaid Setup
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
(add-to-list 'org-structure-template-alist '("m" . "src mermaid :file ~/Pictures/mermaid.png"))

