(tool-bar-mode 0)
(menu-bar-mode 0)

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
      '(which-key
	ivy
	counsel
	counsel-projectile
	avy
	projectile
	magit
	org-roam
	gruvbox-theme))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(package-initialize)

(require 'which-key)
(require 'ivy)
(require 'counsel)
(require 'avy)
(require 'projectile)

(which-key-mode)
(ivy-mode)
(counsel-mode)
(counsel-projectile-mode)


;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Org Configuration
(require 'org)
(global-set-key (kbd "C-x c") 'org-capture)
(define-key org-mode-map (kbd "C-c j") 'counsel-org-goto-all)

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
      org-use-speed-commands t
      org-speed-commands-user '(("g" . counsel-org-goto-all)
				("d" . org-deadline)
				("s" . org-schedule))
      org-agenda-custom-commands '(("n" "Next Actions" todo "NEXT")
				   ("r" "Schedule and NEXT" ((agenda "" ((org-agenda-span 'day)))
							     (todo "NEXT"))))
      org-startup-indented t
      org-link-frame-setup '((file . find-file)) ; opens links to org file in same window
      org-indent-mode-hides-stars t
      org-agenda-dim-blocked-tasks nil ;; Disabled because I am using NEXT
      org-agenda-todo-ignore-scheduled `future
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-deadline-prewarning-if-done t
      org-log-into-drawer t
      org-log-done `time
      org-agenda-show-future-repeats nil
      org-journal-date-format "%A, %D"
      org-todo-keywords '((sequence "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "KILL(k@)")
			  (type "PROJ(p)" "HOLD(h)" "IDEA(i)" "TODO(t)" "|"))
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

(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(setq avy-keys '(?a ?r ?s ?t ?n ?e ?e ?i ?o))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(counsel-mode t)
 '(custom-safe-themes
   '("83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" default))
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(package-selected-packages
   '(org-roam gruvbox-theme counsel-projectile magit counsel projectile avy ivy which-key))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom themes must be loaded after, so that the above "custom set variables"
;; puts the theme in custom-safe-themes

(load-theme 'gruvbox-dark-soft)
