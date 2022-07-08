(tool-bar-mode 0)
(menu-bar-mode 0)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

; fetch the list of packages available. I have commented this out to avoid needlessly pinging melpa etc on startup
; (unless package-archive-contents
;  (package-refresh-contents))

(setq package-list
      '(which-key
	ivy
	counsel
	counsel-projectile
	avy
	projectile
	magit
	evil
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


; Evil Configuration

;; Must come before evil is required
(setq evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'normal (kbd ",") t)
(evil-mode 1)
(evil-define-key 'normal 'global (kbd "<leader>F") 'counsel-find-file)
(evil-define-key 'normal 'global (kbd "<leader>u") 'universal-argument)
(evil-define-key 'normal 'global (kbd "<leader>bp") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bn") 'next-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bl") 'evil-switch-to-windows-last-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'counsel-switch-buffer)
(evil-define-key 'normal 'global (kbd "C-n") 'evil-next-line)
(evil-define-key 'normal 'global (kbd "C-p") 'evil-previous-line)
(evil-define-key 'insert 'global (kbd "C-n") 'evil-next-line)
(evil-define-key 'insert 'global (kbd "C-p") 'evil-previous-line)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>s") 'counsel-projectile-rg)

;; Magit
(evil-define-key 'normal 'global (kbd "<leader>m") 'magit-status)

;; Org Configuration
(require 'org)
(evil-define-key 'normal 'global (kbd "<leader>x") 'org-capture)
(evil-define-key 'normal 'global (kbd "<localleader>x") 'org-toggle-checkbox)

(evil-define-key 'normal 'global (kbd "<leader>ci") 'org-clock-in)
(evil-define-key 'normal 'global (kbd "<leader>co") 'org-clock-out)
(evil-define-key 'normal 'global (kbd "<leader>cc") 'org-clock-cancel)
(evil-define-key 'normal 'global (kbd "<leader>cg") 'org-clock-goto)

(evil-define-key 'normal 'global (kbd "<leader>aa") 'org-agenda)
(evil-define-key 'normal 'global (kbd "<leader>aw") 'org-agenda-list)
(evil-define-key 'normal 'global (kbd "<leader>an") (lambda () (interactive) (org-todo-list "NEXT")))

(evil-define-key 'normal 'global (kbd "<leader>t") 'org-todo)

(evil-define-key 'normal 'global (kbd "<leader>g") 'counsel-org-goto)
(evil-define-key 'normal 'global (kbd "<leader>G") 'counsel-org-goto-all)

(evil-define-key 'normal 'global (kbd "<leader>n") 'org-add-note)

(evil-define-key 'normal 'global (kbd "<leader>]") 'org-next-link)
(evil-define-key 'normal 'global (kbd "<leader>[") 'org-previous-link)
(evil-define-key 'normal 'global (kbd "<leader>l") 'counsel-org-link)
(evil-define-key 'normal 'global (kbd "<leader>s") 'org-store-link)
(evil-define-key 'normal 'global (kbd "<leader>o") 'org-open-at-point)
(evil-set-command-property 'org-open-at-point :jump t)

(evil-define-key 'normal 'global (kbd "<leader>ds") 'org-schedule)
(evil-define-key 'normal 'global (kbd "<leader>dd") 'org-deadline)
(evil-define-key 'normal 'global (kbd "<leader>di") 'org-time-stamp)
(evil-define-key 'normal 'global (kbd "<leader>dI") 'org-time-stamp-inactive)
(evil-define-key 'insert 'global (kbd "C-i di") 'org-time-stamp)
(evil-define-key 'insert 'global (kbd "C-i dI") 'org-time-stamp-inactive)

; For tty
(evil-define-key 'normal 'global (kbd "<leader>TAB") 'org-cycle)
(evil-define-key 'normal 'global (kbd "<leader><leader>TAB") 'org-shifttab)


(setq org-directory "~/org")

(add-to-list 'org-modules 'org-checklist)
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-id)

(setq org-agenda-files (list org-directory)
      org-refile-targets '((nil :maxlevel . 10))
      org-startup-indented t
      org-intend-mode-hides-stars t
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


(evil-define-key 'normal 'global (kbd "<leader>;") 'avy-goto-char-2)
(evil-define-key 'insert 'global (kbd "C-;") 'avy-goto-char-2)
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
   '(evil-collection evil spacemacs-theme gruvbox-theme god-mode counsel-projectile magit counsel projectile avy ivy which-key))
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
