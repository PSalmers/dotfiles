(tool-bar-mode 0)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

; fetch the list of packages available 
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
	god-mode
	spacemacs-theme))

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
(require 'god-mode)

(god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(which-key-mode)
(which-key-enable-god-mode-support)
(ivy-mode)
(counsel-mode)
(counsel-projectile-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Org Configuration
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c C-j") 'counsel-org-goto)
  (define-key org-mode-map (kbd "C-c C-J") 'counsel-org-goto-all)
  (define-key org-mode-map (kbd "C-c C-l") 'counsel-org-link)

  (setq org-directory "~/org")

  (add-to-list 'org-modules 'org-checklist)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)

  (setq org-agenda-files (list org-directory)
	org-refile-targets '((nil :maxlevel . 10))
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

  (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in)
  (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in)
  (define-key org-mode-map (kbd "C-c s") 'org-store-link)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda))


(global-set-key (kbd "C-;") 'avy-goto-char)
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
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" default))
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(package-selected-packages
   '(spacemacs-theme gruvbox-theme god-mode counsel-projectile magit counsel projectile avy ivy which-key))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom themes must be loaded after, so that the above "custom set variables"
;; puts the theme in custom-safe-themes
(load-theme 'spacemacs-dark)

;; God mode cursor indicator
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
