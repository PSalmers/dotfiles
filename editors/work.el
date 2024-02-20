;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Sets directory from which others are derived
(setq root-dir "~/")


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Consistency with modern apps

;; When using a mac-ish board
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
;; These are redundant with cua-mode, but make it consistent with mac keyboards
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "s-r") 'query-replace)
(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-F") 'isearch-backward)

(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)

(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)

(global-set-key (kbd "M-<down>") 'scroll-up-command)
(global-set-key (kbd "M-<up>") 'scroll-down-command)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Initialize shared config
(setq init-files "~/personal/dotfiles/editors")
(add-to-list 'load-path (concat init-files))
(setq init-file (concat init-files "/work.el"))
(load (concat init-files "/shared.el"))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Org

(use-package org
  :config
  (define-key org-mode-map (kbd "s-<right>") 'org-end-of-line)
  (define-key org-mode-map (kbd "s-<left>") 'org-beginning-of-line)
  
  (setq org-tag-persistent-alist '(("decisions" . ?d)
				   ("references" . ?r)
				   ("solved_problems" . ?s)
				   ("obsolete" . ?o)
				   ("atc")
				   ("workflow")
				   ("ergonomics")))

  (setq org-capture-templates (nconc org-capture-templates
				     '(("c" "Code Review" entry
					(file+headline "staging.org" "Code Reviewing Ideas")
					"* NEXT Review: %?")))))

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

