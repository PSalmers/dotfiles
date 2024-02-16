;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Sets directory from which others are derived
(setq root-dir (if (eq system-type 'windows-nt) "C:/Users/patri/" "~/"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~> Consistency with modern apps
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(global-set-key (kbd "C-r") 'query-replace)

(setq init-files "~/src/dotfiles/editors")
(add-to-list 'load-path (concat init-files))
(setq init-file (concat init-files "/home.el"))
(load (concat init-files "/shared.el"))
