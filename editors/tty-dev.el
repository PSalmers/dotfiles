(setq make-backup-files nil)
(setq auto-save-default nil)
;; Text editing tweaks
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-.") 'repeat)
(xterm-mouse-mode 1)

(global-display-line-numbers-mode)
(menu-bar-mode 0)
(setq dired-dwim-target t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package magit)
(use-package ivy
  :config
  (ivy-mode))
(use-package counsel
  :config
  (counsel-mode))
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package rg
  :config
  (rg-enable-default-bindings))
(use-package eglot
;; Gems: Add to bashrc/zshrc to add gems to path
;; if which ruby >/dev/null && which gem >/dev/null; then
;;    PATH="$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
;; fi
)
(use-package which-key
  :config
  (which-key-mode))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package yafolding)
(use-package avy
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  (global-set-key (kbd "C-S-p") 'avy-goto-line-above)
  (global-set-key (kbd "C-S-n") 'avy-goto-line-below))

;; Keeps customizations from polluting my init
(setq custom-file (concat user-emacs-directory "/custom.el"))
