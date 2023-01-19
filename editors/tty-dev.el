(setq make-backup-files nil
      auto-save-default nil)

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
(use-package quelpa-use-package)
(use-package eglot)
(use-package rg)
(use-package magit)
(use-package ivy
  :config
  (ivy-mode))
(use-package counsel
  :config
  (counsel-mode))
(use-package which-key
  :config
  (which-key-mode))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package yafolding
  :config
  (global-set-key (kbd "C-c h h") 'yafolding-hide-element)
  (global-set-key (kbd "C-c h H") 'yafolding-hide-all)
  (global-set-key (kbd "C-c h s") 'yafolding-show-element)
  (global-set-key (kbd "C-c h S") 'yafolding-show-all))
(use-package avy
  :config
  (global-set-key (kbd "C-c ;") 'avy-goto-char-timer)
  (global-set-key (kbd "M-p") 'avy-goto-line-above)
  (global-set-key (kbd "M-n") 'avy-goto-line-below))

;; Keeps customizations from polluting my init
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Tsx from https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  (setq typescript-indent-level 2)

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package solarized-theme
  :config
  (load theme 'solarized-theme-selenized-light t))
