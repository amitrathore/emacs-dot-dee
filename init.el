;;; init.el --- Clean Emacs config for Clojure dev -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Package Setup
;;; ---------------------------------------------------------------------------

(require 'package)

;; Add MELPA and GNU ELPA
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Initialize and refresh once if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if not installed
(dolist (pkg '(clojure-mode cider paredit rainbow-delimiters))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; ---------------------------------------------------------------------------
;;; General UI Tweaks
;;; ---------------------------------------------------------------------------

(setq inhibit-startup-screen t)    ;; No splash screen
(menu-bar-mode -1)                  ;; Hide menu bar
(tool-bar-mode -1)                  ;; Hide toolbar
(scroll-bar-mode -1)                ;; Hide scrollbars
(show-paren-mode 1)                  ;; Highlight matching parens
(setq show-paren-delay 0)           ;; No delay for paren highlight
(global-display-line-numbers-mode)  ;; Line numbers everywhere

(set-face-attribute 'default nil :height 130) ;; Font size (13pt)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; ---------------------------------------------------------------------------
;;; Clojure Development Setup
;;; ---------------------------------------------------------------------------

;; Paredit and rainbow-delimiters in Clojure buffers
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

;; Same for REPL buffers
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;; CIDER settings
(setq cider-repl-display-help-banner nil   ;; No welcome banner
      cider-repl-wrap-history t            ;; Wrap REPL history
      cider-repl-history-file "~/.emacs.d/cider-history"
      cider-save-file-on-load t)            ;; Save buffers on eval

;;; ---------------------------------------------------------------------------
;;; Keybindings for Productivity
;;; ---------------------------------------------------------------------------

;; Evaluate buffer or expression quickly
(global-set-key (kbd "C-c C-k") #'cider-load-buffer)
(global-set-key (kbd "C-x C-e") #'cider-eval-last-sexp)

;;; ---------------------------------------------------------------------------
;;; End of File
;;; ---------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cider claude-code eat exec-path-from-shell inheritenv magit
	   markdown-mode paredit rainbow-delimiters vterm))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode 1)



;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
