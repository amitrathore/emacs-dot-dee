;;; init.el --- Clean Emacs config for Clojure dev -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Package Setup
;;; ---------------------------------------------------------------------------

(require 'package)

;; Add MELPA and GNU ELPA
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
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
 '(package-selected-packages '(cider magit paredit rainbow-delimiters)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode 1)
