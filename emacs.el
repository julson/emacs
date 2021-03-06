;;;;; Julson's .emacs file

(require 'cl)
(require 'package)

;; Set up elpa repos
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(push "/usr/local/bin" exec-path)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Ensure packages are installed on startup (based from Prelude's prelude-packages.el)
;; Start of prelude-packages.el
(defvar personal-packages
  '(clojure-mode magit magithub org paredit nrepl color-theme-solarized yasnippet
                 markdown-mode melpa ido-ubiquitous crontab-mode
                 multi-term elscreen inf-ruby ruby-block ruby-end ruby-tools rvm
                 yari yaml-mode ac-nrepl clojure-test-mode midje-mode puppet-mode
                 auto-complete cljsbuild-mode)
  "A list of packages to ensure are installed at launch.")

(defun personal-packages-installed-p ()
  (loop for p in personal-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun personal-install-packages ()
  (unless (personal-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")

    ;; install missing packages
    (dolist (p personal-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(personal-install-packages)

(defmacro personal-auto-install (extension package mode)
  `(add-to-list 'auto-mode-alist
		`(,extension . (lambda ()
				 (unless (package-installed-p ',package)
				   (package-install ',package))
				 (,mode)))))

(defvar personal-auto-install-alist
  '(("\\.md$" markdown-mode markdown-mode)
    ("\\.markdown$" markdown-mode markdown-mode)
    ("\\.org$" org org-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed (not sure why this is needed...)
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(dolist (entry personal-auto-install-alist)
  (let ((extension (first entry))
	(package (second entry))
	(mode (third entry)))
    (unless (package-installed-p package)
      (personal-auto-install extension package mode))))

;; End of prelude-packages.el

;; Shamelessly copied from prelude-ruby.el
;; Set ruby mode to files
(progn
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))

  (eval-after-load 'ruby-mode
    '(progn
       (defun my-ruby-mode-defaults ()
         (inf-ruby-setup-keybindings)
         ;; turn off the annoying input echo in irb
         (setq comint-process-echoes t)
         (ruby-block-mode t)
         (ruby-end-mode +1)
         (ruby-tools-mode +1)
         ;; CamelCase aware editing operations
         (subword-mode +1)
         ;; bind yari in the local keymap
         (local-set-key (kbd "C-h r") 'yari))

       (setq my-ruby-mode-hook 'my-ruby-mode-defaults)

       (add-hook 'ruby-mode-hook (lambda ()
                                   (run-hooks 'my-ruby-mode-hook))))))
;; End of prelude-ruby.el

;; Set global variables
(setq org-base-path (expand-file-name "~/work/notes"))

;; Set alternative hotkeys for Alt+x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Set alternative hotkeys for backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Remove UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))

(setq-default indent-tabs-mode nil)
(setq-default column-number-mode t)
(set-default-font "Inconsolata-13")

(add-to-list 'default-frame-alist
             '(font . "Inconsolata-13"))

;; Easy scrolling key-bindings
(global-set-key "\M-n" '"\C-u1\C-v")
(global-set-key "\M-p" '"\C-u1\M-v")

(global-set-key [(shift f8)] 'multi-term-dedicated-toggle)

;; Currently not-working in Cocoa Emacs 24.3 from homebrew
(global-set-key [(shift f9)] 'ns-toggle-fullscreen)

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; Set up org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Load org-mode agendas
(setq org-agenda-files (list org-base-path))

;; Set default color theme
(load-theme 'solarized-dark t)

;; Enable IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Setup auto-complete for nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Load paredit and make sure it's hooked to appropriate modes
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
