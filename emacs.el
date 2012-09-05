;;;;; Julson's .emacs file

(require 'cl)
(require 'package)

;; Set up elpa repos
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Ensure packages are installed on startup (based from Prelude's prelude-packages.el)
;; Start of prelude-packages.el
(defvar personal-packages
  '(clojure-mode org paredit nrepl color-theme-solarized yasnippet)
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
  '(("\\.cljs" clojure-mode clojure-mode)
    ("\\.md$" markdown-mode markdown-mode)
    ("\\.markdown$" markdown-mode markdown-mode)
    ("\\.org$" org-mode org-mode)))

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
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default indent-tabs-mode nil)
(setq-default column-number-mode t)
(set-default-font "Inconsolata-13")

;; Easy scrolling key-bindings
(global-set-key "\M-n" '"\C-u1\C-v")
(global-set-key "\M-p" '"\C-u1\M-v")

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; Function to launch a bash shell and rename its buffer
(defun bash (buffer-name)
  "Start ansi-term with bash and rename buffer."
  (interactive "Buffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

;; hook to make the terminal colors match the color theme
(defun ash-term-hooks ()
  (setq term-default-bg-color (face-background 'default))
  (setq term-default-fg-color (face-foreground 'default)))
(add-hook 'term-mode-hook 'ash-term-hooks)

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

;; Load paredit and make sure it's hooked to appropriate modes
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
