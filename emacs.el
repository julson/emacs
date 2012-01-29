;;;;; Julson's .emacs file
; Julson R. Lim
; September 3, 2011

; Set global variables
(setq org-base-path (expand-file-name "~/work/notes"))
(setq site-lisp-base-path (expand-file-name "~/.emacs.d/site-lisp"))
(setq inferior-lisp-program "clisp -K full")

; Set load paths
(let ((base site-lisp-base-path))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

; Initialize markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

; Set alternative hotkeys for Alt+x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; Set alternative hotkeys for backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; Remove UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; Enable clipboard
(setq x-select-enable-clipboard t)

; Function to launch a bash shell and rename its buffer
(defun bash (buffer-name)
  "Start ansi-term with bash and rename buffer."
  (interactive "Buffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

; Set up org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; Load org-mode agendas
(setq org-agenda-files (list org-base-path))

; Load solarized color-theme
(require 'color-theme-solarized)
(color-theme-solarized-dark)

; Interactively Do Things
(ido-mode t)

; Load nxhtml and mumamo
(load (concat site-lisp-base-path "/nxhtml/autostart.el"))

(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb'" . eruby-nxhtml-mumamo))

