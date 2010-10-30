;;;;; Julson's .emacs file
; Julson R. Lim
; October 16, 2010

(tool-bar-mode -1)

;Load packages
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;Configure CEDET
(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
(global-ede-mode 1)                 ;Enable the project management system
(semantic-load-enable-code-helpers) ;Enable prototype help and smart completion
(global-srecode-minor-mode 1)       ;Enable template insertion menu

;Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;JDEE (requires CEDET)
(require 'jde)

;Emacs Code Browser
(require 'ecb)

;Configure Markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))