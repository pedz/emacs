;; [[file:init.org::*Set up Borg][Set up Borg:1]]
(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil))
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory)))
(require 'borg)
(borg-initialize)
;; Set up Borg:1 ends here

;; [[file:init.org::*Set up Leaf][Set up Leaf:1]]
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "lib/leaf" user-emacs-directory))
  (require 'leaf))
(leaf leaf-keywords
  :defun leaf-keywords-init
  :config
  (leaf-keywords-init))
;; Set up Leaf:1 ends here

;; [[file:init.org::*Set up Magit][Set up Magit:1]]
(leaf magit
  :defun magit-add-section-hook
  :defer-config
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-modules
			  'magit-insert-stashes
			  'append))
;; Set up Magit:1 ends here

;; [[file:init.org::*Set up code to help debugging at the front][Set up code to help debugging at the front:1]]
;; Nifty for debugging load issues and sequences
;; This puts out a Loading message all the time
;; (setq force-load-messages t)
;; (defadvice require (before load-log activate)
;;   (message "Requiring %s" (ad-get-arg 0)))
;; Set up code to help debugging at the front:1 ends here

;; [[file:init.org::*macOS path setup][macOS path setup:1]]
(if (eq system-type 'darwin)
    (let* ((path-helper "/usr/libexec/path_helper")
           path-helper-output)
      (unless (or (string-match (getenv "USER") (getenv "PATH"))
                  (not (file-executable-p path-helper)))
        (setenv "PATH" "")                  ; For consistency, zap PATH back to an empty string
        (setenv "MANPATH" "")               ; ditto for MANPATH
        (setq path-helper-output (shell-command-to-string (concat path-helper " -c")))
        (string-match "setenv \\([A-Z]+\\) \"\\([^\"]+\\)\";\nsetenv \\([A-Z]+\\) \"\\([^\"]+\\)\";\n"
                      path-helper-output)
        (setenv (match-string 1 path-helper-output) (match-string 2 path-helper-output))
        (setenv (match-string 3 path-helper-output) (match-string 4 path-helper-output))
        (setenv "PATH" (concat (expand-file-name "~/bin") ":" (getenv "PATH"))))))
;; macOS path setup:1 ends here

;; [[file:init.org::*Modify exec-path][Modify exec-path:1]]
(dolist (dir (split-string (getenv "PATH") ":"))
  (add-to-list 'exec-path dir))
;; Modify exec-path:1 ends here

;; [[file:init.org::*Customize zsh should it be used within an inferior shell][Customize zsh should it be used within an inferior shell:1]]
(defcustom explicit-zsh-args '("+Z" "-i")
  "Args passed to inferior shell by \\[shell], if the shell is zsh.
  Value is a list of strings, which may be nil."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)
;; Customize zsh should it be used within an inferior shell:1 ends here

;; [[file:init.org::*Pull in customized variables][Pull in customized variables:1]]
(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)
;; Pull in customized variables:1 ends here

;; [[file:init.org::*Pull in ~pedz~][Pull in ~pedz~:1]]
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "pedz" user-emacs-directory)))
(pedz/org-require 'pedz)
;; Pull in ~pedz~:1 ends here

;; [[file:init.org::*Set up creating Org mode code blocks easily.][Set up creating Org mode code blocks easily.:1]]
(pedz/org-require 'org-code-blocks)
;; Set up creating Org mode code blocks easily.:1 ends here
