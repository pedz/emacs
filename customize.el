
;; Set by emacs' customizing routines -- don't change directly
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("/opt/homebrew/share/info"))
 '(Info-default-directory-list
   (delq nil
         (mapcar
          (lambda
            (x)
            (and
             (file-executable-p x)
             x))
          Info-default-directory-list)))
 '(ac-modes
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode apples-mode))
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "red" "green" "gold" "blue" "magenta" "darkturquoise" "dark green"])
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist '(("." . "~/.config/emacs/backup")))
 '(case-fold-search nil)
 '(default-frame-alist
   '((minibuffer . t)
     (menu-bar-lines . 0)
     (background-color . "black")
     (cursor-color . "SlateBlue")
     (foreground-color . "grey")
     (mouse-color . "Blue")))
 '(delete-old-versions t)
 '(dired-use-ls-dired nil)
 '(display-buffer-reuse-frames t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mail-file 'none)
 '(enable-recursive-minibuffers t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-bash-args '("--noediting" "--login" "-i"))
 '(explicit-zsh-args '("-l" "-i" "+Z"))
 '(feature-cucumber-command "cucumber {options} \"{feature}\"")
 '(grep-find-ignored-directories (append vc-directory-exclusion-list (list ".bundle")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   (cond
    ((and
      (or
       (=
        (display-pixel-height)
        2490)
       (=
        (display-pixel-height)
        2340)
       (=
        (display-pixel-height)
        1440))
      (=
       (display-pixel-width)
       2560))
     '((top . 23)
       (left . 835)
       (width . 132)
       (height . 95)))
    ((and
      (=
       (display-pixel-height)
       1050)
      (=
       (display-pixel-width)
       1680))
     '((top . 23)
       (left . 360)
       (width . 132)
       (height . 70)))))
 '(js-indent-level 2)
 '(js2-global-externs '("jQuery" "$"))
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(lsp-disabled-clients '(typeprof-ls))
 '(lsp-enable-snippet nil)
 '(lua-indent-level 2)
 '(mac-emulate-three-button-mouse t)
 '(mail-default-reply-to "pedz@easesoftware.com")
 '(mail-self-blind t)
 '(major-mode 'text-mode)
 '(mmm-submode-decoration-level 2)
 '(mumamo-chunk-coloring 1 nil nil "let most of the page be uncolored and color only the sub-chunks")
 '(ns-right-alternate-modifier 'alt)
 '(ns-right-command-modifier 'hyper)
 '(nxhtml-skip-welcome nil nil nil "Shh!!!")
 '(org-clock-auto-clockout-timer 1800)
 '(package-selected-packages '(dash compat company))
 '(project-vc-merge-submodules nil)
 '(rails-ws:default-server-type "webrick")
 '(rbenv-installation-dir "/Users/pedz/.config/rbenv" t)
 '(safe-local-variable-values
   '((eval pedz/hatred-set-web-mode-locals)
     (eval pedz/rails-7-passkey-demo-set-web-mode-locals)
     (eval pedz/rails-7-passkey-demo-set-enh-ruby-mode-locals)
     (eval pedz/rails-7-passkey-demo-set-default-mode-locals)
     (eval pedz/hatred-set-enh-ruby-mode-locals)
     (eval pedz/hatred-set-default-mode-locals)))
 '(save-abbrevs nil)
 '(select-enable-primary t)
 '(send-mail-function 'mailclient-send-it)
 '(shell-popd-regexp "popd\\|P")
 '(shell-prompt-pattern ".+@.+<[0-9]+> on .*\12")
 '(shell-pushd-regexp "pushd\\|p")
 '(split-width-threshold 1600)
 '(standard-indent 2)
 '(text-mode-hook '(text-mode-hook-identify flyspell-mode auto-fill-mode))
 '(tool-bar-mode nil)
 '(user-full-name "Perry Smith")
 '(user-mail-address "pedz@easesoftware.com")
 '(vc-ignore-dir-regexp "\\`\\([\\/][\\/]\\|/\\.\\.\\./\\|/net/\\|/afs/\\)\\'")
 '(version-control t)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
