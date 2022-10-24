;; [[file:pedz.org::*Small tweaks][Small tweaks:1]]
(put 'downcase-region  'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region    'disabled nil)

(define-key help-map (kbd "A") 'apropos-command)
(define-key help-map (kbd "a") 'apropos)
;; Small tweaks:1 ends here

;; [[file:pedz.org::*Personal keymap][Personal keymap:1]]
(defun pedz/toggle-buffer-case-fold-search ()
  "Toggles the case fold search flag for the local buffer"
  (interactive)
  (message "case-fold-search is now %s"
           (prin1-to-string (setq case-fold-search (not case-fold-search)))))

(defun pedz/list-all-matching-lines (regexp &optional nlines)
  (interactive "sList lines matching regexp: \nP")
  (let ((here (point)))
    (goto-char (point-min))
    (occur regexp nlines)
    (goto-char here)))

(defun pedz/recenter-top ()
  "Redraw page with the current line at the top of the page"
  (interactive)
  (recenter 0))

(defun pedz/recenter ()
  "Redraw page with the current line in the middle of the page"
  (interactive)
  (recenter nil))

(defun pedz/recenter-bottom ()
  "Redraw page with the current line at the top of the page"
  (interactive)
  (recenter -1))

;;
;; My own keymap of key bindings is in this map and I hook the map to
;; \C-\\ for now
;;
(defvar pedz/personal-map (make-sparse-keymap)
  "Keymap for all personal key bindings")
(define-key pedz/personal-map (kbd "C-b") #'pedz/recenter-bottom)
(define-key pedz/personal-map (kbd "C-c") #'pedz/toggle-buffer-case-fold-search)
(define-key pedz/personal-map (kbd "C-f") #'auto-fill-mode)
(define-key pedz/personal-map (kbd "C-g") #'goto-line)
(define-key pedz/personal-map (kbd "C-k") #'compile)
(define-key pedz/personal-map (kbd "C-t") #'pedz/recenter-top)
(define-key pedz/personal-map (kbd "C-v") #'view-file)
(define-key pedz/personal-map (kbd "C-w") #'compare-windows)
(define-key pedz/personal-map (kbd "W")   #'whitespace-cleanup)
(define-key pedz/personal-map (kbd "b")   #'bury-buffer)
(define-key pedz/personal-map (kbd "l")   #'pedz/list-all-matching-lines)
(define-key pedz/personal-map (kbd "t")   #'pedz/recenter-top)
(define-key pedz/personal-map (kbd "w")   #'compare-windows)

(define-key global-map (kbd "C-\\") pedz/personal-map)
(define-key global-map (kbd "C-x C-b") #'electric-buffer-list)
(define-key isearch-mode-map (kbd "C-\\") nil)
;; Personal keymap:1 ends here

;; [[file:pedz.org::*Server start][Server start:1]]
(server-start)
;; Server start:1 ends here

;; [[file:pedz.org::*Enable displaying the time in the mode lines][Enable displaying the time in the mode lines:1]]
(display-time)
;; Enable displaying the time in the mode lines:1 ends here

;; [[file:pedz.org::*Quicker point to register / register to point key bindings][Quicker point to register / register to point key bindings:1]]
(define-key global-map (kbd "C-x /") 'point-to-register)
(define-key global-map (kbd "C-x j") 'jump-to-register)
;; Quicker point to register / register to point key bindings:1 ends here

;; [[file:pedz.org::*Tab bar mode][Tab bar mode:1]]
(tab-bar-mode +1)
(global-set-key (kbd "s-}") #'tab-next)
(global-set-key (kbd "s-{") #'tab-previous)
;; Tab bar mode:1 ends here

;; [[file:pedz.org::*Unfill -- used when posting][Unfill -- used when posting:1]]
;;; The compiler tells me:
;;; Warning (bytecomp): ‘replace-regexp’ is for interactive use only; use ‘re-search-forward’ and ‘replace-match’ instead.
;;; so I'll leave this out until I have time to fix it.
;; (defun unfill ()
;;   "Does the opposite of fill.  Lines separated with a single new line
;;   are joined with a single space."
;;   (interactive)
;;   (replace-regexp "\\(.\\)\n\\(.\\)" "\\1 \\2"))

;;; Set Mac type default
(global-set-key "\M-`" 'other-frame)
;; Unfill -- used when posting:1 ends here

;; [[file:pedz.org::*zsh man page search helpers][zsh man page search helpers:1]]
(defun zsh-manpage-search-regexp (string &optional lax)
  "Returns a string to search for entries in the zshall man page"
  (format "\n[A-Z ]*\n \\{7\\}%s%s" string (if lax "" "\\_>")))

(isearch-define-mode-toggle zsh-manpage "z" zsh-manpage-search-regexp "\
Searching zshall man page for where a concept is described")
;; zsh man page search helpers:1 ends here

;; [[file:pedz.org::*TRAMP set up for hatred][TRAMP set up for hatred:1]]
(leaf tramp
  :defvar tramp-connection-properties
  :defer-config
    ;;; This is for working on hatred inside a Docker container:
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/docker:hatred-web-1:")
                     "remote-shell" "/usr/bin/zsh")))
;; TRAMP set up for hatred:1 ends here

;; [[file:pedz.org::*Provide pedz][Provide pedz:1]]
(provide 'pedz)
;; Provide pedz:1 ends here
