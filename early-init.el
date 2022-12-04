;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

;;; I like debugging via printfs.  This makes it easier.
(defun pedz/debug ( &rest args )
  "Simply forwards ARGS to `message'.  Using a different name makes
it easier to go back, find and remove messages added simply for
debugging purposes"
  (apply #'message args))


;;; Needed for pedz/org-require below
;;(require 'org)
(autoload 'org-babel-tangle-file "ob-tangle" "\
Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.

Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.

Optional argument LANG-RE can be used to limit the exported
source code blocks by languages matching a regular expression.

Return a list whose CAR is the tangled file name.

\(fn FILE &optional TARGET-FILE LANG-RE)" t nil)


;;; These came from the instructions to emacscollective/borg and
;;; emacscollective/auto-compile.  Currently I never want to use
;;; package.el thus I set package-enable-at-startup to nil.  I use
;;; Borg instead.
;;;
;;; Stock Emacs has load-prefer-newer which loads the .el file if it
;;; is newer than the .elc file but it does not byte compile it.  The
;;; auto-compile package causes Elisp files to be byte compiled if
;;; needed when saved and also on the load path.  Neither one creates
;;; the byte compiled file unless it already exists.
;;;
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(dolist (dir '( "lib/packed" "lib/auto-compile" "lib/compat" ))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;;;
;;; In shifting from ~.el~ files to ~.org~ files, it seems prudent to
;;; have an ~org-require~ function similar to `require' but will
;;; search and find the ~.org~ file and the ~.el~ file and if the
;;; ~.org~ file is younger or the ~.el~ file does not exist, then
;;; tangle the ~.org~ file to create the ~.el~ file and ultimately
;;; require the ~.el~ file.
;;; 
;;; I keep adding and then removing code to byte compile the '.el'
;;; file if need be.  Currently it is in.  I'm thinking the next time
;;; I remove it, I'll add an optional argument instead of just
;;; deleting the code.
;;; 
(defun pedz/org-require ( sym &optional dont-load paths )
  "An Orgmode tangle replacement for `require'.

Given SYM, searches PATHS looking for `SYM.el' or `SYM.org'.  If
a `SYM.org' file is found first, it is tangled to produce
`SYM.el'.  When the `SYM.el' file is found (perhaps just after
being created), it is byte compiled to `SYM.elc'.

Unless DONT-LOAD is true, SYM is then `require'd."
  (pedz/debug "pedz/org-require for %s" (symbol-name sym))
  (let ((search-paths (or paths load-path)))
    (catch 'found
      (dolist (path search-paths)
	(let* ((basename (symbol-name sym))
	       (el-path  (expand-file-name (concat basename ".el")  path))
	       (elc-path (expand-file-name (concat basename ".elc") path))
	       (org-path (expand-file-name (concat basename ".org") path))
	       (done nil))
	  (when (file-exists-p org-path)
	    (setq done t)
	    (if (file-newer-than-file-p org-path el-path)
		(progn
		  (message "Tangle %s" org-path)
		  (org-babel-tangle-file org-path el-path))))
	  (when (file-exists-p el-path)
	    (setq done t)
	    (if (file-newer-than-file-p el-path elc-path)
		(progn
		  (message "Compile %s" el-path)
		  (byte-compile-file el-path))))
	  (when done
	    (throw 'found t))))))
  (unless dont-load
    (require sym)))

;;; Generate init.el.  It lives in the `user-emacs-directory' and does
;;; not need to be loaded because the start up process will do that
;;; for us.
(pedz/org-require 'init t (list user-emacs-directory))
