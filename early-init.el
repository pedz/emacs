;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(dolist (dir '( "lib/packed" "lib/auto-compile" "lib/compat" ))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;;
;; This is stolen from the master branch of Emacs.  It is not in the
;; emacs-28 branch so I assume it will be in the stock Emacs until
;; Emacs 29.  This is used by epkg.
;;
(defun file-name-split (filename)
  "Return a list of all the components of FILENAME.
On most systems, this will be true:

  (equal (string-join (file-name-split filename) \"/\") filename)"
  (let ((components nil))
    ;; If this is a directory file name, then we have a null file name
    ;; at the end.
    (when (directory-name-p filename)
      (push "" components)
      (setq filename (directory-file-name filename)))
    ;; Loop, chopping off components.
    (while (length> filename 0)
      (push (file-name-nondirectory filename) components)
      (let ((dir (file-name-directory filename)))
        (setq filename (and dir (directory-file-name dir)))
        ;; If there's nothing left to peel off, we're at the root and
        ;; we can stop.
        (when (and dir (equal dir filename))
          (push (if (equal dir "") ""
                  ;; On Windows, the first component might be "c:" or
                  ;; the like.
                  (substring dir 0 -1))
                components)
          (setq filename nil))))
    components))
