;;; debbugs-summarize-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst debbugs-summarize-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst debbugs-summarize-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "debbugs-summarize.el" (debbugs-summarize-package-where)))
    (package-buffer-info)))

(defun debbugs-summarize-package-name ()
  (concat "debbugs-summarize-" (package-version-join
				(package-desc-version
				 (debbugs-summarize-package-desc)))))

(defun debbugs-summarize-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (debbugs-summarize-package-desc))
	(pkg-dir (expand-file-name (debbugs-summarize-package-name)
				   (debbugs-summarize-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (dolist (el (split-string "debbugs-summarize.el debbugs-summarize-generated.el"))
      (copy-file (expand-file-name el (debbugs-summarize-package-where))
		 (expand-file-name el pkg-dir)))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
