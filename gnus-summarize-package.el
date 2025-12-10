;;; gnus-summarize-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst gnus-summarize-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst gnus-summarize-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gnus-summarize.el" (gnus-summarize-package-where)))
    (package-buffer-info)))

(defun gnus-summarize-package-name ()
  (concat "gnus-summarize-" (package-version-join
				(package-desc-version
				 (gnus-summarize-package-desc)))))

(defun gnus-summarize-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (gnus-summarize-package-desc))
	(pkg-dir (expand-file-name (gnus-summarize-package-name)
				   (gnus-summarize-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (dolist (el (split-string "gnus-summarize.el"))
      (copy-file (expand-file-name el (gnus-summarize-package-where))
		 (expand-file-name el pkg-dir)))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
