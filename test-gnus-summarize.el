;;; test-project-gemini.el --- Tests for project-gemini -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickie smalls

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'ert)
(require 'gnus-summarize)

(defconst gnus-tests-load-file-name (or load-file-name
                                        (buffer-file-name)))

(defmacro gnus-tests-let-customs (bindings &rest forms)
  (declare (indent defun))
  `(let (,@(mapcar #'car bindings))
     (ignore ,@(mapcar #'car bindings))
     ;; only Claude could find this:
     ;; gnus-startup-file is a defcustom using gnus-home-directory
     ;; in GNU emacs, and gnus-home-directory has yet to be overridden.
     (if (string-match-p "commercial" (emacs-version))
	 (funcall #'custom-set-variables
		  ,@(mapcar (apply-partially #'list 'quote) bindings))
       (setq ,@(mapcan (lambda (b) (list (car b) (cadr b))) bindings)))
     ,@forms))

(cl-defmacro gnus-tests-doit (&rest
                              body
                              &key
                              (select-methods)
                              (customs)
                              &allow-other-keys
                              &aux
                              (body
                               (cl-loop until (not (keywordp (car body)))
                                        do (setq body (nthcdr 2 body))
                                        finally return body)))
  (declare (indent defun))
  `(let* ((parent-dir (file-name-directory gnus-tests-load-file-name))
          (default-directory (file-name-as-directory (concat parent-dir "gnus-tests")))
	  (user-emacs-directory default-directory))
     (unless (file-exists-p default-directory)
       (make-directory default-directory))
     (gnus-tests-let-customs
       ((gnus-verbose 8)
        (gnus-home-directory default-directory)
        (gnus-use-dribble-file nil)
        (network-security-level (quote low))
        (gnus-interactive-exit (quote quiet))
        ,@(when select-methods
	    (if (boundp 'gnus-select-methods)
		(list `(gnus-select-methods ,select-methods))
	      (list
	       `(gnus-select-method '(nnnil ""))
	       `(gnus-secondary-select-methods ,select-methods))))
        (message-directory (expand-file-name "Mail"))
        (mail-source-directory (expand-file-name "Mail"))
        (mail-source-crash-box (expand-file-name ".whatev"))
	,@(when (boundp 'gnus-startup-file)
	    (list `(gnus-startup-file (nnheader-concat gnus-home-directory ".newsrc"))))
	,@(when (boundp 'gnus-save-dot-newsrc)
	    (list `(gnus-save-dot-newsrc nil)))
	,@(when (boundp 'gnus-newsrc-file)
	    (list `(gnus-newsrc-file (nnheader-concat gnus-home-directory ".newsrc.eld"))))
	,@(when (boundp 'gnus-dot-newsrc)
	    (list `(gnus-dot-newsrc (nnheader-concat gnus-home-directory ".newsrc"))))
        (gnus-init-file (nnheader-concat gnus-home-directory ".gnus"))
        (gnus-directory (nnheader-concat gnus-home-directory "News/"))
        ,@customs)
       (unwind-protect
           (progn ,@body)
         (cl-macrolet ((safe-delete
                        (x)
                        `(if (cl-search "gnus-tests/" ,x)
                             (delete-directory ,x t)
                           (error "Attempted delete of %s!" ,x))))
	   (safe-delete gnus-home-directory))))))

(ert-deftest basic-op ()
  (gnus-tests-doit
   (custom-set-variables '(gnus-select-method
			   `(nnmaildir "test" (directory ,default-directory))))
   (make-directory (expand-file-name "inbox/cur") t)
   (make-directory (expand-file-name "inbox/new"))
   (make-directory (expand-file-name "inbox/tmp"))
   (with-temp-file (expand-file-name "inbox/new/1.msg")
     (insert "From: sender@example.com
Subject: Test Message
Date: Mon, 01 Jan 2024 12:00:00 +0000
Message-ID: <test1@example.com>

Test body.
"))
   (call-interactively #'gnus)
   (should-not (buffer-live-p gnus-summary-buffer))
   (call-interactively #'gnus-group-read-group)
   (should (buffer-live-p gnus-summary-buffer))
   (with-current-buffer gnus-summary-buffer
     (should (eq (key-binding (kbd "z")) #'gnus-summarize-thread)))
   (call-interactively #'gnus-group-exit)))

(ert-deftest trim-text-fits ()
  (let ((bodies '("short" "text" "here")))
    (should (equal (gnus-summarize--trim-text bodies 100)
		   bodies))))

(ert-deftest trim-text-drops-largest ()
  (let* ((small (make-string 100 ?a))
	 (medium (make-string 200 ?b))
	 (large (make-string 300 ?c))
	 (bodies (list small medium large)))
    (should (equal (gnus-summarize--trim-text bodies 350)
		   (list small medium)))))

(ert-deftest trim-text-preserves-order ()
  (let* ((first (make-string 50 ?x))
	 (second (make-string 300 ?y))
	 (third (make-string 60 ?z))
	 (bodies (list first second third)))
    (should (equal (gnus-summarize--trim-text bodies 150)
		   (list first third)))))

(ert-deftest trim-text-drops-multiple ()
  (let* ((a (make-string 100 ?a))
	 (b (make-string 200 ?b))
	 (c (make-string 300 ?c))
	 (d (make-string 400 ?d))
	 (bodies (list a b c d)))
    (should (equal (gnus-summarize--trim-text bodies 350)
		   (list a b)))))

(provide 'test-gnus-summarize)
;;; test-project-gemini.el ends here
