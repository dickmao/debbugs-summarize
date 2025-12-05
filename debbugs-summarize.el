;;; debbugs-summarize.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/debbugs-summarize
;; Package-Requires: ((debbugs "0.46") (spinner "1.7.3"))

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

(require 'debbugs)
(require 'gnus-sum)
(require 'gnus-art)
(require 'spinner)
(require 'auth-source)
(require 'soap-client)

;; gnus reflects this into gnus-article-mode-map
(define-key gnus-summary-mode-map (kbd "z") #'debsum-bug)

(defgroup debbugs-summarize nil
  "Summarize shit."
  :group 'tools
  :prefix "debbugs-summarize-")

(defvar debsum--buffer-alist nil "Alist (BUGNUM . BUFFER)")

(defun debsum--get-api-key ()
  "Get Gemini API key from auth-source."
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com"
       :user "gemini-api")
      (error "No Gemini API key in auth-source")))

(defun debsum--make-citations-clickable ()
  "Find article number references and make them clickable."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "Message \\([0-9]+\\)" nil t)
      (let ((article-num (string-to-number (match-string 1)))
            (start (match-beginning 0))
            (end (match-end 0)))
        (make-text-button
         start end
         'action (lambda (_button)
                   (let ((art article-num))
                     (other-window 1)
                     (gnus-summary-goto-subject art)
                     (gnus-summary-select-article)))
         'face 'link
         'follow-link t
         'help-echo (format "Jump to article %d" article-num))))))

(defmacro debsum-assume-in-summary (&rest body)
  "If we are not in an summary buffer, go there, and execute BODY.  Restore."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (when (or (derived-mode-p 'gnus-summary-mode)
               (when (gnus-buffer-live-p gnus-summary-buffer)
                 (set-buffer gnus-summary-buffer)))
       ,@body)))

(defun debsum--strip-base64-attachments (body)
  "Remove base64 attachments from BODY."
  (replace-regexp-in-string
   "--=_[a-f0-9]+\n\\(?:.*\n\\)*?--=_[a-f0-9]+--\n?" ""
   body))

(defun debsum--strip-emacsbug-template (body)
  "Remove emacsbug.el configuration template from BODY."
  (let* ((marker-regex "^In GNU Emacs")
         (match-pos (string-match marker-regex body)))
    (if (and match-pos (>= (length (replace-regexp-in-string
				    "\\s-" "" (substring body 0 match-pos)))
			   80))
        (substring body 0 match-pos)
      body)))

(defun debsum--bug-header (bug-number status messages)
  (let* ((subject (alist-get 'subject status))
         (severity (alist-get 'severity status))
         (pending-status (alist-get 'pending status))
         (package (car (alist-get 'package status)))
         (date (alist-get 'date status))
         (log-modified (alist-get 'log_modified status))
         (total-count (length messages)))
    (mapconcat
     #'identity
     (delq nil (list (format "Bug #%d: %s" bug-number subject)
		     (format "Status: %s" pending-status)
		     (format "Severity: %s" severity)
		     (when package
		       (format "Package: %s" package))
		     (when date
		       (format "Submitted: %s"
			       (format-time-string "%Y-%m-%d" date)))
		     (when log-modified
		       (format "Last Modified: %s"
			       (format-time-string "%Y-%m-%d" log-modified)))
		     (format "Message Count: %d" total-count)
		     ""))
     "\n")))

(defun debsum--bug-messages (messages)
  (let (lines)
    (dolist (msg messages)
      (let* ((msg-num (alist-get 'msg_num msg))
             (header (alist-get 'header msg))
             (body (alist-get 'body msg))
	     from date subj msg-id in-reply-to references)
        (with-temp-buffer
          (insert header)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (cond
               ((string-match "^From:\\s-*\\(.*\\)$" line)
                (setq from (match-string 1 line)))
               ((string-match "^Date:\\s-*\\(.*\\)$" line)
                (setq date (match-string 1 line)))
               ((string-match "^Subject:\\s-*\\(.*\\)$" line)
                (setq subj (match-string 1 line)))
               ((string-match "^Message-[Ii][Dd]:\\s-*\\(.*\\)$" line)
                (setq msg-id (match-string 1 line)))
               ((string-match "^In-Reply-To:\\s-*\\(.*\\)$" line)
                (setq in-reply-to (match-string 1 line)))
               ((string-match "^References:\\s-*\\(.*\\)$" line)
                (setq references (match-string 1 line)))))
            (forward-line 1)))
        (push (format "---\nMessage %d:" msg-num) lines)
        (when from
          (push (format "From: %s" from) lines))
        (when date
          (push (format "Date: %s" date) lines))
        (when subj
          (push (format "Subject: %s" subj) lines))
        (when msg-id
          (push (format "Message-ID: %s" msg-id) lines))
        (when in-reply-to
          (push (format "In-Reply-To: %s" in-reply-to) lines))
        (when references
          (push (format "References: %s" references) lines))
	(push "" lines)
        (push (debsum--strip-base64-attachments
               (debsum--strip-emacsbug-template body)) lines)
        (push "" lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;###autoload
(defun debsum-bug ()
  "Summarize Bug#XXXX and display in article buffer."
  (interactive)
  (cl-assert (setenv "GEMINI_API_KEY" (debsum--get-api-key)))
  (debsum-assume-in-summary
    (when-let* ((subject (gnus-summary-article-subject))
		(bug-num (when (string-match "bug#\\([0-9]+\\)" subject)
			   (string-to-number (match-string 1 subject))))
		(status (car (debbugs-get-status bug-num)))
		(bug-log (cl-letf (((symbol-function 'soap-validate-xs-basic-type)
				    #'ignore))
			   (debbugs-get-bug-log bug-num)))
		(bug-header (debsum--bug-header bug-num status bug-log))
		(bug-messages (debsum--bug-messages bug-log)))
      ;; (with-current-buffer "*scratch*"
      ;; 	(goto-char (point-max))
      ;; 	(insert bug-header)
      ;; 	(insert "\n")
      ;; 	(insert bug-messages))
      (debsum--get-summary-async bug-num bug-header bug-messages)
      )))

(defun debsum--elpa-dir ()
  (let ((elpa-dir (directory-file-name
		   (file-name-directory
		    (or (locate-library "debbugs-summarize")
			default-directory)))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun debsum--get-summary-async (bug-num bug-header bug-messages)
  "Get summary via Python script, display in article buffer."
  (setq debsum--buffer-alist (assq-delete-all bug-num debsum--buffer-alist))
  (let* ((name (format "debbugs-summarize-Bug#%d" bug-num))
	 (bname (format "*%s*" name))
	 timeout spin-stopper)
    (when (buffer-live-p (get-buffer bname))
      (let (kill-buffer-query-functions)
	(kill-buffer bname)))
    (unwind-protect
	(cl-loop
	 with success-p = nil
	 with default-directory = (debsum--elpa-dir)
	 with proc = (make-process
		      :name name
		      :buffer bname
		      :command (split-string "uv run python summarize.py")
		      :sentinel (lambda (_proc event)
				  (unless success-p
				    (setq success-p (equal (string-trim event)
							   "finished")))))
	 initially (setq spin-stopper (spinner-start)
			 timeout (run-with-timer 30 nil spin-stopper))
	 initially (progn (process-send-string proc bug-messages)
			  (process-send-eof proc))
	 do (accept-process-output proc 0.1)
	 until (or (not (memq timeout timer-list)) (not (process-live-p proc)))
	 finally do (when success-p
		      (setf (alist-get bug-num debsum--buffer-alist)
			    (process-buffer proc))))
      (cancel-timer timeout)
      (funcall spin-stopper)))
  (when-let ((buf (alist-get bug-num debsum--buffer-alist)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(insert bug-header)
	(insert "\n")))
    (debsum--display-article buf)))

(defun debsum--display-article (buffer)
  "Display BUFFER using Gnus article display routines."
  (debsum-assume-in-summary
    (setq-default gnus-newsgroup-name gnus-newsgroup-name)
    ;; No... must use override-method as 0 gets added to newsgroup-reads
    (with-current-buffer (get-buffer-create gnus-original-article-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert-buffer-substring buffer)
	(setq-local gnus-original-group-and-article (cons gnus-newsgroup-name 0))))
    (let ((gnus-article-prepare-hook
	   (list (lambda ()
		   (with-current-buffer gnus-article-buffer
		     (let ((inhibit-read-only t))
		       ;(debsum--make-citations-clickable)
		       (goto-char (point-max))
		       (insert "\n\n---\nPress C-' to ask follow-up questions.\n")
		       (goto-char (point-min))
		       (local-set-key (kbd "C-'") #'debsum-open-chat)))))))
      (gnus-article-prepare 0 nil))
    (let (kill-buffer-query-functions)
      (kill-buffer gnus-original-article-buffer))))

(defun debsum-open-chat ()
  "Open comint buffer for LLM chat."
  (interactive)
  ;; make-comint is idempotent
  (let ((buf (apply #'make-comint "debsum-chat" "uv" nil
		    (split-string "run python chat.py"))))
    (with-current-buffer buf
      (debsum-chat-mode)
      (goto-char (point-max))
      (comint-send-input))
    (pop-to-buffer buf '((display-buffer-at-bottom)
			 (window-height . 0.3)))))

(define-derived-mode debsum-chat-mode comint-mode "Debsum-Chat"
  "Comint mode for LLM chat."
  (setq-local comint-prompt-regexp "^Gemini> ")
  (setq-local comint-use-prompt-regexp t))

(provide 'debsum)

;; Local Variables:
;; read-symbol-shorthands: (("debsum-" . "debbugs-summarize-"))
;; End:

;;; debbugs-summarize.el ends here
