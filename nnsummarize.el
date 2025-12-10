;;; nnsummarize.el --- nnnil modulo request-article  -*- lexical-binding: t; -*-

(require 'nnheader)

(defun nnsummarize-retrieve-headers (_articles &optional _group _server _fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nnsummarize-open-server (_server &optional _definitions)
  t)

(defun nnsummarize-close-server (&optional _server _defs)
  t)

(defun nnsummarize-request-close ()
  t)

(defun nnsummarize-server-opened (&optional _server)
  t)

(defun nnsummarize-status-message (&optional _server)
  "")

(defun nnsummarize-request-head (_id &optional _group _server)
  nil)

(defun nnsummarize-request-article (_article &optional group _server _to-buffer)
  (setq-local gnus-article-buffer (current-buffer))
  (cons (or group gnus-newsgroup-name) "foo"))

(defun nnsummarize-request-group (_group &optional _server _fast _info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "411 no such news group\n")))
  nil)

(defun nnsummarize-close-group (_group &optional _server)
  t)

(defun nnsummarize-request-list (&optional _server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  t)

(defun nnsummarize-request-post (&optional _server)
  nil)

(provide 'nnsummarize)

;;; nnsummarize.el ends here
