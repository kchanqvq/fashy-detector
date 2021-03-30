(ql:quickload "inferior-shell")
(ql:quickload "cl-json")
(defvar fashy-github-usernames (make-hash-table :test 'equal))
(defvar fashy-emails (make-hash-table :test 'equal))
(defvar fashy-repo-path "../../playground/rms-open-letter.github.io/")
(defmacro print-progress (prompt &body body)
  `(let ((old-count (hash-table-count fashy-github-usernames)))
     ,@body
     (remhash "'upstream" fashy-github-usernames)
     (remhash "" fashy-emails)
     (format t "~a: ~s new fashy github usernames added ~%"
             ,prompt (- (hash-table-count fashy-github-usernames) old-count))))
(defun manual-fashy (name &optional email)
  (setf (gethash name fashy-github-usernames) t)
  (when (setf (gethash email fashy-emails) t)))
(print-progress "Commit author email"
  (mapc (lambda (email)
          (destructuring-bind (&optional name domain) (uiop:split-string email :separator "@")
            (if (equal domain "users.noreply.github.com")
                (destructuring-bind (garbage &optional user)
                    (uiop:split-string name :separator "+")
                  (setf (gethash (or user garbage) fashy-github-usernames) t))
                (setf (gethash email fashy-emails) t))))
        (inferior-shell:run/lines `(progn
                                     (cd ,fashy-repo-path)
                                     (git log "--format=%ae")))))
(print-progress "Merge pull request #42 from fashy/xddd"
  (mapc (lambda (message)
          (let ((message (uiop:split-string message)))
            (when (equal (car message) "Merge")
              (destructuring-bind (user &rest branch)
                  (uiop:split-string (car (last message)) :separator "/")
                (when branch
                  (setf (gethash user fashy-github-usernames) t))))))
        (inferior-shell:run/lines `(progn
                                     (cd ,fashy-repo-path)
                                     (git log "--format=%s")))))
(print-progress "Manual input"
  (manual-fashy "travisbrown" "travisrobertbrown@gmail.com"))
(print-progress "GitHub API"
  (loop for i from 0
        for fetch = (cl-json:decode-json-from-string
                     (inferior-shell:run/s `(curl ,(format nil "https://api.github.com/repos/rms-open-letter/rms-open-letter.github.io/contributors?per_page=100&page=~s" i))))
        while fetch
        do (mapc (lambda (item)
                   (setf (gethash (cdr (assoc :login item)) fashy-github-usernames) t))
                 fetch)))
(defun write-hash-table (hash-table file)
  (with-open-file (output file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (let (list)
      (maphash (lambda (k v) (when v (push k list))) hash-table)
      (setq list (sort list #'string<=))
      (mapc (lambda (item) (write-line item output)) list))))
(write-hash-table fashy-github-usernames "~/Projects/fashy-detector/github-users.txt")
(write-hash-table fashy-emails "~/Projects/fashy-detector/emails.txt")
