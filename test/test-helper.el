;;; test-helper.el --- Shared helpers for atomic-chrome tests -*- lexical-binding: t; -*-

(let* ((current-file (or load-file-name buffer-file-name))
       (repo-root (expand-file-name ".." (file-name-directory current-file))))
  (add-to-list 'load-path repo-root)
  (dolist (dep '("../emacs-websocket" "../let-alist"))
    (let ((candidate (expand-file-name dep repo-root)))
      (when (file-directory-p candidate)
        (add-to-list 'load-path candidate)))))

(require 'ert)
(require 'atomic-chrome)

;;; Provide helper utilities for tests.

(defmacro atomic-chrome-test-with-temp-directory (&rest body)
  "Evaluate BODY inside a temporary directory and clean it up afterwards."
  `(let* ((default-directory (make-temp-file "atomic-chrome-test" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory default-directory t))))

(provide 'test-helper)

;;; test-helper.el ends here
