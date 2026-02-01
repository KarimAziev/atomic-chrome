;;; atomic-chrome-tests.el --- ERT tests for atomic-chrome -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'test-helper)
(require 'cl-lib)
(require 'subr-x)

(ert-deftest atomic-chrome-normalize-header/capitalizes-components ()
  (should (equal (atomic-chrome-normalize-header "content-length") "Content-Length"))
  (should (equal (atomic-chrome-normalize-header "x-forwarded-for")
                 "X-Forwarded-For")))

(ert-deftest atomic-chrome-httpd-parse-string/returns-request-alist ()
  (let* ((payload "body")
         (request (concat "POST /edit HTTP/1.1\r\n"
                          "Host: example.com\r\n"
                          "Content-Type: text/plain\r\n"
                          "Content-Length: 4\r\n\r\n"))
         (parsed (atomic-chrome-httpd-parse-string (concat request payload))))
    (should (equal (car parsed) '("POST" "/edit" "HTTP/1.1")))
    (should (equal (cadr parsed) '("Host" "example.com")))
    (should (equal (caddr parsed) '("Content-Type" "text/plain")))
    (should (equal (cadddr parsed) '("Content-Length" "4")))
    (should (equal (car (last parsed)) '("Content" "body")))))

(ert-deftest atomic-chrome--safe-substring/respects-max-width ()
  (should (equal (atomic-chrome--safe-substring "abcdef" 3) "abc"))
  (should (equal (atomic-chrome--safe-substring "abc" 10) "abc")))

(ert-deftest atomic-chrome--title-to-basename/sanitizes-disallowed-characters ()
  (let ((atomic-chrome-max-filename-size 20))
    (should (equal (atomic-chrome--title-to-basename "Foo Bar Baz!?")
                   "Foo-Bar-Baz-"))
    (should (equal (atomic-chrome--title-to-basename "***")
                   "no-title"))
    (should (equal (atomic-chrome--title-to-basename
                    (make-string 50 ?a))
                   (make-string 20 ?a)))))

(ert-deftest atomic-chrome--filename-with-counter/increments-for-existing-files ()
  (atomic-chrome-test-with-temp-directory
   (let ((file-a (expand-file-name "report-0.txt"))
         (file-b (expand-file-name "report-1.txt")))
     (write-region "a" nil file-a)
     (write-region "b" nil file-b)
     (let ((next (atomic-chrome--filename-with-counter "report.txt" default-directory)))
       (should (string-match-p "report-2.txt\\'" next))
       ;; ensure there is no clash and repeated call advances counter
       (write-region "" nil next nil 'silent)
       (should (string-match-p "report-3.txt\\'"
                               (atomic-chrome--filename-with-counter "report.txt"
                                                                     default-directory)))))))

(ert-deftest atomic-chrome--get-dir-strategy/respects-url-and-extension-rules ()
  (let* ((base-dir (or temporary-file-directory default-directory))
         (markdown-dir (expand-file-name "markdown" base-dir))
         (wiki-dir (expand-file-name "wiki" base-dir))
         (atomic-chrome-create-file-strategy
          `((,markdown-dir :extension ("md") :url ("github.com"))
            (,wiki-dir :extension (nil) :url ("redmine"))
            (buffer :url ("internal.example"))
            (temp-directory))))
    (should (equal (atomic-chrome--get-dir-strategy "https://github.com/x"
                                                    "md")
                   markdown-dir))
    (should (eq (atomic-chrome--get-dir-strategy "https://internal.example/y"
                                                 "txt")
                'buffer))
    ;; nil extension should hit :extension (nil) clause
    (should (equal (atomic-chrome--get-dir-strategy "https://redmine/issues/1"
                                                    nil)
                   wiki-dir))
    ;; fallback to temp-directory when no rule matches
    (should (eq (atomic-chrome--get-dir-strategy "https://example.com"
                                                 "el")
                'temp-directory))))

(ert-deftest atomic-chrome-make-file/creates-files-in-directories ()
  (atomic-chrome-test-with-temp-directory
   (let ((atomic-chrome-create-file-strategy default-directory)
         (atomic-chrome-max-filename-size 50))
     (let ((path (atomic-chrome-make-file "My Title" "txt" "https://example.com")))
       (should (string-prefix-p (file-truename default-directory)
                                (file-truename (file-name-directory path))))
       (should (file-exists-p path)))))
  (atomic-chrome-test-with-temp-directory
   (let ((temporary-file-directory default-directory)
         (atomic-chrome-create-file-strategy 'temp-directory)
         (atomic-chrome-max-filename-size 50))
     (let ((path (atomic-chrome-make-file "Another" "log" "https://example.com")))
       (should (file-exists-p path))
       (should (string-prefix-p (file-truename default-directory)
                                (file-truename (file-name-directory path)))))))
  (let ((atomic-chrome-create-file-strategy 'buffer))
    (should-not (atomic-chrome-make-file "No File" "txt" "https://example.com"))))

(ert-deftest atomic-chrome--json-parse-string/respects-object-and-array-types ()
  (let* ((json-str "{\"title\":\"hi\",\"items\":[1,2,3]}")
         (parsed (atomic-chrome--json-parse-string json-str 'alist 'vector)))
    (should (equal (cdr (assq 'title parsed)) "hi"))
    (should (equal (vector 1 2 3) (cdr (assq 'items parsed)))))
  ;; Force fallback path by faking `json-available-p'.
  (when (fboundp 'json-available-p)
    (cl-letf (((symbol-function 'json-available-p)
               (lambda () nil)))
      (let* ((json-str "{\"count\":5,\"flags\":[true,false]}")
             (parsed (atomic-chrome--json-parse-string json-str 'plist 'list)))
        (should (equal (plist-get parsed :count) 5))
        (should (equal (plist-get parsed :flags) '(t nil)))))))

(ert-deftest atomic-chrome--calculate-frame-left-position/adjusts-by-available-space ()
  (cl-letf (((symbol-function 'display-pixel-width) (lambda () 1200))
            ((symbol-function 'frame-char-width) (lambda (&optional _) 10)))
    (let ((atomic-chrome-buffer-frame-width 50))
      ;; more space on the right, expect right edge
      (should (= (atomic-chrome--calculate-frame-left-position
                  '((left . 100) (right . 500)))
                 500))
      ;; more space on the left, expect placement on the left
      (should (= (atomic-chrome--calculate-frame-left-position
                  '((left . 700) (right . 850)))
                 200)))))

(ert-deftest atomic-chrome-normalize-rect/converts-pixels-to-characters ()
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&optional _) 8))
            ((symbol-function 'frame-char-height) (lambda (&optional _) 16))
            ((symbol-function 'display-pixel-height) (lambda () 1080))
            ((symbol-function 'display-pixel-width) (lambda () 1920)))
    (let ((atomic-chrome-buffer-frame-width 80)
          (rect '((width . 400)
                  (height . 200)
                  (left . 100)
                  (right . 500)
                  (top . 42))))
      (let ((result (atomic-chrome-normalize-rect rect)))
        (should (equal (alist-get 'width result) 50))
        (should (equal (alist-get 'height result) 24))
        (should (equal (alist-get 'left result) 500))
        (should (equal (alist-get 'top result) 42))))))

(ert-deftest atomic-chrome-normalize-file-extension/handles-collections ()
  (should (equal (atomic-chrome-normalize-file-extension ".txt") "txt"))
  (should (equal (atomic-chrome-normalize-file-extension '("el")) "el"))
  (should (equal (atomic-chrome-normalize-file-extension nil) nil))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _)
               (car collection))))
    (should (equal (atomic-chrome-normalize-file-extension '("md" "org"))
                   "md")))
  (should (equal (atomic-chrome-normalize-file-extension
                  (vector ".css" ".scss"))
                 "css")))

(provide 'atomic-chrome-tests)

;;; atomic-chrome-tests.el ends here
