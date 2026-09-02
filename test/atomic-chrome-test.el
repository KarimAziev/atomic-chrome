;;; atomic-chrome-test.el --- Tests for Atomic Chrome -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for Atomic Chrome frame geometry calculations.

;;; Code:

(require 'ert)
(require 'atomic-chrome)

(ert-deftest atomic-chrome-prefers-wayland-display-for-pgtk ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "WAYLAND_DISPLAY" "wayland-1")
    (setenv "DISPLAY" ":0")
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (eq feature 'pgtk))))
      (should (equal (atomic-chrome--preferred-display) "wayland-1")))))

(ert-deftest atomic-chrome-display-falls-back-to-x ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "WAYLAND_DISPLAY" "")
    (setenv "DISPLAY" ":1")
    (cl-letf (((symbol-function 'featurep)
               (lambda (_feature &optional _subfeature) nil)))
      (should (equal (atomic-chrome--preferred-display) ":1")))))

(ert-deftest atomic-chrome-linux-pgtk-daemon-uses-wayland-display ()
  (let ((process-environment (copy-sequence process-environment))
        (window-system nil)
        (system-type 'gnu/linux)
        called-with)
    (setenv "WAYLAND_DISPLAY" "wayland-2")
    (setenv "DISPLAY" ":2")
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (eq feature 'pgtk)))
              ((symbol-function 'selected-frame) (lambda () 'daemon-frame))
              ((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) nil))
              ((symbol-function 'make-frame-on-display)
               (lambda (display parameters)
                 (setq called-with (list display parameters))
                 'new-frame)))
      (should (eq (atomic-chrome--create-frame '((width . 80))) 'new-frame))
      (should (equal called-with '("wayland-2" ((width . 80))))))))

(ert-deftest atomic-chrome-existing-pgtk-gui-inherits-its-display ()
  (let ((window-system 'pgtk)
        (x-display-name "wayland-0"))
    (cl-letf (((symbol-function 'make-frame)
               (lambda (parameters) (list 'new-frame parameters)))
              ((symbol-function 'make-frame-on-display)
               (lambda (&rest _arguments)
                 (ert-fail "make-frame-on-display must not be used"))))
      (should (equal (atomic-chrome--create-frame '((height . 52)))
                     '(new-frame ((height . 52))))))))

(ert-deftest atomic-chrome-screen-rect-prefers-screen-coordinates ()
  (should
   (equal (atomic-chrome--screen-rect
           '((x . 1100) (y . 250) (width . 200) (height . 100)
             (left . 100) (top . 50) (right . 300) (bottom . 150)))
          '((left . 1100) (top . 250) (right . 1300) (bottom . 350)
            (width . 200) (height . 100)))))

(ert-deftest atomic-chrome-frame-left-fits-one-side-not-combined-gaps ()
  ;; The old calculation treated two 400-pixel gaps as enough for a
  ;; 500-pixel frame, then placed it partly beyond the right screen edge.
  (should (= (atomic-chrome--calculate-frame-left-position
              '((x . 400) (y . 100) (width . 200) (height . 100))
              500 '(0 0 1000 800))
             500)))

(ert-deftest atomic-chrome-frame-position-stays-in-offset-workarea ()
  (let* ((rect '((x . 1720) (y . 700) (width . 100) (height . 50)))
         (workarea '(1000 20 800 760)))
    (should (= (atomic-chrome--calculate-frame-left-position
                rect 500 workarea)
               1220))
    (should (= (atomic-chrome--calculate-frame-top-position
                rect 400 workarea)
               380))))

(ert-deftest atomic-chrome-frame-position-handles-oversized-frame ()
  (let ((rect '((x . 1200) (y . 400) (width . 100) (height . 50)))
        (workarea '(1000 20 800 760)))
    (should (= (atomic-chrome--calculate-frame-left-position
                rect 900 workarea)
               1000))
    (should (= (atomic-chrome--calculate-frame-top-position
                rect 800 workarea)
               20))))

(ert-deftest atomic-chrome-selects-monitor-containing-most-of-rect ()
  (cl-letf (((symbol-function 'display-monitor-attributes-list)
             (lambda ()
               '(((geometry 0 0 1000 800) (workarea 0 20 1000 780))
                 ((geometry 1000 0 800 800) (workarea 1000 30 800 770))))))
    (should (equal (atomic-chrome--monitor-workarea
                    '((x . 1200) (y . 100) (width . 200) (height . 100)))
                   '(1000 30 800 770)))))

(provide 'atomic-chrome-test)
;;; atomic-chrome-test.el ends here
