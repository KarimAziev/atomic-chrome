;;; atomic-chrome.el --- Edit Chrome text areas -*- lexical-binding: t; -*-

;; Copyright (C) 2016 alpha22jp <alpha22jp@gmail.com>
;;           (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.6") (websocket "1.13"))
;; Keywords: browsers, editing, emacs, extensions, tools
;; URL: https://github.com/KarimAziev/atomic-chrome
;; Version: 2.0.0

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is the Emacs version of Atomic Chrome which is an extension for Google
;; Chrome browser that allows you to edit text areas of the browser in Emacs.
;;
;; It's similar to Edit with Emacs, but has some advantages as below with the
;; help of websocket.
;;
;; * Live update
;;   The input on Emacs is reflected to the browser instantly and continuously.
;; * Bidirectional communication
;;   You can edit both on the browser and Emacs, they are synced to the same.
;;
;; Firefox is supported via the GhostText browser addon.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'json)
(require 'let-alist)
(eval-when-compile (require 'subr-x))
(require 'websocket)

(defgroup atomic-chrome nil
  "Edit browser text area with Emacs using Atomic Chrome or GhostText."
  :prefix "atomic-chrome-"
  :group 'applications)

(defcustom atomic-chrome-extension-type-list '(atomic-chrome ghost-text)
  "List of browser extension type available."
  :type '(repeat (choice
                  (const :tag "Atomic Chrome" atomic-chrome)
                  (const :tag "Ghost Text" ghost-text)))
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-open-style 'split
  "Specify the style to open new buffer for editing."
  :type '(choice (const :tag "Open buffer with full window" full)
                 (const :tag "Open buffer with splitted window" split)
                 (const :tag "Open buffer with new frame" frame))
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-frame-width 80
  "Default width of frames for Atomic Chrome editing sessions.

Specifies the width of the frame created for Atomic Chrome editing sessions.

The value is an integer representing the number of characters that can fit in
the width of the frame.

This width setting applies only when a new frame is created for an Atomic Chrome
session, contingent on `atomic-chrome-buffer-open-style' being set to create a
new frame.

This width setting is applied only if the frame creation is triggered by Atomic
Chrome and the `atomic-chrome-buffer-open-style' is set to create a new frame.

Note: This setting overrides `atomic-chrome-frame-parameters'."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-frame-height 52
  "Height of the frame for Atomic Chrome editing sessions, in lines.

Specifies the height of the frame created for Atomic Chrome editing sessions.

Specifies the number of lines in the frame, used when opening a new frame for
editing text areas in a web browser via Atomic Chrome.

This setting is effective only when the value of
`atomic-chrome-buffer-open-style' is set to create a new frame for editing.

Note: This setting overrides `atomic-chrome-frame-parameters'."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-frame-parameters '((alpha-background . 90)
                                            (fullscreen . nil)
                                            (fullboth . nil))
  "Parameters for frames created by Atomic Chrome editing sessions.

Each list element is a cons cell `(PARAMETER . VALUE)`, where `PARAMETER` is the
frame parameter symbol, and `VALUE` is its corresponding setting.

Note: Settings in `atomic-chrome-frame-parameters' are default values for new
frames and may be overridden by specific settings in
`atomic-chrome-buffer-frame-width' and `atomic-chrome-buffer-frame-height' for
width and height, respectively.

Furthermore, when the Atomic Chrome client includes a `rect' with pixel
dimensions and positions, the `left' and `top' positions of the frame may be
automatically calculated and adjusted to align with a text area in a web
browser, unless `left' and `top' are explicitly specified here, which would
disable automatic calculation in favor of the user-defined positions.

This setting is effective only when the value of
`atomic-chrome-buffer-open-style' is set to create a new frame for editing."
  :group 'atomic-chrome
  :type '(repeat (cons :format "%v"
                  (symbol :tag "Parameter")
                  (sexp :tag "Value"))))

(defcustom atomic-chrome-max-text-size-for-position-sync 5000000
  "Maximum text size for syncing cursor position.

Specifies the maximum size of text (in characters) for which cursor position
synchronization is attempted when editing with Atomic Chrome.

If the size of the text being edited exceeds this limit, cursor position
information (line and column) will not be included in the data sent to the
browser extension. This is to prevent performance issues with large texts.

The default value is 5000000, which should be sufficient for most editing tasks.
Adjusting this value may be necessary for working with very large files or to
improve performance on slower systems.

This value should be an integer."
  :group 'atomic-chrome
  :type 'natnum)

(defcustom atomic-chrome-server-ghost-text-port 4001
  "HTTP server port for Ghost Text."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-enable-auto-update t
  "If non-nil, edit on Emacs is reflected to the browser instantly.
If nil, you need to type \"C-cC-s\" manually."
  :type 'boolean
  :group 'atomic-chrome)

(defcustom atomic-chrome-enable-bidirectional-edit t
  "If non-nil, you can edit both on the browser text area and Emacs.
If nil, edit on browser is ignored while editing on Emacs."
  :type 'boolean
  :group 'atomic-chrome)

(defcustom atomic-chrome-default-major-mode 'text-mode
  "Default major mode for editing buffer."
  :type 'function
  :group 'atomic-chrome)

(defcustom atomic-chrome-url-major-mode-alist nil
  "Association list to select a major mode for a website.
Relates URL (or, for GhostText, hostname) regular expressions to
corresponding major modes."
  :type '(alist :key-type (regexp :tag "regexp")
                :value-type (function :tag "major mode"))
  :group 'atomic-chrome)

(defcustom atomic-chrome-edit-mode-hook nil
  "Customizable hook which run when the editing buffer is created."
  :type 'hook
  :group 'atomic-chrome)

(defcustom atomic-chrome-edit-done-hook nil
  "Customizable hook which run when the editing buffer is closed."
  :type 'hook
  :group 'atomic-chrome)

(defcustom atomic-chrome-debug nil
  "Whether to enable debugging for Atomic Chrome.

Enables or disables debug logging for the Atomic Chrome extension integration.
When set to t, debug messages related to websocket communication and data
handling are logged to the *Messages* buffer, aiding in troubleshooting and
development. The default value is nil, indicating that debug logging is turned
off."
  :group 'atomic-chrome
  :type 'boolean)

(defvar atomic-chrome-server-atomic-chrome nil
  "Websocket server connection handle for Atomic Chrome.")

(defvar atomic-chrome-server-ghost-text nil
  "Websocket server connection handle for Ghost Text.")

(defvar atomic-chrome-buffer-table (make-hash-table :test 'equal)
  "Hash table of editing buffer and its assciated data.
Each element has a list consisting of (websocket, frame).")

(defun atomic-chrome-get-websocket (buffer)
  "Look up websocket associated with buffer BUFFER.
Looks in `atomic-chrome-buffer-table'."
  (nth 0 (gethash buffer atomic-chrome-buffer-table)))

(defun atomic-chrome-get-frame (buffer)
  "Look up frame associated with buffer BUFFER.
Looks in `atomic-chrome-buffer-table'."
  (nth 1 (gethash buffer atomic-chrome-buffer-table)))

(defun atomic-chrome-get-buffer-by-socket (socket)
  "Look up buffer which is associated to the websocket SOCKET.
Looks in `atomic-chrome-buffer-table'."
  (let (buffer)
    (cl-loop for key being the hash-keys of atomic-chrome-buffer-table
             using (hash-values val)
             do (when (equal (nth 0 val) socket) (setq buffer key)))
    buffer))

(defun atomic-chrome-close-connection ()
  "Close client connection associated with current buffer."
  (let ((socket (atomic-chrome-get-websocket (current-buffer))))
    (when socket
      (remhash (current-buffer) atomic-chrome-buffer-table)
      (websocket-close socket))))

(defun atomic-chrome-get-selections ()
  "Return the start and end points of the current selection or cursor position."
  (pcase-let* ((multi-poses (and (bound-and-true-p iedit-mode)
                                 (boundp 'iedit-occurrences-overlays)
                                 (mapcar
                                  (lambda (ov)
                                    (cons (overlay-start ov)
                                          (overlay-end ov)))
                                  iedit-occurrences-overlays)))
               (primary-selection (if (and (use-region-p)
                                           (region-active-p))
                                      (cons (region-beginning)
                                            (region-end))
                                    (let* ((pos (point))
                                           (ov-selection (seq-find
                                                          (pcase-lambda
                                                            (`(,b
                                                               .
                                                               ,e))
                                                            (<= b pos e))
                                                          multi-poses)))
                                      (or ov-selection
                                          (cons pos pos))))))
    `[,@(mapcar
         (lambda (ov)
           `((start . ,(1- (car ov)))
             (end . ,(1- (cdr ov)))))
         (if multi-poses
             (delete-dups (append (list primary-selection)
                           multi-poses))
           (list primary-selection)))]))

(defun atomic-chrome-get-update-text-payload ()
  "Generate payload with text and optionally cursor position from buffer."
  (let ((data (list (cons "text" (buffer-substring-no-properties
                                  (point-min)
                                  (point-max)))
                    (cons "selections" (atomic-chrome-get-selections)))))
    (if (> (buffer-size) atomic-chrome-max-text-size-for-position-sync)
        data
      (append data
              (list (cons "lineNumber" (line-number-at-pos (point) t))
                    (cons "column" (1+ (length (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (point))))))))))


(defun atomic-chrome--send-buffer-text ()
  "Send request to update text with current buffer content."
  (let ((socket (atomic-chrome-get-websocket (current-buffer)))
        (payload (atomic-chrome-get-update-text-payload)))
    (when (and socket payload)
      (websocket-send-text
       socket
       (json-encode
        (if (eq (websocket-server-conn socket) atomic-chrome-server-ghost-text)
            payload
          (list '("type" . "updateText")
                (cons "payload" payload))))))))

(defun atomic-chrome-send-buffer-text ()
  "Send request to update text with the current buffer content."
  (interactive)
  (atomic-chrome--send-buffer-text))

(defun atomic-chrome-set-major-mode (url)
  "Set major mode for editing buffer depending on URL.
`atomic-chrome-url-major-mode-alist' can be used to select major mode.
The specified major mode is used if URL matches to one of the alist,
otherwise fallback to `atomic-chrome-default-major-mode'"
  (let ((mode (assoc-default url atomic-chrome-url-major-mode-alist
                             'string-match)))
    (cond ((and buffer-file-name
                (file-name-extension buffer-file-name))
           (set-auto-mode))
          (mode (funcall mode))
          (t (funcall atomic-chrome-default-major-mode)))))

(defun atomic-chrome--safe-substring (str max-width)
  "Extract a substring from STR up to MAX-WIDTH characters.

Argument STR is the string from which a substring is extracted.

Argument MAX-WIDTH is the maximum length of the substring to extract."
  (substring str 0 (min (length str) max-width)))

(defun atomic-chrome--make-frame (title &optional rect)
  "Create a new frame for Atomic Chrome with specified parameters.

Argument TITLE is a string representing the title of the frame.

Optional argument RECT is an alist containing pixel dimensions and positions for
the frame.

If RECT is provided, the left and top position of the frame may be calculated
automatically, allowing the frame to open in alignment with specific elements on
the client side, such as a text area in a web browser. This is useful for
positioning the frame near the area being edited."
  (let ((rect-params (and rect (atomic-chrome-normalize-rect rect)))
        (frame-params (delq nil
                            (append
                             (list (cons 'width
                                         atomic-chrome-buffer-frame-width))
                             (list (cons 'height
                                         atomic-chrome-buffer-frame-height))
                             atomic-chrome-frame-parameters
                             (list (cons 'name (format "Atomic Chrome: %s"
                                                       (atomic-chrome--safe-substring
                                                        title
                                                        90))))))))
    (when rect-params
      (setq frame-params (append frame-params rect-params)))
    (when (and (or (assq 'left frame-params)
                   (assq 'top frame-params)))
      (when (not (cdr (assq 'user-position frame-params)))
        (push '(user-position . t) frame-params)))
    (cond ((memq window-system '(pgtk x))
           (if (or (not x-display-name)
                   (string-match-p "wayland" x-display-name))
               (make-frame frame-params)
             (make-frame-on-display (getenv "DISPLAY") frame-params)))
          ;; Avoid using make-frame-on-display for Mac OS
          ((memq window-system '(ns mac))
           (make-frame frame-params))
          ((memq window-system '(w32))
           (make-frame-on-display "w32" frame-params))
          (t
           (make-frame frame-params)))))

(defun atomic-chrome-show-edit-buffer (buffer title &optional rect)
  "Open or switch to an edit BUFFER with specified dimensions and title.

Argument BUFFER is the buffer to display in the editing window or frame.

Argument TITLE is the title for the editing window or frame.

Optional argument RECT is an alist containing pixel dimensions and positions for
the editing frame."
  (let ((edit-frame (and (eq atomic-chrome-buffer-open-style 'frame)
                         (atomic-chrome--make-frame title rect))))
    (when edit-frame
      (select-frame edit-frame))
    (if (eq atomic-chrome-buffer-open-style 'split)
        (pop-to-buffer buffer)
      (switch-to-buffer buffer))
    (raise-frame edit-frame)
    (select-frame-set-input-focus (window-frame (selected-window)))
    edit-frame))


(defun atomic-chrome-normalize-file-extension (file-extension)
  "Normalize FILE-EXTENSION input to a string or prompt for choice.

Argument FILE-EXTENSION is a string, list, or vector of strings."
  (when (vectorp file-extension)
    (setq file-extension (seq-find #'stringp (append file-extension nil))))
  (when-let ((ext
              (cond ((or (not file-extension)
                         (stringp file-extension))
                     file-extension)
                    ((length> file-extension 1)
                     (completing-read "File extension: "
                                      file-extension))
                    (t (car-safe file-extension)))))
    (if (string-prefix-p "." ext)
        ext
      (concat "." ext))))

(defun atomic-chrome--goto-line (line)
  "Move cursor to the specified LINE number.

Argument LINE is the line number to go to."
  (when line
    (goto-char (point-min))
    (forward-line (1- line))))

(defun atomic-chrome--goto-position (line column)
  "Move cursor to specified LINE and COLUMN.

Argument LINE is the line number to go to.

Argument COLUMN is the column number to go to."
  (when line
    (atomic-chrome--goto-line line))
  (when column
    (move-to-column (1- column))))


(defun atomic-chrome-create-buffer (socket url title text &optional extension
                                           line column rect)
  "Create and prepare a buffer for editing with given TEXT and URL metadata.

Argument SOCKET is an object representing the WebSocket connection.

Argument URL is a string representing the url of the page being edited.

Argument TITLE is a string representing the title of the page being edited.

Argument TEXT is a string containing the initial text to be inserted into the
buffer.

Optional argument EXTENSION is a string specifying the file extension for the
temporary file. It defaults to the EXTENSION derived from URL.

Optional argument LINE is an integer specifying the line number to position the
cursor at.

Optional argument COLUMN is an integer specifying the column number to position
the cursor at."
  (unless extension (setq extension (file-name-extension url)))
  (let* ((suffix (atomic-chrome-normalize-file-extension extension))
         (file (make-temp-file (if (string-empty-p title)
                                   "no-title"
                                 (replace-regexp-in-string "^[-]+\\|[-][-]" ""
                                                           (replace-regexp-in-string
                                                            "[^a-z0-9._-]+" "-"
                                                            title)))
                               nil
                               suffix))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (puthash buffer
               (list socket (atomic-chrome-show-edit-buffer
                             buffer title
                             rect))
               atomic-chrome-buffer-table)
      (atomic-chrome-set-major-mode url)
      (insert text)
      (atomic-chrome--goto-position line column))))

(defun atomic-chrome-close-edit-buffer (buffer)
  "Close buffer BUFFER if it's one of Atomic Chrome edit buffers."
  (let ((frame (atomic-chrome-get-frame buffer))
        (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (save-restriction
        (run-hooks 'atomic-chrome-edit-done-hook)
        (when frame (delete-frame frame))
        (if (and (eq atomic-chrome-buffer-open-style 'split)
                 window)
            (quit-window t window)
          (kill-buffer buffer))))))

(defun atomic-chrome-close-current-buffer ()
  "Close current buffer and connection from client."
  (interactive)
  (save-buffer)
  (atomic-chrome-close-edit-buffer (current-buffer)))

(defun atomic-chrome-update-buffer (socket text &optional line column)
  "Replace buffer content with TEXT at LINE and COLUMN.

Argument SOCKET is the websocket connection associated with the buffer to
update.

Argument TEXT is the string to insert into the buffer.

Optional argument LINE is the line number where the cursor should be placed
after the update.

Optional argument COLUMN is the column number where the cursor should be placed
after the update."
  (let ((buffer (atomic-chrome-get-buffer-by-socket socket)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert text)
        (atomic-chrome--goto-position line column)))))

(defun atomic-chrome--json-parse-string (str &optional object-type array-type
                                             null-object false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object nil
                         :false-object nil)
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object nil))
          (json-false (or false-object nil)))
      (json-read-from-string str))))

(defun atomic-chrome--calculate-frame-left-position (rect)
  "Calculate left position for an frame based on screen and window dimensions.

Argument RECT is an alist containing `left', and `right' with
their respective numeric values in pixels."
  (let* ((screen-width (display-pixel-width))
         (emacs-pix-width
          (* atomic-chrome-buffer-frame-width (frame-char-width)))
         (left-space (alist-get 'left rect))
         (right-space (- screen-width (alist-get 'right rect)))
         (emacs-frame-pos nil))
    (if (>= (+ left-space right-space) emacs-pix-width)
        ;; We have enough space to place Emacs frame without covering the text area
        (if (> left-space right-space)
            ;; Place to the left
            (setq emacs-frame-pos (- left-space emacs-pix-width))
          ;; Place to the right
          (setq emacs-frame-pos (alist-get 'right rect)))
      ;; Not enough space, cover text area starting from its right or left side
      (if (< emacs-pix-width screen-width)
          (if (> left-space right-space)
              ;; Place to the left, covering the area partially or entirely
              (setq emacs-frame-pos (- left-space emacs-pix-width))
            ;; Place to the right, covering the area partially or entirely
            (setq emacs-frame-pos (alist-get 'left rect)))
        ;; Screen is too small, cover the text area completely
        (setq emacs-frame-pos (alist-get 'left rect))))
    emacs-frame-pos))
    
(defun atomic-chrome-normalize-rect (rect)
  "Normalize pixel dimensions to character dimensions in RECT.

Argument RECT is an alist containing pixel dimensions and positions."
  (let ((char-width (frame-char-width))
        (char-height  (frame-char-height))
        (pix-width (alist-get 'width rect))
        (pix-height (alist-get 'height rect))
        (left)
        (height)
        (width))
    (setq height (min
                  (* 2 (/ pix-height char-height))
                  (display-pixel-height)))
    (setq width (/ pix-width char-width))
    (setq left (atomic-chrome--calculate-frame-left-position
                rect))
    (list (cons 'width width)
          (cons 'height height)
          (cons 'left left)
          (cons 'top (alist-get 'top rect)))))

(defvar atomic-chrome-frame-socket-incomplete-payload-hash (make-hash-table
                                                            :test 'eq)
  "Hash table of sockets and its incomplete frames.")

(defun atomic-chrome-on-message (socket frame)
  "Handle data received from the websocket client specified by SOCKET.
FRAME holds the raw data received."
  (let ((raw-payload (websocket-frame-payload frame))
        (incomplete-payloads
         (gethash socket
                  atomic-chrome-frame-socket-incomplete-payload-hash)))
    (cond ((not (websocket-frame-completep frame))
           (puthash socket (append incomplete-payloads
                                   (list raw-payload))
                    atomic-chrome-frame-socket-incomplete-payload-hash))
          (t
           (let* ((combined-payload (if incomplete-payloads
                                        (concat (string-join incomplete-payloads
                                                             "")
                                                raw-payload)
                                      raw-payload))
                  (payload (decode-coding-string
                            (encode-coding-string
                             combined-payload
                             'utf-8)
                            'utf-8))
                  (msg (atomic-chrome--json-parse-string
                        payload)))
             (remhash socket atomic-chrome-frame-socket-incomplete-payload-hash)
             (let-alist msg
               (if (eq (websocket-server-conn socket)
                       atomic-chrome-server-ghost-text)
                   (if (atomic-chrome-get-buffer-by-socket socket)
                       (atomic-chrome-update-buffer socket .text)
                     (atomic-chrome-create-buffer socket .url .title .text
                                                  .extension))
                 (cond ((string= .type "keepalive")
                        (when atomic-chrome-debug
                          (message "atomic chrome: keepalive")))
                       ((string= .type "register")
                        (atomic-chrome-create-buffer socket .payload.url
                                                     .payload.title
                                                     .payload.text
                                                     .payload.extension
                                                     .payload.lineNumber
                                                     .payload.column
                                                     .payload.rect))
                       ((string= .type "updateText")
                        (when atomic-chrome-enable-bidirectional-edit
                          (atomic-chrome-update-buffer socket .payload.text
                                                       .payload.lineNumber
                                                       .payload.column)))))))))))

(defun atomic-chrome-on-close (socket)
  "Function to handle request from client to close websocket SOCKET."
  (let ((buffer (atomic-chrome-get-buffer-by-socket socket)))
    (when buffer (atomic-chrome-close-edit-buffer buffer))))

(defvar atomic-chrome-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'atomic-chrome-send-buffer-text)
    (define-key map (kbd "C-c C-c") #'atomic-chrome-close-current-buffer)
    map)
  "Keymap for minor mode `atomic-chrome-edit-mode'.")


(defun atomic-chrome-unset-buffer-modified ()
  "Mark buffer as unmodified and return true."
  (set-buffer-modified-p nil)
  t)

(define-minor-mode atomic-chrome-edit-mode
  "Minor mode enabled on buffers opened by Emacs Atomic Chrome server."
  :group 'atomic-chrome
  :lighter " AtomicChrome"
  :init-value nil
  :keymap atomic-chrome-edit-mode-map
  (when atomic-chrome-edit-mode
    (add-hook 'kill-buffer-hook #'atomic-chrome-close-connection nil t)
    (add-hook 'kill-buffer-query-functions
              #'atomic-chrome-unset-buffer-modified nil t)
    (when atomic-chrome-enable-auto-update
      (add-hook 'post-command-hook #'atomic-chrome--send-buffer-text
                nil t))))

(defun atomic-chrome-turn-on-edit-mode ()
  "Turn on `atomic-chrome-edit-mode' if the buffer is an editing buffer."
  (when (gethash (current-buffer) atomic-chrome-buffer-table)
    (atomic-chrome-edit-mode t)))

;;;###autoload
(define-global-minor-mode global-atomic-chrome-edit-mode
  atomic-chrome-edit-mode atomic-chrome-turn-on-edit-mode)

(defun atomic-chrome-start-websocket-server (port)
  "Create websocket server on port PORT."
  (websocket-server
   port
   :host 'local
   :on-message #'atomic-chrome-on-message
   :on-open nil
   :on-close #'atomic-chrome-on-close))

(defun atomic-chrome-start-httpd ()
  "Start the HTTP server for Ghost Text query."
  (interactive)
  (make-network-process
   :name "atomic-chrome-httpd"
   :family 'ipv4
   :host 'local
   :service atomic-chrome-server-ghost-text-port
   :filter 'atomic-chrome-httpd-process-filter
   :filter-multibyte nil
   :server t
   :noquery t))

(defun atomic-chrome-normalize-header (header)
  "Destructively capitalize the components of HEADER."
  (mapconcat #'capitalize (split-string header "-") "-"))

(defun atomic-chrome-httpd-parse-string (string)
  "Parse client http header STRING into alist."
  (let* ((lines (split-string string "[\n\r]+"))
         (req (list (split-string (car lines))))
         (post (cadr (split-string string "\r\n\r\n"))))
    (dolist (line (butlast (cdr lines)))
      (push (list (atomic-chrome-normalize-header (car (split-string line ": ")))
                  (mapconcat #'identity
                             (cdr (split-string line ": ")) ": "))
            req))
    (push (list "Content" post) req)
    (reverse req)))

(defun atomic-chrome-httpd-process-filter (proc string)
  "Process filter of PROC which run each time client make a request.
STRING is the string process received."
  (setf string (concat (process-get proc :previous-string) string))
  (let* ((request (atomic-chrome-httpd-parse-string string))
         (content-length (cadr (assoc "Content-Length" request)))
         (content (cadr (assoc "Content" request))))
    (if (and content-length
             (< (string-bytes content)
                (string-to-number content-length)))
        (process-put proc :previous-string string)
      (atomic-chrome-httpd-send-response proc))))

(defun atomic-chrome-httpd-send-response (proc)
  "Send an HTTP 200 OK response back to process PROC."
  (when (processp proc)
    (unless atomic-chrome-server-ghost-text
      (setq atomic-chrome-server-ghost-text
            (atomic-chrome-start-websocket-server 64293)))
    (let ((header "HTTP/1.0 200 OK\nContent-Type: application/json\n")
          (body (json-encode '(:ProtocolVersion 1 :WebSocketPort 64293))))
      (process-send-string proc (concat header "\n" body))
      (process-send-eof proc))))

;;;###autoload
(defun atomic-chrome-start-server ()
  "Start websocket server for atomic-chrome.
Fails silently if a server is already running."
  (interactive)
  (ignore-errors
    (progn
      (and (not atomic-chrome-server-atomic-chrome)
           (memq 'atomic-chrome atomic-chrome-extension-type-list)
           (setq atomic-chrome-server-atomic-chrome
                 (atomic-chrome-start-websocket-server 64292)))
      (and (not (process-status "atomic-chrome-httpd"))
           (memq 'ghost-text atomic-chrome-extension-type-list)
           (atomic-chrome-start-httpd))
      (global-atomic-chrome-edit-mode 1))))

;;;###autoload
(defun atomic-chrome-stop-server ()
  "Stop websocket server for atomic-chrome."
  (interactive)
  (when atomic-chrome-server-atomic-chrome
    (websocket-server-close atomic-chrome-server-atomic-chrome)
    (setq atomic-chrome-server-atomic-chrome nil))
  (when atomic-chrome-server-ghost-text
    (websocket-server-close atomic-chrome-server-ghost-text)
    (setq atomic-chrome-server-ghost-text nil))
  (when (process-status "atomic-chrome-httpd")
    (delete-process "atomic-chrome-httpd"))
  (global-atomic-chrome-edit-mode 0))

;;;###autoload
(defun atomic-chrome-toggle-server ()
  "Toggle the Atomic Chrome server between starting and stopping."
  (interactive)
  (if (or atomic-chrome-server-atomic-chrome
          (process-status "atomic-chrome-httpd"))
      (atomic-chrome-stop-server)
    (atomic-chrome-start-server)))


(provide 'atomic-chrome)
;;; atomic-chrome.el ends here