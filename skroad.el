;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

(require 'browse-url)

;;; Knobs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--debug t)

(unless skroad--debug
  (setq byte-compile-warnings nil))

;;; User data and Special Nodes. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--data-directory "~/skrode" "All user data is found here.")

(defconst skroad--file-extension "skroad"
  "File extension denoting a skroad node.")

(defconst skroad--regexp-text-in-brackets
  (rx (* blank) (+ (not (any "[]" blank ?\n))) (*? (not (any "[]\n"))))
  "Regexp matching text that could be delimited by square brackets.")

;;; Fonts. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--floating-title-enable t
  "Display floating title at the top of the window if title is not in view.")

(defface skroad--text-face '((t :inherit default))
  "Default face used for skrode text types."
  :group 'skroad-faces)

(defface skroad--selector-face
  '((t :inherit highlight :extend t))
  "Face for use with atomic selections."
  :group 'skroad-faces)

(defface skroad--indirect-renamer-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "ForestGreen"))
  "Face for use with indirect (via link) renamer."
  :group 'skroad-faces)

(defface skroad--title-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "purple"
       :height 300 :weight bold :extend t))
  "Face for skroad node titles."
  :group 'skroad-faces)

(defface skroad--direct-renamer-face
  '((t :inherit skroad--title-face
       :foreground "white" :background "ForestGreen"))
  "Face for use with direct (via node title line) renamer."
  :group 'skroad-faces)

(defconst skroad--renamer-faces-invalid-background "red"
  "Background colour during invalid renamer state.")

(defconst skroad--block-backgrounds
  ["#202050" "#204020" "#5c2020" "#4a2050" "#205050"]
  "Pool of block backgrounds.")

(defface skroad--highlight-link-face
  '((t :inherit highlight))
  "Face used for highlighted links."
  :group 'skroad-faces)

(defface skroad--live-link-face
  '((t :inherit link))
  "Face used for live links."
  :group 'skroad-faces)

(defface skroad--stub-link-face
  '((t :inherit link :foreground "Orange"))
  "Face used for live stub links."
  :group 'skroad-faces)

(defface skroad--self-link-face
  '((t :inherit link :foreground "white" :background "purple"))
  "Face used for live self links."
  :group 'skroad-faces)

(defface skroad--dead-link-face
  '((t :inherit link :foreground "red"))
  "Face used for dead links."
  :group 'skroad-faces)

(defface skroad--url-link-face
  '((t :inherit link))
  "Face used for url links."
  :group 'skroad-faces)

(defface skroad--heading-face
  '((t :inherit skroad--text-face
       :extend t :height 1.2 :inverse-video t))
  "Face used for skroad heading text."
  :group 'skroad-faces)

(defface skroad--atomic-comment-face
  '((t :inherit skroad--text-face
       :inverse-video t))
  "Face used for atomic comments."
  :group 'skroad-faces)

(defface skroad--node-tail-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "purple"
       :weight bold))
  "Face used for skroad tails."
  :group 'skroad-faces)

;; (defface skroad--timestamp-face
;;   '((t :inherit skroad--text-face
;;        :foreground "black" :background "white"))
;;   "Face used for timestamps."
;;   :group 'skroad-faces)

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--info (&rest args)
  "Message with ARGS in the echo bar without polluting the message buffer."
  (let ((message-log-max nil))
    (if (car args)
        (apply #'message args)
      (when (current-message)
        (message nil)))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun skroad--hash-empty-p (hash)
  "Return t if HASH is empty."
  (zerop (hash-table-count hash)))

(defun skroad--mode-p ()
  "Determine whether skroad mode is currently active."
  (derived-mode-p 'skroad-mode))

;; TODO: skroad--clear-buf-undo-info ?
(defmacro skroad--with-file (file &rest body)
  "Evaluate BODY, operating on FILE (must exist).  Use existing buffer, if any."
  (declare (indent defun))
  (let ((visiting-buffer (make-symbol "visiting-buffer")))
    `(let ((,visiting-buffer (find-buffer-visiting ,file)))
       (if ,visiting-buffer ;; If a buffer is visiting this file, use it:
           (with-current-buffer ,visiting-buffer
             (let ((inhibit-read-only t)) ;; Allow changing special nodes
               (save-mark-and-excursion
                 (atomic-change-group ,@body))))
         (with-temp-buffer ;; No visiting buffer, so make one:
           (insert-file-contents ,file t nil nil t) ;; set as unmodified
           ,@body)))))

(defmacro skroad--visit-open-nodes (&rest body)
  "Evaluate BODY in each currently-open node buffer."
  (declare (indent defun))
  (let ((visiting-buffer (make-symbol "visiting-buffer")))
    `(dolist (,visiting-buffer (buffer-list))
       (with-current-buffer ,visiting-buffer
         (when (skroad--mode-p)
           ,@body)))))

(defun skroad--keyword-to-symbol (exp)
  "If EXP is a keyword, convert it to a symbol.  If not, return it as-is."
  (unless (keywordp exp) (error "%s is not a keyword!" exp))
  (read (substring (symbol-name exp) 1)))

(defmacro skroad--do-plist (key val plist &rest body)
  "Evaluate BODY for side-effects with KEY,VAL bound to each pair in PLIST."
  (declare (indent defun))
  (let ((l (gensym)))
    `(let ((,l ,plist))
       (while ,l
         (let ((,key (car ,l)) (,val (cadr ,l)))
           ,@body
           (setq ,l (cddr ,l)))))))

(defun skroad--current-kill-text ()
  "Return a copy of the current kill (plain) text.  Does not modify anything."
  (substring-no-properties (current-kill 0 t)))

(defun skroad--get-start-of-line (pos)
  "Get the position of the start of the line on which POS resides."
  (save-mark-and-excursion (goto-char pos) (line-beginning-position)))

(defun skroad--get-end-of-line (pos)
  "Get the position of the end of the line on which POS resides."
  (save-mark-and-excursion (goto-char pos) (line-end-position)))

(defmacro skroad--with-whole-lines (start end &rest body)
  "Get expanded region defined by START and END that spans whole lines."
  (declare (indent defun))
  `(let ((start-expanded (skroad--get-start-of-line ,start))
         (end-expanded (skroad--get-end-of-line ,end)))
     ,@body))

(defun skroad--delete-line-if-empty ()
  "If the point is currently on an empty line, delete the line."
  (when (and (bolp) (eolp)) (delete-line)))

(defun skroad--zap-match ()
  "Delete the current match, as well as the empty line which may result."
  (delete-region (match-beginning 0) (match-end 0))
  (skroad--delete-line-if-empty))

(defun skroad--prop-at (prop &optional pos)
  "Determine value of PROP, if any, including overlays, at POS (or point)."
  (get-char-property (or pos (point)) prop))

(defun skroad--overlay-active-p (overlay)
  "Determine whether OVERLAY is currently active."
  (and (overlayp overlay) (eq (current-buffer) (overlay-buffer overlay))))

(defun skroad--re-search (finder regexp &optional limit filter)
  "Find REGEXP using FINDER, to LIMIT; filter by FILTER, if given."
  (if (functionp filter)
      (let (found found-match)
        (save-mark-and-excursion
          (while (and (not found) (funcall finder regexp limit t))
            (when (funcall filter)
              (setq found (point)
                    found-match (match-data t)))))
        (when found
          (set-match-data found-match)
          (goto-char found)))
    (funcall finder regexp limit t)))

(defun skroad--re-foreach (regexp fn &optional filter start end)
  "Execute FN for each match of REGEXP; optionally filtered by FILTER.
If START and/or END are given, search only in that range.
Return t if there were any matches, otherwise nil."
  (save-mark-and-excursion
    (goto-char (or start (point-min)))
    (let (did-any)
      (while (skroad--re-search #'re-search-forward regexp end filter)
        (setq did-any t)
        (funcall fn))
      did-any)))

(defun skroad--clean-whitespace (s)
  "Remove excess whitespace from S."
  (save-match-data (string-clean-whitespace s)))

(defun skroad--abbrev-string (string lim)
  "If STRING is LIM or more characters long, truncate it with an ellipsis."
  (if (< (length string) lim)
      string
    (concat (substring string 0 (max 0 (1- lim))) "…")))

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (skroad--mode-p)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

(defun skroad--clear-buf-undo-info ()
  "Clear the undo history for the current buffer."
  (setq-local buffer-undo-list nil))

;; Title/body positions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--goto-node-body-start ()
  "Jump to the position at the start of the current node's body."
  (goto-char (point-min))
  (forward-line 1))

(defun skroad--in-node-title-p (&optional pos)
  "Return t if POS (or point, if not given) is inside the current node's title."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (= (pos-bol) (point-min))))

(defun skroad--in-node-body-p (&optional pos)
  "Return t if POS (or point, if not given) is inside the current node's body."
  (not (skroad--in-node-title-p pos)))

;; Idle queue for background ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--idle-work-queue nil
  "FIFO work queue of thunks to run during idle time.")

(defvar skroad--idle-work-queue-tail nil
  "Last cons cell of `skroad--idle-work-queue', for O(1) tail insertion.")

(defvar skroad--idle-work-count 0
  "Number of work items currently queued.")

(defvar skroad--idle-work-timer nil
  "The idle timer driving the work queue, or nil if not scheduled.")

(defvar skroad--idle-work-epsilon 0.05
  "Idle seconds before the work queue starts draining.")

(defvar skroad--idle-work-quantum 0.1
  "Max wall-clock seconds to spend draining per idle cycle.")

(defun skroad--idle-enqueue (fn)
  "Push FN onto the back of the idle queue."
  (setq skroad--idle-work-count (1+ skroad--idle-work-count))
  (let ((cell (list fn)))
    (if skroad--idle-work-queue-tail
        (setcdr skroad--idle-work-queue-tail cell)
      (setq skroad--idle-work-queue cell))
    (setq skroad--idle-work-queue-tail cell))
  (skroad--idle-ensure-timer))

(defun skroad--idle-ensure-timer ()
  "Schedule the idle timer if it isn't already running and work exists."
  (unless (or skroad--idle-work-timer (null skroad--idle-work-queue))
    (setq skroad--idle-work-timer
          (run-with-idle-timer
           skroad--idle-work-epsilon t #'skroad--idle-work-run-slice))))

(defun skroad--idle-pop ()
  "Pop the head of the queue, keeping tail consistent."
  (setq skroad--idle-work-count (1- skroad--idle-work-count))
  (let ((fn (pop skroad--idle-work-queue)))
    (unless skroad--idle-work-queue
      (setq skroad--idle-work-queue-tail nil))
    fn))

(defun skroad--idle-report ()
  "Display a message reporting the number of tasks remaining in the queue."
  (skroad--info
   (format "Skroad: %d tasks queued..." skroad--idle-work-count)))

(defun skroad--idle-work-run-slice (&optional flush)
  "Pop and run thunks until the queue is empty or the quantum has elapsed.
If FLUSH is true, ignore the quantum and work until the queue is empty."
  (let ((deadline (+ (float-time) skroad--idle-work-quantum)))
    (while (and skroad--idle-work-queue
                (or flush (< (float-time) deadline)))
      (when flush (skroad--idle-report))
      (with-demoted-errors "skroad--idle-work: %S"
        (funcall (skroad--idle-pop))))
    (when skroad--idle-work-timer
      (cancel-timer skroad--idle-work-timer)
      (setq skroad--idle-work-timer nil))
    (cond (skroad--idle-work-queue
           (skroad--idle-report)
           (run-at-time 0 nil #'skroad--idle-ensure-timer))
          (t (skroad--info nil)))))

(defmacro skroad--defer (&rest body)
  "Schedule BODY to run later."
  `(skroad--idle-enqueue (lambda () ,@body)))

(defmacro skroad--defer-in-current-buffer (&rest body)
  "Schedule BODY to run later in the current buffer, supposing it remains live."
  `(let ((here (current-buffer)))
     (skroad--idle-enqueue
      (lambda ()
        (when (buffer-live-p here) (with-current-buffer here ,@body))))))

(defun skroad--complete-all-deferred ()
  "Ensure that the work queue is empty by running all pending work immediately."
  (skroad--idle-work-run-slice t))

;; File and directory ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: prevent backup files litter?
(defun skroad--save-current-node ()
  "Save the current node."
  (save-buffer))

(defun skroad--mv-file (old-file new-file &optional overwrite)
  "Move OLD-FILE to NEW-FILE (may NOT be equal), updating buffers if required.
If OVERWRITE is t, allow overwriting.  Return success."
  (when (and
         (file-readable-p old-file)
         (or overwrite (not (file-exists-p new-file)))
         (file-writable-p new-file)
         (not (file-equal-p old-file new-file)))
    (rename-file old-file new-file overwrite)
    (when (and (not (file-exists-p old-file)) (file-readable-p new-file))
      (let ((visiting-buffer (find-buffer-visiting old-file)))
        (when visiting-buffer
          (with-current-buffer visiting-buffer
            (set-visited-file-name new-file t t))))
      t)))

(defun skroad--ensure-directory (dir)
  "Ensure that DIR exists, and return t on success."
  (or (file-accessible-directory-p dir)
      (progn (make-directory dir) (file-accessible-directory-p dir))))

(defun skroad--storage-ensure ()
  "Ensure that the data directories exist and may be accessed."
  (unless (skroad--ensure-directory skroad--data-directory)
    (error "Unable to access or create skroad data directory!")))

(defun skroad--append-extension (filename)
  "Append the Skroad file extension to FILENAME."
  (concat filename "." skroad--file-extension))

(defun skroad--node-title-to-filename (node)
  "Encode the NODE title into an appropriate filename (with our extension).
Reserved characters are percent-encoded (%XX).  Leading/trailing spaces are
stripped and interior whitespace runs collapsed.  Returns nil if NODE is nil,
empty/whitespace-only, or if the result exceeds 255 UTF-8 bytes.
The original NODE can be recovered using `skroad--file-path-to-node-title'."
  (when node
    (let ((node-nowhite (skroad--clean-whitespace node)))
      (when (not (string-empty-p node-nowhite))
        (let* ((encoded
                (replace-regexp-in-string
                 (rx (| (any "\x00-\x1f\x7f" ?/ ?\\ ?: ?* ?? ?\" ?< ?> ?| ?%)
                        (seq bos (| (+ ".") "~"))
                        (seq (+ ".") eos)))
                 #'(lambda (m)
                     (mapconcat (lambda (ch) (format "%%%02X" ch)) m ""))
                 node-nowhite t t))
               (filename (skroad--append-extension encoded)))
          (when (<= (length (encode-coding-string filename 'utf-8 t)) 255)
            filename))))))

(defun skroad--file-path-to-node-title (file)
  "Get the base name and parse escapes to decode FILE name to a node title."
  (replace-regexp-in-string
   "%[0-9A-F][0-9A-F]"
   (lambda (match)
     (char-to-string (string-to-number (substring match 1) 16)))
   (file-name-base file) t t))

(defun skroad--file-path-in-data-directory (file)
  "Generate the path where FILE would reside in the data directory."
  (when (null file)
    (error "Filename '%s' is invalid!" file))
  (expand-file-name (file-name-concat skroad--data-directory file)))

(defun skroad--storage-list-files ()
  "Return a list of all node files currently stored in the data directory."
  (skroad--storage-ensure)
  (file-expand-wildcards
   (skroad--file-path-in-data-directory
    (skroad--append-extension "*"))))

(defun skroad--node-path (node)
  "Generate the canonical file path where NODE would be found if it exists."
  (skroad--file-path-in-data-directory (skroad--node-title-to-filename node)))

(defun skroad--validate-title (title)
  "Return t when TITLE represents a valid node title."
  (when (skroad--link-valid-p title)
    (let ((encoded-title (skroad--node-title-to-filename title)))
      (and encoded-title
           (equal (skroad--file-path-to-node-title encoded-title) title)))))

;; TODO: alarm unreachables to log
;; TODO: unreachables should not open in skroad mode
(defun skroad--storage-list-nodes ()
  "Return a list of all nodes currently stored on disk.  Verify reachability."
  (skroad--storage-ensure)
  (seq-keep
   #'(lambda (file)
       (let ((title (skroad--file-path-to-node-title file)))
         (cond ((string-equal (skroad--node-path title) file) title)
               (t (message "Node %s is not reachable!"
                           (browse-url-file-url file))
                  nil))))
   (skroad--storage-list-files)))

;; Keymap utils. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--get-keymap-docs (keymap)
  "Return an alist of (KEY . DOCSTRING) for documented bindings in KEYMAP."
  (let (out remaps seen)
    (map-keymap
     (lambda (ev bd)
       (if (eq ev 'remap)
           (map-keymap (lambda (c n) (push (cons c n) remaps)) bd)
         (unless (memq ev seen)
           (push ev seen)
           (and (or (integerp ev)
                    (and (symbolp ev)
                         (not (memq ev '(menu-bar tab-bar tool-bar
                                                  header-line mode-line)))
                         (not (string-match-p
                               "mouse\\|drag\\|click\\|wheel"
                               (symbol-name ev)))))
                (not (eq bd 'ignore))
                (let* ((fn (or (cdr (assq bd remaps)) bd))
                       (doc (and (functionp fn) (documentation fn))))
                  (when doc
                    (push (cons (key-description (vector ev)) doc) out)))))))
     keymap)
    (nreverse out)))

(defun skroad--make-keymap-help (keymap) ;; TODO: wrap?
  "Generate a keymap help string from the given KEYMAP."
  (when (keymapp keymap)
    (mapconcat
     #'(lambda (entry) (format "%s:%s" (car entry) (cdr entry)))
     (skroad--get-keymap-docs keymap) "|")))

;; Skroad text type mechanism and basic types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro skroad--deftype (name &rest properties)
  "Define a new skroad text type NAME with given PROPERTIES."
  (declare (indent defun))
  `(skroad--deftype-impl ',name ,@properties))

(defun skroad--deftype-impl (name &rest properties)
  "Define a new skroad text type NAME with given PROPERTIES."
  (if (plist-get properties :mixin) ;; Save a mixin's properties verbatim.
      (setf (symbol-plist name) properties) ;; ...will be eaten when :use'd.
    (letrec ;; Otherwise, walk all properties, including from any :use'd types:
        ((env nil) ;; Lexical environment for eval to use
         (save-pv #'(lambda (prop val) ;; when we get final value of a prop,
                      (put name prop val) ;; record as a symbol-prop of name,
                      (push (cons prop val) env))) ;; and to where eval sees it
         (require-prop #'(lambda (prop) ;; Verify that prop was defined already
                           (when (null (alist-get prop env))
                             (error "%s must be defined for %s!" prop name))))
         (eat-props ;; Walk a plist, find and save effective value of each prop
          #'(lambda (plist)
              (skroad--do-plist prop val plist
                (cond
                 ((memq prop '(:doc :mixin)) nil) ;; Specials
                 ((eq prop :register) (add-to-list val name)) ;; Add to list
                 ((eq prop :use) ;; Val is another type; eat all of its props
                  (funcall eat-props (symbol-plist val))) ;; recursively
                 ((eq prop :defaults) ;; Default values for props when needed
                  (dolist (kv val)
                    (let ((def-prop (car kv)) (def-val (cadr kv)))
                      (when (null (alist-get def-prop env))
                        (funcall save-pv def-prop def-val)))))
                 ((eq prop :require) ;; val is mandatory prop (or list of them)
                  (if (listp val)
                      (dolist (p val) (funcall require-prop p))
                    (funcall require-prop val)))
                 ((keywordp prop) ;; Explicitly-defined property (:prop value)
                  (funcall
                   save-pv (skroad--keyword-to-symbol prop) ;; lose the `:`
                   (cond
                    ((eq prop :keymap) ;; If provided a keymap:
                     (let ((parent-keymap (alist-get 'keymap env)))
                       (when parent-keymap ;; stack on any existing keymap
                         (set-keymap-parent val parent-keymap))
                       (copy-keymap val)))
                    ((symbolp val) ;; symbols
                     (if (or (boundp val) (fboundp val)
                             (facep val) (symbol-plist val))
                         val (eval val env))) ;; if globally-bound, self-eval
                    (t (byte-compile (eval val env)))))) ;; compile form
                 ;; Property from :use, so don't re-eval it, just store it:
                 (t (unless (eq prop 'type-name) ;; Don't save :use'd type names
                      (funcall save-pv prop val))))))))
      (funcall save-pv 'type-name name) ;; Save the type name to symbol and env
      ;; Add properties, starting with the default ones:
      (funcall eat-props
               (append '(:use skroad--text-mixin-default-type) properties))))
  name)

(skroad--deftype skroad--text-mixin-default-type
  :doc "Default text type from which all other types (except mixins) inherit."
  :mixin t
  :order 100 ;; lower number will get rendered first
  :face 'skroad--text-face
  :mouse-face nil
  :face-function t
  :display-function t
  :rear-nonsticky t
  :finder-filter t
  )

(skroad--deftype skroad--text-mixin-findable
  :doc "Mixin for all findable text types. (Internal use only.)"
  :mixin t
  :require '(regex-any finder-filter)
  :defaults '((visible-match-number nil))
  :find
  '(lambda (method regex &optional lim)
     (skroad--re-search method regex lim finder-filter))
  :find-any-forward
  '(lambda (&optional lim)
     (funcall find #'re-search-forward regex-any lim))
  :find-any-backward
  '(lambda (&optional lim)
     (funcall find #'re-search-backward regex-any lim))
  :for-all-in-region-forward
  '(lambda (start end fn)
     (skroad--re-foreach regex-any fn finder-filter start end))
  :get-match
  '(lambda () (match-string-no-properties match-number))
  :get-payload
  '(lambda () (skroad--clean-whitespace (funcall get-match)))
  :swap
  '(lambda (payload &optional only-payload)
     (replace-match payload t t nil (and only-payload match-number)))
  )

(defvar skroad--text-types-delimited nil "Text types having delimiters.")

(skroad--deftype skroad--text-mixin-delimited
  :doc "Base mixin for delimited text types. Define delimiters before using."
  :mixin t
  :require '(begins ends payload-regex finder-filter)
  :defaults '((match-number 1))
  :generate '(lambda (payload) (concat begins payload ends))
  :make-regex
  '(lambda (payload)
     (rx (seq (literal begins) (group (regexp payload)) (literal ends))))
  :regex-any '(funcall make-regex payload-regex)
  :use 'skroad--text-mixin-findable
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :regex-validator '(rx (seq bos (regexp payload-regex) eos))
  :validate
  '(lambda (string) (string-match-p regex-validator string))
  :search
  '(lambda (payload &optional lim)
     (funcall find #'re-search-forward (funcall make-regex payload) lim))
  :walk
  '(lambda (payload fn &optional start end)
     (skroad--re-foreach
      (funcall make-regex payload) fn finder-filter start end))
  :zap
  '(lambda (payload &optional start end)
     (funcall walk payload #'skroad--zap-match start end))
  :regen
  '(lambda (payload &optional new-type new-payload start end)
     (let ((new-text (funcall (if new-type (get new-type 'generate) generate)
                              (or new-payload payload))))
       (funcall walk payload #'(lambda () (funcall swap new-text)) start end)))
  :register 'skroad--text-types-delimited
  )

(defun skroad--type-action (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name (let ((action (get text-type action-name)))
                      (when action (apply action args)))))

;; Font lock rendered text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--text-types-rendered nil "Text types for use with font-lock.")

(skroad--deftype skroad--text-mixin-regexp-rendered
  :doc "Mixin for regexp text types rendered by font-lock."
  :mixin t
  :require '(find-any-forward render)
  :render-next
  '(lambda (limit)
     (when (funcall find-any-forward limit)
       (with-silent-modifications (funcall render) t))))

(skroad--deftype skroad--text-mixin-rendered
  :doc "Finalization mixin for all text types rendered by font-lock."
  :mixin t
  :require 'render-next
  :font-lock-rule '(lambda () (list render-next '(0 nil append)))
  :register 'skroad--text-types-rendered)

(defconst skroad--font-lock-properties
  '(category face mouse-face zone data display line-prefix wrap-prefix invisible)
  "Let font lock know what props we use in renderers, so it will clean them.")

(defvar skroad--font-lock-keywords nil "Font lock keywords for skroad mode.")

(defun skroad--generate-font-lock-keywords ()
  "Generate font-lock keywords for skroad mode."
  (unless skroad--font-lock-keywords
    (setq skroad--font-lock-keywords
          (mapcar #'(lambda (type) (funcall (get type 'font-lock-rule)))
                  (sort skroad--text-types-rendered
                        #'(lambda (a b) (<= (get a 'order) (get b 'order))))))))

(defun skroad--init-font-lock ()
  "Initialize font lock fontification in a skroad buffer."
  (skroad--generate-font-lock-keywords)
  (setq-local font-lock-defaults '(skroad--font-lock-keywords t)
              font-lock-extra-managed-props skroad--font-lock-properties)
  (font-lock-refresh-defaults))

(defvar-local skroad--font-lock-unfontify-region nil)

(defun skroad--suspend-font-lock ()
  "Suspend font lock rendering in a skroad buffer, but don't depropertize text."
  (setq-local
   skroad--font-lock-unfontify-region font-lock-unfontify-region-function
   font-lock-unfontify-region-function #'(lambda (&rest _args) ())
   font-lock-defaults '(nil t))
  (font-lock-refresh-defaults))

(defun skroad--resume-font-lock ()
  "Resume font lock fontification in a skroad buffer."
  (setq-local
   font-lock-defaults '(skroad--font-lock-keywords t)
   font-lock-unfontify-region-function skroad--font-lock-unfontify-region)
  (font-lock-refresh-defaults))

(defun skroad--refontify-current-line ()
  "Refresh fontification of the current line in a skroad buffer."
  (when (skroad--mode-p)
    (save-mark-and-excursion
      (font-lock-ensure (line-beginning-position) (line-end-position)))))

(defun skroad--refontify-current-buffer ()
  "Refresh fontification in the visible portion of the current buffer."
  (font-lock-flush) ;; Flush all fontification, will get refontified on demand
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (when windows
      (let ((start (apply #'min (mapcar #'window-start windows)))
            (end (apply #'max
                        (mapcar #'(lambda (w) (window-end w t)) windows))))
        (font-lock-ensure start end)))))

(defun skroad--refontify-open-nodes ()
  "Refresh fontification in all currently-open nodes."
  (skroad--visit-open-nodes (skroad--refontify-current-buffer)))

;; Zoned text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: prevent overlap
(skroad--deftype skroad--text-mixin-rendered-zoned
  :doc "Mixin for zoned text types rendered by font-lock."
  :mixin t
  :require '(face face-function get-payload)
  :render
  '(lambda ()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (payload (funcall get-payload))
            (props
             (list 'category type-name
                   'zone (gensym)
                   'data payload))
            (add-face
             (if (functionp face-function)
                 (funcall face-function payload)
               face)))
       (if (facep mouse-face)
           (plist-put props 'mouse-face (list mouse-face)) props)
       (add-text-properties start end props)
       (add-face-text-property start end add-face)
       (when (numberp visible-match-number)
         (put-text-property
          start end 'name ;; For renamer
          (match-string-no-properties visible-match-number))
         (put-text-property start end 'invisible t)
         (remove-text-properties
          (match-beginning visible-match-number)
          (match-end visible-match-number)
          '(invisible)))))
  :use 'skroad--text-mixin-regexp-rendered
  :use 'skroad--text-mixin-rendered)

(defun skroad--zone-start (&optional pos)
  "Return the position where the zone at POS starts."
  (or (previous-single-property-change (1+ (or pos (point))) 'zone)
      (point-min)))

(defun skroad--zone-end (&optional pos)
  "Return the position where the zone at POS ends."
  (or (next-single-property-change (or pos (point)) 'zone) (point-max)))

(defmacro skroad--with-current-zone (&rest body)
  "Evaluate BODY with start and end bound to boundaries of zone at point."
  (declare (indent defun))
  `(let ((start (skroad--zone-start)) (end (skroad--zone-end)))
     ,@body))

;; Block highlighter text type. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--block-indent-columns 3
  "Padding width per nesting level in columns.")

(defvar skroad--block-level-cache (make-hash-table :test 'eql)
  "Cache of level -> (prefix . face).")

(defun skroad--block-invalidate-cache ()
  "Invalidate the block level cache.  Do it when changing colours or indent."
  (clrhash skroad--block-level-cache))

(defun skroad--block-bg-for-level (level)
  "Obtain the background colour used for blocks of LEVEL."
  (aref skroad--block-backgrounds
        (mod (1- level) (length skroad--block-backgrounds))))

(defun skroad--block-level-props (level)
  "Obtain the properties that must be set for a block of LEVEL."
  (or (gethash level skroad--block-level-cache)
      (let* ((prefix "")
             (i 1)
             (face nil))
        (while (<= i level)
          (setq face `(:background ,(skroad--block-bg-for-level i) :extend t))
          (setq prefix
                (concat prefix
                        (propertize
                         (make-string skroad--block-indent-columns ?\s)
                         'face face)))
          (setq i (1+ i)))
        (puthash level (list 'line-prefix prefix
                             'wrap-prefix prefix
                             'face face)
                 skroad--block-level-cache))))

(defvar-local skroad--buf-block-overlays nil
  "List of block overlays active in the current buffer.")

(defconst skroad--block-overlay-id 'skroad--block
  "Identifies a skroad text block overlay.")

(defun skroad--block-add-block (start end)
  "Add a block overlay spanning START and END in the current buffer."
  (let ((ov (make-overlay start end nil nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov skroad--block-overlay-id t)
    (push ov skroad--buf-block-overlays)
    ov))

(defun skroad--block-remove-block (ov)
  "Remove the block overlay OV from the current buffer."
  (setq skroad--buf-block-overlays (delq ov skroad--buf-block-overlays))
  (delete-overlay ov))

(defun skroad--block-depth-at (pos)
  "Return the block depth at POS in the current buffer."
  (let ((depth 0))
    (dolist (ov (overlays-at pos))
      (when (overlay-get ov skroad--block-overlay-id)
        (setq depth (1+ depth))))
    depth))

(skroad--deftype skroad--text-block
  :doc "Text type for block highlighter rendered by font lock."
  :order 1 ;; Must render before all others to avoid bg clobbering
  :render-next
  '(lambda (limit)
     (while (< (point) limit)
       (let ((depth (skroad--block-depth-at (point)))
             (next (min (next-overlay-change (point)) limit)))
         (when (> depth 0)
           (with-silent-modifications
             (skroad--with-whole-lines (point) next
               (set-text-properties start-expanded end-expanded
                                    (skroad--block-level-props depth)))))
         (goto-char next)))
     nil)
  :use 'skroad--text-mixin-rendered)

;; Decorative text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-mixin-render-delimited-decorative
  :doc "Mixin for decorative delimited text types rendered by font-lock."
  :mixin t
  :payload-regex (rx (seq (* blank) (+ print) (* blank)))
  :require 'face
  :render
  '(lambda () (add-face-text-property (match-beginning 0) (match-end 0) face))
  :order 1000 ;; Render these last, so they can amend all other rendered faces
  :use 'skroad--text-mixin-delimited
  :use 'skroad--text-mixin-regexp-rendered
  :use 'skroad--text-mixin-rendered)

(skroad--deftype skroad--text-decorative-heading
  :doc "Heading text."
  :begins "##" :ends "\n"
  :face 'skroad--heading-face
  :finder-filter '(lambda ()
                    (and (skroad--in-node-body-p)
                         (not (text-property-any ;; May not overlap an atomic
                               (match-beginning 0) (match-end 0) 'atomic t))))
  :use 'skroad--text-mixin-render-delimited-decorative
  :order 999)

(skroad--deftype skroad--text-decorative-italic
  :doc "Italicized text."
  :face 'italic
  :begins "__" :ends "__"
  :use 'skroad--text-mixin-render-delimited-decorative)

(skroad--deftype skroad--text-decorative-bold
  :doc "Bold text."
  :face 'bold
  :begins "**" :ends "**"
  :use 'skroad--text-mixin-render-delimited-decorative)

;; Node indices cache. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--cache-table nil
  "Skroad node indices cache.  (Do not access directly: call `skroad--cache'.)
Key is the node title.  Value is `index-me' (this node was not indexed yet);
or the node's indices, if it has been indexed; or `empty' (indices are null).")

(defun skroad--cache ()
  "Access the node indices cache.  Populate it from disk on first access."
  (unless skroad--cache-table
    (setq skroad--cache-table (make-hash-table :test 'equal))
    (mapc #'skroad--cache-intern-unindexed (skroad--storage-list-nodes)))
  skroad--cache-table)

(defun skroad--cache-intern-unindexed (node)
  "Intern NODE in the cache with a marker indicating that it needs indexing."
  (skroad--cache-write node 'index-me))

(defun skroad--cache-write (node data)
  "Update NODE with DATA (using `empty' to represent nil) in the cache."
  (puthash node (or data 'empty) (skroad--cache)))

(defun skroad--cache-peek (node)
  "Return the raw value associated with NODE in the cache; nil if not interned."
  (gethash node (skroad--cache)))

(defun skroad--cache-fetch (node)
  "Return indices for NODE; or `index-me' if not indexed; or nil if empty."
  (let ((data (skroad--cache-peek node))) (when (not (eq data 'empty)) data)))

(defun skroad--cache-indexed-p (node)
  "Return t if NODE is interned in the cache and has been indexed."
  (let ((data (skroad--cache-peek node))) (and data (not (eq data 'index-me)))))

(defun skroad--cache-intern (node)
  "If NODE is already in the cache, do nothing.  Otherwise, intern it."
  (unless (skroad--cache-peek node) (skroad--cache-intern-unindexed node)))

(defun skroad--cache-invalidate (node)
  "If NODE is in the cache, mark it as invalid.  Otherwise, do nothing."
  (when (skroad--cache-peek node) (skroad--cache-intern-unindexed node)))

(defun skroad--cache-evict (node)
  "Evict NODE from the cache."
  (remhash node (skroad--cache)))

(defun skroad--cache-rename (node new-node)
  "Reintern NODE as NEW-NODE in the cache, preserving data.  Return success."
  (let ((data (skroad--cache-peek node)))
    (when (and data (null (skroad--cache-peek new-node)))
      (skroad--cache-write new-node data)
      (skroad--cache-evict node)
      t)))

(defun skroad--cache-foreach (fn)
  "Evaluate (for side effects) FN applied to each node currently in the cache."
  (maphash #'(lambda (key _val) (funcall fn key)) (skroad--cache)))

(defun skroad--node-must-exist (node)
  "Signal an error if NODE does not exist."
  (unless (skroad--cache-peek node)
    (error "Node '%s' does not exist!" node)))

;; Indexed text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--index-delta (index payload delta &optional final create destroy)
  "Update the count of PAYLOAD in INDEX by DELTA.
Return `create' if introduced PAYLOAD; `destroy' if removed last copy; else nil.
If FINAL is t, the count sum going negative will signal an error."
  (let* ((had-prev (gethash payload index 0))
         (had-none (zerop had-prev))
         (sum (+ delta had-prev)))
    (if (zerop sum)
        (unless had-none (remhash payload index) destroy)
      (puthash payload sum index)
      (if (> sum 0)
          (when had-none create)
        (when final (error "Index underflow!"))))))

(defmacro skroad--ensure-index (indices text-type)
  "Retrieve or create the index for TEXT-TYPE in INDICES."
  `(or (alist-get ,text-type ,indices)
       (setf (alist-get ,text-type ,indices) (make-hash-table :test 'equal))))

;; TODO: `on-dupe` action?
(defun skroad--indices-update (indices changes &optional no-actions init-scan)
  "Apply a set of pending CHANGES to INDICES.  Return the updated INDICES.
The tables in CHANGES are emptied out after being applied to the INDICES.
Also execute the following text type actions (unless NO-ACTIONS) :
`on-create': a particular payload of this type first appeared in the buffer.
`on-init': same as above, but during initial scan (INIT-SCAN is t).
`on-destroy': a particular payload of this type no longer appears in the buffer.
Secondary type actions (always run, except for special nodes) :
`on-create-first': the first payload of this type has appeared in the buffer.
`on-init-first': same as above, but during initial scan (INIT-SCAN is t).
`on-destroy-last': the last payload of this type was removed from the buffer.
`on-init-none': during initial scan, no payloads of this type were found."
  (let ((origin (skroad--current-node)))
    (dolist (change changes)
      (let ((text-type (car change)) (type-changes (cdr change)))
        (when (or init-scan (not (skroad--hash-empty-p type-changes)))
          (let* ((type-index (skroad--ensure-index indices text-type))
                 (none-before (skroad--hash-empty-p type-index))
                 (create-action (if init-scan 'on-init 'on-create))
                 (type-create-action
                  (if init-scan 'on-init-first 'on-create-first)))
            (maphash
             #'(lambda (payload count)
                 (let ((action
                        (skroad--index-delta type-index payload count
                                             t create-action 'on-destroy)))
                   (unless (or (null action) no-actions)
                     (skroad--defer
                      (skroad--type-action text-type action origin payload)))))
             type-changes)
            (clrhash type-changes) ;; Empty the type's pending change index
            ;; Created the first or destroyed the last item of this type?
            (let*
                ((none-after (skroad--hash-empty-p type-index))
                 (action
                  (cond ((and init-scan none-after) 'on-init-none)
                        ((and none-before (not none-after)) type-create-action)
                        ((and (not none-before) none-after) 'on-destroy-last))))
              (unless (or (null action) (skroad--node-special-p))
                (skroad--defer
                 (skroad--type-action text-type action origin)))
              (when none-after ;; Don't waste cache space on empty indices
                (setq indices (assq-delete-all text-type indices)))))))))
  indices)

(defvar skroad--lint-in-progress nil
  "Indicates that lint is currently in progress.")

(defvar-local skroad--buf-indices-pending nil
  "Pending changes to the text type indices for the current node.")

(defun skroad--buf-indices-have-pending-p ()
  "Return t if the current node's text type indices have pending changes."
  (not (or (null skroad--buf-indices-pending)
           (seq-every-p
            #'(lambda (pending) (skroad--hash-empty-p (cdr pending)))
            skroad--buf-indices-pending))))

(defvar-local skroad--buf-indices-table 'fetch-me
  "Cached text type indices for the current node.  Do not access directly.")

(defun skroad--buf-indices ()
  "Obtain the current node's text type indices."
  (when (or skroad--lint-in-progress
            (eq skroad--buf-indices-table 'fetch-me)) ;; Fetch from cache?
    (setq-local skroad--buf-indices-table
                (skroad--cache-fetch (skroad--current-node))))
  skroad--buf-indices-table)

(defun skroad--buf-indices-sync (&optional no-actions)
  "If the current node has not been indexed yet, create its text type indices.
Otherwise, apply any pending changes.  Then write the indices back to the cache.
Runs text type actions, unless NO-ACTIONS is t or the current node is special."
  (let* ((indices (skroad--buf-indices))
         (init (eq indices 'index-me))
         (have-changes (skroad--buf-indices-have-pending-p)))
    (when init
      (when have-changes
        (error "Tried to apply changes to unindexed node: '%s'"
               (skroad--current-node)))
      (skroad--index-scan-region (point-min) (point-max) 1)
      (setq have-changes t)
      (setq indices nil)
      (skroad--save-current-node)) ;; We may have rectified links, so save it
    (when have-changes
      (setq indices (skroad--indices-update
                     indices skroad--buf-indices-pending
                     (or no-actions (skroad--node-special-p)) init)) ;; TODO?
      (setq-local skroad--buf-indices-table indices)
      (skroad--cache-write (skroad--current-node) indices))))

(defvar skroad--text-types-indexed nil "Text types that are indexed.")

(defvar-local skroad--buf-indices-scan-enable t "Toggle index scanning.")

(skroad--deftype skroad--text-mixin-indexed
  :doc "Mixin for indexed text types."
  :mixin t
  :require '(for-all-in-region-forward get-match swap)
  :defaults '((index-filter nil))
  :scan-region
  '(lambda (start end delta)
     (let ((pending-index
            (skroad--ensure-index skroad--buf-indices-pending type-name)))
       (funcall
        for-all-in-region-forward start end
        #'(lambda ()
            (let* ((raw-match (funcall get-match))
                   (payload (skroad--clean-whitespace raw-match)))
              ;; If there's an index filter, use it:
              (when (or (null index-filter) (funcall index-filter payload))
                (skroad--index-delta pending-index payload delta))
              ;; Always rectify the payload, if not already canonical:
              (when (and (= delta 1) (not undo-in-progress)
                         (not (string-equal raw-match payload)))
                (let ((skroad--buf-indices-scan-enable nil) ;; Don't recurse
                      (inhibit-read-only t)) ;; Force writability
                  (funcall swap payload t))))))))
  :register 'skroad--text-types-indexed)

(defun skroad--index-scan-region (start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START ... END
to the pending changes in the buffer;  `skroad--buf-indices-sync' must be
called to finalize all pending changes when no further ones are expected.
If `skroad--buf-indices-scan-enable' is nil, index scanning is disabled."
  (when skroad--buf-indices-scan-enable
    (skroad--with-whole-lines start end
      (dolist (text-type skroad--text-types-indexed)
        (funcall (get text-type 'scan-region)
                 start-expanded end-expanded delta)))))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in the buffer in region START...END."
  (skroad--index-scan-region start end -1))

(defun skroad--after-change-function (start end _length)
  "Triggers following a change in the buffer in region START...END."
  (skroad--index-scan-region start end 1))

(defun skroad--buf-indices-install-tracker ()
  "Install the skroad change hooks in the current buffer."
  (add-hook 'before-change-functions 'skroad--before-change-function nil t)
  (add-hook 'after-change-functions 'skroad--after-change-function nil t))

(defun skroad--indices-has-p (text-type payload indices)
  "Test whether a PAYLOAD of TEXT-TYPE exists in INDICES."
  (let* ((index (alist-get text-type indices))
         (count (or (and index (gethash payload index)) 0)))
    (> count 0)))

(defun skroad--current-indices-have-p (text-type payload)
  "Test whether a PAYLOAD of TEXT-TYPE exists in the current node's indices."
  (skroad--indices-has-p text-type payload (skroad--buf-indices)))

(defun skroad--current-indices-foreach (text-type fn &rest other-args)
  "Apply FN to all payloads of TEXT-TYPE in the current node's indices."
  (let ((index (alist-get text-type (skroad--buf-indices))))
    (when index
      (maphash #'(lambda (key _val) (apply fn (cons key other-args))) index))))

;; Top-level keymap for the major mode. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-top-backspace ()
  "If prev point is in an atomic, delete it; otherwise, normal backspace."
  (interactive)
  (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
        ((bobp) nil)
        (t (let* ((p (point)) (left (1- p)))
             (unless (skroad--in-node-title-p left) ;; Never bite into the title
               (if (skroad--prop-at 'data left) ;; If in front of an atomic:
                   (delete-region (skroad--zone-start left) p) ;; ... delete it;
                 (delete-backward-char 1))))))) ;; ... if not: normal backspace.

(defun skroad--match-any-from (type method pos)
  "Search for TYPE from POS via METHOD; return match data (nil if not found)."
  (save-mark-and-excursion
    (goto-char pos)
    (save-match-data (when (funcall (get type method)) (match-data t)))))

(defun skroad--link-find-from (pos &optional backwards)
  "Try to find the next (or previous, if BACKWARDS) live or dead link from POS."
  (let* ((method (if backwards 'find-any-backward 'find-any-forward))
         (live (skroad--match-any-from 'skroad--text-link-node-live method pos))
         (dead (skroad--match-any-from 'skroad--text-link-node-dead method pos))
         (match
          (or
           (and live dead ;; If both, get the nearest one in the given direction
                (or (and (xor backwards (<= (car live) (car dead))) live) dead))
           live dead))) ;; If only one, take that one; or nil if neither
    (when match
      (set-match-data match)
      (match-beginning 0))))

(defun skroad--link-jump-from (pos &optional backwards wrap)
  "Jump to the next (or previous, if BACKWARDS) live or dead link from POS.
If WRAP is t, wrap if there are no further links in the current direction.
Return the new position if the jump actually happened; otherwise nil."
  (let ((found
         (or (skroad--link-find-from pos backwards)
             (and wrap (skroad--link-find-from
                        (if backwards (point-max) (point-min)) backwards)))))
    (when found (goto-char found))))

(defun skroad--cmd-top-jump-to-next-link ()
  "Jump to the next link after the point; try to cycle to first if none."
  (interactive)
  (skroad--link-jump-from (point) nil t))

(defun skroad--cmd-top-jump-to-prev-link ()
  "Jump to the previous link before the point; try to cycle to last if none."
  (interactive)
  (skroad--link-jump-from (point) t t))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--cmd-top-backspace
    "<tab>" #'skroad--cmd-top-jump-to-next-link
    "C-<tab>" #'skroad--cmd-top-jump-to-prev-link
    )
  "Top-level keymap for the skroad major mode.")

;;; Atomic Text Type. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert a space immediately behind the atomic currently under the point.
(defun skroad--cmd-atomic-prepend-space ()
  "Ins.Space"
  (interactive)
  (save-mark-and-excursion (goto-char (skroad--zone-start)) (insert " ")))

(defvar-local skroad--buf-alt-mark nil
  "The opposite end of an atomic zone in which the regular mark had been set.")

(defun skroad--deactivate-mark ()
  "Deactivate the mark and clear the alt-mark."
  (deactivate-mark) (setq-local skroad--buf-alt-mark nil))

(defmacro skroad--define-atomics-region-cmd (wrap-command)
  "Wrap COMMAND to use region if exists, or use the atomic at point as region."
  `(defun ,(read (concat "skroad--cmd-atomic-"
                         (symbol-name wrap-command))) ()
     (interactive)
     (if (use-region-p)
         (call-interactively ',wrap-command)
       (skroad--with-current-zone
         (funcall #',wrap-command start end)))
     (skroad--deactivate-mark)))

(skroad--define-atomics-region-cmd delete-region)
(skroad--define-atomics-region-cmd kill-region)
(skroad--define-atomics-region-cmd kill-ring-save)

;; TODO: make sure selector is NEVER visible if point is not in this buffer!

(defvar-local skroad--buf-selector nil
  "Selector overlay active when an atomic is under the point.")

(defconst skroad--selector-properties
  `((face skroad--selector-face) (evaporate t))
  "Text properties of the selector.")

(defun skroad--selector-unhide ()
  "Reveal the selector overlay when it may have been hidden."
  (when (skroad--overlay-active-p skroad--buf-selector)
    (overlay-put skroad--buf-selector 'face 'skroad--selector-face)))

(defun skroad--selector-hide ()
  "Hide (but not destroy) the selector overlay."
  (when (skroad--overlay-active-p skroad--buf-selector)
    (overlay-put skroad--buf-selector 'face nil)))

(defun skroad--selector-activate-here ()
  "Activate (if inactive) or move the selector to the current zone."
  (skroad--with-current-zone
    (move-overlay skroad--buf-selector start end (current-buffer)))
  (setq-local cursor-type nil show-paren-mode nil))

(defun skroad--selector-deactivate ()
  "Deactivate the selector; it can be reactivated again."
  (when (skroad--overlay-active-p skroad--buf-selector)
    (delete-overlay skroad--buf-selector))
  (setq-local cursor-type t show-paren-mode t))

(defun skroad--cmd-atomic-set-mark ()
  "Set the mark inside an atomic."
  (interactive)
  (save-excursion
    (skroad--with-current-zone
      (setq-local skroad--buf-alt-mark start)
      (goto-char end)
      (call-interactively 'set-mark-command))))

(defun skroad--cmd-atomic-jump-to-next-link ()
  "Jump to the next link following this atomic; cycle to first after the last."
  (interactive)
  (skroad--link-jump-from (skroad--zone-end) nil t))

(defun skroad--show-key-help ()
  "Display the keymap help of the current point."
  (unless (listp last-input-event) ;; Don't trigger during mouse scrolling
    (let ((km (skroad--prop-at 'keymap))) ;; Display keymap help
      (skroad--info (skroad--make-keymap-help km)))))

;; TODO: require zone?
;; TODO: allow mouse click point motion by default
;; TODO: process enter/leave between buffers?
(skroad--deftype skroad--text-atomic
  :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
  :atomic t ;; TODO: check
  :on-enter '(lambda (pos-from auto)
               (skroad--selector-activate-here)
               (goto-char (skroad--zone-start)) ;; point can only sit on start
               (skroad--show-key-help)) ;; Show keymap help
  :on-leave '(lambda (pos-from auto)
               (skroad--selector-deactivate)
               (skroad--info)) ;; Clear the echo bar
  :on-move '(lambda (pos-from auto)
              (goto-char ;; if went forward, jump to the end; else, to the start.
               (if (> (point) pos-from)
                   (skroad--zone-end) (skroad--zone-start))))
  :keymap
  (define-keymap
    "<remap> <skroad--cmd-top-jump-to-next-link>"
    #'skroad--cmd-atomic-jump-to-next-link
    "SPC" #'skroad--cmd-atomic-prepend-space
    "<remap> <set-mark-command>" #'skroad--cmd-atomic-set-mark
    "<remap> <self-insert-command>" #'ignore
    "<remap> <quoted-insert>" #'ignore
    "<delete>" #'skroad--cmd-atomic-delete-region
    "<deletechar>" #'skroad--cmd-atomic-delete-region
    "<backspace>" #'skroad--cmd-atomic-delete-region
    "<drag-mouse-1>" #'ignore "<drag-mouse-2>" #'ignore "<drag-mouse-3>" #'ignore
    "<down-mouse-1>" #'ignore "<down-mouse-2>" #'ignore "<down-mouse-3>" #'ignore
    "<mouse-1>" #'ignore "<mouse-2>" #'ignore "<mouse-3>" #'ignore
    "<remap> <kill-region>" #'skroad--cmd-atomic-kill-region
    "<remap> <kill-ring-save>" #'skroad--cmd-atomic-kill-ring-save
    )
  )

;; Text hider. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-hider nil "Text hider overlay.")

(defun skroad--hide-text (start end)
  "Temporarily hide text in current buffer from START to END positions."
  (setq-local skroad--buf-hider (make-overlay start end (current-buffer)))
  (overlay-put skroad--buf-hider 'invisible t))

(defun skroad--unhide-text ()
  "Unhide all text that has been hidden with `skroad--hide-text'."
  (delete-overlay skroad--buf-hider))

;; Temporary change mechanism. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-unrollable-changes nil "Temporary change group.")

(defun skroad--snapshot-prepare ()
  "Start a temporary change set."
  ;; (assert (null skroad--buf-unrollable-changes))
  (setq-local skroad--buf-unrollable-changes (prepare-change-group))
  (activate-change-group skroad--buf-unrollable-changes))

(defun skroad--snapshot-rollback ()
  "End a temporary change set."
  ;; (assert skroad--buf-unrollable-changes)
  (undo-amalgamate-change-group skroad--buf-unrollable-changes)
  (cancel-change-group skroad--buf-unrollable-changes)
  (setq-local skroad--buf-unrollable-changes nil))

;; Interactive node renamer. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: should be global, rather than per-buffer, and go away when we leave it
(defvar-local skroad--buf-renamer nil "Node renamer overlay.")
(defvar-local skroad--buf-renamer-original nil "Original name.")

(defun skroad--renamer-active-p ()
  "Return t when the renamer is active."
  (skroad--overlay-active-p skroad--buf-renamer))

(defun skroad--fn-or-t (fn &rest args)
  "Call FN on ARGS when it is a function; if it isn't, return t."
  (if (functionp fn) (apply fn args) t))

;; Try to activate the renamer in the current zone.
(defun skroad--cmd-renamer-activate-here ()
  "Rename"
  (interactive)
  (unless (skroad--renamer-active-p)
    (let ((renamer-type (skroad--prop-at 'renamer-overlay-type)))
      (when renamer-type
        (skroad--with-current-zone
          (let ((current-name
                 (funcall (get renamer-type 'name-rename) start)))
            (when (skroad--fn-or-t
                   (get renamer-type 'permit-rename) current-name)
              (setq buffer-read-only nil)
              (skroad--suspend-font-lock)
              (skroad--deactivate-mark)
              (skroad--snapshot-prepare)
              (setq-local
               skroad--buf-indices-scan-enable nil
               cursor-type t
               skroad--buf-renamer-original current-name)
              (skroad--hide-text start end)
              (goto-char end)
              (insert (concat " " skroad--buf-renamer-original " "))
              (setq-local skroad--buf-renamer
                          (make-overlay end (point) (current-buffer)))
              (overlay-put skroad--buf-renamer 'category renamer-type)
              (set-buffer-modified-p nil)
              (goto-char end)
              (skroad--renamer-validate))))))))

(defun skroad--renamer-deactivate ()
  "Deactivate the renamer if it is currently active."
  (when (skroad--renamer-active-p)
    (delete-overlay skroad--buf-renamer)
    (skroad--deactivate-mark)
    (skroad--snapshot-rollback)
    (goto-char (overlay-start skroad--buf-hider))
    (skroad--unhide-text)
    (skroad--resume-font-lock)
    (skroad--refontify-current-line)
    (setq-local skroad--buf-indices-scan-enable t
                skroad--buf-renamer-original nil)
    (skroad--set-writability)))

(defun skroad--get-renamer-text ()
  "Get the proposed text in the current renamer."
  (skroad--clean-whitespace
   (field-string-no-properties (overlay-start skroad--buf-renamer))))

(defun skroad--renamer-get-default-face ()
  "Get the default face of the current renamer."
  (get (overlay-get skroad--buf-renamer 'category) 'face))

(defun skroad--renamer-validate ()
  "If a renamer is active, validate the proposed text.  Return t when valid."
  (when (skroad--renamer-active-p)
    (let ((valid
           (skroad--fn-or-t
            (overlay-get skroad--buf-renamer 'validate-rename)
            skroad--buf-renamer-original (skroad--get-renamer-text))))
      (overlay-put
       skroad--buf-renamer 'face
       (if valid
           (skroad--renamer-get-default-face)
         `(:inherit ,(skroad--renamer-get-default-face)
                    :background ,skroad--renamer-faces-invalid-background)))
      valid)))

(defun skroad--cmd-renamer-accept-changes ()
  "Accept the proposed renaming, if the renamer is currently active and valid."
  (interactive)
  (when (skroad--renamer-validate)
    (let ((old skroad--buf-renamer-original)
          (new (skroad--get-renamer-text))
          (do-rename (overlay-get skroad--buf-renamer 'do-rename)))
      (skroad--renamer-deactivate)
      (unless (string-equal old new)
        (funcall do-rename old new)))))

(skroad--deftype skroad--text-mixin-renamer-overlay
  :doc "Base mixin for renamer overlays."
  :mixin t
  :rear-advance t
  :zone 'type-name
  :field 'zone
  :keymap (define-keymap
            "<remap> <end-of-line>" ;; END jumps to the end of the renamer
            #'(lambda () (interactive) (goto-char (1- (field-end))))
            "<backspace>" ;; backspace must not change preceding text
            #'(lambda () (interactive)
                (cond ((use-region-p)
                       (delete-region (region-beginning) (region-end)))
                      ((> (point) (field-beginning))
                       (delete-char -1))))
            "<return>" #'skroad--cmd-renamer-accept-changes
            "<remap> <keyboard-quit>"
            #'(lambda () (interactive) (skroad--renamer-deactivate)))
  :on-leave '(lambda (pos-from auto) (skroad--renamer-deactivate)))

(skroad--deftype skroad--text-mixin-renameable
  :doc "Mixin for allowing the use of the rename command with an atomic type."
  :mixin t
  :require 'renamer-overlay-type
  :keymap (define-keymap "r" #'skroad--cmd-renamer-activate-here))

(defun skroad--node-renamer-permit (current)
  "Determine whether a node titled CURRENT is renameable."
  (cond ((skroad--node-special-p current)
         (skroad--info "Special nodes cannot be renamed!")
         nil)
        (t t)))

(defun skroad--node-renamer-name (pos)
  "Obtain the current node title at POS."
  (skroad--prop-at 'data pos))

(defun skroad--node-renamer-validate (current proposed)
  "Determine whether a node titled CURRENT may be renamed to PROPOSED."
  (cond ((not (skroad--validate-title proposed))
         (skroad--info "Proposed node name is invalid!")
         nil)
        ((string-equal proposed current)
         (skroad--info "No change proposed")
         t)
        ((skroad--cache-peek proposed)
         (skroad--info "A node named '%s' already exists!" proposed)
         nil)
        (t
         (skroad--info
          "Press <return> to rename, or leave field to cancel.")
         t)))

(defun skroad--node-renamer-do-rename (current proposed)
  "Rename the node titled CURRENT to PROPOSED."
  (skroad--rename-node current proposed))

(skroad--deftype skroad--text-mixin-node-renamer
  :doc "Mixin for the use of the rename command to rename nodes."
  :mixin t
  :permit-rename #'skroad--node-renamer-permit
  :name-rename #'skroad--node-renamer-name
  :validate-rename #'skroad--node-renamer-validate
  :do-rename #'skroad--node-renamer-do-rename)

(skroad--deftype skroad--text-node-renamer-direct
  :doc "Renamer for editing a node's title directly."
  :use 'skroad--text-mixin-renamer-overlay
  :use 'skroad--text-mixin-node-renamer
  :face 'skroad--direct-renamer-face
  :before-string "" :after-string " \n")

(skroad--deftype skroad--text-node-renamer-indirect
  :doc "Renamer for editing a node's title while standing on a link to the node."
  :use 'skroad--text-mixin-renamer-overlay
  :use 'skroad--text-mixin-node-renamer
  :face 'skroad--indirect-renamer-face
  :before-string " " :after-string " ")

(defun skroad--md-url-renamer-name (pos)
  "Obtain the current name of the renameable item at POS."
  (skroad--prop-at 'name pos))

;; TODO: escape [] in caption
(defun skroad--md-url-rename (_old new)
  "Recaption the Markdown-style URL at point from OLD to NEW."
  (skroad--with-current-zone
    (let ((url (skroad--prop-at 'data start)))
      (delete-region start end)
      (insert (skroad--make-md-url url new))
      (goto-char start))))

(skroad--deftype skroad--text-md-url-renamer
  :doc "Renamer for recaptioning a Markdown-style URL link."
  :use 'skroad--text-mixin-renamer-overlay
  :permit-rename '(lambda (current) t)
  :name-rename #'skroad--md-url-renamer-name
  :validate-rename '(lambda (old new) t)
  :do-rename #'skroad--md-url-rename
  :face 'skroad--indirect-renamer-face
  :before-string " " :after-string " ")

;; Link types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--disp-mode-force-this-window
  '((display-buffer-same-window))
  "Open new buffers in this window, even if visible in others.")

(defconst skroad--disp-mode-this-window-or-existing
  '((display-buffer-reuse-window display-buffer-same-window))
  "Open new buffers in this window, unless already visible in another.")

(defconst skroad--disp-mode-force-new-window
  '((display-buffer-pop-up-window))
  "Open new buffers in a new window.")

(defun skroad--do-link-action (pos &optional display-mode)
  "Run action of link at POS, if one was defined, and no region is active.
DISPLAY-MODE controls what happens when this results in opening a buffer."
  (unless (use-region-p)
    (let ((display-buffer-overriding-action
           (or display-mode skroad--disp-mode-this-window-or-existing)))
      (skroad--type-action
       (skroad--prop-at 'category pos)
       'on-activate (skroad--prop-at 'data pos)))))

(defun skroad--mouse-warp ()
  "Warp mouse to the middle of the current zone, if possible; else, to point."
  (skroad--refontify-current-line)
  (redisplay t)
  (let ((posn (or (and (skroad--mode-p) ;; Could be in a non-skroad buffer
                       (skroad--prop-at 'zone)
                       (posn-at-point
                        (+ (point)
                           (skroad--with-current-zone (/ (- end start) 2)))))
                  (posn-at-point))))
    (when posn
      (let* ((pos-xy (posn-x-y posn))
             (x (+ (window-pixel-left) (car pos-xy)))
             (y (+ (window-pixel-top) (window-header-line-height) (cdr pos-xy)
                   (/ (line-pixel-height) 2))))
        (set-mouse-pixel-position (selected-frame) x y)))))

(defun skroad--cmd-link-mouse-activate (click &optional display-mode)
  "Perform the action attribute of the link that got the CLICK.
DISPLAY-MODE is passed to `skroad--do-link-action'."
  (interactive "e")
  (let* ((posn (event-start click))
         (click-pos (posn-point posn))
         (window (posn-window posn))
         (frame (window-frame window)))
    (unless (eq frame (selected-frame))
      (select-frame-set-input-focus frame))
    (unless (eq window (selected-window))
      (select-window window))
    (goto-char (skroad--zone-start click-pos))
    (skroad--save-cache-point)
    (skroad--do-link-action click-pos display-mode)
    (skroad--mouse-warp)))

(defun skroad--cmd-link-mouse-activate-new-win (click)
  "Activate a link via the mouse, opening any buffers in a new window."
  (interactive "e")
  (skroad--cmd-link-mouse-activate click skroad--disp-mode-force-new-window))

;; Transform the item under the point to plain text by removing delimiters.
(defun skroad--cmd-atomic-delimited-textify ()
  "Textify"
  (interactive)
  (skroad--with-current-zone
    (let ((text (skroad--prop-at 'data)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

(skroad--deftype skroad--text-mixin-atomic-delimited
  :doc "Mixin denoting an atomic delimited text type."
  :mixin t
  :require 'atomic
  :keymap (define-keymap "t" #'skroad--cmd-atomic-delimited-textify)
  :use 'skroad--text-mixin-delimited
  :use 'skroad--text-mixin-rendered-zoned)

;; Perform the action attribute of the link at point.
(defun skroad--cmd-link-activate ()
  "Navigate"
  (interactive)
  (skroad--do-link-action (point)))

(defun skroad--cmd-link-activate-new-win ()
  "Nav.NewWin."
  (interactive)
  (skroad--do-link-action (point) skroad--disp-mode-force-new-window))

(skroad--deftype skroad--text-mixin-link-navigable
  :doc "Mixin denoting a navigable link."
  :mixin t
  :keymap (define-keymap
            "<mouse-1>" #'skroad--cmd-link-mouse-activate
            "<mouse-2>" #'skroad--cmd-link-mouse-activate-new-win
            "<return>" #'skroad--cmd-link-activate
            "M-<return>" #'skroad--cmd-link-activate-new-win))

;; Transform the link under the point to plain text by removing delimiters.
(defun skroad--cmd-link-comment ()
  "Textify"
  (interactive)
  (skroad--with-current-zone
    (let ((text (skroad--prop-at 'data)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

;; TODO: preview linked node, or "Stub node" if stub
(defun skroad--link-mouseover (_window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--prop-at 'data position)))

(skroad--deftype skroad--text-link-node
  :doc "Fundamental type for skroad node links (live or dead)."
  :use 'skroad--text-atomic
  :mouse-face 'skroad--highlight-link-face
  :payload-regex skroad--regexp-text-in-brackets
  :index-filter ;; Do not index self-links or links to special nodes
  '(lambda (node)
     (not (or (skroad--node-self-p node)
              (skroad--node-special-p node)))))

;; TODO: somehow generalize to file links, www (when emacs handles), help, etc.
(defun skroad--action-open-node (node)
  "Navigate to NODE.  If visible, go there; else open in the current window."
  (unless (skroad--cache-peek node) ;; Possibly node creation is still pending?
    (skroad--complete-all-deferred)) ;; ... if so, let all deferred work finish.
  (let* ((orig-node (skroad--current-node)) ;; Node we triggered the action in
         (orig-buf (current-buffer)) ;; Buffer we triggered the action in
         (node-path (skroad--node-path node)) ;; Target path
         (node-buf (find-buffer-visiting node-path))) ;; Target buf (maybe nil)
    (unless (skroad--cache-peek node) ;; Suppose the node still doesn't exist?
      (skroad--in-node node #'skroad--connect-to orig-node)) ;; ... create it.
    (if node-buf ;; If node is already open in a buffer, use that buffer:
        (let* ((node-win (get-buffer-window node-buf t))
               (node-frame (window-frame node-win)))
          (if node-win ;; If it already has a visible window, go there
              (progn
                (unless (eq (window-frame) node-frame) ;; If in different frame:
                  (select-frame-set-input-focus node-frame)) ;; ... focus it.
                (select-window node-win))
            (switch-to-buffer node-buf))) ;; ... else, unbury in current window
      ;; If node wasn't open, open it, burying the orig
      (pop-to-buffer (find-file-noselect node-path)))
    ;; TODO: this should be configurable
    (unless (or (skroad--maybe-restore-cached-point) ;; If no cached point...
                (skroad--node-special-p orig-node)) ;; ... and not from special
      (skroad--link-maybe-jump-to-live orig-node))
    (unless (get-buffer-window orig-buf t) ;; Kill orig if we had buried it
      (with-current-buffer orig-buf
        (skroad--buf-indices-sync)
        (skroad--save-current-node)
        (kill-buffer)))))

(defun skroad--action-connected-on-init (origin node)
  "A live link to NODE was found for the first time in ORIGIN during indexing."
  (unless (and (skroad--cache-peek node) ;; Node doesn't exist?
               (skroad--connected-p node origin)) ;; ... or it has no backlink?
    (skroad--in-node origin #'skroad--disconnect-from node)
    (message "Non-reciprocal link in '%s' to '%s' disabled." origin node)))

(defun skroad--action-connected (origin node)
  "The first instance of a live link to NODE was introduced in ORIGIN.
NODE will be created if it does not exist."
  (skroad--in-node node #'skroad--connect-to origin))

(defun skroad--action-disconnected (origin node)
  "The last instance of a live link to NODE was removed from ORIGIN.
If NODE does not exist, this is a no-op."
  (when (skroad--cache-peek node)
    (skroad--in-node node #'skroad--disconnect-from origin)))

(defun skroad--action-orphaned (origin)
  "ORIGIN is an orphan (i.e. it has NO live links)."
  (skroad--node-set-orphan origin t))

(defun skroad--action-unorphaned (origin)
  "ORIGIN is NOT an orphan (i.e. it has live links)."
  (skroad--node-set-orphan origin nil))

;; Yank (with optional ARGS) into a node when standing on a live link to it.
(defun skroad--cmd-teleyank-at (&rest args)
  "Teleyank"
  (interactive)
  (skroad--yank-into (skroad--prop-at 'data) args))

(defun skroad--cmd-unlink-at (&rest _args)
  "Unlink"
  (interactive)
  (skroad--link-unlink (skroad--prop-at 'data)))

(defun skroad--cmd-merge-at (&rest _args)
  "Merge"
  (interactive)
  (skroad--merge-node-into-current (skroad--prop-at 'data)))

(skroad--deftype skroad--text-link-node-live
  :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
  :use 'skroad--text-link-node
  :on-init-none #'skroad--action-orphaned
  :on-destroy-last #'skroad--action-orphaned
  :on-init-first #'skroad--action-unorphaned
  :on-create-first #'skroad--action-unorphaned
  :on-init #'skroad--action-connected-on-init
  :on-create #'skroad--action-connected
  :on-destroy #'skroad--action-disconnected
  :on-activate #'skroad--action-open-node
  :face-function
  '(lambda (payload)
     (cond ((skroad--node-self-p payload)
            'skroad--self-link-face)
           ((skroad--node-stub-p payload)
            'skroad--stub-link-face)
           (t 'skroad--live-link-face)))
  :help-echo 'skroad--link-mouseover
  :begins "[[" :ends "]]"
  :keymap (define-keymap
            "m" #'skroad--cmd-merge-at
            "l" #'skroad--cmd-unlink-at
            "y" #'skroad--cmd-teleyank-at ;; Official teleyank trigger
            "<remap> <yank>" #'skroad--cmd-teleyank-at ;; Regular yank also
            )
  :renamer-overlay-type 'skroad--text-node-renamer-indirect
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-atomic-delimited
  :use 'skroad--text-mixin-renameable
  :use 'skroad--text-mixin-indexed)

;; TODO: do this in title def?
(defun skroad--link-valid-p (string)
  "Determine whether STRING represents a valid link payload."
  (funcall (get 'skroad--text-link-node-live 'validate) string))

(defun skroad--link-maybe-jump-to-live (node)
  "Try to jump to the next live link to NODE after the point, if one exists."
  (when (funcall (get 'skroad--text-link-node-live 'search) node)
    (goto-char (match-beginning 0))))

(defun skroad--link-insert-live (node)
  "Insert a live link to NODE at the current point."
  (insert (funcall (get 'skroad--text-link-node-live 'generate) node)))

(defun skroad--link-has-live-p (node)
  "Determine whether the current node has at least one live link to NODE."
  (skroad--current-indices-have-p 'skroad--text-link-node-live node))

(defun skroad--link-get-all-live ()
  "Return all live links indexed in the current node."
  (let (live-links)
    (skroad--current-indices-foreach
     'skroad--text-link-node-live
     #'(lambda (l) (push l live-links)))
    live-links))

;;;;; TODO: when dead link clicked, simply move the point there
;; (defun skroad--cmd-dead-link-activate ()
;;   "Move the point to a dead link."
;;   (interactive)
;;   (goto-char (point)))

(defun skroad--cmd-liven-at (&rest _args)
  "Liven"
  (interactive)
  (skroad--link-revive (skroad--prop-at 'data)))

(skroad--deftype skroad--text-link-node-dead
  :doc "Dead (i.e. revivable placeholder) link to a skroad node."
  :use 'skroad--text-link-node
  :begins "[-[" :ends "]-]"
  :face 'skroad--dead-link-face
  :keymap (define-keymap "l" #'skroad--cmd-liven-at)
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-atomic-delimited
  :use 'skroad--text-mixin-indexed)

(defun skroad--link-has-dead-p (node)
  "Determine whether the current node has at least one dead link to NODE."
  (skroad--current-indices-have-p 'skroad--text-link-node-dead node))

(defun skroad--link-deaden (node &optional start end)
  "Deaden live links to NODE (optionally, in START...END) in the current node."
  (funcall (get 'skroad--text-link-node-live 'regen)
           node 'skroad--text-link-node-dead node start end))

(defun skroad--link-delete (node &optional start end)
  "Remove live links to NODE (optionally, in START...END) in the current node."
  (funcall (get 'skroad--text-link-node-live 'zap) node start end))

(defun skroad--link-unlink (node)
  "Transform all live links to NODE above the current node's tail to dead links;
and entirely remove all live links to NODE found below the current node's tail."
  (let ((tail (skroad--tail-pos)))
    (skroad--link-deaden node nil tail)
    (skroad--link-delete node tail)))

(defun skroad--link-revive (node)
  "Transform all dead links to NODE in the current node to live links."
  (funcall
   (get 'skroad--text-link-node-dead 'regen) node 'skroad--text-link-node-live))

(defun skroad--link-replace (node target &optional start end)
  "Replace live links to NODE in the current node with links to TARGET.
If START/END are given, constrain the replacement to that range."
  (funcall
   (get 'skroad--text-link-node-live 'regen)
   node 'skroad--text-link-node-live target start end))

(defun skroad--link-emplace (node)
  "Emplace a live link to NODE below the tail."
  (skroad--tail-jump-after)
  (newline)
  (skroad--link-insert-live node)
  (skroad--update-stub-status))

(defun skroad--link-merge (victim target)
  "Merge live links to VICTIM in the current node into links to TARGET."
  (let* ((tail (skroad--tail-pos)))
    (skroad--link-delete victim tail) ;; Always remove victim from tail.
    (if (skroad--link-replace victim target nil tail) ;; Replaced in body?
        (skroad--link-delete target tail) ;; ... then delete target in tail;
      (skroad--connect-to target)))) ;; ... if not, ensure a link to target.

(defun skroad--connect-to (node)
  "Ensure that the current node has at least one live link to NODE.
If it had dead links to NODE, liven them; if not, insert a link under the tail."
  (unless (skroad--node-self-p node) ;; May not connect to self
    (or (skroad--link-has-live-p node) ;; Already has a live link to node?
        (and (skroad--link-has-dead-p node) ;; If not, any dead links to it?
             (skroad--link-revive node)) ;; Liven the dead links.
        (skroad--link-emplace node)))) ;; ... Or create a link below the tail.

(defun skroad--disconnect-from (node &optional delete-all)
  "Ensure that the current node does NOT have any live links to NODE.
If DELETE-ALL is t, delete (rather than deaden) links found above the tail."
  (when (skroad--link-has-live-p node)
    (if delete-all
        (skroad--link-delete node)
      (skroad--link-unlink node))))

(defun skroad--yank-into (node &rest yank-args)
  "Ensure that NODE exists, and yank into it.  YANK-ARGS are passed to yank."
  (unless (skroad--node-special-p node) ;; Don't teleyank into special nodes
    (skroad--with-node node nil ;; Yank could contain links, so actions must run
      (skroad--tail-do-before
       (apply #'yank yank-args)))))

;; URLs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--regexp-url-common
  (rx (or (seq "http" (? "s")) "file" "ftp" "magnet" "help") "://")
  "Regexp for URL stem.")

(defconst skroad--regexp-url-any
  (rx (regex skroad--regexp-url-common)
      (+ (not (in " \t\n\r\"<>"))))
  "Regexp for raw URLs.")

(defconst skroad--unsafe-url-rx '("()" "[]" " " "\\" "`"))

(defconst skroad--regexp-url-encoded
  (rx-to-string
   `(: (regex ,skroad--regexp-url-common)
       (+ (or (: ?% (= 2 xdigit))
              (not (in " \t\n\r\"<>" ,@skroad--unsafe-url-rx))))))
  "Regexp for sanitized URLs.")

(defconst skroad--unsafe-url-chars
  (rx-to-string `(in ,@skroad--unsafe-url-rx)))

(defun skroad--sanitize-urls (text)
  "Percent-encode unsafe characters in URLs found in TEXT.
Already-encoded URLs are left untouched to avoid double-encoding."
  (replace-regexp-in-string
   skroad--regexp-url-any
   (lambda (url)
     (save-match-data
       (if (string-match-p (rx ?% (= 2 xdigit)) url)
           url
         (browse-url-url-encode-chars url skroad--unsafe-url-chars))))
   text nil t))

(defun skroad--browse-url (url)
  "Browse URL, respecting `display-buffer-overriding-action'."
  (let ((buf (save-window-excursion
               (browse-url url)
               (current-buffer))))
    (unless (eq buf (current-buffer))
      (pop-to-buffer buf))))

(defun skroad--zone-insert-space-after (string)
  "Insert a space after STRING in the current zone."
  (skroad--with-current-zone
    (save-mark-and-excursion
      (goto-char start)
      (search-forward string end)
      (insert " "))))

;; Turn the URL at point into plain text by placing a space after the prefix.
(defun skroad--cmd-bare-url-comment ()
  "Textify"
  (interactive)
  (skroad--zone-insert-space-after "//"))

(skroad--deftype skroad-text-bare-url-link
  :doc "Bare URL."
  :use 'skroad--text-atomic
  :help-echo 'skroad--link-mouseover
  :face 'skroad--url-link-face
  :mouse-face 'skroad--highlight-link-face
  :match-number 0
  :regex-any skroad--regexp-url-encoded
  :on-activate #'skroad--browse-url
  :keymap (define-keymap "t" #'skroad--cmd-bare-url-comment)
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  )

(defconst skroad--md-url-regexp
  (rx (seq ?\[
           (group (regexp skroad--regexp-text-in-brackets))
           ?\] ?\(
           (group (regexp skroad--regexp-url-encoded))
           ?\) ))
  "Regexp matching Markdown-style URLs.")

;; Turn the MD URL at point into plain text by placing a space after ']'.
(defun skroad--cmd-md-url-comment ()
  "Textify"
  (interactive)
  (skroad--zone-insert-space-after "]"))

(skroad--deftype skroad-text-md-url-link
  :doc "Markdown-style URL."
  :use 'skroad--text-atomic
  :help-echo 'skroad--link-mouseover
  :face 'skroad--url-link-face
  :mouse-face 'skroad--highlight-link-face
  :order 200 ;; Render after bare URLs
  :match-number 2
  :visible-match-number 1
  :regex-any skroad--md-url-regexp
  :on-activate #'skroad--browse-url
  :keymap (define-keymap "t" #'skroad--cmd-md-url-comment)
  :finder-filter #'skroad--in-node-body-p
  :renamer-overlay-type 'skroad--text-md-url-renamer
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-renameable
  :use 'skroad--text-mixin-rendered-zoned
  )

(defun skroad--make-md-url (url caption)
  "Return a Markdown-style URL with CAPTION."
  (format "[%s](%s)" ;; TODO: escape [] in caption?
          caption
          (browse-url-encode-url url)))

;; Atomic Comments. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-atomic-comment
  :doc "Atomic comment."
  :use 'skroad--text-atomic
  :face 'skroad--atomic-comment-face
  :match-number 1
  :payload-regex (rx (or (seq (+? (or (not ?*)
                                      (seq ?* (not ?$))
                                      (seq "*$" (not ?*))))
                              (?? ?*))
                         (+? ?*)))
  :begins "*$*" :ends "*$*"
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-atomic-delimited)

(defun skroad--atomic-comment-insert (text)
  "Insert TEXT as an atomic comment at the current point."
  (insert (funcall (get 'skroad--text-atomic-comment 'generate) text)))

;; Timestamps. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (skroad--deftype skroad--text-timestamp
;;   :doc "Timestamp."
;;   :use 'skroad--text-atomic
;;   :face 'skroad--timestamp-face
;;   :match-number 1
;;   :payload-regex (rx (+ digit))
;;   :begins "$%&_Time=" :ends "_&%$"
;;   :display-function
;;   '(lambda (payload)
;;      (format-time-string
;;       "%B %d, %Y"
;;       (seconds-to-time (string-to-number payload))))
;;   :finder-filter #'skroad--in-node-body-p
;;   :use 'skroad--text-mixin-atomic-delimited
;;   )

;; Node tail. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--node-tail "@@@" "Node tail indicator.")

;; TODO: readonly tail, bg colour, etc
(skroad--deftype skroad--text-node-tail
  :doc "Node tail."
  :use 'skroad--text-atomic
  :face 'skroad--node-tail-face
  :help-echo "Node tail."
  :match-number 0
  ;; :regex-any "^\\(@@@\\)$"
  :regex-any (rx (seq line-start (literal skroad--node-tail) line-end))
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  )

(defvar-local skroad--buf-tail-marker nil
  "Marker which points after the current node's tail, if known.")

;; TODO: make sure this runs during lint even if node has a tail
(defun skroad--jump-to-computed-tail ()
  "Determine where the tail ought to be per the tail heuristic, and go there.
Any dead links found below the computed tail are deleted."
  (goto-char (point-max))
  (let ((climb
         #'(lambda (type &optional delete)
             (let ((prev-pos (point)))
               (save-mark-and-excursion
                 (when (funcall (get type 'find-any-backward))
                   (goto-char (match-end 0))
                   (skip-syntax-forward " ")
                   (when (= prev-pos (point))
                     (let ((new-pos (match-beginning 0)))
                       (when delete (delete-region new-pos prev-pos))
                       new-pos))))))))
    (while (let ((prev (or (funcall climb 'skroad--text-link-node-live)
                           (funcall climb 'skroad--text-link-node-dead t))))
             (when prev (goto-char prev))))))

(defun skroad--tail-find-or-emplace ()
  "Find or emplace the tail in the current node, and store its location."
  (goto-char (point-min))
  (unless (funcall (get 'skroad--text-node-tail 'find-any-forward))
    (skroad--jump-to-computed-tail)
    (ensure-empty-lines 1)
    (save-mark-and-excursion
      (insert skroad--node-tail)
      (ensure-empty-lines 1))
    (goto-char (line-end-position)))
  (setq-local skroad--buf-tail-marker (copy-marker (point))))

;; TODO: update stub status every time we obtain the tail, when it is cheap
(defun skroad--tail-jump-after ()
  "Find or create the tail in the current node; set point after it."
  (or (and skroad--buf-tail-marker ;; Try using the tail marker, if we have it:
           (goto-char (marker-position skroad--buf-tail-marker))
           (string-equal ;; Verify that tail marker actually points to a tail
            (buffer-substring-no-properties (line-beginning-position) (point))
            skroad--node-tail))
      (skroad--tail-find-or-emplace)))

(defun skroad--tail-jump-before ()
  "Find or create the tail in the current node; set point before it."
  (skroad--tail-jump-after)
  (goto-char (line-beginning-position)))

(defun skroad--tail-pos ()
  "Return the position at the start of the tail."
  (save-mark-and-excursion (skroad--tail-jump-before) (point)))

(defmacro skroad--tail-do-before (&rest body)
  "Run BODY in a space created above the tail."
  `(save-mark-and-excursion
     (atomic-change-group
       (skroad--tail-jump-before)
       (ensure-empty-lines 1)
       ,@body
       (newline 2))
     (skroad--update-stub-status)))

(defun skroad--below-tail-p ()
  "Return t if the tail marker exists and the current point is below it."
  (and skroad--buf-tail-marker
       (> (point) skroad--buf-tail-marker)))

;; (defun skroad--pos-after-tail-p (pos)
;;   "Return t if POS is after the tail (created if it did not exist)."
;;   (>= pos (save-mark-and-excursion (skroad--tail-jump-after) (point))))

(defun skroad--update-stub-status ()
  "Determine whether the current node is a stub, and update Stubs if necessary.
A stub is a node where only whitespace is found between the title and the tail.
If the tail did not previously exist in the current node, it is emplaced."
  (unless (skroad--node-special-p)
    (skroad--node-set-stub (skroad--current-node)
                           (save-mark-and-excursion
                             (skroad--tail-jump-before)
                             (let ((before-tail (point)))
                               (skroad--goto-node-body-start)
                               (skip-syntax-forward " ")
                               (eq (point) before-tail))))))

;; Node title and body. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--current-node-title nil
  "Cached title of the node in the current buffer (Do not access directly).")

(defun skroad--current-node ()
  "Return the filename-derived title of the node in the current buffer."
  (or skroad--current-node-title
      (setq-local skroad--current-node-title
                  (skroad--file-path-to-node-title (buffer-file-name)))))

(defun skroad--node-self-p (node)
  "Return t if NODE is the current node."
  (string-equal node (skroad--current-node)))

(defun skroad--current-internal-title ()
  "Get the current node's title from the buffer."
  (buffer-substring-no-properties (point-min) (skroad--get-end-of-line 1)))

(defun skroad--change-internal-title (new-title)
  "Change the internal title of the current node to NEW-TITLE."
  (let ((inhibit-read-only t))
    (save-mark-and-excursion
      (goto-char (point-min))
      (insert new-title)
      (newline)
      (skroad--refontify-current-line)
      (delete-region (point) (progn (forward-line 1) (point))))))

;; TODO: write log entry if changing
(defun skroad--rectify-node-title ()
  "Ensure that the current node's internal and external titles match."
  (let ((external-title (skroad--current-node))
        (internal-title (skroad--current-internal-title)))
    (unless (string-equal internal-title external-title)
      (message
       "Node '%s' internal title '%s' does not match filename!"
       external-title internal-title)
      ;; temporary:
      (skroad--change-internal-title external-title)
      )))

(defun skroad--node-extract-body ()
  "Return the body of the current node."
  (skroad--clean-whitespace
   (save-mark-and-excursion
     (skroad--goto-node-body-start)
     (skip-syntax-forward " ")
     (buffer-substring-no-properties (point) (skroad--tail-pos)))))

(defun skroad--cmd-title-kill-ring-save ()
  "Save the current node's title, transformed to a live link, to the kill ring."
  (interactive)
  (let ((node (skroad--current-internal-title)))
    (with-temp-buffer
      (skroad--link-insert-live node)
      (copy-region-as-kill (point-min) (point-max)))))

(skroad--deftype skroad--text-node-title
  :doc "Node title."
  :use 'skroad--text-atomic
  :order 500
  :keymap
  (define-keymap
    "<return>" #'ignore "SPC" #'ignore
    "<deletechar>" #'ignore "<backspace>" #'ignore
    "<remap> <set-mark-command>" #'ignore
    "<remap> <yank>" #'ignore
    "<remap> <kill-region>" #'ignore
    "<remap> <kill-ring-save>" #'skroad--cmd-title-kill-ring-save
    )
  :face 'skroad--title-face
  :inhibit-isearch t ;; Don't interactive-search in the title
  :read-only "Title must be changed via rename command!"
  :renamer-overlay-type 'skroad--text-node-renamer-direct
  :use 'skroad--text-mixin-renameable
  :match-number 0
  :regex-any (rx (seq string-start (* not-newline) "\n"))
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  )

;; Cursor motion, mark, and floating title handling. ;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-pre-command-point-state (list (point-min) nil nil)
  "Point, zone at point, and type at point prior to a command.")

(defun skroad--get-point-state ()
  "Return a snapshot of the current point, zone, and type."
  (list (point) (skroad--prop-at 'zone) (skroad--prop-at 'category)))

(defun skroad--motion (prev &optional auto)
  "To be called whenever the zone under the point may have changed."
  (let ((current (skroad--get-point-state)))
    (seq-let (old-p old-zone old-type p zone type) (append prev current)
      (when
          (cond ;; text type actions `on-leave` and `on-enter` may both fire
           ((not (eq old-zone zone)) ;; point moved or text changed under it
            (when old-zone ;; point was in a zone, but has left it
              (skroad--type-action old-type 'on-leave old-p auto))
            (when zone ;; point has entered a different zone
              (skroad--type-action type 'on-enter old-p auto))
            t)
           ((and (not (eq old-p p)) old-zone) ;; moved but remained in zone
            (skroad--type-action old-type 'on-move old-p auto)
            t))
        ;; If done moving point, and we went over alt-mark to mark, jump it:
        (when
            (and mark-active skroad--buf-alt-mark (eq p (point)) (eq p (mark)))
          (if (< skroad--buf-alt-mark p) (forward-char) (backward-char)))
        (skroad--motion current t))) ;; Handle possible auto zone change
    t))

(defun skroad--adjust-mark-if-present ()
  "Put mark and alt-mark in the right order, and show/hide selector."
  (cond
   (mark-active
    (skroad--selector-hide)
    (let ((m (mark)) (am skroad--buf-alt-mark) (p (point)))
      (when (and am (> (abs (- p am)) (abs (- p m))))
        (set-mark am)
        (setq-local skroad--buf-alt-mark m))))
   (t
    (skroad--selector-unhide)
    (setq-local skroad--buf-alt-mark nil))))

;; TODO: make on-leave fire when leaving a buffer, and on-enter when entering
(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  ;; (message "pre-event: %S" last-input-event)
  (setq-local mouse-highlight nil
              skroad--buf-pre-command-point-state (skroad--get-point-state)))

;; TODO: some of these should be done only if buffer modified?
(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  ;; (message "post-event: %S" last-input-event)
  (skroad--refontify-current-line)
  (skroad--motion skroad--buf-pre-command-point-state)
  (skroad--adjust-mark-if-present) ;; swap mark and alt-mark if needed
  (skroad--buf-indices-sync) ;; TODO: do it in save hook?
  (when (buffer-modified-p)
    (skroad--renamer-validate)
    ;; TODO: do it in the change hook?
    (unless (skroad--renamer-active-p)
      (skroad--update-stub-status))
    )
  (unless mark-active (setq-local mouse-highlight t))
  (skroad--save-cache-point))

(defun skroad--before-save-hook ()
  "Triggers prior to a skroad buffer save."
  (skroad--renamer-deactivate))

(when skroad--debug
  (defadvice skroad--post-command-hook (around intercept activate)
    (condition-case err
        ad-do-it
      ;; Let the debugger run
      ((debug error) (signal (car err) (cdr err))))))

;; Misc. major mode setup. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--set-writability ()
  "If the current node is a special node, interactive editing is prohibited."
  (setq-local buffer-read-only (skroad--node-special-p)))

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((atomic (skroad--prop-at 'data pos))
          (fwd (<= pos limit)))
      (cond ((and atomic fwd) (goto-char (skroad--zone-end pos)))
            (atomic (goto-char (skroad--zone-start pos)))
            (fwd (forward-word-strictly))
            (t (backward-word-strictly)))
      (point))))

(defconst skroad--find-word-boundary-function-table
  (let ((char-table (make-char-table nil)))
    (set-char-table-range char-table t #'skroad--find-word-boundary)
    char-table)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--str-url-at (pos str)
  "URL at POS in STR, or nil.  For use strictly in `skroad--yank-transformer'."
  (or (get-text-property pos 'shr-url str)
      (let ((type (get-text-property pos 'type str))
            (args (get-text-property pos 'help-args str)))
        (when (and type args (listp args))
          (let* ((raw (format "%s" (car args)))
                 (val (if (string-match-p "/" raw)
                          (file-name-nondirectory raw)
                        raw)))
            (format "help://%s:%s" type val))))))

;; TODO: escape bare URLs when they appear in the yank
(defun skroad--yank-transformer (str)
  "Recover Eww and Help links in STR; strip all text properties."
  (let ((pos 0) (len (length str)) out)
    (while (< pos len)
      (let ((url (skroad--str-url-at pos str))
            (nxt (or (next-property-change pos str) len)))
        (if (not url)
            (push
             (skroad--sanitize-urls (substring-no-properties str pos nxt))
             out)
          (let ((beg pos))
            (while (and (< nxt len) (equal url (skroad--str-url-at nxt str)))
              (setq nxt (or (next-property-change nxt str) len)))
            (push (skroad--make-md-url
                   url (string-trim (substring-no-properties str beg nxt)))
                  out)))
        (setq pos nxt)))
    (apply #'concat (nreverse out))))

(defun skroad--emacs-help-url-handler (url &rest _args)
  "URL handler for Emacs help links."
  (with-temp-buffer (help-mode))
  (let* ((payload (when (string-match "://" url)
                    (substring url (match-end 0))))
         (offset (and payload (string-search ":" payload)))
         (scheme (and offset (substring payload 0 offset)))
         (arg    (and offset (substring payload (1+ offset))))
         (type   (and offset (intern-soft scheme)))
         (cat    (and type (get type 'button-category-symbol)))
         (fn     (and cat (get cat 'help-function)))
         (val    (and fn (intern-soft arg))))
    (if (and fn val)
        (ignore-errors (funcall fn val))
      (skroad--info "Help page '%s' was not found!" url))))

(defun skroad--open-node ()
  "Open a skroad node."
  (face-remap-set-base 'header-line 'skroad--title-face)
  (skroad--deactivate-mark) ;; Zap spurious mark from opening links via mouse
  (skroad--init-font-lock)
  (skroad--set-writability) ;; If special node, open it as read-only
  (skroad--cache-intern (skroad--current-node)) ;; TODO?
  (skroad--update-stub-status) ;; TODO: do we want this here?
  (skroad--defer-in-current-buffer (skroad--buf-indices-sync))
  (skroad--goto-node-body-start)
  (skip-syntax-forward " ")
  
  ;; tail block test
  (save-mark-and-excursion
    (skroad--tail-jump-before)
    (skroad--block-add-block (point) (point-max)))
  )

;; Floating header line. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--header-text-for-current-window ()
  "If the title is not visible in the current window, enable the header in it.
Otherwise (including if current buffer is not in the mode), simply return nil."
  (when (and skroad--floating-title-enable
             (skroad--mode-p)
             (skroad--in-node-body-p (window-start)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (skroad--abbrev-string
       (buffer-substring (point) (line-end-position))
       (progn (vertical-motion 1) (point))))))

(defun skroad--vacate-window (window)
  "If the current buffer is in a different mode, disable the header in WINDOW."
  (unless (skroad--mode-p)
    (run-with-timer ;; Fires after redraw is complete
     0 nil
     #'(lambda ()
         (when (window-parameter window 'header-line-format)
           (set-window-parameter window 'header-line-format nil)
           (force-window-update window)))))
  nil)

(defun skroad--update-window-state (window &optional _start)
  "Update the state of the given WINDOW (currently containing a Skroad node)."
  (with-selected-window window
    (with-current-buffer (window-buffer)
      (set-window-parameter
       nil 'header-line-format
       (when (skroad--header-text-for-current-window)
         (or (window-parameter nil 'skroad--header-eval)
             (let ((header-updater
                    `(:eval
                      (or (skroad--header-text-for-current-window)
                          (skroad--vacate-window ,window)))))
               (set-window-parameter nil 'skroad--header-eval header-updater)
               header-updater)))))))

;; Point cache. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--point-cache (make-hash-table :test 'equal)
  "Cache storing the last known interactive point position in a node.")

(defun skroad--save-cache-point ()
  "Save the current node's point to the point cache."
  (puthash (skroad--current-node) (point) skroad--point-cache))

(defun skroad--before-kill-buffer-hook ()
  "Triggers prior to a skroad buffer being killed."
  (skroad--save-cache-point))

(defun skroad--maybe-restore-cached-point ()
  "If the current node had been visited in this session, restore the point."
  (let ((cached-point (gethash (skroad--current-node) skroad--point-cache)))
    (when cached-point
      (goto-char cached-point))))

;; Back-end. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--node-ensure (node)
  "Find or create NODE, and ensure that it is interned in the cache.
Return the path where the node is found on disk."
  (let ((node-path (skroad--node-path node))) ;; Path where it would exist
    (unless (skroad--cache-peek node) ;; Do nothing if node is already active
      (cond
       ((file-exists-p node-path) ;; Found on disk, but needs internment
        (skroad--cache-intern-unindexed node)) ;; Intern, index on demand
       ((file-writable-p node-path) ;; Not found on disk, but can be made:
        (write-region (concat node "\n") nil node-path nil 0) ;; Insert title
        (skroad--cache-write node nil) ;; Intern the node with an empty index
        (skroad--node-set-stub node t) ;; It starts as a stub (unless special)
        (message "Created new node: '%s'" node) ;; TODO: write log entry
        )
       (t (error "Could not activate node '%s'!" node))))
    node-path))

(defmacro skroad--with-node (node no-actions &rest body)
  "If NODE does not exist, it is created and interned in the cache.
NODE's indices are synced with any pending changes (or, if absent, created.)
Optional BODY is evaluated with NODE buffer; new changes are synced and saved.
When NO-ACTIONS is nil, changes made by BODY may trigger text type actions."
  (declare (indent defun))
  `(skroad--with-file (skroad--node-ensure ,node)
     (skroad--buf-indices-sync)
     ,(if body
          `(progn
             (skroad--buf-indices-install-tracker)
             (unwind-protect ,@body
               (when (buffer-modified-p)
                 (skroad--buf-indices-sync ,no-actions)
                 (skroad--save-current-node))))
        t)))

;; TODO: override readonly only here, rather than in with-file ?
(defun skroad--in-node (node op target &optional allow-special)
  "Ensure that NODE exists, and run OP on TARGET (nil: current node) from it.
If NODE is a special node, and ALLOW-SPECIAL is nil, do nothing."
  (when (or (not (skroad--node-special-p node)) allow-special)
    (skroad--with-node node t (funcall op target))))

(defun skroad--node-ensure-indices (node)
  "Ensure that NODE (created if required) has been indexed; return its indices."
  (if (skroad--cache-indexed-p node)
      (skroad--cache-fetch node)
    (skroad--with-node node nil (skroad--buf-indices))))

(defvar skroad--special-nodes nil
  "List of all defined special nodes.  These nodes are created automatically;
contain only mechanically-generated content; and cannot be renamed, deleted,
or edited interactively.  Special nodes are not subject to auto-backlinking.")

(defmacro skroad--define-special-node (handle node &rest legend)
  "Define a special NODE; store title in HANDLE."
  (declare (indent defun))
  `(progn
     (defconst ,handle ,node ,@legend) ;; TODO: redundant?
     (add-to-list 'skroad--special-nodes ,node)))

(defun skroad--node-special-p (&optional node)
  "Return t if NODE (if given; else the current node) is a special node."
  (member (or node (skroad--current-node)) skroad--special-nodes))

(defun skroad--connected-p (origin &optional node)
  "Test whether NODE (if given; else the current node) is linked from ORIGIN.
ORIGIN is indexed/created if required.  If NODE is a special node, return nil."
  (unless (skroad--node-special-p node)
    (skroad--indices-has-p 'skroad--text-link-node-live
                           (or node (skroad--current-node))
                           (skroad--node-ensure-indices origin))))

(defun skroad--set-special-status (node special status)
  "Set connection STATUS of NODE from the given SPECIAL (assumed) node.
SPECIAL is created if required.  If NODE itself is a special node, do nothing.
Return t only when the connection status of NODE from SPECIAL actually changed."
  (unless (or (skroad--node-special-p node) ;; If node itself is special, no-op
              (eq (skroad--connected-p special node) status)) ;; no change?
    (skroad--in-node
     special (if status #'skroad--connect-to #'skroad--disconnect-from) node t)
    t))

;; TODO: make log work
(skroad--define-special-node skroad--special-node-log "#Log"
  "Record of node creation, modification, renaming; and lint output.")

(defun skroad--node-logged-p (&optional node)
  "Return t when NODE (if given; else the current node) is linked in the log."
  (skroad--connected-p skroad--special-node-log node))

(skroad--define-special-node skroad--special-node-stubs "#Stubs"
  "A node with links to all known stub nodes. A stub node is a non-special node
without any text between the title and the tail.  New nodes start out as stubs.
Stubs which are disconnected do not retain dead links; when orphaned, a stub
will be queued for auto-deletion (see `skroad--node-set-orphan' below.)")

(defun skroad--node-stub-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known stub."
  (skroad--connected-p skroad--special-node-stubs node))

(defun skroad--node-set-stub (node status)
  "Set the stub STATUS of NODE.  See also `skroad--node-set-orphan'."
  (when (skroad--set-special-status node skroad--special-node-stubs status)
    (skroad--refontify-open-nodes) ;; Refontify links if stub status changed
    (when (and status (skroad--node-orphan-p node)) ;; Became an orphan stub?
      (skroad--defer (skroad--delete-node node))))) ;; ... try deleting.

(skroad--define-special-node skroad--special-node-orphans "#Orphans"
  "A node with links to all known orphans (non-special nodes that have no live
links other than to special nodes and/or to themselves.)  A node found to be an
orphan becomes a candidate for deletion (and the only nodes that may be deleted
are orphan nodes.) A node found to be an orphan stub is auto-deleted if it is
not currently open in any buffer; but if it is, the user is prompted first.")

(defun skroad--node-orphan-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known orphan."
  (skroad--connected-p skroad--special-node-orphans node))

(defun skroad--node-set-orphan (node status)
  "Set the orphan STATUS of NODE.  If it became an orphan stub, try deleting it.
If deletion is blocked, no new auto-deletion attempt will be made until and
unless the node stops being an orphan stub and then later becomes one again."
  (when (and
         (skroad--set-special-status node skroad--special-node-orphans status)
         status (skroad--node-stub-p node))
    (skroad--defer (skroad--delete-node node))))

(defun skroad--delete-node (node &optional force)
  "Request deletion of NODE.  No-op if NODE does not exist.
If NODE is open in a buffer, prompt to ask permission (unless FORCE is t)."
  (when (skroad--cache-peek node)
    (let* ((node-path (skroad--node-path node))
           (visiting-buffer (find-buffer-visiting node-path)))
      (when (or (null visiting-buffer)
                (with-current-buffer visiting-buffer ;; If node is open:
                  (when (or force ;; If force is t, just close it immediately;
                            skroad--lint-in-progress ;; If linting, ditto;
                            (y-or-n-p ;; ... otherwise, user may veto deletion.
                             (format "Permanently delete node '%s' ?" node)))
                    (let ((inhibit-read-only t))
                      (kill-all-local-variables)
                      (restore-buffer-modified-p nil)
                      (kill-buffer)))))
        (skroad--node-set-orphan node nil) ;; Banish from orphans
        (skroad--node-set-stub node nil) ;; Banish from stubs
        (skroad--cache-evict node) ;; Banish from cache
        (delete-file node-path) ;; Permanently delete the node file!
        (message "Deleted node: '%s'" node))))) ;; TODO: write log entry

(defun skroad--current-node-linked-from (&optional all-specials)
  "Return a list of all nodes known to contain a live link to the current node.
If ALL-SPECIALS is t, the list may include any special; otherwise only the log.
The current node itself is not included, even if it contains self-links."
  (let* ((node (skroad--current-node))
         (outgoing-links (skroad--link-get-all-live))
         (possible-specials (if all-specials
                                skroad--special-nodes
                              (list skroad--special-node-log)))
         (candidates (append outgoing-links possible-specials)))
    (seq-filter #'(lambda (n) (skroad--connected-p n node)) candidates)))

;; TODO: log entry
(defun skroad--merge-node-into-current (victim)
  "Permanently merge the node VICTIM (which must exist) into the current node.
Prompts for confirmation, since this op wipes undo info in all affected buffers!
Existing links to VICTIM in the current node are removed.  Its body is copied
to the current node, into a demarcated block above the tail.  Its tail is merged
into the current node's tail.  All non-special nodes (and the Log) which
formerly linked to the VICTIM will now link to the current node instead.
After all of this, the VICTIM is permanently deleted."
  (skroad--buf-indices-sync) ;; Make sure current indices are up to date
  (skroad--complete-all-deferred) ;; Ensure that there are no pending ops
  (let ((this-node (skroad--current-node))) ;; The node we're merging into
    (when (and victim ;; Victim is not nil
               (skroad--cache-peek victim) ;; Victim is a node which exists
               (not (or buffer-read-only ;; Destination must be writable
                        (skroad--node-self-p victim) ;; May not merge self
                        (skroad--node-special-p victim))) ;; ... or special node
               (y-or-n-p ;; Ask first, because the victim will be perma-deleted!
                (format "Permanently merge node '%s' into this node ?" victim)))
      (skroad--disconnect-from victim t) ;; Delete this node's links to victim
      (let (victim-is-stub victim-body victim-linked-from)
        (skroad--with-node victim t ;; Get all relevant info from the victim
          (setq victim-body (skroad--node-extract-body))
          (setq victim-linked-from ;; Nodes (except for this one) linking to it
                (delete this-node (skroad--current-node-linked-from)))
          (setq victim-is-stub (skroad--node-stub-p)))
        (unless victim-is-stub ;; Don't insert anything if victim is a stub
          (skroad--tail-do-before ;; Insert a demarcated copy of victim's body
           (let (import-start import-end)
             (skroad--atomic-comment-insert
              (format "Start of merged node '%s'" victim))
             (newline)
             (setq import-start (point))
             (insert victim-body)
             (setq import-end (point))
             (newline)
             (skroad--atomic-comment-insert
              (format "End of merged node '%s'" victim))
             (newline)
             (goto-char import-start) ;; Jump to the start indicator
             ;; Fix self-links of the victim in the imported body:
             (skroad--link-replace victim this-node import-start import-end))))
        (skroad--buf-indices-sync t) ;; Sync indices, but don't run actions
        ;; Nodes that linked to the victim will now link to this node instead:
        (dolist (affected-node victim-linked-from)
          (skroad--defer
           (skroad--with-node affected-node nil ;; Perform actions
             (skroad--link-merge victim this-node)
             (skroad--clear-buf-undo-info)))))
      (skroad--defer (skroad--refontify-open-nodes))
      (skroad--save-current-node) ;; Save immediately
      (skroad--clear-buf-undo-info) ;; Zap undo info
      (skroad--delete-node victim t)))) ;; Permanently delete the victim!

(defun skroad--rename-node (old new) ;; TODO: log to actual log
  "Rename node OLD to NEW.  OLD is presumed to exist; NEW is a valid title.
Warning: undo info is lost in all affected buffers!"
  (skroad--node-must-exist old)
  (message (format "renaming: '%s' -> '%s'" old new))
  (skroad--complete-all-deferred) ;; Ensure no ops are pending
  (let ((linked-from (skroad--with-node old t
                       (skroad--current-node-linked-from t))))
    (if (and (skroad--cache-rename old new)
             (skroad--mv-file (skroad--node-path old) (skroad--node-path new)))
        (skroad--with-node new t
          (skroad--change-internal-title new)
          (setq-local skroad--current-node-title nil) ;; Zap cached title
          (skroad--link-replace old new) ;; Update any self-links
          (dolist (affected-node linked-from)
            (skroad--defer
             (skroad--with-node affected-node t ;; Don't perform actions
               (skroad--link-replace old new)
               (skroad--clear-buf-undo-info))))
          (skroad--defer (skroad--refontify-open-nodes))
          (skroad--clear-buf-undo-info)) ;; Zap undo info
      (error "Could not rename node '%s' to '%s'!" old new))))

(defun skroad--lint ()
  "Perform a full rescan of all known nodes."
  (message "Lint started...")
  (skroad--complete-all-deferred) ;; Ensure no ops are pending
  (dolist (node ;; Hollow out (don't delete) the nodes we regenerate :
           (list skroad--special-node-orphans skroad--special-node-stubs))
    (skroad--with-node node t
      (skroad--rectify-node-title)
      (skroad--goto-node-body-start)
      (delete-region (point) (point-max))))
  (skroad--cache-foreach ;; Dispatch for each known non-special node:
   #'(lambda (node)
       (unless (skroad--node-special-p node)
         (skroad--defer
          (let ((skroad--lint-in-progress t))
            (message "Linting node: %s" node)
            (skroad--cache-invalidate node)
            (skroad--with-node node t
              (skroad--rectify-node-title)
              (skroad--update-stub-status)
              ))))))
  (skroad--defer (message "Lint completed.")))

(defvar skroad--global-init-done nil
  "Set to t when the Skroad mode global init was completed in this session.")

(defun skroad--global-init ()
  "Perform the mode global init if it has not been done in this session."
  (unless skroad--global-init-done
    ;; Prohibit change hooks firing when only text properties have changed:
    (skroad--silence-modifications 'put-text-property)
    (skroad--silence-modifications 'add-text-properties)
    (skroad--silence-modifications 'remove-text-properties)
    (skroad--silence-modifications 'remove-list-of-text-properties)
    (skroad--silence-modifications 'set-text-properties)
    (skroad--silence-modifications 'add-face-text-property)
    ;; Perform a lint on boot:
    (run-with-idle-timer 0 nil #'skroad--lint)
    (setq skroad--global-init-done t)))

;; TODO: proper mode exit cleanup
;; TODO: do NOT set the mode if file is not in the data dir
(define-derived-mode skroad-mode text-mode "Skroad"
  (skroad--global-init)
  
  ;; Prevent text properties from infesting the kill ring (emacs 28+) :
  (setq-local kill-transform-function #'substring-no-properties)
  
  ;; Don't allow the point to move when a node is scrolled via the mouse:
  ;; (setq-local scroll-preserve-screen-position t)
  
  ;; Install handler for Emacs help URLs:
  (setq-local browse-url-handlers
              '(("\\`help:" . skroad--emacs-help-url-handler)))
  
  ;; Buffer-local hooks:
  (skroad--buf-indices-install-tracker)
  (add-hook 'pre-command-hook 'skroad--pre-command-hook nil t)
  (add-hook 'post-command-hook 'skroad--post-command-hook nil t)
  (add-hook 'before-save-hook 'skroad--before-save-hook nil t)
  (add-hook 'kill-buffer-hook 'skroad--before-kill-buffer-hook nil t)
  (add-hook 'yank-transform-functions #'skroad--yank-transformer nil t)
  (add-hook 'window-scroll-functions #'skroad--update-window-state nil t)
  (add-hook 'window-state-change-functions #'skroad--update-window-state nil t)
  (add-hook 'window-buffer-change-functions #'skroad--update-window-state nil t)
  
  ;; Overlay for when an atomic is under the point. Initially inactive:
  (setq-local skroad--buf-selector (make-overlay (point-min) (point-min)))
  (skroad--selector-deactivate)

  ;; Properties for selector overlay
  (dolist (p skroad--selector-properties)
    (overlay-put skroad--buf-selector (car p) (cadr p)))

  ;; Keymap:
  (use-local-map skroad--mode-keymap)

  ;; Handle word boundaries correctly (atomics are treated as unitary words) :
  (setq-local find-word-boundary-function-table
              skroad--find-word-boundary-function-table)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)

;;; skroad.el ends here
