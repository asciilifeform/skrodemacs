;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Knobs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--debug t)

(unless skroad--debug
  (setq byte-compile-warnings nil))

;;; User data and Special Nodes. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--data-directory "~/skrode" "All user data is found here.")

(defconst skroad--file-extension "skroad"
  "File extension denoting a skroad node.")

;;; Fonts. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--node-title-regex "\\([^][\n\r\f\t\s]+[^][\n\r\f\t]*?\\)"
  "Regex for valid skroad node titles.")

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
  "Face used for dead links."
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
       :weight bold :height 1.2 :inverse-video t))
  "Face used for skroad heading text."
  :group 'skroad-faces)

(defface skroad--node-tail-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "purple"
       :weight bold))
  "Face used for skroad tails."
  :group 'skroad-faces)

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun skroad--hash-empty-p (hash)
  "Return t if HASH is empty."
  (zerop (hash-table-count hash)))

(defun skroad--nop (&rest args)
  "Placeholder function, simply eats its ARGS and does absolutely nothing."
  ())

(defun skroad--mode-p ()
  "Determine whether skroad mode is currently active."
  (derived-mode-p 'skroad-mode))

(defmacro skroad--with-file (file &rest body)
  "Evaluate BODY, operating on FILE (must exist).  Use existing buffer, if any."
  (declare (indent defun))
  (let ((visiting-buffer (make-symbol "visiting-buffer")))
    `(let ((,visiting-buffer (find-buffer-visiting ,file)))
       (if ,visiting-buffer ;; If a buffer is visiting this file, use it:
           (with-current-buffer ,visiting-buffer
             (let ((buffer-read-only nil)) ;; Allow changing special nodes
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
  "If EXP is a keyword, convert it to a symbol. If not, return it as-is."
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
  "Determine value of PROP, if any, including overlays, at POS (or point.)"
  (get-char-property (or pos (point)) prop))

(defun skroad--overlay-active-p (overlay)
  "Determine whether OVERLAY is currently active."
  (and (overlayp overlay) (eq (current-buffer) (overlay-buffer overlay))))

(defun skroad--in-node-title-p (&optional pos)
  "Return t if POS (or point, if not given) is inside the node title."
  (= (line-number-at-pos pos t) 1))

(defun skroad--in-node-body-p (&optional pos)
  "Return t if POS (or point, if not given) is inside the node body."
  (> (line-number-at-pos pos t) 1))

(defun skroad--goto-node-body-start ()
  "Jump to the position at the start of the node body."
  (goto-char (point-min))
  (forward-line 1))

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

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (skroad--mode-p)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

(defconst skroad--time-epsilon 0.01 "Short idle interval for async dispatch.")

;; TODO: do all remote actions async?
(defun skroad--async-dispatch (fn &rest args)
  "Dispatch FN with ARGS asynchronously; buffer is read-only until completed."
  (setq-local buffer-read-only t)
  (let ((here (current-buffer)))
    (run-with-idle-timer
     skroad--time-epsilon nil
     (lambda ()
       (with-current-buffer here
         (apply fn args)
         (skroad--set-writability))))))

;; File and directory ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Ensure that DIR exists, and return t on succeess."
  (or (file-accessible-directory-p dir)
      (progn (make-directory dir) (file-accessible-directory-p dir))))

(defun skroad--storage-ensure ()
  "Ensure that the data directories exist and may be accessed."
  (unless (skroad--ensure-directory skroad--data-directory)
    (error "Unable to access or create skroad data directory!")))

(defun skroad--node-title-to-filename (node)
  "Encode the NODE title into an appropriate filename (with our extension).
Reserved characters are percent-encoded (%XX).  Leading/trailing spaces are
stripped and interior whitespace runs collapsed.  Returns nil if NODE is nil,
empty/whitespace-only, or if the result exceeds 255 UTF-8 bytes.
The original NODE can be recovered using `skroad--file-path-to-node-title'."
  (when node
    (let ((node-nowhite (string-clean-whitespace node)))
      (when (not (string-empty-p node-nowhite))
        (let* ((encoded
                (replace-regexp-in-string
                 (rx (| (any "\x00-\x1f\x7f" ?/ ?\\ ?: ?* ?? ?\" ?< ?> ?| ?%)
                        (seq bos (| (+ ".") "~"))
                        (seq (+ ".") eos)))
                 #'(lambda (m)
                     (mapconcat (lambda (ch) (format "%%%02X" ch)) m ""))
                 node-nowhite t t))
               (filename
                (concat encoded "." skroad--file-extension)))
          (when (<= (length (encode-coding-string filename 'utf-8 t)) 255)
            filename))))))

(defun skroad--file-path-to-node-title (file)
  "Get the base name and parse escapes to decode FILE name to a node title."
  (replace-regexp-in-string
   "%[0-9A-F][0-9A-F]"
   (lambda (match)
     (char-to-string (string-to-number (substring match 1) 16)))
   (file-name-base file) t t))

(defun skroad--current-node-title ()
  "Return the filename-derived title of the node in the current buffer."
  (skroad--file-path-to-node-title (buffer-file-name)))

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
    (concat "*." skroad--file-extension))))

(defun skroad--node-path (node)
  "Generate the canonical file path where NODE would be found if it exists."
  (skroad--file-path-in-data-directory (skroad--node-title-to-filename node)))

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
  :rear-nonsticky t
  :finder-filter t
  )

(skroad--deftype skroad--text-mixin-findable
  :doc "Mixin for all findable text types. (Internal use only.)"
  :mixin t
  :require '(regex-any finder-filter)
  :find-any-forward
  '(lambda (&optional start end)
     (when start (goto-char start))
     (skroad--re-search #'re-search-forward regex-any end finder-filter))
  :find-any-backward
  '(lambda (&optional start end)
     (when start (goto-char start))
     (skroad--re-search #'re-search-backward regex-any end finder-filter))
  :find-any-first '(lambda () (funcall find-any-forward (point-min)))
  :find-any-last '(lambda () (funcall find-any-backward (point-max)))
  :jump-next-from
  '(lambda (pos)
     (goto-char
      (or (save-mark-and-excursion
            (when (or (funcall find-any-forward pos) (funcall find-any-first))
              (match-beginning 0)))
          (point))))
  :jump-prev-from
  '(lambda (pos)
     (goto-char
      (or (save-mark-and-excursion
            (when (or (funcall find-any-backward pos) (funcall find-any-last))
              (match-beginning 0)))
          (point))))
  :for-all-in-region-forward
  '(lambda (start end fn)
     (skroad--re-foreach regex-any fn finder-filter start end))
  :get-match ;; TODO?
  '(lambda ()
     (string-clean-whitespace (match-string-no-properties match-number)))
  )

(skroad--deftype skroad--text-mixin-delimited-findable
  :doc "Base mixin for delimited text types. Define delimiters before using."
  :mixin t
  :require '(payload-regex finder-filter)
  :defaults '((start-delim "") (end-delim "") (match-number 1))
  :generate '(lambda (payload) (concat start-delim payload end-delim))
  :start-delim-regex
  '(if (string-empty-p start-delim) "" (concat (regexp-quote start-delim) "\s*"))
  :end-delim-regex
  '(if (string-empty-p end-delim) "" (concat "\s*" (regexp-quote end-delim)))
  :make-regex '(lambda (s) (concat start-delim-regex s end-delim-regex))
  :regex-any '(funcall make-regex payload-regex)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :walk
  '(lambda (payload fn)
     (skroad--re-foreach (funcall make-regex payload) fn finder-filter))
  :zap '(lambda (payload) (funcall walk payload #'skroad--zap-match))
  :change
  '(lambda (payload &optional new-type new-payload)
     (let ((new-text (funcall (if new-type (get new-type 'generate) generate)
                              (or new-payload payload))))
       (funcall walk payload #'(lambda () (replace-match new-text t t)))))
  :use 'skroad--text-mixin-findable
  )

(defun skroad--transform-at (new-type)
  "Transform the text item at point (including all duplicates) to NEW-TYPE."
  (funcall
   (skroad--prop-at 'change) (skroad--prop-at 'data) new-type))

(defun skroad--type-action (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name (let ((action (get text-type action-name)))
                      (when action (apply action args)))))

;; Font lock rendered text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--text-types-rendered nil "Text types for use with font-lock.")

(skroad--deftype skroad--text-mixin-rendered
  :doc "Finalization mixin for all text types rendered by font-lock."
  :mixin t
  :require '(find-any-forward render)
  :render-next
  '(lambda (limit)
     (when (funcall find-any-forward nil limit)
       (with-silent-modifications (funcall render) t)))
  :font-lock-rule '(lambda () (list render-next '(0 nil append)))
  :register 'skroad--text-types-rendered)

(defconst skroad--font-lock-properties
  '(category face mouse-face zone data)
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
   font-lock-unfontify-region-function #'skroad--nop
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
  (save-mark-and-excursion
    (font-lock-ensure (line-beginning-position) (line-end-position))))

(defun skroad--refontify-current-buffer ()
  "Refresh fontification in the current buffer."
  (font-lock-flush)
  (font-lock-ensure
   (or skroad--visible-start (point-min))
   (or skroad--visible-end (point-max))))

(defun skroad--refontify-open-nodes ()
  "Refresh fontification in all currently-open nodes."
  (skroad--visit-open-nodes (skroad--refontify-current-buffer)))

;; Zoned text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-mixin-rendered-zoned
  :doc "Mixin for zoned text types rendered by font-lock."
  :mixin t
  :require '(face face-function get-match)
  ;; :require 'face
  :render
  '(lambda ()
     (set-text-properties
      (match-beginning 0) (match-end 0)
      (let ((payload (funcall get-match)))
        (list 'category type-name
              'zone (gensym)
              'face (if (functionp face-function)
                        (funcall face-function payload)
                      face)
              'mouse-face (when mouse-face (list mouse-face)) ;; no glomming
              'data payload))))
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

;; Decorative text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-mixin-render-delimited-decorative
  :doc "Mixin for decorative delimited text types rendered by font-lock."
  :mixin t
  :use 'skroad--text-mixin-delimited-findable
  :require 'face
  :render
  '(lambda () (add-face-text-property (match-beginning 0) (match-end 0) face))
  :order 1000 ;; Render these last, so they can amend all other rendered faces
  :use 'skroad--text-mixin-rendered)

;; TODO: fix regexps
(skroad--deftype skroad--text-decorative-italic
  :doc "Italicized text."
  :face 'italic
  :start-delim "__" :end-delim "__"
  :payload-regex "\\([^_]+\\)"
  :use 'skroad--text-mixin-render-delimited-decorative)

(skroad--deftype skroad--text-decorative-bold
  :doc "Bold text."
  :face 'bold
  :start-delim "**" :end-delim "**"
  :payload-regex "\\([^*]+\\)"
  :use 'skroad--text-mixin-render-delimited-decorative)

;; TODO: should have actual delims
(skroad--deftype skroad--text-decorative-heading
  :doc "Heading text."
  :exclude-delims-from-titles t
  :face 'skroad--heading-face
  :payload-regex "^##\s*\\([^\n\r\f\t\s]+[^\n\r\f\t]*\\)"
  :use 'skroad--text-mixin-render-delimited-decorative)

;; Node indices cache. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--cache-table nil
  "Skroad node indices cache.  (Do not access directly: call `skroad--cache`.)
Key is the node title.  Value is `index-me` (this node was not indexed yet);
or the node's indices, if it has been indexed; or `empty` (indices are null).")

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
  "Update NODE with DATA (using `empty` to represent nil) in the cache."
  (puthash node (or data 'empty) (skroad--cache)))

(defun skroad--cache-peek (node)
  "Return the raw value associated with NODE in the cache; nil if not interned."
  (gethash node (skroad--cache)))

(defun skroad--cache-fetch (node)
  "Return indices for NODE; or `index-me` if not indexed; or nil if empty."
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
  (maphash #'(lambda (key val) (funcall fn key)) (skroad--cache)))

;; Indexed text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--index-delta (index payload delta &optional final create destroy)
  "Update the count of PAYLOAD in INDEX by DELTA.
Return `create` if introduced PAYLOAD; `destroy` if removed last copy; else nil.
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
;; TODO: secondary actions should always execute, unless this is a special node
(defun skroad--indices-update (indices changes &optional no-actions init-scan)
  "Apply a set of pending CHANGES to INDICES.  Return the updated INDICES.
The tables in CHANGES are emptied out after being applied to the INDICES.
Also execute the following text type actions (unless NO-ACTIONS) :
`on-create`: a particular payload of this type first appeared in the buffer.
`on-init`: same as above, but during initial scan (INIT-SCAN is t).
`on-destroy`: a particular payload of this type no longer appears in the buffer.
Secondary type actions (run after a primary action has ran, if applicable) :
`on-create-first`: the first payload of this type has appeared in the buffer.
`on-init-first`: same as above, but during initial scan (INIT-SCAN is t).
`on-destroy-last`: the last payload of this type was removed from the buffer.
`on-init-none`: during initial scan, no payloads of this type were found."
  (dolist (pending-change changes)
    (let ((text-type (car pending-change)) (type-changes (cdr pending-change)))
      (when (not (skroad--hash-empty-p type-changes))
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
                   (skroad--type-action text-type action payload))))
           type-changes)
          (clrhash type-changes) ;; Empty the type's pending change index
          ;; Created the first or destroyed the last item of this type?
          (let*
              ((none-after (skroad--hash-empty-p type-index))
               (action
                (cond ((and init-scan none-after) 'on-init-none)
                      ((and none-before (not none-after)) type-create-action)
                      ((and (not none-before) none-after) 'on-destroy-last))))
            ;; was: (or (null action) no-actions)
            (unless (or (null action)
                        (skroad--node-special-p))
              (skroad--type-action text-type action))
            (when none-after ;; Don't waste cache space on empty indices
              (setq indices (assq-delete-all text-type indices))))))))
  indices)

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
  (when (eq skroad--buf-indices-table 'fetch-me) ;; Fetch from cache?
    (setq-local skroad--buf-indices-table
                (skroad--cache-fetch (skroad--current-node-title))))
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
               (skroad--current-node-title)))
      (skroad--index-scan-region (point-min) (point-max) 1)
      (setq have-changes t)
      (setq indices nil))
    (when have-changes
      (setq indices (skroad--indices-update
                     indices skroad--buf-indices-pending
                     (or no-actions (skroad--node-special-p)) init)) ;; TODO?
      (setq-local skroad--buf-indices-table indices)
      (skroad--cache-write (skroad--current-node-title) indices))))

(defvar skroad--text-types-indexed nil "Text types that are indexed.")

(skroad--deftype skroad--text-mixin-indexed
  :doc "Mixin for indexed text types."
  :mixin t
  :require '(for-all-in-region-forward get-match)
  :register 'skroad--text-types-indexed)

(defvar-local skroad--buf-indices-scan-enable t "Toggle index scanning.")

(defun skroad--index-scan-region (start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START ... END
to the pending changes in the buffer;  `skroad--buf-indices-sync` must be
called to finalize all pending changes when no further ones are expected.
If `skroad--buf-indices-scan-enable` is nil, index scanning is disabled."
  (when skroad--buf-indices-scan-enable
    (skroad--with-whole-lines start end
      (dolist (text-type skroad--text-types-indexed) ;; walk all indexed types
        (let ((pending-index
               (skroad--ensure-index skroad--buf-indices-pending text-type)))
          (funcall
           (get text-type 'for-all-in-region-forward)
           start-expanded end-expanded
           #'(lambda ()
               (skroad--index-delta
                pending-index
                (funcall (get text-type 'get-match))
                delta))))))))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in the buffer in region START...END."
  (skroad--index-scan-region start end -1))

(defun skroad--after-change-function (start end length)
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

;; TODO: probably should copy it before walking
(defun skroad--current-indices-foreach (text-type fn &rest other-args)
  "Apply FN to all payloads of TEXT-TYPE in the current node's indices."
  (maphash #'(lambda (key val) (apply fn (cons key other-args)))
           (alist-get text-type (skroad--buf-indices))))

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

(defun skroad--cmd-top-jump-to-next-link ()
  "Jump to the next link following point; cycle to first if no more."
  (interactive)
  (funcall (get 'skroad--text-link-node-alive-or-dead 'jump-next-from) (point)))

(defun skroad--cmd-top-jump-to-prev-link ()
  "Jump to the previous link preceding point; cycle to last if no more."
  (interactive)
  (funcall (get 'skroad--text-link-node-alive-or-dead 'jump-prev-from) (point)))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--cmd-top-backspace
    "<tab>" #'skroad--cmd-top-jump-to-next-link
    "C-<tab>" #'skroad--cmd-top-jump-to-prev-link
    )
  "Top-level keymap for the skroad major mode.")

;;; Atomic Text Type. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-atomic-prepend-space ()
  "Insert a space immediately behind the atomic currently under the point."
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
  (funcall
   (get 'skroad--text-link-node-alive-or-dead 'jump-next-from)
   (skroad--zone-end)))

;; TODO: require zone?
;; TODO: allow mouse click point motion by default
(skroad--deftype skroad--text-atomic
  :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
  :atomic t ;; TODO: check
  :on-enter '(lambda (pos-from auto)
               (skroad--selector-activate-here)
               (goto-char (skroad--zone-start)) ;; point can only sit on start
               (let ((kbd-doc
                      (skroad--prop-at
                       (if buffer-read-only 'kbd-doc-readonly 'kbd-doc))))
                 (when kbd-doc
                   (message kbd-doc))))
  :on-leave '(lambda (pos-from auto) (skroad--selector-deactivate))
  :on-move '(lambda (pos-from auto)
              (goto-char ;; if went forward, jump to the end; else, to the start.
               (if (> (point) pos-from)
                   (skroad--zone-end) (skroad--zone-start))))
  :keymap
  (define-keymap
    "<tab>" #'skroad--cmd-atomic-jump-to-next-link
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
  "Unhide all text that has been hidden with `skroad--hide-text`."
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
(defvar-local skroad--buf-renamer-valid nil "Whether proposed rename is valid.")

(defconst skroad--strings-prohibited-in-titles '("\n" "\r" "\f" "\t" "~"))

(defvar skroad--title-prohibited-regex-cached nil)

(defun skroad--title-prohibited-regex ()
  "Memoized regex matching all strings prohibited in skroad node titles."
  (unless skroad--title-prohibited-regex-cached
    (setq skroad--title-prohibited-regex-cached
          (let ((prohib-strings skroad--strings-prohibited-in-titles))
            (dolist (type skroad--text-types-rendered)
              (when (get type 'exclude-delims-from-titles)
                (let ((start-delim (get type 'start-delim))
                      (end-delim (get type 'end-delim)))
                  (unless (or (null start-delim) (string-empty-p start-delim))
                    (push start-delim prohib-strings))
                  (unless (or (null end-delim) (string-empty-p end-delim))
                    (push end-delim prohib-strings)))))
            (regexp-opt prohib-strings))))
  skroad--title-prohibited-regex-cached)

(defun skroad--renamer-activate (renamer-type start end)
  "Activate the renamer in the current zone, unless already active."
  (unless (skroad--overlay-active-p skroad--buf-renamer)
    (message "Rename node: press <return> to rename, or leave field to cancel.")
    (skroad--suspend-font-lock)
    (skroad--deactivate-mark)
    (skroad--snapshot-prepare)
    (setq-local skroad--buf-indices-scan-enable nil
                cursor-type t
                skroad--buf-renamer-original
                (skroad--prop-at 'data start))
    (skroad--hide-text start end)
    (goto-char end)
    (insert (concat " " skroad--buf-renamer-original " "))
    (setq-local skroad--buf-renamer
                (make-overlay end (point) (current-buffer)))
    (overlay-put skroad--buf-renamer 'category renamer-type)
    (set-buffer-modified-p nil)
    (goto-char end)))

(defun skroad--renamer-deactivate ()
  "Deactivate the renamer if it is currently active."
  (when (skroad--overlay-active-p skroad--buf-renamer)
    (delete-overlay skroad--buf-renamer)
    (skroad--deactivate-mark)
    (skroad--snapshot-rollback)
    (goto-char (overlay-start skroad--buf-hider))
    (skroad--unhide-text)
    (skroad--resume-font-lock)
    (skroad--refontify-current-line)
    (setq-local skroad--buf-indices-scan-enable t
                skroad--buf-renamer-original nil
                skroad--buf-renamer-valid nil)))

(defun skroad--renamer-text ()
  "Get the proposed text in the current renamer."
  (string-clean-whitespace
   (field-string-no-properties (overlay-start skroad--buf-renamer))))

(defun skroad--renamer-get-default-face ()
  "Get the default face of the current renamer."
  (get (overlay-get skroad--buf-renamer 'category) 'face))

(defun skroad--renamer-mark-valid ()
  "Mark the current renamer as valid."
  (overlay-put skroad--buf-renamer 'face (skroad--renamer-get-default-face))
  t)

(defun skroad--renamer-mark-invalid ()
  "Mark the current renamer as invalid."
  (overlay-put
   skroad--buf-renamer 'face
   `(:inherit ,(skroad--renamer-get-default-face)
              :background ,skroad--renamer-faces-invalid-background))
  nil)

;; TODO: bring out the title validator
(defun skroad--renamer-validate-if-active ()
  "If a renamer is active, validate the proposed text."
  (setq-local
   skroad--buf-renamer-valid
   (when (skroad--overlay-active-p skroad--buf-renamer)
     (if (string-match (skroad--title-prohibited-regex) (skroad--renamer-text))
         (skroad--renamer-mark-invalid) (skroad--renamer-mark-valid)))))

(defun skroad--cmd-renamer-accept-changes () ;; TODO: actually rename anything
  "Accept the current renaming."
  (interactive)
  (when skroad--buf-renamer-valid
    (message (format "renaming: '%s' -> '%s'"
                     skroad--buf-renamer-original (skroad--renamer-text)))
    (skroad--renamer-deactivate)))

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
            "RET" #'skroad--cmd-renamer-accept-changes
            "<remap> <keyboard-quit>"
            #'(lambda () (interactive) (skroad--renamer-deactivate)))
  :on-leave '(lambda (pos-from auto) (skroad--renamer-deactivate)))

(defun skroad--cmd-renamer-activate-here ()
  "Activate the renamer for the current zone and type."
  (interactive)
  (skroad--with-current-zone
    (skroad--renamer-activate
     (skroad--prop-at 'renamer-overlay-type) start end)
    (skroad--renamer-validate-if-active)))

(skroad--deftype skroad--text-mixin-renameable
  :doc "Mixin for allowing the use of the rename command with an atomic type."
  :mixin t
  :require 'renamer-overlay-type
  :keymap (define-keymap "r" #'skroad--cmd-renamer-activate-here))

;; Link types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--do-link-action (pos)
  "Run action of link at POS, if one was defined, and no region is active."
  (unless (use-region-p)
    (skroad--type-action
     (skroad--prop-at 'category pos) 'on-activate (skroad--prop-at 'data pos))))

(defun skroad--cmd-link-left-click (click)
  "Perform the action attribute of the link that got the CLICK."
  (interactive "e")
  (let ((estart (event-start click)))
    (select-window (posn-window estart))
    (skroad--do-link-action (posn-point estart))))

(defun skroad--cmd-link-activate ()
  "Perform the action attribute of the link at point."
  (interactive)
  (skroad--do-link-action (point)))

(skroad--deftype skroad--text-link
  :doc "Fundamental type from which all skroad links are derived."
  :use 'skroad--text-atomic
  :exclude-delims-from-titles t
  :keymap (define-keymap
            "<down-mouse-1>" #'skroad--cmd-link-left-click
            "RET" #'skroad--cmd-link-activate))

(defun skroad--cmd-link-comment ()
  "Transform the link under the point to plain text by removing delimiters."
  (interactive)
  (skroad--with-current-zone
    (let ((text (skroad--prop-at 'data)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

;; TODO: preview linked node, or "Stub node" if stub
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--prop-at 'data position)))

(skroad--deftype skroad--text-link-node
  :doc "Fundamental type for skroad node links (live or dead)."
  :use 'skroad--text-link
  :mouse-face 'skroad--highlight-link-face
  :payload-regex skroad--node-title-regex
  :keymap (define-keymap "t" #'skroad--cmd-link-comment))

;; TODO: ensure and open the node
(defun skroad--action-open-link (data)
  (message (format "Live link pushed: '%s'" data)))

;; TODO: store the graph edge for lint
(defun skroad--action-connected-on-init (node)
  "The first instance of a live link to NODE was found during indexing."
  ;; TODO: lint-only?
  ;; (skroad--node-ensure node)
  ;; (skroad--in-node node #'skroad--connect)
  )

(defun skroad--action-connected (node)
  "The first instance of a live link to NODE was introduced."
  ;; (message "Link create: node='%s'" node)
  (skroad--in-node node #'skroad--connect)
  )

(defun skroad--action-disconnected (node)
  "The last instance of a live link to NODE was removed."
  ;; (message "Link destroy: node='%s'" node)
  (skroad--in-node node #'skroad--disconnect)
  )

(defun skroad--action-orphaned ()
  "The current node is an orphan (i.e. it has NO live links)."
  (message "Orphaned: '%s'" (skroad--current-node-title))
  (skroad--node-set-orphan t))

;; TODO: links to specials do not count!
(defun skroad--action-unorphaned ()
  "The current node is NOT an orphan (i.e. it has live links)."
  (message "Unorphaned: '%s'" (skroad--current-node-title))
  (skroad--node-set-orphan nil))

(skroad--deftype skroad--text-renamer-indirect
  :doc "Renamer for editing a node's title while standing on a link to the node."
  :use 'skroad--text-mixin-renamer-overlay
  :face 'skroad--indirect-renamer-face
  :before-string " " :after-string " ")

(defun skroad--cmd-teleyank-at (&rest args)
  "Yank (with optional ARGS) into a node when standing on a live link to it."
  (interactive)
  (skroad--yank-into (skroad--prop-at 'data) args))

(skroad--deftype skroad--text-link-node-live
  :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
  :kbd-doc
  "<return> go|<r> rename|<l> dead|<t> textify|<y> teleyank|<del> zap|<spc> pre-space"
  :kbd-doc-readonly "<return> go|<y> teleyank|"
  :use 'skroad--text-link-node
  :on-init-none #'skroad--action-orphaned
  :on-destroy-last #'skroad--action-orphaned
  :on-init-first #'skroad--action-unorphaned
  :on-create-first #'skroad--action-unorphaned
  :on-init #'skroad--action-connected-on-init
  :on-create #'skroad--action-connected
  :on-destroy #'skroad--action-disconnected
  :on-activate #'skroad--action-open-link
  :face-function #'(lambda (payload) ;; If a link to a stub, use stub link face
                     (if (skroad--node-stub-p payload)
                         'skroad--stub-link-face
                       'skroad--live-link-face))
  :help-echo 'skroad--link-mouseover
  :start-delim "[[" :end-delim "]]"
  :keymap (define-keymap
            "l" #'(lambda () (interactive)
                    (skroad--transform-at 'skroad--text-link-node-dead))
            "y" #'skroad--cmd-teleyank-at ;; Official teleyank trigger
            "<remap> <yank>" #'skroad--cmd-teleyank-at ;; Regular yank also
            )
  :renamer-overlay-type 'skroad--text-renamer-indirect
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-renameable
  :use 'skroad--text-mixin-delimited-findable
  :use 'skroad--text-mixin-rendered-zoned
  :use 'skroad--text-mixin-indexed)

(defun skroad--insert-live-link (node)
  "Insert a live link to NODE at the current point."
  (insert (funcall (get 'skroad--text-link-node-live 'generate) node)))

(defun skroad--connected-p (node)
  "Determine whether the current node has at least one live link to NODE."
  (skroad--current-indices-have-p 'skroad--text-link-node-live node))

;;;;; TODO: when dead link clicked, simply move the point there
;; (defun skroad--cmd-dead-link-activate ()
;;   "Move the point to a dead link."
;;   (interactive)
;;   (goto-char (point)))

(skroad--deftype skroad--text-link-node-dead
  :doc "Dead (i.e. revivable placeholder) link to a skroad node."
  :kbd-doc "<l> liven|<t> textify|<del> zap|<spc> pre-space"
  :use 'skroad--text-link-node
  :start-delim "[-[" :end-delim "]-]"
  :face 'skroad--dead-link-face
  :keymap (define-keymap
            "l" #'(lambda () (interactive)
                    (skroad--transform-at 'skroad--text-link-node-live)))
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-delimited-findable
  :use 'skroad--text-mixin-rendered-zoned
  :use 'skroad--text-mixin-indexed)

(skroad--deftype skroad--text-link-node-alive-or-dead
  :doc "Leaf type exclusively for searching for either live/dead node links."
  :match-number 0
  :regex-any
  (concat "\\(" (get 'skroad--text-link-node-live 'regex-any)
          "\\)\\|\\(" (get 'skroad--text-link-node-dead 'regex-any) "\\)")
  :finder-filter #'skroad--in-node-body-p ;; TODO: use both
  :use 'skroad--text-mixin-findable
  )

(defun skroad--reconnectable-p (node)
  "Determine whether the current node has at least one dead link to NODE."
  (skroad--current-indices-have-p 'skroad--text-link-node-dead node))

(defun skroad--link-deaden (node)
  "Transform all live links to NODE in the current node to dead links."
  (funcall
   (get 'skroad--text-link-node-live 'change)
   node 'skroad--text-link-node-dead))

(defun skroad--link-remove (node)
  "Remove all live links to NODE from the current node."
  (funcall (get 'skroad--text-link-node-live 'zap) node))

(defun skroad--reconnect (node)
  "Transform all dead links to NODE in the current node to live links."
  (funcall
   (get 'skroad--text-link-node-dead 'change)
   node 'skroad--text-link-node-live))

(defun skroad--connect (node)
  "Ensure that the current node has at least one live link to NODE."
  (or (skroad--connected-p node) ;; Already has a live link to node?
      (and (skroad--reconnectable-p node) ;; If not, any dead links to it?
           (skroad--reconnect node)) ;; Try livening the dead links
      (progn ;; If none of the above: create a new link below the tail:
        (skroad--tail-jump-after) ;; If tail didn't exist, it does now
        (newline)
        (skroad--insert-live-link node)
        (skroad--update-stub-status)))) ;; TODO: move this to tail finder

(defun skroad--disconnect (node)
  "Ensure that the current node does NOT have any live links to NODE."
  (and (skroad--connected-p node) ;; Actually has any live links to it?
       (if (or (skroad--node-special-p) (skroad--node-stub-p))
           (skroad--link-remove node) ;; If special or stub, simply remove links
         (skroad--link-deaden node)))) ;; ... otherwise, deaden.

;; TODO: override readonly only here, rather than in with-file ?
(defun skroad--in-node (node op &optional target allow-special)
  "Ensure that NODE exists, and run OP on TARGET (nil: current node) from it.
If NODE is a special node, and ALLOW-SPECIAL is nil, do nothing."
  (when (or allow-special (not (skroad--node-special-p node)))
    (let ((target-or-current (or target (skroad--current-node-title))))
      (skroad--with-node node t (funcall op target-or-current)))))

(defun skroad--yank-into (node &rest yank-args) ;; TODO: undo mechanism?
  "Ensure that NODE exists, and yank into it; then sync indices (with actions.)
Do nothing if NODE is a special node, or if the current kill is empty or blank.
YANK-ARGS (optional) are passed to yank."
  (unless (or (skroad--node-special-p node) ;; Don't teleyank into special nodes
              (string-blank-p (skroad--current-kill-text)))
    (skroad--with-node node nil ;; Yank could contain links, so actions must run
      (skroad--tail-jump-before)
      (ensure-empty-lines 1)
      (apply #'yank yank-args)
      (newline 2)
      (skroad--node-set-stub nil)))) ;; TODO: display confirmation message?

;; URLs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-url-comment ()
  "Turn the URL at point into plain text by placing a space after the prefix."
  (interactive)
  (skroad--with-current-zone
    (save-mark-and-excursion
      (goto-char start)
      (search-forward "//" end)
      (insert " "))))

;; TODO: require whitespace or start/end of line delimiters
(skroad--deftype skroad-text-url-link
  :doc "URL."
  :kbd-doc "<return> go|<t> textify|<del> zap|<spc> pre-space"
  :kbd-doc-readonly "<return> go"
  :use 'skroad--text-link
  :help-echo "External link."
  :face 'skroad--url-link-face
  :match-number 0
  :regex-any ;; TODO: needs whitespace to terminate
  "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\r\f\t\s]+\\)"
  :on-activate #'browse-url
  :keymap (define-keymap "t" #'skroad--cmd-url-comment)
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  :use 'skroad--text-mixin-indexed ;; TODO: do we need this?
  )

;; Node tail. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--node-tail "@@@" "Node tail marker.")

;; TODO: readonly tail, bg colour, etc
(skroad--deftype skroad--text-node-tail
  :doc "Node tail."
  :kbd-doc "Auto-backlinks inserted below this marker; throws inserted above it."
  :use 'skroad--text-atomic
  :face 'skroad--node-tail-face
  :help-echo "Node tail."
  :match-number 0
  :regex-any "^\\(@@@\\)$"
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  )

(defvar-local skroad--buf-tail-marker nil
  "Marker which points after the current node's tail, if known.")

(defun skroad--tail-find-or-emplace ()
  "Find or emplace the tail in the current node, and store its location."
  (unless (funcall (get 'skroad--text-node-tail 'find-any-first))
    (goto-char (point-max))
    (let ((tail (point)))
      (while (and
              (funcall
               (get 'skroad--text-link-node-alive-or-dead 'find-any-backward))
              (eq (match-end 0)
                  (save-mark-and-excursion
                    (goto-char tail)
                    (skip-syntax-backward " ")
                    (point))))
        (setq tail (point)))
      (goto-char tail)
      (ensure-empty-lines 1)
      (setq tail (point))
      (insert skroad--node-tail)
      (ensure-empty-lines 1)
      (goto-char tail)
      (goto-char (line-end-position))))
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

;; (defun skroad--pos-after-tail-p (pos)
;;   "Return t if POS is after the tail (created if it did not exist)."
;;   (>= pos (save-mark-and-excursion (skroad--tail-jump-after) (point))))

(defun skroad--update-stub-status ()
  "Determine whether the current node is a stub, and update Stubs if necessary.
A stub is a node where only whitespace is found between the title and the tail.
If the tail did not previously exist in the current node, it is emplaced."
  (unless (skroad--node-special-p)
    (skroad--node-set-stub
     (save-mark-and-excursion
       (skroad--tail-jump-before)
       (let ((before-tail (point)))
         (skroad--goto-node-body-start)
         (skip-syntax-forward " ")
         (eq (point) before-tail))))))

;; Node title. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--current-internal-title ()
  "Get the current node title from the buffer."
  (buffer-substring-no-properties (point-min) (skroad--get-end-of-line 1)))

(defun skroad--cmd-title-kill-ring-save ()
  "Save the current node's title, transformed to a live link, to the kill ring."
  (interactive)
  (let ((node (skroad--current-internal-title)))
    (with-temp-buffer
      (skroad--insert-live-link node)
      (copy-region-as-kill (point-min) (point-max)))))

(skroad--deftype skroad--text-renamer-direct
  :doc "Renamer for editing a node's title directly."
  :use 'skroad--text-mixin-renamer-overlay
  :face 'skroad--direct-renamer-face
  :before-string "" :after-string " \n")

(skroad--deftype skroad--text-node-title
  :doc "Node title."
  :kbd-doc "<r> Rename this node."
  :use 'skroad--text-atomic
  :order 500
  :keymap
  (define-keymap
    "RET" #'ignore "SPC" #'ignore
    "<deletechar>" #'ignore "<backspace>" #'ignore
    "<remap> <set-mark-command>" #'ignore
    "<remap> <yank>" #'ignore
    "<remap> <kill-region>" #'ignore
    "<remap> <kill-ring-save>" #'skroad--cmd-title-kill-ring-save
    )
  :face 'skroad--title-face
  :inhibit-isearch t ;; Don't interactive-search in the title
  :read-only "Title must be changed via rename command!"
  :renamer-overlay-type 'skroad--text-renamer-direct
  :use 'skroad--text-mixin-renameable
  :match-number 0
  :regex-any "\\`.*\n"
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

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local mouse-highlight nil
              skroad--buf-pre-command-point-state (skroad--get-point-state)))

;; TODO: some of these should be done only if buffer modified?
(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (skroad--refontify-current-line)
  (skroad--motion skroad--buf-pre-command-point-state)
  (skroad--adjust-mark-if-present) ;; swap mark and alt-mark if needed
  (skroad--buf-indices-sync) ;; TODO: do it in save hook?
  (when (buffer-modified-p)
    (skroad--renamer-validate-if-active)
    ;; TODO: do it in the change hook?
    (skroad--update-stub-status) ;; TODO: don't do it if renamer active here?
    )
  (unless mark-active (setq-local mouse-highlight t)))

(defun skroad--before-save-hook ()
  "Triggers prior to a skroad buffer save."
  (skroad--renamer-deactivate))

(defvar-local skroad--visible-start nil
  "Start of last known visible text interval in the current buffer.")

(defvar-local skroad--visible-end nil
  "End of last known visible text interval in the current buffer.")

(defun skroad--update-visible ()
  "Update the last known visible text interval of the current node."
  (setq-local skroad--visible-start (window-start))
  (setq-local skroad--visible-end (window-end)))

(defun skroad--scroll-hook (window start)
  "Triggers when a buffer scrolls."
  (setq-local header-line-format ;; Float the title if it isn't in view
              (when (and skroad--floating-title-enable (> start 1))
                (buffer-substring (point-min)
                                  (skroad--get-end-of-line 1))))
  (skroad--update-visible))

(when skroad--debug
  (defadvice skroad--post-command-hook (around intercept activate)
    (condition-case err
        ad-do-it
      ;; Let the debugger run
      ((debug error) (signal (car err) (cdr err))))))

;; Misc. major mode setup. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; TODO: does this need with-silent-modifications for textmode temp buffers
;;       where skroad--silence-modifications is not in effect?
(defun skroad--yank-handler (category start end)
  "Handler for use with `yank-handled-properties`."
  (message (format "yank! c=%s" category))
  (skroad--with-whole-lines start end
    (remove-list-of-text-properties
     start-expanded end-expanded skroad--font-lock-properties)
    (font-lock-ensure start-expanded end-expanded))
  (skroad--deactivate-mark))

(defun skroad--kill-handler (string)
  "Filter any STRING that enters the kill ring."
  (substring-no-properties string))

(defun skroad--set-writability ()
  "If the current node is a special node, interactive editing is prohibited."
  (setq-local buffer-read-only (skroad--node-special-p)))

(defun skroad--open-node ()
  "Open a skroad node."
  (face-remap-set-base 'header-line 'skroad--title-face)
  (skroad--init-font-lock)
  (skroad--set-writability) ;; If special node, open it as read-only
  ;; (skroad--cache-intern (skroad--current-node-title))
  (skroad--update-visible)
  (skroad--update-stub-status) ;; TODO: do we want this here?
  (skroad--async-dispatch #'skroad--buf-indices-sync) ;; move this to enabler
  )

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
        (write-region (concat node "\n") nil node-path nil 0) ;; only title
        (skroad--cache-write node nil) ;; Intern with an empty index
        (skroad--node-set-stub t node) ;; Becomes a stub (unless special)
        ;; TODO: write journal entry
        (message "Created new node: '%s'" node)
        )
       (t (error "Could not activate node '%s'!" node))))
    node-path))

;; TODO: use async dispatch?
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
                 (save-buffer))))
        t)))

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
  (member (or node (skroad--current-node-title)) skroad--special-nodes))

(skroad--define-special-node skroad--special-node-orphans "#Orphans"
  "A node with links to all known orphans (non-specials without any live links.)
Orphan nodes are candidates for deletion; and only an orphan may be deleted.")

(skroad--define-special-node skroad--special-node-stubs "#Stubs"
  "A node with links to all known stubs (non-specials containing only links.)")

(skroad--define-special-node skroad--special-node-log "#Log"
  "Operation log.")

(defun skroad--special-has-p (special &optional node)
  "Test whether NODE (if given; else the current node) is linked from SPECIAL.
If NODE is a special node, return nil.  If SPECIAL does not exist, create it."
  (unless (skroad--node-special-p node)
    (skroad--indices-has-p 'skroad--text-link-node-live
                           (or node (skroad--current-node-title))
                           (skroad--node-ensure-indices special))))

(defun skroad--node-stub-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known stub."
  (skroad--special-has-p skroad--special-node-stubs node))

(defun skroad--node-orphan-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known orphan."
  (skroad--special-has-p skroad--special-node-orphans node))

(defun skroad--set-special-linkage (special status &optional node)
  "Set connection STATUS of NODE (if given; else the current node) from SPECIAL.
If NODE is a special node, do nothing.  If SPECIAL does not exist, create it.
Return t when the connection status has in fact changed as a result."
  (unless (or (skroad--node-special-p node) ;; If node is special, do nothing
              (eq (skroad--special-has-p special node) status)) ;; no change?
    (let ((node-or-current (or node (skroad--current-node-title))))
      (if status
          (skroad--in-node special #'skroad--connect node-or-current t)
        (skroad--in-node special #'skroad--disconnect node-or-current t)))
    t))

;; TODO: write journal entry
(defun skroad--node-set-stub (status &optional node)
  "Set stub status of NODE (if given; else the current node) to STATUS."
  (when (skroad--set-special-linkage skroad--special-node-stubs status node)
    (skroad--refontify-open-nodes)))

;; TODO: write journal entry
;; TODO: if orphan+stub, delete the node (1st close any buffer where it is open)
(defun skroad--node-set-orphan (status &optional node)
  "Set orphan status of NODE (if given; else the current node) to STATUS."
  (skroad--set-special-linkage skroad--special-node-orphans status node))

;; TODO: write journal entry if changing
(defun skroad--verify-node-title ()
  "Perform consistency check on the title of the current node."
  ;; Verify that the node's internal title matches the filename:
  (let ((node (skroad--current-node-title))
        (internal-title (skroad--current-internal-title)))
    (unless (string-equal internal-title node)
      (message
       "Node '%s' internal title '%s' does not match filename!"
       node internal-title)
      )))

(defun skroad--verify-all-nodes ()
  "Perform consistency check on all nodes, updating the broken node list."
  (skroad--cache-foreach ;; For every node, build indices and verify title
   #'(lambda (node)
       (skroad--with-node node t (skroad--verify-node-title))))
  
  ;; (skroad--cache-foreach
  ;;  #'(lambda (node)
  ;;      (skroad--with-node node t
  ;;        ;; Verify that each live link is reciprocal:
  ;;        )
  ;;      ))
  )

;;(defun skroad--current-indices-foreach (text-type fn &rest other-args)

;; (skroad--verify-all-nodes)
;; skroad--cache-table

;; (skroad--in-node "xyz" #'skroad--connect "qqq1")

;; ;; TODO: handle stub
;; (defun skroad--rename-node (node node-new) ;; TODO: proper renamer
;;   "Rename NODE to NODE-NEW."
;;   (unless (and (skroad--node-cache-rename node node-new)
;;                (skroad--mv-file
;;                 (skroad--node-path node) (skroad--node-path node-new)))
;;     (error "Could not rename node '%s' to '%s'!" node node-new))
;;   t)

;; TODO: proper mode exit cleanup
;; TODO: do NOT set the mode if file is not in the data dir
(define-derived-mode skroad-mode text-mode "Skroad"
  ;; Prohibit change hooks firing when only text properties have changed:
  (skroad--silence-modifications 'put-text-property)
  (skroad--silence-modifications 'add-text-properties)
  (skroad--silence-modifications 'remove-text-properties)
  (skroad--silence-modifications 'remove-list-of-text-properties)
  (skroad--silence-modifications 'set-text-properties)
  (skroad--silence-modifications 'add-face-text-property)

  ;; TODO?
  ;; Zap properties and refontify during yank.
  (setq-local yank-handled-properties '((id . skroad--yank-handler)))

  ;; Prevent text properties from infesting the kill ring (emacs 28+) :
  (setq-local kill-transform-function #'skroad--kill-handler)
  
  ;; Buffer-local hooks:
  
  ;; TODO: allow these in temp mode?
  (skroad--buf-indices-install-tracker)
  
  (add-hook 'pre-command-hook 'skroad--pre-command-hook nil t)
  (add-hook 'post-command-hook 'skroad--post-command-hook nil t)
  (add-hook 'before-save-hook 'skroad--before-save-hook nil t)
  (add-hook 'window-scroll-functions 'skroad--scroll-hook nil t)

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
