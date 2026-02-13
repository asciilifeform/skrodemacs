;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Knobs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--debug t)

(unless skroad--debug
  (setq byte-compile-warnings nil))

;;; User data directories. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--data-directory "~/skrode" "All user data is found here.")

(defconst skroad--active-extension "skroad"
  "File extension denoting a skroad node.")

(defconst skroad--orphans-directory
  (file-name-concat skroad--data-directory "orphans")
  "Subdirectory for storing orphan (i.e. fully-unlinked) nodes.")

(defconst skroad--stub-list-file
  (file-name-concat skroad--data-directory ".stubs")
  "List of nodes that are currently stubs.")

(defconst skroad--stub-removal-list-file
  (file-name-concat skroad--data-directory ".antistubs")
  "List of stubs queued for removal from the stubs list.")

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

;; cyan1

(defface skroad--dead-link-face
  '((t :inherit link :foreground "red"))
  "Face used for dead links."
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

(defun skroad--nop (&rest args)
  "Placeholder function, simply eats its ARGS and does absolutely nothing."
  ())

(defun skroad--mode-p ()
  "Determine whether skroad mode is currently active."
  (derived-mode-p 'skroad-mode))

(defun skroad--keyword-to-symbol (exp)
  "If EXP is a keyword, convert it to a symbol. If not, return it as-is."
  (unless (keywordp exp) (error "%s is not a keyword!" exp))
  (read (substring (symbol-name exp) 1)))

(defmacro skroad--do-plist (key val plist &rest body)
  "Evaluate BODY for side-effects with key,val bound to each pair in PLIST."
  (declare (indent defun))
  (let ((l (gensym)))
    `(let ((,l ,plist))
       (while ,l
         (let ((,key (car ,l)) (,val (cadr ,l)))
           ,@body
           (setq ,l (cddr ,l)))))))

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

(defun skroad--prop-at (prop &optional pos)
  "Determine value of PROP, if any, including overlays, at POS (or point.)"
  (get-char-property (or pos (point)) prop))

(defun skroad--overlay-active-p (overlay)
  "Determine whether OVERLAY is currently active."
  (and (overlayp overlay) (eq (current-buffer) (overlay-buffer overlay))))

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (skroad--mode-p)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

(defconst skroad--time-epsilon 0.01 "Short idle interval for async dispatch.")

(defun skroad--async-dispatch (fn &rest args)
  "Dispatch FN with ARGS asynchronously; buffer is read-only until completed."
  (setq-local buffer-read-only t)
  (let ((here (current-buffer)))
    (run-with-idle-timer
     skroad--time-epsilon nil
     (lambda ()
       (with-current-buffer here
         (apply fn args)
         (setq-local buffer-read-only nil))))))

(defun skroad--canonical-title (s)
  "Return a canonicalized node title from string S."
  (string-clean-whitespace s))

;; File and directory ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--mv-file (old-file new-file &optional overwrite)
  "Move OLD-FILE to NEW-FILE, updating buffers if required.
If OVERWRITE is t, allow overwriting.  Return success."
  (when (and
         (file-readable-p old-file)
         (or overwrite (not (file-exists-p new-file)))
         (file-writable-p new-file))
    (rename-file old-file new-file overwrite)
    (when (and (not (file-exists-p old-file)) (file-readable-p new-file))
      (let ((visiting-buffer (find-buffer-visiting old-file)))
        (when visiting-buffer
          (with-current-buffer visiting-buffer
            (set-visited-file-name new-file t t))))
      t)))

(defun skroad--append-to-file (file string)
  "Silently append STRING to FILE, which is created if it did not exist."
  (write-region (concat string "\n") nil file t 0))

(defun skroad--file-lines-foreach (fn file)
  "Evaluate (for side effects) FN for each line in FILE (if it exists)."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file t nil nil t)
      (while (not (eobp))
        (funcall fn (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
        (forward-line 1)))))

(defun skroad--ensure-directory (dir)
  "Ensure that DIR exists; if not, create it, and return t iff succeeded."
  (or (file-accessible-directory-p dir)
      (progn (make-directory dir) (file-accessible-directory-p dir))))

(defun skroad--ensure-data-directories ()
  "Ensure that the data and orphans directories exist."
  (unless (and (skroad--ensure-directory skroad--data-directory)
               (skroad--ensure-directory skroad--orphans-directory))
    (error "Unable to create skroad data and orphans directories!")))

(defun skroad--filename-to-node (filename)
  "Transform FILENAME to a valid node title."
  (file-name-base filename))

(defun skroad--node-to-filename (node)
  "Transform NODE to a valid file name."
  (file-name-with-extension node skroad--active-extension))

(defun skroad--node-path-in-directory (node directory)
  "Generate the canonical file path for NODE in the given DIRECTORY."
  (expand-file-name
   (file-name-concat directory (skroad--node-to-filename node))))

(defun skroad--node-path (node)
  "Generate the canonical file path where NODE would reside if active."
  (skroad--node-path-in-directory node skroad--data-directory))

(defun skroad--node-orphan-path (node)
  "Generate the canonical file path where NODE would reside if inactive."
  (skroad--node-path-in-directory node skroad--orphans-directory))

(defun skroad--list-active-node-files ()
  "Return a list of all active node files in the data directory."
  (file-expand-wildcards (skroad--node-path "*")))

(defun skroad--current-buffer-node ()
  "Return the filename-derived title of the node in the current buffer."
  (skroad--filename-to-node (buffer-file-name)))

(defun skroad--list-active-nodes-on-disk ()
  "Return a list of all active node titles."
  (mapcar #'skroad--filename-to-node (skroad--list-active-node-files)))

;; Hash sets. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--make-hash-set ()
  "Create a new hash set."
  (make-hash-table :test 'equal))

(defun skroad--hash-set-empty-p (hset)
  "Return t iff HSET is empty."
  (zerop (hash-table-count hset)))

(defun skroad--hashset-member-p (item hset)
  "Return t iff ITEM is a member of the hashset HSET."
  (gethash item hset))

(defun skroad--hashset-add (item hset)
  "Add ITEM to the hashset HSET."
  (puthash item t hset))

(defun skroad--hashset-remove (item hset)
  "Remove ITEM from the hashset HSET."
  (remhash item hset))

(defun skroad--hashset-foreach (fn hset)
  "Evaluate (for side effects) FN on each member of HSET."
  (maphash #'(lambda (k v) (funcall fn k)) hset))

(defun skroad--hashset-subtract (hset-a hset-b)
  "Subtract (destructively) HSET-B from HSET-A."
  (skroad--hashset-foreach #'(lambda (kb) (remhash kb hset-a)) hset-b))

(defun skroad--hashset-from-list (list)
  "Return a hash table where the keys are the elements of LIST.
If LIST is empty, an empty table is returned."
  (let ((hset (skroad--make-hash-set)))
    (mapc #'(lambda (e) (skroad--hashset-add e hset)) list)
    hset))

(defun skroad--hashset-from-list-file (file)
  "Return a hash table where the keys are the lines (without endings) in FILE.
If FILE does not exist, an empty table is returned."
  (let ((hset (skroad--make-hash-set)))
    (skroad--file-lines-foreach
     #'(lambda (line) (skroad--hashset-add line hset)) file)
    hset))

(defun skroad--hashset-to-list-file (hset file)
  "Create or overwrite FILE with a dump of the keys in HSET, one per line."
  (with-temp-file file
    (skroad--hashset-foreach #'(lambda (k) (insert k) (newline)) hset)))

;; Node title cache and autocomplete support. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--nodes-cache (skroad--make-hash-set)
  "Node titles cache.  (Active nodes only).")

(defvar skroad--memo-nodes-ac-list nil
  "Synced list form of the node titles cache.  (Do not access directly).")

(defun skroad--nodes-ac-list ()
  "Return a list of all nodes in the titles cache for use with autocomplete."
  (unless skroad--memo-nodes-ac-list
    (maphash #'(lambda (k v) (push k skroad--memo-nodes-ac-list))
             skroad--nodes-cache))
  skroad--memo-nodes-ac-list)

(defun skroad--nodes-cache-populate ()
  "Populate the titles cache from the data directory listing."
  (skroad--ensure-data-directories)
  (setq skroad--memo-nodes-ac-list (skroad--list-active-nodes-on-disk))
  (setq skroad--nodes-cache
        (skroad--hashset-from-list skroad--memo-nodes-ac-list))
  t) ;; TODO: reverse AC list?

(defun skroad--node-registered-p (node)
  "Test whether NODE is currently interned in the node titles cache."
  (skroad--hashset-member-p node skroad--nodes-cache))

(defun skroad--node-register (node)
  "Intern NODE in the titles cache.  Return t unless it was already interned."
  (unless (skroad--node-registered-p node)
    (push node skroad--memo-nodes-ac-list) ;; Glue it to AC list, fast
    (skroad--hashset-add node skroad--nodes-cache)))

(defun skroad--node-unregister (node)
  "Evict NODE from the titles cache.  Return t unless it was already gone."
  (when (skroad--node-registered-p node)
    (setq skroad--memo-nodes-ac-list nil) ;; Zap AC list, it will get regenned
    (skroad--hashset-remove node skroad--nodes-cache)
    t))

(defun skroad--node-cache-rename (node node-new)
  "Rename NODE to NODE-NEW in the cache.  Return t unless renaming failed."
  (when (skroad--node-registered-p node)
    (unless (skroad--node-registered-p node-new)
      (setq skroad--memo-nodes-ac-list nil) ;; Zap AC list, it will get regenned
      (skroad--hashset-remove node skroad--nodes-cache)
      (skroad--hashset-add node-new skroad--nodes-cache)
      t)))

;; Stub nodes tracker. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--stub-nodes-cache nil "Stub nodes cache.")
(defvar skroad--stub-removal-nodes-cache nil "Cache of stub nodes to remove.")

(defun skroad--stub-cache-populate ()
  "Populate the stub nodes cache (iff empty) from disk, if it exists there.
If the stub removal list exists, eat it, and regenerate the stubs cache file."
  (unless skroad--stub-nodes-cache
    (setq skroad--stub-nodes-cache
          (skroad--hashset-from-list-file skroad--stub-list-file))
    (when (file-exists-p skroad--stub-removal-list-file)
      (skroad--file-lines-foreach
       #'(lambda (line) (skroad--hashset-remove line skroad--stub-nodes-cache))
       skroad--stub-removal-list-file)
      (skroad--hashset-to-list-file
       skroad--stub-nodes-cache skroad--stub-list-file)
      (setq skroad--stub-removal-nodes-cache (skroad--make-hash-set))
      (delete-file skroad--stub-removal-list-file)))
  t)

(defun skroad--stub-registered-p (node)
  "Return t iff NODE is currently interned in the stub nodes cache."
  (skroad--hashset-member-p node skroad--stub-nodes-cache))

(defun skroad--stub-register (node)
  "Intern NODE in the stub nodes cache and add it to the stub list on disk."
  (unless (skroad--stub-registered-p node)
    (skroad--hashset-add node skroad--stub-nodes-cache)
    (skroad--append-to-list-file skroad--stub-list-file node)
    (skroad--hashset-remove node skroad--stub-removal-nodes-cache)))

(defun skroad--stub-unregister (node)
  "Evict NODE from the stub nodes cache and add it to the removal list on disk."
  (when (skroad--stub-registered-p node)
    (skroad--hashset-remove node skroad--stub-nodes-cache)
    (skroad--hashset-add node skroad--stub-removal-nodes-cache)
    (skroad--hashset-to-list-file ;; Stays reasonably small, so won't pound disk
     skroad--stub-removal-nodes-cache skroad--stub-removal-list-file)))

;; (skroad--stub-register "foo3")
;; skroad--stub-nodes-cache

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

(defun skroad--type-action (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name (let ((action (get text-type action-name)))
                      (when action (apply action args)))))

(skroad--deftype skroad--text-mixin-default-type
  :doc "Default text type from which all other types (except mixins) inherit."
  :mixin t
  :order 100 ;; lower number will get rendered first
  :face 'skroad--text-face
  :mouse-face nil
  :rear-nonsticky t)

(skroad--deftype skroad--text-mixin-delimited
  :doc "Base mixin for delimited text types. Define delimiters before using."
  :mixin t
  :require 'payload-regex
  :defaults '((start-delim "") (end-delim "") (match-number 1))
  :make-text '(lambda (payload) (concat start-delim payload end-delim))
  :start-delim-regex
  '(if (string-empty-p start-delim) "" (concat (regexp-quote start-delim) "\s*"))
  :end-delim-regex
  '(if (string-empty-p end-delim) "" (concat "\s*" (regexp-quote end-delim)))
  :make-regex '(lambda (s) (concat start-delim-regex s end-delim-regex))
  :regex-any '(funcall make-regex payload-regex))

(skroad--deftype skroad--text-mixin-findable
  :doc "Mixin for findable text types. (Internal use only.)"
  :mixin t
  :require '(make-regex regex-any finder-regex-forward make-text)
  :find-any-forward '(funcall finder-regex-forward regex-any)
  :find-any-backward '(funcall finder-regex-backward regex-any)
  :find-any-first '(lambda ()
                     (goto-char (point-min))
                     (funcall find-any-forward (point-max)))
  :find-any-last '(lambda ()
                    (goto-char (point-max))
                    (funcall find-any-backward (point-min)))
  :find-payload-forward
  '(lambda (limit p)
     (funcall (funcall finder-regex-forward (funcall make-regex p)) limit))
  :find-payload-backward
  '(lambda (limit p)
     (funcall (funcall finder-regex-backward (funcall make-regex p)) limit))
  :jump-next-from
  '(lambda (pos)
     (goto-char
      (or (save-mark-and-excursion
            (when (or (and (goto-char pos)
                           (funcall find-any-forward (point-max)))
                      (funcall find-any-first))
              (match-beginning 0)))
          (point))))
  :jump-prev-from
  '(lambda (pos)
     (goto-char
      (or (save-mark-and-excursion
            (when (or (and (goto-char pos)
                           (funcall find-any-backward (point-min)))
                      (funcall find-any-last))
              (match-beginning 0)))
          (point))))
  :for-all-in-region-forward
  '(lambda (start end f)
     (save-mark-and-excursion
       (goto-char start)
       (while (funcall find-any-forward end)
         (funcall f (match-string-no-properties match-number)))))
  :have-payload-p
  '(lambda (payload)
     (save-mark-and-excursion
       (goto-char (point-min))
       (funcall find-payload-forward (point-max) payload)))
  :replace-payload-all
  '(lambda (payload payload-new)
     (let ((found-any nil))
       (save-mark-and-excursion
         (goto-char (point-min))
         (while (funcall find-payload-forward (point-max) payload)
           (replace-match payload-new)
           (setq found-any t)))
       found-any))
  :payload-change-type
  '(lambda (payload new-type)
     (funcall replace-payload-all payload
              (funcall (get new-type 'make-text) payload))))

(defun skroad--transform-at (new-type)
  "Transform the text item at point (including all duplicates) to NEW-TYPE."
  (funcall
   (skroad--prop-at 'payload-change-type) (skroad--prop-at 'data) new-type))

(defun skroad--finder-regex-forward (r)
  "Generate a forward finder for regex R."
  (lambda (limit) (re-search-forward r limit t)))

(defun skroad--finder-regex-backward (r)
  "Generate a backward finder for regex R."
  (lambda (limit) (re-search-forward r limit t)))

(skroad--deftype skroad--text-mixin-delimited-anywhere
  :doc "Mixin for delimited text types found anywhere in the buffer."
  :mixin t
  :use 'skroad--text-mixin-delimited
  :finder-regex-forward #'skroad--finder-regex-forward
  :finder-regex-backward #'skroad--finder-regex-backward
  :use 'skroad--text-mixin-findable)

;; TODO: invalidate cache when changing title
(defvar-local skroad--buf-node-body-start-cached nil)

(defun skroad--node-body-start ()
  "Return the first position in the buffer outside of the node title."
  (unless skroad--buf-node-body-start-cached
    (setq-local skroad--buf-node-body-start-cached
                (save-mark-and-excursion
                  (goto-char (point-min))
                  (goto-char (line-beginning-position 2))
                  (point))))
  skroad--buf-node-body-start-cached)

(defun skroad--finder-regex-forward-non-title (r)
  "Generate a forward finder for regex R which excludes the title."
  (lambda (limit)
    (goto-char (max (point) (skroad--node-body-start)))
    (let ((lim (if (< (point) (or limit (point-max)))
                   limit (line-end-position))))
      (re-search-forward r lim t))))

(defun skroad--finder-regex-backward-non-title (r)
  "Generate a backward finder for regex R which excludes the title."
  (lambda (limit)
    (let ((lim (max (skroad--node-body-start) (or limit (point-min)))))
      (when (> (point) lim) (re-search-backward r lim t)))))

(skroad--deftype skroad--text-mixin-delimited-non-title
  :doc "Mixin for delimited text types excluded from the node title."
  :mixin t
  :use 'skroad--text-mixin-delimited
  :finder-regex-forward #'skroad--finder-regex-forward-non-title
  :finder-regex-backward #'skroad--finder-regex-backward-non-title
  :use 'skroad--text-mixin-findable)

;; TODO: broken
;; (defun skroad--finder-regex-forward-non-title-single (r)
;;   "Exactly like `skroad--finder-regex-forward-non-title`, but find one R."
;;   (lambda (limit)
;;     (when (bobp)
;;       (funcall (skroad--finder-regex-forward-non-title r) limit))))

;; (skroad--deftype skroad--text-mixin-delimited-non-title-single
;;   :doc "Mixin for single delimited text types excluded from the node title."
;;   :mixin t
;;   :use 'skroad--text-mixin-delimited
;;   :finder-regex-forward #'skroad--finder-regex-forward-non-title-single
;;   :finder-regex-backward #'skroad--finder-regex-backward-non-title
;;   :use 'skroad--text-mixin-findable
;;   )

;; Font lock rendered text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--text-types-rendered nil "Text types for use with font-lock.")

(skroad--deftype skroad--text-mixin-rendered
  :doc "Finalization mixin for all text types rendered by font-lock."
  :mixin t
  :require '(find-any-forward render)
  :render-next
  '(lambda (limit)
     (when (funcall find-any-forward limit)
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

;; Zoned text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-mixin-render-delimited-zoned
  :doc "Mixin for zoned delimited text types rendered by font-lock."
  :mixin t
  :exclude-delims-from-titles t
  :require 'face
  :render
  '(lambda ()
     (set-text-properties
      (match-beginning 0) (match-end 0)
      (list 'category type-name
            'zone (gensym)
            'face face
            'mouse-face (when mouse-face (list mouse-face)) ;; prevent glomming
            'data (string-clean-whitespace
                   (match-string-no-properties match-number)))))
  :use 'skroad--text-mixin-rendered)

(defun skroad--zone-start (&optional pos)
  "Return the position at which the zone at POS starts."
  (or (previous-single-property-change (1+ (or pos (point))) 'zone)
      (point-min)))

(defun skroad--zone-end (&optional pos)
  "Return the position at which the zone at POS ends."
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
  :use 'skroad--text-mixin-delimited-anywhere
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

(skroad--deftype skroad--text-decorative-heading
  :doc "Heading text."
  :face 'skroad--heading-face
  :payload-regex "^##\s*\\([^\n\r\f\t\s]+[^\n\r\f\t]*\\)"
  :use 'skroad--text-mixin-render-delimited-decorative)

;; Indexed text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--text-types-indexed nil "Text types that are indexed.")

(skroad--deftype skroad--text-mixin-indexed
  :doc "Finalization mixin for indexed text types."
  :mixin t
  :require 'for-all-in-region-forward
  :index-scan-region
  '(lambda (changes start end delta)
     (funcall
      for-all-in-region-forward start end
      #'(lambda (payload)
          (let* ((key (cons type-name payload))
                 (count (+ delta (or (gethash key changes) 0)))) ;; inc/dec
            (if (zerop count) ;; if both added and removed since last update.
                (remhash key changes) ;; ...discard item from changes table.
              (puthash key count changes)))))) ;; otherwise, update.
  :register 'skroad--text-types-indexed)

(defun skroad--index-scan-region (changes start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START ... END,
updating the hash table CHANGES;  `skroad--index-update` must be called on
it to finalize all pending changes when no further ones are expected."
  (dolist (text-type skroad--text-types-indexed) ;; walk all indexed types
    (funcall (get text-type 'index-scan-region) changes start end delta)))

(defun skroad--index-update (index pending &optional init-scan disable-actions)
  "Update INDEX by applying all PENDING changes, running appropriate actions.
If `INIT-SCAN` is t, run a text type's `on-init` rather than `on-create`
action for created entries; an `on-destroy` action runs for destroyed ones.
If `DISABLE-ACTIONS` is t, do not perform any type actions at all."
  (let ((create-action (if init-scan 'on-init 'on-create)))
    (maphash
     #'(lambda (key delta) ;; key and count delta in pending changes table
         (let* ((prior (or (gethash key index) 0)) ;; copies in index prior
                (create (zerop prior)) ;; t if index did not contain this item
                (count (+ prior delta)) ;; copies of item in index + delta
                (destroy (zerop count)) ;; t if change will destroy all copies
                (action ;; text type action to invoke, if any. nil if none.
                 (cond (create create-action)
                       (destroy (remhash key index) 'on-destroy))))
           (unless destroy (puthash key count index)) ;; update index if remains
           (unless disable-actions
             (let ((text-type (car key)) (payload (cdr key))) ;; args for action
               (skroad--type-action text-type action text-type payload)))))
     pending))
  t)

(defvar-local skroad--buf-index nil "Text type index for current buffer.")
(defvar-local skroad--buf-pending-changes nil "Pending index changes.")

;; TODO: scan enable rather than update?
(defvar-local skroad--buf-index-update-enable t "Toggle for index updates.")

(defun skroad--init-buf-index ()
  "Ensure buffer-local indices exist, and populate them from current buffer."
  (when (null skroad--buf-index) ;; Only if this buffer doesn't have one yet
    (setq-local skroad--buf-index (make-hash-table
                                   :test 'equal ;; TODO: custom equal?
                                   :size (line-number-at-pos (point-max) t)))
    ;; Populate while dispatching `on-init`s
    (let ((init-populate
           (make-hash-table :test 'equal
                            :size (hash-table-count skroad--buf-index))))
      (skroad--index-scan-region init-populate (point-min) (point-max) 1)
      (skroad--index-update skroad--buf-index init-populate t))))

;; TODO: unwind protect?
(defun skroad--update-buf-index (&optional disable-actions)
  "Apply all pending changes queued for the buffer-local index.
If `DISABLE-ACTIONS` is t, do not perform type actions while updating."
  (when (and skroad--buf-index-update-enable skroad--buf-pending-changes)
    (skroad--index-update
     skroad--buf-index skroad--buf-pending-changes nil disable-actions)
    (setq-local skroad--buf-pending-changes nil)))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in a skroad buffer in region START...END."
  ;; (message (format "before change: %s %s" start end))
  (when (null skroad--buf-pending-changes)
    (setq-local skroad--buf-pending-changes (make-hash-table :test 'equal)))
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--buf-pending-changes start-expanded end-expanded -1)))

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  ;; (message (format "after change: %s %s %s" start end length))
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--buf-pending-changes start-expanded end-expanded 1)))

(defun skroad--indexed-exists-p (type payload)
  "Find if a PAYLOAD of TYPE currently exists in the buffer-local index."
  (gethash (cons type payload) skroad--buf-index))

(defun skroad--for-all-indexed-of-type (type fn &rest other-args)
  "Apply FN to all payloads of TYPE currently in the buffer-local index."
  (maphash
   #'(lambda (key val)
       (when (eq type (car key)) (apply fn (cons (cdr key) other-args))))
   skroad--buf-index))

;; Top-level keymap for the major mode. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-top-backspace ()
  "If prev point is in an atomic, delete it; otherwise, normal backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
          ((> p (skroad--node-body-start))
           (if (skroad--prop-at 'data (1- p))
               (delete-region (skroad--zone-start (1- p)) p)
             (delete-char -1))))))

(defun skroad--cmd-top-jump-to-next-live-link ()
  "Jump to the next live link following point; cycle to first if no more."
  (interactive)
  (funcall (get 'skroad--text-link-node-live 'jump-next-from) (point)))

(defun skroad--cmd-top-jump-to-prev-live-link ()
  "Jump to the previous live link preceding point; cycle to last if no more."
  (interactive)
  (funcall (get 'skroad--text-link-node-live 'jump-prev-from) (point)))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--cmd-top-backspace
    "<tab>" #'skroad--cmd-top-jump-to-next-live-link
    "C-<tab>" #'skroad--cmd-top-jump-to-prev-live-link
    "<f13>" #'skroad--reboot
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
   (get 'skroad--text-link-node-live 'jump-next-from) (skroad--zone-end)))

(skroad--deftype skroad--text-atomic
  :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
  :on-enter '(lambda (pos-from auto)
               (skroad--selector-activate-here)
               (goto-char (skroad--zone-start)) ;; point can only sit on start
               (let ((kbd-doc (skroad--prop-at 'kbd-doc)))
                 (when kbd-doc (message kbd-doc))))
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
  (assert (null skroad--buf-unrollable-changes))
  (setq-local skroad--buf-unrollable-changes (prepare-change-group))
  (activate-change-group skroad--buf-unrollable-changes))

(defun skroad--snapshot-rollback ()
  "End a temporary change set."
  (assert skroad--buf-unrollable-changes)
  (undo-amalgamate-change-group skroad--buf-unrollable-changes)
  (cancel-change-group skroad--buf-unrollable-changes)
  (setq-local skroad--buf-unrollable-changes nil))

;; Interactive renamer. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  (unless (string-empty-p start-delim)
                    (push start-delim prohib-strings))
                  (unless (string-empty-p end-delim)
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
    (setq-local skroad--buf-index-update-enable nil
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
    (setq-local skroad--buf-index-update-enable t
                skroad--buf-renamer-original nil
                skroad--buf-renamer-valid nil)))

(defun skroad--renamer-text ()
  "Get the text in the current renamer."
  (skroad--canonical-title (field-string-no-properties
                            (overlay-start skroad--buf-renamer))))

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

(defun skroad--renamer-validate-if-active ()
  "If a renamer is active, validate the proposed text."
  (setq-local
   skroad--buf-renamer-valid
   (when (skroad--overlay-active-p skroad--buf-renamer)
     (if (string-match (skroad--title-prohibited-regex) (skroad--renamer-text))
         (skroad--renamer-mark-invalid) (skroad--renamer-mark-valid)))))

(defun skroad--cmd-renamer-accept-changes ()
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
  :face 'link
  :keymap (define-keymap
            "<down-mouse-1>" #'skroad--cmd-link-left-click
            "RET" #'skroad--cmd-link-activate))

(defun skroad--cmd-link-comment ()
  "Delinkify the link under the point to plain text by removing delimiters."
  (interactive)
  (skroad--with-current-zone
    (let ((text (skroad--prop-at 'data)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

;; TODO: preview linked node
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--prop-at 'data position)))

(skroad--deftype skroad--text-link-node
  :doc "Fundamental type for skroad node links (live or dead)."
  :use 'skroad--text-link
  :help-echo 'skroad--link-mouseover
  :payload-regex skroad--node-title-regex
  :keymap (define-keymap "t" #'skroad--cmd-link-comment))

(defun skroad--browse-skroad-link (data)
  (message (format "Live link pushed: '%s'" data)))

(defun skroad--link-init (text-type payload)
  "First instance of PAYLOAD of TEXT-TYPE was found in the buffer during load."
  ;; (message (format "Link init: type=%s payload='%s'" text-type payload))
  )

(defun skroad--link-create (text-type payload)
  "First instance of PAYLOAD of TEXT-TYPE was introduced into the buffer."
  (message (format "Link create: type=%s payload='%s'" text-type payload))
  )

(defun skroad--link-destroy (text-type payload)
  "Last instance of PAYLOAD of TEXT-TYPE was removed from the buffer."
  (message (format "Link destroy: type=%s payload='%s'" text-type payload))
  )

(skroad--deftype skroad--text-renamer-indirect
  :doc "Renamer for editing a node's title while standing on a link to the node."
  :use 'skroad--text-mixin-renamer-overlay
  :face 'skroad--indirect-renamer-face
  :before-string " " :after-string " ")

(skroad--deftype skroad--text-link-node-live
  :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
  :kbd-doc "<return> go|<r> rename|<l> deaden|<t> textify|<del> delete|<spc> prepend space"
  :use 'skroad--text-link-node
  :on-init #'skroad--link-init
  :on-create #'skroad--link-create
  :on-destroy #'skroad--link-destroy
  :on-activate #'skroad--browse-skroad-link
  :mouse-face 'highlight
  :start-delim "[[" :end-delim "]]"
  :keymap (define-keymap
            "l" #'(lambda () (interactive)
                    (skroad--transform-at 'skroad--text-link-node-dead)))
  :renamer-overlay-type 'skroad--text-renamer-indirect
  :use 'skroad--text-mixin-renameable
  :use 'skroad--text-mixin-delimited-non-title
  :use 'skroad--text-mixin-render-delimited-zoned
  :use 'skroad--text-mixin-indexed)

(skroad--deftype skroad--text-link-node-dead
  :doc "Dead (i.e. revivable placeholder) link to a skroad node."
  :kbd-doc "<l> liven|<t> textify|<del> delete|<spc> prepend space"
  :use 'skroad--text-link-node
  :on-init #'skroad--link-init
  :on-create #'skroad--link-create
  :on-destroy #'skroad--link-destroy
  :start-delim "[-[" :end-delim "]-]"
  :face 'skroad--dead-link-face
  :keymap (define-keymap
            "l" #'(lambda () (interactive)
                    (skroad--transform-at 'skroad--text-link-node-live)))
  :use 'skroad--text-mixin-delimited-non-title
  :use 'skroad--text-mixin-render-delimited-zoned
  :use 'skroad--text-mixin-indexed)

(defun skroad--cmd-url-comment ()
  "Turn the URL at point into plain text by placing a space after the prefix."
  (interactive)
  (skroad--with-current-zone
    (save-mark-and-excursion
      (goto-char start)
      (search-forward "//" end)
      (insert " "))))

(skroad--deftype skroad-text-url-link
  :doc "URL."
  :kbd-doc "<return> go|<t> textify|<del> delete|<spc> prepend space"
  :use 'skroad--text-link
  :mouse-face 'highlight
  :help-echo "External link."
  :payload-regex ;; TODO: needs whitespace to terminate
  "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\r\f\t\s]+\\)"
  :on-activate #'browse-url
  :keymap (define-keymap "t" #'skroad--cmd-url-comment)
  :use 'skroad--text-mixin-delimited-non-title
  :use 'skroad--text-mixin-render-delimited-zoned
  :use 'skroad--text-mixin-indexed ;; TODO: do we need this?
  )

;; Skroad link utility ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--link-deaden (node)
  "Turn all live links to NODE in current buffer to dead links."
  (funcall
   (get 'skroad--text-link-node-live 'payload-change-type)
   node
   'skroad--text-link-node-dead))

(defun skroad--link-liven (node)
  "Turn all dead links to NODE in current buffer to live links."
  (funcall
   (get 'skroad--text-link-node-dead 'payload-change-type)
   node
   'skroad--text-link-node-live))

(defun skroad--link-insert-live (node)
  "Insert a live link to NODE into the current buffer at the current point."
  (insert (funcall (get 'skroad--text-link-node-live 'make-text) node)))

(defun skroad--live-p (node)
  "Determine whether a live link to NODE exists in the current buffer."
  (or (and (skroad--mode-p) ;; If we're in skroad mode, use fast path:
           (null skroad--buf-pending-changes) ;; ... no pending changes
           (skroad--indexed-exists-p
            'skroad--text-link-node-live node)) ;; ... query the index.
      (funcall (get 'skroad--text-link-node-live 'have-payload-p) node)))

;; Node tail. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun skroad--tail-marker-init (text-type payload)
;;   "First instance of PAYLOAD of TEXT-TYPE was found in the buffer during load."
;;   (message (format "Tail-Marker init: type=%s payload='%s'" text-type payload))
;;   )

;; (defun skroad--tail-marker-create (text-type payload)
;;   "First instance of PAYLOAD of TEXT-TYPE was introduced into the buffer."
;;   (message (format "Tail-Marker create: type=%s payload='%s'" text-type payload))
;;   )

;; (defun skroad--tail-marker-destroy (text-type payload)
;;   "Last instance of PAYLOAD of TEXT-TYPE was removed from the buffer."
;;   (message (format "Tail-Marker destroy: type=%s payload='%s'" text-type payload))
;;   )

(skroad--deftype skroad--text-node-tail
  :doc "Node tail."
  :kbd-doc "Auto-backlinks inserted below this marker; throws inserted above it."
  :use 'skroad--text-atomic
  ;; :on-init #'skroad--tail-marker-init
  ;; :on-create #'skroad--tail-marker-create
  ;; :on-destroy #'skroad--tail-marker-destroy
  :face 'skroad--node-tail-face
  :help-echo "Node tail."
  :payload-regex "^\\(@@@\\)$"
  :use 'skroad--text-mixin-delimited-non-title
  :use 'skroad--text-mixin-render-delimited-zoned
  ;; :use 'skroad--text-mixin-indexed
  )

(defconst skroad--node-tail "@@@" "Node tail marker.")

(defun skroad--find-node-tail ()
  "Go to the node tail, if one exists, in the current buffer."
  (funcall (get 'skroad--text-node-tail 'find-any-first)))

(defun skroad--put-node-tail () ;; TODO: smart, rather than point-max ?
  "Emplace a node tail in the current buffer."
  (goto-char (point-max))
  (ensure-empty-lines 1)
  (insert skroad--node-tail))

(defun skroad--goto-below-node-tail ()
  "Find or create the node tail in the current buffer; set point below it."
  (or (skroad--find-node-tail) (skroad--put-node-tail)))

(defun skroad--goto-node-tail ()
  "Find or create the node tail in the current buffer; set point at it."
  (skroad--goto-below-node-tail)
  (goto-char (line-beginning-position)))

(defun skroad--put-live-link (node)
  "Emplace a live link to NODE below the node tail in the current buffer."
  (skroad--goto-below-node-tail)
  (ensure-empty-lines 1)
  (skroad--link-insert-live node))

(defun skroad--put-text (text)
  "Emplace given TEXT above the node tail in the current buffer."
  (skroad--goto-node-tail)
  (ensure-empty-lines 1)
  (insert text)
  (ensure-empty-lines 1))

;; Linkage toggling. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: if we zap the last live link, node is now orphaned
(defun skroad--set-linkage (node enable)
  "Ensure that the current buffer has a live link to NODE iff ENABLE is true."
  (if enable
      (or (skroad--live-p node) ;; Already live?
          (skroad--link-liven node) ;; If had dead links to node, liven them
          (skroad--put-live-link node)) ;; If neither: emplace new one at tail.
    (skroad--link-deaden node))) ;; Toggle off: deaden any live links found.

;; Node title. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--get-title ()
  "Get the current node title from the buffer."
  (buffer-substring-no-properties (point-min) (skroad--get-end-of-line 1)))

(defun skroad--cmd-title-kill-ring-save ()
  "Save the current node's title, transformed to a live link, to the kill ring."
  (interactive)
  (let ((title (skroad--get-title)))
    (with-temp-buffer
      (insert (funcall (get 'skroad--text-link-node-live 'make-text) title))
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
  :read-only "Title must be changed via rename command!"
  :renamer-overlay-type 'skroad--text-renamer-direct
  :use 'skroad--text-mixin-renameable
  :find-any-forward
  '(lambda (limit) (when (bobp) (goto-char (skroad--node-body-start)) t))
  :render
  '(lambda ()
     (set-text-properties
      (point-min) (skroad--node-body-start)
      (list 'category type-name
            'zone type-name ;; there can only be one
            'face face
            'data (skroad--get-title))))
  :use 'skroad--text-mixin-rendered)

;;   :use 'skroad--text-mixin-delimited-anywhere
;;   :use 'skroad--text-mixin-render-delimited-zoned
;; :payload-regex "\\`[[:print:]]+$"

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
  ;; (message "pre!")
  (setq-local mouse-highlight nil
              skroad--buf-pre-command-point-state (skroad--get-point-state)))

;; TODO: some of these should be done only if buffer modified?
(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  ;; (message "post!")
  (skroad--refontify-current-line)
  (skroad--motion skroad--buf-pre-command-point-state)
  (skroad--adjust-mark-if-present) ;; swap mark and alt-mark if needed
  (skroad--update-buf-index) ;; TODO: do it in save hook?
  (when (buffer-modified-p)
    (skroad--renamer-validate-if-active))
  (unless mark-active (setq-local mouse-highlight t)))

(defun skroad--before-save-hook ()
  "Triggers prior to a skroad buffer save."
  (skroad--renamer-deactivate))

(defun skroad--scroll-hook (window start)
  "Triggers when a buffer scrolls."
  (setq-local header-line-format ;; Float the title if it isn't in view
              (when (and skroad--floating-title-enable (> start 1))
                (buffer-substring (point-min)
                                  (skroad--get-end-of-line 1)))))

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

(defun skroad--open-node ()
  "Open a skroad node."
  (face-remap-set-base 'header-line 'skroad--title-face)
  (skroad--init-font-lock)
  (skroad--async-dispatch #'skroad--init-buf-index)
  )

(defun skroad--reboot ()
  "Reboot skroad mode. (FOR DEVELOPMENT USE ONLY)."
  (interactive)
  (message "Rebooting skroad!")
  (setq skroad--font-lock-keywords nil)
  (with-temp-buffer
    (setq byte-compile-warnings nil)
    (insert-file-contents (locate-library "skroad.el"))
    (eval-buffer))
  (revert-buffer t t)
  (skroad--init-font-lock))

;; Back-end. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: do initial update in the save hook ?
(defmacro skroad--do-external (&rest body)
  "Evaluate BODY, and then save the current buffer."
  `(unwind-protect
       (progn
         (when (skroad--mode-p)
           (skroad--renamer-deactivate) ;; Cancel renamer to prevent rollback
           (skroad--update-buf-index)) ;; Apply pending updates, firing actions
         (save-mark-and-excursion
           (atomic-change-group ,@body)))
     (when (skroad--mode-p) ;; Only if buffer is actually in skroad mode:
       (skroad--update-buf-index t)) ;; Update index but don't fire any actions
     (save-buffer)))

(defmacro skroad--with-file (node-path &rest body)
  "Evaluate BODY, operating on the node at NODE-PATH."
  `(let ((visiting-buffer (find-buffer-visiting ,node-path)))
     (if visiting-buffer ;; A buffer is visiting this node, so use it:
         (if (eq visiting-buffer (current-buffer))
             (error "May not be called on current buffer!")
           (with-current-buffer visiting-buffer
             (skroad--do-external ,@body)))
       (with-temp-buffer ;; No visiting buffer, so make one:
         (insert-file-contents ,node-path t nil nil t)
         (skroad--do-external ,@body)))))


;; (skroad--with-file
;;  "~/skrode/k.skroad"
;;  (save-mark-and-excursion
;;    (skroad--put-live-link "new node3")
;;    ))

;; (skroad--with-file
;;  "~/skrode/k.skroad"
;;  (save-mark-and-excursion
;;    ;; (skroad--put-text "")
;;    (skroad--put-text "foo123")
;;    ))

(defun skroad--activate-node (node)
  "Find, reactivate, or create NODE; ensure that it is interned in the cache."
  (or (skroad--node-registered-p node) ;; Do nothing if node is already active
      (and
       (let ((node-path (skroad--node-path node))) ;; Path where node belongs
         (or
          (file-exists-p node-path) ;; Found on disk, but needs internment
          (skroad--mv-file ;; May be an orphan, so try reactivating it:
           (skroad--node-orphan-path node) node-path)
          (and ;; Node does not exist on disk, so create it:
           (file-writable-p node-path)
           (progn ;; Initialize the new node, with only a title to start with
             (write-region (concat node "\n") nil node-path nil 0)
             (file-readable-p node-path)))))
       (skroad--node-register node)) ;; Node is on disk, now intern it
      (error "Could not activate node '%s'!" node)))

(defun skroad--deactivate-node (node)
  "Deactivate (i.e. mark as orphan) NODE and evict it from the cache.
If an orphan of NODE already exists in the orphans dir, overwrite it."
  (when (skroad--node-unregister node) ;; Do nothing if already inactive
    (unless (skroad--mv-file
             (skroad--node-path node) (skroad--node-orphan-path node) t)
      (error "Could not deactivate node '%s'!" node)))
  t)

(defun skroad--rename-node (node node-new) ;; TODO: proper renamer
  "Rename NODE to NODE-NEW."
  (unless (and (skroad--node-cache-rename node node-new)
               (skroad--mv-file
                (skroad--node-path node) (skroad--node-path node-new)))
    (error "Could not rename node '%s' to '%s'!" node node-new))
  t)

;; (skroad--activate-node "crap")
;; (skroad--deactivate-node "crap")
;; (skroad--rename-node "crap" "zcrap")
;; (skroad--deactivate-node "zcrap")
;; (skroad--activate-node "zcrap")

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
  (add-hook 'before-change-functions 'skroad--before-change-function nil t)
  (add-hook 'after-change-functions 'skroad--after-change-function nil t)
  
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
