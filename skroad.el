;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

(require 'text-property-search)
(require 'browse-url)

(defvar skroad-mode)
(defvar skroad-search-results-mode)

;;; Knobs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--debug t)

(unless skroad--debug
  (setq byte-compile-warnings nil))

(defconst skroad--lint-on-boot t "If t, Skroad will lint when Emacs loads.")

;;; User data and Special Nodes. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--data-directory "~/skrode" "All user data is found here.")

(defconst skroad--file-extension "skroad"
  "File extension denoting a skroad node.")

(defconst skroad--max-file-name-bytes 253
  "Max bytes permitted in file name.  (Leaves room for lock file suffixes).")

(defconst skroad--log-node-prefix "@" "Prefix character denoting a log node.")

;;; Fonts. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--floating-title-enable t
  "Display floating title at the top of the window if title is not in view.")

(defface skroad--text-face '((t :inherit default))
  "Default face used for skroad text types."
  :group 'skroad-faces)

(defface skroad--search-match-face
  '((t :inherit match :weight bold))
  "Face for occurrences of the search string in search results.")

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

(defconst skroad--quote-backgrounds
  ["#202050" "#204020" "#5c2020" "#4a2050" "#205050"]
  "Pool of quote backgrounds.")

(defface skroad--node-link-face
  '((t :inherit link :underline nil))
  "Face used as a base for all node links."
  :group 'skroad-faces)

(defface skroad--node-link-decor-face
  '((t :background "black"
       :box t))
  "Face inherited from by node link faces used outside of autocomplete."
  :group 'skroad-faces)

(defface skroad--highlight-link-face
  '((t :inherit highlight :box t))
  "Face used for highlighted links."
  :group 'skroad-faces)

(defface skroad--live-link-face
  '((t :inherit skroad--node-link-face))
  "Face used for live links."
  :group 'skroad-faces)

(defface skroad--live-deleted-link-face
  '((t :inherit skroad--live-link-face :strike-through t))
  "Face used for live links to nodes which no longer exist."
  :group 'skroad-faces)

(defface skroad--log-link-face
  '((t :inherit skroad--node-link-face :foreground "white"))
  "Face used for live log links."
  :group 'skroad-faces)

(defface skroad--stub-link-face
  '((t :inherit skroad--node-link-face :foreground "Orange"))
  "Face used for live stub links."
  :group 'skroad-faces)

(defface skroad--special-link-face
  '((t :inherit skroad--node-link-face
       :foreground "black" :background "white"))
  "Face used for live special links."
  :group 'skroad-faces)

(defface skroad--self-link-face
  '((t :inherit skroad--node-link-face
       :foreground "white" :background "purple"))
  "Face used for live self links."
  :group 'skroad-faces)

(defface skroad--dead-link-face
  '((t :inherit skroad--node-link-face :foreground "red"))
  "Face used for dead links."
  :group 'skroad-faces)

(defface skroad--dead-deleted-link-face
  '((t :inherit skroad--dead-link-face :strike-through t))
  "Face used for dead links to nodes which no longer exist."
  :group 'skroad-faces)

(defface skroad--invalid-text-face
  '((t :inherit skroad--text-face
       :foreground "red" :strike-through t :inverse-video t))
  "Face used for invalid text."
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
       :extend t
       :inverse-video t))
  "Face used for atomic comments."
  :group 'skroad-faces)

(defface skroad--node-tail-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "purple"
       :weight bold :extend t))
  "Face used for skroad tails."
  :group 'skroad-faces)

(defface skroad--tail-text-face
  '((t :inherit skroad--text-face
       :background "#002050"
       :extend t))
  "Face used for skroad tail text."
  :group 'skroad-faces)

(defface skroad--quote-glyph-face
  '((t :foreground "gray"))
  "Foreground face for the `> ' glyph in quote-depth blocks."
  :group 'skroad-faces)

(defface skroad--timestamp-face
  '((t :inherit skroad--text-face
       :extend t
       :foreground "black" :background "white"))
  "Face used for timestamps."
  :group 'skroad-faces)

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--info (&rest args)
  "Message with ARGS in the echo bar without polluting the message buffer."
  (let ((message-log-max nil))
    (if (car args)
        (apply #'message args)
      (when (current-message)
        (message nil)))))

(defun skroad--last-ev-was-mouse-p ()
  "Return t when the last input event was a mouse event."
  (listp last-input-event))

(defun skroad--toggle-cursor-state (toggle)
  "Set the cursor and paren mode state for the current buffer to TOGGLE."
  (setq-local cursor-type toggle
              cursor-in-non-selected-windows toggle
              show-paren-mode toggle))

(defun skroad--visible-buffer-substring (from to)
  "Return buffer text between FROM and TO, excluding invisible characters."
  (let (parts (pos from))
    (while (< pos to)
      (let ((next (next-single-property-change pos 'invisible nil to)))
        (unless (invisible-p pos)
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun skroad--fn-or-t (fn &rest args)
  "Call FN on ARGS when it is a function; if it isn't, return t."
  (if (functionp fn) (apply fn args) t))

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

(defun skroad--search-results-p ()
  "Determine whether skroad search results mode is currently active."
  (derived-mode-p 'skroad-search-results-mode))

(defun skroad--mode-or-search-results-p ()
  "Determine whether skroad mode or search results mode is currently active."
  (or (skroad--mode-p) (skroad--search-results-p)))

(defun skroad--modes-only ()
  "Signal an error if a command is performed outside of our modes."
  (unless (skroad--mode-or-search-results-p)
    (user-error
     "This command works only in skroad-mode and skroad-search-results-mode!")))

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

(defvar skroad--at-a-distance nil
  "Will equal t if we are executing inside a `skroad--with-node'.")

(defmacro skroad--with-node (node no-actions &rest body)
  "If NODE does not exist, it is created and interned in the cache.
NODE's indices are synced with any pending changes (or, if absent, created.)
Optional BODY is evaluated with NODE buffer; new changes are synced and saved.
When NO-ACTIONS is nil, changes made by BODY may trigger text type actions."
  (declare (indent defun))
  `(let ((skroad--at-a-distance t))
     (skroad--with-file (skroad--node-ensure ,node)
       (skroad--buf-indices-sync)
       ,(if body
            `(unwind-protect ,@body
               (when (buffer-modified-p)
                 (skroad--buf-indices-sync ,no-actions)
                 (skroad--before-save-common)
                 (skroad--save-current-node))
               (when (skroad--mode-p)
                 (skroad--selector-update)))
          t))))

(defmacro skroad--visit-open-nodes (&rest body)
  "Evaluate BODY in each currently-open node or search result buffer."
  (declare (indent defun))
  (let ((visiting-buffer (make-symbol "visiting-buffer")))
    `(dolist (,visiting-buffer (buffer-list))
       (with-current-buffer ,visiting-buffer
         (when (skroad--mode-or-search-results-p)
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
  (when (and (bolp) (eolp))
    (delete-line)))

(defun skroad--zap-match ()
  "Delete the current match, as well as the empty line which may result."
  (delete-region (match-beginning 0) (match-end 0))
  (skroad--delete-line-if-empty))

(defun skroad--prop-at (prop &optional pos)
  "Determine value of PROP, if any, including overlays, at POS (or point)."
  (get-char-property (or pos (point)) prop))

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
  "Execute FN for each match of REGEXP, scanning from END (or point-max)
back to START (or point-min); optionally filtered by FILTER.  FN must
confine its edits to the matched text.  Return t if there were any matches."
  (save-mark-and-excursion
    (goto-char (or end (point-max)))
    (let ((limit (or start (point-min)))
          did-any)
      (while (skroad--re-search #'re-search-backward regexp limit filter)
        (setq did-any t)
        (let ((beg (match-beginning 0)))
          (funcall fn)
          (goto-char beg)))
      did-any)))

(defun skroad--clean-whitespace (s)
  "Remove excess whitespace from S."
  (save-match-data (string-clean-whitespace s)))

(defconst skroad--whitespace-chars " \t\n\r\f"
  "Denotes characters which are classified as whitespace.")

(defun skroad--skip-whitespace-forward (&optional limit)
  "Skip any whitespace characters in the forward direction, up to LIMIT."
  (skip-chars-forward skroad--whitespace-chars limit))

(defun skroad--skip-whitespace-backward (&optional limit)
  "Skip any whitespace characters in the backward direction, down to LIMIT."
  (skip-chars-backward skroad--whitespace-chars limit))

(defun skroad--abbrev-string (string lim)
  "If STRING is LIM or more characters long, truncate it with an ellipsis."
  (if (< (length string) lim)
      string
    (concat (substring string 0 (max 0 (1- lim))) "…")))

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (skroad--mode-or-search-results-p)
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

(defun skroad--node-body-start-pos () ;; TODO: memoize
  "Return the position where the current node's body begins."
  (save-mark-and-excursion (skroad--goto-node-body-start) (point)))

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
          (t ;; The work queue has emptied:
           (skroad--info nil) ;; Zap the progress indicator, if active
           (skroad--maybe-refontify-now))))) ;; If refontify is pending, do it

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
  (when skroad--idle-work-queue
    (skroad--idle-work-run-slice t)))

(defun skroad--idle-no-work-p ()
  "Return t when the idle queue is empty."
  (null skroad--idle-work-queue))

(defun skroad--idle-have-work-p ()
  "Return t when the idle queue is not empty."
  (not (skroad--idle-no-work-p)))

;; File and directory ops. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: actions at a distance always save?!!!!!
;; TODO: prevent backup files litter?
(defun skroad--save-current-node ()
  "Save the current node."
  (when (and (skroad--current-buffer-node-p)
             buffer-file-name
             (not (skroad--renamer-active-p)))
    (setq-local require-final-newline t) ;; Insert final newline if absent
    (save-buffer)))

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
empty/whitespace-only, or if the encoded result exceeds the max file name.
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
          (when (<= (length (encode-coding-string filename 'utf-8 t))
                    skroad--max-file-name-bytes)
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

(defun skroad--validate-node-title (title)
  "Return t when TITLE is a validly-encodable node title."
  (let ((encoded-title (skroad--node-title-to-filename title)))
    (and encoded-title
         (equal (skroad--file-path-to-node-title encoded-title) title))))

(defun skroad--validate-node-title-for-rename (title)
  "Return t when TITLE represents a valid node title."
  (and (skroad--link-valid-p title) (skroad--validate-node-title title)))

;; TODO: alarm unreachables to log
;; TODO: try to rename unreachables
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

(defun skroad--keymap-write-filter (cmd)
  "Void CMD in read-only buffers if it declares (interactive \"*...\")."
  (let ((spec (and (fboundp cmd) (cadr (interactive-form cmd)))))
    (unless (and buffer-read-only (stringp spec) (string-prefix-p "*" spec))
      cmd)))

(defun skroad--keymap-guard (def)
  (if (symbolp def)
      `(menu-item "" ,def :filter skroad--keymap-write-filter) def))

(defun skroad--define-keymap (&rest pairs)
  "Like `define-keymap'; commands with (interactive \"*\") are void
in read-only buffers.  Accepts :parent MAP among PAIRS."
  (declare (indent 0))
  (let ((map (make-sparse-keymap)))
    (while pairs
      (if (eq (car pairs) :parent)
          (set-keymap-parent map (progn (pop pairs) (pop pairs)))
        (keymap-set map (pop pairs) (skroad--keymap-guard (pop pairs)))))
    map))

(defun skroad--key-resolve (bd)
  "Apply menu-item filters as lookup would; nil if currently void."
  (while (eq (car-safe bd) 'menu-item)
    (let ((f (plist-get (nthcdr 3 bd) :filter)))
      (setq bd (if f (funcall f (nth 2 bd)) (nth 2 bd)))))
  bd)

(defun skroad--get-keymap-docs (keymap)
  "Alist of (KEY . DOC) for live documented bindings in KEYMAP and parents."
  (let (entries remaps seen)
    (while (keymapp keymap)
      (map-keymap
       (lambda (ev bd)
         (cond ((eq ev 'remap)
                (map-keymap (lambda (c n)
                              (unless (assq c remaps) (push (cons c n) remaps)))
                            bd))
               ((and (not (memq ev seen))
                     (setq bd (skroad--key-resolve bd))) ; void: parent active
                (push ev seen)
                (push (cons ev bd) entries))))
       keymap)
      (setq keymap (keymap-parent keymap)))
    (nreverse
     (delq
      nil
      (mapcar
       (lambda (e)
         (let ((ev (car e))
               (fn (skroad--key-resolve
                    (or (cdr (assq (cdr e) remaps)) (cdr e)))))
           (and (or (integerp ev)
                    (and (symbolp ev)
                         (not
                          (string-match-p
                           "mouse\\|drag\\|click\\|wheel\\|-bar\\'\\|-line\\'"
                           (symbol-name ev)))))
                (functionp fn) (not (eq fn 'ignore))
                (let ((doc (documentation fn)))
                  (and doc (cons (key-description (vector ev))
                                 (car (split-string doc "\n"))))))))
       entries)))))

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
  :validator t
  :hide-escapes nil
  :escape #'identity
  :unescape #'identity
  )

(skroad--deftype skroad--text-mixin-findable
  :doc "Mixin for all findable text types. (Internal use only.)"
  :mixin t
  :require '(regex-any finder-filter escape unescape validator)
  :defaults '((visible-match-number nil))
  :validate
  '(lambda (payload)
     (or (not (functionp validator)) (funcall validator payload)))
  :find
  '(lambda (method regex &optional lim)
     (skroad--re-search method regex lim finder-filter))
  :find-any-forward
  '(lambda (&optional lim)
     (funcall find #'re-search-forward regex-any lim))
  :find-any-backward
  '(lambda (&optional lim)
     (funcall find #'re-search-backward regex-any lim))
  :for-all-in-region
  '(lambda (start end fn)
     (skroad--re-foreach regex-any fn finder-filter start end))
  :get-match
  '(lambda () (match-string-no-properties match-number))
  :get-unescaped-payload
  '(lambda ()
     (funcall unescape (skroad--clean-whitespace (funcall get-match))))
  :swap-match
  '(lambda (new-payload &optional only-payload)
     (replace-match new-payload t t nil (and only-payload match-number)))
  )

(defvar skroad--text-types-delimited nil "Text types having delimiters.")

(skroad--deftype skroad--text-mixin-delimited
  :doc "Base mixin for delimited text types. Define delimiters before using."
  :mixin t
  :require '(begins ends payload-regex finder-filter)
  :defaults '((match-number 1))
  :make-regex
  '(lambda (payload)
     (rx (literal begins) (group (regexp payload)) (literal ends)))
  :regex-any '(funcall make-regex payload-regex)
  :use 'skroad--text-mixin-findable
  :generate
  '(lambda (payload)
     (concat begins (funcall escape payload) ends))
  :make-escaped-regex
  '(lambda (payload)
     (funcall make-regex (rx (literal (funcall escape payload)))))
  :regex-validator '(rx bos (regexp payload-regex) eos)
  :regex-validate
  '(lambda (payload)
     (string-match-p regex-validator (funcall escape payload)))
  :search
  '(lambda (payload &optional lim)
     (funcall find #'re-search-forward
              (funcall make-escaped-regex payload) lim))
  :walk
  '(lambda (payload fn &optional start end)
     (skroad--re-foreach
      (funcall make-escaped-regex payload) fn finder-filter start end))
  :zap
  '(lambda (payload &optional start end)
     (funcall walk payload #'skroad--zap-match start end))
  :regen
  '(lambda (payload &optional new-type new-payload start end)
     (let ((new-text (funcall (if new-type (get new-type 'generate) generate)
                              (or new-payload payload))))
       (funcall walk payload
                #'(lambda () (funcall swap-match new-text)) start end)))
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
  :font-lock-rule '(lambda () (list render-next))
  :register 'skroad--text-types-rendered)

(defconst skroad--font-lock-properties
  '(category face mouse-face zone data rear-nonsticky read-only inhibit-isearch
             display line-prefix wrap-prefix invisible quote-depth)
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
(defvar-local skroad--font-lock-fontify-region nil)
(defvar-local skroad--font-lock-suspended nil)

(defun skroad--suspend-font-lock ()
  "Suspend all font-lock fontification and defontification in current buffer.
Existing text properties are preserved.  No visual change occurs."
  (unless skroad--font-lock-suspended
    (setq-local
     skroad--font-lock-suspended t
     skroad--font-lock-unfontify-region font-lock-unfontify-region-function
     skroad--font-lock-fontify-region font-lock-fontify-region-function
     font-lock-unfontify-region-function (lambda (&rest _args) nil)
     font-lock-fontify-region-function (lambda (&rest _args) nil))))

(defun skroad--resume-font-lock ()
  "Resume font-lock in current buffer.
No refontification is triggered; existing properties are untouched."
  (when skroad--font-lock-suspended
    (setq-local
     font-lock-unfontify-region-function skroad--font-lock-unfontify-region
     font-lock-fontify-region-function skroad--font-lock-fontify-region
     skroad--font-lock-suspended nil)))

(defun skroad--fontify-current-line ()
  "Ensure fontification of the current line in a skroad buffer.
Return t when we were actually in the mode and the refontification happened."
  (when (skroad--mode-or-search-results-p)
    (save-mark-and-excursion
      (font-lock-ensure (line-beginning-position) (line-end-position)))
    t))

(defun skroad--refontify-current-buffer ()
  "Refresh fontification in the visible portion of the current buffer."
  (font-lock-flush) ;; Flush all fontification, will get refontified on demand
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (when windows
      (let ((start (apply #'min (mapcar #'window-start windows)))
            (end (apply #'max
                        (mapcar #'(lambda (w) (window-end w t)) windows))))
        (font-lock-ensure start end)
        (skroad--selector-update)))))

(defun skroad--refontify-open-nodes ()
  "Refresh fontification in all currently-open nodes."
  (skroad--visit-open-nodes (skroad--refontify-current-buffer)))

(defvar skroad--refontify-pending nil
  "When true, a refontification has been requested.")

(defun skroad--request-refontify ()
  "Request a refontification of currently-open nodes at the next idle point."
  (setq skroad--refontify-pending t))

(defun skroad--maybe-refontify-now (&optional and-later)
  "Perform any pending request for the refontification of open nodes.
If AND-LATER is true, don't clear the request flag."
  (when skroad--refontify-pending
    (setq skroad--refontify-pending and-later)
    (skroad--refontify-open-nodes)))

;; Deferred replacement mechanism:
;;
;; A replacement is recorded as a text property on the spanned text and applied
;; at command end by the drain, back to front, point-neutrally.  The tag is
;; removed SILENTLY before each recorded edit so undo can never restore a tag
;; onto reinserted text (which would let a later drain re-apply a stale,
;; undo-resurrected schedule).

(defvar-local skroad--buf-has-deferred-replacements nil
  "When true, indicates that deferred replacements are pending in this buffer.")

(defconst skroad--deferred-replace-tag 'skroad--replace-later
  "Text property used to denote a deferred replacement.")

(defun skroad--deferred-replace (start end new)
  "Schedule replacement of START..END with NEW after the current command.
The tag put is silent (unrecorded), so it is never itself in the undo record."
  (setq-local skroad--buf-has-deferred-replacements t)
  (with-silent-modifications
    (put-text-property start end skroad--deferred-replace-tag
                       (substring-no-properties new))))

(defun skroad--do-deferred-replacements ()
  "Apply all deferred replacements in the buffer, back to front.
Point-neutral (`save-excursion').  Highest-position-first, so no edit shifts a
not-yet-visited run.  When the old text is a prefix of the new, only the
divergent tail is inserted (the char point sits on is never deleted).  The tag
is removed silently before each recorded edit, so undo cannot resurrect it.
Edits run under `inhibit-modification-hooks' (no mode hook sees them); the
touched lines are marked stale via `fontified' so redisplay refreshes them."
  (when skroad--buf-has-deferred-replacements
    (setq-local skroad--buf-has-deferred-replacements nil)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (lo most-positive-fixnum)
          (hi 0))
      (save-excursion
        (goto-char (point-max))
        (while (> (point) (point-min))
          (let* ((end (point))
                 (beg (previous-single-property-change
                       end skroad--deferred-replace-tag nil (point-min)))
                 (val (get-text-property beg skroad--deferred-replace-tag)))
            (when (stringp val)
              (let ((old (buffer-substring-no-properties beg end)))
                (setq lo (min lo beg))
                ;; Untag silently FIRST: any text a recorded edit below deletes
                ;; is then tag-free, so undo reinserts it without the tag.
                (with-silent-modifications
                  (remove-text-properties
                   beg end (list skroad--deferred-replace-tag nil)))
                (if (string-prefix-p old val)
                    (progn ; append divergent tail; keep common prefix
                      (goto-char end)
                      (insert (substring val (length old))))
                  (delete-region beg end) ; general case
                  (unless (string-empty-p val)
                    (goto-char beg)
                    (insert val)))
                (setq hi (max hi (point)))))
            (goto-char beg))))
      (when (<= lo hi)
        (put-text-property
         (save-excursion (goto-char lo) (line-beginning-position))
         (save-excursion (goto-char hi) (line-end-position))
         'fontified nil)))))

;; Zoned text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--atomic-show-payload-only t
  "If t, rendered atomics display only their payloads.")

(skroad--deftype skroad--text-mixin-rendered-zoned
  :doc "Mixin for zoned text types rendered by font-lock."
  :mixin t
  :require '(face face-function get-unescaped-payload validate)
  :render
  '(lambda ()
     (let ((start (match-beginning 0))
           (end (match-end 0))
           (payload (funcall get-unescaped-payload)))
       (if (funcall validate payload) ;; If there's a validator, use it
           (let* ((zone (gensym))
                  (props
                   (list 'category type-name
                         'zone zone
                         'data payload))
                  (add-face
                   (if (functionp face-function)
                       (funcall face-function payload)
                     face)))
             (if (facep mouse-face)
                 (plist-put props 'mouse-face (list mouse-face)) props)
             (add-text-properties start end props)
             (when (and (numberp visible-match-number)
                        skroad--atomic-show-payload-only)
               (let ((vis-start (match-beginning visible-match-number))
                     (vis-end (match-end visible-match-number)))
                 (put-text-property start vis-start 'invisible t)
                 (put-text-property vis-end end 'invisible t)
                 (setq start vis-start end vis-end)))
             (when hide-escapes
               (skroad--hide-escape-slashes start end))
             (add-face-text-property start end add-face))
         ;; Invalid: highlight with the invalidity face
         (add-face-text-property start end 'skroad--invalid-text-face)
         )))
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

(defmacro skroad--with-current-visible-zone (&rest body)
  "Evaluate BODY with vis-start and vis-end denoting the visible zone at point."
  (declare (indent defun))
  `(skroad--with-current-zone
     (let* ((vis-start
             (save-mark-and-excursion
               (goto-char start)
               (while (and (< (point) end)
                           (invisible-p
                            (get-char-property (point) 'invisible)))
                 (forward-char))
               (point)))
            (vis-end
             (save-mark-and-excursion
               (goto-char end)
               (while (and (> (point) vis-start)
                           (invisible-p
                            (get-char-property (1- (point)) 'invisible)))
                 (backward-char))
               (point))))
       ,@body)))

(defun skroad--zone-jump-from (pos &optional backwards wrap)
  "Jump to the next (or previous, if BACKWARDS) distinct zone from POS.
If WRAP is t, wrap if there are no further zones in the current direction.
Return the new position if the jump actually happened; otherwise nil."
  (let* ((current (get-text-property pos 'zone))
         (pred #'(lambda (_ v) (and v (not (eq v current)))))
         (search
          (lambda (from)
            (goto-char from)
            (if backwards
                (progn (font-lock-ensure (point-min) from)
                       (text-property-search-backward 'zone nil pred))
              (font-lock-ensure from (point-max))
              (text-property-search-forward 'zone nil pred)))))
    (let ((match (or (funcall search pos)
                     (and wrap
                          (funcall search
                                   (if backwards (point-max) (point-min)))))))
      (when match
        (goto-char (prop-match-beginning match))))))

(defun skroad--data-at (&optional pos)
  "If there is atomic data at POS (if given; otherwise, at point), return it."
  (get-text-property (or pos (point)) 'data))

(defun skroad--find-data-between (start end)
  "Return the first atomic data between START and END; or nil, if none."
  (text-property-not-all start end 'data nil))

;; Line Quotes. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--quote-bg-for-level (level)
  "Obtain the background colour used for quotes of LEVEL."
  (aref skroad--quote-backgrounds
        (mod (1- level) (length skroad--quote-backgrounds))))

(defun skroad--quoted-line-render ()
  "Render a line quote."
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (eol (match-end 0))
         (line-end (if (eq (char-before eol) ?\n) (1- eol) eol))
         (depth 0)
         (seg-start beg)
         (prefix "")
         last-face)
    (goto-char beg)
    (while (< (point) end)
      (skroad--skip-whitespace-forward end)
      (when (eq (char-after) ?>)
        (forward-char 1)
        (setq depth (1+ depth))
        (let* ((face
                `(:background ,(skroad--quote-bg-for-level depth) :extend t))
               (replacement (copy-sequence " > ")))
          (put-text-property 1 2 'cursor (- (point) seg-start 1) replacement)
          (setq last-face face)
          (add-face-text-property seg-start (point) face)
          (add-face-text-property seg-start (point) 'skroad--quote-glyph-face)
          (put-text-property seg-start (point) 'display replacement)
          (setq prefix (concat prefix (propertize "   " 'face face))))
        (setq seg-start (point))))
    (when last-face
      (add-face-text-property seg-start eol last-face)
      (put-text-property end line-end 'wrap-prefix prefix)
      (put-text-property end eol 'quote-depth depth))))

(skroad--deftype skroad--text-quoted-line
  :doc "Text type for quoted lines rendered by font lock."
  :order 2
  :regex-any
  (rx line-start
      (group (one-or-more (seq (zero-or-more blank) ">" (zero-or-more blank))))
      (zero-or-more not-newline)
      (optional "\n"))
  :render #'skroad--quoted-line-render
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-regexp-rendered
  :use 'skroad--text-mixin-rendered)

(defun skroad--quote-restore-point ()
  "Step point past a freshly emplaced quote prefix.  Run once."
  (remove-hook 'post-command-hook #'skroad--quote-restore-point t)
  (when (and (eq (char-before) ?\n)
             (eq (char-after) ?>))
    (skip-chars-forward ">")))

(defun skroad--quote-after-change-hook (beg end length)
  "Sets up deferred rectification of line insertions and deletions in quotes."
  (unless undo-in-progress
    ;; Added text (pure insertion OR the insert half of a replacement) :
    (when (> end beg)
      (let ((depth (or (get-text-property end 'quote-depth) ;; From font lock
                       (and (= end (point-max))
                            (> beg (point-min))
                            (get-text-property (1- beg) 'quote-depth)))))
        (when (and depth (> depth 0))
          (let ((repl (concat "\n" (make-string depth ?>))))
            (save-mark-and-excursion
              (goto-char beg)
              (while (search-forward "\n" end t)
                (skroad--deferred-replace (1- (point)) (point) repl))))
          (add-hook 'post-command-hook #'skroad--quote-restore-point t t))))
    ;; Removed text (pure deletion OR the delete half of a replacement) :
    (when (and (> length 0)
               (eq (char-after beg) ?>)
               (> beg (point-min))
               (save-excursion
                 (goto-char beg)
                 (skip-chars-backward "> \t")
                 (not (bolp))))
      (skroad--deferred-replace
       beg
       (save-mark-and-excursion
         (goto-char beg)
         (skip-chars-forward "> \t")
         (point))
       ""))))

(defun skroad--quote-region (beg end)
  "Increment the quote level of the region BEG...END."
  (save-mark-and-excursion
    (let ((end-marker (copy-marker end t)))
      (replace-regexp-in-region (rx line-start) ">" beg end-marker)
      (replace-regexp-in-region
       (rx line-start ">" (zero-or-more blank) line-end) "" beg end-marker))))

(defun skroad--unquote-region (beg end)
  "Decrement the quote level of the region BEG...END."
  (save-mark-and-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-marker)
        (when (looking-at (rx (group (zero-or-more blank) ">")))
          (delete-region (match-beginning 1) (match-end 1)))
        (forward-line 1)))))

;; Decorative text types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--regexp-padded-any (rx (* blank) (+ print) (* blank))
  "Regexp for any padded text.")

(skroad--deftype skroad--text-mixin-render-delimited-decorative
  :doc "Mixin for decorative delimited text types rendered by font-lock."
  :mixin t
  :payload-regex skroad--regexp-padded-any
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
  "If NODE is in the cache, mark it as unindexed.  Otherwise, do nothing."
  (when (skroad--cache-peek node) (skroad--cache-intern-unindexed node)))

(defun skroad--cache-evict (node)
  "Evict NODE from the cache.  Do this only when deleting or renaming the NODE."
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

(defun skroad--cache-count ()
  "Return the number of nodes currently in the cache."
  (hash-table-count (skroad--cache)))

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

(defun skroad--indices-update (indices changes &optional no-actions init-scan)
  "Apply a set of pending CHANGES to INDICES.  Return the updated INDICES.
The tables in CHANGES are emptied out after being applied to the INDICES.
Also execute the following text type actions (unless NO-ACTIONS) :
`on-create': a particular payload of this type first appeared in the buffer.
`on-init': same as above, but during initial scan (INIT-SCAN is t).
`on-destroy': a particular payload of this type no longer appears."
  (let ((origin (skroad--current-node))
        (changes-ordered
         (sort changes
               #'(lambda (a b)
                   (<= (get (car a) 'order) (get (car b) 'order))))))
    (dolist (change changes-ordered)
      (let ((text-type (car change)) (type-changes (cdr change)))
        (when (or init-scan (not (skroad--hash-empty-p type-changes)))
          (let* ((type-index (skroad--ensure-index indices text-type))
                 (create-action (if init-scan 'on-init 'on-create)))
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
            (when (skroad--hash-empty-p type-index) ;; Don't store empty indices
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

(defvar skroad--lint-in-progress nil
  "Indicates that lint is currently in progress.")

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
    (unless (skroad--node-disable-index-p)
      (skroad--buf-indices-ensure-change-tracker)) ;; Init tracker if not yet
    (when init
      (unless (skroad--node-disable-index-p)
        (when have-changes
          (message "Tried to apply changes to unindexed node: '%s', rescanning!"
                   (skroad--current-node))
          (setq-local skroad--buf-indices-pending nil))
        (skroad--scan-region (point-min) (point-max) 1))
      (setq have-changes t)
      (setq indices nil)
      (skroad--clear-buf-undo-info) ;; May have rectified links, so zap undo;
      (skroad--save-current-node)) ;; ... and save the node.
    (when have-changes
      (setq indices (skroad--indices-update
                     indices skroad--buf-indices-pending
                     (or no-actions (skroad--node-special-p)) init))
      (setq-local skroad--buf-indices-table indices)
      (skroad--cache-write (skroad--current-node) indices)
      (skroad--current-node-update-orphan-status))))

(defvar-local skroad--scan-in-progress nil
  "When true, indicates that scan is currently in progress.")

(defvar-local skroad--change-hook-in-progress nil
  "When true, indicates that we are inside of a change hook.")

(defvar skroad--text-types-indexed nil "Text types subject to indexing.")

(defvar skroad--text-types-indexed-sorted nil
  "Indexed text types, sorted by priority.  Do not access directly.")

(defun skroad--get-indexed-text-types ()
  "Get the indexed text types sorted by priority."
  (or skroad--text-types-indexed-sorted
      (setq skroad--text-types-indexed-sorted
            (sort skroad--text-types-indexed
                  #'(lambda (a b) (<= (get a 'order) (get b 'order)))))))

(skroad--deftype skroad--text-mixin-indexed
  :doc "Mixin for indexed text types."
  :mixin t
  :require '(for-all-in-region get-match swap-match validate)
  :defaults '((index-filter t))
  :scan-region
  '(lambda (start end delta)
     (let ((pending-index
            (skroad--ensure-index skroad--buf-indices-pending type-name))
           (skroad--scan-in-progress t))
       (funcall
        for-all-in-region start end
        #'(lambda ()
            (let* ((raw-match (funcall get-match))
                   (payload (funcall get-unescaped-payload))
                   (escaped-payload (funcall escape payload)))
              (when (and (funcall validate payload)
                         (or (not (functionp index-filter))
                             (funcall index-filter payload)))
                (skroad--index-delta pending-index payload delta))
              ;; Rectify, if not removing, undoing, or already canonical:
              (when (and (= delta 1) (not undo-in-progress)
                         (not (string-equal raw-match escaped-payload)))
                (if skroad--change-hook-in-progress ;; Must defer if in chg hook
                    (skroad--deferred-replace
                     (match-beginning match-number)
                     (match-end match-number)
                     escaped-payload)
                  (let ((inhibit-read-only t) ;; ... else, do it right here:
                        (inhibit-modification-hooks t))
                    (funcall swap-match escaped-payload t)))))))))
  :register 'skroad--text-types-indexed)

(defun skroad--scan-region (start end delta)
  "Find indexable items in the current buffer between START and END;
Apply count DELTA to each item found.  `skroad--buf-indices-sync' must be
called to finalize all pending changes when no further ones are expected."
  (skroad--with-whole-lines start end
    (save-match-data
      (save-mark-and-excursion
        (goto-char start-expanded)
        (skroad--skip-whitespace-forward end-expanded)
        (let ((start-trimmed (point)))
          (when (< start-trimmed end-expanded)
            (dolist (text-type (skroad--get-indexed-text-types))
              (funcall (get text-type 'scan-region)
                       start-trimmed end-expanded delta))))))))

(defvar-local skroad--expecting-after-change-hook nil
  "Detect mismatched change hook activations.
These may occur if ill-behaved minor modes are in use.")

(defun skroad--verify-change-hook-pairing (expected)
  "Alarm if `skroad--expecting-after-change-hook' is not equal to EXPECTED."
  (unless (eq skroad--expecting-after-change-hook expected)
    (message "Missed %s-change hook in '%s' ! (Check for buggy minor modes.)"
             (if expected "before" "after") (skroad--current-node)))
  (setq-local skroad--expecting-after-change-hook (not expected)))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in the buffer in region START...END."
  (let ((skroad--change-hook-in-progress t))
    (skroad--verify-change-hook-pairing nil)
    (skroad--scan-region start end -1)))

(defun skroad--after-change-function (start end _length)
  "Triggers following a change in the buffer in region START...END."
  (let ((skroad--change-hook-in-progress t))
    (skroad--verify-change-hook-pairing t)
    (skroad--scan-region start end 1)))

(defvar-local skroad--buf-indices-change-tracker-installed nil
  "Indicates that the change tracker has been installed in this buffer.")

(defun skroad--buf-indices-ensure-change-tracker ()
  "Install the skroad change hooks in the current buffer."
  (unless skroad--buf-indices-change-tracker-installed
    (add-hook 'before-change-functions 'skroad--before-change-function nil t)
    (add-hook 'after-change-functions 'skroad--after-change-function nil t)
    (setq-local skroad--buf-indices-change-tracker-installed t)))

(defun skroad--indices-has-p (text-type payload indices)
  "Test whether a PAYLOAD of TEXT-TYPE exists in INDICES.
If there are any, return the count; otherwise return nil."
  (let* ((index (alist-get text-type indices))
         (count (or (and index (gethash payload index)) 0)))
    (and (> count 0) count)))

(defun skroad--current-indices-have-p (text-type payload)
  "Test whether a PAYLOAD of TEXT-TYPE exists in the current payload's indices.
If there are any, return the count; otherwise return nil."
  (skroad--indices-has-p text-type payload (skroad--buf-indices)))

(defun skroad--current-indices-foreach (text-type fn &rest other-args)
  "Apply FN to all payloads of TEXT-TYPE in the current payload's indices."
  (let ((index (alist-get text-type (skroad--buf-indices))))
    (when index
      (maphash #'(lambda (key _val) (apply fn (cons key other-args))) index))))

(defun skroad--current-indices-any-p (text-type pred)
  "Determine whether PRED is true on some indexed payload of TEXT-TYPE."
  (when-let* ((index (alist-get text-type (skroad--buf-indices))))
    (catch 'found
      (maphash (lambda (k _v)
                 (when (funcall pred k)
                   (throw 'found k)))
               index))))

(defun skroad--current-indices-live-link-count ()
  "Return the number of live links in the current node's indices."
  (let ((index
         (alist-get 'skroad--text-link-node-live (skroad--buf-indices))))
    (or (and (hash-table-p index) (hash-table-count index)) 0)))

;;; Atomic Text Type. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert a space immediately behind the atomic currently under the point.
(defun skroad--cmd-atomic-prepend-space ()
  "InsSp"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (save-mark-and-excursion (goto-char (skroad--zone-start)) (insert " ")))

(defvar-local skroad--buf-alt-mark nil
  "The opposite end of an atomic zone in which the regular mark had been set.")

(defun skroad--deactivate-mark ()
  "Deactivate the mark and clear the alt-mark."
  (deactivate-mark) (setq-local skroad--buf-alt-mark nil))

(defmacro skroad--define-atomics-region-cmd (wrap-command &optional i-arg)
  "Wrap COMMAND to use region if exists, or use the atomic at point as region."
  `(defun ,(read (concat "skroad--cmd-atomic-"
                         (symbol-name wrap-command))) ()
     (interactive ,i-arg skroad-mode skroad-search-results-mode)
     (skroad--modes-only)
     (if (use-region-p)
         (call-interactively ',wrap-command)
       (skroad--with-current-zone
         (funcall #',wrap-command start end)))
     (skroad--deactivate-mark)))

;; TODO: don't sync link disconnects until obvious that we won't yank them back
(skroad--define-atomics-region-cmd delete-region "*")
(skroad--define-atomics-region-cmd kill-region "*")
(skroad--define-atomics-region-cmd kill-ring-save)

(defun skroad--cmd-atomic-set-mark ()
  "Set the mark inside an atomic."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (save-excursion
    (skroad--with-current-zone
      (setq-local skroad--buf-alt-mark start)
      (goto-char end)
      (call-interactively 'set-mark-command))))

(skroad--deftype skroad--text-atomic
  :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
  :atomic t ;; TODO: check
  :keymap
  (skroad--define-keymap
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

;; Interactive node renamer. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--renamer nil "Node renamer overlay.  There can be only one.")

(defun skroad--renamer-active-p ()
  "Return t when the renamer is active."
  (overlayp skroad--renamer))

(defun skroad--point-in-renamer-p ()
  "Return t when the point is in the renamer (presumed to be active)."
  (memq skroad--renamer (overlays-at (point))))

(defun skroad--renamer-go-to-text-start ()
  "Jump to the start of the text in the renamer (presumed to be active)."
  (goto-char (overlay-start skroad--renamer))
  (unless (string-empty-p (skroad--get-renamer-text))
    (skroad--skip-whitespace-forward)))

;; Try to activate the renamer in the current zone.
(defun skroad--cmd-renamer-activate-here ()
  "Rename"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--renamer-deactivate) ;; Deactivate when already active somewhere
  (let ((renamer-type (skroad--prop-at 'renamer-overlay-type)))
    (when renamer-type
      (cond
       ((skroad--idle-have-work-p)
        (user-error "Please wait until queued work completes!"))
       (t
        (skroad--with-current-zone
          (let ((old-name (funcall (get renamer-type 'name-rename) start)))
            (when (skroad--fn-or-t (get renamer-type 'permit-rename) old-name)
              (setq buffer-read-only nil)
              (skroad--suspend-font-lock)
              (skroad--deactivate-mark)
              (setq-local inhibit-modification-hooks t)
              (skroad--selector-deactivate)
              (let ((hider (make-overlay start end (current-buffer) t nil))
                    (snapshot (prepare-change-group)))
                (activate-change-group snapshot)
                (overlay-put hider 'invisible t)
                (goto-char start)
                (let ((inhibit-read-only t)
                      (prefix (or (get renamer-type 'prefix) ""))
                      (suffix (or (get renamer-type 'suffix) "")))
                  (insert (concat prefix old-name suffix)))
                (setq skroad--renamer
                      (make-overlay start (point) (current-buffer)))
                (overlay-put skroad--renamer 'category renamer-type)
                (overlay-put skroad--renamer 'old-name old-name)
                (overlay-put skroad--renamer 'hider hider)
                (overlay-put skroad--renamer 'snapshot snapshot))
              (set-buffer-modified-p nil)
              (skroad--renamer-go-to-text-start)
              (add-hook 'post-command-hook #'skroad--renamer-validate)))))))))

(defun skroad--cmd-direct-renamer-activate-here ()
  "Rename"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (call-interactively #'skroad--cmd-renamer-activate-here))

(defun skroad--renamer-deactivate ()
  "Deactivate the renamer if it is currently active."
  (when (skroad--renamer-active-p)
    (let ((renamer-buffer (overlay-buffer skroad--renamer)))
      (when (buffer-live-p renamer-buffer)
        (with-current-buffer renamer-buffer
          (skroad--deactivate-mark)
          (when-let* ((snapshot (overlay-get skroad--renamer 'snapshot)))
            (undo-amalgamate-change-group snapshot)
            (cancel-change-group snapshot))
          (delete-overlay (overlay-get skroad--renamer 'hider))
          (delete-overlay skroad--renamer)
          (skroad--resume-font-lock)
          (setq-local inhibit-modification-hooks nil)
          (skroad--set-writability)
          (skroad--selector-update))))
    (remove-hook 'post-command-hook #'skroad--renamer-validate)
    (setq skroad--renamer nil)))

(defun skroad--get-renamer-text ()
  "Get the proposed text in the current renamer."
  (skroad--clean-whitespace
   (field-string-no-properties (overlay-start skroad--renamer))))

(defun skroad--renamer-get-default-face ()
  "Get the default face of the current renamer."
  (get (overlay-get skroad--renamer 'category) 'face))

(defun skroad--renamer-validate ()
  "If the cursor is inside the renamer, validate it and return the result.
If the cursor has left the renamer or the buffer in which it was active,
disable the renamer and return nil."
  (when (skroad--renamer-active-p)
    (if (and (eq (current-buffer) (overlay-buffer skroad--renamer))
             (or (skroad--point-in-renamer-p)
                 (and mark-active
                      (goto-char (1- (overlay-end skroad--renamer))))))
        (let ((valid
               (skroad--fn-or-t
                (overlay-get skroad--renamer 'validate-rename)
                (overlay-get skroad--renamer 'old-name)
                (skroad--get-renamer-text))))
          (overlay-put
           skroad--renamer 'face
           (if valid
               (skroad--renamer-get-default-face)
             `(:inherit ,(skroad--renamer-get-default-face)
                        :background ,skroad--renamer-faces-invalid-background)))
          (when (buffer-modified-p)
            (skroad--deactivate-mark)
            (set-buffer-modified-p nil))
          valid)
      (skroad--renamer-deactivate)
      nil)))

(defun skroad--cmd-renamer-accept-changes ()
  "Accept the proposed renaming, if the renamer is currently active and valid."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (when (skroad--renamer-validate)
    (let ((old (overlay-get skroad--renamer 'old-name))
          (new (skroad--get-renamer-text))
          (do-rename (overlay-get skroad--renamer 'do-rename)))
      (skroad--renamer-deactivate)
      (unless (string-equal old new)
        (funcall do-rename old new)))))

(defmacro skroad--keymap-with-global-fallthrough (form)
  "Wrap a `define-keymap' FORM for use as a field keymap in a special buffer.
Inherits `global-map' and shadows the `self-insert-command'
suppression that `suppress-keymap' installs via command remapping."
  `(,(car form)
    :parent global-map
    "<remap> <self-insert-command>" #'self-insert-command
    ,@(cdr form)))

(skroad--deftype skroad--text-mixin-renamer-overlay
  :doc "Base mixin for renamer overlays."
  :mixin t
  :rear-advance t
  :field 'zone
  :keymap
  (skroad--keymap-with-global-fallthrough
   (skroad--define-keymap
     "<remap> <beginning-of-line>" ;; HOME jumps to the start of the text
     #'(lambda () (interactive) (skroad--renamer-go-to-text-start))
     "<remap> <end-of-line>" ;; END jumps to the end of the text
     #'(lambda () (interactive)
         (goto-char (1- (field-end)))
         (skroad--skip-whitespace-backward (field-beginning)))
     "<backspace>" ;; backspace must not change preceding text
     #'(lambda () (interactive)
         (cond ((use-region-p)
                (delete-region (region-beginning) (region-end)))
               ((> (point) (field-beginning))
                (delete-char -1))))
     "<return>" #'skroad--cmd-renamer-accept-changes
     "<remap> <keyboard-quit>"
     #'(lambda () (interactive)
         (if mark-active
             (skroad--deactivate-mark)
           (skroad--renamer-deactivate))))))

(skroad--deftype skroad--text-mixin-renameable-indirect
  :doc "Mixin for allowing the use of the rename command with an atomic type."
  :mixin t
  :require 'renamer-overlay-type
  :keymap (skroad--define-keymap
            "r" #'skroad--cmd-renamer-activate-here))

(skroad--deftype skroad--text-mixin-renameable-direct
  :doc "Mixin for allowing the use of the rename command with an atomic type."
  :mixin t
  :require 'renamer-overlay-type
  :keymap (skroad--define-keymap
            "r" #'skroad--cmd-direct-renamer-activate-here))

(defun skroad--node-renamer-permit (current)
  "Determine whether a node titled CURRENT is renameable."
  (cond ((skroad--node-special-p current)
         (user-error "A special node cannot be renamed!")
         nil)
        ((skroad--node-log-p current)
         (user-error "A log node cannot be renamed!")
         nil)
        (t t)))

(defun skroad--node-renamer-validate (current proposed)
  "Determine whether a node titled CURRENT may be renamed to PROPOSED."
  (cond ((not (skroad--validate-node-title-for-rename proposed))
         (skroad--info "'%s' is not a valid node title!" proposed)
         nil)
        ((string-equal proposed current)
         (skroad--info "No change proposed")
         t)
        ((skroad--cache-peek proposed)
         (skroad--info "'%s' is an existing node!" proposed)
         nil)
        ((skroad--node-log-p proposed)
         (skroad--info
          (format "The prefix '%s' is reserved for auto-created log nodes!"
                  skroad--log-node-prefix))
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
  :name-rename #'skroad--data-at
  :validate-rename #'skroad--node-renamer-validate
  :do-rename #'skroad--node-renamer-do-rename)

(skroad--deftype skroad--text-node-renamer-direct
  :doc "Renamer for editing a node's title directly."
  :use 'skroad--text-mixin-renamer-overlay
  :use 'skroad--text-mixin-node-renamer
  :face 'skroad--direct-renamer-face
  :suffix "\n")

(skroad--deftype skroad--text-node-renamer-indirect
  :doc "Renamer for editing a node's title while standing on a link to the node."
  :use 'skroad--text-mixin-renamer-overlay
  :use 'skroad--text-mixin-node-renamer
  :face 'skroad--indirect-renamer-face
  :suffix " ")

(defun skroad--url-renamer-name (pos)
  "Obtain the current caption (if any) of the URL at POS."
  (save-mark-and-excursion
    (goto-char pos)
    (if (eq (skroad--prop-at 'category) 'skroad-text-md-url-link)
        (skroad--with-current-zone
          (skroad--visible-buffer-substring start end))
      " ")))

(defun skroad--url-renamer-validate (_current proposed)
  "Determine whether a URL may be recaptioned to PROPOSED."
  (cond ((string-empty-p proposed)
         (skroad--info "URL caption may not be empty!")
         nil)
        (t t)))

(defun skroad--url-recaption (_old new)
  "Recaption the URL at point from OLD to NEW."
  (let ((url (skroad--data-at)))
    (when url
      (skroad--with-current-zone
        (delete-region start end)
        (insert (skroad--md-make-url url new))
        (goto-char start)))))

(skroad--deftype skroad--text-url-renamer
  :doc "Renamer for recaptioning a URL link."
  :use 'skroad--text-mixin-renamer-overlay
  :permit-rename '(lambda (current) t)
  :name-rename #'skroad--url-renamer-name
  :validate-rename #'skroad--url-renamer-validate
  :do-rename #'skroad--url-recaption
  :face 'skroad--indirect-renamer-face
  :suffix " ")

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
       (skroad--prop-at 'category pos) 'on-activate (skroad--data-at pos)))))

(defun skroad--mouse-warp-to-pos (pos)
  "Move the mouse to the geometric center of the character at POS."
  (redisplay t)
  (let ((posn (posn-at-point pos)))
    (when posn
      (let* ((edges (window-edges nil t nil t))  ; body edges, pixelwise
             (pos-xy (posn-x-y posn))
             (obj (posn-object-width-height posn))
             (char-w (if (zerop (car obj)) (frame-char-width) (car obj)))
             (char-h (if (zerop (cdr obj)) (line-pixel-height) (cdr obj)))
             (x (+ (nth 0 edges) (car pos-xy) (/ char-w 2)))
             (y (+ (nth 1 edges) (cdr pos-xy) (/ char-h 2))))
        (set-mouse-pixel-position (selected-frame) x y)))))

(defun skroad--mouse-warp-to-current ()
  "Warp mouse to the middle of the current zone, if possible; else, to point."
  (let ((warp-pos
         (if (and (skroad--fontify-current-line) ;; We're in-mode
                  (skroad--prop-at 'zone)) ;; There's a zone at point
             (skroad--with-current-visible-zone
               (if (< vis-start vis-end) ;; Valid zone?
                   (+ vis-start (/ (- vis-end vis-start) 2)) ;; Middle of zone
                 (point))) ;; If no valid zone: use point.
           (point))))
    (skroad--mouse-warp-to-pos warp-pos))) ;; Warp the mouse.

(defun skroad--cmd-link-mouse-activate (click &optional display-mode)
  "Perform the action attribute of the link that got the CLICK.
DISPLAY-MODE is passed to `skroad--do-link-action'."
  (interactive "e" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let* ((posn (event-start click))
         (click-pos (posn-point posn))
         (window (posn-window posn))
         (frame (window-frame window)))
    (unless (eq frame (selected-frame))
      (select-frame-set-input-focus frame))
    (unless (eq window (selected-window))
      (select-window window))
    (goto-char (skroad--zone-start click-pos))
    (skroad--selector-update) ;; Make sure we update before leaving a node
    (skroad--save-cache-point)
    (let ((old-buf (current-buffer)))
      (skroad--do-link-action click-pos display-mode)
      (unless (eq old-buf (current-buffer)) ;; Don't warp if remained in buffer
        (skroad--mouse-warp-to-current)))))

(defun skroad--cmd-link-mouse-activate-new-win (click)
  "Activate a link via the mouse, opening any buffers in a new window."
  (interactive "e" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--cmd-link-mouse-activate click skroad--disp-mode-force-new-window))

;; Transform the item under the point to plain text by removing delimiters.
(defun skroad--cmd-atomic-delimited-textify ()
  "Text"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--with-current-zone
    (let ((text (skroad--data-at)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

(skroad--deftype skroad--text-mixin-atomic-delimited
  :doc "Mixin denoting an atomic delimited text type."
  :mixin t
  :require 'atomic
  :use 'skroad--text-mixin-delimited
  :use 'skroad--text-mixin-rendered-zoned)

;; Perform the action attribute of the link at point.
(defun skroad--cmd-link-activate ()
  "Go"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--do-link-action (point)))

(defun skroad--cmd-link-activate-new-win ()
  "Win."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--do-link-action (point) skroad--disp-mode-force-new-window))

(skroad--deftype skroad--text-mixin-link-navigable
  :doc "Mixin denoting a navigable link."
  :mixin t
  :keymap (skroad--define-keymap
            "<mouse-1>" #'skroad--cmd-link-mouse-activate
            "<mouse-2>" #'skroad--cmd-link-mouse-activate-new-win
            "<return>" #'skroad--cmd-link-activate
            "M-<return>" #'skroad--cmd-link-activate-new-win))

(defun skroad--mouseover-node-preview (_window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION.  Preview body."
  (with-current-buffer buf
    (let ((node (skroad--data-at position)))
      (when (stringp node)
        (cond ((skroad--node-stub-p node) (format "Stub node '%s'" node))
              ((skroad--node-special-p node) (format "Special node '%s'" node))
              ((skroad--node-self-p node) "This node!")
              ((skroad--cache-peek node)
               (propertize
                (or (ignore-errors
                      (skroad--with-file (skroad--node-path node)
                        (skroad--current-node-extract-body)))
                    "This node is missing from the data directory !?")
                'help-echo-inhibit-substitution t)) ;; Emacs 29+
              (t "A deleted node."))))))

(defun skroad--link-escaper (payload)
  "Escape PAYLOAD for links."
  (skroad--bracket-escape payload))

(defun skroad--link-unescaper (payload)
  "Unescape PAYLOAD for links."
  (skroad--bracket-unescape payload))

;; TODO: [[test\[123\]] matches early if inserted manually
(defconst skroad--regexp-text-in-brackets
  (rx (* blank)
      (+ (or (: ?\\ not-newline)
             (not (any "[]" blank ?\n))))
      (*? (or (: ?\\ not-newline)
              (not (any "[]\n")))))
  "Regexp matching text that could be delimited by square brackets.")

(defun skroad--link-validate (payload)
  "Validate a proposed PAYLOAD (unescaped) representing a node link.
If an invalid link was seen during indexing, report it to the lint."
  (let ((valid
         (or (skroad--cache-peek payload) ;; Already in the cache? valid!
             (skroad--validate-node-title payload)))) ;; or actually validate.
    (when (and (not valid) skroad--scan-in-progress)
      (skroad--lint-report (format "Invalid link: '%s'" payload)))
    valid))

(skroad--deftype skroad--text-link-node
  :doc "Fundamental type for skroad node links (live or dead)."
  :use 'skroad--text-atomic
  :mouse-face 'skroad--highlight-link-face
  :help-echo 'skroad--mouseover-node-preview
  :payload-regex skroad--regexp-text-in-brackets
  :visible-match-number 1
  :hide-escapes t
  :escape #'skroad--link-escaper
  :unescape #'skroad--link-unescaper
  :validator #'skroad--link-validate
  :index-filter ;; Do not index self-links or links to special nodes
  '(lambda (node)
     (not (or (skroad--node-self-p node)
              (skroad--node-special-p node)))))

(defun skroad--foreground-node (node)
  "If NODE is visible somewhere, go there; otherwise open in the current window.
If NODE does not exist, this is a no-op.  On success, return t."
  (if (and node (skroad--cache-peek node))
    (let* ((node-path (skroad--node-path node)) ;; Target path
           (node-buf (find-buffer-visiting node-path))) ;; buf (maybe nil)
      (if node-buf ;; If node is already open in a buffer, use that buffer:
          (let* ((node-win (get-buffer-window node-buf t))
                 (node-frame (window-frame node-win)))
            (if node-win ;; If it already has a visible window, go there
                (progn
                  (unless (eq (window-frame) node-frame) ;; In different frame?
                    (select-frame-set-input-focus node-frame)) ;; ... focus it.
                  (select-window node-win))
              (switch-to-buffer node-buf))) ;; ... unbury in current window.
        (pop-to-buffer (find-file-noselect node-path))) ;; ... or open it.
      t) ;; Return t when displayed.
    (user-error "Node '%s' does not exist!" node) ;; If it did not exist
    nil))

(defun skroad--show-existing-node (node)
  "Navigate to NODE, if it exists.  If called from a node buffer, may close it."
  (let* ((from-node (and (skroad--mode-p) (skroad--current-node))) ;; origin
         (from-buf (current-buffer)))
    (when (skroad--foreground-node node) ;; Display the node, if exists
      (let ((restored-point (skroad--maybe-restore-cached-point)))
        (unless (or (null from-node) ;; Did we come from minibuffer or search?
                    (skroad--node-special-p from-node)) ;; ... from a special?
          ;; TODO: this should be configurable?
          (unless restored-point ;; If we don't have a cached pos:
            (skroad--link-maybe-jump-to-live from-node)) ;; Find link to origin
          (unless (or (get-buffer-window from-buf t) ;; Origin buf not buried?
                      (skroad--node-special-p node)) ;; ... or opened a special?
            (with-current-buffer from-buf ;; Sync and close the buried node:
              (skroad--buf-indices-sync)
              (skroad--save-current-node)
              (kill-buffer)))))
      t))) ;; Return t when displayed.

(defun skroad--show-node (node &optional no-create)
  "Navigate to NODE.
Unless NO-CREATE is true, the node will be created if it does not yet exist.
Must be called from a buffer containing a node."
  (unless (or (skroad--cache-peek node) ;; May already have been created
              no-create) ;; Creation may be disabled
    (skroad--complete-all-deferred) ;; ... if so, let all deferred work finish.
    (unless (skroad--cache-peek node) ;; Suppose the node still doesn't exist?
      (skroad--in-node ;; Force immediate creation.
       node #'skroad--connect-to (skroad--current-node))))
  (skroad--show-existing-node node))

(defun skroad--action-live-link-activate (node)
  "Activate a live link to NODE.
If we are coming from a search result buffer or a special, never create NODE.
In the case of a search result, activate isearch with the query string if NODE
actually loaded."
  (let ((from-search (skroad--get-search-status))
        (from-res (get-text-property (point) 'skroad-search-result)))
    (when (skroad--show-node
           node
           (or (skroad--node-special-p) ;; Don't create from specials
               from-search)) ;; ... or from search res
      (when from-search
        (if from-res
            (skroad--maybe-restore-cached-point)
          (goto-char (point-min))
          (isearch-mode t) ;; Forward search
          (isearch-yank-string from-search))))))

;; TODO
;; (defun skroad--verify-dead-link (node)
;;   "A dead link to NODE must be revived if there is a live reciprocal link."
;;   (when (and (skroad--cache-peek node) ;; Target node exists, and:
;;              (skroad--connected-p node)) ;; and has a live link to current node.
;;     (skroad--link-revive node)
;;     (skroad--lint-report (format "Auto-revived dead link to '%s'" node))))

;; TODO: log entry
;; TODO: check if we have an unreachable that can be renamed and will correspond
(defun skroad--action-live-link-init (origin target)
  "A live link to TARGET was found to exist during the indexing of ORIGIN."
  (message "live init: origin=%s target=%s" origin target)
  (when (skroad--cache-peek origin) ;; Check that origin still exists
    (unless (and (skroad--cache-peek target) ;; Target doesn't exist?
                 (skroad--connected-p target origin)) ;; ... or has no backlink?
      (skroad--in-node origin #'skroad--disconnect-from target) ;; Disconnect
      (message "Non-reciprocal link in '%s' to '%s' disabled." origin target)))
    )

(defun skroad--action-live-link-create (origin target)
  "A live link to TARGET was first introduced into an already-indexed ORIGIN.
TARGET will be created if it does not exist."
  (message "connected: origin=%s target=%s" origin target)
  (when (skroad--cache-peek origin) ;; Check that origin still exists
    (skroad--in-node target #'skroad--connect-to origin))) ;; Connect in target.

(defun skroad--action-live-link-destroy (origin target)
  "The last instance of a live link to TARGET was removed from ORIGIN.
If TARGET does not exist, this is a no-op."
  (message "disconnected: origin=%s target=%s" origin target)
  (when (skroad--cache-peek target) ;; Check that target still exists
    (skroad--in-node target #'skroad--disconnect-from origin))) ;; Disconnect.

;; (defun skroad--action-check-dead-link (origin target)
;;   "Revive, if necessary, a dead link found in ORIGIN to TARGET."
;;   ;; (message "dead init: origin=%s target=%s" origin target)
;;   (when (and (skroad--cache-peek origin) (skroad--cache-peek target))
;;     (when (or (skroad--connected-p origin target)
;;               (and (skroad--connected-p target origin)
;;                    (message "lint")))
;;       (message "dead link in '%s' to '%s' should be revived!" origin target))
;;     )
;;   )

(defun skroad--action-dead-link-init (origin target)
  "A dead link to TARGET was found to exist during the indexing of ORIGIN."
  (message "dead init: origin=%s target=%s" origin target)
  (when (and (skroad--cache-peek origin) (skroad--cache-peek target)
             (skroad--connected-p target origin))
    (message "dead link in '%s' to '%s' should be revived!" origin target)
    )
  )

;; [-[zoolag]-]
;; [-[xyz]-]

(defun skroad--action-dead-link-create (origin target)
  "A dead link to TARGET was first introduced into an already-indexed ORIGIN."
  (message "dead create: origin=%s target=%s dist=%s" origin target
           skroad--at-a-distance)
  (when (and (skroad--cache-peek origin) (skroad--cache-peek target)
             (skroad--connected-p origin target))
    (message "new dead link in '%s' to '%s' should be revived!" origin target)
    )
  )

;; Yank (with optional ARGS) into a node when standing on a live link to it.
(defun skroad--cmd-teleyank-at (&rest args)
  "YankTo"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--yank-into (skroad--data-at) args))

(defun skroad--cmd-deaden-at (&rest _args)
  "Deaden"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--link-unlink (skroad--data-at)))

(defun skroad--cmd-merge-at (&rest _args)
  "Merge"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--merge-node-into-current (skroad--data-at)))

;; TODO: deleting tail links should zap any resulting empty line?
;; TODO: make this work for selections?
;; If live link is a duplicate, self-link, or special: delete it.
;; ... but if none of the above, move it to the node's tail.
(defun skroad--cmd-banish-at (&rest _args)
  "Banish"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--buf-indices-sync) ;; Make sure this node's indices are up to date
  (let* ((node (skroad--data-at)) ;; The live link being banished
         (single ;; Is this the last remaining copy of an indexed live link?
          (eq (skroad--link-has-live-p node) 1))) ;; nil for self/specials/dupes
    (if (and single (skroad--in-node-tail-p)) ;; No-op if a single in the tail
        (skroad--info "'%s' is already in the tail and has no duplicates!" node)
      (when single ;; About to delete a single from the body?
        (skroad--link-insert-live-in-tail node) ;; ... copy to the tail first;
        (skroad--info "'%s' is now in the tail." node))
      (delete-region (skroad--zone-start) (skroad--zone-end))))) ;; now delete.

(defconst skroad--link-node-live-start-delim "[["
  "Delimiter indicating the start of a live Skroad link.")

(defconst skroad--link-node-live-end-delim "]]"
  "Delimiter indicating the end of a live Skroad link.")

(skroad--deftype skroad--text-link-node-live
  :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
  :order 100
  :use 'skroad--text-link-node
  :on-init #'skroad--action-live-link-init
  :on-create #'skroad--action-live-link-create
  :on-destroy #'skroad--action-live-link-destroy
  :on-activate #'skroad--action-live-link-activate
  :face-function
  '(lambda (payload)
     (list
      (if (skroad--cache-peek payload)
          (cond ((skroad--node-self-p payload) 'skroad--self-link-face)
                ((skroad--node-special-p payload) 'skroad--special-link-face)
                ((skroad--node-log-p payload) 'skroad--log-link-face)
                ((skroad--node-stub-p payload) 'skroad--stub-link-face)
                (t 'skroad--live-link-face))
        'skroad--live-deleted-link-face)
      'skroad--node-link-decor-face))
  :begins skroad--link-node-live-start-delim
  :ends skroad--link-node-live-end-delim
  :keymap (skroad--define-keymap
            "t" #'skroad--cmd-atomic-delimited-textify
            "b" #'skroad--cmd-banish-at
            "m" #'skroad--cmd-merge-at
            "l" #'skroad--cmd-deaden-at
            "y" #'skroad--cmd-teleyank-at ;; Official teleyank trigger
            "<remap> <yank>" #'skroad--cmd-teleyank-at ;; Regular yank also
            )
  :renamer-overlay-type 'skroad--text-node-renamer-indirect
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-atomic-delimited
  :use 'skroad--text-mixin-renameable-indirect
  :use 'skroad--text-mixin-indexed)

;; TODO: do this in title def?
(defun skroad--link-valid-p (string)
  "Determine whether STRING represents a valid link payload."
  (funcall (get 'skroad--text-link-node-live 'regex-validate) string))

(defun skroad--link-maybe-jump-to-live (node)
  "Try to jump to the next live link to NODE after the point, if one exists."
  (when (funcall (get 'skroad--text-link-node-live 'search) node)
    (goto-char (match-beginning 0))))

(defun skroad--link-generate-live (node)
  "Generate a live link to NODE."
  (funcall (get 'skroad--text-link-node-live 'generate) node))

(defun skroad--link-insert-live (node)
  "Insert a live link to NODE at the current point."
  (insert (skroad--link-generate-live node)))

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

(defun skroad--cmd-liven-at (&rest _args)
  "Liven"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--link-revive (skroad--data-at)))

(skroad--deftype skroad--text-link-node-dead
  :doc "Dead (i.e. revivable placeholder) link to a skroad node."
  :order 101
  :use 'skroad--text-link-node
  :on-init #'skroad--action-dead-link-init
  :on-create #'skroad--action-dead-link-create
  :begins "[-[" :ends "]-]"
  :face-function
  '(lambda (payload)
     (list (if (skroad--cache-peek payload)
               'skroad--dead-link-face
             'skroad--dead-deleted-link-face)
           'skroad--node-link-decor-face))
  :keymap (skroad--define-keymap
            "t" #'skroad--cmd-atomic-delimited-textify
            "<return>" #'ignore
            "l" #'skroad--cmd-liven-at
            "<mouse-1>" #'skroad--cmd-link-mouse-activate) ;; Only move point
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-atomic-delimited
  :use 'skroad--text-mixin-indexed)

(defun skroad--link-has-dead-p (node)
  "Determine whether the current node has at least one dead link to NODE."
  (skroad--current-indices-have-p 'skroad--text-link-node-dead node))

(defun skroad--link-generate-dead (node)
  "Generate a dead link to NODE."
  (funcall (get 'skroad--text-link-node-dead 'generate) node))

(defun skroad--link-deaden (node &optional start end)
  "Deaden live links to NODE (optionally, in START...END) in the current node."
  (funcall (get 'skroad--text-link-node-live 'regen)
           node 'skroad--text-link-node-dead node start end))

(defun skroad--link-delete (node &optional start end)
  "Remove live links to NODE (optionally, in START...END) in the current node."
  (funcall (get 'skroad--text-link-node-live 'zap) node start end))

(defun skroad--link-delete-in-tail (node)
  "Delete all live links to NODE from the current node's tail."
  (skroad--link-delete node (skroad--node-tail-start-pos)))

(defun skroad--link-unlink (node)
  "Live links to NODE in the current node's body are deadened;
Any such links found in its tail are simply deleted."
  (skroad--link-deaden node nil (skroad--node-body-end-pos))
  (skroad--link-delete-in-tail node))

(defun skroad--link-revive (node)
  "Transform all dead links to NODE in the current node to live links."
  (funcall
   (get 'skroad--text-link-node-dead 'regen) node 'skroad--text-link-node-live))

(defun skroad--link-replace (old new &optional start end)
  "Replace live links to OLD in the current node with live links to NEW.
If START/END are given, constrain the replacement to that range."
  (funcall
   (get 'skroad--text-link-node-live 'regen)
   old 'skroad--text-link-node-live new start end))

(defun skroad--link-replace-in-body (old new)
  "Replace live links to OLD in the current node's body with live links to NEW."
  (skroad--link-replace old new nil (skroad--node-body-end-pos)))

(defun skroad--link-merge (victim target)
  "Merge live links to VICTIM in the current node into links to TARGET."
  (skroad--link-delete-in-tail victim) ;; Always remove victim from tail.
  (if (skroad--link-replace-in-body victim target) ;; Replaced in body?
      (skroad--link-delete-in-tail target) ;; ... delete target in tail;
    (skroad--connect-to target))) ;; ... if not, ensure a link to target.

(defun skroad--link-revive-to (node)
  "If the current node has at least one dead link to NODE, revive that link.
Return true if any such links were in fact revived."
  (and (skroad--link-has-dead-p node) ;; Any dead links to node?
       (skroad--link-revive node))) ;; Liven the dead links.

(defun skroad--connect-to (node)
  "Ensure that the current node has at least one live link to NODE.
If it had dead links to NODE, liven them; else, test for existing live links;
failing either of the above, emplace a new live link in the tail."
  (unless (skroad--node-self-p node) ;; May not connect to self
    (or (skroad--link-revive-to node) ;; Can we revive any dead links?
        (skroad--link-has-live-p node) ;; ... else, had live links already?
        (skroad--link-insert-live-in-tail node)))) ;; ... else, need a new one.

(defun skroad--disconnect-from (node &optional delete-all)
  "Ensure that the current node does NOT have any live links to NODE.
If DELETE-ALL is t, delete rather than deaden in the body as well as the tail."
  (when (skroad--link-has-live-p node)
    (if delete-all
        (skroad--link-delete node)
      (skroad--link-unlink node))))

(defun skroad--yank-into (node &rest yank-args)
  "Ensure that NODE exists, and yank into it.  YANK-ARGS are passed to yank."
  (if (or (skroad--node-special-p node) ;; Don't teleyank into special nodes
          (skroad--node-log-p node)) ;; ... or into log nodes
      (skroad--info (format "Cannot teleyank to node '%s' !" node))
    (skroad--with-node node nil ;; Yank could contain links, so actions must run
      (skroad--install-yank-transformer) ;; Ensure that transformer is present
      (atomic-change-group
        (goto-char (skroad--node-body-end-pos))
        (ensure-empty-lines)
        (apply #'yank yank-args)
        (insert "\n")))
    (skroad--log-node-revise node)
    (skroad--info (format "Teleyanked to node '%s'." node))))

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
  (rx-to-string `(in ,@skroad--unsafe-url-rx))
  "An enumeration of unsafe URL chars (see `browse-url-url-encode-chars').")

(defun skroad--url-strip-trailing-paren (url)
  "Remove trailing ) from URL if parens are unbalanced."
  (if (and (string-suffix-p ")" url)
           (< (seq-count (lambda (c) (= c ?\()) url)
              (seq-count (lambda (c) (= c ?\))) url)))
      (substring url 0 -1)
    url))

(defun skroad--sanitize-urls (text)
  "Percent-encode unsafe characters in URLs found in TEXT.
Already-encoded URLs are left untouched to avoid double-encoding."
  (replace-regexp-in-string
   skroad--regexp-url-any
   (lambda (url)
     (save-match-data
       (if (string-match-p (rx ?% (= 2 xdigit)) url)
           url
         (let ((trimmed (skroad--url-strip-trailing-paren url)))
           (concat
            (browse-url-url-encode-chars trimmed skroad--unsafe-url-chars)
            (substring url (length trimmed)))))))
   text nil t))

(defun skroad--browse-url (url)
  "Browse URL, respecting `display-buffer-overriding-action'."
  (let ((buf (save-window-excursion
               (browse-url url)
               (current-buffer))))
    (unless (eq buf (current-buffer))
      (pop-to-buffer buf))))

;; Turn the URL at point into plain text by placing a space after the prefix.
(defun skroad--cmd-bare-url-comment ()
  "Text"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--with-current-zone
    (save-mark-and-excursion
      (goto-char start)
      (when (search-forward "//" end)
        (insert " ")))))

(defun skroad--mouseover-show-url (_window buf position)
  "User is mousing over an external link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--data-at position)))

(skroad--deftype skroad-text-bare-url-link
  :doc "Bare URL."
  :use 'skroad--text-atomic
  :help-echo 'skroad--mouseover-show-url
  :face 'skroad--url-link-face
  :mouse-face 'skroad--highlight-link-face
  :match-number 0
  :regex-any skroad--regexp-url-encoded
  :on-activate #'skroad--browse-url
  :keymap (skroad--define-keymap
            "t" #'skroad--cmd-bare-url-comment)
  :finder-filter #'skroad--in-node-body-p
  :renamer-overlay-type 'skroad--text-url-renamer
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-renameable-indirect
  :use 'skroad--text-mixin-rendered-zoned
  )

(defconst skroad--md-url-regexp
  (rx ?\[
      (group (regexp skroad--regexp-text-in-brackets))
      ?\] ?\( ?\<
      (group (regexp skroad--regexp-url-encoded))
      ?\> ?\) )
  "Regexp matching Markdown-style URLs.")

(defconst skroad--md-unsafe-caption-chars '("[]" "\\"))

(defconst skroad--regexp-brackets
  (rx-to-string `(in ,@skroad--md-unsafe-caption-chars)))

(defconst skroad--regexp-escape-slash
  (rx-to-string `(: (group ?\\) (in ,@skroad--md-unsafe-caption-chars))))

(defconst skroad--regexp-maybe-escape-slash
  (rx-to-string `(: (? "\\") (group (in ,@skroad--md-unsafe-caption-chars)))))

;;; Caption escaping

(defun skroad--bracket-escape (text)
  "Backslash-escape characters in TEXT."
  (save-match-data
    (replace-regexp-in-string
     skroad--regexp-brackets
     (lambda (ch) (concat "\\" ch))
     text nil t)))

(defun skroad--bracket-unescape (text)
  "Remove backslash escapes from TEXT."
  (save-match-data
    (replace-regexp-in-string
     skroad--regexp-escape-slash
     (lambda (match) (substring match 1))
     text nil t)))

(defun skroad--isearch-search-fun ()
  "Return a search function that treats escape backslashes as optional."
  (lambda (string &optional bound noerror count)
    (let* ((re (regexp-quote string))
           (adjusted (replace-regexp-in-string
                      skroad--regexp-maybe-escape-slash
                      (lambda (match)
                        (concat "\\\\?" (regexp-quote (match-string 1 match))))
                      re nil t)))
      (re-search-forward adjusted bound noerror count))))

(defun skroad--hide-escape-slashes (from to)
  "Hide backslash escapes between FROM and TO."
  (save-match-data
    (save-mark-and-excursion
      (goto-char from)
      (while (re-search-forward skroad--regexp-escape-slash to t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'invisible t)))))

(defun skroad--md-make-url (url caption)
  "Return a Markdown-style URL with CAPTION."
  (format "[%s](<%s>)"
          (skroad--bracket-escape caption)
          (browse-url-url-encode-chars url skroad--unsafe-url-chars)))

;; Simply display the URL without navigating to it.
(defun skroad--cmd-link-show ()
  "Show"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--info (skroad--data-at)))

;; Turn the MD URL at point into plain text by breaking it with a space
(defun skroad--cmd-md-url-comment ()
  "Text"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--with-current-zone
    (save-mark-and-excursion
      (goto-char end)
      (when (search-backward "](<" start)
        (replace-match "] (<")))))

(skroad--deftype skroad-text-md-url-link
  :doc "Markdown-style URL."
  :use 'skroad--text-atomic
  :help-echo 'skroad--mouseover-show-url
  :face 'skroad--url-link-face
  :mouse-face 'skroad--highlight-link-face
  :order 200 ;; Render after bare URLs
  :match-number 2
  :visible-match-number 1
  :hide-escapes t
  :regex-any skroad--md-url-regexp
  :on-activate #'skroad--browse-url
  :keymap (skroad--define-keymap
            "t" #'skroad--cmd-md-url-comment
            "s" #'skroad--cmd-link-show)
  :finder-filter #'skroad--in-node-body-p
  :renamer-overlay-type 'skroad--text-url-renamer
  :use 'skroad--text-mixin-link-navigable
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-renameable-indirect
  :use 'skroad--text-mixin-rendered-zoned
  )

;; Text Search. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--search-current-buffer (string)
  "Return the text of each line in the current buffer matching STRING.
Matching is case-insensitive.  One entry per matching line, in buffer order."
  (let ((case-fold-search t)
        (search-upper-case nil)
        (matches nil))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward string nil t)
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              matches)
        (end-of-line)))
    (reverse matches)))

(defun skroad--search-highlight-matcher (string limit)
  "Find the next occurrence of STRING within a result line, up to LIMIT.
Searches only inside text marked `skroad-search-result'.  Sets the
match data and returns non-nil on success."
  (let ((case-fold-search t))
    (catch 'found
      (while (< (point) limit)
        (if (get-text-property (point) 'skroad-search-result)
            ;; Inside a result line: search bounded by its end.
            (let ((end (min limit
                            (next-single-property-change
                             (point) 'skroad-search-result nil limit))))
              (if (search-forward string end t)
                  (throw 'found t)
                (goto-char end)))
          ;; Outside: skip to the next result line.
          (goto-char (next-single-property-change
                      (point) 'skroad-search-result nil limit))))
      nil)))

(defun skroad--search-in-progress-p (buf)
  "Return non-nil if BUF is a live results buffer still being filled."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "(searching\\.\\.\\.)"
                              (line-end-position) t)))))

(defun skroad--search-insert-group (buf node matches)
  "Append MATCHES for NODE to results buffer BUF, if it still lives."
  (when (and (buffer-live-p buf) matches)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (skroad--link-generate-live node) " :\n")
          (dolist (text matches)
            (insert ">" (propertize text 'skroad-search-result t)
                    "\n"))
          (insert "\n"))))))

(defun skroad--search-finish (buf match-count match-node-count node-count)
  "Mark the search in BUF as complete, if it still lives.
Jump to the first result node link, if there are any."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (save-excursion
          (when (re-search-forward "(searching\\.\\.\\.)" nil t)
            (replace-match "" t t))
          (skroad--goto-node-body-start)
          (insert "\n")
          (skroad--atomic-comment-insert
           (format "%s results in %s nodes (of %s searched) :"
                   match-count match-node-count node-count))))
      (skroad--goto-node-body-start)
      (when (funcall (get 'skroad--text-link-node-live 'find-any-forward))
        (goto-char (match-beginning 0))
        (skroad--fontify-current-line)
        (run-with-idle-timer 0 nil #'skroad--selector-update)))))

(defvar-local skroad--search-query nil
  "Current query in a search result buffer.")

(defun skroad--get-search-status ()
  "If we're in a search results buffer, return the query string.  If not, nil."
  (and (skroad--search-results-p) skroad--search-query))

(defun skroad--search-render (string)
  "Begin a case-insensitive full-text search for STRING across all known nodes.
If a search for STRING is already in progress, do nothing except return its
buffer.  Otherwise create (or reuse and reset) the results buffer immediately
and return it; the per-file searches are deferred, and append their results,
grouped by node, as they complete.  Occurrences of STRING within the matched
lines are highlighted."
  (let* ((buf-name ;; Strings differing only in case share a buffer.
          (format "*skroad-search: %S*" (downcase string))) ;; Case-insensitive.
         (buf (get-buffer buf-name)))
    (if (skroad--search-in-progress-p buf)
        buf ;; If already searching for this string? no-op.
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (skroad-search-results-mode) ;; first: resets keywords/locals
          (setq-local revert-buffer-function ;; TODO: make one for regular mode?
                      #'(lambda (_ignore-auto _noconfirm)
                          (skroad--search-render string)))
          (font-lock-add-keywords
           nil
           (list (list #'(lambda (limit)
                           (skroad--search-highlight-matcher string limit))
                       0 ''skroad--search-match-face 'prepend))
           'append)
          (insert (format "Nodes containing %S: (searching...)\n\n" string))
          (setq-local skroad--search-query string))
        (skroad--goto-node-body-start))
      ;; Actually schedule the search:
      (let ((match-count 0)
            (match-node-count 0)
            (node-count (skroad--cache-count)))
        (skroad--info "Searching %s nodes..." node-count)
        (skroad--cache-foreach ;; Dispatch for each known node:
         #'(lambda (node)
             (skroad--defer ;; Defer each individual node search:
              (ignore-errors
                (when (skroad--cache-peek node) ;; Make sure it still exists!
                  (with-temp-buffer ;; Fastest possible load
                    (insert-file-contents
                     (skroad--node-path node) nil nil nil t)
                    (let ((matches (skroad--search-current-buffer string)))
                      (when matches
                        (setq match-node-count (1+ match-node-count))
                        (setq match-count (+ match-count (length matches)))
                        (skroad--search-insert-group buf node matches)))))))))
        (skroad--defer
         (skroad--search-finish
          buf match-count match-node-count node-count)))
      buf)))

;; Atomic Comments. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--deftype skroad--text-atomic-comment
  :doc "Atomic comment.
Used to indicate the effects of certain irreversible modifications to a node.
See e.g. `skroad--merge-node-into-current'."
  :use 'skroad--text-atomic
  :face 'skroad--atomic-comment-face
  :match-number 1
  :visible-match-number 1
  :payload-regex (rx (+ print) "\n")
  :begins "@Comment@" :ends ""
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-atomic-delimited)

(defun skroad--atomic-comment-insert (text)
  "Insert TEXT as an atomic comment at the current point."
  (insert (funcall (get 'skroad--text-atomic-comment 'generate) text) "\n"))

;; Timestamps. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: M-up/down should apply to all atomic types???

(defconst skroad--log-day-format "%d %B, %Y" "Format used for log dates.")
(defconst skroad--log-month-format "%B %Y" "Format used for log months.")
(defconst skroad--log-year-format "%Y" "Format used for log years.")

(defun skroad--make-date-string (format)
  "Generate a string representing the current date in the given FORMAT."
  (format-time-string format (seconds-to-time (current-time))))

(skroad--deftype skroad--text-timestamp
  :doc "Timestamp."
  :use 'skroad--text-atomic
  :face 'skroad--timestamp-face
  :match-number 1
  :visible-match-number 1
  :payload-regex (rx (+ print) "\n")
  :begins "@Time@" :ends ""
  :keymap (skroad--define-keymap
            "M-<up>" #'skroad--cmd-timestamp-jump-later
            "M-<down>" #'skroad--cmd-timestamp-jump-earlier
            )
  :finder-filter #'skroad--in-node-body-p
  :use 'skroad--text-mixin-atomic-delimited
  )

(defun skroad--cmd-timestamp-jump-earlier ()
  "Previous"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let ((pos (save-mark-and-excursion
               (forward-char)
               (skroad--timestamp-find-forward))))
    (when pos (goto-char pos))))

(defun skroad--cmd-timestamp-jump-later ()
  "Next"
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let ((pos (skroad--timestamp-find-backward)))
    (when pos (goto-char pos))))

(defun skroad--timestamp-find-forward ()
  "Return the position of the timestamp at or below the point; nil if none."
  (save-mark-and-excursion
    (when (funcall (get 'skroad--text-timestamp 'find-any-forward))
      (match-beginning 0))))

(defun skroad--timestamp-find-backward ()
  "Return the position of the timestamp above the point; nil if none."
  (save-mark-and-excursion
    (when (funcall (get 'skroad--text-timestamp 'find-any-backward))
      (match-beginning 0))))

(defun skroad--emplace-log-entry (text &optional unique)
  "Insert TEXT as a log entry in the current buffer, under the current date.
If UNIQUE is true, TEXT found to be a duplicate is simply moved to the end."
  (skroad--goto-node-body-start)
  (let ((now (skroad--make-date-string skroad--log-day-format)))
    (unless (funcall (get 'skroad--text-timestamp 'search) now)
      (insert "\n" (funcall (get 'skroad--text-timestamp 'generate) now) "\n")))
  (let ((day-bottom
         (copy-marker (or (skroad--timestamp-find-forward)
                          (skroad--node-body-end-pos)))) ;; Will create tail!
        (log-line (concat text "\n")))
    (when (and unique (search-forward log-line day-bottom t))
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char day-bottom)
    (skroad--skip-whitespace-backward)
    (forward-line)
    (insert log-line)))

(defun skroad--node-log-p (&optional node)
  "Return t when NODE (if not given: the current node) is a log node."
  (let ((n (or node (skroad--current-node))))
    (and (stringp n) (string-prefix-p skroad--log-node-prefix n))))

(defun skroad--make-log-node (title)
  "Create a log node from the given TITLE."
  (concat skroad--log-node-prefix title))

(defun skroad--log-earlier-p (a b)
  "Return non-nil if date string A represents an earlier date than B.
Unparseable strings are treated as earlier than parseable ones,
and equal to each other.  Never signals an error."
  (let ((da (ignore-errors (date-to-day a)))
        (db (ignore-errors (date-to-day b))))
    (cond
     ((and da db) (< da db))
     (db t)
     (t nil))))

(defun skroad--current-year-log-name ()
  "Generate the name of the current year log node."
  (skroad--make-log-node (skroad--make-date-string skroad--log-year-format)))

(defun skroad--current-log-name ()
  "Generate the name of the current log node."
  (skroad--make-log-node (skroad--make-date-string skroad--log-month-format)))

;; TODO: log nodes should be creatable from here and from nowhere else!
(defun skroad--log-node-op (node live op &optional unique reason)
  "Record an OP on NODE (if LIVE: emplace live link) to the current log node.
The current log node is created if it did not previously exist.
If UNIQUE is true, attempted duplication simply moves an entry to the day's end.
REASON, if given, is a comment describing the cause of the operation."
  (let ((entry
         (concat op " " (if live
                            (skroad--link-generate-live node)
                          (skroad--link-generate-dead node))
                 (or (and (stringp reason) (concat " (" reason ")")) "")
                 ".")))
    (skroad--defer
     (message (concat "Skroad Log Entry: " entry))
     (skroad--with-node (skroad--current-log-name) nil ;; Run actions!
       (skroad--emplace-log-entry entry unique)
       (skroad--connect-to (skroad--make-log-node "Log"))
       (skroad--connect-to (skroad--current-year-log-name))))))

(defun skroad--log-node-revise (node)
  "Record a revision of NODE to the current log."
  (skroad--log-node-op node t "Revised" t)) ;; May NOT duplicate

(defun skroad--log-node-create (node)
  "Record the creation of NODE to the current log, with optional REASON."
  (skroad--log-node-op node t "Created")) ;; May duplicate

(defun skroad--log-node-remove (node &optional reason)
  "Record the removal of NODE to the current log, with optional REASON."
  (skroad--log-node-op node nil "Deleted" nil reason) ;; May duplicate
  (skroad--lint-deaden node) ;; Deaden any old links in the lint log
  )

(defun skroad--log-node-rename (old node)
  "Record the renaming of OLD to NODE to the current log."
  (skroad--log-node-op ;; May duplicate
   node t "Renamed" nil (concat "from '" old "'"))
  (skroad--lint-deaden old) ;; Deaden any old links in the lint log
  )

(defun skroad--log-node-merge (victim target)
  "Record the merging of VICTIM into TARGET to the current log."
  (skroad--log-node-remove
   victim (concat "Merged into " (skroad--link-generate-live target)))
  (skroad--log-node-revise target))

;; Node tail. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--node-tail-indicator "\n@@@\n"
  "Node tail indicator.  The newlines must NOT be removed.")

(defvar-local skroad--buf-tail-overlay nil
  "Overlay which covers the current node's tail indicator.")

(defun skroad--buf-have-tail-indicator-p ()
  "Determine whether the current buffer has a registered tail indicator."
  (skroad--buf-overlay-active-p skroad--buf-tail-overlay))

(defun skroad--tail-indicator-modified (_ov after-p _beg _end &optional _len)
  "Called if the tail indicator was unexpectedly modified (by e.g. undo)."
  (when after-p
    (delete-overlay skroad--buf-tail-overlay)
    (run-with-idle-timer 0 nil #'skroad--node-tail-ensure)))

(defun skroad--buf-register-tail-indicator (start end)
  "Register the tail indicator for the current buffer from START to END."
  (setq-local skroad--buf-tail-overlay (make-overlay start end nil t nil))
  (when (skroad--mode-p) ;; Only do these when in an interactive buffer:
    (overlay-put skroad--buf-tail-overlay
               'modification-hooks '(skroad--tail-indicator-modified))
    (skroad--refontify-current-buffer)))

(defun skroad--emplace-tail-indicator ()
  "Insert the tail indicator at the current point and register its location."
  (skroad--buf-register-tail-indicator
   (prog1 (point) (insert skroad--node-tail-indicator)) (point)))

(defun skroad--move-tail-indicator-here ()
  "Move (and reregister) the tail indicator of the current node to the point.
Let the user know whether the tail had actually moved (or was already here.)
No-op if the proposed tail indicator would overlap the old one.
For use in interactive commands only."
  (skroad--node-tail-ensure)
  (let ((old-start (copy-marker (overlay-start skroad--buf-tail-overlay)))
        (old-end (overlay-end skroad--buf-tail-overlay)))
    (if (and (>= (point) old-start) (<= (point) old-end))
        (skroad--info "The tail did not move!")
      (let ((inhibit-read-only t))
        (delete-overlay skroad--buf-tail-overlay)
        (delete-region old-start old-end)
        (skroad--emplace-tail-indicator)
        (save-mark-and-excursion
          (goto-char old-start)
          (unless (bolp)
            (insert "\n"))))
      (skroad--info "The tail has been moved."))))

(defun skroad--jump-to-suggested-node-tail (&optional delete-dead)
  "Find where the node tail ought to be per the tail heuristic, and go there:
Starting at the end, move up, skipping dead links (if DELETE-DEAD is t: delete),
live links, and whitespace until something that is neither a node link nor
whitespace is encountered.  The proposed tail will start just below that point."
  (goto-char (point-max))
  (let ((climb
         #'(lambda (type &optional delete)
             (let ((prev-pos (point)))
               (save-mark-and-excursion
                 (when (funcall (get type 'find-any-backward))
                   (goto-char (match-end 0))
                   (skroad--skip-whitespace-forward)
                   (when (= prev-pos (point))
                     (let ((new-pos (match-beginning 0)))
                       (when delete (delete-region new-pos prev-pos))
                       new-pos))))))))
    (while (let ((prev (or (funcall climb 'skroad--text-link-node-live)
                           (funcall climb 'skroad--text-link-node-dead
                                    delete-dead))))
             (when prev (goto-char prev))))))

(defun skroad--node-tail-ensure ()
  "Ensure that the tail indicator exists in the current node, and register it.
Return true when an existing one was not found and a new one was inserted."
  (cond ((skroad--buf-have-tail-indicator-p) nil) ;; Already registered? No-op.
        ((save-mark-and-excursion ;; Not registered? Look for an existing one:
           (skroad--goto-node-body-start)
           (search-forward skroad--node-tail-indicator nil t))
         (skroad--buf-register-tail-indicator (match-beginning 0) (match-end 0))
         nil)
        (t ;; Looked but did not find? Emplace a new tail indicator:
         (save-mark-and-excursion
           (let ((inhibit-read-only t))
             (skroad--jump-to-suggested-node-tail t)
             (skroad--emplace-tail-indicator)
             (skroad--lint-report "Tail was auto-positioned.")))
         t)))

(defun skroad--node-body-end-pos ()
  "Return the end position of the current node's body (tail indicator start).
If this node did not have a tail indicator, a new one is emplaced."
  (skroad--node-tail-ensure)
  (overlay-start skroad--buf-tail-overlay))

(defun skroad--node-tail-start-pos ()
  "Return the start position of the current node's tail (tail indicator end).
If this node did not have a tail indicator, a new one is emplaced."
  (skroad--node-tail-ensure)
  (overlay-end skroad--buf-tail-overlay))

(defun skroad--in-node-tail-p ()
  "Determine whether the point is currently inside the tail."
  (>= (point) (skroad--node-tail-start-pos)))

(defun skroad--tail-indicator-jail (p-old p-new)
  "Constrain a point motion from P-OLD to P-NEW to avoid the tail indicator.
Return the constrained point.  For use with `skroad--point-zone-handler'."
  (or (and (skroad--buf-have-tail-indicator-p)
           (let ((s (overlay-start skroad--buf-tail-overlay))
                 (e (overlay-end skroad--buf-tail-overlay)))
             (when (and (> p-new s) (< p-new e))
               (if (< p-new p-old) s e))))
      p-new))

(defun skroad--link-insert-live-in-tail (node)
  "Emplace a link to NODE in the tail of the current node.
If this node did not have a tail indicator, a new one is emplaced.
If NODE is a log, and the tail has no other log links, it goes at the bottom;
If there were other log links in the tail, it goes in chronological order.
A non-log link is emplaced at the top of the tail, just below the indicator."
  (save-mark-and-excursion
    (let ((tail-start (skroad--node-tail-start-pos))
          (link (skroad--link-generate-live node)))
      (cond ((skroad--node-log-p node) ;; A log link?
             (goto-char (point-max))
             (skroad--skip-whitespace-backward tail-start)
             (while (and
                     (funcall
                      (get 'skroad--text-link-node-live 'find-any-backward)
                      tail-start)
                     (if (skroad--log-earlier-p
                          node (match-string-no-properties 1))
                         (goto-char (match-beginning 0))
                       (goto-char (match-end 0))
                       (insert "\n")
                       nil)))
             (insert link))
            (t (goto-char tail-start) ;; An ordinary link?
               (insert link "\n"))))))

;; Tail text highlighting. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--render-tail-indicator (limit)
  "Render the tail indicator when its overlay exists between point and LIMIT.
Also highlight any included portion of the tail text itself.
If this node did not have a tail indicator, this is a no-op."
  (when (skroad--buf-have-tail-indicator-p)
    (let ((s (overlay-start skroad--buf-tail-overlay))
          (e (overlay-end skroad--buf-tail-overlay)))
      (when (and (< (point) e) (< s limit)) ;; Overlay overlaps the region?
        (put-text-property s e 'read-only "Tail indicator may not be modified!")
        (put-text-property s e 'inhibit-isearch t)
        (put-text-property (1+ s) (1- e) 'invisible t)
        (put-text-property (1- e) e 'rear-nonsticky t)
        (put-text-property (1+ s) e 'face 'skroad--node-tail-face))
      (when (< e limit) ;; Render any tail text overlapping the region:
        (add-face-text-property
         (max e (point)) limit 'skroad--tail-text-face t))))
  nil)

(skroad--deftype skroad--text-tail-indicator
  :doc "Text type for rendering the tail indicator."
  :render-next #'skroad--render-tail-indicator
  :use 'skroad--text-mixin-rendered)

;; Node title and body. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--current-node-title nil
  "Cached title of the node in the current buffer (Do not access directly).")

(defun skroad--current-node ()
  "Return the filename-derived title of the node in the current buffer.
If we're in search results mode, return the name of the buffer."
  (or skroad--current-node-title
      (setq-local
       skroad--current-node-title
       (or (and buffer-file-name
                (skroad--file-path-to-node-title buffer-file-name))
           (buffer-name)))))

(defun skroad--node-self-p (node)
  "Return t if NODE is the current node."
  (and (skroad--mode-p) ;; No node is considered equal to a search res buffer
       (string-equal node (skroad--current-node))))

(defun skroad--current-buffer-node-p ()
  "Return t when the current buffer contains a Skroad node."
  (or (skroad--mode-p)
      (and buffer-file-name
           (when-let* ((file buffer-file-name)
                       (extension (file-name-extension file)))
             (string-equal extension skroad--file-extension)))))

(defun skroad--current-internal-title ()
  "Get the current node's title from the buffer."
  (buffer-substring-no-properties (point-min) (skroad--get-end-of-line 1)))

(defun skroad--change-internal-title (new-title)
  "Change the internal title of the current node to NEW-TITLE."
  (let ((inhibit-read-only t))
    (save-mark-and-excursion
      (goto-char (point-min))
      (insert new-title)
      (insert "\n")
      (skroad--fontify-current-line)
      (delete-region (point) (progn (forward-line 1) (point))))))

;; TODO: suppose there's no tail indicator?
(defun skroad--current-node-extract-body ()
  "Return the body of the current node."
  (save-mark-and-excursion
    (skroad--goto-node-body-start)
    (string-trim
     (buffer-substring-no-properties (point) (skroad--node-body-end-pos)))))

(defun skroad--cmd-title-kill-ring-save ()
  "Save the current node's title, transformed to a live link, to the kill ring."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let ((node (skroad--current-internal-title))
        (is-node (skroad--mode-p)))
    (with-temp-buffer
      (if is-node
          (skroad--link-insert-live node)
        (insert node))
      (copy-region-as-kill (point-min) (point-max)))))

(defun skroad--cmd-title-delete-current-node ()
  "Delete"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let ((node (skroad--current-node)))
    (if (or buffer-read-only
            (skroad--node-special-p node)
            (skroad--node-log-p node))
        (user-error "Node '%s' cannot be deleted!" node)
      (skroad--complete-all-deferred) ;; Pending ops must complete first
      (when (skroad--delete-node node)
        (skroad--log-node-remove node)
        (skroad--info "Node '%s' was permanently deleted!" node)))))

;; Move the tail indicator to the position suggested by the tail heuristic.
(defun skroad--cmd-title-reset-tail ()
  "TailReset"
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (let ((node (skroad--current-node)))
    (if buffer-read-only
        (user-error "This node's tail cannot be moved!")
      (when (y-or-n-p (format "Auto-reposition the tail of node '%s' ?" node))
        (skroad--node-tail-ensure)
        (save-mark-and-excursion
          (skroad--jump-to-suggested-node-tail)
          (skroad--move-tail-indicator-here))))))

(skroad--deftype skroad--text-node-title
  :doc "Node title."
  :use 'skroad--text-atomic
  :order 500
  :keymap
  (skroad--define-keymap
    "<return>" #'ignore "SPC" #'ignore
    "<deletechar>" #'ignore "<backspace>" #'ignore
    "<remap> <set-mark-command>" #'ignore
    "<remap> <yank>" #'ignore
    "<remap> <kill-region>" #'ignore
    "<remap> <kill-ring-save>" #'skroad--cmd-title-kill-ring-save
    "d" #'skroad--cmd-title-delete-current-node
    "T" #'skroad--cmd-title-reset-tail
    )
  :face 'skroad--title-face
  :inhibit-isearch t ;; Don't interactive-search in the title
  :read-only "Title must be changed via rename command!"
  :renamer-overlay-type 'skroad--text-node-renamer-direct
  :use 'skroad--text-mixin-renameable-direct
  :match-number 0
  :regex-any (rx string-start (* not-newline) "\n")
  :use 'skroad--text-mixin-findable
  :use 'skroad--text-mixin-rendered-zoned
  )

;; Link selector. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-selector nil
  "Selector overlay active when an atomic is under the point.")

(defconst skroad--selector-properties
  `((face skroad--selector-face) (evaporate t))
  "Text properties of the selector.")

(defun skroad--buf-overlay-active-p (overlay)
  "Determine whether OVERLAY is active in the current buffer."
  (and (overlayp overlay) (eq (current-buffer) (overlay-buffer overlay))))

(defun skroad--selector-active-p ()
  "Return t if the selector is active in this buffer."
  (skroad--buf-overlay-active-p skroad--buf-selector))

(defun skroad--selector-zone ()
  "Get the zone in which the selector is currently active (or nil if none)."
  (when-let* ((selector skroad--buf-selector)
              (start (overlay-start selector)))
    (skroad--prop-at 'zone start)))

(defun skroad--selector-init ()
  "Initialize the selector overlay in the current buffer."
  (setq-local skroad--buf-selector (make-overlay (point-min) (point-min)))
  (skroad--selector-deactivate)
  (dolist (p skroad--selector-properties)
    (overlay-put skroad--buf-selector (car p) (cadr p))))

(defun skroad--selector-unhide ()
  "Reveal the selector overlay when it may have been hidden."
  (when (skroad--selector-active-p)
    (overlay-put skroad--buf-selector 'face 'skroad--selector-face)))

(defun skroad--selector-hide ()
  "Hide (but not destroy) the selector overlay."
  (when (skroad--selector-active-p)
    (overlay-put skroad--buf-selector 'face nil)))

(defun skroad--selector-activate-in-current-zone ()
  "Activate (if inactive) or move the selector to the current zone."
  (skroad--with-current-zone
    (move-overlay skroad--buf-selector start end)
    (unless (skroad--last-ev-was-mouse-p)
      (when (and (eq (current-buffer) (window-buffer))
                 (frame-focus-state))
        (skroad--show-key-help))))
  (skroad--toggle-cursor-state nil))

(defun skroad--selector-deactivate ()
  "Deactivate the selector; it can be reactivated again."
  (when (skroad--selector-active-p)
    (delete-overlay skroad--buf-selector)
    (skroad--info))
  (skroad--toggle-cursor-state t))

;; TODO: bug during undo of delete-line?
(defun skroad--selector-update ()
  "Enable the selector if point is on an atomic zone; otherwise disable it."
  (let ((zone (skroad--prop-at 'zone)))
    (if zone
        (when (not (eq zone (skroad--selector-zone)))
          (skroad--selector-activate-in-current-zone))
      (skroad--selector-deactivate))))

(defun skroad--show-key-help ()
  "Display the keymap help of the current point."
  (let ((km (skroad--prop-at 'keymap))) ;; Display keymap help
    (when km (skroad--info (skroad--make-keymap-help km)))))

;; Cursor motion, mark, and floating title handling. ;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-pre-command-point-state (list (point-min) nil)
  "Point, zone at point, and type at point prior to a command.")

(defun skroad--get-point-state ()
  "Return a snapshot of the current point and zone."
  (list (point) (skroad--prop-at 'zone)))

(defun skroad--mark-jail (p)
  "If mark is inactive, return point P; otherwise, return a constrained P."
  (if mark-active
      (let ((m (mark)))
        (if (skroad--buf-have-tail-indicator-p)
            (if (<= m (skroad--node-body-end-pos)) ;; Mark is in the node body
                (min (max p (skroad--node-body-start-pos))
                     (skroad--node-body-end-pos))
              (max p (skroad--node-tail-start-pos))) ;; Mark is in the node tail
          (max p (skroad--node-body-start-pos)))) ;; No tail indicator
    p)) ;; No mark

(defun skroad--point-zone-handler (prev)
  "To be called when the point may have moved.  PREV has old point and zone."
  (let ((done nil))
    (while (not done)
      (let ((current (skroad--get-point-state)))
        (seq-let (old-p old-zone p zone) (append prev current)
          (setq done (or (= old-p p)
                         (let ((detent
                                (skroad--mark-jail
                                 (skroad--tail-indicator-jail old-p p))))
                           (when (/= p detent)
                             (goto-char detent)))))
          (unless done
            (when zone
              (setq disable-point-adjustment t) ;; Don't skip invisibles
              (goto-char ;; Point may still need to move:
               (if (and (eq zone old-zone) ;; Point moved inside a zone?
                        (> p old-p)) ;; ... forward?
                   (skroad--zone-end) ;; ... jump forward out of this zone.
                 (skroad--zone-start)))) ;; Moved back or jumped from outside
            (when (and mark-active skroad--buf-alt-mark (= p (point) (mark)))
                (if (< skroad--buf-alt-mark p) (forward-char) (backward-char)))
            (setq prev current)))))))

(defun skroad--adjust-mark-if-present ()
  "Put mark and alt-mark in the right order, and show/hide selector."
  (cond
   (mark-active
    (setq-local mouse-highlight nil)
    (skroad--selector-hide)
    (let ((m (mark)) (am skroad--buf-alt-mark) (p (point)))
      (when (and am (> (abs (- p am)) (abs (- p m))))
        (set-mark am)
        (setq-local skroad--buf-alt-mark m))))
   (t
    (setq-local mouse-highlight t)
    (skroad--selector-unhide)
    (setq-local skroad--buf-alt-mark nil))))

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local skroad--buf-pre-command-point-state (skroad--get-point-state)))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (skroad--do-deferred-replacements)
  (skroad--buf-indices-sync)
  (skroad--fontify-current-line)
  (unless (skroad--renamer-active-p)
    (unless (and isearch-mode (not (use-region-p)))
      (skroad--point-zone-handler skroad--buf-pre-command-point-state))
    (skroad--selector-update)
    (skroad--adjust-mark-if-present)
    (skroad--save-cache-point))
  (when (or (skroad--idle-no-work-p) skroad--lint-in-progress)
    (skroad--maybe-refontify-now (skroad--idle-have-work-p))))

(defun skroad--after-save-hook ()
  "Triggers following a skroad buffer save."
  (when (and (not skroad--at-a-distance)
             (memq this-command '(save-buffer save-some-buffers
                                  write-file basic-save-buffer)))
    (skroad--log-node-revise (skroad--current-node))))

(defun skroad--before-save-common ()
  "Operations to perform before any save (interactive or not)."
  (skroad--do-deferred-replacements)
  (skroad--current-node-update-stub-status))

(defun skroad--before-save-hook ()
  "Triggers prior to an interactive save."
  (skroad--renamer-deactivate)
  ;; (when (buffer-modified-p)
  ;;   (skroad--buf-indices-sync))
  (skroad--before-save-common))

(when skroad--debug
  (defadvice skroad--post-command-hook (around intercept activate)
    (condition-case err
        ad-do-it
      ;; Let the debugger run
      ((debug error) (signal (car err) (cdr err))))))

;; Misc. major mode setup. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--set-writability ()
  "If the current node is a special node, interactive editing is prohibited."
  (setq-local buffer-read-only
              (or (skroad--node-special-p) (skroad--search-results-p))))

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((atomic (skroad--data-at pos))
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
            (push (skroad--md-make-url
                   url (string-trim (substring-no-properties str beg nxt)))
                  out)))
        (setq pos nxt)))
    (apply #'concat (nreverse out))))

(defun skroad--install-yank-transformer ()
  "Install the yank transformer in the current buffer."
  (add-hook 'yank-transform-functions #'skroad--yank-transformer nil t))

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
      (user-error "Help page '%s' was not found!" url))))

(defun skroad--open-node ()
  "Open a skroad node."
  (face-remap-add-relative 'header-line 'skroad--title-face)
  (skroad--deactivate-mark) ;; Zap spurious mark from opening links via mouse
  (skroad--set-writability) ;; If special node, open it as read-only
  (skroad--cache-intern (skroad--current-node)) ;; TODO?
  (skroad--init-font-lock)
  (skroad--node-tail-ensure)
  (skroad--current-node-update-stub-status) ;; TODO: do we want this here?
  (skroad--defer-in-current-buffer (skroad--buf-indices-sync))
  (skroad--skip-whitespace-forward)
  (skroad--update-modeline-node-label)
  )

;; Floating header line. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--header-text-for-current-window ()
  "If the title is not visible in the current window, enable the header in it.
Otherwise (including if current buffer is not in the mode), simply return nil."
  (when (and skroad--floating-title-enable
             (skroad--mode-or-search-results-p)
             (skroad--in-node-body-p (window-start)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (skroad--abbrev-string
       (buffer-substring (point) (line-end-position))
       (progn (vertical-motion 1) (point))))))

(defun skroad--vacate-window (window)
  "If the current buffer is in a different mode, disable the header in WINDOW."
  (unless (skroad--mode-or-search-results-p)
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

;; Modeline. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--buf-modeline-node-label nil
  "The modeline label for the current buffer.")

(defun skroad--update-modeline-node-label ()
  "Update the modeline label for the currently-open node."
  (when (skroad--mode-p) ;; Only in-mode
    (setq-local
     skroad--buf-modeline-node-label
     (concat
      (cond ((skroad--node-special-p) "Special ")
            ((skroad--node-log-p) "Log ")
            (t (concat
                (if (skroad--node-orphan-p) "Orphan " "")
                (if (skroad--node-stub-p) "Stub " ""))))
      "Node"))))

(defun skroad--setup-mode-line ()
  "Replace the buffer name in the mode with a node description."
  (setq-local mode-line-buffer-identification
              '(:eval
                (propertized-buffer-identification
                 (format "%s"
                         (or
                          (and (stringp skroad--buf-modeline-node-label)
                               (concat
                                skroad--buf-modeline-node-label
                                (format
                                 " (%s)"
                                 (skroad--current-indices-live-link-count))))
                          (buffer-name)))))))

;; Point cache. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--point-cache (make-hash-table :test 'equal)
  "Cache storing the last known interactive point position in a node.")

(defun skroad--save-cache-point ()
  "Save the current node's point to the point cache."
  (when (skroad--current-buffer-node-p)
    (puthash (skroad--current-node) (point) skroad--point-cache)))

(defun skroad--before-kill-buffer-hook ()
  "Triggers prior to a skroad buffer being killed."
  (skroad--renamer-deactivate)
  (skroad--save-cache-point)
  ;;; TODO: remove!
  (message "evicting: %s because closed!" (skroad--current-node))
  (skroad--cache-invalidate (skroad--current-node)) ;; Evict when closing
  ;;;
  )

(defun skroad--maybe-restore-cached-point ()
  "If the current node had been visited in this session, restore the point.
Returns t when a cached position was actually found."
  (when (skroad--current-buffer-node-p)
    (let ((cached-point (gethash (skroad--current-node) skroad--point-cache)))
      (if cached-point
          (goto-char (min (point-max) cached-point))
        (skroad--goto-node-body-start)
        nil))))

;; Back-end. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: 'import buffer as node' interactive command?

(defun skroad--node-ensure (node)
  "Find or create NODE, and ensure that it is interned in the cache.
Return the path where the node is found on disk."
  (let ((node-path (skroad--node-path node))) ;; Path where it would exist
    (unless (skroad--cache-peek node) ;; Do nothing if node is already active
      (cond
       ((file-exists-p node-path) ;; Found on disk, but needs internment
        (skroad--cache-intern-unindexed node)) ;; Intern, index on demand
       ((file-writable-p node-path) ;; Not found on disk, but can be made:
        (write-region
         (concat node "\n" skroad--node-tail-indicator)
         nil node-path nil 0) ;; Insert title and tail indicator.
        (skroad--cache-write node nil) ;; Intern the node with an empty index
        (skroad--node-set-stub node t) ;; It starts as a stub (unless special)
        (skroad--log-node-create node)) ;; Report creation to log.
       (t (error "Could not activate node '%s'!" node))))
    node-path))

(defun skroad--in-node (node op target &optional allow-special)
  "Ensure that NODE exists, and run OP on TARGET (nil: current node) from it.
If NODE is a special node, and ALLOW-SPECIAL is nil, do nothing."
  (when (or (not (skroad--node-special-p node)) allow-special)
    (skroad--with-node node t (funcall op target))))

(defun skroad--node-ensure-indices (node)
  "Ensure that NODE (created if required) has been indexed; return its indices."
  (if (skroad--cache-indexed-p node)
      (skroad--cache-fetch node)
    (skroad--with-node node nil (skroad--buf-indices)))) ;; Runs actions!

(defvar skroad--special-nodes nil
  "List of pre-defined special nodes.  These nodes are created automatically;
contain only mechanically-generated content; and cannot be renamed, deleted,
or edited interactively.  Special nodes are not subject to auto-backlinking.")

(defun skroad--node-special-p (&optional node)
  "Return t if NODE (if given; else the current node) is a special node."
  (member (or node (skroad--current-node)) skroad--special-nodes))

(defvar skroad--special-nodes-no-index nil
  "List of pre-defined special nodes where indexing is inhibited.")

(defun skroad--node-disable-index-p ()
  "Return t if the current node has indexing inhibited."
  (or (null buffer-file-name)
      (member (skroad--current-node) skroad--special-nodes-no-index)))

(defmacro skroad--define-special-node (handle node allow-index &rest legend)
  "Define a special NODE; store title in HANDLE.
If ALLOW-INDEX is false, do not track changes or maintain indices for the node."
  (declare (indent defun))
  `(progn
     (defconst ,handle ,node ,@legend)
     (add-to-list 'skroad--special-nodes ,node)
     (unless ,allow-index
       (add-to-list 'skroad--special-nodes-no-index ,node))))

(defun skroad--reset-special-node (node)
  "Hollow out the given special NODE."
  (when (skroad--node-special-p node)
    (skroad--with-node node t
      (skroad--change-internal-title node)
      (delete-region (skroad--node-tail-start-pos) (point-max))
      )))

(defun skroad--connected-p (origin &optional node)
  "Test whether NODE (if given; else the current node) is linked from ORIGIN.
ORIGIN is indexed/created if required.  If NODE is a special node, return nil."
  (unless (skroad--node-special-p node)
    (skroad--indices-has-p 'skroad--text-link-node-live
                           (or node (skroad--current-node))
                           (skroad--node-ensure-indices origin))))

(defun skroad--set-special-status (node special status)
  "Set connection STATUS of NODE from the given SPECIAL (assumed) node.
SPECIAL is created if required.  If NODE itself is a special or log, do nothing.
Return t only when the connection status of NODE from SPECIAL actually changed."
  (unless (or (skroad--node-special-p node) ;; If node itself is special, no-op
              (skroad--node-log-p node) ;; Log nodes are never stubs or orphans
              (eq (skroad--connected-p special node) status)) ;; no change?
    (skroad--in-node
     special (if status #'skroad--connect-to #'skroad--disconnect-from) node t)
    t))

(skroad--define-special-node skroad--special-node-log "#Log" nil
  "Record of node creation, modification, and renaming.")

(skroad--define-special-node skroad--special-node-lint "#Lint" nil
  "Record of all lint output (including problems corrected at run time).")

(defun skroad--lint-report (text &optional use-prefix)
  "Log TEXT to the current lint report (when it does not already appear there.)
If USE-PREFIX is given, use it.  Otherwise prefix the current node, if any."
  (let* ((prefix
          (or use-prefix
              (if (skroad--current-buffer-node-p)
                  (format "Node %s : "
                          (skroad--link-generate-live (skroad--current-node)))
                "")))
         (report (concat prefix text)))
    (message (concat "Skroad Lint: " report)) ;; Always print to console also
    (skroad--with-node skroad--special-node-lint t
      (skroad--emplace-log-entry report t))))

(defun skroad--lint-deaden (node)
  "Deaden any links to defunct NODE in the current lint report."
  (skroad--with-node skroad--special-node-lint t
    (skroad--link-deaden node)))

(skroad--define-special-node skroad--special-node-stubs "#Stubs" t
  "A node with links to all known stub nodes. A stub node is a regular node
without any text between the title and the tail.  New nodes start out as stubs.
Stubs which get disconnected do not retain dead links; when orphaned, a stub
becomes a candidate for auto-deletion (see below.)")

(skroad--define-special-node skroad--special-node-orphans "#Orphans" t
  "A node with links to all known orphans (regular nodes that have no live
links to nodes other than themselves, specials, and logs.)  A node found to be
an orphan stub becomes a candidate for automatic deletion: if the node is not
open in any buffer, it is deleted immediately; otherwise, user must confirm.")

(defun skroad--node-stub-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known stub."
  (skroad--connected-p skroad--special-node-stubs node))

(defun skroad--node-orphan-p (&optional node)
  "Return t when NODE (if given; else the current node) is a known orphan."
  (skroad--connected-p skroad--special-node-orphans node))

(defun skroad--defer-orphan-stub-check (node)
  "Defer a check for orphan-stub status of NODE; propose deletion if true."
  (skroad--defer
   (when (and (skroad--node-orphan-p node) (skroad--node-stub-p node))
     (when (skroad--delete-node node)
       (skroad--log-node-remove node)))))

(defun skroad--node-set-stub (node status)
  "Set the stub STATUS of NODE.  See also `skroad--node-set-orphan'."
  (when (skroad--set-special-status node skroad--special-node-stubs status)
    (when status
      (skroad--defer-orphan-stub-check node)) ;; Possible deletion
    (skroad--request-refontify) ;; Schedule a refontification.
    t)) ;; Return t if status changed.

(defun skroad--current-node-stubbed-p ()
  "Determine whether the current node is presently a stub.
A stub is a node where only whitespace is found between the title and the tail.
If the tail did not previously exist in the current node, it is emplaced."
  (save-mark-and-excursion
    (skroad--goto-node-body-start)
    (let ((body-end (skroad--node-body-end-pos)))
      (skroad--skip-whitespace-forward body-end)
      (= (point) body-end))))

(defun skroad--current-node-update-stub-status ()
  "Update the current node's saved stub status.  No-op when renamer is active."
  (unless (skroad--renamer-active-p)
    (when (skroad--node-set-stub
           (skroad--current-node) (skroad--current-node-stubbed-p))
      (skroad--update-modeline-node-label))))

(defun skroad--node-set-orphan (node status)
  "Set the orphan STATUS of NODE.  If it became an orphan stub, try deleting it.
If deletion is blocked, no new auto-deletion attempt will be made until and
unless the node stops being an orphan stub and then later becomes one again,
or until a lint is performed (node will be silently deleted unless open.)"
  (when (skroad--set-special-status node skroad--special-node-orphans status)
    (when status
      (skroad--defer-orphan-stub-check node)) ;; Possible deletion
    t)) ;; Return t if status changed.

(defun skroad--current-node-orphaned-p ()
  "Determine whether the current node is presently an orphan.
The current node's indices must exist."
  (not (skroad--current-indices-any-p
        'skroad--text-link-node-live ;; If indices have live links: not orphan
        #'(lambda (l) (not (skroad--node-log-p l)))))) ;; Log links don't count

(defun skroad--current-node-update-orphan-status ()
  "Unless the current node is a special or log, update its saved orphan status."
  (unless (or (skroad--node-special-p) (skroad--node-log-p))
    (when (skroad--node-set-orphan
           (skroad--current-node) (skroad--current-node-orphaned-p))
      (skroad--update-modeline-node-label))))

(defun skroad--prompt-delete-node (node)
  "Prompt to confirm the deletion of NODE and return the answer."
  (y-or-n-p (format "Permanently delete node '%s' ?" node)))

(defun skroad--delete-node (node &optional force)
  "Request deletion of NODE.  No-op if NODE does not exist or is special.
If NODE is open in a buffer, prompt to ask permission (unless FORCE is t).
Before deleting, disconnect any remaining live links."
  (when (and (skroad--cache-peek node)
             (not (or (skroad--node-special-p node)
                      (skroad--node-log-p node))))
    (let* ((node-path (skroad--node-path node))
           (visiting-buffer (find-buffer-visiting node-path))
           (node-closed (null visiting-buffer)))
      (when (or node-closed ;; If node is closed, don't need to offer a veto
                force ;; If force is t, just close the node silently right now
                skroad--lint-in-progress ;; ... or if we're linting;
                (skroad--prompt-delete-node node)) ;; else, ask first.
        ;; TODO: report when we delete a node during lint!
        (let ((node-peers
               (skroad--with-node node t (skroad--link-get-all-live))))
          (dolist (peer node-peers)
            (when (skroad--cache-peek peer)
              (skroad--in-node peer #'skroad--disconnect-from node))))
        (unless node-closed ;; Unless already closed, clean it up and close:
          (with-current-buffer visiting-buffer
            (let ((inhibit-read-only t))
              (kill-all-local-variables)
              (restore-buffer-modified-p nil)
              (kill-buffer))))
        (skroad--node-set-orphan node nil) ;; Banish it from orphans
        (skroad--node-set-stub node nil) ;; Banish it from stubs
        (skroad--cache-evict node) ;; Banish it from the cache
        (delete-file node-path) ;; Permanently delete the node file!
        (skroad--request-refontify) ;; Schedule a refontification.
        t)))) ;; Return t when actually deleted.

;; TODO: exclude lint log, as it has no indices?
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

;; TODO: smarter merge that preserves un-tail-ish chunks in victim's tail?
(defun skroad--merge-node-into-current (victim)
  "Permanently merge the node VICTIM (which must exist) into the current node.
Prompts for confirmation, since this op wipes undo info in all affected buffers!
Existing links to VICTIM in the current node are removed.  Its body is copied
to the current node, into a demarcated block above the tail.  Its tail is merged
into the current node's tail.  All non-special nodes (and the Log) which
formerly linked to the VICTIM will now link to the current node instead.
After all of this, the VICTIM is permanently deleted."
  (let ((this-node (skroad--current-node))) ;; The node we're merging into
    (when (and victim ;; Victim is not nil
               (skroad--cache-peek victim) ;; Victim is a node which exists
               (not (or buffer-read-only ;; Destination must be writable
                        (skroad--node-self-p victim) ;; May not merge self
                        (skroad--node-log-p victim) ;; May not merge log nodes
                        (skroad--node-log-p this-node) ;; ... or into one
                        (skroad--node-special-p victim))) ;; ... or special node
               (y-or-n-p ;; Ask first, because the victim will be perma-deleted!
                (format "Permanently merge node '%s' into this node ?" victim)))
      (skroad--buf-indices-sync) ;; Make sure current indices are up to date
      (skroad--complete-all-deferred) ;; Ensure that there are no pending ops
      (skroad--disconnect-from victim) ;; Break this node's links to victim
      (let (victim-is-stub victim-body victim-peers)
        (skroad--with-node victim t ;; Get all relevant info from the victim
          (setq victim-body (skroad--current-node-extract-body))
          (setq victim-peers ;; Nodes (except for this one) linking to it
                (delete this-node (skroad--current-node-linked-from))) ;; TODO?
          (setq victim-is-stub (skroad--node-stub-p)))
        (unless victim-is-stub ;; Don't insert anything if victim is a stub
          (let (import-start import-end)
            (atomic-change-group ;; Insert a demarcated copy of victim's body
              (goto-char (skroad--node-body-end-pos))
              (ensure-empty-lines)
              (skroad--atomic-comment-insert
               (format "Start merged body of node '%s'" victim))
              (setq import-start (point))
              (insert victim-body)
              (setq import-end (point))
              (insert "\n")
              (skroad--atomic-comment-insert
               (format "End merged body of node '%s'" victim))
              (insert "\n"))
            ;; Fix self-links of the victim in the imported body:
            (skroad--link-replace victim this-node import-start import-end)
            (goto-char import-start))) ;; Jump to the start indicator
        (skroad--buf-indices-sync t) ;; Sync indices, but don't run actions
        ;; Nodes that linked to the victim will now link to this node instead:
        (dolist (peer victim-peers)
          (skroad--defer
           (skroad--with-node peer nil ;; Run index actions, so tails get merged
             (skroad--link-merge victim this-node)
             (skroad--clear-buf-undo-info)))))
      (skroad--save-current-node) ;; Save immediately
      (skroad--clear-buf-undo-info) ;; Zap undo info
      ;; TODO: defer deletion?
      (skroad--delete-node victim t) ;; Permanently delete the victim!
      (skroad--log-node-merge victim this-node)
      (skroad--request-refontify) ;; Schedule a refontification.
      )))

(defun skroad--rename-node (old new)
  "Rename node OLD to NEW.  OLD is presumed to exist; NEW is a valid title.
Warning: undo info is lost in all affected buffers!"
  (skroad--complete-all-deferred) ;; Ensure no ops are pending
  (let ((old-peers (skroad--with-node old t
                     (skroad--current-node-linked-from t))))
    (cond
     ((and (skroad--cache-rename old new)
           (skroad--mv-file (skroad--node-path old) (skroad--node-path new)))
      (dolist (peer old-peers)
        (skroad--defer
         (skroad--with-node peer t ;; Don't perform actions
           (skroad--link-replace old new)
           (skroad--clear-buf-undo-info))))
      (skroad--with-node new t
        (skroad--change-internal-title new)
        (setq-local skroad--current-node-title nil) ;; Zap cached title
        (let ((inhibit-modification-hooks t))
          (skroad--link-replace old new)) ;; Update any self-links
        (skroad--log-node-rename old new)
        (skroad--request-refontify) ;; Schedule a refontification.
        (skroad--clear-buf-undo-info))) ;; Zap undo info
     (t (error "Could not rename node '%s' to '%s'!" old new)))))

;; TODO: write log entry if changing
(defun skroad--rectify-node-title ()
  "Ensure that the current node's internal and external titles match."
  (let ((external-title (skroad--current-node))
        (internal-title (skroad--current-internal-title)))
    (unless (string-equal internal-title external-title)
      (skroad--lint-report
       (format
        "Internal title '%s' does not match external title!" internal-title))
      ;; temporary:
      (skroad--change-internal-title external-title)
      )))

(defun skroad--lint ()
  "Perform a full rescan of all known nodes."
  (unless skroad--lint-in-progress
    (skroad--complete-all-deferred) ;; Ensure no ops are pending
    (setq skroad--lint-in-progress t)
    (dolist (node ;; Hollow out (don't delete) the nodes we regenerate :
             (list skroad--special-node-orphans
                   skroad--special-node-stubs
                   skroad--special-node-lint))
      (skroad--reset-special-node node))
    (let ((count 0))
      (skroad--cache-foreach ;; Dispatch for each known non-special node:
       #'(lambda (node)
           (unless (skroad--node-special-p node)
             (skroad--defer
              (setq count (1+ count))
              (skroad--cache-invalidate node) ;; Zap existing indices
              (skroad--with-node node t ;; TODO: permit actions???
                (skroad--rectify-node-title)
                (skroad--current-node-update-stub-status)
                (skroad--clear-buf-undo-info)
                )))))
      (skroad--defer
       (skroad--lint-report
        (format "Complete, linted %s nodes." count) "Lint: ")
       (skroad--request-refontify) ;; Schedule a refontification.
       (setq skroad--lint-in-progress nil)))))

;; Autocomplete for live node links. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--autocomplete-affixation (candidates)
  "Propertize filtered completion CANDIDATES before they are displayed."
  (mapcar (lambda (c)
            (list (propertize
                   c 'face
                   (cond ((skroad--node-log-p c) 'skroad--log-link-face)
                         ((skroad--node-special-p c) 'skroad--special-link-face)
                         ((skroad--node-stub-p c) 'skroad--stub-link-face)
                         (t 'skroad--live-link-face)))
                  ""
                  (if (skroad--node-orphan-p c) " (Orphan)" "")))
          candidates))

(defun skroad--autocomplete-collection (string pred action)
  "Completion table over cache entry keys, ignoring values."
  (let ((table (skroad--cache))
        (kpred (when pred (lambda (k _v) (funcall pred k)))))
    (cond
     ((eq action 'metadata)
      '(metadata (affixation-function . skroad--autocomplete-affixation)))
     ((eq action 'lambda)
      (not (eq (gethash string table 'missing) 'missing)))
     (t
      (complete-with-action action table string kpred)))))

(defun skroad--autocomplete-start-pos ()
  "If autocomplete may engage at point, return the starting position; else nil."
  (let ((here (point))) ;; Position where autocomplete is being attempted
    (unless (or (skroad--data-at here) ;; Prohibited inside any atomic
                (skroad--renamer-active-p)) ;; ... and inside the renamer
      (save-mark-and-excursion
        (when (search-backward ;; Find a start delimiter to the left of here
               skroad--link-node-live-start-delim (line-beginning-position) t)
          (unless (skroad--find-data-between (point) here) ;; Avoid atomics
            (goto-char (match-end 0))
            (point))))))) ;; Return position right after the start delimiter

(defun skroad--autocomplete-end-pos ()
  "Find the end of a proposed autocomplete interval at the current point."
  (save-mark-and-excursion
    (let* ((eol (line-end-position))
           (bound (or (skroad--find-data-between (point) eol) eol)))
      (skip-chars-forward "^ \t\n" bound)
      (point))))

(defun skroad--autocomplete-insert (open end candidate)
  "Replace OPEN..END with escaped CANDIDATE + the live link end delimiter."
  (delete-region open end)
  (insert
   (skroad--bracket-escape (substring-no-properties candidate))
   skroad--link-node-live-end-delim))

(defun skroad--autocomplete-in-buf-capf ()
  "CAPF for node name autocompletion."
  (let ((open (skroad--autocomplete-start-pos)))
    (when open
      (let ((end (skroad--autocomplete-end-pos)))
        (list open end #'skroad--autocomplete-collection
              :exit-function
              #'(lambda (candidate status)
                  (when (eq status 'finished)
                    (skroad--autocomplete-insert open (point) candidate))))))))

(defun skroad--autocomplete-buf-init ()
  "Initialize autocomplete in the current buffer."
  (setq-local completion-styles '(substring flex))
  (setq-local completion-ignore-case t)
  (setq-local completion-auto-help 'always))

(defun skroad--autocomplete-minibuffer-prompt (prompt) ;; TODO: limit count?
  "PROMPT in minibuffer for a node name; return it (or nil, if none selected)."
  (minibuffer-with-setup-hook #'skroad--autocomplete-buf-init
    (let ((choice (completing-read
                   prompt #'skroad--autocomplete-collection nil t)))
      (when (and choice (not (string-empty-p choice)))
        choice))))

;; Top-level key bindings. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-top-backspace ()
  "If prev point is in an atomic, delete it; otherwise, normal backspace."
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
        ((bobp) nil)
        (t (let* ((p (point)) (left (1- p)))
             (unless (skroad--in-node-title-p left) ;; Never bite into the title
               (if (skroad--data-at left) ;; If in front of an atomic:
                   (delete-region (skroad--zone-start left) p) ;; ... delete it;
                 (delete-char -1))))))) ;; ... if not: normal backspace.

(defun skroad--cmd-top-gt ()
  "If there is a region, increase its quote level; otherwise insert `>'."
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (unless (skroad--in-node-title-p)
    (if (and (use-region-p) (not (skroad--renamer-active-p)))
        (skroad--quote-region (region-beginning) (region-end))
      (insert ">"))))

(defun skroad--cmd-top-lt ()
  "If there is a region, decrease its quote level; otherwise insert `<'."
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (unless (skroad--in-node-title-p)
    (if (and (use-region-p) (not (skroad--renamer-active-p)))
        (skroad--unquote-region (region-beginning) (region-end))
      (insert "<"))))

(defun skroad--cmd-top-jump-to-next-atomic ()
  "Jump to the next atomic after the point; try to cycle to first if none."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--zone-jump-from (point) nil t))

(defun skroad--cmd-top-jump-to-prev-atomic ()
  "Jump to the previous atomic before the point; try to cycle to last if none."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (skroad--zone-jump-from (point) t t))

(defun skroad--cmd-top-tab ()
  "Top-level key binding for TAB."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (if (skroad--autocomplete-start-pos) ;; Are we sitting in a [[..... ?
      (completion-at-point) ;; ... trigger the autocomplete.
    (skroad--cmd-top-jump-to-next-atomic))) ;; ... if not, regular tab binding.

(defun skroad--cmd-top-goto-tail ()
  "Top-level jump-to-tail."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (goto-char (skroad--node-tail-start-pos)))

(defun skroad--cmd-top-move-tail-here ()
  "Move the current node tail indicator to the point."
  (interactive "*" skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (if buffer-read-only
      (skroad--info "This node's tail cannot be moved!")
    (skroad--move-tail-indicator-here)))

(defun skroad--cmd-top-toggle-atomic-text-hiding ()
  "Toggle non-match text hiding in atomics having a `visible-match-number'."
  (interactive nil skroad-mode skroad-search-results-mode)
  (skroad--modes-only)
  (setq skroad--atomic-show-payload-only (not skroad--atomic-show-payload-only))
  (skroad--request-refontify)) ;; Schedule a refontification.

(defvar skroad--mode-map
  (skroad--define-keymap
    ">" #'skroad--cmd-top-gt
    "<" #'skroad--cmd-top-lt
    "<remap> <delete-backward-char>" #'skroad--cmd-top-backspace
    "TAB" #'skroad--cmd-top-tab ;; binding <tab> interferes with autocomplete
    "C-<tab>" #'skroad--cmd-top-jump-to-prev-atomic
    "M-T" #'skroad--cmd-top-move-tail-here
    "M-t" #'skroad--cmd-top-goto-tail
    "C-M-l" #'skroad--cmd-top-toggle-atomic-text-hiding ;; TODO: do we need it?
    )
  "Top-level command keymap for the skroad major mode.")

;; Skroad commands with global bindings. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-skroad-node ()
  "Find and open a Skroad node using the minibuffer.  Works from any mode."
  (interactive)
  (let* ((display-buffer-overriding-action
          skroad--disp-mode-this-window-or-existing)
         (node (skroad--autocomplete-minibuffer-prompt "Find Skroad node: ")))
    (when node
      (skroad--show-existing-node node))))

(defun search-skroad (string)
  "Full-text search for STRING across all known nodes.
Case-insensitive.  Results appear incrementally, grouped by node;
repeating a search already in progress is a no-op."
  (interactive (list (read-string "Search all Skroad nodes for: ")))
  (when (string-empty-p string)
    (user-error "Empty search string!"))
  (pop-to-buffer (skroad--search-render string)))

;; Global binding for find-node and node text search:
(keymap-global-set "M-o" #'find-skroad-node)
(keymap-global-set "C-M-o" #'search-skroad)

;; Mode init. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--global-init-done nil
  "Indicates that the Skroad mode global init was completed in this session.")

(defun skroad--ensure-global-init ()
  "Perform the mode global init if it has not been done in this session."
  (unless skroad--global-init-done
    ;; Prohibit change hooks firing when only text properties have changed:
    (skroad--silence-modifications 'put-text-property)
    (skroad--silence-modifications 'add-text-properties)
    (skroad--silence-modifications 'remove-text-properties)
    (skroad--silence-modifications 'remove-list-of-text-properties)
    (skroad--silence-modifications 'set-text-properties)
    (skroad--silence-modifications 'add-face-text-property)
    (when skroad--lint-on-boot ;; Perform a lint on boot?
      (run-with-idle-timer 0 nil #'skroad--lint))
    (setq skroad--global-init-done t)))

(defun skroad--mode-common-init ()
  "Init aspects common to both skroad-mode and skroad-search-results-mode."
  (font-lock-mode 1)
  (visual-line-mode 1)
  ;; Handle word boundaries correctly (atomics are treated as unitary words) :
  (setq-local find-word-boundary-function-table
              skroad--find-word-boundary-function-table)
  ;; Prevent text properties from infesting the kill ring (emacs 28+) :
  (setq-local kill-transform-function #'substring-no-properties)
  ;; Modified isearch which ignores escaped brackets:
  (setq-local isearch-search-fun-function #'skroad--isearch-search-fun)
  ;; Install handler for Emacs help URLs:
  (setq-local browse-url-handlers
              '(("\\`help:" . skroad--emacs-help-url-handler)))
  (add-hook 'pre-command-hook #'skroad--pre-command-hook nil t)
  (add-hook 'post-command-hook #'skroad--post-command-hook nil t)
  (add-hook 'window-scroll-functions #'skroad--update-window-state nil t)
  (add-hook 'window-state-change-functions #'skroad--update-window-state nil t)
  (add-hook 'window-buffer-change-functions #'skroad--update-window-state nil t)
  (face-remap-add-relative 'header-line 'skroad--title-face)
  (skroad--deactivate-mark) ;; Zap spurious mark from opening links via mouse
  (skroad--selector-init)
  )

;; TODO: proper mode exit cleanup
;; TODO: do NOT set the mode if file is not in the data dir
(define-derived-mode skroad-mode text-mode "Skroad"
  (skroad--ensure-global-init)
  (skroad--mode-common-init)
  (setq-local require-final-newline t) ;; Insert final newline if absent
  ;; Disable default auto-save:
  (add-hook 'auto-save-mode-hook
            (lambda () (setq buffer-auto-save-file-name nil))
            nil t)
  (setq-local buffer-auto-save-file-name nil)
  ;; Buffer-local hooks (other than change tracker: installed on first sync)
  (add-hook 'before-save-hook #'skroad--before-save-hook nil t)
  (add-hook 'after-save-hook #'skroad--after-save-hook nil t)
  (add-hook 'kill-buffer-hook #'skroad--before-kill-buffer-hook nil t)
  (add-hook 'after-change-functions #'skroad--quote-after-change-hook nil t)
  (skroad--install-yank-transformer)
  ;; (add-hook 'auto-save-hook #'skroad--autosave-hook nil t)
  ;; Initialize autocomplete support.
  (setq-local completion-at-point-functions '(skroad--autocomplete-in-buf-capf))
  (skroad--autocomplete-buf-init)
  (skroad--setup-mode-line) ;; Modeline diddler
  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

;; Set up keymap for Skroad mode:
(set-keymap-parent skroad-mode-map
                   (make-composed-keymap skroad--mode-map text-mode-map))

(define-derived-mode skroad-search-results-mode special-mode "Skroad-Search"
  "Major mode for Skroad full-text search result buffers.
Not derived from `skroad', but fontifies its contents using
skroad's font-lock rules.  Read-only (via `special-mode');
contents are rewritten only by `skroad--search-render'."
  (skroad--mode-common-init)
  (skroad--init-font-lock)
  (skroad--deactivate-mark)
  )

;; Set up keymap for Skroad search results mode:
(set-keymap-parent skroad-search-results-mode-map
                   (make-composed-keymap skroad--mode-map special-mode-map))

(provide 'skroad)

;;; skroad.el ends here
