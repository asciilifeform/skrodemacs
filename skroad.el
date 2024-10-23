;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--keyword-to-symbol (exp)
  "If EXP is a keyword, convert it to a symbol. If not, return it as-is."
  (if (keywordp exp)
      (read (substring (symbol-name exp) 1))
    exp))

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
  (save-mark-and-excursion
    (goto-char pos)
    (line-beginning-position)))

(defun skroad--get-end-of-line (pos)
  "Get the position of the end of the line on which POS resides."
  (save-mark-and-excursion
    (goto-char pos)
    (line-end-position)))

(defmacro skroad--with-whole-lines (start end &rest body)
  "Get expanded region defined by START and END that spans whole lines."
  (declare (indent defun))
  `(let ((start-expanded (skroad--get-start-of-line ,start))
         (end-expanded (skroad--get-end-of-line ,end)))
     ,@body))

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (eq major-mode 'skroad-mode)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

(defun skroad--find-different-text-property (prop direction &optional pos)
  "Find the next/previous (DIRECTION) position where PROP is not nil and
differs from its value at POS (or point, if POS not given); nil if not found."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (let ((r (funcall
              (cond ((eq direction :forward) #'text-property-search-forward)
                    ((eq direction :backward) #'text-property-search-backward)
                    (t (error "Invalid direction: %s !" direction)))
              prop (get-text-property (point) prop)
              #'(lambda (oldval newval)
                  (and newval (not (eq oldval newval)))))))
      (if r (prop-match-beginning r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--floating-title-enable t
  "Display floating title at the top of the window if title is not in view.")

(defface skroad--text-face '((t :inherit default))
  "Default face used for skrode text types."
  :group 'skroad-faces)

(defface skroad--title-face
  '((t :foreground "white" :background "purple"
       :height 300 :weight bold :extend t))
  "Face for skroad node titles."
  :group 'skroad-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default properties for skroad text types.
(put 'skroad--default-type 'face 'skroad--text-face)

;; Prevent insertions adjacent to skroad text from inheriting its properties.
(put 'skroad--default-type 'rear-nonsticky t)

;; Default delimiters are null strings:
(put 'skroad--default-type 'start-delim "")
(put 'skroad--default-type 'end-delim "")

(defun skroad--type-fn (text-type name &rest args)
  "Evaluate function NAME, which must be defined for TEXT-TYPE, with ARGS."
  (let ((fn (get text-type name)))
    (unless fn
      (error "%s is not defined for type %s !" fn text-type))
    (apply fn text-type args)))

(defmacro skroad--define-default-type-fn (name args &rest body)
  "Define function NAME with ARGS to assign to the default text type."
  (declare (indent defun))
  (let ((fn (read (concat "skroad--default-type-" (symbol-name name)))))
    `(progn
       (defun ,fn ,args ,@body)
       (put 'skroad--default-type ',name #',fn))))

(skroad--define-default-type-fn make-text (this payload)
  (concat (get this 'start-delim) payload (get this 'end-delim)))

(skroad--define-default-type-fn make-regex (this &optional payload)
  (let* ((start-delim (get this 'start-delim))
         (end-delim (get this 'end-delim))
         (payload-regex (or payload (get this 'payload-regex)))
         (start-regex (concat (regexp-quote start-delim) "\s*"))
         (end-regex (concat "\s*" (regexp-quote end-delim))))
    (concat start-regex payload-regex end-regex)))

(skroad--define-default-type-fn find-next (this limit &optional payload)
  (funcall (get this 'finder)
           (skroad--type-fn this 'make-regex payload)
           limit))

(skroad--define-default-type-fn make-font-lock-rule (this)
  (let ((regex (funcall (get this 'make-regex) this))
        (find-next (get this 'finder))
        (renderer (get this 'renderer)))
    (list
     #'(lambda (limit)
         (when (funcall find-next regex limit)
           (with-silent-modifications
             (funcall renderer this)
             t)))
     '(0 nil append))))

(defvar skroad--rendered-text-types nil "Text types for use with font-lock.")
(defvar skroad--indexed-text-types nil "Text types that are indexed.")

(defun skroad--init-font-lock ()
  "Initialize font-lock rules for a skroad mode buffer."
  (let ((rules nil))
    (dolist (type skroad--rendered-text-types)
      (push (skroad--type-fn type 'make-font-lock-rule) rules))
    (font-lock-add-keywords nil rules t)))

(defun skroad--define-text-type (name &rest properties)
  ;; Add properties to the symbol:
  (skroad--do-plist prop val properties
    (cond
     ((eq prop :inherit) ;; Inherit all properties from given parent type
      (skroad--do-plist parent-prop parent-val (symbol-plist val)
        (put name parent-prop parent-val)))
     ((and (eq prop :rendered) val) ;; Use this type with font-lock?
      (add-to-list 'skroad--rendered-text-types name))
     ((and (eq prop :indexed) val) ;; Use this type with indexer?
      (add-to-list 'skroad--indexed-text-types name))
     ((eq prop :keymap) ;; If given a keymap:
      (let ((parent-keymap (get name 'keymap)))
        (when parent-keymap ;; stack on parent keymaps, if they exist
          (set-keymap-parent val parent-keymap))
        (put name 'keymap val)))
     (t (put name (skroad--keyword-to-symbol prop) val)))) ;; simply save it
  name)

(defun skroad--type-action (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name (let ((action (get text-type action-name)))
                      (when action (apply action args)))))

;; (symbol-plist 'skroad-live)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: return t if any replacements happened
(defun skroad--text-type-replace-all
    (text-type-old payload-old text-type-new payload-new)
  "Replace all text of TEXT-TYPE-OLD having PAYLOAD-OLD with
instances of TEXT-TYPE-NEW having PAYLOAD-NEW."
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (skroad--type-fn text-type-old 'find-next (point-max) payload-old)
      (replace-match (skroad--type-fn text-type-new 'make-text payload-new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--atomic-at (&optional pos)
  "Get the payload of the atomic found at the given POS, or nil if none."
  (get-text-property (or pos (point)) 'data))

(defun skroad--type-at (&optional pos)
  "Determine text type, if any, at position POS."
  (get-text-property (or pos (point)) 'category))

(defun skroad--zone-at (&optional pos)
  "Return the zone ID at POS (or point)."
  (get-text-property (or pos (point)) 'id))

(defun skroad--zone-start (&optional pos)
  "Return the position at which the zone at POS starts."
  (or (previous-single-property-change (1+ (or pos (point))) 'id) (point-min)))

(defun skroad--zone-end (&optional pos)
  "Return the position at which the zone at POS ends."
  (or (next-single-property-change (or pos (point)) 'id) (point-max)))

(defmacro skroad--with-zone (pos &rest body)
  "Evaluate BODY with start and end bound to boundaries of zone at POS."
  (declare (indent defun))
  `(let ((start (skroad--zone-start ,pos)) (end (skroad--zone-end ,pos)))
     ,@body))

;; TODO: abolish
(defmacro skroad--with-link-at-point (&rest body)
  "Evaluate BODY with link bound to the link under the point."
  `(let ((link (skroad--atomic-at (point))))
     (when link
       ,@body)))

(defun skroad--cmd-backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
          ((and (> p (point-min)) (skroad--atomic-at (1- p)))
           (delete-region (skroad--zone-start (1- p)) p))
          (t (delete-char -1)))))

(defun skroad--cmd-jump-to-next-link ()
  "Jump to the next link following point; cycle to first after the last."
  (interactive)
  (goto-char
   (or (skroad--find-different-text-property 'data :forward)
       (skroad--find-different-text-property 'data :forward (point-min))
       (point))))

(defun skroad--cmd-jump-to-prev-link ()
  "Jump to the previous link preceding point; cycle to last after the first."
  (interactive)
  (goto-char
   (or (skroad--find-different-text-property 'data :backward)
       (skroad--find-different-text-property 'data :backward (point-max))
       (point))))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--cmd-backspace
    "<tab>" #'skroad--cmd-jump-to-next-link
    "C-<tab>" #'skroad--cmd-jump-to-prev-link)
  "Keymap for skroad mode.")

;;; Text Types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--text-properties
  '(category face id data)
  "Properties added by font-lock that must be removed when unfontifying.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--point-in-title-p ()
  "Returns t if point is in the first line of the buffer, otherwise nil."
  (eq (line-beginning-position) (point-min)))

(defun skroad--find-next-nontitle (regex limit)
  "Find next REGEX, up to LIMIT, but only outside of the title line."
  (when (skroad--point-in-title-p) (goto-char (line-beginning-position 2)))
  (re-search-forward
   regex
   (if (< (point) limit) limit (skroad--get-end-of-line (point)))
   t))

(skroad--define-text-type
 'skroad-not-title
 :doc "Mixin type for all text types that do not apply to the node title."
 :finder #'skroad--find-next-nontitle)

(defun skroad--cmd-atomics-prepend-space ()
  "Insert a space immediately behind the atomic currently under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (skroad--zone-start))
    (insert " ")))

(defun skroad--deactivate-mark ()
  "Deactivate the mark and clear the alt-mark."
  (deactivate-mark)
  (setq-local skroad--alt-mark nil))

(defmacro skroad--define-atomics-region-cmd (wrap-command)
  "Wrap COMMAND to use region if exists, or use the atomic at point as region."
  `(defun ,(read (concat "skroad--cmd-atomics-"
                         (symbol-name wrap-command))) ()
     (interactive)
     (if (use-region-p)
         (call-interactively ',wrap-command)
       (skroad--with-zone (point)
         (funcall #',wrap-command start end)))
     (skroad--deactivate-mark)))

(skroad--define-atomics-region-cmd delete-region)
(skroad--define-atomics-region-cmd kill-region)
(skroad--define-atomics-region-cmd kill-ring-save)

(defvar-local skroad--selector nil
  "Selector overlay active when an atomic is under the point.")

(defconst skroad--selector-properties
  `((face highlight)
    (evaporate t))
  "Text properties of the selector.")

(defun skroad--selector-activate ()
  "Activate (if inactive) or move the selector to the current zone."
  (skroad--with-zone (point)
    (move-overlay skroad--selector start end (current-buffer)))
  ;; (setq-local cursor-type nil)
  )

(defun skroad--selector-deactivate ()
  "Deactivate the selector; it can be reactivated again."
  (when (skroad--selector-active-p)
    (delete-overlay skroad--selector))
  ;; (setq-local cursor-type t)
  )

(defun skroad--selector-active-p ()
  "Return t if the selector is active; otherwise nil."
  (and (overlayp skroad--selector)
       (eq (current-buffer) (overlay-buffer skroad--selector))))

(defvar-local skroad--alt-mark nil
  "Opposite end of a link in which the mark had been set.")

;; (defun skroad--region-selection-active-p ()
;;   "Return t if a region selection is active (even if length 0); otherwise nil."
;;   (or (use-region-p) skroad--alt-mark))

(defun skroad--atomic-enter (pos-from pos-to)
  ;; (skroad--move-point
  ;;  (if (and mark-active (eq (mark) pos-to))
  ;;      (skroad--zone-end)
  ;;    (skroad--zone-start)))

  (skroad--move-point (skroad--zone-start))
  (when (not mark-active) (skroad--selector-activate))
  (message (format "atomic enter from '%s' to '%s'"
                   skroad--prev-zone (skroad--zone-at pos-to)))
  )

(defun skroad--atomic-leave (pos-from pos-to)
  (skroad--selector-deactivate)
  (message (format "atomic leave from '%s' to '%s'"
                   skroad--prev-zone (skroad--zone-at pos-to)))
  )

(defun skroad--atomic-move (pos-from pos-to)
  (skroad--with-zone (point)
    (if (>= pos-to pos-from)
        (skroad--move-point end)
      (goto-char start)))
  (message (format "atomic move: %s" (skroad--zone-at)))
  )

(defun skroad--cmd-atomics-set-mark ()
  "Set the mark inside an atomic."
  (interactive)
  (save-excursion
    (skroad--with-zone (point)
      (setq-local skroad--alt-mark start)
      (goto-char end)
      (call-interactively 'set-mark-command))))

(defun skroad--atomic-renderer (this)
  "Font lock rendering for atomics."
  (set-text-properties
   (match-beginning 0) (match-end 0)
   (list 'category this
         'id (gensym)
         'face (get this 'face)
         'data (match-string-no-properties 1))))

(skroad--define-text-type
 'skroad-atomic
 :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
 :renderer #'skroad--atomic-renderer
 :on-enter #'skroad--atomic-enter
 :on-leave #'skroad--atomic-leave
 :on-move #'skroad--atomic-move
 :keymap
 (define-keymap
   "SPC" #'skroad--cmd-atomics-prepend-space
   "<remap> <set-mark-command>" #'skroad--cmd-atomics-set-mark
   "<remap> <self-insert-command>" #'ignore
   "<deletechar>" #'skroad--cmd-atomics-delete-region
   "<backspace>" #'skroad--cmd-atomics-delete-region
   "<drag-mouse-1>" #'ignore "<drag-mouse-2>" #'ignore "<drag-mouse-3>" #'ignore
   "<down-mouse-1>" #'ignore "<down-mouse-2>" #'ignore "<down-mouse-3>" #'ignore
   "<mouse-1>" #'ignore "<mouse-2>" #'ignore "<mouse-3>" #'ignore
   "<remap> <kill-region>" #'skroad--cmd-atomics-kill-region
   "<remap> <kill-ring-save>" #'skroad--cmd-atomics-kill-ring-save
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--do-link-action (pos)
  "Run action of link at POS, if one was defined, and no region is active."
  (unless (use-region-p)
    (skroad--type-action
     (skroad--type-at pos) 'on-activate (skroad--atomic-at pos))))

(defun skroad--cmd-left-click-link (click)
  "Perform the action attribute of the link that got the CLICK."
  (interactive "e")
  (let ((estart (event-start click)))
    (select-window (posn-window estart))
    (skroad--do-link-action (posn-point estart))))

(defun skroad--cmd-enter-link ()
  "Perform the action attribute of the link at point."
  (interactive)
  (skroad--do-link-action (point)))

(skroad--define-text-type
 'skroad-link
 :doc "Fundamental type from which all skroad links are derived."
 :inherit 'skroad--default-type
 :inherit 'skroad-atomic
 :inherit 'skroad-not-title
 :face 'link
 :mouse-face 'highlight
 :keymap (define-keymap
           "<down-mouse-1>" #'skroad--cmd-left-click-link
           "RET" #'skroad--cmd-enter-link))

(defun skroad--link-to-plain-text ()
  "Delinkify the link under the point to plain text by removing delimiters."
  (interactive)
  (skroad--with-zone (point)
    (let ((text (skroad--atomic-at)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

;; TODO: preview linked node
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--atomic-at position)))

(skroad--define-text-type
 'skroad-node-link
 :doc "Fundamental type for skroad node links (live or dead)."
 :inherit 'skroad-link
 :help-echo 'skroad--link-mouseover
 :payload-regex "\\([^][\n\t\s]+[^][\n\t]*?\\)"
 :keymap (define-keymap
           "t" #'skroad--link-to-plain-text))

(defun skroad--live-link-to-dead ()
  "Transform all live links with payload LINK to dead links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all 'skroad-live link 'skroad-dead link)))

(defun skroad--browse-skroad-link (data)
  (message (format "Live link pushed: '%s'" data)))

(defun skroad--link-init (text-type payload)
  (message (format "Link init: type=%s payload='%s'" text-type payload)))

(defun skroad--link-create (text-type payload)
  (message (format "Link create: type=%s payload='%s'" text-type payload)))

(defun skroad--link-destroy (text-type payload)
  (message (format "Link destroy: type=%s payload='%s'" text-type payload)))

(skroad--define-text-type
 'skroad-live
 :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
 :inherit 'skroad-node-link
 :on-init #'skroad--link-init
 :on-create #'skroad--link-create
 :on-destroy #'skroad--link-destroy
 :on-activate #'skroad--browse-skroad-link
 :start-delim "[[" :end-delim "]]"
 :keymap (define-keymap
           "l" #'skroad--live-link-to-dead)
 :rendered t
 :indexed t)

(defun skroad--dead-link-to-live ()
  "Transform all dead links with payload LINK to live links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all 'skroad-dead link 'skroad-live link)))

(skroad--define-text-type
 'skroad-dead
 :doc "Dead (i.e. revivable placeholder) link to a skroad node."
 :inherit 'skroad-node-link
 :on-init #'skroad--link-init
 :on-create #'skroad--link-create
 :on-destroy #'skroad--link-destroy
 :start-delim "[-[" :end-delim "]-]"
 :face '(:inherit link :foreground "red")
 :keymap (define-keymap
           "l" #'skroad--dead-link-to-live)
 :rendered t
 :indexed t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--comment-url ()
  "Debuttonize the URL at point by inserting a space after the prefix."
  (interactive)
  (skroad--with-zone (point)
    (save-mark-and-excursion
      (goto-char start)
      (search-forward "//" end)
      (insert " "))))

(skroad--define-text-type
 'skroad-url-link
 :doc "URL."
 :inherit 'skroad-link
 :help-echo "External link."
 :payload-regex
 "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\t\s]+\\)"
 :on-activate #'browse-url
 :keymap (define-keymap
           "t" #'skroad--comment-url
           "l" #'skroad--comment-url)
 :rendered t
 :indexed t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--current-node-title "")

(defun skroad--title-init (text-type payload)
  (message (format "Title init: type=%s payload='%s'" text-type payload))
  (setq-local skroad--current-node-title payload)
  )

(defun skroad--title-create (text-type payload)
  (message (format "Title create: type=%s payload='%s'" text-type payload))
  (setq-local skroad--current-node-title payload)
  )

(defun skroad--title-destroy (text-type payload)
  (message (format "Title destroy: type=%s payload='%s'" text-type payload)))

;; TODO: proper moves, in case stop point is in a zone
(defun skroad--title-enter (&rest args)
  (when mark-active ;; If a region is active, prohibit moving into title.
    (goto-char (point-min))
    (goto-char (line-beginning-position 2)))) ;; stop right below the title.

(defun skroad--title-leave (&rest args)
  (when mark-active ;; If a region is active, prohibit moving out of title.
    (goto-char (point-min))
    (goto-char (line-end-position))))

(defun skroad--title-renderer (this)
  "Font lock rendering for title."
  (set-text-properties
   (match-beginning 0) (match-end 0)
   (list 'category this
         'id (gensym)
         'face (get this 'face))))

(defun skroad--find-next-title (regex limit)
  "Find next REGEX, up to LIMIT, but only when inside of the title line."
  (and (skroad--point-in-title-p)
       (re-search-forward regex (max limit (line-beginning-position 2)) t)))

(skroad--define-text-type
 'skroad-node-title
 :doc "Node title."
 :inherit 'skroad--default-type
 :finder #'skroad--find-next-title
 :renderer #'skroad--title-renderer
 :on-init #'skroad--title-init
 :on-create #'skroad--title-create
 :on-destroy #'skroad--title-destroy
 :on-enter #'skroad--title-enter
 :on-leave #'skroad--title-leave
 :face 'skroad--title-face
 :start-delim "" :end-delim "\n"
 :payload-regex "\\([^\n]+\\)"
 :keymap (define-keymap
           "RET" #'ignore)
 :rendered t
 :indexed t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--find-next-anywhere (regex limit)
  "Find next REGEX, up to LIMIT, anywhere (including in the node title.)"
  (re-search-forward regex limit t))

(defun skroad--decor-renderer (this)
  "Font lock rendering for decorations."
  (add-face-text-property (match-beginning 0) (match-end 0) (get this 'face)))

(skroad--define-text-type
 'skroad-decor
 :doc "Fundamental type for skroad text decorations."
 :inherit 'skroad--default-type
 :finder #'skroad--find-next-anywhere
 :renderer #'skroad--decor-renderer)

(skroad--define-text-type
 'skroad-decor-italic
 :doc "Italicized text."
 :inherit 'skroad-decor
 :face 'italic
 :start-delim "__" :end-delim "__"
 :payload-regex "\\([^_]+\\)"
 :rendered t)

(skroad--define-text-type
 'skroad-decor-bold
 :doc "Bold text."
 :inherit 'skroad-decor
 :face 'bold
 :start-delim "**" :end-delim "**"
 :payload-regex "\\([^*]+\\)"
 :rendered t)

(skroad--define-text-type
 'skroad-decor-heading
 :doc "Heading text."
 :inherit 'skroad-decor
 :face '(:weight bold :height 1.2 :inverse-video t)
 :start-delim "##" :end-delim "##"
 :payload-regex "\\([^#\n\t\s]+[^#\n\t]*?\\)"
 :rendered t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--index-update (index pending &optional init-scan)
  "Update INDEX by applying all PENDING changes, and run text type actions when
appropriate. If `INIT-SCAN` is t, run a text type's `on-init` rather than
`on-create` for created entries; `on-destroy` runs for destroyed ones."
  (maphash
   #'(lambda (key delta) ;; key and count delta in pending changes table
       (let* ((prior (or (gethash key index) 0)) ;; copies in index prior
              (create (zerop prior)) ;; t if index did not contain this item
              (count (+ prior delta)) ;; copies of item in index + delta
              (destroy (zerop count)) ;; t if change will destroy all copies
              (action ;; text type action to invoke, if any. nil if none.
               (cond (create (if init-scan 'on-init 'on-create))
                     (destroy (remhash key index) 'on-destroy))))
         (unless destroy (puthash key count index)) ;; update index if remains
         (let ((text-type (car key)) (payload (cdr key))) ;; args for action
           (skroad--type-action text-type action text-type payload))))
   pending)
  t)

(defun skroad--index-scan-region (changes start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START..END,
updating the hash table CHANGES, and `skroad--index-update` must be called on
it to finalize all pending changes when no further ones are expected."
  (dolist (text-type skroad--indexed-text-types) ;; try all indexed types
    (save-mark-and-excursion
      (goto-char start)
      (while (skroad--type-fn text-type 'find-next end)  ;; got match in region
        (let* ((payload (match-string-no-properties 1)) ;; item payload
               (key (cons text-type payload)) ;; key for changes table
               (count (+ delta (or (gethash key changes) 0)))) ;; inc or dec
          (if (zerop count) ;; if both added and removed since last update...
              (remhash key changes) ;; ...discard item from changes table.
            (puthash key count changes))))))) ;; otherwise update the count.

(defvar-local skroad--index nil "Text type index for current buffer.")
(defvar-local skroad--changes nil "Pending index changes for current buffer.")

(defun skroad--init-local-index ()
  "Create the buffer-local indices and populate them from current buffer."
  (unless (null skroad--index)
    (error "Text type index already exists for this buffer!"))
  (setq skroad--index (make-hash-table :test 'equal))
  ;; Populate while dispatching `on-init`s
  (let ((init-populate (make-hash-table :test 'equal)))
    (skroad--index-scan-region init-populate (point-min) (point-max) 1)
    (skroad--index-update skroad--index init-populate t)))

(defun skroad--update-local-index ()
  "Apply all pending changes queued for the buffer-local text type index."
  (when skroad--changes
    (skroad--index-update skroad--index skroad--changes)
    (setq skroad--changes nil)))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in a skroad buffer in region START...END."
  (when (null skroad--changes)
    (setq skroad--changes (make-hash-table :test 'equal)))
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--changes start-expanded end-expanded -1)))

(defvar-local skroad--text-changed nil
  "Whether buffer text has changed (incl. undo, i.e. buffer `not modified`)")

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  (setq-local skroad--text-changed t)
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--changes start-expanded end-expanded 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--prev-point (point-min) "Point before a command.")
(defvar-local skroad--prev-zone nil "Zone at prev point.")
(defvar-local skroad--prev-type nil "Text type at prev point.")

(defun skroad--point-save ()
  "Save previous point and properties."
  (setq-local skroad--prev-point (point))
  (setq-local skroad--prev-zone (skroad--zone-at))
  (setq-local skroad--prev-type (skroad--type-at)))

(defun skroad--zone-may-have-changed ()
  "To be called whenever the zone under the point may have changed."
  (let* ((p (point))
         (pos-moved (not (eq skroad--prev-point p)))
         (zone (skroad--zone-at p))
         (type (skroad--type-at p)))
    (cond ;; text type actions `on-leave` and `on-enter` may both fire
     ((and (or pos-moved skroad--text-changed) ;; point moved or text changed
           (not (eq skroad--prev-zone zone))) ;; and point changed zones
      (when skroad--prev-zone ;; point was in a zone, but has left it
        (skroad--type-action skroad--prev-type 'on-leave skroad--prev-point p))
      (when zone ;; point has entered a different zone
        (skroad--type-action type 'on-enter skroad--prev-point p)))
     ((and pos-moved skroad--prev-zone) ;; point moved, but remained in zone
      (skroad--type-action skroad--prev-type 'on-move skroad--prev-point p)))
    t))

(defun skroad--move-point (pos)
  "Move point to POS, triggering text type actions if necessary."
  (skroad--point-save)
  (goto-char pos)
  (skroad--zone-may-have-changed))

(defun skroad--adjust-mark-if-present ()
  (cond
   (mark-active
    (let ((m (mark)) (am skroad--alt-mark) (p (point)))
      (when (eq p m)
        (message "p=mark"))

      (when (eq p am)
        (message "p=altmark"))
      
      (when (and am (> (abs (- p am)) (abs (- p m))))
        (set-mark am)
        (setq-local skroad--alt-mark m)
        (message "swapped marks!")
        )))
   ))

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local mouse-highlight nil)
  (skroad--point-save))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (font-lock-ensure)
  (skroad--zone-may-have-changed)
  (skroad--adjust-mark-if-present) ;; swap mark and alt-mark if needed
  (skroad--update-local-index) ;; TODO: do it in save hook?
  (setq-local skroad--text-changed nil)
  (unless (use-region-p) (setq-local mouse-highlight t)))

(defun skroad--scroll-hook (window start)
  "Triggers when a buffer scrolls."
  (setq-local header-line-format ;; Float the title if it isn't in view
              (when (and skroad--floating-title-enable (> start 1))
                (buffer-substring (point-min)
                                  (skroad--get-end-of-line 1)))))

(defadvice skroad--post-command-hook (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((atomic (skroad--atomic-at pos))
          (fwd (<= pos limit)))
      (cond ((and atomic fwd) (goto-char (skroad--zone-end pos)))
            (atomic (goto-char (skroad--zone-start pos)))
            (fwd (forward-word-strictly))
            (t (backward-word-strictly)))
      (point))))

(defconst skroad--find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad--find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--open-node ()
  "Open a skroad node."
  (skroad--init-font-lock)
  (font-lock-ensure)
  (skroad--init-local-index)
  (face-remap-set-base 'header-line 'skroad--title-face)
  )

(define-derived-mode skroad-mode text-mode "Skroad"
  ;; Prohibit change hooks firing when only text properties have changed:
  (skroad--silence-modifications 'put-text-property)
  (skroad--silence-modifications 'add-text-properties)
  (skroad--silence-modifications 'remove-text-properties)
  (skroad--silence-modifications 'remove-list-of-text-properties)
  (skroad--silence-modifications 'set-text-properties)
  (skroad--silence-modifications 'add-face-text-property)
  
  ;; Zap properties during unfontification:
  (setq-local font-lock-extra-managed-props skroad--text-properties)
  
  ;; Zap properties and refontify during yank.
  ;; TODO: does this need with-silent-modifications for textmode temp buffers
  ;;       where skroad--silence-modifications is not in effect?
  (setq-local yank-handled-properties
              '((button . (lambda (category start end)
                            (remove-list-of-text-properties
                             start end skroad--text-properties)
                            (skroad--with-whole-lines
                              start end
                              (font-lock-ensure
                               start-expanded end-expanded))
                            ))))
  ;; TODO: not button ^ ??

  ;; Buffer-local hooks:
  (add-hook 'before-change-functions 'skroad--before-change-function nil t)
  (add-hook 'after-change-functions 'skroad--after-change-function nil t)
  (add-hook 'pre-command-hook 'skroad--pre-command-hook nil t)
  (add-hook 'post-command-hook 'skroad--post-command-hook nil t)
  (add-hook 'window-scroll-functions 'skroad--scroll-hook nil t)
  
  ;; Overlay for when a link is under the point. Initially inactive:
  (setq-local skroad--selector (make-overlay (point-min) (point-min)))
  (skroad--selector-deactivate)

  ;; Properties for selector overlay
  (dolist (p skroad--selector-properties)
    (overlay-put skroad--selector (car p) (cadr p)))

  ;; Keymap:
  (use-local-map skroad--mode-keymap)

  ;; Handle word boundaries correctly (links are treated as unitary words) :
  (setq-local find-word-boundary-function-table
              skroad--find-word-boundary-function-table)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
