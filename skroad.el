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

(defmacro skroad--with-sym-props (sym properties &rest body)
  "Bind each of the given SYM's PROPERTIES -- e.g. (foo bar ...)
as (foo (get NAME foo)) etc., and evaluate BODY."
  (declare (indent defun))
  `(let ,(mapcar
          #'(lambda (p)
              (cons (read (symbol-name p))
                    `((get ,sym ',p))))
          properties)
     (progn
       ,@body)))

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
    (when pos
      (goto-char pos))
    (let ((r (funcall
              (cond ((eq direction :forward) #'text-property-search-forward)
                    ((eq direction :backward) #'text-property-search-backward)
                    (t (error "Invalid direction: %s !" direction)))
              prop (get-text-property (point) prop)
              #'(lambda (oldval newval)
                  (and newval (not (eq oldval newval)))))))
      (if r (prop-match-beginning r)))))

(defun skroad--call-text-type-action-if-defined
    (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name
    (let ((action (get text-type action-name)))
      (when action
        (apply action args)))))

(defun skroad--point-in-title-p ()
  "Returns t if point is in the first line of the buffer, otherwise nil."
  (eq (line-beginning-position) (point-min)))

(defun skroad--pos-in-title-p (pos)
  "Returns t if POS is in the first line of the buffer, otherwise nil."
  (save-mark-and-excursion
    (goto-char pos)
    (skroad--point-in-title-p)))

(defun skroad--find-next-nontitle (regex limit)
  "Find next REGEX, up to LIMIT, but only outside of the title line."
  (when (skroad--point-in-title-p)
    (goto-char (line-beginning-position 2)))
  (re-search-forward regex
                     (if (< (point) limit)
                         limit
                       (skroad--get-end-of-line (point)))
                     t))

(defun skroad--find-next-title (regex limit)
  "Find next REGEX, up to LIMIT, but only inside of the title line."
  (and (skroad--point-in-title-p)
       (re-search-forward regex
                          (max limit (line-beginning-position 2))
                          t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface skroad--text '((t :inherit default))
  "Default face used for skrode text types."
  :group 'basic-faces)

;; Default properties for skroad text types.
(put 'default-skroad-text 'face 'skroad--text)

;; Prevent insertions adjacent to skroad text from inheriting its properties.
(put 'default-skroad-text 'rear-nonsticky t)

;; Default delimiters are null strings:
(put 'default-skroad-text 'start-delim "")
(put 'default-skroad-text 'end-delim "")

(defvar skroad--displayed-text-types nil "Text types for use with font-lock.")
(defvar skroad--indexed-text-types nil "Text types that are indexed.")

(defun skroad--define-text-type (name &rest properties)
  (let ((super (or (plist-get properties 'supertype)
	           (plist-get properties :supertype)
	           'default-skroad-text)))
    ;; Inherit properties from supertype (but not keyworded ones) :
    (skroad--do-plist prop val (symbol-plist super)
      (unless (keywordp prop)
        (put name prop val)))
    
    ;; Add the properties in PROPERTIES to the symbol:
    (skroad--do-plist prop val properties
      (cond
       ((eq prop :keymap)
        (let ((parent-keymap (get name 'keymap)))
          (when parent-keymap
            (set-keymap-parent val parent-keymap))
          (put name 'keymap val)))
       (t (put name (skroad--keyword-to-symbol prop) val))))
    
    ;; Make sure there's a `supertype' property.
    (unless (get name 'supertype)
      (put name 'supertype 'default-skroad-text))

    ;; Generate certain properties for displayed and indexed types:
    (when (get name 'displayed)
      (skroad--with-sym-props name
        (start-delim payload-regex end-delim
                     title indexed decorative face atomic)
        (unless payload-regex
          (error "A displayed text type must define payload-regex!"))
        (let* ((make-text
                (lambda (payload)
                  (concat start-delim payload end-delim)))
               (start-regex (concat (regexp-quote start-delim) "\s*"))
               (end-regex (concat "\s*" (regexp-quote end-delim)))
               
               (make-regex
                (lambda (&optional payload)
                  (concat start-regex
                          (or payload payload-regex)
                          end-regex)))
               (finder
                (if title
                    #'skroad--find-next-title
                  #'skroad--find-next-nontitle))
               (find-next
                (lambda (limit &optional payload)
                  (let ((regex (funcall make-regex payload)))
                    (funcall finder regex limit))))
               (font-lock-colorizer
                (cond (decorative
                       (lambda (start end)
                         (add-face-text-property start end face t)))
                      (atomic
                       (lambda (start end)
                         (set-text-properties
                          start end
                          (list 'category name
                                'id (gensym)
                                'face face
                                'data (match-string-no-properties 1)))))
                      (t
                       (lambda (start end)
                         (set-text-properties
                          start end
                          (list 'category name
                                'face face))))))
               (font-lock-matcher
                (lambda (limit)
                  (when (funcall find-next limit)
                    (with-silent-modifications
                      (funcall font-lock-colorizer
                               (match-beginning 0) (match-end 0))
                      t))))
               (font-lock-rule
                (list font-lock-matcher '(0 nil append))))
          
          (put name :make-text make-text)
          (put name :find-next find-next)
          (put name :font-lock-rule font-lock-rule)

          (add-to-list 'skroad--displayed-text-types name)
          (when indexed
            (add-to-list 'skroad--indexed-text-types name)))))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: return t if any replacements happened
(defun skroad--text-type-replace-all
    (text-type-old payload-old text-type-new payload-new)
  "Replace all text of TEXT-TYPE-OLD having PAYLOAD-OLD with
instances of TEXT-TYPE-NEW having PAYLOAD-NEW."
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (funcall (get text-type-old :find-next) (point-max) payload-old)
      (replace-match (funcall (get text-type-new :make-text) payload-new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--atomic-at (pos)
  "Get the payload of the atomic found at the given POS, or nil if none."
  (get-text-property pos 'data))

(defun skroad--atomic-at-prev (pos)
  "Determine whether there is an atomic at the position prior to POS."
  (and (> pos (point-min)) (skroad--atomic-at (1- pos))))

(defun skroad--atomic-start (pos)
  "Return the position at which the atomic segment at POS starts."
  (or (previous-single-property-change (1+ pos) 'id)
      (point-min)))

(defun skroad--atomic-end (pos)
  "Return the position at which the atomic segment at POS ends."
  (or (next-single-property-change pos 'id)
      (point-max)))

;; (defun skroad--pos-of-type-p (pos text-type)
;;   "Determine whether POS is on text of the given TEXT-TYPE."
;;   (eq (get-text-property pos 'category) text-type))

(defmacro skroad--with-atomic-at-point (&rest body)
  "Evaluate BODY with link bound to the link under the point."
  `(let ((link (skroad--atomic-at (point))))
     (when link
       ,@body)))

(defun skroad--cmd-backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
          ((skroad--atomic-at-prev p) (delete-region (skroad--atomic-start (1- p)) p))
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

(defun skroad--cmd-atomics-prepend-space ()
  "Insert a space immediately behind the atomic currently under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (skroad--atomic-start (point)))
    (insert " ")))

(defmacro skroad--define-atomics-region-cmd (wrap-command)
  "Wrap COMMAND to use region if exists, or use the atomic at point as region."
  `(defun ,(read (concat "skroad--cmd-atomics-"
                         (symbol-name wrap-command))) ()
     (interactive)
     (apply #',wrap-command
            (if (use-region-p)
                (list (region-beginning) (region-end))
              (list (skroad--atomic-start (point))
                    (skroad--atomic-end (point)))))))

(skroad--define-atomics-region-cmd delete-region)
(skroad--define-atomics-region-cmd kill-region)
(skroad--define-atomics-region-cmd kill-ring-save)

(skroad--define-text-type
 'skroad-atomic
 :doc "Selected, clicked, killed, etc. as units. Point enters only first pos."
 :atomic t
 :keymap
 (define-keymap
   "SPC" #'skroad--cmd-atomics-prepend-space
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
  "Perform the action attribute of the link at POS, if one was defined."
  (skroad--call-text-type-action-if-defined
   (get-text-property pos 'category)
   'link-action
   (get-text-property pos 'data)))

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
 :supertype 'skroad-atomic
 :face 'link
 :mouse-face 'highlight
 :keymap (define-keymap
           "<down-mouse-1>" #'skroad--cmd-left-click-link
           "RET" #'skroad--cmd-enter-link))

(defun skroad--link-to-plain-text ()
  "Delinkify the link under the point to plain text by removing delimiters."
  (interactive)
  (let* ((p (point))
         (start (skroad--atomic-start p))
         (end (skroad--atomic-end p))
         (text (skroad--atomic-at p)))
    (save-mark-and-excursion
      (goto-char start)
      (delete-region start end)
      (insert text))))

;; TODO: preview linked node
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--atomic-at position)))

(skroad--define-text-type
 'skroad-node-link
 :doc "Fundamental type for skroad node links (live or dead)."
 :supertype 'skroad-link
 :help-echo 'skroad--link-mouseover
 :payload-regex "\\([^][\n\t\s]+[^][\n\t]*?\\)"
 :keymap (define-keymap
           "t" #'skroad--link-to-plain-text))

(defun skroad--live-link-to-dead ()
  "Transform all live links with payload LINK to dead links."
  (interactive)
  (skroad--with-atomic-at-point
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
 :supertype 'skroad-node-link
 :displayed t
 :indexed t
 :init-action #'skroad--link-init
 :create-action #'skroad--link-create
 :destroy-action #'skroad--link-destroy
 :start-delim "[[" :end-delim "]]"
 :link-action #'skroad--browse-skroad-link
 :keymap (define-keymap
           "l" #'skroad--live-link-to-dead))

(defun skroad--dead-link-to-live ()
  "Transform all dead links with payload LINK to live links."
  (interactive)
  (skroad--with-atomic-at-point
   (skroad--text-type-replace-all 'skroad-dead link 'skroad-live link)))

(skroad--define-text-type
 'skroad-dead
 :doc "Dead (i.e. revivable placeholder) link to a skroad node."
 :supertype 'skroad-node-link
 :displayed t
 :indexed t
 :init-action #'skroad--link-init
 :create-action #'skroad--link-create
 :destroy-action #'skroad--link-destroy
 :start-delim "[-[" :end-delim "]-]"
 :face '(:inherit link :foreground "red")
 :keymap (define-keymap
           "l" #'skroad--dead-link-to-live))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--comment-url ()
  "Debuttonize the URL at point by inserting a space after the prefix."
  (interactive)
  (skroad--with-atomic-at-point
   (save-mark-and-excursion
     (goto-char (skroad--atomic-start (point)))
     (search-forward "//" (skroad--atomic-end (point)))
     (insert " "))))

(skroad--define-text-type
 'skroad-url-link
 :doc "URL."
 :supertype 'skroad-link
 :displayed t
 :indexed t
 :help-echo "External link."
 :payload-regex
 "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\t\s]+\\)"
 :link-action #'browse-url
 :keymap (define-keymap
           "t" #'skroad--comment-url
           "l" #'skroad--comment-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--title-init (text-type payload)
  (message (format "Title init: type=%s payload='%s'" text-type payload)))

(defun skroad--title-create (text-type payload)
  (message (format "Title create: type=%s payload='%s'" text-type payload)))

(defun skroad--title-destroy (text-type payload)
  (message (format "Title destroy: type=%s payload='%s'" text-type payload)))

(skroad--define-text-type
 'skroad-node-title
 :doc "Node title."
 :title t
 :displayed t
 :indexed t
 :init-action #'skroad--title-init
 :create-action #'skroad--title-create
 :destroy-action #'skroad--title-destroy
 :face '(:weight bold :foreground "purple"
                 :height 1.5 :inverse-video t :extend t)
 :start-delim "" :end-delim "\n"
 :payload-regex "\\([^\n]+\\)"
 :keymap (define-keymap
           "RET" #'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--define-text-type
 'skroad-italic
 :doc "Italicized text."
 :decorative t
 :displayed t
 :face 'italic
 :start-delim "__" :end-delim "__"
 :payload-regex "\\([^_]+\\)")

(skroad--define-text-type
 'skroad-bold
 :doc "Bold text."
 :decorative t
 :displayed t
 :face 'bold
 :start-delim "**" :end-delim "**"
 :payload-regex "\\([^*]+\\)")

(skroad--define-text-type
 'skroad-heading
 :doc "Heading text."
 :decorative t
 :displayed t
 :face '(:weight bold :height 1.2 :inverse-video t)
 :start-delim "##" :end-delim "##"
 :payload-regex "\\([^#\n\t\s]+[^#\n\t]*?\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--index-update (index pending &optional init-scan)
  "Update INDEX by applying all PENDING changes, and run text type actions when
appropriate. If `INIT-SCAN` is t, run a text type's `init-action` rather than
`create-action` for created entries; `destroy-action` runs for destroyed ones."
  (maphash
   #'(lambda (key delta) ;; key and count delta in pending changes table
       (let* ((prior (or (gethash key index) 0)) ;; copies in index prior
              (create (zerop prior)) ;; t if index did not contain this item
              (count (+ prior delta)) ;; copies of item in index + delta
              (destroy (zerop count)) ;; t if change will destroy all copies
              (action ;; text type action to invoke, if any. nil if none.
               (cond (create (if init-scan 'init-action 'create-action))
                     (destroy (remhash key index) 'destroy-action))))
         (unless destroy (puthash key count index)) ;; update index if remains
         (let ((text-type (car key)) (payload (cdr key))) ;; args for action
           (skroad--call-text-type-action-if-defined ;; invoke action, if any
            text-type
            action text-type payload))))
   pending)
  t)

(defun skroad--index-scan-region (changes start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START..END,
updating the hash table CHANGES, and `skroad--index-update` must be called on
it to finalize all pending changes when no further ones are expected."
  (dolist (text-type skroad--indexed-text-types) ;; try all indexed types
    (save-mark-and-excursion
      (goto-char start)
      (while (funcall (get text-type :find-next) end) ;; got match in region
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
  ;; Populate while dispatching `init-action`s
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

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--changes start-expanded end-expanded 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--prev-point (point-min)
  "Point prior to the current command.")

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local skroad--prev-point (point)))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (font-lock-ensure)
  (skroad--update-local-index) ;; TODO: do it in save hook?
  (skroad--update-selector)
  (skroad--adjust-mark-if-present)
  )

(defadvice skroad--post-command-hook (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

(defvar-local skroad--selector nil
  "Overlay active when a link is under the point.")

(defconst skroad--selector-properties
  `((face highlight)
    (evaporate t))
  "Text properties of the selector.")

(defun skroad--selector-activate (start end)
  "Activate (if inactive) or move the selector from START...END."
  (move-overlay skroad--selector start end (current-buffer))
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

(defun skroad--region-selection-active-p ()
  "Return t if a region selection is active (even if length 0); otherwise nil."
  (or (use-region-p) skroad--alt-mark))

;; !!!!TODO: prev-point may be above point-max!!!
(defun skroad--update-selector ()
  "Update the selector, because the point,
the text under the point, or both, may have changed."
  (let* ((p (point)))
    ;; If there is a link under the point, we may have to bounce the point:
    (when (skroad--atomic-at p)
      ;; Moved into link from outside of it, or tried to move left inside it:
      (cond ((or (not (eq (skroad--atomic-at p)
                          (skroad--atomic-at skroad--prev-point)))
                 (< p skroad--prev-point))
             ;; Go to start of link.
             (goto-char (skroad--atomic-start p)))
            ;; If tried to move right from anywhere:
            ((> p skroad--prev-point)
             ;; Go to end of link.
             (goto-char (skroad--atomic-end p))))))

  ;; If a region is active, point may not cross title boundary:
  (let* ((was-in-title (skroad--pos-in-title-p skroad--prev-point))
         (now-in-title (skroad--point-in-title-p)))
    (when (and (skroad--region-selection-active-p)
               (not (eq was-in-title now-in-title)))
      (if was-in-title
          (goto-char skroad--prev-point)
        (progn
          (goto-char (point-min))
          (goto-char (line-beginning-position 2))))))
  
  ;; Point may have moved. Enable selector iff on top of a link:
  (let ((p (point)))
    (if (and (skroad--atomic-at p)
             (not (skroad--region-selection-active-p)))
        (skroad--selector-activate
         (skroad--atomic-start p) (skroad--atomic-end p))
      (skroad--selector-deactivate))))

(defun skroad--adjust-mark-if-present ()
  "Mark and alt-mark may swap places so that link remains in the region."
  (let ((m (mark))
        (am skroad--alt-mark))
    (when (and m am)
      (let* ((left-mark (min m am))
             (right-mark (max m am))
             (p (point))
             (new-marks (cond
                         ((> p m) (cons left-mark right-mark))
                         ((< p m) (cons right-mark left-mark))
                         (t (cons am m)))))
        (set-mark (car new-marks))
        (setq-local skroad--alt-mark (cdr new-marks))))))

(defun skroad--activate-mark-hook ()
  "Triggers when the mark is activated or reactivated."
  ;; Turn off mouse highlights and selector:
  (setq-local mouse-highlight nil)
  (skroad--selector-deactivate)
  (let ((m (mark)))
    ;; When mark is set in a link, set mark to link end and alt-mark to start:
    (when (skroad--atomic-at m)
      (set-mark (skroad--atomic-end m))
      (setq-local skroad--alt-mark (skroad--atomic-start m))
      (message "mark adjusted")
      )))

(defun skroad--deactivate-mark-hook ()
  "Triggers when the mark becomes inactive."
  (setq-local skroad--alt-mark nil)
  (skroad--update-selector)
  (setq-local mouse-highlight t)
  )

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((atomic (skroad--atomic-at pos))
          (fwd (<= pos limit)))
      (cond ((and atomic fwd) (goto-char (skroad--atomic-end pos)))
            (atomic (goto-char (skroad--atomic-start pos)))
            (fwd (forward-word-strictly))
            (t (backward-word-strictly)))
      (point))))

(defconst skroad-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad--find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--init-font-lock ()
  "Enable font-lock for skroad mode."
  (let ((keywords nil))
    (dolist (type skroad--displayed-text-types)
      (push (get type :font-lock-rule) keywords))
    (font-lock-add-keywords nil keywords t)))

(defun skroad--open-node ()
  "Open a skroad node."
  (skroad--init-font-lock)
  (font-lock-ensure)
  (skroad--init-local-index)
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
  (add-hook 'activate-mark-hook 'skroad--activate-mark-hook nil t)
  (add-hook 'deactivate-mark-hook 'skroad--deactivate-mark-hook nil t)

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
              skroad-find-word-boundary-function-table)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
