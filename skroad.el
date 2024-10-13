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

(defun skroad--delete-once-helper (elt seq)
  "Return (V . SEQprime) where SEQprime is SEQ after destructively removing
at most one instance of ELT. If something was removed, V is T, otherwise nil."
  (if (equal elt (car seq))
      (cons t (cdr seq))
    (let ((l seq))
      (while (and (cdr seq)
                  (not (equal elt (cadr seq))))
        (setq seq (cdr seq)))
      (cond ((cdr seq)
             (setcdr seq (cddr seq))
             (cons t l))
            (t (cons nil l))))))

(defmacro skroad--delete-once (elt seq)
  "Destructively remove at most one instance of ELT from SEQ.
If something was removed, returns T, otherwise nil."
  (let ((tempvar (gensym)))
    `(let ((,tempvar (skroad--delete-once-helper ,elt ,seq)))
       (setq ,seq (cdr ,tempvar))
       (car ,tempvar))))

;; TODO: comb over seq1 properly with setcdr rather than using delete-once
;; TODO: maybe use hash tables and other speedups here
(defmacro skroad--delete-intersection (seq1 seq2)
  "Destructively remove all elements common to SEQ1 and SEQ2 from both."
  (let ((l (gensym))
        (current (gensym)))
    `(progn
       (setq ,l (copy-list ,seq1))
       (while (and ,l ,seq2)
         (setq ,current (car ,l))
         (when (skroad--delete-once ,current ,seq2)
           (skroad--delete-once ,current ,seq1))
         (setq ,l (cdr ,l))))))

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

(defun skroad--point-in-title-p ()
  "Returns t if point is in the first line of the buffer, otherwise nil."
  (eq (line-beginning-position) (point-min)))

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
        (start-delim payload-regex end-delim title link indexed)
        (unless payload-regex
          (error "A displayed text type must define payload-regex!"))
        (let* ((make-text
                (lambda (payload)
                  (concat start-delim payload end-delim)))
               (start-regex (regexp-quote start-delim))
               (end-regex (regexp-quote end-delim))
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
               (font-lock-matcher
                (lambda (limit)
                  (when (funcall find-next limit)
                    (let ((all-types-props
                           (list 'category name
                                 'id (gensym)))
                          (additional-props
                           (if link
                               (list
                                'data (match-string-no-properties 1)))))
                      (with-silent-modifications
                        (add-text-properties
                         (match-beginning 0) (match-end 0)
                         (append all-types-props additional-props))
                        t)))))
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
    (type-name-old payload-old type-name-new payload-new)
  "Replace all text of TYPE-NAME-OLD having PAYLOAD-OLD with
instances of TYPE-NAME-NEW having PAYLOAD-NEW."
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (funcall
            (get type-name-old :find-next)
            (point-max) payload-old)
      (replace-match
       (funcall
        (get type-name-new :make-text)
        payload-new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--link-at (pos)
  "Get the payload of the link found at the given POS, or nil if none."
  (get-text-property pos 'data))

(defun skroad--link-at-prev (pos)
  "Determine whether there is a link at the position prior to POS."
  (and (> pos (point-min)) (skroad--link-at (1- pos))))

(defun skroad--tt-start (pos)
  "Return the position at which the text type segment at POS starts."
  (or (previous-single-property-change (1+ pos) 'id)
      (point-min)))

(defun skroad--tt-end (pos)
  "Return the position at which the text type segment at POS ends."
  (or (next-single-property-change pos 'id)
      (point-max)))

(defun skroad--pos-of-type-p (pos text-type)
  "Determine whether POS is on text of the given TEXT-TYPE."
  (eq (get-text-property pos 'category) text-type))

(defmacro skroad--with-link-at-point (&rest body)
  "Evaluate BODY with link bound to the link under the point."
  `(let ((link (skroad--link-at (point))))
     (when link
       ,@body)))

(defmacro skroad--make-link-region-cmd (command)
  "Wrap COMMAND to use region if one exists, or use link at point as region."
  `#'(lambda ()
       (interactive)
       (apply #',command
              (if (use-region-p)
                  (list (region-beginning) (region-end))
                (list (skroad--tt-start (point))
                      (skroad--tt-end (point)))))))

(defun skroad--backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
          ((skroad--link-at-prev p) (delete-region (skroad--tt-start (1- p)) p))
          (t (delete-char -1)))))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--backspace)
  "Keymap for skroad mode.")

;;; Text Types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--text-properties
  '(category face font-lock-face id data)
  "Properties added by font-lock that must be removed when unfontifying.")

;; Text types are defined in order of ascending (lowest -- first) priority:

(skroad--define-text-type
 'skroad-italic
 :doc "Italicized text."
 :displayed t
 :face '(:slant italic)
 :start-delim "__" :end-delim "__"
 :payload-regex "\\([^_]+\\)")

(skroad--define-text-type
 'skroad-bold
 :doc "Bold text."
 :displayed t
 :face '(:weight bold)
 :start-delim "**" :end-delim "**"
 :payload-regex "\\([^*]+\\)")

(skroad--define-text-type
 'skroad-heading
 :doc "Heading text."
 :displayed t
 :face '(:weight bold :height 1.2 :inverse-video t)
 :start-delim "" :end-delim "\n"
 :payload-regex "^##\\([^#\n]+\\)")

(defun skroad--link-insert-space ()
  "Insert a space immediately behind the link under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (skroad--tt-start (point)))
    (insert " ")))

(skroad--define-text-type
 'skroad-button
 :doc "Fundamental type from which all skroad links are derived."
 :link t
 :face 'link
 :mouse-face 'highlight
 :keymap (define-keymap
           "SPC" #'skroad--link-insert-space
           "<remap> <self-insert-command>" #'ignore
           "<deletechar>" (skroad--make-link-region-cmd delete-region)
           "<backspace>" (skroad--make-link-region-cmd delete-region)
           "<mouse-1>" #'ignore
           "<mouse-2>" #'ignore
           "<mouse-3>" #'ignore
           "RET" #'ignore
           "<remap> <kill-region>" (skroad--make-link-region-cmd kill-region)
           "<remap> <kill-ring-save>" (skroad--make-link-region-cmd kill-ring-save)
           ))

(defun skroad--link-to-plain-text ()
  "Debuttonize the link under the point to plain text by removing delimiters."
  (interactive)
  (let* ((p (point))
         (start (skroad--tt-start p))
         (end (skroad--tt-end p))
         (text (skroad--link-at p)))
    (save-mark-and-excursion
      (goto-char start)
      (delete-region start end)
      (insert text))))

;; TODO: preview linked node
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--link-at position)))

(skroad--define-text-type
 'skroad-node-link
 :doc "Fundamental type for skroad node links (live or dead)."
 :supertype 'skroad-button
 :help-echo 'skroad--link-mouseover
 :payload-regex "\s*\\([^][\n\t\s]+[^][\n\t]*?\\)\s*"
 :keymap (define-keymap
           "t" #'skroad--link-to-plain-text))

(defun skroad--live-link-to-dead ()
  "Transform all live links with payload LINK to dead links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all 'skroad-live link 'skroad-dead link)))

(defun skroad--go-to-live-link ()
  "Navigate to the live skroad link at point."
  (interactive)
  (skroad--with-link-at-point
   (message (format "Live link pushed: '%s'" link))))

(skroad--define-text-type
 'skroad-live
 :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
 :supertype 'skroad-node-link
 :displayed t
 :indexed t
 :start-delim "[[" :end-delim "]]"
 :keymap (define-keymap
           "RET" #'skroad--go-to-live-link
           "<mouse-1>" #'skroad--go-to-live-link
           "l" #'skroad--live-link-to-dead))

(defun skroad--dead-link-to-live ()
  "Transform all dead links with payload LINK to live links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all 'skroad-dead link 'skroad-live link)))

(skroad--define-text-type
 'skroad-dead
 :doc "Dead (i.e. revivable placeholder) link to a skroad node."
 :supertype 'skroad-node-link
 :displayed t
 :indexed t
 :start-delim "[-[" :end-delim "]-]"
 :face '(:inherit link :foreground "red")
 :keymap (define-keymap
           "l" #'skroad--dead-link-to-live))

(defun skroad--go-to-url ()
  "Navigate to the URL at point."
  (interactive)
  (skroad--with-link-at-point
   (browse-url link)))

(defun skroad--comment-url ()
  "Debuttonize the URL at point by inserting a space after the prefix."
  (interactive)
  (skroad--with-link-at-point
   (save-mark-and-excursion
     (goto-char (skroad--tt-start (point)))
     (search-forward "//" (skroad--tt-end (point)))
     (insert " "))))

(skroad--define-text-type
 'skroad-url-link
 :doc "URL."
 :supertype 'skroad-button
 :displayed t
 :help-echo "External link."
 :payload-regex
 "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\t\s]+\\)"
 :keymap (define-keymap
           "t" #'skroad--comment-url
           "RET" #'skroad--go-to-url
           "<mouse-1>" #'skroad--go-to-url))

(skroad--define-text-type
 'skroad-node-title
 :doc "Node title."
 :indexed t
 :displayed t
 :title t
 :face '(:weight bold :foreground "purple"
                 :height 1.5 :inverse-video t :extend t)
 :start-delim "" :end-delim "\n"
 :payload-regex "\\([^\n]+\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--get-tracked-in-region (start end)
  "Find all tracked text type instances in region START...END."
  (let ((tracked nil))
    (skroad--with-whole-lines start end
      (dolist (type skroad--indexed-text-types)
        (save-mark-and-excursion
          (goto-char start-expanded)
          (while (funcall (get type :find-next) end-expanded)
            (push (cons type (match-string-no-properties 1))
                  tracked)))))
    tracked
    ))

(defvar-local skroad--tracked-propose-create nil
  "Tracked items (may be dupes) introduced to the buffer by a command.")

(defvar-local skroad--tracked-propose-destroy nil
  "Tracked items removed from the buffer by a command; dupes may remain.")

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in a skroad buffer in region START...END."
  (let ((tracked (skroad--get-tracked-in-region start end)))
    (skroad--delete-intersection skroad--tracked-propose-create tracked)
    (setq-local skroad--tracked-propose-destroy
                (nconc skroad--tracked-propose-destroy tracked))))

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  (let ((tracked (skroad--get-tracked-in-region start end)))
    (skroad--delete-intersection skroad--tracked-propose-destroy tracked)
    (setq-local skroad--tracked-propose-create
                (nconc skroad--tracked-propose-create tracked))))

(defun skroad--sync-proposed-tracked-changes ()
  "Process proposed create/destroy of tracked text type items."
  (when skroad--tracked-propose-destroy
    (message (format "proposed destroy: %s"
                     skroad--tracked-propose-destroy))
    (setq-local skroad--tracked-propose-destroy nil)
    )
  (when skroad--tracked-propose-create
    (message (format "proposed create: %s"
                     skroad--tracked-propose-create))
    (setq-local skroad--tracked-propose-create nil)
    )
  )

(defvar-local skroad--prev-point (point-min)
  "Point prior to the current command.")

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local skroad--prev-point (point)))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (font-lock-ensure)
  (skroad--sync-proposed-tracked-changes)
  (skroad--update-current-link)
  (skroad--adjust-mark-if-present)
  )

(defvar-local skroad--current-link-overlay nil
  "Overlay active when a link is under the point.")

(defconst skroad--current-link-overlay-properties
  `((face highlight)
    (evaporate t))
  "Text properties of the current link overlay.")

(defun skroad--current-link-overlay-activate (start end)
  "Activate (if inactive) or move the current link overlay from START...END."
  (move-overlay skroad--current-link-overlay start end (current-buffer))
  ;; (setq-local cursor-type nil)
  )

(defun skroad--current-link-overlay-deactivate ()
  "Deactivate the current link overlay; it can be reactivated again."
  (when (skroad--current-link-overlay-active-p)
    (delete-overlay skroad--current-link-overlay))
  ;; (setq-local cursor-type t)
  )

(defun skroad--current-link-overlay-active-p ()
  "Return t if the current link overlay is active; otherwise nil."
  (and (overlayp skroad--current-link-overlay)
       (eq (current-buffer)
           (overlay-buffer skroad--current-link-overlay))))

(defvar-local skroad--alt-mark nil
  "Opposite end of a link in which the mark had been set.")

(defun skroad--region-selection-active-p ()
  "Return t if a region selection is active (even if length 0); otherwise nil."
  (or (use-region-p) skroad--alt-mark))

(defun skroad--update-current-link ()
  "Update the current link overlay, because the point,
the text under the point, or both, may have changed."
  (let* ((p (point)))
    ;; If there is a link under the point, we may have to bounce the point:
    (when (skroad--link-at p)
      ;; Moved into link from outside of it, or tried to move left inside it:
      (cond ((or (not (eq (skroad--link-at p)
                          (skroad--link-at skroad--prev-point)))
                 (< p skroad--prev-point))
             ;; Go to start of link.
             (goto-char (skroad--tt-start p)))
            ;; If tried to move right from anywhere:
            ((> p skroad--prev-point)
             ;; Go to end of link.
             (goto-char (skroad--tt-end p))))))

  ;; If a region is active, point may not cross title boundary:
  (let* ((p (point))
         (was-in-title (skroad--pos-of-type-p
                        skroad--prev-point 'skroad-node-title))
         (now-in-title (skroad--pos-of-type-p
                        p 'skroad-node-title)))
    (when (and (skroad--region-selection-active-p)
               (not (eq was-in-title now-in-title)))
      (if was-in-title
          (goto-char skroad--prev-point)
        (progn
          (goto-char (point-min))
          (goto-char (line-beginning-position 2))))))
  
  ;; Point may have moved. Enable current link overlay iff on top of a link:
  (let ((p (point)))
    (if (and (skroad--link-at p)
             (not (skroad--region-selection-active-p)))
        (skroad--current-link-overlay-activate
         (skroad--tt-start p) (skroad--tt-end p))
      (skroad--current-link-overlay-deactivate))))

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
  ;; Turn off mouse highlights and current link overlay:
  (setq-local mouse-highlight nil)
  (skroad--current-link-overlay-deactivate)
  (let ((m (mark)))
    ;; When mark is set in a link, set mark to link end and alt-mark to start:
    (when (skroad--link-at m)
      (set-mark (skroad--tt-end m))
      (setq-local skroad--alt-mark (skroad--tt-start m))
      (message "mark adjusted")
      )))

(defun skroad--deactivate-mark-hook ()
  "Triggers when the mark becomes inactive."
  (setq-local skroad--alt-mark nil)
  (skroad--update-current-link)
  ;; Turn on mouse highlight
  (setq-local mouse-highlight t)
  )

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((link (skroad--link-at pos))
          (fwd (<= pos limit)))
      (cond ((and link fwd) (goto-char (skroad--tt-end pos)))
            (link (goto-char (skroad--tt-start pos)))
            (fwd (forward-word-strictly))
            (t (backward-word-strictly)))
      (point))))

(defconst skroad-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad--find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--font-lock-turn-on ()
  "Enable font-lock for skroad mode."
  (let ((keywords nil))
    (dolist (type skroad--displayed-text-types)
      (push (get type :font-lock-rule) keywords))
    (font-lock-add-keywords nil keywords t)))

(defun skroad--open-node ()
  "Open a skroad node."
  ;; (button-mode)
  (skroad--font-lock-turn-on)
  (font-lock-ensure)
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
  (setq-local skroad--current-link-overlay
              (make-overlay (point-min) (point-min)))
  (skroad--current-link-overlay-deactivate)

  ;; Properties for selected link overlay
  (dolist (p skroad--current-link-overlay-properties)
    (overlay-put skroad--current-link-overlay (car p) (cadr p)))

  ;; Keymap:
  (use-local-map skroad--mode-keymap)

  ;; Handle word boundaries correctly (links are treated as unitary words) :
  (setq-local find-word-boundary-function-table
              skroad-find-word-boundary-function-table)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
