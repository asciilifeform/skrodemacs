;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--delete-once-helper (elt seq)
  "Return (V . SEQprime) where SEQprime is SEQ after destructively removing
at most one instance of ELT. If something was removed, V is T, nil otherwise."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--text-type-table nil
  "Definitions for all text types used in skroad.")

(defun skroad--text-type-get-prop (type-name property)
  "Get value of PROPERTY from text type TYPE-NAME, or from its supertype(s)."
  (let ((type-props (plist-get skroad--text-type-table type-name)))
    (when type-props
      (or (plist-get type-props property)
          (let ((parent (plist-get type-props :supertype)))
            (when parent
              (skroad--text-type-get-prop parent property)))))))

(defun skroad--text-type-set-prop (type-name property value)
  "Set value of PROPERTY of text type TYPE-NAME to VALUE."
  (let ((type-props (plist-get skroad--text-type-table type-name)))
    (when (not type-props)
      (error (format "Type %s not defined!" type-name)))
    (setq skroad--text-type-table
          (plist-put skroad--text-type-table type-name
                     (plist-put type-props property value)))
    value))

(defmacro skroad--with-text-type-props (type-name properties &rest body)
  "Bind each of the given text type PROPERTIES -- e.g. (:foo :bar ...)
as (foo (skroad--text-type-get-prop TYPE-NAME :foo)) etc., and evaluate BODY."
  `(let ,(mapcar
          #'(lambda (p)
              (cons (read (substring (symbol-name p) 1))
                    `((skroad--text-type-get-prop ,type-name ,p))))
          properties)
     (progn
       ,@body)))

(defmacro skroad--define-text-type (type-name &rest args)
  "Define a text type for use in skroad."
  (setq skroad--text-type-table
        (plist-put skroad--text-type-table type-name args))
  
  (skroad--with-text-type-props
   type-name
   (:supertype :face :buttonized :title
               :keymap :parent-keymap :keys
               :help-echo :mouse-face
               :display
               :start-delim :payload-regex :end-delim
               )
   
   `(progn
      ,(when (and keys (not (plist-get args :keymap)))
         (let ((inherit-keymap
                (or keymap (eval parent-keymap))))
           (setq keymap (make-sparse-keymap))
           (when inherit-keymap
             (set-keymap-parent keymap inherit-keymap))
           (dolist (k keys)
             (eval `(define-key keymap ,@k))))
         (skroad--text-type-set-prop type-name :keymap keymap)
         t)
      
      ,(when buttonized
         `(define-button-type ',type-name
            'face ,face
            ,@(when keymap
                `('keymap ',keymap))
            ,@(when help-echo
                `('help-echo ',help-echo))
            ,@(when mouse-face
                `('mouse-face ',mouse-face))
            ,@(when supertype
                `(:supertype ',supertype))
            ))

      ,(when display
         (skroad--text-type-set-prop
          type-name :make-regex
          `(lambda (&optional payload)
             (concat ,(regexp-quote (or start-delim ""))
                     (or payload ,payload-regex)
                     ,(regexp-quote (or end-delim "")))))

         (skroad--text-type-set-prop
          type-name :find-next
          `(lambda (end &optional payload)
             ,(when (not title)
                `(when (eq (line-beginning-position) (point-min))
                   (goto-char (line-beginning-position 2))))
             (and
              ,(if title
                   `(eq (line-beginning-position) (point-min))
                 t)
              (re-search-forward
               (funcall
                (skroad--text-type-get-prop ',type-name :make-regex)
                payload)
               ,(if title
                    `(max end (line-beginning-position 2))
                  `(if (< (point) end)
                       end
                     (skroad--get-end-of-line (point)))
                  )
               t))
             ))
         
         (skroad--text-type-set-prop
          type-name :font-lock-rule
          `((lambda (limit)
              (when (funcall
                     (skroad--text-type-get-prop ',type-name :find-next)
                     limit)
                (with-silent-modifications
                  ,(if buttonized
                       `(make-text-button
                         (match-beginning 0) (match-end 0)
                         :type ',type-name
                         'button-data (match-string-no-properties 1)
                         'button (gensym))
                     `(put-text-property
                       (match-beginning 0) (match-end 0)
                       'font-lock-face ,face)))))
            (0 'nil 'append))
          )
         t
         )
      )))

(defun skroad--text-type-generate (type-name payload)
  "Generate text of given TYPE-NAME with given PAYLOAD."
  (skroad--with-text-type-props
   type-name (:start-delim :end-delim)
   (concat start-delim payload end-delim)))

(defmacro skroad--with-each-text-type (properties &rest body)
  "Evaluate BODY, binding PROPERTIES, for every known text type."
  `(dolist (text-type skroad--text-type-table)
     (skroad--with-text-type-props
      text-type ,properties
      ,@body)))

(defun skroad--text-type-replace-all
    (type-name-old payload-old type-name-new payload-new)
  "Replace all text of TYPE-NAME-OLD having PAYLOAD-OLD with
instances of TYPE-NAME-NEW having PAYLOAD-NEW."
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (funcall
            (skroad--text-type-get-prop type-name-old :find-next)
            (point-max) payload-old)
      (replace-match
       (skroad--text-type-generate type-name-new payload-new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--link-at (pos)
  "Get the payload of the link found at the given POS, or nil if none."
  (get-text-property pos 'button-data))

(defun skroad--link-at-prev (pos)
  "Determine whether there is a link at the position prior to POS."
  (and (> pos (point-min))
       (skroad--link-at (1- pos))))

(defmacro skroad--with-link-at-point (&rest body)
  "Evaluate BODY with link bound to the link under the point."
  `(let ((link (skroad--link-at (point))))
     (when link
       ,@body)))

(defun skroad--backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p)
           (delete-region (region-beginning) (region-end)))
          ((skroad--link-at-prev p)
           (delete-region (button-start (1- p)) p))
          (t
           (delete-char -1)))))

(defmacro skroad--make-link-region-cmd (command)
  "Wrap COMMAND to use region if one exists, or use link at point as region."
  `#'(lambda ()
       (interactive)
       (apply #',command
              (if (use-region-p)
                  (list (region-beginning) (region-end))
                (list (button-start (point))
                      (button-end (point)))))))

(defvar skroad--mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-backward-char] #'skroad--backspace)
    map)
  "Keymap for skroad mode.")

;;; Text Types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--text-properties
  '(button category face font-lock-face button-data)
  "Properties added by font-lock that must be removed when unfontifying.")

(defun skroad--link-insert-space ()
  "Insert a space immediately behind the link under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (button-start (point)))
    (insert " ")))

(skroad--define-text-type
 skroad-button
 :doc "Fundamental type for all skroad buttonized links."
 :buttonized t
 :face 'link
 :mouse-face highlight
 :parent-keymap skroad--mode-keymap
 :keys (((kbd "SPC") #'skroad--link-insert-space)
        ([remap self-insert-command] 'ignore)
        ((kbd "<deletechar>") (skroad--make-link-region-cmd delete-region))
        ((kbd "<backspace>") (skroad--make-link-region-cmd delete-region))
        ([mouse-1] 'ignore)
        ([mouse-2] 'ignore)
        ([mouse-3] 'ignore)
        ((kbd "RET") 'ignore)
        ([remap kill-region] (skroad--make-link-region-cmd kill-region))
        ([remap kill-ring-save] (skroad--make-link-region-cmd kill-ring-save))
        )
 )

(defun skroad--link-to-plain-text ()
  "Debuttonize the link under the point to plain text by removing delimiters."
  (interactive)
  (let* ((p (point))
         (start (button-start p))
         (end (button-end p))
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
 skroad-node-link
 :doc "Fundamental type for skroad node links (live or dead)."
 :supertype skroad-button
 :help-echo skroad--link-mouseover
 :keys (((kbd "t") #'skroad--link-to-plain-text))
 :payload-regex "\s*\\([^][\n\t\s]+[^][\n\t]*?\\)\s*"
 )

(defun skroad--live-link-to-dead ()
  "Transform all live links with payload LINK to dead links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all
    'skroad-live link 'skroad-dead link)))

(defun skroad--go-to-live-link ()
  "Navigate to the live skroad link at point."
  (interactive)
  (skroad--with-link-at-point
   (message (format "Live link pushed: '%s'" link))))

(skroad--define-text-type
 skroad-live
 :doc "Live link to a skroad node."
 :supertype skroad-node-link
 :display t
 :track t
 :start-delim "[[" :end-delim "]]"
 :keys (((kbd "RET") #'skroad--go-to-live-link)
        ([mouse-1] #'skroad--go-to-live-link)
        ((kbd "l") #'skroad--live-link-to-dead))
 )

(defun skroad--dead-link-to-live ()
  "Transform all dead links with payload LINK to live links."
  (interactive)
  (skroad--with-link-at-point
   (skroad--text-type-replace-all
    'skroad-dead link 'skroad-live link)))

(skroad--define-text-type
 skroad-dead
 :doc "Dead link to a skroad node."
 :supertype skroad-node-link
 :display t
 :track t
 :start-delim "[-[" :end-delim "]-]"
 :face '(:inherit link :foreground "red")
 :keys (((kbd "l") #'skroad--dead-link-to-live))
 )

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
     (goto-char (button-start (point)))
     (search-forward "//" (button-end (point)))
     (insert " "))))

(skroad--define-text-type
 skroad-url-link
 :doc "URL."
 :supertype skroad-button
 :display t
 :help-echo "External link."
 :payload-regex
 "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\t\s]+\\)"
 :keys (((kbd "t") #'skroad--comment-url)
        ((kbd "RET") #'skroad--go-to-url)
        ([mouse-1] #'skroad--go-to-url))
 )

(skroad--define-text-type
 skroad-node-title
 :doc "Node title."
 :track t
 :display t
 :title t
 :face '(:weight bold :foreground "purple"
                 :height 1.5 :inverse-video t :extend t)
 :start-delim "" :end-delim "\n"
 :payload-regex "\\([^\n]+\\)"
 )

(skroad--define-text-type
 skroad-italic
 :doc "Italicized text."
 :display t
 :face '(:slant italic)
 :start-delim "__" :end-delim "__"
 :payload-regex "\\([^\\_]+\\)"
 )

(skroad--define-text-type
 skroad-bold
 :doc "Bold text."
 :display t
 :face '(:weight bold)
 :start-delim "**" :end-delim "**"
 :payload-regex "\\([^\\*]+\\)"
 )

(skroad--define-text-type
 skroad-heading
 :doc "Heading text."
 :display t
 :face '(:weight bold :height 1.2 :inverse-video t)
 :start-delim "" :end-delim "\n"
 :payload-regex "^##\\([^\n\\#]+\\)"
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--get-tracked-in-region (start end)
  "Find all tracked text type instances in region START...END."
  (let ((tracked nil))
    (skroad--with-whole-lines
     start end
     (skroad--with-each-text-type
      (:track :find-next)
      (when track
        (save-mark-and-excursion
          (goto-char start-expanded)
          (while (funcall find-next end-expanded)
            (push (cons text-type (match-string-no-properties 1))
                  tracked))))))
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
             (goto-char (button-start p)))
            ;; If tried to move right from anywhere:
            ((> p skroad--prev-point)
             ;; Go to end of link.
             (goto-char (button-end p))))))

  ;; Point may have moved. Enable current link overlay iff on top of a link:
  (let ((p (point)))
    (if (and (skroad--link-at p)
             (not (skroad--region-selection-active-p)))
        (skroad--current-link-overlay-activate
         (button-start p) (button-end p))
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
      (set-mark (button-end m))
      (setq-local skroad--alt-mark (button-start m))
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
      (cond ((and link fwd)
             (goto-char (button-end pos)))
            (link
             (goto-char (button-start pos)))
            (fwd
             (forward-word-strictly))
            (t
             (backward-word-strictly)))
      (point))))

(defconst skroad-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad--find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--font-lock-turn-on ()
  "Enable font-lock for skroad mode."
  (let ((keywords nil))
    (skroad--with-each-text-type
     (:font-lock-rule)
     (when font-lock-rule
       (push font-lock-rule keywords)))
    (font-lock-add-keywords nil keywords t)))

(defun skroad--open-node ()
  "Open a skroad node."
  (button-mode)
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
