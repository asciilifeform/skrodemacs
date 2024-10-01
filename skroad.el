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

(defvar-local skroad--links-propose-create nil
  "Links (may be dupes of existing) introduced to the buffer by a command.")

(defvar-local skroad--links-propose-destroy nil
  "Links removed from the buffer by a command; other instances may remain.")

(defun skroad--link-at-pos-p (pos)
  "Determine whether there is a link at the given POS."
  (get-text-property pos 'id))

(defun skroad--link-at-prev-pos-p (pos)
  "Determine whether there is a link at the position prior to POS."
  (and (> pos (point-min))
       (get-text-property (1- pos) 'id)))

(defun skroad--link-start (pos)
  "Get the start of the link found at the given POS."
  (or (previous-single-property-change (1+ pos) 'id)
      (point-min)))

(defun skroad--link-end (pos)
  "Get the end of the link found at the given POS."
  (or (next-single-property-change pos 'id)
      (point-max)))

(defun skroad--link-at-pos (pos)
  "Get the payload of the link found at the given POS."
  (get-text-property pos 'button-data))

(defun skroad--follow-link (data)
  "User clicked, or pressed ENTER on, a link."
  (message (format "Link '%s' pushed!" (string-trim data))))

(defun skroad--help-echo (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
      (if (skroad--link-at-pos-p position)
	  (skroad--link-at-pos position))))

(defmacro skroad--make-link-region-cmd (command)
  "Wrap COMMAND to use region if one exists, or use link at point as region."
  `#'(lambda ()
       (interactive)
        (apply #',command
               (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (skroad--link-start (point))
                       (skroad--link-end (point)))))))

(defmacro skroad--remap-cmd-link-region (map command)
  "Remap COMMAND in keymap MAP using `skroad--make-link-region-cmd'."
  `(define-key ,map [remap ,command]
               (skroad--make-link-region-cmd ,command)))

(defun skroad--backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p)
           (delete-region (region-beginning) (region-end)))
          ((skroad--link-at-prev-pos-p p)
           (delete-region (skroad--link-start (1- p)) p))
          (t
           (delete-char -1)))))

(defvar skroad--mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-backward-char] #'skroad--backspace)
    map)
  "Keymap for skroad mode.")

(defvar skroad--link-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map skroad--mode-keymap)
    (define-key map (kbd "<left>") #'skroad--link-skip-left)
    (define-key map (kbd "<right>") #'skroad--link-skip-right)
    (define-key map (kbd "SPC") #'skroad--link-insert-space)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "<deletechar>")
                (skroad--make-link-region-cmd delete-region))
    (define-key map (kbd "<backspace>")
                (skroad--make-link-region-cmd delete-region))
    (define-key map [mouse-1] 'ignore)
    (define-key map [mouse-2] 'ignore)
    (define-key map [mouse-3] 'ignore)
    (define-key map (kbd "RET") 'ignore)
    (skroad--remap-cmd-link-region map kill-region)
    (skroad--remap-cmd-link-region map kill-ring-save)
    map)
  "Keymap automatically activated when the point is above any link.")

(defun skroad--link-insert-space ()
  "Insert a space immediately behind the link under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (skroad--link-start (point)))
    (insert " ")))

(defun skroad--link-skip-left ()
  "Move the point to the immediate left of the link under the point."
  (interactive)
  (goto-char (max (point-min) (1- (skroad--link-start (point))))))

(defun skroad--link-skip-right ()
  "Move the point to the immediate right of the link under the point."
  (interactive)
  (goto-char (skroad--link-end (point))))

;; Fundamental skroad link type:
(define-button-type 'skroad
  'help-echo 'skroad--help-echo
  'keymap skroad--link-keymap
  )

(defface skroad--live-link-face
  '((t :inherit link))
  "Face for live links in skroad mode.")

(defface skroad--dead-link-face
  '((t :inherit link :foreground "red"))
  "Face for dead links in skroad mode.")

;; Live (i.e. node exists) links:
(define-button-type 'skroad-live
  'face 'skroad--live-link-face
  :supertype 'skroad
  )

;; Dead (i.e. placeholder) links:
(define-button-type 'skroad-dead
  'face 'skroad--dead-link-face
  :supertype 'skroad
  )

(defconst skroad--text-properties
  '(button category face button-data id)
  "Properties added by font-lock that must be removed when unfontifying.")

(defconst skroad--link-payload "\s*\\([^][\n\t\s]+[^][\n\t]*?\\)\s*"
  "Regex used for the payload of any link.")

(defconst skroad--live-links-regex
  (concat "\\[\\[" skroad--link-payload "\\]\\]")
  "Regex used to find live links in a node.")

(defconst skroad--dead-links-regex
  (concat "\\[-\\[" skroad--link-payload "\\]-\\]")
  "Regex used to find dead links in a node.")

;; TODO: replace with a table-generated thing
(defmacro skroad--make-regex-matcher (regex button-type)
  "Make a regex matcher for given REGEX and BUTTON-TYPE for use in font-lock."
  `(lambda (limit)
     (when (re-search-forward ,regex limit t)
       (let ((start (match-beginning 0))
             (end (match-end 0))
             (match (match-string-no-properties 0))
             (target (match-string-no-properties 1)))
         (with-silent-modifications
           (make-text-button start end
                             :type ,button-type
                             'button-data target
                             'id (gensym)
                             ))
         t))))

;; TODO: replace with a table-generated thing
(defun skroad--extract-region-links (start end)
  (let ((links nil))
    (save-mark-and-excursion
      (goto-char start)
      (while (re-search-forward skroad--live-links-regex end t)
        (push (cons 'live (match-string-no-properties 1)) links))
      (goto-char start)
      (while (re-search-forward skroad--dead-links-regex end t)
        (push (cons 'dead (match-string-no-properties 1)) links))
    )
    links
  ))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in a skroad buffer in region START...END."
  (skroad--with-whole-lines
   start end
   (let ((links (skroad--extract-region-links
                 start-expanded end-expanded)))
     (skroad--delete-intersection skroad--links-propose-create links)
     (setq-local skroad--links-propose-destroy
                 (nconc skroad--links-propose-destroy links)))))

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  (skroad--with-whole-lines
   start end
   (let ((links (skroad--extract-region-links
                 start-expanded end-expanded)))
     (skroad--delete-intersection skroad--links-propose-destroy links)
     (setq-local skroad--links-propose-create
                 (nconc skroad--links-propose-create links)))))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (when skroad--links-propose-destroy
    (message (format "proposed destroy: %s"
                     skroad--links-propose-destroy))
    (setq-local skroad--links-propose-destroy nil)
    )
  (when skroad--links-propose-create
    (message (format "proposed create: %s"
                     skroad--links-propose-create))
    (setq-local skroad--links-propose-create nil)
    )

  ;; Update the current link overlay and toggle the cursor when required:
  (font-lock-ensure)
  (skroad--update-current-link)
  (skroad--adjust-mark-if-present)
  )

(defvar-local skroad--current-link-overlay nil
  "Overlay active when a link is under the point.")

(defmacro skroad--with-current-link (&rest body)
  "Use in a command which operates on the current link overlay, if it exists."
  `(when (skroad--current-link-overlay-active-p)
     (let ((link-start (overlay-start skroad--current-link-overlay))
           (link-end (overlay-end skroad--current-link-overlay)))
       ,@body)))

;; (defmacro skroad--make-region-cmd (command)
;;   `#'(lambda ()
;;        (interactive)
;;        (skroad--with-current-link
;;         (apply #',command (list link-start link-end)))))

;; (defmacro skroad--remap-cmd-active-region (map command)
;;   `(define-key ,map [remap ,command]
;;                (skroad--make-region-cmd ,command)))

(defvar skroad--current-link-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map skroad--link-keymap)
    ;; (define-key map (kbd "<deletechar>") (skroad--make-region-cmd delete-region))
    ;; (define-key map (kbd "<backspace>") (skroad--make-region-cmd delete-region))
    ;; (skroad--remap-cmd-active-region map kill-region)
    ;; (skroad--remap-cmd-active-region map kill-ring-save)
    map)
  "Keymap automatically activated when the point is above a selected link.")

(defconst skroad--current-link-overlay-properties
  `((face highlight)
    (evaporate t)
    (keymap ,skroad--current-link-overlay-keymap)
    )
  "Text properties of the current link overlay.")

(defun skroad--current-link-overlay-activate (start end)
  "Activate (if inactive) or move the current link overlay from START...END."
  (move-overlay skroad--current-link-overlay start end (current-buffer)))

(defun skroad--current-link-overlay-deactivate ()
  "Deactivate the current link overlay; it can be reactivated again."
  (when (skroad--current-link-overlay-active-p)
    (delete-overlay skroad--current-link-overlay)))

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
    (if (skroad--link-at-pos-p p)
        ;; There is a link is under the point:
        (let ((link-start (skroad--link-start p))
              (link-end (skroad--link-end p)))
          ;; Activate/move the overlay, unless a region is active:
          (if (skroad--region-selection-active-p)
              (skroad--current-link-overlay-deactivate)
            (skroad--current-link-overlay-activate link-start link-end))
          ;; jump to the start of the link, or skip if abutting in region:
          ;; (goto-char (if (eq p (mark))
          ;;                link-end
          ;;              link-start))
          ;; (if (eq p (mark))
          ;;     (message "p=m"))
          ;; (if (eq p skroad--alt-mark)
          ;;     (message "p=am"))
          (goto-char link-start)
          (message (format "oldp=%s p=%s m=%s am=%s"
                           p (point) (mark) skroad--alt-mark))
          )
      ;; When there is no link under the point, deactivate the overlay:
      (skroad--current-link-overlay-deactivate)))
  ;; Enable cursor if and only if it is not currently under the link overlay:
  (setq-local cursor-type (not (skroad--current-link-overlay-active-p))))

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
  (let ((m (mark)))
    ;; When mark is set in a link, set mark to link end and alt-mark to start:
    (when (skroad--link-at-pos-p m)
      (set-mark (skroad--link-end m))
      (setq-local skroad--alt-mark (skroad--link-start m))
      (skroad--current-link-overlay-deactivate))))

(defun skroad--deactivate-mark-hook ()
  "Triggers when the mark becomes inactive."
  (setq-local skroad--alt-mark nil)
  (skroad--update-current-link))

(defun skroad-find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-excursion
    (let ((link (skroad--link-at-pos-p pos))
          (fwd (<= pos limit)))
      (cond ((and link fwd)
             (goto-char (skroad--link-end pos)))
            (link
             (goto-char (skroad--link-start pos)))
            (fwd
             (forward-word-strictly))
            (t
             (backward-word-strictly)))
      (point))))

(defconst skroad-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad-find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

(defun skroad--open-node ()
  "Open a skroad node."
  (button-mode)
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
  
  ;; Fontification rules:
  (font-lock-add-keywords
   nil
   `((,(skroad--make-regex-matcher skroad--live-links-regex 'skroad-live)
      (0 'nil 'append))
     (,(skroad--make-regex-matcher skroad--dead-links-regex 'skroad-dead)
      (0 'nil 'append))
     )
   'set)
  
  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
