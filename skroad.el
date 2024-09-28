;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun skroad--follow-link (data)
  "User clicked, or pressed ENTER on, a link."
  (message (format "Link '%s' pushed!" data)))

(defun skroad--help-echo (window buf position)
  "User is mousing over a link."
  (with-current-buffer buf
    (let ((target (button-at position)))
      (if target
	  (button-get target 'button-data)))))

;; Fundamental link type.
;; TODO: derive all link types from this in the highlighter.
(define-button-type 'skroad
  'action 'skroad--follow-link
  'help-echo 'skroad--help-echo
  'follow-link t
  )

(defconst skroad--text-properties
  '(button category face button-data id)
  "Properties added by font-lock that must be removed when unfontifying.")

;; TODO: eat whitespace between link and delimiters
(defconst skroad--live-links-regex "\\[\\[\\([^][\n\t]+\\)\\]\\]"
  "Regex used to find live links in a node.")

(defun skroad--next-live-link (limit)
  "Buttonify links during fontification."
  (when (re-search-forward skroad--live-links-regex limit t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (match (match-string-no-properties 0))
          (target (match-string-no-properties 1)))
      (with-silent-modifications
        ;; Get rid of old text property intervals when in undo:
        (when undo-in-progress
          (save-mark-and-excursion (replace-match match)))
        
        ;; Buttonize the match:
        (make-text-button start end
                          :type 'skroad
                          'button-data target
                          'id (gensym)
                          ))
      t)))

;;; TODO: next-single-property-change?
(defun skroad--extract-region-links (start end &optional object)
  "Extract links from OBJECT, as (CATEGORY . TARGET), from START...END."
  (let ((buf (or object (current-buffer)))
        (links nil))
    (while (let* ((properties (text-properties-at start buf))
                  (link (cons (plist-get properties 'category)
                              (plist-get properties 'button-data))))
             (when (car link)
               (push link links))
             (< (setq start (next-single-property-change
                             start 'button-data buf end))
                end)))
    links))

(defun skroad--before-change-function (&optional start end)
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
   (save-mark-and-excursion
     (font-lock-fontify-region start-expanded end-expanded))
   (let ((links (skroad--extract-region-links
                 start-expanded end-expanded)))
     (skroad--delete-intersection skroad--links-propose-destroy links)
     (setq-local skroad--links-propose-create
                 (nconc skroad--links-propose-create links)))))

;; Currently we just print the proposed link changes and do nothing else.
(defun skroad--post-command-hook ()
  "Process proposed creation and destruction of links."
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

  ;; Handle consequences of point motion and/or link create/destroy:
  (skroad--update-current-link)
  )

(defvar-local skroad--current-link-overlay nil
  "Overlay active when a link is under the point.")

(defun skroad--current-link-overlay-activate (start end)
  "Activate the current link overlay from START...END."
  (move-overlay skroad--current-link-overlay start end))

(defun skroad--current-link-overlay-deactivate ()
  "Deactivate the current link overlay; it can be reactivated again."
  (delete-overlay skroad--current-link-overlay))

(defun skroad--current-link-overlay-active-p ()
  "Return nil if the current link overlay is deactivated; otherwise buffer."
  (overlay-buffer skroad--current-link-overlay))

(defun skroad--link-at-pos-p (pos)
  "Determine whether there is a link at the given POS."
  (get-text-property pos 'button))

(defun skroad--link-start (pos)
  "Get the start of the link found at the given POS."
  (or (previous-single-property-change (1+ pos) 'id)
      (point-min)))

(defun skroad--link-end (pos)
  "Get the end of the link found at the given POS."
  (or (next-single-property-change pos 'id)
      (point-max)))

(defun skroad--update-current-link ()
  "Update the current link overlay."
  (let* ((p (point))
         (l (skroad--link-at-pos-p p)))
    (cond ((and (skroad--current-link-overlay-active-p) (not l))
           (skroad--current-link-overlay-deactivate)
           (setq-local cursor-type t)
           )
          (l
           (let ((link-start (skroad--link-start p))
                 (link-end (skroad--link-end p)))
             (skroad--current-link-overlay-activate link-start link-end)
             (goto-char link-start)
             (setq-local cursor-type nil)
             )))))

(defmacro skroad--with-current-link (&rest body)
  "Use in a command which operates on the current link overlay, if it exists."
  `(when (skroad--current-link-overlay-active-p)
     (let ((start (overlay-start skroad--current-link-overlay))
           (end (overlay-end skroad--current-link-overlay)))
       ,@body)))

(defun skroad--current-link-insert-space ()
  "Insert a space to the left of the current link."
  (interactive)
  (save-mark-and-excursion
    (skroad--with-current-link
     (goto-char start)
     (insert " "))))

(defun skroad--current-link-skip-left ()
  "Move the point to the left of the current link overlay, if active."
  (interactive)
  (skroad--with-current-link
   (goto-char (max (point-min) (1- start)))))

(defun skroad--current-link-skip-right ()
  "Move the point to the right of the current link overlay, if active."
  (interactive)
  (skroad--with-current-link
   (goto-char end)))

(defvar skroad--current-link-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (button-type-get 'skroad 'keymap))
    (define-key map (kbd "<left>") #'skroad--current-link-skip-left)
    (define-key map (kbd "<right>") #'skroad--current-link-skip-right)
    (define-key map (kbd "SPC") #'skroad--current-link-insert-space)
    (define-key map [remap self-insert-command] 'ignore)
    map)
  "Keymap automatically activated inside the current link overlay.")

(defconst skroad--current-link-overlay-properties
  `((face highlight)
    (evaporate t)
    (keymap ,skroad--current-link-overlay-keymap)
    )
  "Text properties of the current link overlay.")

(defun skroad--open-node ()
  "Open a skroad node."
  (button-mode)
  (skroad--current-link-overlay-deactivate)
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
  
  (advice-add 'use-region-p :filter-return
              (lambda (v)
                (or v (and (eq major-mode 'skroad-mode)
                           (skroad--current-link-overlay-active-p)))))

  (advice-add 'mark :filter-return
              (lambda (v)
                (cond (v v)
                      ((and (eq major-mode 'skroad-mode)
                            (skroad--current-link-overlay-active-p))
                       (overlay-start skroad--current-link-overlay))
                      (t nil))))
  
  (advice-add 'region-beginning :around
              (lambda (orig-fun &rest args)
                (if (and (eq major-mode 'skroad-mode)
                         (not mark-active)
                         (skroad--current-link-overlay-active-p))
                    (overlay-start skroad--current-link-overlay)
                  (apply orig-fun args))))
  
  (advice-add 'region-end :around
              (lambda (orig-fun &rest args)
                (if (and (eq major-mode 'skroad-mode)
                         (not mark-active)
                         (skroad--current-link-overlay-active-p))
                    (overlay-end skroad--current-link-overlay)
                  (apply orig-fun args))))
  
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

  ;; Overlay for when a link is under the point. Initially inactive:
  (setq-local skroad--current-link-overlay
              (make-overlay (point-min) (point-min)))
  (skroad--current-link-overlay-deactivate)

  ;; Properties for selected link overlay
  (dolist (p skroad--current-link-overlay-properties)
    (overlay-put skroad--current-link-overlay (car p) (cadr p)))
  
  ;; Fontification rules:
  (font-lock-add-keywords
   nil
   '((skroad--next-live-link (0 'link t))) ;; Live skroad links
   'set)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
