;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

(defun skroad--follow-link (data)
  (message (format "Link '%s' pushed!" data)))

(defun skroad--help-echo (window buf position)
  (with-current-buffer buf
    (let ((target (button-at position)))
      (if target
	  (button-get target 'button-data)))))

(define-button-type 'skroad
  'action 'skroad--follow-link
  'help-echo 'skroad--help-echo
  'follow-link t)

(defconst skroad--button-properties '(button category face button-data id))

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
        ;; Get rid of old text property intervals:
        ;; TODO: check whether we actually need to do it in this region?
        (save-mark-and-excursion (replace-match match))
        
        ;; Buttonize the match:
        (make-text-button start end
                          :type 'skroad
                          'button-data target
                          ))
      t)))

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

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (eq major-mode 'skroad-mode)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

(defun skroad--extract-region-links (start end &optional object)
  "Extract links from OBJECT, as (CATEGORY . TARGET), from START...END."
  (let ((buf (or object (current-buffer)))
        (links nil))
    (while (let* ((properties (text-properties-at start buf))
                  (link (cons (plist-get properties 'category)
                              (plist-get properties 'button-data))))
             (when (car link)
               (push link links))
             (< (setq start (next-property-change start buf end))
                end)))
    links))

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
       (setq ,l ,seq1)
       (while (and ,l ,seq2)
         (setq ,current (car ,l))
         (when (skroad--delete-once ,current ,seq2)
           (skroad--delete-once ,current ,seq1))
         (setq ,l (cdr ,l))))))

(defun skroad--before-change-function (&optional start end)
  (let ((start-expanded (skroad--get-start-of-line start))
        (end-expanded (skroad--get-end-of-line end)))
    (let ((links (skroad--extract-region-links
                  start-expanded end-expanded)))
      (skroad--delete-intersection skroad--links-propose-create links)
      (setq-local skroad--links-propose-destroy
                  (nconc skroad--links-propose-destroy links))
      ))
  )

(defun skroad--after-change-function (start end length)
  (let ((start-expanded (skroad--get-start-of-line start))
        (end-expanded (skroad--get-end-of-line end)))
    (save-mark-and-excursion
      (font-lock-fontify-region start-expanded end-expanded))
    (let ((links (skroad--extract-region-links
                  start-expanded end-expanded)))
      (skroad--delete-intersection skroad--links-propose-destroy links)
      (setq-local skroad--links-propose-create
                  (nconc skroad--links-propose-create links))
      ))
  )

;; Right now, we just print the proposed link changes and do nothing.
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
  )

(defvar-local skroad--links-propose-create nil
  "New links (which may be dupes of existing links) created by a command.")

(defvar-local skroad--links-propose-destroy nil
  "Links removed from the buffer by a command; copies of them may remain.")

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
  (setq-local font-lock-extra-managed-props skroad--button-properties)
  
  ;; Zap properties and refontify during yank.
  ;; TODO: does this need with-silent-modifications for textmode temp buffers
  ;;       where skroad--silence-modifications is not in effect?
  (setq-local yank-handled-properties
              '((button . (lambda (category start end)
                            (remove-list-of-text-properties
                             start end skroad--button-properties)
                            (font-lock-ensure (skroad--get-start-of-line start)
                                              (skroad--get-end-of-line end))
                            ))))
  
  (add-hook 'before-change-functions 'skroad--before-change-function nil t)
  (add-hook 'after-change-functions 'skroad--after-change-function nil t)
  (add-hook 'post-command-hook 'skroad--post-command-hook nil t)
  
  ;; Fontification rules:
  (font-lock-add-keywords
   nil
   '((skroad--next-live-link (0 'link t))) ;; Live skroad links
   'set)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
